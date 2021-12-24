{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuantifiedConstraints #-}
module FA
  ( Label
  , NFA (..)
  , DFA (..)
  , FreezeT
  , evalFreezeT
  , toDFA
  , ThawT
  , evalThawT
  , toNFA
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Coerce
import Data.Foldable
import Data.Functor.Classes
import Data.Map (Map, findWithDefault)
import Data.Map qualified as Map
import Data.Ord
import GHC.Records (getField)

import Ref
import Set (Set)
import Set qualified
import State qualified
import Supply

type Label = Word

data NFA r f = NFA
  { label :: Label
  , trans :: r (f (Set (NFA r f)))
  , epsilonTrans :: r (Set (NFA r f))
  , flow :: r (Set (NFA r f))
  }

instance Eq (NFA r f) where
  x == y = getField @"label" x == getField @"label" y

instance Ord (NFA r f) where
  compare = comparing $ getField @"label"

data DFA f = DFA
  { label :: Label
  , trans :: f (DFA f)
  , flow :: Set (DFA f)
  }

instance Eq (DFA f) where
  x == y = getField @"label" x == getField @"label" y

instance Ord (DFA f) where
  compare = comparing $ getField @"label"

epsilonClosure' :: MonadRef r m => Set (NFA r f) -> NFA r f -> m (Set (NFA r f))
epsilonClosure' = fix $ \ recur xs x ->
  if Set.member x xs
  then pure xs
  else foldlM recur (Set.insert x xs) =<< readRef x.epsilonTrans

epsilonClosure :: MonadRef r m => NFA r f -> m (Set (NFA r f))
epsilonClosure = epsilonClosure' mempty

transClosure :: ( State.Map f
                , MonadRef r m
                ) => NFA r f -> m (f (Set (NFA r f)))
transClosure x = traverse (foldlM epsilonClosure' mempty) =<< readRef x.trans

foldMapM :: (Foldable f, Monad m, Monoid b) => (a -> m b) -> f a -> m b
foldMapM f = foldrM (\ x z -> mappend z <$> f x) mempty

type FixT s m = ReaderT s (WriterT s m)

evalFixT :: MonadFix m => FixT s m a -> m a
evalFixT m = fmap fst $ mfix $ \ ~(_, s) -> runWriterT $ runReaderT m s

newtype SemiMap k v = SemiMap { getSemiMap :: Map k v }

instance (Ord k, Semigroup v) => Semigroup (SemiMap k v) where
  x <> y = SemiMap $ Map.unionWith (<>) (coerce x) (coerce y)

instance (Ord k, Semigroup v) => Monoid (SemiMap k v) where
  mempty = SemiMap mempty

type FreezeT r f m =
  FixT
  (SemiMap (NFA r f) (Set (DFA f)))
  (StateT (Map (Set (NFA r f)) (DFA f)) (SupplyT Label m))

evalFreezeT :: MonadFix m => FreezeT r f m a -> m a
evalFreezeT m = runSupplyT (evalStateT (evalFixT m) mempty)

toDFA' :: ( State.Map f
          , MonadFix m
          , MonadRef r m
          ) => Bool -> Set (NFA r f) -> FreezeT r f m (DFA f)
toDFA' p = fix $ \ recur xs -> gets (Map.lookup xs) >>= \ case
  Just y -> pure y
  Nothing -> mfix $ \ y -> do
    modify $ Map.insert xs y
    tell $ coerce $ Map.fromSet (const (Set.singleton y)) xs
    env <- coerce <$> ask
    DFA <$>
      supply <*>
      undefined xs <*>
      foldMapM (fmap (foldMap (flip (findWithDefault mempty) env)) . readRef . (.flow)) xs

toDFA :: ( State.Map f
         , MonadFix m
         , MonadRef r m
         ) => Bool -> NFA r f -> FreezeT r f m (DFA f)
toDFA p = toDFA' p <=< epsilonClosure

type ThawT r f = StateT (Map (DFA f) (NFA r f))

evalThawT :: Monad m => ThawT r f m a -> m a
evalThawT m = evalStateT m mempty

toNFA :: ( Traversable f
         , MonadFix m
         , MonadRef r m
         , MonadSupply Label m
         ) => DFA f -> ThawT r f m (NFA r f)
toNFA = fix $ \ recur x -> gets (Map.lookup x) >>= \ case
  Just x -> pure x
  Nothing -> mfix $ \ y -> do
    modify $ Map.insert x y
    NFA <$>
      supply <*>
      (newRef <=< traverse (fmap Set.singleton . recur)) x.trans <*>
      newRef mempty <*>
      (newRef <=< Set.traverse recur) x.flow
