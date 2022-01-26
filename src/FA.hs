{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module FA
  ( Label
  , NFA (..)
  , DFA (..)
  , FreezeT
  , evalFreezeT
  , fromNegNFA
  , fromPosNFA
  , ThawT
  , evalThawT
  , fromDFA
  ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Coerce
import Data.Foldable
import Data.Ord
import GHC.Records (getField)

import Ref
import Map.Lazy (Map)
import Map.Lazy qualified as Map
import Set (Set)
import Set qualified
import State qualified
import Supply

type Label = Word

data NFA r s = NFA
  { label :: Label
  , trans :: r (s (Set (NFA r s)))
  , flow :: r (Set (NFA r s))
  }

instance Eq (NFA r s) where
  x == y = getField @"label" x == getField @"label" y

instance Ord (NFA r s) where
  compare = comparing $ getField @"label"

data DFA s = DFA
  { label :: Label
  , trans :: s (DFA s)
  , flow :: Set (DFA s)
  }

instance Eq (DFA s) where
  x == y = getField @"label" x == getField @"label" y

instance Ord (DFA s) where
  compare = comparing $ getField @"label"

type FreezeT r s m =
  FixT
  (MultiMap (NFA r s) (DFA s))
  (StateT (Map (Set (NFA r s)) (DFA s)) (SupplyT Label m))

evalFreezeT :: MonadFix m => FreezeT r t m a -> m a
evalFreezeT m = runSupplyT (evalStateT (evalFixT m) mempty)

fromNegNFA :: ( State.Map s
              , MonadFix m
              , MonadRef r m
              ) => NFA r s -> FreezeT r s m (DFA s)
fromNegNFA = fromNegNFA' . Set.singleton

fromNegNFA' :: ( State.Map s
               , MonadFix m
               , MonadRef r m
               ) => Set (NFA r s) -> FreezeT r s m (DFA s)
fromNegNFA' xs =
  gets (Map.lookup xs) >>=
  (flip maybe pure $ mfix $ \ y ->
      putDFA xs y >>
      DFA <$> supply <*> getTrans xs <*> getDFAFlow xs)
  where
    getTrans =
      foldMapM' append State.empty (readRef . (.trans)) >=>
      State.bitraverse fromPosNFA' fromNegNFA'
    append =
      State.intersectionWith (<>)

fromPosNFA :: ( State.Map s
              , MonadFix m
              , MonadRef r m
              ) => NFA r s -> FreezeT r s m (DFA s)
fromPosNFA = fromPosNFA' . Set.singleton

fromPosNFA' :: ( State.Map s
               , MonadFix m
               , MonadRef r m
               ) => Set (NFA r s) -> FreezeT r s m (DFA s)
fromPosNFA' xs =
  gets (Map.lookup xs) >>=
  (flip maybe pure $ mfix $ \ y ->
      putDFA xs y >>
      DFA <$> supply <*> getTrans xs <*> getDFAFlow xs)
  where
    getTrans =
      foldMapM' append State.empty (readRef . (.trans)) >=>
      State.bitraverse fromNegNFA' fromPosNFA'
    append =
      State.unionWith (<>)

putDFA :: ( MonadState (Map (Set (NFA r s)) (DFA s)) m
          , MonadWriter (MultiMap (NFA r s) (DFA s)) m
          ) => Set (NFA r s) -> DFA s -> m ()
putDFA xs y = modify (Map.insert xs y) >> tell (fromSet xs y)

getDFAFlow :: ( MonadReader (MultiMap (NFA r s) (DFA s)) m
              , MonadRef r m
              ) => Set (NFA r s) -> m (Set (DFA s))
getDFAFlow = foldMapM (\ x -> lookupMany <$> readRef x.flow <*> ask)

type ThawT r s = StateT (Map (DFA s) (NFA r s))

evalThawT :: Monad m => ThawT r t m a -> m a
evalThawT m = evalStateT m mempty

fromDFA :: ( Traversable s
           , MonadFix m
           , MonadRef r m
           , MonadSupply Label m
           ) => DFA s -> ThawT r s m (NFA r s)
fromDFA x =
  gets (Map.lookup x) >>=
  (flip maybe pure $ mfix $ \ y ->
      modify (Map.insert x y) >>
      NFA <$> supply <*> getTrans x <*> getFlow x)
  where
    getTrans = newRef <=< traverse (fmap Set.singleton . fromDFA) . (.trans)
    getFlow = newRef <=< Set.traverse fromDFA . (.flow)

foldMapM :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM = foldMapM' mappend mempty

foldMapM' :: (Foldable t, Monad m) => (b -> b -> b) -> b -> (a -> m b) -> t a -> m b
foldMapM' append empty f = foldrM (\ x z -> append z <$> f x) empty

type FixT s m = ReaderT s (WriterT s m)

evalFixT :: MonadFix m => FixT s m a -> m a
evalFixT m = fmap fst $ mfix $ \ ~(_, s) -> runWriterT $ runReaderT m s

newtype MultiMap k a = MultiMap (Map k (Set a))

instance (Ord k, Ord v) => Semigroup (MultiMap k v) where
  x <> y = MultiMap $ Map.unionWith (<>) (coerce x) (coerce y)

instance (Ord k, Ord v) => Monoid (MultiMap k v) where
  mempty = MultiMap mempty

lookupMany :: (Ord k, Ord a) => Set k -> MultiMap k a -> Set a
lookupMany xs = fold . flip Map.restrictKeys xs . coerce

fromSet :: Set k -> a -> MultiMap k a
fromSet xs y = MultiMap $ Map.fromSet (const (Set.singleton y)) xs
