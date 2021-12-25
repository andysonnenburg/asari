{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord
import GHC.Records (getField)

import Ref
import Set (Set)
import Set qualified
import State qualified
import Supply

type Label = Word

data NFA r t = NFA
  { label :: Label
  , trans :: r (t (Set (NFA r t)))
  , epsilonTrans :: r (Set (NFA r t))
  , flow :: r (Set (NFA r t))
  }

instance Eq (NFA r t) where
  x == y = getField @"label" x == getField @"label" y

instance Ord (NFA r t) where
  compare = comparing $ getField @"label"

data DFA t = DFA
  { label :: Label
  , trans :: t (DFA t)
  , flow :: Set (DFA t)
  }

instance Eq (DFA t) where
  x == y = getField @"label" x == getField @"label" y

instance Ord (DFA t) where
  compare = comparing $ getField @"label"

epsilonClosure' :: MonadRef r m => Set (NFA r t) -> NFA r t -> m (Set (NFA r t))
epsilonClosure' = fix $ \ recur xs x ->
  if Set.member x xs
  then pure xs
  else foldlM recur (Set.insert x xs) =<< readRef x.epsilonTrans

epsilonClosure :: MonadRef r m => NFA r t -> m (Set (NFA r t))
epsilonClosure = epsilonClosure' mempty

transClosure :: ( State.Map t
                , MonadRef r m
                ) => NFA r t -> m (t (Set (NFA r t)))
transClosure x = traverse (foldlM epsilonClosure' mempty) =<< readRef x.trans

foldMapM' :: (Foldable t, Monad m) => (b -> b -> b) -> b -> (a -> m b) -> t a -> m b
foldMapM' append empty f = foldrM (\ x z -> append z <$> f x) empty

foldMapM :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM = foldMapM' mappend mempty

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
fromSet xs y = MultiMap $ Map.fromSet (const ys) xs
  where
    ys = Set.singleton y

type FreezeT r t m =
  FixT
  (MultiMap (NFA r t) (DFA t))
  (StateT (Map (Set (NFA r t)) (DFA t)) (SupplyT Label m))

evalFreezeT :: MonadFix m => FreezeT r t m a -> m a
evalFreezeT m = runSupplyT (evalStateT (evalFixT m) mempty)

putDFA :: ( MonadState (Map (Set (NFA r t)) (DFA t)) m
          , MonadWriter (MultiMap (NFA r t) (DFA t)) m
          ) => Set (NFA r t) -> DFA t -> m ()
putDFA xs y = modify (Map.insert xs y) >> tell (fromSet xs y)

getDFAFlow :: ( MonadReader (MultiMap (NFA r t) (DFA t)) m
              , MonadRef r m
              ) => Set (NFA r t) -> m (Set (DFA t))
getDFAFlow = foldMapM (\ x -> lookupMany <$> readRef x.flow <*> ask)

fromNegNFA' :: ( State.Map t
               , MonadFix m
               , MonadRef r m
               ) => Set (NFA r t) -> FreezeT r t m (DFA t)
fromNegNFA' xs = gets (Map.lookup xs) >>= \ case
  Just y -> pure y
  Nothing -> mfix $ \ y ->
    putDFA xs y >>
    DFA <$> supply <*> getDFATrans xs <*> getDFAFlow xs
  where
    getDFATrans = recur <=< foldMapM' append State.empty transClosure
    recur = State.traverse' fromPosNFA' fromNegNFA'
    append = State.intersectionWith (<>)

fromPosNFA' :: ( State.Map t
               , MonadFix m
               , MonadRef r m
               ) => Set (NFA r t) -> FreezeT r t m (DFA t)
fromPosNFA' xs = gets (Map.lookup xs) >>= \ case
  Just y -> pure y
  Nothing -> mfix $ \ y ->
    putDFA xs y >>
    DFA <$> supply <*> getDFATrans xs <*> getDFAFlow xs
  where
    getDFATrans = recur <=< foldMapM' append State.empty transClosure
    recur = State.traverse' fromNegNFA' fromPosNFA'
    append = State.unionWith (<>)

fromNegNFA :: ( State.Map t
              , MonadFix m
              , MonadRef r m
              ) => NFA r t -> FreezeT r t m (DFA t)
fromNegNFA = fromNegNFA' <=< epsilonClosure

fromPosNFA :: ( State.Map t
              , MonadFix m
              , MonadRef r m
              ) => NFA r t -> FreezeT r t m (DFA t)
fromPosNFA = fromPosNFA' <=< epsilonClosure

type ThawT r t = StateT (Map (DFA t) (NFA r t))

evalThawT :: Monad m => ThawT r t m a -> m a
evalThawT m = evalStateT m mempty

fromDFA :: ( Traversable t
           , MonadFix m
           , MonadRef r m
           , MonadSupply Label m
           ) => DFA t -> ThawT r t m (NFA r t)
fromDFA = fix $ \ recur x -> gets (Map.lookup x) >>= \ case
  Just y -> pure y
  Nothing -> mfix $ \ y ->
    modify (Map.insert x y) >>
    NFA <$> supply <*> getNFATrans x <*> newRef mempty <*> getNFAFlow x
    where
      getNFATrans = newRef <=< traverse (fmap Set.singleton . recur) . (.trans)
      getNFAFlow = newRef <=< Set.traverse recur . (.flow)
