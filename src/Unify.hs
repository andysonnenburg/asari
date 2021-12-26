{-# LANGUAGE OverloadedRecordDot #-}
module Unify
  ( unify
  ) where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.State.Strict
import Data.Bool
import Data.Foldable

import FA
import Ref
import Set (Set)
import Set qualified
import State qualified

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m = p >>= bool m (pure ())

mergeNeg :: (State.Map t, MonadRef r m) => NFA r t -> NFA r t -> m ()
mergeNeg x y = do
  writeRef x.trans =<< State.intersectionWith (<>) <$> readRef y.trans <*> readRef x.trans
  writeRef x.flow =<< (<>) <$> readRef x.flow <*> readRef y.flow

mergePos :: (State.Map t, MonadRef r m) => NFA r t -> NFA r t -> m ()
mergePos x y = do
  writeRef x.trans =<< State.unionWith (<>) <$> readRef y.trans <*> readRef x.trans
  writeRef x.flow =<< (<>) <$> readRef x.flow <*> readRef y.flow

unify' :: ( State.Map t
          , MonadError () m
          , MonadFix m
          , MonadRef r m
          , MonadState (Set (NFA r t, NFA r t)) m
          ) => NFA r t -> NFA r t -> m ()
unify' = fix $ \ recur x y -> unlessM (gets (Set.member (x, y))) $ do
  modify $ Set.insert (x, y)
  unlessM (State.isSubmapOf <$> readRef y.trans <*> readRef x.trans) $ throwError ()
  readRef y.flow >>= traverse_ (flip mergePos x)
  readRef x.flow >>= traverse_ (flip mergeNeg y)
  let f xs ys = sequence $ recur <$> toList xs <*> toList ys
  join $ State.bizipWithM_ (flip f) f <$> readRef x.trans <*> readRef y.trans


unify :: ( State.Map t
         , MonadError () m
         , MonadFix m
         , MonadRef r m
         ) => NFA r t -> NFA r t -> m ()
unify x y = evalStateT (unify' x y) mempty
