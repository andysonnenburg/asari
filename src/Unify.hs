{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Unify
  ( unify
  ) where

import Control.Monad
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Fix
import Control.Monad.State.Strict
import Data.Bool
import Data.Foldable

import Error
import FA
import Head
import Ref
import Set (Set)
import Set qualified
import State qualified

unify :: ( MonadError Error m
         , MonadFix m
         , MonadRef r m
         ) => NFA r HeadMap -> NFA r HeadMap -> m ()
unify x y = evalStateT (unify' x y) mempty

unify' :: ( MonadError Error m
          , MonadFix m
          , MonadRef r m
          , MonadState (Set (NFA r HeadMap, NFA r HeadMap)) m
          ) => NFA r HeadMap -> NFA r HeadMap -> m ()
unify' = fix $ \ recur x y -> unlessM (gets (Set.member (x, y))) $ do
  modify $ Set.insert (x, y)
  unlessM (State.isSubmapOf <$> readRef y.trans <*> readRef x.trans) $ do
    e <- UnifyError <$> (void <$> readRef y.trans) <*> (void <$> readRef x.trans)
    throwError e
  readRef y.flow >>= traverse_ (flip mergePos x)
  readRef x.flow >>= traverse_ (flip mergeNeg y)
  let f xs ys = sequence $ recur <$> toList xs <*> toList ys
  join $ State.bizipWithM_ (flip f) f <$> readRef x.trans <*> readRef y.trans

mergeNeg :: (State.Map t, MonadRef r m) => NFA r t -> NFA r t -> m ()
mergeNeg x y = do
  writeRef x.trans =<< State.intersectionWith (<>) <$> readRef y.trans <*> readRef x.trans
  writeRef x.flow =<< (<>) <$> readRef x.flow <*> readRef y.flow

mergePos :: (State.Map t, MonadRef r m) => NFA r t -> NFA r t -> m ()
mergePos x y = do
  writeRef x.trans =<< State.unionWith (<>) <$> readRef y.trans <*> readRef x.trans
  writeRef x.flow =<< (<>) <$> readRef x.flow <*> readRef y.flow

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m = p >>= bool m (pure ())
