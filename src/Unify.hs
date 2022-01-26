{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Unify
  ( MonadUnifyError (..)
  , unify
  , mergeNeg
  , mergePos
  ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.State.Strict
import Data.Bool
import Data.Foldable

import FA
import Ref
import Set (Set)
import Set qualified
import State qualified

class Monad m => MonadUnifyError t m where
  throwUnifyError :: t a -> t a -> m b

instance MonadUnifyError t m => MonadUnifyError t (StateT s m) where
  throwUnifyError x y = lift $ throwUnifyError x y

unify :: ( State.Map t
         , MonadUnifyError t m
         , MonadFix m
         , MonadRef r m
         ) => NFA r t -> NFA r t -> m ()
unify x y = evalStateT (unify' x y) mempty

unify' :: ( State.Map t
          , MonadUnifyError t m
          , MonadFix m
          , MonadRef r m
          , MonadState (Set (NFA r t, NFA r t)) m
          ) => NFA r t -> NFA r t -> m ()
unify' = fix $ \ recur x y -> unlessM (gets (Set.member (x, y))) $ do
  modify $ Set.insert (x, y)
  unlessM (State.isSubmapOf <$> readRef y.trans <*> readRef x.trans) $
    join $ throwUnifyError <$> readRef x.trans <*> readRef y.trans
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
