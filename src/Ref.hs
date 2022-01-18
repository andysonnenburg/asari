{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Ref
  ( MonadRef (..)
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.IORef
import Data.STRef

class Monad m => MonadRef r m | m -> r where
  newRef :: a -> m (r a)
  readRef :: r a -> m a
  writeRef :: r a -> a -> m ()

instance MonadRef IORef IO where
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

instance MonadRef (STRef s) (ST s) where
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef

instance MonadRef r m => MonadRef r (ExceptT e m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef = (lift .) . writeRef

instance MonadRef r m => MonadRef r (ReaderT r' m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef = (lift .) . writeRef

instance MonadRef r m => MonadRef r (StateT s m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef = (lift .) . writeRef

instance (Monoid w, MonadRef r m) => MonadRef r (WriterT w m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef = (lift .) . writeRef
