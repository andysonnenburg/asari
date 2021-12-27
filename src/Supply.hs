{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Supply
  ( MonadSupply (..)
  , Supply
  , runSupply
  , SupplyT
  , runSupplyT
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Functor.Identity

import Ref

class Monad m => MonadSupply s m | m -> s where
  supply :: m s

type Supply s = SupplyT s Identity

runSupply :: Num s => Supply s a -> a
runSupply = runIdentity . runSupplyT

newtype SupplyT s m a = SupplyT (StateT s m a) deriving (Functor, Applicative, Monad)

deriving instance MonadError e m => MonadError e (SupplyT s m)

deriving instance MonadFix m => MonadFix (SupplyT s m)

deriving instance MonadRef r m => MonadRef r (SupplyT s m)

runSupplyT :: (Num s, Monad m) => SupplyT s m a -> m a
runSupplyT (SupplyT m) = evalStateT m 0

instance (Num s, Monad m) => MonadSupply s (SupplyT s m) where
  supply = SupplyT $ state $ \ x -> (x, x + 1)

instance MonadSupply s m => MonadSupply s (ExceptT e m) where
  supply = lift supply

instance MonadSupply s m => MonadSupply s (ReaderT r m) where
  supply = lift supply

instance MonadSupply s m => MonadSupply s (StateT s' m) where
  supply = lift supply

instance (Monoid w, MonadSupply s m) => MonadSupply s (WriterT w m) where
  supply = lift supply
