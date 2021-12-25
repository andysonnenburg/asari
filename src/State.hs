{-# LANGUAGE TypeFamilies #-}
module State
  ( Map (..)
  ) where

import Data.Kind (Type)

class Traversable f => Map f where
  type Key f :: Type -> Type
  empty :: f a
  singleton :: Key f a -> f a
  traverse' :: Applicative m => (a -> m b) -> (a -> m b) -> f a -> m (f b)
  unionWith :: (a -> a -> a) -> f a -> f a -> f a
  intersectionWith :: (a -> a -> a) -> f a -> f a -> f a
