{-# LANGUAGE TypeFamilies #-}
module State
  ( Map (..)
  ) where

import Data.Kind (Type)

class Map f where
  type Key f :: Type -> Type
  empty :: f a
  singleton :: Key f a -> f a
  unionWith :: (a -> a -> a) -> f a -> f a -> f a
  intersectionWith :: (a -> a -> a) -> f a -> f a -> f a
