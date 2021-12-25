{-# LANGUAGE TypeFamilies #-}
module State
  ( Map (..)
  ) where

import Data.Kind (Type)

class Traversable t => Map t where
  type Key t :: Type -> Type
  empty :: t a
  singleton :: Key t a -> t a
  bitraverse :: Applicative f => (a -> f b) -> (a -> f b) -> t a -> f (t b)
  unionWith :: (a -> a -> a) -> t a -> t a -> t a
  intersectionWith :: (a -> a -> a) -> t a -> t a -> t a
