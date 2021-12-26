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
  bizipWithM_ :: Applicative f => (a -> b -> f c) -> (a -> b -> f c) -> t a -> t b -> f ()
  unionWith :: (a -> a -> a) -> t a -> t a -> t a
  intersectionWith :: (a -> a -> a) -> t a -> t a -> t a
  isSubmapOf :: t a -> t b -> Bool
