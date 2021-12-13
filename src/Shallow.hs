{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Shallow (Shallow (..)) where

import Data.Functor

newtype Shallow f a = Shallow { getShallow :: f a } deriving (Functor, Foldable, Traversable)

instance Functor f => Eq (Shallow f a) where
  x == y = (x $> ()) == (y $> ())

instance Functor f => Ord (Shallow f a) where
  compare x y = compare (x $> ()) (y $> ())
