{-# LANGUAGE LambdaCase #-}
module Set
  ( module Data.Set
  , Set.traverse
  ) where

import Data.Traversable as Traversable
import Data.Set

traverse :: (Applicative f, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverse f = fmap fromList . Traversable.traverse f . toList
