{-# LANGUAGE LambdaCase #-}
module Set
  ( module Data.Set
  , insertMember
  , Set.traverse
  ) where

import Data.Traversable as Traversable
import Data.Set

insertMember :: Ord a => a -> Set a -> Maybe (Set a)
insertMember x xs = if member x xs then Nothing else Just $ insert x xs

traverse :: (Applicative f, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverse f = fmap fromList . Traversable.traverse f . toList
