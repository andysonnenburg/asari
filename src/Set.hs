{-# LANGUAGE LambdaCase #-}
module Set
  ( module Data.Set
  , insertWith
  , insertWithA
  , unionWith
  , unionWithA
  , mapMonotonicM
  , Set.traverse
  ) where

import Control.Category ((>>>))
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.Traversable as Traversable
import Data.Set
import Data.Set.Internal

insertWith :: Ord a => (a -> a -> a) -> a -> Set a -> Set a
insertWith f = (runIdentity .) . insertWithA ((Identity .) . f)

insertWithA :: (Applicative f, Ord a) => (a -> a -> f a) -> a -> Set a -> f (Set a)
insertWithA f x = fix $ \ recur -> \ case
  Tip -> pure $ singleton x
  Bin n y l r -> case compare x y of
    LT -> link y <$> recur l <*> pure r 
    EQ -> Bin n <$> f x y <*> pure l <*> pure r
    GT -> link y l <$> recur r

unionWith :: Ord a => (a -> a -> a) -> Set a -> Set a -> Set a
unionWith f = (runIdentity .) . unionWithA ((Identity .) . f)

unionWithA :: (Applicative f, Ord a) => (a -> a -> f a) -> Set a -> Set a -> f (Set a)
unionWithA f = fix $ \ recur -> curry $ \ case
  (Tip, x') -> pure x'
  (x@(Bin _ y l r), x') -> case splitMember' y x' of
    (l', Just y', r') -> flip link <$> recur l l' <*> f y y' <*> recur r r'
    (l', Nothing, r') -> link y <$> recur l l' <*> recur r r'

splitMember' :: Ord a => a -> Set a -> (Set a, Maybe a, Set a)
splitMember' x = fix $ \ recur -> \ case
  Tip -> (Tip, Nothing, Tip)
  Bin _ y l r -> case compare x y of
    LT -> case recur l of
      (l', x', r') -> (l', x', link y r' r)
    EQ -> (l, Just y, r)
    GT -> case recur r of
      (l', x', r') -> (link y l' l, x', r)

mapMonotonicM :: Applicative f => (a -> f b) -> Set a -> f (Set b)
mapMonotonicM f = fix $ \ recur -> \ case
  Tip -> pure Tip
  Bin n x l r -> flip (Bin n) <$> recur l <*> f x <*> recur r

traverse :: (Applicative f, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverse f = fmap fromList . Traversable.traverse f . toList
