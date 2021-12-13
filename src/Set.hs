{-# LANGUAGE LambdaCase #-}
module Set
  ( module Data.Set
  , insertWith
  , insertWithA
  , unionWith
  , unionWithA
  ) where

import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.Set
import Data.Set.Internal

insertWith :: Ord a => (a -> a -> a) -> a -> Set a -> Set a
insertWith f = (runIdentity .) . insertWithA ((Identity .) . f)

insertWithA :: (Applicative f, Ord a) => (a -> a -> f a) -> a -> Set a -> f (Set a)
insertWithA f x = fix $ \ recur -> \ case
  Tip -> pure $ singleton x
  Bin n y l r -> case compare x y of
    LT -> recur l <&> \ l -> binL y l r 
    EQ -> f x y <&> \ x -> Bin n x l r
    GT -> recur r <&> \ r -> binR y l r
  where
    binL = undefined
    binR = undefined

unionWith :: Ord a => (a -> a -> a) -> Set a -> Set a -> Set a
unionWith f = (runIdentity .) . unionWithA ((Identity .) . f)

unionWithA :: (Applicative f, Ord a) => (a -> a -> f a) -> Set a -> Set a -> f (Set a)
unionWithA = undefined
