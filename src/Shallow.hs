{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}
module Shallow
  ( Shallow (..)
  ) where

import Data.Functor
import Data.Functor.Apply
import Data.Functor.Classes
import Data.Ord

newtype Shallow f a = Shallow { getShallow :: f (ConstEQ a) } deriving
  ( Functor
  , Foldable
  , Traversable
  )

instance Eq1 f => Eq (Shallow f a) where
  x == y = eq1 (getShallow x) (getShallow y)

instance Ord1 f => Ord (Shallow f a) where
  compare x y = compare1 (getShallow x) (getShallow y)

instance Apply f => Apply (Shallow f) where
  f <.> x = Shallow (ConstEQ <$> ((getConstEQ <$> getShallow f) <.> (getConstEQ <$> getShallow x)))

newtype ConstEQ a = ConstEQ { getConstEQ :: a } deriving (Functor, Foldable, Traversable)

instance Eq (ConstEQ a) where
  _ == _ = True

instance Ord (ConstEQ a) where
  compare _ _ = EQ
