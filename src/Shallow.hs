{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Shallow (Shallow (..)) where

import Data.Functor

newtype Shallow f a = Shallow { getShallow :: f (Equal a) } deriving
  ( Functor
  , Foldable
  , Traversable
  )

deriving instance Eq (f (Equal a)) => Eq (Shallow f a)
deriving instance Ord (f (Equal a)) => Ord (Shallow f a)

newtype Equal a = Equal { getEqual :: a } deriving (Functor, Foldable, Traversable)

instance Eq (Equal a) where
  _ == _ = True

instance Ord (Equal a) where
  compare _ _ = EQ
