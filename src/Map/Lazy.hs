module Map.Lazy
  ( module Data.Map.Lazy
  , zipWithM_
  ) where

import Control.Monad qualified as Monad
import Data.Foldable qualified as Foldable
import Data.Map.Lazy

zipWithM_ :: ( Ord k
             , Applicative f
             ) => (a -> b -> f c) -> Map k a -> Map k b -> f ()
zipWithM_ f x y =
  Monad.zipWithM_
  f
  (Foldable.toList (intersection x y))
  (Foldable.toList (intersection y x))
    
