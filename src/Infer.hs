module Infer
  ( infer
  ) where

import Exp
import Map.Lazy (Map)
import Name
import Type
import Unify

infer :: Map Name Type -> Exp -> m (MType r)
infer = undefined
