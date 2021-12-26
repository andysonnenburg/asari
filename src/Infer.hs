module Infer
  ( infer
  ) where

import Data.Map (Map)

import Exp
import Name
import Type
import Unify

infer :: Map Name Type -> Exp -> m (MType r)
infer = undefined
