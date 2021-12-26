module Exp
  ( Exp (..)
  ) where

import Map.Lazy (Map)
import Name

data Exp
  = Var Name
  | Abs Name Exp
  | App Exp Exp
  | Let Name Exp Exp
  | Select Exp Name
  | Label Name Exp deriving Show
