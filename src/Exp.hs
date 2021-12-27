module Exp
  ( Exp (..)
  ) where

import Name

data Exp
  = Var Name
  | Abs Name Exp
  | App Exp Exp
  | Let Name Exp Exp
  | Struct [(Name, Exp)]
  | Field Exp Name
  | Case Name Exp deriving Show
