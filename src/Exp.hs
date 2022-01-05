module Exp
  ( Exp (..)
  ) where

import Name

data Exp
  = Var Name
  | Abs Name Exp
  | App Exp Exp
  | Let Name Exp Exp
  | Seq Exp Exp
  | Struct (Maybe Name) [(Name, Exp)]
  | Field Exp Name
  | Switch Exp (Name, Exp) [(Name, Exp)]
  | Enum Name
  | Void deriving Show
