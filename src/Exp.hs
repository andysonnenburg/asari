module Exp
  ( Exp (..)
  ) where

import Name

data Exp a
  = Var a
  | Lam [a] (Exp a)
  | App (Exp a) (Exp a)
  | Val a (Exp a) (Exp a)
  | Fn a [a] (Exp a) (Exp a)
  | Seq (Exp a) (Exp a)
  | Block (Exp a)
  | Struct (Maybe Name) [(Name, Exp a)]
  | Field (Exp a) Name
  | Switch (Exp a) (Name, Exp a) [(Name, Exp a)] (Maybe (Exp a))
  | Enum Name
  | Void deriving Show
