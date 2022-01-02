module Error
  ( Error (..)
  ) where

import Token

data Error
  = LexError
  | ParseError Token
  | UnifyError deriving Show
