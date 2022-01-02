module Error
  ( Error (..)
  ) where

import Head
import Token

data Error
  = LexError
  | ParseError Token
  | UnifyError (HeadMap ()) (HeadMap ()) deriving Show
