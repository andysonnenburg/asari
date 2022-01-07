module Token
  ( Token (..)
  ) where

import Name

data Token
  = LeftBrace
  | RightBrace
  | LeftParen
  | RightParen
  | Period
  | Comma
  | Colon
  | Semicolon
  | Equals
  | Backslash
  | Fn
  | Val
  | Var
  | Struct
  | Switch
  | Case
  | Default
  | Enum
  | Name Name
  | EOF deriving Show
