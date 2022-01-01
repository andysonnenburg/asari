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
  | Semicolon
  | Fn
  | Val
  | Var
  | Switch
  | Case
  | Name Name
