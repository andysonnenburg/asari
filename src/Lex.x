{
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lex (Lex, lex) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Int
import Data.Word

import Token
}

$digit = 0-9
$alpha = [\_a-zA-Z]

:-
  $white+ ;
  "{" { const LeftBrace }
  "}" { const RightBrace }
  "(" { const LeftParen }
  ")" { const RightParen }
  "." { const Period }
  "," { const Comma }
  ";" { const Semicolon }
  fn { const Fn }
  val { const Val }
  var { const Var }
  switch { const Switch }
  case { const Case }
  $alpha [$alpha $digit]* { \ s -> Name s }

{
data AlexInput = AlexInput { alexStr :: !ByteString, alexBytePos :: {-# UNPACK #-} !Int64 }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte AlexInput {..} = case ByteString.uncons alexStr of
  Nothing -> Nothing
  Just (x, xs) -> Just (x, AlexInput { alexStr = xs, alexBytePos = alexBytePos + 1 })
}
