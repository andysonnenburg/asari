{
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Lex
  ( Lex
  , runLex
  , getToken
  , throwError
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Function
import Data.Text.Encoding qualified as Text
import Data.Word

import Error
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
  "=" { const Equals }
  fn { const Fn }
  val { const Val }
  var { const Var }
  struct { const Struct }
  switch { const Switch }
  $alpha [$alpha $digit]* { Name }

{
runLex :: Lex a -> ByteString -> Either Error a
runLex m input = case getLex m AlexInput { pos = 0, .. } of
  Left e -> Left e
  Right (_, x) -> Right x

newtype Lex a = Lex { getLex :: AlexInput -> Either Error (AlexInput, a) }

instance Functor Lex where
  fmap f m = Lex $ \ s -> case getLex m s of
    Left e -> Left e
    Right (s, x) -> Right (s, f x)

instance Applicative Lex where
  pure x = Lex $ \ s -> Right (s, x)
  f <*> x = Lex $ \ s -> case getLex f s of
    Left e -> Left e
    Right (s, f) -> case getLex x s of
      Left e -> Left e
      Right (s, x) -> Right (s, f x)

instance Monad Lex where
  x >>= f = Lex $ \ s -> case getLex x s of
    Left e -> Left e
    Right (s, x) -> getLex (f x) s

getToken :: Lex Token
getToken = Lex $ fix $ \ recur s -> case alexScan s 0 of
  AlexEOF -> Right (s, EOF)
  AlexError _ -> Left LexError
  AlexSkip s _ -> recur s
  AlexToken s' _ f ->
    let n = s'.pos - s.pos
    in Right (s', f (Text.decodeUtf8 (ByteString.take n s.input)))

throwError :: Token -> Lex a
throwError = Lex . const . Left . ParseError

data AlexInput = AlexInput { input :: !ByteString, pos :: {-# UNPACK #-} !Int }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte AlexInput {..} = case ByteString.uncons input of
  Nothing -> Nothing
  Just (x, input) -> Just (x, AlexInput { pos = pos + 1, .. })
}
