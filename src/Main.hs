{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main
  ( main
  ) where

import Control.Category ((>>>))
import Control.Monad
import Data.ByteString qualified as ByteString
import System.IO

import Lex
import Infer
import Parse

main :: IO ()
main =
  ByteString.getContents >>=
  ((runLex parse >=> infer') >>> either (hPrint stderr) (const (pure ())))
