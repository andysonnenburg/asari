{-# LANGUAGE ImportQualifiedPost #-}
module Main
  ( main
  ) where

import Data.ByteString qualified as ByteString
import Lex (Lex, runLex)
import Infer
import Parse

main :: IO ()
main = ByteString.getContents >>= print . runLex parse
