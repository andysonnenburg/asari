{-# LANGUAGE ImportQualifiedPost #-}
module Main
  ( main
  ) where

import Data.ByteString qualified as ByteString
import Lex
import Infer
import Parse

main :: IO ()
main = ByteString.getContents >>= print . runLex parse
