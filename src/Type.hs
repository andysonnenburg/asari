module Type
  ( Type
  , MType
  ) where

import Data.Map (Map)

import FA
import Head
import Name

type Type = (Map Name (DFA HeadMap), DFA HeadMap)

type MType r = (Map Name (NFA r HeadMap), NFA r HeadMap)
