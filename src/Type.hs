module Type
  ( Type
  , MType
  ) where

import FA
import Head
import Map.Lazy (Map)
import Name

type Type = (Map Name (DFA HeadMap), DFA HeadMap)

type MType r = (Map Name (NFA r HeadMap), NFA r HeadMap)
