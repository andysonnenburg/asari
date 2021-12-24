{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Head
  ( Head (..)
  , HeadMap
  ) where

import Data.Map qualified as Ord (Map)
import Data.Map qualified as Ord.Map
import Data.Maybe (catMaybes)

import State

data Head a
  = Void
  | Ref a a
  | Fn a a
  | Struct (Ord.Map String a)
  | Union (Ord.Map String a) deriving Show

data HeadMap a
  = HeadMap
    { void :: Bool
    , ref :: Maybe (a, a)
    , fn :: Maybe (a, a)
    , struct :: Maybe (Ord.Map String a)
    , union :: Maybe (Ord.Map String a)
    } deriving (Functor, Foldable, Traversable)

instance Show a => Show (HeadMap a) where
  showsPrec prec = showParen (prec > 10) . (showString "fromList " .) . shows . toList

toList :: HeadMap a -> [Head a]
toList HeadMap {..} = catMaybes
  $ (if void then Just Void else Nothing)
  : (uncurry Ref <$> ref)
  : (uncurry Fn <$> fn)
  : (Struct <$> struct)
  : [Union <$> union]  

unionMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionMaybe f = curry $ \ case
  (Nothing, Nothing) -> Nothing
  (x@Just {}, Nothing) -> x
  (Nothing, y@Just {}) -> y
  (Just x, Just y) -> Just $ f x y

unionTuple :: (a -> a -> b) -> (a, a) -> (a, a) -> (b, b)
unionTuple f = \ (x, y) (x', y') -> (f x x', f y y')

instance Map HeadMap where
  type Key HeadMap = Head
  empty =
    HeadMap
    { void = False
    , ref = Nothing
    , fn = Nothing
    , struct = Nothing
    , union = Nothing
    }
  singleton = \ case
    Void -> empty { void = True }
    Ref x y -> empty { ref = Just (x, y) }
    Fn x y -> empty { fn = Just (x, y) }
    Struct xs -> empty { struct = Just xs }
    Union xs -> empty { union = Just xs }
  unionWith f = \ x y ->
    HeadMap
    { void = x.void || y.void
    , ref = unionMaybe (unionTuple f) x.ref y.ref
    , fn = unionMaybe (unionTuple f) x.fn y.fn
    , struct = unionMaybe (Ord.Map.intersectionWith f) x.struct y.struct
    , union = unionMaybe (Ord.Map.unionWith f) x.union y.union
    }
  intersectionWith f = \ x y ->
    HeadMap
    { void = x.void || y.void
    , ref = unionMaybe (unionTuple f) x.ref y.ref
    , fn = unionMaybe (unionTuple f) x.fn y.fn
    , struct = unionMaybe (Ord.Map.unionWith f) x.struct y.struct
    , union = unionMaybe (Ord.Map.intersectionWith f) x.union y.union
    }
