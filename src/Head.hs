{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Head
  ( Head (..)
  , HeadMap
  ) where

import Control.Monad
import Data.Bitraversable qualified as Bitraversable
import Data.Functor qualified as Functor
import Data.Maybe (catMaybes)

import Map.Lazy qualified as Ord (Map)
import Map.Lazy qualified as Ord.Map
import Name
import State

data Head a
  = Void
  | Ref a a
  | Fn a a
  | Struct (Ord.Map Name a)
  | Union (Ord.Map Name a) deriving (Functor, Foldable, Traversable, Show)

data HeadMap a
  = HeadMap
    { void :: Bool
    , ref :: Maybe (a, a)
    , fn :: Maybe (a, a)
    , struct :: Maybe (Ord.Map Name a)
    , union :: Maybe (Ord.Map Name a)
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
  bitraverse f g HeadMap {..} =
    HeadMap void <$>
    traverse (Bitraversable.bitraverse f g) ref <*>
    traverse (Bitraversable.bitraverse f g) fn <*>
    traverse (traverse g) struct <*>
    traverse (traverse g) union
  bizipWithM_ f g x y =
    (case (x.ref, y.ref) of
       (Just (x, x'), Just (y, y')) -> Functor.void $ f x y *> f x' y'
       _ -> pure ()) *>
    (case (x.fn, y.fn) of
       (Just (x, x'), Just (y, y')) -> Functor.void $ f x y *> f x' y'
       _ -> pure ()) *>
    (case (x.struct, y.struct) of
       (Just x, Just y) -> Ord.Map.zipWithM_ f x y
       _ -> pure ()) *>
    (case (x.union, y.union) of
       (Just x, Just y) -> Ord.Map.zipWithM_ f x y
       _ -> pure ())    
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
  x `isSubmapOf` y =
    flip allKeys x $ \ x ->
    flip allKeys y $ \ y ->
    case (x, y) of
      (Void, Void) -> True
      (Ref {}, Ref {}) -> True
      (Fn {}, Fn {}) -> True
      (Struct x, Struct y) -> Ord.Map.isSubmapOfBy (\ _ _ -> True) x y
      (Union x, Union y) -> Ord.Map.isSubmapOfBy (\ _ _ -> True) y x
      _ -> False

allKeys :: (Head a -> Bool) -> HeadMap a -> Bool
allKeys f HeadMap {..} =
  (if void then True else f Void) &&
  all (f . uncurry Ref) ref &&
  all (f . uncurry Fn) fn &&
  all (f . Struct) struct &&
  all (f . Union) union
