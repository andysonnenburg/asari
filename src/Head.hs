{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Head
  ( Head (..)
  , HeadMap
  ) where

import Control.Applicative (liftA2)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable qualified as Bitraversable
import Data.Functor qualified as Functor
import Data.Maybe (catMaybes, isJust)

import Map.Lazy qualified as Ord (Map)
import Map.Lazy qualified as Ord.Map
import Name
import State

data Head a
  = Void
  | Ref a a
  | Fn a a
  | Struct (Ord.Map Name a)
  | Union (Ord.Map Name (Maybe a)) (Maybe a)
  deriving (Functor, Foldable, Traversable, Show)

data HeadMap a
  = HeadMap
    { void :: Bool
    , ref :: Maybe (a, a)
    , fn :: Maybe (a, a)
    , struct :: Maybe (Ord.Map Name a)
    , union :: Maybe (Ord.Map Name (Maybe a), Maybe a)
    }

fmapTuple :: (a -> b) -> (a, a) -> (b, b)
fmapTuple f = \ (x, y) -> (f x, f y)

instance Functor HeadMap where
  fmap f HeadMap {..} =
    HeadMap
    { ref = fmap (fmapTuple f) ref
    , fn = fmap (fmapTuple f) fn
    , struct = fmap (fmap f) struct
    , union = fmap (bimap (fmap (fmap f)) (fmap f)) union
    , ..
    }

foldMapTuple :: Semigroup m => (a -> m) -> (a, a) -> m
foldMapTuple f = \ (x, y) -> f x <> f y

instance Foldable HeadMap where
  foldMap f HeadMap {..} =
    foldMap (foldMapTuple f) ref <>
    foldMap (foldMapTuple f) fn <>
    foldMap (foldMap f) struct <>
    foldMap (bifoldMap (foldMap (foldMap f)) (foldMap f)) union

traverseTuple :: Applicative f => (a -> f b) -> (a, a) -> f (b, b)
traverseTuple f = \ (x, y) -> (,) <$> f x <*> f y

instance Traversable HeadMap where
  traverse f HeadMap {..} =
    HeadMap void <$>
    traverse (traverseTuple f) ref <*>
    traverse (traverseTuple f) fn <*>
    traverse (traverse f) struct <*>
    traverse (Bitraversable.bitraverse (traverse (traverse f)) (traverse f)) union

instance Show a => Show (HeadMap a) where
  showsPrec prec = showParen (prec > 10) . (showString "fromList " .) . shows . toList

toList :: HeadMap a -> [Head a]
toList HeadMap {..} = catMaybes
  $ (if void then Just Void else Nothing)
  : (uncurry Ref <$> ref)
  : (uncurry Fn <$> fn)
  : (Struct <$> struct)
  : [uncurry Union <$> union]

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
    Union xs x -> empty { union = Just (xs, x) }
  bitraverse f g HeadMap {..} =
    HeadMap void <$>
    traverse (Bitraversable.bitraverse f g) ref <*>
    traverse (Bitraversable.bitraverse f g) fn <*>
    traverse (traverse g) struct <*>
    traverse (Bitraversable.bitraverse (traverse (traverse g)) (traverse g)) union
  bizipWithM_ f g x y =
    (case (x.ref, y.ref) of
       (Just (x, x'), Just (y, y')) -> Functor.void $ f x y *> g x' y'
       _ -> pure ()) *>
    (case (x.fn, y.fn) of
       (Just (x, x'), Just (y, y')) -> Functor.void $ f x y *> g x' y'
       _ -> pure ()) *>
    (case (x.struct, y.struct) of
       (Just x, Just y) -> Ord.Map.zipWithM_ g x y
       _ -> pure ()) *>
    (case (x.union, y.union) of
       (Just x, Just y) -> unionBizipWithM_ g x y
       _ -> pure ())
  unionWith f = \ x y ->
    HeadMap
    { void = x.void || y.void
    , ref = unionMaybe (unionTuple f) x.ref y.ref
    , fn = unionMaybe (unionTuple f) x.fn y.fn
    , struct = unionMaybe (Ord.Map.intersectionWith f) x.struct y.struct
    , union = unionMaybe (unionUnionWith f) x.union y.union
    }
  intersectionWith f = \ x y ->
    HeadMap
    { void = x.void || y.void
    , ref = unionMaybe (unionTuple f) x.ref y.ref
    , fn = unionMaybe (unionTuple f) x.fn y.fn
    , struct = unionMaybe (Ord.Map.unionWith f) x.struct y.struct
    , union = unionMaybe (unionIntersectionWith f) x.union y.union
    }
  x `isSubmapOf` y =
    flip allKeys x $ \ x ->
    flip allKeys y $ \ y ->
    case (x, y) of
      (Void, Void) -> True
      (Ref {}, Ref {}) -> True
      (Fn {}, Fn {}) -> True
      (Struct x, Struct y) -> Ord.Map.isSubmapOfBy (\ _ _ -> True) x y
      (Union xs x, Union ys y) -> unionIsSubmapOf xs x ys y
      _ -> False

unionBizipWithM_ f (xs, x) (ys, y) =
  case (x, y) of
    (Nothing, Nothing) ->
      Ord.Map.zipWithM_ ((sequenceA .) . liftA2 f) xs ys
    (Nothing, Just y) ->
      Ord.Map.zipWithM_'
      (maybe (pure ()) (Functor.void . flip f y))
      (const (pure ()))
      ((sequenceA .) . liftA2 f)
      xs ys
    (Just x, Nothing) ->
      Ord.Map.zipWithM_'
      (const (pure ()))
      (maybe (pure ()) (Functor.void . f x))
      ((sequenceA .) . liftA2 f)
      xs ys
    (Just x, Just y) ->
      Ord.Map.zipWithM_'
      (maybe (pure ()) (Functor.void . flip f y))
      (maybe (pure ()) (Functor.void . f x))
      ((sequenceA .) . liftA2 f)
      xs ys

unionUnionWith :: Ord k =>
                  (a -> a -> a) ->
                  (Ord.Map.Map k (Maybe a), Maybe a) ->
                  (Ord.Map.Map k (Maybe a), Maybe a) ->
                  (Ord.Map.Map k (Maybe a), Maybe a)
unionUnionWith f (xs, x) (ys, y) =
  case (x, y) of
    (Nothing, Nothing) ->
      (zs, Nothing)
      where
        zs = Ord.Map.unionWith
             (unionMaybe f)
             xs ys
    (Nothing, Just y) ->
      (zs, Just y)
      where
        zs = Ord.Map.unionWith'
             (Just . maybe y (flip f y))
             id
             (unionMaybe f)
             xs ys
    (Just x, Nothing) ->
      (zs, Just x)
      where
        zs = Ord.Map.unionWith'
             id
             (Just . maybe x (f x))
             (unionMaybe f)
             xs ys
    (Just x, Just y) ->
      (zs, Just (f x y))
      where
        zs = Ord.Map.unionWith'
             (Just . maybe y (flip f y))
             (Just . maybe x (f x))
             (unionMaybe f)
             xs ys

unionIntersectionWith f (xs, x) (ys, y) =
  case (x, y) of
    (Nothing, Nothing) ->
      (zs, Nothing)
      where
        zs = Ord.Map.intersectionWith
             (liftA2 f)
             xs ys
    (Nothing, Just y) ->
      (zs, Nothing)
      where
        zs = Ord.Map.differenceWith'
             (fmap (flip f y))
             (liftA2 f)
             xs ys
    (Just x, Nothing) ->
      (zs, Nothing)
      where
        zs = Ord.Map.differenceWith'
             (fmap (f x))
             (liftA2 (flip f))
             ys xs
    (Just x, Just y) ->
      (zs, Just (f x y))
      where
        zs = Ord.Map.unionWith'
             (Just . maybe y (flip f y))
             (Just . maybe x (f x))
             (liftA2 f)
             xs ys

unionIsSubmapOf :: Ord k =>
                   Ord.Map.Map k (Maybe a) ->
                   Maybe a ->
                   Ord.Map.Map k (Maybe a) ->
                   Maybe a ->
                   Bool
unionIsSubmapOf xs x ys y = case (x, y) of
  (Nothing, Nothing) ->
    Ord.Map.isSubmapOfBy'
    (maybe True (const False))
    (const True)
    (maybe (const True) (const isJust))
    xs ys
  (Nothing, Just _) ->
    Ord.Map.isSubmapOfBy'
    (const True)
    (const True)
    (maybe (const True) (const isJust))
    xs ys
  (Just _, Nothing) ->
    False
  (Just _, Just _) ->
    Ord.Map.isSubmapOfBy'
    (const True)
    (maybe False (const True))
    (maybe (const True) (const isJust))
    xs ys

allKeys :: (Head a -> Bool) -> HeadMap a -> Bool
allKeys f HeadMap {..} =
  (if void then f Void else True) &&
  all (f . uncurry Ref) ref &&
  all (f . uncurry Fn) fn &&
  all (f . Struct) struct &&
  all (f . uncurry Union) union
