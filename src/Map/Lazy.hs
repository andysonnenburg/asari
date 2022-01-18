{-# LANGUAGE ImportQualifiedPost #-}
module Map.Lazy
  ( module Data.Map.Lazy
  , deleteAll
  , zipWithM_
  , zipWithM_'
  , unionWith'
  , differenceWith'
  , isSubmapOfBy'
  ) where

import Data.Coerce
import Data.Foldable qualified as Foldable
import Data.Functor
import Data.Map.Lazy
import Data.Map.Merge.Lazy

deleteAll :: (Foldable f, Ord k) => f k -> Map k a -> Map k a
deleteAll = flip (Foldable.foldl' (flip delete))

newtype ZipWithM_ f a = ZipWithM_ { getZipWithM_ :: f () }

instance Functor (ZipWithM_ f) where
  fmap _ = coerce

instance Applicative f => Applicative (ZipWithM_ f) where
  pure _ = ZipWithM_ $ pure ()
  f <*> x = ZipWithM_ $ getZipWithM_ f *> getZipWithM_ x

zipWithM_ :: ( Ord k
             , Applicative f
             ) => (a -> b -> f c) -> Map k a -> Map k b -> f ()
zipWithM_ f =
  (getZipWithM_ .) .
  mergeA
  dropMissing
  dropMissing
  (zipWithMaybeAMatched $ \ _ a b -> ZipWithM_ $ void $ f a b)

zipWithM_' :: ( Ord k
              , Applicative f
              ) => (a -> f c) -> (b -> f d) -> (a -> b -> f e) -> Map k a -> Map k b -> f ()
zipWithM_' f g h =
  (getZipWithM_ .) .
  mergeA
  (traverseMaybeMissing $ \ _ a -> ZipWithM_ $ void $ f a)
  (traverseMaybeMissing $ \ _ b -> ZipWithM_ $ void $ g b)
  (zipWithMaybeAMatched $ \ _ a b -> ZipWithM_ $ void $ h a b)


unionWith' :: Ord k =>
              (a -> c) ->
              (b -> c) ->
              (a -> b -> c) ->
              Map k a -> Map k b -> Map k c
unionWith' f g h =
  merge
  (mapMissing (const f))
  (mapMissing (const g))
  (zipWithMatched (const h))

differenceWith' :: Ord k =>
                   (a -> c) ->
                   (a -> b -> c) ->
                   Map k a -> Map k b -> Map k c
differenceWith' f g =
  merge
  (mapMissing (const f))
  dropMissing
  (zipWithMatched (const g))

newtype IsSubmapOfBy a = IsSubmapOfBy { getIsSubmapOfBy :: Bool }

instance Functor IsSubmapOfBy where
  fmap _ = coerce

instance Applicative IsSubmapOfBy where
  pure _ = IsSubmapOfBy True
  f <*> x = IsSubmapOfBy $ getIsSubmapOfBy f && getIsSubmapOfBy x

isSubmapOfBy' :: Ord k =>
                 (a -> Bool) ->
                 (b -> Bool) ->
                 (a -> b -> Bool) ->
                 Map k a -> Map k b -> Bool
isSubmapOfBy' f g h =
  (getIsSubmapOfBy .) .
  mergeA
  (traverseMaybeMissing $ \ _ a -> IsSubmapOfBy $ f a)
  (traverseMaybeMissing $ \ _ b -> IsSubmapOfBy $ g b)
  (zipWithMaybeAMatched $ \ _ a b -> IsSubmapOfBy $ h a b)
