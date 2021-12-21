{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (main) where

import Control.Applicative
import Control.Category (Category, (<<<))
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Coerce
import Data.Foldable
import Data.Functor
import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as IntMap
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Map.Lazy (Map, findWithDefault)
import Data.Map.Lazy qualified as Map
import Data.Map.Merge.Lazy qualified as Map
import Data.Ord
import Data.STRef
import Data.Word
import GHC.Exts (dataToTag#)
import Prettyprinter

import FA
import Ref
import Supply
import Ref
import Set (Set, isSubsetOf)
import Set qualified as Set
import Shallow

forWithKey_ :: Applicative f => Map a b -> (a -> b -> f c) -> f ()
forWithKey_ xs f =
  Map.foldlWithKey' (\ m k x -> m *> f k x $> ()) (pure ()) xs

infixr 0 $$$

($$$) :: Category f => f b c -> f a b  -> f a c
($$$) = (<<<)

runMemoT :: (Monoid s, Monad m) => StateT s m a -> m a
runMemoT = flip evalStateT mempty

memo_ :: (Ord a, MonadState (Set a) m) => (a -> m b) -> a -> m ()
memo_ f x = get <&> Set.member x >>= \ case
  True -> pure ()
  False -> modify (Set.insert x) <* f x

visit_ :: ( Ord a
          , Monad m
          ) => ((a -> StateT (Set a) m ()) -> a -> StateT (Set a) m b) -> a -> m ()
visit_ f = runMemoT $$$ fix $ memo_ . f

data Trans a
  = Void
  | Ref a a
  | Fn a a deriving (Show, Functor, Foldable, Traversable)

type Type = (Map Name (DFA Trans), DFA Trans)

type MType r = (Map Name (NFA r f), NFA r f)

type Name = Word

freeze :: (MonadFix m, MonadRef r m) => MType r -> m Type
freeze (env, t) = evalFreezeT $ (,) <$> traverse toDFA env <*> toDFA t

thaw :: (MonadFix m, MonadRef r m, MonadSupply Label m) => Type -> m (MType r)
thaw (env, t) = evalThawT $ (,) <$> traverse toNFA env <*> toNFA t

minimize :: MonadSupply Label m => DFA -> m DFA
minimize x = undefined

merge :: MonadRef r m => NFA r -> NFA r -> m ()
merge x y = do
  writeRef x.trans =<< Map.unionWith (<>) <$> readRef y.trans <*> readRef x.trans
  writeRef x.flow =<< (<>) <$> readRef x.flow <*> readRef y.flow

unify :: (MonadError () m, MonadFix m, MonadRef r m) => NFA r -> NFA r -> m ()
unify = curry $ visit_ $ \ recur (x, y) -> do
  y_flow <- readRef y.flow
  for_ y_flow $ \ x' ->
    merge x' x
  x_flow <- readRef x.flow
  for_ x_flow $ \ y' ->
    merge y' y
  x_trans <- readRef x.trans
  y_trans <- readRef y.trans
  forWithKey_ y_trans $ \ f y' -> do
    case Map.lookup f x_trans of
      Nothing -> throwError ()
      Just x' ->
        sequence $
        if contra f
        then curry recur <$> toList y' <*> toList x'
        else curry recur <$> toList x' <*> toList y'

data Exp
  = Var Name
  | Abs Name Exp
  | App Exp Exp
  | Let Name Exp Exp
  | Unit deriving Show

type Env = Map Name Type

newNFA :: ( MonadRef r m
          , MonadSupply Label m
          ) => TransSet (Set (NFA r)) -> Set (NFA r) -> Set (NFA r) -> m (NFA r)
newNFA cons trans epsilonTrans flow =
  NFA <$>
  supply <*>
  newRef cons <*>
  newRef trans <*>
  newRef epsilonTrans <*>
  newRef flow

fresh :: ( MonadRef r m
         , MonadSupply Label m
         ) => m (NFA r)
fresh = newNFA mempty mempty mempty mempty

newFn :: ( MonadRef r m
           , MonadSupply Label m
           ) => NFA r -> NFA r -> m (NFA r)
newFn x y =
  newNFA
  (Set.singleton Fn)
  (Map.fromList [(Domain, Set.singleton x), (Range, Set.singleton y)])
  mempty
  mempty

union :: ( MonadRef r m
         , MonadSupply Label m
         ) => Map Name (NFA r) -> Map Name (NFA r) -> m (Map Name (NFA r))
union = Map.mergeA Map.preserveMissing Map.preserveMissing $ Map.zipWithAMatched $ \ _ x y ->
  newNFA mempty mempty (Set.fromList [x, y]) mempty

infer :: ( MonadError () m
         , MonadFix m
         , MonadRef r m
         , MonadSupply Label m
         ) => Env -> Exp -> m (MType r)
infer env = \ case
  Var x -> case Map.lookup x env of
    Just t -> thaw t
    Nothing -> mdo
      t_neg <- newNFA mempty mempty mempty (Set.singleton t_pos)
      t_pos <- newNFA mempty mempty mempty (Set.singleton t_neg)
      pure (Map.singleton x t_neg, t_pos)
  Abs x e -> do
    (env_e, t_e) <- infer env e
    case Map.lookup x env_e of
      Just t_x -> (Map.delete x env_e,) <$> newFn t_x t_e
      Nothing -> (env_e,) <$> join (newFn <$> fresh <*> pure t_e)
  App e1 e2 -> do
    (env_e1, t_e1) <- infer env e1
    (env_e2, t_e2) <- infer env e2
    t_a <- fresh
    unify t_e1 =<< newFn t_e2 t_a
    (,) <$> union env_e1 env_e2 <*> pure t_a
  Let x e1 e2 -> do
    t_e1 <- freeze =<< infer env e1
    infer (Map.insert x t_e1 env) e2
  Unit ->
    (mempty,) <$> newNFA (Set.singleton Void) mempty mempty mempty

infer' :: Exp -> Maybe Type
infer' env =
  either (const Nothing) Just $
  runST (runExceptT $ runSupplyT $ freeze =<< infer mempty env)

main :: IO ()
main = pure ()
