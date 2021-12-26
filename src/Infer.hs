{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module Infer
  ( infer
  ) where

import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.Map.Merge.Lazy qualified as Map

import Exp
import FA
import Head
import Map.Lazy (Map)
import Map.Lazy qualified as Map
import Name
import Ref
import Set (Set)
import Set qualified
import State qualified
import Supply
import Type
import Unify

infer :: ( MonadError () m
         , MonadFix m
         , MonadReader (Map Name Type) m
         , MonadRef r m
         , MonadSupply Label m
         ) => Exp -> m (MType r)
infer = \ case
  Var x -> Map.lookup x <$> ask >>= \ case
    Just t -> thaw t
    Nothing -> mdo
      t_neg <- newNFA State.empty mempty (Set.singleton t_pos)
      t_pos <- newNFA State.empty mempty (Set.singleton t_neg)
      pure (Map.singleton x t_neg, t_pos)
  Abs x e -> infer e >>= \ (env, t_e) -> case Map.lookup x env of
    Just t_x -> do
      let env = Map.delete x env
      (env,) <$> freshFn t_x t_e
    Nothing -> do
      t_x <- fresh
      (env,) <$> freshFn t_x t_e
  App e1 e2 -> do
    (env_e1, t_e1) <- infer e1
    (env_e2, t_e2) <- infer e2
    t <- fresh
    unify t_e1 =<< freshFn t_e2 t
    (,) <$> union env_e1 env_e2 <*> pure t
  Let x e1 e2 -> do
    t_e1 <- freeze =<< infer e1
    local (Map.insert x t_e1) $ infer e2
  Select e i -> mdo
    (env_e, t_e) <- infer e
    t_neg <- newNFA State.empty mempty (Set.singleton t_pos)
    t_pos <- newNFA State.empty mempty (Set.singleton t_neg)
    unify t_e =<< freshStruct i t_neg
    pure (env_e, t_pos)
  Label i e -> do
    (env_e, t_e) <- infer e
    (env_e,) <$> freshUnion i t_e

union :: ( State.Map t
         , MonadRef r m
         , MonadSupply Label m
         ) => Map Name (NFA r t) -> Map Name (NFA r t) -> m (Map Name (NFA r t))
union =
  Map.mergeA Map.preserveMissing Map.preserveMissing $ Map.zipWithAMatched $ \ _ x y ->
  newNFA State.empty (Set.fromList [x, y]) mempty

fresh :: ( State.Map t
         , MonadRef r m
         , MonadSupply Label m
         ) => m (NFA r t)
fresh = newNFA State.empty mempty mempty

freshFn :: ( MonadRef r m
           , MonadSupply Label m
           ) => NFA r HeadMap -> NFA r HeadMap -> m (NFA r HeadMap)
freshFn x y =
  newNFA
  (State.singleton (Fn (Set.singleton x) (Set.singleton y)))
  mempty
  mempty

freshStruct :: ( MonadRef r m
               , MonadSupply Label m
               ) => Name -> NFA r HeadMap -> m (NFA r HeadMap)
freshStruct i x =
  newNFA
  (State.singleton (Struct (Map.singleton i (Set.singleton x))))
  mempty
  mempty

freshUnion :: ( MonadRef r m
              , MonadSupply Label m
              ) => Name -> NFA r HeadMap -> m (NFA r HeadMap)
freshUnion i x =
  newNFA
  (State.singleton (Union (Map.singleton i (Set.singleton x))))
  mempty
  mempty

freeze :: ( MonadFix m
          , MonadRef r m
          ) => MType r -> m Type
freeze (env, t) = evalFreezeT $ (,) <$> traverse fromNegNFA env <*> fromPosNFA t

thaw :: ( MonadFix m
        , MonadSupply Label m
        , MonadRef r m
        ) => Type -> m (MType r)
thaw (env, t) = evalThawT $ (,) <$> traverse fromDFA env <*> fromDFA t

newNFA :: ( MonadRef r m
          , MonadSupply Label m
          ) => t (Set (NFA r t)) -> Set (NFA r t) -> Set (NFA r t) -> m (NFA r t)
newNFA trans epsilonTrans flow =
  NFA <$> supply <*> newRef trans <*> newRef epsilonTrans <*> newRef flow
