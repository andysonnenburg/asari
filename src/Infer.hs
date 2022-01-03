{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Infer
  ( infer
  , infer'
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST
import Data.Coerce
import Data.Foldable
import Data.Map.Merge.Lazy qualified as Map

import Error
import Exp as Exp
import FA
import Head as Head
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

infer :: ( MonadError Error m
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
    Just t_x ->
      (Map.delete x env,) <$> freshFn t_x t_e
    Nothing -> do
      t_x <- fresh State.empty
      (env,) <$> freshFn t_x t_e
  App e1 e2 -> do
    (env_e1, t_e1) <- infer e1
    (env_e2, t_e2) <- infer e2
    t <- fresh State.empty
    unify t_e1 =<< freshFn t_e2 t
    (, t) <$> union env_e1 env_e2
  Let x e1 e2 -> do
    t_e1 <- freeze =<< infer e1
    local (Map.insert x t_e1) $ infer e2
  Exp.Struct xs -> do
    (env, xs) <- forAccumLM mempty xs $ \ env (i, e) -> do
      (env_e, t_e) <- infer e
      (, (i, Set.singleton t_e)) <$> union env env_e
    (env,) <$> fresh (State.singleton (Head.Struct (Map.fromList xs)))
  Field e i -> mdo
    (env_e, t_e) <- infer e
    t_neg <- newNFA State.empty mempty (Set.singleton t_pos)
    t_pos <- newNFA State.empty mempty (Set.singleton t_neg)
    unify t_e =<< freshStruct i t_neg
    pure (env_e, t_pos)
  Switch e@(Var i) x xs -> do
    (env_e, t_e_pos) <- infer e
    (env_x, t_x, t_x_i) <- uncurry inferCase x
    let z = (env_x, t_x, [(i, t_x_i)])
    (env_xs, t_xs, t_xs_i) <- rotate foldlM z xs $ \ (env_xs, t_xs, z) (i, e) -> do
      (env_e, t_e, t_e_i) <- inferCase i e
      (,, (i, t_e_i):z) <$> union env_xs env_e <*> append t_xs t_e
    t_e_neg <- fresh (State.singleton (Union (Map.fromList  t_xs_i)))
    unify t_e_pos t_e_neg
    (, t_xs) <$> union env_e env_xs
  Case i e -> do
    (env_e, t_e) <- infer e
    (env_e,) <$> freshUnion i t_e

inferCase :: ( MonadError Error m
             , MonadFix m
             , MonadReader (Map Name Type) m
             , MonadRef r m
             , MonadSupply Label m
             ) => Name -> Exp -> m (Map Name (NFA r HeadMap), NFA r HeadMap, Set (NFA r HeadMap))
inferCase i e = do
  (env_e, t_e) <- infer e
  case Map.lookup i env_e of
    Just t_i_neg -> mdo
      t_neg <- newNFA State.empty mempty (Set.singleton t_pos)
      t_pos <- newNFA State.empty mempty (Set.singleton t_neg)
      t_i_pos <- freshUnion i t_pos
      unify t_i_pos t_i_neg
      pure (Map.delete i env_e, t_e, Set.singleton t_neg)
    Nothing -> do
      t_neg <- fresh State.empty
      pure (env_e, t_e, Set.singleton t_neg)

newtype StateL s f a = StateL { runStateL :: s -> f (s, a) }

instance Functor f => Functor (StateL s f) where
  fmap f m = StateL $ fmap (fmap f) . coerce m

instance Monad f => Applicative (StateL s f) where
  pure x = StateL $ pure . (, x)
  f <*> x = StateL $ \ s -> runStateL f s >>= \ (s, f) -> fmap f <$> runStateL x s

forAccumLM :: forall t f a b c .
              ( Traversable t
              , Monad f
              ) => b -> t a -> (b -> a -> f (b, c)) -> f (b, t c)
forAccumLM s t f = coerce (traverse @t @(StateL b f) @a @c) (flip f) t s

infer' :: Exp -> Either Error Type
infer' e = runST (runSupplyT (runReaderT (runExceptT (infer e >>= freeze)) mempty))

union :: ( State.Map t
         , MonadRef r m
         , MonadSupply Label m
         ) => Map Name (NFA r t) -> Map Name (NFA r t) -> m (Map Name (NFA r t))
union =
  Map.mergeA Map.preserveMissing Map.preserveMissing $
  Map.zipWithAMatched $ const append

append :: ( State.Map t
          , MonadRef r m
          , MonadSupply Label m
          ) => NFA r t -> NFA r t -> m (NFA r t)
append x y = newNFA State.empty (Set.fromList [x, y]) mempty

fresh :: ( State.Map t
         , MonadRef r m
         , MonadSupply Label m
         ) => t (Set (NFA r t)) -> m (NFA r t)
fresh x = newNFA x mempty mempty

freshFn :: ( MonadRef r m
           , MonadSupply Label m
           ) => NFA r HeadMap -> NFA r HeadMap -> m (NFA r HeadMap)
freshFn x y =
  fresh (State.singleton (Fn (Set.singleton x) (Set.singleton y)))

freshStruct :: ( MonadRef r m
               , MonadSupply Label m
               ) => Name -> NFA r HeadMap -> m (NFA r HeadMap)
freshStruct i x =
  fresh (State.singleton (Head.Struct (Map.singleton i (Set.singleton x))))

freshUnion :: ( MonadRef r m
              , MonadSupply Label m
              ) => Name -> NFA r HeadMap -> m (NFA r HeadMap)
freshUnion i x =
  fresh (State.singleton (Union (Map.singleton i (Set.singleton x))))

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

rotate :: (a -> b -> c -> d) -> b -> c -> a -> d
rotate f b c a = f a b c
