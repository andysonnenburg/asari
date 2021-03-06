{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Infer
  ( infer
  , infer'
  ) where

import Control.Arrow ((***), first)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST
import Data.Coerce
import Data.Foldable
import Data.Functor ((<&>))
import Data.Functor qualified as Functor
import Data.Map.Merge.Lazy qualified as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup.Foldable
import Data.Traversable

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
import Unify

type Mono = DFA HeadMap

type Poly a = (Map a Mono, Mono)

type MMono r = NFA r HeadMap

type MPoly a r = (Map a (MMono r), MMono r)

data VarState a r
  = Abs (Map a (r (VarState a r))) [a] (Exp a)
  | Mono
  | Poly (Poly a)

infer :: ( Ord a
         , MonadError Error m
         , MonadFix m
         , MonadReader (Map a (r (VarState a r))) m
         , MonadRef r m
         , MonadSupply Label m
         ) => Exp a -> m (MPoly a r)
infer = \ case
  Var x -> inferVar x
  Lam xs e -> inferAbs xs e
  App e1 e2 -> do
    (env_e1, t_e1) <- infer e1
    (env_e2, t_e2) <- infer e2
    (t_neg, t_pos) <- freshVar
    unify' t_e1 =<< freshFn t_e2 t_neg
    (, t_pos) <$> unionEnv env_e1 env_e2
  Val x e1 e2 -> do
    t_e1 <- freeze =<< infer e1
    s <- newRef $ Poly t_e1
    local (Map.insert x s) $ infer e2
  Exp.Fn x xs e1 e2 -> mdo
    z0 <- asks (, [])
    (env, t_xs) <- rotateL foldlM z0 (y :| ys) $ \ z (x, xs, e1) -> do
      newRef (Abs env xs e1) <&> \ t_x -> Map.insert x t_x *** ((x, t_x):) $ z
    for_ t_xs $ uncurry inferVar_
    local (const env) $ infer e2'
    where
      y = (x, xs, e1)
      (ys, e2') = unfoldr' getFn e2
  Seq e1 e2 -> do
    (env_e1, _) <- infer e1
    (env_e2, t_e2) <- infer e2
    (, t_e2) <$> unionEnv env_e1 env_e2
  Block e -> do
    (env, t_e) <- infer e
    t_x <- freshVoid
    (env,) <$> freshFn t_x t_e
  Exp.Struct x xs -> do
    (env, xs) <- forAccumLM mempty xs $ \ env (i, e) -> do
      (env_e, t_e) <- infer e
      (, (i, Set.singleton t_e)) <$> unionEnv env env_e
    (env,) <$> freshStruct x xs
  Field e i -> do
    (env_e, t_e) <- infer e
    (t_neg, t_pos) <- freshVar
    unify' t_e =<< freshField i t_neg
    pure (env_e, t_pos)
  Switch e@(Var v) x xs y -> do
    (env_e, t_e_pos) <- infer e
    z0 <- do
      let (i, e) = x
      (env_e, t_e, t_i) <- inferVarCase v i e
      pure (env_e, t_e, [(i, Just (Set.singleton t_i))])
    (env_xs, t_xs, t_i) <- rotateL foldlM z0 xs $ \ (env_xs, t_xs, z) (i, e) -> do
      (env_e, t_e, t_i) <- inferVarCase v i e
      (,, (i, Just (Set.singleton t_i)):z) <$> unionEnv env_xs env_e <*> union t_xs t_e
    (env_y, t_y, t_def) <- funzip3 <$> traverse (inferVarDefault v (fst <$> t_i)) y
    t_e_neg <- freshUnion t_i t_def
    unify' t_e_pos t_e_neg
    (,) <$> unionEnv' (env_e :| env_xs :| env_y) <*> union' (t_xs :| t_y)
  Switch e x xs y -> do
    (env_e, t_e_pos) <- infer e
    z0 <- do
      let (i, e) = x
      (env_e, t_e) <- infer e
      t_i <- fresh State.empty
      pure (env_e, t_e, [(i, Just (Set.singleton t_i))])
    (env_xs, t_xs, t_i) <- rotateL foldlM z0 xs $ \ (env_xs, t_xs, z) (i, e) -> do
      (env_e, t_e) <- infer e
      t_i <- fresh State.empty
      (,, (i, Just (Set.singleton t_i)):z) <$> unionEnv env_xs env_e <*> union t_xs t_e
    (env_y, t_y) <- funzip <$> traverse infer y
    t_e_neg <- freshUnion t_i . Just =<< fresh State.empty
    unify' t_e_pos t_e_neg
    (,) <$> unionEnv' (env_e :| env_xs :| env_y) <*> union' (t_xs :| t_y)
  Enum i -> (mempty,) <$> (freshCase i =<< fresh (State.singleton Head.Void))
  Exp.Void -> (mempty,) <$> freshVoid

inferVar :: ( Ord a
            , MonadError Error m
            , MonadFix m
            , MonadReader (Map a (r (VarState a r))) m
            , MonadRef r m
            , MonadSupply Label m
            ) => a -> m (MPoly a r)
inferVar x = lookupVar x >>= \ case
  Nothing -> inferMonoVar x
  Just t -> thaw t

lookupVar :: ( Ord a
             , MonadError Error m
             , MonadFix m
             , MonadReader (Map a (r (VarState a r))) m
             , MonadRef r m
             , MonadSupply Label m
             ) => a -> m (Maybe (Poly a))
lookupVar x = asks (Map.lookup x) >>= \ case
  Nothing -> pure Nothing
  Just s -> readRef s >>= \ case
    Abs env xs e -> do
      writeRef s Mono
      t@(env, t_pos) <- local (const env) $ inferAbs xs e
      t <- rotateR maybe (Map.lookup x env) (freeze t) $ \ t_neg -> do
        unify' t_pos t_neg
        freeze (Map.delete x env, t_pos)
      writeRef s $ Poly t
      pure $ Just t
    Mono -> pure Nothing
    Poly t -> pure $ Just t

inferMonoVar :: ( MonadFix m
                , MonadRef r m
                , MonadSupply Label m
                ) => a -> m (MPoly a r)
inferMonoVar x = do
  (t_neg, t_pos) <- freshVar
  pure (Map.singleton x t_neg, t_pos)

inferVar_ :: ( Ord a
             , MonadError Error m
             , MonadFix m
             , MonadReader (Map a (r (VarState a r))) m
             , MonadRef r m
             , MonadSupply Label m
             ) => a -> r (VarState a r) -> m ()
inferVar_ x s = readRef s >>= \ case
  Abs env xs e -> do
    writeRef s Mono
    t@(t_env, t_pos) <- local (const env) $ inferAbs xs e
    t <- rotateR maybe (Map.lookup x t_env) (freeze t) $ \ t_neg -> do
      unify' t_pos t_neg
      freeze (Map.delete x t_env, t_pos)
    writeRef s $ Poly t
  Mono -> pure ()
  Poly _ -> pure ()

inferAbs :: ( Ord a
            , Foldable t
            , MonadError Error m
            , MonadFix m
            , MonadReader (Map a (r (VarState a r))) m
            , MonadRef r m
            , MonadSupply Label m
            ) => t a -> Exp a -> m (MPoly a r)
inferAbs xs e = do
  z <- local (Map.deleteAll xs) $ infer e
  rotateL foldrM z xs $ \ x (env, t) ->
    case Map.lookup x env of
      Just t_x ->
        (Map.delete x env,) <$> freshFn t_x t
      Nothing -> do
        t_x <- fresh State.empty
        (env,) <$> freshFn t_x t

inferVarCase :: ( Ord a
                , MonadError Error m
                , MonadFix m
                , MonadReader (Map a (r (VarState a r))) m
                , MonadRef r m
                , MonadSupply Label m
                ) => a -> Name -> Exp a -> m (Map a (MMono r), MMono r, MMono r)
inferVarCase x i e = lookupVar x >>= \ case
  Just t_x -> do
    (env_x, t_x_pos) <- thaw t_x
    (t_i_neg, t_i_pos) <- freshVar
    t_neg <- freshUnion [(i, Just (Set.singleton t_i_neg))] . Just =<< fresh State.empty
    unify' t_x_pos t_neg
    t_x_pos <- freshCase i t_i_pos
    s_x <- newRef . Poly =<< freeze (env_x, t_x_pos)
    (env_e, t_e_pos) <- local (Map.insert x s_x) $ infer e
    pure (env_e, t_e_pos, t_i_neg)
  Nothing -> do
    (env_e, t_e_pos) <- infer e
    case Map.lookup x env_e of
      Just t_x_neg -> do
        (t_i_neg, t_i_pos) <- freshVar
        t_x_pos <- freshCase i t_i_pos
        unify' t_x_pos t_x_neg
        pure (Map.delete x env_e, t_e_pos, t_i_neg)
      Nothing -> do
        t_i_neg <- fresh State.empty
        pure (env_e, t_e_pos, t_i_neg)

inferVarDefault :: ( Ord a
                   , MonadError Error m
                   , MonadFix m
                   , MonadReader (Map a (r (VarState a r))) m
                   , MonadRef r m
                   , MonadSupply Label m
                   ) => a -> [Name] -> Exp a -> m (Map a (MMono r), MMono r, MMono r)
inferVarDefault x i e = lookupVar x >>= \ case
  Just t_x -> do
    (env_x, t_x_pos) <- thaw t_x
    t_i <- for i $ \ i -> do
      t_i <- fresh State.empty
      pure (i, Just (Set.singleton t_i))
    (t_def_neg, t_def_pos) <- freshVar
    t_neg <- freshUnion t_i (Just t_def_neg)
    unify' t_x_pos t_neg
    t_x_pos <- freshDefault i t_def_pos
    s_x <- newRef . Poly =<< freeze (env_x, t_x_pos)
    (env_e, t_e_pos) <- local (Map.insert x s_x) $ infer e
    pure (env_e, t_e_pos, t_def_neg)
  Nothing -> do
    (env_e, t_e_pos) <- infer e
    case Map.lookup x env_e of
      Just t_x_neg -> do
        (t_def_neg, t_def_pos) <- freshVar
        t_x_pos <- freshDefault i t_def_pos
        unify' t_x_pos t_x_neg
        pure (Map.delete x env_e, t_e_pos, t_def_neg)
      Nothing -> do
        t_def_neg <- fresh State.empty
        pure (env_e, t_e_pos, t_def_neg)

newtype UnifyErrorT m a =
  UnifyErrorT { runUnifyErrorT :: m a
              } deriving (Functor, Applicative, Monad, MonadFix)

deriving instance MonadRef r m => MonadRef r (UnifyErrorT m)

instance MonadTrans UnifyErrorT where
  lift = UnifyErrorT

instance MonadError Error m => MonadUnifyError HeadMap (UnifyErrorT m) where
  throwUnifyError x y =
    lift $ throwError $ UnifyError (Functor.void x) (Functor.void y)

unify' :: ( MonadError Error m
          , MonadFix m
          , MonadRef r m
          ) => MMono r -> MMono r -> m ()
unify' x y = runUnifyErrorT (unify x y)

getFn :: Exp a -> Maybe ((a, [a], Exp a), Exp a)
getFn = \ case
  Exp.Fn x xs e1 e2 -> Just ((x, xs, e1), e2)
  _ -> Nothing

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

infer' :: Ord a => Exp a -> Either Error (Poly a)
infer' e = runST (runSupplyT (runReaderT (runExceptT (infer e >>= freeze)) mempty))

unionEnv' :: ( Ord a
             , State.Map s
             , Foldable t
             , MonadFix m
             , MonadRef r m
             , MonadSupply Label m
             ) => t (Map a (NFA r s)) -> m (Map a (NFA r s))
unionEnv' = foldlM unionEnv mempty

unionEnv :: ( Ord a
            , State.Map s
            , MonadFix m
            , MonadRef r m
            , MonadSupply Label m
            ) => Map a (NFA r s) -> Map a (NFA r s) -> m (Map a (NFA r s))
unionEnv =
  Map.mergeA Map.preserveMissing Map.preserveMissing $
  Map.zipWithAMatched $ const intersection

intersection :: ( State.Map s
                , MonadFix m
                , MonadRef r m
                , MonadSupply Label m
                ) => NFA r s -> NFA r s -> m (NFA r s)
intersection x_neg y_neg = do
  (z_neg, z_pos) <- freshVar
  mergeNeg z_neg x_neg
  readRef x_neg.flow >>= traverse_ (flip mergePos z_pos)
  mergeNeg z_neg y_neg
  readRef y_neg.flow >>= traverse_ (flip mergePos z_pos)
  pure z_neg

union' :: ( State.Map s
          , Foldable1 t
          , MonadFix m
          , MonadRef r m
          , MonadSupply Label m
          ) => t (NFA r s) -> m (NFA r s)
union' = foldlM1 union

union :: ( State.Map s
         , MonadFix m
         , MonadRef r m
         , MonadSupply Label m
         ) => NFA r s -> NFA r s -> m (NFA r s)
union x_pos y_pos = do
  (z_neg, z_pos) <- freshVar
  readRef x_pos.flow >>= traverse_ (flip mergeNeg z_neg)
  mergePos z_pos x_pos
  readRef y_pos.flow >>= traverse_ (flip mergeNeg z_neg)
  mergePos z_pos y_pos
  pure z_pos

fresh :: ( State.Map s
         , MonadRef r m
         , MonadSupply Label m
         ) => s (Set (NFA r s)) -> m (NFA r s)
fresh x = newNFA x mempty

freshFn :: ( MonadRef r m
           , MonadSupply Label m
           ) => MMono r -> MMono r -> m (MMono r)
freshFn x y =
  freshCase "fn" =<< fresh (State.singleton (Head.Fn (Set.singleton x) (Set.singleton y)))

freshStruct :: ( MonadRef r m
               , MonadSupply Label m
               ) => Maybe Name -> [(Name, Set (MMono r))] -> m (MMono r)
freshStruct x =
  freshCase (fromMaybe "struct" x) <=< fresh . State.singleton . Head.Struct . Map.fromList

freshField :: ( MonadRef r m
              , MonadSupply Label m
              ) => Name -> MMono r -> m (MMono r)
freshField i x =
  freshAll =<< fresh (State.singleton (Head.Struct (Map.singleton i (Set.singleton x))))

freshUnion :: ( MonadRef r m
              , MonadSupply Label m
              ) => [(Name, Maybe (Set (MMono r)))] -> Maybe (MMono r) -> m (MMono r)
freshUnion xs =
  fresh . State.singleton . Union (Map.fromList xs) . fmap Set.singleton

freshCase :: ( MonadRef r m
             , MonadSupply Label m
             ) => Name -> MMono r -> m (MMono r)
freshCase i =
  fresh . State.singleton . flip Union Nothing . Map.singleton i . Just . Set.singleton


freshDefault :: ( MonadRef r m
                , MonadSupply Label m
                ) => [Name] -> MMono r -> m (MMono r)
freshDefault i =
  fresh . State.singleton . Union (Map.fromList ((, Nothing) <$> i)) . Just . Set.singleton

freshAll :: ( MonadRef r m
            , MonadSupply Label m
            ) => MMono r -> m (MMono r)
freshAll =
  fresh . State.singleton . Union mempty . Just . Set.singleton

freshVoid :: ( MonadRef r m
             , MonadSupply Label m
             ) => m (MMono r)
freshVoid =
  freshCase "void" =<< fresh (State.singleton Head.Void)

freshVar :: ( State.Map s
            , MonadFix m
            , MonadRef r m
            , MonadSupply Label m
            ) => m (NFA r s, NFA r s)
freshVar = mdo
  t_neg <- newNFA State.empty (Set.singleton t_pos)
  t_pos <- newNFA State.empty (Set.singleton t_neg)
  pure (t_neg, t_pos)

freeze :: ( MonadFix m
          , MonadRef r m
          ) => MPoly a r -> m (Poly a)
freeze (env, t) = evalFreezeT $ (,) <$> traverse fromNegNFA env <*> fromPosNFA t

thaw :: ( MonadFix m
        , MonadSupply Label m
        , MonadRef r m
        ) => Poly a -> m (MPoly a r)
thaw (env, t) = evalThawT $ (,) <$> traverse fromDFA env <*> fromDFA t

newNFA :: ( MonadRef r m
          , MonadSupply Label m
          ) => s (Set (NFA r s)) -> Set (NFA r s) -> m (NFA r s)
newNFA trans flow =
  NFA <$> supply <*> newRef trans <*> newRef flow

unfoldr' :: (b -> Maybe (a, b)) -> b -> ([a], b)
unfoldr' f = fix $ \ recur x -> case f x of
  Nothing -> ([], x)
  Just (y, x) -> first (y:) (recur x)

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip x = (fst <$> x, snd <$> x)

funzip3 :: Functor f => f (a, b, c) -> (f a, f b, f c)
funzip3 x = ((\ (x, _, _) -> x) <$> x, (\ (_, x, _) -> x) <$> x, (\ (_, _, x) -> x) <$> x)

rotateL :: (a -> b -> c -> d) -> b -> c -> a -> d
rotateL f b c a = f a b c

rotateR :: (a -> b -> c -> d) -> c -> a -> b -> d
rotateR f c a b = f a b c

infixr 5 :|

data NonEmptyF f a = a :| f a deriving (Functor, Foldable, Traversable)

instance Foldable f => Foldable1 (NonEmptyF f) where
  foldMap1 f (x :| xs) = foldl' ((. f) . (<>)) (f x) xs
