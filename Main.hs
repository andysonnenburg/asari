{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Main (main) where

import Control.Category (Category, (<<<))
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Coerce
import Data.Foldable
import Data.Functor
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Map.Lazy (Map, findWithDefault)
import Data.Map.Lazy qualified as Map
import Data.Ord
import Data.STRef
import Data.Set (Set, isSubsetOf)
import Data.Set qualified as Set
import Data.Word
import GHC.Records (getField)

class Monad m => MonadRef r m | m -> r where
  newRef :: a -> m (r a)
  readRef :: r a -> m a
  writeRef :: r a -> a -> m ()

instance MonadRef IORef IO where
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

instance MonadRef (STRef s) (ST s) where
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef

instance MonadRef r m => MonadRef r (ReaderT r' m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef x = lift . writeRef x

instance MonadRef r m => MonadRef r (StateT s m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef x = lift . writeRef x

instance (Monoid w, MonadRef r m) => MonadRef r (WriterT w m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef x = lift . writeRef x

class Monad m => MonadSupply s m | m -> s where
  supply :: m s

newtype SupplyT s m a = SupplyT (StateT s m a) deriving (Functor, Applicative, Monad)

instance (Num s, Monad m) => MonadSupply s (SupplyT s m) where
  supply = SupplyT $ state $ \ x -> (x, x + 1)

instance MonadSupply s m => MonadSupply s (ReaderT r m) where
  supply = lift supply

instance MonadSupply s m => MonadSupply s (StateT s' m) where
  supply = lift supply

instance (Monoid w, MonadSupply s m) => MonadSupply s (WriterT w m) where
  supply = lift supply

traverseSet :: (Applicative f, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverseSet f =
  foldl' (\ m x -> Set.insert <$> f x <*> m) (pure Set.empty)

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

memo :: (Ord a, MonadFix m, MonadState (Map a b) m) => (a -> m b) -> a -> m b
memo f x = get <&> Map.lookup x >>= \ case
  Just x' -> pure x'
  Nothing -> mdo
    modify (Map.insert x x')
    x' <- f x
    pure x'

visit :: ( Ord a
         , MonadFix m
         ) => ((a -> StateT (Map a b) m b) -> a -> StateT (Map a b) m b) -> a -> m b
visit f = runMemoT $$$ fix $ memo . f

data Cons
  = Ref
  | Fn deriving (Show, Eq, Ord)

data Symbol
  = Write
  | Read
  | Domain
  | Range deriving (Show, Eq, Ord)

contra :: Symbol -> Bool
contra = \ case
  Write -> True
  Read -> False
  Domain -> True
  Range -> False

type Label = Word

data NFA r = NFA
  { label :: Label
  , cons :: r (Set Cons)
  , trans :: r (Map Symbol (Set (NFA r)))
  , epsilonTrans :: r (Set (NFA r))
  , flow :: r (Set (NFA r))
  }

instance Eq (NFA r) where
  x == y = getField @"label" x == getField @"label" y

instance Ord (NFA r) where
  compare = comparing $ getField @"label"

data DFA = DFA
  { label :: Label
  , cons :: Set Cons
  , trans :: Map Symbol DFA
  , flow :: Set DFA
  }

instance Eq DFA where
  x == y = getField @"label" x == getField @"label" y

instance Ord DFA where
  compare = comparing $ getField @"label"

epsilonClosure' :: MonadRef r m => Set (NFA r) -> NFA r -> m (Set (NFA r))
epsilonClosure' = fix $ \ recur xs x ->
  if Set.member x xs
  then pure xs
  else foldlM recur (Set.insert x xs) =<< readRef x.epsilonTrans

epsilonClosure :: MonadRef r m => NFA r -> m (Set (NFA r))
epsilonClosure = epsilonClosure' mempty
  
transClosure :: (MonadFix m, MonadRef r m) => NFA r -> m (Map Symbol (Set (NFA r)))
transClosure x = traverse (foldlM epsilonClosure' mempty) =<< readRef x.trans

foldMapM :: (Foldable f, Monad m, Monoid b) => (a -> m b) -> f a -> m b
foldMapM f = foldrM (\ x z -> mappend z <$> f x) mempty

type FixT s m = ReaderT s (WriterT s m)

evalFixT :: MonadFix m => FixT s m a -> m a
evalFixT m = fmap fst $ mfix $ \ (_, s) -> runWriterT $ runReaderT m s

newtype SemiMap k v = SemiMap { getSemiMap :: Map k v }

instance (Ord k, Semigroup v) => Semigroup (SemiMap k v) where
  x <> y = SemiMap $ Map.unionWith (<>) (coerce x) (coerce y)

instance (Ord k, Semigroup v) => Monoid (SemiMap k v) where
  mempty = SemiMap mempty

type FreezeT r m = FixT (SemiMap (NFA r) (Set DFA)) (StateT (Map (Set (NFA r)) DFA) m)

evalFreezeT :: MonadFix m => FreezeT r m a -> m a
evalFreezeT m = evalStateT (evalFixT m) mempty

toDFA' :: ( MonadFix m
          , MonadRef r m
          , MonadSupply Label m
          ) => Set (NFA r) -> FreezeT r m DFA
toDFA' = fix $ \ recur xs -> gets (Map.lookup xs) >>= \ case
  Just xs -> pure xs
  Nothing -> mfix $ \ y -> do
    modify $ Map.insert xs y
    tell $ coerce $ Map.fromSet (const (Set.singleton y)) xs
    env <- coerce <$> ask
    DFA <$>
      supply <*>
      foldMapM (readRef . (.cons)) xs <*>
      (traverse recur . coerce <=< foldMapM (fmap SemiMap . transClosure)) xs <*>
      foldMapM (fmap (foldMap (flip (findWithDefault mempty) env)) . readRef . (.flow)) xs

toDFA :: ( MonadFix m
         , MonadRef r m
         , MonadSupply Label m
         ) => NFA r -> FreezeT r m DFA
toDFA = toDFA' <=< epsilonClosure

type Type = (Map Name DFA, DFA)

type MType r = (Map Name (NFA r), NFA r)

type Name = Word

freeze :: (MonadFix m, MonadRef r m, MonadSupply Label m) => MType r -> m Type
freeze (env, t) = evalFreezeT $ (,) <$> traverse toDFA env <*> toDFA t

type ThawT r = StateT (Map DFA (NFA r))

evalThawT :: Monad m => ThawT r m a -> m a
evalThawT m = evalStateT m mempty

toNFA :: ( MonadFix m
         , MonadRef r m
         , MonadSupply Label m
         ) => DFA -> ThawT r m (NFA r)
toNFA = fix $ \ recur x -> gets (Map.lookup x) >>= \ case
  Just x -> pure x
  Nothing -> mfix $ \ y -> do
    modify $ Map.insert x y
    NFA <$>
      supply <*>
      newRef x.cons <*>
      (newRef =<< traverse (fmap Set.singleton . toNFA) x.trans) <*>
      newRef mempty <*>
      (newRef =<< foldMapM (fmap Set.singleton . toNFA) x.flow)

thaw :: (MonadFix m, MonadRef r m, MonadSupply Label m) => Type -> m (MType r)
thaw (env, t) = evalThawT $ (,) <$> traverse toNFA env <*> toNFA t

minimize :: MonadSupply Label m => DFA -> m DFA
minimize x = undefined

merge :: MonadRef r m => NFA r -> NFA r -> m ()
merge x y = do
  writeRef x.cons =<< Set.union <$> readRef x.cons <*> readRef y.cons
  writeRef x.trans =<< Map.unionWith (<>) <$> readRef y.trans <*> readRef x.trans
  writeRef x.flow =<< (<>) <$> readRef x.flow <*> readRef y.flow

unify :: (MonadError () m, MonadFix m, MonadRef r m) => NFA r -> NFA r -> m ()
unify = curry $ visit_ $ \ recur (x, y) -> do
  x_cons <- readRef x.cons
  y_cons <- readRef y.cons
  when (not (y_cons `isSubsetOf` x_cons)) $
    throwError ()
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
  | Let Name Exp Exp deriving Show

type Env = Map Name Type

newNFA :: ( MonadRef r m
          , MonadSupply Label m
          ) => Set Cons -> Map Symbol (Set (NFA r)) -> Set (NFA r) -> Set (NFA r) -> m (NFA r)
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

union :: MonadRef r m => Map Name (NFA r) -> Map Name (NFA r) -> m (Map Name (NFA r))
union = undefined

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

main :: IO ()
main = pure ()
