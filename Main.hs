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
import Data.Map.Lazy (Map, (!?))
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
toDFA' = fix $ \ recur xs -> get <&> Map.lookup xs >>= \ case
  Just xs -> pure xs
  Nothing -> mfix $ \ y -> do
    modify $ Map.insert xs y
    tell $ coerce $ Map.fromSet (const (Set.singleton y)) xs
    env <- coerce <$> ask
    DFA <$>
      supply <*>
      foldMapM (readRef . (.cons)) xs <*>
      (traverse recur . coerce <=< foldMapM (fmap SemiMap . transClosure)) xs <*>
      foldMapM (fmap (foldMap (fromMaybe mempty . (env!?))) . readRef . (.flow)) xs

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

thaw :: (MonadFix m, MonadRef r m, MonadSupply Label m) => Type -> m (MType r)
thaw = undefined

{-
merge :: MonadRef r m => NFA r -> NFA r -> m ()
merge x y = do
  writeRef x.cons =<< Set.union <$> readRef x.cons <*> readRef y.cons
  x_level <- readRef x.level
  y_trans <- readRef y.trans
  traverse (traverse (writeLevels x_level)) y_trans
  writeRef x.trans =<< Map.unionWith (++) y_trans <$> readRef x.trans
  y_flow <- readRef y.flow
  traverseSet (writeLevels x_level) y_flow
  writeRef x.flow =<< Set.union y_flow <$> readRef x.flow

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
    let x' = x_trans!f
    sequence $
      if contra f
      then curry recur <$> y' <*> x'
      else curry recur <$> x' <*> y'

data Exp
  = Var Name
  | Abs Name Exp
  | App Exp Exp
  | Let Name Exp Exp deriving Show

type Env r = Map Name (Bool, Type r)

newType :: ( MonadReader Level m
           , MonadRef r m
           , MonadSupply Label m
           ) => Set Cons -> Trans r -> Flow r -> m (Type r)
newType cons trans flow =
  Type <$>
  supply <*>
  (newRef =<< ask) <*>
  newRef cons <*>
  newRef trans <*>
  newRef flow

fresh :: ( MonadReader Level m
         , MonadRef r m
         , MonadSupply Label m
         ) => m (Type r)
fresh = newType Set.empty Map.empty Set.empty

newTypeFn :: ( MonadReader Level m
             , MonadRef r m
             , MonadSupply Label m
             ) => Type r -> Type r -> m (Type r)
newTypeFn x y =
  newType
  (Set.singleton Fn)
  (Map.fromList [(Domain, [x]), (Range, [y])])
  Set.empty

genLevel :: Word
genLevel = maxBound

inst :: ( MonadFix m
        , MonadReader Level m
        , MonadRef r m
        , MonadSupply Label m
        ) => Type r -> m (Type r)
inst = visit $ \ recur x -> do
  x_level <- readRef x.level
  if x_level == genLevel
    then join $
         newType <$>
         readRef x.cons <*>
         (traverse (traverse recur) =<< readRef x.trans) <*>
         (traverseSet recur =<< readRef x.flow)
    else pure x

gen :: ( MonadFix m
       , MonadReader Level m
       , MonadRef r m
       , MonadSupply Label m
       ) => Type r -> m ()
gen = visit_ $ \ recur x -> do
  x_level <- readRef x.level
  y <- ask
  when (x_level > y) $ do
    writeRef x.level genLevel
    traverse_ (traverse_ recur) =<< readRef x.trans
    traverse_ recur =<< readRef x.flow

infer :: ( MonadError () m
         , MonadFix m
         , MonadReader Level m
         , MonadRef r m
         , MonadSupply Label m
         ) => Env r -> Exp -> m (Type r)
infer env = \ case
  Var x -> case Map.lookup x env of
    Nothing ->
      throwError ()
    Just (False, t) -> do
      t' <- newType Set.empty Map.empty (Set.singleton t)
      writeRef t.flow =<< Set.insert t' <$> readRef t.flow
      pure t'
    Just (True, t) ->
      inst t
  Abs x e -> do
    t_x <- fresh
    let env = Map.insert x (False, t_x) env
    t_e <- infer env e
    newTypeFn t_x t_e
  App e1 e2 -> do
    t_e1 <- infer env e1
    t_e2 <- infer env e2
    t_a <- fresh
    unify t_e1 =<< newTypeFn t_e2 t_a
    pure t_a
  Let x e1 e2 -> do
    t_e1 <- local succ (infer env e1)
    gen t_e1
    let env = Map.insert x (True, t_e1) env
    infer env e2
-}
main :: IO ()
main = pure ()
