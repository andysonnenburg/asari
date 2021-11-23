{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Main (main) where

import Control.Category (Category, (<<<))
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Strict

import Data.Foldable
import Data.Functor
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Ord
import Data.Set (Set, isSubsetOf)
import qualified Data.Set as Set
import Data.Word

class Monad m => MonadRef r m | m -> r where
  newRef :: a -> m (r a)
  readRef :: r a -> m a
  writeRef :: r a -> a -> m ()

instance MonadRef r m => MonadRef r (StateT s m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef x = lift . writeRef x

class Monad m => MonadSupply s m | m -> s where
  supply :: m s

instance MonadSupply s m => MonadSupply s (StateT s' m) where
  supply = lift supply

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

type Level = Word

type Trans r = Map Symbol [Type r]

type Flow r = Set (Type r)

data Type r = Type
  { label :: Label
  , level :: r Level
  , cons :: r (Set Cons)
  , trans :: r (Trans r)
  , flow :: r (Flow r)
  }

instance Eq (Type r) where
  x == y = label x == label y

instance Ord (Type r) where
  compare = comparing label

infixr 0 $$$

($$$) :: Category f => f b c -> f a b  -> f a c
($$$) = (<<<)

traverseSet :: (Applicative f, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverseSet f =
  foldl' (\ m x -> Set.insert <$> f x <*> m) (pure Set.empty)

runVisitT :: (Monoid s, Monad m) => StateT s m a -> m a
runVisitT = flip evalStateT mempty

visit_ :: (Ord a, MonadState (Set a) m) => a -> m () -> m ()
visit_ x m = get <&> Set.member x >>= \ case
  True -> pure ()
  False -> m *> modify (Set.insert x)

visit :: (Ord a, MonadState (Map a a) m) => a -> m a -> m a
visit x m = get <&> Map.lookup x >>= \ case
  Just x' -> pure x'
  Nothing -> m >>= \ x' -> modify (Map.insert x x') $> x'

writeLevels :: MonadRef r m => Level -> Type r -> m ()
writeLevels x = runVisitT $$$ fix $ \ rec y -> visit_ y $ do
  y_level <- readRef y.level
  when (y_level > x) $ do
    writeRef y.level x
    traverse_ (traverse_ rec) =<< readRef y.trans
    traverse_ rec =<< readRef y.flow

merge :: MonadRef r m => Type r -> Type r -> m ()
merge x y = do
  writeRef x.cons =<< Set.union <$> readRef x.cons <*> readRef y.cons
  x_level <- readRef x.level
  y_trans <- readRef y.trans
  traverse (traverse (writeLevels x_level)) y_trans
  writeRef x.trans =<< Map.unionWith (++) y_trans <$> readRef x.trans
  y_flow <- readRef y.flow
  traverseSet (writeLevels x_level) y_flow
  writeRef x.flow =<< Set.union y_flow <$> readRef x.flow

unify :: (MonadError () m, MonadRef r m) => Type r -> Type r -> m ()
unify = curry $ runVisitT $$$ fix $ \ rec (x, y) -> visit_ (x, y) $ do
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
  for_ (Map.assocs y_trans) $ \ (f, y') -> do
    let x' = x_trans!f
    sequence $
      if contra f
      then curry rec <$> y' <*> x'
      else curry rec <$> x' <*> y'

type Name = Word

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

inst :: ( MonadReader Level m
        , MonadRef r m
        , MonadSupply Label m
        ) => Type r -> m (Type r)
inst = runVisitT $$$ fix $ \ rec x -> visit x $ do
  x_level <- readRef x.level
  if x_level == genLevel
    then join $
         newType <$>
         readRef x.cons <*>
         (traverse (traverse rec) =<< readRef x.trans) <*>
         (traverseSet rec =<< readRef x.flow)
    else pure x

gen :: ( MonadReader Level m
       , MonadRef r m
       , MonadSupply Label m
       ) => Type r -> m ()
gen = runVisitT $$$ fix $ \ rec x -> visit_ x $ do
  x_level <- readRef x.level
  y <- ask
  when (x_level > y) $ do
    writeRef x.level genLevel
    traverse_ (traverse_ rec) =<< readRef x.trans
    traverse_ rec =<< readRef x.flow

infer :: ( MonadError () m
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

main :: IO ()
main = pure ()
