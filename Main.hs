{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Main (main) where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader.Class

import Data.Foldable
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

class Monad m => MonadSupply s m | m -> s where
  supply :: m s

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

writeLevels :: MonadRef r m => Type r -> Level -> m ()
writeLevels x y = do
  x_level <- readRef x.level
  when (x_level > y) $ do
    writeRef x.level y
    traverse_ (traverse_ (flip writeLevels y)) =<< readRef x.trans
    traverse_ (flip writeLevels y) =<< readRef x.flow

merge :: MonadRef r m => Type r -> Type r -> m ()
merge x y = do
  writeRef x.cons =<< Set.union <$> readRef x.cons <*> readRef y.cons
  x_level <- readRef x.level
  y_level <- readRef y.level
  case compare x_level y_level of
    LT -> writeLevels y x_level
    EQ -> pure ()
    GT -> writeLevels x y_level
  writeRef x.trans =<< Map.unionWith (++) <$> readRef x.trans <*> readRef y.trans
  writeRef x.flow =<< Set.union <$> readRef x.flow <*> readRef y.flow

unify :: (MonadError () m, MonadRef r m) => Type r -> Type r -> m ()
unify x y = do
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
      then unify <$> y' <*> x'
      else unify <$> x' <*> y'

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
inst x = do
  x_level <- readRef x.level
  if x_level == genLevel then do
    x_cons <- readRef x.cons
    join $
      newType <$>
      readRef x.cons <*>
      (traverse (traverse inst) =<< readRef x.trans) <*>
      (mapM inst =<< readRef x.flow)
    else pure x
  where
    mapM f =
      foldl (\ m x -> Set.insert <$> f x <*> m) (pure Set.empty)

gen :: ( MonadReader Level m
       , MonadRef r m
       , MonadSupply Label m
       ) => Type r -> m (Type r)
gen = undefined

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
    t_e1 <- gen =<< local succ (infer env e1)
    let env = Map.insert x (True, t_e1) env
    infer env e2

main :: IO ()
main = pure ()
