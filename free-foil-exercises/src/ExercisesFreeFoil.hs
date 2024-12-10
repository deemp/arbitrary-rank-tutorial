{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module ExercisesFreeFoil where

import Control.Monad.Foil
import Control.Monad.Free.Foil
import Data.Bifoldable (Bifoldable)
import Data.Bifunctor (Bifunctor)
import Data.Bifunctor.Tannen


class A a where
  abra :: a -> a

class B b where
  bebra :: b -> b
  
-- t :: (A a => B a) => B a => a -> a
-- t x = x

-- foldGExpr :: (Bifunctor sig) => (sig  a -> a) -> AST binder sig n -> a
-- foldGExpr phi = \case
--   Var x -> x`
--   Node node -> phi (fmap (foldGExpr phi) node)

sizeOfAST :: (Bifunctor sig) => AST binder sig n -> Int
sizeOfAST = _

heightOfAST :: (Bifunctor sig) => AST binder sig n -> Int
heightOfAST = _

widthOfAST :: (Bifunctor sig) => AST binder sig n -> Int
widthOfAST = _

freeVarsOfAST :: (Bifoldable sig) => AST binder sig n -> [Name n]
freeVarsOfAST = _

stringFromAST :: (sig String String -> String) -> AST binder sig n -> String
stringFromAST = _

-- stringFromType :: Type n -> String
-- stringFromType = _

-- stringFromExp :: Exp n -> String
-- stringFromExp = _

-- substitute' :: Scope o -> (Name i -> AST sig o) -> AST sig i -> AST sig o
-- substitute' = _

-- comm :: ExpSig scope term -> ExpSig scope term
-- comm (EAddSig l r) = EAddSig r l
-- comm node = node

-- transAST' ::
--   (Bifunctor sig) =>
--   (forall m. sig (ScopedAST binder sig m) (AST binder sig m) -> sig (ScopedAST binder sig m) (AST binder sig m)) ->
--   AST binder sig n ->
--   AST binder sig' n
-- transAST' = _

-- transAST :: (Bifunctor sig) => (forall x y. sig x y -> sig' x y) -> AST binder sig n -> AST binder sig' n
-- transAST = _
-- cutoff :: (Bifunctor sig) => Int -> AST binder sig n -> AST binder (Tannen Maybe sig) n
-- cutoff = _
-- foldAST :: (Bifunctor sig) => (Name n -> a) -> (sig a a -> a) -> AST binder sig n -> a
-- foldAST = _

-- evalExp :: Exp VoidS -> Int
-- evalExp = _
