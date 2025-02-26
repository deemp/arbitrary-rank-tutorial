{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module ExercisesFreeFoil where

import Control.Monad.Foil.Internal (CoSinkable, Id, Name (UnsafeName), NameBinder (UnsafeNameBinder), NameBinderList (NameBinderListCons, NameBinderListEmpty), S (VoidS), nameBinderListOf, nameId, nameOf)
import Control.Monad.Free.Foil (AST (..), ScopedAST (..))
import Control.Monad.Free.Foil.Example (Expr, ExprF (..), churchN, ppName, pattern AppE, pattern LamE)
import Data.Bifoldable (Bifoldable (bifold, bifoldr))
import Data.Bifunctor (Bifunctor (..))
import Data.Bifunctor.TH (deriveBifoldable)
import Data.Bifunctor.Tannen
import Data.List (nub, (\\))
import Data.Monoid (Sum (..))
import Exercises (WidthState (..), defaultWidthState, gWidthOfExpr')

-- TODO don't rank-n?
foldAST :: (Bifunctor sig) => (forall t. Name t -> a) -> (sig a a -> a) -> AST binder sig n -> a
foldAST fName fNode = \case
  Var x -> fName x
  Node node ->
    fNode
      ( bimap
          (\(ScopedAST _ ast) -> foldAST fName fNode ast)
          (foldAST fName fNode)
          node
      )

sizeOfAST :: (Bifunctor sig, Bifoldable sig) => AST binder sig n -> Int
sizeOfAST = getSum . foldAST (const 1) ((+ 1) . bifold)

ex1 :: Expr VoidS
ex1 = churchN 3

$(deriveBifoldable ''ExprF)

test1 :: Bool
test1 = sizeOfAST ex1 == 9

-- TODO what is a node?
-- Should we count "Var"-s?

-- >>> ex1
-- λx0. λx1. (x0 (x0 (x0 x1)))

-- lam
--     \
--      lam
--         \
--          app
--           |  \
--           x0  app
--                |  \
--                x0  app
--                     |  \
--                     x0  x1

-- >>> test1
-- True

bifoldrSame :: (Bifoldable p) => (a -> c -> c) -> c -> p a a -> c
bifoldrSame f = bifoldr f f

heightOfAST :: (Bifunctor sig, Bifoldable sig) => AST binder sig n -> Int
heightOfAST = foldAST (const 1) ((+ 1) . bifoldrSame max 0)

test2 :: Bool
test2 = heightOfAST ex1 == 6

-- >>> test2
-- True

newtype Foldable' sig a = Foldable' (sig a a)

instance (Show (sig a a)) => Show (Foldable' sig a) where
  show :: Foldable' sig a -> String
  show (Foldable' sig) = show sig

instance (Show scope, Show term) => Show (ExprF scope term) where
  show = \case
    AppF x y -> "AppF (" <> show x <> " " <> show y <> ")"
    LamF body -> "LamF (" <> "?" <> ". " <> show body <> ")"

ppExpr' :: Expr n -> String
ppExpr' = \case
  Var name -> "Var " <> ppName name
  AppE x y -> "AppE " <> show x <> " " <> show y
  LamE binder body -> "LamE " <> ppName (nameOf binder) <> " (" <> ppExpr' body <> ")"

test4 :: String
test4 = ppExpr' ex1

-- >>> test4
-- "LamE x0 (LamE x1 (AppE x0 (x0 (x0 x1))))"

instance (Bifoldable sig) => Foldable (Foldable' sig) where
  foldr :: (a -> b -> b) -> b -> Foldable' sig a -> b
  foldr f z (Foldable' s) = bifoldrSame f z s

widthOfAST :: (Bifunctor sig, Bifoldable sig) => AST binder sig n -> WidthState
widthOfAST = foldAST (const defaultWidthState) (gWidthOfExpr' . Foldable')

test3 :: Bool
test3 = widthOfAST ex1 == WidthState{maxWidth = 6, maxHeight = 5}

-- >>> widthOfAST ex1
-- WidthState {maxWidth = 5, maxHeight = 5}

-- >>> test3
-- False

nameBinderListToListNameBinder :: NameBinderList n s -> [Id]
nameBinderListToListNameBinder NameBinderListEmpty = []
nameBinderListToListNameBinder (NameBinderListCons (UnsafeNameBinder name) nameBinderList) = nameId name : nameBinderListToListNameBinder nameBinderList

-- TODO improve complexity to MlogN, where M is the number of bound names, N is the number of free variables
freeVarsOfAST :: forall sig binder n. (Bifunctor sig, Bifoldable sig, CoSinkable binder) => AST binder sig n -> [Name n]
freeVarsOfAST = \case
  Var x -> [x]
  -- TODO should we extract "Name l"-s?
  Node node -> bifold (bimap go freeVarsOfAST node)
 where
  go :: ScopedAST binder sig n -> [Name n]
  go (ScopedAST binder ast) = UnsafeName <$> (nub freeVars \\ nub boundNames)
   where
    boundNames = nameBinderListToListNameBinder $ nameBinderListOf binder
    freeVars = nameId <$> freeVarsOfAST ast

test5 :: [Name VoidS]
test5 = freeVarsOfAST ex1

-- >>> test5
-- []

-- >>> [4,4,4] \\ [4]
-- [4,4]

stringFromAST :: (Bifunctor sig) => (forall t. Name t -> String) -> (sig String String -> String) -> (forall k l. binder k l -> AST binder sig l -> String) -> AST binder sig n -> String
stringFromAST fName fNode fBinder = \case
  Var x -> fName x
  Node node ->
    fNode
      ( bimap
          (\(ScopedAST binder ast) -> fBinder binder ast)
          (foldAST fName fNode)
          node
      )

newtype Wrapper a = Wrapper {unwrap :: a}

-- deriving Foldable

instance Show (Wrapper String) where
  show (Wrapper s) = s

instance Semigroup (Wrapper String) where
  (Wrapper x) <> (Wrapper y) = Wrapper (x <> y)

instance Monoid (Wrapper String) where
  mempty = Wrapper ""

ex2 :: Expr VoidS
ex2 = churchN 5

test6 :: String
test6 = myStringFromAST ex2

myStringFromAST :: Expr n -> String
myStringFromAST =
  stringFromAST
    ppName
    (show . bimap Wrapper Wrapper)
    (\binder ast -> "NameBinder " <> ppName (nameOf binder) <> " (" <> myStringFromAST ast <> ")")

-- >>> test6
-- "LamF (?. NameBinder x0 (LamF (?. NameBinder x1 (AppF (x0 AppF (x0 AppF (x0 AppF (x0 AppF (x0 x1)))))))))"

-- TODO ????
t :: forall binder sig n. (Bifunctor sig, Show (sig String String), (forall k l. Show (binder k l))) => AST binder sig n -> String
t = stringFromAST (show . nameId) show l
 where
  l :: (Show (binder k l)) => binder k l -> AST binder sig l -> String
  l = (\binder ast -> show binder <> stringFromAST (show . nameId) show l ast)

transAST' ::
  (Bifunctor sig, Bifunctor sig') =>
  ( forall m.
    sig (ScopedAST binder sig m) (AST binder sig m) ->
    sig' (ScopedAST binder sig m) (AST binder sig m)
  ) ->
  AST binder sig n ->
  AST binder sig' n
transAST' phi = \case
  Var v -> Var v
  Node node ->
    Node $
      bimap
        (\(ScopedAST binder ast) -> ScopedAST binder (transAST' phi ast))
        (transAST' phi)
        (phi node)

transAST :: (Bifunctor sig, Bifunctor sig') => (forall x y. sig x y -> sig' x y) -> AST binder sig n -> AST binder sig' n
transAST phi = transAST' phi

cutoff :: (Bifunctor sig) => Int -> AST binder sig n -> AST binder (Tannen Maybe sig) n
cutoff depth = \case
  Var x -> Var x
  Node node ->
    Node $
      bimap
        (\(ScopedAST binder ast) -> ScopedAST binder (cutoff (depth - 1) ast))
        (cutoff (depth - 1))
        (Tannen (cond node))
 where
  cond x = if (depth < 0) then Nothing else (Just x)
