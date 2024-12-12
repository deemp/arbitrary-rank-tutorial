{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module ExercisesFreeFoil where

import Control.Monad.Foil
import Control.Monad.Free.Foil
import Data.Bifoldable (Bifoldable (bifold, bifoldr))
import Data.Bifunctor (Bifunctor (..))
import Data.Bifunctor.Tannen
import Data.Coerce (coerce)
import Data.Monoid (Sum (..))

foldAST :: (Bifunctor sig) => (Name t -> a) -> (sig a a -> a) -> AST binder sig n -> a
foldAST fName fNode = \case
  Var x -> fName (coerce x)
  Node node ->
    fNode
      ( bimap
          (\(ScopedAST _ ast) -> foldAST fName fNode ast)
          (foldAST fName fNode)
          node
      )

sizeOfAST :: (Bifunctor sig, Bifoldable sig) => AST binder sig n -> Int
sizeOfAST = getSum . foldAST (const 1) bifold

bifoldrSame :: (Bifoldable p) => (a -> c -> c) -> c -> p a a -> c
bifoldrSame f = bifoldr f f

heightOfAST :: (Bifunctor sig, Bifoldable sig) => AST binder sig n -> Int
heightOfAST = foldAST (const 0) ((+ 1) . bifoldrSame max 0)

data WidthState = WidthState
  { maxWidth :: Int
  , maxDepth :: Int
  }
  deriving (Show)

defaultWidthState :: WidthState
defaultWidthState = WidthState{maxWidth = 0, maxDepth = 0}

widthOfAST :: (Bifunctor sig, Bifoldable sig) => AST binder sig n -> WidthState
widthOfAST = foldAST (const defaultWidthState) go
 where
  -- ast' = bimap (const defaultWidthState) (const defaultWidthState) ast
  go :: (Bifoldable sig) => sig WidthState WidthState -> WidthState
  go f =
    WidthState
      { maxDepth = maxDepth'
      , maxWidth = maxWidth'
      }
   where
    maxWidth'1 = bifoldrSame (max . (.maxWidth)) 0 f
    maxWidth'2 = bifoldrSame (\x acc -> 1 + x.maxDepth + acc) 0 f
    maxWidth' = max maxWidth'1 maxWidth'2
    maxDepth' = bifoldrSame (max . (+ 1) . (.maxDepth)) 0 f

freeVarsOfAST :: (Bifunctor sig, Bifoldable sig) => AST binder sig n -> [Name n]
freeVarsOfAST = \case
  Var x -> [x]
  -- TODO should we extract "Name l"-s?
  Node node -> bifold (bimap (const []) freeVarsOfAST node)

stringFromAST :: (Bifunctor sig) => (Name t -> String) -> (sig String String -> String) -> AST binder sig n -> String
stringFromAST f g = foldAST f g

transAST' ::
  (Bifunctor sig) =>
  ( forall m.
    sig (ScopedAST binder sig m) (AST binder sig m) ->
    sig' (ScopedAST binder sig' m) (AST binder sig' m)
  ) ->
  AST binder sig n ->
  AST binder sig' n
transAST' = _

-- transAST :: (Bifunctor sig) => (forall x y. sig x y -> sig' x y) -> AST binder sig n -> AST binder sig' n
-- transAST = _

-- cutoff :: (Bifunctor sig) => Int -> AST binder sig n -> AST binder (Tannen Maybe sig) n
-- cutoff = _
