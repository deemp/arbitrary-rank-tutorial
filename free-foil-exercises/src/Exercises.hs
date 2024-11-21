{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Exercises () where

import Control.Monad (ap)
import Data.Foldable (Foldable (..))
import Data.Functor.Compose
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Data.Monoid (Sum (..))
import Data.Void (Void)

data Expr a
  = Expr'Var a
  | Expr'Lit Int
  | Expr'Add (Expr a) (Expr a)
  | Expr'Mul (Expr a) (Expr a)
  deriving (Show, Functor)

instance Applicative Expr where
  pure = Expr'Var
  (<*>) = ap

instance Monad Expr where
  e >>= f = flattenExpr (fmap f e)

-- 1. change variable identifier type
-- 2. replace variable identifier with its value
-- 3. control presence of variable identifier

newtype VarId = VarId Int
  deriving (Eq, Show)

-- VarId -> String
-- Expr VarId -> Expr String

example1 :: Expr VarId
example1 = Expr'Mul (Expr'Add x y) (Expr'Add z w)
 where
  x = Expr'Var (VarId 1)
  y = Expr'Var (VarId 2)
  z = Expr'Var (VarId 3)
  w = Expr'Lit 123

-- (1)

ppVarId :: VarId -> String
ppVarId (VarId n) = "x" ++ show n

-- ppVarId (VarId n) = ["x", "y", "z", ...] !! n

-- >>> ppExpr (fmap ppVarId example1)
-- "(x1 + x2) * (x3 + 123)"
ppExpr :: Expr String -> String
ppExpr = \case
  Expr'Var x -> x
  Expr'Lit n -> show n
  Expr'Add l r -> "(" ++ ppExpr l ++ " + " ++ ppExpr r ++ ")"
  Expr'Mul l r -> ppExpr l ++ " * " ++ ppExpr r

-- (2)

type VarValues a = [(VarId, a)]

varValue :: VarValues a -> VarId -> a
varValue values x =
  case lookup x values of
    Nothing -> error ("undefined variable " ++ ppVarId x)
    Just n -> n

-- >>> evalExpr (fmap (varValue [(VarId 1, 5), (VarId 2, 7), (VarId 3, 8)]) example1)
-- 1572
-- >>> evalExpr (fmap (varValue [(VarId 1, 5), (VarId 3, 8)]) example1)
-- undefined variable x2
evalExpr :: Expr Int -> Int
evalExpr = \case
  Expr'Var x -> x
  Expr'Lit n -> n
  Expr'Add l r -> evalExpr l + evalExpr r
  Expr'Mul l r -> evalExpr l * evalExpr r

substExpr :: VarValues (Expr VarId) -> Expr VarId -> Expr VarId
substExpr defs = \case
  Expr'Var x ->
    case lookup x defs of
      Nothing -> Expr'Var x
      Just expr -> expr
  Expr'Lit n -> Expr'Lit n
  Expr'Add l r -> Expr'Add (substExpr defs l) (substExpr defs r)
  Expr'Mul l r -> Expr'Mul (substExpr defs l) (substExpr defs r)

substExpr' :: VarValues (Expr VarId) -> Expr VarId -> Expr VarId
substExpr' defs expr = flattenExpr (fmap (varValue defs) expr)

flattenExpr :: Expr (Expr a) -> Expr a
flattenExpr = \case
  Expr'Var e -> e
  Expr'Lit n -> Expr'Lit n
  Expr'Add l r -> Expr'Add (flattenExpr l) (flattenExpr r)
  Expr'Mul l r -> Expr'Mul (flattenExpr l) (flattenExpr r)

-- (3)

-- >>> ppExpr (fmap show example2)
-- "(() + ()) * 23"
-- >>> ppExpr (fmap (const "x") example2)
-- "(x + x) * 23"
-- >>> evalExpr (fmap (const 1) example2)
-- 46
example2 :: Expr ()
example2 = Expr'Mul (Expr'Add x x) (Expr'Lit 23)
 where
  x = Expr'Var ()

-- >>> ppExpr (fmap absurd example3)
-- "(2 + 5) * 23"
-- >>> evalExpr (fmap absurd example3)
-- 161
example3 :: Expr Void
example3 = Expr'Mul (Expr'Add (Expr'Lit 2) (Expr'Lit 5)) (Expr'Lit 23)

------------

-- fix f = f (fix f)

newtype Fix f = In {out :: f (Fix f)}

deriving instance (Show (f (Fix f))) => Show (Fix f)

data EFix e
  = EFix'Var VarId
  | EFix'Lit Int
  | EFix'Add e e
  | EFix'Mul e e
  deriving (Show, Functor)

type ExprFix = Fix EFix

-- foldFix :: (EFix a -> a) -> Fix EFix -> a
foldFix :: (Functor f) => (f b -> b) -> Fix f -> b
foldFix phi (In e) = phi (fmap (foldFix phi) e)

ppEFix :: EFix String -> String
ppEFix = \case
  EFix'Var x -> ppVarId x
  EFix'Lit n -> show n
  EFix'Add l r -> "(" ++ l ++ " + " ++ r ++ ")"
  EFix'Mul l r -> l ++ " * " ++ r

ppExprFix :: ExprFix -> String
ppExprFix = foldFix ppEFix

------------------------------

-- data Free f a
--   = Pure a
--   | Free (f (Free f a))

data GExpr node var
  = GExpr'Var var
  | GExpr'Node (node (GExpr node var))
  deriving (Functor)

deriving instance (Show var, Show (node (GExpr node var))) => Show (GExpr node var)

data EFree e
  = EFree'Lit Int
  | EFree'Add e e
  | EFree'Mul e e
  deriving (Show, Functor, Foldable)

type ExprFree a = GExpr EFree a

-- foldGExpr :: (EFree String -> String) -> (ExprFree String -> String)
-- foldGExpr :: (EFree a -> a) -> ExprFree a -> a
foldGExpr :: (Functor f) => (f a -> a) -> GExpr f a -> a
foldGExpr phi = \case
  GExpr'Var x -> x
  GExpr'Node node -> phi (fmap (foldGExpr phi) node)

ppEFree :: EFree String -> String
ppEFree = \case
  EFree'Lit n -> show n
  EFree'Add l r -> "(" ++ l ++ " + " ++ r ++ ")"
  EFree'Mul l r -> l ++ " * " ++ r

ppExprFree :: ExprFree String -> String
ppExprFree = foldGExpr ppEFree

flattenGExpr :: (Functor f) => GExpr f (GExpr f a) -> GExpr f a
flattenGExpr = \case
  GExpr'Var e -> e
  GExpr'Node node -> GExpr'Node (fmap flattenGExpr node)

instance (Functor f) => Applicative (GExpr f) where
  pure = GExpr'Var
  (<*>) = ap

instance (Functor f) => Monad (GExpr f) where
  e >>= f = flattenGExpr (fmap f e)

-- Homework

-- 1. implement evalExpr, substExpr for Fix and GExpr
-- 2. implement generic functions:
--    varsOf :: GExpr f a -> [a]
--    tryClose :: GExpr f a -> Maybe (GExpr f Void)
--    isAffine :: Eq a => GExpr f a -> Bool   -- every variable occurs at most once
--    sizeOfExpr :: GExpr f a -> Int
--    heightOfExpr :: GExpr f a -> Int
--    widthOfExpr :: GExpr f a -> Int
--    transGExpr :: (forall x. f x -> g x) -> GExpr f a -> GExpr g a
--
--    import Data.Functor.Compose
--    cutoff :: Int -> GExpr f a -> GExpr (Compose Maybe f) a
--
--    + implement anything you want from Control.Monad.Free

-- ## Ex 1.

-- ### Fix

-- #### evalExpr

evalEFix :: (Num a) => VarValues a -> EFix a -> a
evalEFix defs = \case
  EFix'Var x -> varValue defs x
  EFix'Lit x -> fromIntegral x
  EFix'Add x y -> x + y
  EFix'Mul x y -> x * y

evalExprFix :: (Num a) => VarValues a -> Fix EFix -> a
evalExprFix = foldFix . evalEFix

exprFix1 :: ExprFix
exprFix1 = In (EFix'Add (In (EFix'Var (VarId 1))) (In (EFix'Lit 5)))

-- >>> ppExprFix exprFix1
-- "(x1 + 5)"

example4 :: Integer
example4 = evalExprFix [(VarId 1, 5)] exprFix1

-- >>> example4
-- 10

-- #### substExpr

substExprFix :: VarValues ExprFix -> ExprFix -> ExprFix
substExprFix defs (out -> out') =
  case out' of
    EFix'Var x ->
      case lookup x defs of
        Nothing -> In (EFix'Var x)
        Just val -> val
    EFix'Add x y -> In (EFix'Add (substExprFix defs x) (substExprFix defs y))
    EFix'Mul x y -> In (EFix'Add (substExprFix defs x) (substExprFix defs y))
    r -> In r

example5 :: ExprFix
example5 = substExprFix [(VarId 1, In (EFix'Lit 2))] exprFix1

-- >>> example5
-- In {out = EFix'Add (In {out = EFix'Lit 2}) (In {out = EFix'Lit 5})}

-- ### GExpr

-- #### evalGExpr

evalEFree :: EFree Int -> Int
evalEFree = \case
  EFree'Lit x -> x
  EFree'Add x y -> x + y
  EFree'Mul x y -> x * y

evalGExpr :: VarValues Int -> GExpr EFree VarId -> Int
evalGExpr defs = foldGExpr evalEFree . (fmap (varValue defs))

exampleGExpr1 :: ExprFree VarId
exampleGExpr1 = GExpr'Node (EFree'Mul (GExpr'Node (EFree'Add (GExpr'Var (VarId 1)) (GExpr'Node (EFree'Lit 3)))) (GExpr'Var (VarId 2)))

example6 :: Int
example6 = evalGExpr [(VarId 1, 7), (VarId 2, 3)] exampleGExpr1

-- >>> example6
-- 30

-- #### substGExpr

varValue' :: VarValues (ExprFree VarId) -> VarId -> ExprFree VarId
varValue' values x =
  case lookup x values of
    Nothing -> GExpr'Var x
    Just n -> n

substGExpr :: VarValues (ExprFree VarId) -> ExprFree VarId -> ExprFree VarId
substGExpr defs = (>>= varValue' defs)

example7 :: ExprFree VarId
example7 = substGExpr [(VarId 1, GExpr'Node (EFree'Lit 4))] exampleGExpr1

ppExprFree' :: ExprFree VarId -> String
ppExprFree' x = ppExprFree (ppVarId <$> x)

-- >>> ppExprFree' example7
-- "(4 + 3) * x2"

-- ## Ex2

-- ### varsOf

varsOf :: (Functor f, Foldable f) => GExpr f a -> [a]
varsOf = foldGExpr fold . ((: []) <$>)

example8 :: [VarId]
example8 = varsOf exampleGExpr1

-- >>> example8
-- [VarId 1,VarId 2]

-- ### tryClose

tryClose :: (Functor f, Foldable f) => GExpr f a -> Maybe (GExpr f Void)
tryClose ex =
  case listToMaybe (varsOf ex) of
    Just _ -> Nothing
    _ -> Just (const (error "contains variables!") <$> ex)

-- >>> tryClose exampleGExpr1
-- Nothing

-- ### isAffine

isAffine :: (Eq a, Functor f, Foldable f) => GExpr f a -> Bool
isAffine ex = nub (varsOf ex) == varsOf ex

-- >>> isAffine exampleGExpr1
-- True

exampleGExpr2 :: ExprFree VarId
exampleGExpr2 = substGExpr [(VarId 1, GExpr'Var (VarId 2))] exampleGExpr1

-- >>> ppExprFree' exampleGExpr2
-- "(x2 + 3) * x2"

-- >>> isAffine exampleGExpr2
-- False

-- ### sizeOfExpr
-- Number of variables and nodes

sizeOfExpr :: (Functor f, Foldable f) => GExpr f a -> Int
sizeOfExpr ex = getSum (foldGExpr (\x -> 1 + fold x) (const 1 <$> ex))

-- >>> sizeOfExpr exampleGExpr2
-- 5

-- ### heightOfExpr

heightOfExpr :: ExprFree a -> Int
heightOfExpr = \case
  GExpr'Var _ -> 0
  GExpr'Node node ->
    case node of
      EFree'Lit _ -> 0
      EFree'Add x y -> getNodeHeight x y
      EFree'Mul x y -> getNodeHeight x y
   where
    getNodeHeight x y = 1 + max (heightOfExpr x) (heightOfExpr y)

exampleGExpr3 :: ExprFree VarId
exampleGExpr3 = GExpr'Node (EFree'Add (GExpr'Var (VarId 1)) exampleGExpr2)

-- >>> ppExprFree' exampleGExpr3
-- "(x1 + (x2 + 3) * x2)"

-- [+]
-- !  \
-- x1  [*]
--     |   \
--     [+]  x2
--     | \
--     x2 3

-- >>> heightOfExpr exampleGExpr3
-- 3

gHeightOfExpr :: (Foldable f, Functor f) => GExpr f a -> Int
gHeightOfExpr = foldGExpr ((+ 1) . foldr max 0) . fmap (const 1)

-- >>> gHeightOfExpr exampleGExpr3
-- 4

-- ### widthOfExpr

data WidthState = WidthState {maxWidth :: Int, maxDepth :: Int} deriving (Show)

defaultWidthState :: WidthState
defaultWidthState = WidthState{maxWidth = 0, maxDepth = 0}

widthOfExpr' :: ExprFree a -> WidthState
widthOfExpr' ex =
  case ex of
    GExpr'Var _ -> defaultWidthState
    GExpr'Node node ->
      case node of
        EFree'Lit _ -> defaultWidthState
        EFree'Add x y -> processBinOpNode x y
        EFree'Mul x y -> processBinOpNode x y
 where
  processBinOpNode (widthOfExpr' -> x) (widthOfExpr' -> y) =
    WidthState
      { maxWidth = maximum [(1 + x.maxDepth) + (1 + y.maxDepth), x.maxWidth, y.maxWidth]
      , maxDepth = 1 + maximum [x.maxDepth, y.maxDepth]
      }

widthOfExpr :: ExprFree a -> Int
widthOfExpr = maxWidth . widthOfExpr'

-- >>> ppExprFree' exampleGExpr3
-- "(x1 + (x2 + 3) * x2)"

-- [+]
-- !  \
-- x1 [*]
--     | \
--    [+]  x2
--     | \
--     x2 3

-- >>> widthOfExpr exampleGExpr3
-- 4

-- >>> ppExprFree' example7
-- "(4 + 3) * x2"

-- [*]
-- !  \
-- +   x2
-- l \
-- 4  3

-- >>> widthOfExpr example7
-- 3

gWidthOfExpr' :: (Foldable f) => f WidthState -> WidthState
gWidthOfExpr' f =
  WidthState
    { maxDepth = maxDepth'
    , maxWidth = maxWidth'
    }
 where
  maxWidth'1 = foldr (max . (.maxWidth)) 0 f
  maxWidth'2 = foldr (\x acc -> 1 + x.maxDepth + acc) 0 f
  maxWidth' = max maxWidth'1 maxWidth'2
  maxDepth' = foldr (max . (+ 1) . (.maxDepth)) 0 f

gWidthStateOfExpr :: (Foldable f, Functor f) => GExpr f a -> WidthState
gWidthStateOfExpr = foldGExpr gWidthOfExpr' . fmap (const defaultWidthState)

-- >>> gWidthStateOfExpr example7
-- WidthState {maxWidth = 3, maxDepth = 2}

-- >>> gWidthStateOfExpr exampleGExpr3
-- WidthState {maxWidth = 4, maxDepth = 3}

-- ### transGExpr

transGExpr :: (Functor g) => (forall x. f x -> g x) -> GExpr f a -> GExpr g a
transGExpr f = \case
  GExpr'Var x -> GExpr'Var x
  GExpr'Node node -> GExpr'Node (fmap (transGExpr f) (f node))

-- ### cutoff

cutoff :: (Functor f) => Int -> GExpr f a -> GExpr (Compose Maybe f) a
cutoff depth = \case
  GExpr'Var x -> if depth < 0 then GExpr'Node (Compose Nothing) else GExpr'Var x
  GExpr'Node node -> GExpr'Node (fmap (cutoff (depth - 1)) (Compose (if (depth >= 0) then (Just node) else Nothing)))

-- >>> ppExprFree' exampleGExpr3
-- "(x1 + (x2 + 3) * x2)"

-- >>> cutoff 1 exampleGExpr3
-- GExpr'Node (Compose (Just (EFree'Add (GExpr'Var (VarId 1)) (GExpr'Node (Compose (Just (EFree'Mul (GExpr'Node (Compose Nothing)) (GExpr'Node (Compose Nothing)))))))))

-- [+]
-- !  \
-- x1  [*]
--      | \
--      ?  ?

-- >>> cutoff 0 exampleGExpr3
-- GExpr'Node (Compose (Just (EFree'Add (GExpr'Node (Compose Nothing)) (GExpr'Node (Compose Nothing)))))

-- [+]
-- !  \
-- ?  ?
