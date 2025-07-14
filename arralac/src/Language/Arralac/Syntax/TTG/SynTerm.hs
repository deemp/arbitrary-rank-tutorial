module Language.Arralac.Syntax.TTG.SynTerm where

import Language.Arralac.Syntax.TTG.Extension
import Language.Arralac.Syntax.TTG.Lit
import Language.Arralac.Syntax.TTG.SynTermVar

-- | Syntactic terms.
--
-- Similar to @HsExpr@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Expr.hs#L332
--
-- Usecase in GHC:
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Expr.hs#L1294
data SynTerm x
  = -- | x
    -- TODO replace with a family
    SynTerm'Var (XSynTerm'Var' x) (XSynTerm'Var x)
  | -- | 3
    SynTerm'Lit (XSynTerm'Lit' x) (XSynTerm'Lit x)
  | -- | f x
    SynTerm'App (XSynTerm'App' x) (XSynTerm'App'Fun x) (XSynTerm'App'Arg x)
  | -- | \ x -> x
    SynTerm'Lam (XSynTerm'Lam' x) (XSynTerm'Lam'Var x) (XSynTerm'Lam'Body x)
  | -- | \ (x :: a) -> x
    SynTerm'ALam (XSynTerm'ALam' x) (XSynTerm'ALam'Var x) (XSynTerm'ALam'Type x) (XSynTerm'ALam'Body x)
  | -- | let x = f y in x+1
    SynTerm'Let (XSynTerm'Let' x) (XSynTerm'Let'Name x) (XSynTerm'Let'AssignedTerm x) (XSynTerm'Let'InTerm x)
  | -- | (f x) :: Int
    SynTerm'Ann (XSynTerm'Ann' x) (XSynTerm'Ann'Term x) (XSynTerm'Ann'Type x)

-- TODO ^ make each constructor a record?

-- TODO ^ add parentheses?
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Expr.hs#L382
-- https://github.com/ghc/ghc/blob/ef03d8b8851a1cace5f792fe5a91b6b227198aa2/compiler/Language/Haskell/Syntax/Expr.hs#L585

-- TODO ^ add extension point to support Trees That Grow
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Expr.hs#L537

-- Type families for non-extension fields should be defined together with the type.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Expr.hs#L206

type family XSynTerm'App'Fun x
type family XSynTerm'App'Arg x

type family XSynTerm'Lam'Var x
type family XSynTerm'Lam'Body x

type family XSynTerm'ALam'Var x
type family XSynTerm'ALam'Type x
type family XSynTerm'ALam'Body x

type family XSynTerm'Let'Name x
type family XSynTerm'Let'AssignedTerm x
type family XSynTerm'Let'InTerm x

type family XSynTerm'Ann'Term x
type family XSynTerm'Ann'Type x
