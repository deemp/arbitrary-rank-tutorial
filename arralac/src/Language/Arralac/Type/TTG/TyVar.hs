module Language.Arralac.Type.TTG.TyVar where

-- We use different representations for term and type variables.
--
-- In our calculus, term variables don't have types, while in GHC they do.
--
-- GHC uses the same 'LIdP p' type to represent type and term variables in the AST.
--
-- Term variables:
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Expr.hs#L334
--
-- Type variables:
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Type.hs#L828

type family XTyVar p
