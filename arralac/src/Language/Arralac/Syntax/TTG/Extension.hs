module Language.Arralac.Syntax.TTG.Extension where

-- ==============================================
-- [Term extensions]
-- ==============================================

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Extension.hs#L412
type family XSynTerm'Var' x
type family XSynTerm'Lit' x
type family XSynTerm'App' x
type family XSynTerm'Lam' x
type family XSynTerm'ALam' x
type family XSynTerm'Let' x
type family XSynTerm'Ann' x

-- ==============================================
-- [Syntactic type extensions]
-- ==============================================
type family XSynType'Var' x
type family XSynType'ForAll' x
type family XSynType'Fun' x
type family XSynType'Concrete' x
