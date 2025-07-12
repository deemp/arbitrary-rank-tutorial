module Language.Arralac.Syntax.TTG.SynType where

import Language.Arralac.Syntax.TTG.Extension

-- ==============================================
-- [Syntactic type]
-- ==============================================

-- | Type AST in TTG representation.
--
-- Similar to @HsType@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Type.hs#L812
data SynType x
  = -- | Type variable.
    --
    -- @x@
    SynType'Var (XSynType'Var' x) (XSynType'Var x)
  | -- | Forall construct.
    --
    -- @forall a. b@
    SynType'ForAll (XSynType'ForAll' x) [XSynType'ForAll'Var x] (XSynType'ForAll'Body x)
  | -- | Function type.
    --
    -- @a -> b@
    SynType'Fun (XSynType'Fun' x) (XSynType'Fun'Arg x) (XSynType'Fun'Res x)
  | -- | Concrete type.
    --
    -- @String@
    SynType'Concrete (XSynType'Concrete' x) (XSynType'Concrete x)

-- ==============================================
-- [Type families for type AST nodes]
-- ==============================================

type family XSynType'Var x

type family XSynType'ForAll'Var x
type family XSynType'ForAll'Body x

type family XSynType'Fun'Arg x
type family XSynType'Fun'Res x

type family XSynType'Concrete x