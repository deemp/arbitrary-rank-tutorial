module Language.Arralac.Type.TTG.Type where

import Language.Arralac.Type.TTG.TyVar

-- | Type of expressions parameterised over the compiler pass.
--
-- Similar to @Type@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core/TyCo/Rep.hs#L124
--
-- For type safety, we need different representations of 'Type'
-- during different passes. E.g., 'Type' may not contain
-- mutable variables after zonking.
--
-- GHC authors also considered using different representations for different passes.
--
-- A proposal to move 'TcTyVar' from 'Var'.
-- https://gitlab.haskell.org/ghc/ghc/-/issues/15552#note_159240
--
-- See Note [TyVars and TcTyVars during type checking] in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L558
--
-- We use different representations of type variables via the 'XTyVar' type family.
data Type p
  = -- | Type variable.
    --
    -- @x@
    Type'Var (XTyVar p)
  | -- | Forall construct.
    --
    -- @forall a. b@
    Type'ForAll
      [XTyVar p]
      (Type p)
  | -- | Function type.
    --
    -- @a -> b@
    Type'Fun (Type p) (Type p)
  | -- | Concrete type.
    Type'Concrete (XTyConcrete p)

type family XTyConcrete p