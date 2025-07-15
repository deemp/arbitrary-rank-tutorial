{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Syntax.Local.Type where

import Language.Arralac.Syntax.TTG.Type

import Data.IORef (IORef)
import Language.Arralac.Pass.Types (CompRn, CompTc, CompZn)
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.TTG.TyVar
import Prettyprinter

-- | Expected type.
--
-- Similar to @ExpType@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L401
data Expected a = Infer (IORef a) | Check a

-- TODO ^ Do we need TcLevel in Infer?
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L418

-- =======
-- [Types]
-- =======

-- | A type that can have mutable type variables.
--
-- Similar to @TcType@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L346
--
-- GHC also has 'Kind's, but we don't have them.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core/TyCo/Rep.hs#L107
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core/TyCo/Rep.hs#L110
type TcType = Type CompTc

type RnType = Type CompRn

type ZnType = Type CompZn

type TcTypeMeta = TcType

type Sigma = TcType
type Rho = TcType -- No top-level ForAll
type Tau = TcType -- No ForAlls anywhere

-- ================
-- [Concrete types]
-- ================

data Concrete = Concrete
  { concreteName :: Name
  , concreteType :: TypeConcrete
  }

-- =====================
-- [Instances for Types]
-- =====================

instance (Pretty' a) => Pretty' (Expected a) where
  pretty' (Infer _) = "[Infer]"
  pretty' (Check a) = "[Check]: " <> pretty' a

instance (Pretty' (XTyVar p)) => Pretty' (Type p) where
  pretty' = \case
    Type'Var var -> pretty' var
    Type'ForAll vars body -> "forall " <> hsep (pretty' <$> vars) <> "." <+> pretty' body
    Type'Fun arg res -> parens' arg' <+> "->" <+> pretty' res
     where
      arg' = pretty' arg
      parens' =
        case arg of
          Type'Fun _ _ -> parens
          Type'ForAll _ _ -> parens
          _ -> id
    Type'Concrete ty -> pretty' ty

instance Pretty' Concrete where
  pretty' c = pretty' c.concreteType
