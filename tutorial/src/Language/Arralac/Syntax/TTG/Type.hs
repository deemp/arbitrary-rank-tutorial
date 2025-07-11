module Language.Arralac.Syntax.TTG.Type where

import Language.Arralac.Utils.Pretty
import Language.Arralac.Utils.Types (FastString)

-- | Type of expressions parameterised over the compiler pass.
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
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L558
--
-- We use different representations of type variables via the 'XVar'' type family.
data Type p
  = -- | Vanilla type variable
    Type'Var (XVar' p)
  | Type'ForAll
      [XVar' p]
      (Type p)
  | -- | This is a special case of a type constructor
    -- where the type constructor is (->).
    Type'Fun (Type p) (Type p)
  | -- | Type literals are similar to type constructors.
    Type'Concrete TypeConcrete

type family XVar' p

data TypeConcrete
  = TypeConcrete'Int
  | TypeConcrete'Bool
  | TypeConcrete'String
  | TypeConcrete'Con FastString
  deriving stock (Eq)

typeConcreteName :: TypeConcrete -> FastString
typeConcreteName = \case
  TypeConcrete'Int -> "Int"
  TypeConcrete'Bool -> "Bool"
  TypeConcrete'String -> "String"
  TypeConcrete'Con name -> name

instance Pretty' TypeConcrete where
  pretty' = pretty' . typeConcreteName