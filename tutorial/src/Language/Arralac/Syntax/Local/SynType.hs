{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.SynType where

import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.Local.Var.Tc
import Language.Arralac.Syntax.Local.Var.Zn (ZnTyVar)
import Language.Arralac.Syntax.TTG.SynType
import Language.Arralac.Utils.Pass
import Language.Arralac.Utils.Pretty
import Prettyprinter (hsep, parens, (<+>))

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

type instance XSynType'Var CompRn = Name
type instance XSynType'Var CompTc = TcTyVar
type instance XSynType'Var CompZn = ZnTyVar

type instance XSynType'ForAll'Var CompRn = Name
type instance XSynType'ForAll'Var CompTc = TcTyVar
type instance XSynType'ForAll'Var CompZn = ZnTyVar
type instance XSynType'ForAll'Body x = SynType x

type instance XSynType'Fun'Arg x = SynType x
type instance XSynType'Fun'Res x = SynType x

-- We record the type inside the 'Concrete',
-- not in the annotation.
type instance XSynType'Concrete x = Concrete

instance Pretty' (SynType CompRn) where
  pretty' = \case
    SynType'Var _ var -> pretty' var
    SynType'ForAll _ vars body -> "forall" <+> hsep (pretty' <$> vars) <> "." <+> pretty' body
    SynType'Fun _ ty1 ty2 -> parens (pretty' ty1) <+> "->" <+> pretty' ty2
    SynType'Concrete _ ty -> pretty' ty

instance Pretty' (SynType CompTc) where
  pretty' = \case
    SynType'Var _ var -> pretty' var
    SynType'ForAll _ vars body -> "forall" <+> hsep (pretty' <$> vars) <> "." <+> pretty' body
    SynType'Fun _ ty1 ty2 -> pretty' ty1 <+> "->" <+> pretty' ty2
    SynType'Concrete _ ty -> pretty' ty

instance Pretty' (SynType CompZn) where
  pretty' = \case
    SynType'Var _ var -> pretty' var
    SynType'ForAll _ vars body -> "forall" <+> hsep (pretty' <$> vars) <> "." <+> pretty' body
    SynType'Fun _ ty1 ty2 -> pretty' ty1 <+> "->" <+> pretty' ty2
    SynType'Concrete _ ty -> pretty' ty
