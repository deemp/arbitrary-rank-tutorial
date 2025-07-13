{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.SynType.Zn where

import Language.Arralac.Syntax.Local.TyVar.Zn (ZnTyVar)
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.TTG.SynType
import Language.Arralac.Utils.Pass
import Language.Arralac.Utils.Pretty
import Prettyprinter (hsep, (<+>))

type instance XSynType'Var CompZn = ZnTyVar

type instance XSynType'ForAll'Var CompZn = ZnTyVar
type instance XSynType'ForAll'Body CompZn = SynType CompZn

type instance XSynType'Fun'Arg CompZn = SynType CompZn
type instance XSynType'Fun'Res CompZn = SynType CompZn

type instance XSynType'Concrete CompZn = Concrete

instance Pretty' (SynType CompZn) where
  pretty' = \case
    SynType'Var _ var -> pretty' var
    SynType'ForAll _ vars body -> "forall" <+> hsep (pretty' <$> vars) <> "." <+> pretty' body
    SynType'Fun _ ty1 ty2 -> pretty' ty1 <+> "->" <+> pretty' ty2
    SynType'Concrete _ ty -> pretty' ty
