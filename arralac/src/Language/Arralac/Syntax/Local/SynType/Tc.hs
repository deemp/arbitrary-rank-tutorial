{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.SynType.Tc where

import Language.Arralac.Pass.Types
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.TyVar.Tc
import Language.Arralac.Syntax.Local.TyVar.Zn (ZnTyVar)
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.TTG.SynType
import Prettyprinter (hsep, (<+>))

type instance XSynType'Var CompRn = Name
type instance XSynType'Var CompTc = TcTyVar
type instance XSynType'Var CompZn = ZnTyVar

type instance XSynType'ForAll'Var CompRn = Name
type instance XSynType'ForAll'Var CompTc = TcTyVar
type instance XSynType'ForAll'Var CompZn = ZnTyVar
type instance XSynType'ForAll'Body x = SynType x

type instance XSynType'Fun'Arg x = SynType x
type instance XSynType'Fun'Res x = SynType x

type instance XSynType'Concrete x = Concrete

instance Pretty' (SynType CompTc) where
  pretty' = \case
    SynType'Var _ var -> pretty' var
    SynType'ForAll _ vars body -> "forall" <+> hsep (pretty' <$> vars) <> "." <+> pretty' body
    SynType'Fun _ ty1 ty2 -> pretty' ty1 <+> "->" <+> pretty' ty2
    SynType'Concrete _ ty -> pretty' ty
