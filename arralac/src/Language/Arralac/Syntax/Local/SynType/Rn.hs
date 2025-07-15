{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.SynType.Rn where

import Language.Arralac.Pass.Types
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.SynTypeConcrete()
import Language.Arralac.Syntax.TTG.SynType
import Prettyprinter (hsep, parens, (<+>))

type instance XSynType'Var CompRn = Name

type instance XSynType'ForAll'Var CompRn = Name
type instance XSynType'ForAll'Body CompRn = SynType CompRn

type instance XSynType'Fun'Arg CompRn = SynType CompRn
type instance XSynType'Fun'Res CompRn = SynType CompRn

instance Pretty' (SynType CompRn) where
  pretty' = \case
    SynType'Var _ var -> pretty' var
    SynType'ForAll _ vars body -> "forall" <+> hsep (pretty' <$> vars) <> "." <+> pretty' body
    SynType'Fun _ ty1 ty2 -> parens (pretty' ty1) <+> "->" <+> pretty' ty2
    SynType'Concrete _ ty -> pretty' ty
