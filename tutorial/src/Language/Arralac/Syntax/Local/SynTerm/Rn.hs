{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.SynTerm.Rn where

import Language.Arralac.Syntax.Local.SynLit
import Language.Arralac.Syntax.Local.SynTermVar.Rn ()
import Language.Arralac.Syntax.Local.SynType.Rn ()
import Language.Arralac.Syntax.TTG.Lit
import Language.Arralac.Syntax.TTG.SynTerm
import Language.Arralac.Syntax.TTG.SynTermVar
import Language.Arralac.Syntax.TTG.SynType
import Language.Arralac.Utils.Pass
import Language.Arralac.Utils.Pretty
import Prettyprinter

-- In GHC, the extension field for the variable AST node constructor
-- is set to NoExtField.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Expr.hs#L238
--
-- We set it to () and use the extension field
-- in other constructors for annotations.

type instance XSynTerm'Lit CompRn = SynLit

type instance XSynTerm'App'Fun CompRn = SynTerm CompRn
type instance XSynTerm'App'Arg CompRn = SynTerm CompRn

type instance XSynTerm'Lam'Var CompRn = XSynTerm'Var CompRn
type instance XSynTerm'Lam'Body CompRn = SynTerm CompRn

type instance XSynTerm'ALam'Var CompRn = XSynTerm'Var CompRn
type instance XSynTerm'ALam'Type CompRn = SynType CompRn
type instance XSynTerm'ALam'Body CompRn = SynTerm CompRn

type instance XSynTerm'Let'Name CompRn = XSynTerm'Var CompRn
type instance XSynTerm'Let'AssignedTerm CompRn = SynTerm CompRn
type instance XSynTerm'Let'InTerm CompRn = SynTerm CompRn

type instance XSynTerm'Ann'Term CompRn = SynTerm CompRn
type instance XSynTerm'Ann'Type CompRn = SynType CompRn

instance Pretty' (SynTerm CompRn) where
  pretty' = \case
    SynTerm'Var _ var -> pretty' var
    SynTerm'Lit _ val -> pretty' val
    SynTerm'App _ term1 term2 ->
      parens (pretty' term1) <+> pretty' term2
    SynTerm'Lam _ var term ->
      "\\"
        <> pretty' var
        <> "." <+> pretty' term
    SynTerm'ALam _ var ty term ->
      "\\"
        <> parens (pretty' var <+> "::" <+> pretty' ty)
        <> "." <+> pretty' term
    SynTerm'Let _ name term1 term2 ->
      "let" <+> pretty' name <+> "=" <+> pretty' term1 <+> "in" <+> pretty' term2
    SynTerm'Ann _ term ty ->
      parens (parens (pretty' term) <+> "::" <+> pretty' ty)
