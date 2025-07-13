{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.SynTerm.Zn where

import Language.Arralac.Prelude.Pass
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Syntax.Local.Extension.Zn
import Language.Arralac.Syntax.Local.SynLit
import Language.Arralac.Syntax.Local.SynTermVar.Zn
import Language.Arralac.Syntax.Local.SynType.Zn ()
import Language.Arralac.Syntax.TTG.Lit
import Language.Arralac.Syntax.TTG.SynTerm
import Language.Arralac.Syntax.TTG.SynTermVar
import Language.Arralac.Syntax.TTG.SynType
import Prettyprinter

-- In GHC, the extension field for the variable AST node constructor
-- is set to NoExtField.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Expr.hs#L238
--
-- We set it to () and use the extension field
-- in other constructors for annotations.

type instance XSynTerm'Lit CompZn = SynLit

type instance XSynTerm'App'Fun CompZn = SynTerm CompZn
type instance XSynTerm'App'Arg CompZn = SynTerm CompZn

type instance XSynTerm'Lam'Var CompZn = XSynTerm'Var CompZn
type instance XSynTerm'Lam'Body CompZn = SynTerm CompZn

type instance XSynTerm'ALam'Var CompZn = XSynTerm'Var CompZn
type instance XSynTerm'ALam'Type CompZn = SynType CompZn
type instance XSynTerm'ALam'Body CompZn = SynTerm CompZn

type instance XSynTerm'Let'Name CompZn = XSynTerm'Var CompZn
type instance XSynTerm'Let'AssignedTerm CompZn = SynTerm CompZn
type instance XSynTerm'Let'InTerm CompZn = SynTerm CompZn

type instance XSynTerm'Ann'Term CompZn = SynTerm CompZn
type instance XSynTerm'Ann'Type CompZn = SynType CompZn

instance Pretty' (SynTerm CompZn) where
  pretty' = \case
    SynTerm'Var _ var -> pretty' var
    SynTerm'Lit _ val -> pretty' val
    SynTerm'App anno term1 term2 ->
      hsep
        [ parensIndent (parensIndent (pretty' term1) <> line <> indent 2 (pretty' term2))
        , "::"
        , pretty' anno.annoType
        ]
    SynTerm'Lam anno var term ->
      hsep
        [ parensIndent
            ( "\\"
                <> pretty' var
                <> "."
                <> line
                <> indent 2 (parensIndent (pretty' term))
            )
        , "::"
        , pretty' anno.annoType
        ]
    SynTerm'ALam anno var ty term ->
      hsep
        [ parens
            ( line
                <> "\\"
                <> parens
                  ( hsep
                      [ pretty' var.varName
                      , "::"
                      , braces (pretty' ty)
                      , "::"
                      , pretty' var.varType
                      ]
                  )
                <> "."
                  <+> pretty' term
                <> line
            )
        , "::"
        , pretty' anno.annoType
        ]
    SynTerm'Let anno var term1 term2 ->
      hsep
        [ parensIndent
            ( vsep
                [ "let"
                , indent
                    2
                    ( vsep
                        [ pretty' var.varName <+> "="
                        , indent
                            2
                            ( hsep
                                [ parensIndent (pretty' term1)
                                , "::"
                                , pretty' var.varType
                                ]
                            )
                        ]
                    )
                , "in"
                , indent 2 (pretty' term2)
                ]
            )
        , "::"
        , pretty' anno.annoType
        ]
    SynTerm'Ann anno term ty ->
      hsep
        [ parensIndent
            ( hsep
                [ parensIndent (pretty' term)
                , "::"
                , braces (pretty' ty)
                ]
            )
        , "::"
        , pretty' anno.annoType
        ]
