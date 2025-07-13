{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.SynTerm.Tc where

import Language.Arralac.Prelude.Pass
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Syntax.Local.Extension.Tc
import Language.Arralac.Syntax.Local.SynLit
import Language.Arralac.Syntax.Local.SynTermVar.Tc
import Language.Arralac.Syntax.Local.SynType.Tc ()
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

type instance XSynTerm'Lit CompTc = SynLit

type instance XSynTerm'App'Fun CompTc = SynTerm CompTc
type instance XSynTerm'App'Arg CompTc = SynTerm CompTc

type instance XSynTerm'Lam'Var CompTc = XSynTerm'Var CompTc
type instance XSynTerm'Lam'Body CompTc = SynTerm CompTc

type instance XSynTerm'ALam'Var CompTc = XSynTerm'Var CompTc
type instance XSynTerm'ALam'Type CompTc = SynType CompTc
type instance XSynTerm'ALam'Body CompTc = SynTerm CompTc

type instance XSynTerm'Let'Name CompTc = XSynTerm'Var CompTc
type instance XSynTerm'Let'AssignedTerm CompTc = SynTerm CompTc
type instance XSynTerm'Let'InTerm CompTc = SynTerm CompTc

type instance XSynTerm'Ann'Term CompTc = SynTerm CompTc
type instance XSynTerm'Ann'Type CompTc = SynType CompTc

instance Pretty' (SynTerm CompTc) where
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
