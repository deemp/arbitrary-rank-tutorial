{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.SynTerm where

import Language.Arralac.Syntax.Local.Extension
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.SynType ()
import Language.Arralac.Syntax.Local.Var.Tc
import Language.Arralac.Syntax.Local.Var.Zn
import Language.Arralac.Syntax.TTG.SynTerm
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

type instance XSynTerm'VarCommon CompRn = Name
type instance XSynTerm'VarCommon CompTc = TcTermVar
type instance XSynTerm'VarCommon CompZn = ZnTermVar

type instance XSynTerm'Var x = XSynTerm'VarCommon x

type instance XSynTerm'Lit x = SynLit

type instance XSynTerm'App'Fun x = SynTerm x
type instance XSynTerm'App'Arg x = SynTerm x

type instance XSynTerm'Lam'Var x = XSynTerm'VarCommon x
type instance XSynTerm'Lam'Body x = SynTerm x

type instance XSynTerm'ALam'Var x = XSynTerm'VarCommon x
type instance XSynTerm'ALam'Type x = SynType x
type instance XSynTerm'ALam'Body x = SynTerm x

type instance XSynTerm'Let'Name x = XSynTerm'VarCommon x
type instance XSynTerm'Let'AssignedTerm x = SynTerm x
type instance XSynTerm'Let'InTerm x = SynTerm x

type instance XSynTerm'Ann'Term x = SynTerm x
type instance XSynTerm'Ann'Type x = SynType x

instance Pretty' (SynTerm CompRn) where
  pretty' = \case
    SynTerm'Var _ var -> pretty' var
    SynTerm'Lit _ val -> pretty' val
    SynTerm'App _ term1 term2 -> parens (pretty' term1) <+> pretty' term2
    SynTerm'Lam _ var term -> "\\" <> pretty' var <> "." <+> pretty' term
    SynTerm'ALam _ var ty term -> "\\" <> parens (pretty' var <+> "::" <+> pretty' ty) <> "." <+> pretty' term
    SynTerm'Let _ name term1 term2 -> "let" <+> pretty' name <+> "=" <+> pretty' term1 <+> "in" <+> pretty' term2
    SynTerm'Ann _ term ty -> parens (parens (pretty' term) <+> "::" <+> pretty' ty)

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
