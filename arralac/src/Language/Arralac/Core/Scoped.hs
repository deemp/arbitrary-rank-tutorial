module Language.Arralac.Core.Scoped where

import Control.Monad.Foil
import Control.Monad.Free.Foil (AST (..), ScopedAST (..))
import Data.Bifunctor.TH (deriveBifunctor)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Language.Arralac.Core.CoreNameBinder
import Language.Arralac.Core.PrettyScoped
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.SynLit
import Language.Arralac.Syntax.Local.SynTerm.Zn ()
import Prettyprinter

-- | Core AST.
--
-- Similar to @Expr@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core.hs#L253
--
-- However, this version is untyped for simplicity.
data Core scope term
  = Core'Lam scope
  | Core'App term term
  | -- | Non-recursive @let@.
    --
    -- @let a = b in c@.
    --
    -- @a@ may not occur in @b@.
    Core'Let term scope
  | Core'Lit SynLit
  deriving stock (Functor)

deriveBifunctor ''Core

pattern SCore'Lit :: SynLit -> AST binder Core n
pattern SCore'Lit x = Node (Core'Lit x)

pattern SCore'App :: AST binder Core n -> AST binder Core n -> AST binder Core n
pattern SCore'App x y = Node (Core'App x y)

pattern SCore'Lam :: binder n l -> AST binder Core l -> AST binder Core n
pattern SCore'Lam binder body = Node (Core'Lam (ScopedAST binder body))

pattern SCore'Let :: binder n l -> AST binder Core n -> AST binder Core l -> AST binder Core n
pattern SCore'Let binder body rhs = Node (Core'Let body (ScopedAST binder rhs))

{-# COMPLETE SCore'Lit, SCore'App, SCore'Lam, SCore'Let #-}

type SCore = AST CoreNameBinder Core

instance PrettyScoped (SCore n) where
  prettyScoped = \case
    Node x -> case x of
      Core'Lit lit -> pretty' lit
      Core'App fun arg ->
        parens (prettyScoped fun) <+> parens (prettyScoped arg)
      Core'Lam (ScopedAST binder body) ->
        "\\"
          <> pretty' binder
          <> "."
            <+> withExtendedPrettyScope binder (prettyScoped body)
      Core'Let body (ScopedAST binder rhs) ->
        "let"
          <+> pretty' binder
          <+> "="
          <+> prettyScoped body
          <+> "in"
          <+> withExtendedPrettyScope binder (prettyScoped rhs)
    Var x ->
      pretty' ((Map.lookup (nameId x) ?prettyScope) <&> (.nameOcc.occNameFS))
        <> "_"
        <> pretty' (nameId x)

instance Pretty' (SCore n) where
  pretty' = let ?prettyScope = Map.empty in prettyScoped
