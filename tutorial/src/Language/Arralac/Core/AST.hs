module Language.Arralac.Core.AST where

import Control.Monad.Foil.Internal (DExt, Distinct, Name (..), Scope (..), identitySubst)
import Control.Monad.Free.Foil (AST (..), ScopedAST (..), substitute)
import Data.Bifunctor.TH (deriveBifunctor)
import Language.Arralac.Core.CoreNameBinder
import Language.Arralac.Prelude.Pass
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Prelude.Unique (Unique (unique))
import Language.Arralac.Syntax.Local.Name qualified as BT
import Language.Arralac.Syntax.Local.SynLit
import Language.Arralac.Syntax.Local.SynTerm.Zn ()
import Language.Arralac.Syntax.Local.SynTermVar.Zn
import Language.Arralac.Syntax.TTG.SynTerm
import Prettyprinter

-- TODO Delayed substitution and Normalization by Evaluation

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

pattern LitE :: SynLit -> AST binder Core n
pattern LitE x = Node (Core'Lit x)

pattern AppE :: AST binder Core n -> AST binder Core n -> AST binder Core n
pattern AppE x y = Node (Core'App x y)

pattern LamE :: binder n l -> AST binder Core l -> AST binder Core n
pattern LamE binder body = Node (Core'Lam (ScopedAST binder body))

pattern LetE :: binder n l -> AST binder Core n -> AST binder Core l -> AST binder Core n
pattern LetE binder body rhs = Node (Core'Let body (ScopedAST binder rhs))

{-# COMPLETE LitE, AppE, LamE, LetE #-}

type CoreE = AST CoreNameBinder Core

instance Pretty' (CoreE n) where
  pretty' = \case
    Node x -> case x of
      Core'Lit lit -> pretty' lit
      Core'App fun arg -> parens (pretty' fun) <+> parens (pretty' arg)
      Core'Lam (ScopedAST binder body) -> "\\" <> pretty' binder <> "." <+> pretty' body
      Core'Let body (ScopedAST binder rhs) -> "let" <+> pretty' binder <+> "=" <+> pretty' body <+> "in" <+> pretty' rhs
    Var x -> pretty' (PrettyName x)
