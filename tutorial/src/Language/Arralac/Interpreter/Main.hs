module Language.Arralac.Interpreter.Main where

import Control.Monad.Foil.Internal (DExt, Distinct, Name (..), Scope (..), identitySubst)
import Control.Monad.Free.Foil (AST (..), ScopedAST (..), substitute)
import Data.Bifunctor.TH (deriveBifunctor)
import Language.Arralac.Interpreter.FreeFoil (CoreNameBinder (..), PrettyName (..), addSubst, extendScope, withFreshUsingUnique)
import Language.Arralac.Syntax.Local.Name qualified as BT
import Language.Arralac.Syntax.Local.SynTerm ()
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.TTG.SynTerm
import Language.Arralac.Utils.Pretty
import Language.Arralac.Utils.Types.Pass
import Language.Arralac.Utils.Unique (Unique (unique))
import Prettyprinter

-- TODO Delayed substitution and Normalization by Evaluation

data Core scope term
  = Core'Lit SynLit
  | Core'App term term
  | Core'Lam scope
  | -- | Non-recursive @let@.
    --
    -- @let a = b in c@.
    --
    -- @a@ may not occur in @b@.
    Core'Let term scope
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

litE :: SynLit -> AST binder Core n
litE = LitE

appE :: CoreE n -> CoreE n -> CoreE n
appE = AppE

lamE :: Scope n -> BT.Name -> (forall l. (DExt n l) => Scope l -> CoreE l) -> CoreE n
lamE scope name' mkBody = withFreshUsingUnique scope name' $ \x ->
  let scope' = extendScope x scope
   in LamE x (mkBody scope')

letE :: Scope n -> BT.Name -> CoreE n -> (forall l. (DExt n l) => Scope l -> CoreE l) -> CoreE n
letE scope name' body mkRhs = withFreshUsingUnique scope name' $ \x ->
  let scope' = extendScope x scope
   in LetE x body (mkRhs scope')

convertASTToCore :: Scope n -> SynTerm CompZn -> CoreE n
convertASTToCore scope = \case
  SynTerm'Lit _ lit ->
    Node (Core'Lit lit)
  SynTerm'App _ fun arg ->
    appE (convertASTToCore scope fun) (convertASTToCore scope arg)
  SynTerm'Lam _ var body ->
    lamE scope var.varName (\scope' -> convertASTToCore scope' body)
  SynTerm'ALam _ var _ body ->
    lamE scope var.varName (\scope' -> convertASTToCore scope' body)
  SynTerm'Let _ var body rhs ->
    letE scope var.varName (convertASTToCore scope body) (\scope' -> convertASTToCore scope' rhs)
  SynTerm'Ann _ term _ ->
    convertASTToCore scope term
  SynTerm'Var _ var -> Var (UnsafeName var.varName.nameUnique.unique)

whnf :: (Distinct n) => Scope n -> CoreE n -> CoreE n
whnf scope = \case
  AppE fun arg ->
    case whnf scope fun of
      LamE binder body ->
        let subst = addSubst identitySubst binder arg
         in whnf scope (substitute scope subst body)
      fun' -> AppE fun' arg
  LetE binder body rhs ->
    let subst = addSubst identitySubst binder body
     in whnf scope (substitute scope subst rhs)
  t -> t

instance Pretty' (CoreE n) where
  pretty' = \case
    Node x -> case x of
      Core'Lit lit -> pretty' lit
      Core'App fun arg -> parens (pretty' fun) <+> parens (pretty' arg)
      Core'Lam (ScopedAST binder body) -> "\\" <> pretty' binder <> "." <+> pretty' body
      Core'Let body (ScopedAST binder rhs) -> "let" <+> pretty' binder <+> "=" <+> pretty' body <+> "in" <+> pretty' rhs
    Var x -> pretty' (PrettyName x)
