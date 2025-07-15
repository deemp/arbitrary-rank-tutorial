module Language.Arralac.Core.ConvertZonked where

import Control.Monad.Foil
import Control.Monad.Foil.Internal
import Control.Monad.Free.Foil
import Language.Arralac.Core.AST
import Language.Arralac.Core.CoreNameBinder as CNB
import Language.Arralac.Pass.Types
import Language.Arralac.Prelude.Unique
import Language.Arralac.Syntax.Local.Name qualified as Name
import Language.Arralac.Syntax.Local.SynLit
import Language.Arralac.Syntax.Local.SynTermVar.Zn
import Language.Arralac.Syntax.TTG.SynTerm

convertZonked :: Scope n -> SynTerm CompZn -> CoreE n
convertZonked scope = \case
  SynTerm'Lit _ lit ->
    Node (Core'Lit lit)
  SynTerm'App _ fun arg ->
    appE (convertZonked scope fun) (convertZonked scope arg)
  SynTerm'Lam _ var body ->
    lamE scope var.varName (\scope' -> convertZonked scope' body)
  SynTerm'ALam _ var _ body ->
    lamE scope var.varName (\scope' -> convertZonked scope' body)
  SynTerm'Let _ var body rhs ->
    letE scope var.varName (convertZonked scope body) (\scope' -> convertZonked scope' rhs)
  SynTerm'Ann _ term _ ->
    convertZonked scope term
  SynTerm'Var _ var -> Var (UnsafeName var.varName.nameUnique.unique)

litE :: SynLit -> AST binder Core n
litE = LitE

appE :: CoreE n -> CoreE n -> CoreE n
appE = AppE

lamE :: Scope n -> Name.Name -> (forall l. (DExt n l) => Scope l -> CoreE l) -> CoreE n
lamE scope name' mkBody = withFreshUsingUnique scope name' $ \x ->
  let scope' = CNB.extendScope x scope
   in LamE x (mkBody scope')

letE :: Scope n -> Name.Name -> CoreE n -> (forall l. (DExt n l) => Scope l -> CoreE l) -> CoreE n
letE scope name' body mkRhs = withFreshUsingUnique scope name' $ \x ->
  let scope' = CNB.extendScope x scope
   in LetE x body (mkRhs scope')
