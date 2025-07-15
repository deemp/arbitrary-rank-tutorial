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

convertZonked :: Scope n -> SynTerm CompZn -> SCore n
convertZonked scope = \case
  SynTerm'Lit _ lit ->
    Node (Core'Lit lit)
  SynTerm'App _ fun arg ->
    sCoreApp (convertZonked scope fun) (convertZonked scope arg)
  SynTerm'Lam _ var body ->
    sCoreLam scope var.varName (\scope' -> convertZonked scope' body)
  SynTerm'ALam _ var _ body ->
    sCoreLam scope var.varName (\scope' -> convertZonked scope' body)
  SynTerm'Let _ var body rhs ->
    sCoreLet scope var.varName (convertZonked scope body) (\scope' -> convertZonked scope' rhs)
  SynTerm'Ann _ term _ ->
    convertZonked scope term
  SynTerm'Var _ var -> Var (UnsafeName var.varName.nameUnique.unique)

sCoreLit :: SynLit -> AST binder Core n
sCoreLit = SCore'Lit

sCoreApp :: SCore n -> SCore n -> SCore n
sCoreApp = SCore'App

sCoreLam :: Scope n -> Name.Name -> (forall l. (DExt n l) => Scope l -> SCore l) -> SCore n
sCoreLam scope name' mkBody = withFreshUsingUnique scope name' $ \x ->
  let scope' = CNB.extendScope x scope
   in SCore'Lam x (mkBody scope')

sCoreLet :: Scope n -> Name.Name -> SCore n -> (forall l. (DExt n l) => Scope l -> SCore l) -> SCore n
sCoreLet scope name' body mkRhs = withFreshUsingUnique scope name' $ \x ->
  let scope' = CNB.extendScope x scope
   in SCore'Let x body (mkRhs scope')
