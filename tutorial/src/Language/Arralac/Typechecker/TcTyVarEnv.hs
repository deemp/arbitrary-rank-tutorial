module Language.Arralac.Typechecker.TcTyVarEnv where

import Data.Map qualified as Map
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Typechecker.Error (CtxTcErrorPropagated, TcError (..), dieTc)

-- ===========================
-- [Type variable environment]
-- ===========================

newtype TcTyVarEnv = TcTyVarEnv {env :: Map.Map Name Sigma}

emptyTcTyVarEnv :: TcTyVarEnv
emptyTcTyVarEnv = TcTyVarEnv mempty

lookupTcTyVarEnv :: Name -> TcTyVarEnv -> Maybe Sigma
lookupTcTyVarEnv k = Map.lookup k . (.env)

toAscListTcTyVarEnv :: TcTyVarEnv -> [(Name, Sigma)]
toAscListTcTyVarEnv = Map.toAscList . (.env)

insertTcTyVarEnv :: Name -> Sigma -> TcTyVarEnv -> TcTyVarEnv
insertTcTyVarEnv k v = TcTyVarEnv . Map.insert k v . (.env)

extendTcTyVarEnv :: (CtxTcTyVarEnv) => Name -> Sigma -> ((CtxTcTyVarEnv) => IO a) -> IO a
extendTcTyVarEnv var ty tcAction =
  let ?tcTyVarEnv = insertTcTyVarEnv var ty ?tcTyVarEnv in tcAction

lookupTcTyVarType :: (CtxTcErrorPropagated, CtxTcTyVarEnv) => Name -> IO Sigma -- May fail
lookupTcTyVarType n =
  case lookupTcTyVarEnv n ?tcTyVarEnv of
    Just ty -> pure ty
    Nothing -> dieTc (TcError'UndefinedVariable n)

type CtxTcTyVarEnv = (?tcTyVarEnv :: TcTyVarEnv)
