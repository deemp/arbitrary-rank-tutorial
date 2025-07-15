module Language.Arralac.Typechecker.TcTyVarEnv where

import Data.Map qualified as Map
import GHC.Stack (HasCallStack)
import Language.Arralac.Type.Local.RnVar
import Language.Arralac.Type.Local.Type
import Language.Arralac.Typechecker.Error (CtxTcErrorPropagated, TcError (..), dieTc)

-- ===========================
-- [Type variable environment]
-- ===========================

newtype TcTyVarEnv = TcTyVarEnv {env :: Map.Map RnVar Sigma}

type CtxTcTyVarEnv = (?tcTyVarEnv :: TcTyVarEnv)

emptyTcTyVarEnv :: TcTyVarEnv
emptyTcTyVarEnv = TcTyVarEnv mempty

lookupTcTyVarEnv :: RnVar -> TcTyVarEnv -> Maybe Sigma
lookupTcTyVarEnv k = Map.lookup k . (.env)

toAscListTcTyVarEnv :: TcTyVarEnv -> [(RnVar, Sigma)]
toAscListTcTyVarEnv = Map.toAscList . (.env)

insertTcTyVarEnv :: RnVar -> Sigma -> TcTyVarEnv -> TcTyVarEnv
insertTcTyVarEnv k v = TcTyVarEnv . Map.insert k v . (.env)

extendTcTyVarEnv :: (HasCallStack, CtxTcTyVarEnv) => RnVar -> Sigma -> ((HasCallStack, CtxTcTyVarEnv) => IO a) -> IO a
extendTcTyVarEnv var ty tcAction =
  let ?tcTyVarEnv = insertTcTyVarEnv var ty ?tcTyVarEnv
   in tcAction

lookupTcTyVarType :: (HasCallStack, CtxTcErrorPropagated, CtxTcTyVarEnv) => RnVar -> IO Sigma -- May fail
lookupTcTyVarType n =
  case lookupTcTyVarEnv n ?tcTyVarEnv of
    Just ty -> pure ty
    Nothing -> dieTc (TcError'UndefinedVariable n)
