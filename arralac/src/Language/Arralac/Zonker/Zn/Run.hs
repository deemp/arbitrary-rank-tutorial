module Language.Arralac.Zonker.Zn.Run where

import GHC.Stack (HasCallStack)
import Language.Arralac.Pass.Types
import Language.Arralac.Prelude.Pretty (CtxPrettyVerbosity)
import Language.Arralac.Prelude.Types (CtxDebug)
import Language.Arralac.Syntax.TTG.SynTerm
import Language.Arralac.Zonker.Zn.Zonk

runZonker ::
  ( HasCallStack
  , CtxDebug
  , CtxPrettyVerbosity
  ) =>
  SynTerm CompTc -> IO (SynTerm CompZn)
runZonker = zonk