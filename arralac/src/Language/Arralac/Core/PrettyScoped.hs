module Language.Arralac.Core.PrettyScoped where

import Control.Monad.Foil.Internal
import Data.Map qualified as Map
import GHC.Stack (HasCallStack)
import Language.Arralac.Core.CoreNameBinder
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Syntax.Local.Name qualified as SynName
import Prettyprinter

type PrettyScope = Map.Map RawName SynName.Name

type CtxPrettyScope = (?prettyScope :: PrettyScope)

type CtxPrettyScoped =
  ( HasCallStack
  , CtxPrettyVerbosity
  , CtxPrettyScope
  )

class PrettyScoped a where
  prettyScoped :: (CtxPrettyScoped) => a -> Doc ann

withExtendedPrettyScope :: (CtxPrettyScoped) => CoreNameBinder n l -> ((CtxPrettyScoped) => a) -> a
withExtendedPrettyScope binder act =
  let ?prettyScope = Map.insert (getCoreNameBinderRawName binder) binder.name ?prettyScope
   in act
