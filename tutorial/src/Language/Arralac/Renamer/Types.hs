module Language.Arralac.Renamer.Types where

import Data.Map
import GHC.Stack (HasCallStack)
import Language.Arralac.Prelude.Types
import Language.Arralac.Prelude.Unique
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.RnVar

-- | A name.
type NameFs = FastString

-- | Variables scope.
--
-- Variable names and their ids.
type Scope = Map NameFs Unique

-- | Current term variables scope.
--
-- Visible term variable names and their ids.
type CtxTermVarScope = (?termVarScope :: Scope)

-- | Current type variables scope.
--
-- Visible type variable names and their ids.
type CtxTyVarScope = (?tyVarScope :: Scope)

-- | Concrete types scope.
--
-- Concrete type names and their ids.
type CtxTyConcreteScope = (?tyConcreteScope :: Scope)

data LetOccursCheckInfo = LetOccursCheckInfo
  { letSrcSpan :: SrcSpan
  , letLhs :: RnVar
  }

type CtxLetOccursCheckInfo = (?letOccursCheckInfo :: Maybe LetOccursCheckInfo)

type CtxRnScopes = (CtxTermVarScope, CtxTyVarScope, CtxTyConcreteScope)

type CtxRnConstraints =
  ( HasCallStack
  , CtxUniqueSupply
  , CtxCurrentFilePath
  , CtxDebug
  , CtxRnScopes
  , CtxLetOccursCheckInfo
  )

type RnM a = (CtxRnConstraints) => IO a
