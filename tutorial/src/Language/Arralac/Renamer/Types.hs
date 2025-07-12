module Language.Arralac.Renamer.Types where

import Data.Map
import GHC.Stack (HasCallStack)
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Utils.Types
import Language.Arralac.Utils.Unique
import Language.Arralac.Utils.Unique.Supply

-- | A name.
type NameFs = FastString

-- | Variables scope.
--
-- Variable names and their ids.
type Scope = Map NameFs Unique

-- | Current term variables scope.
--
-- Visible term variable names and their ids.
type ITermVarScope = (?termVarScope :: Scope)

-- | Current type variables scope.
--
-- Visible type variable names and their ids.
type ITyVarScope = (?tyVarScope :: Scope)

-- | Concrete types scope.
--
-- Concrete type names and their ids.
type ITyConcreteScope = (?tyConcreteScope :: Scope)

data LetOccursCheckInfo = LetOccursCheckInfo
  { letSrcSpan :: SrcSpan
  , letLhs :: Name
  }

type ILetOccursCheckInfo = (?letOccursCheckInfo :: Maybe LetOccursCheckInfo)

type IRnScopes = (ITermVarScope, ITyVarScope, ITyConcreteScope)

type IRnConstraints =
  ( HasCallStack
  , IUniqueSupply
  , ICurrentFilePath
  , IDebug
  , IRnScopes
  , ILetOccursCheckInfo
  )

type RnM a = (IRnConstraints) => IO a