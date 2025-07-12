module Language.Arralac.Renamer.Run where

import Data.Map qualified as Map
import GHC.Stack (HasCallStack)
import Language.Arralac.Renamer.ConvertRename
import Language.Arralac.Utils.Types
import Language.Arralac.Utils.Unique.Supply (CtxUniqueSupply)

convertRenameAbs ::
  ( HasCallStack
  , CtxCurrentFilePath
  , CtxDebug
  , CtxUniqueSupply
  , ConvertRename a
  ) =>
  a -> ConvertRenameTo a
convertRenameAbs a = do
  let
    ?termVarScope = Map.empty
    ?tyVarScope = Map.empty
    -- TODO put existing types here?
    ?tyConcreteScope = Map.empty
    ?letOccursCheckInfo = Nothing
  convertRename a