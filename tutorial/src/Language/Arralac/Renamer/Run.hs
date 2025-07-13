module Language.Arralac.Renamer.Run where

import Data.Map qualified as Map
import GHC.Stack (HasCallStack)
import Language.Arralac.Parser.Abs qualified as Abs
import Language.Arralac.Prelude.Pass
import Language.Arralac.Prelude.Types
import Language.Arralac.Prelude.Unique.Supply (CtxUniqueSupply)
import Language.Arralac.Renamer.ConvertParsed
import Language.Arralac.Syntax.TTG.SynTerm

runRenamer ::
  ( HasCallStack
  , CtxInputFilePath
  , CtxDebug
  , CtxUniqueSupply
  ) =>
  Abs.Program -> IO (SynTerm CompRn)
runRenamer a = do
  let
    ?termVarScope = Map.empty
    ?tyVarScope = Map.empty
    ?tyConcreteScope = Map.empty
    ?letOccursCheckInfo = Nothing
  convertParsed a
