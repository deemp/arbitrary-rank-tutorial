module Language.Arralac.Driver.ReaderToZonker.Run where

import Data.IORef
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import Language.Arralac.Parser.Run (runParser)
import Language.Arralac.Prelude.Pass
import Language.Arralac.Prelude.Pretty (CtxPrettyVerbosity)
import Language.Arralac.Prelude.Types (CtxDebug, FastString)
import Language.Arralac.Renamer.Run (runRenamer)
import Language.Arralac.Solver.Types (CtxSolverIterations)
import Language.Arralac.Syntax.TTG.SynTerm
import Language.Arralac.Typechecker.Run (runTypechecker)
import Language.Arralac.Zonker.Zn.Zonk (Zonk (..))

runReaderToZonker ::
  ( HasCallStack
  , CtxDebug
  , CtxPrettyVerbosity
  , CtxSolverIterations
  ) =>
  FastString ->
  T.Text ->
  IO (SynTerm CompZn)
runReaderToZonker filePath content = do
  uniqueSupply <- newIORef @Int 0
  let ?uniqueSupply = uniqueSupply
      ?currentFilePath = filePath
   in do
        parsed <- runParser content
        program <- runRenamer parsed
        typechecked <- runTypechecker program
        zonk typechecked
