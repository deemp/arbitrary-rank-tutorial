module Language.Arralac.Driver.ReaderToZonker.Run where

import GHC.Stack (HasCallStack)
import Language.Arralac.Driver.ParserToZonker.Run
import Language.Arralac.Pass.Types
import Language.Arralac.Prelude.Pretty (CtxPrettyVerbosity)
import Language.Arralac.Prelude.Types (CtxDebug, FastFilePath)
import Language.Arralac.Reader.Run
import Language.Arralac.Solver.Types (CtxSolverIterations)
import Language.Arralac.Syntax.TTG.SynTerm

runReaderToZonker ::
  ( HasCallStack
  , CtxDebug
  , CtxPrettyVerbosity
  , CtxSolverIterations
  ) =>
  FastFilePath ->
  IO (SynTerm CompZn)
runReaderToZonker filePath = do
  fileContent <- runReader filePath
  runParserToZonker filePath fileContent
