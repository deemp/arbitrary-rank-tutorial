module Language.Arralac.Driver.ParserToZonker.Run where

import Data.IORef (newIORef)
import GHC.Stack (HasCallStack)
import Language.Arralac.Parser.Run
import Language.Arralac.Prelude.Pass
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Prelude.Types
import Language.Arralac.Renamer.Run
import Language.Arralac.Solver.Types
import Language.Arralac.Syntax.TTG.SynTerm
import Language.Arralac.Typechecker.Run
import Language.Arralac.Zonker.Zn.Run (runZonker)

runParserToZonker ::
  ( HasCallStack
  , CtxDebug
  , CtxPrettyVerbosity
  , CtxSolverIterations
  ) =>
  FastFilePath ->
  FastString ->
  IO (SynTerm CompZn)
runParserToZonker filePath fileContent = do
  uniqueSupply <- newIORef @Int 0
  let ?uniqueSupply = uniqueSupply
      ?inputFilePath = filePath
  do
    parsed <- runParser fileContent
    program <- runRenamer parsed
    typechecked <- runTypechecker program
    runZonker typechecked
