module Language.Arralac.Examples where

import Control.Monad.Foil (emptyScope)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.Arralac.Driver.ReaderToZonker.Run (runReaderToZonker)
import Language.Arralac.Interpreter.Run
import Language.Arralac.Interpreter.Whnf (whnf)
import Language.Arralac.Parser.Error
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Reader.Error
import Language.Arralac.Renamer.Error (RnErrorWithCallStack)
import Language.Arralac.Solver.Error (SolverErrorWithCallStack)
import Language.Arralac.Typechecker.Error (TcErrorWithCallStack)
import Prettyprinter (line)
import Prettyprinter.Render.Text (putDoc)
import Prettyprinter.Util (putDocW)
import UnliftIO (catch)
import Prelude hiding (exp)

main :: IO ()
main = do
  let filePath = "test/data/Program1.stlc"
  let ?debug = True
      ?prettyVerbosity = PrettyVerbosity'Normal
      ?solverIterations = 10
  programZn <- do
    let prettyError x = putDocW 100 (pretty' x) >> error "Error!"
    runReaderToZonker filePath
      `catch` (\(x :: ReaderErrorWithCallStack) -> prettyError x)
      `catch` (\(x :: ParserErrorWithCallStack) -> prettyError x)
      `catch` (\(x :: RnErrorWithCallStack) -> prettyError x)
      `catch` (\(x :: TcErrorWithCallStack) -> prettyError x)
      `catch` (\(x :: SolverErrorWithCallStack) -> prettyError x)
  putDoc $ line <> prettyUser programZn <> line
  putDoc $ line <> prettyUser (runInterpreter InterpreterMode'Whnf programZn) <> line
