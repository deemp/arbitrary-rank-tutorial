module Language.Arralac.Examples where

import Language.Arralac.Driver.ReaderToZonker.Run (runReaderToZonker)
import Language.Arralac.Evaluator.Run
import Language.Arralac.Parser.Error
import Language.Arralac.Prelude.Debug
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Reader.Error
import Language.Arralac.Renamer.Error (RnErrorWithCallStack)
import Language.Arralac.Solver.Error (SolverErrorWithCallStack)
import Language.Arralac.Typechecker.Error (TcErrorWithCallStack)
import Prettyprinter.Util (putDocW)
import UnliftIO (catch)

main :: IO ()
main = do
  let filePath = "test/data/Program1.arralac"
  let ?debug = True
      ?prettyVerbosity = PrettyVerbosity'Normal
      ?solverIterations = 10
  programZn <- do
    let prettyError x = putDocW defaultPrettyWidth (pretty' x) >> error "Error!"
    runReaderToZonker filePath
      `catch` (\(x :: ReaderErrorWithCallStack) -> prettyError x)
      `catch` (\(x :: ParserErrorWithCallStack) -> prettyError x)
      `catch` (\(x :: RnErrorWithCallStack) -> prettyError x)
      `catch` (\(x :: TcErrorWithCallStack) -> prettyError x)
      `catch` (\(x :: SolverErrorWithCallStack) -> prettyError x)
  debug'
    "main"
    [ ("programZn", prettyCompact programZn)
    , ("whnf", prettyUser (runEvaluator EvaluatorMode'Whnf programZn))
    ]
