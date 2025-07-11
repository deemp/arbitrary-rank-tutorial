module Language.STLC.Typing.Jones2007.Main where

import Control.Monad.Foil (emptyScope)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.STLC.Interpreter.Main (convertASTToCore, whnf)
import Language.STLC.Typing.Jones2007.BasicTypes (Pretty' (..), PrettyVerbosity (PrettyVerbosity'Normal), prettyUser)
import Language.STLC.Typing.Jones2007.Solver (SolverErrorWithCallStack)
import Language.STLC.Typing.Jones2007.TcMonad (TcErrorWithCallStack)
import Language.STLC.Typing.Renamer (RnErrorWithCallStack)
import Language.STLC.Typing.Run (runTypechecker')
import Prettyprinter (line)
import Prettyprinter.Render.Text (putDoc)
import Prettyprinter.Util (putDocW)
import UnliftIO (catch)
import Prelude hiding (exp)

main :: IO ()
main = do
  let filePath = "test/data/Program1.stlc"
  content <- T.readFile filePath
  let ?debug = True
      ?prettyVerbosity = PrettyVerbosity'Normal
      ?solverIterations = 10
  programZn <- do
    let prettyError x = putDocW 100 (pretty' x) >> error "Error!"
    runTypechecker' (T.pack filePath) content
      `catch` (\(x :: RnErrorWithCallStack) -> prettyError x)
      `catch` (\(x :: TcErrorWithCallStack) -> prettyError x)
      `catch` (\(x :: SolverErrorWithCallStack) -> prettyError x)
  putDoc $ line <> prettyUser programZn <> line
  putDoc $ line <> prettyUser (whnf emptyScope (convertASTToCore emptyScope programZn)) <> line
