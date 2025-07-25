module Language.Arralac.CLI.Handle where

import Data.Maybe (fromMaybe)
import Language.Arralac.CLI.Commands
import Language.Arralac.CLI.Defaults (Defaults (..))
import Language.Arralac.Driver.ReaderToZonker.Run
import Language.Arralac.Evaluator.Run
import Language.Arralac.LanguageServer.Run
import Language.Arralac.Prelude.Pretty
import Prettyprinter
import Prettyprinter.Util
import System.Exit

handleCLI :: Defaults -> CLI -> IO ()
handleCLI defaults = \case
  CLI'LanguageServer cmd -> do
    exitCode <-
      runLanguageServer
        (fromMaybe defaults.settingsSectionName cmd.settingsSectionName)
    exitWith exitCode
  CLI'Typecheck cmd -> do
    let
      ?debug = cmd.debug
      ?prettyVerbosity = defaults.prettyVerbosity
      ?solverIterations = defaults.solverIterations
    do
      znTerm <- runReaderToZonker cmd.inputFilePath
      putDocW defaultPrettyWidth $ pretty' znTerm <> line
  CLI'Evaluate cmd -> do
    let
      ?debug = cmd.debug
      ?prettyVerbosity = defaults.prettyVerbosity
      ?solverIterations = defaults.solverIterations
    do
      znTerm <- runReaderToZonker cmd.inputFilePath
      let evaluatedTerm = runEvaluator EvaluatorMode'Whnf znTerm
      putDocW defaultPrettyWidth $ pretty' evaluatedTerm <> line
