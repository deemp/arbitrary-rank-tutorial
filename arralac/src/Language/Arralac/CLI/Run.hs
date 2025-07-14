module Language.Arralac.CLI.Run where

import Data.Version (showVersion)
import Language.Arralac.CLI.Defaults
import Language.Arralac.CLI.Handle (handleCLI)
import Language.Arralac.CLI.Parse (cliParserInfo)
import Options.Applicative (ParserPrefs, customExecParser)
import Options.Applicative.Builder (prefs, showHelpOnEmpty, showHelpOnError)
import Paths_arralac (version)

parserPrefs :: ParserPrefs
parserPrefs = prefs (showHelpOnEmpty <> showHelpOnError)

runCli :: IO ()
runCli = do
  command <- customExecParser parserPrefs (cliParserInfo (showVersion version))
  handleCLI defaults command