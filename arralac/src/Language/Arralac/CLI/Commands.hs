module Language.Arralac.CLI.Commands where

import Language.Arralac.Evaluator.Run (EvaluatorMode (..))
import Language.Arralac.Prelude.Types
import Options.Applicative

data CLI
  = CLI'LanguageServer Command'LanguageServer
  | CLI'Typecheck Command'Typecheck
  | CLI'Evaluate Command'Evaluate

data Command'LanguageServer = Command'LanguageServer
  { settingsSectionName :: Maybe FastString
  }

data Command'Typecheck = Command'Typecheck
  { inputFilePath :: FastFilePath
  , solverIterations :: Maybe Int
  , debug :: Bool
  }

data Command'Evaluate = Command'Evaluate
  { inputFilePath :: FastFilePath
  , solverIterations :: Maybe Int
  , evaluatorMode :: EvaluatorMode
  , debug :: Bool
  }

data ParseCommand = ParseCommand
  { languageServer :: Parser Command'LanguageServer
  , typecheck :: Parser Command'Typecheck
  , evaluate :: Parser Command'Evaluate
  }

data CommandParserInfo = CommandParserInfo
  { languageServer :: ParserInfo CLI
  , typecheck :: ParserInfo CLI
  , evaluate :: ParserInfo CLI
  }
