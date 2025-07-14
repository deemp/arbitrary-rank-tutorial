module Language.Arralac.CLI.Commands where

import Language.Arralac.Interpreter.Run (InterpreterMode (..))
import Language.Arralac.Prelude.Types
import Options.Applicative

data CLI
  = CLI'LanguageServer Command'LanguageServer
  | CLI'Typecheck Command'Typecheck
  | CLI'Interpret Command'Interpret

data Command'LanguageServer = Command'LanguageServer
  { settingsSectionName :: Maybe FastString
  }

data Command'Typecheck = Command'Typecheck
  { inputFilePath :: FastFilePath
  , solverIterations :: Maybe Int
  , debug :: Bool
  }

data Command'Interpret = Command'Interpret
  { inputFilePath :: FastFilePath
  , solverIterations :: Maybe Int
  , interpreterMode :: InterpreterMode
  , debug :: Bool
  }

data ParseCommand = ParseCommand
  { languageServer :: Parser Command'LanguageServer
  , typecheck :: Parser Command'Typecheck
  , interpret :: Parser Command'Interpret
  }

data CommandParserInfo = CommandParserInfo
  { languageServer :: ParserInfo CLI
  , typecheck :: ParserInfo CLI
  , interpret :: ParserInfo CLI
  }
