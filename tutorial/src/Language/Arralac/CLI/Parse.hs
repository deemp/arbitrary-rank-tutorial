module Language.Arralac.CLI.Parse where

import Data.Functor ((<&>))
import Language.Arralac.CLI.Commands
import Language.Arralac.Interpreter.Run
import Language.Arralac.Prelude.Types
import Options.Applicative
import PyF

cli :: Parser CLI
cli =
  hsubparser
    ( command "language-server" commandParserInfo.languageServer
        <> command "typecheck" commandParserInfo.typecheck
        <> command "interpret" commandParserInfo.interpret
    )

cliParserInfo :: String -> ParserInfo CLI
cliParserInfo version =
  info
    (cli <**> helper <**> simpleVersioner version <**> topLevelOptions)
    (fullDesc <> progDesc "Work with Arralac programs.")

topLevelOptions :: Parser (CLI -> CLI)
topLevelOptions = do
  switch
    ( long "stdio"
        <> help "Use stdio. This flag is required by the `vscode-languageclient' library."
    )
    <&> \stdio ->
      if
        | stdio ->
            const $
              CLI'LanguageServer
                Command'LanguageServer
                  { settingsSectionName = Nothing
                  }
        | otherwise -> id

commandParserInfo :: CommandParserInfo
commandParserInfo =
  CommandParserInfo
    { languageServer =
        info
          (CLI'LanguageServer <$> parseCommand.languageServer)
          (progDesc "Run Arralac language server.")
    , typecheck =
        info
          (CLI'Typecheck <$> parseCommand.typecheck)
          (progDesc "Typecheck an Arralac program.")
    , interpret =
        info
          (CLI'Interpret <$> parseCommand.interpret)
          (progDesc "Typecheck and interpret an Arralac program.")
    }

data MetavarNames = MetavarNames
  { file :: String
  , int :: String
  , section :: String
  }

metavarNames :: MetavarNames
metavarNames =
  MetavarNames
    { file = "FILE"
    , int = "INT"
    , section = "SECTION"
    }

data Metavars a b = Metavars
  { file :: Mod a b
  , int :: Mod a b
  , section :: Mod a b
  }

metavars :: (HasMetavar a) => Metavars a b
metavars =
  Metavars
    { file = metavar metavarNames.file
    , int = metavar metavarNames.int
    , section = metavar metavarNames.section
    }

parseInputFilePath :: Parser FastFilePath
parseInputFilePath =
  strArgument $
    metavars.file
      <> help [fmt|Absolute or relative path to a file to read a program from.|]

parseSolverIterations :: Parser (Maybe Int)
parseSolverIterations =
  optional $
    option
      auto
      ( long "solver-iterations"
          <> metavar "INT"
          <> help "Number of iterations for the constraint solver."
      )

parseSettingsSection :: Parser (Maybe FastString)
parseSettingsSection =
  optional $
    strOption $
      long "settings-section"
        <> short 's'
        <> metavars.section
        <> help
          [fmt|
          Read VS Code extension configuration from 
          this `settings.json' section {metavarNames.section}. 
          When this option is not specified, 
          use default settings.
          |]
        <> value "arralac"

parseDebug :: Parser Bool
parseDebug =
  switch $
    long "debug"
      <> short 'd'
      <> help
        [fmt|
        
        |]

parseCommand :: ParseCommand
parseCommand =
  ParseCommand
    { languageServer
    , typecheck
    , interpret
    }
 where
  languageServer = do
    settingsSectionName <- parseSettingsSection
    pure
      Command'LanguageServer
        { settingsSectionName
        }

  typecheck = do
    inputFilePath <- parseInputFilePath
    solverIterations <- parseSolverIterations
    debug <- parseDebug
    pure
      Command'Typecheck
        { inputFilePath
        , solverIterations
        , debug
        }

  interpret = do
    hsubparser
      ( command
          "whnf"
          ( info
              interpretWhnf
              (progDesc "Evaluate weak head normal form of the program.")
          )
      )

  interpret' = do
    inputFilePath <- parseInputFilePath
    solverIterations <- parseSolverIterations
    debug <- parseDebug
    pure (inputFilePath, solverIterations, debug)

  interpretWhnf =
    interpret'
      <&> \(inputFilePath, solverIterations, debug) ->
        Command'Interpret
          { inputFilePath
          , solverIterations
          , debug
          , interpreterMode = InterpreterMode'Whnf
          }
