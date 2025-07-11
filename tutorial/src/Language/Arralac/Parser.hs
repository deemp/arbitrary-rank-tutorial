{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Parser where

import Control.Exception (Exception, throw)
import Data.Char (isDigit)
import Data.Function ((&))
import Data.Text as T (Text, pack)
import Data.Text.IO.Utf8 as TIO
import GHC.Exception (prettyCallStack)
import GHC.Stack (HasCallStack, callStack)
import Language.Arralac.Parser.Abs as Abs
import Language.Arralac.Parser.Lex (Token)
import Language.Arralac.Parser.Par
import Language.Arralac.Utils.Pretty
import Language.Arralac.Utils.Types
import Prettyprinter (indent)

type ParserM a = (ICurrentFilePath) => IO a

parseWith :: ([Token] -> Either String a) -> Text -> Either String a
parseWith parser input = parser tokens
 where
  tokens = myLexer input

-- TODO use megaparsec
getLineAndColumnFromError :: String -> Maybe (Int, Int)
getLineAndColumnFromError s =
  s
    & filter (/= ',')
    & words
    & filter (isDigit . head)
    & \x -> case x of
      [lineNumber, columnNumber] ->
        pure (read lineNumber, read columnNumber)
      _ -> Nothing

parseText :: Text -> ParserM Program
parseText input = do
  let
    parsed = parseWith pProgram input
  case parsed of
    Left err -> do
      -- TODO parse error message with megaparsec
      let lexerError = getLineAndColumnFromError err
      dieParser $ case lexerError of
        Just (lineNumber, columnNumber) ->
          ParserError'LexerError
            { lineNumber = lineNumber - 1
            , columnNumber = columnNumber - 1
            , currentFilePath = ?currentFilePath
            }
        Nothing -> ParserError'Unknown{message = T.pack err}
    Right prog -> pure prog

parseInput :: Either String Text -> ParserM Program
parseInput input = do
  input' <- either TIO.readFile pure input
  parseText input'

parseFile :: String -> ParserM Program
parseFile filename = parseInput (Left filename)

-- | A parser exception.
data ParserError
  = ParserError'LexerError {currentFilePath :: FastString, lineNumber :: Int, columnNumber :: Int}
  | ParserError'ParserError {currentFilePath :: FastString, lineNumber :: Int, columnNumber :: Int}
  | ParserError'Unknown {message :: FastString}

-- | A parser exception that can capture the 'callStack' at the 'throw' site.
--
-- https://maksbotan.github.io/posts/2021-01-20-callstacks.html
data ParserErrorWithCallStack where
  ParserErrorWithCallStack :: (HasCallStack) => ParserError -> ParserErrorWithCallStack

instance Pretty' ParserError where
  pretty' = \case
    ParserError'LexerError{currentFilePath, lineNumber, columnNumber} ->
      vsep'
        [ "Lexer error at:"
        , mkLocatedError currentFilePath lineNumber columnNumber
        ]
    ParserError'ParserError{currentFilePath, lineNumber, columnNumber} ->
      vsep'
        [ "Parser error at:"
        , mkLocatedError currentFilePath lineNumber columnNumber
        ]
    ParserError'Unknown{message} ->
      vsep'
        [ "Unknown error:"
        , prettyIndent message
        ]
   where
    mkLocatedError currentFilePath lineNumber columnNumber =
      indent 2 $
        (pretty' currentFilePath <> ":")
          <> (pretty' (lineNumber + 1) <> ":")
          <> (pretty' (columnNumber + 1))

instance Pretty' ParserErrorWithCallStack where
  pretty' (ParserErrorWithCallStack err) =
    vsep'
      [ pretty' (prettyCallStack callStack)
      , pretty' err
      ]

instance Exception ParserError

instance Exception ParserErrorWithCallStack

-- | Fail unconditionally with a 'RnErrorWithCallStack'.
dieParser :: (HasCallStack) => ParserError -> IO a
dieParser err = throw (ParserErrorWithCallStack err)
