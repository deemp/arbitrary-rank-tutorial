{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Parser.Parse where

import Data.Char (isDigit)
import Data.Function ((&))
import Data.Text as T (Text, pack)
import Data.Text.IO.Utf8 as TIO
import Language.Arralac.Parser.Abs as Abs
import Language.Arralac.Parser.Error
import Language.Arralac.Parser.Generated.Lex (Token)
import Language.Arralac.Parser.Generated.Par
import Language.Arralac.Utils.Types

type ParserM a = (CtxCurrentFilePath) => IO a

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
