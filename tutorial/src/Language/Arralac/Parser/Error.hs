{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Parser.Error where

import Control.Exception (Exception, throw)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Language.Arralac.Utils.Pretty
import Language.Arralac.Utils.Types
import Prettyprinter

-- | A parser error.
data ParserError
  = ParserError'LexerError {currentFilePath :: FastString, lineNumber :: Int, columnNumber :: Int}
  | ParserError'ParserError {currentFilePath :: FastString, lineNumber :: Int, columnNumber :: Int}
  | ParserError'Unknown {message :: FastString}

-- | A parser error that can capture the 'callStack' at the 'throw' site.
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
