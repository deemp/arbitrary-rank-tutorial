{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Parser.Error where

import Control.Exception (Exception, throw)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Prelude.Types
import Prettyprinter

-- | A parser error.
data ParserError
  = ParserError'LexerError {currentFilePath :: FastFilePath, lineNumber :: Int, columnNumber :: Int}
  | ParserError'ParserError {currentFilePath :: FastFilePath, lineNumber :: Int, columnNumber :: Int}
  | ParserError'Unknown {message :: FastString}

-- | A parser error that can capture the 'callStack' at the 'throw' site.
--
-- https://maksbotan.github.io/posts/2021-01-20-callstacks.html
data ParserErrorWithCallStack where
  ParserErrorWithCallStack :: (HasCallStack) => ParserError -> ParserErrorWithCallStack

instance Pretty' ParserError where
  pretty' err = case err of
    ParserError'LexerError{} ->
      vsep'
        [ "Lexer error at:"
        , mkLocatedError err.currentFilePath err.lineNumber err.columnNumber
        ]
    ParserError'ParserError{} ->
      vsep'
        [ "Parser error at:"
        , mkLocatedError err.currentFilePath err.lineNumber err.columnNumber
        ]
    ParserError'Unknown{} ->
      vsep'
        [ "Unknown error:"
        , prettyIndent err.message
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
