{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Reader.Error where

import Control.Exception (Exception, throw)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Language.Arralac.Prelude.Pretty (Pretty' (..), prettyIndent, vsep')
import Language.Arralac.Prelude.Types (FastFilePath)

data ReaderError
  = ReaderError'CouldNotReadStdin
  | ReaderError'CouldNotReadFile {filePath :: FastFilePath}

data ReaderErrorWithCallStack where
  ReaderErrorWithCallStack :: (HasCallStack) => ReaderError -> ReaderErrorWithCallStack

dieReader :: (HasCallStack) => ReaderError -> IO a
dieReader err = throw (ReaderErrorWithCallStack err)

instance Exception ReaderError

instance Exception ReaderErrorWithCallStack

instance Pretty' ReaderError where
  pretty' err = case err of
    ReaderError'CouldNotReadStdin ->
      "Could not read program from stdin."
    ReaderError'CouldNotReadFile{} ->
      vsep'
        [ "Could not read file at path:"
        , prettyIndent err.filePath
        ]

instance Pretty' ReaderErrorWithCallStack where
  pretty' (ReaderErrorWithCallStack err) =
    vsep'
      [ pretty' (prettyCallStack callStack)
      , pretty' err
      ]
