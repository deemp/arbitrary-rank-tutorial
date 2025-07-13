module Language.Arralac.Reader.Run where

import Control.Exception (SomeException)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.Arralac.Reader.Error
import UnliftIO (catch)

runReader :: Maybe FilePath -> IO Text
runReader mbFilePath =
  catch
    (maybe T.getContents T.readFile mbFilePath)
    ( \(_ :: SomeException) ->
        case mbFilePath of
          Nothing ->
            dieReader ReaderError'CouldNotReadStdin{}
          Just filePath ->
            dieReader ReaderError'CouldNotReadFile{filePath = T.pack filePath}
    )
