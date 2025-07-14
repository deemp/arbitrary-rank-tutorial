module Language.Arralac.Reader.Run where

import Control.Exception (SomeException)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.Arralac.Prelude.Types
import Language.Arralac.Reader.Error
import UnliftIO (catch)

runReader :: FastFilePath -> IO Text
runReader filePath =
  catch
    (T.readFile (T.unpack filePath))
    ( \(_ :: SomeException) ->
        dieReader ReaderError'CouldNotReadFile{filePath}
    )
