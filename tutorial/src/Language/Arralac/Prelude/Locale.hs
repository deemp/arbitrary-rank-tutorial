module Language.Arralac.Prelude.Locale where

import Control.Exception (SomeException)
import GHC.IO.Exception (ExitCode)
import Main.Utf8 (withUtf8)
import System.Exit (exitFailure, exitWith)
import System.IO.CodePage (withCP65001)
import UnliftIO (Exception (..), Handler (..), catches)

withCorrectLocale' :: IO a -> IO a
withCorrectLocale' = withCP65001 . withUtf8

withCorrectLocale :: IO a -> IO a
withCorrectLocale act = do
  withCorrectLocale' act
    `catches` [ Handler $ \(x :: ExitCode) -> exitWith x
              , Handler $ \(x :: SomeException) ->
                  withCorrectLocale' do
                    putStrLn (displayException x)
                    exitFailure
              ]