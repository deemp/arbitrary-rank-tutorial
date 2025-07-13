module Language.Arralac.LanguageServer.Run where

import Control.Concurrent.STM (newTVarIO)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import Language.Arralac.LanguageServer.Server
import Language.Arralac.Prelude.Locale (withCorrectLocale)
import Language.LSP.Server

-- | The main entry point for our server.
runLanguageServer :: Maybe Text -> IO ExitCode
runLanguageServer settingsSectionName = withCorrectLocale do
  let settingsSectionName' = fromMaybe "arralac" settingsSectionName

  -- Create the initial empty server state.
  initialServerState <- newTVarIO M.empty

  -- Start the server.
  -- 'runServer' wraps 'runServerWithHandles'
  exitCode' <- runServer $ serverDefinition settingsSectionName' initialServerState

  pure $
    if
      | exitCode' == 0 -> ExitSuccess
      | otherwise -> ExitFailure exitCode'
