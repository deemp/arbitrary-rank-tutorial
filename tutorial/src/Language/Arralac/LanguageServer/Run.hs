module Language.Arralac.LanguageServer.Run where

import Control.Concurrent.STM (newTVarIO)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Language.Arralac.LanguageServer.Server
import Language.Arralac.Prelude.Locale (withCorrectLocale)
import System.Environment (lookupEnv)

-- ==============================================
-- The Main Entry Point for the Server
-- ==============================================

-- | The main entry point for our server.
runLanguageServer :: IO Int
runLanguageServer = withCorrectLocale do
  sectionName <- lookupEnv "LSP_SETTINGS_ARRALAC_SECTION"
  let sectionName' = T.pack $ fromMaybe "arralac" sectionName

  -- Create the initial empty server state.
  initialServerState <- newTVarIO M.empty

  -- Start the server.
  -- 'runServer' wraps 'runServerWithHandles'
  exitCode <- runServer $ serverDefinition sectionName' initialServerState

  -- Return the exit code.
  pure exitCode
