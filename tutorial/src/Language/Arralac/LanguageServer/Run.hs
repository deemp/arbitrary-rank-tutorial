module Language.Arralac.LanguageServer.Run where

import Control.Concurrent.STM (newTVarIO)
import Data.Map qualified as M
import Data.Text (Text)
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import Language.Arralac.LanguageServer.Server
import Language.LSP.Server

-- | The main entry point for our server.
runLanguageServer :: Text -> IO ExitCode
runLanguageServer settingsSectionName = do
  let settingsSectionName' = settingsSectionName

  -- Create the initial empty server state.
  initialServerState <- newTVarIO M.empty

  -- Start the server.
  -- 'runServer' wraps 'runServerWithHandles'
  exitCode' <- runServer $ serverDefinition settingsSectionName' initialServerState

  pure $
    if
      | exitCode' == 0 -> ExitSuccess
      | otherwise -> ExitFailure exitCode'
