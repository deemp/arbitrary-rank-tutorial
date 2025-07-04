{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.STLC.LanguageServer.Main where

import Colog.Core
import Colog.Core qualified as L
import Control.Concurrent.STM
import Control.Lens hiding (Iso)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson qualified as J
import Data.Foldable (find)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Internal.Search qualified as T
import GHC.Generics (Generic)
import Language.LSP.Logging (defaultClientLogger)
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message (SMethod (..), TRequestMessage (..))
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as L
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server (Handlers, LspT (..), Options (..), ServerDefinition (..), defaultOptions, getVirtualFile, notificationHandler, requestHandler, runLspT, runServer, sendNotification, type (<~>) (Iso))
import Language.LSP.VFS (virtualFileText)
import System.IO

-- =============================================================================
--  1. Our Custom State and Monad
-- =============================================================================

-- | This is a mock representation of your annotated AST.
--   For our purposes, it's just a list of nodes, where each node has
--   a span (where it is in the code) and a type (as a string).
newtype AnnotatedAst = AnnotatedAst [(Range, T.Text)]
  deriving (Show)

-- | The server's state. We store an AST for each file URI.
--   Using a Map is efficient for lookups.
type ServerState = Map NormalizedUri AnnotatedAst

type IServerState = (?serverState :: TVar ServerState)
type ILogger = (?logger :: L.LogAction (LspT Config IO) (WithSeverity T.Text))

-- TODO specify meaningful options
data Config = Config {}
  deriving stock (Generic, Show)
  deriving anyclass (J.ToJSON, J.FromJSON)

-- | Like library LspM, but with a constant config.
-- https://hackage.haskell.org/package/lsp-2.7.0.0/docs/Language-LSP-Server.html#t:LspM
type LspM a = (IServerState) => LspT Config IO a

type Handlers' = (IServerState, ILogger) => Handlers (LspT Config IO)

-- | We'll wrap the main LSP monad in a ReaderT to grant our handlers
--   access to the server's state. We use a TVar for thread-safe mutable state.
-- newtype LspM a = LspM { unLspM :: ReaderT (TVar ServerState) (LspT () IO) a }
--   deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (TVar ServerState))

-- Helper to run our monad in the base LSP monad
runLspM :: TVar ServerState -> LspT Config IO a -> LspM a
runLspM state m = let ?serverState = state in m

-- runReaderT (unLspM m) state

-- =============================================================================
--  2. The "Typechecker" (Replace this with your own)
-- =============================================================================

-- | This function simulates your typechecker. It takes the text of a file
--   and produces our `AnnotatedAst`.
--   It's very simple: it just finds specific keywords and assigns them a type.
runFakeTypechecker :: T.Text -> AnnotatedAst
runFakeTypechecker content = AnnotatedAst nodes
 where
  -- Define our "language"
  keywords = [("hello", "String"), ("42", "Int"), ("main", "IO ()")]

  -- Find all occurrences of the keywords and create a (Range, Type) tuple
  nodes = concatMap findKeywordOccurrences keywords

  findKeywordOccurrences (keyword, typeName) =
    let textLines = T.lines content
     in concatMap (\(lineIdx, line) -> findInLine lineIdx line) (zip [0 ..] textLines)
   where
    findInLine lineIdx line =
      let cols = T.indices keyword line
       in map (createRange lineIdx) cols

    createRange :: Integer -> Int -> (Range, T.Text)
    createRange lineIdx col =
      let startPos = Position (fromIntegral lineIdx) (fromIntegral col)
          endPos = Position (fromIntegral lineIdx) (fromIntegral col + fromIntegral (T.length keyword))
       in (Range startPos endPos, typeName)

-- | A helper to check if a position is within a given range.
isPositionInsideRange :: Position -> Range -> Bool
isPositionInsideRange (Position l c) (Range (Position sl sc) (Position el ec)) =
  (l > sl || (l == sl && c >= sc)) && (l < el || (l == el && c < ec))

-- =============================================================================
--  3. The LSP Handlers
-- =============================================================================

-- | The handler for the 'textDocument/hover' request.
hoverHandler :: Handlers'
hoverHandler = requestHandler SMethod_TextDocumentHover $ \req responder -> do
  let TRequestMessage _ _ _ (HoverParams doc pos _workDone) = req
      uri = doc ^. L.uri . to toNormalizedUri

  -- Get the current state
  state <- liftIO $ readTVarIO ?serverState

  -- Look up the AST for the current file
  case M.lookup uri state of
    Just (AnnotatedAst nodes) -> do
      -- Find the node the user is hovering over
      let mNode = find (\(range, _) -> isPositionInsideRange pos range) nodes
      case mNode of
        Just (range, typeInfo) -> do
          -- Found it! Create the hover response.
          let ms = LSP.InL $ LSP.mkMarkdown $ "```haskell\n" <> typeInfo <> "\n```"
              rsp = Hover ms (Just range)
          responder (Right $ LSP.InL rsp)
        Nothing -> do
          -- No node at this position
          responder (Right $ LSP.InR Null)
    Nothing -> do
      -- No AST found for this file
      responder (Right $ LSP.InR Null)

-- TODO handle didSave, didClose

-- | This is the "learning" part. It gets called when a file is opened or
--   its text changes. We run our typechecker and update the state.
documentChangeHandler :: Handlers'
documentChangeHandler =
  mconcat
    [ notificationHandler SMethod_TextDocumentDidOpen update
    , notificationHandler SMethod_TextDocumentDidChange update
    ]
 where
  getUri msg = msg ^. L.params . L.textDocument . L.uri . to L.toNormalizedUri

  update msg = do
    let docUri = getUri msg
    doc <- getVirtualFile docUri
    case doc of
      Just file ->
        updateStateForFile docUri (virtualFileText file)
      Nothing -> do
        ?logger <& ("Didn't find anything in the VFS for: " <> T.pack (show doc)) `WithSeverity` Info

updateStateForFile :: NormalizedUri -> T.Text -> LspM ()
updateStateForFile docUri docText = do
  -- Run our typechecker
  let ast = runFakeTypechecker docText

  -- Update the state map atomically
  liftIO $ atomically $ modifyTVar' ?serverState (M.insert docUri ast)

  -- Optionally, send a log message to the client for debugging
  sendNotification SMethod_WindowLogMessage $ LogMessageParams MessageType_Info $ "Updated AST for: " <> T.pack (show docUri)

-- | A handler for when the client is initialized.
initializeHandler :: Handlers'
initializeHandler = notificationHandler SMethod_Initialized $ \_ -> do
  sendNotification SMethod_WindowLogMessage $ LogMessageParams MessageType_Info "Simple LSP Server Initialized!"

-- | We combine all our handlers into a single definition.
serverHandlers :: ClientCapabilities -> Handlers'
serverHandlers _cs =
  mconcat
    [ initializeHandler
    , hoverHandler
    , documentChangeHandler
    ]

-- =============================================================================
--  4. The Server Definition and Main Entry Point
-- =============================================================================

stderrLogger :: LogAction IO (WithSeverity T.Text)
stderrLogger = L.cmap show L.logStringStderr
clientLogger :: LogAction (LspT Config IO) (WithSeverity T.Text)
clientLogger = defaultClientLogger
dualLogger :: LogAction (LspT Config IO) (WithSeverity T.Text)
dualLogger = clientLogger <> L.hoistLogAction liftIO stderrLogger

-- | This defines the server's capabilities and how to run the handlers.
serverDefinition :: TVar ServerState -> ServerDefinition Config
serverDefinition serverState =
  ServerDefinition
    { onConfigChange = const $ pure ()
    , -- TODO something meaninful
      doInitialize = \env _req -> pure $ Right env
    , staticHandlers =
        let
          ?serverState = serverState
          ?logger = dualLogger
         in
          serverHandlers
    , options = lspOptions
    , -- TODO fix
      interpretHandler = \env -> Iso (runLspT env) liftIO
    , defaultConfig = Config{}
    , configSection = "arralac"
    , parseConfig = \_ _ -> Right Config{}
    }
 where
  lspOptions =
    defaultOptions
      { -- This tells the client to send us the full text of the document
        -- on open and change, which is what our `updateStateForFile` function needs.
        optTextDocumentSync =
          Just $
            TextDocumentSyncOptions
              { _openClose = Just True
              , _change = Just TextDocumentSyncKind_Full
              , _willSave = Just False
              , _willSaveWaitUntil = Just False
              , _save = Just $ InR $ SaveOptions $ Just False
              }
      }

-- | The main entry point for our server.
main :: IO Int
main = do
  -- Set up logging to a file for debugging
  h <- openFile "simple-lsp.log" WriteMode
  hSetBuffering h LineBuffering

  -- Create the initial empty state
  initialState <- newTVarIO M.empty

  -- Start the server
  exitCode <- runServer $ serverDefinition initialState

  -- runServer's first argument is a ServerDefinition, which can be created
  -- with the `serverDefinition` function.
  -- runServerWithHandles h h $ serverDefinition initialState

  -- Clean up
  hClose h

  -- Return the exit code
  pure exitCode