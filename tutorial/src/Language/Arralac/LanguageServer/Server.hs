module Language.Arralac.LanguageServer.Server where

import Colog.Core
import Colog.Core qualified as L
import Control.Concurrent.STM
import Control.Exception (SomeException)
import Control.Lens hiding (Iso)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (fromJSON)
import Data.Aeson qualified as J
import Data.IntervalMap.Generic.Strict qualified as IM
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Language.Arralac.LanguageServer.IntervalMap (IMPosition (..), IMRange (..), SpanInfo (..), lookupAtIMPosition, prettyIM, toIntervalMap, toRealSrcSpan)
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Prelude.Types
import Language.LSP.Logging (defaultClientLogger)
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message (SMethod (..), TRequestMessage (..))
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as L
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server (Handlers, LspT (..), Options (..), ServerDefinition (..), defaultOptions, getConfig, getVirtualFile, notificationHandler, requestHandler, runLspT, sendNotification, type (<~>) (Iso))
import Language.LSP.VFS (virtualFileText)
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import UnliftIO (catch)
import Language.Arralac.Driver.ReaderToZonker.Run

-- | The server's state.
--
-- We store an AST for each file URI.
--
-- Using an 'IM.IntervalMap' is efficient for lookups.
type ServerState = Map NormalizedUri (IM.IntervalMap IMRange SpanInfo)

-- | A mutable variable for storing the server state.
type CtxServerState = (?serverState :: TVar ServerState)

-- | Logger to run in the LSP monad.
type CtxLogger = (?logger :: L.LogAction (LspT Config IO) (WithSeverity T.Text))

-- | Language server config
--
-- TODO specify meaningful options
data Config = Config
  { solverIterations :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (J.ToJSON, J.FromJSON)

type CtxConfig = (HasCallStack, CtxLogger, CtxServerState, CtxPrettyVerbosity)

-- | Like library LspM, but with a constant config.
-- https://hackage.haskell.org/package/lsp-2.7.0.0/docs/Language-LSP-Server.html#t:LspM
type LspM a = (CtxConfig) => LspT Config IO a

-- | 'Handlers' with additional context
type Handlers' = (CtxConfig) => Handlers (LspT Config IO)

-- ==============================================
-- The LSP Handlers
-- ==============================================

renderStrictDoc :: Doc ann -> T.Text
renderStrictDoc doc = renderStrict (layoutPretty defaultLayoutOptions doc)

-- | The handler for the requests:
--
-- - @textDocument/hover@
hoverHandler :: Handlers'
hoverHandler = requestHandler SMethod_TextDocumentHover $ \req responder -> do
  let TRequestMessage _ _ _ (HoverParams doc pos _workDone) = req
      uri = doc ^. L.uri . to toNormalizedUri
      imPos = IMPosition pos

  state <- liftIO $ readTVarIO ?serverState

  ?logger <& ("Hover at: " <> T.pack (show imPos)) `WithSeverity` Info

  -- Look up the AST for the current file
  case M.lookup uri state of
    Just mp -> do
      let mNode = lookupAtIMPosition imPos mp
          mbFilePath = fromNormalizedFilePath <$> uriToNormalizedFilePath uri
          mNode' =
            ( \filePath (range', spanInfo') ->
                (toRealSrcSpan (T.pack filePath) range', spanInfo')
            )
              <$> mbFilePath
              <*> mNode

      ?logger <& ("Found node: " <> renderStrictDoc (pretty' mNode')) `WithSeverity` Info

      case mNode of
        Just (range', typeInfo) -> do
          -- Create the hover response.
          let ms = LSP.InL $ LSP.mkMarkdown $ "```haskell\n" <> renderStrictDoc (pretty' typeInfo) <> "\n```"
              rsp = Hover ms (Just (imRange range'))
          responder (Right $ LSP.InL rsp)
        Nothing -> do
          -- No node at this position
          responder (Right $ LSP.InR Null)
    Nothing -> do
      -- No AST found for this file
      responder (Right $ LSP.InR Null)

-- | The handler for the requests:
--
-- - @workspace/didChangeConfiguration@
configurationChangeHandler :: Handlers'
configurationChangeHandler =
  mconcat
    [ notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_msg -> do
        -- TODO react to config changes like here:
        -- https://github.com/haskell/lsp/blob/81f8b94f30230446f14c16f5d847e4125b7afa67/lsp/example/Reactor.hs#L233
        ?logger <& "Configuration changed" `WithSeverity` Info
    ]

-- | The handler for the requests:
--
-- - @textDocument/didOpen@
-- - @textDocument/didChange@
-- - @textDocument/didSave@
documentChangeHandler :: Handlers'
documentChangeHandler =
  mconcat
    [ notificationHandler SMethod_TextDocumentDidOpen update
    , notificationHandler SMethod_TextDocumentDidChange update
    , notificationHandler SMethod_TextDocumentDidSave update
    ]
 where
  getUri msg = msg ^. L.params . L.textDocument . L.uri . to L.toNormalizedUri

  update msg = do
    let docUri = getUri msg
        -- I assume we only work with files
        mbFilePath = fromNormalizedFilePath <$> uriToNormalizedFilePath docUri
    mbDoc <- getVirtualFile docUri
    case (mbDoc, mbFilePath) of
      (Just file, Just filePath) ->
        updateStateForFile docUri (T.pack filePath) (virtualFileText file)
      (_, _) -> do
        ?logger <& ("Didn't find anything in the VFS for: " <> T.pack (show docUri)) `WithSeverity` Info

updateStateForFile :: NormalizedUri -> FastString -> T.Text -> LspM ()
updateStateForFile docUri filePath docText =
  do
    ast <- do
      config <- getConfig
      let 
          ?solverIterations = config.solverIterations
          ?debug = False
      liftIO $ runReaderToZonker filePath docText

    let mp = toIntervalMap ast

    ?logger <& renderStrictDoc (prettyIM filePath mp) `WithSeverity` Info

    liftIO $ atomically $ modifyTVar' ?serverState (M.insert docUri mp)

    -- Send a log message to the client for debugging.
    sendNotification SMethod_WindowLogMessage $
      LogMessageParams MessageType_Info $
        "Updated AST for: " <> T.pack (show docUri)
    -- Typechecker may throw.
    `catch` ( \(err :: SomeException) -> do
                ?logger <& T.pack (show err) `WithSeverity` Error

                sendNotification SMethod_WindowLogMessage $
                  LogMessageParams MessageType_Error $
                    "Could not update AST for: " <> T.pack (show docUri)
            )

-- | A handler for when the client is initialized.
initializeHandler :: Handlers'
initializeHandler = notificationHandler SMethod_Initialized $ \_ -> do
  sendNotification SMethod_WindowLogMessage $
    LogMessageParams MessageType_Info "Simple LSP Server Initialized!"

-- | We combine all our handlers into a single definition.
serverHandlers :: ClientCapabilities -> Handlers'
serverHandlers _cs =
  mconcat
    [ initializeHandler
    , hoverHandler
    , documentChangeHandler
    , configurationChangeHandler
    ]

-- ==============================================
-- The Server Definition
-- ==============================================

stderrLogger :: LogAction IO (WithSeverity T.Text)
stderrLogger = L.cmap show L.logStringStderr
clientLogger :: LogAction (LspT Config IO) (WithSeverity T.Text)
clientLogger = defaultClientLogger
dualLogger :: LogAction (LspT Config IO) (WithSeverity T.Text)
dualLogger = clientLogger <> L.hoistLogAction liftIO stderrLogger

-- | This defines the server's capabilities and how to run the handlers.
serverDefinition :: T.Text -> TVar ServerState -> ServerDefinition Config
serverDefinition sectionName serverState =
  ServerDefinition
    { onConfigChange = const $ pure ()
    , -- TODO something meaninful
      doInitialize = \env _req -> pure $ Right env
    , staticHandlers =
        let
          ?serverState = serverState
          ?logger = dualLogger
          ?prettyVerbosity = PrettyVerbosity'Normal
         in
          serverHandlers
    , options = lspOptions
    , -- TODO Need to fix?
      interpretHandler = \env -> Iso (runLspT env) liftIO
    , defaultConfig =
        Config
          { solverIterations = 10
          }
    , configSection = sectionName
    , parseConfig = \_ val ->
        case fromJSON val of
          J.Error str -> Left $ T.pack str
          J.Success conf -> Right conf
    }
 where
  lspOptions =
    defaultOptions
      { -- This tells the client to send us the full text of the document
        -- on open and change, which is what our 'updateStateForFile' function needs.
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
