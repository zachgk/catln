--------------------------------------------------------------------
-- |
-- Module    :  LSP
-- Copyright :  (c) Zach Kimberg 2024
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This file defines the language server protocol server for catln
--------------------------------------------------------------------
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LSP where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Reader          (ReaderT (runReaderT), ask)
import           Control.Monad.State
import           Control.Monad.Trans.Writer    (execWriter, tell)
import           CRes                          (cresToMaybe)
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server
import           Language.LSP.VFS
import           MapMeta                       (ExprMetaType (ExprMetaVal),
                                                MetaType (ExprMeta))
import           Semantics.Prgm                (getMetaPos)
import           Syntax.Ct.MapRawMeta          (mapMetaRawPrgmM)
import           Syntax.Ct.Parser.Syntax       (PPrgm)
import           Syntax.Parsers                (mkDesCanonicalImportStr)
import           Text.Megaparsec               (SourcePos (sourceColumn),
                                                sourceLine, unPos)
import           Utils
import           WebDocs                       (WDProvider, cRaw,
                                                emptyWDProvider, mkWDProvider)

type ServeConfig = ()
newtype ServeState = ServeState WDProvider

newServeState :: ServeState
newServeState = ServeState emptyWDProvider

newServeStateVar :: IO (MVar ServeState)
newServeStateVar = newMVar newServeState

recomputeServeState :: LSM ()
recomputeServeState = do
  workspaceFolders <- fromMaybe [] <$> getWorkspaceFolders
  VFS vfsMap' _ <- getVirtualFiles
  let getWorkspaceFolderPath (WorkspaceFolder uri _) = uriToFilePath uri
  let baseFiles = mapMaybe getWorkspaceFolderPath workspaceFolders ++ mapMaybe (uriToFilePath . fromNormalizedUri) (Map.keys vfsMap')
  -- TODO Support passing data from virtual files
  provider <- liftIO $ mkWDProvider baseFiles
  put (ServeState provider)
  return ()

type LSM = LspT ServeConfig (ReaderT (MVar ServeState) IO)

instance MonadState ServeState LSM where
  -- get = do
  --   stVar <- lift ask
  --   liftIO $ readMVar stVar
  -- put s = do
  --   stVar <- lift ask
  --   liftIO $ putMVar stVar s
  state f = do
    stVar <- lift ask
    liftIO $ modifyMVar stVar $ \st -> do
      let (a, st') = f st
      return (st', a)

runLSM :: LSM a -> MVar ServeState -> LanguageContextEnv ServeConfig -> IO a
runLSM lsm stVar cfg = runReaderT (runLspT cfg lsm) stVar

handlers :: Handlers LSM
handlers = mconcat
  [ notificationHandler SMethod_Initialized $ \_not -> do
      recomputeServeState
      return ()
  , notificationHandler SMethod_WorkspaceDidChangeWorkspaceFolders $ \_not -> do
      recomputeServeState
      return ()
  , notificationHandler SMethod_WorkspaceDidChangeWatchedFiles $ \_not -> do
      recomputeServeState
      return ()
  , notificationHandler SMethod_TextDocumentDidOpen $ \_not -> do
      recomputeServeState
      return ()
  , notificationHandler SMethod_TextDocumentDidSave $ \_not -> do
      recomputeServeState
      return ()
  , notificationHandler SMethod_SetTrace $ \_not -> do
      return ()
  , requestHandler SMethod_TextDocumentSemanticTokensFull $ \req responder -> do
      let TRequestMessage _ _ _ (SemanticTokensParams _ _ (TextDocumentIdentifier uri)) = req
          prgmName = uriToFilePath uri
      ServeState provider <- get
      prgmNameRaw <- liftIO $ mkDesCanonicalImportStr (fromJust prgmName)
      tokens <- case cresToMaybe (cRaw provider) >>= graphLookup prgmNameRaw of
        Nothing   -> return []
        Just prgm -> return $ prgmSemanticTokens prgm
      case makeSemanticTokens defaultSemanticTokensLegend tokens of
        Right encoded -> do
          responder (Right $ InL encoded)
        Left err -> responder (Left $ ResponseError (InL LSPErrorCodes_RequestFailed) err Nothing)
  ]

prgmSemanticTokens :: PPrgm -> [SemanticTokenAbsolute]
prgmSemanticTokens prgm = execWriter $ mapMetaRawPrgmM f prgm
  where
    f mType m = case getMetaPos m of
      Just pos -> do
        _ <- case mType of
          (ExprMeta _ ExprMetaVal) -> tell [SemanticTokenAbsolute (fromInteger $ toInteger $ unPos $ sourceLine $ fst pos) (fromInteger $ toInteger $ unPos $ sourceColumn $ fst pos) (fromInteger $ toInteger (unPos (sourceColumn $ snd pos) - unPos (sourceColumn $ fst pos))) SemanticTokenTypes_Type []]
          _ -> return ()
        return m
      Nothing -> return m

lspRun :: IO Int
lspRun = do
  st <- newServeStateVar
  runServer $ ServerDefinition
    { onConfigurationChange = const $ const $ Right ()
    , defaultConfig = ()
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = handlers
    , interpretHandler = \env -> Iso (\lsm -> runLSM lsm st env) liftIO
    , options = defaultOptions
    }
