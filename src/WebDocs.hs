--------------------------------------------------------------------
-- |
-- Module    :  WebDos
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This file defines the server for the Catln webdocs.
--------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module WebDocs where

import           Data.Aeson                    (ToJSON, Value, object, (.=))
import           Web.Scotty

import qualified Data.HashMap.Strict           as H
import qualified Data.Text.Lazy                as T
import           Data.Text.Lazy.Encoding       (decodeUtf8)
import           GHC.Generics                  (Generic)
import           Network.Wai.Middleware.Static

import           Control.Monad.Trans           (lift)
import           CRes
import           CtService
import           Data.Bifunctor                (Bifunctor (first))
import           Data.Graph
import           Eval                          (evalAllTargetModes, evalAnnots,
                                                prgmFromGraphData)
import           MapMeta                       (addMetaID, interleaveMeta,
                                                interleavePrgm, zip3MetaFun)
import           Semantics                     (buildCtssConfig)
import           Semantics.Prgm
import           Syntax.Ct.Desugarf            (desFinalPasses, desPrgm)
import           Syntax.Ct.Desugarf.Expr       (desFileImport)
import           Syntax.Ct.MapRawMeta          (mapMetaRawPrgm, mapMetaRawPrgmM)
import           Syntax.Ct.Parser.Expr         (pExpr)
import           Syntax.Ct.Prgm                (RawExpr (RawValue),
                                                RawPrgm (RawPrgm),
                                                RawStatement (RawAnnot),
                                                RawStatementTree (RawStatementTree),
                                                mkRawFileImport)
import           Syntax.Parsers                (mkDesCanonicalImportStr)
import           Text.Megaparsec               (errorBundlePretty, runParser)
import           Text.Printf
import           TypeCheck                     (typecheckPrgms)
import           TypeCheck.Common              (filterTraceConstrain,
                                                flipTraceConstrain,
                                                typeCheckToRes)
import           Utils

data ResSuccess a n = Success a [n]
  | ResFail [n]
  deriving (Generic, ToJSON)

maybeJson :: (ToJSON a) => CRes a -> ActionM ()
maybeJson (CRes notes r) = json $ Success r notes
maybeJson (CErr notes)   = json (ResFail notes :: ResSuccess () CNote)

-- | Both providers return the service handle alongside the per-request refresh action.
-- The handle is needed by writes, which must not pay for the live provider's rebuild.
mkLiveWDProvider :: [String] -> IO (CTSS, IO CTSS)
mkLiveWDProvider baseFileNames = do
  ss <- ctssBaseFiles buildCtssConfig baseFileNames
  ctssBuildAll ss
  return (ss, do
    ctssRead ss
    ctssBuildAll ss -- TODO Remove when fixing incremental build
    return ss)

mkCacheWDProvider :: [String] -> IO (CTSS, IO CTSS)
mkCacheWDProvider baseFileNames = do
  ss <- ctssBaseFiles buildCtssConfig baseFileNames
  ctssBuildAll ss
  return (ss, return ss)

docApiBase :: Bool -> CTSS -> IO CTSS -> ScottyM ()
docApiBase readonly ctss getProvider = do

  get "/api/config" $
    maybeJson (return (object ["readonly" .= readonly]) :: CRes Value)

  -- Writes are rejected under --readonly so a shared server cannot be edited, no matter
  -- what the client believes it is allowed to do.
  post "/api/write" $ do
    prgmName <- param "prgmName"
    src <- body
    resp <- liftAndCatchIO $ runCResT $ if readonly
      then fail "Saving is disabled because the doc server is running with --readonly"
      else do
        prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
        setSource ctss prgmName' (T.unpack $ decodeUtf8 src)
    maybeJson resp

  get "/api/toc" $ do
    provider <- liftAndCatchIO getProvider
    pages <- liftAndCatchIO $ ctssKeys provider
    let resp = return $ zip (map impDisp pages) pages
    maybeJson resp

  get "/api/page" $ do
    provider <- liftAndCatchIO getProvider
    prgmName <- param "prgmName"
    resp <- liftAndCatchIO $ runCResT $ do
      prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
      rawPrgm' <- ctssLookupReq (Just . ssfRaw) prgmName' provider
      tprgms <- getTPrgm provider
      maybeTPrgmRes <- lift $ runCResT $ do
        case graphLookup prgmName' tprgms of
          Just tprgm -> do
            reachTprgms <- asCResT $ prgmFromGraphData prgmName' tprgms
            return (tprgm, reachTprgms)
          Nothing -> fail $ printf "Could not find typechecked program %s" (show prgmName)
      let maybeTPrgm = fmap fst maybeTPrgmRes
      let maybeTPrgmFull = fmap snd maybeTPrgmRes
      maybeAnnots <- lift $ runCResT $ getEvalAnnots provider prgmName'
      let tprgm' = maybe H.empty interleavePrgm (cresToMaybe maybeTPrgm)
      let targetModes = maybe H.empty evalAllTargetModes (cresToMaybe maybeTPrgmFull)
      let annots' = maybe H.empty (H.fromList . map (first (getMetaID . getExprMeta))) (cresToMaybe maybeAnnots)
      let prgm' = mapMetaRawPrgm (zip3MetaFun (interleaveMeta tprgm') (interleaveMeta targetModes) (interleaveMeta annots')) rawPrgm'
      maybeSource <- lift $ runCResT $ getSource provider prgmName'
      return (prgm', cresToMaybe maybeSource)
    maybeJson resp

  get "/api/annot" $ do
    provider <- liftAndCatchIO getProvider
    annot <- param "annot"
    -- The synthetic <annot> program below depends on every loaded file, so the stages it can list
    -- are joined over all of them. Its own contribution is the one annotation statement and no
    -- declarations, which a listing does not show.
    stages <- liftAndCatchIO $ ctssStagePrgms provider
    resp <- liftAndCatchIO $ runCResT $ do
      rawPrgm <- case runParser pExpr "<annot>" annot of
        Left err     -> fail $ show $ errorBundlePretty err
        Right parsed -> return (RawPrgm [] [RawStatementTree (RawAnnot parsed) []])
      rawPrgm' <- lift $ mapMetaRawPrgmM addMetaID rawPrgm
      (annotDesPrgm, annotPrgmName, _) <- asCResT $ desPrgm (rawPrgm', desFileImport $ mkRawFileImport $ RawValue emptyMetaN "<annot>", [])
      [annotDesPrgm'] <- desFinalPasses [(annotDesPrgm, [], [])]
      tprgmsTrace <- getTPrgmWithTrace provider
      tprgms <- getTPrgm provider
      let annotDeps = map snd3 $ graphToNodes tprgms
      [(annotTprgm, _, _)] <- asCResT $ typeCheckToRes $ typecheckPrgms [(annotDesPrgm', [], map fst3 $ graphToNodes tprgmsTrace)]
      let joinedTprgms = graphFromEdges ((annotTprgm, annotPrgmName, annotDeps) : graphToNodes tprgms)
      let targetModes = evalAllTargetModes annotTprgm
      maybeAnnots <- asCResT $ evalAnnots stages annotPrgmName joinedTprgms
      let annots' = H.fromList $ map (first (getMetaID . getExprMeta)) maybeAnnots
      return $ mapMetaRawPrgm (zip3MetaFun (interleaveMeta $ interleavePrgm annotTprgm) (interleaveMeta targetModes) (interleaveMeta annots')) rawPrgm'
    maybeJson resp

  get "/api/constrain" $ do
    provider <- liftAndCatchIO getProvider
    prgmName <- param "prgmName"
    resp <- liftAndCatchIO $ runCResT $ do
      prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
      (tprgm, vprgm, _tr) <- ctssLookupReq ssfTPrgmWithTrace prgmName' provider
      return (tprgm, vprgm)
    maybeJson resp

  get "/api/constrain/pnt/:pnt" $ do
    provider <- liftAndCatchIO getProvider
    prgmName <- param "prgmName"
    pnt <- param "pnt"
    resp <- liftAndCatchIO $ runCResT $ do
      prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
      (_, _, tr) <- ctssLookupReq ssfTPrgmWithTrace prgmName' provider
      return $ flipTraceConstrain $ filterTraceConstrain tr pnt
    maybeJson resp

  get "/api/treebug" $ do
    provider <- liftAndCatchIO getProvider
    prgmName <- param "prgmName"
    fun <- param "function" `rescue` (\_ -> return "main")
    resp <- liftAndCatchIO $ runCResT $ do
      prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
      getTreebug provider prgmName' fun
    maybeJson resp


docApi :: Bool -> Bool -> [String] -> IO ()
docApi cached readonly baseFileNames = do

  (ctss, provider) <- if cached
    then mkCacheWDProvider baseFileNames
    else mkLiveWDProvider baseFileNames

  scotty 31204 $ docApiBase readonly ctss provider

docServe :: Bool -> Bool -> [String] -> IO ()
docServe cached readonly baseFileNames = do

  let handleIndex p = case p of
        "" -> Just "index.html"
        _  -> Just p

  (ctss, provider) <- if cached
    then mkCacheWDProvider baseFileNames
    else mkLiveWDProvider baseFileNames

  scotty 8080 $ do

    docApiBase readonly ctss provider

    middleware $ staticPolicy (noDots >-> policy handleIndex >-> addBase "webdocs/dist")

    get (regex ".*") $ file "webdocs/dist/index.html"
