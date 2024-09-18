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

import           Data.Aeson                    (ToJSON)
import           Web.Scotty

import qualified Data.HashMap.Strict           as H
import qualified Data.Text.Lazy                as T
import           GHC.Generics                  (Generic)
import           Network.Wai.Middleware.Static

import           Control.Monad
import           Control.Monad.Trans           (lift)
import           CRes
import           CtService
import           Data.Bifunctor                (Bifunctor (first))
import           Data.Graph
import           Eval                          (evalAllTargetModes, evalAnnots,
                                                prgmFromGraphData)
import           MapMeta                       (addMetaID, interleaveMeta,
                                                interleavePrgm, zip3MetaFun)
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Desugarf            (desFinalPasses, desPrgm)
import           Syntax.Ct.Desugarf.Expr       (desFileImport)
import           Syntax.Ct.MapRawMeta          (mapMetaRawPrgm, mapMetaRawPrgmM)
import           Syntax.Ct.Parser.Expr         (pExpr)
import           Syntax.Ct.Prgm                (RawExpr (RawValue),
                                                RawStatement (RawAnnot),
                                                RawStatementTree (RawStatementTree),
                                                mkRawFileImport)
import           Syntax.Parsers                (mkDesCanonicalImportStr)
import           Text.Megaparsec               (errorBundlePretty, runParser)
import           Text.Printf
import           TypeCheck                     (typecheckPrgms)
import           TypeCheck.Common              (TPrgm, filterTraceConstrain,
                                                flipTraceConstrain,
                                                typeCheckToRes)
import           Utils

data ResSuccess a n = Success a [n]
  | ResFail [n]
  deriving (Generic, ToJSON)

maybeJson :: (ToJSON a) => CRes a -> ActionM ()
maybeJson (CRes notes r) = json $ Success r notes
maybeJson (CErr notes)   = json (ResFail notes :: ResSuccess () CNote)

-- | Filters a program's object map and class map to only highlight a particular type or class name
filterByType :: String -> TPrgm -> TPrgm
filterByType name (objMap, ClassGraph classGraph, _) = (objMap', classGraph', [])
  where
    objMap' = filter (relativeNameMatches name . oaObjPath) objMap
    classGraph' = ClassGraph $ graphFromEdges $ filter (\(_, n, subTypes) -> relativeNameMatches name (fromPartialName n) || n `elem` subTypes) $ graphToNodes classGraph

mkLiveWDProvider :: [String] -> IO (IO CTSS)
mkLiveWDProvider baseFileNames = do
  ss1 <- ctssRead $ ctssBaseFiles baseFileNames
  ss2 <- ctssBuildAll ss1
  return $ do
    ss3 <- ctssRead ss2
    ctssBuildAll ss3 -- TODO Remove buildAll with incremental support in CTSS

mkCacheWDProvider :: [String] -> IO (IO CTSS)
mkCacheWDProvider baseFileNames = do
  ss1 <- ctssRead $ ctssBaseFiles baseFileNames
  ss2 <- ctssBuildAll ss1
  return $ return ss2

docApiBase :: IO CTSS -> ScottyM ()
docApiBase getProvider = do

  get "/api/raw" $ do
    provider <- liftAndCatchIO getProvider
    resp <- liftAndCatchIO $ runCResT $ do
      rawPrgms <- getRawPrgm provider
      return $ graphToNodes rawPrgms
    maybeJson resp

  get "/api/toc" $ do
    provider <- liftAndCatchIO getProvider
    resp <- liftAndCatchIO $ runCResT $ do
      rawPrgms <- getRawPrgm provider
      let rawPrgms' = map snd3 $ graphToNodes rawPrgms
      return $ zip (map impDisp rawPrgms') rawPrgms'
    maybeJson resp

  get "/api/page" $ do
    provider <- liftAndCatchIO getProvider
    prgmName <- param "prgmName"
    resp <- liftAndCatchIO $ runCResT $ do
      prgmName' <- lift $ mkDesCanonicalImportStr prgmName
      rawPrgms <- getRawPrgm provider
      rawPrgm' <- case graphLookup prgmName' rawPrgms of
        Just p  -> return p
        Nothing -> fail $ printf "Could not find program %s" (show prgmName)
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
      return $ mapMetaRawPrgm (zip3MetaFun (interleaveMeta tprgm') (interleaveMeta targetModes) (interleaveMeta annots')) rawPrgm'
    maybeJson resp

  get "/api/annot" $ do
    provider <- liftAndCatchIO getProvider
    annot <- param "annot"
    resp <- liftAndCatchIO $ runCResT $ do
      rawPrgm <- case runParser pExpr "<annot>" annot of
        Left err     -> fail $ show $ errorBundlePretty err
        Right parsed -> return ([], [RawStatementTree (RawAnnot parsed) []])
      rawPrgm' <- lift $ mapMetaRawPrgmM addMetaID rawPrgm
      (annotDesPrgm, annotPrgmName, _) <- asCResT $ desPrgm (rawPrgm', desFileImport $ mkRawFileImport $ RawValue emptyMetaN "<annot>", [])
      [annotDesPrgm'] <- desFinalPasses [annotDesPrgm] []
      tprgmsTrace <- getTPrgmWithTrace provider
      tprgms <- getTPrgm provider
      let annotDeps = map snd3 $ graphToNodes tprgms
      [(annotTprgm, _, _)] <- asCResT $ typeCheckToRes $ typecheckPrgms [annotDesPrgm'] (map fst3 $ graphToNodes tprgmsTrace)
      let joinedTprgms = graphFromEdges ((annotTprgm, annotPrgmName, annotDeps) : graphToNodes tprgms)
      let targetModes = evalAllTargetModes annotTprgm
      maybeAnnots <- asCResT $ evalAnnots annotPrgmName joinedTprgms
      let annots' = H.fromList $ map (first (getMetaID . getExprMeta)) maybeAnnots
      return $ mapMetaRawPrgm (zip3MetaFun (interleaveMeta $ interleavePrgm annotTprgm) (interleaveMeta targetModes) (interleaveMeta annots')) rawPrgm'
    maybeJson resp

  get "/api/desugar" $ do
    provider <- liftAndCatchIO getProvider
    resp <- liftAndCatchIO $ runCResT $ do
      prgmGraph <- getPrgm provider
      return $ mergePrgms $ map fst3 $ graphToNodes prgmGraph
    maybeJson resp

  get "/api/constrain" $ do
    provider <- liftAndCatchIO getProvider
    prgmName <- param "prgmName"
    resp <- liftAndCatchIO $ runCResT $ do
      prgmName' <- lift $ mkDesCanonicalImportStr prgmName
      tprgmWithTraceGraph <- getTPrgmWithTrace provider
      case graphLookup prgmName' tprgmWithTraceGraph of
        Just (tprgm, vprgm, _tr) -> return (tprgm, vprgm)
        Nothing -> fail $ printf "Could not find typechecked program %s" (show prgmName)
    maybeJson resp

  get "/api/constrain/pnt/:pnt" $ do
    provider <- liftAndCatchIO getProvider
    prgmName <- param "prgmName"
    pnt <- param "pnt"
    resp <- liftAndCatchIO $ runCResT $ do
      prgmName' <- lift $ mkDesCanonicalImportStr prgmName
      tprgmWithTraceGraph <- getTPrgmWithTrace provider
      case graphLookup prgmName' tprgmWithTraceGraph of
        Just (_, _, tr) -> return $ flipTraceConstrain $ filterTraceConstrain tr pnt
        Nothing -> fail $ printf "Could not find typechecked program %s" (show prgmName)
    maybeJson resp

  get "/api/typecheck" $ do
    provider <- liftAndCatchIO getProvider
    resp <- liftAndCatchIO $ runCResT $ getTPrgmJoined provider
    maybeJson resp

  get "/api/treebuild" $ do
    provider <- liftAndCatchIO getProvider
    resp <- liftAndCatchIO $ runCResT $ getTBPrgmJoined provider
    maybeJson resp

  get "/api/type" $ do
    provider <- liftAndCatchIO getProvider
    objName <- param "type"
    resp <- liftAndCatchIO $ runCResT $ do
      tprgm <- getTPrgmJoined provider
      return $ filterByType objName tprgm
    maybeJson resp

  get "/api/treebug" $ do
    provider <- liftAndCatchIO getProvider
    prgmName <- param "prgmName"
    fun <- param "function" `rescue` (\_ -> return "main")
    resp <- liftAndCatchIO $ runCResT $ do
      prgmName' <- lift $ mkDesCanonicalImportStr prgmName
      getTreebug provider prgmName' fun
    maybeJson resp

  get "/api/eval" $ do
    provider <- liftAndCatchIO getProvider
    prgmName <- param "prgmName"
    fun <- param "function" `rescue` (\_ -> return "main")
    resp <- liftAndCatchIO $ runCResT $ do
      prgmName' <- lift $ mkDesCanonicalImportStr prgmName
      getEvaluated provider prgmName' fun
    maybeJson resp

  get "/api/evalBuild" $ do
    provider <- liftAndCatchIO getProvider
    prgmName <- param "prgmName"
    fun <- param "function" `rescue` (\_ -> return "main")
    resp <- liftAndCatchIO $ runCResT $ do
      prgmName' <- lift $ mkDesCanonicalImportStr prgmName
      getEvalBuild provider prgmName' fun
    maybeJson resp

  get "/api/web" $ do
    provider <- liftAndCatchIO getProvider
    prgmName <- param "prgmName"
    fun <- param "function" `rescue` (\_ -> return "main")
    maybeBuild <- liftAndCatchIO $ runCResT $ do
      prgmName' <- lift $ mkDesCanonicalImportStr prgmName
      getWeb provider prgmName' fun
    case maybeBuild of
      CRes notes build -> do
        unless (null notes) $ liftAndCatchIO $ putStrLn $ prettyCNotes notes
        html (T.pack build)
      CErr notes -> do
        unless (null notes) $ liftAndCatchIO $ putStrLn $ prettyCNotes notes
        fail $ printf "Could not build web page for %s.%s" prgmName fun


docApi :: Bool -> [String] -> IO ()
docApi cached baseFileNames = do

  provider <- if cached
    then mkCacheWDProvider baseFileNames
    else mkLiveWDProvider baseFileNames

  scotty 31204 $ docApiBase provider

docServe :: Bool -> [String] -> IO ()
docServe cached baseFileNames = do

  let handleIndex p = case p of
        "" -> Just "index.html"
        _  -> Just p

  provider <- if cached
    then mkCacheWDProvider baseFileNames
    else mkLiveWDProvider baseFileNames

  scotty 8080 $ do

    docApiBase provider

    middleware $ staticPolicy (noDots >-> policy handleIndex >-> addBase "webdocs/dist")

    get (regex ".*") $ file "webdocs/dist/index.html"
