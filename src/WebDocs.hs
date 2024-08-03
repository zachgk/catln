--------------------------------------------------------------------
-- |
-- Module    :  WebDos
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This file defines the server for the Catln webdocs. It has two
-- modes defined by the 'WDProvider': cached and live. The frontend
-- for the webdocs is located inside the /webdocs directory of the
-- repo.
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

import           Control.Monad.Trans           (lift)
import           CRes
import           Data.Bifunctor                (Bifunctor (first))
import           Data.Graph
import           Eval                          (evalAllTargetModes, evalAnnots,
                                                evalBuild, evalBuildAll,
                                                evalRun)
import           Eval.Common                   (EvalMetaDat, EvalResult, TExpr,
                                                Val (..))
import           MapMeta                       (interleaveMeta, interleavePrgm,
                                                zip3MetaFun)
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Desugarf            (desFiles)
import           Syntax.Ct.MapRawMeta          (mapMetaRawPrgm)
import           Syntax.Ct.Parser.Syntax       (DesPrgm, PPrgmGraphData)
import           Syntax.Ct.Prgm                (rawImpDisp)
import           Syntax.Parsers                (mkDesCanonicalImportStr,
                                                mkRawCanonicalImportStr,
                                                readFiles)
import           Text.Printf
import           TypeCheck                     (typecheckPrgm,
                                                typecheckPrgmWithTrace)
import           TypeCheck.Common              (TPrgm, TraceConstrain, VPrgm,
                                                filterTraceConstrain,
                                                flipTraceConstrain)
import           Utils

type TBPrgm = Prgm TExpr EvalMetaDat

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

data WDProvider
  = LiveWDProvider Bool String
  | CacheWDProvider {
    cCore           :: Bool
  , cBaseFileName   :: String
  , cRaw            :: CRes PPrgmGraphData
  , cPrgm           :: CRes (GraphData DesPrgm FileImport)
  , cTPrgmWithTrace :: CRes (GraphData (TPrgm, VPrgm, TraceConstrain) FileImport)
  , cTPrgm          :: CRes (GraphData TPrgm FileImport)
  , cTBPrgm          :: CRes (GraphData TBPrgm FileImport)
                    }

mkCacheWDProvider :: Bool -> String -> IO WDProvider
mkCacheWDProvider includeCore baseFileName = do
  let live = LiveWDProvider includeCore baseFileName
  p <- runCResT $ do
    rawPrgm <- getRawPrgm live
    prgm <- getPrgm live
    withTrace <- getTPrgmWithTrace live
    tprgm <- getTPrgm live
    tbprgm <- getTBPrgm live
    return $ CacheWDProvider {
        cCore = includeCore
      , cBaseFileName = baseFileName
      , cRaw = return rawPrgm
      , cPrgm = return prgm
      , cTPrgmWithTrace = return withTrace
      , cTPrgm = return tprgm
      , cTBPrgm = return tbprgm
                            }
  case p of
    CRes notes r -> do
      putStrLn $ prettyCNotes notes
      return r
    CErr notes -> do
      putStrLn $ prettyCNotes notes
      fail "Could not build webdocs"

getRawPrgm :: WDProvider -> CResT IO PPrgmGraphData
getRawPrgm (LiveWDProvider includeCore baseFileName) = do
  baseFileName' <- lift $ mkRawCanonicalImportStr baseFileName
  readFiles includeCore [baseFileName']
getRawPrgm CacheWDProvider{cRaw} = asCResT cRaw

getPrgm :: WDProvider -> CResT IO (GraphData DesPrgm FileImport)
getPrgm provider@LiveWDProvider{} = do
  base <- getRawPrgm provider
  desFiles base
getPrgm CacheWDProvider{cPrgm} = asCResT cPrgm

getTPrgmWithTrace :: WDProvider -> CResT IO (GraphData (TPrgm, VPrgm, TraceConstrain) FileImport)
getTPrgmWithTrace provider@LiveWDProvider{} = do
  base <- getPrgm provider
  asCResT $ typecheckPrgmWithTrace base
getTPrgmWithTrace CacheWDProvider{cTPrgmWithTrace} = asCResT cTPrgmWithTrace

getTPrgm :: WDProvider -> CResT IO (GraphData TPrgm FileImport)
getTPrgm provider@LiveWDProvider{} = do
  base <- getPrgm provider
  asCResT $ typecheckPrgm base
getTPrgm CacheWDProvider{cTPrgm} = asCResT cTPrgm

getTPrgmJoined :: WDProvider -> CResT IO TPrgm
getTPrgmJoined provider = do
  base <- getTPrgm provider
  return $ mergePrgms $ map fst3 $ graphToNodes base

getTBPrgm :: WDProvider -> CResT IO (GraphData TBPrgm FileImport)
getTBPrgm provider@LiveWDProvider{} = do
  base <- getTPrgm provider
  asCResT $ evalBuildAll base
getTBPrgm CacheWDProvider{cTBPrgm} = asCResT cTBPrgm

getTBPrgmJoined :: WDProvider -> CResT IO TBPrgm
getTBPrgmJoined provider = do
  base <- getTBPrgm provider
  return $ mergePrgms $ map fst3 $ graphToNodes base

getTreebug :: WDProvider -> FileImport -> String -> CResT IO EvalResult
getTreebug provider prgmName fun = do
  base <- getTPrgm provider
  snd <$> evalRun fun prgmName base

getEvaluated :: WDProvider -> FileImport -> String -> CResT IO Integer
getEvaluated provider prgmName fun = do
  base <- getTPrgm provider
  fst <$> evalRun fun prgmName base

getEvalBuild :: WDProvider -> FileImport -> String -> CResT IO Val
getEvalBuild provider prgmName fun = do
  base <- getTPrgm provider
  fst <$> evalBuild fun prgmName base

getEvalAnnots :: WDProvider -> FileImport -> CResT IO [(Expr EvalMetaDat, Val)]
getEvalAnnots provider prgmName = do
  base <- getTPrgm provider
  asCResT $ evalAnnots prgmName base

getWeb :: WDProvider -> FileImport -> String -> CResT IO String
getWeb provider prgmName fun = do
  base <- getTPrgm provider
  (TupleVal _ args, _) <- evalBuild fun prgmName base
  case H.lookup "contents" args of
    Just (StrVal s) -> return s
    _               -> return "";

docApiBase :: WDProvider -> ScottyM ()
docApiBase provider = do

  get "/api/raw" $ do
    resp <- liftAndCatchIO $ runCResT $ do
      rawPrgms <- getRawPrgm provider
      return $ graphToNodes rawPrgms
    maybeJson resp

  get "/api/toc" $ do
    resp <- liftAndCatchIO $ runCResT $ do
      rawPrgms <- getRawPrgm provider
      let rawPrgms' = map snd3 $ graphToNodes rawPrgms
      return $ zip (map rawImpDisp rawPrgms') rawPrgms'
    maybeJson resp

  get "/api/page" $ do
    prgmName <- param "prgmName"
    resp <- liftAndCatchIO $ runCResT $ do
      prgmNameRaw <- lift $ mkRawCanonicalImportStr prgmName
      prgmName' <- lift $ mkDesCanonicalImportStr prgmName
      rawPrgms <- getRawPrgm provider
      rawPrgm' <- case graphLookup prgmNameRaw rawPrgms of
        Just p  -> return p
        Nothing -> fail $ printf "Could not find program %s" (show prgmName)
      maybeTPrgm <- lift $ runCResT $ do
        tprgms <- getTPrgm provider
        case graphLookup prgmName' tprgms of
          Just tprgm -> return tprgm
          Nothing -> fail $ printf "Could not find typechecked program %s" (show prgmName)
      maybeAnnots <- lift $ runCResT $ getEvalAnnots provider prgmName'
      let tprgm' = maybe H.empty interleavePrgm (cresToMaybe maybeTPrgm)
      let targetModes = maybe H.empty evalAllTargetModes (cresToMaybe maybeTPrgm)
      let annots' = maybe H.empty (H.fromList . map (first (getMetaID . getExprMeta))) (cresToMaybe maybeAnnots)
      return $ mapMetaRawPrgm (zip3MetaFun (interleaveMeta tprgm') (interleaveMeta targetModes) (interleaveMeta annots')) rawPrgm'
    maybeJson resp

  get "/api/desugar" $ do
    resp <- liftAndCatchIO $ runCResT $ do
      prgmGraph <- getPrgm provider
      return $ mergePrgms $ map fst3 $ graphToNodes prgmGraph
    maybeJson resp

  get "/api/constrain" $ do
    prgmName <- param "prgmName"
    resp <- liftAndCatchIO $ runCResT $ do
      prgmName' <- lift $ mkDesCanonicalImportStr prgmName
      tprgmWithTraceGraph <- getTPrgmWithTrace provider
      case graphLookup prgmName' tprgmWithTraceGraph of
        Just (tprgm, vprgm, _tr) -> return (tprgm, vprgm)
        Nothing -> fail $ printf "Could not find typechecked program %s" (show prgmName)
    maybeJson resp

  get "/api/constrain/pnt/:pnt" $ do
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
    resp <- liftAndCatchIO $ runCResT $ getTPrgmJoined provider
    maybeJson resp

  get "/api/treebuild" $ do
    resp <- liftAndCatchIO $ runCResT $ getTBPrgmJoined provider
    maybeJson resp

  get "/api/object/:objName" $ do
    objName <- param "objName"
    resp <- liftAndCatchIO $ runCResT $ do
      tprgm <- getTPrgmJoined provider
      return $ filterByType objName tprgm
    maybeJson resp

  get "/api/class/:className" $ do
    className <- param "className"
    resp <- liftAndCatchIO $ runCResT $ do
      tprgm <- getTPrgmJoined provider
      return $ filterByType className tprgm
    maybeJson resp

  get "/api/treebug" $ do
    prgmName <- param "prgmName"
    fun <- param "function" `rescue` (\_ -> return "main")
    resp <- liftAndCatchIO $ runCResT $ do
      prgmName' <- lift $ mkDesCanonicalImportStr prgmName
      getTreebug provider prgmName' fun
    maybeJson resp

  get "/api/eval" $ do
    prgmName <- param "prgmName"
    fun <- param "function" `rescue` (\_ -> return "main")
    resp <- liftAndCatchIO $ runCResT $ do
      prgmName' <- lift $ mkDesCanonicalImportStr prgmName
      getEvaluated provider prgmName' fun
    maybeJson resp

  get "/api/evalBuild" $ do
    prgmName <- param "prgmName"
    fun <- param "function" `rescue` (\_ -> return "main")
    resp <- liftAndCatchIO $ runCResT $ do
      prgmName' <- lift $ mkDesCanonicalImportStr prgmName
      getEvalBuild provider prgmName' fun
    maybeJson resp

  get "/api/web" $ do
    prgmName <- param "prgmName"
    fun <- param "function" `rescue` (\_ -> return "main")
    maybeBuild <- liftAndCatchIO $ runCResT $ do
      prgmName' <- lift $ mkDesCanonicalImportStr prgmName
      getWeb provider prgmName' fun
    case maybeBuild of
      CRes notes build -> do
        liftAndCatchIO $putStrLn $ prettyCNotes notes
        html (T.pack build)
      CErr notes -> do
        liftAndCatchIO $putStrLn $ prettyCNotes notes
        fail $ printf "Could not build web page for %s.%s" prgmName fun


docApi :: Bool -> Bool -> String -> IO ()
docApi cached includeCore baseFileName = do

  provider <- if cached
    then mkCacheWDProvider includeCore baseFileName
    else return $ LiveWDProvider includeCore baseFileName

  scotty 31204 $ docApiBase provider

docServe :: Bool -> Bool -> String -> IO ()
docServe cached includeCore baseFileName = do

  let handleIndex p = case p of
        "" -> Just "index.html"
        _  -> Just p

  provider <- if cached
    then mkCacheWDProvider includeCore baseFileName
    else return $ LiveWDProvider includeCore baseFileName

  scotty 8080 $ do

    docApiBase provider

    middleware $ staticPolicy (noDots >-> policy handleIndex >-> addBase "webdocs/build")

    get (regex ".*") $ file "webdocs/build/index.html"
