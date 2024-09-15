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

import           Control.Monad
import           Control.Monad.Trans           (lift)
import           CRes
import           Data.Bifunctor                (Bifunctor (first))
import           Data.Graph
import           Data.Maybe
import           Eval                          (evalAllTargetModes, evalAnnots,
                                                evalBuild, evalBuildAll,
                                                evalRun, prgmFromGraphData)
import           Eval.Common                   (EvalMetaDat, EvalResult, TExpr,
                                                Val (..))
import           MapMeta                       (addMetaID, interleaveMeta,
                                                interleavePrgm, zip3MetaFun)
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Desugarf            (desFiles, desFinalPasses,
                                                desPrgm)
import           Syntax.Ct.MapRawMeta          (mapMetaRawPrgm, mapMetaRawPrgmM)
import           Syntax.Ct.Parser.Expr         (pExpr)
import           Syntax.Ct.Parser.Syntax       (DesPrgm, PPrgmGraphData)
import           Syntax.Ct.Prgm                (RawExpr (RawValue),
                                                RawStatement (RawAnnot),
                                                RawStatementTree (RawStatementTree),
                                                mkRawFileImport, rawImpDisp)
import           Syntax.Parsers                (mkDesCanonicalImportStr,
                                                mkRawCanonicalImportStr,
                                                readFiles)
import           Text.Megaparsec               (errorBundlePretty, runParser)
import           Text.Printf
import           TypeCheck                     (typecheckPrgmWithTrace,
                                                typecheckPrgms)
import           TypeCheck.Common              (TPrgm, TraceConstrain, VPrgm,
                                                filterTraceConstrain,
                                                flipTraceConstrain,
                                                typeCheckToRes)
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
  = WDProvider {
    cCore           :: Bool
  , cBaseFileNames   :: [String]
  , cRaw            :: CRes PPrgmGraphData
  , cPrgm           :: CRes (GraphData DesPrgm FileImport)
  , cTPrgmWithTrace :: CRes (GraphData (TPrgm, VPrgm, TraceConstrain) FileImport)
  , cTPrgm          :: CRes (GraphData TPrgm FileImport)
  , cTBPrgm          :: CRes (GraphData TBPrgm FileImport)
  , cAnnots          :: CRes (GraphData [(Expr EvalMetaDat, Val)] FileImport)
                    }

emptyWDProvider :: WDProvider
emptyWDProvider = WDProvider {
        cCore = True
      , cBaseFileNames = []
      , cRaw = return $ graphFromEdges []
      , cPrgm = return $ graphFromEdges []
      , cTPrgmWithTrace = return $ graphFromEdges []
      , cTPrgm = return $ graphFromEdges []
      , cTBPrgm = return $ graphFromEdges []
      , cAnnots = return $ graphFromEdges []
                            }


mkWDProvider :: Bool -> [String] -> IO WDProvider
mkWDProvider includeCore baseFileNames = do
  p <- runCResT $ do
    baseFileNames' <- lift $ mapM mkRawCanonicalImportStr baseFileNames
    rawPrgm <- readFiles includeCore baseFileNames'
    prgm <- desFiles rawPrgm
    let withTrace = typecheckPrgmWithTrace prgm
    let tprgm = fmap (fmapGraph fst3) withTrace
    let tbprgm = tprgm >>= evalBuildAll
    let annots' = tprgm >>= (\jtprgm -> fmap graphFromEdges $ traverse (\(_, n, deps) -> (, n, deps) <$> evalAnnots n jtprgm) $  graphToNodes jtprgm)
    return $ WDProvider {
        cCore = includeCore
      , cBaseFileNames = baseFileNames
      , cRaw = return rawPrgm
      , cPrgm = return prgm
      , cTPrgmWithTrace = withTrace
      , cTPrgm = tprgm
      , cTBPrgm = tbprgm
      , cAnnots = annots'
                            }
  case p of
    CRes notes r -> do
      unless (null notes) $ putStrLn $ prettyCNotes notes
      return r
    CErr notes -> do
      unless (null notes) $ putStrLn $ prettyCNotes notes
      fail "Could not build webdocs"

mkLiveWDProvider :: Bool -> [String] -> IO (IO WDProvider)
mkLiveWDProvider includeCore baseFileNames = return $ mkWDProvider includeCore baseFileNames

mkCacheWDProvider :: Bool -> [String] -> IO (IO WDProvider)
mkCacheWDProvider includeCore baseFileNames = do
  p <- mkWDProvider includeCore baseFileNames
  return $ return p

getRawPrgm :: WDProvider -> CResT IO PPrgmGraphData
getRawPrgm WDProvider{cRaw} = asCResT cRaw

getPrgm :: WDProvider -> CResT IO (GraphData DesPrgm FileImport)
getPrgm WDProvider{cPrgm} = asCResT cPrgm

getTPrgmWithTrace :: WDProvider -> CResT IO (GraphData (TPrgm, VPrgm, TraceConstrain) FileImport)
getTPrgmWithTrace WDProvider{cTPrgmWithTrace} = asCResT cTPrgmWithTrace

getTPrgm :: WDProvider -> CResT IO (GraphData TPrgm FileImport)
getTPrgm WDProvider{cTPrgm} = asCResT cTPrgm

getTPrgmJoined :: WDProvider -> CResT IO TPrgm
getTPrgmJoined provider = do
  base <- getTPrgm provider
  return $ mergePrgms $ map fst3 $ graphToNodes base

getTBPrgm :: WDProvider -> CResT IO (GraphData TBPrgm FileImport)
getTBPrgm WDProvider{cTBPrgm} = asCResT cTBPrgm

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
  annotsGraph <- asCResT $ cAnnots provider
  return $ fromMaybe [] $ graphLookup prgmName annotsGraph

getWeb :: WDProvider -> FileImport -> String -> CResT IO String
getWeb provider prgmName fun = do
  base <- getTPrgm provider
  (TupleVal _ args, _) <- evalBuild fun prgmName base
  case H.lookup "contents" args of
    Just (StrVal s) -> return s
    _               -> return "";

docApiBase :: IO WDProvider -> ScottyM ()
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
      return $ zip (map rawImpDisp rawPrgms') rawPrgms'
    maybeJson resp

  get "/api/page" $ do
    provider <- liftAndCatchIO getProvider
    prgmName <- param "prgmName"
    resp <- liftAndCatchIO $ runCResT $ do
      prgmNameRaw <- lift $ mkRawCanonicalImportStr prgmName
      prgmName' <- lift $ mkDesCanonicalImportStr prgmName
      rawPrgms <- getRawPrgm provider
      rawPrgm' <- case graphLookup prgmNameRaw rawPrgms of
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
      (annotDesPrgm, annotPrgmName, _) <- asCResT $ desPrgm (rawPrgm', mkRawFileImport $ RawValue emptyMetaN "<annot>", [])
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


docApi :: Bool -> Bool -> [String] -> IO ()
docApi cached includeCore baseFileNames = do

  provider <- if cached
    then mkCacheWDProvider includeCore baseFileNames
    else mkLiveWDProvider includeCore baseFileNames

  scotty 31204 $ docApiBase provider

docServe :: Bool -> Bool -> [String] -> IO ()
docServe cached includeCore baseFileNames = do

  let handleIndex p = case p of
        "" -> Just "index.html"
        _  -> Just p

  provider <- if cached
    then mkCacheWDProvider includeCore baseFileNames
    else mkLiveWDProvider includeCore baseFileNames

  scotty 8080 $ do

    docApiBase provider

    middleware $ staticPolicy (noDots >-> policy handleIndex >-> addBase "webdocs/dist")

    get (regex ".*") $ file "webdocs/dist/index.html"
