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

import           CRes
import           Data.Graph
import           Data.Maybe                    (fromJust)
import           Eval                          (evalAnnots, evalBuild, evalRun)
import           Eval.Common                   (EvalResult, Val (..))
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Desugarf            (desFiles)
import           Syntax.Ct.Parser              (readFiles)
import           Syntax.Ct.Parser.Syntax       (FinalDesPrgm, PPrgmGraphData)
import           TypeCheck                     (typecheckPrgm,
                                                typecheckPrgmWithTrace)
import           TypeCheck.Common              (TPrgm, TraceConstrain, VPrgm)
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
    objMap' = filter (\(o, _, _) -> relativeNameMatches name (objPath o)) objMap
    classGraph' = ClassGraph $ graphFromEdges $ filter (\(_, n, subTypes) -> relativeNameMatches name (fromPartialName n) || n `elem` subTypes) $ graphToNodes classGraph

data WDProvider
  = LiveWDProvider Bool String
  | CacheWDProvider {
    cCore           :: Bool
  , cBaseFileName   :: String
  , cRaw            :: CRes PPrgmGraphData
  , cPrgm           :: CRes (GraphData FinalDesPrgm String)
  , cTPrgmWithTrace :: CRes (GraphData (TPrgm, VPrgm, TraceConstrain) String)
  , cTPrgm          :: CRes (GraphData TPrgm String)
                    }

mkCacheWDProvider :: Bool -> String -> IO WDProvider
mkCacheWDProvider includeCore baseFileName = do
  let live = LiveWDProvider includeCore baseFileName
  rawPrgm <- getRawPrgm live
  prgm <- getPrgm live
  withTrace <- getTPrgmWithTrace live
  tprgm <- getTPrgm live
  return $ CacheWDProvider {
      cCore = includeCore
    , cBaseFileName = baseFileName
    , cRaw = rawPrgm
    , cPrgm = prgm
    , cTPrgmWithTrace = withTrace
    , cTPrgm = tprgm
                           }

getRawPrgm :: WDProvider -> IO (CRes PPrgmGraphData)
getRawPrgm (LiveWDProvider includeCore baseFileName) = readFiles includeCore True [baseFileName]
getRawPrgm CacheWDProvider{cRaw} = return cRaw

getPrgm :: WDProvider -> IO (CRes (GraphData FinalDesPrgm String))
getPrgm provider@LiveWDProvider{} = do
  base <- getRawPrgm provider
  return (base >>= desFiles)
getPrgm CacheWDProvider{cPrgm} = return cPrgm

getTPrgmWithTrace :: WDProvider -> IO (CRes (GraphData (TPrgm, VPrgm, TraceConstrain) String))
getTPrgmWithTrace provider@LiveWDProvider{} = do
  base <- getPrgm provider
  return (base >>= typecheckPrgmWithTrace)
getTPrgmWithTrace CacheWDProvider{cTPrgmWithTrace} = return cTPrgmWithTrace

getTPrgm :: WDProvider -> IO (CRes (GraphData TPrgm String))
getTPrgm provider@LiveWDProvider{} = do
  base <- getPrgm provider
  return (base >>= typecheckPrgm)
getTPrgm CacheWDProvider{cTPrgm} = return cTPrgm

getTPrgmJoined :: WDProvider -> IO (CRes TPrgm)
getTPrgmJoined provider = do
  base <- getTPrgm provider
  return (mergePrgms . map fst3 . graphToNodes <$> base)

getTreebug :: WDProvider -> String -> String -> IO EvalResult
getTreebug provider prgmName fun = do
  base <- getTPrgm provider
  let pre = base >>= evalRun fun prgmName
  case pre of
    CRes _ r -> snd <$> r
    CErr _   -> fail "No eval result found"

getEvaluated :: WDProvider -> String -> String -> IO Integer
getEvaluated provider prgmName fun = do
  base <- getTPrgm provider
  let pre = base >>= evalRun fun prgmName
  case pre of
    CRes _ r -> fst <$> r
    CErr _   -> return 999

getEvalBuild :: WDProvider -> String -> String -> IO Val
getEvalBuild provider prgmName fun = do
  base <- getTPrgm provider
  let pre = base >>= evalBuild fun prgmName
  case pre of
    CRes _ r -> fst <$> r
    CErr _   -> return NoVal

getEvalAnnots :: WDProvider -> String -> IO [(Expr (), Val)]
getEvalAnnots provider prgmName = do
  base <- getTPrgm provider
  let pre = base >>= evalAnnots prgmName
  case pre of
    CRes _ r -> return r
    CErr _   -> return []

getWeb :: WDProvider -> String -> String -> IO String
getWeb provider prgmName fun = do
  base <- getTPrgm provider
  let pre = base >>= evalBuild fun prgmName
  case pre of
    CRes _ r -> do
      (TupleVal _ args, _) <- r
      case H.lookup "contents" args of
        Just (StrVal s) -> return s
        _               -> return "";
    CErr _ -> return ""

docApiBase :: WDProvider -> ScottyM ()
docApiBase provider = do

  get "/api/raw" $ do
    maybeRawPrgms <- liftAndCatchIO $ getRawPrgm provider
    let maybeRawPrgms' = graphToNodes <$> maybeRawPrgms
    maybeJson maybeRawPrgms'

  get "/api/toc" $ do
    maybeRawPrgms <- liftAndCatchIO $ getRawPrgm provider
    let maybeRawPrgms' = map snd3 . graphToNodes <$> maybeRawPrgms
    maybeJson maybeRawPrgms'

  get "/api/page" $ do
    prgmName <- param "prgmName"
    maybeRawPrgms <- liftAndCatchIO $ getRawPrgm provider
    let maybeRawPrgms' = fromJust . graphLookup prgmName <$> maybeRawPrgms

    maybeTPrgms <- liftAndCatchIO $ getTPrgm provider
    let maybeTPrgms' = fromJust . graphLookup prgmName <$> maybeTPrgms

    annots <- liftAndCatchIO $ getEvalAnnots provider prgmName
    maybeJson $ do
      rawPrgm <- maybeRawPrgms'
      tprgm <- maybeTPrgms'
      return (rawPrgm, tprgm, annots)

  get "/api/desugar" $ do
    maybePrgmGraph <- liftAndCatchIO $ getPrgm provider
    let maybePrgm = mergePrgms . map fst3 . graphToNodes <$> maybePrgmGraph
    maybeJson maybePrgm

  get "/api/constrain" $ do
    prgmName <- param "prgmName"
    maybeTprgmWithTraceGraph <- liftAndCatchIO $ getTPrgmWithTrace provider
    let maybeTprgmWithTrace = maybeTprgmWithTraceGraph >>= \graphData -> do
          case graphLookup prgmName graphData of
            Just v -> return $ fst3 v
            Nothing -> CErr [MkCNote $ GenCErr Nothing "Invalid file to constrain"]
    maybeJson maybeTprgmWithTrace

  get "/api/typecheck" $ do
    maybeTprgm <- liftAndCatchIO $ getTPrgmJoined provider
    maybeJson maybeTprgm

  get "/api/object/:objName" $ do
    objName <- param "objName"
    maybeTprgm <- liftAndCatchIO $ getTPrgmJoined provider
    let filterTprgm = filterByType objName <$> maybeTprgm
    maybeJson filterTprgm

  get "/api/class/:className" $ do
    className <- param "className"
    maybeTprgm <- liftAndCatchIO $ getTPrgmJoined provider
    let filterTprgm = filterByType className <$> maybeTprgm
    maybeJson filterTprgm

  get "/api/treebug" $ do
    prgmName <- param "prgmName"
    fun <- param "function" `rescue` (\_ -> return "main")
    treebug <- liftAndCatchIO $ getTreebug provider prgmName fun
    json $ Success treebug ([] :: [String])

  get "/api/eval" $ do
    prgmName <- param "prgmName"
    fun <- param "function" `rescue` (\_ -> return "main")
    evaluated <- liftAndCatchIO $ getEvaluated provider prgmName fun
    json $ Success evaluated ([] :: [String])

  get "/api/evalBuild" $ do
    prgmName <- param "prgmName"
    fun <- param "function" `rescue` (\_ -> return "main")
    build <- liftAndCatchIO $ getEvalBuild provider prgmName fun
    json $ Success build ([] :: [String])

  get "/api/web" $ do
    prgmName <- param "prgmName"
    fun <- param "function" `rescue` (\_ -> return "main")
    build <- liftAndCatchIO $ getWeb provider prgmName fun
    html (T.pack build)


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
