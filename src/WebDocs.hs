--------------------------------------------------------------------
-- |
-- Module    :  WebDos
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module WebDocs where

import Web.Scotty
import Data.Aeson (ToJSON)

import qualified Data.HashMap.Strict as H
import qualified Data.Text.Lazy as T
import           GHC.Generics          (Generic)
import Network.Wai.Middleware.Static

import           Desugarf         (desFiles)
import           CRes
import TypeCheck (typecheckPrgm, typecheckPrgmWithTrace)
import           Eval (evalMainx, evalMain, evalAnnots)
import Parser (readFiles)
import Parser.Syntax (DesPrgm, PPrgmGraphData)
import TypeCheck.Common (TraceConstrain, VPrgm, TPrgm)
import Eval.Common (Val(..), Val, EvalResult)
import Syntax.Prgm
import Syntax (Typed)
import Utils
import Syntax.Types
import Data.Maybe (fromJust)

data ResSuccess a n = Success a [n]
  | ResFail [n]
  deriving (Generic, ToJSON)

maybeJson :: (ToJSON a) => CRes a -> ActionM ()
maybeJson (CRes notes r) = json $ Success r notes
maybeJson (CErr notes) = json (ResFail notes :: ResSuccess () CNote)

filterByObj :: String -> TPrgm -> TPrgm
filterByObj objName (objMap, (typeToClass, classToType), _) = (objMap', (typeToClass', classToType'), [])
  where
    objMap' = filter (\(Object{objName=n}, _) -> objName == n) objMap
    typeToClass' = H.filterWithKey (\n _ -> n == objName) typeToClass
    classToType' = H.filter (\(_, vars, types) -> any involvesType vars || any involvesType types) classToType
    involvesType (SumType leafs) = any involvesPartial $ splitPartialLeafs leafs
    involvesType _ = False
    involvesPartial PartialType{ptName, ptVars, ptProps, ptArgs} = ptName == PTypeName objName || any involvesType ptVars || any involvesType ptProps || any involvesType ptArgs

filterByClass :: String -> TPrgm -> TPrgm
filterByClass className (_, (_, classToType), _) = ([], (typeToClass', classToType'), [])
  where
    typeToClass' = H.empty
    classToType' = H.filterWithKey (\n _ -> n == className) classToType

data WDProvider
  = LiveWDProvider Bool String
  | CacheWDProvider {
    cCore :: Bool
  , cBaseFileName :: String
  , cRaw :: CRes PPrgmGraphData
  , cPrgm :: CRes (GraphData DesPrgm String)
  , cTPrgmWithTrace :: CRes (GraphData (TPrgm, VPrgm, TraceConstrain) String)
  , cTPrgm :: CRes (GraphData TPrgm String)
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
getRawPrgm (LiveWDProvider includeCore baseFileName) = readFiles includeCore [baseFileName]
getRawPrgm CacheWDProvider{cRaw} = return cRaw

getPrgm :: WDProvider -> IO (CRes (GraphData DesPrgm String))
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

getTreebug :: WDProvider -> String -> IO EvalResult
getTreebug provider prgmName = do
  base <- getTPrgm provider
  let pre = base >>= evalMainx prgmName
  case pre of
    CRes _ r -> snd <$> r
    CErr _ -> fail "No eval result found"

getEvaluated :: WDProvider -> String -> IO Integer
getEvaluated provider prgmName = do
  base <- getTPrgm provider
  let pre = base >>= evalMainx prgmName
  case pre of
    CRes _ r -> fst <$> r
    CErr _ -> return 999

getEvalBuild :: WDProvider -> String -> IO Val
getEvalBuild provider prgmName = do
  base <- getTPrgm provider
  let pre = base >>= evalMain prgmName
  case pre of
    CRes _ r -> fst <$> r
    CErr _ -> return NoVal

getEvalAnnots :: WDProvider -> String -> IO [(Expr Typed, Val)]
getEvalAnnots provider prgmName = do
  base <- getTPrgm provider
  let pre = base >>= evalAnnots prgmName
  case pre of
    CRes _ r -> return r
    CErr _ -> return []

getWeb :: WDProvider -> String -> IO String
getWeb provider prgmName = do
  base <- getTPrgm provider
  let pre = base >>= evalMain prgmName
  case pre of
    CRes _ r -> do
      (TupleVal _ args, _) <- r
      case H.lookup "contents" args of
        Just (StrVal s) -> return s
        _ -> return "";
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
    let maybeRawPrgms' = (\(_, nodeFromVertex, vertexFromKey) -> nodeFromVertex $ fromJust $ vertexFromKey prgmName) <$> maybeRawPrgms

    maybeTPrgms <- liftAndCatchIO $ getTPrgm provider
    let maybeTPrgms' = (\(_, nodeFromVertex, vertexFromKey) -> nodeFromVertex $ fromJust $ vertexFromKey prgmName) <$> maybeTPrgms

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
    let maybeTprgmWithTrace = maybeTprgmWithTraceGraph >>= \(_, prgmFromVertex, vertexFromName) -> do
          vertex <- case vertexFromName prgmName of
            Just v -> return v
            Nothing -> CErr [MkCNote $ GenCErr Nothing "Invalid file to constrain"]
          return $ fst3 $ prgmFromVertex vertex
    maybeJson maybeTprgmWithTrace

  get "/api/typecheck" $ do
    maybeTprgm <- liftAndCatchIO $ getTPrgmJoined provider
    maybeJson maybeTprgm

  get "/api/object/:objName" $ do
    objName <- param "objName"
    maybeTprgm <- liftAndCatchIO $ getTPrgmJoined provider
    let filterTprgm = filterByObj objName <$> maybeTprgm
    maybeJson filterTprgm

  get "/api/class/:className" $ do
    className <- param "className"
    maybeTprgm <- liftAndCatchIO $ getTPrgmJoined provider
    let filterTprgm = filterByClass className <$> maybeTprgm
    maybeJson filterTprgm

  get "/api/treebug" $ do
    prgmName <- param "prgmName"
    treebug <- liftAndCatchIO $ getTreebug provider prgmName
    json $ Success treebug ([] :: [String])

  get "/api/eval" $ do
    prgmName <- param "prgmName"
    evaluated <- liftAndCatchIO $ getEvaluated provider prgmName
    json $ Success evaluated ([] :: [String])

  get "/api/evalBuild" $ do
    prgmName <- param "prgmName"
    build <- liftAndCatchIO $ getEvalBuild provider prgmName
    json $ Success build ([] :: [String])

  get "/api/web" $ do
    prgmName <- param "prgmName"
    build <- liftAndCatchIO $ getWeb provider prgmName
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
        _ -> Just p

  provider <- if cached
    then mkCacheWDProvider includeCore baseFileName
    else return $ LiveWDProvider includeCore baseFileName

  scotty 8080 $ do

    docApiBase provider

    middleware $ staticPolicy (noDots >-> policy handleIndex >-> addBase "webdocs/build")
