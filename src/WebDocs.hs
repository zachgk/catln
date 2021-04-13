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

import           Desugarf         (desFiles)
import           CRes
import TypeCheck (typecheckPrgm, typecheckPrgmWithTrace)
import           Eval (evalMain, evalMainb, evalAnnots)
import Parser (readFiles)
import Parser.Syntax (DesPrgm, PPrgmGraphData)
import TypeCheck.Common (TraceConstrain, VPrgm, TPrgm)
import Eval.Common (Val(..), Val, EvalResult)
import Syntax.Prgm
import Syntax (Typed)
import Utils
import Syntax.Types

data ResSuccess a n = Success a [n]
  | ResFail [n]
  deriving (Generic, ToJSON)

maybeJson :: (ToJSON a) => CRes a -> ActionM ()
maybeJson (CRes notes r) = json $ Success r notes
maybeJson (CErr notes) = json (ResFail notes :: ResSuccess () CNote)

getRawPrgm :: Bool -> String -> IO (CRes PPrgmGraphData )
getRawPrgm includeCore fileName = readFiles includeCore [fileName]

filterByObj :: String -> TPrgm -> TPrgm
filterByObj objName (objMap, (typeToClass, classToType), _) = (objMap', (typeToClass', classToType'), [])
  where
    objMap' = filter (\(Object _ _ n _ _, _) -> objName == n) objMap
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

getPrgm :: Bool -> String -> IO (CRes (GraphData DesPrgm String))
getPrgm includeCore fileName = do
  base <- getRawPrgm includeCore fileName
  return (base >>= desFiles)

getTPrgmWithTrace :: Bool -> String -> IO (CRes (GraphData (TPrgm, VPrgm, TraceConstrain) String))
getTPrgmWithTrace includeCore fileName = do
  base <- getPrgm includeCore fileName
  return (base >>= typecheckPrgmWithTrace)

getTPrgm :: Bool -> String -> IO (CRes (GraphData TPrgm String))
getTPrgm includeCore fileName = do
  base <- getPrgm includeCore fileName
  return (base >>= typecheckPrgm)

getTPrgmJoined :: Bool -> String -> IO (CRes TPrgm)
getTPrgmJoined includeCore fileName = do
  base <- getTPrgm includeCore fileName
  return (mergePrgms . map fst3 . graphToNodes <$> base)

getTreebug :: Bool -> String -> String -> IO EvalResult
getTreebug includeCore baseFileName prgmName = do
  base <- getTPrgm includeCore baseFileName
  let pre = base >>= evalMain prgmName
  case pre of
    CRes _ r -> snd <$> r
    CErr _ -> fail "No eval result found"

getEvaluated :: Bool -> String -> String -> IO Integer
getEvaluated includeCore baseFileName prgmName = do
  base <- getTPrgm includeCore baseFileName
  let pre = base >>= evalMain prgmName
  case pre of
    CRes _ r -> fst <$> r
    CErr _ -> return 999

getEvalBuild :: Bool -> String -> String -> IO Val
getEvalBuild includeCore baseFileName prgmName = do
  base <- getTPrgm includeCore baseFileName
  let pre = base >>= evalMainb prgmName
  case pre of
    CRes _ r -> fst <$> r
    CErr _ -> return NoVal

getEvalAnnots :: Bool -> String -> String -> IO [(Expr Typed, Val)]
getEvalAnnots includeCore baseFileName prgmName = do
  base <- getTPrgm includeCore baseFileName
  let pre = base >>= evalAnnots prgmName
  case pre of
    CRes _ r -> return r
    CErr _ -> return []

getWeb :: Bool -> String -> String -> IO String
getWeb includeCore baseFileName prgmName = do
  base <- getTPrgm includeCore baseFileName
  let pre = base >>= evalMainb prgmName
  case pre of
    CRes _ r -> do
      (TupleVal _ args, _) <- r
      case H.lookup "contents" args of
        Just (StrVal s) -> return s
        _ -> return "";
    CErr _ -> return ""


docServe :: Bool -> String -> IO ()
docServe includeCore baseFileName = do

  scotty 31204 $ do
    get "/files" $ do
      json $ Success ["File: ", T.pack baseFileName] ([] :: String)

    get "/raw" $ do
      maybeRawPrgms <- liftAndCatchIO $ getRawPrgm includeCore baseFileName
      let maybeRawPrgms' = graphToNodes <$> maybeRawPrgms
      maybeJson maybeRawPrgms'

    -- TODO: pages should load single page, move TOC to separate call
    get "/pages" $ do
      maybeRawPrgms <- liftAndCatchIO $ getRawPrgm includeCore baseFileName
      let maybeRawPrgms' = graphToNodes <$> maybeRawPrgms
      annots <- liftAndCatchIO $ getEvalAnnots includeCore baseFileName baseFileName
      maybeJson $ do
        rawPrgm <- maybeRawPrgms'
        return (rawPrgm, annots)

    get "/desugar" $ do
      maybePrgmGraph <- liftAndCatchIO $ getPrgm includeCore baseFileName
      let maybePrgm = mergePrgms . map fst3 . graphToNodes <$> maybePrgmGraph
      maybeJson maybePrgm

    get "/constrain" $ do
      prgmName <- param "prgmName"
      maybeTprgmWithTraceGraph <- liftAndCatchIO $ getTPrgmWithTrace includeCore baseFileName
      let maybeTprgmWithTrace = maybeTprgmWithTraceGraph >>= \(_, prgmFromVertex, vertexFromName) -> do
            vertex <- case vertexFromName prgmName of
              Just v -> return v
              Nothing -> CErr [MkCNote $ GenCErr Nothing "Invalid file to constrain"]
            return $ fst3 $ prgmFromVertex vertex
      maybeJson maybeTprgmWithTrace

    get "/typecheck" $ do
      maybeTprgm <- liftAndCatchIO $ getTPrgmJoined includeCore baseFileName
      maybeJson maybeTprgm

    get "/object/:objName" $ do
      objName <- param "objName"
      maybeTprgm <- liftAndCatchIO $ getTPrgmJoined includeCore baseFileName
      let filterTprgm = filterByObj objName <$> maybeTprgm
      maybeJson filterTprgm

    get "/class/:className" $ do
      className <- param "className"
      maybeTprgm <- liftAndCatchIO $ getTPrgmJoined includeCore baseFileName
      let filterTprgm = filterByClass className <$> maybeTprgm
      maybeJson filterTprgm

    get "/treebug" $ do
      prgmName <- param "prgmName"
      treebug <- liftAndCatchIO $ getTreebug includeCore baseFileName prgmName
      json $ Success treebug ([] :: [String])

    get "/eval" $ do
      prgmName <- param "prgmName"
      evaluated <- liftAndCatchIO $ getEvaluated includeCore baseFileName prgmName
      json $ Success evaluated ([] :: [String])

    get "/evalBuild" $ do
      prgmName <- param "prgmName"
      build <- liftAndCatchIO $ getEvalBuild includeCore baseFileName prgmName
      json $ Success build ([] :: [String])

    get "/web" $ do
      prgmName <- param "prgmName"
      build <- liftAndCatchIO $ getWeb includeCore baseFileName prgmName
      html (T.pack build)
