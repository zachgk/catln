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
import Syntax.Prgm (Expr)
import Syntax (Typed)
import Utils

data ResSuccess a n = Success a [n]
  | ResFail [n]
  deriving (Generic, ToJSON)

maybeJson :: (ToJSON a) => CRes a -> ActionM ()
maybeJson (CRes notes r) = json $ Success r notes
maybeJson (CErr notes) = json (ResFail notes :: ResSuccess () CNote)

getRawPrgm :: Bool -> String -> IO (CRes PPrgmGraphData )
getRawPrgm includeCore fileName = readFiles (fileName : ["stack/core/main.ct" | includeCore])

getPrgm :: Bool -> String -> IO (CRes DesPrgm)
getPrgm includeCore fileName = do
  base <- getRawPrgm includeCore fileName
  return (base >>= desFiles)

getTPrgmWithTrace :: Bool -> String -> IO (CRes (TPrgm, VPrgm, TraceConstrain))
getTPrgmWithTrace includeCore fileName = do
  base <- getPrgm includeCore fileName
  return (base >>= typecheckPrgmWithTrace)

getTPrgm :: Bool -> String -> IO (CRes TPrgm)
getTPrgm includeCore fileName = do
  base <- getPrgm includeCore fileName
  return (base >>= typecheckPrgm)

getTreebug :: Bool -> String -> IO EvalResult
getTreebug includeCore fileName = do
  base <- getTPrgm includeCore fileName
  let pre = base >>= evalMain
  case pre of
    CRes _ r -> snd <$> r
    CErr _ -> fail "No eval result found"

getEvaluated :: Bool -> String -> IO Integer
getEvaluated includeCore fileName = do
  base <- getTPrgm includeCore fileName
  let pre = base >>= evalMain
  case pre of
    CRes _ r -> fst <$> r
    CErr _ -> return 999

getEvalBuild :: Bool -> String -> IO Val
getEvalBuild includeCore fileName = do
  base <- getTPrgm includeCore fileName
  let pre = base >>= evalMainb
  case pre of
    CRes _ r -> fst <$> r
    CErr _ -> return NoVal

getEvalAnnots :: Bool -> String -> IO [(Expr Typed, Val)]
getEvalAnnots includeCore fileName = do
  base <- getTPrgm includeCore fileName
  let pre = base >>= evalAnnots
  case pre of
    CRes _ r -> return r
    CErr _ -> return []

getWeb :: Bool -> String -> IO String
getWeb includeCore fileName = do
  base <- getTPrgm includeCore fileName
  let pre = base >>= evalMainb
  case pre of
    CRes _ r -> do
      (TupleVal _ args, _) <- r
      case H.lookup "contents" args of
        Just (StrVal s) -> return s
        _ -> return "";
    CErr _ -> return ""


docServe :: Bool -> String -> IO ()
docServe includeCore fileName = do

  scotty 31204 $ do
    get "/files" $ do
      json $ Success ["File: ", T.pack fileName] ([] :: String)

    get "/raw" $ do
      maybeRawPrgms <- liftAndCatchIO $ getRawPrgm includeCore fileName
      let maybeRawPrgms' = graphToNodes <$> maybeRawPrgms
      maybeJson maybeRawPrgms'

    get "/pages" $ do
      maybeRawPrgms <- liftAndCatchIO $ getRawPrgm includeCore fileName
      let maybeRawPrgms' = graphToNodes <$> maybeRawPrgms
      annots <- liftAndCatchIO $ getEvalAnnots includeCore fileName
      maybeJson $ do
        rawPrgm <- maybeRawPrgms'
        return (rawPrgm, annots)

    get "/desugar" $ do
      maybePrgm <- liftAndCatchIO $ getPrgm includeCore fileName
      maybeJson maybePrgm

    get "/constrain" $ do
      maybeTprgmWithTrace <- liftAndCatchIO $ getTPrgmWithTrace includeCore fileName
      maybeJson maybeTprgmWithTrace

    get "/typecheck" $ do
      maybeTprgm <- liftAndCatchIO $ getTPrgm includeCore fileName
      maybeJson maybeTprgm

    get "/treebug" $ do
      treebug <- liftAndCatchIO $ getTreebug includeCore fileName
      json $ Success treebug ([] :: [String])

    get "/eval" $ do
      evaluated <- liftAndCatchIO $ getEvaluated includeCore fileName
      json $ Success evaluated ([] :: [String])

    get "/evalBuild" $ do
      build <- liftAndCatchIO $ getEvalBuild includeCore fileName
      json $ Success build ([] :: [String])

    get "/web" $ do
      build <- liftAndCatchIO $ getWeb includeCore fileName
      html (T.pack build)
