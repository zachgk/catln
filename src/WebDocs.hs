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
import Parser.Syntax (DesPrgm, PPrgm)
import TypeCheck.Common (TraceConstrain, VPrgm, TPrgm)
import Eval.Common (Val(..), Val, EvalResult)
import Syntax.Prgm (Expr)
import Syntax (Typed)

data ResSuccess a n = Success a [n]
  | ResFail [n]
  deriving (Generic, ToJSON)

maybeJson :: (ToJSON a) => CRes a -> ActionM ()
maybeJson (CRes notes r) = json $ Success r (map show notes)
maybeJson (CErr notes) = json (ResFail (map show notes) :: ResSuccess () [Char])

getRawPrgm :: Bool -> String -> IO (CRes [(String, PPrgm)])
getRawPrgm includeStd fileName = readFiles (fileName : ["std/std.ct" | includeStd])

getPrgm :: Bool -> String -> IO (CRes DesPrgm)
getPrgm includeStd fileName = do
  base <- getRawPrgm includeStd fileName
  return (base >>= desFiles)

getTPrgmWithTrace :: Bool -> String -> IO (CRes (TPrgm, VPrgm, TraceConstrain))
getTPrgmWithTrace includeStd fileName = do
  base <- getPrgm includeStd fileName
  return (base >>= typecheckPrgmWithTrace)

getTPrgm :: Bool -> String -> IO (CRes TPrgm)
getTPrgm includeStd fileName = do
  base <- getPrgm includeStd fileName
  return (base >>= typecheckPrgm)

getTreebug :: Bool -> String -> IO EvalResult
getTreebug includeStd fileName = do
  base <- getTPrgm includeStd fileName
  let pre = base >>= evalMain
  case pre of
    CRes _ r -> snd <$> r
    CErr _ -> fail "No eval result found"

getEvaluated :: Bool -> String -> IO Integer
getEvaluated includeStd fileName = do
  base <- getTPrgm includeStd fileName
  let pre = base >>= evalMain
  case pre of
    CRes _ r -> fst <$> r
    CErr _ -> return 999

getEvalBuild :: Bool -> String -> IO Val
getEvalBuild includeStd fileName = do
  base <- getTPrgm includeStd fileName
  let pre = base >>= evalMainb
  case pre of
    CRes _ r -> fst <$> r
    CErr _ -> return NoVal

getEvalAnnots :: Bool -> String -> IO [(Expr Typed, Val)]
getEvalAnnots includeStd fileName = do
  base <- getTPrgm includeStd fileName
  let pre = base >>= evalAnnots
  case pre of
    CRes _ r -> return r
    CErr _ -> return []

getWeb :: Bool -> String -> IO String
getWeb includeStd fileName = do
  base <- getTPrgm includeStd fileName
  let pre = base >>= evalMainb
  case pre of
    CRes _ r -> do
      (TupleVal _ args, _) <- r
      case H.lookup "contents" args of
        Just (StrVal s) -> return s
        _ -> return "";
    CErr _ -> return ""


docServe :: Bool -> String -> IO ()
docServe includeStd fileName = do

  scotty 31204 $ do
    get "/files" $ do
      json $ Success ["File: ", T.pack fileName] ([] :: String)

    get "/raw" $ do
      maybeRawPrgm <- liftAndCatchIO $ getRawPrgm includeStd fileName
      maybeJson maybeRawPrgm

    get "/pages" $ do
      maybeRawPrgm <- liftAndCatchIO $ getRawPrgm includeStd fileName
      annots <- liftAndCatchIO $ getEvalAnnots includeStd fileName
      maybeJson $ do
        rawPrgm <- maybeRawPrgm
        return (rawPrgm, annots)

    get "/desugar" $ do
      maybePrgm <- liftAndCatchIO $ getPrgm includeStd fileName
      maybeJson maybePrgm

    get "/constrain" $ do
      maybeTprgmWithTrace <- liftAndCatchIO $ getTPrgmWithTrace includeStd fileName
      maybeJson maybeTprgmWithTrace

    get "/typecheck" $ do
      maybeTprgm <- liftAndCatchIO $ getTPrgm includeStd fileName
      maybeJson maybeTprgm

    get "/treebug" $ do
      treebug <- liftAndCatchIO $ getTreebug includeStd fileName
      json $ Success treebug ([] :: [String])

    get "/eval" $ do
      evaluated <- liftAndCatchIO $ getEvaluated includeStd fileName
      json $ Success evaluated ([] :: [String])

    get "/evalBuild" $ do
      build <- liftAndCatchIO $ getEvalBuild includeStd fileName
      json $ Success build ([] :: [String])

    get "/web" $ do
      build <- liftAndCatchIO $ getWeb includeStd fileName
      html (T.pack build)
