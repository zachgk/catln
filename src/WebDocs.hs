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

import qualified Data.Text.Lazy as T
import           GHC.Generics          (Generic)
import qualified Data.ByteString.UTF8            as BSU

import           Desugarf         (desFiles)
import           CRes
import TypeCheck (typecheckPrgmWithTrace)
import           Eval (evalMain)
import Emit (initModule, codegen)
import Parser (readFiles)
import Parser.Syntax (DesPrgm, PPrgm)
import TypeCheck.Common (TraceConstrain, VPrgm, TPrgm)

data ResSuccess a n = Success a [n]
  deriving (Generic, ToJSON)

data ResFail n = ResFail [n]
  deriving (Generic, ToJSON)

maybeJson :: ToJSON a => CRes a -> ActionM ()
maybeJson (CRes notes r) = json $ Success r (map show notes)
maybeJson (CErr notes) = json $ ResFail (map show notes)

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
  base <- getTPrgmWithTrace includeStd fileName
  return ((\(a, _, _) -> a) <$> base)

getEvaluated :: Bool -> String -> IO Integer
getEvaluated includeStd fileName = do
  base <- getTPrgm includeStd fileName
  let pre = base >>= evalMain
  case pre of
    CRes _ r -> r
    CErr _ -> return 999

getLlvm :: Bool -> String -> IO String
getLlvm includeStd fileName = do
  base <- getTPrgm includeStd fileName
  let pre = base >>= return . codegen initModule
  case pre of
    CRes _ r -> BSU.toString <$> r
    CErr _ -> return ""


docServe :: Bool -> String -> IO ()
docServe includeStd fileName = do

  scotty 31204 $ do
    get "/files" $ do
      json $ Success ["File: ", T.pack fileName] ([] :: String)

    get "/raw" $ do
      maybeRawPrgm <- liftAndCatchIO $ getRawPrgm includeStd fileName
      maybeJson maybeRawPrgm

    get "/desugar" $ do
      maybePrgm <- liftAndCatchIO $ getPrgm includeStd fileName
      maybeJson maybePrgm

    get "/constrain" $ do
      maybeTprgmWithTrace <- liftAndCatchIO $ getTPrgmWithTrace includeStd fileName
      maybeJson maybeTprgmWithTrace

    get "/typecheck" $ do
      maybeTprgm <- liftAndCatchIO $ getTPrgm includeStd fileName
      maybeJson maybeTprgm

    get "/eval" $ do
      evaluated <- liftAndCatchIO $ getEvaluated includeStd fileName
      json $ Success evaluated ([] :: [String])

    get "/llvm" $ do
      llvm <- liftAndCatchIO $ getLlvm includeStd fileName
      json $ Success llvm ([] :: [String])
