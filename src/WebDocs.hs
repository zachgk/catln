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
import TypeCheck (typecheckPrgm)
import           Eval (evalMain)
import Emit (initModule, codegen)
import Parser (readFiles)

data ResSuccess a n = Success a [n]
  deriving (Generic, ToJSON)

data ResFail n = ResFail [n]
  deriving (Generic, ToJSON)

maybeJson :: ToJSON a => CRes a -> ActionM ()
maybeJson (CRes notes r) = json $ Success r (map show notes)
maybeJson (CErr notes) = json $ ResFail (map show notes)

docServe :: Bool -> String -> IO ()
docServe includeStd fileName = do

  maybeRawPrgm <- readFiles (fileName : ["std/std.ct" | includeStd])

  let maybePrgm = maybeRawPrgm >>= desFiles

  let maybeTprgm = maybePrgm >>= typecheckPrgm

  let maybeEvalMainPre = maybeTprgm >>= evalMain
  evaluated <- case maybeEvalMainPre of
        CRes _ r -> r
        CErr _ -> return 999

  let maybeLlvmPre = maybeTprgm >>= return . codegen initModule
  llvm <- case maybeLlvmPre of
        CRes _ r -> r
        CErr _ -> return ""

  scotty 31204 $ do
    get "/files" $ do
      json ["File: ", T.pack fileName]

    get "/raw" $ do
      maybeJson maybeRawPrgm

    get "/desugar" $ do
      maybeJson maybePrgm

    get "/typecheck" $ do
      maybeJson maybeTprgm

    get "/eval" $ do
      json $ Success evaluated ([] :: [String])

    get "/llvm" $ do
      let llvmStr = BSU.toString llvm
      json $ Success llvmStr ([] :: [String])
