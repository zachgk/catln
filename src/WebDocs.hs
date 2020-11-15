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

import           Desugarf         (desFiles)
import           CRes
import TypeCheck (typecheckPrgm)

data ResSuccess a n = Success a [n]
  deriving (Generic, ToJSON)

data ResFail n = ResFail [n]
  deriving (Generic, ToJSON)

maybeJson :: ToJSON a => CRes a -> ActionM ()
maybeJson (CRes notes r) = json $ Success r (map show notes)
maybeJson (CErr notes) = json $ ResFail (map show notes)

docServe :: Bool -> String -> IO ()
docServe includeStd fileName = do
  maybePrgm <- desFiles $ (fileName : ["std/std.ct" | includeStd])

  let maybeTprgm = maybePrgm >>= typecheckPrgm

  scotty 31204 $ do
    get "/files" $ do
      json ["File: ", T.pack fileName]

    get "/desugar" $ do
      maybeJson maybePrgm

    get "/typecheck" $ do
      maybeJson maybeTprgm
