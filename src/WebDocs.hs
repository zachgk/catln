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

module WebDocs where

import Web.Scotty

import qualified Data.Text.Lazy as T

docServe :: String -> IO ()
docServe fileName = scotty 3000 $
    get "/files" $ do
        json ["File: ", T.pack fileName]
