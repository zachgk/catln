--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Pandoc.Syntax
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines conversions to and from pandoc syntax
--------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Syntax.Pandoc.Syntax where

import           CRes
import qualified Data.ByteString.Lazy    as BS
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as L
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Syntax.Ct.Formatter
import           Syntax.Ct.Prgm
import           Text.Pandoc             (runIOorExplode)
import           Text.Pandoc.Class       (PandocIO)
import           Text.Pandoc.Definition
import           Text.Pandoc.Options     (def)
import           Text.Pandoc.Writers
import           Text.Printf

toPandocStatement :: Int -> RawStatement RawExpr () -> Block
toPandocStatement indent statement = CodeBlock catlnAttrs (T.pack $ formatStatement indent statement)
  where
    catlnAttrs = ("catln", [], [])

toPandocStatements :: Int -> RawStatementTree RawExpr () -> [Block]
toPandocStatements indent (RawStatementTree statement subTree) = toPandocStatement indent statement : concatMap (toPandocStatements indent) subTree

toPandoc :: RawPrgm () -> Pandoc
toPandoc (_, statements) = Pandoc nullMeta (concatMap (toPandocStatements 0) statements)

toDocument :: String -> RawPrgm () -> IO BS.ByteString
toDocument format prgm = do
  writer <- case lookup (T.pack format) writers of
        Just w  -> return w
        Nothing -> fail $ printf "Unexpected pandoc format %s" format
  let prgm' = toPandoc prgm
  runIOorExplode $ do
    case writer of
      TextWriter f -> do
        tx <- f def prgm'
        return $ encodeUtf8 $ L.fromStrict tx
      ByteStringWriter f -> f def prgm'

documentFormats :: [String]
documentFormats = map (T.unpack . fst) (writers :: [(T.Text, Writer PandocIO)])

pandocParser :: String -> IO (CRes (RawPrgm ()))
pandocParser = undefined
-- See https://github.com/jgm/pandoc/blob/e5fed51529120067672b840ba80084281761649e/src/Text/Pandoc/App/FormatHeuristics.hs#L36
