--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Parsers
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is the main module for arbitary syntax parsing.
-- It will read in files from their file paths and then parse into a 'RawPrgm'.
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parsers where

import qualified Data.HashSet            as S

import           CRes
import           Data.Graph
import qualified Data.HashMap.Strict     as H
import           Data.List
import           Semantics.Prgm
import           Syntax.Ct.Parser        (ctParser, ctxParser)
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Syntax.Haskell.Parser   (hsParser)
import           Syntax.InferImport      (dirParser, inferRawImportStr)
import           Text.Printf
import           Utils

importParsers :: H.HashMap String ImportParser
importParsers = H.fromList [
  ("dir", dirParser),
  ("ct", ctParser),
  ("ctx", ctxParser),
  ("haskell", hsParser)
                             ]

-- Converts import into a standard form of an object with absolute paths
canonicalImport :: RawFileImport -> IO RawFileImport
canonicalImport (RawCExpr _ (CStr name)) = inferRawImportStr name
canonicalImport imp = case maybeExprPath imp of
  Nothing -> fail $ printf "Invalid import %s must be string or oject" (show imp)
  Just _ -> return imp

readImport :: RawFileImport -> ImportParseResult
readImport imp = case maybeExprPath imp of
  Nothing -> fail $ printf "Invalid import %s must be string or oject" (show imp)
  Just impPath -> case H.lookup impPath importParsers of
    Nothing -> fail $ printf "No parser available for oject name in %s" (show imp)
    Just parser -> parser imp

processParsed :: Bool -> RawFileImport -> (RawPrgm (), [RawFileImport]) -> IO (GraphNodes (RawPrgm ()) RawFileImport, [RawFileImport])
processParsed includeCore imp ((prgmImports, statements), extraImports) = do
  let prgmImports' = if includeCore && not ("stack/core" `isInfixOf` name)
      then rawStr "stack/core" : prgmImports
      else prgmImports
  prgmImports'' <- mapM canonicalImport prgmImports'
  extraImports' <- mapM canonicalImport extraImports
  let totalImports = extraImports' ++ prgmImports''
  let prgm' = (prgmImports'', statements)
  return ((prgm', imp, prgmImports''), totalImports)
  where
    name = case imp of
      RawCExpr _ (CStr n) -> n
      RawTupleApply _ _ [RawObjArr{roaArr=Just (Just (GuardExpr (RawCExpr _ (CStr n)) _), _)}] -> n
      _ -> ""

parseFile :: Bool -> RawFileImport -> IO (CRes (GraphNodes (RawPrgm ()) RawFileImport))
parseFile includeCore imp = do
  r <- readImport imp
  p <- processParsed includeCore imp r
  return $ pure $ fst p

readFiles :: Bool -> [RawFileImport] -> IO (CRes (GraphData (RawPrgm ()) RawFileImport))
readFiles includeCore = fmap (pure . graphFromEdges) . aux [] S.empty
  where
    aux :: [GraphNodes (RawPrgm ()) RawFileImport] -> S.HashSet RawFileImport -> [RawFileImport] -> IO [GraphNodes (RawPrgm ()) RawFileImport]
    aux acc _ [] = return acc
    aux acc visited (nextToVisit:restToVisit) | S.member nextToVisit visited = aux acc visited restToVisit
    aux acc visited (nextToVisit:restToVisit) = do
      r <- readImport nextToVisit
      (newPrgm, newToVisit) <- processParsed includeCore nextToVisit r
      let restToVisit' = newToVisit ++ restToVisit
      aux (newPrgm : acc) (S.insert nextToVisit visited) restToVisit'
