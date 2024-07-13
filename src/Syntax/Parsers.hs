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
import           Syntax.Ct.Builder
import           Syntax.Ct.Desugarf.Expr (SemiDesMode (SDOutput), semiDesExpr)
import           Syntax.Ct.Parser        (ctParser, ctxParser)
import           Syntax.Ct.Prgm
import           Syntax.Haskell.Parser   (hsParser)
import           Syntax.InferImport      (dirParser, inferRawImportStr)
import           System.FilePath         (isAbsolute, joinPath, takeDirectory)
import           Text.Printf
import           Utils

importParsers :: H.HashMap String ImportParser
importParsers = H.fromList [
  ("dir", dirParser),
  ("ct", ctParser),
  ("ctx", ctxParser),
  ("haskell", hsParser)
                             ]

-- Processes an import, producing the rawImpAbs and rawImpDisp
canonicalImport :: Maybe RawFileImport -> RawFileImport -> IO RawFileImport
canonicalImport caller imp@RawFileImport{rawImpAbs=(RawCExpr _ (CStr name))} = do
  inferred <- inferRawImportStr caller name
  canonicalImport caller imp{rawImpAbs=inferred}
canonicalImport caller imp = case maybeExprPath $ rawImpAbs imp of
  Nothing -> fail $ printf "Invalid import %s must be string or oject" (show imp)
  Just _ -> do
    impDir' <- case (calledDir', disp') of
      (_, Just b) | isAbsolute b -> return $ Just $ takeDirectory b
      (Just a, Just b)           -> return $ Just $ takeDirectory $ joinPath [a, b]
      _                          -> return Nothing
    return imp{rawImpDisp=disp', rawImpCalledDir=calledDir', rawImpDir=impDir'}
  where
    disp' = case exprAppliedArgs $ rawImpAbs imp of
      [ObjArr{oaArr=Just (Just (RawCExpr _ (CStr s)), _)}] -> Just s
      _                                                    -> Nothing
    calledDir' = rawImpDir =<< caller

mkRawCanonicalImportStr :: String -> IO RawFileImport
mkRawCanonicalImportStr = canonicalImport Nothing . mkRawFileImport . rawStr

mkDesCanonicalImportStr :: String -> IO FileImport
mkDesCanonicalImportStr = fmap (semiDesExpr SDOutput Nothing . rawImpAbs) . mkRawCanonicalImportStr

readImport :: RawFileImport -> ImportParseResult
readImport imp = case maybeExprPath (rawImpAbs imp) of
  Nothing -> fail $ printf "Invalid import %s must be string or oject" (show imp)
  Just impPath -> case H.lookup impPath importParsers of
    Nothing -> fail $ printf "No parser available for oject name in %s" (show imp)
    Just parser -> parser $ rawImpAbs imp

processParsed :: Bool -> RawFileImport -> (RawPrgm (), [RawFileImport]) -> IO (GraphNodes (RawPrgm ()) RawFileImport, [RawFileImport])
processParsed includeCore imp ((prgmImports, statements), extraImports) = do
  let prgmImports' = if includeCore && not ("core" `isInfixOf` name)
      then mkRawFileImport (rawStr "core") : prgmImports
      else prgmImports
  prgmImports'' <- mapM (canonicalImport (Just imp)) prgmImports'
  extraImports' <- mapM (canonicalImport (Just imp)) extraImports
  let totalImports = extraImports' ++ prgmImports''
  let prgm' = (prgmImports'', statements)
  return ((prgm', imp, prgmImports''), totalImports)
  where
    name = case rawImpAbs imp of
      RawCExpr _ (CStr n) -> n
      RawTupleApply _ _ [RawObjArr{roaArr=Just (Just (RawCExpr _ (CStr n)), _, _)}] -> n
      _ -> ""

parseFile :: Bool -> RawFileImport -> IO (CRes (GraphNodes (RawPrgm ()) RawFileImport))
parseFile includeCore imp = do
  imp' <- canonicalImport Nothing imp
  r <- readImport imp'
  p <- processParsed includeCore imp' r
  return $ pure $ fst p

readFiles :: Bool -> [RawFileImport] -> IO (CRes (GraphData (RawPrgm ()) RawFileImport))
readFiles includeCore initialImps = do
  initialImps' <- mapM (canonicalImport Nothing) initialImps
  pure . graphFromEdges <$> aux [] S.empty initialImps'
  where
    aux :: [GraphNodes (RawPrgm ()) RawFileImport] -> S.HashSet RawFileImport -> [RawFileImport] -> IO [GraphNodes (RawPrgm ()) RawFileImport]
    aux acc _ [] = return acc
    aux acc visited (nextToVisit:restToVisit) = do
      if S.member nextToVisit visited
        then aux acc visited restToVisit
        else do
          r <- readImport nextToVisit
          (newPrgm, newToVisit) <- processParsed includeCore nextToVisit r
          let restToVisit' = newToVisit ++ restToVisit
          aux (newPrgm : acc) (S.insert nextToVisit visited) restToVisit'
