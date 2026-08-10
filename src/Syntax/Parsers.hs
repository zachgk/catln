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

import           Control.Applicative     ((<|>))
import           Control.Monad           (join)
import           Control.Monad.Identity  (Identity (..))
import           Control.Monad.Trans
import           CRes
import           CtConstants             (noCoreAnnot)
import           Data.Graph
import qualified Data.HashMap.Strict     as H
import           Data.List
import           Data.Maybe              (listToMaybe, mapMaybe)
import           MapMeta                 (MetaLocation (ImportMeta), mapMeta,
                                          parseMetaIDScope)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Builder
import           Syntax.Ct.Desugarf.Expr (desFileImport)
import           Syntax.Ct.MapRawMeta    (addRawPrgmMetaIDs)
import           Syntax.Ct.Parser        (ctParser, ctxParser)
import           Syntax.Ct.Prgm
import           Syntax.Haskell.Parser   (hsParser)
import           Syntax.InferImport      (dirParser, findImportPath,
                                          inferRawImportStr)
import           Syntax.Md.Parser        (mdParser)
import           Syntax.Obsidian.Parser  (obsidianParser)
import           Syntax.Python.Parser    (pyParser)
import           System.FilePath         (isAbsolute, joinPath, takeDirectory)
import           Text.Printf
import           Utils

importParsers :: H.HashMap String ImportParser
importParsers = H.fromList [
  ("dir", dirParser),
  ("ct", ctParser),
  ("ctx", ctxParser),
  ("md", mdParser),
  ("obsidian", obsidianParser),
  ("haskell", hsParser),
  ("python", pyParser)
                             ]

-- | The path an applied import names: its first positional argument.
--
-- Both argument shapes are read because an import is canonicalized before any desugar runs. A
-- written @strategy("path")@ leaves its lone argument in 'roaObj' — 'Syntax.Ct.Parser.Expr.pArrowFull'
-- makes a single expression an input, for the desugar to move to its proper place — while
-- 'Syntax.Ct.Builder.applyRawArgs', which every inferred import is built with, puts it in 'roaArr'.
-- A named argument keeps a 'RawValue' in 'roaObj', so it is not mistaken for the path.
rawImportPath :: RawExpr () -> Maybe String
rawImportPath e = listToMaybe $ mapMaybe argStr $ rawExprAppliedArgs e
  where
    argStr RawObjArr{roaObj=Nothing, roaArr=Just (Just (RawCExpr _ (CStr s)), _)} = Just s
    argStr RawObjArr{roaObj=Just (RawCExpr _ (CStr s)), roaArr=Nothing}           = Just s
    argStr _                                                                      = Nothing

-- | Rewrites the path an applied import names, leaving the rest of it alone.
--
-- The other arguments are kept because a strategy reads them: @haskell@ takes the Stack root to
-- resolve its module against, and whether the module was imported qualified. Matches the same two
-- argument shapes 'rawImportPath' reads.
setRawImportPath :: String -> RawExpr () -> RawExpr ()
setRawImportPath name (RawTupleApply m be args) = RawTupleApply m be (aux args)
  where
    aux [] = []
    aux ((spread, arg):rest) = case arg of
      RawObjArr{roaObj=Nothing, roaArr=Just (Just (RawCExpr am (CStr _)), at)} ->
        (spread, arg{roaArr=Just (Just (RawCExpr am (CStr name)), at)}) : rest
      RawObjArr{roaObj=Just (RawCExpr am (CStr _)), roaArr=Nothing} ->
        (spread, arg{roaObj=Just (RawCExpr am (CStr name))}) : rest
      _ -> (spread, arg) : aux rest
setRawImportPath _ e = e

-- Processes an import, producing the rawImpAbs and rawImpDisp
canonicalImport :: CTSSConfig -> Maybe FileImport -> RawFileImport -> IO FileImport
canonicalImport config caller imp@AFileImport{impAbs=(RawCExpr _ (CStr name))} = do
  inferred <- inferRawImportStr config caller name
  canonicalImport config caller imp{impAbs=inferred}
-- The metas are cleared because 'impAbs' is the identity of a page: 'AFileImport' compares by it and
-- 'MapMeta.parseMetaIDScope' names a file's meta-id scope by it. A written @import strategy("path")@
-- is the one import form which reaches here carrying the positions and types its parse gave it, and
-- keeping those would make one file imported from two places into two pages holding the same meta
-- ids. Every other import is built by 'Syntax.InferImport.inferRawImportStr' with empty metas
-- already, so clearing them changes nothing.
--
-- The path is made absolute for the same reason, so that a page is the same page however it was
-- named. An import which names nothing on disk is left as written rather than rejected: @haskell@
-- takes a module name and resolves it itself, against the Stack project rather than the filesystem.
--
-- 'impRaw' is deliberately left alone. It is what 'Syntax.Ct.Formatter.formatImport' prints, so
-- rewriting it would have the formatter replace what the file says with an absolute path.
canonicalImport config caller imp = case maybeExprPath impAbs' of
  Nothing -> fail $ printf "Invalid import %s must be string or oject" (show imp)
  Just _ -> do
    resolved <- join <$> mapM (findImportPath config caller) named'
    let disp' = resolved <|> named'
    impDir' <- case (calledDir', disp') of
      (_, Just b) | isAbsolute b -> return $ Just $ takeDirectory b
      (Just a, Just b)           -> return $ Just $ takeDirectory $ joinPath [a, b]
      _                          -> return Nothing
    let impAbs'' = maybe impAbs' (`setRawImportPath` impAbs') resolved
    return $ desFileImport $ imp{impAbs=impAbs'', impDisp=disp', impCalledDir=calledDir', impDir=impDir'}
  where
    impAbs' = mapMeta (\_ _ -> Identity emptyMetaN) ImportMeta $ impAbs imp
    named' = rawImportPath impAbs'
    calledDir' = impDir =<< caller

mkRawImportStr :: String -> RawFileImport
mkRawImportStr = mkRawFileImport . rawStr

mkDesCanonicalImportStr :: CTSSConfig -> String -> IO FileImport
mkDesCanonicalImportStr config = canonicalImport config Nothing . mkRawImportStr

readImport :: FileImport -> ImportParseResult
readImport imp = case maybeExprPath (impAbs imp) of
  Nothing -> fail $ printf "Invalid import %s must be string or oject" (show imp)
  Just impPath -> case H.lookup (nameStr impPath) importParsers of
    Nothing -> fail $ printf "No parser available for oject name in %s" (show imp)
    -- TODO Modify the ImportParseResult to CResT to report back parse errors
    Just parser -> parser $ impAbs imp

processParsed :: CTSSConfig -> FileImport -> (RawPrgm (), [RawFileImport], Maybe String) -> IO (GraphNodes (RawPrgm (), Maybe String) FileImport, [FileImport])
processParsed config imp (prgm@(RawPrgm prgmImports statements), extraImports, src) = do
  let includeCore = not (rawPrgmHasAnnot prgm noCoreAnnot)
  let prgmImports' = if includeCore && not ("core" `isInfixOf` name)
      then mkRawFileImport (rawStr "core") : prgmImports
      else prgmImports
  prgmImports'' <- mapM (canonicalImport config (Just imp)) prgmImports'
  extraImports' <- mapM (canonicalImport config (Just imp)) extraImports
  let totalImports = extraImports' ++ prgmImports''
  let prgm' = RawPrgm prgmImports' statements
  let prgm'' = addRawPrgmMetaIDs (parseMetaIDScope imp) prgm'
  return (((prgm'', src), imp, prgmImports''), totalImports)
  where
    name = case impAbs imp of
      CExpr _ (CStr n) -> n
      TupleApply _ _ (EAppArg ObjArr{oaArr=Just (Just (CExpr _ (CStr n)), _)}) -> n
      _ -> ""

parseFile :: CTSSConfig -> RawFileImport -> CResT IO (GraphNodes (RawPrgm ()) FileImport)
parseFile config imp = do
  imp' <- lift $ canonicalImport config Nothing imp
  r <- lift $ readImport imp'
  ((rawPrgm, _src), fi, deps) <- lift $ fst <$> processParsed config imp' r
  return (rawPrgm, fi, deps)

-- TODO Change readImport, then make this return GraphData of CRes
readFiles :: CTSSConfig -> [RawFileImport] -> IO (GraphData (RawPrgm (), Maybe String) FileImport)
readFiles config initialImps = do
  initialImps' <- mapM (canonicalImport config Nothing) initialImps
  res <- aux [] S.empty initialImps'
  return $ graphFromEdges res
  where
    aux :: [GraphNodes (RawPrgm (), Maybe String) FileImport] -> S.HashSet FileImport -> [FileImport] -> IO [GraphNodes (RawPrgm (), Maybe String) FileImport]
    aux acc _ [] = return acc
    aux acc visited (nextToVisit:restToVisit) = do
      if S.member nextToVisit visited
        then aux acc visited restToVisit
        else do
          r <- readImport nextToVisit
          (newPrgm, newToVisit) <- processParsed config nextToVisit r
          let restToVisit' = newToVisit ++ restToVisit
          aux (newPrgm : acc) (S.insert nextToVisit visited) restToVisit'
