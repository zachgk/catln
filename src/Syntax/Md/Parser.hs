--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Md.Parser
-- Copyright :  (c) Zach Kimberg 2024
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is for parsing markdown.
--------------------------------------------------------------------

module Syntax.Md.Parser where

import           CtConstants
import           Semantics.Prgm
import           Semantics.Types   (partialKey)
import           Syntax.Ct.Builder
import           Syntax.Ct.Prgm
import           System.Directory

mdParser :: ImportParser
mdParser imp = case exprAppliedArgs imp of
  (ObjArr{oaArr=Just (Just (CExpr _ (CStr filename)), _)}:_impArgs) -> do
    isFile <- doesFileExist filename
    if isFile
      then do
        str <- readFile filename
        return (RawPrgm [] [RawStatementTree (RawAnnot (rawVal mdAnnot `applyRawArgs` [(Just $ partialKey mdAnnotText, RawCExpr emptyMetaN (CStr str))])) []], [])
      else return (RawPrgm [] [], [])
  _ -> undefined


