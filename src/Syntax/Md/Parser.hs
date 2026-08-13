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
import           Semantics.Types   (Constant (CStr), mkName, partialKey)
import           Syntax.Ct.Builder
import           Syntax.Ct.Prgm
import           System.Directory

-- TODO Write a markdown file back as the text of the one annotation it is read as.
mdParser :: ImportParser
mdParser = readOnlyParser $ \_ imp -> case exprAppliedArgs imp of
  (ObjArr{oaArr=Just (Just (CExpr _ (CStr filename)), _)}:_impArgs) -> do
    isFile <- doesFileExist filename
    -- The file this read is a read of, whether it was there or not: a page read from a file which
    -- does not exist is a page the file appearing has something to say to.
    let reads' = Just [ReadFile filename]
    if isFile
      then do
        str <- readFile filename
        return $ ParsedImport (PageRead (RawPrgm [] [RawStatementTree (RawAnnot (rawVal mdAnnot `applyRawArgs` [(Just $ partialKey (mkName mdAnnotText), RawCExpr emptyMetaN (CStr str))])) []])) [] (Just str) reads'
      else return $ ParsedImport (PageRead (RawPrgm [] [])) [] Nothing reads'
  _ -> undefined


