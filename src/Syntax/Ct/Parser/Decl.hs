--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Parser.Decl
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is used to define function definitions and
-- declarations.
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Syntax.Ct.Parser.Decl where

import           Text.Megaparsec            hiding (pos1)
import qualified Text.Megaparsec.Char.Lexer as L

import           CtConstants
import           Data.List
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Builder
import           Syntax.Ct.Parser.Expr
import           Syntax.Ct.Parser.Lexer
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Text.Megaparsec.Char

pCompAnnot :: Parser PCompAnnot
pCompAnnot = do
  _ <- lookAhead $ string "#"
  pExpr

pComment :: Parser PCompAnnot
pComment = do
  pos1 <- getSourcePos
  c <- L.indentBlock scn p
  pos2 <- getSourcePos
  let m = emptyMeta pos1 pos2
  return (RawValue (emptyMetaM m) mdAnnot `applyRawArgs` [(Just $ partialKey mdAnnotText, RawCExpr m (CStr c))])
  where
    takeLine = takeWhileP (Just "character") (/= '\n')
    p = do
      _ <- string "# "
      l <- takeLine
      return (L.IndentMany Nothing (\ls -> return $ intercalate "\n" (l:ls)) takeLine)

pDeclStatement :: Parser PStatement
pDeclStatement = do
  af <- pArrowFull FunctionObj
  case af of
    Left stmt -> return stmt
    Right roa -> return $ RawDeclStatement roa
