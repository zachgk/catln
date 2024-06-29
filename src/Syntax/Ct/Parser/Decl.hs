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
import           Syntax.Ct.Parser.Expr
import           Syntax.Ct.Parser.Lexer
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Text.Megaparsec.Char
import           Text.Printf

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
  return (RawValue (emptyMetaM "val" m) mdAnnot `applyRawArgs` [(Just $ partialKey "text", RawCExpr m (CStr c))])
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
    Right roa@RawObjArr{roaObj=(Just inExpr), roaArr=out} -> case out of -- No equals but meta so declaration
      (Just (Nothing, _, _)) -> return $ RawDeclStatement roa

      -- Equals and expr (inline definition)
      (Just (Just{}, _, _))  -> return $ RawDeclStatement roa

      -- Must be either a declaration or an expression
      Nothing                -> return $ RawExprStatement inExpr
    Right roa -> fail $ printf "Unexpected decl statement found during parsing: %s" (show roa)
