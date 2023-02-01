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

import           Constants
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
  pExpr ParseInputExpr

pComment :: Parser PCompAnnot
pComment = do
  pos1 <- getSourcePos
  c <- L.indentBlock scn p
  pos2 <- getSourcePos
  let m = emptyMeta pos1 pos2
  return (RawValue (emptyMetaM "val" m) mdAnnot `applyRawArgs` [(Just "text", RawCExpr m (CStr c))])
  where
    takeLine = takeWhileP (Just "character") (/= '\n')
    p = do
      _ <- string "# "
      l <- takeLine
      return (L.IndentMany Nothing (\ls -> return $ intercalate "\n" (l:ls)) takeLine)

pDeclStatement :: Parser PStatement
pDeclStatement = do
  roa@RawObjArr{roaObj=(Just (RawGuardExpr inExpr guard)), roaM=arrMeta, roaArr=out} <- pArrowFull Nothing FunctionObj

  case out of
    -- No equals (declaration or expression)
    Nothing -> case guard of

      -- expression
      NoGuard | getMetaType arrMeta == TopType -> return $ RawExprStatement inExpr

      -- Declaration
      _ | getMetaType arrMeta /= TopType -> return $ RawDeclStatement $ RawDecl roa

      -- Must be either a declaration or an expression
      _ -> fail $ printf "Invalid declaration or expression: %s" (show roa)

    -- Equals and expr (inline definition)
    Just{} -> return $ RawDeclStatement $ RawDecl roa
