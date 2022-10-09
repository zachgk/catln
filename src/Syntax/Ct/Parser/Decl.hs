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

import           Control.Applicative        hiding (many, some)
import           Data.Maybe
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

pArrowRes :: Parser ParseMeta
pArrowRes = do
  _ <- symbol "->"
  pos1 <- getSourcePos
  t <- pSingleType
  pos2 <- getSourcePos
  return $ Meta t (Just (pos1, pos2, "")) emptyMetaDat

pDeclLHS :: Parser PDeclLHS
pDeclLHS = do
  pos1 <- getSourcePos
  patt <- pPattern FunctionObj
  maybeArrMeta <- optional pArrowRes
  pos2 <- getSourcePos
  let arrMeta = fromMaybe (emptyMeta pos1 pos2) maybeArrMeta
  return $ DeclLHS arrMeta patt

pComment :: Parser PCompAnnot
pComment = do
  pos1 <- getSourcePos
  c <- L.indentBlock scn p
  pos2 <- getSourcePos
  let m = emptyMeta pos1 pos2
  return $ RawTupleApply (emptyMetaM "appArg" m) (emptyMetaM "valC" m, RawValue (emptyMetaM "val" m) mdAnnot) [TupleArgIO (emptyMetaM "text" m) "text" (RawCExpr m (CStr c))]
  where
    takeLine = takeWhileP (Just "character") (/= '\n')
    p = do
      _ <- string "# "
      l <- takeLine
      return (L.IndentMany Nothing (\ls -> return $ intercalate "\n" (l:ls)) takeLine)

pDeclStatement :: Parser PStatement
pDeclStatement = do
  lhs <- pDeclLHS
  eqAndExpr <- optional $ do
    _ <- symbol "=>" <|> symbol "="
    optional $ try $ pExpr ParseOutputExpr
  case eqAndExpr of
    -- No equals (declaration or expression)
    Nothing -> case lhs of

      -- expression
      DeclLHS lhsM (Pattern lhsObj NoGuard) | getMetaType lhsM == TopType -> return $ RawExprStatement (eobjExpr lhsObj)

      -- Declaration
      DeclLHS lhsM _ | getMetaType lhsM /= TopType -> return $ RawDeclStatement $ RawDecl lhs Nothing

      -- Must be either a declaration or an expression
      _ -> fail $ printf "Invalid declaration or expression: %s" (show lhs)

    -- Equals but no expr (multi-line definition)
    Just Nothing -> return $ RawDeclStatement $ RawDecl lhs (Just $ rawVal nestedDeclaration)

    -- Equals and expr (inline definition)
    Just (Just expr) -> return $ RawDeclStatement $ RawDecl lhs (Just expr)
