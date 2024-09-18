--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Parser
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is the main module for parsing. It will read in files
-- from their file paths and then parse into a 'RawPrgm'.
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Syntax.Ct.Parser where

import           Control.Applicative        hiding (many, some)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           CtConstants
import           Data.Bifunctor             (Bifunctor (first))
import           Data.Maybe
import           Semantics.Prgm
import           Semantics.Types            (partialKey)
import           Syntax.Ct.Builder
import           Syntax.Ct.Parser.Decl
import           Syntax.Ct.Parser.Expr
import           Syntax.Ct.Parser.Lexer
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import qualified Text.Megaparsec.Char.Lexer as L

pImport :: Parser RawFileImport
pImport = do
  _ <- symbol "import"
  imp <- term
  _ <- newline
  return $ mkRawFileImport imp

liftPStatement :: Parser PStatement -> Parser PStatementTree
liftPStatement pSt = L.indentBlock scn p
  where
    pack st children = return $ RawStatementTree st children
    p = do
      st <- pSt
      return (L.IndentMany Nothing (pack st) pStatementTree)

pCommentStatement :: Parser PStatementTree
pCommentStatement = do
  c <- pComment
  return $ RawStatementTree (RawAnnot c) []

pPrintStatement :: Parser PStatementTree
pPrintStatement = do
  _ <- string "> "
  e <- pExpr
  let annot = rawVal printAnnot `applyRawArgs` [(Just $ partialKey printAnnotText, e)]
  return $ RawStatementTree (RawAnnot annot) []


pStatementTree :: Parser PStatementTree
pStatementTree = do
  notFollowedBy newline
  pCommentStatement
    <|> pPrintStatement
    <|> liftPStatement (RawAnnot <$> pCompAnnot)
    <|> liftPStatement pDeclStatement

pNothingNewline :: Parser (Maybe a)
pNothingNewline = do
  _ <- newline
  return Nothing

pPrgm :: Parser PPrgm
pPrgm = do
  _ <- many newline
  imports <- many pImport
  statements <- many (Just <$> pStatementTree <|> pNothingNewline)
  return $ RawPrgm imports (catMaybes statements)

contents :: Parser a -> Parser a
contents p = do
  r <- p
  eof
  return r

ctParser :: ImportParser
ctParser imp = case exprAppliedArgs imp of
  [ObjArr{oaArr=Just (Just (CExpr _ (CStr fileName)), _)}] -> do
    fileContents <- readFile fileName
    case runParser (contents pPrgm) fileName fileContents of
      Left err   -> fail $ show $ errorBundlePretty err
      Right prgm -> return (prgm, [])
  _ -> undefined

ctxParser :: ImportParser
ctxParser fileName = do
  cp <- ctParser fileName
  return $ first annotate cp
  where
    annotate (RawPrgm imports statements) = RawPrgm imports (RawStatementTree (RawAnnot (RawValue emptyMetaN ctxAnnot)) [] :statements)

parseRepl :: String -> PReplRes
parseRepl s = case runParser (contents p) "<repl>" s of
                Left e@(ParseErrorBundle _ _) -> ReplErr e
                Right (Left statement)        -> ReplStatement statement
                Right (Right expr)            -> ReplExpr expr
  where p = try (Left <$> pStatementTree) <|> try (Right <$> pExpr)
