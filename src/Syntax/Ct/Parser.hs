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

import           Constants
import           CRes
import           Data.Maybe
import           Semantics.Prgm
import           Syntax.Ct.Parser.Decl
import           Syntax.Ct.Parser.Expr
import           Syntax.Ct.Parser.Lexer
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Parser.Type      (pTypeStatement)
import           Syntax.Ct.Prgm
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf

pImport :: Parser String
pImport = do
  _ <- symbol "import"
  imp <- some printChar
  _ <- newline
  return imp

liftPStatement :: Parser PStatement -> Parser PStatementTree
liftPStatement pSt = L.indentBlock scn p
  where
    pack st children = return $ RawStatementTree st children
    p = do
      st <- pSt
      return (L.IndentMany Nothing (pack st) pStatementTree)

pModule :: Parser PStatement
pModule = do
  _ <- symbol "module"
  RawModule <$> ttypeidentifier

pApply :: Parser PStatement
pApply = do
  _ <- symbol "apply"
  term1 <- RATermDeep <$> term
  termRest <- many $ do
    sep <- symbol ">" <|> symbol " "
    val <- term
    return $ case sep of
      ">" -> RATermChild val
      " " -> RATermDeep val
      _   -> error $ printf "Unexpected seperator " (show sep)
  return $ RawApplyStatement $ RawApply (term1 : termRest)

pCommentStatement :: Parser PStatementTree
pCommentStatement = do
  c <- pComment
  return $ RawStatementTree (RawAnnot c) []


pStatementTree :: Parser PStatementTree
pStatementTree = do
  notFollowedBy newline
  liftPStatement pTypeStatement
    <|> pCommentStatement
    <|> liftPStatement (RawAnnot <$> pCompAnnot)
    <|> liftPStatement pModule
    <|> liftPStatement pApply
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
  return (imports, catMaybes statements)

contents :: Parser a -> Parser a
contents p = do
  r <- p
  eof
  return r

ctParser :: String -> IO (CRes PPrgm)
ctParser fileName = do
  fileContents <- readFile fileName
  return $ case runParser (contents pPrgm) fileName fileContents of
    Left err   -> CErr [MkCNote $ ParseCErr err]
    Right prgm -> return prgm

ctxParser :: String -> IO (CRes PPrgm)
ctxParser fileName = do
  cp <- ctParser fileName
  return $ fmap annotate cp
  where
    annotate (imports, statements) = (imports, RawStatementTree (RawAnnot (RawValue emptyMetaN ctxAnnot)) [] :statements)

parseRepl :: String -> PReplRes
parseRepl s = case runParser (contents p) "<repl>" s of
                Left e@(ParseErrorBundle _ _) -> ReplErr e
                Right (Left statement)        -> ReplStatement statement
                Right (Right expr)            -> ReplExpr expr
  where p = try (Left <$> pStatementTree) <|> try (Right <$> pExpr)
