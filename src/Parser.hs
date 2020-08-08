--------------------------------------------------------------------
-- |
-- Module    :  Parser
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Applicative            hiding (many, some)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Syntax
import           Parser.Lexer
import Parser.Syntax
import Parser.Expr (pExpr)
import Parser.Decl
import Parser.Type (pTypeStatement)

pImport :: Parser String
pImport = do
  _ <- symbol "import"
  some printChar

pStatement :: Parser PStatement
pStatement = pTypeStatement
             <|> pRootDecl

pPrgm :: Parser PPrgm
pPrgm = do
  _ <- many newline
  imports <- many pImport
  _ <- many newline
  statements <- sepBy1 pStatement (some newline)
  _ <- many newline
  return (imports, statements)

contents :: Parser a -> Parser a
contents p = do
  r <- p
  eof
  return r

parseFile :: String -> CRes PPrgm
parseFile f = case runParser (contents pPrgm) "<stdin>" f of
  Left err -> CErr [ParseCErr err]
  Right prgm -> return prgm

parseRepl :: String -> PReplRes
parseRepl s = case runParser (contents p) "<stdin>" s of
                Left e@(ParseErrorBundle _ _) -> ReplErr e
                Right (Left statement)             -> ReplStatement statement
                Right (Right expr)            -> ReplExpr expr
  where p = try (Left <$> pStatement) <|> try (Right <$> pExpr)
