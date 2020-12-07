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
import qualified Data.HashSet        as S

import           Syntax
import           CRes
import           Parser.Lexer
import Parser.Syntax
import Parser.Expr (pExpr)
import Parser.Decl
import Parser.Type (pTypeStatement)
import Syntax.Prgm

pImport :: Parser String
pImport = do
  _ <- symbol "import"
  some printChar

pGlobalAnnot :: Parser PStatement
pGlobalAnnot = do
  _ <- string "// "
  RawComment <$> takeWhileP (Just "character") (/= '\n')

pComment :: Parser PStatement
pComment = do
  RawGlobalAnnot <$> pCompAnnot

pStatement :: Parser PStatement
pStatement = pTypeStatement
             <|> pComment
             <|> pGlobalAnnot
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
  Left err -> CErr [MkCNote $ ParseCErr err]
  Right prgm -> return prgm

parseRepl :: String -> PReplRes
parseRepl s = case runParser (contents p) "<stdin>" s of
                Left e@(ParseErrorBundle _ _) -> ReplErr e
                Right (Left statement)             -> ReplStatement statement
                Right (Right expr)            -> ReplExpr expr
  where p = try (Left <$> pStatement) <|> try (Right <$> pExpr)

readFiles :: [String] -> IO (CRes [(String, PPrgm)])
readFiles = aux [] S.empty
  where
    aux acc _ [] = return $ return acc
    aux acc visited (nextToVisit:restToVisit) | S.member nextToVisit visited = aux acc visited restToVisit
    aux acc visited (nextToVisit:restToVisit) = do
      f <- readFile nextToVisit
      case parseFile f of
        CErr notes -> return $ CErr notes
        CRes _ prgm@(parsedImports, _) -> aux ((nextToVisit, prgm):acc) (S.insert nextToVisit visited) (parsedImports ++ restToVisit)
