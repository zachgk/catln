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
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List
import Data.Maybe
import Data.Graph

pImport :: Parser String
pImport = do
  _ <- symbol "import"
  imp <- some printChar
  _ <- newline
  return imp

pComment :: Parser PStatement
pComment = RawComment <$> L.indentBlock scn p
  where
    takeLine = takeWhileP (Just "character") (/= '\n')
    p = do
      _ <- string "# "
      l <- takeLine
      return (L.IndentMany Nothing (\ls -> return $ intercalate "\n" (l:ls)) takeLine)

pGlobalAnnot :: Parser PStatement
pGlobalAnnot = do
  RawGlobalAnnot <$> pCompAnnot

pStatement :: Parser PStatement
pStatement = pTypeStatement
    <|> pComment
    <|> pGlobalAnnot
    <|> pRootDecl

pNothingNewline :: Parser (Maybe a)
pNothingNewline = do
  _ <- newline
  return Nothing

pPrgm :: Parser PPrgm
pPrgm = do
  _ <- many newline
  imports <- many pImport
  statements <- many (Just <$> try pStatement <|> pNothingNewline)
  return (imports, catMaybes statements)

contents :: Parser a -> Parser a
contents p = do
  r <- p
  eof
  return r

parseFile :: String -> String -> CRes PPrgm
parseFile fileName fileContents = case runParser (contents pPrgm) fileName fileContents of
  Left err -> CErr [MkCNote $ ParseCErr err]
  Right prgm -> return prgm

parseRepl :: String -> PReplRes
parseRepl s = case runParser (contents p) "<repl>" s of
                Left e@(ParseErrorBundle _ _) -> ReplErr e
                Right (Left statement)             -> ReplStatement statement
                Right (Right expr)            -> ReplExpr expr
  where p = try (Left <$> pStatement) <|> try (Right <$> pExpr)

readFiles :: Bool -> [String] -> IO (CRes PPrgmGraphData)
readFiles includeCore = fmap (fmap (graphFromEdges . snd)) . aux [] S.empty
  where
    aux acc visited [] = return $ return (visited, acc)
    aux acc visited (nextToVisit:restToVisit) | S.member nextToVisit visited = aux acc visited restToVisit
    aux acc visited (nextToVisit:restToVisit) = do
      f <- readFile nextToVisit
      case parseFile nextToVisit f of
        CErr notes -> return $ CErr notes
        CRes _ prgm@(parsedImports, statements) -> do
          let prgm'@(parsedImports', _) = if includeCore && not ("stack/core" `isPrefixOf` nextToVisit)
                then ("stack/core/main.ct":parsedImports, statements)
                else prgm
          aux ((prgm', nextToVisit, parsedImports') : acc) (S.insert nextToVisit visited) (parsedImports' ++ restToVisit)
