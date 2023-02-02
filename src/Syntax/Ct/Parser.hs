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
import qualified Data.HashSet               as S
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           CRes
import           Control.Monad
import           Data.Graph
import           Data.List
import           Data.Maybe
import           Syntax.Ct.Parser.Decl
import           Syntax.Ct.Parser.Expr
import           Syntax.Ct.Parser.Lexer
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Parser.Type      (pTypeStatement)
import           Syntax.Ct.Prgm
import           System.Directory
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
  name <- ttypeidentifier
  return $ RawModule name (getPath name)

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

parseFile :: String -> String -> CRes PPrgm
parseFile fileName fileContents = case runParser (contents pPrgm) fileName fileContents of
  Left err   -> CErr [MkCNote $ ParseCErr err]
  Right prgm -> return prgm

parseRepl :: String -> PReplRes
parseRepl s = case runParser (contents p) "<repl>" s of
                Left e@(ParseErrorBundle _ _) -> ReplErr e
                Right (Left statement)        -> ReplStatement statement
                Right (Right expr)            -> ReplExpr expr
  where p = try (Left <$> pStatementTree) <|> try (Right <$> pExpr)

-- replaces imports of a directory with directory/main.ct
dirImportToMain :: String -> IO String
dirImportToMain f = do
  isFile <- doesFileExist f
  isDir <- doesDirectoryExist f
  return $ case (isFile, isDir) of
    (True, False) -> f
    (False, True) -> f ++ "/main.ct"
    _             -> error "bad dir"

readFiles :: Bool -> Bool -> [String] -> IO (CRes PPrgmGraphData)
readFiles includeCore includeDependencies = fmap (fmap (graphFromEdges . snd)) . aux [] S.empty
  where
    aux acc visited [] = return $ return (visited, acc)
    aux acc visited (nextToVisit:restToVisit) | S.member nextToVisit visited = aux acc visited restToVisit
    aux acc visited (nextToVisit:restToVisit) = do
      isFile <- doesFileExist nextToVisit
      isDir <- doesDirectoryExist nextToVisit
      case (isFile, isDir) of
        (True, False) -> do -- file
          f <- readFile nextToVisit
          case parseFile nextToVisit f of
            CErr notes -> return $ CErr notes
            CRes _ (parsedImports, statements) -> do
              let parsedImports' = if includeCore && not ("stack/core" `isPrefixOf` nextToVisit)
                    then "stack/core":parsedImports
                    else parsedImports
              parsedImports'' <- mapM dirImportToMain parsedImports'
              let prgm' = (parsedImports'', statements)
              let restToVisit' = if includeDependencies
                    then parsedImports' ++ restToVisit
                    else restToVisit
              aux ((prgm', nextToVisit, parsedImports'') : acc) (S.insert nextToVisit visited) restToVisit'
        (False, True) -> do -- directory
          files <- listDirectory nextToVisit
          files' <- forM files $ \file -> do
            let file' = nextToVisit ++ "/" ++ file
            isF <- doesFileExist file'
            isD <- doesDirectoryExist file'
            case (isF, isD) of
              (True, False) -> if not ("." `isPrefixOf` file) && ".ct" `isSuffixOf` file'
                then return (Just file')
                else return Nothing
              (False, True) -> return (Just file')
              _ -> error $ printf "Found non-file or directory: %s" file'
          aux acc visited (catMaybes files' ++ restToVisit)
        _ -> return $ CErr [MkCNote $ GenCErr Nothing $ printf "Could not find file or directory %s" nextToVisit]
