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

import           Data.List                  (intercalate)
import           Data.Maybe
import           Semantics.Prgm
import           Semantics.Types            (Constant (CStr), mkName,
                                             partialKey)
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
  (e, resultLines) <- L.indentBlock scn p
  let baseArg = (Just $ partialKey (mkName printAnnotText), e)
      resultArg = [(Just $ partialKey (mkName resultAnnotText), rawStr (intercalate "\n" resultLines)) | not (null resultLines)]
      annot = rawVal printAnnot `applyRawArgs` (baseArg : resultArg)
  return $ RawStatementTree (RawAnnot annot) []
  where
    takeLine = takeWhileP (Just "character") (/= '\n')
    pResultLine = do
      _ <- string ">>>"
      _ <- optional (char ' ')
      takeLine
    p = do
      _ <- string "> "
      e <- pExpr
      return (L.IndentMany Nothing (\rls -> return (e, rls)) pResultLine)


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

-- TODO Write a .ct file back with 'Syntax.Ct.Formatter.formatRootPrgm', which is already the
-- program-to-text direction for this syntax.
ctParser :: ImportParser
ctParser = readOnlyParser $ \imp -> case exprAppliedArgs imp of
  [ObjArr{oaArr=Just (Just (CExpr _ (CStr fileName)), _)}] -> do
    fileContents <- readFile fileName
    case runParser (contents pPrgm) fileName fileContents of
      Left err   -> fail $ show $ errorBundlePretty err
      Right prgm -> return $ ParsedImport prgm [] (Just fileContents) (Just [fileName])
  _ -> undefined

-- TODO Write a .ctx file back, once 'ctParser' can be written back: it is a .ct file with the one
-- annotation this adds taken off again.
ctxParser :: ImportParser
ctxParser = readOnlyParser $ \fileName -> do
  -- Updated rather than rebuilt, so the file this read is a read of travels with it without being
  -- named again here.
  parsed <- ipParse ctParser fileName
  return parsed{piPrgm = annotate (piPrgm parsed)}
  where
    annotate (RawPrgm imports statements) = RawPrgm imports (RawStatementTree (RawAnnot (RawValue emptyMetaN (mkName ctxAnnot))) [] :statements)

parseRepl :: String -> PReplRes
parseRepl s = case runParser (contents p) "<repl>" s of
                Left e@(ParseErrorBundle _ _) -> ReplErr e
                Right (Left statement)        -> ReplStatement statement
                Right (Right expr)            -> ReplExpr expr
  where p = try (Left <$> pStatementTree) <|> try (Right <$> pExpr)
