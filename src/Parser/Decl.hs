--------------------------------------------------------------------
-- |
-- Module    :  Parser.Decl
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Parser.Decl where

import           Control.Applicative            hiding (many, some)
import qualified Data.HashMap.Strict as H
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Syntax.Prgm
import           Syntax
import           Parser.Lexer
import Parser.Syntax
import Parser.Expr

pDeclArg :: Parser (String, PExpr)
pDeclArg = do
  argName <- identifier
  _ <- symbol "="
  expr <- pExpr
  return (argName, expr)

pCompAnnot :: Parser PCompAnnot
pCompAnnot = do
  _ <- string "#"
  annotName <- (:) <$> letterChar <*> many alphaNumChar
  argVals <- parens $ sepBy1 pDeclArg (symbol ",")
  return $ CompAnnot annotName (H.fromList argVals)

pArrowRes :: Parser ParseMeta
pArrowRes = do
  _ <- symbol "->"
  PreTyped <$> pSingleType

pDeclLHS :: Parser PDeclLHS
pDeclLHS = do
  patt <- pPattern FunctionObj
  maybeArrMeta <- optional pArrowRes
  let arrMeta = fromMaybe emptyMeta maybeArrMeta
  return $ DeclLHS arrMeta patt

pDeclSingle :: Parser PDecl
pDeclSingle = do
  lhs@(DeclLHS arrMeta _) <- pDeclLHS
  maybeExpr <- optional $ do
    _ <- symbol "="
    pExpr
  case maybeExpr of
    Nothing | arrMeta == emptyMeta -> fail "Declaration must include an arrow or an equals"
    Nothing -> return $ RawDecl lhs [] Nothing
    Just expr -> return $ RawDecl lhs [] (Just expr)

data TreeRes
  = TRDecl PDecl
  | TRExpr PExpr
  | TRAnnot PCompAnnot
  deriving (Eq, Show)

validDeclTree :: [TreeRes] -> Either String ([PDeclSubStatement], PExpr)
validDeclTree = aux ([], Nothing)
  where
    aux (_, Nothing) [] = Left "No expression found. The declaration must contain an expression"
    aux (subSt, Just expr) [] = Right (subSt, expr)
    aux (subSt, maybeExpr) ((TRDecl decl):trs) = aux (RawDeclSubStatementDecl decl:subSt, maybeExpr) trs
    aux (subSt, maybeExpr) ((TRAnnot annot):trs) = aux (RawDeclSubStatementAnnot annot:subSt, maybeExpr) trs
    aux (subSt, Nothing) ((TRExpr expr):trs) = aux (subSt, Just expr) trs
    aux (_, Just{}) ((TRExpr _):_) = Left "Multiple expressions found. The declaration should only have one expression line"

pDeclTree :: Parser PDecl
pDeclTree = L.indentBlock scn p
  where
    pack lhs children = case validDeclTree children of
      Right (subStatements, expr) -> return $ RawDecl lhs subStatements (Just expr)
      Left err -> fail err
    childParser :: Parser TreeRes
    childParser = try (TRDecl <$> pDeclTree) <|> try (TRDecl <$> pDeclSingle) <|> try (TRAnnot <$> pCompAnnot) <|> (TRExpr <$> pExpr)
    p = do
      lhs <- pDeclLHS
      _ <- symbol "="
      return (L.IndentSome Nothing (pack lhs) childParser)

pRootDecl :: Parser PStatement
pRootDecl = do
  decl <- L.nonIndented scn (try pDeclTree <|> pDeclSingle)
  return $ RawDeclStatement decl
