{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
--------------------------------------------------------------------
-- |
-- Module    :  Parser
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Parser where

import Control.Applicative hiding (some, many)
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Lexer
import Syntax

ops :: [[Operator Parser Expr]]
ops = [
    [ Prefix (UnaryOp "-" <$ symbol "-")
    , Prefix (UnaryOp "~" <$ symbol "~")
    ],
    [ InfixL (BinaryOp "*" <$ symbol "*")
    , InfixL (BinaryOp "/" <$ symbol "/")
    ],
    [ InfixL (BinaryOp "+" <$ symbol "+")
    , InfixL (BinaryOp "-" <$ symbol "-")
    ],
    [ InfixL (BinaryOp "<" <$ symbol "<")
    , InfixL (BinaryOp ">" <$ symbol ">")
    , InfixL (BinaryOp "<=" <$ symbol "<=")
    , InfixL (BinaryOp ">=" <$ symbol ">=")
    , InfixL (BinaryOp "==" <$ symbol "==")
    , InfixL (BinaryOp "!=" <$ symbol "!=")
    ],
    [ InfixL (BinaryOp "&" <$ symbol "&")
    , InfixL (BinaryOp "|" <$ symbol "|")
    , InfixL (BinaryOp "^" <$ symbol "^")
    ]
  ]

opExpr :: Parser Expr
opExpr = makeExprParser term ops

placeholderExpr :: Expr
placeholderExpr = CExpr $ Int 0

pCall :: Parser Expr
pCall = do
  funName <- identifier
  args <- parens $ sepBy1 opExpr (symbol ",")
  return $ Call funName args

term :: Parser Expr
term = parens opExpr
       <|> Var <$> identifier
       <|> (CExpr . Int) <$> integer
       <|> pCall

pExpr :: Parser Expr
pExpr = undefined

pArgs :: Parser [Name]
pArgs = sepBy1 identifier (symbol ",")

pDeclLHS :: Parser DeclLHS
pDeclLHS = do
  val <- identifier
  args <- optional $ try $ parens pArgs
  _ <- symbol "="
  return $ case args of
    Just a -> DeclFun val a
    Nothing -> DeclVal val

pDeclSingle :: Parser Decl
pDeclSingle = do
  header <- pDeclLHS
  expr <- pExpr
  return $ Decl header [] expr

pDeclTree :: Parser Decl
pDeclTree = L.indentBlock scn p
  where
    pack header children = return $ Decl header children placeholderExpr
    p = do
      header <- pDeclLHS
      return (L.IndentSome Nothing (pack header) (try pDeclTree <|> pDeclSingle))

pRootDecl :: Parser Decl
pRootDecl = L.nonIndented scn (pDeclTree)

pPrgm :: Parser Prgm
pPrgm = do
  decls <- many pRootDecl
  return $ Prgm [] [] decls

-- toplevel :: IndentParser Prgm
-- toplevel = do
--   es <- exprs
--   return $ Prgm [] [] es

-- parseExpr :: String -> Either ParseError Expr
-- parseExpr s = parse (contents expr) "<stdin>" s

-- parseToplevel :: String -> Either ParseError Prgm
-- parseToplevel s = parse (contents toplevel) "<stdin>" s
