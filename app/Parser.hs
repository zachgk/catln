{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
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

module Parser where

import Control.Applicative hiding (some, many)
import Control.Monad.Combinators.Expr
import Data.Void
import Data.Either
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

placeholderExpr :: Expr
placeholderExpr = CExpr $ Int 0

pCall :: Parser Expr
pCall = do
  funName <- identifier
  args <- parens $ sepBy1 pExpr (symbol ",")
  return $ Call funName args

term :: Parser Expr
term = parens pExpr
       <|> Var <$> identifier
       <|> (CExpr . Int) <$> integer
       <|> pCall

pExpr :: Parser Expr
pExpr = makeExprParser term ops

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
  lhs <- pDeclLHS
  expr <- pExpr
  return $ Decl lhs [] expr

pDeclTree :: Parser Decl
pDeclTree = L.indentBlock scn p
  where
    pack lhs children = if (isLeft $ last children) && (all isRight $ init children)
      then return $ Decl lhs (rights $ init children) (head $ lefts $ [last children])
      else fail $ "The declaration must end with an expression"
    childParser :: Parser (Either Expr Decl)
    childParser = try (Right <$> pDeclTree) <|> try (Right <$> pDeclSingle) <|> (Left <$> pExpr)
    p = do
      lhs <- pDeclLHS
      return (L.IndentSome Nothing (pack lhs) childParser)

pRootDecl :: Parser Decl
pRootDecl = L.nonIndented scn (try pDeclTree <|> pDeclSingle)

pPrgm :: Parser Prgm
pPrgm = do
  decls <- sepBy1 pRootDecl newline
  return $ Prgm [] [] decls

contents :: Parser a -> Parser a
contents p = do
  r <- p
  eof
  return r

parseFile :: String -> Either (ParseErrorBundle String Void) Prgm
parseFile s = runParser (contents pPrgm) "<stdin>" s

-- toplevel :: IndentParser Prgm
-- toplevel = do
--   es <- exprs
--   return $ Prgm [] [] es

-- parseExpr :: String -> Either ParseError Expr
-- parseExpr s = parse (contents expr) "<stdin>" s

-- parseToplevel :: String -> Either ParseError Prgm
-- parseToplevel s = parse (contents toplevel) "<stdin>" s
