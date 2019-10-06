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

import           Control.Applicative            hiding (many, some)
import           Control.Monad.Combinators.Expr
import           Data.Either
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Lexer
import           Syntax

type ParseMeta = PreTyped
type PExpr = RawExpr ParseMeta
type PDecl = RawDecl ParseMeta
type PDeclLHS = RawDeclLHS ParseMeta
type PPrgm = RawPrgm ParseMeta
type PReplRes = ReplRes ParseMeta

emptyMeta = PreTyped Nothing
intMeta = PreTyped (Just intType)
boolMeta = PreTyped (Just boolType)
strMeta = PreTyped (Just strType)

mkOp1 meta op x = Call meta op [x]
mkOp2 meta op x y = Call meta op [x, y]

ops :: [[Operator Parser PExpr]]
ops = [
    [ Prefix (mkOp1 intMeta "-"  <$ symbol "-")
    , Prefix (mkOp1 boolMeta "~" <$ symbol "~")
    ],
    [ InfixL (mkOp2 intMeta "*" <$ symbol "*")
    , InfixL (mkOp2 intMeta "/" <$ symbol "/")
    ],
    [ InfixL (mkOp2 intMeta "+" <$ symbol "+")
    , InfixL (mkOp2 intMeta "-" <$ symbol "-")
    ],
    [ InfixL (mkOp2 boolMeta "<" <$ symbol "<")
    , InfixL (mkOp2 boolMeta ">" <$ symbol ">")
    , InfixL (mkOp2 boolMeta "<=" <$ symbol "<=")
    , InfixL (mkOp2 boolMeta ">=" <$ symbol ">=")
    , InfixL (mkOp2 boolMeta "==" <$ symbol "==")
    , InfixL (mkOp2 boolMeta "!=" <$ symbol "!=")
    ],
    [ InfixL (mkOp2 boolMeta "&" <$ symbol "&")
    , InfixL (mkOp2 boolMeta "|" <$ symbol "|")
    , InfixL (mkOp2 boolMeta "^" <$ symbol "^")
    ]
  ]

pCall :: Parser PExpr
pCall = do
  funName <- (:) <$> letterChar <*> many alphaNumChar
  args <- parens $ sepBy1 pExpr (symbol ",")
  return $ Call emptyMeta funName args

pStringLiteral :: Parser PExpr
pStringLiteral = CExpr emptyMeta . CStr <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

term :: Parser PExpr
term = try (parens pExpr)
       <|> try pCall
       <|> pStringLiteral
       <|> try (Var emptyMeta <$> identifier)
       <|> CExpr emptyMeta . CInt <$> integer

pExpr :: Parser PExpr
pExpr = makeExprParser term ops

pArgs :: Parser [Name]
pArgs = sepBy1 identifier (symbol ",")

pDeclLHS :: Parser PDeclLHS
pDeclLHS = do
  val <- identifier
  args <- optional $ try $ parens pArgs
  _ <- symbol "="
  return $ case args of
    Just a  -> RawDeclFun val (zip a (repeat emptyMeta))
    Nothing -> RawDeclVal val

pDeclSingle :: Parser PDecl
pDeclSingle = do
  lhs <- pDeclLHS
  expr <- pExpr
  return $ RawDecl lhs [] expr

pDeclTree :: Parser PDecl
pDeclTree = L.indentBlock scn p
  where
    pack lhs children = if isLeft ( last children) && all isRight (init children)
      then return $ RawDecl lhs (rights $ init children) (head $ lefts [last children])
      else fail "The declaration must end with an expression"
    childParser :: Parser (Either PExpr PDecl)
    childParser = try (Right <$> pDeclTree) <|> try (Right <$> pDeclSingle) <|> (Left <$> pExpr)
    p = do
      lhs <- pDeclLHS
      return (L.IndentSome Nothing (pack lhs) childParser)

pRootDecl :: Parser PDecl
pRootDecl = L.nonIndented scn (try pDeclTree <|> pDeclSingle)

pPrgm :: Parser PPrgm
pPrgm = sepBy1 pRootDecl newline

contents :: Parser a -> Parser a
contents p = do
  r <- p
  eof
  return r

parseFile :: String -> Either ParseErrorRes PPrgm
parseFile = runParser (contents pPrgm) "<stdin>"

parseRepl :: String -> PReplRes
parseRepl s = case runParser (contents p) "<stdin>" s of
                Left e@(ParseErrorBundle _ _) -> ReplErr e
                Right (Left decl)             -> ReplDecl decl
                Right (Right expr)            -> ReplExpr expr
  where p = try (Left <$> pRootDecl) <|> try (Right <$> pExpr)
