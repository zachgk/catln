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
import           Control.Monad.Combinators.Expr
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet          as S
import           Data.Either
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Lexer
import           Syntax

type ParseMeta = PreTyped
type PExpr = RawExpr ParseMeta
type PDecl = RawDecl ParseMeta
type PDeclLHS = DeclLHS ParseMeta
type PPrgm = RawPrgm ParseMeta
type PReplRes = ReplRes ParseMeta

emptyMeta :: ParseMeta
emptyMeta = PreTyped RawTopType

mkOp1 :: ParseMeta -> String -> PExpr -> PExpr
mkOp1 meta op x = Tuple meta op (H.singleton "a" x)

mkOp2 :: ParseMeta -> String -> PExpr -> PExpr -> PExpr
mkOp2 meta op x y = Tuple meta op (H.fromList [("a", x), ("b", y)])

ops :: [[Operator Parser PExpr]]
ops = [
    [ Prefix (mkOp1 emptyMeta "-"  <$ symbol "-")
    , Prefix (mkOp1 emptyMeta "~" <$ symbol "~")
    ],
    [ InfixL (mkOp2 emptyMeta "*" <$ symbol "*")
    , InfixL (mkOp2 emptyMeta "/" <$ symbol "/")
    ],
    [ InfixL (mkOp2 emptyMeta "+" <$ symbol "+")
    , InfixL (mkOp2 emptyMeta "-" <$ symbol "-")
    ],
    [ InfixL (mkOp2 emptyMeta "<" <$ symbol "<")
    , InfixL (mkOp2 emptyMeta ">" <$ symbol ">")
    , InfixL (mkOp2 emptyMeta "<=" <$ symbol "<=")
    , InfixL (mkOp2 emptyMeta ">=" <$ symbol ">=")
    , InfixL (mkOp2 emptyMeta "==" <$ symbol "==")
    , InfixL (mkOp2 emptyMeta "!=" <$ symbol "!=")
    ],
    [ InfixL (mkOp2 emptyMeta "&" <$ symbol "&")
    , InfixL (mkOp2 emptyMeta "|" <$ symbol "|")
    , InfixL (mkOp2 emptyMeta "^" <$ symbol "^")
    ]
  ]

pCallArg :: Parser (String, PExpr)
pCallArg = do
  argName <- identifier
  equals <- symbol "="
  expr <- pExpr
  return (argName, expr)

pCall :: Parser PExpr
pCall = do
  funName <- (:) <$> letterChar <*> many alphaNumChar
  args <- parens $ sepBy1 pCallArg (symbol ",")
  return $ Tuple emptyMeta funName (H.fromList args)

pStringLiteral :: Parser PExpr
pStringLiteral = CExpr emptyMeta . CStr <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

term :: Parser PExpr
term = try (parens pExpr)
       <|> try pCall
       <|> pStringLiteral
       <|> try ((\i -> Tuple emptyMeta i H.empty) <$> identifier)
       <|> CExpr emptyMeta . CInt <$> integer

pExpr :: Parser PExpr
pExpr = makeExprParser term ops

pTypeArg :: Parser (String, RawLeafType)
pTypeArg = do
  argName <- identifier
  equals <- symbol "="
  tp <- tidentifier
  return (argName, RawLeafType tp)

pTypeProduct :: Parser RawLeafType
pTypeProduct = do
  name <- tidentifier
  args <- parens (sepBy1 pTypeArg (symbol ","))
  return $ RawProdType name (H.fromList args)

pLeafType :: Parser RawLeafType
pLeafType = try (RawLeafType <$> tidentifier)
        <|> try pTypeProduct

pType :: Parser RawType
pType = RawSumType . S.fromList <$> parens (sepBy1 pLeafType (symbol "|"))

pTypedIdentifier :: Parser (Name, ParseMeta)
pTypedIdentifier = do
  tp <- try $ optional pType
  val <- identifier
  return (val, PreTyped (fromMaybe RawTopType tp))

pArgs :: Parser [(Name, ParseMeta)]
pArgs = sepBy1 pTypedIdentifier (symbol ",")

pDeclLHS :: Parser PDeclLHS
pDeclLHS = do
  (val, meta) <- pTypedIdentifier
  args <- optional $ try $ parens pArgs
  _ <- symbol "="
  return $ case args of
    Just a  -> DeclLHS meta val (H.fromList a)
    Nothing -> DeclLHS meta val H.empty

pDeclSingle :: Parser PDecl
pDeclSingle = do
  lhs <- pDeclLHS
  RawDecl lhs [] <$> pExpr

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
