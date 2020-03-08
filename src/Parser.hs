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
import Parser.Syntax

emptyMeta :: ParseMeta
emptyMeta = PreTyped RawTopType

identifierMeta :: String -> ParseMeta
identifierMeta i = PreTyped $ RawSumType $ S.singleton $ RawLeafType i H.empty

intMeta, floatMeta, boolMeta, strMeta :: ParseMeta
intMeta = PreTyped rintType
floatMeta = PreTyped rfloatType
boolMeta = PreTyped rboolType
strMeta = PreTyped rstrType

mkOp1 :: H.HashMap String RawLeafType -> String -> PExpr -> PExpr
mkOp1 metaArgs op x = Tuple meta op (H.singleton "a" x)
  where meta = PreTyped $ RawSumType $ S.singleton $ RawLeafType op metaArgs

mkOp2 :: H.HashMap String RawLeafType -> String -> PExpr -> PExpr -> PExpr
mkOp2 metaArgs op x y = Tuple meta op (H.fromList [("l", x), ("r", y)])
  where meta = PreTyped $ RawSumType $ S.singleton $ RawLeafType op metaArgs

ops :: [[Operator Parser PExpr]]
ops = [
    [ Prefix (mkOp1 (H.singleton "a" rintLeaf) "-"  <$ symbol "-")
    , Prefix (mkOp1 (H.singleton "a" rboolLeaf) "~" <$ symbol "~")
    ],
    [ InfixL (mkOp2 intOpArgs "*" <$ symbol "*")
    , InfixL (mkOp2 intOpArgs "/" <$ symbol "/")
    ],
    [ InfixL (mkOp2 intOpArgs "+" <$ symbol "+")
    , InfixL (mkOp2 intOpArgs "-" <$ symbol "-")
    ],
    [ InfixL (mkOp2 cmpOpArgs "<" <$ symbol "<")
    , InfixL (mkOp2 cmpOpArgs ">" <$ symbol ">")
    , InfixL (mkOp2 cmpOpArgs "<=" <$ symbol "<=")
    , InfixL (mkOp2 cmpOpArgs ">=" <$ symbol ">=")
    , InfixL (mkOp2 cmpOpArgs "==" <$ symbol "==")
    , InfixL (mkOp2 cmpOpArgs "!=" <$ symbol "!=")
    ],
    [ InfixL (mkOp2 boolOpArgs "&" <$ symbol "&")
    , InfixL (mkOp2 boolOpArgs "|" <$ symbol "|")
    , InfixL (mkOp2 boolOpArgs "^" <$ symbol "^")
    ]
  ]
  where
    intOpArgs = H.fromList [("l", rintLeaf), ("r", rintLeaf)]
    cmpOpArgs = H.fromList [("l", rboolLeaf), ("r", rboolLeaf)]
    boolOpArgs = H.fromList [("l", rboolLeaf), ("r", rboolLeaf)]

pCallArg :: Parser ((String, RawLeafType), (String, PExpr))
pCallArg = do
  tp <- tidentifier
  argName <- identifier
  equals <- symbol "="
  expr <- pExpr
  return ((argName, RawLeafType tp H.empty), (argName, expr))

pCall :: Parser PExpr
pCall = do
  funName <- (:) <$> letterChar <*> many alphaNumChar
  argsList <- parens $ sepBy1 pCallArg (symbol ",")
  let (argTypes, argVals) = unzip argsList
  let metaArgs = H.fromList argTypes
  let meta = PreTyped $ RawSumType $ S.singleton $ RawLeafType funName metaArgs
  return $ Tuple meta funName (H.fromList argVals)

pStringLiteral :: Parser PExpr
pStringLiteral = CExpr strMeta . CStr <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

term :: Parser PExpr
term = try (parens pExpr)
       <|> try pCall
       <|> pStringLiteral
       <|> try ((\i -> Tuple (identifierMeta i) i H.empty) <$> identifier)
       <|> CExpr intMeta . CInt <$> integer

pExpr :: Parser PExpr
pExpr = makeExprParser term ops

pTypeArg :: Parser (String, RawLeafType)
pTypeArg = do
  argName <- identifier
  equals <- symbol "="
  tp <- tidentifier
  return (argName, RawLeafType tp H.empty)

pTypeProduct :: Parser RawLeafType
pTypeProduct = do
  name <- tidentifier
  args <- parens (sepBy1 pTypeArg (symbol ","))
  return $ RawLeafType name (H.fromList args)

pLeafType :: Parser RawLeafType
pLeafType = try ((`RawLeafType` H.empty) <$> tidentifier)
        <|> try pTypeProduct

pType :: Parser RawLeafSet
pType = S.fromList <$> sepBy1 pLeafType (symbol "|")

pTypedIdentifier :: Parser (Name, ParseMeta)
pTypedIdentifier = do
  tp <- try $ optional $ parens $ RawSumType <$> pType
  val <- identifier
  let tp' = case tp of
        Just t -> PreTyped t
        Nothing -> identifierMeta val
  return (val, tp')

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
  RawDecl lhs [] . Just <$> pExpr

pDeclTree :: Parser PDecl
pDeclTree = L.indentBlock scn p
  where
    pack lhs children = if isLeft ( last children)
      then return $ RawDecl lhs (rights $ init children) (head $ lefts [last children])
      else fail "The declaration must end with an expression"
    childParser :: Parser (Either (Maybe PExpr) PDecl)
    childParser = try (Right <$> pDeclTree) <|> try (Right <$> pDeclSingle) <|> (Left . Just <$> pExpr)
    p = do
      lhs <- pDeclLHS
      return (L.IndentSome Nothing (pack lhs) childParser)

pRootDecl :: Parser PStatement
pRootDecl = do
  decl <- L.nonIndented scn (try pDeclTree <|> pDeclSingle)
  return $ RawDeclStatement decl

pTypeDefStatement :: Parser PStatement
pTypeDefStatement = do
  symbol "data"
  name <- tidentifier
  symbol "="
  RawTypeDefStatement . RawTypeDef name <$> pType

pClassDefStatement :: Parser PStatement
pClassDefStatement = do
  symbol "instance"
  typeName <- tidentifier
  symbol "of"
  className <- tidentifier
  return $ RawClassDefStatement (typeName, className)

pStatement :: Parser PStatement
pStatement = try pTypeDefStatement
             <|> try pClassDefStatement
             <|> try pRootDecl

pPrgm :: Parser PPrgm
pPrgm = sepBy1 pStatement newline

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
                Right (Left statement)             -> ReplStatement statement
                Right (Right expr)            -> ReplExpr expr
  where p = try (Left <$> pStatement) <|> try (Right <$> pExpr)
