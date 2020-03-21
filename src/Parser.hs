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
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Lexer
import           Syntax
import Parser.Syntax

identifierMeta :: String -> ParseMeta
identifierMeta i = PreTyped $ RawSumType $ S.singleton $ RawLeafType i H.empty

intMeta, floatMeta, boolMeta, strMeta :: ParseMeta
intMeta = PreTyped rintType
floatMeta = PreTyped rfloatType
boolMeta = PreTyped rboolType
strMeta = PreTyped rstrType

mkOp1 :: String -> PExpr -> PExpr
mkOp1 op x = TupleApply emptyMeta (emptyMeta, Value metaVal op) (H.singleton "a" x)
  where metaVal = identifierMeta op

mkOp2 :: String -> PExpr -> PExpr -> PExpr
mkOp2 op x y = TupleApply emptyMeta (emptyMeta, Value metaVal op) (H.fromList [("l", x), ("r", y)])
  where metaVal = identifierMeta op

ops :: [[Operator Parser PExpr]]
ops = [
    [ Prefix (mkOp1 "-"  <$ symbol "-")
    , Prefix (mkOp1 "~" <$ symbol "~")
    ],
    [ InfixL (mkOp2 "*" <$ symbol "*")
    , InfixL (mkOp2 "/" <$ symbol "/")
    ],
    [ InfixL (mkOp2 "+" <$ symbol "+")
    , InfixL (mkOp2 "-" <$ symbol "-")
    ],
    [ InfixL (mkOp2 "<" <$ symbol "<")
    , InfixL (mkOp2 ">" <$ symbol ">")
    , InfixL (mkOp2 "<=" <$ symbol "<=")
    , InfixL (mkOp2 ">=" <$ symbol ">=")
    , InfixL (mkOp2 "==" <$ symbol "==")
    , InfixL (mkOp2 "!=" <$ symbol "!=")
    ],
    [ InfixL (mkOp2 "&" <$ symbol "&")
    , InfixL (mkOp2 "|" <$ symbol "|")
    , InfixL (mkOp2 "^" <$ symbol "^")
    ]
  ]

pCallArg :: Parser (String, PExpr)
pCallArg = do
  argName <- identifier
  _ <- symbol "="
  expr <- pExpr
  return (argName, expr)

pCall :: Parser PExpr
pCall = do
  funName <- (:) <$> letterChar <*> many alphaNumChar
  argVals <- parens $ sepBy1 pCallArg (symbol ",")
  return $ TupleApply emptyMeta (emptyMeta, Value (identifierMeta funName) funName) (H.fromList argVals)

pCompAnnot :: Parser PCompAnnot
pCompAnnot = do
  _ <- string "#"
  annotName <- (:) <$> letterChar <*> many alphaNumChar
  argVals <- parens $ sepBy1 pCallArg (symbol ",")
  return $ CompAnnot annotName (H.fromList argVals)

pStringLiteral :: Parser PExpr
pStringLiteral = CExpr strMeta . CStr <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

term :: Parser PExpr
term = try (parens pExpr)
       <|> try pCall
       <|> pStringLiteral
       <|> try ((\i -> Value (identifierMeta i) i) <$> identifier)
       <|> CExpr intMeta . CInt <$> integer

pExpr :: Parser PExpr
pExpr = makeExprParser term ops

pTypeArg :: Parser (String, RawLeafType)
pTypeArg = do
  argName <- identifier
  _ <- symbol "="
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

data TreeRes
  = TRDecl PDecl
  | TRExpr PExpr
  | TRAnnot PCompAnnot

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
      return (L.IndentSome Nothing (pack lhs) childParser)

pRootDecl :: Parser PStatement
pRootDecl = do
  decl <- L.nonIndented scn (try pDeclTree <|> pDeclSingle)
  return $ RawDeclStatement decl

pTypeDefStatement :: Parser PStatement
pTypeDefStatement = do
  _ <- symbol "data"
  name <- tidentifier
  _ <- symbol "="
  RawTypeDefStatement . RawTypeDef name <$> pType

pClassDefStatement :: Parser PStatement
pClassDefStatement = do
  _ <- symbol "instance"
  typeName <- tidentifier
  _ <- symbol "of"
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
