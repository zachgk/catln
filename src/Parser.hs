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
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Lexer
import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import Parser.Syntax

mkOp1 :: String -> PExpr -> PExpr
mkOp1 opChars x = RawTupleApply emptyMeta (emptyMeta, RawValue emptyMeta op) (H.singleton "a" x)
  where op = "operator" ++ opChars

mkOp2 :: String -> PExpr -> PExpr -> PExpr
mkOp2 opChars x y = RawTupleApply emptyMeta (emptyMeta, RawValue emptyMeta op) (H.fromList [("l", x), ("r", y)])
  where op = "operator" ++ opChars

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
    [ InfixL (mkOp2 "<=" <$ symbol "<=")
    , InfixL (mkOp2 ">=" <$ symbol ">=")
    , InfixL (mkOp2 "<" <$ symbol "<")
    , InfixL (mkOp2 ">" <$ symbol ">")
    , InfixL (mkOp2 "==" <$ symbol "==")
    , InfixL (mkOp2 "!=" <$ symbol "!=")
    ],
    [ InfixL (mkOp2 "&" <$ symbol "&")
    , InfixL (mkOp2 "|" <$ symbol "|")
    , InfixL (mkOp2 "^" <$ symbol "^")
    ]
  ]

pImport :: Parser String
pImport = do
  _ <- symbol "import"
  some printChar

pCallArg :: Parser (String, PExpr)
pCallArg = do
  argName <- identifier
  _ <- symbol "="
  expr <- pExpr
  return (argName, expr)

pCall :: Parser PExpr
pCall = do
  funName <- identifier
  maybeArgVals <- optional $ parens $ sepBy1 pCallArg (symbol ",")
  let baseValue = RawValue emptyMeta funName
  return $ case maybeArgVals of
    Just argVals -> RawTupleApply emptyMeta (emptyMeta, baseValue) (H.fromList argVals)
    Nothing -> baseValue

pCompAnnot :: Parser PCompAnnot
pCompAnnot = do
  _ <- string "#"
  annotName <- (:) <$> letterChar <*> many alphaNumChar
  argVals <- parens $ sepBy1 pCallArg (symbol ",")
  return $ CompAnnot annotName (H.fromList argVals)

pStringLiteral :: Parser PExpr
pStringLiteral = RawCExpr emptyMeta . CStr <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

pIfThenElse :: Parser PExpr
pIfThenElse = do
  _ <- symbol "if"
  condExpr <- pExpr
  _ <- symbol "then"
  thenExpr <- pExpr
  _ <- symbol "else"
  RawIfThenElse emptyMeta condExpr thenExpr <$> pExpr


pPatternGuard :: Parser PGuard
pPatternGuard = fromMaybe NoGuard <$> optional (try pIfGuard
                                              <|> pElseGuard
                                            )

pObjTreeVar :: Parser (TypeVarName, ParseMeta)
pObjTreeVar = do
  var <- tvar
  return (var, emptyMeta)

pObjTreeArgPattern :: Parser (ArgName, PObjArg)
pObjTreeArgPattern = do
  val <- identifier
  _ <- symbol "="
  subTree <- pObjTree PatternObj
  return (val, (emptyMeta, Just subTree))

pObjTreeArgName :: Parser (ArgName, PObjArg)
pObjTreeArgName = do
  tp <- try $ optional pType
  val <- identifier
  let tp' = maybe emptyMeta PreTyped tp
  return (val, (tp', Nothing))

pObjTreeArgs :: Parser [(ArgName, PObjArg)]
pObjTreeArgs = sepBy1 (try pObjTreeArgPattern <|> pObjTreeArgName) (symbol ",")

pObjTree :: ObjectBasis -> Parser PObject
pObjTree basis = do
  name <- opIdentifier <|> identifier
  vars <- try $ optional $ angleBraces $ sepBy1 pObjTreeVar (symbol ",")
  args <- optional $ parens pObjTreeArgs
  let vars' = maybe H.empty H.fromList vars
  let args' = H.fromList $ fromMaybe [] args
  return $ Object emptyMeta basis name vars' args'

pPattern :: ObjectBasis -> Parser PPattern
pPattern basis = do
  objTree <- pObjTree basis
  Pattern objTree <$> pPatternGuard

pMatchCaseHelper :: String -> Parser (PExpr, [(PPattern, PExpr)])
pMatchCaseHelper keyword = L.indentBlock scn p
  where
    pack expr matchItems = return (expr, matchItems)
    pItem = do
      patt <- pPattern PatternObj
      _ <- symbol "=>"
      expr <- pExpr
      return (patt, expr)
    p = do
      _ <- symbol keyword
      expr <- pExpr
      _ <- symbol "of"
      return $ L.IndentSome Nothing (pack expr) pItem

pCase :: Parser PExpr
pCase = do
  (expr, matchItems) <- pMatchCaseHelper "case"
  return $ RawCase emptyMeta expr matchItems

pMatch :: Parser PExpr
pMatch = do
  (expr, matchItems) <- pMatchCaseHelper "match"
  return $ RawMatch emptyMeta expr (H.fromList matchItems)

term :: Parser PExpr
term = try (parens pExpr)
       <|> pIfThenElse
       <|> pMatch
       <|> pCase
       <|> pStringLiteral
       <|> RawCExpr emptyMeta . CInt <$> integer
       <|> try pCall
       <|> (RawValue emptyMeta <$> tidentifier)

pExpr :: Parser PExpr
pExpr = makeExprParser term ops

pTypeArg :: Parser (String, Type)
pTypeArg = do
  argName <- identifier
  _ <- symbol "="
  tp <- tidentifier
  return (argName, SumType $ joinPartialLeafs [(tp, H.empty, H.empty)])

pTypeVar :: Parser Type
pTypeVar = TypeVar <$> tvar

pLeafType :: Parser PartialType
pLeafType = do
  name <- tidentifier
  maybeArgs <- optional $ parens (sepBy1 pTypeArg (symbol ","))
  case maybeArgs of
    Just args -> return (name, H.empty, H.fromList args)
    Nothing -> return (name, H.empty, H.empty)

pSingleType :: Parser Type
pSingleType = pTypeVar
              <|> SumType . joinPartialLeafs . pure <$> pLeafType

pType :: Parser Type
pType = pTypeVar
        <|> SumType . joinPartialLeafs <$> sepBy1 pLeafType (symbol "|")

pIfGuard :: Parser PGuard
pIfGuard = do
  _ <- symbol "if"
  IfGuard <$> pExpr

pElseGuard :: Parser PGuard
pElseGuard = do
  _ <- symbol "else"
  return ElseGuard

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
  lhs <- pDeclLHS
  maybeExpr <- optional $ do
    _ <- symbol "="
    pExpr
  return $ case maybeExpr of
    Just expr -> RawDecl lhs [] (Just expr)
    Nothing -> RawDecl lhs [] Nothing

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
      _ <- symbol "="
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
  TypeDefStatement . TypeDef name <$> pType

pClassDefStatement :: Parser PStatement
pClassDefStatement = do
  _ <- symbol "instance"
  typeName <- tidentifier
  _ <- symbol "of"
  className <- tidentifier
  return $ RawClassDefStatement (typeName, className)

pStatement :: Parser PStatement
pStatement = pTypeDefStatement
             <|> try pClassDefStatement
             <|> pRootDecl

pPrgm :: Parser PPrgm
pPrgm = do
  _ <- many newline
  imports <- many pImport
  _ <- many newline
  statements <- sepBy1 pStatement (some newline)
  _ <- many newline
  return (imports, statements)

contents :: Parser a -> Parser a
contents p = do
  r <- p
  eof
  return r

parseFile :: String -> CRes PPrgm
parseFile f = case runParser (contents pPrgm) "<stdin>" f of
  Left err -> CErr [ParseCErr err]
  Right prgm -> return prgm

parseRepl :: String -> PReplRes
parseRepl s = case runParser (contents p) "<stdin>" s of
                Left e@(ParseErrorBundle _ _) -> ReplErr e
                Right (Left statement)             -> ReplStatement statement
                Right (Right expr)            -> ReplExpr expr
  where p = try (Left <$> pStatement) <|> try (Right <$> pExpr)
