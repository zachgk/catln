--------------------------------------------------------------------
-- |
-- Module    :  Parser.Expr
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Parser.Expr where

import           Control.Applicative            hiding (many, some)
import           Control.Monad.Combinators.Expr
import qualified Data.HashMap.Strict as H
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           Parser.Lexer
import Parser.Syntax

mkOp1 :: String -> PExpr -> PExpr
mkOp1 opChars x = RawTupleApply emptyMetaN (emptyMetaN, RawValue emptyMetaN op) [RawTupleArgNamed "a" x]
  where op = "operator" ++ opChars

mkOp2 :: String -> PExpr -> PExpr -> PExpr
mkOp2 opChars x y = RawTupleApply emptyMetaN (emptyMetaN, RawValue emptyMetaN op) [RawTupleArgNamed "l" x, RawTupleArgNamed "r" y]
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

pCallArg :: Parser PTupleArg
pCallArg = do
  maybeArgName <- optional $ do
    n <- identifier
    _ <- symbol "="
    return n
  expr <- pExpr
  return $ case maybeArgName of
    Just argName -> RawTupleArgNamed argName expr
    Nothing -> RawTupleArgInfer expr

pCall :: Parser PExpr
pCall = do
  pos <- getSourcePos
  funName <- identifier <|> tidentifier
  maybeArgVals <- optional $ parens $ sepBy1 pCallArg (symbol ",")
  let baseValue = RawValue (emptyMeta pos) funName
  return $ case maybeArgVals of
    Just argVals -> RawTupleApply (emptyMeta pos) (emptyMeta pos, baseValue) argVals
    Nothing -> baseValue

pStringLiteral :: Parser PExpr
pStringLiteral = do
  pos <- getSourcePos
  RawCExpr (emptyMeta pos) . CStr <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

pIfThenElse :: Parser PExpr
pIfThenElse = do
  pos <- getSourcePos
  _ <- symbol "if"
  condExpr <- pExpr
  _ <- symbol "then"
  thenExpr <- pExpr
  _ <- symbol "else"
  RawIfThenElse (emptyMeta pos) condExpr thenExpr <$> pExpr

pMatchCaseHelper :: String -> Parser (PExpr, [(PPattern, PExpr)])
pMatchCaseHelper keyword = L.indentBlock scn p
  where
    pack expr matchItems = return (expr, matchItems)
    pItem = do
      patt <- pPattern MatchObj
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
  pos <- getSourcePos
  (expr, matchItems) <- pMatchCaseHelper "case"
  return $ RawCase (emptyMeta pos) expr matchItems

pMatch :: Parser PExpr
pMatch = do
  pos <- getSourcePos
  (expr, matchItems) <- pMatchCaseHelper "match"
  return $ RawMatch (emptyMeta pos) expr matchItems

pMethod :: Parser PExpr
pMethod = do
  _ <- string "."
  pCall

term :: Parser PExpr
term = do
  pos <- getSourcePos
  base <- try (parens pExpr)
       <|> pIfThenElse
       <|> pMatch
       <|> pCase
       <|> pStringLiteral
       <|> RawCExpr (emptyMeta pos) . CInt <$> integer
       <|> try pCall
       <|> (RawValue (emptyMeta pos) <$> tidentifier)
  methods <- many pMethod
  return $ case methods of
    [] -> base
    ms -> RawMethods base ms

pExpr :: Parser PExpr
pExpr = makeExprParser term ops

-- Pattern

pIfGuard :: Parser PGuard
pIfGuard = do
  _ <- symbol "if"
  IfGuard <$> pExpr

pElseGuard :: Parser PGuard
pElseGuard = do
  _ <- symbol "else"
  return ElseGuard

pPatternGuard :: Parser PGuard
pPatternGuard = fromMaybe NoGuard <$> optional (try pIfGuard
                                              <|> pElseGuard
                                            )

pObjTreeVar :: Parser (TypeVarName, ParseMeta)
pObjTreeVar = do
  -- TODO: Should support multiple class identifiers such as <Eq Ord $T>
  pos <- getSourcePos
  maybeClass <- optional tidentifier
  var <- tvar
  let tp = maybe TopType (\n -> singletonType (PartialType (PTypeName n) H.empty H.empty H.empty PtArgExact)) maybeClass
  return (var, PreTyped tp (Just pos))

pObjTreeArgPattern :: Parser (ArgName, PObjArg)
pObjTreeArgPattern = do
  val <- identifier
  _ <- symbol "="
  pos <- getSourcePos
  subTree <- pObjTree PatternObj
  return (val, (emptyMeta pos, Just subTree))

pObjTreeArgName :: Parser (ArgName, PObjArg)
pObjTreeArgName = do
  pos <- getSourcePos
  tp <- try $ optional pType
  val <- identifier
  let tp' = maybe (emptyMeta pos) (`PreTyped` Just pos) tp
  return (val, (tp', Nothing))

pObjTreeArgs :: Parser [(ArgName, PObjArg)]
pObjTreeArgs = sepBy1 (try pObjTreeArgPattern <|> pObjTreeArgName) (symbol ",")

pObjTree :: ObjectBasis -> Parser PObject
pObjTree basis = do
  pos <- getSourcePos
  name <- opIdentifier <|> identifier <|> tidentifier
  vars <- try $ optional $ angleBraces $ sepBy1 pObjTreeVar (symbol ",")
  args <- optional $ parens pObjTreeArgs
  let vars' = maybe H.empty H.fromList vars
  let args' = H.fromList $ fromMaybe [] args
  return $ Object (emptyMeta pos) basis name vars' args'

pPattern :: ObjectBasis -> Parser PPattern
pPattern basis = do
  objTree <- pObjTree basis
  Pattern objTree <$> pPatternGuard

-- Pattern Types


pLeafVar :: Parser (TypeVarName, Type)
pLeafVar = do
  var <- tvar
  return (var, TopType)

pTypeArg :: Parser (String, Type)
pTypeArg = do
  argName <- identifier
  _ <- symbol "="
  tp <- tidentifier
  return (argName, singletonType (PartialType (PTypeName tp) H.empty H.empty H.empty PtArgExact))

pTypeVar :: Parser Type
pTypeVar = TypeVar . TVVar <$> tvar

-- TODO: Parse type properties
pLeafType :: Parser PartialType
pLeafType = do
  name <- tidentifier
  maybeVars <- try $ optional $ angleBraces $ sepBy1 pLeafVar (symbol ",")
  maybeArgs <- optional $ parens (sepBy1 pTypeArg (symbol ","))
  let vars = maybe H.empty H.fromList maybeVars
  let args = maybe H.empty H.fromList maybeArgs
  return (PartialType (PTypeName name) vars H.empty args PtArgExact)

pSingleType :: Parser Type
pSingleType = pTypeVar
              <|> singletonType <$> pLeafType

pType :: Parser Type
pType = pTypeVar
        <|> SumType . joinPartialLeafs <$> sepBy1 pLeafType (symbol "|")
