--------------------------------------------------------------------
-- |
-- Module    :  Parser.Expr
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is used to parse 'RawExpr'.
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Parser.Expr where

import           Control.Applicative            hiding (many, some)
import           Control.Monad.Combinators.Expr
import qualified Data.HashMap.Strict            as H
import           Data.Maybe
import           Text.Megaparsec                hiding (pos1)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Parser.Lexer
import           Parser.Syntax
import           Syntax
import           Syntax.Prgm
import           Syntax.Types

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
    , InfixL (mkOp2 "//" <$ symbol "//")
    ],
    [ InfixL (mkOp2 "++" <$ symbol "++")
    , InfixL (mkOp2 "+" <$ symbol "+")
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
  maybeArgName <- optional . try $ do
    n <- identifier
    _ <- symbol "="
    return n
  expr <- pExpr
  return $ case maybeArgName of
    Just argName -> RawTupleArgNamed argName expr
    Nothing      -> RawTupleArgInfer expr

pCall :: Parser PExpr
pCall = do
  pos1 <- getSourcePos
  funName <- identifier <|> tidentifier
  pos2 <- getSourcePos
  maybeArgVals <- optional $ parens $ sepBy1 pCallArg (symbol ",")
  pos3 <- getSourcePos
  let m1 = emptyMeta pos1 pos2
  let m2 = emptyMeta pos2 pos3
  let baseValue = RawValue m1 funName
  return $ case maybeArgVals of
    Just argVals -> RawTupleApply m2 (labelPosM "call" m1, baseValue) argVals
    Nothing      -> baseValue

pStringLiteral :: Parser PExpr
pStringLiteral = do
  pos1 <- getSourcePos
  s <- char '\"' *> manyTill L.charLiteral (char '\"')
  pos2 <- getSourcePos
  return $ RawCExpr (emptyMeta pos1 pos2) (CStr s)

parenExpr :: Parser PExpr
parenExpr = do
  e <- parens pExpr
  return $ RawParen e

pIfThenElse :: Parser PExpr
pIfThenElse = do
  pos1 <- getSourcePos
  _ <- symbol "if"
  condExpr <- pExpr
  _ <- symbol "then"
  thenExpr <- pExpr
  _ <- symbol "else"
  elseExpr <- pExpr
  pos2 <- getSourcePos
  return $ RawIfThenElse (emptyMeta pos1 pos2) condExpr thenExpr elseExpr

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
  pos1 <- getSourcePos
  (expr, matchItems) <- pMatchCaseHelper "case"
  pos2 <- getSourcePos
  return $ RawCase (emptyMeta pos1 pos2) expr matchItems

pMatch :: Parser PExpr
pMatch = do
  pos1 <- getSourcePos
  (expr, matchItems) <- pMatchCaseHelper "match"
  pos2 <- getSourcePos
  return $ RawMatch (emptyMeta pos1 pos2) expr matchItems

pMethod :: Parser PExpr
pMethod = do
  _ <- string "."
  pCall

pInt :: Parser PExpr
pInt = do
  pos1 <- getSourcePos
  i <- integer
  pos2 <- getSourcePos
  return $ RawCExpr (emptyMeta pos1 pos2) (CInt i)

pList :: Parser PExpr
pList = do
  pos1 <- getSourcePos
  _ <- string "["
  lst <- sepBy pExpr (symbol ",")
  _ <- string "]"
  pos2 <- getSourcePos
  return $ RawList (emptyMeta pos1 pos2) lst

term :: Parser PExpr
term = do
  base <- parenExpr
       <|> pIfThenElse
       <|> pMatch
       <|> pCase
       <|> pStringLiteral
       <|> pInt
       <|> pList
       <|> pCall
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
pPatternGuard = fromMaybe NoGuard <$> optional (pIfGuard
                                              <|> pElseGuard
                                            )

pMethodCallerType :: Parser PObjArg
pMethodCallerType = do
  pos1 <- getSourcePos
  t <- singletonType <$> pLeafType
  pos2 <- getSourcePos
  _ <- symbol "."
  return (PreTyped t (Just (pos1, pos2, "")), Nothing)

pObjTreeVar :: Parser (TypeVarName, ParseMeta)
pObjTreeVar = do
  -- TODO: Should support multiple class identifiers such as <Eq Ord $T>
  pos1 <- getSourcePos
  maybeClass <- optional tidentifier
  var <- tvar
  pos2 <- getSourcePos
  let tp = maybe TopType (\n -> singletonType (PartialType (PTypeName n) H.empty H.empty H.empty PtArgExact)) maybeClass
  return (var, PreTyped tp (Just (pos1, pos2, "")))

pObjTreeArgPattern :: Parser (ArgName, PObjArg)
pObjTreeArgPattern = do
  pos1 <- getSourcePos
  val <- identifier
  _ <- symbol "="
  subTree <- pObjTree PatternObj
  pos2 <- getSourcePos
  return (val, (emptyMeta pos1 pos2, Just subTree))

pObjTreeArgName :: Parser (ArgName, PObjArg)
pObjTreeArgName = do
  pos1 <- getSourcePos
  tp <- try $ optional pType
  val <- identifier
  pos2 <- getSourcePos
  let tp' = maybe (emptyMeta pos1 pos2) (`PreTyped` Just (pos1, pos2, "")) tp
  return (val, (tp', Nothing))

pObjTreeArgs :: Parser [(ArgName, PObjArg)]
pObjTreeArgs = sepBy1 (try pObjTreeArgPattern <|> pObjTreeArgName) (symbol ",")

pObjTreeInner :: ObjectBasis -> Parser PObject
pObjTreeInner basis = do
  pos1 <- getSourcePos
  name <- opIdentifier <|> identifier <|> tidentifier
  maybeCtx <- optional $ curlyBraces $ sepBy pObjTreeArgName (symbol ",")
  vars <- optional $ angleBraces $ sepBy1 pObjTreeVar (symbol ",")
  args <- optional $ parens pObjTreeArgs
  pos2 <- getSourcePos
  let vars' = maybe H.empty H.fromList vars
  let args' = H.fromList $ fromMaybe [] args
  let objMeta = emptyMeta pos1 pos2
  let obj = Object objMeta basis name vars' args' Nothing name
  return $ case maybeCtx of
    Just ctx -> Object (emptyMetaM "context" objMeta) basis "Context" H.empty (H.insert "value" (emptyMetaN, Just obj) $ H.fromList ctx) Nothing "Context"
    Nothing -> obj

objTreeJoinMethods :: PObject -> PObject -> PObject
objTreeJoinMethods obj@Object{objM} meth@Object{objArgs=methArgs} = meth{objArgs = H.insert "this" (emptyMetaM "meth" objM, Just obj) methArgs}

pObjTree :: ObjectBasis -> Parser PObject
pObjTree basis = do
  maybeStartType <- optional $ try pMethodCallerType
  objs <- sepBy1 (pObjTreeInner basis) (string ".")
  let objs'@Object{objArgs} = foldl1 objTreeJoinMethods objs
  return $ case maybeStartType of
    Just startType -> objs'{objArgs = H.insert "this" startType objArgs}
    Nothing        -> objs'

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
  maybeVars <- optional $ angleBraces $ sepBy1 pLeafVar (symbol ",")
  maybeArgs <- optional $ parens (sepBy1 pTypeArg (symbol ","))
  let vars = maybe H.empty H.fromList maybeVars
  let args = maybe H.empty H.fromList maybeArgs
  return (PartialType (PTypeName name) vars H.empty args PtArgExact)

pSingleType :: Parser Type
pSingleType = pTypeVar
              <|> singletonType <$> pLeafType

pType :: Parser Type
pType = pTypeVar
        <|> UnionType . joinUnionType <$> sepBy1 pLeafType (symbol "|")
