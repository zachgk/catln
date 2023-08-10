--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Parser.Expr
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is used to parse 'RawExpr'.
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Syntax.Ct.Parser.Expr where

import           Control.Applicative            hiding (many, some)
import           Control.Monad.Combinators.Expr
import           Data.Maybe
import           Text.Megaparsec                hiding (pos1)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Constants
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Desugarf.Expr
import           Syntax.Ct.Parser.Lexer
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm

mkOp1 :: String -> PExpr -> PExpr
mkOp1 opChars x = applyRawArgs (RawValue emptyMetaN op) [(Just operatorArgUnary, x)]
  where op = operatorPrefix ++ opChars

mkOp2 :: String -> PExpr -> PExpr -> PExpr
mkOp2 opChars x y = applyRawArgs (RawValue emptyMetaN op) [(Just operatorArgL, x), (Just operatorArgR, y)]
  where op = operatorPrefix ++ opChars

pMinus :: Parser String
pMinus = do
  isArrow <- optional $ try $ lookAhead $ symbol "->"
  case isArrow of
    Just _  -> fail "-> should not be matched for - operator"
    Nothing -> symbol "-"

ops :: [[Operator Parser PExpr]]
ops = [
    [ Prefix (mkOp1 "-"  <$ pMinus)
    , Prefix (mkOp1 "~" <$ symbol "~")
    ],
    [ InfixL (RawMethod <$ symbol ".")
    ],
    [ InfixL (mkOp2 ":" <$ symbol ":")
    ],
    [ InfixL (mkOp2 "*" <$ symbol "*")
    , InfixL (mkOp2 "//" <$ symbol "//")
    ],
    [ InfixL (mkOp2 "++" <$ symbol "++")
    , InfixL (mkOp2 "+" <$ symbol "+")
    , InfixL (mkOp2 "-" <$ pMinus)
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

pValue :: Parser PExpr
pValue = do
  pos1 <- getSourcePos
  usingTheExpr <- optional $ string ":"
  name <- identifier <|> tidentifier <|> pAnnotIdentifier <|> tvar <|> pHole
  pos2 <- getSourcePos
  let m = emptyMeta pos1 pos2

  -- check if name is a hole
  return $ case name of
        "_"         -> RawHoleExpr m $ HoleActive Nothing
        ('_':_)     -> RawHoleExpr m $ HoleActive (Just name)
        "undefined" -> RawHoleExpr m HoleUndefined
        "todefine"  -> RawHoleExpr m HoleTodefine
        _           -> case usingTheExpr of
          Just _  -> RawTheExpr (RawValue m name)
          Nothing -> RawValue m name

pStringLiteral :: Parser PExpr
pStringLiteral = do
  pos1 <- getSourcePos
  s <- char '\"' *> manyTill L.charLiteral (char '\"')
  pos2 <- getSourcePos
  return $ RawCExpr (emptyMeta pos1 pos2) (CStr s)

parenExpr ::  Parser PExpr
parenExpr = do
  e <- parens pExpr
  return $ RawParen e

pArrowFull :: ObjectBasis -> Parser PObjArr
pArrowFull basis = do
  expr1 <- pExpr
  (guard, guardAnnots) <- pPatternGuard
  maybeDecl <- optional $ do
    _ <- symbol "->"
    exprToTypeMeta <$> term
  maybeExpr2 <- optional $ do
    _ <- symbol "=>" <|> symbol "="
    optional $ try pExprWithPostCond
  maybeDef <- optional $ do
    _ <- symbol "?"
    try pExpr

  let arrMeta = fromMaybe emptyMetaN maybeDecl
  (i', o') <- return $ case (expr1, maybeExpr2) of
    (i, Just (Just o)) -> (Just (GuardExpr i guard), Just o)
    (i, Just Nothing) -> (Just (GuardExpr i guard), Just (GuardExpr (rawVal nestedDeclaration) Nothing))
    (i, Nothing) -> (Just (GuardExpr i guard), Nothing) -- If only one expression, always make it as an input and later desugar to proper place

  return $ RawObjArr i' basis Nothing guardAnnots arrMeta o' maybeDef

data TermSuffix
  = ArgsSuffix ParseMeta [PObjArr]
  | VarsSuffix ParseMeta [(PExpr, ParseMeta)]
  | ContextSuffix ParseMeta [(ArgName, ParseMeta)]
  | AliasSuffix ParseMeta TypeName
  deriving (Show)

pArgSuffix :: Parser PObjArr
pArgSuffix = pArrowFull ArgObj

pArgsSuffix :: Parser TermSuffix
pArgsSuffix = do
  pos1 <- getSourcePos
  args <- parens $ sepBy1 pArgSuffix (symbol ",")
  pos2 <- getSourcePos
  return $ ArgsSuffix (emptyMeta pos1 pos2) args

pVarSuffix :: Parser (PExpr, ParseMeta)
pVarSuffix = do
  pos1 <- getSourcePos
  -- TODO: Should support multiple class identifiers such as <Eq Ord $T>
  var <- term
  maybeTp <- optional $ do
    _ <- symbol ":"
    ttypeidentifier <|> tvar -- Either type (<Eq $T>) or var (<$T>)
  pos2 <- getSourcePos
  let tp = fromMaybeTypeName maybeTp
  return (var, Meta tp (Just (pos1, pos2, "")) emptyMetaDat)

pVarsSuffix :: Parser TermSuffix
pVarsSuffix = do
  pos1 <- getSourcePos
  vars <- squareBraces $ sepBy1 pVarSuffix (symbol ",")
  pos2 <- getSourcePos
  return $ VarsSuffix (emptyMeta pos1 pos2) vars

pContextElSuffix :: Parser (ArgName, ParseMeta)
pContextElSuffix = do
  pos1 <- getSourcePos
  arg <- identifier
  _ <- symbol ":"
  tp <- tidentifier
  pos2 <- getSourcePos
  return (arg, Meta (singletonType (partialVal (PRelativeName tp))) (Just (pos1, pos2, "")) emptyMetaDat)

pContextSuffix :: Parser TermSuffix
pContextSuffix = do
  pos1 <- getSourcePos
  ctxs <- curlyBraces $ sepBy pContextElSuffix (symbol ",")
  pos2 <- getSourcePos
  return $ ContextSuffix (emptyMeta pos1 pos2) ctxs

pAliasSuffix :: Parser TermSuffix
pAliasSuffix = do
  _ <- string "@"
  pos1 <- getSourcePos
  aliasName <- identifier <|> tidentifier -- TODO Alias can be full term instead
  pos2 <- getSourcePos
  return $ AliasSuffix (emptyMeta pos1 pos2) aliasName

pTermSuffix :: Parser TermSuffix
pTermSuffix = pArgsSuffix <|> pVarsSuffix <|> pContextSuffix <|> pAliasSuffix

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

pMacroValue :: Parser PExpr
pMacroValue = do
  pos1 <- getSourcePos
  _ <- string "${"
  n <- identifier <|> tidentifier
  _ <- string "}"
  pos2 <- getSourcePos
  return $ RawMacroValue (emptyMeta pos1 pos2) n

applyTermSuffix :: PExpr -> TermSuffix -> PExpr
applyTermSuffix base (ArgsSuffix m args) = RawTupleApply m (labelPosM "arg" $ getExprMeta base, base) args
applyTermSuffix base (VarsSuffix m vars) = RawVarsApply m base vars
applyTermSuffix base (ContextSuffix m args) = RawContextApply m (labelPosM "ctx" $ getExprMeta base, base) args
applyTermSuffix base (AliasSuffix m n) = RawAliasExpr base (RawValue m n)

term :: Parser PExpr
term = do
  base <- parenExpr
       <|> pStringLiteral
       <|> pInt
       <|> pList
       <|> pMacroValue
       <|> pValue
  suffixes <- many pTermSuffix
  _ <- sc
  return $ foldl applyTermSuffix base suffixes

pExpr :: Parser PExpr
pExpr = makeExprParser term ops

pPatternGuard :: Parser (Maybe PExpr, [PCompAnnot])
pPatternGuard = do
  cond <- optional $ do
    _ <- symbol "if"
    pExpr
  els <- optional $ symbol "else"
  let els' = [rawVal elseAnnot | isJust els]
  return (cond, els')

pWithPostCond :: Parser a -> Parser (a, Maybe PExpr)
pWithPostCond pE = do
  e <- pE
  cond <- optional $ do
    _ <- symbol "where"
    pExpr
  return (e, cond)

pTermWithPostCond :: Parser PGuardExpr
pTermWithPostCond = do
  (e, g) <- pWithPostCond term
  return $ GuardExpr e g

pExprWithPostCond :: Parser PGuardExpr
pExprWithPostCond = do
  (e, g) <- pWithPostCond pExpr
  return $ GuardExpr e g
