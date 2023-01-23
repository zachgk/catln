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
import           Text.Printf

data ExprParseMode
  = ParseInputExpr -- ^ Parses a LHS or input expression
  | ParseOutputExpr -- ^ Parses a RHS or output expression
  | ParseTypeExpr -- ^ Parses an object in a multi type (class) definition
  deriving (Eq, Show)

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

parenExpr :: ExprParseMode -> Parser PExpr
parenExpr exprMode = do
  e <- parens (pExpr exprMode)
  return $ RawParen e

pArrowFull :: Maybe ExprParseMode -> Parser (PExpr, PGuard, Maybe ParseMeta, Maybe (Maybe PExpr))
pArrowFull exprMode = do
  expr1 <- pExpr (fromMaybe ParseInputExpr exprMode)
  guard <- pPatternGuard
  maybeDecl <- optional $ do
    _ <- symbol "->"
    exprToTypeMeta <$> term ParseInputExpr
  maybeExpr2 <- optional $ do
    _ <- symbol "=>" <|> symbol "="
    optional $ try $ pExpr (fromMaybe ParseOutputExpr exprMode)
  return (expr1, guard, maybeDecl, maybeExpr2)

data TermSuffix
  = ArgsSuffix ParseMeta [PTupleArg]
  | VarsSuffix ParseMeta [(TypeVarName, ParseMeta)]
  | ContextSuffix ParseMeta [(ArgName, ParseMeta)]
  | AliasSuffix ParseMeta TypeName
  deriving (Show)

pArgSuffix :: ExprParseMode -> Parser PTupleArg
pArgSuffix exprMode = do
  (expr1, NoGuard, Nothing, maybeExpr2) <- pArrowFull (Just exprMode)

  let expr1' = case expr1 of
        RawTupleApply _ (_, RawValue _ "/operator:") [TupleArgIO _ _ (RawValue m n), TupleArgIO _ _ tp] -> RawValue (mWithType (exprToType tp) m) n
        RawTheExpr t -> desugarTheExpr t
        _ -> expr1

  case (exprMode, expr1', maybeExpr2) of
    (_, RawValue m argName, Just (Just expr)) -> return $ TupleArgIO m argName expr
    (ParseOutputExpr, _, Nothing) -> return $ TupleArgO (emptyMetaE "tuple" expr1) expr1
    (_, RawValue m argName, Nothing) -> return $ TupleArgI m argName
    _ -> fail $ printf "Unexpected argName: %s" (show expr1)

pArgsSuffix :: ExprParseMode -> Parser TermSuffix
pArgsSuffix exprMode = do
  pos1 <- getSourcePos
  args <- parens $ sepBy1 (pArgSuffix exprMode) (symbol ",")
  pos2 <- getSourcePos
  return $ ArgsSuffix (emptyMeta pos1 pos2) args

pVarSuffix :: Parser (TypeVarName, ParseMeta)
pVarSuffix = do
  pos1 <- getSourcePos
  -- TODO: Should support multiple class identifiers such as <Eq Ord $T>
  var <- tvar
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
  ctxs <- curlyBraces $ sepBy1 pContextElSuffix (symbol ",")
  pos2 <- getSourcePos
  return $ ContextSuffix (emptyMeta pos1 pos2) ctxs

pAliasSuffix :: Parser TermSuffix
pAliasSuffix = do
  _ <- string "@"
  pos1 <- getSourcePos
  aliasName <- identifier <|> tidentifier -- TODO Alias can be full term instead
  pos2 <- getSourcePos
  return $ AliasSuffix (emptyMeta pos1 pos2) aliasName

pTermSuffix :: ExprParseMode -> Parser TermSuffix
pTermSuffix exprMode = pArgsSuffix exprMode <|> pVarsSuffix <|> pContextSuffix <|> pAliasSuffix

pInt :: Parser PExpr
pInt = do
  pos1 <- getSourcePos
  i <- integer
  pos2 <- getSourcePos
  return $ RawCExpr (emptyMeta pos1 pos2) (CInt i)

pList :: ExprParseMode -> Parser PExpr
pList exprMode = do
  pos1 <- getSourcePos
  _ <- string "["
  lst <- sepBy (pExpr exprMode) (symbol ",")
  _ <- string "]"
  pos2 <- getSourcePos
  return $ RawList (emptyMeta pos1 pos2) lst

applyTermSuffix :: PExpr -> TermSuffix -> PExpr
applyTermSuffix base (ArgsSuffix m args) = RawTupleApply m (labelPosM "arg" $ getExprMeta base, base) args
applyTermSuffix base (VarsSuffix m vars) = RawVarsApply m base vars
applyTermSuffix base (ContextSuffix m args) = RawContextApply m (labelPosM "ctx" $ getExprMeta base, base) args
applyTermSuffix base (AliasSuffix m n) = RawAliasExpr base (RawValue m n)

term :: ExprParseMode -> Parser PExpr
term exprMode = do
  base <- parenExpr exprMode
       <|> pStringLiteral
       <|> pInt
       <|> pList exprMode
       <|> pValue
  suffixes <- many (pTermSuffix exprMode)
  _ <- sc
  return $ foldl applyTermSuffix base suffixes

pExpr :: ExprParseMode -> Parser PExpr
pExpr exprMode = makeExprParser (term exprMode) ops

-- Pattern

pIfGuard :: Parser PGuard
pIfGuard = do
  _ <- symbol "if"
  IfGuard <$> pExpr ParseInputExpr

pElseGuard :: Parser PGuard
pElseGuard = do
  _ <- symbol "else"
  return ElseGuard

pPatternGuard :: Parser PGuard
pPatternGuard = fromMaybe NoGuard <$> optional (pIfGuard
                                              <|> pElseGuard
                                            )
