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
import qualified Data.HashMap.Strict            as H
import           Data.Maybe
import           Text.Megaparsec                hiding (pos1)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Constants
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
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

pTypesAndVal :: Parser (Type, Either Name Hole)
pTypesAndVal = do
  let anyId = identifier <|> tidentifier <|> pAnnotIdentifier <|> tvar <|> pHole
  typesAndVal <- some $ do
    typeName <- anyId
    vars <- optional . try $ do
      vs <- pVarsSuffix
      -- Need to ensure that vars are not matched if it is the val name and not a type for the val
      _ <- lookAhead anyId
      return vs
    return (typeName, vars)
  (tp, argValName) <- case typesAndVal of
        [] -> fail "Nothing found when parsing an argument or value"
        [(v, Nothing)] -> return (TopType, v)
        [(typeName, maybeTypeVars), (v, Nothing)] -> do
          let baseType = fromMaybeTypeName $ Just typeName
          case maybeTypeVars of
            Nothing -> return (baseType, v)
            Just suffix -> do
              let (VarsSuffix _ vars) = suffix
              let (UnionType basePartials) = baseType
              let [partial] = splitUnionType basePartials
              let partial' = partial{ptVars = getMetaType <$> H.fromList vars}
              return (singletonType partial', v)
        _ -> fail $ printf "Multiple types is not yet supported with %s" (show typesAndVal)

  -- check if name is a hole
  let argValName' = case argValName of
        "_"         -> Right $ HoleActive Nothing
        ('_':_)     -> Right $ HoleActive (Just argValName)
        "undefined" -> Right HoleUndefined
        "todefine"  -> Right HoleTodefine
        _           -> Left argValName

  return (tp, argValName')

pArgValue :: Parser PExpr
pArgValue = do
  pos1 <- getSourcePos
  (tp, argValName) <- pTypesAndVal
  pos2 <- getSourcePos
  case argValName of
    Left n  -> return $ RawValue (Meta tp (Just (pos1, pos2, "")) emptyMetaDat) n
    Right h -> return $ RawHoleExpr (Meta tp (Just (pos1, pos2, "")) emptyMetaDat) h

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

data TermSuffix
  = MethodSuffix ParseMeta TypeName
  | ArgsSuffix ParseMeta [PTupleArg]
  | VarsSuffix ParseMeta [(TypeVarName, ParseMeta)]
  | ContextSuffix ParseMeta [(ArgName, ParseMeta)]
  | AliasSuffix ParseMeta TypeName
  deriving (Show)

pMethod :: Parser TermSuffix
pMethod = do
  _ <- string "."
  pos1 <- getSourcePos
  methodName <- identifier <|> tidentifier
  pos2 <- getSourcePos
  return $ MethodSuffix (emptyMeta pos1 pos2) methodName

pArgSuffix :: ExprParseMode -> Parser PTupleArg
pArgSuffix exprMode = do
  pos1 <- getSourcePos
  maybeArgNameType <- optional . try $ do
    n <- pTypesAndVal
    _ <- symbol "="
    return n
  expr <- pExpr exprMode
  pos2 <- getSourcePos
  case maybeArgNameType of
    Just (_, Right _) -> fail "Unexpected hole in arg name"
    Just (tp, Left argName) -> return $ TupleArgIO (Meta tp (Just (pos1, pos2, "")) emptyMetaDat) argName expr
    Nothing      -> if exprMode == ParseOutputExpr
      then return $ TupleArgO (emptyMeta pos1 pos2) expr
      else case expr of
        RawValue m n -> return $ TupleArgI (Meta (getMetaType m) (Just (pos1, pos2, "")) emptyMetaDat) n
        _ -> fail $ printf "Unexpected parsed argName: %s" (show expr)

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
  v1 <- ttypeidentifier <|> tvar -- Either type (<Eq $T>) or var (<$T>)
  v2 <- optional tvar -- var only with type
  pos2 <- getSourcePos
  let (var', tp') = case (v1, v2) of
        (var, Nothing) -> (var, TopType)
        (tp, Just var) -> (var, fromMaybeTypeName (Just tp))
  return (var', Meta tp' (Just (pos1, pos2, "")) emptyMetaDat)

pVarsSuffix :: Parser TermSuffix
pVarsSuffix = do
  pos1 <- getSourcePos
  vars <- squareBraces $ sepBy1 pVarSuffix (symbol ",")
  pos2 <- getSourcePos
  return $ VarsSuffix (emptyMeta pos1 pos2) vars

pContextElSuffix :: Parser (ArgName, ParseMeta)
pContextElSuffix = do
  pos1 <- getSourcePos
  tp <- tidentifier
  arg <- identifier
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
  aliasName <- identifier <|> tidentifier -- TODO Alais can be full term instead
  pos2 <- getSourcePos
  return $ AliasSuffix (emptyMeta pos1 pos2) aliasName

pTermSuffix :: ExprParseMode -> Parser TermSuffix
pTermSuffix exprMode = pMethod <|> pArgsSuffix exprMode <|> pVarsSuffix <|> pContextSuffix <|> pAliasSuffix
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
applyTermSuffix base (MethodSuffix m n) = RawMethod base (RawValue m n)
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
       <|> pArgValue
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

pPattern :: ObjectBasis -> Parser PPattern
pPattern basis = do
  e <- term ParseInputExpr
  Pattern (ExprObject basis Nothing e) <$> pPatternGuard

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
  return (argName, singletonType (partialVal (PRelativeName tp)))

pTypeVar :: Parser Type
pTypeVar = TypeVar . TVVar <$> tvar

-- TODO: Parse type properties
pLeafType :: Parser PartialType
pLeafType = do
  name <- tidentifier
  maybeVars <- optional $ squareBraces $ sepBy1 pLeafVar (symbol ",")
  maybeArgs <- optional $ parens (sepBy1 pTypeArg (symbol ","))
  let vars = maybe H.empty H.fromList maybeVars
  let args = maybe H.empty H.fromList maybeArgs
  return ((partialVal (PRelativeName name)){ptVars=vars, ptArgs=args})

pSingleType :: Parser Type
pSingleType = pTypeVar
              <|> singletonType <$> pLeafType

pType :: Parser Type
pType = pTypeVar
        <|> UnionType . joinUnionType <$> sepBy1 pLeafType (symbol "|")
