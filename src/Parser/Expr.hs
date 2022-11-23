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
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf

data ExprParseMode
  = ParseInputExpr -- ^ Parses a LHS or input expression
  | ParseOutputExpr -- ^ Parses a RHS or output expression
  | ParseTypeExpr -- ^ Parses an object in a multi type (class) definition
  deriving (Eq, Show)

mkOp1 :: String -> PExpr -> PExpr
mkOp1 opChars x = applyRawArgs (RawValue emptyMetaN op) [(Just "a", x)]
  where op = "/operator" ++ opChars

mkOp2 :: String -> PExpr -> PExpr -> PExpr
mkOp2 opChars x y = applyRawArgs (RawValue emptyMetaN op) [(Just "l", x), (Just "r", y)]
  where op = "/operator" ++ opChars

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

pTypesAndVal :: ExprParseMode -> Parser (Type, Either Name Hole)
pTypesAndVal exprMode = do
  let anyId = identifier <|> tidentifier <|> pAnnotIdentifier <|> tvar <|> pHole
  typesAndVal <- some $ do
    typeName <- anyId
    vars <- optional . try $ do
      vs <- pVarsSuffix exprMode
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

pArgValue :: ExprParseMode -> Parser PExpr
pArgValue exprMode = do
  pos1 <- getSourcePos
  (tp, argValName) <- pTypesAndVal exprMode
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

pIfThenElse :: ExprParseMode -> Parser PExpr
pIfThenElse exprMode = do
  pos1 <- getSourcePos
  _ <- symbol "if"
  condExpr <- pExpr exprMode
  _ <- symbol "then"
  thenExpr <- pExpr exprMode
  _ <- symbol "else"
  elseExpr <- pExpr exprMode
  pos2 <- getSourcePos
  return $ RawIfThenElse (emptyMeta pos1 pos2) condExpr thenExpr elseExpr

pMatchCaseHelper :: ExprParseMode -> String -> Parser (PExpr, [(PPattern, PExpr)])
pMatchCaseHelper exprMode keyword = L.indentBlock scn p
  where
    pack expr matchItems = return (expr, matchItems)
    pItem = do
      patt <- pPattern MatchObj
      _ <- symbol "=>"
      expr <- pExpr exprMode
      return (patt, expr)
    p = do
      _ <- symbol keyword
      expr <- pExpr exprMode
      _ <- symbol "of"
      return $ L.IndentSome Nothing (pack expr) pItem

pCase :: ExprParseMode -> Parser PExpr
pCase exprMode = do
  pos1 <- getSourcePos
  (expr, matchItems) <- pMatchCaseHelper exprMode "case"
  pos2 <- getSourcePos
  return $ RawCase (emptyMeta pos1 pos2) expr matchItems

pMatch :: ExprParseMode -> Parser PExpr
pMatch exprMode = do
  pos1 <- getSourcePos
  (expr, matchItems) <- pMatchCaseHelper exprMode "match"
  pos2 <- getSourcePos
  return $ RawMatch (emptyMeta pos1 pos2) expr matchItems

data TermSuffix
  = MethodSuffix ParseMeta TypeName
  | ArgsSuffix ParseMeta [PTupleArg]
  | VarsSuffix ParseMeta [(TypeVarName, ParseMeta)]
  | ContextSuffix ParseMeta [(ArgName, ParseMeta)]
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
    n <- pTypesAndVal exprMode
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

pVarSuffix :: ExprParseMode -> Parser (TypeVarName, ParseMeta)
pVarSuffix exprMode = do
  pos1 <- getSourcePos
  -- TODO: Should support multiple class identifiers such as <Eq Ord $T>
  maybeClass <- optional ttypeidentifier
  var <- tvar
  pos2 <- getSourcePos
  let tp = case (exprMode, maybeClass) of
        (ParseTypeExpr, Nothing) -> TypeVar (TVVar var) -- For multi types, Just<$T> would be Just<$T=$T>
        _                        -> fromMaybeTypeName maybeClass
  return (var, Meta tp (Just (pos1, pos2, "")) emptyMetaDat)

pVarsSuffix :: ExprParseMode -> Parser TermSuffix
pVarsSuffix exprMode = do
  pos1 <- getSourcePos
  vars <- angleBraces $ sepBy1 (pVarSuffix exprMode) (symbol ",")
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

pTermSuffix :: ExprParseMode -> Parser TermSuffix
pTermSuffix exprMode = pMethod <|> pArgsSuffix exprMode <|> pVarsSuffix exprMode <|> pContextSuffix
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

term :: ExprParseMode -> Parser PExpr
term exprMode = do
  base <- (if exprMode == ParseOutputExpr then pIfThenElse exprMode
        <|> pMatch exprMode
        <|> pCase exprMode
        <|> parenExpr exprMode
        else parenExpr exprMode)
       <|> pStringLiteral
       <|> pInt
       <|> pList exprMode
       <|> pArgValue exprMode
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
  maybeVars <- optional $ angleBraces $ sepBy1 pLeafVar (symbol ",")
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
