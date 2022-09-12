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
import           Text.Printf

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
    Left n  -> return $ RawValue (PreTyped tp (Just (pos1, pos2, ""))) n
    Right h -> return $ RawHoleExpr (PreTyped tp (Just (pos1, pos2, ""))) h

pStringLiteral :: Parser PExpr
pStringLiteral = do
  pos1 <- getSourcePos
  s <- char '\"' *> manyTill L.charLiteral (char '\"')
  pos2 <- getSourcePos
  return $ RawCExpr (emptyMeta pos1 pos2) (CStr s)

parenExpr :: Bool -> Parser PExpr
parenExpr outputExpr = do
  e <- parens (pExpr outputExpr)
  return $ RawParen e

pIfThenElse :: Bool -> Parser PExpr
pIfThenElse outputExpr = do
  pos1 <- getSourcePos
  _ <- symbol "if"
  condExpr <- pExpr outputExpr
  _ <- symbol "then"
  thenExpr <- pExpr outputExpr
  _ <- symbol "else"
  elseExpr <- pExpr outputExpr
  pos2 <- getSourcePos
  return $ RawIfThenElse (emptyMeta pos1 pos2) condExpr thenExpr elseExpr

pMatchCaseHelper :: Bool -> String -> Parser (PExpr, [(PPattern, PExpr)])
pMatchCaseHelper outputExpr keyword = L.indentBlock scn p
  where
    pack expr matchItems = return (expr, matchItems)
    pItem = do
      patt <- pPattern MatchObj
      _ <- symbol "=>"
      expr <- pExpr outputExpr
      return (patt, expr)
    p = do
      _ <- symbol keyword
      expr <- pExpr outputExpr
      _ <- symbol "of"
      return $ L.IndentSome Nothing (pack expr) pItem

pCase :: Bool -> Parser PExpr
pCase outputExpr = do
  pos1 <- getSourcePos
  (expr, matchItems) <- pMatchCaseHelper outputExpr "case"
  pos2 <- getSourcePos
  return $ RawCase (emptyMeta pos1 pos2) expr matchItems

pMatch :: Bool -> Parser PExpr
pMatch outputExpr = do
  pos1 <- getSourcePos
  (expr, matchItems) <- pMatchCaseHelper outputExpr "match"
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

pArgSuffix :: Bool -> Parser PTupleArg
pArgSuffix outputExpr = do
  pos1 <- getSourcePos
  maybeArgNameType <- optional . try $ do
    n <- pTypesAndVal
    _ <- symbol "="
    return n
  expr <- pExpr outputExpr
  pos2 <- getSourcePos
  case maybeArgNameType of
    Just (_, Right _) -> fail "Unexpected hole in arg name"
    Just (tp, Left argName) -> return $ TupleArgIO (PreTyped tp (Just (pos1, pos2, ""))) argName expr
    Nothing      -> if outputExpr
      then return $ TupleArgO (emptyMeta pos1 pos2) expr
      else case expr of
        RawValue m n -> return $ TupleArgI (PreTyped (getMetaType m) (Just (pos1, pos2, ""))) n
        _ -> fail $ printf "Unexpected parsed argName: %s" (show expr)

pArgsSuffix :: Bool -> Parser TermSuffix
pArgsSuffix outputExpr = do
  pos1 <- getSourcePos
  args <- parens $ sepBy1 (pArgSuffix outputExpr) (symbol ",")
  pos2 <- getSourcePos
  return $ ArgsSuffix (emptyMeta pos1 pos2) args

pVarSuffix :: Parser (TypeVarName, ParseMeta)
pVarSuffix = do
  pos1 <- getSourcePos
  -- TODO: Should support multiple class identifiers such as <Eq Ord $T>
  maybeClass <- optional ttypeidentifier
  var <- tvar
  pos2 <- getSourcePos
  let tp = fromMaybeTypeName maybeClass
  return (var, PreTyped tp (Just (pos1, pos2, "")))

pVarsSuffix :: Parser TermSuffix
pVarsSuffix = do
  pos1 <- getSourcePos
  vars <- angleBraces $ sepBy1 pVarSuffix (symbol ",")
  pos2 <- getSourcePos
  return $ VarsSuffix (emptyMeta pos1 pos2) vars

pContextElSuffix :: Parser (ArgName, ParseMeta)
pContextElSuffix = do
  pos1 <- getSourcePos
  tp <- tidentifier
  arg <- identifier
  pos2 <- getSourcePos
  return (arg, PreTyped (singletonType (PartialType (PRelativeName tp) H.empty H.empty H.empty PtArgExact)) (Just (pos1, pos2, "")))

pContextSuffix :: Parser TermSuffix
pContextSuffix = do
  pos1 <- getSourcePos
  ctxs <- curlyBraces $ sepBy1 pContextElSuffix (symbol ",")
  pos2 <- getSourcePos
  return $ ContextSuffix (emptyMeta pos1 pos2) ctxs

pTermSuffix :: Bool -> Parser TermSuffix
pTermSuffix outputExpr = pMethod <|> pArgsSuffix outputExpr <|> pVarsSuffix <|> pContextSuffix
pInt :: Parser PExpr
pInt = do
  pos1 <- getSourcePos
  i <- integer
  pos2 <- getSourcePos
  return $ RawCExpr (emptyMeta pos1 pos2) (CInt i)

pList :: Bool -> Parser PExpr
pList outputExpr = do
  pos1 <- getSourcePos
  _ <- string "["
  lst <- sepBy (pExpr outputExpr) (symbol ",")
  _ <- string "]"
  pos2 <- getSourcePos
  return $ RawList (emptyMeta pos1 pos2) lst

applyTermSuffix :: PExpr -> TermSuffix -> PExpr
applyTermSuffix base (MethodSuffix m n) = RawMethod base (RawValue m n)
applyTermSuffix base (ArgsSuffix m args) = RawTupleApply m (labelPosM "arg" $ getExprMeta base, base) args
applyTermSuffix base (VarsSuffix m vars) = RawVarsApply m base vars
applyTermSuffix base (ContextSuffix m args) = RawContextApply m (labelPosM "ctx" $ getExprMeta base, base) args

term :: Bool -> Parser PExpr
term outputExpr = do
  base <- (if outputExpr then pIfThenElse outputExpr
        <|> pMatch outputExpr
        <|> pCase outputExpr
        <|> parenExpr outputExpr
        else parenExpr outputExpr)
       <|> pStringLiteral
       <|> pInt
       <|> pList outputExpr
       <|> pArgValue
  suffixes <- many (pTermSuffix outputExpr)
  _ <- sc
  return $ foldl applyTermSuffix base suffixes

pExpr :: Bool -> Parser PExpr
pExpr outputExpr = makeExprParser (term outputExpr) ops

-- Pattern

pIfGuard :: Parser PGuard
pIfGuard = do
  _ <- symbol "if"
  IfGuard <$> pExpr False

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
  e <- term False
  Pattern (rawExprToObj basis Nothing e) <$> pPatternGuard

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
  return (argName, singletonType (PartialType (PRelativeName tp) H.empty H.empty H.empty PtArgExact))

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
  return (PartialType (PRelativeName name) vars H.empty args PtArgExact)

pSingleType :: Parser Type
pSingleType = pTypeVar
              <|> singletonType <$> pLeafType

pType :: Parser Type
pType = pTypeVar
        <|> UnionType . joinUnionType <$> sepBy1 pLeafType (symbol "|")
