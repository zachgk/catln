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
mkOp1 opChars x = applyRawArgs (RawValue emptyMetaN op) [(Just $ partialKey operatorArgUnary, x)]
  where op = operatorPrefix ++ opChars

mkOp2 :: String -> PExpr -> PExpr -> PExpr
mkOp2 opChars x y = applyRawArgs (RawValue emptyMetaN op) [(Just $ partialKey operatorArgL, x), (Just $ partialKey operatorArgR, y)]
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
    [ InfixL (mkOp2 "::" <$ symbol "::")
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
    [ InfixL (mkOp2 "&&" <$ symbol "&&")
    , InfixL (mkOp2 "||" <$ symbol "||")
    , InfixL (mkOp2 "^" <$ symbol "^")
    ]
  ]

pValue :: Parser PExpr
pValue = do
  pos1 <- getSourcePos
  usingTheExpr <- optional $ string ":"
  name <- identifier <|> tidentifier <|> pAnnotIdentifier <|> tvar <|> pHole
  spread <- optional $ string ".."
  pos2 <- getSourcePos
  let m = emptyMeta pos1 pos2

  -- check if name is a hole
  return $ case name of
        "_"         -> RawHoleExpr m $ HoleActive Nothing
        ('_':_)     -> RawHoleExpr m $ HoleActive (Just name)
        "undefined" -> RawHoleExpr m HoleUndefined
        "todefine"  -> RawHoleExpr m HoleTodefine
        _           -> case (usingTheExpr, spread) of
          (Just{}, Nothing)  -> RawTheExpr (RawValue m name)
          (Nothing, Just{}) -> RawSpread $ RawValue (mWithType (relTypeVal name) m) name
          (Nothing, Nothing) -> RawValue (mWithType (relTypeVal name) m) name
          (Just{}, Just{}) -> undefined

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
    _ <- symbol "->" <|> symbol ":"
    exprToTypeMeta <$> term
  maybeExpr2 <- optional $ do
    _ <- symbol "=>" <|> symbol "="
    optional $ try pExprWithPostCond
  maybeDef <- optional $ do
    _ <- symbol "?"
    try pExpr

  let arrMeta = fromMaybe emptyMetaN maybeDecl
  (i', o') <- return $ case (maybeDecl, expr1, maybeExpr2) of
    -- Input, equals, and in expression
    (_, i, Just (Just o)) -> (Just (GuardExpr i guard), Just (Just o, arrMeta))

    -- Input, equals, but no out expression
    (_, i, Just Nothing) -> (Just (GuardExpr i guard), Just (Just (GuardExpr (rawVal nestedDeclaration) Nothing), arrMeta))

    -- Input, no equals, but declaration
    (Just am, i, Nothing) -> (Just (GuardExpr i guard), Just (Nothing, am)) -- If only one expression, always make it as an input and later desugar to proper place

    -- Input, no equals nor declaration
    (Nothing, i, Nothing) -> (Just (GuardExpr i guard), Nothing) -- If only one expression, always make it as an input and later desugar to proper place

  return $ RawObjArr i' basis Nothing guardAnnots o' maybeDef

data TermSuffix
  = ArgsSuffix ParseMeta [PObjArr]
  | VarsSuffix ParseMeta [(PExpr, ParseMeta)]
  | ContextSuffix ParseMeta [(ArgName, ParseMeta)]
  | AliasSuffix ParseMeta TypeName
  | TypePropSuffix ParseMeta (TypeProperty RawExpr ParseMetaDat)
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
  return (partialKey arg, Meta (relTypeVal tp) (Just (pos1, pos2, "")) emptyMetaDat)

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

pTypePropRelSuffix :: Parser TermSuffix
pTypePropRelSuffix = do
  _ <- string "__"
  pos1 <- getSourcePos
  p <- identifier
  v <- parens pExpr
  pos2 <- getSourcePos
  return $ TypePropSuffix (emptyMeta pos1 pos2) $ TypePropRel p v

pTypePropProjSuffix :: Parser TermSuffix
pTypePropProjSuffix = do
  _ <- string "_"
  pos1 <- getSourcePos
  p <- identifier
  v <- optional $ parens pExpr
  pos2 <- getSourcePos
  let v' = fromMaybe (RawValue (mWithType trueType (emptyMeta pos1 pos2)) truePrim) v
  return $ TypePropSuffix (emptyMeta pos1 pos2) $ TypePropProj p v'


pTermSuffix :: Parser TermSuffix
pTermSuffix = pArgsSuffix <|> pVarsSuffix <|> pContextSuffix <|> pAliasSuffix <|> pTypePropRelSuffix <|> pTypePropProjSuffix

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
applyTermSuffix base (TypePropSuffix m p) = RawTypeProp m base p

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
    _ <- symbol "|"
    pExpr
  els <- optional $ symbol "else"
  let els' = [rawVal elseAnnot | isJust els]
  return (cond, els')

pWithPostCond :: Parser a -> Parser (a, Maybe PExpr)
pWithPostCond pE = do
  e <- pE
  cond <- optional $ do
    _ <- symbol "|"
    pExpr
  return (e, cond)

pExprWithPostCond :: Parser PGuardExpr
pExprWithPostCond = do
  (e, g) <- pWithPostCond pExpr
  return $ GuardExpr e g
