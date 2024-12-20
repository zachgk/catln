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

import           CtConstants
import           GHC.Data.Maybe                 (rightToMaybe)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Builder
import           Syntax.Ct.Parser.Lexer
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Text.Printf

mkOp1 :: String -> PExpr -> PExpr
mkOp1 opChars x = applyRawArgs (RawValue emptyMetaN op) [(Just $ partialKey operatorArgUnary, x)]
  where op = operatorName opChars

mkOp2 :: String -> PExpr -> PExpr -> PExpr
mkOp2 opChars x y = applyRawArgs (RawValue emptyMetaN op) [(Just $ partialKey operatorArgL, x), (Just $ partialKey operatorArgR, y)]
  where op = operatorName opChars

symbolExcludes :: String -> [String] -> Parser String
symbolExcludes goal excludes = do
  checkExcludes <- mapM (optional . try . lookAhead . symbol) excludes
  case catMaybes checkExcludes of
    []     -> symbol goal -- Zero excludes, so parse goal
    (ex:_) -> fail $ printf "%s should not be matched for %s operator" ex goal

ops :: [[Operator Parser PExpr]]
ops = [
    [ Prefix (mkOp1 "-"  <$ symbolExcludes "-" ["->"])
    , Prefix (mkOp1 "~" <$ symbol "~")
    ],
    [ InfixL (RawMethod emptyMetaN <$ symbol ".")
    ],
    [ InfixL (mkOp2 "::" <$ symbol "::")
    , InfixL (mkOp2 "~::" <$ symbol "~::")
    ],
    [ InfixL (mkOp2 "*" <$ symbol "*")
    , InfixL (mkOp2 "//" <$ symbol "//")
    ],
    [ InfixL (mkOp2 "++" <$ symbol "++")
    , InfixL (mkOp2 "+" <$ symbol "+")
    , InfixL (mkOp2 "-" <$ symbolExcludes "-" ["->"])
    ],
    [ InfixL (mkOp2 "<=" <$ symbol "<=")
    , InfixL (mkOp2 ">=" <$ symbol ">=")
    , InfixL (mkOp2 "<" <$ symbolExcludes "<" ["<-"])
    , InfixL (mkOp2 ">" <$ symbol ">")
    , InfixL (mkOp2 "==" <$ symbol "==")
    , InfixL (mkOp2 "!=" <$ symbol "!=")
    ],
    [ InfixL (mkOp2 "&&" <$ symbol "&&")
    , InfixL (mkOp2 "||" <$ symbol "||")
    , InfixL (mkOp2 "^" <$ symbol "^")
    ],
    [ InfixL (mkOp2 "?->" <$ symbol "?->")
    , InfixL (mkOp2 ":=" <$ symbol ":=")
    ],
    [ InfixL (RawWhere emptyMetaN <$ symbol "|") ]
  ]

pValue :: Parser PExpr
pValue = do
  pos1 <- getSourcePos
  usingTheExpr <- optional $ string ":"
  name <- identifier <|> pHole
  suffix <- optional ((Left <$> string "..") <|> (Right <$> pStringLiteral))
  pos2 <- getSourcePos
  let m = emptyMeta pos1 pos2

  -- check if name is a hole
  return $ case name of
        "_"         -> RawHoleExpr m $ HoleActive Nothing
        ('_':_)     -> RawHoleExpr m $ HoleActive (Just name)
        "undefined" -> RawHoleExpr m HoleUndefined
        "todefine"  -> RawHoleExpr m HoleTodefine
        _           -> case (usingTheExpr, suffix) of
          (Just{}, Nothing)  -> RawTheExpr (RawValue m name)
          (Nothing, Just Left{}) -> RawTupleApply (emptyMetaM m) (emptyMetaM m, RawValue (mWithType (relTypeVal name) m) name) [(True, rawInObjArr True (RawHoleExpr (emptyMetaM m) (HoleActive Nothing)))]
          (Nothing, Just (Right s)) -> RawFmtStrExpr m name s
          (Nothing, Nothing) -> RawValue (mWithType (relTypeVal name) m) name
          _ -> undefined

pStringLiteral :: Parser String
pStringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

pStringConst :: Parser PExpr
pStringConst = do
  pos1 <- getSourcePos
  s <- pStringLiteral
  pos2 <- getSourcePos
  return $ RawCExpr (emptyMeta pos1 pos2) (CStr s)

parenExpr ::  Parser PExpr
parenExpr = do
  pos1 <- getSourcePos
  res <- parens $ do
    es <- sepBy pArgSuffix (symbol ",")
    trailingComma <- optional $ symbol ","
    return (es, trailingComma)
  pos2 <- getSourcePos
  return $ case res of
    ([(False, RawObjArr{roaObj=Just e', roaArr=Nothing})], Nothing) -> RawParen e' -- Paren
    (args, _) -> do -- Tuple
      let base = rawAnon
      RawTupleApply (emptyMeta pos1 pos2) (emptyMetaE base, base) args

pEndOfLine :: Parser ()
pEndOfLine = do
  _ <- many $ string " "
  _ <- lookAhead newline
  return ()

pArrowFull :: ObjectBasis -> Parser (Either PStatement PObjArr)
pArrowFull basis = do
  expr1 <- pExpr
  guardAnnots <- pPatternGuard
  arrM <- optional $ do
    _ <- symbol "->" <|> symbol ":"
    term
  maybeExpr2 <- optional $ do
    s <- (Left <$> symbol "=>") <|> (Left <$> symbol "=") <|> (Right <$> symbol "<-")
    afterEquals <- (Left <$> pEndOfLine) <|> (Right <$> pExpr)
    return (s, rightToMaybe afterEquals)
  maybeDef <- optional $ do
    _ <- symbol "?"
    try pExpr

  (i', o') <- return $ case (arrM, expr1, maybeExpr2) of
    -- Input, equals, and in expression
    (_, i, Just (_, Just o)) -> (Just i, Just (Just o, arrM))

    -- Input, equals, but no out expression
    (_, i, Just (_, Nothing)) -> (Just i, Just (Just (rawVal nestedDeclaration), arrM))

    -- Input, no equals, but declaration
    (Just _, i, Nothing) -> (Just i, Just (Nothing, arrM)) -- If only one expression, always make it as an input and later desugar to proper place

    -- Input, no equals nor declaration
    (Nothing, i, Nothing) -> (Just i, Nothing) -- If only one expression, always make it as an input and later desugar to proper place

  let oa = RawObjArr i' basis Nothing guardAnnots o' maybeDef
  return $ case maybeExpr2 of
    Just (Right{}, _) -> Left $ RawBindStatement oa
    _                 -> Right oa

pArrowFullOA :: ObjectBasis -> Parser PObjArr
pArrowFullOA basis = do
  oa <- pArrowFull basis
  case oa of
    Right oa' -> return oa'
    Left _    -> fail "Found unexpected bind object arrow"

data TermSuffix
  = ArgsSuffix ParseMeta [(Bool, PObjArr)]
  | VarsSuffix ParseMeta [PObjArr]
  | ContextSuffix ParseMeta [PObjArr]
  | AliasSuffix ParseMeta TypeName
  | TypePropSuffix ParseMeta (TypeProperty RawExpr ParseMetaDat)
  deriving (Show)

pArgSuffix :: Parser (Bool, PObjArr)
pArgSuffix = do
  spread <- optional $ symbol ".."
  arg <- pArrowFullOA ArgObj
  return (isJust spread, arg)

pArgsSuffix :: Parser TermSuffix
pArgsSuffix = do
  pos1 <- getSourcePos
  args <- parens $ sepBy1 pArgSuffix (symbol ",")
  pos2 <- getSourcePos
  return $ ArgsSuffix (emptyMeta pos1 pos2) args

pVarsSuffix :: Parser TermSuffix
pVarsSuffix = do
  pos1 <- getSourcePos
  vars <- squareBraces $ sepBy1 (pArrowFullOA ArgObj) (symbol ",")
  pos2 <- getSourcePos
  return $ VarsSuffix (emptyMeta pos1 pos2) vars

pContextSuffix :: Parser TermSuffix
pContextSuffix = do
  pos1 <- getSourcePos
  ctxsRW <- curlyBraces $ do
    rw <- sepBy (pArrowFullOA ArgObj) (symbol ",")
    ctxsRO <- optional $ do
      _ <- symbol ";"
      curlyBraces $ sepBy (pArrowFullOA ArgObj) (symbol ",")
    ctxsCover <- optional $ do
      _ <- symbol ";"
      curlyBraces $ sepBy (pArrowFullOA ArgObj) (symbol ",")
    case (ctxsRO, ctxsCover) of
      (Nothing, Nothing) -> return rw
      _ -> fail "Context with read-only or covered components are not yet supported"
  pos2 <- getSourcePos
  return $ ContextSuffix (emptyMeta pos1 pos2) ctxsRW

pAliasSuffix :: Parser TermSuffix
pAliasSuffix = do
  _ <- string "@"
  pos1 <- getSourcePos
  aliasName <- identifier -- TODO Alias can be full term instead
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
  n <- identifier
  _ <- string "}"
  pos2 <- getSourcePos
  return $ RawMacroValue (emptyMeta pos1 pos2) n

pApply :: Parser PRawApply
pApply = do
  term1 <- RATermDeep <$> term
  termRest <- many $ do
    sep <- symbol ">" <|> symbol " "
    val <- term
    case sep of
      ">" -> return $ RATermChild val
      " " -> return $ RATermDeep val
      _   -> fail $ printf "Unexpected seperator " (show sep)
  return $ RawApply (term1 : termRest)

applyTermSuffix :: PExpr -> TermSuffix -> PExpr
applyTermSuffix base (ArgsSuffix m args) = RawTupleApply m (emptyMetaE base, base) args
applyTermSuffix base (VarsSuffix m vars) = RawVarsApply m base vars
applyTermSuffix base (ContextSuffix m args) = RawContextApply m (emptyMetaE base, base) args
applyTermSuffix base (AliasSuffix m n) = RawAliasExpr base (RawValue m n)
applyTermSuffix base (TypePropSuffix m p) = RawTypeProp m base p

term :: Parser PExpr
term = do
  e1 <- parenExpr
       <|> pStringConst
       <|> pInt
       <|> pList
       <|> pMacroValue
       <|> pValue
  suffixes <- many pTermSuffix
  _ <- sc
  let e2 = foldl applyTermSuffix e1 suffixes
  return e2

pExpr :: Parser PExpr
pExpr = makeExprParser term ops

pPatternGuard :: Parser [PCompAnnot]
pPatternGuard = do
  els <- optional $ symbol "else"
  let els' = [rawVal elseAnnot | isJust els]
  return els'
