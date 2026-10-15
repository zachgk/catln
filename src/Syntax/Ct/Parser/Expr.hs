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
mkOp1 opChars x = applyRawArgs (RawValue emptyMetaN (mkName op)) [(Just $ partialKey (mkName operatorArgUnary), x)]
  where op = operatorName opChars

mkOp2 :: String -> PExpr -> PExpr -> PExpr
mkOp2 opChars x y = applyRawArgs (RawValue emptyMetaN (mkName op)) [(Just $ partialKey (mkName operatorArgL), x), (Just $ partialKey (mkName operatorArgR), y)]
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
    [ InfixL (pMethodCall <$ symbol ".")
    ],
    [ InfixL (mkOp2 "::" <$ symbol "::")
    , InfixL (mkOp2 "~::" <$ symbol "~::")
    ],
    [ InfixL (mkOp2 "*" <$ symbol "*")
    , InfixL (mkOp2 "//" <$ symbol "//")
    , InfixL (mkOp2 "%" <$ symbol "%")
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

-- | A method call @base.method@, positioned from the start of its receiver to the end of the
-- method. It is the call written there: the desugared call takes this meta ('semiDesExpr' on
-- 'RawMethod'), and what reads where a call is written (the @callerPage@ macro completing
-- @io.lcm(input= ..)@ with the page it is on) reads it here.
pMethodCall :: PExpr -> PExpr -> PExpr
pMethodCall base method = RawMethod m base method
  where
    m = case (getMetaPos (getExprMeta base), getMetaPos (getExprMeta method)) of
      (Just (start, _), Just (_, end)) -> emptyMetaN{getMetaPos = Just (start, end)}
      (Nothing, pos@Just{})            -> emptyMetaN{getMetaPos = pos}
      (pos, Nothing)                   -> emptyMetaN{getMetaPos = pos}

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
        ('_':_)     -> RawHoleExpr m $ HoleActive (Just (mkName name))
        "undefined" -> RawHoleExpr m HoleUndefined
        "todefine"  -> RawHoleExpr m HoleTodefine
        _           -> case (usingTheExpr, suffix) of
          -- A bare type receiver @:X@ is read as the spread @X..@ by the desugarer once it is
          -- known to be bare ('semiDesExpr' on 'RawMethod'): the suffixes have not been parsed
          -- yet here, and @:Cons[$T](head, tail)@ is a destructuring receiver, not a spread.
          -- @:X..@ says the spread explicitly.
          (Just{}, Nothing)  -> RawTheExpr (RawValue m (mkName name))
          (Just{}, Just Left{}) -> RawTheExpr spread
          (Nothing, Just Left{}) -> spread
          (Nothing, Just (Right s)) -> RawFmtStrExpr m name s
          (Nothing, Nothing) -> RawValue (mWithType (relTypeVal (mkName name)) m) (mkName name)
          _ -> undefined -- a format string on a type receiver
          where
            spread = RawTupleApply (emptyMetaM m) (emptyMetaM m, RawValue (mWithType (relTypeVal (mkName name)) m) (mkName name)) [(True, rawInObjArr True (RawHoleExpr (emptyMetaM m) (HoleActive Nothing)))]

pStringLiteral :: Parser String
pStringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

pStringConst :: Parser PExpr
pStringConst = do
  pos1 <- getSourcePos
  s <- pStringLiteral
  pos2 <- getSourcePos
  return $ RawCExpr (emptyMeta pos1 pos2) (CStr s)

-- | A character literal: @'a'@, or an escape such as @'\n'@, written as a Haskell one is.
--
-- Tried before a name, because a quote is a character a name may hold: @'a'@ was a name before there
-- were character literals. Only a quote, one character or escape, and a closing quote is taken as a
-- literal, so a name with a quote elsewhere in it is still read as the name it was.
pCharConst :: Parser PExpr
pCharConst = do
  pos1 <- getSourcePos
  c <- char '\'' *> L.charLiteral <* char '\''
  pos2 <- getSourcePos
  return $ RawCExpr (emptyMeta pos1 pos2) (CChar c)

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
    pTypeTerm
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

-- | A type written after @->@: a term, or the intersection of several (@Functor[$T -> Eq] && Eq@,
-- the values in every one of them). Nothing else may follow a type there, so @&&@ is not read
-- as the boolean operator; 'Syntax.Ct.Desugarf.Expr.exprToType' reads the application as the
-- intersection.
pTypeTerm :: Parser PExpr
pTypeTerm = do
  t <- term
  rest <- many (symbol "&&" *> term)
  return $ foldl (mkOp2 "&&") t rest

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

-- | The arguments applied to a term.
--
-- No arguments is an application too, and means the term it is applied to: an empty apply is read
-- away by 'Syntax.Ct.Desugarf.Expr.semiDesExpr', so @f()@ and @f@ are the one thing. It is written
-- by an import which names a syntax taking no path of its own — @import hsbuiltins()@ — where a
-- bare name would read as a path to look for.
pArgsSuffix :: Parser TermSuffix
pArgsSuffix = do
  pos1 <- getSourcePos
  args <- parens $ sepBy pArgSuffix (symbol ",")
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
  return $ AliasSuffix (emptyMeta pos1 pos2) (mkName aliasName)

pTypePropRelSuffix :: Parser TermSuffix
pTypePropRelSuffix = do
  _ <- string "__"
  pos1 <- getSourcePos
  p <- identifier
  v <- parens pExpr
  pos2 <- getSourcePos
  return $ TypePropSuffix (emptyMeta pos1 pos2) $ TypePropRel (mkName p) v

pTypePropProjSuffix :: Parser TermSuffix
pTypePropProjSuffix = do
  _ <- string "_"
  pos1 <- getSourcePos
  p <- identifier
  v <- optional $ parens pExpr
  pos2 <- getSourcePos
  let v' = fromMaybe (RawValue (mWithType trueType (emptyMeta pos1 pos2)) (mkName truePrim)) v
  return $ TypePropSuffix (emptyMeta pos1 pos2) $ TypePropProj (mkName p) v'


pTermSuffix :: Parser TermSuffix
pTermSuffix = pArgsSuffix <|> pVarsSuffix <|> pContextSuffix <|> pAliasSuffix <|> pTypePropRelSuffix <|> pTypePropProjSuffix

pFloat :: Parser PExpr
pFloat = do
  pos1 <- getSourcePos
  f <- float
  pos2 <- getSourcePos
  return $ RawCExpr (emptyMeta pos1 pos2) (CFloat f)

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
  return $ RawMacroValue (emptyMeta pos1 pos2) (mkName n)

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
       <|> try pCharConst
       <|> try pFloat
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
