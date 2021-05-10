--------------------------------------------------------------------
-- |
-- Module    :  Parser.Decl
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Parser.Decl where

import           Control.Applicative            hiding (many, some)
import           Data.Maybe
import           Text.Megaparsec hiding (pos1)
import qualified Text.Megaparsec.Char.Lexer     as L

import           Syntax.Prgm
import           Syntax
import           Parser.Lexer
import Parser.Syntax
import Parser.Expr
import Syntax.Types
import qualified Data.HashMap.Strict as H
import Text.Megaparsec.Char
import Data.List
import Data.Either

pDeclArg :: Parser (String, PExpr)
pDeclArg = do
  argName <- identifier
  _ <- symbol "="
  expr <- pExpr
  return (argName, expr)

pCompAnnot :: Parser PCompAnnot
pCompAnnot = do
  pos1 <- getSourcePos
  annotName <- pAnnotIdentifier
  pos2 <- getSourcePos
  maybeArgVals <- optional $ parens $ sepBy1 pCallArg (symbol ",")
  pos3 <- getSourcePos
  let baseValue = RawValue (emptyMeta pos1 pos2) annotName
  return $ case maybeArgVals of
    Just argVals -> RawTupleApply (emptyMeta pos2 pos3) (PreTyped TopType (Just (pos1, pos2, "apply")), baseValue) argVals
    Nothing -> baseValue

pArrowRes :: Parser ParseMeta
pArrowRes = do
  _ <- symbol "->"
  pos1 <- getSourcePos
  t <- pSingleType
  pos2 <- getSourcePos
  return $ PreTyped t (Just (pos1, pos2, ""))

pMethodCallerType :: Parser PObjArg
pMethodCallerType = do
  pos1 <- getSourcePos
  t <- singletonType <$> pLeafType
  pos2 <- getSourcePos
  _ <- symbol "."
  return (PreTyped t (Just (pos1, pos2, "")), Nothing)

pMethodCallerObj :: Parser PObjArg
pMethodCallerObj = do
  pos1 <- getSourcePos
  obj <- pObjTree FunctionObj
  pos2 <- getSourcePos
  _ <- symbol "."
  return (emptyMeta pos1 pos2, Just obj)

pMethodCaller :: Parser PObjArg
pMethodCaller = try pMethodCallerType <|> pMethodCallerObj

pDeclLHS :: Parser PDeclLHS
pDeclLHS = do
  pos1 <- getSourcePos
  meth <-  optional $ try pMethodCaller
  patt@(Pattern obj@Object{objArgs} guard) <- pPattern FunctionObj
  let patt' = case meth of
        Just meth' -> Pattern obj{objArgs = H.insert "this" meth' objArgs} guard
        Nothing -> patt
  maybeArrMeta <- optional pArrowRes
  pos2 <- getSourcePos
  let arrMeta = fromMaybe (emptyMeta pos1 pos2) maybeArrMeta
  return $ DeclLHS arrMeta patt'

pComment :: Parser String
pComment = L.indentBlock scn p
  where
    takeLine = takeWhileP (Just "character") (/= '\n')
    p = do
      _ <- string "# "
      l <- takeLine
      return (L.IndentMany Nothing (\ls -> return $ intercalate "\n" (l:ls)) takeLine)


data TreeRes
  = TRDecl PDecl
  | TRExpr PExpr
  | TRAnnot PCompAnnot
  | TRComment String
  deriving (Show)

validDeclTree :: [TreeRes] -> Either String ([PDeclSubStatement], PExpr)
validDeclTree = aux ([], Nothing)
  where
    aux (_, Nothing) [] = Left "No expression found. The declaration must contain an expression"
    aux (subSt, Just expr) [] = Right (subSt, expr)
    aux (subSt, maybeExpr) ((TRDecl decl):trs) = aux (RawDeclSubStatementDecl decl:subSt, maybeExpr) trs
    aux (subSt, maybeExpr) ((TRAnnot annot):trs) = aux (RawDeclSubStatementAnnot annot:subSt, maybeExpr) trs
    aux (subSt, maybeExpr) ((TRComment comment):trs) = aux (RawDeclSubStatementComment comment:subSt, maybeExpr) trs
    aux (subSt, Nothing) ((TRExpr expr):trs) = aux (subSt, Just expr) trs
    aux (_, Just{}) ((TRExpr _):_) = Left "Multiple expressions found. The declaration should only have one expression line"

-- Used to verify only annotations or comments used for declarations and single line definitions
validSubStatementInSingle :: TreeRes -> Either String PDeclSubStatement
validSubStatementInSingle TRDecl{} = Left "Found an unexpected subDefinition when the expression is defined in a single line."
validSubStatementInSingle TRExpr{} = Left "Found an unexpected subExpression when the expression is defined in a single line."
validSubStatementInSingle (TRAnnot annot) = return $ RawDeclSubStatementAnnot annot
validSubStatementInSingle (TRComment comment) = return $ RawDeclSubStatementComment comment

pDeclTree :: Parser PDecl
pDeclTree = L.indentBlock scn p
  where
    pack lhs maybeSingleExpr children = case maybeSingleExpr of
      Just (Just singleExpr) -> -- fun(...) = expr
        case partitionEithers $ map validSubStatementInSingle children of
          ([], subStatements) -> return $ RawDecl lhs subStatements (Just singleExpr)
          (err:_, _) -> fail err
      Just Nothing -> -- fun(...) = \n defined in next lines
        case validDeclTree children of
          Right (subStatements, expr) -> return $ RawDecl lhs subStatements (Just expr)
          Left err -> fail err
      Nothing -> -- fun(...) -> retType   -- Declaration only
        case partitionEithers $ map validSubStatementInSingle children of
          ([], subStatements) -> case lhs of
            (DeclLHS arrMeta _) | getMetaType arrMeta == TopType -> fail "Declaration must include an arrow or an equals"
            _ -> return $ RawDecl lhs subStatements Nothing
          (err:_, _) -> fail err
    childParser :: Parser TreeRes
    childParser = try (TRDecl <$> pDeclTree) <|>  try (TRComment <$> pComment) <|> try (TRAnnot <$> pCompAnnot) <|> (TRExpr <$> pExpr)
    p = do
      lhs <- pDeclLHS
      eqAndExpr <- optional $ do
        _ <- symbol "="
        optional . try $ pExpr
      return (L.IndentMany Nothing (pack lhs eqAndExpr) childParser)

pRootDecl :: Parser PStatement
pRootDecl = RawDeclStatement <$> pDeclTree
