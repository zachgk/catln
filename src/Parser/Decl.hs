--------------------------------------------------------------------
-- |
-- Module    :  Parser.Decl
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is used to define function definitions and
-- declarations.
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Parser.Decl where

import           Control.Applicative        hiding (many, some)
import           Data.Maybe
import           Text.Megaparsec            hiding (pos1)
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.Either
import           Data.List
import           Parser.Expr
import           Parser.Lexer
import           Parser.Syntax
import           Syntax
import           Syntax.Prgm
import           Syntax.Types
import           Text.Megaparsec.Char

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

pDeclLHS :: Parser PDeclLHS
pDeclLHS = do
  pos1 <- getSourcePos
  patt <- pPattern FunctionObj
  maybeArrMeta <- optional pArrowRes
  pos2 <- getSourcePos
  let arrMeta = fromMaybe (emptyMeta pos1 pos2) maybeArrMeta
  return $ DeclLHS arrMeta patt

pComment :: Parser PCompAnnot
pComment = do
  pos1 <- getSourcePos
  c <- L.indentBlock scn p
  pos2 <- getSourcePos
  let m = emptyMeta pos1 pos2
  return $ RawTupleApply (emptyMetaM "appArg" m) (emptyMetaM "valC" m, RawValue (emptyMetaM "val" m) "/Catln/#md") [RawTupleArgNamed (emptyMetaM "text" m) "text" (RawCExpr m (CStr c))]
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
  deriving (Show)

validDeclTree :: [TreeRes] -> Either String ([PDeclSubStatement], PExpr)
validDeclTree = aux ([], Nothing)
  where
    aux (_, Nothing) [] = Left "No expression found. The declaration must contain an expression"
    aux (subSt, Just expr) [] = Right (subSt, expr)
    aux (subSt, maybeExpr) ((TRDecl decl):trs) = aux (RawDeclSubStatementDecl decl:subSt, maybeExpr) trs
    aux (subSt, maybeExpr) ((TRAnnot annot):trs) = aux (RawDeclSubStatementAnnot annot []:subSt, maybeExpr) trs
    aux (subSt, Nothing) ((TRExpr expr):trs) = aux (subSt, Just expr) trs
    aux (_, Just{}) ((TRExpr _):_) = Left "Multiple expressions found. The declaration should only have one expression line"

-- Used to verify only annotations or comments used for declarations and single line definitions
validSubStatementInSingle :: TreeRes -> Either String PDeclSubStatement
validSubStatementInSingle TRDecl{} = Left "Found an unexpected subDefinition when the expression is defined in a single line."
validSubStatementInSingle TRExpr{} = Left "Found an unexpected subExpression when the expression is defined in a single line."
validSubStatementInSingle (TRAnnot annot) = return $ RawDeclSubStatementAnnot annot []

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
    childParser = try (TRDecl <$> pDeclTree) <|>  try (TRAnnot <$> pComment) <|> try (TRAnnot <$> pCompAnnot) <|> (TRExpr <$> pExpr)
    p = do
      lhs <- pDeclLHS
      eqAndExpr <- optional $ do
        _ <- symbol "="
        optional . try $ pExpr
      return (L.IndentMany Nothing (pack lhs eqAndExpr) childParser)

pRootDecl :: Parser PStatement
pRootDecl = RawDeclStatement <$> pDeclTree
