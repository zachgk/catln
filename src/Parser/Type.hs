--------------------------------------------------------------------
-- |
-- Module    :  Parser.Type
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module parses data, class, and annotation declarations.
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Parser.Type where

import           Control.Applicative        hiding (many, some)
import qualified Data.HashMap.Strict        as H
import           Text.Megaparsec            hiding (pos1)

import           Data.List
import           Data.Maybe
import           Parser.Decl
import           Parser.Lexer
import           Parser.Syntax
import           Syntax
import           Syntax.Prgm
import           Syntax.Types

import           Data.Either
import qualified Text.Megaparsec.Char.Lexer as L

pLeafVar :: Parser (TypeVarName, Type)
pLeafVar = do
  -- TODO: Should support multiple class identifiers such as <Eq Ord $T>
  maybeClass <- optional tidentifier
  var <- tvar
  let tp = maybe TopType (\n -> singletonType (PartialType (PRelativeName n) H.empty H.empty H.empty PtArgExact)) maybeClass
  return (var, tp)

pTypeVar :: Parser (TypeVarName, Type)
pTypeVar = do
  var <- tvar
  maybeVal <- optional $ do
    _ <- symbol "="
    tvar
  let val = fromMaybe var maybeVal
  return (var, TypeVar $ TVVar val)

-- TODO: Parse type properties
pIdArg :: Parser (String, Type)
pIdArg = do
  tp <- tidentifier
  maybeVars <- optional $ angleBraces $ sepBy1 pTypeVar (symbol ",")
  let vars = maybe H.empty H.fromList maybeVars
  argName <- identifier
  -- Use PRelativeName for now and replace with classes during Desugarf.Passes.typeNameToClass
  return (argName, singletonType (PartialType (PRelativeName tp) vars H.empty H.empty PtArgExact))

pVarArg :: Parser (String, Type)
pVarArg = do
  tp <- tvar
  argName <- identifier
  return (argName, TypeVar $ TVVar tp)

pTypeArg :: Parser (String, Type)
pTypeArg = pVarArg <|> pIdArg

-- TODO: Parse type properties
data PLeafTypeMode = PLeafTypeData | PLeafTypeSealedClass | PLeafTypeAnnot
pLeafType :: PLeafTypeMode -> Parser ParseMeta
pLeafType mode = do
  pos1 <- getSourcePos
  let parseIdentifier = case mode of
        PLeafTypeData        -> ttypeidentifier
        PLeafTypeAnnot       -> pAnnotIdentifier
        PLeafTypeSealedClass -> tidentifier
  name <- parseIdentifier
  let parseVar = case mode of
        PLeafTypeData        -> pLeafVar
        PLeafTypeAnnot       -> pLeafVar
        PLeafTypeSealedClass -> pTypeVar
  maybeVars <- optional $ angleBraces $ sepBy1 parseVar (symbol ",")
  maybeArgs <- optional $ parens (sepBy1 pTypeArg (symbol ","))
  pos2 <- getSourcePos
  let vars = maybe H.empty H.fromList maybeVars
  let args = maybe H.empty H.fromList maybeArgs
  -- Use PRelativeName for now and replace with classes during Desugarf.Passes.typeNameToClass
  let tp = PreTyped (singletonType (PartialType (PRelativeName name) vars H.empty args PtArgExact)) (Just (pos1, pos2, ""))
  return tp

-- Parses the options for a sealed class definition
pType :: Parser [ParseMeta]
pType = sepBy1 (pLeafType PLeafTypeSealedClass <|> varOption) (symbol "|")
  where varOption = do
          pos1 <- getSourcePos
          name <- tvar
          pos2 <- getSourcePos
          return $ PreTyped (TypeVar $ TVVar name) (Just (pos1, pos2, ""))


pClassStatement' :: Parser
  (Either
     (MultiTypeDef ParseMeta) RawClassDecl)
pClassStatement' = do
  _ <- symbol "class"
  name <- ttypeidentifier
  maybeVars <- optional $ angleBraces $ sepBy1 pLeafVar (symbol ",")
  let vars = maybe H.empty H.fromList maybeVars
  maybeTypes <- optional $ do
    _ <- symbol "="
    MultiTypeDef name vars <$> pType
  case maybeTypes of
    Just types -> return $ Left types
    Nothing    -> return $ Right (name, vars)

getPath :: String -> Path
getPath name = if "/" `isPrefixOf` name then
  Absolute name
  else Relative name


pClassStatement :: Parser PStatementTree
pClassStatement = L.indentBlock scn p
  where
    pack pclass children = case pclass of
      Left multi@(MultiTypeDef name _ _) -> case partitionEithers $ map validSubStatementInSingle children of
        ([], subStatements) -> return $ RawStatementTree (MultiTypeDefStatement multi (getPath name)) subStatements
        (_, _) -> return $ RawStatementTree (MultiTypeDefStatement multi (getPath name)) []
      Right pcl@(name, _) -> case partitionEithers $ map validSubStatementInSingle children of
        ([], subStatements) -> return $ RawStatementTree (RawClassDeclStatement pcl (getPath name)) subStatements
        (_, _)              -> return $ RawStatementTree (RawClassDeclStatement pcl (getPath name)) []
    childParser :: Parser TreeRes
    childParser = try (TRAnnot <$> pComment)
    p = do
      rawclass <- pClassStatement'
      return (L.IndentMany Nothing (pack rawclass) childParser)

pAnnotDefStatement :: Parser PStatementTree
pAnnotDefStatement = L.indentBlock scn p
  where
    pack pclass children = case partitionEithers $ map validSubStatementInSingle children of
        ([], subStatements) -> return $ RawStatementTree (TypeDefStatement pclass) subStatements
        (_, _)              -> return $ RawStatementTree (TypeDefStatement pclass) []
    childParser :: Parser TreeRes
    childParser = try (TRAnnot <$> pComment)
    p = do
      rawclass <- do
        _ <- symbol "annot"
        TypeDef <$> pLeafType PLeafTypeAnnot
      return (L.IndentMany Nothing (pack rawclass) childParser)

pTypeDefStatement :: Parser PStatementTree
pTypeDefStatement = L.indentBlock scn p
  where
    pack pclass children = case partitionEithers $ map validSubStatementInSingle children of
        ([], subStatements) -> return $ RawStatementTree (TypeDefStatement pclass) subStatements
        (_, _)              -> return $ RawStatementTree (TypeDefStatement pclass) []
    childParser :: Parser TreeRes
    childParser = try (TRAnnot <$> pComment)
    p = do
      rawclass <- do
        _ <- symbol "data"
        TypeDef <$> pLeafType PLeafTypeData
      return (L.IndentMany Nothing (pack rawclass) childParser)

pClassDefStatement :: Parser PStatementTree
pClassDefStatement = L.indentBlock scn p
  where
    pack pclass@((typeName, _), _) children = case partitionEithers $ map validSubStatementInSingle children of
        ([], subStatements) -> return $ RawStatementTree (RawClassDefStatement pclass (getPath typeName)) subStatements
        (_, _) -> return $ RawStatementTree (RawClassDefStatement pclass (getPath typeName)) []
    childParser :: Parser TreeRes
    childParser = try (TRAnnot <$> pComment)
    p = do
      rawclass <- do
        _ <- symbol "every"
        typeName <- tidentifier
        maybeVars <- optional $ angleBraces $ sepBy1 pTypeVar (symbol ",")
        let vars = maybe H.empty H.fromList maybeVars
        _ <- symbol "isa"
        className <- ttypeidentifier
        return ((typeName, vars), className)
      return (L.IndentMany Nothing (pack rawclass) childParser)

pTypeStatement :: Parser PStatementTree
pTypeStatement = pClassStatement
                 <|> pAnnotDefStatement
                 <|> pTypeDefStatement
                 <|> pClassDefStatement
