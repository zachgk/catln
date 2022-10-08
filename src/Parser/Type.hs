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

import           Control.Applicative hiding (many, some)
import qualified Data.HashMap.Strict as H
import           Text.Megaparsec     hiding (pos1)

import           Data.Maybe
import           Parser.Lexer
import           Parser.Syntax
import           Syntax.Prgm
import           Syntax.Types


pLeafVar :: Parser (TypeVarName, Type)
pLeafVar = do
  -- TODO: Should support multiple class identifiers such as <Eq Ord $T>
  maybeClass <- optional tidentifier
  var <- tvar
  let tp = maybe TopType (singletonType . partialVal . PRelativeName) maybeClass
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
  return (argName, singletonType (partialVal (PRelativeName tp)){ptVars=vars})

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
  let tp = Meta (singletonType ((partialVal (PRelativeName name)){ptVars=vars, ptArgs=args})) (Just (pos1, pos2, "")) emptyMetaDat
  return tp

-- Parses the options for a sealed class definition
pType :: Parser [ParseMeta]
pType = sepBy1 (pLeafType PLeafTypeSealedClass <|> varOption) (symbol "|")
  where varOption = do
          pos1 <- getSourcePos
          name <- tvar
          pos2 <- getSourcePos
          return $ Meta (TypeVar $ TVVar name) (Just (pos1, pos2, "")) emptyMetaDat

pClassStatement :: Parser PStatement
pClassStatement = do
  _ <- symbol "class"
  name <- ttypeidentifier
  maybeVars <- optional $ angleBraces $ sepBy1 pLeafVar (symbol ",")
  let vars = maybe H.empty H.fromList maybeVars
  maybeTypes <- optional $ do
    _ <- symbol "="
    MultiTypeDef name vars <$> pType
  return $ case maybeTypes of
    Just types -> MultiTypeDefStatement types (getPath name)
    Nothing    -> RawClassDeclStatement (name, vars) (getPath name)

pAnnotDefStatement :: Parser PStatement
pAnnotDefStatement = do
  _ <- symbol "annot"
  rawAnnot <- TypeDef <$> pLeafType PLeafTypeAnnot
  return $ TypeDefStatement rawAnnot

pTypeDefStatement :: Parser PStatement
pTypeDefStatement = do
  _ <- symbol "data"
  def <- TypeDef <$> pLeafType PLeafTypeData
  return $ TypeDefStatement def

pClassDefStatement :: Parser PStatement
pClassDefStatement = do
  _ <- symbol "every"
  typeName <- tidentifier
  maybeVars <- optional $ angleBraces $ sepBy1 pTypeVar (symbol ",")
  let vars = maybe H.empty H.fromList maybeVars
  _ <- symbol "isa"
  className <- ttypeidentifier
  let def = ((typeName, vars), className)
  return $ RawClassDefStatement def (getPath typeName)

pTypeStatement :: Parser PStatement
pTypeStatement = pClassStatement
                 <|> pAnnotDefStatement
                 <|> pTypeDefStatement
                 <|> pClassDefStatement
