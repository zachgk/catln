--------------------------------------------------------------------
-- |
-- Module    :  Parser.Type
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Parser.Type where

import           Control.Applicative            hiding (many, some)
import qualified Data.HashMap.Strict as H
import           Text.Megaparsec

import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           Parser.Lexer
import Parser.Syntax

pLeafVar :: Parser (TypeVarName, Type)
pLeafVar = do
  -- TODO: Should support multiple class identifiers such as <Eq Ord $T>
  maybeClass <- optional tidentifier
  var <- tvar
  let tp = maybe TopType (\n -> singletonType (PartialType (PTypeName n) H.empty H.empty H.empty PtArgExact)) maybeClass
  return (var, tp)

-- TODO: Currently only parses `$T` as sugar for `$T=$T`
-- Should eventually also support the full `$A=$B`
pTypeVar :: Parser (TypeVarName, Type)
pTypeVar = do
  var <- tvar
  return (var, TypeVar $ TVVar var)

-- TODO: Parse type properties
pIdArg :: Parser (String, Type)
pIdArg = do
  tp <- tidentifier
  maybeVars <- optional $ angleBraces $ sepBy1 pTypeVar (symbol ",")
  let vars = maybe H.empty H.fromList maybeVars
  argName <- identifier
  -- Use PTypeName for now and replace with classes during Desugarf.Passes.typeNameToClass
  return (argName, singletonType (PartialType (PTypeName tp) vars H.empty H.empty PtArgExact))

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
        PLeafTypeData -> tidentifier
        PLeafTypeAnnot -> pAnnotIdentifier
        PLeafTypeSealedClass -> tidentifier
  name <- parseIdentifier
  let parseVar = case mode of
        PLeafTypeData -> pLeafVar
        PLeafTypeAnnot -> pLeafVar
        PLeafTypeSealedClass -> pTypeVar
  maybeVars <- optional $ angleBraces $ sepBy1 parseVar (symbol ",")
  maybeArgs <- optional $ parens (sepBy1 pTypeArg (symbol ","))
  pos2 <- getSourcePos
  let vars = maybe H.empty H.fromList maybeVars
  let args = maybe H.empty H.fromList maybeArgs
  -- Use PTypeName for now and replace with classes during Desugarf.Passes.typeNameToClass
  let tp = PreTyped (singletonType (PartialType (PTypeName name) vars H.empty args PtArgExact)) (Just (pos1, pos2, ""))
  return tp

-- Parses the options for a sealed class definition
pType :: Parser [ParseMeta]
pType = sepBy1 (pLeafType PLeafTypeSealedClass <|> varOption) (symbol "|")
  where varOption = do
          pos1 <- getSourcePos
          name <- tvar
          pos2 <- getSourcePos
          return $ PreTyped (TypeVar $ TVVar name) (Just (pos1, pos2, ""))


pMultiTypeDefStatement :: Parser PStatement
pMultiTypeDefStatement = do
  _ <- symbol "class"
  name <- tidentifier
  maybeVars <- optional $ angleBraces $ sepBy1 pLeafVar (symbol ",")
  let vars = maybe H.empty H.fromList maybeVars
  _ <- symbol "="
  MultiTypeDefStatement . MultiTypeDef name vars <$> pType

pAnnotDefStatement :: Parser PStatement
pAnnotDefStatement = do
  _ <- symbol "annot"
  TypeDefStatement . TypeDef <$> pLeafType PLeafTypeAnnot

pTypeDefStatement :: Parser PStatement
pTypeDefStatement = do
  _ <- symbol "data"
  TypeDefStatement . TypeDef <$> pLeafType PLeafTypeData

pClassDefStatement :: Parser PStatement
pClassDefStatement = do
  _ <- symbol "instance"
  typeName <- tidentifier
  _ <- symbol "of"
  className <- tidentifier
  return $ RawClassDefStatement (typeName, className)

pTypeStatement :: Parser PStatement
pTypeStatement = pMultiTypeDefStatement
                 <|> pAnnotDefStatement
                 <|> pTypeDefStatement
                 <|> pClassDefStatement
