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
  var <- tvar
  return (var, TopType)

-- TODO: Currently only parses `$T` as sugar for `$T=$T`
-- Should eventually also support the full `$A=$B`
pTypeVar :: Parser (TypeVarName, Type)
pTypeVar = do
  var <- tvar
  return (var, TypeVar $ TVVar var)

pIdArg :: Parser (String, Type)
pIdArg = do
  tp <- tidentifier
  maybeVars <- optional $ angleBraces $ sepBy1 pTypeVar (symbol ",")
  let vars = maybe H.empty H.fromList maybeVars
  argName <- identifier
  -- Use PTypeName for now and replace with classes during Desugarf.Passes.typeNameToClass
  return (argName, SumType $ joinPartialLeafs [(PTypeName tp, vars, H.empty)])

pVarArg :: Parser (String, Type)
pVarArg = do
  tp <- tvar
  argName <- identifier
  return (argName, TypeVar $ TVVar tp)

pTypeArg :: Parser (String, Type)
pTypeArg = pVarArg <|> pIdArg

data PLeafTypeMode = PLeafTypeData | PLeafTypeSealedClass
pLeafType :: PLeafTypeMode -> Parser ParseMeta
pLeafType mode = do
  name <- tidentifier
  let parseVar = case mode of
        PLeafTypeData -> pLeafVar
        PLeafTypeSealedClass -> pTypeVar
  maybeVars <- optional $ angleBraces $ sepBy1 parseVar (symbol ",")
  maybeArgs <- optional $ parens (sepBy1 pTypeArg (symbol ","))
  let vars = maybe H.empty H.fromList maybeVars
  let args = maybe H.empty H.fromList maybeArgs
  -- Use PTypeName for now and replace with classes during Desugarf.Passes.typeNameToClass
  let tp = PreTyped $ SumType $ joinPartialLeafs [(PTypeName name, vars, args)]
  return tp

-- Parses the options for a sealed class definition
pType :: Parser [ParseMeta]
pType = sepBy1 (pLeafType PLeafTypeSealedClass <|> varOption) (symbol "|")
  where varOption = PreTyped . TypeVar . TVVar <$> tvar


pMultiTypeDefStatement :: Parser PStatement
pMultiTypeDefStatement = do
  _ <- symbol "class"
  name <- tidentifier
  maybeVars <- optional $ angleBraces $ sepBy1 tvar (symbol ",")
  let vars = maybe H.empty (H.fromList . map (, TopType)) maybeVars
  _ <- symbol "="
  MultiTypeDefStatement . MultiTypeDef name vars <$> pType

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
                 <|> pTypeDefStatement
                 <|> pClassDefStatement
