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

import           Lexer
import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import Parser.Syntax

pLeafVar :: Parser (TypeVarName, ParseMeta)
pLeafVar = do
  var <- tvar
  return (var, emptyMeta)

pTypeArg :: Parser (String, PObjArg)
pTypeArg = do
  tp <- tvar <|> tidentifier
  argName <- identifier
  return (argName, (PreTyped $ SumType $ joinPartialLeafs [(tp, H.empty, H.empty)], Nothing))

pLeafType :: Parser PObject
pLeafType = do
  name <- tidentifier
  maybeVars <- try $ optional $ angleBraces $ sepBy1 pLeafVar (symbol ",")
  maybeArgs <- optional $ parens (sepBy1 pTypeArg (symbol ","))
  let vars = maybe H.empty H.fromList maybeVars
  let args = maybe H.empty H.fromList maybeArgs
  return $ Object emptyMeta TypeObj name vars args

pType :: Parser [PObject]
pType = sepBy1 pLeafType (symbol "|")

pMultiTypeDefStatement :: Parser PStatement
pMultiTypeDefStatement = do
  _ <- symbol "data"
  name <- tidentifier
  _ <- symbol "="
  MultiTypeDefStatement . MultiTypeDef name <$> pType

pTypeDefStatement :: Parser PStatement
pTypeDefStatement = do
  _ <- symbol "data"
  TypeDefStatement . TypeDef <$> pLeafType

pClassDefStatement :: Parser PStatement
pClassDefStatement = do
  _ <- symbol "instance"
  typeName <- tidentifier
  _ <- symbol "of"
  className <- tidentifier
  return $ RawClassDefStatement (typeName, className)

pTypeStatement :: Parser PStatement
pTypeStatement = try pMultiTypeDefStatement
                 <|> try pTypeDefStatement
                 <|> pClassDefStatement
