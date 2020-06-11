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
import Parser.Syntax

pLeafVar :: Parser (TypeVarName, Type)
pLeafVar = do
  var <- tvar
  return (var, TopType)

pTypeArg :: Parser (String, Type)
pTypeArg = do
  tp <- tvar <|> tidentifier
  argName <- identifier
  return (argName, SumType $ joinPartialLeafs [(tp, H.empty, H.empty)])

pTypeVar :: Parser Type
pTypeVar = TypeVar <$> tvar

pLeafType :: Parser PartialType
pLeafType = do
  name <- tidentifier
  maybeVars <- try $ optional $ angleBraces $ sepBy1 pLeafVar (symbol ",")
  maybeArgs <- optional $ parens (sepBy1 pTypeArg (symbol ","))
  let vars = maybe H.empty H.fromList maybeVars
  let args = maybe H.empty H.fromList maybeArgs
  return (name, vars, args)

pType :: Parser Type
pType = pTypeVar
        <|> SumType . joinPartialLeafs <$> sepBy1 pLeafType (symbol "|")

pTypeDefStatement :: Parser PStatement
pTypeDefStatement = do
  _ <- symbol "data"
  name <- tidentifier
  _ <- symbol "="
  TypeDefStatement . TypeDef name <$> pType

pClassDefStatement :: Parser PStatement
pClassDefStatement = do
  _ <- symbol "instance"
  typeName <- tidentifier
  _ <- symbol "of"
  className <- tidentifier
  return $ RawClassDefStatement (typeName, className)
