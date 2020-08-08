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

pIdArg :: Parser (String, PObjArg)
pIdArg = do
  tp <- tidentifier
  argName <- identifier
  return (argName, (PreTyped $ SumType $ joinPartialLeafs [(tp, H.empty, H.empty)], Nothing))

pVarArg :: Parser (String, PObjArg)
pVarArg = do
  tp <- tvar
  argName <- identifier
  return (argName, (PreTyped $ TypeVar $ TVVar tp, Nothing))

pTypeArg :: Parser (String, PObjArg)
pTypeArg = pVarArg <|> pIdArg

pLeafType :: Parser ParseMeta
pLeafType = do
  name <- tidentifier
  maybeVars <- try $ optional $ angleBraces $ sepBy1 pLeafVar (symbol ",")
  maybeArgs <- optional $ parens (sepBy1 pTypeArg (symbol ","))
  let vars = maybe H.empty H.fromList maybeVars
  let args = maybe H.empty H.fromList maybeArgs
  let tp = PreTyped $ SumType $ joinPartialLeafs [(name, fmap (const TopType) vars, fmap (const TopType) args)]
  return tp

-- Parses the options for a sealed class definition
pType :: Parser [ParseMeta]
pType = sepBy1 (pLeafType <|> varOption) (symbol "|")
  where varOption = PreTyped . TypeVar . TVVar <$> tvar


pMultiTypeDefStatement :: Parser PStatement
pMultiTypeDefStatement = do
  _ <- symbol "class"
  name <- tidentifier
  maybeVars <- try $ optional $ angleBraces $ sepBy1 tvar (symbol ",")
  let vars = maybe H.empty (H.fromList . map (, TopType)) maybeVars
  _ <- symbol "="
  MultiTypeDefStatement . MultiTypeDef name vars <$> pType

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
