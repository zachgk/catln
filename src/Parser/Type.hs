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
import           Parser.Expr
import           Parser.Lexer
import           Parser.Syntax
import           Syntax.Prgm


pMultiTerm :: Parser [PExpr]
pMultiTerm = sepBy1 (term ParseTypeExpr) (symbol "|")

pClassStatement :: Parser PStatement
pClassStatement = do
  _ <- symbol "class"
  name <- ttypeidentifier
  maybeVars <- optional $ angleBraces $ sepBy1 pLeafVar (symbol ",")
  let vars = maybe H.empty H.fromList maybeVars
  maybeTypes <- optional $ do
    _ <- symbol "="
    MultiTypeDef name vars <$> pMultiTerm
  return $ case maybeTypes of
    Just types -> MultiTypeDefStatement types (getPath name)
    Nothing    -> RawClassDeclStatement (name, vars) (getPath name)

pAnnotDefStatement :: Parser PStatement
pAnnotDefStatement = do
  _ <- symbol "annot"
  rawAnnot <- TypeDef <$> term ParseInputExpr
  return $ TypeDefStatement rawAnnot

pTypeDefStatement :: Parser PStatement
pTypeDefStatement = do
  _ <- symbol "data"
  def <- TypeDef <$> term ParseInputExpr
  return $ TypeDefStatement def

pClassDefStatement :: Parser PStatement
pClassDefStatement = do
  _ <- symbol "every"
  instanceTerm <- term ParseTypeExpr
  _ <- symbol "isa"
  className <- ttypeidentifier
  let def = (instanceTerm, className)
  return $ RawClassDefStatement def (getPath $ fromJust $ maybeExprPath instanceTerm)

pTypeStatement :: Parser PStatement
pTypeStatement = pClassStatement
                 <|> pAnnotDefStatement
                 <|> pTypeDefStatement
                 <|> pClassDefStatement
