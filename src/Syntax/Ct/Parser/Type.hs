--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Parser.Type
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module parses data, class, and annotation declarations.
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Syntax.Ct.Parser.Type where

import           Control.Applicative     hiding (many, some)
import           Text.Megaparsec         hiding (pos1)

import           Data.Maybe
import           Semantics.Prgm
import           Syntax.Ct.Parser.Expr
import           Syntax.Ct.Parser.Lexer
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm


pMultiTerm :: Parser [PGuardExpr]
pMultiTerm = do
  terms <- sepBy1 term (symbol "|")
  return $ map (`GuardExpr` Nothing) terms

pExtends :: Parser ExtendedClasses
pExtends = do
  _ <- symbol "isa"
  sepBy1 ttypeidentifier (symbol ",")

pClassStatement :: Parser PStatement
pClassStatement = do
  _ <- symbol "class"
  clss <- term
  maybeTypes <- optional $ do
    _ <- symbol "="
    terms <- pMultiTerm
    extends <- optional pExtends
    return (terms, fromMaybe [] extends)
  return $ case maybeTypes of
    Just (types, extends) -> MultiTypeDefStatement (MultiTypeDef clss types extends)
    Nothing    -> RawClassDeclStatement clss

pAnnotDefStatement :: Parser PStatement
pAnnotDefStatement = do
  _ <- symbol "annot"
  TypeDefStatement <$> term

pTypeDefStatement :: Parser PStatement
pTypeDefStatement = do
  _ <- symbol "data"
  TypeDefStatement <$> term

pClassDefStatement :: Parser PStatement
pClassDefStatement = do
  _ <- symbol "every"
  instanceTerm <- term
  extends <- pExtends
  let def = (instanceTerm, extends)
  return $ RawClassDefStatement def

pTypeStatement :: Parser PStatement
pTypeStatement = pClassStatement
                 <|> pAnnotDefStatement
                 <|> pTypeDefStatement
                 <|> pClassDefStatement
