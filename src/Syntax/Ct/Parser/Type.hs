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
import           Syntax.Ct.Parser.Expr
import           Syntax.Ct.Parser.Lexer
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm

pMultiTerm :: Parser [PExpr]
pMultiTerm = sepBy1 item (symbol "||")
  where
    item = do
      t <- term
      maybeWhere <- optional $ do
        _ <- symbol "| "
        term
      case maybeWhere of
        Nothing -> return t
        Just wh -> return $ RawWhere t wh

pExtends :: Parser (ExtendedClasses RawExpr ParseMetaDat)
pExtends = do
  _ <- symbol "isa"
  sepBy1 term (symbol ",")

pClassStatement :: Parser PStatement
pClassStatement = do
  _ <- symbol "class"
  clss <- term
  maybeTypes <- optional $ do
    _ <- symbol "="
    pMultiTerm
  extends <- optional pExtends
  let extends' = fromMaybe [] extends
  return $ case maybeTypes of
    Just types -> MultiTypeDefStatement (MultiTypeDef clss types extends')
    Nothing    -> RawClassDeclStatement clss extends'

pTypeDefStatement :: Parser PStatement
pTypeDefStatement = do
  _ <- symbol "data" <|> symbol "annot"
  d <- term
  extends <- optional pExtends
  let extends' = fromMaybe [] extends
  return $ TypeDefStatement d extends'

pClassDefStatement :: Parser PStatement
pClassDefStatement = do
  _ <- symbol "every"
  instanceTerm <- term
  extends <- pExtends
  let def = (instanceTerm, extends)
  return $ RawClassDefStatement def

pTypeStatement :: Parser PStatement
pTypeStatement = pClassStatement
                 <|> pTypeDefStatement
                 <|> pClassDefStatement
