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
import qualified Data.HashMap.Strict     as H
import           Text.Megaparsec         hiding (pos1)

import           Control.Monad           (unless)
import           Data.Maybe
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Desugarf.Expr (exprToPartialType)
import           Syntax.Ct.Parser.Expr
import           Syntax.Ct.Parser.Lexer
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm


pMultiTerm :: Parser [PGuardExpr]
pMultiTerm = sepBy1 pTermWithPostCond (symbol "|")

pClassStatement :: Parser PStatement
pClassStatement = do
  _ <- symbol "class"
  clss@PartialType{ptName, ptArgs} <- exprToPartialType <$> term
  unless (H.null ptArgs) $ fail "Classes do not currently support arguments"
  let name = fromPartialName ptName
  maybeTypes <- optional $ do
    _ <- symbol "="
    MultiTypeDef clss <$> pMultiTerm
  return $ case maybeTypes of
    Just types -> MultiTypeDefStatement types (getPath name)
    Nothing    -> RawClassDeclStatement clss (getPath name)

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
  _ <- symbol "isa"
  className <- ttypeidentifier
  let def = (instanceTerm, className)
  return $ RawClassDefStatement def (getPath $ fromJust $ maybeExprPath instanceTerm)

pTypeStatement :: Parser PStatement
pTypeStatement = pClassStatement
                 <|> pAnnotDefStatement
                 <|> pTypeDefStatement
                 <|> pClassDefStatement
