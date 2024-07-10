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

import           Text.Megaparsec         hiding (pos1)

import           Syntax.Ct.Parser.Expr
import           Syntax.Ct.Parser.Lexer
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm

pExtends :: Parser (ExtendedClasses RawExpr ParseMetaDat)
pExtends = do
  _ <- symbol "isa"
  sepBy1 term (symbol ",")

pClassDefStatement :: Parser PStatement
pClassDefStatement = do
  _ <- symbol "every"
  instanceTerm <- term
  extends <- pExtends
  let def = (instanceTerm, extends)
  return $ RawClassDefStatement def

pTypeStatement :: Parser PStatement
pTypeStatement = pClassDefStatement
