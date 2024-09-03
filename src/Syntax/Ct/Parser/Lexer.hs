{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Parser.Lexer
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines some of the simplest elements used during
-- parsing such as numbers and identifiers. It is used by the other
-- parsers.
--------------------------------------------------------------------

module Syntax.Ct.Parser.Lexer where

import           Control.Applicative        hiding (many, some)
import           Control.Monad              (void)
import           Data.Char                  (isPrint, isSpace)
import qualified Data.HashSet               as S
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

scn :: Parser ()
scn = L.space space1 empty empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc


  -- Parse simple sequences

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

angleBraces :: Parser a -> Parser a
angleBraces = between (symbol "<") (symbol ">")

squareBraces :: Parser a -> Parser a
squareBraces = between (symbol "[") (symbol "]")

curlyBraces :: Parser a -> Parser a
curlyBraces = between (symbol "{") (symbol "}")

-- TODO Avoid "//" in identifier. It's currently usable by separating by space
identifier :: Parser String
identifier = some (satisfy idChar)
  where
    idChar c = isPrint c && not (isSpace c) && not (c `S.member` invalidChars)
    invalidChars = S.fromList "-~:*+<>()[]{}=!&|.@_,?\""

pHole :: Parser String
pHole = do
  _ <- string "_"
  chars <- many alphaNumChar
  return ('_':chars)
