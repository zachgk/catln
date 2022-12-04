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

reservedWords :: [String]
reservedWords = ["data", "type", "every", "isa", "module"]


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

identifier :: Parser String
identifier = try (p >>= check)
  where
    p       = ((:) <$> lowerChar <*> many (alphaNumChar <|> char '/')) <|> ttypeidentifier
    check x = if x `elem` reservedWords
                 then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                 else return x

tidentifier :: Parser String
tidentifier = lexeme $ (:) <$> upperChar <*> many (alphaNumChar <|> char '/')

ttypeidentifier :: Parser String
ttypeidentifier = try $ lexeme $ do
  first <- upperChar <|> char '/'
  rest <- case first of
    '/' -> tidentifier
    _   -> many (alphaNumChar <|> char '/')
  return $ first : rest

tvar :: Parser String
tvar = try $ lexeme $ do
  _ <- string "$"
  first <- upperChar
  rest <- many alphaNumChar
  return $ '$' : first : rest

pAnnotIdentifier :: Parser String
pAnnotIdentifier = do
  _ <- string "#"
  f <- letterChar
  rst <- many alphaNumChar
  return ('#':f:rst)

pHole :: Parser String
pHole = do
  _ <- string "_"
  chars <- many alphaNumChar
  return ('_':chars)
