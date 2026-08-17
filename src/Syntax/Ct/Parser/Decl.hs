--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Parser.Decl
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is used to define function definitions and
-- declarations.
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Syntax.Ct.Parser.Decl where

import           Text.Megaparsec            hiding (pos1)
import qualified Text.Megaparsec.Char.Lexer as L

import           CtConstants
import           Data.List
import           Data.Maybe                 (isJust)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Builder
import           Syntax.Ct.Parser.Expr
import           Syntax.Ct.Parser.Lexer
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Text.Megaparsec.Char

pCompAnnot :: Parser PCompAnnot
pCompAnnot = do
  _ <- lookAhead $ string "#"
  pExpr

pComment :: Parser PCompAnnot
pComment = do
  pos1 <- getSourcePos
  scn
  ref <- L.indentLevel
  _ <- string "# "
  l <- takeLine
  ls <- pCommentBody ref
  let c = intercalate "\n" (l:ls)
  -- Compute pos2 from pos1 and content rather than using getSourcePos, because
  -- getSourcePos after the body's trailing scn includes whitespace that scn consumed.
  let cls = lines c
      endLineNum = unPos (sourceLine pos1) + length cls - 1
      endColNum  = if null cls then unPos (sourceColumn pos1) else 1 + length (last cls)
      pos2 = SourcePos (sourceName pos1) (mkPos endLineNum) (mkPos endColNum)
      m = emptyMeta pos1 pos2
  return (RawValue (emptyMetaM m) (mkName mdAnnot) `applyRawArgs` [(Just $ partialKey (mkName mdAnnotText), RawCExpr m (CStr c))])

takeLine :: Parser String
takeLine = takeWhileP (Just "character") (/= '\n')

-- | The lines under a comment, which are its writing: every line indented past the @#@ it is written
-- on, with the indentation of the first of them taken off each.
--
-- Taken off rather than required, so a line may be indented further than the first and keep the
-- difference. A comment is markdown, and markdown says with indentation what a nested list, a
-- continued item or an indented block is, so a comment whose every line had to sit at one
-- indentation could hold none of them — which is what @Text.Megaparsec.Char.Lexer.indentBlock@, an
-- indented block of ITEMS rather than of writing, requires.
pCommentBody :: Pos -> Parser [String]
pCommentBody ref = do
  -- The first line of the writing decides what its indentation is, as the first item of an indented
  -- block does. A comment with nothing under it is the one line it was written on.
  mlvl <- (optional . try) (eol *> L.indentGuard scn GT ref)
  case mlvl of
    Nothing  -> scn >> return []
    Just lvl -> go lvl
  where
    go lvl = do
      scn
      pos <- L.indentLevel
      done <- isJust <$> optional eof
      if done || pos <= ref
        then return []
        else if pos < lvl
          -- A line dedented out of the writing but not out of the comment is in neither, and is said
          -- rather than taken to be one or the other.
          then L.incorrectIndent EQ lvl pos
          else do
            l <- takeLine
            ((replicate (unPos pos - unPos lvl) ' ' ++ l) :) <$> go lvl

pDeclStatement :: Parser PStatement
pDeclStatement = do
  af <- pArrowFull FunctionObj
  case af of
    Left stmt -> return stmt
    Right roa -> return $ RawDeclStatement roa
