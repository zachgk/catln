--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Parser
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is the main module for parsing. It will read in files
-- from their file paths and then parse into a 'RawPrgm'.
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Syntax.Ct.Parser where

import           Control.Applicative        hiding (many, some)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           CtConstants

import           Control.Monad              (when)
import           Data.Either                (lefts, rights)
import           Data.List                  (dropWhileEnd, intercalate)
import           Data.Maybe
import           Semantics.Prgm
import           Semantics.Types            (Constant (CStr), mkName,
                                             partialKey)
import           Syntax.Ct.Builder
import           Syntax.Ct.Parser.Decl
import           Syntax.Ct.Parser.Expr
import           Syntax.Ct.Parser.Lexer
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import qualified Text.Megaparsec.Char.Lexer as L

-- | An @import@ line. The keyword is followed by whitespace, so a name that only begins with it
-- (@importer(..)@) is still read as the statement it is.
pImport :: Parser RawFileImport
pImport = do
  _ <- try (string "import" <* lookAhead (char ' ' <|> char '\t'))
  sc
  imp <- term
  _ <- newline
  return $ mkRawFileImport imp

liftPStatement :: Parser PStatement -> Parser PStatementTree
liftPStatement pSt = L.indentBlock scn p
  where
    pack st children = return $ RawStatementTree st children
    p = do
      st <- pSt
      return (L.IndentMany Nothing (pack st) pStatementTree)

pCommentStatement :: Parser PStatementTree
pCommentStatement = do
  c <- pComment
  return $ RawStatementTree (RawAnnot c) []

-- | One @>>>@ line under an annotation: a line of what it resulted in.
pResultLine :: Parser String
pResultLine = do
  _ <- string ">>>"
  _ <- optional (char ' ')
  takeLine

-- | An annotation given the @>>>@ lines written under it, joined, as its 'resultAnnotText'.
withResultLines :: PCompAnnot -> [String] -> PCompAnnot
withResultLines annot [] = annot
withResultLines annot rls = annot `applyRawArgs` [(Just $ partialKey (mkName resultAnnotText), rawStr (intercalate "\n" rls))]

pPrintStatement :: Parser PStatementTree
pPrintStatement = L.indentBlock scn p
  where
    p = do
      _ <- string "> "
      e <- pExpr
      let annot = rawVal printAnnot `applyRawArgs` [(Just $ partialKey (mkName printAnnotText), e)]
      return (L.IndentMany Nothing (\rls -> return $ RawStatementTree (RawAnnot $ withResultLines annot rls) []) pResultLine)

-- | A line written @prefix text@, short for an annotation of one line of text: a question written
-- @q> text@ ('queryAnnot'), or a request of an agent written @a> text@ ('agentAnnot'). The rest of
-- the line, as written, is the annotation's input.
--
-- Unlike a print, the annotation is positioned at its line. Nothing else in it is, and both
-- answering it (the page it is asked about is the one its position names) and writing the answer
-- back (by that position) need to know where it is written.
pLineAnnotStatement :: String -> String -> Parser PStatementTree
pLineAnnotStatement prefix annotName = L.indentBlock scn p
  where
    p = do
      start <- getSourcePos
      _ <- try $ string prefix
      text <- takeLine
      end <- getSourcePos
      let m = emptyMeta start end
          annot = RawValue m (mkName annotName) `applyRawArgs` [(Just $ partialKey (mkName lineAnnotText), RawCExpr m (CStr $ dropWhileEnd (== ' ') text))]
      return (L.IndentMany Nothing (\rls -> return $ RawStatementTree (RawAnnot $ withResultLines annot rls) []) pResultLine)

-- | An annotation statement. The @>>>@ lines under it are its 'resultAnnotText', as under a print,
-- and the statements under it are its children.
pAnnotStatement :: Parser PStatementTree
pAnnotStatement = L.indentBlock scn p
  where
    p = do
      annot <- pCompAnnot
      return (L.IndentMany Nothing (pack annot) (Left <$> pResultLine <|> Right <$> pStatementTree))
    pack annot subs = do
      let rls = lefts subs
      when (not (null rls) && isJust (rawExprArgNamed resultAnnotText annot)) $
        fail "An annotation with a result argument can not also be given >>> result lines"
      return $ RawStatementTree (RawAnnot $ withResultLines annot rls) (rights subs)


pStatementTree :: Parser PStatementTree
pStatementTree = do
  notFollowedBy newline
  pCommentStatement
    <|> pLineAnnotStatement queryAnnotPrefix queryAnnot
    <|> pLineAnnotStatement agentAnnotPrefix agentAnnot
    <|> pPrintStatement
    <|> pAnnotStatement
    <|> liftPStatement pDeclStatement

pNothingNewline :: Parser (Maybe a)
pNothingNewline = do
  _ <- newline
  return Nothing

-- | A page: its imports and its statements. An @import@ may be written anywhere at the top level,
-- among the statements as well as before them (@annot(#noCore)@ is commonly written first), and
-- the imports are kept in the order they were written. Read only before the first statement, an
-- import written after one was taken for two statements — a bare @import@ and a bare string — and
-- the string crashed the desugarer.
pPrgm :: Parser PPrgm
pPrgm = do
  _ <- many newline
  items <- many (Just . Left <$> pImport <|> Just . Right <$> pStatementTree <|> pNothingNewline)
  let items' = catMaybes items
  return $ RawPrgm (lefts items') (rights items')

contents :: Parser a -> Parser a
contents p = do
  r <- p
  eof
  return r

-- TODO Write a .ct file back with 'Syntax.Ct.Formatter.formatRootPrgm', which is already the
-- program-to-text direction for this syntax.
ctSyntax :: ImportSyntax
ctSyntax = eagerSyntax $ \imp -> case exprAppliedArgs imp of
  [ObjArr{oaArr=Just (Just (CExpr _ (CStr fileName)), _)}] -> do
    fileContents <- readFile fileName
    case runParser (contents pPrgm) fileName fileContents of
      Left err   -> fail $ show $ errorBundlePretty err
      Right prgm -> return $ ParsedImport prgm (Just fileContents) (Just [ReadFile fileName])
  _ -> undefined

-- | A .ctx file is a .ct file with one annotation added, so it is read by a reader of those and
-- added to.
--
-- The reader it reads through is one it made rather than the one the read holds under @ct@: a
-- syntax which is another syntax added to is a syntax in its own right, and what the two share is
-- what the read gave both of them ('Syntax.Ct.Prgm.ReadEnv'). Asking the read for another syntax's
-- reader would make the table of syntaxes something a syntax reads out of while it is being built.
--
-- Whatever the inner reader has to write back is passed on rather than dropped, so that a syntax
-- read through is finished by the read which read through it.
--
-- TODO Write a .ctx file back, once 'ctSyntax' can be written back: it is a .ct file with the one
-- annotation this adds taken off again.
ctxSyntax :: ImportSyntax
ctxSyntax = ImportSyntax reader (const Nothing) importedLinkDir Nothing Nothing Nothing
  where
    reader env = do
      ct <- isReader ctSyntax env
      -- Updated rather than rebuilt, so the file this read is a read of travels with it without
      -- being named again here.
      let ctxParse fileName = (\parsed -> parsed{piPrgm = annotate (piPrgm parsed)}) <$> irParse ct fileName
      return $ readyReader (fmap eagerPreload . ctxParse) ctxParse (irFinish ct)
    annotate (RawPrgm imports statements) = RawPrgm imports (RawStatementTree (RawAnnot (RawValue emptyMetaN (mkName ctxAnnot))) [] :statements)

parseRepl :: String -> PReplRes
parseRepl s = case runParser (contents p) "<repl>" s of
                Left e@(ParseErrorBundle _ _) -> ReplErr e
                Right (Left statement)        -> ReplStatement statement
                Right (Right expr)            -> ReplExpr expr
  where p = try (Left <$> pStatementTree) <|> try (Right <$> pExpr)
