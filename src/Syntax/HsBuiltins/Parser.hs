--------------------------------------------------------------------
-- |
-- Module    :  Syntax.HsBuiltins.Parser
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- The page of definitions the compiler supplies itself.
--
-- @import hsbuiltins()@ is the one import which names nothing: no file, no directory, no module of
-- another language. What it reads is written here, in Haskell, and built as a program the way
-- 'Syntax.FileDir.Parser' and 'Syntax.Rss.Parser' build theirs — with the 'Syntax.Ct.Builder'
-- combinators rather than by writing Catln text and parsing it back.
--
-- It is an import rather than something the compiler folds into every program because a definition
-- has to belong to a page: a page is what the doc server displays, what a meta id is scoped by, and
-- what the standard library imports in order to have it. A definition with no page of its own would
-- be one nothing can name, show, or say where it came from.
--
-- For now the page holds only what it says about itself. What it is for is that the definitions
-- written into it later are decided here rather than in a @.ct@ file.
--------------------------------------------------------------------

module Syntax.HsBuiltins.Parser where

import           Syntax.Ct.Builder
import           Syntax.Ct.Prgm

-- | The name an import reads the compiler's own definitions with.
hsBuiltins :: String
hsBuiltins = "hsbuiltins"

-- | The page an @hsbuiltins()@ import stands at, written relative to the standard library root.
--
-- Nothing is there on disk, and nothing reads it — the path is the page's name rather than a file to
-- be opened. It is in the standard library because that is what the page is part of, and that is
-- also what keeps the standard library from being imported into it: 'Syntax.Parsers.pageImports'
-- gives every page the core except the core's own ('Syntax.Ct.Prgm.isCorePage'), and this is a page
-- the core imports.
hsBuiltinsPage :: FilePath
hsBuiltinsPage = "core/hsbuiltins.ct"

-- | Reading the definitions the compiler supplies, which is reading nothing at all.
--
-- No reversion: the page is not a file, so there is nothing it could be written back as. Its one
-- page is named here, since the import which reaches it names no path of its own.
hsBuiltinsSyntax :: ImportSyntax
hsBuiltinsSyntax = (eagerSyntax (const hsBuiltinsParse)){isPagePath = Just hsBuiltinsPage}

-- | The imports the page writes: the core's compiling types, which is where the annotations it is
-- written with are declared.
--
-- Written rather than left to 'Syntax.Parsers.pageImports', which gives no page of the standard
-- library the standard library. A comment is a @#md@ annotation ('Syntax.Ct.Builder.rawComment') and
-- @annot(#md(text -> String))@ is declared in @stack\/core\/compile.ct@, so a page holding one and
-- importing nothing would be a page written in an annotation nobody declared.
--
-- That one file rather than the core as a whole, because the core imports this page: importing the
-- core back would put the two in one cycle, where importing the file they both need does not.
hsBuiltinsImports :: [RawFileImport]
hsBuiltinsImports = [mkRawFileImport $ rawStr "core/compile.ct"]

-- | The page, which reads nothing and so is the same page every time it is read.
--
-- No source text, because the page is not one file to show or to write back. Nothing read, because
-- nothing was: a page which read nothing has nothing underneath it that could be written to, and so
-- is never read a second time.
hsBuiltinsParse :: ImportParseResult
hsBuiltinsParse = return $ ParsedImport (RawPrgm hsBuiltinsImports hsBuiltinsStatements) Nothing (Just [])

-- | What the page says about itself, until there is something in it to say.
hsBuiltinsStatements :: [RawStatementTree RawExpr ()]
hsBuiltinsStatements = map comment [
  "# The Catln Builtins",
  "These are the definitions the compiler supplies itself, rather than ones written in Catln.",
  "They are built in Haskell, in `Syntax.HsBuiltins.Parser`, and reached by writing `import hsbuiltins()`. The standard library writes that import, so every program has them.",
  "There are none yet. This page is where they will be written."
                                    ]
  where
    comment text = RawStatementTree (RawAnnot (rawComment text)) []
