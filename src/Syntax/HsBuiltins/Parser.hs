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
-- What it declares is not written here either. Each declaration is derived from the shape of the
-- Haskell type it stands for ("Syntax.HsBuiltins.Derive"), by the same reading that converts a value
-- of that type to and from an evaluated one — so a builtin's name, its declaration, and what a
-- program is handed for it cannot come apart. The types themselves are in "Syntax.HsBuiltins.Types".
--------------------------------------------------------------------

module Syntax.HsBuiltins.Parser where

import           Data.List                (union)
import           Syntax.Ct.Builder
import           Syntax.Ct.Prgm
import           Syntax.HsBuiltins.Derive (CtPage (..))
import           Syntax.HsBuiltins.Types  (hsBuiltinPage)

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

-- | The imports the page writes: the core's compiling types, and whatever the types declared on the
-- page are written in terms of.
--
-- Written rather than left to 'Syntax.Parsers.pageImports', which gives no page of the standard
-- library the standard library. A comment is a @#md@ annotation ('Syntax.Ct.Builder.rawComment') and
-- @annot(#md(text -> String))@ is declared in @stack\/core\/compile.ct@, so a page holding one and
-- importing nothing would be a page written in an annotation nobody declared.
--
-- Named files rather than the core as a whole, because the core imports this page: importing the
-- core back would put the two in one cycle, where importing the files they both need does not.
--
-- The rest are asked of the types rather than listed here ('Syntax.HsBuiltins.Derive.cpImports'), so
-- that a builtin whose field is a list brings @core\/list.ct@ onto the page by being declared rather
-- than by someone remembering it.
hsBuiltinsImports :: [RawFileImport]
hsBuiltinsImports = map (mkRawFileImport . rawStr) $ ["core/compile.ct"] `union` cpImports hsBuiltinPage

-- | The page, which reads nothing and so is the same page every time it is read.
--
-- No source text, because the page is not one file to show or to write back. Nothing read, because
-- nothing was: a page which read nothing has nothing underneath it that could be written to, and so
-- is never read a second time.
hsBuiltinsParse :: ImportParseResult
hsBuiltinsParse = return $ ParsedImport (RawPrgm hsBuiltinsImports hsBuiltinsStatements) Nothing (Just [])

-- | What the page says about itself, and then what it declares.
--
-- The declarations are derived rather than written: each is read off the shape of the Haskell type
-- it stands for, by the same reading that converts a value of that type to and from an evaluated
-- one. See "Syntax.HsBuiltins.Derive".
hsBuiltinsStatements :: [RawStatementTree RawExpr ()]
hsBuiltinsStatements = map comment [
  "# The Catln Builtins",
  "These are the definitions the compiler supplies itself, rather than ones written in Catln.",
  "They are built in Haskell, in `Syntax.HsBuiltins.Types`, and reached by writing `import hsbuiltins()`. The standard library writes that import, so every program has them.",
  "Each is derived from the shape of the Haskell type it stands for, which is the same reading the runtime converts a value of that type by. So what a builtin is called here and what it is handed to a program as cannot disagree."
                                    ] ++ cpDecls hsBuiltinPage
  where
    comment text = RawStatementTree (RawAnnot (rawComment text)) []
