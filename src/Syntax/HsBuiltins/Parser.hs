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
-- Haskell type it stands for ("Semantics.CtType"), by the same reading that converts a value
-- of that type to and from an evaluated one — so a builtin's name, its declaration, and what a
-- program is handed for it cannot come apart. The types themselves are in "Syntax.HsBuiltins.Types".
--------------------------------------------------------------------

module Syntax.HsBuiltins.Parser where

import           CtConstants
import           Data.List                (union)
import           Semantics.CtType         (CtPage (..), ctBuiltinNamespace)
import           Semantics.Prgm           (ObjectBasis (FunctionObj))
import           Semantics.Types          (mkName, partialKey)
import           Syntax.Ct.Builder
import           Syntax.Ct.Parser.Syntax  (PExpr)
import           Syntax.Ct.Prgm
import           Syntax.HsBuiltins.Render (renderCtDecls)
import           Syntax.HsBuiltins.Types  (hsBuiltinPage, hsPrgmPage,
                                           hsRawPrgmPage)

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
-- The rest are asked of the types rather than listed here ('Semantics.CtType.cpImports'), so
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
-- one. See "Semantics.CtType".
hsBuiltinsStatements :: [RawStatementTree RawExpr ()]
hsBuiltinsStatements = map commentSt [
  "# The Catln Builtins",
  "These are the definitions the compiler supplies itself, rather than ones written in Catln.",
  "They are built in Haskell, in `Syntax.HsBuiltins.Types`, and reached by writing `import hsbuiltins()`. The standard library writes that import, so every program has them.",
  "Each is derived from the shape of the Haskell type it stands for, which is the same reading the runtime converts a value of that type by. So what a builtin is called here and what it is handed to a program as cannot disagree."
                                    ] ++ renderCtDecls hsBuiltinPage

-- | One paragraph a page says about itself, as the statement carrying it.
commentSt :: String -> RawStatementTree RawExpr ()
commentSt text = RawStatementTree (RawAnnot (rawComment text)) []

-- | The name an import reads the compiler's own program types with.
hsPrgm :: String
hsPrgm = "hsprgm"

-- | The page an @hsprgm()@ import stands at.
--
-- Beside @core\/hsbuiltins.ct@ and not on it, because of who pays. Every program compiles the
-- builtins page — @stack\/core\/main.ct@ imports it — and these are dozens of mutually recursive
-- declarations rather than the two fields of a build result. A program which wants to read programs
-- writes @import hsprgm()@ and pays for them; one which does not is not slowed down by their being
-- available.
--
-- In @core\/@ for the same reason the other page is: that is what 'Syntax.Ct.Prgm.isCorePage'
-- answers from, and so what keeps 'Syntax.Parsers.pageImports' from handing this page the standard
-- library. Which matters more here than there, since this page reaches @core\/list.ct@ — the
-- standard library importing it back is exactly the cycle @stack\/core\/result.ct@'s comment
-- measures at 700 seconds. Nothing in @stack\/@ writes this import, and nothing in @stack\/@ should.
hsPrgmPagePath :: FilePath
hsPrgmPagePath = "core/hsprgm.ct"

hsPrgmSyntax :: ImportSyntax
hsPrgmSyntax = (eagerSyntax (const hsPrgmParse)){isPagePath = Just hsPrgmPagePath}

-- | What the page must import for its declarations to name something, asked of the types on it.
hsPrgmImports :: [RawFileImport]
hsPrgmImports = map (mkRawFileImport . rawStr) $ ["core/compile.ct"] `union` cpImports hsPrgmPage

hsPrgmParse :: ImportParseResult
hsPrgmParse = return $ ParsedImport (RawPrgm hsPrgmImports hsPrgmStatements) Nothing (Just [])

-- | What the page says about itself, and then what it declares.
hsPrgmStatements :: [RawStatementTree RawExpr ()]
hsPrgmStatements = map commentSt [
  "# The Catln Program",
  "These are the compiler's own types for the program it is compiling, declared so that a Catln program can be handed one and read it.",
  "They are reached by writing `import hsprgm()`. The standard library does not write that import and must not: these declarations reach `core/list.ct`, and a page every program compiles reaching it is the import cycle `core/result.ct` measures at seven hundred seconds. A program which wants to read programs asks for them and pays for them.",
  "Each is derived from the shape of the Haskell type the compiler itself uses, by the same reading that converts a value of that type to and from an evaluated one — so what a program is handed and what the compiler holds cannot come apart."
                                 ] ++ renderCtDecls hsPrgmPage ++ [hsPrgmForStatement]

-- | The one definition the page supplies rather than a type: a way to be given a program.
--
-- Written here rather than derived, because it is not a type read off a shape — it is the compiler
-- saying what it will answer. It belongs on this page and nowhere else: a page which does not
-- declare @\/Catln\/Prgm@ could not say what this returns, and a @.ct@ file which does would have to
-- be one no page of the standard library may import.
--
-- Narrowed to a name rather than the whole program, for the reason 'Eval.Runtime.Prgm.prgmFor'
-- gives: what a value's type costs to compute is what the value holds, so a program asks for what
-- it wants to read.
hsPrgmForStatement :: RawStatementTree RawExpr ()
hsPrgmForStatement = RawStatementTree (RawDeclStatement decl) (map commentSt [
  "Answers with the program being compiled, narrowed to the things written under [name].",
  "The program is the one the page asking was compiled as part of — that page and everything it reaches — so what is answered is what the asking program can see rather than everything there is.",
  "It is narrowed by [name] rather than handed over whole because what a value costs to work with is what it holds, and the standard library is a large thing to be handed in order to read one definition of it."
                                                                            ])
  where
    decl = RawObjArr (Just obj) FunctionObj Nothing [runtimeAnnotOf "prgmFor"] (Just (Nothing, Just returns)) Nothing
    obj = rawVal (ctBuiltinNamespace ++ "prgmFor") `applyRawIArgs` [(partialKey (mkName "name"), IArgM (Just (rawVal strPrim)))]
    -- Every value of the type, which is what a return annotation says and what the spread is for
    -- ('Syntax.Ct.Builder.rawSpreadVal'): a bare name is the argument-less partial application, and
    -- a program answered with carries its three fields.
    returns = rawSpreadVal (ctBuiltinNamespace ++ "Prgm")

-- | The @#runtime@ annotation naming which primitive answers a declaration.
runtimeAnnotOf :: String -> PExpr
runtimeAnnotOf key = rawVal runtimeAnnot `applyRawArgs` [(Just $ partialKey (mkName runtimeAnnotK), rawStr key)]

-- | The name an import reads the compiler's own program-as-written types with.
hsRawPrgm :: String
hsRawPrgm = "hsrawprgm"

-- | The page an @hsrawprgm()@ import stands at.
--
-- Below @core\/hsprgm.ct@ and not beside it, for the reason that page is below
-- @core\/hsbuiltins.ct@: what a page costs is paid by everything importing it. These are the raw
-- syntax — sixteen kinds of expression with the sugar still in them — where @hsprgm()@ is the six
-- kinds left after desugaring. A program which READS programs wants that page; only a program which
-- PRODUCES one, which is what an importer does, wants this.
--
-- In @core\/@ for the reason the other two are: that is what 'Syntax.Ct.Prgm.isCorePage' answers
-- from, and so what keeps 'Syntax.Parsers.pageImports' from handing this page the standard library.
-- No page of @stack\/core\/@ may write this import — this page reaches @core\/list.ct@ through
-- @hsprgm()@, and the core reaching back into it is the cycle @stack\/core\/result.ct@ measures at
-- 700 seconds.
hsRawPrgmPagePath :: FilePath
hsRawPrgmPagePath = "core/hsrawprgm.ct"

hsRawPrgmSyntax :: ImportSyntax
hsRawPrgmSyntax = (eagerSyntax (const hsRawPrgmParse)){isPagePath = Just hsRawPrgmPagePath}

-- | What the page must import: what its own declarations are written in terms of, and the page
-- declaring the types they share.
--
-- @hsprgm()@ is imported rather than the shared types being declared again here. A raw expression
-- carries a @\/Catln\/Meta@ and a @\/Catln\/Constant@, which that page already declares, and two
-- pages declaring one type is a program holding two of it. 'Syntax.HsBuiltins.Types.hsRawPrgmPage'
-- is seeded from the other page's 'Semantics.CtType.cpSeen' so that the walk leaves them alone.
hsRawPrgmImports :: [RawFileImport]
hsRawPrgmImports = map (mkRawFileImport . rawStr) (["core/compile.ct"] `union` cpImports hsRawPrgmPage `union` cpImports hsPrgmPage)
               ++ [mkRawFileImport (rawVal hsPrgm)]

hsRawPrgmParse :: ImportParseResult
hsRawPrgmParse = return $ ParsedImport (RawPrgm hsRawPrgmImports hsRawPrgmStatements) Nothing (Just [])

hsRawPrgmStatements :: [RawStatementTree RawExpr ()]
hsRawPrgmStatements = map commentSt [
  "# The Catln Program As Written",
  "These are the compiler's types for a program as the parser produced it, before desugaring reduces what was written to what it means. They are reached by writing `import hsrawprgm()`.",
  "Where `core/hsprgm.ct` declares the six kinds of expression a program has been reduced to, this declares the sixteen it was written as: a method call, a list, a format string, a bracket. Each of those becomes an application of the others once desugaring has run.",
  "So that page is what a program READING a program reads, and this is what a program PRODUCING one writes. An importer is the case: it is handed a file and answers with a `/Catln/ImportResult` holding a page of this.",
  "It is also what a page is written as, which is what a program showing a page as it was written reads: `/Catln/pageAt` answers with one.",
  "The core of the standard library does not write this import and must not, for the reason `core/hsprgm.ct` gives about itself. A page which needs it writes it and pays for it."
                                    ] ++ renderCtDecls hsRawPrgmPage ++ [hsPageAtStatement]

-- | The one definition the page supplies rather than a type: a way to be given a page as written.
--
-- Written here rather than derived, for the reason 'hsPrgmForStatement' is: it is the compiler saying
-- what it will answer, and a page that does not declare @\/Catln\/RawPrgm@ could not say what it
-- returns.
hsPageAtStatement :: RawStatementTree RawExpr ()
hsPageAtStatement = RawStatementTree (RawDeclStatement decl) (map commentSt [
  "Answers with the page read from [source], as it was written.",
  "The page is one the program being evaluated was read from, under the path its positions carry. A page the evaluation was not handed is reported rather than read, so what is answered is always what the program was compiled from."
                                                                          ])
  where
    decl = RawObjArr (Just obj) FunctionObj Nothing [runtimeAnnotOf "pageAt"] (Just (Nothing, Just returns)) Nothing
    obj = rawVal (ctBuiltinNamespace ++ "pageAt") `applyRawIArgs` [(partialKey (mkName "source"), IArgM (Just (rawVal strPrim)))]
    returns = rawSpreadVal (ctBuiltinNamespace ++ "RawPrgm")
