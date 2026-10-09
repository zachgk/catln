--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Prgm
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines most of the types that make up the Catln Syntax.
--------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}

module Syntax.Ct.Prgm where

import           Data.Hashable
import qualified Data.HashMap.Strict as H
import           GHC.Generics        (Generic)

import           Control.Exception   (IOException, try)
import           CtConstants
import           Data.Aeson          hiding (Object)
import           Data.IORef          (IORef, modifyIORef', newIORef, readIORef)
import           Data.List           (isInfixOf, isPrefixOf, sort, stripPrefix)
import           Data.Maybe          (catMaybes, fromJust, isJust, listToMaybe,
                                      mapMaybe)
import           Data.Time.Clock     (UTCTime)
import           Semantics
import           Semantics.Annots    (isAnnot)
import           Semantics.CtType    (CtType (..), ctBuiltinNamespace)
import           Semantics.Prgm
import           Semantics.Types
import           System.Directory    (doesDirectoryExist, getModificationTime,
                                      listDirectory, pathIsSymbolicLink)
import           System.FilePath     (splitDirectories, takeFileName, (</>))
import           Text.Printf
import           Utils

data RawObjArr e m = RawObjArr {
  roaObj    :: !(Maybe (e m)),
  roaBasis  :: !ObjectBasis,
  roaDoc    :: !(Maybe DocComment),
  roaAnnots :: ![CompAnnot (e m)],
  roaArr    :: !(Maybe (Maybe (e m), Maybe (e m))), -- (arrExpr, arrMetaExpr for formatter)
  roaDef    :: !(Maybe (e m))
                               }
  deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)

-- | The fields below are written in record syntax, and so are every raw type down to 'RawPrgm'.
--
-- Not for anything Haskell wants — nothing here is built or matched by field — but because a Catln
-- argument is named and a positional constructor's arguments are not. 'Semantics.CtType' refuses to
-- invent names ("Semantics.CtType", the @GCtFields@ instance for an unnamed selector), so a type the
-- compiler wants to hand a Catln program has to say what its arguments are called where the
-- constructor is written, which is also the only place the two cannot drift apart. Naming them is
-- additive: positional construction and positional patterns are unchanged Haskell, so the several
-- hundred sites building and matching a 'RawExpr' compile untouched.
--
-- The prefix is dropped when the name is read ('Semantics.CtType.ctDropFieldPrefix'), so @tppName@
-- is declared as @name@. It must be entirely lowercase up to the first capital for that to work.
data TypeProperty e m
  = TypePropProj { tppName :: TypeName, tppExpr :: e m }
  | TypePropRel { tprName :: TypeName, tprExpr :: e m }
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

data RawApplyTerm e m = RATermDeep { ratdExpr :: e m } | RATermChild { ratcExpr :: e m }
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)
newtype RawApply e m = RawApply { raTerms :: [RawApplyTerm e m] }
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

-- Expr before desugar
data RawExpr m
  = RawCExpr { rceMeta :: Meta m, rceConst :: Constant }
  | RawValue { rvMeta :: Meta m, rvName :: TypeName }
  | RawHoleExpr { rheMeta :: Meta m, rheHole :: Hole }
  | RawMacroValue { rmvMeta :: Meta m, rmvName :: TypeName }
  | RawApplyExpr { raeMeta :: Meta m, raeApply :: RawApply RawExpr m }
  -- | The name a format string is written with, and the string itself.
  | RawFmtStrExpr { rfsMeta :: Meta m, rfsName :: String, rfsStr :: String }
  | RawTheExpr { rteExpr :: RawExpr m } -- ^ Written :TypeName and read as The TypeName
  | RawAliasExpr { raliasBase :: RawExpr m, raliasAlias :: RawExpr m } -- ^ base aliasExpr
  | RawTupleApply { rtaMeta :: Meta m, rtaBase :: (Meta m, RawExpr m), rtaArgs :: [(Bool, RawObjArr RawExpr m)] } -- Boolean for isSpreadArg
  | RawVarsApply { rvaMeta :: Meta m, rvaBase :: RawExpr m, rvaVars :: [RawObjArr RawExpr m] }
  | RawContextApply { rcaMeta :: Meta m, rcaBase :: (Meta m, RawExpr m), rcaArgs :: [RawObjArr RawExpr m] }
  | RawWhere { rwMeta :: Meta m, rwBase :: RawExpr m, rwCond :: RawExpr m } -- ^ base cond
  | RawParen { rpnExpr :: RawExpr m }
  | RawMethod { rmMeta :: Meta m, rmBase :: RawExpr m, rmValue :: RawExpr m } -- ^ base methodValue
  | RawList { rlMeta :: Meta m, rlValues :: [RawExpr m] }
  | RawTypeProp { rtpMeta :: Meta m, rtpBase :: RawExpr m, rtpProp :: TypeProperty RawExpr m }
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

data RawStatement e m
  = RawDeclStatement { rdsObjArr :: RawObjArr e m }
  | RawBindStatement { rbsObjArr :: RawObjArr e m } -- Uses <-
  | RawAnnot { ranAnnot :: CompAnnot (e m) }
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

data RawStatementTree e m = RawStatementTree { rstStatement :: RawStatement e m, rstChildren :: [RawStatementTree e m] }
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

type RawFileImport = AFileImport (RawExpr ())
-- | 'rprgmImports' rather than @rpImports@, which 'ReadPage' below already takes in this module.
data RawPrgm m = RawPrgm { rprgmImports :: [RawFileImport], rprgmStatements :: [RawStatementTree RawExpr m] } -- TODO: Include [Export]
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

-- | The program as the parser produced it, read as a Catln type.
--
-- These are what @hsrawprgm()@ declares, and they are the OTHER side of what @hsprgm()@ declares.
-- That page is the program the compiler has finished reducing — six kinds of expression, which is
-- what a program that rewrites programs works on. This is the program as it was written, sixteen
-- kinds of expression including the sugar, which is what a program that PRODUCES one has to build.
-- An importer is the case: it is handed a file and answers with a page of this.
--
-- Declared on a page of their own rather than added to @hsprgm()@ for the reason that page is
-- separate from @hsbuiltins()@ — see 'Syntax.HsBuiltins.Types.hsPrgmTypes'. A program which reads
-- programs need not pay for the syntax it never sees, and a program which writes one asks for it.
--
-- The instances are here rather than in "Syntax.HsBuiltins.Types" because a type says how it is read
-- as a Catln one in the file it is declared in, so a constructor added to one of these cannot fail
-- to reach the page. 'AFileImport' is the exception and is an orphan below, since it is declared in
-- "Semantics.Prgm", which cannot name a 'RawExpr'.
instance CtType (RawExpr ()) where
  ctDoc _ = [
    "An expression as it was written, before the compiler reduces what was written to what it means.",
    "Where a [/Catln/Expr] is six arms, this is sixteen: the sugar is still here. A [RawMethod] is `a.b`, a [RawList] is `[a, b]`, a [RawFmtStrExpr] is a format string, a [RawTheExpr] is `:Name`, and a [RawParen] is a bracket that was written. Each of them becomes a [TupleApply] of something once desugaring is done.",
    "So this is what a program BUILDING a program writes, and [/Catln/Expr] is what a program READING one reads. An importer writes this."
            ]

instance CtType (TypeProperty RawExpr ()) where
  ctDoc _ = [
    "A property written about a type: a projection ([TypePropProj]) or a relation ([TypePropRel]), each by [name] and the [expr] it is written over."
            ]

instance CtType (RawApplyTerm RawExpr ())
instance CtType (RawApply RawExpr ()) where
  ctDoc _ = [
    "The terms of an `apply` statement, which is how a program written in Catln rewrites itself."
            ]

instance CtType (RawObjArr RawExpr ()) where
  ctDoc _ = [
    "One thing written in a program, before desugaring: the [obj] written on the left and the [arr] it becomes.",
    "The written counterpart of [/Catln/ObjArr]. [arr] is a [Pair] of what it becomes and the type it was annotated with, either of which may be absent — a declaration writes the type and no value, a definition writes the value and no type, and a pattern writes neither.",
    "[def] is a default written for an argument, which nothing after desugaring carries."
            ]

instance CtType (RawStatement RawExpr ()) where
  ctDoc _ = [
    "One statement of a page: a declaration, a bind written with `<-`, or an annotation.",
    "A [RawAnnot] is how a page's comments reach the program, since a doc comment is written as a [#md] annotation."
            ]

instance CtType (RawStatementTree RawExpr ()) where
  ctDoc _ = [
    "A statement and the statements written underneath it.",
    "Catln is written by indentation, so a page is a forest rather than a list: what is indented under a declaration belongs to it, and what is indented under an annotation is annotated by it."
            ]

instance CtType (RawPrgm ()) where
  ctDoc _ = [
    "A page as it was written: the files it [imports] and the [statements] it holds.",
    "This is what reading a file produces, and so what an importer written in Catln answers with. The compiler takes it from there — desugaring it, typechecking it and building it exactly as it would a page it had parsed itself."
            ]

-- | An import as it is written, read as a Catln type.
--
-- An orphan, and it has to be: 'AFileImport' is declared in "Semantics.Prgm", which sits below this
-- module and cannot name the 'RawExpr' this is an import of.
instance CtType (AFileImport (RawExpr ())) where
  ctDoc _ = [
    "One `import` line: the expression it was written as ([raw]), the expression it resolved to ([abs]), and where that landed.",
    "[abs] is the identity of the page imported — two imports naming the same page are the same import, however each was written — and [raw] is what the file says, which is what the formatter writes back.",
    "[disp] is the path the page is displayed at, [calledDir] the directory the import was written in, and [dir] the directory the page itself sits in, which is what a relative import inside it resolves against."
            ]

data ReplRes m
  = ReplStatement (RawStatementTree RawExpr m)
  | ReplExpr (RawExpr m)
  | ReplErr ParseErrorRes
  deriving (Eq, Show)

-- | Something on disk a reading depended on.
--
-- A reading which walked a tree says so, rather than naming every directory it walked. Both are the
-- same question — has anything the reading looked at been written to — but a reading which holds
-- what a whole tree holds ('Syntax.Obsidian.Parser.vaultPrgm' holds every note of a vault) would
-- otherwise name every directory of that tree, which is a directory named once for each of them.
--
-- What was read is said apart from where it was, because the two are answered differently and only
-- the reading knows which it did. A file is written to when its text changes; a file a reading only
-- found is a different thing again, and stops being what the reading found only when it is no
-- longer there; a directory is written to when an entry appears in it, goes from it or is renamed
-- within it. Nothing on disk tells them apart without being asked, so a reading which said only a
-- path would have every reader of it ask — 'CtService.readPathWatches' would have to look at the
-- disk to know what to watch, and a note would be read again every time a note it links to was
-- typed into.
data ReadPath
  -- | A file whose text was read.
  = ReadFile { rfPath :: FilePath }
  -- | A file a reading found without reading it, and would have done something else without.
  -- 'Syntax.Obsidian.Parser.notePrgm' resolves each of a note's wiki links to a note of the vault
  -- and imports it, which is a reading of that note's name and of nothing inside it.
  | ReadExists { rePath :: FilePath }
  -- | A directory whose listing was read.
  | ReadDir { rdPath :: FilePath }
  -- | A directory and the listings under it, at any depth.
  | ReadTree { rtPath :: FilePath }
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | Something a reading looked at, read as a Catln type.
--
-- Declared so that an importer written in Catln can say what it read. It has to be able to: a
-- reading which cannot say is one whose page is read again on every read
-- ('Syntax.Parsers.readTimeCurrent'), which for an importer means being evaluated again on every
-- read of the service.
instance CtType ReadPath where
  ctDoc _ = [
    "Something a reading of a page looked at, and how it looked at it.",
    "A [ReadFile] is a file whose text was read; a [ReadExists] is one a reading only found, which a write to does not change what was read. A [ReadDir] is a directory whose listing was read, and a [ReadTree] is a directory and everything under it.",
    "A page is read again exactly when one of these is written to, so an importer which leaves one out is one whose page goes stale, and one which reports more than it read is a page read more often than it need be."
            ]

-- | Whether a reading of a tree walks into an entry named this.
--
-- One rule in one place because three things must agree on it, and nothing but agreement makes them
-- right: what a tree read walks and is timed by ('walkTree', which is both), what writing to a tree
-- means ('readsUnder', through 'Syntax.Parsers.readPathWrittenIn'), and what a watch on a tree
-- reports ('CtService.ctssWatch'). A path one of them counts and another does not is at best a page
-- read again for a change it never read, and at worst a page never read again for one it did.
--
-- Entries named with a leading dot are passed over. An Obsidian vault keeps its own configuration
-- in one and rewrites it as the vault is used, which is not notes of the vault; a repository keeps
-- its history in another.
readsTreeEntry :: FilePath -> Bool
readsTreeEntry entry = not $ "." `isPrefixOf` takeFileName entry

-- | Whether a reading of the tree under a directory reads at a path.
--
-- Every part of the way down is asked, not just the last, because a reading walks: a directory it
-- never walked into is one whose every entry it never saw.
--
-- Compared a directory at a time rather than as text, or a vault at @notes@ would be read from
-- every write to a @notes-archive@ beside it.
readsUnder :: FilePath -> FilePath -> Bool
readsUnder dir path = case stripPrefix (splitDirectories dir) (splitDirectories path) of
  Just under -> all readsTreeEntry under
  Nothing    -> False

-- | When a path was last written, or nothing where there is no path to have been written.
modifiedAt :: FilePath -> IO (Maybe UTCTime)
modifiedAt file = either (const Nothing) Just <$> (try (getModificationTime file) :: IO (Either IOException UTCTime))

-- | What one walk of the tree under a directory found.
data TreeScan = TreeScan {
  -- | The files a reading of the tree reads, in the order it walks them.
  tsFiles   :: [FilePath],
  -- | When any of the directories it walked was last written to, and 'Nothing' where it walked
  -- none. A directory is written to when an entry appears in it, goes from it or is renamed within
  -- it, and not when a file already in it is edited, so this is when the tree last gained or lost
  -- anything — which is what a reading of its listings depends on, and no more.
  tsWritten :: Maybe UTCTime
                         }

-- | What a read gives every syntax it reads with: what it has already found on disk, and where a
-- syntax which keeps a record between runs may keep one.
--
-- Everything here is shared, either because more than one syntax could want it or because two
-- things which want it must be given the one answer. What only one syntax wants belongs to that
-- syntax's reader ('ImportReader'), which is why the record of Obsidian vaults is no longer named
-- here.
-- | An importer a page declares: a way of reading files written in some other syntax.
--
-- Read off the raw statements of a page rather than off a compiled one, which is what the
-- @#importer@ annotation is for. Deciding which importer reads a file happens while the imports are
-- being resolved, long before anything has been desugared, so the answer has to be available from a
-- parse alone.
data ImporterDecl = ImporterDecl {
  -- | The page the declaration is written on, which is the page that must be built before the
  -- importer can be run.
  idPage :: FileImport,
  -- | The name the declaration is written under, as written: @parsePython@ rather than
  -- @\/Python\/parsePython@. Relative because the evaluator resolves it
  -- ('Eval.evalDShowCall' expands a relative name), and because the absolute name is not known
  -- until the page has been desugared — which is after this is needed.
  idFn   :: String,
  -- | The name an @import@ writes at its head to be read with this: the @python@ of
  -- @import python("x.py")@.
  idName :: String,
  -- | The file extensions a file is read with this importer by, without their dots.
  idExts :: [String]
                                 }
  deriving (Eq, Show)

-- | Why an importer could not be run yet.
--
-- Not an error. An importer's page has to be BUILT before the importer can be called, and a read
-- cannot build — it holds the read lock, and building takes it again. So a page whose importer is
-- not ready is left unread, and whoever is driving the read builds the importer and reads again
-- ('CtService.ctssLoadOf'). This says which page it is waiting on so that the driver knows what to
-- build.
-- | The page is named by its path rather than as a 'FileImport'. A native import carries the
-- importer's path as a string in its own expression, since that is all an import expression can
-- hold, and the reader which asks about it has only that string to ask with — turning it back into
-- a 'FileImport' is 'Syntax.Parsers.canonicalImport''s job and is above this module.
data ImportBlocked = ImportBlocked {
  ibPage :: FilePath,
  ibWhy  :: String
                                   }
  deriving (Eq, Show)

-- | What a read can ask the compiler about importers written in Catln.
--
-- A record of callbacks rather than the compilation service itself, because the service sits above
-- this module — "CtService" imports "Syntax.Parsers", so nothing here may name a @CTSS@. The service
-- builds one of these over itself and hands it down.
--
-- Everything here is answered out of what the service ALREADY holds. None of it reads a file, builds
-- a page, or takes a lock the read is running inside: a read holds 'CtService.ctssReadLock' for its
-- whole walk, and an MVar is not re-entrant, so an answer that went and built something would
-- deadlock rather than be slow.
data ImporterEnv = ImporterEnv {
  -- | The importers declared directly on these pages, in the order the pages are given.
  --
  -- Directly, not transitively: an importer must be imported to be used. The alternative walks the
  -- whole import closure of every page, and since every page imports the standard library, that is
  -- the standard library walked once per import resolved.
  ieDeclared :: [FileImport] -> IO [ImporterDecl],
  -- | The page a file reached with no import line above it looks on for its importer.
  --
  -- A root file the compiler was given, or a file found in a directory listing, has no preceding
  -- imports to search. This is the standard library's registry of importers, and it is the ONLY
  -- place an extension is resolved against — see 'Syntax.InferImport.inferFileParser' for why
  -- resolving an extension against the caller's imports would make one file into two pages.
  ieRegistry :: IO (Maybe FileImport),
  -- | The file extensions the importers of that registry read, without their dots.
  --
  -- Asked by 'Syntax.InferImport.isSupportedFileExtension', which decides whether a file found in a
  -- directory's listing is one to read at all. Without it that question is answered from
  -- 'Syntax.InferImport.fileExtensionParsers' alone — the syntaxes the compiler supplies — so a file
  -- an importer reads is skipped, and a directory of Python or of JSON lists none of it.
  --
  -- The extensions rather than the declarations, because that is the whole of what the asking needs
  -- and the rest of a declaration is of no use to it. A page is still read by the importer the
  -- registry names ('Syntax.Parsers.registryImport'); this only decides whether the read is
  -- attempted.
  ieExtensions :: IO [String],
  -- | Whether the page at this path is built and can answer, and why not where it cannot.
  --
  -- Apart from 'ieInvoke' rather than being found out by trying, because the two are asked at
  -- different moments and one of them must be cheap. A read asks this of every native page it
  -- reaches, to decide whether to read the page at all ('irBlocked'); running the importer to find
  -- out would run it twice for every page that is ready, and the whole cost of an importer is
  -- running it.
  ieReady    :: FilePath -> IO (Maybe ImportBlocked),
  -- | Run an importer whose page is already built, over the path it is to read.
  --
  -- The importer's page, the function's written name, the path to read, and whatever named
  -- arguments the import was written with. Answers 'ImportBlocked' where the page turned out not to
  -- be ready after all, which is the deferral above rather than a failure — 'ieReady' is asked
  -- first, but nothing holds a lock between the two.
  ieInvoke   :: FilePath -> String -> String -> [(String, String)] -> IO (Either ImportBlocked ParsedImport)
                               }

-- | A read with no compiler behind it, which reads the syntaxes the compiler supplies and no others.
--
-- What 'Syntax.Parsers.parseFile' and 'Syntax.Parsers.parseImportAlone' are given: the formatter and
-- @catln convert@ hold one page rather than a program, and have no service to ask. An import naming
-- a Catln-declared importer is reported there rather than read, which is what 'ieInvoke' says.
noImporters :: ImporterEnv
noImporters = ImporterEnv{
  ieDeclared = const $ return [],
  ieRegistry = return Nothing,
  -- No compiler, so no registry to read and no importer to name. Such a read sees the extensions
  -- the compiler supplies and no others, which is what the formatter and @catln convert@ already
  -- did before an importer could add one.
  ieExtensions = return [],
  ieReady = return . Just . noCompiler,
  ieInvoke = \page _ _ _ -> return $ Left $ noCompiler page
                         }
  where
    noCompiler page = ImportBlocked page "reading a page with a Catln-declared importer needs the compiler, which this command does not start"

-- | Where one import is being resolved: what wrote it, what was written above it, and what may be
-- asked about importers.
--
-- The imports above it are what an importer is looked up in. An @import@ line may name an importer
-- declared by a page imported on an earlier line, which is what lets a file bring in an importer and
-- then use it. Only the lines ABOVE, so that what an import means is decided by what has been said
-- before it rather than by the whole file.
data ImportCtx = ImportCtx {
  icCaller    :: Maybe FileImport,
  icPreceding :: [FileImport],
  icImporters :: ImporterEnv
                           }

-- | Resolving an import that no page wrote: one the compiler was given, or one a syntax built.
rootImportCtx :: ImporterEnv -> ImportCtx
rootImportCtx = ImportCtx Nothing []

-- | Resolving an import written on a page, with nothing above it yet.
pageImportCtx :: ImporterEnv -> FileImport -> ImportCtx
pageImportCtx importers caller = ImportCtx (Just caller) [] importers

data ReadEnv = ReadEnv {
  -- | The trees this read has walked.
  --
  -- A read reads many pages out of the one tree. Every note of an Obsidian vault resolves its links
  -- against the notes of that vault, and the page which is the vault holds all of them, so a vault
  -- of a thousand notes would be walked a thousand and one times by a read which walked it for
  -- each.
  --
  -- Shared rather than held by the syntax which reads the notes, because two things read it: what a
  -- vault holds ('Syntax.Obsidian.Parser.vaultNotes'), and when a tree was last written to
  -- ('Syntax.Parsers.treeWrittenAt'). A syntax walking for itself would be a read whose links
  -- resolved against one walk and whose page was timed by another, and a note appearing between the
  -- two would be recorded as seen by a read which never read it — a page that goes stale, rather
  -- than a tree walked twice.
  --
  -- Scoped to one read rather than kept between reads. What is on disk changes between reads, and
  -- deciding that it has is what 'ReadPath' and the times a read records already are.
  reScans     :: IORef (H.HashMap FilePath TreeScan),
  -- | Where a syntax which can answer for a page out of what an earlier run recorded keeps that
  -- record, and 'Nothing' for a read which is to read every page it reaches.
  --
  -- The root alone. What is written under it, whether anything is, and whether what is written
  -- still holds are the syntax's own — see 'Syntax.Obsidian.Cache', which is one syntax's record
  -- and not a table of everyone's.
  reCacheRoot :: Maybe FilePath,
  -- | What this read may ask the compiler about importers written in Catln.
  --
  -- Here rather than on 'Syntax.Parsers.Reading' because a syntax is handed this and not that: a
  -- reader is made from a 'ReadEnv' ('isReader'), and the syntax which decides whether a file of a
  -- directory is one to read at all ('Syntax.InferImport.isSupportedFileExtension', reached from
  -- @dirPreload@) has to be able to ask which extensions a declared importer handles.
  reImporters :: ImporterEnv
                       }

-- | A read which reads every page it reaches, keeping and consulting no record.
newReadEnv :: IO ReadEnv
newReadEnv = newRecordedReadEnv Nothing noImporters

-- | A read which may answer for a page out of what an earlier run recorded, and which may ask the
-- compiler about importers written in Catln.
newRecordedReadEnv :: Maybe FilePath -> ImporterEnv -> IO ReadEnv
newRecordedReadEnv cacheRoot importers = do
  scans <- newIORef H.empty
  return $ ReadEnv scans cacheRoot importers

-- | The walk of the tree under a directory this read made, walking it if this read has not.
--
-- Only the directory asked about is kept, not every directory walked under it: what asks is a page
-- read from a whole tree, and keeping each level would be the tree held once for every depth in it.
scanTree :: ReadEnv -> FilePath -> IO TreeScan
scanTree ReadEnv{reScans=scans} dir = do
  found <- readIORef scans
  case H.lookup dir found of
    Just scan -> return scan
    Nothing -> do
      scan <- walkTree dir
      modifyIORef' scans $ H.insert dir scan
      return scan

-- | The walk itself.
--
-- What is walked into is 'readsTreeEntry', and a symlinked directory is not walked at all: a tree
-- holding one which points at a directory above it would otherwise be walked forever.
--
-- Sorted, because 'listDirectory' answers in whatever order the filesystem holds it and what this
-- finds becomes the imports of the page read from the tree — a part of the program that re-reading
-- an unchanged tree must reproduce, or every read takes itself for an edit.
--
-- A directory which cannot be listed is a failure rather than an empty directory, because a reading
-- of a tree either read it or did not: a vault which quietly lost the notes under an unreadable
-- directory would drop them from itself and break every link into them.
walkTree :: FilePath -> IO TreeScan
walkTree dir = do
  here <- modifiedAt dir
  entries <- listDirectory dir
  under <- mapM aux $ sort $ filter readsTreeEntry entries
  return $ TreeScan (concatMap tsFiles under) (latest $ here : map tsWritten under)
  where
    aux entry = do
      let path = dir </> entry
      isDir <- doesDirectoryExist path
      isLink <- pathIsSymbolicLink path
      if isDir
        then if isLink then return (TreeScan [] Nothing) else walkTree path
        else return $ TreeScan [path] Nothing
    latest times = case catMaybes times of
      [] -> Nothing
      ts -> Just $ maximum ts

-- | Everything on disk a reading depended on: the files it read, and the directories and trees
-- whose listing decided what it found. A page is read again exactly when one of these is written
-- to, so one left out is a page which goes stale, and one put in needlessly is a page read more
-- often than it need be. A directory is written to when an entry is added to it, removed from it or
-- renamed within it, which is every way a listing of it can change.
--
-- 'Nothing' is a reading which cannot say what it read, and has its page read again every time
-- rather than have a change to it go unseen.
type ReadPaths = Maybe [ReadPath]

-- | A page read to the program it holds.
data ParsedImport = ParsedImport {
  piPrgm   :: RawPrgm (),
  -- | The text the page was read from, where the page is one file and so has one to show.
  piSource :: Maybe String,
  piReads  :: ReadPaths
                                 }
  deriving (Generic)

-- | What reading a page produced, read as a Catln type. This is what an importer written in Catln
-- answers with, and it is the same record the compiler's own parsers answer with rather than a
-- second shape kept in step with it.
--
-- Called @\/Catln\/ImportResult@ on the page though the Haskell type is 'ParsedImport', which is
-- what 'ctPath' is overridable for — the Haskell name says it came from a parse, and the Catln name
-- says what an importer is being asked for.
--
-- All three fields matter and none may be dropped. Without [reads] the page is read again on every
-- read of the service, so the importer is evaluated again every time; without [source] a natively
-- read page has no text for the doc server to show or the editor to save.
-- | The type an importer answers with, as a caller building a call to one names it.
--
-- @PtArgAny@ rather than the @PtArgExact@ 'Semantics.Types.partialVal' gives, and the difference is
-- not a detail: an exact partial listing no arguments is the type of an @ImportResult@ carrying NO
-- arguments, which nothing is. A call answering one with three was reported as not reaching the
-- destination at all, which reads like the tree builder refusing to evaluate and is really the
-- destination naming a type nothing inhabits. What a caller means is an @ImportResult@ whatever it
-- holds.
importResultType :: Type
importResultType = singletonType (partialVal (mkName $ ctBuiltinNamespace ++ "ImportResult")){ptArgMode = PtArgMode True False}

instance CtType ParsedImport where
  ctPath _ = ctBuiltinNamespace ++ "ImportResult"
  ctDoc _ = [
    "What reading a file produced: the [prgm] it holds, the [source] text it was read from, and what the reading [reads].",
    "[source] is absent for a page which is not one file, and so has no single text to show. [reads] is absent for a reading which cannot say what it looked at, which is a page read again every time — an importer should say, and answer with the file it opened.",
    "This is what an `#importer` declaration answers with. The compiler takes the program from here and desugars, typechecks and builds it exactly as it would a page it had parsed itself."
            ]

-- | Where a page sits in the import graph, and the program it holds where reading it was the only
-- way to say.
data PreloadImport = PreloadImport {
  -- | The imports the page writes: its edges in the graph.
  plImports      :: [RawFileImport],
  -- | Pages the reading found which the program does not itself import: the files of a directory,
  -- the notes of an Obsidian vault.
  plExtraImports :: [RawFileImport],
  -- | The program, where the only way to say what the page imports was to read it.
  --
  -- A syntax which answers 'Nothing' leaves its page in the graph with its edges and without its
  -- program, to be read the rest of the way when something asks what is in it
  -- ('CtService.ctssLoad'). That is what lets a reader take up the shape of a program without
  -- reading all of it.
  --
  -- A syntax whose pages can write @#noCore@ must not answer 'Nothing'. Whether to give a page the
  -- standard library is read out of the program ('Syntax.Parsers.pageImports'), so a page whose
  -- program has not been read is given it — which is right for every page that does not turn it
  -- down, and is the whole of the standard library wrongly imported by one that does.
  plParsed       :: Maybe ParsedImport,
  plReads        :: ReadPaths
                                   }

-- | When the files a page was read from were last written, as the read that read them found them.
data ReadTime = ReadTime {
  -- | When the read began.
  --
  -- A file written since then is not one these times can vouch for: the read may have read it
  -- before the write, and a time taken after the read cannot tell the two apart. Such a file is
  -- taken as changed, which costs one further read of the page and never a page left stale.
  rtStarted :: UTCTime,
  -- | When each thing the read looked at was last written, and 'Nothing' where nothing was there. A
  -- path which is still not there has not changed — a page read from a file yet to be written reads
  -- back the same both times — so an absent path is recorded rather than passed over.
  rtPaths   :: [(ReadPath, Maybe UTCTime)]
                         }
  deriving (Eq, Show)

-- | A page as a read of the files left it.
--
-- Held by whoever keeps the pages ('Eval.Common.ssfRead') and handed back to the next
-- 'Syntax.Parsers.readFiles', which gives the page back rather than reading its files again when
-- nothing it was read from has been written to.
--
-- A page given back is given back whole, imports included, so an import is resolved where the page
-- was read rather than at every read of it. A file appearing where an import used to resolve
-- elsewhere is therefore not noticed until the page which writes that import is read again. The
-- files a service was given are resolved afresh on every read, and a directory holding a new file
-- is read again because the directory was written to, so this is confined to an @import@ written in
-- a page whose own files have not changed.
--
-- It sits here beside the 'ParsedImport' and 'PreloadImport' a read is assembled from, rather than
-- in the module that assembles them, because the page a read leaves is held by the compilation
-- service and so has to be nameable below it.
data ReadPage = ReadPage {
  -- | The program the page holds, or 'Nothing' for a page whose imports are known and whose program
  -- has not been read ('plParsed'). Such a page is in the graph with its edges, and is read the
  -- rest of the way when something asks what is in it ('CtService.ctssLoad').
  rpPrgm    :: Maybe (RawPrgm ()),
  rpSource  :: Maybe String,
  -- | The imports the page writes: its edges in the import graph.
  rpImports :: [FileImport],
  -- | Pages the read found which the page does not import ('plExtraImports'): the files of a
  -- directory, the notes of a vault. The read walks on to these as well as to the edges, so a page
  -- given back that replayed only its edges would drop every file of a directory, and every note of
  -- a vault, out of the graph.
  rpExtras  :: [FileImport],
  -- | 'Nothing' where nothing timed the read, which is a page that is always read again: either the
  -- parser could not say what it read ('ReadPaths') or the read was not one
  -- 'Syntax.Parsers.readFiles' made.
  rpReads   :: Maybe ReadTime,
  -- | The pages this one cannot be read until something has BUILT ('irBlocked'), and empty for the
  -- ordinary page which can be read now.
  --
  -- A page with these is in the graph with its edges and without its program, exactly as a page
  -- whose syntax could say what it imports without reading it is ('rpPrgm'). The difference is why:
  -- that page has not been asked for yet, and this one cannot be answered yet. Whoever is driving
  -- the read builds these and reads again ('CtService.ctssLoadOf').
  rpBlocked :: [FileImport]
                         }

-- | Every page the read walks on to from this one.
--
-- The pages it is waiting on are among them, so that a page which cannot be read yet still puts what
-- it is waiting for into the graph. Without that the importer's page would be reachable from
-- nothing, and a build of the page that needs it would not build it.
readPageVisits :: ReadPage -> [FileImport]
readPageVisits page = rpExtras page ++ rpImports page ++ rpBlocked page

-- | The path an import names, where it names one at all.
--
-- Of 'impAbs', which is the path the import resolved to and so the one a page is the same page by,
-- rather than of 'impRaw', which is whatever was written to reach it. Both forms an import takes are
-- read: a bare path, and a path handed to a named parser.
-- The first UNNAMED string argument, wherever in the application it sits, which is what
-- 'Syntax.Parsers.rawImportPath' reads on the raw side and for the same reason: an import carries
-- its path unnamed and everything else named. Matching the outermost application instead answered
-- with whatever argument happened to be written last -- the @fn@ of a @native@ import, or the
-- @stackRoot@ of a @haskell@ one -- which is a path that names no page, and so a page nothing could
-- find by the file it was read from.
importPath :: FileImport -> Maybe String
importPath imp = case impAbs imp of
  CExpr _ (CStr n) -> Just n
  e -> listToMaybe [n | ObjArr{oaObj=Nothing, oaArr=Just (Just (CExpr _ (CStr n)), _)} <- exprAppliedArgs e]

-- | Whether a page is part of the standard library.
--
-- Asked of the path, because that is all there is to ask: the standard library is a directory of
-- ordinary pages and nothing in one of them says it belongs to it. It is the compiler's own rule
-- and not a display's — 'Syntax.Parsers.pageImports' is what needs it, to keep from giving the
-- standard library to itself — so a display asking which pages are the library's asks this rather
-- than matching on paths again, and the two cannot come apart.
isCorePage :: FileImport -> Bool
isCorePage imp = maybe False ("core" `isInfixOf`) (importPath imp)

type ImportParseResult = IO ParsedImport
type ImportPreloadResult = IO PreloadImport

-- | How a file written in one syntax is read as a page, and written back as that syntax.
--
-- The directions belong together because only the syntax which read a file knows what its program
-- means as that syntax. A syntax which cannot be written back says so by answering nothing, rather
-- than by having no reversion at all, so that every syntax is asked the same question and a page
-- which cannot be saved is reported rather than mistaken for one which can.
--
-- Reading is asked of a reader made for one read ('ImportReader') rather than of the syntax itself,
-- because reading one page is never the whole of what a read of that syntax does. A syntax which
-- resolves a page against everything beside it works that out once for the read and answers each
-- page out of it; a syntax which keeps a record between runs holds that record open for the read
-- and writes it back at the end. Neither has anywhere to put such a thing when every page is read
-- by a function of the page alone, and handing every syntax a bag of whatever any of them might
-- want makes what one syntax needs into something all of them carry.
--
-- Writing back is asked of the syntax rather than of a reader, because it happens outside any read:
-- a page is written back out of the program something is already holding, with nothing to be found
-- on disk and nothing to record.
data ImportSyntax = ImportSyntax {
  -- | Makes the reader one read reads this syntax with.
  --
  -- Must be cheap and must look at nothing. A read is begun for every request a doc server answers,
  -- and a reader is made for every syntax whether or not the read reaches a page written in it. A
  -- syntax with something expensive to work out works it out when a page first needs it and keeps
  -- it, as 'scanTree' does, rather than when its reader is made.
  isReader   :: ReadEnv -> IO ImportReader,
  -- | Writes a page back as the file it was read from, or nothing when that syntax has no reversion.
  --
  -- A reversion writes the program rather than the file: it may write back what the file said in a
  -- form other than the one the file said it in.
  isUnparse  :: RawPrgm () -> Maybe String,
  -- | The directory a relative link written in a page's documentation resolves against.
  --
  -- The directory the page was imported relative to for nearly every syntax ('importedLinkDir'), and
  -- asked of the syntax rather than taken as that, because a syntax whose pages name each other by
  -- where they sit within something larger resolves a link against that larger thing.
  isLinkDir  :: FileImport -> IO (Maybe FilePath),
  -- | The page an import of this syntax stands at when it names no path of its own, written relative
  -- to the standard library root, and 'Nothing' for a syntax whose imports always name their own.
  --
  -- A page is identified by the import which names it, and everything which displays one names it by
  -- the path it stands at ('Semantics.Prgm.impDisp'). A syntax read out of nothing on disk still has
  -- to answer that, rather than being a page nothing can link to, open or draw. Asked of the syntax
  -- for the reason 'isUnparse' and 'isLinkDir' are: which page a syntax's import means is the
  -- syntax's own business, and a second table beside the table of syntaxes could drift out of step
  -- with it.
  isPagePath :: Maybe FilePath,
  -- | Writes a page back where writing it back is more than 'isUnparse' can say: as every file the
  -- write reaches, the page's own first, or why it cannot be written. 'Nothing' for a syntax whose
  -- reversion is the whole of it, which is every syntax but one.
  --
  -- A reversion says what one page's file holds, and only out of the program. Writing a note back
  -- can need more than that: what its vault is set to, which says how a line moved to another time
  -- is written, and the other notes a line moved to another day is written into. So it is asked
  -- with the page it writes and in 'IO', and answers with each file it writes.
  isWriteBack :: Maybe (FileImport -> RawPrgm () -> IO (Either String [(FilePath, String)])),
  -- | Writes a new value where values like it are written, beside a page of this syntax: as every
  -- file that writes, or why it cannot be written. 'Nothing' for a syntax with nowhere of its own to
  -- put one, which is every syntax but one.
  --
  -- Where a value goes can be the syntax's to say rather than the caller's. A block of the day
  -- created in a day planner goes into the daily note of its day, which the caller cannot name and
  -- which may not be written yet; the page is only which vault it goes into.
  isPlace     :: Maybe (FileImport -> RawExpr () -> IO (Either String [(FilePath, String)]))
                                 }

-- | One read's reading of one syntax, holding whatever that syntax keeps for the length of a read.
--
-- Reading is asked as two questions rather than one, because a reader wants the answers at
-- different times: where every page sits in the graph, and then what is in the pages it goes on to
-- work with.
data ImportReader = ImportReader {
  -- | Reads what the file an import names says about the shape of the program: what it imports, and
  -- what else it brings in.
  irPreload :: Expr () -> ImportPreloadResult,
  -- | Reads the file an import names as the program it is.
  irParse   :: Expr () -> ImportParseResult,
  -- | What the syntax has to write back now the read is over, and nothing for a syntax which
  -- records nothing.
  --
  -- At the end rather than as the read goes, so that a read which read nothing writes nothing. Not
  -- run for a read which failed part way: a record written out of a read that stopped halfway is a
  -- record of a program nobody read.
  irFinish  :: IO (),
  -- | The pages this one cannot be read until something has BUILT, and nothing for a page which can
  -- be read now.
  --
  -- Asked before 'irParse' rather than found out by trying, because a read cannot build: it holds
  -- 'CtService.ctssReadLock' for its whole walk, and building takes that lock again. A page which
  -- answers with pages here is left in the graph with its edges and without its program, and
  -- whoever is driving the read builds what it named and reads again ('CtService.ctssLoadOf').
  --
  -- Nothing for every syntax the compiler supplies: they read a file and are done. Only a page read
  -- by an importer written in Catln waits on anything.
  irBlocked :: Expr () -> IO [RawFileImport]
                                 }

-- | A reader of a syntax which needs nothing built before it can read: every syntax the compiler
-- supplies itself.
readyReader :: (Expr () -> ImportPreloadResult) -> (Expr () -> ImportParseResult) -> IO () -> ImportReader
readyReader preload parse finish = ImportReader preload parse finish (const $ return [])

-- | The preload of a syntax which can say what a page imports only by reading it: the one read
-- answers both questions, so the program it produced is handed on rather than read for again.
eagerPreload :: ParsedImport -> PreloadImport
eagerPreload parsed@ParsedImport{piPrgm=RawPrgm imports _, piReads} = PreloadImport imports [] (Just parsed) piReads

-- | A syntax which needs nothing of a read at all: its pages are read out of the files they name
-- and out of nothing else, so the reader made for each read is the same reader every time.
statelessSyntax :: (Expr () -> ImportPreloadResult) -> (Expr () -> ImportParseResult) -> (RawPrgm () -> Maybe String) -> ImportSyntax
statelessSyntax preload parse unparse = ImportSyntax (const $ return $ readyReader preload parse (return ())) unparse importedLinkDir Nothing Nothing Nothing

-- | Where a relative link resolves for a syntax whose pages are read from the directory they sit in,
-- which is the directory they sit in. Every syntax but the one which reads a page out of a
-- collection of them.
importedLinkDir :: FileImport -> IO (Maybe FilePath)
importedLinkDir = return . impDir

-- | A syntax which needs nothing of a read and which cannot be written back.
readOnlySyntax :: (Expr () -> ImportPreloadResult) -> (Expr () -> ImportParseResult) -> ImportSyntax
readOnlySyntax preload parse = statelessSyntax preload parse (const Nothing)

-- | A read-only stateless syntax whose preload is its parse ('eagerPreload').
eagerSyntax :: (Expr () -> ImportParseResult) -> ImportSyntax
eagerSyntax parse = readOnlySyntax (fmap eagerPreload . parse) parse

instance ExprClass RawExpr where
  getExprMeta expr = case expr of
    RawCExpr m _          -> m
    RawValue m _          -> m
    RawHoleExpr m _       -> m
    RawMacroValue m _     -> m
    RawFmtStrExpr m _ _   -> m
    RawApplyExpr m _      -> m
    RawTheExpr e          -> getExprMeta e
    RawAliasExpr b _      -> getExprMeta b
    RawWhere m _ _        -> m
    RawTupleApply m _ _   -> m
    RawVarsApply m _ _    -> m
    RawContextApply m _ _ -> m
    RawParen e            -> getExprMeta e
    RawMethod m _ _       -> m
    RawList m _           -> m
    RawTypeProp m _ _     -> m

  setExprMeta expr m' = case expr of
    RawCExpr _ c           -> RawCExpr m' c
    RawValue _ v           -> RawValue m' v
    RawHoleExpr _ v        -> RawHoleExpr m' v
    RawMacroValue _ v      -> RawMacroValue m' v
    RawFmtStrExpr _ f s    -> RawFmtStrExpr m' f s
    RawApplyExpr _ v       -> RawApplyExpr m' v
    RawTheExpr e           -> RawTheExpr $ setExprMeta e m'
    RawAliasExpr b a       -> RawAliasExpr (setExprMeta b m') a
    RawWhere _ b c         -> RawWhere m' b c
    RawTupleApply _ b as   -> RawTupleApply m' b as
    RawVarsApply _ b vs    -> RawVarsApply m' b vs
    RawContextApply _ b vs -> RawContextApply m' b vs
    RawParen e             -> RawParen $ setExprMeta e m'
    RawMethod _ b e        -> RawMethod m' b e
    RawList _ ls           -> RawList m' ls
    RawTypeProp _ t v      -> RawTypeProp m' t v

  maybeExprPathM (RawValue m n)               = Just (n, m)
  maybeExprPathM (RawTupleApply _ (_, e) _)   = maybeExprPathM e
  maybeExprPathM (RawVarsApply _ e _)         = maybeExprPathM e
  maybeExprPathM (RawContextApply _ (_, e) _) = maybeExprPathM e
  maybeExprPathM (RawParen e)                 = maybeExprPathM e
  maybeExprPathM (RawMethod _ _ e)            = maybeExprPathM e
  maybeExprPathM (RawWhere _ b _)             = maybeExprPathM b
  maybeExprPathM _                            = Nothing

  exprAppliedArgs = error "Called exprAppliedArgs on a RawExpr. Should have called rawExprAppliedArgs"

  exprAppliedOrdVars (RawValue _ _) = []
  exprAppliedOrdVars (RawTupleApply _ (_, be) _) = exprAppliedOrdVars be
  -- exprAppliedOrdVars (RawVarsApply _ e vars) = H.union (exprAppliedOrdVars e) (H.fromList vars)
  exprAppliedOrdVars RawVarsApply{} = error "Not implemented"
  exprAppliedOrdVars (RawContextApply _ (_, e) _) = exprAppliedOrdVars e
  exprAppliedOrdVars (RawParen e) = exprAppliedOrdVars e
  exprAppliedOrdVars (RawMethod _ _ e) = exprAppliedOrdVars e
  exprAppliedOrdVars _ = error "Unsupported RawExpr exprAppliedOrdVars"

  exprVarArgs RawCExpr{} = H.empty
  exprVarArgs RawHoleExpr{} = H.empty
  exprVarArgs RawValue{} = H.empty
  exprVarArgs RawTheExpr{} = H.empty
  exprVarArgs (RawAliasExpr base alias) = H.unionWith (++) (exprVarArgs base) (exprVarArgs alias)
  exprVarArgs (RawTupleApply _ (_, be) args) = H.unionWith (++) (exprVarArgs be) (unionsWith (++) $ map aux args)
    where
      aux (False, a) = roaVarArgs a
      aux (True, a)  = error $ printf "Not yet implemented %s" (show a)
  exprVarArgs (RawVarsApply _ e vars) = H.unionWith (++) (exprVarArgs e) (unionsWith (++) $ map aux vars)
    where
      aux var = H.singleton (TVVar $ rawInExprSingleton $ fromJust $ roaObj var) [(fromJust $ roaObj var, getExprMeta $ fromJust $ snd $ fromJust $ roaArr var)]
  exprVarArgs (RawContextApply _ (_, e) _) = exprVarArgs e
  exprVarArgs (RawParen e) = exprVarArgs e
  exprVarArgs (RawMethod _ be me) = H.unionWith (++) (exprVarArgs be) (exprVarArgs me)
  exprVarArgs e = error $ printf "Unsupported RawExpr exprVarArgs for %s" (show e)

  exprVarArgsWithSrc = undefined
  mkValue = undefined

instance ObjArrClass RawObjArr where
  oaVarArgs _roa = error "Found call to raw oaVarArgs. Should be using roaVarArgs."
  getOaAnnots = roaAnnots

instance (Show m, Show (e m)) => Show (RawObjArr e m) where
  show RawObjArr{roaObj, roaArr, roaDef} = printf "%s%s%s" (showNoMaybe roaObj) showArr showDef
    where
      showNoMaybe :: (Show a) => Maybe a -> String
      showNoMaybe (Just a) = show a
      showNoMaybe Nothing  = ""

      showArr :: String
      showArr = case roaArr of
        Just (Just e, Just m) -> printf " -> %s = %s" (show m) (show e)
        Just (Just e, _) | isJust roaObj -> printf "= %s" (show e)
        Just (Just e, _) -> show e
        Just (Nothing, Just m) -> printf " -> %s" (show m)
        Just (Nothing, _) -> ""
        Nothing -> ""

      showDef :: String
      -- showDef = maybe "" show roaDef
      showDef = case roaDef of
        Just def -> printf " ? %s" (show def)
        Nothing  -> ""

mkRawFileImport :: RawExpr () -> RawFileImport
mkRawFileImport e = AFileImport e e Nothing Nothing Nothing

rawExprAppliedArgs :: (Show m, MetaDat m) => RawExpr m -> [RawObjArr RawExpr m]
rawExprAppliedArgs (RawValue _ _) = []
rawExprAppliedArgs (RawTupleApply _ (_, be) args) = rawExprAppliedArgs be ++ concatMap aux args
  where
    aux (False, a) = [a]
    aux (True, a)  = error $ printf "Not yet implemented %s" (show a)
rawExprAppliedArgs (RawVarsApply _ e _) = rawExprAppliedArgs e
rawExprAppliedArgs (RawContextApply _ (_, e) _) = rawExprAppliedArgs e
rawExprAppliedArgs (RawParen e) = rawExprAppliedArgs e
rawExprAppliedArgs (RawMethod _ _ e) = rawExprAppliedArgs e
rawExprAppliedArgs _ = error "Unsupported RawExpr rawExprAppliedArgs"

rawExprAppliedArgsMap :: (MetaDat m, Show m) => RawExpr m -> H.HashMap ArgName (Maybe (Meta m, Maybe (RawExpr m)))
rawExprAppliedArgsMap = H.fromList . mapMaybe mapArg . rawExprAppliedArgs
  where
    mapArg :: (MetaDat m, Show m) => RawObjArr RawExpr m -> Maybe (ArgName, Maybe (Meta m, Maybe (RawExpr m)))
    mapArg RawObjArr{roaObj=Just oe, roaArr=Just (arrExpr, oaMExpr)} = Just (rawInExprSingleton oe, Just (getExprMeta $ fromJust oaMExpr, arrExpr))
    mapArg RawObjArr{roaObj=Just oe, roaArr=Nothing} = Just (rawInExprSingleton oe, Nothing)
    mapArg _ = Nothing

rawInExprSingleton :: (MetaDat m, Show m) => RawExpr m -> ArgName
rawInExprSingleton e = maybe
  (partialKey $ makeAbsoluteName $ exprPath e) {pkArgs = H.keysSet $ rawExprAppliedArgsMap e, pkVars = H.keysSet $ exprAppliedVars e} partialToKey (maybeGetSingleton (getExprType e))

roaVarArgs :: (MetaDat m, Show m) => RawObjArr RawExpr m -> H.HashMap TypeVarAux [(RawExpr m, Meta m)]
roaVarArgs RawObjArr{roaArr=(Just (Just argVal, _))} = exprVarArgs argVal
roaVarArgs RawObjArr{roaObj=(Just obj), roaArr= Nothing} = H.singleton (TVArg $ rawInExprSingleton obj) [(obj, emptyMetaE obj)]
roaVarArgs RawObjArr{roaObj=(Just obj), roaArr= Just (Nothing, _)} = H.singleton (TVArg $ rawInExprSingleton obj) [(obj, emptyMetaE obj)]
roaVarArgs oa = error $ printf "exprVarArgs not defined for arg %s" (show oa)

-- | The importers a page declares: what each is called in an import, which extensions it reads, and
-- the name to call it by.
--
-- Read off the PARSED program rather than a compiled one, which is the whole point of the
-- @#importer@ annotation being an annotation. Deciding which importer reads a file happens while a
-- page's imports are being resolved — before anything has been desugared, let alone typechecked —
-- so the answer has to be available from a parse and nothing more.
--
-- The name given is the one written on the page (@parsePython@) rather than the absolute one
-- (@\/Python\/parsePython@), because the absolute name is not known until the page has been
-- desugared and that is after this is wanted. 'Eval.evalCallTo' resolves a relative name when the
-- importer is eventually run.
--
-- Walks the whole forest, so a declaration written inside a @module@ is found: a module is a
-- declaration whose contents are its children, and an importer is normally written in one.
rawPrgmImporters :: RawPrgm () -> [(String, String, [String])]
rawPrgmImporters (RawPrgm _ statements) = concatMap fromTree statements
  where
    fromTree t@(RawStatementTree (RawDeclStatement oa) children) =
      [ (fn, name, words exts)
      | Just fn <- [nameStr <$> maybeExprPath (fromJust $ roaObj oa)]
      , isJust (roaObj oa)
      , a <- annotsOf t
      , isImporterAnnot a
      , Just name <- [rawAnnotStrArg importerAnnotName a]
      , Just exts <- [rawAnnotStrArg importerAnnotExts a]
      ] ++ concatMap fromTree children
    fromTree (RawStatementTree _ children) = concatMap fromTree children

    -- Written on the declaration, or indented beneath it. The second is how one is actually written
    -- and the first is what the desugar would make of it, and this runs before the desugar — so both
    -- are read rather than relying on which form a given page happens to be in.
    annotsOf (RawStatementTree (RawDeclStatement oa) children) =
      roaAnnots oa ++ [a | RawStatementTree (RawAnnot a) _ <- children]
    annotsOf _ = []

    isImporterAnnot a = case nameStr <$> maybeExprPath a of
      Just n  -> n `elem` [importerAnnot, importerAnnotAbs]
      Nothing -> False

-- | A string written as a named argument of a raw annotation.
rawAnnotStrArg :: String -> RawExpr () -> Maybe String
rawAnnotStrArg name a = listToMaybe
  [ s
  | RawObjArr{roaObj=Just obj, roaArr=Just (Just (RawCExpr _ (CStr s)), _)} <- rawExprAppliedArgs a
  , (nameStr <$> maybeExprPath obj) `elem` [Just name, Just ('/':name)]
  ]

-- | Checks whether a program has a global annot
rawPrgmHasAnnot :: RawPrgm m -> String -> Bool
rawPrgmHasAnnot (RawPrgm _ statements) annot = any checkStatementTree statements
  where
    checkStatementTree (RawStatementTree (RawAnnot a) _) = isAnnot annot a
    checkStatementTree _                                 = False
