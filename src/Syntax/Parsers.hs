--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Parsers
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is the main module for arbitary syntax parsing.
-- It will read in files from their file paths and then parse into a 'RawPrgm'.
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parsers where

import qualified Data.HashSet             as S

import           Control.Applicative      ((<|>))
import           Control.Concurrent.STM   (TVar, atomically, modifyTVar',
                                           newTVarIO, readTVar, readTVarIO,
                                           writeTVar)
import           Control.Exception        (IOException, try)
import           Control.Monad            (forM_, join)
import           Control.Monad.Identity   (Identity (..))
import           Control.Monad.Trans
import           CRes
import           CtConstants              (noCoreAnnot)
import           Data.Graph
import qualified Data.HashMap.Strict      as H
import           Data.IORef               (modifyIORef', newIORef, readIORef)
import           Data.List
import           Data.Maybe               (fromMaybe, isJust, listToMaybe,
                                           mapMaybe)
import qualified Data.Sequence            as Sq
import           Data.Time.Clock          (UTCTime, getCurrentTime)
import           MapMeta                  (MetaLocation (ImportMeta), mapMeta,
                                           parseMetaIDScope)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Builder
import           Syntax.Ct.Desugarf.Expr  (desFileImport)
import           Syntax.Ct.MapRawMeta     (addRawPrgmMetaIDs)
import           Syntax.Ct.Parser         (contents, ctSyntax, ctxSyntax)
import           Syntax.Ct.Parser.Expr    (term)
import           Syntax.Ct.Prgm
import           Syntax.FileDir.Parser    (fileDirReference, fileDirSyntax)
import           Syntax.Haskell.Parser    (hsSyntax)
import           Syntax.HsBuiltins.Parser (hsBuiltins, hsBuiltinsSyntax, hsPrgm,
                                           hsPrgmSyntax, hsRawPrgm,
                                           hsRawPrgmSyntax)
import           Syntax.InferImport       (dirSyntax, findImportPath,
                                           inferRawImportStr, parserImport,
                                           stackPath)
import           Syntax.Md.Parser         (mdSyntax)
import           Syntax.Native.Parser     (nativeReference, nativeSyntax)
import           Syntax.Obsidian.Parser   (obsidian, obsidianSyntax)
import           Syntax.Python.Parser     (pySyntax)
import           Syntax.Rss.Parser        (rss, rssSyntax)
import           System.Directory         (makeAbsolute)
import           System.FilePath          (isAbsolute, joinPath, takeDirectory)
import           Text.Megaparsec          (runParser)
import           Text.Printf
import           Utils

importSyntaxes :: H.HashMap String ImportSyntax
importSyntaxes = H.fromList [
  ("dir", dirSyntax),
  ("ct", ctSyntax),
  ("ctx", ctxSyntax),
  ("md", mdSyntax),
  (obsidian, obsidianSyntax),
  (rss, rssSyntax),
  (fileDirReference, fileDirSyntax),
  ("haskell", hsSyntax),
  ("python", pySyntax),
  (hsBuiltins, hsBuiltinsSyntax),
  (hsPrgm, hsPrgmSyntax),
  (hsRawPrgm, hsRawPrgmSyntax),
  (nativeReference, nativeSyntax)
                             ]

-- | The path an applied import names: its first positional argument.
--
-- Both argument shapes are read because an import is canonicalized before any desugar runs. A
-- written @parser("path")@ leaves its lone argument in 'roaObj' — 'Syntax.Ct.Parser.Expr.pArrowFull'
-- makes a single expression an input, for the desugar to move to its proper place — while
-- 'Syntax.Ct.Builder.applyRawArgs', which every inferred import is built with, puts it in 'roaArr'.
-- A named argument keeps a 'RawValue' in 'roaObj', so it is not mistaken for the path.
rawImportPath :: RawExpr () -> Maybe String
rawImportPath e = listToMaybe $ mapMaybe argStr $ rawExprAppliedArgs e
  where
    argStr RawObjArr{roaObj=Nothing, roaArr=Just (Just (RawCExpr _ (CStr s)), _)} = Just s
    argStr RawObjArr{roaObj=Just (RawCExpr _ (CStr s)), roaArr=Nothing}           = Just s
    argStr _                                                                      = Nothing

-- | Rewrites the path an applied import names, leaving the rest of it alone.
--
-- The other arguments are kept because a parser reads them: @haskell@ takes the Stack root to
-- resolve its module against, and whether the module was imported qualified. Matches the same two
-- argument shapes 'rawImportPath' reads.
setRawImportPath :: String -> RawExpr () -> RawExpr ()
setRawImportPath name (RawTupleApply m be args) = RawTupleApply m be (aux args)
  where
    aux [] = []
    aux ((spread, arg):rest) = case arg of
      RawObjArr{roaObj=Nothing, roaArr=Just (Just (RawCExpr am (CStr _)), at)} ->
        (spread, arg{roaArr=Just (Just (RawCExpr am (CStr name)), at)}) : rest
      RawObjArr{roaObj=Just (RawCExpr am (CStr _)), roaArr=Nothing} ->
        (spread, arg{roaObj=Just (RawCExpr am (CStr name))}) : rest
      _ -> (spread, arg) : aux rest
setRawImportPath _ e = e

-- Processes an import, producing the rawImpAbs and rawImpDisp
canonicalImport :: CTSSConfig -> ImportCtx -> RawFileImport -> IO FileImport
canonicalImport config ctx imp@AFileImport{impAbs=(RawCExpr _ (CStr name))} = do
  inferred <- inferRawImportStr config (icCaller ctx) name
  canonicalImport config ctx imp{impAbs=inferred}
-- The metas are cleared because 'impAbs' is the identity of a page: 'AFileImport' compares by it and
-- 'MapMeta.parseMetaIDScope' names a file's meta-id scope by it. A written @import parser("path")@
-- is the one import form which reaches here carrying the positions and types its parse gave it, and
-- keeping those would make one file imported from two places into two pages holding the same meta
-- ids. Every other import is built by 'Syntax.InferImport.inferRawImportStr' with empty metas
-- already, so clearing them changes nothing.
--
-- The path is made absolute for the same reason, so that a page is the same page however it was
-- named. An import which names nothing on disk is left as written rather than rejected: @haskell@
-- takes a module name and resolves it itself, against the Stack project rather than the filesystem.
--
-- 'impRaw' is deliberately left alone. It is what 'Syntax.Ct.Formatter.formatImport' prints, so
-- rewriting it would have the formatter replace what the file says with an absolute path.
canonicalImport config ctx imp = case maybeExprPath impAbs0 of
  Nothing -> fail $ printf "Invalid import %s must be string or oject" (show imp)
  Just impPath -> do
    impAbs' <- syntaxPageImport config (nameStr impPath) impAbs0
    let named' = rawImportPath impAbs'
    resolved <- join <$> mapM (findImportPath config (icCaller ctx)) named'
    let disp' = resolved <|> named'
    impDir' <- case (calledDir', disp') of
      (_, Just b) | isAbsolute b -> return $ Just $ takeDirectory b
      (Just a, Just b)           -> return $ Just $ takeDirectory $ joinPath [a, b]
      _                          -> return Nothing
    let impAbs'' = maybe impAbs' (`setRawImportPath` impAbs') resolved
    return $ desFileImport $ imp{impAbs=impAbs'', impDisp=disp', impCalledDir=calledDir', impDir=impDir'}
  where
    impAbs0 = mapMeta (\_ _ -> Identity emptyMetaN) ImportMeta $ impAbs imp
    calledDir' = impDir =<< icCaller ctx

-- | Resolve the imports a page writes, in the order it writes them, each seeing the ones above it.
--
-- A left fold rather than a 'mapM' because that order is what an importer is looked up in: a page
-- may import the page declaring an importer on one line and use that importer on the next, so what
-- an import means depends on what has been imported before it. A 'mapM' would resolve them all
-- against the same empty context and could not see that.
--
-- Nothing reads 'icPreceding' yet — the resolution that does is the next commit — but the order is
-- established here, so that adding the lookup is adding a lookup rather than also rethreading this.
canonicalImports :: CTSSConfig -> ImporterEnv -> Maybe FileImport -> [RawFileImport] -> IO [FileImport]
canonicalImports config importers caller = go []
  where
    go _ [] = return []
    go preceding (imp:rest) = do
      imp' <- canonicalImport config (ImportCtx caller preceding importers) imp
      (imp' :) <$> go (preceding ++ [imp']) rest

-- | An import which names no path of its own, written as the page its syntax stands at.
--
-- Written into the import rather than only into 'impDisp', so that a page with no file behind it is
-- the same kind of thing as every other page from here on: @parser(\"path\")@ is what an import
-- canonicalizes to ('Syntax.InferImport.inferRawImportStr' makes every written path into one), and
-- everything which asks an import where its page is asks 'Syntax.Ct.Prgm.importPath', which reads
-- that path back out. A page named only by its display would be one 'Syntax.Ct.Prgm.isCorePage'
-- could not answer for.
--
-- Nothing is at the path, which is the point: 'findImportPath' looks and finds nothing, and the
-- import is left naming the page it names. An import which names a path, and a syntax which stands
-- at no page of its own, are both left exactly as they were.
syntaxPageImport :: CTSSConfig -> String -> RawExpr () -> IO (RawExpr ())
syntaxPageImport config syntax e = case (rawImportPath e, isPagePath =<< H.lookup syntax importSyntaxes) of
  (Nothing, Just page) -> do
    page' <- makeAbsolute =<< stackPath config page
    return $ parserImport syntax page'
  _ -> return e

-- | The import a name written outside a program means: an import expression, as it would be written
-- after @import@ in one, or else the path of a file to be read with whichever parser suits it.
--
-- Both forms are what an @import@ statement itself accepts, and the expression is read with the
-- parser that statement is read with, so a name given to the compiler and a name written in a
-- program mean the same thing. A path is guessed from what is on disk
-- ('Syntax.InferImport.inferRawImportStr'), which is enough to name an Obsidian vault as one, since
-- what makes a directory a vault is a directory inside it. Naming the parser says it outright, and
-- is the only way to say which parser something is read with where what is on disk cannot.
--
-- A name is taken for an expression by its shape rather than by whether it parses, because a path
-- parses too: @a\/b.ct@ is a value with a @.ct@ method suffix, and @core@ is a value. What is asked
-- of it is what 'canonicalImport' and 'readImport' ask of an import — a head to name the parser by
-- ('maybeExprPath') and a string argument to name the path by ('rawImportPath') — so a name is read
-- as an import exactly when it is one. No path takes that shape without holding both a bracket and a
-- quote.
mkRawImportStr :: String -> RawFileImport
mkRawImportStr name = mkRawFileImport $ fromMaybe (rawStr name) (importExpr name)
  where
    importExpr s = case runParser (contents term) "<import>" s of
      Right e | isJust (maybeExprPath e), isJust (rawImportPath e) -> Just e
      _                                                            -> Nothing

mkDesCanonicalImportStr :: CTSSConfig -> String -> IO FileImport
mkDesCanonicalImportStr config = canonicalImport config (rootImportCtx noImporters) . mkRawImportStr

-- | The syntax a page is written in, and how that syntax is read and written back.
--
-- The syntax is the name at the head of the import, which is what says it. Asked the one way by
-- everything which reads or writes a page, so that a page is read and written back by the one
-- syntax.
importSyntax :: MonadFail m => FileImport -> m (String, ImportSyntax)
importSyntax imp = case maybeExprPath (impAbs imp) of
  Nothing -> fail $ printf "Invalid import %s must be string or oject" (show imp)
  Just impPath -> case H.lookup (nameStr impPath) importSyntaxes of
    Nothing     -> fail $ printf "No parser available for oject name in %s" (show imp)
    Just syntax -> return (nameStr impPath, syntax)

-- | What one read reads with: what it has already found on disk, and the reader it reads each
-- syntax with.
--
-- Apart from 'ReadEnv' because the two are wanted apart. A caller which looks at when files were
-- written and parses nothing ('unchangedPages') has no business making readers, and a syntax being
-- given the read's own findings is not the same thing as a syntax keeping findings of its own.
data Reading = Reading {
  rdEnv     :: ReadEnv,
  -- | Keyed as 'importSyntaxes' is and built from it, so a syntax an import can name is a reader
  -- here exactly when it is a syntax there.
  rdReaders :: H.HashMap String ImportReader
                       }

-- | The readers a read is about to read with, one for each syntax there is.
--
-- Made when the read begins rather than when a page of that syntax is first reached. Making one
-- costs nothing ('Syntax.Ct.Prgm.isReader' says so), and making them all up front is what makes
-- finishing them all a thing 'finishReading' can do: a read whose readers appeared as pages were
-- reached would write back whatever the pages it happened to reach had made, which is a record that
-- depends on the order a graph was walked in.
newReading :: Maybe FilePath -> ImporterEnv -> IO Reading
newReading cacheRoot importers = do
  env <- newRecordedReadEnv cacheRoot importers
  Reading env <$> traverse (`isReader` env) importSyntaxes

-- | Writes back whatever the read has left each syntax with. See 'Syntax.Ct.Prgm.irFinish' for why
-- a read which failed part way must not reach this.
finishReading :: Reading -> IO ()
finishReading = mapM_ irFinish . rdReaders

-- | The reader this read reads the syntax an import names with.
importReader :: MonadFail m => Reading -> FileImport -> m ImportReader
importReader reading imp = do
  (syntax, _) <- importSyntax imp
  case H.lookup syntax (rdReaders reading) of
    Just reader -> return reader
    Nothing     -> fail $ printf "The read holds no reader for the syntax %s of %s" (show syntax) (show imp)

-- | What a read has left to read, and every page it has taken up.
--
-- A queue rather than a stack, so that a page reached first is read first. What a stack reads is the
-- whole of an import before the import written after it, which is an order nothing outside the walk
-- can say anything about; what a queue reads is the pages of a program in the order the program
-- names them, and what is left of it is a count rather than a shape.
--
-- Kept apart from 'Reading' because the two belong to different things. 'Reading' is what a read
-- reads WITH and is made and finished by the read itself; this is what a read has left TO DO, and is
-- held by whoever is doing the reading — see 'CtService.ctssReadQueue', which is a service taking up
-- each page as the read reaches it rather than the whole graph once the read is over.
data ReadQueueDat = ReadQueueDat {
  -- | The pages left to read, the one reached first at the front.
  rqPending :: Sq.Seq FileImport,
  -- | Every page this read has queued, which is every page it reads.
  --
  -- A page already queued is kept out of the queue rather than passed over when it comes off, so
  -- that the queue holds each page once and its length is what is left to do. It doubles as the
  -- read's record of which pages it reached, which is what a caller taking up the pages one at a
  -- time needs in order to say which of the pages it held this read did not find
  -- ('CtService.ctssReadOf').
  rqSeen    :: S.HashSet FileImport
                                 }

-- | The queue one read reads from.
--
-- Behind a 'TVar' rather than an 'IORef' because what a read has left to do is a thing to be asked
-- about from outside the read.
newtype ReadQueue = ReadQueue (TVar ReadQueueDat)

newReadQueue :: IO ReadQueue
newReadQueue = ReadQueue <$> newTVarIO (ReadQueueDat Sq.empty S.empty)

-- | Begin a read, from the pages named.
--
-- What an earlier read took up is dropped: a page read then is a page to be read again now, and it
-- is only within one read that a page queued twice is a page read twice for nothing. A read that
-- threw part way leaves pages queued, and those are dropped with it — they are reached again from
-- the pages this read is beginning from.
rqRestart :: ReadQueue -> [FileImport] -> IO ()
rqRestart queue@(ReadQueue v) imps = do
  atomically $ writeTVar v $ ReadQueueDat Sq.empty S.empty
  rqPush queue imps

-- | Queue these pages to be read, passing over whichever of them this read has queued already.
rqPush :: ReadQueue -> [FileImport] -> IO ()
rqPush (ReadQueue v) imps = atomically $ modifyTVar' v $ \q -> foldl' add q imps
  where
    add q imp
      | S.member imp (rqSeen q) = q
      | otherwise = q{rqPending = rqPending q Sq.|> imp, rqSeen = S.insert imp (rqSeen q)}

-- | The next page to read, and nothing where the read has read everything it reached.
rqPop :: ReadQueue -> IO (Maybe FileImport)
rqPop (ReadQueue v) = atomically $ do
  q <- readTVar v
  case Sq.viewl (rqPending q) of
    Sq.EmptyL -> return Nothing
    imp Sq.:< rest -> do
      writeTVar v q{rqPending = rest}
      return $ Just imp

-- | Every page the read has reached, queued or read already.
rqRead :: ReadQueue -> IO (S.HashSet FileImport)
rqRead (ReadQueue v) = rqSeen <$> readTVarIO v

-- | How many pages the read has left to read.
--
-- Answered without the read stopping, which is the whole reason this queue is behind a 'TVar'.
--
-- The length of what is pending rather than the size of what has been seen, because this is asked
-- once per page read: 'Sq.length' is constant time where the size of a 'S.HashSet' is a walk of it,
-- and a read of a vault of a few thousand notes which asked the second way would spend the read
-- counting. A page not yet reached is not in either, so what this says is what is left of what is
-- KNOWN — which grows as the walk finds more.
rqLeft :: ReadQueue -> IO Int
rqLeft (ReadQueue v) = Sq.length . rqPending <$> readTVarIO v

-- | Reads where the page an import names sits in the import graph.
-- TODO Modify the ImportPreloadResult to CResT to report back parse errors
preloadImport :: Reading -> FileImport -> ImportPreloadResult
preloadImport reading imp = do
  reader <- importReader reading imp
  irPreload reader $ impAbs imp

-- | Reads the page an import names as the program it holds.
-- TODO Modify the ImportParseResult to CResT to report back parse errors
parseImport :: Reading -> FileImport -> ImportParseResult
parseImport reading imp = do
  reader <- importReader reading imp
  irParse reader $ impAbs imp

-- | Reads one page as the program it holds, on its own and out of no record.
--
-- Out of no record whatever the caller is otherwise configured with. A caller which wants one page
-- wants what that page says now, where a record is a thing a read of a whole program is answered
-- faster by. The formatter is the case: it writes the page back afterwards, and writing back what a
-- record said rather than what the file says would be a formatter which undoes edits.
parseImportAlone :: FileImport -> IO ParsedImport
parseImportAlone imp = do
  reading <- newReading Nothing noImporters
  parsed <- parseImport reading imp
  finishReading reading
  return parsed

-- | Writes a page back as the file it was read from, by the syntax which read it.
--
-- The mirror of 'parseImport'. A syntax with no reversion is reported rather than passed over, so
-- that a page which cannot be saved says so instead of being saved as something else.
--
-- Asked of the syntax rather than of a read's reader, because writing a page back happens outside
-- any read: what is written is the program the caller is holding.
unparseImport :: FileImport -> RawPrgm () -> CRes String
unparseImport imp prgm = do
  (syntax, syntax') <- importSyntax imp
  case isUnparse syntax' prgm of
    Just src -> return src
    Nothing  -> fail $ printf "The page %s can not be written back as %s" (show $ impRaw imp) (show syntax)

-- | The directory a relative link written in a page's documentation resolves against.
--
-- Asked of the syntax which read the page, as 'unparseImport' asks it how to write the page back and
-- for the same reason: where a page's links resolve is a property of the syntax that read it, and a
-- caller which wants to show a page has no business knowing which syntaxes there are. A page whose
-- syntax cannot be named is answered with the directory it was imported relative to, which is what
-- every syntax but one answers anyway ('Syntax.Ct.Prgm.importedLinkDir').
pageLinkDir :: FileImport -> IO (Maybe FilePath)
pageLinkDir imp = case importSyntax imp of
  Just (_, syntax) -> isLinkDir syntax imp
  Nothing          -> return $ impDir imp

-- | When anything under a directory was last added, removed or renamed, and 'Nothing' where the
-- tree could not be walked at all.
--
-- This is the tree's own walk ('Syntax.Ct.Prgm.scanTree') asked for its times, so what a reading of
-- a tree reads and what timing that reading looks at cannot come apart — a tree timed by more than
-- the reading walked is a page read again for a change it never read.
--
-- A tree which cannot be walked is not an error here, though it is to whatever reads it: timing runs
-- over what an earlier read recorded, and a directory since deleted must answer that something has
-- changed rather than fail the read which asked.
treeWrittenAt :: ReadEnv -> FilePath -> IO (Maybe UTCTime)
treeWrittenAt env dir = either (const Nothing) tsWritten <$> (try (scanTree env dir) :: IO (Either IOException TreeScan))

-- | When what a read read at a path was last written.
--
-- A directory is timed by itself rather than by what it holds, because its listing is what was read
-- and a file already in it being edited leaves that listing as it was.
readPathWrittenAt :: ReadEnv -> ReadPath -> IO (Maybe UTCTime)
readPathWrittenAt _ (ReadFile file)   = modifiedAt file
readPathWrittenAt _ (ReadExists file) = modifiedAt file
readPathWrittenAt _ (ReadDir dir)     = modifiedAt dir
readPathWrittenAt env (ReadTree dir)  = treeWrittenAt env dir

-- | Time what a read read, as of a read which began at the given time.
readTime :: ReadEnv -> UTCTime -> Maybe [ReadPath] -> IO (Maybe ReadTime)
readTime env started = traverse (fmap (ReadTime started) . mapM (\read' -> (read',) <$> readPathWrittenAt env read'))

-- | Whether everything a page was read from is still as the read found it, given the times things
-- were last written at.
--
-- Pure, and given the times rather than looking them up, so that a path read by many pages is
-- looked at once rather than once for each of them. A vault is read from by every note in it.
readTimeCurrent :: H.HashMap ReadPath (Maybe UTCTime) -> Maybe ReadTime -> Bool
readTimeCurrent _ Nothing = False
readTimeCurrent times (Just ReadTime{rtStarted, rtPaths}) = all current rtPaths
  where
    current (read', was) = case H.lookup read' times of
      Just now -> readPathCurrent rtStarted read' was now
      Nothing  -> False

-- | Whether what a read found at one of the things it read is still what it would find, given when
-- the read began, when that thing was last written as the read found it, and when it was last
-- written now.
--
-- What counts as still is what was read there. A file whose text was read is as it was while it has
-- not been written to at all. A file a read only found is as it was while it is still there, and
-- writing to it is not a change to that — what is written in it belongs to its own page.
--
-- A file written since the read began is not one a read of its text can vouch for: the read may
-- have read it before the write, and a time taken after the read cannot tell the two apart. Such a
-- file is taken as changed, which costs one further read of the page and never a page left stale.
-- A read which only found the file has nothing to vouch for, as no write to a file makes it a file
-- which is not there.
readPathCurrent :: UTCTime -> ReadPath -> Maybe UTCTime -> Maybe UTCTime -> Bool
readPathCurrent _ ReadExists{} was now = isJust was == isJust now
readPathCurrent started _ was now      = now == was && maybe True (< started) now

-- | Whether any of these paths is this one, or a directory holding it at any depth.
--
-- A directory appearing, going or being renamed is a write to everything under it, and is reported
-- as a write to that directory alone: nothing under it is written to in its own right, and yet
-- every path under it names something else afterwards. Renaming a directory of an Obsidian vault is
-- the case — the notes in it keep their text and their names, and a note linking into it is left
-- importing a path which is no longer there.
--
-- Walked upwards rather than compared against each written path, because what has been written to
-- is a set and how deep a path is, is not.
writtenAbove :: S.HashSet FilePath -> FilePath -> Bool
writtenAbove written entry
  | S.member entry written = True
  | otherwise              = up /= entry && writtenAbove written up
  where up = takeDirectory entry

-- | Whether writing to any of these paths could be writing to something a read read at this path.
--
-- Could, not did: what has been written to is a set of paths, and a path written to is a path
-- created, changed, deleted or renamed all the same. Which of those it was is what
-- 'readPathCurrent' decides, of the pages this reaches ('CtService.ctssUnchangedOf').
readPathWrittenIn :: S.HashSet FilePath -> ReadPath -> Bool
-- The file itself, as its text is what was read.
readPathWrittenIn written (ReadFile file) = writtenAbove written file
-- The file itself, as whether it is there is what was read.
readPathWrittenIn written (ReadExists file) = writtenAbove written file
-- A directory is read for its listing, so an entry appearing in it or going from it is a write to
-- it, and that entry is a path inside it.
readPathWrittenIn written (ReadDir dir) = writtenAbove written dir || any ((== dir) . takeDirectory) written
-- Everything under the directory, at whatever depth, that a reading of the tree would have read.
readPathWrittenIn written (ReadTree dir) = any (readsUnder dir) written

-- | The imports a page holds: the ones the read found it writes, and the standard library unless
-- the page turns it down or is part of it.
--
-- A page whose program has not been read is given the standard library, since whether to give it is
-- read out of the program. See 'Syntax.Ct.Prgm.plParsed' for why that is a limit on which syntaxes
-- may leave a page unread rather than a guess made here.
pageImports :: FileImport -> Maybe (RawPrgm ()) -> [RawFileImport] -> [RawFileImport]
pageImports imp prgm imports
  | includeCore && not (isCorePage imp) = mkRawFileImport (rawStr "core") : imports
  | otherwise = imports
  where
    includeCore = maybe True (\prgm' -> not (rawPrgmHasAnnot prgm' noCoreAnnot)) prgm

-- | The program as the page holds it: writing the page's imports rather than its own, and with its
-- meta ids named for the page.
pagePrgm :: FileImport -> [RawFileImport] -> RawPrgm () -> RawPrgm ()
pagePrgm imp imports (RawPrgm _ statements) = addRawPrgmMetaIDs (parseMetaIDScope imp) $ RawPrgm imports statements

-- | The page a preload found: where it sits in the graph, and the program it holds where the
-- preload had to read it.
preloadPage :: CTSSConfig -> ImporterEnv -> FileImport -> PreloadImport -> IO ReadPage
preloadPage config importers imp PreloadImport{plImports, plExtraImports, plParsed} = do
  let imports = pageImports imp (piPrgm <$> plParsed) plImports
  imports' <- canonicalImports config importers (Just imp) imports
  -- The extras are what a reading FOUND rather than what the page wrote — the files of a directory,
  -- the notes of a vault — so none of them is written above any other and there is no order for one
  -- to see the others in.
  extras' <- mapM (canonicalImport config (pageImportCtx importers imp)) plExtraImports
  return $ ReadPage (pagePrgm imp imports . piPrgm <$> plParsed) (piSource =<< plParsed) imports' extras' Nothing []

-- | What a parse gives a page: the program as the page holds it, and the edges that program writes.
parsedPage :: CTSSConfig -> ImporterEnv -> FileImport -> ParsedImport -> IO (RawPrgm (), [FileImport])
parsedPage config importers imp ParsedImport{piPrgm=prgm@(RawPrgm rawImports _)} = do
  let imports = pageImports imp (Just prgm) rawImports
  imports' <- canonicalImports config importers (Just imp) imports
  return (pagePrgm imp imports prgm, imports')

-- | Read one page as its program, on its own and out of no record. What a caller which is to work
-- with a page's program rather than with a graph of them wants: the formatter, and the goldens.
parseFile :: CTSSConfig -> RawFileImport -> CResT IO (GraphNodes (RawPrgm ()) FileImport)
parseFile config imp = lift $ do
  imp' <- canonicalImport config (rootImportCtx noImporters) imp
  parsed <- parseImportAlone imp'
  (prgm, imports) <- parsedPage config noImporters imp' parsed
  return (prgm, imp', imports)

-- | What a page was read from, or nothing at all where the read could not say what it read.
readPaths :: ReadPage -> [ReadPath]
readPaths = maybe [] (map fst . rtPaths) . rpReads

-- | The pages of an earlier read which need not be read again: those everything of which is still
-- as that read found it.
--
-- What to hand the next 'readFiles', for a caller with no better way of knowing what changed than
-- looking. A caller which is told what changed — by watching the files, or by having written them
-- itself — knows without looking, and passes what it was told instead.
--
-- Each distinct thing read is looked at once for all the pages that read it, rather than once for
-- each: every note of an Obsidian vault is read from that vault, and walking it for each of them
-- would be the vault walked once per note.
unchangedPages :: [(FileImport, ReadPage)] -> IO (H.HashMap FileImport ReadPage)
unchangedPages pages = do
  env <- newReadEnv
  let paths = S.toList $ S.fromList $ concatMap (readPaths . snd) pages
  times <- H.fromList <$> mapM (\read' -> (read',) <$> readPathWrittenAt env read') paths
  return $ H.fromList [page | page <- pages, readTimeCurrent times (rpReads $ snd page)]

-- | The page an import names: the one the last read left, or the files read again, and its program
-- read where this read is one to have it.
--
-- A page left by an earlier read is read the rest of the way in place rather than from the start,
-- since where it sits in the graph is already known and nothing it was read from has changed.
--
-- The time is taken before the files are read rather than after them, so that a file written while
-- it is being read is one the next read reads again. A time taken afterwards would say the read had
-- seen the write when it may have happened first.
readPage :: CTSSConfig -> Reading -> S.HashSet FileImport -> H.HashMap FileImport ReadPage -> FileImport -> IO ReadPage
readPage config reading inFull prev imp = case H.lookup imp prev of
  Just page -> readOn page
  Nothing -> do
    started <- getCurrentTime
    preloaded <- preloadImport reading imp
    page <- preloadPage config (reImporters $ rdEnv reading) imp preloaded
    times <- readTime (rdEnv reading) started (plReads preloaded)
    readOn page{rpReads = times}
  where
    readOn page
      | isJust (rpPrgm page) || not (S.member imp inFull) = return page
      | otherwise = do
          started <- getCurrentTime
          parsed <- parseImport reading imp
          (prgm, imports) <- parsedPage config (reImporters $ rdEnv reading) imp parsed
          times <- readTime (rdEnv reading) started (piReads parsed)
          return page{rpPrgm=Just prgm, rpSource=piSource parsed, rpImports=imports, rpReads=times}

-- | Read what the queue names, and everything those pages lead to, handing each page over as it is
-- read.
--
-- A page of @prev@ is given back as it is, its files read again only where it is one of the pages
-- named and its program has yet to be read, and the read walks on through the imports it was left
-- with. Which of them are still worth giving back is the caller's to decide, by whatever it knows
-- about what has changed ('unchangedPages' looks, where there is nothing better), so that this
-- reads what it is asked to read and no more.
--
-- The pages named are read as their programs; every other page a parser can place in the graph
-- without reading is left with its imports and without its program. What that leaves is every page
-- with the programs of the ones asked for, which is what lets a service take up the shape of a
-- program without reading all of it.
--
-- Each page is handed over before the pages it leads to are queued, so that whoever is reading has
-- it before the read walks on from it. That is what this is for, and is the whole of the difference
-- between it and 'readFiles' below, which is this with the pages gathered up and given back at the
-- end: a caller which takes each page up as it arrives is one the read itself can be answered out
-- of, where a caller given a graph at the end cannot be asked anything until the read is over.
readQueued :: CTSSConfig -> Reading -> S.HashSet FileImport -> H.HashMap FileImport ReadPage -> ReadQueue -> (FileImport -> ReadPage -> IO ()) -> IO ()
readQueued config reading inFull prev queue found = aux
  where
    aux = do
      next <- rqPop queue
      forM_ next $ \imp -> do
        page <- readPage config reading inFull prev imp
        found imp page
        rqPush queue $ readPageVisits page
        aux

-- | Read the files an import names, and everything they import, as the graph they make.
--
-- 'readQueued' for a caller which wants the whole of what a read found rather than each page as it
-- is read: the compiler asked for one program's dependencies, and the cases which assert on what a
-- read reads. A service takes the pages up as they arrive instead ('CtService.ctssReadOf').
--
-- Whatever the read recorded is written back at the end rather than as it goes, so that a read
-- which read nothing writes nothing. Not bracketed: see 'Syntax.Ct.Prgm.irFinish' for why a read
-- which failed part way must not reach it.
--
-- TODO Change preloadImport and parseImport, then make this return GraphData of CRes
readFiles :: CTSSConfig -> S.HashSet FileImport -> H.HashMap FileImport ReadPage -> [RawFileImport] -> IO (GraphData ReadPage FileImport)
readFiles config inFull prev initialImps = do
  reading <- newReading (ctssCachePath config) noImporters
  queue <- newReadQueue
  rqRestart queue =<< mapM (canonicalImport config (rootImportCtx noImporters)) initialImps
  acc <- newIORef []
  readQueued config reading inFull prev queue $ \imp page ->
    modifyIORef' acc ((page, imp, rpImports page) :)
  finishReading reading
  graphFromEdges <$> readIORef acc
