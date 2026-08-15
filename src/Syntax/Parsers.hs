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

import qualified Data.HashSet            as S

import           Control.Applicative     ((<|>))
import           Control.Exception       (IOException, try)
import           Control.Monad           (join)
import           Control.Monad.Identity  (Identity (..))
import           Control.Monad.Trans
import           CRes
import           CtConstants             (noCoreAnnot)
import           Data.Graph
import qualified Data.HashMap.Strict     as H
import           Data.List
import           Data.Maybe              (fromMaybe, isJust, listToMaybe,
                                          mapMaybe)
import           Data.Time.Clock         (UTCTime, getCurrentTime)
import           MapMeta                 (MetaLocation (ImportMeta), mapMeta,
                                          parseMetaIDScope)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Builder
import           Syntax.Ct.Desugarf.Expr (desFileImport)
import           Syntax.Ct.MapRawMeta    (addRawPrgmMetaIDs)
import           Syntax.Ct.Parser        (contents, ctParser, ctxParser)
import           Syntax.Ct.Parser.Expr   (term)
import           Syntax.Ct.Prgm
import           Syntax.Haskell.Parser   (hsParser)
import           Syntax.InferImport      (dirParser, findImportPath,
                                          inferRawImportStr)
import           Syntax.Md.Parser        (mdParser)
import           Syntax.Obsidian.Cache   (flushObsidianCache)
import           Syntax.Obsidian.Parser  (obsidian, obsidianParser)
import           Syntax.Python.Parser    (pyParser)
import           System.FilePath         (isAbsolute, joinPath, takeDirectory)
import           Text.Megaparsec         (runParser)
import           Text.Printf
import           Utils

importParsers :: H.HashMap String ImportParser
importParsers = H.fromList [
  ("dir", dirParser),
  ("ct", ctParser),
  ("ctx", ctxParser),
  ("md", mdParser),
  (obsidian, obsidianParser),
  ("haskell", hsParser),
  ("python", pyParser)
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
canonicalImport :: CTSSConfig -> Maybe FileImport -> RawFileImport -> IO FileImport
canonicalImport config caller imp@AFileImport{impAbs=(RawCExpr _ (CStr name))} = do
  inferred <- inferRawImportStr config caller name
  canonicalImport config caller imp{impAbs=inferred}
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
canonicalImport config caller imp = case maybeExprPath impAbs' of
  Nothing -> fail $ printf "Invalid import %s must be string or oject" (show imp)
  Just _ -> do
    resolved <- join <$> mapM (findImportPath config caller) named'
    let disp' = resolved <|> named'
    impDir' <- case (calledDir', disp') of
      (_, Just b) | isAbsolute b -> return $ Just $ takeDirectory b
      (Just a, Just b)           -> return $ Just $ takeDirectory $ joinPath [a, b]
      _                          -> return Nothing
    let impAbs'' = maybe impAbs' (`setRawImportPath` impAbs') resolved
    return $ desFileImport $ imp{impAbs=impAbs'', impDisp=disp', impCalledDir=calledDir', impDir=impDir'}
  where
    impAbs' = mapMeta (\_ _ -> Identity emptyMetaN) ImportMeta $ impAbs imp
    named' = rawImportPath impAbs'
    calledDir' = impDir =<< caller

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
mkDesCanonicalImportStr config = canonicalImport config Nothing . mkRawImportStr

-- | The syntax a page is written in, and the parser which reads it.
--
-- The syntax is the name at the head of the import, which is what says it. Asked the one way by
-- everything which reads or writes a page, so that a page is read and written back by the one
-- parser.
importParser :: MonadFail m => FileImport -> m (String, ImportParser)
importParser imp = case maybeExprPath (impAbs imp) of
  Nothing -> fail $ printf "Invalid import %s must be string or oject" (show imp)
  Just impPath -> case H.lookup (nameStr impPath) importParsers of
    Nothing     -> fail $ printf "No parser available for oject name in %s" (show imp)
    Just parser -> return (nameStr impPath, parser)

-- | Reads where the page an import names sits in the import graph.
-- TODO Modify the ImportPreloadResult to CResT to report back parse errors
preloadImport :: ReadEnv -> FileImport -> ImportPreloadResult
preloadImport env imp = do
  (_, parser) <- importParser imp
  ipPreload parser env $ impAbs imp

-- | Reads the page an import names as the program it holds.
-- TODO Modify the ImportParseResult to CResT to report back parse errors
parseImport :: ReadEnv -> FileImport -> ImportParseResult
parseImport env imp = do
  (_, parser) <- importParser imp
  ipParse parser env $ impAbs imp

-- | Writes a page back as the file it was read from, by the parser which read it.
--
-- The mirror of 'parseImport'. A syntax with no reversion is reported rather than passed over, so
-- that a page which cannot be saved says so instead of being saved as something else.
unparseImport :: FileImport -> RawPrgm () -> CRes String
unparseImport imp prgm = do
  (syntax, parser) <- importParser imp
  case ipUnparse parser prgm of
    Just src -> return src
    Nothing  -> fail $ printf "The page %s can not be written back as %s" (show $ impRaw imp) (show syntax)

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
-- Held by whoever keeps the pages ('CtService.ssfRead') and handed back to the next 'readFiles',
-- which gives the page back rather than reading its files again when nothing it was read from has
-- been written to.
--
-- A page given back is given back whole, imports included, so an import is resolved where the page
-- was read rather than at every read of it. A file appearing where an import used to resolve
-- elsewhere is therefore not noticed until the page which writes that import is read again. The
-- files a service was given are resolved afresh on every read, and a directory holding a new file
-- is read again because the directory was written to, so this is confined to an @import@ written in
-- a page whose own files have not changed.
data ReadPage = ReadPage {
  -- | The program the page holds, or 'Nothing' for a page whose imports are known and whose program
  -- has not been read ('Syntax.Ct.Prgm.plParsed'). Such a page is in the graph with its edges, and
  -- is read the rest of the way when something asks what is in it ('CtService.ctssLoad').
  rpPrgm    :: Maybe (RawPrgm ()),
  rpSource  :: Maybe String,
  -- | The imports the page writes: its edges in the import graph.
  rpImports :: [FileImport],
  -- | Pages the read found which the page does not import ('Syntax.Ct.Prgm.plExtraImports'): the
  -- files of a directory, the notes of a vault. The read walks on to these as well as to the
  -- edges, so a page given back that replayed only its edges would drop every file of a directory,
  -- and every note of a vault, out of the graph.
  rpExtras  :: [FileImport],
  -- | 'Nothing' where nothing timed the read, which is a page that is always read again: either the
  -- parser could not say what it read ('Syntax.Ct.Prgm.ReadPaths') or the read was not one
  -- 'readFiles' made.
  rpReads   :: Maybe ReadTime
                         }

-- | Every page the read walks on to from this one.
readPageVisits :: ReadPage -> [FileImport]
readPageVisits page = rpExtras page ++ rpImports page

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
  | includeCore && not ("core" `isInfixOf` name) = mkRawFileImport (rawStr "core") : imports
  | otherwise = imports
  where
    includeCore = maybe True (\prgm' -> not (rawPrgmHasAnnot prgm' noCoreAnnot)) prgm
    name = case impAbs imp of
      CExpr _ (CStr n) -> n
      TupleApply _ _ (EAppArg ObjArr{oaArr=Just (Just (CExpr _ (CStr n)), _)}) -> n
      _ -> ""

-- | The program as the page holds it: writing the page's imports rather than its own, and with its
-- meta ids named for the page.
pagePrgm :: FileImport -> [RawFileImport] -> RawPrgm () -> RawPrgm ()
pagePrgm imp imports (RawPrgm _ statements) = addRawPrgmMetaIDs (parseMetaIDScope imp) $ RawPrgm imports statements

-- | The page a preload found: where it sits in the graph, and the program it holds where the
-- preload had to read it.
preloadPage :: CTSSConfig -> FileImport -> PreloadImport -> IO ReadPage
preloadPage config imp PreloadImport{plImports, plExtraImports, plParsed} = do
  let imports = pageImports imp (piPrgm <$> plParsed) plImports
  imports' <- mapM (canonicalImport config (Just imp)) imports
  extras' <- mapM (canonicalImport config (Just imp)) plExtraImports
  return $ ReadPage (pagePrgm imp imports . piPrgm <$> plParsed) (piSource =<< plParsed) imports' extras' Nothing

-- | What a parse gives a page: the program as the page holds it, and the edges that program writes.
parsedPage :: CTSSConfig -> FileImport -> ParsedImport -> IO (RawPrgm (), [FileImport])
parsedPage config imp ParsedImport{piPrgm=prgm@(RawPrgm rawImports _)} = do
  let imports = pageImports imp (Just prgm) rawImports
  imports' <- mapM (canonicalImport config (Just imp)) imports
  return (pagePrgm imp imports prgm, imports')

-- | Read one page as its program, on its own and out of no record. What a caller which is to work
-- with a page's program rather than with a graph of them wants: the formatter, and the goldens.
parseFile :: CTSSConfig -> RawFileImport -> CResT IO (GraphNodes (RawPrgm ()) FileImport)
parseFile config imp = lift $ do
  env <- newReadEnv
  imp' <- canonicalImport config Nothing imp
  parsed <- parseImport env imp'
  (prgm, imports) <- parsedPage config imp' parsed
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
readPage :: CTSSConfig -> ReadEnv -> S.HashSet FileImport -> H.HashMap FileImport ReadPage -> FileImport -> IO ReadPage
readPage config env inFull prev imp = case H.lookup imp prev of
  Just page -> readOn page
  Nothing -> do
    started <- getCurrentTime
    preloaded <- preloadImport env imp
    page <- preloadPage config imp preloaded
    times <- readTime env started (plReads preloaded)
    readOn page{rpReads = times}
  where
    readOn page
      | isJust (rpPrgm page) || not (S.member imp inFull) = return page
      | otherwise = do
          started <- getCurrentTime
          parsed <- parseImport env imp
          (prgm, imports) <- parsedPage config imp parsed
          times <- readTime env started (piReads parsed)
          return page{rpPrgm=Just prgm, rpSource=piSource parsed, rpImports=imports, rpReads=times}

-- | Read the files an import names, and everything they import.
--
-- A page of @prev@ is given back as it is, its files read again only where it is one of the pages
-- named and its program has yet to be read, and the read walks on through the imports it was left
-- with. Which of them are still worth giving back is the caller's to decide, by whatever it knows
-- about what has changed ('unchangedPages' looks, where there is nothing better), so that this
-- reads what it is asked to read and no more.
--
-- The pages named are read as their programs; every other page a parser can place in the graph
-- without reading is left with its imports and without its program. What that leaves is a graph of
-- every page with the programs of the ones asked for, which is what lets a service take up the
-- shape of a program without reading all of it.
--
-- Whatever the read recorded is written back at the end rather than as it goes, so that a read
-- which read nothing writes nothing.
--
-- TODO Change preloadImport and parseImport, then make this return GraphData of CRes
readFiles :: CTSSConfig -> S.HashSet FileImport -> H.HashMap FileImport ReadPage -> [RawFileImport] -> IO (GraphData ReadPage FileImport)
readFiles config inFull prev initialImps = do
  env <- newRecordedReadEnv (ctssCachePath config)
  initialImps' <- mapM (canonicalImport config Nothing) initialImps
  res <- aux env [] S.empty initialImps'
  mapM_ flushObsidianCache $ reObsidianCache env
  return $ graphFromEdges res
  where
    aux :: ReadEnv -> [GraphNodes ReadPage FileImport] -> S.HashSet FileImport -> [FileImport] -> IO [GraphNodes ReadPage FileImport]
    aux _ acc _ [] = return acc
    aux env acc visited (nextToVisit:restToVisit) = do
      if S.member nextToVisit visited
        then aux env acc visited restToVisit
        else do
          page <- readPage config env inFull prev nextToVisit
          let restToVisit' = readPageVisits page ++ restToVisit
          aux env ((page, nextToVisit, rpImports page) : acc) (S.insert nextToVisit visited) restToVisit'
