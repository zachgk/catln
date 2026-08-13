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
import           Data.Maybe              (catMaybes, fromMaybe, isJust,
                                          listToMaybe, mapMaybe)
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
import           Syntax.Obsidian.Parser  (obsidian, obsidianParser)
import           Syntax.Python.Parser    (pyParser)
import           System.Directory        (doesDirectoryExist,
                                          getModificationTime, listDirectory,
                                          pathIsSymbolicLink)
import           System.FilePath         (isAbsolute, joinPath, takeDirectory,
                                          (</>))
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
-- program mean the same thing. Naming the parser is the only way to say which one a directory is
-- read with: a path is guessed from what is on disk, and 'Syntax.InferImport.inferRawImportStr'
-- guesses @dir@ for every directory, so an Obsidian vault has no path that names it as one.
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

readImport :: FileImport -> ImportParseResult
readImport imp = case maybeExprPath (impAbs imp) of
  Nothing -> fail $ printf "Invalid import %s must be string or oject" (show imp)
  Just impPath -> case H.lookup (nameStr impPath) importParsers of
    Nothing -> fail $ printf "No parser available for oject name in %s" (show imp)
    -- TODO Modify the ImportParseResult to CResT to report back parse errors
    Just parser -> ipParse parser $ impAbs imp

-- | Writes a page back as the file it was read from, by the parser which read it.
--
-- The mirror of 'readImport', and it finds its parser the same way: by the name at the head of the
-- import, which is what says the syntax the page is written in. A syntax with no reversion is
-- reported rather than passed over, so that a page which cannot be saved says so instead of being
-- saved as something else.
unparseImport :: FileImport -> RawPrgm () -> CRes String
unparseImport imp prgm = case maybeExprPath (impAbs imp) of
  Nothing -> fail $ printf "Invalid import %s must be string or oject" (show imp)
  Just impPath -> case H.lookup (nameStr impPath) importParsers of
    Nothing -> fail $ printf "No parser available for oject name in %s" (show imp)
    Just parser -> case ipUnparse parser prgm of
      Just src -> return src
      Nothing -> fail $ printf "The page %s can not be written back as %s" (show $ impRaw imp) (show $ nameStr impPath)

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
  rpPrgm    :: RawPrgm (),
  rpSource  :: Maybe String,
  -- | The imports the page writes: its edges in the import graph.
  rpImports :: [FileImport],
  -- | Every import the read walked on from, 'rpImports' included. The two differ for 'dirParser'
  -- and 'hsParser', which find imports the program does not write, so a page given back that
  -- replayed only its edges would drop every file of a directory out of the graph.
  rpVisits  :: [FileImport],
  -- | 'Nothing' where nothing timed the read, which is a page that is always read again: either the
  -- parser could not say what it read ('piReads') or the read was not one 'readFiles' made.
  rpReads   :: Maybe ReadTime
                         }

-- | When a path was last written, or nothing where there is no path to have been written.
modifiedAt :: FilePath -> IO (Maybe UTCTime)
modifiedAt file = either (const Nothing) Just <$> (try (getModificationTime file) :: IO (Either IOException UTCTime))

-- | When anything under a directory was last added, removed or renamed: the latest of the times its
-- directories were written to, at any depth.
--
-- A directory is written to when an entry appears in it, goes from it or is renamed within it, and
-- not when a file already in it is edited — so this answers what a reading of a tree's listings
-- depends on, and leaves what a reading of a file depends on to that file's own time. Every such
-- write stamps its directory with the time it happened, which is later than any time already there,
-- so the latest of them moves whenever the tree gains or loses anything.
--
-- What is walked into is 'Syntax.Ct.Prgm.readsTreeEntry', and directory symlinks are not followed,
-- which is what the readings that walk trees do ('Syntax.Obsidian.Parser.vaultNotes'). A tree timed
-- by more than the reading walked is a page read again for a change it never read — an Obsidian
-- vault writes its own @.obsidian@ configuration as it is used, which is not notes of it.
treeWrittenAt :: FilePath -> IO (Maybe UTCTime)
treeWrittenAt dir = do
  here <- modifiedAt dir
  entries <- either (const []) id <$> (try (listDirectory dir) :: IO (Either IOException [FilePath]))
  under <- mapM aux $ filter readsTreeEntry entries
  return $ maximumMaybe $ catMaybes $ here : under
  where
    aux entry = do
      let entryPath = dir </> entry
      isDir <- doesDirectoryExist entryPath
      isLink <- pathIsSymbolicLink entryPath
      if isDir && not isLink then treeWrittenAt entryPath else return Nothing
    maximumMaybe [] = Nothing
    maximumMaybe ts = Just $ maximum ts

-- | When what a read read at a path was last written.
--
-- A directory is timed by itself rather than by what it holds, because its listing is what was read
-- and a file already in it being edited leaves that listing as it was.
readPathWrittenAt :: ReadPath -> IO (Maybe UTCTime)
readPathWrittenAt (ReadFile file) = modifiedAt file
readPathWrittenAt (ReadDir dir)   = modifiedAt dir
readPathWrittenAt (ReadTree dir)  = treeWrittenAt dir

-- | Time what a read read, as of a read which began at the given time.
readTime :: UTCTime -> Maybe [ReadPath] -> IO (Maybe ReadTime)
readTime started = traverse (fmap (ReadTime started) . mapM (\read' -> (read',) <$> readPathWrittenAt read'))

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
      Just now -> now == was && maybe True (< rtStarted) now
      Nothing  -> False

-- | Whether writing to any of these paths is writing to something a read read at this path.
readPathWrittenIn :: S.HashSet FilePath -> ReadPath -> Bool
-- The file itself, as its text is what was read.
readPathWrittenIn written (ReadFile file) = S.member file written
-- A directory is read for its listing, so an entry appearing in it or going from it is a write to
-- it, and that entry is a path inside it.
readPathWrittenIn written (ReadDir dir) = S.member dir written || any ((== dir) . takeDirectory) written
-- Everything under the directory, at whatever depth, that a reading of the tree would have read.
readPathWrittenIn written (ReadTree dir) = any (readsUnder dir) written

processParsed :: CTSSConfig -> FileImport -> ParsedImport -> IO ReadPage
processParsed config imp ParsedImport{piPrgm=prgm@(RawPrgm prgmImports statements), piExtraImports=extraImports, piSource=src} = do
  let includeCore = not (rawPrgmHasAnnot prgm noCoreAnnot)
  let prgmImports' = if includeCore && not ("core" `isInfixOf` name)
      then mkRawFileImport (rawStr "core") : prgmImports
      else prgmImports
  prgmImports'' <- mapM (canonicalImport config (Just imp)) prgmImports'
  extraImports' <- mapM (canonicalImport config (Just imp)) extraImports
  let totalImports = extraImports' ++ prgmImports''
  let prgm' = RawPrgm prgmImports' statements
  let prgm'' = addRawPrgmMetaIDs (parseMetaIDScope imp) prgm'
  return $ ReadPage prgm'' src prgmImports'' totalImports Nothing
  where
    name = case impAbs imp of
      CExpr _ (CStr n) -> n
      TupleApply _ _ (EAppArg ObjArr{oaArr=Just (Just (CExpr _ (CStr n)), _)}) -> n
      _ -> ""

parseFile :: CTSSConfig -> RawFileImport -> CResT IO (GraphNodes (RawPrgm ()) FileImport)
parseFile config imp = do
  imp' <- lift $ canonicalImport config Nothing imp
  r <- lift $ readImport imp'
  page <- lift $ processParsed config imp' r
  return (rpPrgm page, imp', rpImports page)

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
  let paths = S.toList $ S.fromList $ concatMap (readPaths . snd) pages
  times <- H.fromList <$> mapM (\read' -> (read',) <$> readPathWrittenAt read') paths
  return $ H.fromList [page | page <- pages, readTimeCurrent times (rpReads $ snd page)]

-- | The page an import names: the one the last read left, or the files read again.
--
-- The time is taken before the files are read rather than after them, so that a file written while
-- it is being read is one the next read reads again. A time taken afterwards would say the read had
-- seen the write when it may have happened first.
readPage :: CTSSConfig -> H.HashMap FileImport ReadPage -> FileImport -> IO ReadPage
readPage config prev imp = case H.lookup imp prev of
  Just page -> return page
  Nothing -> do
    started <- getCurrentTime
    parsed <- readImport imp
    page <- processParsed config imp parsed
    times <- readTime started (piReads parsed)
    return page{rpReads = times}

-- | Read the files an import names, and everything they import.
--
-- A page of @prev@ is given back as it is, without its files being read at all, and the read walks
-- on through the imports it was left with. Which of them are still worth giving back is the
-- caller's to decide, by whatever it knows about what has changed ('unchangedPages' looks, where
-- there is nothing better), so that this reads what it is asked to read and no more.
--
-- TODO Change readImport, then make this return GraphData of CRes
readFiles :: CTSSConfig -> H.HashMap FileImport ReadPage -> [RawFileImport] -> IO (GraphData ReadPage FileImport)
readFiles config prev initialImps = do
  initialImps' <- mapM (canonicalImport config Nothing) initialImps
  res <- aux [] S.empty initialImps'
  return $ graphFromEdges res
  where
    aux :: [GraphNodes ReadPage FileImport] -> S.HashSet FileImport -> [FileImport] -> IO [GraphNodes ReadPage FileImport]
    aux acc _ [] = return acc
    aux acc visited (nextToVisit:restToVisit) = do
      if S.member nextToVisit visited
        then aux acc visited restToVisit
        else do
          page <- readPage config prev nextToVisit
          let restToVisit' = rpVisits page ++ restToVisit
          aux ((page, nextToVisit, rpImports page) : acc) (S.insert nextToVisit visited) restToVisit'
