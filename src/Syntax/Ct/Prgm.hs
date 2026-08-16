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
{-# LANGUAGE InstanceSigs              #-}

module Syntax.Ct.Prgm where

import           Data.Hashable
import qualified Data.HashMap.Strict   as H
import           GHC.Generics          (Generic)

import           Control.Exception     (IOException, try)
import           Data.Aeson            hiding (Object)
import           Data.IORef            (IORef, modifyIORef', newIORef,
                                        readIORef)
import           Data.List             (isPrefixOf, sort, stripPrefix)
import           Data.Maybe            (catMaybes, fromJust, isJust, mapMaybe)
import           Data.Time.Clock       (UTCTime)
import           Semantics
import           Semantics.Annots      (isAnnot)
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Obsidian.Cache (ObsidianCache, newObsidianCache)
import           System.Directory      (doesDirectoryExist, getModificationTime,
                                        listDirectory, pathIsSymbolicLink)
import           System.FilePath       (splitDirectories, takeFileName, (</>))
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

data TypeProperty e m
  = TypePropProj TypeName (e m)
  | TypePropRel TypeName (e m)
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

data RawApplyTerm e m = RATermDeep (e m) | RATermChild (e m)
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)
newtype RawApply e m = RawApply [RawApplyTerm e m]
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

-- Expr before desugar
data RawExpr m
  = RawCExpr (Meta m) Constant
  | RawValue (Meta m) TypeName
  | RawHoleExpr (Meta m) Hole
  | RawMacroValue (Meta m) TypeName
  | RawApplyExpr (Meta m) (RawApply RawExpr m)
  | RawFmtStrExpr (Meta m) String String
  | RawTheExpr (RawExpr m) -- ^ Written :TypeName and read as The TypeName
  | RawAliasExpr (RawExpr m) (RawExpr m) -- ^ base aliasExpr
  | RawTupleApply (Meta m) (Meta m, RawExpr m) [(Bool, RawObjArr RawExpr m)] -- Boolean for isSpreadArg
  | RawVarsApply (Meta m) (RawExpr m) [RawObjArr RawExpr m]
  | RawContextApply (Meta m) (Meta m, RawExpr m) [RawObjArr RawExpr m]
  | RawWhere (Meta m) (RawExpr m) (RawExpr m) -- ^ base cond
  | RawParen (RawExpr m)
  | RawMethod (Meta m) (RawExpr m) (RawExpr m) -- ^ base methodValue
  | RawList (Meta m) [RawExpr m]
  | RawTypeProp (Meta m) (RawExpr m) (TypeProperty RawExpr m)
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

data RawStatement e m
  = RawDeclStatement (RawObjArr e m)
  | RawBindStatement (RawObjArr e m) -- Uses <-
  | RawAnnot (CompAnnot (e m))
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

data RawStatementTree e m = RawStatementTree (RawStatement e m) [RawStatementTree e m]
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

type RawFileImport = AFileImport (RawExpr ())
data RawPrgm m = RawPrgm [RawFileImport] [RawStatementTree RawExpr m] -- TODO: Include [Export]
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

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
  = ReadFile FilePath
  -- | A file a reading found without reading it, and would have done something else without.
  -- 'Syntax.Obsidian.Parser.notePrgm' resolves each of a note's wiki links to a note of the vault
  -- and imports it, which is a reading of that note's name and of nothing inside it.
  | ReadExists FilePath
  -- | A directory whose listing was read.
  | ReadDir FilePath
  -- | A directory and the listings under it, at any depth.
  | ReadTree FilePath
  deriving (Eq, Ord, Show, Generic, Hashable)

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

-- | What a read has already found on disk, so that one read finds it once, and what it may answer
-- without looking at all.
data ReadEnv = ReadEnv {
  -- | The trees this read has walked.
  --
  -- A read reads many pages out of the one tree. Every note of an Obsidian vault resolves its links
  -- against the notes of that vault, and the page which is the vault holds all of them, so a vault
  -- of a thousand notes would be walked a thousand and one times by a read which walked it for
  -- each.
  --
  -- Scoped to one read rather than kept between reads. What is on disk changes between reads, and
  -- deciding that it has is what 'ReadPath' and the times a read records already are.
  reScans         :: IORef (H.HashMap FilePath TreeScan),
  -- | What an earlier run recorded of the Obsidian vaults this read reaches, and what this read has
  -- to write back to them. Absent for a read which is to read every page it reaches.
  --
  -- Named for the one syntax which has such a record rather than being a table of them, because
  -- what a second syntax wanting one would share with this is not yet known — see
  -- 'Syntax.Obsidian.Cache'. It is here, and not inside that module, because a read reads many
  -- notes of one vault and each would otherwise read that vault's record for itself: this is the
  -- only thing a read has which lasts a whole read.
  --
  -- This is the cache 'reScans' above says a read does not keep between reads, and the difference
  -- is that this one decides for itself whether it still holds ('Syntax.Obsidian.Cache.cachedNoteLinks')
  -- rather than being trusted because a read is under way.
  reObsidianCache :: Maybe ObsidianCache
                       }

-- | A read which reads every page it reaches, keeping and consulting no record.
newReadEnv :: IO ReadEnv
newReadEnv = newRecordedReadEnv Nothing

-- | A read which may answer for a page out of what an earlier run recorded.
newRecordedReadEnv :: Maybe FilePath -> IO ReadEnv
newRecordedReadEnv cacheRoot = do
  scans <- newIORef H.empty
  cache <- traverse newObsidianCache cacheRoot
  return $ ReadEnv scans cache

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
  isReader  :: ReadEnv -> IO ImportReader,
  -- | Writes a page back as the file it was read from, or nothing when that syntax has no reversion.
  --
  -- A reversion writes the program rather than the file: it may write back what the file said in a
  -- form other than the one the file said it in.
  isUnparse :: RawPrgm () -> Maybe String
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
  irFinish  :: IO ()
                                 }

-- | The preload of a syntax which can say what a page imports only by reading it: the one read
-- answers both questions, so the program it produced is handed on rather than read for again.
eagerPreload :: ParsedImport -> PreloadImport
eagerPreload parsed@ParsedImport{piPrgm=RawPrgm imports _, piReads} = PreloadImport imports [] (Just parsed) piReads

-- | A syntax which needs nothing of a read at all: its pages are read out of the files they name
-- and out of nothing else, so the reader made for each read is the same reader every time.
statelessSyntax :: (Expr () -> ImportPreloadResult) -> (Expr () -> ImportParseResult) -> (RawPrgm () -> Maybe String) -> ImportSyntax
statelessSyntax preload parse unparse = ImportSyntax (const $ return $ ImportReader preload parse (return ())) unparse

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

-- | Checks whether a program has a global annot
rawPrgmHasAnnot :: RawPrgm m -> String -> Bool
rawPrgmHasAnnot (RawPrgm _ statements) annot = any checkStatementTree statements
  where
    checkStatementTree (RawStatementTree (RawAnnot a) _) = isAnnot annot a
    checkStatementTree _                                 = False
