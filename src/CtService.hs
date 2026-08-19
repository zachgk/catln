--------------------------------------------------------------------
-- |
-- Module    :  CtService
-- Copyright :  (c) Zach Kimberg 2024
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is a top-level manager for the compilation pipeline.
-- It supports partial and incremental compilation and rebuilding, for both the Main build and pieces like webdocs or IDE integration.
--------------------------------------------------------------------

module CtService where
import           Control.Concurrent      (MVar, forkIO, modifyMVar, modifyMVar_,
                                          newMVar, readMVar, withMVar)
import           Control.Concurrent.STM  (STM, TVar, atomically, modifyTVar',
                                          newTVarIO, readTVar, readTVarIO,
                                          retry, writeTVar)
import           Control.Exception       (bracket)
import           Control.Monad           (forM, forM_, forever, unless, void,
                                          when)
import           Control.Monad.Trans     (lift)
import           CRes
import           CtConstants             (testAnnot)
import           Data.Graph
import qualified Data.HashMap.Strict     as H
import qualified Data.HashSet            as S
import           Data.List               (elemIndex, intercalate, partition,
                                          sortOn)
import           Data.Maybe
import           Data.UUID               (UUID)
import           Eval                    (evalAnnots, evalBaseEnvStages,
                                          evalBuild, evalBuildAll,
                                          evalDShowCall, evalDebugExprPrgm,
                                          evalDebugPrgm, evalPageValues,
                                          evalRun, evalRunMock,
                                          prgmFromGraphData)
import           Eval.Common             (DebugRun, EvalMetaDat, MockIO,
                                          SSF (..), StagePrgms (..), TExpr,
                                          TestResult, Val, oaTestId)
import           Eval.Testing            (TestDesc (..), evalPrints, evalTest,
                                          runTestDesc)
import           FileWatch               (Watch, Watcher, allCovered,
                                          awaitSettled, covered, startWatcher,
                                          watchReads, withPending)
import           MapMeta                 (parseMetaIDScope)
import           Semantics               (CTSSConfig)
import           Semantics.Annots        (hasAnnot)
import           Semantics.Prgm
import           Semantics.Types         (Name)
import           Syntax.Ct.Desugarf      (desFinalPass, desFinalPasses, desPrgm,
                                          validPrgm)
import           Syntax.Ct.Desugarf.Expr (desFileImport)
import           Syntax.Ct.MapRawMeta    (addRawPrgmMetaIDs,
                                          rawStatementExprIds)
import           Syntax.Ct.Parser.Expr   (pExpr)
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Syntax.Parsers
import           System.FilePath         (takeDirectory)
import           Text.Megaparsec         (errorBundlePretty, runParser)
import           Text.Printf
import           TypeCheck               (typecheckPrgms)
import           TypeCheck.Common        (TPrgm, TraceConstrain, VPrgm,
                                          typeCheckToRes)
import           Utils

type TBPrgm = Prgm TExpr EvalMetaDat

newtype CTSS = CTSS (MVar CTSSDat)

-- Main data type for CtService Status
data CTSSDat = CTSSDat {
  ctssConfig        :: CTSSConfig,
  ctssBaseFileNames :: [String],
  ctssData          :: GraphData SSF FileImport,
  -- | A watch over the files the pages were read from, for a service read from over and over.
  --
  -- Without one a read looks at every file the service holds to find what changed, which costs what
  -- the program is; with one it costs what the change is. Absent by default because a service read
  -- once, or told what changed by whoever changed it, has nothing to gain from it.
  ctssWatcher       :: Maybe Watcher,
  -- | Held for a whole read, so that two reads cannot each take half of what has changed.
  --
  -- Not the lock the graph is behind: a read holding that one across its parsing would put every
  -- read behind a build, which holds it for a whole SCC.
  ctssReadLock      :: MVar (),
  -- | What readers of the service have open, for a service read from by something which is watching
  -- rather than asking.
  ctssShown         :: TVar Displays
                                       }

-- | What one thing a reader has open is compiled from, which is what decides whether a read reaches
-- it.
--
-- Held by the service rather than worked out by whoever is showing it, because deciding it needs the
-- import graph the change was computed in — and because what a reader is told has to be about the
-- one thing in front of them rather than about the program. A write low in the standard library
-- invalidates nearly every page there is, so a service which said what had changed would be sending
-- a message about the whole program in order to say something about one page.
newtype Shown = Shown {
  -- | The page whose program it is compiled from, where it is compiled from one. What that page
  -- imports comes with it: a page is shown from its own program and the programs it imports, which
  -- is the closure 'ctssClearInvalidations' walks the other way. Nothing for something compiled
  -- against every page there is.
  shPage :: Maybe FileImport
                      }

-- | The displays readers have open, and how to wake each.
data Displays = Displays {
  -- | The number the next display opened is given. Numbered rather than keyed by what is shown,
  -- because two readers looking at one page are two displays to wake, and one of them closing is not
  -- the other closing.
  dsNext :: Int,
  -- | Each display, with the flag which says a read reached it.
  dsOpen :: H.HashMap Int (Shown, TVar Bool)
                         }

-- | What a read changed.
--
-- It does not leave the service. It is the whole import graph's worth of consequence — a page low in
-- the standard library carries almost every other page with it — and what a reader is told is
-- whether it reached what they have open ('changeReaches').
data ReadChange = ReadChange {
  -- | The pages the read dropped built work from: those something they were read from was written
  -- to, and every page which imports one of them at any depth.
  rcChanged :: S.HashSet FileImport,
  -- | Whether the pages there are have changed. Something shown from the pages there are is changed
  -- by a page appearing or going without any page it is compiled from being invalidated.
  rcPages   :: Bool
                             }

-- | Whether a read reaches what is being shown.
--
-- Everything shown is about which pages there are, whatever else it is about, so a page appearing or
-- going reaches all of it: a navigation lists the pages, and a page names the pages either side of it
-- in the order they are read in. That is a change no page it is compiled FROM was invalidated by,
-- which is why it is asked separately.
--
-- Reaching a display is not the same as changing it. A page appearing between two others leaves every
-- page but its neighbours saying exactly what it said, and whoever is answering says nothing where
-- the answer has not changed ('WebDocs.serve') — which is decided by the answer rather than guessed
-- at from here, since what a thing shows is its own business.
changeReaches :: ReadChange -> Shown -> Bool
changeReaches ReadChange{rcChanged, rcPages} Shown{shPage} =
  rcPages || maybe (not $ S.null rcChanged) (`S.member` rcChanged) shPage

emptyCTSS :: CTSSConfig -> IO CTSS
emptyCTSS config = do
  v <- newMVar =<< newCTSSDat config []
  return $ CTSS v

newCTSSDat :: CTSSConfig -> [String] -> IO CTSSDat
newCTSSDat config baseFileNames = CTSSDat config baseFileNames (graphFromEdges []) Nothing
  <$> newMVar () <*> newTVarIO (Displays 0 H.empty)

-- | A page as a read of it left it, with nothing yet built from it.
--
-- The source and the program are taken from the read rather than given, so that what a page holds
-- and what it was read from cannot come apart.
newSSF :: ReadPage -> SSF
newSSF rp = SSF { ssfSource = rpSource rp
                , ssfRaw = pure <$> rpPrgm rp
                , ssfRead = rp
                , ssfBuilt = False
                , ssfDes = Nothing
                , ssfTPrgmWithTrace = Nothing
                , ssfTPrgm = Nothing
                , ssfTBPrgm = Nothing
                , ssfAnnots = Nothing
                , ssfTests = Nothing
                }

ctssSetFiles :: CTSS -> [String] -> IO ()
ctssSetFiles (CTSS ssmv) baseFileNames = do
  modifyMVar_ ssmv $ \ctss -> return ctss{ctssBaseFileNames=baseFileNames}
  ctssRead (CTSS ssmv)

ctssBaseFiles :: CTSSConfig -> [String] -> IO CTSS
ctssBaseFiles config baseFileNames = do
  v <- newMVar =<< newCTSSDat config baseFileNames
  let ctss = CTSS v
  ctssRead ctss
  return ctss

-- | Watch the files the service is read from, so that later reads read only what has changed.
--
-- For a service read from over and over, rather than read once and thrown away. Without it a read
-- looks at every file the service holds, which is cheap next to reading them but is still work
-- proportional to the program rather than to the change.
ctssWatch :: CTSS -> IO ()
ctssWatch ctss@(CTSS ssmv) = do
  -- A watch reaching under a directory reports whatever a reading of that tree would have read, so
  -- that a path no reading reads is a path no read is woken for. Without it a vault open in Obsidian
  -- reports a write every time its configuration is touched, and every read of the service goes the
  -- long way round to find that nothing it holds has changed.
  watcher <- startWatcher readsUnder
  modifyMVar_ ssmv $ \ss -> return ss{ctssWatcher=Just watcher}
  -- The watches go on here rather than being left to the next read. A read only sees to them when
  -- it read something, and the read after this one gives every page back — so leaving it to a read
  -- would leave a service watching nothing, and answering forever with what it read once.
  ctssRewatch ctss watcher

-- | Read the service whenever a file it was read from is written to, and wake whatever that reached.
--
-- The reads are the reads a request makes; what this adds is who asks for them and when. A reader
-- watching a page is not asking — that is what watching is — so without this a doc server holding a
-- page nobody is clicking on never notices that the page has changed.
--
-- For a service that is watching ('ctssWatch'); there is nothing to wait on without a watcher, and
-- a service told what changed by whoever changed it has nothing to wait for.
ctssWatchLive :: CTSS -> IO ()
ctssWatchLive ctss@(CTSS ssmv) = do
  CTSSDat{ctssWatcher} <- readMVar ssmv
  forM_ ctssWatcher $ \watcher -> void $ forkIO $ forever $ do
    awaitSettled watcher settleDelay
    readAll
  where
    -- What one save looks like from outside: the file, a rename, the directory holding it. A reader
    -- is served by one reading of all of them rather than by three readings of one, so the reading
    -- waits for the writing to stop before it starts.
    settleDelay = 150000
    -- Read until a read finds nothing. Watching a directory for the first time is what finds a page
    -- written between being read and being watched, and 'ctssRewatch' leaves such a page stale
    -- rather than read — it is the NEXT read that reads it. Reading once would leave those pages
    -- until something asked, and for a reader who is watching rather than asking that is forever.
    --
    -- It ends because a read with nothing written to and every path watched gives every page back
    -- ('ctssUnchanged'), which invalidates nothing, which is a read that found nothing.
    readAll = do
      change <- ctssReadOf ctss []
      unless (S.null (rcChanged change) && not (rcPages change)) readAll

-- | Follow what a display is shown from, for as long as it is open. What the action is given is a
-- wait which ends when a read has reached the display.
--
-- Bracketed rather than opened and closed, so a reader closing a tab, a connection dropping and a
-- handler failing all take the display off the list the same way. One left on it would be woken for
-- nobody, forever.
--
-- The display is on the list before the action runs, so a change arriving between opening it and
-- showing what it holds for the first time is one the next wait ends on, rather than one nothing
-- ever hears about.
--
-- The wait is handed back unrun, so that whoever is waiting can wait for something else at the same
-- time — a connection holds one of these open for as long as a reader is reading, and has its own
-- reasons to stop waiting.
ctssFollow :: CTSS -> Shown -> (STM () -> IO a) -> IO a
ctssFollow (CTSS ssmv) shown act = do
  CTSSDat{ctssShown} <- readMVar ssmv
  bracket (open ctssShown) (close ctssShown) $ \(_, reached) -> act (await reached)
  where
    open displays = do
      reached <- newTVarIO False
      atomically $ do
        Displays{dsNext, dsOpen} <- readTVar displays
        writeTVar displays $ Displays (dsNext + 1) (H.insert dsNext (shown, reached) dsOpen)
        return (dsNext, reached)
    close displays (i, _) = atomically $ modifyTVar' displays $ \d -> d{dsOpen=H.delete i (dsOpen d)}
    -- Taken rather than read, so a change is waited out once. A change arriving while whoever was
    -- woken by the last one is still working is left set, so the next wait ends at once instead of
    -- passing over it — showing what a page held two changes ago is the one thing a display which
    -- follows its page must not do.
    await reached = do
      hit <- readTVar reached
      unless hit retry
      writeTVar reached False

-- | How many displays are being followed.
--
-- Nothing acts on this — it is here to be asserted against. A display left on the list after it
-- closed is woken for nobody, which shows in no other way than by being counted.
ctssFollowing :: CTSS -> IO Int
ctssFollowing (CTSS ssmv) = do
  CTSSDat{ctssShown} <- readMVar ssmv
  H.size . dsOpen <$> readTVarIO ctssShown

-- | Wake every display a read reached.
--
-- Asked of the displays rather than of the pages: a change reaching a thousand pages nobody has open
-- wakes nothing, and the work is what is being read rather than what has changed.
ctssNotify :: CTSS -> ReadChange -> IO ()
ctssNotify (CTSS ssmv) change = do
  CTSSDat{ctssShown} <- readMVar ssmv
  atomically $ do
    Displays{dsOpen} <- readTVar ctssShown
    forM_ (H.elems dsOpen) $ \(shown, reached) ->
      when (changeReaches change shown) $ writeTVar reached True

-- | Read the files the service was given, and take up whatever they now say.
--
-- Only the pages whose files have been written to are read again; every other page is given back by
-- 'readFiles' as the last read left it. A read therefore costs what has changed rather than what the
-- program is, which is what lets a caller read as often as it likes instead of choosing between
-- working from what it read once and re-reading everything to find out what it now says.
--
-- The files are read outside the lock and merged into the graph inside it, against the graph as it
-- is by then rather than the one the read started from. Holding the lock across the read would put
-- every read behind a build, which holds the same lock for a whole SCC; merging into the graph the
-- read started from would throw away everything a build ('ctssBuildSCC') or a store ('ctssStore')
-- committed while the read was parsing.
ctssRead :: CTSS -> IO ()
ctssRead ctss = void $ ctssReadOf ctss []

-- | 'ctssRead', additionally reading in full the programs of the pages named.
--
-- The pages named are named to the read as pages it must have in full. A page it gives back as its
-- imports alone is read the rest of the way where it was named ('Syntax.Parsers.readPage'), which
-- is what turns a page held as its imports alone into a page held as its program, and a page whose
-- program it gives back is given back with it rather than read for a second time.
--
-- Every page whose program the service ALREADY holds is named too, whoever asked. Such a page may
-- be read again for a reason of its own — it has changed, or 'ctssStale' has had it forgotten — and
-- a read of it which brought only its imports would lose the program the service holds: 'buildSSF'
-- below cannot match a program it holds against a read which brought none, so the page would be
-- replaced by one with no program and everything built from it thrown away.
ctssReadOf :: CTSS -> [FileImport] -> IO ReadChange
ctssReadOf ctss@(CTSS ssmv) load = do
  CTSSDat{ctssConfig, ctssBaseFileNames, ctssData=before, ctssWatcher, ctssReadLock} <- readMVar ssmv
  change <- withMVar ctssReadLock $ \() -> withWritten ctssWatcher $ \written -> do
    unchanged <- ctssUnchanged ctssWatcher written [(prgmName, ssfRead ssf) | (ssf, prgmName, _) <- graphToNodes before]
    let held = [prgmName | (ssf, prgmName, _) <- graphToNodes before, isJust (ssfRaw ssf)]
    rawPrgms <- readFiles ctssConfig (S.fromList $ load ++ held) unchanged (map mkRawImportStr ctssBaseFileNames)
    change <- modifyMVar ssmv $ \ss -> do
      let (data', invalidations) = unzip $ map (buildSSF (ctssData ss)) $ graphToNodes rawPrgms
      let read'' = graphFromEdges data'
      let pagesChanged = pageKeys (ctssData ss) /= pageKeys read''
      let (changed, ss') = ctssClearInvalidations pagesChanged (catMaybes invalidations) ss{ctssData=read''}
      return (ss', ReadChange changed pagesChanged)
    -- The watches only need seeing to where the read read something, since they are watches on what
    -- the reads read. A read which gave every page back left every path it was watching for exactly
    -- as it was, and gathering them up again to say so would be work for every path of every page,
    -- to find nothing to do. A read with pages asked of it is not one of those: a page given back
    -- as its imports alone is read the rest of the way where it was asked for, which is a read of a
    -- page the read also gave back. Taken as having read something whenever anything was asked of
    -- it, rather than by looking for which of them were read the rest of the way, since seeing to
    -- watches that were already seen to costs a set difference and finds nothing.
    let read' = graphToNodes rawPrgms
    let readNothing = null load && length read' == H.size unchanged && all (\(_, prgmName, _) -> H.member prgmName unchanged) read'
    unless readNothing $ forM_ ctssWatcher $ ctssRewatch ctss
    return change
  -- Outside the read, so that a reader woken by it can read the service without waiting on the read
  -- that woke them, and so that a display failing to be woken is not a change put back as unread.
  ctssNotify ctss change
  return change
  where
    pageKeys = S.fromList . map snd3 . graphToNodes
    buildSSF held (page, prgmName, imports) = case graphLookup prgmName held of
      -- A page given back unread is the page already held, so there is nothing to compare it
      -- against and nothing to invalidate. The comparison below walks the whole syntax tree, so a
      -- read which read nothing would otherwise compare every page against itself.
      --
      -- Read times identify the read rather than the files: a page read again is timed afresh
      -- before it is parsed, so times equal to the ones the page already holds are the very times
      -- it was handed back with.
      Just old | isJust (rpReads page), rpReads (ssfRead old) == rpReads page -> ((old, prgmName, imports), Nothing)
      -- A page which read back the same program keeps everything built from it, but takes the new
      -- source text: a change the parse discards (trailing whitespace) leaves the program equal
      -- while the file on disk is no longer the one the page last served. It takes the new read
      -- too, or a page written to without changing would be read again on every read forever.
      Just old@SSF{ssfRaw=Just (CRes _ oldRaw)} | Just oldRaw == rpPrgm page -> ((old{ssfSource=rpSource page, ssfRead=page}, prgmName, imports), Nothing)
      -- A page still held as its imports alone, read again as its imports alone. Nothing was built
      -- from it to invalidate and nothing about it has changed, so this is not a change — without
      -- this arm every read of a page nothing has asked for yet would invalidate every page which
      -- imports it.
      Just old@SSF{ssfRaw=Nothing} | isNothing (rpPrgm page), rpImports (ssfRead old) == imports -> ((old{ssfRead=page}, prgmName, imports), Nothing)
      Just{} -> ((newSSF page, prgmName, imports), Just prgmName)
      Nothing -> ((newSSF page, prgmName, imports), Nothing)

-- | Act on the paths a watcher saw written, where there is one to have seen any.
withWritten :: Maybe Watcher -> (S.HashSet FilePath -> IO a) -> IO a
withWritten Nothing act        = act S.empty
withWritten (Just watcher) act = withPending watcher act

-- | The pages a read need not read again: those nothing they were read from has been written to.
--
-- A watched page is answered from what the watcher saw written, which costs nothing to ask. Every
-- other page — one no watcher covers, or one whose read could not say what it read — is looked at
-- instead, so that a page is given back only when something can say it is unchanged.
ctssUnchanged :: Maybe Watcher -> S.HashSet FilePath -> [(FileImport, ReadPage)] -> IO (H.HashMap FileImport ReadPage)
ctssUnchanged Nothing _ pages = unchangedPages pages
ctssUnchanged (Just watcher) written pages = do
  everyPath <- allCovered watcher
  if S.null written && everyPath then return (H.fromList pages) else ctssUnchangedOf watcher written pages

-- | 'ctssUnchanged' where something has been written to, or some path is one the watcher cannot
-- answer for.
--
-- Split out because it asks its question of each path of each page, and a page may be read from as
-- many paths as the program has pages — a page read from a directory listing is read from it
-- however many other pages that listing holds. Asking that of every page every time would be the
-- program squared, to find that nothing had changed; nothing having changed is answered whole
-- instead, and this is reached only when something has.
--
-- What the watcher says is which pages a write could have reached, not what it did to them: a note
-- written to and a note deleted are the one event, and only the page which read that file's text
-- is changed by the first. So the pages the write reached are looked at, which is exact and costs
-- what the change is, and the pages it did not reach are given back without a syscall — which is
-- the whole of the program but for the change.
ctssUnchangedOf :: Watcher -> S.HashSet FilePath -> [(FileImport, ReadPage)] -> IO (H.HashMap FileImport ReadPage)
ctssUnchangedOf watcher written pages = do
  watched <- covered watcher
  let isWatched page = maybe False (all (all (`S.member` watched) . readPathWatches . fst) . rtPaths) (rpReads page)
  let (byWatch, byLook) = partition (isWatched . snd) pages
  let (reached, untouched) = partition (any (readPathWrittenIn written) . readPaths . snd) byWatch
  looked <- unchangedPages (byLook ++ reached)
  return $ H.union looked $ H.fromList untouched

-- | The watches it takes to notice a change to what a read read.
--
-- A file needs the directory holding it, so that it being written to, appearing or going is seen. A
-- directory needs itself, so that an entry appearing in it is seen — a listing is changed by what
-- appears in it, which the directory holding it says nothing about — and the directory holding it,
-- so that the directory itself going or being renamed is seen. A tree needs one watch reaching
-- under it, which says all of that for every directory in it at once.
--
-- This is where what was read meets what is watched, and it is the only place the two vocabularies
-- touch. Answering in whole directories rather than in the paths themselves is what lets the
-- watcher de-duplicate: every note of an Obsidian vault reads the vault, so a vault of any size
-- asks for the one watch on it however many notes it holds.
readPathWatches :: ReadPath -> [Watch]
readPathWatches (ReadFile file)   = [(takeDirectory file, False)]
readPathWatches (ReadExists file) = [(takeDirectory file, False)]
readPathWatches (ReadDir dir)     = [(dir, False), (takeDirectory dir, False)]
readPathWatches (ReadTree dir)    = [(dir, True), (takeDirectory dir, False)]

-- | Watch the files the pages were read from, and look at whichever of them were read before there
-- was a watch on them.
--
-- The pages read before their files were watched are the gap the watcher cannot close by itself: a
-- write between the reading of a file and the watching of it is one no event will ever report.
-- Every page is in that gap when a service first starts watching, and a page found in a directory
-- nothing had been read from before is in it afterwards, so both are answered the same way — by
-- looking, once, at the pages whose files have only now come to be watched.
ctssRewatch :: CTSS -> Watcher -> IO ()
ctssRewatch (CTSS ssmv) watcher = do
  CTSSDat{ctssData} <- readMVar ssmv
  before <- covered watcher
  watchReads watcher $ concatMap readPathWatches $ concatMap (readPaths . ssfRead . fst3) $ graphToNodes ctssData
  after <- covered watcher
  let fresh = S.difference after before
  unless (S.null fresh) $ do
    let freshly = [(prgmName, ssfRead ssf) | (ssf, prgmName, _) <- graphToNodes ctssData
                                           , any (any (`S.member` fresh) . readPathWatches) $ readPaths $ ssfRead ssf]
    current <- unchangedPages freshly
    forM_ freshly $ \(prgmName, _) -> unless (H.member prgmName current) $ ctssStale (CTSS ssmv) prgmName

-- | Drop what was built from the pages a change reached, and say which pages those were.
--
-- Which pages a change reached is the graph walked the other way: edges run from a page to its
-- imports, so the pages which import a changed one are what 'transposeG' finds. Answered as well as
-- acted on, because it is also what decides whether the change reaches anything a reader has open
-- ('changeReaches'), and working it out a second time would be walking the graph a second time.
ctssClearInvalidations :: Bool -> [FileImport] -> CTSSDat -> (S.HashSet FileImport, CTSSDat)
ctssClearInvalidations pagesChanged invalidations ss@CTSSDat{ctssData=(g, nodeFromVertex, vertexFromKey)} = (propagated, ss{ctssData=fmapGraphWithKey maybeClear $ ctssData ss})
  where
    propagated = S.fromList $ map (snd3 . nodeFromVertex) $ foldMap (foldMap (:[])) $ dfs (transposeG g) (mapMaybe vertexFromKey invalidations)
    maybeClear prgmName ssf
      -- Rebuilt from the read the page already has: an invalidation drops what was built from a
      -- page, not what the page was read from, so the next read still gives it back unread.
      | prgmName `S.member` propagated = newSSF (ssfRead ssf)
      | null invalidations && not pagesChanged = ssf
      -- Which tests a page has, and what they find, is decided over the whole program the page
      -- belongs to rather than over the page and its imports (see 'ctssTestsFrom'). Invalidation
      -- walks the pages that DEPEND on the changed one, so it never reaches a page the change is
      -- downstream of — whose tests the change can still alter. Every page's tests are therefore
      -- dropped by any change at all, and re-run for whichever page is asked about next.
      | otherwise = ssf{ssfTests = Nothing, ssfAnnots = keptAnnots ssf}

    -- | A page's annotations, where the change cannot have altered them.
    --
    -- An annotation is evaluated against the stages of the page's own program ('ctssStagePrgmsFrom'),
    -- which the walk above covers, and against two things which reach past them: which pages there
    -- are ('StagePrgms.spAllPages'), and the page's tests already run ('StagePrgms.spTests'). Both
    -- can change without any page the annotation is compiled from being invalidated, and a cached
    -- annotation is what 'getEvalAnnots' answers with before it looks at either — so the cache is
    -- what has to be dropped.
    --
    -- Asked of each rather than dropped wholesale, because evaluating a page's annotations is work
    -- and every change would otherwise cost it for every page: which pages there are changes only
    -- when a page appears or goes, and a test run can only be read by a page which writes a test,
    -- since 'getEvalAnnots' hands the run to no other page.
    keptAnnots ssf
      | pagesChanged      = Nothing
      | writesATest ssf   = Nothing
      | otherwise         = ssfAnnots ssf

    writesATest ssf = maybe False prgmWritesATest (cresToMaybe =<< ssfTPrgm ssf)

-- | Build pages incrementally, one SCC at a time.
-- Each successfully built SCC is committed to the MVar independently,
-- so earlier SCCs (e.g. core library) remain cached even if a later SCC fails.
ctssBuildPagesExact :: CTSS -> GraphData SSF FileImport -> IO ()
ctssBuildPagesExact _ pages | all (ssfBuilt . fst3) (graphToNodes pages) = return ()
ctssBuildPagesExact (CTSS ssmv) pages = do
  -- stronglyConnCompR returns SCCs in reverse topological order (dependencies first)
  let sccs = stronglyConnCompR $ graphToNodes pages
  forM_ sccs $ \comp -> do
    let sccNodes = flattenSCC comp
    unless (all (ssfBuilt . fst3) sccNodes) $
      ctssBuildSCC ssmv sccNodes pages

-- | Build a single SCC and commit the result to the MVar atomically.
ctssBuildSCC :: MVar CTSSDat -> [(SSF, FileImport, [FileImport])] -> GraphData SSF FileImport -> IO ()
ctssBuildSCC ssmv sccNodes pages = do
  let sccFileNames = map snd3 sccNodes
  let sccNamesSet = S.fromList sccFileNames
  -- Get dep lists from the pages graph structure (stable across MVar updates)
  let pageDeps = H.fromList [(name, deps) | (_, name, deps) <- graphToNodes pages]
  modifyMVar_ ssmv $ \ctssdat@CTSSDat{ctssData} -> do
    -- Re-check with current state (another thread may have built this SCC)
    let currentSSFs = mapMaybe (`graphLookup` ctssData) sccFileNames
    if all ssfBuilt currentSSFs
      then return ctssdat
      else do
        -- Look up current SSFs and deps for SCC members
        let currentSCCNodes = mapMaybe (\name -> do
              ssf <- graphLookup name ctssData
              let deps = fromMaybe [] $ H.lookup name pageDeps
              return (ssf, name, deps)) sccFileNames
        p <- runCResT $ buildSCCPipeline ctssData sccNamesSet currentSCCNodes pages
        case p of
          CRes notes updates -> do
            unless (null notes) $ putStrLn $ prettyCNotes notes
            let updatedData = fmapGraphWithKey (\k ssf -> fromMaybe ssf (H.lookup k updates)) ctssData
            return ctssdat{ctssData = updatedData}
          CErr notes -> do
            unless (null notes) $ putStrLn $ prettyCNotes notes
            fail "Could not build catln service"

-- | Run the full pipeline (desugar → typecheck → build → annotate) for a single SCC.
-- Returns a map of updated SSFs for the SCC members.
buildSCCPipeline :: GraphData SSF FileImport -> S.HashSet FileImport
                 -> [(SSF, FileImport, [FileImport])] -> GraphData SSF FileImport
                 -> CResT IO (H.HashMap FileImport SSF)
buildSCCPipeline ctssData sccNamesSet sccNodes pages = do
  -- Get raw programs for all SCC members
  rawPrgms <- forM sccNodes $ \(ssf, name, deps) -> do
    -- Read by whoever asked for the build ('ctssLoadOf'), so a page still unread here is a build of
    -- a page nothing read rather than a page to build without its program.
    raw <- asCResT $ requireComputed $ ssfRaw ssf
    return (raw, ssf, name, deps)

  -- Separate valid programs from ctx-annotated ones (which skip the pipeline)
  let (validRaws, invalidRaws) = partition (\(raw, _, _, _) -> validPrgm raw) rawPrgms
  let invalidUpdates = [(name, ssf{ssfBuilt=True}) | (_, ssf, name, _) <- invalidRaws]

  if null validRaws
    then return $ H.fromList invalidUpdates
    else do
      let namesAndDeps = [(name, deps) | (_, _, name, deps) <- validRaws]

      -- Step 1: Desugar each raw program individually
      desResults <- forM validRaws $ \(raw, _, name, deps) ->
        asCResT $ desPrgm (raw, name, deps)

      -- Step 2: Collect data from already-built transitive dependencies
      -- All non-SCC nodes in `pages` are potential external deps
      let extDepNames = filter (not . (`S.member` sccNamesSet)) $
            map snd3 $ graphToNodes pages
      let extDepDes = mapMaybe (\name ->
            graphLookup name ctssData >>= ssfDes >>= cresToMaybe
            ) extDepNames

      -- Step 3: Run final desugar passes with SCC peers and external deps
      let desNodesForFinal = [(des, name, peerDes, extDepDes)
            | (des, name, _) <- desResults
            , let peerDes = [d | (d, n, _) <- desResults, n /= name]]
      let finalDes = desFinalPasses desNodesForFinal

      -- Step 4: Collect typechecked data from already-built dependencies
      let extDepTC = mapMaybe (\name ->
            graphLookup name ctssData >>= ssfTPrgmWithTrace >>= cresToMaybe
            ) extDepNames

      -- Step 5: Typecheck with SCC peers and external deps
      let tcNodesForTC = [(des, peerDes, extDepTC)
            | (des, (name, _)) <- zip finalDes namesAndDeps
            , let peerDes = [d | (d, (n, _)) <- zip finalDes namesAndDeps, n /= name]]
      let tcCRes = typeCheckToRes $ typecheckPrgms tcNodesForTC
      let tcNotes = getCNotes tcCRes
      tcResults <- asCResT tcCRes

      -- Step 6: Construct TPrgm graph for eval (SCC results + built dep TPrgms)
      let sccTPrgmNodes = [( fst3 tc, name, deps)
            | (tc, (name, deps)) <- zip tcResults namesAndDeps]
      let depTPrgmNodes = mapMaybe (\(_, name, deps) ->
            if name `S.member` sccNamesSet then Nothing
            else do
              ssf <- graphLookup name ctssData
              ssfTPrgm ssf >>= cresToMaybe >>= \t -> Just (t, name, deps)
            ) (graphToNodes pages)
      let tprgmGraph = graphFromEdges (sccTPrgmNodes ++ depTPrgmNodes)

      -- Step 7: Eval build all (processes SCC + deps, we extract SCC results).
      -- Tolerate failure: a type-erroneous program cannot build a tree, but we
      -- still want to cache the desugar/typecheck artifacts (including the trace)
      -- for tooling. Any eval-build error is deferred into ssfTBPrgm below.
      let tbprgmGraphRes = evalBuildAll tprgmGraph

      -- Step 8: Assemble updated SSFs
      let validUpdates = [(name, ssf{
              ssfBuilt = True,
              ssfDes = Just $ pure des,
              -- Carries the notes but is deliberately not failOnErrorNotes: the whole point of
              -- keeping the trace is to explain a program that failed to typecheck, and the notes
              -- are what say why, so a type inference view needs both together.
              ssfTPrgmWithTrace = Just $ CRes tcNotes tc,
              -- failOnErrorNotes so eval consumers (run/test/build) cleanly reject
              -- a type-erroneous program, while ssfTPrgmWithTrace still carries the trace.
              ssfTPrgm = Just $ failOnErrorNotes $ CRes tcNotes (fst3 tc),
              ssfTBPrgm = case tbprgmGraphRes of
                CRes _ g  -> fmap pure $ graphLookup name g
                CErr errs -> Just $ CErr errs
            })
            | ((_, ssf, name, _), des, tc) <-
                zip3 validRaws finalDes tcResults]

      return $ H.fromList (validUpdates ++ invalidUpdates)

-- | Read the programs of the pages a selection of the graph has left unread.
--
-- A read may leave a page as its imports alone, so a page held is not a page whose program is held.
-- Whatever is to work with a page's program asks for it here first, and what it selects is what it
-- is about to work with: a build of everything selects everything, and a build of one page selects
-- what that page reaches.
--
-- Looped, because reading a page can add pages. A page answered out of a record names the imports
-- that record holds; reading it afresh can name others, and those arrive unread in their turn and
-- inside a selection which has only now grown to hold them. It ends because a page read stays read
-- ('ctssReadOf'), so each turn reads at least one page which was not read before — and a page asked
-- for which comes back unread is that promise broken, which is reported rather than looped on.
ctssLoadOf :: CTSS -> (GraphData SSF FileImport -> [FileImport]) -> IO ()
ctssLoadOf ctss@(CTSS ssmv) select = aux S.empty
  where
    aux asked = do
      CTSSDat{ctssData} <- readMVar ssmv
      let unread = [prgmName | prgmName <- select ctssData
                             , maybe False (isNothing . ssfRaw) (graphLookup prgmName ctssData)]
      unless (null unread) $ do
        case filter (`S.member` asked) unread of
          [] -> return ()
          again -> fail $ printf "Asked for %s and was given them back unread" (intercalate ", " $ map (show . impRaw) again)
        void $ ctssReadOf ctss unread
        aux $ S.union asked $ S.fromList unread

-- | Read the programs of these pages, where a read has left them unread.
ctssLoad :: CTSS -> [FileImport] -> IO ()
ctssLoad ctss names = ctssLoadOf ctss (const names)

ctssBuildFrom :: CTSS -> FileImport -> IO ()
ctssBuildFrom ctss@(CTSS ssmv) src = do
  ctssLoadOf ctss (map snd3 . graphToNodes . graphFilterReaches src)
  CTSSDat{ctssData} <- readMVar ssmv
  ctssBuildPagesExact ctss (graphFilterReaches src ctssData)

ctssBuildAll :: CTSS -> IO ()
ctssBuildAll ctss@(CTSS ssmv) = do
  ctssLoadOf ctss (map snd3 . graphToNodes)
  CTSSDat{ctssData} <- readMVar ssmv
  ctssBuildPagesExact ctss ctssData

-- | Bring the service into line with the files on disk: read whatever has changed, and build
-- whatever that left unbuilt.
--
-- What to ask for before working from the service, where the files may have been written to since
-- it was last read, so that what it holds is what the files now say.
ctssRefresh :: CTSS -> IO ()
ctssRefresh ctss = do
  ctssRead ctss
  ctssBuildAll ctss

ctssKeys :: CTSS -> IO [FileImport]
ctssKeys (CTSS ssmv) = do
  CTSSDat{ctssData} <- readMVar ssmv
  return $ map snd3 $ graphToNodes ctssData

-- | The page a service opens at: the first file it was started with. Which page that is, is the
-- service's own knowledge, and a reader of it would otherwise have to guess it out of a list of
-- every page. Resolved the same way a page asked for by name is, so it is a page that can be asked
-- for; a service started with no files at all has none.
ctssRootPage :: CTSS -> IO (Maybe String)
ctssRootPage (CTSS ssmv) = do
  CTSSDat{ctssConfig, ctssBaseFileNames} <- readMVar ssmv
  case ctssBaseFileNames of
    []       -> return Nothing
    (name:_) -> impDisp <$> mkDesCanonicalImportStr ctssConfig name

ctssGet :: (SSF -> Maybe (CRes a)) -> CTSS -> CResT IO (GraphData a FileImport)
ctssGet f (CTSS ssmv) = do
  lift $ ctssBuildAll (CTSS ssmv)
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  let computed = graphFromEdges $ filter (isJust . f . fst3) $ graphToNodes ctssData
  asCResT $ mapMGraph (requireComputed . f) computed

ctssGetFrom :: (SSF -> Maybe (CRes a)) -> FileImport -> CTSS -> CResT IO (GraphData a FileImport)
ctssGetFrom f prgmName (CTSS ssmv) = do
  lift $ ctssBuildFrom (CTSS ssmv) prgmName
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  asCResT $ mapMGraph (requireComputed . f) (graphFilterReaches prgmName ctssData)

requireComputed :: Maybe (CRes r) -> CRes r
requireComputed = fromMaybe (CErr [MkCNote $ GenCErr Nothing "Value not computed in catln service"])

-- | The pages of a scope which can be evaluated: those whose programs typechecked, with why the rest
-- could not carried as notes rather than failing the lot.
--
-- A page which did not typecheck has definitions carrying no type, and an environment cannot be
-- built over one at all — 'TreeBuild.buildTBEnv' raises rather than reporting when it meets a
-- definition typed 'PTopType'. That is what 'ssfTPrgm' failing such a page is guarding: an
-- evaluation handed the whole scope does not report the type error, it dies on it.
--
-- So the pages which cannot be built are left out and said in the notes. What is evaluated is then
-- what compiled: an annotation which reads none of the broken page — a navigation is about which
-- pages there are and reads no program at all — answers exactly as it did before anything broke,
-- one which reads it fails to find what it names, and either way the type errors are in the answer.
-- Leaving them in would make every annotation in a doc server go out with the first page a reader
-- breaks, which is the moment a doc server is most worth reading.
ctssEvaluableFrom :: (SSF -> Maybe (CRes a)) -> Maybe FileImport -> CTSS -> CResT IO (GraphData a FileImport)
ctssEvaluableFrom f scope ctss@(CTSS ssmv) = do
  lift $ maybe (ctssBuildAll ctss) (ctssBuildFrom ctss) scope
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  let inScope = graphToNodes $ maybe ctssData (`graphFilterReaches` ctssData) scope
  asCResT $ evaluablePages f (isJust scope) inScope

-- | The pure half of 'ctssEvaluableFrom': which of a set of pages already built can be evaluated,
-- given the stage to read off each. 'ctssEvaluableFrom' is this plus the building and reading that
-- produce that set; separate so that a caller which already holds the pages — the debugger's
-- expression compile, 'debugExprRun', which runs inside an evaluation and so has no 'IO' — can ask
-- the same question without asking for the same work again.
--
-- @scoped@ is whether the pages are the ones an evaluation was scoped to, which is what decides
-- whether a page nothing has computed is an error or is simply not one of the pages being asked
-- about.
evaluablePages :: (SSF -> Maybe (CRes a)) -> Bool -> [(SSF, FileImport, [FileImport])] -> CRes (GraphData a FileImport)
evaluablePages f scoped inScope = do
  -- An evaluation over every page passes over one nothing has computed and an evaluation scoped to a
  -- page requires the pages it was asked for, which is the distinction 'ctssGet' and 'ctssGetFrom'
  -- already draw. Kept, so that leaving out what cannot be built is the only thing that is new here.
  let asked = if scoped then inScope else filter (isJust . f . fst3) inScope
  -- Judged by 'ssfTPrgm', which is the stage that fails a page that did not typecheck, whatever
  -- stage the caller is asking for: a page whose type check failed is one nothing downstream of it
  -- can be built from either.
  --
  -- Only a page that FAILED, never one that was not computed. The two are different things and only
  -- the first is understood here: a page nothing has built is a page this was asked about too early,
  -- which 'requireComputed' below reports as it always did. Leaving it out instead would answer as
  -- though the page held nothing.
  let failedToTypecheck ssf = case ssfTPrgm ssf of
        Just CErr{} -> True
        _           -> False
  let (broken, buildable) = partition (failedToTypecheck . fst3) asked
  _ <- CRes (concatMap (getCNotes . requireComputed . ssfTPrgm . fst3) broken) ()
  mapMGraph (requireComputed . f) (graphFromEdges buildable)

ctssGetJoined :: (Monoid a) => (SSF -> Maybe (CRes a)) -> CTSS -> CResT IO a
ctssGetJoined f ctss = do
  graph <- ctssGet f ctss
  return $ mconcat $ map fst3 $ graphToNodes graph

ctssLookup :: (SSF -> Maybe (CRes a)) -> FileImport -> CTSS -> CResT IO (Maybe a)
ctssLookup f k ctss = do
  graph <- ctssGetFrom f k ctss
  return $ graphLookup k graph

ctssLookupReq :: (SSF -> Maybe (CRes a)) -> FileImport -> CTSS -> CResT IO a
ctssLookupReq f k ctss = do
  mv <- ctssLookup f k ctss
  case mv of
    Just v  -> return v
    Nothing -> fail $ printf "Could not find %s" (show $ impRaw k)

-- This does not use ctssGet because it doesn't require building, though it does require reading:
-- a page's source is what reading it produced.
getSource :: CTSS -> FileImport -> CResT IO String
getSource ctss@(CTSS ssmv) prgmName = do
  lift $ ctssLoad ctss [prgmName]
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  case graphLookup prgmName ctssData of
    Just SSF{ssfSource=Just src} -> return src
    Just SSF{ssfSource=Nothing}  -> fail $ printf "No source text available for %s" (show prgmName)
    Nothing                      -> fail $ printf "Could not find %s" (show $ impRaw prgmName)

-- | The parsed program of one page. Like 'getSource' this does not build, as the parse is what
-- reading the page already produced.
getRawPrgmOf :: CTSS -> FileImport -> CResT IO PPrgm
getRawPrgmOf ctss@(CTSS ssmv) prgmName = do
  lift $ ctssLoad ctss [prgmName]
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  case graphLookup prgmName ctssData of
    Just SSF{ssfRaw} -> asCResT $ requireComputed ssfRaw
    Nothing          -> fail $ printf "Could not find %s" (show $ impRaw prgmName)

-- | Overwrite a page with a program, written back as the syntax the page is written in.
--
-- The reversion belongs to the parser which read the page, so a page whose syntax cannot be written
-- back fails here rather than being written as something else. What it produces is the page's new
-- source, so the write itself is 'setSource' and everything that guards a write guards this one too.
setRawPrgm :: CTSS -> FileImport -> PPrgm -> CResT IO ()
setRawPrgm ctss prgmName prgm = do
  src <- asCResT $ unparseImport prgmName prgm
  setSource ctss prgmName src

-- | Overwrite the source file backing a page, then re-read so the change is picked up.
-- Only pages already in the service graph with source text can be written, which confines
-- writes to the files served by the doc roots.
setSource :: CTSS -> FileImport -> String -> CResT IO ()
setSource ctss@(CTSS ssmv) prgmName src = do
  -- Read first, since a page yet to be read has no source either, and "no source text available"
  -- means a page which is not one file to be written back rather than one not yet looked at.
  lift $ ctssLoad ctss [prgmName]
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  case graphLookup prgmName ctssData of
    Nothing -> fail $ printf "Could not find %s" (show $ impRaw prgmName)
    Just SSF{ssfSource=Nothing} -> fail $ printf "No source text available for %s" (show prgmName)
    Just SSF{ssfSource=Just _} -> case impDisp prgmName of
      Nothing -> fail $ printf "No file path available for %s" (show prgmName)
      Just filePath -> lift $ do
        writeFile filePath src
        -- The page is read again because it was written, not because the file looks written. A
        -- filesystem which keeps modification times to the second has nothing to show for a page
        -- written and read within one, and the read would give back the page it already holds.
        --
        -- Read in full for that same reason, rather than left to the read to decide. Whatever a
        -- syntax recorded of this page is answered for by the same look at the same modification
        -- time, so a read which decided for itself would answer with what the page said before it
        -- was written -- which is the very thing dropping the read times is here to prevent.
        ctssStale ctss prgmName
        void $ ctssReadOf ctss [prgmName]

-- This does not use ctssGet because it doesn't require building
getRawPrgm :: CTSS -> CResT IO PPrgmGraphData
getRawPrgm ctss@(CTSS ssmv) = do
  lift $ ctssLoadOf ctss (map snd3 . graphToNodes)
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  asCResT $ mapMGraph (requireComputed . ssfRaw) ctssData

-- | Like getRawPrgm but only returns the subgraph reachable from a given file.
getRawPrgmFrom :: FileImport -> CTSS -> CResT IO PPrgmGraphData
getRawPrgmFrom prgmName ctss@(CTSS ssmv) = do
  lift $ ctssLoadOf ctss (map snd3 . graphToNodes . graphFilterReaches prgmName)
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  asCResT $ mapMGraph (requireComputed . ssfRaw) (graphFilterReaches prgmName ctssData)

getEvaluated :: CTSS -> FileImport -> String -> CResT IO Integer
getEvaluated ctss prgmName fun = do
  base <- ctssGetFrom ssfTPrgm prgmName ctss
  fst <$> evalRun fun prgmName base

getEvaluatedMock :: CTSS -> MockIO -> FileImport -> String -> CResT IO Integer
getEvaluatedMock ctss mockIO prgmName fun = do
  base <- ctssGetFrom ssfTPrgm prgmName ctss
  fst <$> evalRunMock mockIO fun prgmName base

getEvalBuild :: CTSS -> FileImport -> String -> CResT IO Val
getEvalBuild ctss prgmName fun = do
  base <- ctssGetFrom ssfTPrgm prgmName ctss
  fst <$> evalBuild fun prgmName base

getTestResults :: CTSS -> CResT IO [TestDesc]
getTestResults ctss = do
  base <- ctssGet ssfTPrgm ctss
  asCResT $ evalTest base

-- | The tests a page wrote, run and kept.
--
-- Which tests exist at all is decided over the whole PROGRAM the page belongs to, NOT over the page
-- and what it imports. A class law is checked against the instances of that class, and those live in
-- the files that define them — which are downstream of the file stating the law. @/Data/Algebra@
-- states that @++@ is associative and imports nothing that is a @Semigroup@; deriving its tests from
-- what it can reach would leave the law quantifying over nothing and passing vacuously. Likewise an
-- agreement test pairs two definitions of one function, which two unrelated files can each hold half
-- of. See 'ctssTestPages' for what "the program the page belongs to" means.
--
-- Each derived test is then attributed to the page that wrote the definition it came from, and only
-- that page's are run here. So the answer is the same set of tests @catln test@ runs on that program,
-- sliced by who wrote them, and asking about one page costs that page's tests rather than every
-- page's.
ctssTestsFrom :: CTSS -> FileImport -> CResT IO [TestResult]
ctssTestsFrom ctss prgmName = do
  cached <- lift $ ctssCached ssfTests prgmName ctss
  case cached of
    Just tests -> asCResT tests
    Nothing -> do
      tprgms <- ctssTestPages ctss prgmName
      descs <- asCResT $ evalTest tprgms
      let owners = pageOwners tprgms
      -- Every test comes from a definition and every definition was written in a page, so a test
      -- that cannot be placed is reported rather than dropped — dropped, it would be a test that
      -- belongs to no page and so is run by nobody.
      case [tdName d | d <- descs, not (H.member (tdOwnerId d) owners)] of
        []      -> return ()
        orphans -> fail $ printf "Could not find the page that wrote %s" (intercalate ", " orphans)
      results <- lift $ mapM runTestDesc [d | d <- descs, H.lookup (tdOwnerId d) owners == Just prgmName]
      lift $ ctssStore prgmName (\ssf -> ssf{ssfTests = Just $ pure results}) ctss
      return results

-- | The pages a page's tests are decided over: every page reached by a file the service was asked to
-- serve that reaches this page.
--
-- Not every loaded page. A service can be asked to serve several files that are separate programs —
-- the integration tests load thirty of them — and a page belongs to the ones that reach it, not to
-- the ones beside it. Joining programs that were never one is also what the compiler cannot yet do:
-- @buildSCCPipeline@ offers every loaded page as an external dependency of every other, so an
-- unrelated file's @abs@ collides with a test file's own.
--
-- Not just what the page imports either, which is the point of 'ctssTestsFrom' above. Asked to serve
-- the standard library, the file it is asked for reaches every page of it, so a law stated in one
-- file is checked against the instances defined in another.
ctssTestPages :: CTSS -> FileImport -> CResT IO (GraphData TPrgm FileImport)
ctssTestPages ctss@(CTSS ssmv) prgmName = do
  CTSSDat{ctssConfig, ctssBaseFileNames, ctssData} <- lift $ readMVar ssmv
  roots <- lift $ mapM (mkDesCanonicalImportStr ctssConfig) ctssBaseFileNames
  let reaching = [root | root <- roots, isJust $ graphLookup prgmName $ graphFilterReaches root ctssData]
  when (null reaching) $
    fail $ printf "No file this service was asked to serve reaches %s, so it is in no program to take the tests of" (show $ impRaw prgmName)
  mapM_ (lift . ctssBuildFrom ctss) reaching
  CTSSDat{ctssData=built} <- lift $ readMVar ssmv
  let scope = S.fromList $ concat [map snd3 $ graphToNodes $ graphFilterReaches root built | root <- reaching]
  asCResT $ mapMGraph (requireComputed . ssfTPrgm) $ graphFromEdges
    [node | node@(_, name, _) <- graphToNodes built, name `S.member` scope]

-- | Which page each definition was written in, keyed the way a test names the definition it was
-- derived from. A page's own program holds what that page wrote; the joined program the tests are
-- derived over has lost which file anything came from.
pageOwners :: GraphData TPrgm FileImport -> H.HashMap UUID FileImport
pageOwners tprgms = H.fromList
  [ (i, name)
  | (Prgm objMap _ globalAnnots, name, _) <- graphToNodes tprgms
  , i <- map oaTestId (flatObjectMap objMap) ++ map (getMetaID . getExprMeta) globalAnnots
  ]

-- | The freshly-evaluated golden values for every string-serializable @#print@ reachable from the
-- loaded program, each keyed by the source range of its @p@ argument. Used by @catln test --update@.
getPrintGoldens :: CTSS -> CResT IO [(CodeRangeDat, String)]
getPrintGoldens ctss = do
  base <- ctssGet ssfTPrgm ctss
  asCResT $ evalPrints $ mconcat $ map fst3 $ graphToNodes base

-- | The annotations a page writes, evaluated to the values they display as.
--
-- They are evaluated when something asks to see them rather than when the page is built, so a build
-- that displays nothing — a run, a compile, an editor session — does not pay for them. The result is
-- kept on the page, so the second reader of a page gets the first reader's answer.
getEvalAnnots :: CTSS -> FileImport -> CResT IO [(Expr EvalMetaDat, Val)]
getEvalAnnots ctss prgmName = do
  cached <- lift $ ctssCached ssfAnnots prgmName ctss
  case cached of
    Just annots -> asCResT annots
    Nothing -> do
      tprgms <- ctssGetFrom ssfTPrgm prgmName ctss
      stages <- lift $ ctssStagePrgmsFrom ctss prgmName
      -- A @#test@ displays the run of the test it marks, which is the one thing an evaluation
      -- cannot work out for itself. The page's tests are run only when the page writes one, since
      -- running them is work rather than a lookup and no other annotation reads them.
      stages' <- if pageWritesATest prgmName tprgms
        then (\tests -> stages{spTests = pure tests}) <$> ctssTestsFrom ctss prgmName
        else return stages
      let annots = evalAnnots stages' prgmName tprgms
      lift $ ctssStore prgmName (\ssf -> ssf{ssfAnnots = Just annots}) ctss
      asCResT annots

-- | Call a named function of the program with arguments supplied as values, for the value it
-- displays as.
--
-- This is what a reader's typing reaches: the webdocs name a function the program holds and say what
-- to compute over, rather than sending an expression for the compiler to read
-- ('WebDocs.annotResp'). Nothing is parsed or typechecked per call, so what a call costs is building
-- and running it against a program which is already compiled and kept.
--
-- Scoped the same way an annotation is: naming a page calls the function as though it were written
-- there, and naming none calls it against every loaded file. Which is about which program the
-- function is looked up and run in — a primitive reporting on the pages there are ('spAllPages')
-- answers about all of them either way, so a caller scopes to keep from compiling every page rather
-- than to narrow what it is told.
ctssCallDShow :: CTSS -> Maybe FileImport -> String -> [(String, Val)] -> CResT IO Val
ctssCallDShow ctss scope fun args = do
  stages <- lift $ maybe (ctssStagePrgms ctss) (ctssStagePrgmsFrom ctss) scope
  -- The pages of the scope which can be evaluated, with the rest reported rather than fatal, for the
  -- same reason 'WebDocs.annotResp' takes them that way: a page which did not typecheck cannot be
  -- built over at all, and a search box is not something to lose because some other page is broken.
  tprgms <- ctssEvaluableFrom ssfTPrgm scope ctss
  prgm <- asCResT $ maybe (return $ mconcat $ map fst3 $ graphToNodes tprgms) (`prgmFromGraphData` tprgms) scope
  asCResT $ evalDShowCall (evalBaseEnvStages stages prgm) fun args

-- | Whether a page marks any of its definitions as a test. A page that does not has no annotation
-- that reads a test run, so it is not made to wait for one.
pageWritesATest :: FileImport -> GraphData TPrgm FileImport -> Bool
-- A page that is not in its own program graph has no annotations either; 'evalAnnots' is what
-- reports that, rather than it being decided here.
pageWritesATest prgmName tprgms = maybe False prgmWritesATest (graphLookup prgmName tprgms)

-- | Whether a program marks any of its definitions as a test. What a page's own program says about
-- itself, which is also what decides whether a change to anything at all can alter the page's
-- annotations ('ctssClearInvalidations').
prgmWritesATest :: TPrgm -> Bool
prgmWritesATest (Prgm objMap _ _) = any (hasAnnot testAnnot) (flatObjectMap objMap)

-- | Read a page's computed item, without building the page or computing anything.
ctssCached :: (SSF -> Maybe (CRes a)) -> FileImport -> CTSS -> IO (Maybe (CRes a))
ctssCached f prgmName (CTSS ssmv) = do
  CTSSDat{ctssData} <- readMVar ssmv
  return $ f =<< graphLookup prgmName ctssData

-- | Keep a page's computed item on the page, for whoever asks for it next.
--
-- The page is read again under the lock rather than being written back from a copy captured before
-- the computation: a page that was invalidated while its item was being computed is unbuilt again,
-- and restoring it from a stale copy of itself would undo the invalidation. Two threads racing to
-- compute the same item both store it, which is harmless — they computed it from the same built
-- page, so they computed the same thing.
--
-- Whatever computes the item must do so OUTSIDE this lock. It is the same lock a build takes, and
-- computing an item needs the page built.
ctssStore :: FileImport -> (SSF -> SSF) -> CTSS -> IO ()
ctssStore prgmName store (CTSS ssmv) = modifyMVar_ ssmv $ \ctssdat ->
  return ctssdat{ctssData = fmapGraphWithKey keep (ctssData ctssdat)}
  where
    keep k ssf | k == prgmName && ssfBuilt ssf = store ssf
               | otherwise = ssf

-- | Forget when a page's files were last written, so the next read reads it again.
--
-- What a page was read from is kept, so the read still knows which files to look at; only the claim
-- that they are still as they were is dropped.
ctssStale :: CTSS -> FileImport -> IO ()
ctssStale (CTSS ssmv) prgmName = modifyMVar_ ssmv $ \ctssdat ->
  return ctssdat{ctssData = fmapGraphWithKey forget (ctssData ctssdat)}
  where
    forget k ssf | k == prgmName = ssf{ssfRead = (ssfRead ssf){rpReads = Nothing}}
                 | otherwise = ssf

-- Helpers

getPrgm :: CTSS -> CResT IO (GraphData DesPrgm FileImport)
getPrgm = ctssGet ssfDes

getTPrgmWithTrace :: CTSS -> CResT IO (GraphData (TPrgm, VPrgm, TraceConstrain) FileImport)
getTPrgmWithTrace = ctssGet ssfTPrgmWithTrace

getTPrgm :: CTSS -> CResT IO (GraphData TPrgm FileImport)
getTPrgm = ctssGet ssfTPrgm

getTPrgmJoined :: CTSS -> CResT IO TPrgm
getTPrgmJoined = ctssGetJoined ssfTPrgm

getTBPrgm :: CTSS -> CResT IO (GraphData TBPrgm FileImport)
getTBPrgm = ctssGet ssfTBPrgm

-- | The desugar and tree build programs of every loaded page, joined and left unforced. This is the
-- 'StagePrgms' for an evaluation that is not scoped to one page, and so reaches all of them.
--
-- It cannot go through 'ctssGetJoined': that runs in @CResT IO@ and so forces its result, and
-- merely inspecting 'ssfTBPrgm' forces 'evalBuildAll' over every page. Keeping the join a thunk is
-- what lets a caller that never reads a stage avoid building it.
--
-- Neither the type inference nor a debug run is available here. Points are numbered per typecheck
-- run, so the traces of every page joined together would be a set of colliding numbers rather than an
-- answer, and a run needs one program with one entry point where the join has as many as it has
-- pages. Compiling an expression is out for the same reason a run is: an expression means what the
-- page it is written in makes it mean, and the join of every page is not a page anything was written
-- in. An evaluation that wants any of them has to name the page it is asking about
-- ('ctssStagePrgmsFrom').
ctssStagePrgms :: CTSS -> IO StagePrgms
ctssStagePrgms ctss@(CTSS ssmv) = do
  ctssBuildAll ctss
  CTSSDat{ctssData} <- readMVar ssmv
  let ssfs = map fst3 $ graphToNodes ctssData
  let pages = return $ map snd3 $ graphToNodes ctssData
  return $ StagePrgms {
      spDesPrgm = joinedStage ssfDes ssfs
    , spTBPrgm = joinedStage ssfTBPrgm ssfs
    , spTypeInfer = notScoped "type inference to report on"
    , spDebugRun = const $ notScoped "program to run"
    , spDebugExpr = const $ notScoped "expression to run"
    , spPages = pages
    , spAllPages = pages
    , spTests = notScoped "tests to report on"
    , spPageValues = pageValues ctssData
    }
  where
    notScoped :: String -> CRes a
    notScoped what = CErr [MkCNote $ GenCErr Nothing $ "This evaluation is not scoped to a single page, so it has no " ++ what]

-- | Like 'ctssStagePrgms' but for an evaluation scoped to one page: every stage covers that page and
-- what it imports, the type inference is that page's own, which is the only scope its points are
-- numbered in, and a debug run runs that page.
ctssStagePrgmsFrom :: CTSS -> FileImport -> IO StagePrgms
ctssStagePrgmsFrom ctss@(CTSS ssmv) prgmName = do
  ctssBuildFrom ctss prgmName
  CTSSDat{ctssData} <- readMVar ssmv
  let reaches = graphFilterReaches prgmName ctssData
  let ssfs = map fst3 $ graphToNodes reaches
  let typeInferred = do
        (_, vprgm, trace) <- requireComputed $ ssfTPrgmWithTrace =<< graphLookup prgmName ctssData
        return (vprgm, trace)
  -- Runs against the page's own typecheck program rather than the evaluation's, which for an
  -- annotation is that program joined with the synthetic one holding the annotation itself.
  let debugRun fun = evalDebugPrgm fun =<< joinedStage ssfTPrgm ssfs
  -- Against the pages of the scope rather than their joined program, since compiling the expression
  -- typechecks it against them one by one, the way a page of the scope was itself typechecked.
  let debugExpr = debugExprRun prgmName (graphToNodes reaches)
  -- No test run: unlike every stage here, one cannot be looked up — it has to be produced by
  -- running the page's tests, which is the work an evaluation with nothing to report on them must
  -- not be made to do. An evaluation that does have a test to report on is given one by whoever
  -- asked for it (see 'getEvalAnnots').
  -- Every page the service holds rather than the ones this scope reaches, which is what makes an
  -- annotation reporting on the pages there are answer the same wherever it is written. It is the
  -- graph as it is, so nothing is built to produce it.
  let allPages = return $ map snd3 $ graphToNodes ctssData
  return $ StagePrgms {
      spDesPrgm = joinedStage ssfDes ssfs
    , spTBPrgm = joinedStage ssfTBPrgm ssfs
    , spTypeInfer = typeInferred
    , spDebugRun = debugRun
    , spDebugExpr = debugExpr
    , spPages = return $ map snd3 $ graphToNodes reaches
    , spAllPages = allPages
    , spTests = notRun
    , spPageValues = pageValues reaches
    }
  where
    notRun = CErr [MkCNote $ GenCErr Nothing "This evaluation was not given a run of the page's tests"]

-- | Compiles one expression, written as Catln source, as though it were a statement of a page, and
-- runs it. This is 'StagePrgms.spDebugExpr' of a scope: the same compile an annotation asked for over
-- @\/api\/annot@ gets ('WebDocs.annotResp'), with the expression run for its evaluation tree instead
-- of displayed.
--
-- Compiled rather than looked up because the expressions worth debugging are largely ones the program
-- does not contain: a test case's input was generated, and never existed as anything but the text of
-- the call that ran. So the expression is compiled against the page the same way the page's own
-- statements were, which is also what makes the names in it mean what they mean on that page.
--
-- Pure, so that it can be handed to an evaluation, which has no 'IO' to run: every step of the
-- compile is a rewrite of a program that is already read and already built.
debugExprRun :: FileImport -> [(SSF, FileImport, [FileImport])] -> String -> CRes DebugRun
debugExprRun prgmName scopeNodes src = do
  parsed <- case runParser pExpr "<debug>" src of
    Left err     -> fail $ errorBundlePretty err
    Right parsed -> return parsed
  let rawPrgm = RawPrgm [] [RawStatementTree (RawAnnot parsed) []]
  let exprName = desFileImport $ mkRawFileImport $ RawValue emptyMetaN "<debug>"
  let rawPrgm' = addRawPrgmMetaIDs (parseMetaIDScope exprName) rawPrgm
  (desP, desName, _) <- desPrgm (rawPrgm', exprName, [])
  let desP' = desFinalPass (desP, desName, [], [])
  tprgmsTrace <- evaluablePages ssfTPrgmWithTrace True scopeNodes
  let deps = graphToNodes $ fmapGraph fst3 tprgmsTrace
  (exprTprgm, _, _) <- case typeCheckToRes $ typecheckPrgms [(desP', [], map fst3 $ graphToNodes tprgmsTrace)] of
    CErr notes   -> CErr notes
    CRes _ []    -> fail $ printf "The debugger could not typecheck %s" src
    CRes _ (t:_) -> return t
  prgm <- prgmFromGraphData desName $ graphFromEdges ((exprTprgm, desName, map snd3 deps) : deps)
  -- The one statement parsed above, now compiled. Read back off the program rather than kept from
  -- before it, so what is run is what the whole pipeline produced.
  expr <- case prgmAnnots exprTprgm of
    [expr] -> return expr
    exprs  -> fail $ printf "The debugger was given %d expressions to run in %s rather than one" (length exprs) (show prgmName)
  evalDebugExprPrgm expr prgm

-- | The values one page declares of one type, evaluated to the values they display as. This is the
-- 'StagePrgms.spPageValues' of a set of pages: what a value gathered across pages means, without any
-- notion of which type is worth gathering, which belongs to whatever is doing the gathering.
--
-- Ordered, which a program's declarations are not: they are held in a hash map, so one page's arrive
-- in no order at all, and a declaration a syntax builds rather than parses carries no source
-- position to be sorted by either (@Syntax.Ct.Builder@ builds it with an empty meta). They are put
-- back into the order the page writes them, so what gathers them reads as the pages do rather than
-- reshuffling itself between readings.
--
-- A page that was never built has nothing to report and is answered with nothing, as 'joinedStage'
-- passes over such a page in the stages beside this one; a page that failed to build fails what
-- asked. The graph is read once for whichever pages are asked about, since the answer is a field of
-- one 'StagePrgms' rather than a call made afresh each time.
--
-- Every type asked about is gathered in the one pass, and the order below is what makes that matter:
-- asking twice and joining the answers would be two runs of the page's order rather than one.
pageValues :: GraphData SSF FileImport -> [Name] -> FileImport -> CRes [(Expr EvalMetaDat, Val)]
pageValues graph = valuesOf
  where
    computed = graphFromEdges $ filter (isJust . ssfTPrgm . fst3) $ graphToNodes graph
    tprgms = mapMGraph (requireComputed . ssfTPrgm) computed

    valuesOf names prgmName = case (graphLookup prgmName graph, graphLookup prgmName computed) of
      -- A page which was built and has no typechecked program declares nothing to gather: a @ctx@
      -- page is marked built while deliberately skipping the pipeline ('buildSCCPipeline').
      (Just _, Nothing) -> return []
      -- A page whose program was never read is not a page with nothing in it. Reported, so that a
      -- gathering across pages is never quietly short of the ones nobody has looked at yet.
      (Nothing, _) -> fail $ printf "The page %s has not been read, so what it declares is not known" (show $ impRaw prgmName)
      (_, Just ssf) -> do
        values <- evalPageValues names prgmName =<< tprgms
        written <- rawStatementExprIds <$> requireComputed (ssfRaw ssf)
        return $ sortOn (writtenAt written . fst) values

    -- A value the page does not write is one the page was rebuilt without, so it goes after the ones
    -- it does rather than before them.
    writtenAt written e = fromMaybe (length written) $ elemIndex (exprId e) written

    exprId = getMetaID . getExprMeta

-- | Joins one stage over a set of pages, left unforced. See 'ctssStagePrgms' for why it must stay a
-- thunk.
joinedStage :: (Monoid a) => (SSF -> Maybe (CRes a)) -> [SSF] -> CRes a
joinedStage f ssfs = fmap mconcat $ sequence $ mapMaybe f ssfs
