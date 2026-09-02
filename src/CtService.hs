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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module CtService where
import           Control.Concurrent      (MVar, forkIO, modifyMVar, modifyMVar_,
                                          newMVar, readMVar, withMVar)
import           Control.Concurrent.STM  (STM, TVar, atomically, modifyTVar',
                                          newTVarIO, readTVar, readTVarIO,
                                          retry, writeTVar)
import           Control.Exception       (bracket, bracket_, evaluate)
import           Control.Monad           (forM, forM_, forever, unless, void,
                                          when)
import           Control.Monad.Trans     (lift)
import           CRes
import           Data.Aeson              (ToJSON)
import           Data.Graph
import           Data.Hashable           (hash)
import qualified Data.HashMap.Strict     as H
import qualified Data.HashSet            as S
import           Data.IORef              (modifyIORef', newIORef, readIORef)
import           Data.List               (elemIndex, intercalate, partition,
                                          sortOn)
import           Data.Maybe
import           Data.Time.Calendar      (Day)
import           Data.Time.Clock         (getCurrentTime, utctDay)
import           Data.UUID               (UUID)
import           Eval                    (evalAnnots, evalBaseEnv,
                                          evalBaseEnvService, evalBuild,
                                          evalBuildAllOf, evalCallTo,
                                          evalDShowCall, evalDebugExprPrgm,
                                          evalDebugPrgm, evalPageAsk,
                                          evalPageValues, evalRun, evalRunMock,
                                          prgmFromGraphData)
import           Eval.Common             (AnnotAsks (..), CtServiceApi (..),
                                          DebugRun, EvalMetaDat, MockIO,
                                          SSF (..), TExpr, TestResult, Val,
                                          esAsks, joinedStage, newEvalService,
                                          oaTestId)
import           Eval.Testing            (TestDesc (..), evalPrints, evalTest,
                                          runTestDesc)
import           FileWatch               (Watch, Watcher, allCovered,
                                          awaitSettled, covered, startWatcher,
                                          watchReads, withPending)
import           GHC.Generics            (Generic)
import           MapMeta                 (parseMetaIDScope)
import           Semantics               (CTSSConfig, ctssCachePath, ctssToday)
import           Semantics.CtType        (fromCt)
import           Semantics.Prgm
import           Semantics.Types         (Name)
import           Semantics.Val           hiding (Val)
import           Syntax.Ct.Desugarf      (desFinalPass, desFinalPasses, desPrgm,
                                          validPrgm)
import           Syntax.Ct.Desugarf.Expr (desFileImport)
import           Syntax.Ct.MapRawMeta    (addRawPrgmMetaIDs,
                                          rawStatementExprIds)
import           Syntax.Ct.Parser.Expr   (pExpr)
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Syntax.InferImport      (findImportPath)
import           Syntax.Native.Parser    (isNativeImport)
import           Syntax.Parsers
import           System.FilePath         (takeDirectory)
import           Text.Megaparsec         (errorBundlePretty, runParser)
import           Text.Printf
import           TypeCheck               (typecheckPrgms, typecheckPrgmsWith)
import           TypeCheck.Common        (AnyInstrument, InstrumentResults,
                                          TPrgm, VPrgm, lookupEpochCountResult,
                                          lookupFragmentCountResult,
                                          lookupTraceResult, typeCheckToRes)
import           TypeCheck.Instruments   (defaultInstruments,
                                          typeInfoInstruments)
import           Utils

type TBPrgm = Prgm TExpr EvalMetaDat

-- | The compilation service, and what it is working on.
--
-- Two things rather than one because they are asked about at different moments. The service itself is
-- behind an MVar which a build holds for a whole SCC ('ctssBuildSCC'), so anything reached THROUGH
-- that MVar is reached only once the build is over — and what a reader waiting on that build wants to
-- be told is how far through it is. So what the service is working on sits beside the lock rather
-- than under it, and is read without taking it.
data CTSS = CTSS (MVar CTSSDat) (TVar Working)

-- | What the service is working on, for whoever is waiting at something it has not answered yet.
--
-- Said as what the work IS rather than as one number for how much of it is left. What a page costs is
-- the reading of the files it is compiled from, the compiling of them, and the evaluating of what it
-- displays; the first two are counted in pages and the last is a program the reader wrote, which has
-- no count at all. Fusing them into a percentage would need a weight for each, and there is nothing
-- here that knows one — so each step says what it can say, and a step with nothing to count says so
-- rather than inventing a fraction.
data Progress = Progress {
  -- | Whether anything is being computed for a reader at all.
  --
  -- Kept apart from the step below because the step is the LAST thing said rather than a thing being
  -- done: a build that ended left "compiled 57 of 57 pages" behind it, and a reader shown that while
  -- the service sat idle would be told something untrue about the present. Maintained by 'ctssWorking'
  -- and by nothing else.
  prWorking :: Bool,
  prStep    :: ProgressStep,
  -- | The pages this step has finished, and the pages it knows of.
  --
  -- The total grows as the step goes: a read finds out what it has to read by reading it, so what is
  -- known of the work is what has been reached so far. Both zero for a step with no count of its own.
  prDone    :: Int,
  prTotal   :: Int,
  -- | What is being worked on, where there is one thing to name it by.
  prAt      :: Maybe String
                         }
  deriving (Eq, Generic, ToJSON)

-- | Which kind of work the service is doing.
data ProgressStep
  = StepIdle
  | StepReading
  | StepBuilding
  | StepTypechecking
  | StepEvaluating
  deriving (Eq, Show, Generic, ToJSON)

-- | The progress, and how many readers are being computed for.
--
-- The count does not leave the service: what a reader is told is whether anything is happening, not
-- how many other readers there are. It is counted rather than flagged because two readers can be
-- waiting at once, and the first of them being answered is not the service falling idle.
data Working = Working {
  wkBusy :: Int,
  wkNow  :: Progress
                       }
  deriving (Eq)

-- | Nothing is being computed, and so there is nothing to say about what.
idleProgress :: Progress
idleProgress = Progress False StepIdle 0 0 Nothing

newWorking :: IO (TVar Working)
newWorking = newTVarIO $ Working 0 idleProgress

-- Main data type for CtService Status
data CTSSDat = CTSSDat {
  ctssConfig        :: CTSSConfig,
  ctssBaseFileNames :: [String],
  -- | The pages the service holds, which is what it is.
  --
  -- Kept as the pages themselves, with the import graph below derived from them, because a read
  -- takes the pages up one at a time ('ctssAddPage'). A page added to the graph itself costs a
  -- 'graphFromEdges', which sorts by 'FileImport' and so compares the import expressions of every
  -- page against each other — a read of a vault of a few thousand notes would spend the read
  -- rebuilding the graph rather than reading. A page added here costs one insert.
  ctssPages         :: H.HashMap FileImport SSF,
  -- | The pages as the graph their imports make, which is how everything but a read reads them.
  --
  -- Written only alongside 'ctssPages', by 'ctssWithPages' and 'ctssMapPages', so that the two
  -- cannot come apart. Lazily, so that adding a page does not build it: a read adds every page it
  -- reads and forces nothing, and the graph is built once when something first walks it.
  --
  -- A page's edges are exactly the imports the read left it with ('Syntax.Ct.Prgm.rpImports', which
  -- is what that field is). Nothing which writes a page changes them: a build writes what it built
  -- and an invalidation drops it, and both keep the page's read.
  ctssData          :: GraphData SSF FileImport,
  -- | What a read of the service has left to read.
  --
  -- Held here rather than made inside the walk so that the reading is the service's own: a page is
  -- taken up as it is read, and what is left to read is something to be looked at while it happens.
  ctssReadQueue     :: ReadQueue,
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
  ctssShown         :: TVar Displays,
  -- | Which instruments each component's typecheck is watched by, given the pages of that
  -- component ('TypeCheck.Common.Instrument').
  --
  -- 'TypeCheck.Instruments.defaultInstruments' for every page by default, which keeps nothing: an
  -- inference is read by asking for it ('ctssTypeInfo'), and a build that kept one per page would
  -- hold a multiple of the whole program for the sake of the pages nobody looks at.
  --
  -- It is a function of the pages rather than one list, so that something which knows in advance
  -- which page it is going to read — @catln typetrace@, which is named a page on the command line —
  -- can have that page's component watched as it is built and every other component left alone.
  -- That page is then compiled once rather than compiled and compiled again.
  ctssInstrument    :: [FileImport] -> [AnyInstrument],
  -- | What each component's typecheck cost, for a service being measured rather than read.
  --
  -- One entry per COMPONENT, named by the first of its pages, because the epoch count is the count
  -- of the solve and an SCC's pages share one solve — a per-page tally of the same solve would
  -- count it once per page. Empty unless the instruments a build was set to run with include
  -- 'TypeCheck.Instruments.epochCountInstrument', so an ordinary build fills nothing.
  ctssEpochs        :: TVar [(FileImport, (Integer, Integer))]
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
  CTSS v <$> newWorking

newCTSSDat :: CTSSConfig -> [String] -> IO CTSSDat
newCTSSDat config baseFileNames = CTSSDat config baseFileNames H.empty (graphFromEdges []) <$> newReadQueue
  <*> pure Nothing <*> newMVar () <*> newTVarIO (Displays 0 H.empty) <*> pure (const defaultInstruments)
  <*> newTVarIO []

-- | Watch the typechecks of some pages with more than the default instruments.
--
-- Set before anything is built, by something which knows what it is going to read: a build is done
-- once and kept, so a page already compiled is not compiled again to be watched — asking for its
-- inference afterwards runs its typecheck a second time instead ('ctssTypeInfo'), which is the
-- right thing for a reader who could not have said in advance and the wrong thing for one who
-- could.
ctssSetInstrument :: CTSS -> ([FileImport] -> [AnyInstrument]) -> IO ()
ctssSetInstrument (CTSS ssmv _) instrument = modifyMVar_ ssmv $ \ss -> return ss{ctssInstrument=instrument}

-- | What each component built so far cost in typecheck epochs, oldest build first.
--
-- Says nothing unless the build was set to run with an instrument that counts them
-- ('ctssSetInstrument'), which is the point: a measurement is asked for and is otherwise not paid
-- for.
ctssEpochCounts :: CTSS -> IO [(FileImport, (Integer, Integer))]
ctssEpochCounts (CTSS ssmv _) = do
  CTSSDat{ctssEpochs} <- readMVar ssmv
  reverse <$> readTVarIO ctssEpochs

-- | How many pages the service holds that did not typecheck.
--
-- Judged by 'ssfTPrgm', the stage that fails a page which did not ('buildSCCPipeline' stores it
-- through 'failOnErrorNotes'). A page never reached at all is not counted as failed: it was not
-- asked for.
ctssFailedPageCount :: CTSS -> IO Int
ctssFailedPageCount (CTSS ssmv _) = do
  CTSSDat{ctssPages} <- readMVar ssmv
  return $ length [() | ssf <- H.elems ctssPages, Just (CErr _) <- [ssfTPrgm ssf]]

-- | The service holding these pages, with the graph they make derived from them.
--
-- For a change to WHICH pages the service holds, or to what one of them imports: a read taking up a
-- page it has read, and a read dropping the pages it did not reach. A change to what a page holds is
-- 'ctssMapPages' below, which is the same thing without the sorting.
ctssWithPages :: H.HashMap FileImport SSF -> CTSSDat -> CTSSDat
ctssWithPages pages ss = ss{ctssPages=pages, ctssData=graphFromEdges (map node $ H.toList pages)}
  where node (prgmName, ssf) = (ssf, prgmName, rpImports $ ssfRead ssf)

-- | The service with each page it holds replaced by what this makes of it: a build's results, an
-- invalidation dropping them, a page's computed item kept.
--
-- The graph is rebuilt out of the one the service already has rather than out of the pages, so that
-- its keys arrive sorted and sorting them again is a merge over one run rather than a sort. That is
-- what a graph of pages costs: a key is the import expression naming a file, so two pages of one
-- directory are told apart only by the end of a long path, and a read of a few thousand notes which
-- sorted them afresh would spend longer on the graph than on the notes.
--
-- Which is why this cannot add a page, drop one, or change what one imports: it is given a change to
-- each page the service holds and keeps the shape those pages already made. A change to a page's
-- imports would leave the graph saying what the pages no longer say, and nothing which uses this
-- makes one — the imports are the page's read ('Syntax.Ct.Prgm.rpImports'), and what is written here
-- is what was built from a page, or that it was built from nothing.
ctssMapPages :: (FileImport -> SSF -> SSF) -> CTSSDat -> CTSSDat
ctssMapPages f ss = ss{ctssPages=H.fromList [(prgmName, ssf) | (ssf, prgmName, _) <- nodes], ctssData=graphFromEdges nodes}
  -- Read off the nodes rather than mapped over the pages as well, so that the change is made once
  -- for each page rather than once for the graph and again for the pages.
  where nodes = [(f prgmName ssf, prgmName, imports) | (ssf, prgmName, imports) <- graphToNodes $ ctssData ss]

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
                , ssfTPrgm = Nothing
                , ssfTcInputs = Nothing
                , ssfTypeInfo = Nothing
                , ssfTBPrgm = Nothing
                , ssfAnnots = Nothing
                , ssfAnnotAsks = mempty
                , ssfTests = Nothing
                }

ctssSetFiles :: CTSS -> [String] -> IO ()
ctssSetFiles ctss@(CTSS ssmv _) baseFileNames = do
  modifyMVar_ ssmv $ \ss -> return ss{ctssBaseFileNames=baseFileNames}
  ctssRead ctss

ctssBaseFiles :: CTSSConfig -> [String] -> IO CTSS
ctssBaseFiles config baseFileNames = do
  v <- newMVar =<< newCTSSDat config baseFileNames
  ctss <- CTSS v <$> newWorking
  ctssRead ctss
  return ctss

-- | Watch the files the service is read from, so that later reads read only what has changed.
--
-- For a service read from over and over, rather than read once and thrown away. Without it a read
-- looks at every file the service holds, which is cheap next to reading them but is still work
-- proportional to the program rather than to the change.
ctssWatch :: CTSS -> IO ()
ctssWatch ctss@(CTSS ssmv _) = do
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
ctssWatchLive ctss@(CTSS ssmv _) = do
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
ctssFollow (CTSS ssmv _) shown act = do
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
ctssFollowing (CTSS ssmv _) = do
  CTSSDat{ctssShown} <- readMVar ssmv
  H.size . dsOpen <$> readTVarIO ctssShown

-- | Wake every display a read reached.
--
-- Asked of the displays rather than of the pages: a change reaching a thousand pages nobody has open
-- wakes nothing, and the work is what is being read rather than what has changed.
ctssNotify :: CTSS -> ReadChange -> IO ()
ctssNotify (CTSS ssmv _) change = do
  CTSSDat{ctssShown} <- readMVar ssmv
  atomically $ do
    Displays{dsOpen} <- readTVar ctssShown
    forM_ (H.elems dsOpen) $ \(shown, reached) ->
      when (changeReaches change shown) $ writeTVar reached True

-- | Say what the service is now doing.
--
-- Takes nothing but what to say. This is called from inside the service's own lock —
-- 'buildSCCPipeline' runs under the 'modifyMVar_' of 'ctssBuildSCC' — so anything it read through
-- that lock would be a report that deadlocks the build it is reporting on.
--
-- Says the step whether or not anybody is waiting. Whether anybody is, is 'prWorking', which
-- 'ctssWorking' keeps: a build asked for by @catln build@ or by the LSP publishes its steps here and
-- they are read by nobody, which costs one 'writeTVar' per SCC.
ctssReport :: CTSS -> ProgressStep -> Int -> Int -> Maybe String -> IO ()
ctssReport (CTSS _ wv) step done total at = atomically $ modifyTVar' wv $ \w ->
  w{wkNow = Progress (wkBusy w > 0) step done total at}

-- | Say what the service is now doing WITHIN the work it is already counting.
--
-- For a step which is part of a counted one rather than a step of its own: a type check is what a
-- build of a component spends its time on, and the pages left to build are still the pages left to
-- build while it runs. Reporting it as a step with no count would take the count away exactly when
-- the reader has longest to wait for it — a build reports its pages once per component and then
-- spends the whole component type checking, so a type check which cleared the count would clear it
-- for all but an instant of the build.
ctssReportAt :: CTSS -> ProgressStep -> Maybe String -> IO ()
ctssReportAt (CTSS _ wv) step at = atomically $ modifyTVar' wv $ \w ->
  w{wkNow = (wkNow w){prWorking = wkBusy w > 0, prStep = step, prAt = at}}

-- | Compute something a reader is waiting for, counted as being worked on while it is computed.
--
-- Bracketed, so that a connection dropping, a compile throwing and an answer being sent all take the
-- count back down the same way. One left up is a reader told forever that something is happening.
ctssWorking :: CTSS -> IO a -> IO a
ctssWorking (CTSS _ wv) = bracket_ (count 1) (count (-1))
  where
    -- The last reader to stop waiting leaves the service idle rather than leaving the last step it
    -- published behind it. A step is what is being done, and once nothing is being done there is no
    -- step to be part way through.
    count n = atomically $ modifyTVar' wv $ \w ->
      let busy = wkBusy w + n
      in Working busy $ if busy > 0 then (wkNow w){prWorking = True} else idleProgress

-- | What the service is doing, asked without waiting for it to stop doing it.
ctssProgress :: CTSS -> IO Progress
ctssProgress (CTSS _ wv) = wkNow <$> readTVarIO wv

-- | Wait until what the service is doing is something other than this.
--
-- Handed back unrun for the same reason 'ctssFollow' hands its wait back: whoever is waiting has its
-- own reasons to stop, and waits for those at the same time.
ctssProgressChanged :: CTSS -> Progress -> STM ()
ctssProgressChanged (CTSS _ wv) p = do
  w <- readTVar wv
  when (wkNow w == p) retry

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
-- a read of it which brought only its imports would lose the program the service holds: 'mergePage'
-- below cannot match a program it holds against a read which brought none, so the page would be
-- replaced by one with no program and everything built from it thrown away.
ctssReadOf :: CTSS -> [FileImport] -> IO ReadChange
ctssReadOf ctss@(CTSS ssmv _) load = do
  CTSSDat{ctssReadLock} <- readMVar ssmv
  change <- withMVar ctssReadLock $ \() -> do
    -- Taken under the read lock rather than before it, because what the read is to make of what it
    -- finds is decided against the pages the service holds as the read begins. Reads are one at a
    -- time, so nothing can add or drop a page between here and the end of this one.
    CTSSDat{ctssConfig, ctssBaseFileNames, ctssPages=before, ctssWatcher, ctssReadQueue} <- readMVar ssmv
    withWritten ctssWatcher $ \written -> do
      unchanged <- ctssUnchanged ctssWatcher written [(prgmName, ssfRead ssf) | (prgmName, ssf) <- H.toList before]
      let held = [prgmName | (prgmName, ssf) <- H.toList before, isJust (ssfRaw ssf)]
      reading <- newReading (ctssCachePath ctssConfig) (ctssImporterEnv ctss)
      rqRestart ctssReadQueue =<< mapM (canonicalImport ctssConfig (rootImportCtx noImporters) . mkRawImportStr) ctssBaseFileNames
      -- Gathered up as the read goes and acted on once at the end. Which pages an invalidation
      -- reaches is the import graph walked the other way, and mid-read that is a graph some of whose
      -- pages this read has taken up and some of whose pages it has not — so the walk is made once,
      -- over the graph the read leaves.
      invalidations <- newIORef []
      -- Counted here rather than asked of the queue, which holds what is left to do and not what has
      -- been done. Reads are one at a time ('ctssReadLock'), so an 'IORef' is as safe as the queue is.
      readSoFar <- newIORef (0 :: Int)
      readQueued ctssConfig reading (S.fromList $ load ++ held) unchanged ctssReadQueue $ \prgmName page -> do
        invalidated <- ctssAddPage ctss prgmName page
        when invalidated $ modifyIORef' invalidations (prgmName :)
        -- What the read knows of itself at this moment. 'readQueued' takes a page off the queue,
        -- calls this, and only then queues what that page imports — so here the page is neither
        -- pending nor unqueued, and the two counts do not overlap or leave it out. The total grows
        -- as the walk reaches more, which is what a read finding its work by doing it looks like.
        modifyIORef' readSoFar (+ 1)
        done <- readIORef readSoFar
        left <- rqLeft ctssReadQueue
        ctssReport ctss StepReading done (done + left) (impDisp prgmName)
      -- Not bracketed, as 'Syntax.Parsers.readFiles' does not bracket it: a read which failed part
      -- way must not have what it half read written back.
      finishReading reading
      read' <- rqRead ctssReadQueue
      invalidated <- readIORef invalidations
      change <- modifyMVar ssmv $ \ss -> do
        -- The pages the read did not reach are the pages there no longer are. Compared against what
        -- the service held as the read began rather than against what it holds now, which by now is
        -- those pages and the read's together.
        let pagesChanged = H.keysSet before /= read'
        let kept = H.filterWithKey (\prgmName _ -> S.member prgmName read') (ctssPages ss)
        let (changed, ss') = ctssClearInvalidations pagesChanged invalidated (ctssWithPages kept ss)
        return (ss', ReadChange changed pagesChanged)
      -- The watches only need seeing to where the read read something, since they are watches on
      -- what the reads read. A read which gave every page back left every path it was watching for
      -- exactly as it was, and gathering them up again to say so would be work for every path of
      -- every page, to find nothing to do. A read with pages asked of it is not one of those: a page
      -- given back as its imports alone is read the rest of the way where it was asked for, which is
      -- a read of a page the read also gave back. Taken as having read something whenever anything
      -- was asked of it, rather than by looking for which of them were read the rest of the way,
      -- since seeing to watches that were already seen to costs a set difference and finds nothing.
      let readNothing = null load && S.size read' == H.size unchanged && all (`H.member` unchanged) read'
      unless readNothing $ forM_ ctssWatcher $ ctssRewatch ctss
      return change
  -- Outside the read, so that a reader woken by it can read the service without waiting on the read
  -- that woke them, and so that a display failing to be woken is not a change put back as unread.
  ctssNotify ctss change
  return change

-- | Take up one page a read has read, saying whether that dropped what was built from it.
--
-- What the service does with a read, one page at a time: a page is held by the service as soon as it
-- has been read, rather than when the read it belongs to is over. Which pages the read did not reach
-- and what an invalidation carries with it are the read's own to settle at the end ('ctssReadOf'),
-- since neither is a question about one page.
--
-- The page is compared against the page the service holds as of this call rather than as of the
-- start of the read. A read queues each page once ('Syntax.Parsers.rqSeen'), so no page is ever
-- compared against a version of itself this same read wrote.
ctssAddPage :: CTSS -> FileImport -> ReadPage -> IO Bool
ctssAddPage (CTSS ssmv _) prgmName page = modifyMVar ssmv $ \ss -> do
  let (ssf, invalidated) = mergePage (H.lookup prgmName $ ctssPages ss) page
  return (ctssWithPages (H.insert prgmName ssf $ ctssPages ss) ss, invalidated)

-- | What a page read becomes, given the page the service holds under that name, and whether taking
-- it up drops what was built from it.
mergePage :: Maybe SSF -> ReadPage -> (SSF, Bool)
mergePage held page = case held of
  -- A page given back unread is the page already held, so there is nothing to compare it
  -- against and nothing to invalidate. The comparison below walks the whole syntax tree, so a
  -- read which read nothing would otherwise compare every page against itself.
  --
  -- Read times identify the read rather than the files: a page read again is timed afresh
  -- before it is parsed, so times equal to the ones the page already holds are the very times
  -- it was handed back with.
  Just old | isJust (rpReads page), rpReads (ssfRead old) == rpReads page -> (old, False)
  -- A page which read back the same program keeps everything built from it, but takes the new
  -- source text: a change the parse discards (trailing whitespace) leaves the program equal
  -- while the file on disk is no longer the one the page last served. It takes the new read
  -- too, or a page written to without changing would be read again on every read forever.
  Just old@SSF{ssfRaw=Just (CRes _ oldRaw)} | Just oldRaw == rpPrgm page -> (old{ssfSource=rpSource page, ssfRead=page}, False)
  -- A page still held as its imports alone, read again as its imports alone. Nothing was built
  -- from it to invalidate and nothing about it has changed, so this is not a change — without
  -- this arm every read of a page nothing has asked for yet would invalidate every page which
  -- imports it.
  -- What it is waiting on is compared as well as what it imports. A page held unread because its
  -- importer was not built, read again now that the importer IS built, is a page which has changed
  -- even though its edges have not — and taking it for unchanged is what would keep it unread
  -- forever.
  Just old@SSF{ssfRaw=Nothing} | isNothing (rpPrgm page), rpImports (ssfRead old) == rpImports page, rpBlocked (ssfRead old) == rpBlocked page -> (old{ssfRead=page}, False)
  Just{} -> (newSSF page, True)
  Nothing -> (newSSF page, False)

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
ctssRewatch ctss@(CTSS ssmv _) watcher = do
  CTSSDat{ctssData} <- readMVar ssmv
  before <- covered watcher
  watchReads watcher $ concatMap readPathWatches $ concatMap (readPaths . ssfRead . fst3) $ graphToNodes ctssData
  after <- covered watcher
  let fresh = S.difference after before
  unless (S.null fresh) $ do
    let freshly = [(prgmName, ssfRead ssf) | (ssf, prgmName, _) <- graphToNodes ctssData
                                           , any (any (`S.member` fresh) . readPathWatches) $ readPaths $ ssfRead ssf]
    current <- unchangedPages freshly
    forM_ freshly $ \(prgmName, _) -> unless (H.member prgmName current) $ ctssStale ctss prgmName

-- | Drop what was built from the pages a change reached, and say which pages those were.
--
-- Which pages a change reached is the graph walked the other way: edges run from a page to its
-- imports, so the pages which import a changed one are what 'transposeG' finds. Answered as well as
-- acted on, because it is also what decides whether the change reaches anything a reader has open
-- ('changeReaches'), and working it out a second time would be walking the graph a second time.
ctssClearInvalidations :: Bool -> [FileImport] -> CTSSDat -> (S.HashSet FileImport, CTSSDat)
ctssClearInvalidations pagesChanged invalidations ss@CTSSDat{ctssData=(g, nodeFromVertex, vertexFromKey)} = (propagated, ctssMapPages maybeClear ss)
  where
    propagated = S.fromList $ map (snd3 . nodeFromVertex) $ foldMap (foldMap (:[])) $ dfs (transposeG g) (mapMaybe vertexFromKey invalidations)
    maybeClear prgmName ssf
      -- Rebuilt from the read the page already has: an invalidation drops what was built from a
      -- page, not what the page was read from, so the next read still gives it back unread.
      --
      -- Except for a page read by an importer written in Catln, whose read IS a build: its program
      -- is whatever running that importer produced. Keeping it would leave the page holding what the
      -- OLD importer said, and since the page's own files have not changed, nothing would ever read
      -- it again — editing an importer would drop what was built from every page it reads and leave
      -- every one of those pages saying what it said before. So such a page is marked unread as well
      -- as unbuilt, and the next read runs the importer again.
      | prgmName `S.member` propagated = newSSF (reReadNative prgmName $ ssfRead ssf)
      | null invalidations && not pagesChanged = ssf
      -- Which tests a page has, and what they find, is decided over the whole program the page
      -- belongs to rather than over the page and its imports (see 'ctssTestsFrom'). Invalidation
      -- walks the pages that DEPEND on the changed one, so it never reaches a page the change is
      -- downstream of — whose tests the change can still alter. Every page's tests are therefore
      -- dropped by any change at all, and re-run for whichever page is asked about next.
      | otherwise = ssf{ssfTests = Nothing, ssfAnnots = keptAnnots ssf}

    -- | A page whose program came from running an importer, forgotten back to nothing, so that the
    -- next read runs the importer again. Every other page is left exactly as it was.
    --
    -- Only a page reached THROUGH the change, never the page the change is. 'propagated' holds both:
    -- it is the walk outward from what was invalidated, and a walk includes where it started. The
    -- distinction is the difference between the two things that put a native page here. Its importer
    -- changed, and its program is what the old importer said — that is the case this exists for.
    -- Or it was itself just read, by an importer that had this moment become ready, and the program
    -- it now holds is the freshly imported one — wiping that reads the page, throws the reading
    -- away, and asks for it again, for ever.
    reReadNative prgmName rp
      | isNativeImport prgmName && prgmName `notElem` invalidations = rp{rpPrgm = Nothing, rpSource = Nothing, rpReads = Nothing}
      | otherwise = rp

    -- | A page's annotations, where the change cannot have altered them.
    --
    -- An annotation is evaluated against the page's own program, which the walk above covers, and
    -- against whatever it asked the compilation service for. A kept annotation is what
    -- 'getEvalAnnots' answers with before anything is asked again, so an ask whose answer the change
    -- could alter is what has to drop it.
    --
    -- Which asks those were is read off the page ('Eval.Common.ssfAnnotAsks') rather than worked out
    -- from what displays there are. Enumerating it was already wrong: a @#taskList@ reads the tasks of
    -- every note, a vault does not import its notes, and so the walk above never reached the page
    -- showing them — one note edited left another page's list of its tasks as it was. A display may
    -- now ask about any page at any stage, so there is no list of what one may reach to keep correct.
    --
    -- An ask about a scope is altered by a change to page @C@ only if that scope imports @C@, and the
    -- pages which import a changed one are exactly 'propagated'. So an ask is checked by asking
    -- whether the scope it named is in the walk — no second walk, and no widening to every page.
    keptAnnots ssf
      | pagesChanged                                = Nothing
      | aaEvery asks                                = Nothing
      | any (`S.member` propagated) (aaScopes asks) = Nothing
      | otherwise                                   = ssfAnnots ssf
      where asks = ssfAnnotAsks ssf

-- | Build pages incrementally, one SCC at a time.
-- Each successfully built SCC is committed to the MVar independently,
-- so earlier SCCs (e.g. core library) remain cached even if a later SCC fails.
ctssBuildPagesExact :: CTSS -> GraphData SSF FileImport -> IO ()
ctssBuildPagesExact _ pages | all (ssfBuilt . fst3) (graphToNodes pages) = return ()
ctssBuildPagesExact ctss pages = do
  -- stronglyConnCompR returns SCCs in reverse topological order (dependencies first)
  let sccs = stronglyConnCompR $ graphToNodes pages
  -- What is left to do, counted in pages rather than in components. A component is one page or a
  -- hundred, so a count of components says almost nothing about how much is left, while a page is
  -- roughly a page's worth of work.
  --
  -- 'ssfBuilt' is read from the graph this was handed, while 'ctssBuildSCC' re-checks it against the
  -- service as it is by the time it takes the lock, and may find another thread has built the
  -- component already. So a page can be counted done without this thread having done it — which is
  -- still what a reader waiting here is waiting for, since what they are waiting for is the page
  -- being built and not this thread being the one to build it.
  let unbuilt nodes = length [() | node <- nodes, not $ ssfBuilt (fst3 node)]
  let toBuild = sum [unbuilt (flattenSCC comp) | comp <- sccs]
  built <- newIORef (0 :: Int)
  forM_ sccs $ \comp -> do
    let sccNodes = flattenSCC comp
    unless (all (ssfBuilt . fst3) sccNodes) $ do
      done <- readIORef built
      ctssReport ctss StepBuilding done toBuild (listToMaybe $ mapMaybe (impDisp . snd3) sccNodes)
      ctssBuildSCC ctss sccNodes pages
      modifyIORef' built (+ unbuilt sccNodes)

-- | Build a single SCC and commit the result to the MVar atomically.
ctssBuildSCC :: CTSS -> [(SSF, FileImport, [FileImport])] -> GraphData SSF FileImport -> IO ()
-- Nothing is reported before the lock is taken. A thread waiting for it is not the thread doing the
-- work, and a report from here would overwrite the reporting of the one that is.
ctssBuildSCC ctss@(CTSS ssmv _) sccNodes pages = do
  let sccFileNames = map snd3 sccNodes
  let sccNamesSet = S.fromList sccFileNames
  -- Get dep lists from the pages graph structure (stable across MVar updates)
  let pageDeps = H.fromList [(name, deps) | (_, name, deps) <- graphToNodes pages]
  modifyMVar_ ssmv $ \ctssdat@CTSSDat{ctssPages, ctssData, ctssInstrument, ctssEpochs} -> do
    -- Re-check with current state (another thread may have built this SCC)
    let currentSSFs = mapMaybe (`H.lookup` ctssPages) sccFileNames
    if all ssfBuilt currentSSFs
      then return ctssdat
      else do
        -- Look up current SSFs and deps for SCC members
        let currentSCCNodes = mapMaybe (\name -> do
              ssf <- H.lookup name ctssPages
              let deps = fromMaybe [] $ H.lookup name pageDeps
              return (ssf, name, deps)) sccFileNames
        -- The epoch tally is handed in rather than read back off the service, because this is
        -- inside the lock the service is held under: a pipeline reading the 'MVar' it is being run
        -- from would wait for itself.
        p <- runCResT $ buildSCCPipeline ctss ctssData sccNamesSet currentSCCNodes pages (ctssInstrument sccFileNames) ctssEpochs
        case p of
          CRes notes updates -> do
            unless (null notes) $ putStrLn $ prettyCNotes notes
            return $ ctssMapPages (\k ssf -> fromMaybe ssf (H.lookup k updates)) ctssdat
          CErr notes -> do
            unless (null notes) $ putStrLn $ prettyCNotes notes
            -- A page whose pipeline hard-failed is still a page the service holds: commit the
            -- failure to the SCC's members rather than killing the host process (tests, LSP,
            -- webdocs all live above this). Every stage field already carries 'CRes', and every
            -- reader ('evaluablePages', 'joinedStage', 'ctssGetFrom') fails a 'CErr' page with
            -- these notes; dependent SCCs skip a failed dependency ('cresToMaybe') and report
            -- their own unresolved names. 'ssfBuilt' marks the build decided so the loop moves on.
            let failSSF ssf = ssf{
                  ssfBuilt = True,
                  ssfDes = Just $ CErr notes,
                  ssfTPrgm = Just $ CErr notes,
                  ssfTypeInfo = Just $ CErr notes,
                  ssfTBPrgm = Just $ CErr notes
                }
            let failedUpdates = H.fromList [(name, failSSF ssf) | (ssf, name, _) <- currentSCCNodes]
            return $ ctssMapPages (\k ssf -> fromMaybe ssf (H.lookup k failedUpdates)) ctssdat

-- | Run the full pipeline (desugar → typecheck → build → annotate) for a single SCC.
-- Returns a map of updated SSFs for the SCC members.
buildSCCPipeline :: CTSS -> GraphData SSF FileImport -> S.HashSet FileImport
                 -> [(SSF, FileImport, [FileImport])] -> GraphData SSF FileImport
                 -> [AnyInstrument] -> TVar [(FileImport, (Integer, Integer))] -> CResT IO (H.HashMap FileImport SSF)
buildSCCPipeline ctss ctssData sccNamesSet sccNodes pages instruments epochsVar = do
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
      -- Which of them those are is written down beside them, because a page's inference is read by
      -- running its typecheck again ('ctssTypeInfo') and a typecheck means what the set it was
      -- solved against makes it mean. A dependency that failed to typecheck is passed over here, so
      -- the names kept are the ones taken rather than the ones looked at.
      let extDepTCNames = [name | name <- extDepNames, isJust (graphLookup name ctssData >>= ssfTPrgm >>= cresToMaybe)]
      let extDepTC = mapMaybe (\name ->
            graphLookup name ctssData >>= ssfTPrgm >>= cresToMaybe
            ) extDepNames

      -- Step 5: Typecheck with SCC peers and external deps
      let tcNodesForTC = [(des, peerDes, extDepTC)
            | (des, (name, _)) <- zip finalDes namesAndDeps
            , let peerDes = [d | (d, (n, _)) <- zip finalDes namesAndDeps, n /= name]]
      -- One instrument list for the whole component, because the solve is one: the pages of an SCC
      -- share an environment and an instrument watches that environment, so an instrument asked for
      -- by one of them is attached to all of them. Ordinarily this is 'defaultInstruments' and the
      -- inference is not kept; where something asked first — @catln typetrace@ naming a page before
      -- anything was built — the build is watched and the page is compiled once rather than
      -- compiled and then compiled again.
      let tcCRes = typeCheckToRes $ typecheckPrgmsWith instruments tcNodesForTC
      let tcNotes = getCNotes tcCRes
      -- Said here and nowhere above, because nothing above forces anything. Step 1's 'desPrgm' is
      -- bound through 'asCResT', which settles only whether it failed, and step 3's 'desFinalPasses'
      -- is a let. The bind below is what forces the whole of both, and is where a build of anything
      -- larger than a single page spends nearly all of its time — so reporting the steps above as
      -- done would be reporting work which has not happened yet.
      --
      -- Keeping the count the build published rather than one of its own. 'typecheckPrgms' has no
      -- place to report from — giving it one means threading a report through the whole of
      -- 'TypeCheck' — so there is no fraction to be had for the type check itself, and none is
      -- invented. What is still true is how many pages the build has left, which is what the reader
      -- is waiting on; this only says which of them it is inside.
      lift $ ctssReportAt ctss StepTypechecking (listToMaybe $ mapMaybe (impDisp . fst) namesAndDeps)
      tcResults <- asCResT tcCRes

      -- What that solve cost, for a service being measured. Recorded ONCE for the component and
      -- named by the first of its pages: the pages of an SCC share one solve and are handed the
      -- same results ('typecheckPrgmsWith'), so a per-page tally would count the one solve once per
      -- page. Nothing is recorded unless the instruments this build was given count epochs, so an
      -- ordinary build does not pay for the measurement.
      forM_ [ (name, (n, f))
            | (name, _) <- take 1 namesAndDeps
            , (_, _, results) <- take 1 tcResults
            , Just n <- [lookupEpochCountResult results]
            , Just f <- [lookupFragmentCountResult results]
            ] $ \e -> lift $ atomically $ modifyTVar' epochsVar (e :)

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

      -- Step 7: Eval build the SCC's pages. The dependency pages stay in the graph, because the
      -- SCC's eval environments are assembled from the whole reachable set, but their own trees are
      -- NOT rebuilt: each was built when its own fragment was solved and is cached in its
      -- 'ssfTBPrgm', and step 8 below reads back only the SCC's names.
      -- Tolerate failure: a type-erroneous program cannot build a tree, but we
      -- still want to cache the desugar/typecheck artifacts (including the trace)
      -- for tooling. Any eval-build error is deferred into ssfTBPrgm below.
      let tbprgmGraphRes = evalBuildAllOf (`S.member` sccNamesSet) tprgmGraph

      -- Step 8: Assemble updated SSFs
      let validUpdates = [(name, ssf{
              ssfBuilt = True,
              ssfDes = Just $ pure des,
              -- failOnErrorNotes so eval consumers (run/test/build) cleanly reject
              -- a type-erroneous program, while the inference below still explains why.
              ssfTPrgm = Just $ failOnErrorNotes $ CRes tcNotes tprgm,
              -- The pages of this solve IN THE ORDER it took them, not the component as a set: the
              -- solve is over their programs joined, and a rerun which joined them differently
              -- would be a rerun of a different program.
              ssfTcInputs = Just (map fst namesAndDeps, extDepTCNames),
              -- Kept only where this solve was watched by something that produced an inference to
              -- keep. Carries the notes but is deliberately not failOnErrorNotes: the whole point
              -- of reading an inference is to explain a program that failed to typecheck, and the
              -- notes are what say why, so a type inference view needs both together.
              ssfTypeInfo = if isJust (lookupTraceResult results)
                then Just $ CRes tcNotes (vprgm, results)
                else Nothing,
              ssfTBPrgm = case tbprgmGraphRes of
                CRes _ g  -> fmap pure $ graphLookup name g
                CErr errs -> Just $ CErr errs
            })
            | ((_, ssf, name, _), des, (tprgm, vprgm, results)) <-
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
ctssLoadOf ctss@(CTSS ssmv _) select = aux S.empty S.empty
  where
    aux asked built = do
      CTSSDat{ctssData} <- readMVar ssmv
      let unread = [prgmName | prgmName <- select ctssData
                             , maybe False (isNothing . ssfRaw) (graphLookup prgmName ctssData)]
      unless (null unread) $ do
        -- A page read by an importer written in Catln cannot be read until the page declaring that
        -- importer has been BUILT, and a read cannot build — it holds 'ctssReadLock' for its whole
        -- walk. So the read leaves such a page unread and names what it is waiting on
        -- ('Syntax.Ct.Prgm.rpBlocked'), and this is where that is acted on: the importers are built
        -- HERE, outside any read, and then the read is asked again.
        let waiting = [(prgmName, b) | prgmName <- unread
                                     , Just ssf <- [graphLookup prgmName ctssData]
                                     , b <- rpBlocked (ssfRead ssf)]
        let blockers = S.toList $ S.difference (S.fromList $ map snd waiting) built
        case blockers of
          [] -> do
            -- What the loop did before importers existed, unchanged: a page asked for which comes
            -- back unread is a promise broken, and is reported rather than asked for again.
            case filter (`S.member` asked) unread of
              [] -> return ()
              again -> fail $ printf "Asked for %s and was given them back unread" (intercalate ", " $ map (show . impRaw) again)
            void $ ctssReadOf ctss unread
            aux (S.union asked $ S.fromList unread) built
          _ -> do
            -- A page which can only be read by an importer that is itself only reachable through
            -- that page is a page nothing can ever read. Nothing above would stop: building the
            -- importer would ask for the page, which would wait for the importer. Reported by name,
            -- since a loop here is a hang rather than a failure.
            forM_ waiting $ \(prgmName, blocker) ->
              when (blocker `elem` blockers && prgmName `S.member` reaches ctssData blocker) $
                fail $ printf "The importer declared in %s reads %s, which %s is compiled from" (show $ impRaw blocker) (show $ impRaw prgmName) (show $ impRaw blocker)
            mapM_ (ctssBuildFrom ctss) blockers
            void $ ctssReadOf ctss unread
            -- 'built' only grows, and is bounded by the pages there are, so a blocker which does not
            -- unblock its page is tried once and then falls into the report above rather than being
            -- built for ever.
            aux asked (S.union built $ S.fromList blockers)

    reaches ctssData prgmName = S.fromList $ map snd3 $ graphToNodes $ graphFilterReaches prgmName ctssData

-- | Read the programs of these pages, where a read has left them unread.
ctssLoad :: CTSS -> [FileImport] -> IO ()
ctssLoad ctss names = ctssLoadOf ctss (const names)

ctssBuildFrom :: CTSS -> FileImport -> IO ()
ctssBuildFrom ctss@(CTSS ssmv _) src = do
  ctssLoadOf ctss (map snd3 . graphToNodes . graphFilterReaches src)
  CTSSDat{ctssData} <- readMVar ssmv
  ctssBuildPagesExact ctss (graphFilterReaches src ctssData)

ctssBuildAll :: CTSS -> IO ()
ctssBuildAll ctss@(CTSS ssmv _) = do
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
ctssKeys (CTSS ssmv _) = do
  CTSSDat{ctssData} <- readMVar ssmv
  return $ map snd3 $ graphToNodes ctssData

-- | The page a service opens at: the first file it was started with. Which page that is, is the
-- service's own knowledge, and a reader of it would otherwise have to guess it out of a list of
-- every page. Resolved the same way a page asked for by name is, so it is a page that can be asked
-- for; a service started with no files at all has none.
ctssRootPage :: CTSS -> IO (Maybe String)
ctssRootPage (CTSS ssmv _) = do
  CTSSDat{ctssConfig, ctssBaseFileNames} <- readMVar ssmv
  case ctssBaseFileNames of
    []       -> return Nothing
    (name:_) -> impDisp <$> mkDesCanonicalImportStr ctssConfig name

-- | The day the service reads @today@ as, which is what an evaluation resolves a relative date
-- against.
--
-- Whatever the service was configured with, or the day it is now for a service configured with none.
-- Read per evaluation rather than once for the run, so a doc server left open across a midnight
-- answers the next day's questions with the next day.
serviceToday :: CTSS -> IO Day
serviceToday (CTSS ssmv _) = do
  CTSSDat{ctssConfig} <- readMVar ssmv
  maybe (utctDay <$> getCurrentTime) return $ ctssToday ctssConfig

ctssGet :: (SSF -> Maybe (CRes a)) -> CTSS -> CResT IO (GraphData a FileImport)
ctssGet f ctss@(CTSS ssmv _) = do
  lift $ ctssBuildAll ctss
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  let computed = graphFromEdges $ filter (isJust . f . fst3) $ graphToNodes ctssData
  asCResT $ mapMGraph (requireComputed . f) computed

ctssGetFrom :: (SSF -> Maybe (CRes a)) -> FileImport -> CTSS -> CResT IO (GraphData a FileImport)
ctssGetFrom f prgmName ctss@(CTSS ssmv _) = do
  lift $ ctssBuildFrom ctss prgmName
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  asCResT $ mapMGraph (requireComputed . f) (graphFilterReaches prgmName ctssData)

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
ctssEvaluableFrom f scope ctss@(CTSS ssmv _) = do
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
getSource ctss@(CTSS ssmv _) prgmName = do
  lift $ ctssLoad ctss [prgmName]
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  case graphLookup prgmName ctssData of
    Just SSF{ssfSource=Just src} -> return src
    Just SSF{ssfSource=Nothing}  -> fail $ printf "No source text available for %s" (show prgmName)
    Nothing                      -> fail $ printf "Could not find %s" (show $ impRaw prgmName)

-- | The parsed program of one page. Like 'getSource' this does not build, as the parse is what
-- reading the page already produced.
getRawPrgmOf :: CTSS -> FileImport -> CResT IO PPrgm
getRawPrgmOf ctss@(CTSS ssmv _) prgmName = do
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
setSource ctss@(CTSS ssmv _) prgmName src = do
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
getRawPrgm ctss@(CTSS ssmv _) = do
  lift $ ctssLoadOf ctss (map snd3 . graphToNodes)
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  asCResT $ mapMGraph (requireComputed . ssfRaw) ctssData

-- | Like getRawPrgm but only returns the subgraph reachable from a given file.
getRawPrgmFrom :: FileImport -> CTSS -> CResT IO PPrgmGraphData
getRawPrgmFrom prgmName ctss@(CTSS ssmv _) = do
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
ctssTestPages ctss@(CTSS ssmv _) prgmName = do
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
  , i <- map oaTestId (flatObjectMap objMap) ++ map (getMetaId . getExprMeta) globalAnnots
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
      -- A page's tests are no longer run here before anything asks. A @#test@ display asks the
      -- service for the run itself, so they are run when one is being displayed and not when a page
      -- merely writes one — which is the same set of pages, decided by the thing that needs them
      -- rather than guessed at in advance.
      svc <- lift $ newEvalService ctss (Just prgmName) =<< serviceToday ctss
      let annots = evalAnnots (Just svc) prgmName tprgms
      -- An evaluation is a program the reader wrote, so there is no count of what it has left to do
      -- and none is invented. What can be said is whose annotations are being evaluated. Said after
      -- 'ctssGetFrom' above, which is what did the building, so this does not overwrite the report of
      -- a build it was waiting on.
      lift $ ctssReport ctss StepEvaluating 0 0 (impDisp prgmName)
      -- Evaluated here rather than left for whoever displays it, because what it asked the service
      -- about is only known once it has asked, and that record is kept with it. It is the same work
      -- either way and it is done once: what is kept is the value that was forced, not a second
      -- computation of it.
      asks <- lift $ annots `seq` esAsks svc
      lift $ ctssStore prgmName (\ssf -> ssf{ssfAnnots = Just annots, ssfAnnotAsks = asks}) ctss
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
  -- The pages of the scope which can be evaluated, with the rest reported rather than fatal, for the
  -- same reason 'WebDocs.annotResp' takes them that way: a page which did not typecheck cannot be
  -- built over at all, and a search box is not something to lose because some other page is broken.
  tprgms <- ctssEvaluableFrom ssfTPrgm scope ctss
  prgm <- asCResT $ maybe (return $ mconcat $ map fst3 $ graphToNodes tprgms) (`prgmFromGraphData` tprgms) scope
  -- Nothing keeps what this answers, so nothing reads what it asked: a call is answered and
  -- forgotten, and there is no kept value to be dropped when the answer changes.
  svc <- lift $ newEvalService ctss scope =<< serviceToday ctss
  asCResT $ evalDShowCall (evalBaseEnvService (Just svc) prgm) fun args

-- | The service, as a read asks it about importers written in Catln.
--
-- Everything here is answered out of a snapshot and nothing here builds, reads, or takes a lock.
-- That is not an optimisation but the condition of the thing working at all: this runs INSIDE
-- 'ctssReadOf''s hold of 'ctssReadLock', and building would reach 'ctssLoadOf' and take that lock
-- again. An MVar is not re-entrant, so a callback which went and built something would hang rather
-- than be slow. Whoever drives the read is what builds ('ctssLoadOf'); this only ever reports what
-- has been built already.
--
-- The lock order is the one everything else here uses -- read lock, then 'ssmv' -- so taking 'ssmv'
-- from inside the read is safe. It stays safe only while nothing takes the read lock while holding
-- 'ssmv', which nothing does.
ctssImporterEnv :: CTSS -> ImporterEnv
ctssImporterEnv (CTSS ssmv _) = noImporters{ieReady, ieInvoke}
  where
    -- The page a path names, out of the pages the service holds. Pages are keyed by the import which
    -- names them, so this is the reverse of 'Syntax.Ct.Prgm.importPath'.
    --
    -- The path is resolved the same way an import of it would be, because a page is keyed by where
    -- it IS and an importer may be named by where it was written from. The two differ whenever the
    -- name is relative, and comparing the written name against the resolved key finds nothing —
    -- which reads as "the importer's page has not been read" about a page that has been read and
    -- built. The written name is still matched as well, for a page that resolves to nothing on disk.
    pageAt config ctssData at = do
      resolved <- findImportPath config Nothing at
      return $ listToMaybe [prgmName | (_, prgmName, _) <- graphToNodes ctssData
                                     , importPath prgmName `elem` (Just at : [resolved | isJust resolved])]

    ieReady page = do
      CTSSDat{ctssConfig, ctssData} <- readMVar ssmv
      at <- pageAt ctssConfig ctssData page
      return $ case at >>= (`graphLookup` ctssData) of
        Nothing -> Just $ ImportBlocked page "has not been read"
        Just ssf
          | not (ssfBuilt ssf) -> Just $ ImportBlocked page "has not been built"
          -- A page marked built with no typechecked program is a @ctx@ page, which
          -- 'buildSCCPipeline' deliberately skips. It will never gain one, so this is a page that
          -- waits for ever rather than one that waits: reported as such, so that 'ctssLoadOf' retires
          -- it instead of building it again and again.
          | isNothing (ssfTPrgm ssf) -> Just $ ImportBlocked page "declares no program, so nothing on it can be run"
          | otherwise -> Nothing

    ieInvoke page fun readPath args = do
      CTSSDat{ctssConfig, ctssData} <- readMVar ssmv
      at <- pageAt ctssConfig ctssData page
      case at of
        Nothing -> return $ Left $ ImportBlocked page "has not been read"
        Just prgmName -> do
          let scope = graphToNodes $ graphFilterReaches prgmName ctssData
          let called = do
                tprgms <- evaluablePages ssfTPrgm True scope
                prgm <- prgmFromGraphData prgmName tprgms
                -- No 'EvalService'. An importer must not be able to ask the service anything: every
                -- way of asking reaches 'ctssBuildAll' and so 'ctssReadOf', which is the deadlock
                -- above. It is the same reason 'Eval.evalPageValues' takes none.
                val <- evalCallTo (evalBaseEnv prgm) importResultType fun (("path", StrVal readPath) : [(k, StrVal v) | (k, v) <- args])
                either fail return (fromCt val)
          -- Forced before this returns. 'Eval.Common.CtServiceApi' states the rule: a lazy value
          -- carrying an unforced ask is forced by whoever reads it, which here would be
          -- 'buildSCCPipeline' inside @modifyMVar_ ssmv@ -- where the 'readMVar' above would hang
          -- rather than fail.
          case called of
            CErr notes -> return $ Left $ ImportBlocked page $ prettyCNotes notes
            CRes _ parsed -> do
              _ <- evaluate (hash $ piPrgm parsed)
              return $ Right parsed

-- | Read a page's computed item, without building the page or computing anything.
ctssCached :: (SSF -> Maybe (CRes a)) -> FileImport -> CTSS -> IO (Maybe (CRes a))
ctssCached f prgmName (CTSS ssmv _) = do
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
ctssStore prgmName store (CTSS ssmv _) = modifyMVar_ ssmv $ \ctssdat ->
  return $ ctssMapPages keep ctssdat
  where
    keep k ssf | k == prgmName && ssfBuilt ssf = store ssf
               | otherwise = ssf

-- | A page's type inference: the encoded program its typecheck was over, and what the given
-- instruments made of that typecheck.
--
-- The page is built first, whatever comes of this. A build is what settles whether the page
-- compiles at all, and what everything else about the page is read from; running a typecheck of
-- one's own instead would answer about a page the service does not hold.
--
-- Then, if the build was already watched — because whoever set it up knew which page they were
-- going to read — that is the answer. Otherwise the page is typechecked AGAIN, watched this time,
-- from the same programs the build was given: its component's peers as they were desugared, and
-- its dependencies as they were typechecked. Solved from the same inputs by the same code, so what
-- comes back is the inference of the compiled page rather than of some other reading of it.
--
-- What comes back is kept on the page, because an inference is read a point at a time: a reader
-- clicking through the points of one definition would otherwise pay for the whole solve at each of
-- them.
ctssTypeInfo :: [AnyInstrument] -> FileImport -> CTSS -> CResT IO (VPrgm, InstrumentResults)
ctssTypeInfo instruments prgmName ctss@(CTSS ssmv _) = do
  lift $ ctssBuildFrom ctss prgmName
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  ssf <- case graphLookup prgmName ctssData of
    Just ssf -> return ssf
    Nothing  -> fail $ printf "Could not find %s" (show $ impRaw prgmName)
  case ssfTypeInfo ssf of
    Just kept -> asCResT kept
    Nothing -> do
      (sccNames, depNames) <- case ssfTcInputs ssf of
        Just inputs -> return inputs
        -- A page which is built but does not say what it was built against. Every page the pipeline
        -- typechecks writes it, so this is a page the pipeline passed over — a @ctx@ page, which
        -- has no typecheck to report on.
        Nothing -> fail $ printf "%s was not typechecked, so it has no inference to report on" (show $ impRaw prgmName)
      let desOf name = requireComputed (ssfDes =<< graphLookup name ctssData)
      let tprgmOf name = requireComputed (ssfTPrgm =<< graphLookup name ctssData)
      computed <- asCResT $ do
        sccDes <- mapM desOf sccNames
        depTPrgms <- mapM tprgmOf depNames
        -- Assembled exactly as the build assembled it: one node per page of the component, each
        -- given the same dependency set. The whole component is passed rather than the one page
        -- asked about, because the component is what was solved — its pages share an environment,
        -- and a page solved alone is a different program from the same page solved with its peers.
        --
        -- Not failOnErrorNotes, for the reason the build's own store is not: a page which failed
        -- to typecheck is exactly the page whose inference is worth reading, and the notes are
        -- what say why.
        let peersOf name = [d | (n, d) <- zip sccNames sccDes, n /= name]
        let nodes = [(des, peersOf name, depTPrgms) | (name, des) <- zip sccNames sccDes]
        typeCheckToRes $ typecheckPrgmsWith instruments nodes
      case [(vprgm, results) | (name, (_, vprgm, results)) <- zip sccNames computed, name == prgmName] of
        [] -> fail $ printf "Typechecking %s again for its inference did not produce that page" (show $ impRaw prgmName)
        info:_ -> do
          -- Stored outside the lock the build takes, which this is: the typecheck above is the work
          -- and it has already happened.
          lift $ ctssStore prgmName (\s -> s{ssfTypeInfo = Just $ pure info}) ctss
          return info

-- | Forget when a page's files were last written, so the next read reads it again.
--
-- What a page was read from is kept, so the read still knows which files to look at; only the claim
-- that they are still as they were is dropped.
ctssStale :: CTSS -> FileImport -> IO ()
ctssStale (CTSS ssmv _) prgmName = modifyMVar_ ssmv $ \ctssdat ->
  return $ ctssMapPages forget ctssdat
  where
    forget k ssf | k == prgmName = ssf{ssfRead = (ssfRead ssf){rpReads = Nothing}}
                 | otherwise = ssf

-- Helpers

getPrgm :: CTSS -> CResT IO (GraphData DesPrgm FileImport)
getPrgm = ctssGet ssfDes

getTPrgm :: CTSS -> CResT IO (GraphData TPrgm FileImport)
getTPrgm = ctssGet ssfTPrgm

getTPrgmJoined :: CTSS -> CResT IO TPrgm
getTPrgmJoined = ctssGetJoined ssfTPrgm

getTBPrgm :: CTSS -> CResT IO (GraphData TBPrgm FileImport)
getTBPrgm = ctssGet ssfTBPrgm

-- | The service as an evaluation of one of its programs asks things of it.
--
-- Each of these is an existing operation of the service under the name an evaluation asks for it by.
-- Nothing here decides anything on the evaluation's behalf — what a scope means, which pages an
-- answer covers, and what to make of what comes back are all the asking primitive's
-- ('Eval.Runtime.WebDocs'), which is what keeps a display from needing a change here to be added.
instance CtServiceApi CTSS where
  csPages = ctssKeys

  -- Every page, with the scope built. 'graphFilterReaches' over what comes back is the scope; the
  -- rest is there for a display that reports on which pages there are rather than on the program it
  -- was evaluated against. Building all of them for an unscoped evaluation is what that evaluation
  -- asked for by not naming a page.
  csBuilt ctss@(CTSS ssmv _) scope = do
    maybe (ctssBuildAll ctss) (ctssBuildFrom ctss) scope
    ctssData <$> readMVar ssmv

  csTests ctss prgmName = runCResT $ ctssTestsFrom ctss prgmName

  -- With the whole trace attached, which is what a reader of an inference reads, and the epoch
  -- count beside it. A page whose build was already watched answers out of what that build kept;
  -- any other is typechecked again here.
  csTypeInfo ctss prgmName = runCResT $ ctssTypeInfo typeInfoInstruments prgmName ctss

  -- Against the page's own typecheck program rather than the caller's, which for an annotation is
  -- that program joined with the synthetic one holding the annotation itself.
  csRunFun ctss prgmName fun = do
    ssfs <- scopeSSFs ctss prgmName
    return $ evalDebugPrgm fun =<< joinedStage ssfTPrgm ssfs

  -- Against the pages of the scope rather than their joined program, since compiling the expression
  -- typechecks it against them one by one, the way a page of the scope was itself typechecked.
  csRunExpr ctss@(CTSS ssmv _) prgmName src = do
    ctssBuildFrom ctss prgmName
    CTSSDat{ctssData} <- readMVar ssmv
    return $ debugExprRun prgmName (graphToNodes $ graphFilterReaches prgmName ctssData) src

  -- 'pageValues' partially applied, which is what shares the scope's typechecked programs across
  -- every page the gatherer is then asked about. Applying it per page instead is the quadratic
  -- reading its own documentation warns about.
  csValues ctss scope = do
    graph <- csBuilt ctss scope
    return $ pageValues $ maybe graph (`graphFilterReaches` graph) scope

  -- 'pageAsk' partially applied over the same scope 'csValues' gathers from, so that a value
  -- gathered from a page is asked about in the program of the page it was gathered from.
  csAsk ctss scope = do
    graph <- csBuilt ctss scope
    return $ pageAsk $ maybe graph (`graphFilterReaches` graph) scope

-- | The pages of a scope, built, as the pipeline records of each.
scopeSSFs :: CTSS -> FileImport -> IO [SSF]
scopeSSFs ctss@(CTSS ssmv _) prgmName = do
  ctssBuildFrom ctss prgmName
  CTSSDat{ctssData} <- readMVar ssmv
  return $ map fst3 $ graphToNodes $ graphFilterReaches prgmName ctssData

-- | Compiles one expression, written as Catln source, as though it were a statement of a page, and
-- runs it. This is 'CtServiceApi.csRunExpr' of a page: the same compile an annotation asked for over
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
  scopeTPrgms <- evaluablePages ssfTPrgm True scopeNodes
  let deps = graphToNodes scopeTPrgms
  exprTprgm <- case typeCheckToRes $ typecheckPrgms [(desP', [], map fst3 deps)] of
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
-- 'CtServiceApi.csValues' of a set of pages: what a value gathered across pages means, without any
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
-- one gatherer rather than a call made afresh each time.
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

    exprId = getMetaId . getExprMeta

-- | The answers a named method gives about the values of one page. This is the
-- 'CtServiceApi.csAsk' of a set of pages, and the companion of 'pageValues' above: that says what a
-- page declares, this asks one of those values something.
--
-- A page which was never built is reported rather than answered with nothing, as 'pageValues'
-- reports one and for its reason: a display gathering across pages must never be quietly short of
-- the pages nobody has looked at yet. Unlike there, a page built without a typechecked program is
-- reported too — a value was gathered from it, so it has a program, and being asked about a page
-- that declared nothing is a caller's mistake rather than an empty answer.
pageAsk :: GraphData SSF FileImport -> Name -> FileImport -> CRes (Val -> CRes Val)
pageAsk graph method prgmName = case graphLookup prgmName graph of
  Nothing -> fail $ printf "The page %s has not been read, so its values cannot be asked about" (show $ impRaw prgmName)
  Just _ -> do
    let computed = graphFromEdges $ filter (isJust . ssfTPrgm . fst3) $ graphToNodes graph
    tprgms <- mapMGraph (requireComputed . ssfTPrgm) computed
    evalPageAsk method prgmName tprgms

