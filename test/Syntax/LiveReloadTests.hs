-- | Tests for a doc service as its files change under it: which of the things a reader has open a
-- read reaches, and what a reader is still shown once one of those files stops compiling.
--
-- The decision is the service's rather than the reader's, and it is made against the import graph. A
-- page is shown from its own program and the programs it imports, so a write low in the standard
-- library reaches nearly every page there is — which is why a reader is told whether the thing in
-- front of them has changed rather than told what changed. Each case here makes one change and asks
-- which displays it reaches, since that is exactly what a doc server has to get right for a reader
-- looking at one page not to be interrupted by a change to another.
module Syntax.LiveReloadTests where

import           Control.Concurrent     (forkIO, modifyMVar_, newEmptyMVar,
                                         putMVar, takeMVar, threadDelay)
import           Control.Concurrent.STM (STM, atomically, orElse)
import           Control.Exception      (SomeException, evaluate, try)
import           Control.Monad          (void)
import           CRes                   (CNote (..), CNoteI (..), CRes (..),
                                         runCResT)
import           CtService              (CTSS (..), Progress (..),
                                         ProgressStep (..), ReadChange (..),
                                         Shown (..), changeReaches, ctssAddPage,
                                         ctssBaseFiles, ctssCached,
                                         ctssEvaluableFrom, ctssFollow,
                                         ctssFollowing, ctssGetFrom, ctssKeys,
                                         ctssMapPages, ctssProgress, ctssReadOf,
                                         ctssReport, ctssWatch, ctssWatchLive,
                                         ctssWorking, emptyCTSS, serviceToday)
import qualified Data.HashMap.Strict    as H
import qualified Data.HashSet           as S
import           Data.IORef             (modifyIORef', newIORef, readIORef)
import           Data.List              (isPrefixOf, sort)
import           Data.Maybe             (isJust, isNothing, listToMaybe)
import qualified Data.Text              as T
import           Data.Time.Calendar     (fromGregorian)
import           Data.Time.Clock        (addUTCTime, getCurrentTime, utctDay)
import           Eval.Common            (AnnotAsks (..), SSF (..), Val)
import           Eval.Runtime.WebDocs   (askService)
import           Semantics              (CTSSConfig (..), ctssCachePath,
                                         testCtssConfig)
import           Semantics.Prgm         (FileImport, impDisp)
import           Syntax.Ct.Prgm         (noImporters, rootImportCtx)
import           Syntax.Parsers         (canonicalImport, finishReading,
                                         mkRawImportStr, newReadQueue,
                                         newReading, readQueued, rqLeft,
                                         rqRestart)
import           Syntax.RereadTests     (caseDir, writeLater)
import           System.Directory       (setModificationTime)
import           System.FilePath        ((</>))
import           System.Timeout         (timeout)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Utils                  (graphToNodes)

-- | The pages under a directory whose own display a read reaches, by the path each is shown at.
--
-- Asked of each page's display through 'changeReaches' rather than of the change directly, because
-- that is the decision a doc server makes. Confined to one directory to leave out the pages a case
-- did not write.
pagesReached :: FilePath -> ReadChange -> CTSS -> IO [FilePath]
pagesReached dir change ctss = do
  pages <- ctssKeys ctss
  return $ sort [path | page <- pages
                      , changeReaches change (Shown (Just page))
                      , Just path <- [impDisp page]
                      , dir `isPrefixOf` path]

-- | The page a service holds at a path. Reported rather than passed over, since a case that asked
-- about a page the service does not hold would assert about nothing and pass.
pageAt :: FilePath -> CTSS -> IO FileImport
pageAt path ctss = do
  pages <- ctssKeys ctss
  case [page | page <- pages, impDisp page == Just path] of
    [page] -> return page
    found  -> assertFailure $ "the service holds " ++ show (length found) ++ " pages shown at " ++ path

-- | Whether the wait would end now, asked without waiting.
awoken :: STM () -> IO Bool
awoken await = atomically $ (await >> return True) `orElse` return False

-- | Whether the wait ends within a few seconds, or has not by then.
--
-- Waited for rather than slept on, so a machine which wakes quickly is not waited on and one which
-- wakes slowly is not failed. Only the case driven by the watcher needs this: everywhere else the
-- read is made by the case itself, so whether it woke anything is already decided when it returns.
waitFor :: STM () -> IO Bool
waitFor await = aux (300 :: Int)
  where
    aux 0 = return False
    aux n = do
      hit <- awoken await
      if hit then return True else threadDelay 10000 >> aux (n - 1)

-- | A page written to reaches the pages which import it, and no others.
--
-- This is the whole of what makes a doc server bearable to read while its files are being edited: a
-- change is shown where it can be seen and nowhere else. @c.ct@ is a page of the same service which
-- imports nothing that changed, and it must not move.
testImporters :: TestTree
testImporters = testCase "a page written to reaches the pages which import it, and no others" $ do
  dir <- caseDir "live-importers"
  now <- getCurrentTime
  writeLater now (dir </> "a.ct") "import \"b.ct\"\na = 1\n"
  writeLater now (dir </> "b.ct") "b = 2\n"
  writeLater now (dir </> "c.ct") "c = 3\n"
  ctss <- ctssBaseFiles testCtssConfig [dir </> "a.ct", dir </> "c.ct"]

  unchanged <- ctssReadOf ctss []
  assertEqual "a read of unchanged files reached a page" [] =<< pagesReached dir unchanged ctss

  writeLater (addUTCTime 60 now) (dir </> "b.ct") "b = 3\n"
  changed <- ctssReadOf ctss []
  assertEqual "the write did not reach the page written to and the page which imports it"
    [dir </> "a.ct", dir </> "b.ct"] =<< pagesReached dir changed ctss
  assertBool "the write said that which pages there are had changed" $ not (rcPages changed)

-- | A page written to without changing reaches nothing.
--
-- A file saved with nothing altered, or altered only in what the parse discards, is read again and
-- gives back the program it gave before. Nothing built from it is dropped, so nothing showing it has
-- anything to show again — an editor which saves on every keystroke would otherwise recompile every
-- page downstream of the file being edited.
testUnchangedText :: TestTree
testUnchangedText = testCase "a page written to without changing reaches nothing" $ do
  dir <- caseDir "live-unchanged"
  now <- getCurrentTime
  writeLater now (dir </> "a.ct") "import \"b.ct\"\na = 1\n"
  writeLater now (dir </> "b.ct") "b = 2\n"
  ctss <- ctssBaseFiles testCtssConfig [dir </> "a.ct"]

  writeLater (addUTCTime 60 now) (dir </> "b.ct") "b = 2\n"
  change <- ctssReadOf ctss []
  assertEqual "the page read again reached something though it said the same"
    [] =<< pagesReached dir change ctss

-- | A page appearing reaches every display, including one showing a page it is nothing to.
--
-- Everything shown is about which pages there are, whatever else it is about: a navigation lists
-- them, and a page names the pages either side of it in the order they are read in. So a page
-- appearing between two others genuinely changes what those two say, and there is nothing here that
-- can tell which two without working out the whole order twice.
--
-- Reaching them is not moving them. What keeps the page beside the new one from moving is that its
-- answer comes back the same and is not sent again ('WebDocs.serve'), which is decided by the answer
-- rather than guessed at here. So this asserts the waking, and that a change to a page's CONTENTS
-- stays as narrow as 'testImporters' says.
testPageAdded :: TestTree
testPageAdded = testCase "a page added reaches every display, and an edit still reaches only some" $ do
  dir <- caseDir "live-added"
  now <- getCurrentTime
  writeLater now (dir </> "a.ct") "a = 1\n"
  setModificationTime dir now
  ctss <- ctssBaseFiles testCtssConfig [dir]
  a <- pageAt (dir </> "a.ct") ctss

  let later = addUTCTime 60 now
  writeLater later (dir </> "b.ct") "b = 2\n"
  setModificationTime dir later
  added <- ctssReadOf ctss []
  assertBool "the page added did not change which pages there are" $ rcPages added
  assertBool "the page added did not reach a page it is nothing to" $
    changeReaches added (Shown (Just a))
  assertBool "the page added did not reach a navigation" $
    changeReaches added (Shown Nothing)

  -- Writing to the page that appeared is an ordinary change: it says nothing about which pages there
  -- are, so it reaches what imports it and nothing else.
  writeLater (addUTCTime 120 now) (dir </> "b.ct") "b = 3\n"
  edited <- ctssReadOf ctss []
  assertBool "writing to a page said that which pages there are had changed" $ not (rcPages edited)
  assertBool "writing to one page reached a page which does not import it" $
    not $ changeReaches edited (Shown (Just a))

-- | A display is followed while it is open and not after it has closed.
--
-- A display left on the list is woken for nobody forever, and shows in no other way — it is not an
-- error, it is a service doing work for a reader who left. So the count is asserted as well as the
-- waking, and asserted after the action has thrown as well as after it has returned, because a
-- connection dropping is how a display most often closes.
testFollowCloses :: TestTree
testFollowCloses = testCase "a display is followed only while it is open" $ do
  dir <- caseDir "live-follow"
  now <- getCurrentTime
  writeLater now (dir </> "a.ct") "import \"b.ct\"\na = 1\n"
  writeLater now (dir </> "b.ct") "b = 2\n"
  ctss <- ctssBaseFiles testCtssConfig [dir </> "a.ct"]
  a <- pageAt (dir </> "a.ct") ctss

  ctssFollow ctss (Shown (Just a)) $ \await -> do
    assertEqual "the display being followed was not counted" 1 =<< ctssFollowing ctss
    assertBool "the display was woken before anything changed" . not =<< awoken await
    writeLater (addUTCTime 60 now) (dir </> "b.ct") "b = 3\n"
    _ <- ctssReadOf ctss []
    assertBool "a change to what the display is shown from did not wake it" =<< awoken await
    assertBool "the display was woken twice by one change" . not =<< awoken await
  assertEqual "the display was left followed after it closed" 0 =<< ctssFollowing ctss

  went <- try $ ctssFollow ctss (Shown (Just a)) $ \_ -> fail "the display's reader went"
  case went :: Either SomeException () of
    Left _  -> return ()
    Right _ -> assertFailure "the failing display did not fail"
  assertEqual "the display was left followed after its reader failed" 0 =<< ctssFollowing ctss

-- | A write wakes a display without anything having asked.
--
-- The case the others cannot make: everywhere else the read is made by the case itself, and what a
-- reader following a page needs is that the read happens because the file changed. A reader watching
-- a page is not making requests, so a service which only read when asked would go on showing them a
-- page whose file has been written to.
testWatchLive :: TestTree
testWatchLive = testCase "a write wakes a display with nothing having asked" $ do
  dir <- caseDir "live-watch"
  now <- getCurrentTime
  writeLater now (dir </> "a.ct") "import \"b.ct\"\na = 1\n"
  writeLater now (dir </> "b.ct") "b = 2\n"
  ctss <- ctssBaseFiles testCtssConfig [dir </> "a.ct"]
  ctssWatch ctss
  ctssWatchLive ctss
  a <- pageAt (dir </> "a.ct") ctss

  ctssFollow ctss (Shown (Just a)) $ \await -> do
    writeLater (addUTCTime 60 now) (dir </> "b.ct") "b = 3\n"
    assertBool "the display was not woken by a write nothing asked about" =<< waitFor await


-- | Stand in for a build having left these on the pages.
--
-- Written into the graph rather than built for real, because what the two cases below assert is what
-- a CHANGE does to what a build left, and building the real thing costs either the standard library
-- or -- for a page that does not typecheck -- minutes of the type checker hunting a name that is not
-- there. That a real type error leaves 'ssfTPrgm' failed is 'buildSCCPipeline's doing and is asserted
-- by every other test in this suite going through a real build.
seedBuilt :: CTSS -> (FileImport -> SSF -> SSF) -> IO ()
seedBuilt (CTSS ssmv _) f = modifyMVar_ ssmv $ \d -> return $ ctssMapPages f d

-- | A page that stops compiling does not take the rest of the scope with it.
--
-- What made this matter: a navigation is about which pages there are and reads no program at all,
-- yet a doc server answered it with the type errors of a page it says nothing about — so the moment
-- a reader broke any file, the navigation went out. It cannot simply be left in either: an
-- environment cannot be built over a definition that failed to typecheck at all
-- ('TreeBuild.buildTBEnv' raises rather than reporting on one), which is what 'ssfTPrgm' failing
-- such a page is guarding. So it is left out and said in the notes, which is what this asserts.
testBrokenPageLeftOut :: TestTree
testBrokenPageLeftOut = testCase "a page that does not typecheck is left out, and said in the notes" $ do
  dir <- caseDir "live-broken"
  now <- getCurrentTime
  writeLater now (dir </> "main.ct") "import \"broken.ct\"\nok= 1\n"
  writeLater now (dir </> "broken.ct") "broken= noSuchNameAnywhere\n"
  ctss <- ctssBaseFiles testCtssConfig [dir </> "main.ct"]
  main' <- pageAt (dir </> "main.ct") ctss
  broken <- pageAt (dir </> "broken.ct") ctss
  let note = MkCNote $ GenCErr Nothing "broken.ct does not typecheck"
  seedBuilt ctss $ \name ssf -> ssf{ssfBuilt = True, ssfTPrgm = Just $
    if name == broken then CErr [note] else pure mempty}

  -- What the guard does on its own: the whole scope fails, so nothing in it can be evaluated.
  guarded <- runCResT $ ctssGetFrom ssfTPrgm main' ctss
  case guarded of
    CRes _ _   -> assertFailure "the page which does not typecheck was offered to be evaluated"
    CErr notes -> assertBool "the failure said nothing about why" (not $ null notes)

  evaluable <- runCResT $ ctssEvaluableFrom ssfTPrgm (Just main') ctss
  case evaluable of
    CErr _ -> assertFailure "a page which does not typecheck failed the whole scope"
    CRes notes graph -> do
      assertBool "the type errors were dropped rather than carried as notes" (not $ null notes)
      assertEqual "the scope is not the pages which compiled"
        [dir </> "main.ct"] (sort [path | (_, name, _) <- graphToNodes graph
                                        , Just path <- [impDisp name], dir `isPrefixOf` path])

-- | A page's annotations are dropped when a page appears, though nothing it imports changed.
--
-- An annotation is evaluated against which pages there are as well as against the program it is
-- written in ('Eval.Common.csPages'), and the invalidation walk only reaches pages that import what
-- changed. So a page carrying a navigation went on serving the navigation as it was when the page was
-- built, and a page added never appeared on it — while the same annotation asked for on its own, which
-- is not cached, listed it at once.
testAnnotsFollowThePages :: TestTree
testAnnotsFollowThePages = testCase "a page appearing drops the annotations of a page it is nothing to" $ do
  dir <- caseDir "live-annots"
  now <- getCurrentTime
  writeLater now (dir </> "a.ct") "a = 1\n"
  writeLater now (dir </> "b.ct") "b = 2\n"
  setModificationTime dir now
  ctss <- ctssBaseFiles testCtssConfig [dir]
  a <- pageAt (dir </> "a.ct") ctss
  seedBuilt ctss $ \_ ssf -> ssf{ssfBuilt = True, ssfAnnots = Just (pure [])}
  assertBool "the annotations were not there to be dropped" . isJust =<< ctssCached ssfAnnots a ctss

  -- Writing to one page must not cost another its annotations: nothing that one is evaluated against
  -- has moved, and evaluating a page's annotations again is work.
  writeLater (addUTCTime 60 now) (dir </> "b.ct") "b = 3\n"
  _ <- ctssReadOf ctss []
  assertBool "writing to one page dropped an unrelated page's annotations"
    . isJust =<< ctssCached ssfAnnots a ctss

  -- A page appearing changes which pages there are, which any annotation may be about.
  let later = addUTCTime 120 now
  writeLater later (dir </> "c.ct") "c = 3\n"
  setModificationTime dir later
  _ <- ctssReadOf ctss []
  assertBool "a page appearing left an annotation cached from before it existed"
    . isNothing =<< ctssCached ssfAnnots a ctss

-- | A page's annotations are dropped when a page they asked the compilation service about changed,
-- and only then.
--
-- A display may ask about a page other than the one it is written in — a @#taskList@ gathers the
-- tasks of every note — and the invalidation walk only reaches pages that IMPORT what changed. A
-- vault does not import its notes, so a note edited left another page's list of its tasks exactly as
-- it was. Enumerating what a display may reach was what went wrong, and there is no such list to keep
-- correct now that one may ask for any page; so what each display actually asked is recorded with the
-- answer, and that is what this holds to.
--
-- Both directions matter. A change to a page nobody asked about must NOT cost an unrelated page its
-- annotations, which is what makes recording the asks worth doing rather than dropping everything.
testAnnotsFollowTheirAsks :: TestTree
testAnnotsFollowTheirAsks = testCase "a page's annotations are dropped when a page they asked about changed" $ do
  dir <- caseDir "live-asks"
  now <- getCurrentTime
  writeLater now (dir </> "a.ct") "a = 1\n"
  writeLater now (dir </> "b.ct") "b = 2\n"
  writeLater now (dir </> "c.ct") "c = 3\n"
  setModificationTime dir now
  ctss <- ctssBaseFiles testCtssConfig [dir]
  a <- pageAt (dir </> "a.ct") ctss
  b <- pageAt (dir </> "b.ct") ctss
  -- a.ct's display asked the service about b.ct and nothing else. a.ct does not import b.ct, so the
  -- walk never reaches a.ct from it: the record is the only thing that can say the two are connected.
  seedBuilt ctss $ \name ssf -> ssf{ssfBuilt = True, ssfAnnots = Just (pure [])
                                   , ssfAnnotAsks = if name == a then mempty{aaScopes = S.singleton b} else mempty}
  assertBool "the annotations were not there to be dropped" . isJust =<< ctssCached ssfAnnots a ctss

  -- c.ct is a page of the same service that nothing asked about.
  writeLater (addUTCTime 60 now) (dir </> "c.ct") "c = 4\n"
  _ <- ctssReadOf ctss []
  assertBool "writing to a page nothing asked about dropped an annotation anyway"
    . isJust =<< ctssCached ssfAnnots a ctss

  writeLater (addUTCTime 120 now) (dir </> "b.ct") "b = 3\n"
  _ <- ctssReadOf ctss []
  assertBool "writing to the page an annotation asked about left it saying what it said before"
    . isNothing =<< ctssCached ssfAnnots a ctss

-- | A display's ask of the service is finished before its value is handed back.
--
-- The service is behind an MVar which a build holds for a whole SCC ('CtService.ctssBuildSCC') and a
-- read holds for a whole read ('CtService.ctssReadOf'), and an MVar is not re-entrant. A display's
-- value is kept on the page it was evaluated for and read again whenever something shows that page —
-- so an ask left unfinished inside one would be run at some unrelated later moment, which may be
-- inside one of those locks, and would HANG there rather than fail. That is a failure no test would
-- report as one, which is why the discipline is worth holding to directly.
--
-- 'Eval.Runtime.WebDocs.askService' is where every display's ask happens and where the answer is
-- forced, and it takes its answer as text rather than as a value so that forcing it is total.
testAskServiceForcesItsAnswer :: TestTree
testAskServiceForcesItsAnswer = testCase "a display's ask is finished before its value is handed back" $ do
  -- An answer whose tail cannot be produced. Forced where it is asked, this raises here; left lazy,
  -- an intact value is handed back and raises whenever something later reads it. The answer is
  -- strict text, which is forced whole or not at all, so its tail is reached by forcing it at all.
  let asked = askService $ return $ Right (T.pack ("ok" ++ error "the answer was not forced where it was asked"))
  forced <- try (evaluate asked) :: IO (Either SomeException (Either String Val))
  case forced of
    Left _  -> return ()
    Right _ -> assertFailure "askService handed back a display value with its ask still to be run"

-- | The service holds a page as soon as the read has read it, rather than once the read is over.
--
-- This is what moving the read's queue into the service is for. A read used to hand back the whole
-- graph at the end, so nothing the service held could have any part in the read producing it; now
-- each page is taken up as it arrives ('CtService.ctssAddPage'), and the service is something to be
-- asked about while a read of it is still running.
--
-- Driven page by page rather than through 'ctssReadOf', because what it asserts is what the service
-- holds PART WAY through a read, and a read asked for from outside is over by the time it answers.
-- The pieces are the ones that read makes: the same queue, the same walk, and the same taking up of
-- each page as it arrives.
testPageHeldWhileReading :: TestTree
testPageHeldWhileReading = testCase "the service holds a page before the read that found it is over" $ do
  dir <- caseDir "mid-read"
  now <- getCurrentTime
  writeLater now (dir </> "a.ct") "import \"b.ct\"\na = 1\n"
  writeLater now (dir </> "b.ct") "b = 2\n"
  ctss <- emptyCTSS testCtssConfig
  reading <- newReading (ctssCachePath testCtssConfig) noImporters
  queue <- newReadQueue
  rqRestart queue =<< mapM (canonicalImport testCtssConfig (rootImportCtx noImporters)) [mkRawImportStr (dir </> "a.ct")]
  steps <- newIORef []
  readQueued testCtssConfig reading S.empty H.empty queue $ \prgmName page -> do
    _ <- ctssAddPage ctss prgmName page
    held <- ctssKeys ctss
    modifyIORef' steps ((prgmName, held) :)
  finishReading reading
  taken <- reverse <$> readIORef steps
  -- Only the files written for this case; the standard library is read as well and is nothing to do
  -- with what is being asserted.
  let under names = sort [path | name <- names, Just path <- [impDisp name], dir `isPrefixOf` path]
  assertEqual "the service did not hold each page as soon as the read reached it"
    [(dir </> "a.ct", [dir </> "a.ct"]), (dir </> "b.ct", [dir </> "a.ct", dir </> "b.ct"])]
    [(path, under held) | (prgmName, held) <- taken, Just path <- [impDisp prgmName], dir `isPrefixOf` path]

-- | What the service is working on is answered while it is working on it.
--
-- The case the whole shape of progress reporting exists for. A build holds the service's lock for a
-- whole component, which for the standard library is minutes, so anything reached THROUGH that lock
-- is reached only once the build is over — and a reader waiting on that build is waiting for exactly
-- the answer they cannot then be given. So what the service is working on sits beside the lock, and
-- this holds the lock and asks anyway.
--
-- Timed out rather than left to hang, so that moving the report back under the lock fails this case
-- by name instead of stopping the suite.
testProgressUnderTheLock :: TestTree
testProgressUnderTheLock = testCase "what the service is working on is answered while it is working" $ do
  ctss@(CTSS ssmv _) <- emptyCTSS testCtssConfig
  held <- newEmptyMVar
  release <- newEmptyMVar
  void $ forkIO $ modifyMVar_ ssmv $ \d -> do
    ctssReport ctss StepBuilding 3 7 (Just "a.ct")
    putMVar held ()
    takeMVar release
    return d
  takeMVar held
  answered <- timeout 2000000 $ ctssProgress ctss
  putMVar release ()
  case answered of
    Nothing -> assertFailure "what the service is working on went behind the lock it is reporting on"
    Just p -> assertEqual "the service did not say what it was working on"
      (StepBuilding, 3, 7, Just "a.ct") (prStep p, prDone p, prTotal p, prAt p)

-- | A read says how much of itself it has done, and never says more than it has.
--
-- The counting is asserted page by page over the pieces a read is made of — the same queue and the
-- same walk 'ctssReadOf' drives — because what is being asserted is what a read says PART WAY
-- through itself, and a read asked for from outside is over by the time it answers. This is the sum
-- 'ctssReadOf' publishes: what it has taken up, and what 'rqLeft' says is still queued.
--
-- The total grows as the walk reaches more pages, so it is not asserted to be any particular number.
-- What is asserted is what makes a count worth putting in front of a reader: it only goes forwards,
-- it never claims more done than there is to do, and it ends having done all of it.
testProgressCountsARead :: TestTree
testProgressCountsARead = testCase "a read says how much of itself it has done" $ do
  dir <- caseDir "read-progress"
  now <- getCurrentTime
  writeLater now (dir </> "a.ct") "import \"b.ct\"\nimport \"c.ct\"\na = 1\n"
  writeLater now (dir </> "b.ct") "import \"c.ct\"\nb = 2\n"
  writeLater now (dir </> "c.ct") "c = 3\n"
  reading <- newReading (ctssCachePath testCtssConfig) noImporters
  queue <- newReadQueue
  rqRestart queue =<< mapM (canonicalImport testCtssConfig (rootImportCtx noImporters)) [mkRawImportStr (dir </> "a.ct")]
  steps <- newIORef []
  readSoFar <- newIORef (0 :: Int)
  readQueued testCtssConfig reading S.empty H.empty queue $ \_ _ -> do
    modifyIORef' readSoFar (+ 1)
    done <- readIORef readSoFar
    left <- rqLeft queue
    modifyIORef' steps ((done, done + left) :)
  finishReading reading
  counts <- reverse <$> readIORef steps
  -- At least the three pages of this case. Not exactly three: a page imports the standard library,
  -- which the read reaches as well, and the counting being about the whole read rather than about
  -- the pages one case wrote is the point of it.
  assertBool "the read did not reach the pages of this case" (length counts >= 3)
  assertEqual "a read said it had done more than there was to do"
    [] [(d, t) | (d, t) <- counts, d > t]
  assertEqual "what a read had done went backwards"
    [] [(a, b) | (a, b) <- zip (map fst counts) (drop 1 $ map fst counts), b < a]
  assertEqual "a read did not end having read everything it reached"
    (Just True) (uncurry (==) <$> listToMaybe (reverse counts))

-- | A read of the service publishes that count as it goes.
--
-- The half 'testProgressCountsARead' cannot assert: that the sum it checks is one the service
-- actually says, rather than one only its own case computes. Asserted after the read, which is when
-- a read asked for from outside can be asked about — what it said part way through is the case
-- above.
testProgressSaysWhatARead :: TestTree
testProgressSaysWhatARead = testCase "a read of the service says what it read" $ do
  dir <- caseDir "read-progress-said"
  now <- getCurrentTime
  writeLater now (dir </> "a.ct") "a = 1\n"
  ctss <- ctssBaseFiles testCtssConfig [dir </> "a.ct"]
  p <- ctssProgress ctss
  assertEqual "a read did not say it was reading" StepReading (prStep p)
  assertBool "a read said it had read no pages" (prDone p > 0)
  assertEqual "a read did not end having read everything it reached" (prDone p) (prTotal p)
  -- Nobody was waiting on it. A read the service made for itself is not a reader's wait to report on,
  -- which is what keeps a bar off the screen of a reader who is not waiting for anything.
  assertEqual "a read nobody asked for said somebody was waiting" False (prWorking p)

-- | A service nobody is waiting at says so, whatever it was last doing.
--
-- The step is the last thing said rather than a thing being done, so a build that ended leaves
-- "compiled 7 of 7 pages" behind it. A reader shown that while nothing was happening would be told
-- something untrue about the present, which is what 'prWorking' is for and why it is bracketed: an
-- answer sent, a compile throwing and a connection dropping all have to take it back down.
testProgressIdleWhenNothingIsWorking :: TestTree
testProgressIdleWhenNothingIsWorking = testCase "a service nobody is waiting at says nothing is happening" $ do
  ctss <- emptyCTSS testCtssConfig
  fresh <- ctssProgress ctss
  assertEqual "a service that has done nothing said something was happening" (False, StepIdle) (prWorking fresh, prStep fresh)
  inside <- ctssWorking ctss $ do
    ctssReport ctss StepBuilding 1 2 (Just "a.ct")
    ctssProgress ctss
  assertEqual "a service working for a reader did not say so" (True, StepBuilding) (prWorking inside, prStep inside)
  answered <- ctssProgress ctss
  assertEqual "a service left the last thing it did behind it" (False, StepIdle) (prWorking answered, prStep answered)
  thrown <- try $ ctssWorking ctss $ do
    ctssReport ctss StepBuilding 1 2 (Just "a.ct")
    fail "a compile that threw"
  assertBool "the failure did not happen" (either (const True) (const False) (thrown :: Either SomeException ()))
  afterThrow <- ctssProgress ctss
  assertEqual "a compile that threw left the service saying it was still working"
    (False, StepIdle) (prWorking afterThrow, prStep afterThrow)

-- | The first of two readers being answered is not the service falling idle.
--
-- Which is why what is working is counted rather than flagged. Two readers waiting at once is the
-- ordinary case for a doc server — a page and the navigation beside it are two requests — and a flag
-- would have the first of them to be answered tell the second that nothing was happening.
testProgressCountsItsReaders :: TestTree
testProgressCountsItsReaders = testCase "one reader being answered is not the service falling idle" $ do
  ctss <- emptyCTSS testCtssConfig
  still <- ctssWorking ctss $ do
    ctssReport ctss StepBuilding 1 2 (Just "a.ct")
    _ <- ctssWorking ctss $ return ()
    ctssProgress ctss
  assertEqual "the service fell idle while a reader was still waiting"
    (True, StepBuilding) (prWorking still, prStep still)
  bothAnswered <- ctssProgress ctss
  assertEqual "the service did not fall idle once every reader had been answered"
    (False, StepIdle) (prWorking bothAnswered, prStep bothAnswered)

-- | Which day the service reads @today@ as.
--
-- A note may write a query saying @due before tomorrow@, and what that asks for is decided by the
-- day it is asked on. A service configured with a day answers with that one, so a test written over
-- such a query asks one thing forever; a service configured with none reads the clock, which is what
-- a doc server does so that a query asked tomorrow is tomorrow's question.
testServiceToday :: TestTree
testServiceToday = testCase "the day a service reads today as is the day it was given" $ do
  pinned <- serviceToday =<< emptyCTSS testCtssConfig
  assertEqual "a service given a day did not answer with it" (fromGregorian 2026 1 3) pinned
  clock <- utctDay <$> getCurrentTime
  read' <- serviceToday =<< emptyCTSS testCtssConfig{ctssToday = Nothing}
  assertEqual "a service given no day did not read the clock" clock read'

liveReloadTests :: TestTree
liveReloadTests = testGroup "LiveReload" [
  testImporters,
  testPageHeldWhileReading,
  testProgressUnderTheLock,
  testProgressCountsARead,
  testProgressSaysWhatARead,
  testProgressIdleWhenNothingIsWorking,
  testProgressCountsItsReaders,
  testUnchangedText,
  testPageAdded,
  testFollowCloses,
  testWatchLive,
  testBrokenPageLeftOut,
  testAnnotsFollowTheirAsks,
  testAskServiceForcesItsAnswer,
  testAnnotsFollowThePages,
  testServiceToday
                                         ]
