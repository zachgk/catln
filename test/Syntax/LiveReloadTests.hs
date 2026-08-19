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

import           Control.Concurrent     (modifyMVar_, threadDelay)
import           Control.Concurrent.STM (STM, atomically, orElse)
import           Control.Exception      (SomeException, try)
import           CRes                   (CNote (..), CNoteI (..), CRes (..),
                                         runCResT)
import           CtService              (CTSS (..), CTSSDat (..),
                                         ReadChange (..), Shown (..),
                                         changeReaches, ctssBaseFiles,
                                         ctssCached, ctssEvaluableFrom,
                                         ctssFollow, ctssFollowing, ctssGetFrom,
                                         ctssKeys, ctssReadOf, ctssWatch,
                                         ctssWatchLive)
import           Data.List              (isPrefixOf, sort)
import           Data.Maybe             (isJust, isNothing)
import           Data.Time.Clock        (addUTCTime, getCurrentTime)
import           Eval.Common            (SSF (..))
import           Semantics              (testCtssConfig)
import           Semantics.Prgm         (FileImport, impDisp)
import           Syntax.RereadTests     (caseDir, writeLater)
import           System.Directory       (setModificationTime)
import           System.FilePath        ((</>))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Utils                  (fmapGraphWithKey, graphToNodes)

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
seedBuilt (CTSS ssmv) f = modifyMVar_ ssmv $ \d -> return d{ctssData = fmapGraphWithKey f (ctssData d)}

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

liveReloadTests :: TestTree
liveReloadTests = testGroup "LiveReload" [
  testImporters,
  testUnchangedText,
  testPageAdded,
  testFollowCloses,
  testWatchLive,
  testBrokenPageLeftOut,
  testAnnotsFollowThePages
                                         ]
