module Stack.StackTests where

import           Control.Concurrent.MVar (readMVar)
import           Control.Monad           (unless)
import           CRes
import           CtService
import qualified Data.HashSet            as S
import           Data.Maybe              (fromMaybe, isJust)
import           Eval.Common             (SSF (..), TestResult (..))
import           Eval.Testing            (TestDesc (..), runTestDesc)
import           Semantics               (testCtssConfig)
import           Syntax.Ct.Prgm          (ReadPage (..))
import           Syntax.Parsers          (mkDesCanonicalImportStr)
import           Test.Tasty
import           Test.Tasty.HUnit
import           TypeCheck.Instruments   (epochInstruments)
import           Utils                   (graphToNodes, snd3)

-- | The library's own tests, run in the library's service ('TestService.libraryService').
stackTests :: CTSS -> IO TestTree
stackTests lib = do
  res <- runCResT $ getTestResults lib
  rereadTest <- testReread lib
  seededTest <- testSeeded lib
  case res of
    CErr notes ->
      return $ testCase "load stack library" $
        assertFailure $ "Failed to load stack library:\n" ++ prettyCNotes notes
    CRes _ testDescs ->
      return $ testGroup "Stack" $ rereadTest : seededTest : map makeTest testDescs
  where
    -- Reading files which have not changed must read no page again, and so must leave every
    -- already-built page built. A read compares the program it read against the one the page holds,
    -- and until meta ids were made deterministic that comparison never matched, so any read (a
    -- webdocs save makes one) threw away every page and rebuilt the whole library.
    --
    -- The read times are what say no page was parsed again: a page which is read again is timed
    -- afresh before it is parsed, so times which came back unchanged are the ones the page was
    -- handed back with. Pages still being built only says nothing was thrown away, which was also
    -- true when every page was parsed again and compared equal.
    --
    -- Run here rather than as a case of its own so it cannot race the tests tasty runs in parallel,
    -- which share this service.
    testReread ctss@(CTSS ssmv _) = do
      let pages = do
            CTSSDat{ctssData} <- readMVar ssmv
            return [(name, ssfBuilt ssf, rpReads (ssfRead ssf)) | (ssf, name, _) <- graphToNodes ctssData]
      beforeRead <- pages
      ctssRead ctss
      afterRead <- pages
      return $ testCase "reading unchanged files reads no page again" $ do
        assertBool "no page was built to begin with, so this proves nothing" (any snd3 beforeRead)
        assertBool "no page was timed to begin with, so a re-read could not be told from a page given back"
          (all (\(_, _, times) -> isJust times) beforeRead)
        assertEqual "a read of unchanged files did not leave the pages as they were" beforeRead afterRead

    -- A service started from the library ('ctssBaseFilesFrom') builds none of the library again: a
    -- page importing core reaches all of it, so this is what lets every test's service start from it
    -- rather than spend most of its time building it. Watched by the epoch count, which records one
    -- entry for each component a build typechecks, so a library page built again would be named in
    -- it. The pages being built already before the build is the other half: what the read handed
    -- back is the library's pages, not ones read afresh under another name.
    --
    -- Run here for the reason the reread is: it reads the library service, which tasty's cases share.
    testSeeded ctss@(CTSS ssmv _) = do
      let page = "test/Typecheck/unit/foldDeclaredResult.ct"
      seeded@(CTSS seededv _) <- ctssBaseFilesFrom ctss testCtssConfig [page]
      CTSSDat{ctssData=libData} <- readMVar ssmv
      CTSSDat{ctssData=seededData} <- readMVar seededv
      let library = S.fromList [name | (_, name, _) <- graphToNodes libData]
      let reachedLibrary = [ssfBuilt ssf | (ssf, name, _) <- graphToNodes seededData, name `S.member` library]
      ctssSetInstrument seeded (const epochInstruments)
      name <- mkDesCanonicalImportStr testCtssConfig page
      ctssBuildFrom seeded name
      built <- map fst <$> ctssEpochCounts seeded
      return $ testCase "a service started from the library builds none of it again" $ do
        assertBool "the page reaches none of the library, so this proves nothing" (not $ null reachedLibrary)
        assertBool "a library page was handed over unbuilt" (and reachedLibrary)
        assertEqual "the components typechecked" [name] built

    -- The tests are run one tasty case at a time rather than all at once up front, so each keeps its
    -- own name, runs under tasty's parallelism, and is skipped outright by @--pattern@.
    makeTest td = testCase (tdName td) $ do
      result <- runTestDesc td
      unless (trPassed result) $ assertFailure $ fromMaybe "Failed" (trNotes result)
