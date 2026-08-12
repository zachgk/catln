module Stack.StackTests where

import           Control.Concurrent.MVar (readMVar)
import           Control.Monad           (unless)
import           CRes
import           CtService
import           Data.Maybe              (fromMaybe, isJust)
import           Eval.Common             (TestResult (..))
import           Eval.Testing            (TestDesc (..), runTestDesc)
import           Semantics               (testCtssConfig)
import           Syntax.Parsers          (ReadPage (..))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Utils                   (graphToNodes, snd3)

stackTests :: IO TestTree
stackTests = do
  ctss <- ctssBaseFiles testCtssConfig ["stack"]
  res <- runCResT $ getTestResults ctss
  rereadTest <- testReread ctss
  case res of
    CErr notes ->
      return $ testCase "load stack library" $
        assertFailure $ "Failed to load stack library:\n" ++ prettyCNotes notes
    CRes _ testDescs ->
      return $ testGroup "Stack" $ rereadTest : map makeTest testDescs
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
    testReread ctss@(CTSS ssmv) = do
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

    -- The tests are run one tasty case at a time rather than all at once up front, so each keeps its
    -- own name, runs under tasty's parallelism, and is skipped outright by @--pattern@.
    makeTest td = testCase (tdName td) $ do
      result <- runTestDesc td
      unless (trPassed result) $ assertFailure $ fromMaybe "Failed" (trNotes result)
