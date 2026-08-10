module Stack.StackTests where

import           Control.Concurrent.MVar (readMVar)
import           Control.Monad           (unless)
import           CRes
import           CtService
import           Data.Maybe              (fromMaybe)
import           Eval.Common             (TestResult (..))
import           Eval.Testing            (TestDesc (..), runTestDesc)
import           Semantics               (testCtssConfig)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Utils                   (graphToNodes)

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
    -- Reading files which have not changed must leave every already-built page built. A read
    -- compares the program it read against the one the page holds, and until meta ids were made
    -- deterministic that comparison never matched, so any read (a webdocs save makes one) threw
    -- away every page and rebuilt the whole library.
    --
    -- Run here rather than as a case of its own so it cannot race the tests tasty runs in parallel,
    -- which share this service.
    testReread ctss@(CTSS ssmv) = do
      let builtPages = do
            CTSSDat{ctssData} <- readMVar ssmv
            return [name | (ssf, name, _) <- graphToNodes ctssData, ssfBuilt ssf]
      beforeRead <- builtPages
      ctssRead ctss
      afterRead <- builtPages
      return $ testCase "reading unchanged files keeps every page built" $ do
        assertBool "no page was built to begin with, so this proves nothing" (not $ null beforeRead)
        assertEqual "a read of unchanged files dropped built pages" beforeRead afterRead

    -- The tests are run one tasty case at a time rather than all at once up front, so each keeps its
    -- own name, runs under tasty's parallelism, and is skipped outright by @--pattern@.
    makeTest td = testCase (tdName td) $ do
      result <- runTestDesc td
      unless (trPassed result) $ assertFailure $ fromMaybe "Failed" (trNotes result)
