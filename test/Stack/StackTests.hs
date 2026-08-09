module Stack.StackTests where

import           Control.Monad    (unless)
import           CRes
import           CtService
import           Data.Maybe       (fromMaybe)
import           Eval.Common      (TestResult (..))
import           Eval.Testing     (TestDesc (..), runTestDesc)
import           Semantics        (testCtssConfig)
import           Test.Tasty
import           Test.Tasty.HUnit

stackTests :: IO TestTree
stackTests = do
  ctss <- ctssBaseFiles testCtssConfig ["stack"]
  res <- runCResT $ getTestResults ctss
  case res of
    CErr notes ->
      return $ testCase "load stack library" $
        assertFailure $ "Failed to load stack library:\n" ++ prettyCNotes notes
    CRes _ testDescs ->
      return $ testGroup "Stack" $ map makeTest testDescs
  where
    -- The tests are run one tasty case at a time rather than all at once up front, so each keeps its
    -- own name, runs under tasty's parallelism, and is skipped outright by @--pattern@.
    makeTest td = testCase (tdName td) $ do
      result <- runTestDesc td
      unless (trPassed result) $ assertFailure $ fromMaybe "Failed" (trNotes result)
