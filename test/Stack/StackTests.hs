module Stack.StackTests where

import           CRes
import           CtService
import           Semantics         (testCtssConfig)
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
    makeTest (name, testAction) = testCase name $ do
      result <- runCResT testAction
      case result of
        CErr notes -> assertFailure $ prettyCNotes notes
        CRes _ _   -> return ()
