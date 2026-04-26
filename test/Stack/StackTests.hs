module Stack.StackTests where

import           Control.Exception (SomeException, try)
import           CRes
import           CtService
import qualified Data.Set          as Set
import           Semantics         (testCtssConfig)
import           Test.Tasty
import           Test.Tasty.HUnit

-- These tests fail because unqualified 'mempty' and 'inverse' resolve to both
-- /Data/Algebra/<name> and /Data/Primitive/<name>, causing a getSingleton error.
-- Remove entries when the type resolution ambiguity is fixed.
knownWIP :: Set.Set String
knownWIP = Set.fromList
  [ "/Data/Algebra/testRightMonoidIdentity"
  , "/Data/Algebra/testLeftMonoidIdentity"
  , "/Data/Algebra/testGroup"
  ]

tryRunTest :: IO (CRes a) -> IO (Either SomeException (CRes a))
tryRunTest = try

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
    makeTest (name, testAction)
      | name `Set.member` knownWIP = testCase name $ do
          r <- tryRunTest (runCResT testAction)
          case r of
            Left _           -> return ()
            Right (CErr _)   -> return ()
            Right (CRes _ _) -> assertFailure $
              name ++ " unexpectedly passed; remove from knownWIP in StackTests.hs"
      | otherwise = testCase name $ do
          result <- runCResT testAction
          case result of
            CErr notes -> assertFailure $ prettyCNotes notes
            CRes _ _   -> return ()
