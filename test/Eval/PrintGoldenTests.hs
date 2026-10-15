-- | Tests of what the test engine derives from a page, and of its verdict on a print golden.
--
-- These are derived tests, which the integration fixtures run but never read the verdict of: a
-- fixture fails when one of its tests fails, which cannot say that a wrong golden was REFUSED or
-- that a test was not derived at all. So the verdicts are asserted here directly.
module Eval.PrintGoldenTests (printGoldenTests) where

import           CRes
import           CtService        (CTSS, ctssBaseFiles, getTestResultsOf)
import           Data.List        (isInfixOf, sortOn)
import           Data.Maybe       (fromMaybe)
import           Eval.Common      (TestKind (..), TestResult (..))
import           Eval.Testing     (runTestDesc)
import           Semantics        (testCtssConfig)
import           Syntax.Parsers   (mkDesCanonicalImportStr)
import           Test.Tasty
import           Test.Tasty.HUnit

printGoldenTests :: TestTree
printGoldenTests = withResource fixtures (const $ return ()) $ \getCtss -> testGroup "PrintGolden"
  [ testCase "a print is checked against its golden, and told when it has none" $ do
      results <- testResultsOf getCtss printGoldenPage
      -- In the order the prints were written, which their names carry as the source range.
      let prints = sortOn trName [r | r <- results, trKind r == TestPrintGolden]
      assertEqual "one passes, two fail" [True, False, False] (map trPassed prints)
      let notesOf r = fromMaybe "" (trNotes r)
      assertContains "the mismatch names the golden" "expected: some other text" (notesOf (prints !! 1))
      assertContains "and what the print rendered to" "actual:   differs from its golden" (notesOf (prints !! 1))
      assertContains "a print with no golden says how to write one" "catln test --update" (notesOf (prints !! 2))
  -- The branches of an @if@ are arms of a generated function with disjoint domains, so no
  -- agreement test pairs them. A gap once bounded both arms at ⊤ and derived a test asserting that
  -- the @if@ must not branch; the fixture holds the branching, and this holds the derivation.
  , testCase "an if whose branches differ derives no agreement test" $ do
      results <- testResultsOf getCtss ifPage
      assertEqual "no agreement test is derived" [] [trName r | r <- results, trKind r == TestOverlapAgreement]
  ]
  where
    printGoldenPage = "test/Eval/code/printGolden.ct"
    ifPage = "test/Integration/code/ifNonAgreeingBranches.ct"
    -- One service for both pages, so the library they import is built once rather than once per
    -- case.
    fixtures = ctssBaseFiles testCtssConfig [printGoldenPage, ifPage] :: IO CTSS
    -- The tests of the page alone, as the integration runner derives them.
    testResultsOf getCtss file = do
      ctss <- getCtss
      name <- mkDesCanonicalImportStr testCtssConfig file
      res <- runCResT $ getTestResultsOf name ctss
      case res of
        CErr notes   -> assertFailure ("Could not load " ++ file ++ ":\n" ++ prettyCNotes notes) >> return []
        CRes _ descs -> mapM runTestDesc descs
    assertContains msg needle haystack = assertBool (msg ++ ": expected " ++ show needle ++ " in\n" ++ haystack) (needle `isInfixOf` haystack)
