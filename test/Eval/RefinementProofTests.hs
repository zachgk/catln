-- | Tests for the proofs of @#proof@ refinements ('TypeCheck.Proof').
--
-- The proofs are derived tests, which the integration tests never run, so their pass or fail is
-- asserted here directly: a proof that passes when its claim is wrong, or fails when it is right,
-- is the bug this guards against.
module Eval.RefinementProofTests where

import           CRes
import           CtService
import           Data.List        (isInfixOf, sort)
import           Data.Maybe       (fromMaybe)
import           Eval.Common      (TestKind (..), TestResult (..))
import           Eval.Testing     (runTestDesc)
import           Semantics        (testCtssConfig)
import           Test.Tasty
import           Test.Tasty.HUnit

refinementProofTests :: TestTree
refinementProofTests = testGroup "RefinementProof"
  [ testCase "each overlapping definition is proven or refuted" $ do
      results <- testResultsOf "test/Eval/code/refinementProof.ct"
      let proofs = [r | r <- results, trKind r == TestRefinementProof]
      assertEqual "which proofs passed"
        (sort [ ("/collapse", True), ("/collapse", True)
              , ("/keep", True)
              , ("/grow", False), ("/grow", True)
              , ("/keep2", False)])
        (sort [(trOwner r, trPassed r) | r <- proofs])
      assertEqual "a refinement to prove is not also property tested" []
        [trName r | r <- results, trKind r == TestRefinementAgreement]
      let notesOf owner = concat [fromMaybe "" (trNotes r) | r <- proofs, trOwner r == owner, not (trPassed r)]
      assertContains "the grow failure names the overlap" "on the input /grow(/n=/Z)" (notesOf "/grow")
      assertContains "the grow failure names the result" "Unproven part: /S(/n=/Z)" (notesOf "/grow")
      assertContains "the keep2 failure names the narrowed result" "Unproven part: /S(/n=∀/Nat)" (notesOf "/keep2")
  , testCase "a refinement of a generic operator leaves its arms inside the declaration" $ do
      results <- testResultsOf "test/Eval/code/refinementGenericOperator/refinementGenericOperator.ct"
      assertEqual "both arms are proven"
        [(TestRefinementProof, True), (TestRefinementProof, True)]
        [(trKind r, trPassed r) | r <- results]
  , testCase "#proof without #refine is an error" $ do
      ctss <- ctssBaseFiles testCtssConfig ["test/Eval/code/proofWithoutRefine/proofWithoutRefine.ct"]
      res <- runCResT $ getTestResults ctss
      case res of
        CRes _ _   -> assertFailure "a #proof on a plain declaration was accepted"
        CErr notes -> assertContains "the error says why" "which is not a #refine declaration" (prettyCNotes notes)
  ]
  where
    testResultsOf file = do
      ctss <- ctssBaseFiles testCtssConfig [file]
      res <- runCResT $ getTestResults ctss
      case res of
        CErr notes     -> assertFailure ("Could not load " ++ file ++ ":\n" ++ prettyCNotes notes) >> return []
        CRes _ descs   -> mapM runTestDesc descs
    assertContains msg needle haystack = assertBool (msg ++ ": expected " ++ show needle ++ " in\n" ++ haystack) (needle `isInfixOf` haystack)
