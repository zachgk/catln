-- | Tests for arrow quantification ('Semantics.Prgm.omiQuant'): how the derived tests and the proofs
-- of a function marked @#exist@ differ from those of a unique one.
--
-- These are derived tests, which the integration tests never run, so their pass or fail is asserted
-- here directly.
module Eval.QuantificationTests where

import           CRes
import           CtService
import           Data.List        (isInfixOf, sort)
import           Data.Maybe       (fromMaybe)
import           Eval.Common      (TestKind (..), TestResult (..))
import           Eval.Testing     (runTestDesc)
import           Semantics        (testCtssConfig)
import           Test.Tasty
import           Test.Tasty.HUnit

quantificationTests :: TestTree
quantificationTests = testGroup "Quantification"
  [ testCase "an existential refinement holds when some definition establishes it" $ do
      results <- testResultsOf "test/Eval/code/existQuant.ct"
      assertEqual "which proofs passed"
        (sort [ ("/choose", True)
              , ("/chooseU", False), ("/chooseU", True)
              , ("/cover", False)])
        (sort [(trOwner r, trPassed r) | r <- results, trKind r == TestRefinementProof])
      -- Both unique tests fail, the one for @pickU(n)= n@ included: a unique function's refinement
      -- test evaluates the definition's input through dispatch, which takes the other arm on a Z.
      assertEqual "which refinement tests passed"
        (sort [("/pick", True), ("/pickU", False), ("/pickU", False)])
        (sort [(trOwner r, trPassed r) | r <- results, trKind r == TestRefinementAgreement])
      let notesOf owner = concat [fromMaybe "" (trNotes r) | r <- results, trOwner r == owner, not (trPassed r)]
      assertContains "the cover failure names what was proven" "Proven input: /cover(/n=/Z)" (notesOf "/cover")
      assertContains "the cover failure names what is uncovered" "Uncovered input: /cover(/n=∀/Nat - /Z)" (notesOf "/cover")
  , testCase "an existential function's overlapping definitions are not tested for agreement" $ do
      results <- testResultsOf "test/Eval/code/existOverlap/existOverlap.ct"
      let mine = [r | r <- results, trOwner r `elem` ["/nudge", "/nudgeU"]]
      assertEqual "only the unique function derives an agreement test, and it fails"
        [("/nudgeU", TestOverlapAgreement, False)]
        [(trOwner r, trKind r, trPassed r) | r <- mine]
  , testCase "#exist on a definition is an error" $ do
      ctss <- ctssBaseFiles testCtssConfig ["test/Eval/code/existOnDefinition/existOnDefinition.ct"]
      res <- runCResT $ getTestResults ctss
      case res of
        CRes _ _   -> assertFailure "an #exist on a definition was accepted"
        CErr notes -> assertContains "the error says why" "which is not a declaration" (prettyCNotes notes)
  ]
  where
    testResultsOf file = do
      ctss <- ctssBaseFiles testCtssConfig [file]
      res <- runCResT $ getTestResults ctss
      case res of
        CErr notes   -> assertFailure ("Could not load " ++ file ++ ":\n" ++ prettyCNotes notes) >> return []
        CRes _ descs -> mapM runTestDesc descs
    assertContains msg needle haystack = assertBool (msg ++ ": expected " ++ show needle ++ " in\n" ++ haystack) (needle `isInfixOf` haystack)
