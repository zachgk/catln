-- | Tests for the runtime of a refinement the type checker could not decide ('Eval.valWithinType').
--
-- A value predicate bottoming out at a @#runtime@ primitive (@Integer_nonNeg@'s @>=@) is undecided
-- on every value by the type graph, so it is evaluated on the value where it is held to the
-- refinement: an argument at the refined arm's dispatch, a result where it is returned, a value
-- narrowed by a condition where the condition is written. The runs where the predicate holds are
-- integration tests (@refineRuntime.ct@); the runs where it does not hold are asserted here, since
-- a run that fails is what they are.
module Eval.RefinementRuntimeTests where

import           CRes
import           CtService
import           Data.List        (isInfixOf)
import           Eval.Common      (newMockIO)
import           Semantics        (testCtssConfig)
import           Syntax.Parsers   (mkDesCanonicalImportStr)
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestService      (MkService)

refinementRuntimeTests :: MkService -> TestTree
refinementRuntimeTests mk = withResource fixtures (const $ return ()) $ \getCtss -> testGroup "RefinementRuntime"
  [ failsWith getCtss "an argument outside its refinement fails the refined arm's dispatch" "argOutside.ct" "is outside the refinement"
  , failsWith getCtss "a result outside its declared refinement fails as it is returned" "resultOutside.ct" "is outside the refinement"
  , failsWith getCtss "a value not satisfying its condition fails where the condition is written" "whereOutside.ct" "does not satisfy its condition"
  , failsWith getCtss "a result outside the refinement written over $out fails as it is returned" "outOutside.ct" "is outside the refinement"
  ]
  where
    -- One service for every fixture, so the library they import is built once.
    fixtures = mk (map fixture ["argOutside.ct", "resultOutside.ct", "whereOutside.ct", "outOutside.ct"]) :: IO CTSS
    fixture name = "test/Integration/code/artifacts/refineRuntime/" ++ name
    failsWith getCtss title page needle = testCase title $ do
      ctss <- getCtss
      name <- mkDesCanonicalImportStr testCtssConfig (fixture page)
      ctssBuildFrom ctss name
      mockIO <- newMockIO
      res <- runCResT $ getEvaluatedMock ctss mockIO name "main"
      case res of
        CRes _ code -> assertFailure $ "the run was expected to fail its refinement, but exited " ++ show code
        CErr notes  -> assertBool ("the failure names the refinement: expected " ++ show needle ++ " in\n" ++ prettyCNotes notes) (needle `isInfixOf` prettyCNotes notes)
