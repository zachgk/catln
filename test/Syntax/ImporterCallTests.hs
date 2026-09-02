-- | Whether the compiler can call a declaration shaped like an importer and get a value back.
--
-- This is the one question the whole design rests on and the one that could not be answered by
-- reading: 'Eval.dshowDestType' says, three times over, that a tree built toward an expression's own
-- type comes back as the unevaluated application, and an importer's declared return type IS what a
-- caller wants of it. Every existing caller sidesteps this by building toward @DShow@, which is a
-- display class and not something the compiler should have to make a program's result belong to in
-- order to run it.
--
-- So the cases below call one declaration two ways and compare. What they establish is which
-- destination 'Eval.evalCallTo' must be given, which is what the service's invocation is written
-- against.
module Syntax.ImporterCallTests where

import           CRes                (CRes (..), runCResT)
import           CtService           (CTSS, ctssBaseFiles, ctssBuildFrom,
                                      ctssEvaluableFrom)
import qualified Data.HashMap.Strict as H
import           Eval                (evalBaseEnv, evalCallTo,
                                      prgmFromGraphData)
import           Eval.Common         (SSF (..), Val)
import           Semantics           (testCtssConfig)
import           Semantics.Types     (Type)
import           Semantics.Val       hiding (Val)
import           Syntax.Ct.Prgm      (importResultType)
import           Syntax.Parsers      (mkDesCanonicalImportStr)
import           Test.Tasty
import           Test.Tasty.HUnit

-- | The fixture page: a declaration of an importer's shape, and nothing marking it as one.
importerPage :: FilePath
importerPage = "test/Integration/code/artifacts/native/importer.ct"


-- | Build the fixture and call the declaration on it, toward the destination given.
callWith :: Type -> String -> IO (CRes Val)
callWith dest path = do
  ctss <- ctssBaseFiles testCtssConfig [importerPage] :: IO CTSS
  name <- mkDesCanonicalImportStr testCtssConfig importerPage
  ctssBuildFrom ctss name
  built <- runCResT $ ctssEvaluableFrom ssfTPrgm (Just name) ctss
  return $ case built of
    CErr notes -> CErr notes
    CRes _ graph -> do
      prgm <- prgmFromGraphData name graph
      evalCallTo (evalBaseEnv prgm) dest "/Spike/mkResult" [("path", StrVal path)]

-- | Building toward the declared return type is what a caller would reach for first.
--
-- Whatever this answers is the finding: if it comes back as an evaluated tuple then the destination
-- can simply be the declared type, and if it does not then the invocation needs another one. It is
-- asserted rather than merely printed so that the answer is pinned — a later change to the tree
-- builder which made the other case true would be a change to what an importer's call must be
-- written as, and this is where that would be noticed.
testCallTowardDeclaredType :: TestTree
testCallTowardDeclaredType = testCase "calling toward the declared return type" $ do
  res <- callWith importResultType "spike.py"
  case res of
    CErr notes -> assertFailure $ "the call did not build: " ++ show notes
    CRes _ v -> case v of
      TupleVal n args -> do
        assertEqual "it is the result the declaration answers with" "/Catln/ImportResult" n
        assertBool "and its arguments were evaluated" (H.member "/source" args)
      other -> assertFailure $ "the call came back unevaluated: " ++ show other

importerCallTests :: TestTree
importerCallTests = testGroup "ImporterCall" [testCallTowardDeclaredType]
