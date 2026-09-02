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

import           CRes                (CRes (..), prettyCNotes, runCResT)
import           CtService           (CTSS, ctssBaseFiles, ctssBuildFrom,
                                      ctssCached, ctssEvaluableFrom, ctssLoad,
                                      getSource)
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

-- | The whole deferral loop, driven end to end against a real service.
--
-- A @native@ import can be named to the compiler directly, because a base file name is read exactly
-- as an @import@ payload is ('Syntax.Parsers.mkRawImportStr'). So the loop can be exercised now,
-- before anything resolves an ordinary import to an importer: the service is started on a native
-- page and has to work out on its own that it must build the importer first.
--
-- What has to happen, in order, is the whole of what this commit added. The read reaches the page
-- and cannot read it, because the importer's page is not built. It leaves the page unread, naming
-- what it waits on. 'ctssLoadOf' sees that, builds the importer OUTSIDE the read, and asks for the
-- page again. The second read runs the importer and the page is read. If any of those is missing the
-- page stays unread and this fails.
testDeferredPageIsEventuallyRead :: TestTree
testDeferredPageIsEventuallyRead = testCase "a page waits for its importer, which is then built" $ do
  let imp = concat ["native(\"read-me.py\", importer= \"", importerPage, "\", fn= \"mkResult\")"]
  ctss <- ctssBaseFiles testCtssConfig [imp] :: IO CTSS
  name <- mkDesCanonicalImportStr testCtssConfig imp

  -- Asking for the page's program is what drives the loop; nothing before this has run an importer.
  ctssLoad ctss [name]

  raw <- ctssCached ssfRaw name ctss
  case raw of
    Nothing -> assertFailure "the page was never read, so the importer was never built and run"
    Just (CErr notes) -> assertFailure $ "the page failed to read: " ++ prettyCNotes notes
    Just (CRes _ _) -> return ()

  -- The importer echoes the path it was given back as the page's source, so this says the page was
  -- read by THAT importer over THAT file rather than by anything else.
  src <- runCResT $ getSource ctss name
  case src of
    CErr notes -> assertFailure $ "the page has no source: " ++ prettyCNotes notes
    CRes _ s -> assertEqual "the importer was run over the file the import named" "read-me.py" s

importerCallTests :: TestTree
importerCallTests = testGroup "ImporterCall" [
  testCallTowardDeclaredType,
  testDeferredPageIsEventuallyRead
                                             ]
