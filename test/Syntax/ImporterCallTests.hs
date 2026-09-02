-- | The compiler finding an importer, calling it, and reading a page with it.
--
-- Three things the design rests on, each of which had to be established rather than assumed. That an
-- importer can be FOUND by reading its page and not by compiling it, which is what the @#importer@
-- annotation is for. That the compiler can CALL such a declaration and get an evaluated value back,
-- which the documentation on 'Eval.dshowDestType' suggests it cannot -- it says three times that a
-- tree built toward an expression's own type comes back unevaluated, and every other caller
-- sidesteps that by building toward @DShow@. And that a page whose importer is not built yet WAITS
-- for it, gets it built, and is then read.
--
-- Driven against a real service rather than a stub, which is what has made these worth having: every
-- defect they found lay between two pieces that each passed their own tests.
module Syntax.ImporterCallTests where

import           CRes                (CRes (..), prettyCNotes, runCResT)
import           CtService           (CTSS, ctssBaseFiles, ctssBuildFrom,
                                      ctssCached, ctssEvaluableFrom,
                                      ctssImporterEnv, ctssLoad, getSource)
import qualified Data.HashMap.Strict as H
import           Eval                (evalBaseEnv, evalCallTo,
                                      prgmFromGraphData)
import           Eval.Common         (SSF (..), Val)
import           Semantics           (testCtssConfig)
import           Semantics.Types     (Type)
import           Semantics.Val       hiding (Val)
import           Syntax.Ct.Prgm      (ImporterDecl (..), ImporterEnv (..),
                                      importResultType)
import           Syntax.Parsers      (mkDesCanonicalImportStr)
import           Test.Tasty
import           Test.Tasty.HUnit

-- | The fixture page, which declares one importer and does nothing with the file it is given.
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

-- | An importer is found by reading the page, and not by compiling it.
--
-- That is what the @#importer@ annotation buys and it is the reason it is an annotation. Which
-- importer reads a file has to be decided while a page's imports are being resolved, which is before
-- anything has been desugared — so the case asks for the declaration from a service that has read
-- the page and has NOT been told to build anything.
testImporterIsFoundByReadingAlone :: TestTree
testImporterIsFoundByReadingAlone = testCase "an importer is found without compiling its page" $ do
  ctss <- ctssBaseFiles testCtssConfig [importerPage] :: IO CTSS
  name <- mkDesCanonicalImportStr testCtssConfig importerPage

  found <- ieDeclared (ctssImporterEnv ctss) [name]
  case found of
    [d] -> do
      assertEqual "the name an import writes to be read with it" "spike" (idName d)
      assertEqual "the declaration to run" "mkResult" (idFn d)
      assertEqual "the extensions it reads" ["spikefile"] (idExts d)
      assertEqual "the page it is declared on" name (idPage d)
    ds -> assertFailure $ "expected one importer, found " ++ show (length ds)

  -- Said explicitly, because the whole point is that this needed no build. A page which had been
  -- built would find the declaration too, and would not be evidence of anything.
  built <- ctssCached (Just . pure . ssfBuilt) name ctss
  assertEqual "and its page was never built" (Just False) (fmap (orElse True) built)

-- | Whatever a computed value came to, for an assertion that is about something else.
orElse :: a -> CRes a -> a
orElse _ (CRes _ v) = v
orElse d _          = d

importerCallTests :: TestTree
importerCallTests = testGroup "ImporterCall" [
  testImporterIsFoundByReadingAlone,
  testCallTowardDeclaredType,
  testDeferredPageIsEventuallyRead
                                             ]
