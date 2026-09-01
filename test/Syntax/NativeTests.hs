-- | Tests for reading a page with an importer written in Catln.
--
-- The @native@ syntax is the one reader that cannot read anything by itself: it runs a declaration
-- somebody wrote in a @.ct@ file, and so it has to ask the compiler both whether that declaration
-- can be run yet and to run it. Those two questions are what the cases below are about.
--
-- Driven against a stub rather than against a compilation service, because what is being tested is
-- the reader and not the service: the reader has to put the importer's page into the graph when it
-- cannot read yet, run the importer when it can, and pass on exactly what the import was written
-- with. A real service would answer all three at once and say which of them was wrong only by
-- failing somewhere else.
module Syntax.NativeTests where

import           Data.IORef           (IORef, modifyIORef', newIORef, readIORef)
import           Semantics.Prgm       (Expr, impAbs)
import           Syntax.Ct.Prgm       (ImportBlocked (..), ImportReader (..),
                                       ImportSyntax (..), ImporterEnv (..),
                                       ParsedImport (..), PreloadImport (..),
                                       RawPrgm (..), ReadPath (..),
                                       newRecordedReadEnv, noImporters)
import           Syntax.Native.Parser (NativeImport (..), nativeSyntax,
                                       readNativeImport)
import           Syntax.Parsers       (mkDesCanonicalImportStr)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Semantics            (testCtssConfig)

-- | The import expression a page read by a Catln-declared importer canonicalizes to.
--
-- Built the way the compiler will build it, by writing it as a caller would and letting
-- 'mkDesCanonicalImportStr' resolve it, so that the shape this reads back is the shape the rest of
-- the pipeline produces rather than one written to suit the reader.
nativeExpr :: FilePath -> FilePath -> String -> IO (Expr ())
nativeExpr path importer fn = impAbs <$> mkDesCanonicalImportStr testCtssConfig
  (concat ["native(\"", path, "\", importer= \"", importer, "\", fn= \"", fn, "\")"])

-- | A stub compiler which says every importer is ready and answers with the page given.
--
-- Records what it was asked, since half of what the reader has to get right is passing on what the
-- import was written with.
readyImporters :: IORef [(FilePath, String, FilePath, [(String, String)])] -> ParsedImport -> ImporterEnv
readyImporters asked parsed = noImporters{
  ieReady = const $ return Nothing,
  ieInvoke = \importer fn path args -> do
    modifyIORef' asked ((importer, fn, path, args) :)
    return $ Right parsed
                                         }

-- | A stub compiler which says the importer's page is not built, and refuses to run it.
--
-- The refusal is what says the reader never runs an importer it has been told is not ready: a case
-- which stubbed 'ieInvoke' with an answer would pass whether or not the reader asked first.
blockedImporters :: ImporterEnv
blockedImporters = noImporters{
  ieReady = \p -> return $ Just $ ImportBlocked p "not built",
  ieInvoke = \_ _ _ _ -> assertFailure "the reader ran an importer it had been told was not ready"
                             }

-- | A page holding nothing, as an importer that read an empty file would answer.
emptyParsed :: ParsedImport
emptyParsed = ParsedImport (RawPrgm [] []) (Just "src") (Just [ReadFile "/tmp/x.py"])

withReader :: ImporterEnv -> (ImportReader -> IO a) -> IO a
withReader importers act = do
  env <- newRecordedReadEnv Nothing importers
  act =<< isReader nativeSyntax env

-- | What the import says is what the reader reads back out of it.
testReadsWhatWasWritten :: TestTree
testReadsWhatWasWritten = testCase "an import says which importer reads which file" $ do
  e <- nativeExpr "/tmp/x.py" "/tmp/importer.ct" "parsePython"
  ni <- readNativeImport e
  assertEqual "the file to read" "/tmp/x.py" (niPath ni)
  assertEqual "the page declaring the importer" "/tmp/importer.ct" (niImporter ni)
  assertEqual "the declaration to run" "parsePython" (niFn ni)
  -- The two arguments the syntax reads for itself are its own and are not passed on again.
  assertEqual "nothing else was written" [] (niArgs ni)

-- | An import may carry arguments of the importer's own, as @haskell@ carries a Stack root.
testPassesOnOtherArgs :: TestTree
testPassesOnOtherArgs = testCase "an importer's own arguments are passed on to it" $ do
  e <- impAbs <$> mkDesCanonicalImportStr testCtssConfig
    "native(\"/tmp/x.py\", importer= \"/tmp/i.ct\", fn= \"f\", dialect= \"py2\")"
  ni <- readNativeImport e
  assertEqual "the importer's own argument" [("dialect", "py2")] (niArgs ni)

-- | A page whose importer is not built is in the graph with that page as its one edge, and without
-- a program.
testBlockedPageWaits :: TestTree
testBlockedPageWaits = testCase "a page whose importer is not built waits on it" $ do
  e <- nativeExpr "/tmp/x.py" "/tmp/importer.ct" "f"
  withReader blockedImporters $ \reader -> do
    blocked <- irBlocked reader e
    assertEqual "it waits on one page" 1 (length blocked)

    preloaded <- irPreload reader e
    assertEqual "and has no program yet" True (null [() | Just _ <- [plParsed preloaded]])
    assertEqual "but is in the graph by its importer" 1 (length $ plImports preloaded)

-- | A page whose importer IS built is read, and the importer is asked for exactly what the import
-- named.
testReadyPageIsRead :: TestTree
testReadyPageIsRead = testCase "a page whose importer is built is read by it" $ do
  e <- nativeExpr "/tmp/x.py" "/tmp/importer.ct" "parsePython"
  asked <- newIORef []
  withReader (readyImporters asked emptyParsed) $ \reader -> do
    blocked <- irBlocked reader e
    assertEqual "it waits on nothing" [] blocked

    parsed <- irParse reader e
    assertEqual "and is read as the program the importer answered with" (Just "src") (piSource parsed)
    assertEqual "with what the importer said it read" (Just [ReadFile "/tmp/x.py"]) (piReads parsed)

    was <- readIORef asked
    assertEqual "the importer was asked once" 1 (length was)
    assertEqual "for what the import named" [("/tmp/importer.ct", "parsePython", "/tmp/x.py", [])] was

-- | A page read in one go still carries the edge to the page which read it.
--
-- Without it the importer would be reachable from nothing once every page it reads has been read,
-- and a later change to the importer would invalidate none of them.
testReadPageKeepsImporterEdge :: TestTree
testReadPageKeepsImporterEdge = testCase "a page read by an importer still imports it" $ do
  e <- nativeExpr "/tmp/x.py" "/tmp/importer.ct" "f"
  asked <- newIORef []
  withReader (readyImporters asked emptyParsed) $ \reader -> do
    preloaded <- irPreload reader e
    assertEqual "it was read in one go" True (not $ null [() | Just _ <- [plParsed preloaded]])
    assertEqual "and imports the page which read it" 1 (length $ plImports preloaded)

-- | With no compiler behind it, a native page is reported rather than read as something else.
testNoCompilerReports :: TestTree
testNoCompilerReports = testCase "a native page cannot be read without the compiler" $ do
  e <- nativeExpr "/tmp/x.py" "/tmp/importer.ct" "f"
  withReader noImporters $ \reader -> do
    blocked <- irBlocked reader e
    assertEqual "it waits, rather than being read as an empty page" 1 (length blocked)

nativeTests :: TestTree
nativeTests = testGroup "Native" [
  testReadsWhatWasWritten,
  testPassesOnOtherArgs,
  testBlockedPageWaits,
  testReadyPageIsRead,
  testReadPageKeepsImporterEdge,
  testNoCompilerReports
                                 ]
