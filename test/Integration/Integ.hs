module Integration.Integ where

import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf

import           Common.TestCommon       (findCt, findJson, findPy)
import           Control.Monad.Trans
import           CRes
import           CtConstants             (escapedPathName)
import           CtService
import           Data.Graph              (graphFromEdges)
import           Data.List               (intercalate, isSuffixOf)
import           Data.Maybe
import qualified Data.Text.Lazy          as T
import           Eval
import           Eval.Common             (EvalMetaDat, SSF (..),
                                          TestResult (..), Val, getMockIOOutput,
                                          newMockIO)
import           Eval.SyntaxTreeTests    (syntaxTreeTests)
import           Eval.Testing            (runTestDesc)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types         (nameStr)
import           Syntax.Ct.Formatter     (formatRootPrgm)
import           Syntax.Ct.Parser.Syntax (PPrgmGraphData)
import           Syntax.Parsers          (mkDesCanonicalImportStr)
import           System.Directory        (createDirectoryIfMissing,
                                          doesFileExist, getCurrentDirectory)
import           System.Environment      (lookupEnv)
import           System.FilePath         (replaceExtension, takeDirectory)
import           TestService             (MkService)
import           Text.Pretty.Simple      (pShowNoColor)
import           Utils
import           WebDocs                 (docApi, docServe)

testDir, disabledTestDir :: String
testDir = "test/Integration/code/"
disabledTestDir = "test/Integration/disabled/"

goldenDesugarDir :: String
goldenDesugarDir = "test/Integration/golden/desugar"

goldenTypecheckDir :: String
goldenTypecheckDir = "test/Integration/golden/typecheck"

goldenTreebuildDir :: String
goldenTreebuildDir = "test/Integration/golden/tbuild"

goldenConvertDir :: String
goldenConvertDir = "test/Integration/golden/convert"

goldenAnnotsDir :: String
goldenAnnotsDir = "test/Integration/golden/annots"

goldenWriteEnv :: String
goldenWriteEnv = "GOLDEN_TEST_WRITE"

-- | General golden test runner parameterized on serializer and path builder.
-- @serialize cwd prgm@ produces the file content; @buildPath relPath@ maps
-- a cwd-relative path to the golden file path.
runGoldenTestGen :: String -> (String -> prgm -> String) -> (String -> String) -> GraphData prgm (AFileImport em) -> (String -> IO ()) -> IO ()
runGoldenTestGen goldenType serialize buildPath prgms step = do
  step $ printf "Golden test %s..." goldenType
  goldenWrite <- lookupEnv goldenWriteEnv
  cwd <- getCurrentDirectory
  forM_ (graphToNodes prgms) $ \(prgm, AFileImport{impDisp=maybePath}, _) -> do
    let relPath = drop (length cwd) (fromJust maybePath)
    let goldenPath = buildPath relPath
    let content = serialize cwd prgm
    goldenExists <- doesFileExist goldenPath
    if goldenExists && goldenWrite /= Just "1"
      then do
        golden <- readFile goldenPath
        when (golden /= content) (assertFailure $ printf "%s doesn't match golden test" goldenType)
      else do
        createDirectoryIfMissing True (takeDirectory goldenPath)
        writeFile goldenPath content

-- | The working directory written out as a fixed string, so a golden holds the same text wherever
-- it was written. A page read by an importer imports the page declaring that importer by the
-- absolute path it was resolved to, and a JSON file's declaration is NAMED after its own absolute
-- path — so without this a golden holds the working directory of whoever last wrote it, and every
-- other checkout rewrites it back.
--
-- Both forms a path appears in, because they are different strings. Written as a path it is the
-- plain one. Written as a name built from a path ('CtConstants.escapedPathName', which is how a
-- JSON file's declaration and a directory's are named) every character a name may not hold has
-- become @q@ and two hex digits — so a working directory with a dot in it (@.claude/worktrees/…@,
-- which is where these run) does not match the plain substitution at all. The two replacements are
-- disjoint wherever the directory holds such a character, and are the same replacement done twice
-- where it does not, so neither order nor overlap matters.
withoutCwd :: FilePath -> T.Text -> T.Text
withoutCwd cwd = T.replace (T.pack $ escapedPathName "" cwd) "/repo/dir" . T.replace (T.pack cwd) "/repo/dir"

runGoldenTest :: (Show em, Show prgm) => String -> String -> String -> GraphData prgm (AFileImport em) -> (String -> IO ()) -> IO ()
runGoldenTest goldenType goldenDir _fileNameStr =
  runGoldenTestGen goldenType
    (\cwd prgm -> T.unpack $ withoutCwd cwd $ pShowNoColor prgm)
    (\relPath ->
      let p = goldenDir ++ T.unpack (T.replace (T.pack ".ct") ".txt" (T.pack relPath))
      in if ".txt" `isSuffixOf` p then p else p ++ ".txt")

-- | Golden test of every page reached, as the Catln it was read as: each raw program formatted as
-- Catln source and compared with test/Integration/golden/convert.
--
-- Every page and not the entry page alone, because what a reader wrote is only visible here. A
-- note of a vault is reached by the fixture importing the vault, and its desugared golden shows
-- the declarations it makes but not the imports its links became or the writing its comments
-- carry; the Catln it was read as shows all three.
runConvertGoldenTest :: PPrgmGraphData -> (String -> IO ()) -> IO ()
runConvertGoldenTest =
  runGoldenTestGen "convert"
    (\cwd p -> T.unpack $ withoutCwd cwd $ T.pack $ formatRootPrgm p)
    (\relPath -> goldenConvertDir ++ relPath ++ ".ct")

-- | Golden test of what the entry page's annotations evaluate to, compared with
-- test/Integration/golden/annots: each annotation by its name beside the value it displays as.
--
-- This is where a display is pinned. A @#calendar@, a note's @#tasks@ query or a @#prgmGraph@
-- evaluates to a value carrying what it found, which no golden of the program records and no
-- @main@ can reach; written down here, a change to what a display finds is a golden that moves.
-- A page with no annotations writes no golden, but a golden already written is always compared,
-- so a page whose annotations stop evaluating is noticed rather than read as having none.
runAnnotsGoldenTest :: FileImport -> [(Expr EvalMetaDat, Val)] -> (String -> IO ()) -> IO ()
runAnnotsGoldenTest fileName annots step = do
  cwd <- getCurrentDirectory
  let relPath = drop (length cwd) (fromJust (impDisp fileName))
  goldenExists <- doesFileExist (goldenAnnotsDir ++ relPath ++ ".txt")
  when (goldenExists || not (null annots)) $
    runGoldenTestGen "annots"
      (\cwd' _ -> T.unpack $ withoutCwd cwd' $ pShowNoColor [(nameStr (exprPath a), v) | (a, v) <- annots])
      (\rel -> goldenAnnotsDir ++ rel ++ ".txt")
      (graphFromEdges [((), fileName, [])])
      step

runTest :: Bool -> IO CTSS -> String -> TestTree
runTest runGolden getCtss fileNameStr = testCaseSteps fileNameStr $ \step -> do
  step $ printf "Read file %s..." fileNameStr
  fileName <- mkDesCanonicalImportStr testCtssConfig fileNameStr
  ctss <- getCtss

  res <- runCResT $ do
    rawPrgm <- getRawPrgmFrom fileName ctss

    -- A convert golden for every page, whatever it was written in. For a page a reader produced it
    -- pins the Catln that reader wrote; for a page written in Catln it pins the round trip, since
    -- what is shown is the raw program formatted back out — so a page whose text parses to something
    -- other than what it says is a golden that moves.
    when runGolden $ do
      lift $ runConvertGoldenTest rawPrgm step

    prgm <- ctssGetFrom ssfDes fileName ctss

    when runGolden $ do
      lift $ runGoldenTest "desugar" goldenDesugarDir fileNameStr prgm step

    lift $ step "Typecheck..."
    tprgm <- ctssGetFrom ssfTPrgm fileName ctss

    when runGolden $ do
      lift $ runGoldenTest "typecheck" goldenTypecheckDir fileNameStr tprgm step

    when runGolden $ do
      lift $ step "TreeBuild..."
      tbprgm <- ctssGetFrom ssfTBPrgm fileName ctss
      lift $ runGoldenTest "treebuild" goldenTreebuildDir fileNameStr tbprgm step

    evalTarget <- asCResT $ evalTargetMode "main" fileName tprgm
    when (evalRunnable evalTarget) $ do
      lift $ step "Eval Run..."
      mockIO <- lift newMockIO
      returnValue <- getEvaluatedMock ctss mockIO fileName "main"
      case returnValue of
        0 -> return ()
        v -> lift $ assertFailure $ "Bad result for:\n \t " ++ show v
      let stdoutPath = replaceExtension fileNameStr ".stdout.txt"
      stdoutExists <- lift $ doesFileExist stdoutPath
      when stdoutExists $ do
        expected <- lift $ readFile stdoutPath
        actual   <- lift $ getMockIOOutput mockIO
        when (actual /= expected) $
          lift $ assertFailure $ printf "stdout doesn't match %s\n  expected: %s\n  actual:   %s" stdoutPath (show expected) (show actual)

    -- Guarded by 'evalBuildable', as the run above is guarded by 'evalRunnable'. A page with no
    -- `main` has nothing to build, and asking for one anyway is an error note rather than an empty
    -- answer -- which every `.ct` and `.py` fixture hid by defining one. A page a JSON file is read
    -- as holds a value and no functions, so it is the first fixture that cannot.
    when (evalBuildable evalTarget) $ do
      lift $ step "Eval Build..."
      _ <- getEvalBuild ctss fileName "main"
      return ()

    lift $ step "Eval Annots..."
    annots <- getEvalAnnots ctss fileName
    when runGolden $ do
      lift $ runAnnotsGoldenTest fileName annots step

    -- The tests the page itself wrote -- a @#test@, a derived agreement test of its own arms -- run
    -- as `catln test` would run them. Derived from the page alone ('getTestResultsOf'): the
    -- library's own tests are the Stack suite's to run, once, rather than every fixture's.
    lift $ step "Derived tests..."
    tests <- getTestResultsOf fileName ctss
    results <- lift $ mapM runTestDesc tests
    let failures = [r | r <- results, not (trPassed r)]
    unless (null failures) $
      lift $ assertFailure $ intercalate "\n" [printf "Derived test %s failed:\n%s" (trName r) (fromMaybe "" (trNotes r)) | r <- failures]
  case failOnErrorNotes res of
    CRes notes _ -> do
      unless (null notes) $ putStrLn $ prettyCNotes notes
      step $ printf "Test %s finished" fileNameStr
      return ()
    CErr notes ->
      assertFailure $ "Failed test: \n" ++ prettyCNotes notes

runTests :: Bool -> [String] -> TestTree
runTests runGolden testFiles =
  withResource (ctssBaseFiles testCtssConfig testFiles) (const $ return ()) $ \getCtss ->
    testGroup "Tests" $ map (runTest runGolden getCtss) testFiles

test :: IO ()
test = defaultMain $ runTests False ["test/test.ct"]

testd :: IO ()
testd = docApi False ["test/test.ct"]

runPyTest :: Bool -> IO CTSS -> String -> TestTree
runPyTest = runTest

standardTests :: IO [String]
standardTests = findCt testDir

-- | Pages named to the suite as imports rather than as files: the notes of the fixture vaults
-- whose own annotations are worth evaluating. A fixture importing a vault reaches its notes, but
-- only the entry page's annotations are evaluated, so a note's @#tasks@ query is answered only
-- when the note is the entry. An import is a name the compiler accepts as it accepts a file name
-- ('Syntax.Parsers.mkRawImportStr'), so a note can be named directly.
pageTests :: [String]
pageTests = map (\note -> "obsidian(\"test/Integration/code/artifacts/" ++ note ++ "\")")
  [ "catln-tasks/Query.md"
  , "catln-test/Tasks.md"
  , "catln-tasks/Queries/Status.md"
  , "catln-tasks/Queries/Dates.md"
  , "catln-tasks/Queries/Relative.md"
  , "catln-tasks/Queries/Other.md"
  , "catln-tasks/Queries/Boolean.md"
  , "catln-tasks/Queries/Whole.md"
  , "catln-tasks/Queries/Order.md"
  ]

integrationTests :: MkService -> IO TestTree
integrationTests mk = do
  ctFiles <- standardTests
  pyFiles <- findPy testDir
  jsonFiles <- findJson testDir
  let allFiles = ctFiles ++ pyFiles ++ jsonFiles ++ pageTests
  return $ withResource (mk allFiles) (const $ return ()) $ \getCtss ->
    testGroup "Integration" [
      testGroup "Tests" $ map (runTest True getCtss) ctFiles,
      testGroup "Python" $ map (runPyTest True getCtss) pyFiles,
      testGroup "Json" $ map (runTest True getCtss) jsonFiles,
      testGroup "Pages" $ map (runTest True getCtss) pageTests,
      testGroup "Debugger" [syntaxTreeTests getCtss]
    ]

mtFileName :: String -> IO String
mtFileName k = do
  let fileName = testDir ++ k ++ ".ct"
  fileNameExists <- doesFileExist fileName
  let disabledFileName = disabledTestDir ++ k ++ ".ct"
  disabledFileNameExists <- doesFileExist disabledFileName
  case () of
        _ | fileNameExists -> return fileName
        _ | disabledFileNameExists -> return disabledFileName
        _ -> fail $ printf "invalid test name %s" fileName

mtBase :: Bool -> String -> IO ()
mtBase runGolden k = do
  fileName' <- mtFileName k
  defaultMain $ runTests runGolden [fileName']

mt :: String -> IO ()
mt = mtBase False

mtg :: String -> IO ()
mtg = mtBase True

mtd :: String -> IO ()
mtd k = do
  fileName' <- mtFileName k
  docApi False [fileName']

mtde :: String -> IO ()
mtde k = do
  fileName' <- mtFileName k
  docServe False [fileName']
