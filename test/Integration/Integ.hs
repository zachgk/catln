module Integration.Integ where

import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf

import           Common.TestCommon       (findCt, findJson, findPy)
import           CtConstants             (escapedPathName)
import           Control.Monad.Trans
import           CRes
import           CtService
import           Data.Graph              (graphFromEdges)
import           Data.List               (isSuffixOf)
import           Data.Maybe
import qualified Data.Text.Lazy          as T
import           Eval
import           Eval.Common             (SSF (..), getMockIOOutput, newMockIO)
import           Semantics
import           Semantics.Prgm
import           Syntax.Ct.Formatter     (formatRootPrgm)
import           Syntax.Ct.Parser.Syntax (PPrgmGraphData)
import           Syntax.Parsers          (mkDesCanonicalImportStr)
import           System.Directory        (createDirectoryIfMissing,
                                          doesFileExist, getCurrentDirectory)
import           System.Environment      (lookupEnv)
import           System.FilePath         (replaceExtension, takeDirectory)
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

runGoldenTest :: (Show em, Show prgm) => String -> String -> String -> GraphData prgm (AFileImport em) -> (String -> IO ()) -> IO ()
runGoldenTest goldenType goldenDir _fileNameStr =
  runGoldenTestGen goldenType
    (\cwd prgm -> T.unpack $ T.replace (T.pack cwd) "/repo/dir" $ pShowNoColor prgm)
    (\relPath ->
      let p = goldenDir ++ T.unpack (T.replace (T.pack ".ct") ".txt" (T.pack relPath))
      in if ".txt" `isSuffixOf` p then p else p ++ ".txt")

-- | Golden test for non-Catln files: formats the converted RawPrgm as Catln
-- source and compares/writes it to test/Integration/golden/convert.
runConvertGoldenTest :: FileImport -> PPrgmGraphData -> (String -> IO ()) -> IO ()
runConvertGoldenTest fileName rawPrgms step =
  case graphLookup fileName rawPrgms of
    Nothing -> assertFailure $ printf "File not in raw prgm graph: %s" (show $ impRaw fileName)
    Just prgm ->
      -- The working directory is written out as a fixed string, as 'runGoldenTest' writes it. A page
      -- read by an importer imports the page declaring that importer, by the absolute path it was
      -- resolved to — so without this the golden holds the path of whoever last wrote it.
      --
      -- Twice, because a path can appear in a page in two forms. Written as a path it is the plain
      -- string; written as a NAME built from a path ('CtConstants.escapedPathName', which is how a
      -- JSON file's declaration and a directory's are named) every character a name may not hold has
      -- become @q@ and two hex digits, so the plain substitution does not match it and the golden
      -- keeps the absolute path of whoever last wrote it. The escaped form is replaced first: the two
      -- differ wherever the working directory holds such a character, and a directory holding none
      -- makes them the same replacement done twice, which is harmless.
      runGoldenTestGen "convert"
        (\cwd p -> T.unpack $ T.replace (T.pack $ escapedPathName "" cwd) "/repo/dir" $ T.replace (T.pack cwd) "/repo/dir" $ T.pack $ formatRootPrgm p)
        (\relPath -> goldenConvertDir ++ relPath ++ ".ct")
        (graphFromEdges [(prgm, fileName, [])])
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
      lift $ runConvertGoldenTest fileName rawPrgm step

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
    _ <- getEvalAnnots ctss fileName
    return ()
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

integrationTests :: IO TestTree
integrationTests = do
  ctFiles <- standardTests
  pyFiles <- findPy testDir
  jsonFiles <- findJson testDir
  let allFiles = ctFiles ++ pyFiles ++ jsonFiles
  return $ withResource (ctssBaseFiles testCtssConfig allFiles) (const $ return ()) $ \getCtss ->
    testGroup "Integration" [
      testGroup "Tests" $ map (runTest True getCtss) ctFiles,
      testGroup "Python" $ map (runPyTest True getCtss) pyFiles,
      testGroup "Json" $ map (runTest True getCtss) jsonFiles
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
