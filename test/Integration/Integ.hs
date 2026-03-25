module Integration.Integ where

import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf

import           Common.TestCommon       (findCt, findPy)
import           Control.Monad.Trans
import           CRes
import           CtService
import           Data.Graph              (graphFromEdges)
import           Data.List               (isSuffixOf)
import           Data.Maybe
import qualified Data.Text.Lazy          as T
import           Eval
import           Semantics
import           Semantics.Prgm
import           Syntax.Ct.Formatter     (formatRootPrgm)
import           Syntax.Ct.Parser.Syntax (PPrgmGraphData)
import           Syntax.Parsers          (mkDesCanonicalImportStr)
import           System.Directory        (createDirectoryIfMissing,
                                          doesFileExist, getCurrentDirectory)
import           System.Environment      (lookupEnv)
import           System.FilePath         (takeDirectory)
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
      runGoldenTestGen "convert"
        (\_cwd p -> formatRootPrgm p)
        (\relPath -> goldenConvertDir ++ relPath ++ ".ct")
        (graphFromEdges [(prgm, fileName, [])])
        step

runTest :: Bool -> String -> TestTree
runTest runGolden fileNameStr = testCaseSteps fileNameStr $ \step -> do
  step $ printf "Read file %s..." fileNameStr
  fileName <- mkDesCanonicalImportStr testCtssConfig fileNameStr
  ctss <- ctssBaseFiles testCtssConfig [fileNameStr]

  res <- runCResT $ do
    rawPrgm <- getRawPrgm ctss

    when (runGolden && ".py" `isSuffixOf` fileNameStr) $ do
      lift $ runConvertGoldenTest fileName rawPrgm step

    prgm <- getPrgm ctss

    when runGolden $ do
      lift $ runGoldenTest "desugar" goldenDesugarDir fileNameStr prgm step

    lift $ step "Typecheck..."
    tprgm <- getTPrgm ctss

    when runGolden $ do
      lift $ runGoldenTest "typecheck" goldenTypecheckDir fileNameStr tprgm step

    when runGolden $ do
      lift $ step "TreeBuild..."
      tbprgm <- getTBPrgm ctss
      lift $ runGoldenTest "treebuild" goldenTreebuildDir fileNameStr tbprgm step

    evalTarget <- asCResT $ evalTargetMode "main" fileName tprgm
    when (evalRunnable evalTarget) $ do
      lift $ step "Eval Run..."
      returnValue <- getEvaluated ctss fileName "main"
      case returnValue of
        0 -> return ()
        v -> lift $ assertFailure $ "Bad result for:\n \t " ++ show v

    lift $ step "Eval Build..."
    _ <- getEvalBuild ctss fileName "main"

    lift $ step "Eval Annots..."
    _ <- getEvalAnnots ctss fileName
    return ()
  case res of
    CRes notes _ -> do
      unless (null notes) $ putStrLn $ prettyCNotes notes
      step $ printf "Test %s finished" fileNameStr
      return ()
    CErr notes ->
      assertFailure $ "Failed test: \n" ++ prettyCNotes notes

runTests :: Bool -> [String] -> TestTree
runTests runGolden testFiles = testGroup "Tests" testTrees
  where testTrees = map (runTest runGolden) testFiles

test :: IO ()
test = defaultMain $ runTests False ["test/test.ct"]

testd :: IO ()
testd = docApi False ["test/test.ct"]

runPyTest :: Bool -> String -> TestTree
runPyTest = runTest

standardTests :: IO [String]
standardTests = findCt testDir

integrationTests :: IO TestTree
integrationTests = do
  ctFiles <- standardTests
  pyFiles <- findPy testDir
  let ctGroup = runTests True ctFiles
  let pyGroup = testGroup "Python" $ map (runPyTest True) pyFiles
  return $ testGroup "Integration" [ctGroup, pyGroup]

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
