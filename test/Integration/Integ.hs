module Integration.Integ where

import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf

import           Common.TestCommon   (findCt)
import           Control.Monad.Trans
import           CRes
import           Data.List           (isPrefixOf)
import qualified Data.Text.Lazy      as T
import           Eval
import           Syntax.Ct.Desugarf  (desFiles)
import           Syntax.Parsers      (mkDesCanonicalImportStr,
                                      mkRawCanonicalImportStr, readFiles)
import           System.Directory    (createDirectoryIfMissing, doesFileExist,
                                      getCurrentDirectory)
import           System.FilePath     (takeBaseName)
import           Text.Pretty.Simple  (pShowNoColor)
import           TypeCheck
import           Utils
import           WebDocs             (docApi, docServe)

testDir, disabledTestDir :: String
testDir = "test/Integration/code/"
disabledTestDir = "test/Integration/disabled/"

goldenDesugarDir :: String
goldenDesugarDir = "test/Integration/golden/desugar/"

goldenTypecheckDir :: String
goldenTypecheckDir = "test/Integration/golden/typecheck/"

goldenTreebuildDir :: String
goldenTreebuildDir = "test/Integration/golden/tbuild/"

runGoldenTest :: (Show fn, Show prgm) => String -> String -> String -> GraphData fn prgm -> (String -> IO ()) -> IO ()
runGoldenTest goldenType goldenDir fileNameStr prgm step = do
  let goldenPath = goldenDir ++ takeBaseName fileNameStr ++ ".txt"
  createDirectoryIfMissing True goldenDir
  goldenExists <- doesFileExist goldenPath
  let showPrgm = pShowNoColor $ graphToNodes prgm
  cwd <- getCurrentDirectory
  let showPrgm' = T.unpack $ T.replace (T.pack cwd) "/repo/dir" showPrgm
  if testDir `isPrefixOf` fileNameStr && goldenExists
    then do
      step $ printf "Golden test %s..." goldenType
      golden <- readFile goldenPath
      when (golden /= showPrgm') (assertFailure $ printf "%s doesn't match golden test" goldenType)
    else do
      step $ printf "No golden test for %s. Writing" goldenType
      writeFile goldenPath showPrgm'

runTest :: Bool -> String -> TestTree
runTest runGolden fileNameStr = testCaseSteps fileNameStr $ \step -> do
  step $ printf "Read file %s..." fileNameStr
  fileNameRaw <- mkRawCanonicalImportStr fileNameStr
  fileName <- mkDesCanonicalImportStr fileNameStr

  res <- runCResT $ do
    rawPrgm <- readFiles [fileNameRaw]
    prgm <- desFiles rawPrgm

    when runGolden $ do
      lift $ runGoldenTest "desugar" goldenDesugarDir fileNameStr prgm step

    lift $ step "Typecheck..."
    tprgm <- asCResT $ typecheckPrgm prgm

    when runGolden $ do
      lift $ runGoldenTest "typecheck" goldenTypecheckDir fileNameStr tprgm step

    when runGolden $ do
      lift $ step "TreeBuild..."
      tbprgm <- asCResT $ evalBuildAll tprgm
      lift $ runGoldenTest "treebuild" goldenTreebuildDir fileNameStr tbprgm step

    evalTarget <- asCResT $ evalTargetMode "main" fileName tprgm
    when (evalRunnable evalTarget) $ do
      lift $ step "Eval Run..."
      returnValue <- evalRun "main" fileName tprgm
      case returnValue of
        (0, _) -> return ()
        _ -> lift $ assertFailure $ "Bad result for:\n \t " ++ show (fst returnValue)

    lift $ step "Eval Build..."
    _ <- evalBuild "main" fileName tprgm

    lift $ step "Eval Annots..."
    _ <- asCResT $ evalAnnots fileName tprgm
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

standardTests :: IO [String]
standardTests = findCt testDir

integrationTests :: IO TestTree
integrationTests = runTests True <$> standardTests

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
