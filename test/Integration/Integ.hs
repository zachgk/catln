module Integration.Integ where

import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf

import           Common.TestCommon  (findCt)
import           CRes
import           Data.List          (isPrefixOf)
import qualified Data.Text.Lazy     as T
import           Eval
import           Syntax.Ct.Desugarf (desFiles)
import           Syntax.Parsers     (mkDesCanonicalImportStr,
                                     mkRawCanonicalImportStr, readFiles)
import           System.Directory   (doesFileExist, getCurrentDirectory)
import           System.FilePath    (takeBaseName)
import           Text.Pretty.Simple (pShowNoColor)
import           TypeCheck
import           Utils
import           WebDocs            (docApi)

testDir, disabledTestDir :: String
testDir = "test/Integration/code/"
disabledTestDir = "test/Integration/disabled/"

goldenDesugarDir :: String
goldenDesugarDir = "test/Integration/goldenDesugar/"

goldenTypecheckDir :: String
goldenTypecheckDir = "test/Integration/goldenTypecheck/"

runGoldenTest :: (Show fn, Show prgm) => String -> String -> String -> GraphData fn prgm -> (String -> IO ()) -> IO ()
runGoldenTest goldenType goldenDir fileNameStr prgm step = do
  let goldenPath = goldenDir ++ takeBaseName fileNameStr ++ ".txt"
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

runTest :: Bool -> Bool -> String -> TestTree
runTest runGolden includeCore fileNameStr = testCaseSteps fileNameStr $ \step -> do
  step $ printf "Read file %s..." fileNameStr
  fileNameRaw <- mkRawCanonicalImportStr fileNameStr
  fileName <- mkDesCanonicalImportStr fileNameStr
  maybeRawPrgm <- readFiles includeCore [fileNameRaw]


  case maybeRawPrgm of
    CErr notes -> assertFailure $ "Could not parse:" ++ prettyCNotes notes
    CRes _ rawPrgm -> do
      case desFiles rawPrgm of
        CErr notes -> assertFailure $ "Could not desguar:" ++ prettyCNotes notes
        CRes _ prgm -> do

          when runGolden $ do
            runGoldenTest "desugar" goldenDesugarDir fileNameStr prgm step

          step "Typecheck..."
          case typecheckPrgm prgm of
            CErr errs -> do
              assertFailure $ "Could not typecheck:" ++ prettyCNotes errs
            CRes _ tprgm -> do

              when runGolden $ do
                runGoldenTest "typecheck" goldenTypecheckDir fileNameStr tprgm step

              when (evalRunnable $ evalTargetMode "main" fileName tprgm) $ do
                step "Eval Run..."
                case evalRun "main" fileName tprgm of
                  CErr notes -> do
                    assertFailure $ "Could not eval: " ++ prettyCNotes notes
                  CRes notes io -> do
                    returnValue <- io
                    case (notes, returnValue) of
                      ([], (0, _)) -> return () -- success
                      _ -> assertFailure $ "Bad result for:\n \t " ++ show (fst returnValue) ++ "\n \tNotes:" ++ prettyCNotes notes
              step "Eval Build..."
              case evalBuild "main" fileName tprgm of
                CErr notes -> do
                  assertFailure $ "Could not eval: " ++ prettyCNotes notes
                CRes _ ioRes -> do
                  _ <- ioRes
                  return () -- success
              step "evalAnnots..."
              case evalAnnots fileName tprgm of
                CErr notes -> do
                  assertFailure $ "Could not eval: " ++ prettyCNotes notes
                CRes _ _ -> return () -- success
              step "Done"

runTests :: Bool -> Bool -> [String] -> TestTree
runTests runGolden includeCore testFiles = testGroup "Tests" testTrees
  where testTrees = map (runTest runGolden includeCore) testFiles

test :: IO ()
test = defaultMain $ runTests False False ["test/test.ct"]

testd :: IO ()
testd = docApi False False "test/test.ct"

standardTests :: IO [String]
standardTests = findCt testDir

integrationTests :: IO TestTree
integrationTests = runTests True True <$> standardTests

mtBase :: Bool -> String -> IO ()
mtBase runGolden k = do
  let fileName = testDir ++ k ++ ".ct"
  fileNameExists <- doesFileExist fileName
  let disabledFileName = disabledTestDir ++ k ++ ".ct"
  disabledFileNameExists <- doesFileExist disabledFileName
  fileName' <- case () of
        _ | fileNameExists -> return fileName
        _ | disabledFileNameExists -> return disabledFileName
        _ -> fail $ printf "invalid test name %s" fileName
  defaultMain $ runTests runGolden True [fileName']

mt :: String -> IO ()
mt = mtBase False

mtg :: String -> IO ()
mtg = mtBase True

mtd :: String -> IO ()
mtd k = do
  let fileName = testDir ++ k ++ ".ct"
  tests <- standardTests
  if fileName `elem` tests
     then docApi False True fileName
     else error $ printf "invalid test name %s in %s" fileName (show tests)
