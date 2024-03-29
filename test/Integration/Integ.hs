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
import           Syntax.InferImport (inferImportStr, inferRawImportStr)
import           Syntax.Parsers     (readFiles)
import           System.Directory   (doesFileExist)
import           System.FilePath    (takeBaseName)
import           Text.Pretty.Simple (pShowNoColor)
import           TypeCheck
import           Utils
import           WebDocs            (docApi)

testDir :: String
testDir = "test/Integration/code/"

goldenDesugarDir :: String
goldenDesugarDir = "test/Integration/goldenDesugar/"

goldenTypecheckDir :: String
goldenTypecheckDir = "test/Integration/goldenTypecheck/"

runTest :: Bool -> Bool -> String -> TestTree
runTest runGolden includeCore fileNameStr = testCaseSteps fileNameStr $ \step -> do
  step $ printf "Read file %s..." fileNameStr
  fileNameRaw <- inferRawImportStr fileNameStr
  fileName <- inferImportStr fileNameStr
  maybeRawPrgm <- readFiles includeCore [fileNameRaw]


  case maybeRawPrgm of
    CErr notes -> assertFailure $ "Could not parse:" ++ prettyCNotes notes
    CRes _ rawPrgm -> do
      case desFiles rawPrgm of
        CErr notes -> assertFailure $ "Could not desguar:" ++ prettyCNotes notes
        CRes _ prgm -> do

          when runGolden $ do
            let goldenDesugarPath = goldenDesugarDir ++ takeBaseName fileNameStr ++ ".txt"
            goldenDesugarExists <- doesFileExist goldenDesugarPath
            let showPrgm = T.unpack $ pShowNoColor $ graphToNodes prgm
            if testDir `isPrefixOf` fileNameStr && goldenDesugarExists
              then do
                step "Golden test desugar..."
                golden <- readFile goldenDesugarPath
                when (golden /= showPrgm) (assertFailure "Desugar doesn't match golden test" )
              else do
                step "No golden test for desugar. Writing"
                writeFile goldenDesugarPath showPrgm

          step "Typecheck..."
          case typecheckPrgm prgm of
            CErr errs -> do
              assertFailure $ "Could not typecheck:" ++ prettyCNotes errs
            CRes _ tprgm -> do

              when runGolden $ do
                let goldenTypecheckPath = goldenTypecheckDir ++ takeBaseName fileNameStr ++ ".txt"
                goldenTypecheckExists <- doesFileExist goldenTypecheckPath
                let showTPrgm = T.unpack $ pShowNoColor $ graphToNodes tprgm
                if testDir `isPrefixOf` fileNameStr && goldenTypecheckExists
                  then do
                    step "Golden test typecheck..."
                    golden <- readFile goldenTypecheckPath
                    when (golden /= showTPrgm) (assertFailure "Typecheck doesn't match golden test" )
                  else do
                    step "No golden test for typecheck. Writing"
                    writeFile goldenTypecheckPath showTPrgm

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
  tests <- standardTests
  if fileName `elem` tests
     then defaultMain $ runTests runGolden True [fileName]
     else error $ printf "invalid test name %s in %s" fileName (show tests)

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
