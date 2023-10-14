module Integration.Integ where

import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf

import           CRes
import           Eval
import           Syntax.Ct.Desugarf (desFiles)
import           Syntax.Parsers     (readFiles)
import           TestCommon
import           TypeCheck
import           WebDocs            (docApi)

testDir :: String
testDir = "test/Integration/code/"

runTest :: Bool -> String -> TestTree
runTest includeCore fileName = testCaseSteps fileName $ \step -> do
  step $ printf "Read file %s..." fileName
  maybeRawPrgm <- readFiles includeCore True [fileName]
  case maybeRawPrgm of
    CErr notes -> assertFailure $ "Could not parse:" ++ prettyCNotes notes
    CRes _ rawPrgm -> do
      -- step $ T.unpack $ pShow rawPrgm
      case desFiles rawPrgm of
        CErr notes -> assertFailure $ "Could not desguar:" ++ prettyCNotes notes
        CRes _ prgm -> do
          -- step $ T.unpack $ pShow prgm
          step "Typecheck..."
          case typecheckPrgm prgm of
            CErr errs -> do
              assertFailure $ "Could not typecheck:" ++ prettyCNotes errs
            CRes _ tprgm -> do
              -- step $ T.unpack $ pShow $ tprgm
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

runTests :: Bool -> [String] -> TestTree
runTests includeCore testFiles = testGroup "Tests" testTrees
  where testTrees = map (runTest includeCore) testFiles

test :: IO ()
test = defaultMain $ runTests False ["test/test.ct"]

testd :: IO ()
testd = docApi False False "test/test.ct"

standardTests :: IO [String]
standardTests = findCt testDir

integrationTests :: IO TestTree
integrationTests = runTests True <$> standardTests
