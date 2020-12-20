module Main where

import System.Directory
import           Data.List
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf

import CRes
import           Parser (readFiles)
import           Desugarf         (desFiles)
import           Eval
import           TypeCheck
import qualified Data.Text.Lazy as T
import Text.Pretty.Simple
import WebDocs (docServe)

testDir :: String
testDir = "test/code/"

buildDir :: String
buildDir = "test/build/"

prettyCNotes :: [CNote] -> String
prettyCNotes notes = "\n\n\t\t" ++ intercalate "\n\n\t\t" (map prettyNote notes)
  where
    prettyNote note = case posCNote note of
      Just pos -> printf "%s\n\t\t%s" (show pos) (T.unpack $ pShow note)
      Nothing -> T.unpack $ pShow note

runTest :: Bool -> String -> TestTree
runTest includeStd fileName = testCaseSteps fileName $ \step -> do
  step $ printf "Read file %s..." fileName
  maybeRawPrgm <- readFiles (fileName : ["std/std.ct" | includeStd])
  case maybeRawPrgm of
    CErr notes -> assertFailure $ "Could not parse:" ++ prettyCNotes notes
    CRes _ rawPrgm -> do
      -- step $ T.unpack $ pShow rawPrgm
      case desFiles rawPrgm of
        CErr notes -> assertFailure $ "Could not desguar:" ++ prettyCNotes notes
        CRes _ prgm -> do
          -- step $ T.unpack $ pShow prgm
          step "Typecheck..."
          -- step $ T.unpack $ pShow $ traceTestPrgm prgm
          case typecheckPrgm prgm of
            CErr errs -> do
              assertFailure $ "Could not typecheck:" ++ prettyCNotes errs
            CRes _ tprgm -> do
              -- step $ T.unpack $ pShow $ tprgm
              step "Eval tests..."
              case evalMain tprgm of
                CErr notes -> do
                  assertFailure $ "Could not eval: " ++ prettyCNotes notes
                CRes notes io -> do
                  returnValue <- io
                  case (notes, returnValue) of
                    ([], (0, _)) -> return () -- success
                    _ -> assertFailure $ "Bad result for:\n \t " ++ show (fst returnValue) ++ "\n \tNotes:" ++ prettyCNotes notes
              step "evalBuild..."
              case evalMainb tprgm of
                CErr notes -> do
                  assertFailure $ "Could not eval: " ++ prettyCNotes notes
                CRes _ ioRes -> do
                  _ <- ioRes
                  return () -- success
              step "evalAnnots..."
              case evalAnnots tprgm of
                CErr notes -> do
                  assertFailure $ "Could not eval: " ++ prettyCNotes notes
                CRes _ _ -> return () -- success
              step "Done"

runTests :: Bool -> [String] -> TestTree
runTests includeStd testFiles = testGroup "Tests" testTrees
  where testTrees = map (runTest includeStd) testFiles

runBuild :: String -> TestTree
runBuild fileName = testCaseSteps fileName $ \step -> do
  step $ printf "Read file %s..." fileName
  maybeRawPrgm <- readFiles (fileName : ["std/std.ct"])
  case maybeRawPrgm of
    CErr notes -> assertFailure $ "Could not parse:" ++ prettyCNotes notes
    CRes _ rawPrgm -> do
      -- step $ T.unpack $ pShow rawPrgm
      case desFiles rawPrgm of
        CErr notes -> assertFailure $ "Could not desguar:\n \t" ++ prettyCNotes notes
        CRes _ prgm -> do
          -- step $ T.unpack $ pShow prgm
          step "Typecheck..."
          -- step $ T.unpack $ pShow $ traceTestPrgm prgm
          case typecheckPrgm prgm of
            CErr errs -> do
              assertFailure $ "Could not typecheck:\n\n\n\t" ++ prettyCNotes errs
            CRes _ tprgm -> do
              -- step $ T.unpack $ pShow $ tprgm
              step "Eval tests..."
              case evalMainb tprgm of
                CErr notes -> do
                  assertFailure $ "Could not eval:\n\t " ++ prettyCNotes notes
                CRes _ _ -> return () -- success
              step "evalAnnots..."
              case evalAnnots tprgm of
                CErr notes -> do
                  assertFailure $ "Could not eval:\n\t " ++ prettyCNotes notes
                CRes _ _ -> return () -- success

runBuilds :: [String] -> TestTree
runBuilds testFiles = testGroup "Builds" testTrees
  where testTrees = map runBuild testFiles

test :: IO ()
test = defaultMain $ runTests False ["test/test.ct"]

testd :: IO ()
testd = docServe False "test/test.ct"

standardTests :: IO [String]
standardTests = do
  fileNames <- listDirectory testDir
  return $ map (testDir ++) fileNames

buildTests :: IO [String]
buildTests = do
  fileNames <- listDirectory buildDir
  return $ map (buildDir ++) fileNames

mt :: String -> IO ()
mt k = do
  let fileName = testDir ++ k ++ ".ct"
  tests <- standardTests
  if fileName `elem` tests
     then defaultMain $ runTests True [fileName]
     else error $ printf "invalid test name %s in %s" fileName (show tests)

mtd :: String -> IO ()
mtd k = do
  let fileName = testDir ++ k ++ ".ct"
  tests <- standardTests
  if fileName `elem` tests
     then docServe True fileName
     else error $ printf "invalid test name %s in %s" fileName (show tests)

mb :: String -> IO ()
mb k = do
  let fileName = buildDir ++ k ++ ".ct"
  tests <- buildTests
  if fileName `elem` tests
     then defaultMain $ runBuilds [fileName]
     else error $ printf "invalid build test name %s in %s" fileName (show tests)

mbd :: String -> IO ()
mbd k = do
  let fileName = buildDir ++ k ++ ".ct"
  tests <- buildTests
  if fileName `elem` tests
     then docServe True fileName
     else error $ printf "invalid build test name %s in %s" fileName (show tests)

mtt :: IO ()
mtt = mt "match"

main :: IO ()
main = do
  tests <- standardTests
  let tests' = runTests True tests
  builds <- buildTests
  let builds' = runBuilds builds
  let full = testGroup "full" [tests', builds']
  defaultMain full
