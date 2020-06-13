module Main where

import System.Directory
import           Data.List
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf

import Syntax
import           Desugarf         (desFiles)
-- import           Emit             (codegen, initModule)
import           Eval.Common
import           Eval
import           TypeCheck.Common  (TypeCheckResult(TypeCheckResult, TypeCheckResE))
import           TypeCheck
import qualified Data.Text.Lazy as T
import Text.Pretty.Simple

testDir :: String
testDir = "test/code/"

runTest :: Bool -> String -> TestTree
runTest includeStd fileName = testCaseSteps fileName $ \step -> do
  step $ printf "Read file %s..." fileName
  maybePrgm <- desFiles $ (fileName : ["std/std.ct" | includeStd])
  case maybePrgm of
    CErr notes -> assertFailure $ "Could not parse and desguar:\n \t" ++ concat (map prettyCNote notes)
    CRes _ prgm -> do
      -- step $ T.unpack $ pShow prgm
      step "Typecheck..."
      -- step $ T.unpack $ pShow $ traceTestPrgm prgm
      case typecheckPrgm prgm of
        TypeCheckResE err -> do
          assertFailure $ "Could not typecheck:\n\n\n\t" ++ intercalate "\n\n\n\t\t" (map (T.unpack . pShow) err)
        TypeCheckResult _ tprgm -> do
          -- step $ T.unpack $ pShow $ tprgm
          step "Eval tests..."
          -- step $ T.unpack $ pShow $ evalBuildMain tprgm
          case evalMain tprgm of
            CErr notes -> do
              assertFailure $ "Could not eval:\n\t " ++ intercalate "\n\t" (map prettyCNote notes)
            CRes [] (IntVal 0) -> return ()
            CRes notes res -> assertFailure $ "Bad result for:\n \t " ++ show res ++ "\n \tNotes\t" ++ concat (map prettyCNote notes)
          -- step "Codegen"
          -- void (codegen initModule tprgm)

runTests :: Bool -> [String] -> IO ()
runTests includeStd testFiles = defaultMain $ testGroup "Tests" testTrees
  where testTrees = map (runTest includeStd) testFiles

test :: IO ()
test = runTests False ["test/test.ct"]

standardTests :: IO ([String])
standardTests = do
  fileNames <- listDirectory testDir
  return $ map (testDir ++) fileNames

mt :: String -> IO ()
mt k = do
  let fileName = testDir ++ k ++ ".ct"
  tests <- standardTests
  if elem fileName tests
     then runTests True [fileName]
     else error $ "invalid test name" ++ fileName ++ show tests

mtt :: IO ()
mtt = mt "match"

main :: IO ()
main = do
  tests <- standardTests
  runTests True tests
