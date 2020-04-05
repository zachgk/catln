module Main where

import           Data.List
import           Test.Tasty
import           Test.Tasty.HUnit

import           Syntax
import           Desugarf         (desFiles)
-- import           Emit             (codegen, initModule)
import           Eval.Common
import           Eval
import           TypeCheck.Common  (TypeCheckResult(TypeCheckResult, TypeCheckResE))
import           TypeCheck
import qualified Data.Text.Lazy as T
import Text.Pretty.Simple

runTest :: String -> String -> IO ()
runTest displayName fileName = defaultMain $ testCaseSteps displayName $ \step -> do
  step "Read file..."
  maybePrgm <- desFiles [fileName, "std/std.flng"]
  case maybePrgm of
    CErr notes -> assertFailure $ "Could not parse and desguar:\n \t" ++ show notes
    CRes _ prgm -> do
      step "Typecheck..."
      case typecheckPrgm prgm of
        TypeCheckResE err -> do
          step $ T.unpack $ pShow $ traceTestPrgm prgm
          assertFailure $ "Could not typecheck:\n\n\n\t" ++ intercalate "\n\n\n\t\t" (map (T.unpack . pShow) err)
        TypeCheckResult _ tprgm -> do
          -- step $ T.unpack $ pShow $ traceTestPrgm prgm
          step "Eval tests..."
          case evalMain tprgm of
            Left err -> do
              -- step $ T.unpack $ pShow $ makeBaseEnv (fst tprgm)
              assertFailure $ "Could not eval:\n \t " ++ show err
            Right (IntVal 0) -> return ()
            Right err -> assertFailure $ "Bad result for:\n \t " ++ show err
          -- step "Codegen"
          -- void (codegen initModule tprgm)

test :: IO ()
test = runTest "Test"  "test/code/test.flng"

main :: IO ()
main = runTest "Add"  "test/code/add.flng"
