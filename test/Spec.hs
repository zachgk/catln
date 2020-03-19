module Main where

import           Data.List
import           Test.Tasty
import           Test.Tasty.HUnit

import           Desugarf         (desPrgm)
-- import           Emit             (codegen, initModule)
import           Eval.Common
import           Eval
import           Parser           (parseFile)
import           TypeCheck
import qualified Data.Text.Lazy as T
import Text.Pretty.Simple

runTest :: String -> String -> IO ()
runTest displayName fileName = defaultMain $ testCaseSteps displayName $ \step -> do
  step "Read file..."
  contents <- readFile fileName
  case parseFile contents of
    Left err -> assertFailure $ "Could not parse:\n \t" ++ show err
    Right rprgm -> do
      -- step $ T.unpack $ pShow rprgm
      step "Desgugar..."
      let prgm = desPrgm rprgm
      step "Typecheck..."
      case typecheckPrgm prgm of
        Left err -> do
          step $ T.unpack $ pShow $ traceTestPrgm prgm
          assertFailure $ "Could not typecheck:\n\n\n\t" ++ intercalate "\n\n\n\t\t" (map (T.unpack . pShow) err)
        Right tprgm -> do
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
