module Main where

import           Data.List
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.HashMap.Strict as H
import Syntax
import           Desugarf         (desFiles)
-- import           Emit             (codegen, initModule)
import           Eval.Common
import           Eval
import           TypeCheck.Common  (TypeCheckResult(TypeCheckResult, TypeCheckResE))
import           TypeCheck
import qualified Data.Text.Lazy as T
import Text.Pretty.Simple

runTest :: Bool -> String -> String -> TestTree
runTest includeStd displayName fileName = testCaseSteps displayName $ \step -> do
  step "Read file..."
  maybePrgm <- desFiles $ (fileName : ["std/std.ct" | includeStd])
  case maybePrgm of
    CErr notes -> assertFailure $ "Could not parse and desguar:\n \t" ++ concat (map prettyCNote notes)
    CRes _ prgm -> do
      -- step $ T.unpack $ pShow prgm
      step "Typecheck..."
      case typecheckPrgm prgm of
        TypeCheckResE err -> do
          -- step $ T.unpack $ pShow $ traceTestPrgm prgm
          assertFailure $ "Could not typecheck:\n\n\n\t" ++ intercalate "\n\n\n\t\t" (map (T.unpack . pShow) err)
        TypeCheckResult _ tprgm -> do
          -- step $ T.unpack $ pShow $ traceTestPrgm prgm
          -- step $ T.unpack $ pShow $ tprgm
          step "Eval tests..."
          case evalMain tprgm of
            CErr notes -> do
              -- step $ T.unpack $ pShow $ evalBuildMain tprgm
              assertFailure $ "Could not eval:\n\t " ++ intercalate "\n\t" (map prettyCNote notes)
            CRes [] (IntVal 0) -> return ()
            CRes notes res -> assertFailure $ "Bad result for:\n \t " ++ show res ++ "\n \tNotes\t" ++ concat (map prettyCNote notes)
          -- step "Codegen"
          -- void (codegen initModule tprgm)

runTests :: Bool -> [(String, String)] -> IO ()
runTests includeStd testFiles = defaultMain $ testGroup "Tests" testTrees
  where testTrees = map (uncurry (runTest includeStd)) testFiles

test :: IO ()
test = runTests False [("Test", "test/code/test.ct")]

standardTests :: H.HashMap String String
standardTests = H.fromList [ ("Syntax", "test/code/syntax.ct")
  , ("Arithmetic", "test/code/arith.ct")
  , ("Id", "test/code/id.ct")
  , ("Cond", "test/code/cond.ct")
  , ("Rec", "test/code/rec.ct")
  , ("Fib", "test/code/fib.ct")
  , ("IfThenElse", "test/code/ifThenElse.ct")
  ]

mt :: String -> IO ()
mt k = case H.lookup k standardTests of
  Just v -> runTests True [(k,v)]
  Nothing -> error "invalid test name"

main :: IO ()
main = runTests True $ H.toList standardTests
