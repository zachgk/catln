module Main where

import           Data.List
import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Monad
import           Desugarf         (desPrgm)
import           Emit             (codegen, initModule)
import           Eval
import           Parser           (parseFile)
import           Syntax
import           TypeCheck
import qualified Data.Text.Lazy as T
import Text.Pretty.Simple

main :: IO ()
main = defaultMain $ testCaseSteps "Add" $ \step -> do
  step "Read file..."
  contents <- readFile "test/code/add.flng"
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
          assertFailure $ "Could not typecheck:\n \t" ++ intercalate "\n\t" err
        Right tprgm -> do
          step "Eval tests..."
          case evalMain tprgm of
            Left err -> assertFailure $ "Could not eval:\n \t " ++ show err
            Right (IntVal 0) -> return ()
            Right err -> assertFailure $ "Bad result for:\n \t " ++ show err
          step "Codegen"
          void (codegen initModule tprgm)
