module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Monad
import           Desugarf         (desPrgm)
import           Emit             (codegen, initModule)
import           Eval
import           Parser           (parseFile)
import           Syntax
import           TypeCheck        (typecheckPrgm)

main :: IO ()
main = defaultMain $ testCaseSteps "Add" $ \step -> do
  step "Read file..."
  contents <- readFile "test/code/add.flng"
  case parseFile contents of
    Left err -> assertFailure $ "Could not parse" ++ show err
    Right rprgm -> do
      step "Desgugar..."
      let prgm = desPrgm rprgm
      step "Typecheck..."
      case typecheckPrgm prgm of
        Left err -> assertFailure $ "Could not typecheck " ++ show err
        Right tprgm -> do
          step "Eval tests..."
          forM_ prgm $ \decl -> do
            let name = getDeclName decl
            step $ "Eval " ++ name ++ "..."
            case evalDecl decl of
              Left err -> assertFailure $ "Could not eval " ++ name ++ show err
              Right (BoolVal True) -> return ()
              Right err -> assertFailure $ "Bad result for " ++ name ++ show err
          step "Codegen"
          void (codegen initModule tprgm)
