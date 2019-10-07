module Main where

import Test.Tasty
import Test.Tasty.HUnit

import           Eval
import           Parser                   (parseFile)
import           Desugarf (desPrgm)
import           TypeCheck (typecheckPrgm)
import           Syntax
import           Control.Monad

main :: IO ()
main = defaultMain $ testCaseSteps "Add" $ \step -> do
  step "Read file..."
  contents <- readFile "test/code/add.flng"
  case parseFile contents of
    Left err -> assertFailure $ "Could not parse" ++ show err
    Right rprgm -> do
      step "Desgugar..."
      let prgm = desPrgm rprgm
      step "Typecheck"
      case typecheckPrgm prgm of
        Left err -> assertFailure $ "Could not typecheck " ++ show err
        Right result -> do
          step "Eval tests..."
          forM_ prgm $ \decl -> do
            let name = getDeclName decl
            step $ "Eval " ++ name ++ "..."
            case evalDecl decl of
              Left err -> assertFailure $ "Could not eval " ++ name ++ show err
              Right (BoolVal True) -> return ()
              Right err -> assertFailure $ "Bad result for " ++ name ++ show err
