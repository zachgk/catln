{-# LANGUAGE BlockArguments #-}
module Main where
import           Integration.Integ
import           Semantics.TypesTests     (typeTests)
import           Stack.StackTests         (stackTests)
import           Syntax.SyntaxTests       (syntaxTests)
import           Test.Tasty
import           Typecheck.TypeCheckTests (typecheckTests)

main :: IO ()
main = do
  integrationTests' <- integrationTests
  syntaxTests' <- syntaxTests
  stackTests' <- stackTests
  let catlnTests = testGroup "CatlnTests" [integrationTests', typeTests, typecheckTests, syntaxTests', stackTests']
  defaultMain catlnTests
