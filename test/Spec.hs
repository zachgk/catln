{-# LANGUAGE BlockArguments #-}
module Main where
import           Integration.Integ
import           Semantics.TypesTests     (typeTests)
import           Syntax.SyntaxTests       (syntaxTests)
import           Test.Tasty
import           Typecheck.TypeCheckTests (typecheckTests)

main :: IO ()
main = do
  integrationTests' <- integrationTests
  typeTests' <- typeTests
  syntaxTests' <- syntaxTests
  let catlnTests = testGroup "CatlnTests" [integrationTests', typeTests', typecheckTests, syntaxTests']
  defaultMain catlnTests
