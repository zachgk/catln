{-# LANGUAGE BlockArguments #-}
module Main where
import           Integration.Integ
import           Semantics.TypesTests     (typeTests)
import           Test.Tasty
import           Typecheck.TypeCheckTests (typecheckTests)

main :: IO ()
main = do
  integrationTests' <- integrationTests
  typeTests' <- typeTests
  let catlnTests = testGroup "CatlnTests" [integrationTests', typeTests', typecheckTests]
  defaultMain catlnTests
