{-# LANGUAGE BlockArguments #-}
module Main where
import           Integration.Integ
import           Semantics.TypesTests (typeTests)
import           Test.Tasty
import           Text.Printf
import           WebDocs              (docApi)


mt :: String -> IO ()
mt k = do
  let fileName = testDir ++ k ++ ".ct"
  tests <- standardTests
  if fileName `elem` tests
     then defaultMain $ runTests True [fileName]
     else error $ printf "invalid test name %s in %s" fileName (show tests)

mtd :: String -> IO ()
mtd k = do
  let fileName = testDir ++ k ++ ".ct"
  tests <- standardTests
  if fileName `elem` tests
     then docApi False True fileName
     else error $ printf "invalid test name %s in %s" fileName (show tests)

main :: IO ()
main = do
  integrationTests' <- integrationTests
  let catlnTests = testGroup "CatlnTests" [integrationTests', typeTests]
  defaultMain catlnTests
