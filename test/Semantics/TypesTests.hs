module Semantics.TypesTests where

import           Semantics.Types
import           Test.Tasty
import           Test.Tasty.HUnit (assertBool, testCase)

typeTests :: TestTree
typeTests = testGroup "TypeTests" [
  testCase "TopTypeSubtype" $ assertBool "TopType is subtype of TopType" (isSubtypeOf emptyClassGraph topType topType)
                                  ]
