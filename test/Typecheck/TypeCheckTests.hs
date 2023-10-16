--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.TypeCheckTests
-- Copyright :  (c) Zach Kimberg 2023
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This is the module for testing typechecking.
--------------------------------------------------------------------
module Typecheck.TypeCheckTests where
import           CRes
import           Data.Graph          (graphFromEdges)
import           Hedgehog
import           Test.Tasty
import qualified Test.Tasty.Hedgehog as HG
import           Testing.Generation  (genPrgms)
import           TypeCheck           (typecheckPrgm)

propTypeChecks :: Property
propTypeChecks = property $ do
  prgms <- forAll genPrgms
  let tprgm = typecheckPrgm $ graphFromEdges prgms
  case tprgm of
    CRes{}    -> return ()
    CErr errs -> fail $ prettyCNotes errs

typecheckTests :: TestTree
typecheckTests = testGroup "TypeCheckTests" [
    HG.testProperty "propTypeChecks" (p propTypeChecks)
                                  ]
  where
    p prop = prop
    -- p prop = withTests 100000 prop
