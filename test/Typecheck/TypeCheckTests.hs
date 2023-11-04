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
import qualified Hedgehog.Gen        as HG
import           Semantics
import           Semantics.Prgm      (mergeExprPrgms)
import           Test.Tasty
import qualified Test.Tasty.Hedgehog as HG
import           Testing.Generation  (genPrgms)
import           Text.Printf
import           TypeCheck           (typecheckPrgm)
import           TypeCheck.Common    (tcreToMaybe, feCons)
import           TypeCheck.Constrain (executeConstraint)
import           TypeCheck.Decode    (decodeExprPrgms)
import           TypeCheck.Encode    (fromPrgms, makeBaseFEnv)
import           Utils

propExprEncodeDecode :: Property
propExprEncodeDecode = property $ do
  prgmsNodes <- forAll genPrgms
  let prgms = map fst3 prgmsNodes
  let classGraph = snd3 $ mergeExprPrgms prgms
  let fenv = makeBaseFEnv classGraph
  (encoded, fenv') <- evalMaybe $ tcreToMaybe $ fromPrgms fenv (map fromExprPrgm prgms) []
  decoded <- evalMaybe $ tcreToMaybe $ decodeExprPrgms fenv' encoded
  annotate $ printf "Decoded to: \n\t%s" (show decoded)
  map fst3 prgms === map fst3 decoded

propConstraint :: Property
propConstraint = property $ do
  prgmsNodes <- forAll genPrgms
  let prgms = map fst3 prgmsNodes
  let classGraph = snd3 $ mergeExprPrgms prgms
  let fenv = makeBaseFEnv classGraph
  (_, fenv') <- evalMaybe $ tcreToMaybe $ fromPrgms fenv (map fromExprPrgm prgms) []
  let cons = feCons fenv'
  con <- forAll $ HG.element cons
  let _fenv'' = executeConstraint fenv' con
  return ()

propTypeChecks :: Property
propTypeChecks = property $ do
  prgms <- forAll genPrgms
  let tprgm = typecheckPrgm $ graphFromEdges prgms
  case tprgm of
    CRes{}    -> return ()
    CErr errs -> fail $ prettyCNotes errs

typecheckTests :: TestTree
typecheckTests = testGroup "TypeCheckTests" [
    HG.testProperty "propExprEncodeDecode" (p propExprEncodeDecode)
    , HG.testProperty "propConstraint" (p propConstraint)
    , HG.testProperty "propTypeChecks" (p propTypeChecks)
                                  ]
  where
    p prop = prop
    -- p prop = withTests 100000 prop

main :: IO ()
main = defaultMain typecheckTests
