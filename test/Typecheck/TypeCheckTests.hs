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
import           TypeCheck           (runConstraintsLimit, typecheckPrgm)
import           TypeCheck.Common    (FEnv (FEnv, feCons), tcreToMaybe)
import           TypeCheck.Constrain (executeConstraint, runConstraints)
import           TypeCheck.Decode    (decodeExprPrgms, toPrgms)
import           TypeCheck.Encode    (fromPrgms, makeBaseFEnv)
import           Utils

propEncodeDecode :: Property
propEncodeDecode = property $ do
  prgmsNodes <- forAll genPrgms
  let prgms = map fst3 prgmsNodes
  let classGraph = snd3 $ mergeExprPrgms prgms
  let fenv = makeBaseFEnv classGraph
  (encoded, fenv') <- evalMaybe $ tcreToMaybe $ fromPrgms fenv (map fromExprPrgm prgms) []
  decoded <- evalMaybe $ tcreToMaybe $ toPrgms fenv' encoded
  annotate $ printf "Decoded to: \n\t%s" (show decoded)
  map fst3 prgms === map (fst3 . toExprPrgm) decoded

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

propConstrainMatchesNew :: Property
propConstrainMatchesNew = property $ do
  prgmsNodes <- forAll genPrgms
  let prgms = map fst3 prgmsNodes
  let classGraph = snd3 $ mergeExprPrgms prgms
  let fenv = makeBaseFEnv classGraph
  (encoded, fenv'@FEnv{feCons}) <- evalMaybe $ tcreToMaybe $ fromPrgms fenv (map fromExprPrgm prgms) []
  fenv'' <- evalMaybe $ tcreToMaybe $ runConstraints runConstraintsLimit fenv' feCons
  decodedByOld <- evalMaybe $ tcreToMaybe $ toPrgms fenv'' encoded
  annotate $ printf "Decoded by old to: \n\t%s" (show $ map toExprPrgm decodedByOld)
  decodedByNew <- evalMaybe $ tcreToMaybe $ decodeExprPrgms fenv'' encoded
  annotate $ printf "Decoded by new to: \n\t%s" (show decodedByNew)
  map (fst3 . toExprPrgm) decodedByOld === map fst3 decodedByNew

propTypeChecks :: Property
propTypeChecks = property $ do
  prgms <- forAll genPrgms
  let tprgm = typecheckPrgm $ graphFromEdges prgms
  case tprgm of
    CRes{}    -> return ()
    CErr errs -> fail $ prettyCNotes errs

typecheckTests :: TestTree
typecheckTests = testGroup "TypeCheckTests" [
    HG.testProperty "propEncodeDecode" (p propEncodeDecode)
    , HG.testProperty "propExprEncodeDecode" (p propExprEncodeDecode)
    , HG.testProperty "propConstraint" (p propConstraint)
    , HG.testProperty "propConstrainMatchesNew" (p propConstrainMatchesNew)
    , HG.testProperty "propTypeChecks" (p propTypeChecks)
                                  ]
  where
    p prop = prop
    -- p prop = withTests 100000 prop

main :: IO ()
main = defaultMain typecheckTests
