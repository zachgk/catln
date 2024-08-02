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
import           MapMeta
import           Semantics.Prgm      (mergePrgms)
import           Test.Tasty
import qualified Test.Tasty.Hedgehog as THG
import           Testing.Generation  (genPrgms)
import           Text.Printf
import           TypeCheck           (typecheckPrgm)
import           TypeCheck.Common    (feCons, tcreToMaybe)
import           TypeCheck.Constrain (executeConstraint)
import           TypeCheck.Decode    (toPrgms)
import           TypeCheck.Encode    (fromPrgms, makeBaseFEnv)
import           Utils
import Control.Monad.State (execStateT)

propExprEncodeDecode :: Property
propExprEncodeDecode = property $ do
  prgmsNodes <- forAll genPrgms
  let prgms = map fst3 prgmsNodes
  annotate $ printf "Programs: \n\t%s" (show prgms)
  let prgm = mergePrgms prgms
  let fenv = makeBaseFEnv prgm
  (encoded, fenv') <- evalMaybe $ tcreToMaybe $ fromPrgms fenv prgms []
  decoded <- evalMaybe $ tcreToMaybe $ toPrgms fenv' encoded
  let decodedClear = map (mapMetaPrgm clearMetaDat) decoded
  annotate $ printf "Decoded to: \n\t%s" (show decoded)
  map fst3 prgms === map fst3 decodedClear

propConstraint :: Property
propConstraint = property $ do
  prgmsNodes <- forAll genPrgms
  let prgms = map fst3 prgmsNodes
  let prgm = mergePrgms prgms
  let fenv = makeBaseFEnv prgm
  (_, fenv') <- evalMaybe $ tcreToMaybe $ fromPrgms fenv prgms []
  let cons = feCons fenv'
  con <- forAll $ HG.element cons
  let _fenv'' = execStateT (executeConstraint con) fenv'
  return ()

propConstraints :: Property
propConstraints = property $ do
  prgmsNodes <- forAll genPrgms
  let prgms = map fst3 prgmsNodes
  let prgm = mergePrgms prgms
  let fenv = makeBaseFEnv prgm
  (_, fenv') <- evalMaybe $ tcreToMaybe $ fromPrgms fenv prgms []
  let cons = feCons fenv'
  usedCons <- forAll $ HG.subsequence (concat $ replicate 4 cons)
  let _fenv'' = execStateT (mapM executeConstraint usedCons) fenv'
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
    -- THG.testProperty "propExprEncodeDecode" (p propExprEncodeDecode) -- TODO: Debug
    THG.testProperty "propConstraint" (p propConstraint)
    , THG.testProperty "propConstraints" (p propConstraints)
    , THG.testProperty "propTypeChecks" (p propTypeChecks)
                                  ]
  where
    p prop = prop
    -- p prop = withTests 10000 prop

main :: IO ()
main = defaultMain typecheckTests
