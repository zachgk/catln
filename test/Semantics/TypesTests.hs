module Semantics.TypesTests where

import           Common.TestCommon   (findCt)
import           Control.Monad
import           CRes                (fromCRes)
import qualified Data.HashMap.Strict as H
import           Data.Maybe
import           Hedgehog
import qualified Hedgehog.Gen        as HG
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Desugarf  (desFiles)
import           Syntax.Parsers      (mkRawCanonicalImportStr, readFiles)
import           Test.Tasty
import           Test.Tasty.Hedgehog as HG
import           Testing.Generation
import           Text.Printf
import           TypeCheck           (typecheckPrgm)
import           Utils

type Prgms = H.HashMap String (Prgm Expr ())
type GenPrgm = Gen (Prgm Expr ())

findPrgms :: IO Prgms
findPrgms = do
  let classGraphDir = "test/Semantics/code/"
  fileNames <- findCt classGraphDir
  prgms <- forM fileNames $ \fileName -> do
    fileName' <- mkRawCanonicalImportStr fileName
    rawPrgm <- fromCRes <$> readFiles False [fileName']
    let prgm = fromCRes $ desFiles rawPrgm
    let tprgm = fromCRes $ typecheckPrgm prgm
    return (fileName, mergePrgms $ map fst3 $ graphToNodes tprgm)
  return (H.insert "empty" emptyPrgm $ H.fromList prgms)

ggPrgm :: Prgms -> GenPrgm
ggPrgm prgms = HG.choice [genPremade, genPrgm]
  where
    genPremade = do
      prgmName <- HG.element $ H.keys prgms
      return $ fromJust $ H.lookup prgmName prgms

propCompactNoChanges :: GenPrgm -> Property
propCompactNoChanges gPrgm = property $ do
  prgm <- forAll gPrgm
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  let compacted = compactType typeEnv H.empty a
  annotate $ printf "compacted = %s" (show compacted)
  assert $ isEqType typeEnv a compacted

propCompactIdempotent :: GenPrgm -> Property
propCompactIdempotent gPrgm = property $ do
  prgm <- forAll gPrgm
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  let compact1 = compactType typeEnv H.empty a
  let compact2 = compactType typeEnv H.empty compact1
  annotate $ printf "compact once to %s" (show compact1)
  annotate $ printf "compact twice to %s" (show compact2)
  compact1 === compact2


propExpandEq :: GenPrgm -> Property
propExpandEq gPrgm = property $ do
  prgm <- forAll gPrgm
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  let expanded = expandType typeEnv H.empty a
  annotate $ printf "expanded = %s" (show expanded)
  assert $ isEqType typeEnv a expanded


propSubtypeByUnion :: GenPrgm -> Property
propSubtypeByUnion gPrgm = property $ do
  prgm <- forAll gPrgm
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  let subtype = isSubtypeOf typeEnv a b
  annotate $ printf "subtype = %s" (show subtype)
  let byUnion = isEqType typeEnv (unionTypes typeEnv a b) b
  annotate $ printf "a∪b = %s" (show $ unionTypes typeEnv a b)
  annotate $ printf "byUnion = %s" (show byUnion)
  subtype === byUnion

propSubtypeByIntersection :: GenPrgm -> Property
propSubtypeByIntersection gPrgm = property $ do
  prgm <- forAll gPrgm
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  let subtype = isSubtypeOf typeEnv a b
  annotate $ printf "subtype = %s" (show subtype)
  let byIntersection = isEqType typeEnv (intersectTypes typeEnv a b) a
  annotate $ printf "a∩b = %s" (show $ intersectTypes typeEnv a b)
  annotate $ printf "byIntersection = %s" (show byIntersection)
  subtype === byIntersection

propUnionReflexive :: GenPrgm -> Property
propUnionReflexive gPrgm = property $ do
  prgm <- forAll gPrgm
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  assert $ isEqType typeEnv (unionTypes typeEnv a b) (unionTypes typeEnv b a)

propUnionCommutative :: GenPrgm -> Property
propUnionCommutative gPrgm = property $ do
  prgm <- forAll gPrgm
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm
  assert $ isEqType typeEnv (unionAllTypes typeEnv [a, b, c]) (unionAllTypes typeEnv [c, b, a])

propDifferenceShrinks :: GenPrgm -> Property
propDifferenceShrinks gPrgm = property $ do
  prgm <- forAll gPrgm
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  assert $ isSubtypeOf typeEnv (differenceTypeEnv typeEnv a b) a

propIntersectionReflexive :: GenPrgm -> Property
propIntersectionReflexive gPrgm = property $ do
  prgm <- forAll gPrgm
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  assert $ isEqType typeEnv (intersectTypes typeEnv a b) (intersectTypes typeEnv b a)

propIntersectionCommutative :: GenPrgm -> Property
propIntersectionCommutative gPrgm = property $ do
  prgm <- forAll gPrgm
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm

  let abc = intersectAllTypes typeEnv [a, b, c]
  annotate $ printf "a∩b∩c = %s" (show abc)

  let cba = intersectAllTypes typeEnv [c, b, a]
  annotate $ printf "c∩b∩a = %s" (show cba)

  annotate $ printf "a∩b∩c ⊆ c∩b∩a = %s" (show $ isSubtypeOf typeEnv abc cba)
  annotate $ printf "c∩b∩a ⊆ a∩b∩c = %s" (show $ isSubtypeOf typeEnv cba abc)

  assert $ isEqType typeEnv abc cba

propIntersectionDistributesUnion :: GenPrgm -> Property
propIntersectionDistributesUnion gPrgm = property $ do
  prgm <- forAll gPrgm
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm
  let f = intersectTypes typeEnv a (unionTypes typeEnv b c)
  annotate $ printf "f = %s" (show f)
  let d = unionTypes typeEnv (intersectTypes typeEnv a b) (intersectTypes typeEnv a c)
  annotate $ printf "a∩b = %s" (show $ intersectTypes typeEnv a b)
  annotate $ printf "a∩c = %s" (show $ intersectTypes typeEnv a c)
  annotate $ printf "d = %s" (show d)
  assert $ isEqType typeEnv f d

propUnionDistributesIntersection :: GenPrgm  -> Property
propUnionDistributesIntersection gPrgm = property $ do
  prgm <- forAll gPrgm
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm
  let f = unionTypes typeEnv a (intersectTypes typeEnv b c)
  annotate $ printf "f = %s" (show f)
  let d = intersectTypes typeEnv (unionTypes typeEnv a b) (unionTypes typeEnv a c)
  annotate $ printf "a∪b = %s" (show $ unionTypes typeEnv a b)
  annotate $ printf "a∪c = %s" (show $ unionTypes typeEnv a c)
  annotate $ printf "d = %s" (show d)
  assert $ isEqType typeEnv f d

typeTests :: IO TestTree
typeTests = do
  prgms <- findPrgms
  let gPrgm = ggPrgm prgms
  return $ testGroup "TypeTests" [
    HG.testProperty "(propCompactNoChanges gPrgm)" (p $ propCompactNoChanges gPrgm)
    , HG.testProperty "(propCompactIdempotent gPrgm)" (p $ propCompactIdempotent gPrgm)
    , HG.testProperty "(propExpandEq gPrgm)" (p $ propExpandEq gPrgm)
    , HG.testProperty "(propSubtypeByUnion gPrgm)" (p $ propSubtypeByUnion gPrgm)
    , HG.testProperty "(propSubtypeByIntersection gPrgm)" (p $ propSubtypeByIntersection gPrgm)
    , HG.testProperty "(propUnionReflexive gPrgm)" (p $ propUnionReflexive gPrgm)
    , HG.testProperty "(propUnionCommutative gPrgm)" (p $ propUnionCommutative gPrgm)
    -- , HG.testProperty "(propDifferenceShrinks gPrgm)" (p $ propDifferenceShrinks gPrgm)
    , HG.testProperty "(propIntersectionReflexive gPrgm)" (p $ propIntersectionReflexive gPrgm)
    , HG.testProperty "(propIntersectionCommutative gPrgm)" (p $ propIntersectionCommutative gPrgm)
    , HG.testProperty "(propIntersectionDistributesUnion gPrgm)" (p $ propIntersectionDistributesUnion gPrgm)
    , HG.testProperty "(propUnionDistributesIntersection gPrgm)" (p $ propUnionDistributesIntersection gPrgm)
                                  ]
  where
    p prop = prop
    -- p prop = withTests 100000 prop

main :: IO ()
main = do
  ts <- typeTests
  defaultMain ts
