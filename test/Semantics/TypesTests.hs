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
import           Syntax.Parsers      (readFiles)
import           Test.Tasty
import           Test.Tasty.Hedgehog as HG
import           Testing.Generation
import           Text.Printf
import           TypeCheck           (typecheckPrgm)
import           Utils

type Prgms = H.HashMap String (ExprPrgm Expr ())

findPrgms :: IO Prgms
findPrgms = do
  let classGraphDir = "test/Semantics/code/"
  fileNames <- findCt classGraphDir
  prgms <- forM fileNames $ \fileName -> do
    rawPrgm <- fromCRes <$> readFiles False True [fileName]
    let prgm = fromCRes $ desFiles rawPrgm
    let tprgm = fromCRes $ typecheckPrgm prgm
    return (fileName, mergeExprPrgms $ map fst3 $ graphToNodes tprgm)
  return (H.insert "empty" emptyExprPrgm $ H.fromList prgms)

propCompactNoChanges :: Prgms -> Property
propCompactNoChanges prgms = property $ do
  prgmName <- forAll $ HG.element $ H.keys prgms
  let prgm = fromJust $ H.lookup prgmName prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  let compacted = compactType classGraph a
  annotate $ printf "compacted = %s" (show compacted)
  assert $ isEqType classGraph a compacted

propCompactIdempotent :: Prgms -> Property
propCompactIdempotent prgms = property $ do
  prgmName <- forAll $ HG.element $ H.keys prgms
  let prgm = fromJust $ H.lookup prgmName prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  let compact1 = compactType classGraph a
  let compact2 = compactType classGraph compact1
  annotate $ printf "compact once to %s" (show compact1)
  annotate $ printf "compact twice to %s" (show compact2)
  compact1 === compact2


propExpandEq :: Prgms -> Property
propExpandEq prgms = property $ do
  prgmName <- forAll $ HG.element $ H.keys (H.delete "empty" prgms)
  let prgm = fromJust $ H.lookup prgmName prgms
  let classGraph = snd3 prgm
  a <- forAll $ genPartialType prgm
  let expanded = expandPartial classGraph a
  annotate $ printf "expanded = %s" (show expanded)
  assert $ isEqType classGraph (singletonType a) expanded


propSubtypeByUnion :: Prgms -> Property
propSubtypeByUnion prgms = property $ do
  prgmName <- forAll $ HG.element $ H.keys prgms
  let prgm = fromJust $ H.lookup prgmName prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  let subtype = isSubtypeOf classGraph a b
  annotate $ printf "subtype = %s" (show subtype)
  let byUnion = isEqType classGraph (unionTypes classGraph a b) b
  annotate $ printf "a∪b = %s" (show $ unionTypes classGraph a b)
  annotate $ printf "byUnion = %s" (show byUnion)
  subtype === byUnion

propSubtypeByIntersection :: Prgms -> Property
propSubtypeByIntersection prgms = property $ do
  prgmName <- forAll $ HG.element $ H.keys prgms
  let prgm = fromJust $ H.lookup prgmName prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  let subtype = isSubtypeOf classGraph a b
  annotate $ printf "subtype = %s" (show subtype)
  let byIntersection = isEqType classGraph (intersectTypes classGraph a b) a
  annotate $ printf "a∩b = %s" (show $ intersectTypes classGraph a b)
  annotate $ printf "byIntersection = %s" (show byIntersection)
  subtype === byIntersection

propUnionReflexive :: Prgms -> Property
propUnionReflexive prgms = property $ do
  prgmName <- forAll $ HG.element $ H.keys prgms
  let prgm = fromJust $ H.lookup prgmName prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  assert $ isEqType classGraph (unionTypes classGraph a b) (unionTypes classGraph b a)

propUnionCommutative :: Prgms -> Property
propUnionCommutative prgms = property $ do
  prgmName <- forAll $ HG.element $ H.keys prgms
  let prgm = fromJust $ H.lookup prgmName prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm
  assert $ isEqType classGraph (unionAllTypes classGraph [a, b, c]) (unionAllTypes classGraph [c, b, a])

propIntersectionReflexive :: Prgms -> Property
propIntersectionReflexive prgms = property $ do
  prgmName <- forAll $ HG.element $ H.keys prgms
  let prgm = fromJust $ H.lookup prgmName prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  assert $ isEqType classGraph (intersectTypes classGraph a b) (intersectTypes classGraph b a)

propIntersectionCommutative :: Prgms -> Property
propIntersectionCommutative prgms = property $ do
  prgmName <- forAll $ HG.element $ H.keys prgms
  let prgm = fromJust $ H.lookup prgmName prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm

  let abc = intersectAllTypes classGraph [a, b, c]
  annotate $ printf "a∩b∩c = %s" (show abc)

  let cba = intersectAllTypes classGraph [c, b, a]
  annotate $ printf "c∩b∩a = %s" (show cba)

  annotate $ printf "a∩b∩c ⊆ c∩b∩a = %s" (show $ isSubtypeOf classGraph abc cba)
  annotate $ printf "c∩b∩a ⊆ a∩b∩c = %s" (show $ isSubtypeOf classGraph cba abc)

  assert $ isEqType classGraph abc cba

propIntersectionDistributesUnion :: Prgms -> Property
propIntersectionDistributesUnion prgms = property $ do
  prgmName <- forAll $ HG.element $ H.keys prgms
  let prgm = fromJust $ H.lookup prgmName prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm
  let f = intersectTypes classGraph a (unionTypes classGraph b c)
  annotate $ printf "f = %s" (show f)
  let d = unionTypes classGraph (intersectTypes classGraph a b) (intersectTypes classGraph a c)
  annotate $ printf "a∩b = %s" (show $ intersectTypes classGraph a b)
  annotate $ printf "a∩c = %s" (show $ intersectTypes classGraph a c)
  annotate $ printf "d = %s" (show d)
  assert $ isEqType classGraph f d

propUnionDistributesIntersection :: Prgms -> Property
propUnionDistributesIntersection prgms = property $ do
  prgmName <- forAll $ HG.element $ H.keys prgms
  let prgm = fromJust $ H.lookup prgmName prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm
  let f = unionTypes classGraph a (intersectTypes classGraph b c)
  annotate $ printf "f = %s" (show f)
  let d = intersectTypes classGraph (unionTypes classGraph a b) (unionTypes classGraph a c)
  annotate $ printf "a∪b = %s" (show $ unionTypes classGraph a b)
  annotate $ printf "a∪c = %s" (show $ unionTypes classGraph a c)
  annotate $ printf "d = %s" (show d)
  assert $ isEqType classGraph f d

typeTests :: IO TestTree
typeTests = do
  prgms <- findPrgms
  return $ testGroup "TypeTests" [
    HG.testProperty "(propCompactNoChanges prgms)" (p $ propCompactNoChanges prgms)
    , HG.testProperty "(propCompactIdempotent prgms)" (p $ propCompactIdempotent prgms)
    , HG.testProperty "(propExpandEq prgms)" (p $ propExpandEq prgms)
    , HG.testProperty "(propSubtypeByUnion prgms)" (p $ propSubtypeByUnion prgms)
    , HG.testProperty "(propSubtypeByIntersection prgms)" (p $ propSubtypeByIntersection prgms)
    , HG.testProperty "(propUnionReflexive prgms)" (p $ propUnionReflexive prgms)
    , HG.testProperty "(propUnionCommutative prgms)" (p $ propUnionCommutative prgms)
    , HG.testProperty "(propIntersectionReflexive prgms)" (p $ propIntersectionReflexive prgms)
    , HG.testProperty "(propIntersectionCommutative prgms)" (p $ propIntersectionCommutative prgms)
    , HG.testProperty "(propIntersectionDistributesUnion prgms)" (p $ propIntersectionDistributesUnion prgms)
    , HG.testProperty "(propUnionDistributesIntersection prgms)" (p $ propUnionDistributesIntersection prgms)
                                  ]
  where
    p prop = prop
    -- p prop = withTests 100000 prop

main :: IO ()
main = do
  ts <- typeTests
  defaultMain ts
