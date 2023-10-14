module Semantics.TypesTests where

import           Common.TestCommon   (findCt)
import           Control.Monad
import           CRes                (fromCRes)
import           Hedgehog
import qualified Hedgehog.Gen        as HG
import           Semantics.Prgm      (mergeExprPrgms)
import           Semantics.Types
import           Syntax.Ct.Desugarf  (desFiles)
import           Syntax.Parsers      (readFiles)
import           Test.Tasty
import           Test.Tasty.Hedgehog as HG
import           TypeCheck           (typecheckPrgm)
import           Utils

type ClassGraphs = [ClassGraph]

findClassGraphs :: IO ClassGraphs
findClassGraphs = do
  let classGraphDir = "test/Semantics/classGraphs/"
  fileNames <- findCt classGraphDir
  classGraphs <- forM fileNames $ \fileName -> do
    rawPrgm <- fromCRes <$> readFiles False True [fileName]
    let prgm = fromCRes $ desFiles rawPrgm
    let tprgm = fromCRes $ typecheckPrgm prgm
    return $ snd3 $ mergeExprPrgms $ map fst3 $ graphToNodes tprgm
  return (emptyClassGraph : classGraphs)

genType :: ClassGraph -> Gen Type
genType (ClassGraph cg) = HG.choice $ genBasic: [genCG | not (graphEmpty cg)]
  where
    genBasic :: Gen Type
    genBasic = HG.element [topType, bottomType]

    genCG :: Gen Type
    genCG = do
      classNode <- HG.element $ graphToNodes cg
      return $ typeVal $ snd3 classNode

propSubtypeByUnion :: ClassGraphs -> Property
propSubtypeByUnion classGraphs = property $ do
  classGraph <- forAll $ HG.element classGraphs
  a <- forAll $ genType classGraph
  b <- forAll $ genType classGraph
  let subtype = isSubtypeOf classGraph a b
  let byUnion = isEqType classGraph (unionTypes classGraph a b) b
  subtype === byUnion

propSubtypeByIntersection :: ClassGraphs -> Property
propSubtypeByIntersection classGraphs = property $ do
  classGraph <- forAll $ HG.element classGraphs
  a <- forAll $ genType classGraph
  b <- forAll $ genType classGraph
  let subtype = isSubtypeOf classGraph a b
  let byIntersection = isEqType classGraph (intersectTypes classGraph a b) a
  subtype === byIntersection

propUnionReflexive :: ClassGraphs -> Property
propUnionReflexive classGraphs = property $ do
  classGraph <- forAll $ HG.element classGraphs
  a <- forAll $ genType classGraph
  b <- forAll $ genType classGraph
  assert $ isEqType classGraph (unionTypes classGraph a b) (unionTypes classGraph b a)

propUnionCommutative :: ClassGraphs -> Property
propUnionCommutative classGraphs = property $ do
  classGraph <- forAll $ HG.element classGraphs
  a <- forAll $ genType classGraph
  b <- forAll $ genType classGraph
  c <- forAll $ genType classGraph
  assert $ isEqType classGraph (unionAllTypes classGraph [a, b, c]) (unionAllTypes classGraph [c, b, a])

propIntersectionReflexive :: ClassGraphs -> Property
propIntersectionReflexive classGraphs = property $ do
  classGraph <- forAll $ HG.element classGraphs
  a <- forAll $ genType classGraph
  b <- forAll $ genType classGraph
  assert $ isEqType classGraph (intersectTypes classGraph a b) (intersectTypes classGraph b a)

propIntersectionCommutative :: ClassGraphs -> Property
propIntersectionCommutative classGraphs = property $ do
  classGraph <- forAll $ HG.element classGraphs
  a <- forAll $ genType classGraph
  b <- forAll $ genType classGraph
  c <- forAll $ genType classGraph
  assert $ isEqType classGraph (intersectAllTypes classGraph [a, b, c]) (intersectAllTypes classGraph [c, b, a])

propIntersectionDistributesUnion :: ClassGraphs -> Property
propIntersectionDistributesUnion classGraphs = property $ do
  classGraph <- forAll $ HG.element classGraphs
  a <- forAll $ genType classGraph
  b <- forAll $ genType classGraph
  c <- forAll $ genType classGraph
  let f = intersectTypes classGraph a (unionTypes classGraph b c)
  let d = unionTypes classGraph (intersectTypes classGraph a b) (intersectTypes classGraph a c)
  assert $ isEqType classGraph f d

propUnionDistributesIntersection :: ClassGraphs -> Property
propUnionDistributesIntersection classGraphs = property $ do
  classGraph <- forAll $ HG.element classGraphs
  a <- forAll $ genType classGraph
  b <- forAll $ genType classGraph
  c <- forAll $ genType classGraph
  let f = unionTypes classGraph a (intersectTypes classGraph b c)
  let d = intersectTypes classGraph (unionTypes classGraph a b) (unionTypes classGraph a c)
  assert $ isEqType classGraph f d

typeTests :: IO TestTree
typeTests = do
  classGraphs <- findClassGraphs
  return $ testGroup "TypeTests" [
    HG.testProperty "Subtype by union" (propSubtypeByUnion classGraphs)
    , HG.testProperty "Subtype by intersection" (propSubtypeByIntersection classGraphs)
    , HG.testProperty "Union is Reflexive" (propUnionReflexive classGraphs)
    , HG.testProperty "Union is Commutative" (propUnionCommutative classGraphs)
    , HG.testProperty "Intersection is Reflexive" (propIntersectionReflexive classGraphs)
    , HG.testProperty "Intersection is Commutative" (propIntersectionCommutative classGraphs)
    , HG.testProperty "Intersection distributes over union" (propIntersectionDistributesUnion classGraphs)
    , HG.testProperty "Union distributes over intersection" (propUnionDistributesIntersection classGraphs)
                                  ]
