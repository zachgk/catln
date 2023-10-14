module Semantics.TypesTests where

import           Control.Monad
import           CRes                (fromCRes)
import           Hedgehog            (Gen, Property, assert, forAll, property)
import           Hedgehog.Gen        (element)
import           Semantics.Prgm      (mergeExprPrgms)
import           Semantics.Types
import           Syntax.Ct.Desugarf  (desFiles)
import           Syntax.Parsers      (readFiles)
import           Test.Tasty
import           Test.Tasty.Hedgehog as HG
import           TestCommon          (findCt)
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
genType _ = element [topType, bottomType]

propUnionReflexive :: ClassGraphs -> Property
propUnionReflexive classGraphs = property $ do
  classGraph <- forAll $ element classGraphs
  a <- forAll $ genType classGraph
  b <- forAll $ genType classGraph
  assert $ isEqType classGraph (unionTypes classGraph a b) (unionTypes classGraph b a)

propIntersectionReflexive :: ClassGraphs -> Property
propIntersectionReflexive classGraphs = property $ do
  classGraph <- forAll $ element classGraphs
  a <- forAll $ genType classGraph
  b <- forAll $ genType classGraph
  assert $ isEqType classGraph (intersectTypes classGraph a b) (intersectTypes classGraph b a)

typeTests :: IO TestTree
typeTests = do
  classGraphs <- findClassGraphs
  return $ testGroup "TypeTests" [
    HG.testProperty "Union is Reflexive" (propUnionReflexive classGraphs)
    , HG.testProperty "Intersection is Reflexive" (propIntersectionReflexive classGraphs)
                                  ]
