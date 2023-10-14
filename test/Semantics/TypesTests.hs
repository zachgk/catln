module Semantics.TypesTests where

import           Common.TestCommon   (findCt)
import           Control.Monad
import           CRes                (fromCRes)
import           Hedgehog
import qualified Hedgehog.Gen        as HG
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Desugarf  (desFiles)
import           Syntax.Parsers      (readFiles)
import           Test.Tasty
import           Test.Tasty.Hedgehog as HG
import           TypeCheck           (typecheckPrgm)
import           Utils
import Data.Maybe
import Text.Printf

type Prgms = [ExprPrgm Expr ()]

findPrgms :: IO Prgms
findPrgms = do
  let classGraphDir = "test/Semantics/code/"
  fileNames <- findCt classGraphDir
  prgms <- forM fileNames $ \fileName -> do
    rawPrgm <- fromCRes <$> readFiles False True [fileName]
    let prgm = fromCRes $ desFiles rawPrgm
    let tprgm = fromCRes $ typecheckPrgm prgm
    return $ mergeExprPrgms $ map fst3 $ graphToNodes tprgm
  return (emptyExprPrgm : prgms)

genTypeFromExpr :: ExprPrgm Expr () -> Expr () -> Gen Type
genTypeFromExpr _ (CExpr _ c) = return $ constantType c
genTypeFromExpr _ (Value _ n) = return $ typeVal $ PTypeName n
genTypeFromExpr _ e = error $ printf "Unimplemented genTypeFromExpr for %s" (show e)

genType :: ExprPrgm Expr () -> Gen Type
genType prgm@(objMap, ClassGraph cg, _) = HG.choice gens
  where

    gens = if graphEmpty cg
      then [genBasic]
      else [genBasic, genCG, genObj]

    genBasic :: Gen Type
    genBasic = HG.element [topType, bottomType]

    genCG :: Gen Type
    genCG = do
      classNode <- HG.element $ graphToNodes cg
      return $ typeVal $ snd3 classNode

    genObj :: Gen Type
    genObj = do
      oa <- HG.element $ objMap
      let (GuardExpr objExpr Nothing) = fromJust $ oaObj oa
      genTypeFromExpr prgm objExpr


propSubtypeByUnion :: Prgms -> Property
propSubtypeByUnion prgms = property $ do
  prgm <- forAll $ HG.element prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  let subtype = isSubtypeOf classGraph a b
  let byUnion = isEqType classGraph (unionTypes classGraph a b) b
  subtype === byUnion

propSubtypeByIntersection :: Prgms -> Property
propSubtypeByIntersection prgms = property $ do
  prgm <- forAll $ HG.element prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  let subtype = isSubtypeOf classGraph a b
  let byIntersection = isEqType classGraph (intersectTypes classGraph a b) a
  subtype === byIntersection

propUnionReflexive :: Prgms -> Property
propUnionReflexive prgms = property $ do
  prgm <- forAll $ HG.element prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  assert $ isEqType classGraph (unionTypes classGraph a b) (unionTypes classGraph b a)

propUnionCommutative :: Prgms -> Property
propUnionCommutative prgms = property $ do
  prgm <- forAll $ HG.element prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm
  assert $ isEqType classGraph (unionAllTypes classGraph [a, b, c]) (unionAllTypes classGraph [c, b, a])

propIntersectionReflexive :: Prgms -> Property
propIntersectionReflexive prgms = property $ do
  prgm <- forAll $ HG.element prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  assert $ isEqType classGraph (intersectTypes classGraph a b) (intersectTypes classGraph b a)

propIntersectionCommutative :: Prgms -> Property
propIntersectionCommutative prgms = property $ do
  prgm <- forAll $ HG.element prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm
  assert $ isEqType classGraph (intersectAllTypes classGraph [a, b, c]) (intersectAllTypes classGraph [c, b, a])

propIntersectionDistributesUnion :: Prgms -> Property
propIntersectionDistributesUnion prgms = property $ do
  prgm <- forAll $ HG.element prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm
  let f = intersectTypes classGraph a (unionTypes classGraph b c)
  let d = unionTypes classGraph (intersectTypes classGraph a b) (intersectTypes classGraph a c)
  assert $ isEqType classGraph f d

propUnionDistributesIntersection :: Prgms -> Property
propUnionDistributesIntersection prgms = property $ do
  prgm <- forAll $ HG.element prgms
  let classGraph = snd3 prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm
  let f = unionTypes classGraph a (intersectTypes classGraph b c)
  let d = intersectTypes classGraph (unionTypes classGraph a b) (unionTypes classGraph a c)
  assert $ isEqType classGraph f d

typeTests :: IO TestTree
typeTests = do
  prgms <- findPrgms
  return $ testGroup "TypeTests" [
    HG.testProperty "Subtype by union" (propSubtypeByUnion prgms)
    , HG.testProperty "Subtype by intersection" (propSubtypeByIntersection prgms)
    , HG.testProperty "Union is Reflexive" (propUnionReflexive prgms)
    , HG.testProperty "Union is Commutative" (propUnionCommutative prgms)
    , HG.testProperty "Intersection is Reflexive" (propIntersectionReflexive prgms)
    , HG.testProperty "Intersection is Commutative" (propIntersectionCommutative prgms)
    , HG.testProperty "Intersection distributes over union" (propIntersectionDistributesUnion prgms)
    , HG.testProperty "Union distributes over intersection" (propUnionDistributesIntersection prgms)
                                  ]

main :: IO ()
main = do
  ts <- typeTests
  defaultMain ts
