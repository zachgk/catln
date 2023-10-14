module Semantics.TypesTests where

import           Common.TestCommon   (findCt)
import           Control.Monad
import           CRes                (fromCRes)
import           Hedgehog
import qualified Hedgehog.Gen        as HG
import           Semantics.Prgm
import qualified Data.HashMap.Strict as H
import           Semantics.Types
import           Syntax.Ct.Desugarf  (desFiles)
import           Syntax.Parsers      (readFiles)
import           Test.Tasty
import           Test.Tasty.Hedgehog as HG
import           TypeCheck           (typecheckPrgm)
import           Utils
import Data.Maybe
import Text.Printf
import Semantics (getExprType, oaObjPath)

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

genTypeFromExpr :: ExprPrgm Expr () -> Expr () -> Gen PartialType
genTypeFromExpr _ (CExpr _ c) = return $ constantPartialType c
genTypeFromExpr _ (Value _ n) = return $ partialVal $ PTypeName n
genTypeFromExpr prgm (TupleApply _ (_, baseExpr) oa) = do
  base@PartialType{ptArgs=baseArgs} <- genTypeFromExpr prgm baseExpr
  shouldAddArg <- HG.bool
  if shouldAddArg
    then case oaArr oa of
           Just (GuardExpr arrExpr _) -> do
             arrExpr' <- genTypeFromExpr prgm arrExpr
             return base{ptArgs = H.insert (oaObjPath oa) (singletonType arrExpr') baseArgs}
           Nothing -> return base{ptArgs = H.insert (oaObjPath oa) (getMetaType $ oaM oa) baseArgs}
    else return base
genTypeFromExpr prgm (VarApply _ baseExpr varName m) = do
  base@PartialType{ptVars=baseVars} <- genTypeFromExpr prgm baseExpr
  shouldAddVar <- HG.bool
  return $ if shouldAddVar
    then base{ptVars = H.insert varName (getMetaType m) baseVars}
    else base
genTypeFromExpr _ e = error $ printf "Unimplemented genTypeFromExpr for %s" (show e)

genType :: ExprPrgm Expr () -> Gen Type
genType prgm@(objMap, ClassGraph cg, _) = HG.choice gens
  where

    gens = if graphEmpty cg
      then [genBasic]
      else [genBasic, genCG, genObjM, genObj]

    genBasic :: Gen Type
    genBasic = HG.element [topType, bottomType]

    genCG :: Gen Type
    genCG = do
      classNode <- HG.element $ graphToNodes cg
      return $ typeVal $ snd3 classNode

    genObjM :: Gen Type
    genObjM = do
      oa <- HG.element objMap
      let (GuardExpr objExpr Nothing) = fromJust $ oaObj oa
      return $ getExprType objExpr

    genObj :: Gen Type
    genObj = do
      oa <- HG.element objMap
      let (GuardExpr objExpr Nothing) = fromJust $ oaObj oa
      singletonType <$> genTypeFromExpr prgm objExpr


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
  assert $ isEqType classGraph (intersectAllTypes classGraph [a, b, c]) (intersectAllTypes classGraph [c, b, a])

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
    HG.testProperty "(propSubtypeByUnion prgms)" (propSubtypeByUnion prgms)
    , HG.testProperty "(propSubtypeByIntersection prgms)" (propSubtypeByIntersection prgms)
    , HG.testProperty "(propUnionReflexive prgms)" (propUnionReflexive prgms)
    , HG.testProperty "(propUnionCommutative prgms)" (propUnionCommutative prgms)
    , HG.testProperty "(propIntersectionReflexive prgms)" (propIntersectionReflexive prgms)
    , HG.testProperty "(propIntersectionCommutative prgms)" (propIntersectionCommutative prgms)
    , HG.testProperty "(propIntersectionDistributesUnion prgms)" (propIntersectionDistributesUnion prgms)
    , HG.testProperty "(propUnionDistributesIntersection prgms)" (propUnionDistributesIntersection prgms)
                                  ]

main :: IO ()
main = do
  ts <- typeTests
  defaultMain ts
