module Semantics.TypesTests where

import           Common.TestCommon         (findCt)
import           Control.Monad
import           Control.Monad.Trans       (lift)
import           CRes
import           CtService
import qualified Data.HashMap.Strict       as H
import qualified Data.HashSet              as S
import           Data.List                 (isInfixOf)
import           Data.Maybe
import           Hedgehog
import qualified Hedgehog.Gen              as HG
import           MapMeta                   (clearMetaDat, mapMetaPrgm)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Test.Tasty
import           Test.Tasty.Hedgehog       as HG
import qualified Test.Tasty.Patterns.Types as TT
import qualified Test.Tasty.Runners        as TP
import           Testing.Generation
import           Text.Printf
import           Utils

type Prgms = H.HashMap String (Prgm Expr ())
type GenPrgm = Gen (Prgm Expr ())

findPrgms :: IO Prgms
findPrgms = do
  let classGraphDir = "test/Semantics/code/"
  fileNames <- findCt classGraphDir
  prgms <- runCResT $ do
    forM fileNames $ \fileName -> do
      ctss <- lift $ ctssBaseFiles testCtssConfig [fileName]
      tprgm <- getTPrgm ctss
      return (fileName, mconcat $ map (mapMetaPrgm clearMetaDat . fst3) (graphToNodes tprgm))
  return (H.insert "empty" mempty $ H.fromList $ fromCRes prgms)

ggPrgm :: Prgms -> GenPrgm
ggPrgm prgms = HG.choice [genPremade, genPrgm]
  where
    genPremade = do
      prgmName <- HG.element $ H.keys prgms
      return $ fromJust $ H.lookup prgmName prgms

propCompactNoChanges :: IO GenPrgm -> Property
propCompactNoChanges gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  let compacted = compactType typeEnv H.empty a
  annotate $ printf "compacted = %s" (show compacted)
  assert $ isEqType typeEnv a compacted

propCompactIdempotent :: IO GenPrgm -> Property
propCompactIdempotent gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  let compact1 = compactType typeEnv H.empty a
  let compact2 = compactType typeEnv H.empty compact1
  annotate $ printf "compact once to %s" (show compact1)
  annotate $ printf "compact twice to %s" (show compact2)
  compact1 === compact2



propSubtypeByUnion :: IO GenPrgm -> Property
propSubtypeByUnion gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  let subtype = isSubtypeOf typeEnv a b
  annotate $ printf "subtype = %s" (show subtype)
  let byUnion = isEqType typeEnv (unionTypes typeEnv a b) b
  annotate $ printf "a∪b = %s" (show $ unionTypes typeEnv a b)
  annotate $ printf "byUnion = %s" (show byUnion)
  subtype === byUnion

propSubtypeByIntersection :: IO GenPrgm -> Property
propSubtypeByIntersection gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  let subtype = isSubtypeOf typeEnv a b
  annotate $ printf "subtype = %s" (show subtype)
  let byIntersection = isEqType typeEnv (intersectTypes typeEnv a b) a
  annotate $ printf "a∩b = %s" (show $ intersectTypes typeEnv a b)
  annotate $ printf "byIntersection = %s" (show byIntersection)
  subtype === byIntersection

propUnionReflexive :: IO GenPrgm -> Property
propUnionReflexive gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  assert $ isEqType typeEnv (unionTypes typeEnv a b) (unionTypes typeEnv b a)

propUnionCommutative :: IO GenPrgm -> Property
propUnionCommutative gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm
  assert $ isEqType typeEnv (unionAllTypes typeEnv [a, b, c]) (unionAllTypes typeEnv [c, b, a])

propIntersectionReflexive :: IO GenPrgm -> Property
propIntersectionReflexive gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  assert $ isEqType typeEnv (intersectTypes typeEnv a b) (intersectTypes typeEnv b a)

propIntersectionCommutative :: IO GenPrgm -> Property
propIntersectionCommutative gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
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

propIntersectionDistributesUnion :: IO GenPrgm -> Property
propIntersectionDistributesUnion gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
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

propUnionDistributesIntersection :: IO GenPrgm  -> Property
propUnionDistributesIntersection gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm
  let f = unionTypes typeEnv a (intersectTypes typeEnv b c)
  annotate $ printf "f = %s" (show f)
  let ab = unionTypes typeEnv a b
  let ac = unionTypes typeEnv a c
  annotate $ printf "a∪b = %s" (show ab)
  annotate $ printf "a∪c = %s" (show ac)
  let d = intersectTypes typeEnv ab ac
  annotate $ printf "d = %s" (show d)
  assert $ isEqType typeEnv f d

propDifferenceShrinks :: IO GenPrgm -> Property
propDifferenceShrinks gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  let diff' = differenceTypeEnv typeEnv a b
  annotate $ printf "diff' = %s" (show diff')
  assert $ isSubtypeOf typeEnv diff' a

-- A∪A=U
propUnionWithComplement :: IO GenPrgm -> Property
propUnionWithComplement gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  let a' = complementTypeEnv typeEnv H.empty a
  annotate $ printf "a' = %s" (show a')
  let combined = unionTypes typeEnv a a'
  annotate $ printf "combined = %s" (show combined)
  assert $ isEqType typeEnv combined PTopType

-- A∩A=∅
propIntersectionWithComplement :: IO GenPrgm -> Property
propIntersectionWithComplement gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  let a' = complementTypeEnv typeEnv H.empty a
  annotate $ printf "a' = %s" (show a')
  let combined = intersectTypes typeEnv a a'
  annotate $ printf "combined = %s" (show combined)
  assert $ isEqType typeEnv combined BottomType

-- A^c^c=A
propComplementInverse :: IO GenPrgm -> Property
propComplementInverse gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  let a' = complementTypeEnv typeEnv H.empty a
  annotate $ printf "a' = %s" (show a')
  let a'' = complementTypeEnv typeEnv H.empty a'
  annotate $ printf "a'' = %s" (show a'')
  assert $ isEqType typeEnv a a''

-- A-A=∅
propDifferenceIdentity :: IO GenPrgm -> Property
propDifferenceIdentity gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  let a' = complementTypeEnv typeEnv H.empty a
  annotate $ printf "a' = %s" (show a')
  let a'' = differenceTypeEnv typeEnv a a
  annotate $ printf "a-a = %s" (show a'')
  assert $ isEqType typeEnv a'' BottomType

-- A+B-A ⊆ B
propDifferenceSubset :: IO GenPrgm -> Property
propDifferenceSubset gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  let intersected = intersectTypes typeEnv a b
  let overlapping = intersected /= BottomType
  annotate $ printf "does a overlaps b? %s" (show overlapping)
  let union = unionTypes typeEnv a b
  annotate $ printf "a + b = %s" (show union)
  let sub = differenceTypeEnv typeEnv union a
  annotate $ printf "a+b-a = %s" (show sub)
  if overlapping
    then assert $ isSubtypeOf typeEnv sub b
    else assert $ isEqType typeEnv sub b


-- A∩B=A-(A-B)
propIntersectByDifference :: IO GenPrgm -> Property
propIntersectByDifference gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  let intersect' = intersectTypes typeEnv a b
  annotate $ printf "intersect' = %s" (show intersect')
  let subDiff' = differenceTypeEnv typeEnv a b
  let diff' = differenceTypeEnv typeEnv a subDiff'
  annotate $ printf "a-b' = %s" (show subDiff')
  annotate $ printf "diff' = a-(a-b) = %s" (show diff')
  assert $ isEqType typeEnv intersect' diff'

-- A-B=A-(A∩B)
propDifferenceByIntersection :: IO GenPrgm -> Property
propDifferenceByIntersection gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  let diff' = differenceTypeEnv typeEnv a b
  annotate $ printf "diff' = %s" (show diff')
  let subInter' = intersectTypes typeEnv a b
  let withInter' = differenceTypeEnv typeEnv a subInter'
  annotate $ printf "a∩b' = %s" (show subInter')
  annotate $ printf "withInter' = a-(a∩b) = %s" (show withInter')
  assert $ isEqType typeEnv diff' withInter'

-- P[$T=(A+B)] should be recognized as equal to P[$T=A] + P[$T=B]
propSubtypeUnionVarSplit :: IO GenPrgm -> Property
propSubtypeUnionVarSplit gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'

  let typeEnv = mkTypeEnv prgm

  -- Generate two distinct partial types to use as var values
  a <- forAll $ genPartialType prgm
  b <- forAll $ genPartialType prgm

  -- Create P[$T=(A+B)] - a partial with a union-typed var
  let varName = partialKey "T"
  let baseName = "/TestType"
  let unionVar = UnionType Nothing (joinUnionType [a, b]) []
  let joinedPartial = (partialVal baseName){ptVars = H.singleton varName unionVar}
  let joinedType = singletonType joinedPartial

  -- Create P[$T=A] + P[$T=B] - the split version
  let splitPartialA = (partialVal baseName){ptVars = H.singleton varName (singletonType a)}
  let splitPartialB = (partialVal baseName){ptVars = H.singleton varName (singletonType b)}
  let splitType = UnionType Nothing (joinUnionType [splitPartialA, splitPartialB]) []

  annotate $ printf "joined = %s" (show joinedType)
  annotate $ printf "split = %s" (show splitType)

  -- P[$T=(A+B)] should be equivalent to P[$T=A] + P[$T=B]
  assert $ isEqType typeEnv joinedType splitType

-- Parameterized-class covariance: for a class C parameterized by /T, the membership predicate is
-- covariant in the element bound - @PredClass C[/T=a] ⊆ PredClass C[/T=b]@ whenever @a ⊆ b@ (a value
-- whose class-C element is within @a@ is also within the wider @b@), and correspondingly
-- @C[/T=a] − C[/T=b] = ∅@. Uses @a ⊆ (a∪b)@ to guarantee the wider bound for any generated a, b.
propParamClassCovariant :: IO GenPrgm -> Property
propParamClassCovariant gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  let wider = unionTypes typeEnv a b
  let mkClassOver elemT =
        let cP = (partialVal "/TestParamClass"){ptVars = H.singleton (partialKey "T") elemT}
        in UnionType (Just (PredsOne (PredClass cP), H.empty)) H.empty []
  let cNarrow = mkClassOver a
  let cWide = mkClassOver wider
  annotate $ printf "a = %s" (show a)
  annotate $ printf "a∪b = %s" (show wider)
  annotate $ printf "C[a] = %s" (show cNarrow)
  annotate $ printf "C[a∪b] = %s" (show cWide)
  annotate $ printf "C[a] − C[a∪b] = %s" (show $ differenceTypeEnv typeEnv cNarrow cWide)
  assert $ isSubtypeOf typeEnv cNarrow cWide
  assert $ isEqType typeEnv (differenceTypeEnv typeEnv cNarrow cWide) BottomType

-- | Every leaf of a class's (materialized) member expansion is a subtype of the class — both
-- as expanded (covers class-named members like @Nil@ reached through @Eq@'s @ConsList[Eq]@) and
-- with its declared fields erased to ⊤ (a data value's fields are constrained by its data
-- definition, so @Cons(head=⊤, tail=⊤)@ has the same values as @Cons@ and stays inside
-- @∀ConsList@). Regression for the symbolic engine manufacturing escape witnesses (e.g. a Cons
-- with a non-ConsList tail) that no real value has.
propClassExpansionLeafSubtype :: IO GenPrgm -> Property
propClassExpansionLeafSubtype gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData} = mkTypeEnv prgm
  let classPartials = [cpt | (CGClass (_, cpt, cts, _), _, _) <- graphToNodes cgData, not (null cts)]
  when (null classPartials) discard
  cpt <- forAll $ HG.element classPartials
  let clsT = classPartial cpt
  let expanded = expandClassPartial typeEnv H.empty cpt
  leaves <- case expanded of
    UnionType Nothing leafs _ | not (H.null leafs) -> return $ splitUnionType leafs
    _                                              -> discard
  leaf <- forAll $ HG.element leaves
  let erased = leaf{ptArgs = fmap (const PTopType) (ptArgs leaf)}
  annotate $ printf "class = %s" (show clsT)
  annotate $ printf "leaf = %s" (show leaf)
  annotate $ printf "erased = %s" (show erased)
  assert $ isSubtypeOf typeEnv (singletonType leaf) clsT
  assert $ isSubtypeOf typeEnv (singletonType erased) clsT

propClassGraphMatchesPredClass :: IO GenPrgm -> Property
propClassGraphMatchesPredClass gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'

  -- Skip programs with no objects (like empty or error programs)
  let Prgm{prgmObjMap} = prgm
  when (nullObjectMap prgmObjMap) discard

  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData} = mkTypeEnv prgm
  let classNodes = filter isClassNode $ graphToNodes cgData

  -- Discard if no classes
  when (null classNodes) discard

  -- Pick a random class
  (cgNode, className, _) <- forAll $ HG.element classNodes
  case cgNode of
    CGClass (sealed, classPartialType, constituentTypes, _doc) -> do
      -- For unsealed classes, the class type is not provably equal to its current members
      -- (future implementations could be added). Skip them.
      when (not sealed) discard
      annotate $ printf "Testing class: %s" (nameStr (fromPartialName className))
      annotate $ printf "Class definition: %s" (show classPartialType)

      -- Expand the class using expandClassPartial
      let cls = classPartial classPartialType

      annotate $ printf "Constituent types: %s" (show constituentTypes)
      annotate $ printf "Expanded PredClass: %s" (show cls)

      -- Compare: the expanded PredClass should match expectations
      case constituentTypes of
        [] -> do
          -- Empty class should expand to classPlaceholder[$T = className]
          let expectedPlaceholder = singletonType $ classPlaceholderLeaf (fromPartialName className)
          annotate $ printf "Expected placeholder: %s" (show expectedPlaceholder)
          assert $ isEqType typeEnv cls expectedPlaceholder
        _ -> do
          -- Non-empty class should equal the union of constituent types
          let unionConstituents = unionAllTypes typeEnv constituentTypes
          annotate $ printf "Union of constituents: %s" (show unionConstituents)
          assert $ isEqType typeEnv cls unionConstituents

    CGType -> discard
  where
    isClassNode (CGClass{}, _, _) = True
    isClassNode _                 = False

-- | A parameterized class over ⊤ must not imply the same class over a narrower element:
-- @∀ConsList[⊤]@ contains a list of ANY element (e.g. a list of lists), which
-- @∀ConsList[Integer]@ excludes. The symbolic engine once claimed this implication: class
-- members enumerate with an uninstantiated parameter left flexible, and the ∀-cover escape
-- check ('partialCoversSat') bound the flexible slot on demand toward being covered, so no
-- member ever escaped the narrower class. Also pins the sound directions: covariance upward,
-- and the class's member expansion staying within the class (flexible-vs-flexible cover).
propParamClassWideNotNarrow :: IO Prgms -> Property
propParamClassWideNotNarrow ioPrgms = property $ do
  prgms <- lift ioPrgms
  prgm <- case [v | (k, v) <- H.toList prgms, "containers" `isInfixOf` k] of
    (prgm:_) -> return prgm
    []       -> fail "containers.ct program not found"
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData, teNames} = mkTypeEnv prgm
  clsPt <- case [cpt | (CGClass (_, cpt, cts, _), _, _) <- graphToNodes cgData, "ConsList" `isInfixOf` nameStr (ptName cpt), not (null cts)] of
    (cpt:_) -> return cpt
    []      -> fail "ConsList class not found"
  intT <- case [n | n <- S.toList teNames, "Integer" `isInfixOf` nameStr n] of
    (n:_) -> return $ singletonType $ partialVal n
    []    -> fail "Integer type not found"
  tKey <- case H.keys (ptVars clsPt) of
    (k:_) -> return k
    []    -> fail "ConsList class has no type parameter"
  let clsOver elemT = UnionType (Just (PredsOne (PredClass clsPt{ptVars=H.singleton tKey elemT}), H.empty)) H.empty []
  let wide = clsOver PTopType
  let narrow = clsOver intT
  let bare = classPartial clsPt
  let expanded = expandClassPartial typeEnv H.empty clsPt
  annotate $ printf "wide = %s" (show wide)
  annotate $ printf "narrow = %s" (show narrow)
  annotate $ printf "bare = %s" (show bare)
  annotate $ printf "expanded = %s" (show expanded)
  -- Covariance holds upward: a narrower element bound is within the wider.
  assert $ isSubtypeOf typeEnv narrow wide
  -- Soundness downward: the wide form contains values the narrow form excludes.
  assert $ not (isSubtypeOf typeEnv wide narrow)
  assert $ not (isSubtypeOf typeEnv bare narrow)
  -- The class's own member expansion (flexible parameters) stays within the class.
  assert $ isSubtypeOf typeEnv expanded bare

-- | Intersecting the same class over an unresolved element variable and over a concrete
-- element must keep the concrete bound (@C[Ord] ⟹ C[$_T]@ by covariance with the unbound
-- variable reading as ⊤, so the conjunction absorbs to @C[Ord]@). The unsound flexible
-- cover made the implication hold the OTHER way as well, absorbing to the flexible form —
-- an intersection that WIDENED, which let a constraint re-widen a narrowed point invisibly.
propParamClassIntersectAbsorb :: IO Prgms -> Property
propParamClassIntersectAbsorb ioPrgms = property $ do
  prgms <- lift ioPrgms
  prgm <- case [v | (k, v) <- H.toList prgms, "containers" `isInfixOf` k] of
    (prgm:_) -> return prgm
    []       -> fail "containers.ct program not found"
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData, teNames} = mkTypeEnv prgm
  clsPt <- case [cpt | (CGClass (_, cpt, cts, _), _, _) <- graphToNodes cgData, "ConsList" `isInfixOf` nameStr (ptName cpt), not (null cts)] of
    (cpt:_) -> return cpt
    []      -> fail "ConsList class not found"
  intT <- case [n | n <- S.toList teNames, "Integer" `isInfixOf` nameStr n] of
    (n:_) -> return $ singletonType $ partialVal n
    []    -> fail "Integer type not found"
  tKey <- case H.keys (ptVars clsPt) of
    (k:_) -> return k
    []    -> fail "ConsList class has no type parameter"
  let clsOver elemT = UnionType (Just (PredsOne (PredClass clsPt{ptVars=H.singleton tKey elemT}), H.empty)) H.empty []
  -- The variable is deliberately NAMED like the class's own parameter key (an external @$_T@
  -- meeting a class whose parameter is also @/T@) — the shape a completed data field takes.
  let flexVar = TypeVar (TVVar tKey) TVExt
  let narrow = clsOver intT
  let flexed = clsOver flexVar
  -- The meet's standalone reading: the intersection records a var's binding in the returned
  -- env while the type keeps the var, so resolve the discovered bindings back in. This is
  -- exactly what env-holding consumers ('updateTypeProp') do; the regression pinned here was
  -- 'mergePredsAnd' WIPING an earlier unification's binding, leaving nothing to resolve.
  let resolvedIntersect a b =
        let (venv, t) = intersectTypesWithVarEnv typeEnv H.empty a b
            newBindings = H.filter (/= PTopType) venv
        in if H.null newBindings then t else substituteKeep newBindings t
  let met = resolvedIntersect flexed narrow
  annotate $ printf "flexed = %s" (show flexed)
  annotate $ printf "narrow = %s" (show narrow)
  annotate $ printf "flexed ∩ narrow = %s" (show met)
  -- The meet must not widen: it stays within the concrete-bounded form.
  assert $ isSubtypeOf typeEnv met narrow
  -- And the concrete-bounded form stays within the meet (the meet is exactly the narrow form).
  assert $ isSubtypeOf typeEnv narrow met
  -- The same meet through a CONJUNCTION with a second class (the shape a completed data field
  -- takes: the field's class with an unresolved element ∧ a supertype class with the bound):
  -- @(ConsList[$_v] ∧ List[Int]) ∩ (ConsList[Int] ∧ List[Int])@ must also keep the bound.
  listPt <- case [cpt | (CGClass (_, cpt, _, _), _, _) <- graphToNodes cgData, "List" `isInfixOf` nameStr (ptName cpt), not ("ConsList" `isInfixOf` nameStr (ptName cpt))] of
    (cpt:_) -> return cpt
    []      -> fail "List class not found"
  lKey <- case H.keys (ptVars listPt) of
    (k:_) -> return k
    []    -> fail "List class has no type parameter"
  let conj elemA elemB = UnionType (Just (predsAnd (PredsOne (PredClass clsPt{ptVars=H.singleton tKey elemA})) (PredsOne (PredClass listPt{ptVars=H.singleton lKey elemB})), H.empty)) H.empty []
  let flexConj = conj flexVar intT
  let narrowConj = conj intT intT
  let metConj = resolvedIntersect flexConj narrowConj
  annotate $ printf "flexConj = %s" (show flexConj)
  annotate $ printf "narrowConj = %s" (show narrowConj)
  annotate $ printf "flexConj ∩ narrowConj = %s" (show metConj)
  assert $ isSubtypeOf typeEnv metConj narrowConj
  assert $ isSubtypeOf typeEnv narrowConj metConj

-- | A conjunction of a flexible-element outer class with a concrete-element member class
-- stays within the member class's expansion: @Bag[$_A] ∧ ConsList[Integer] ⊆ ∀ConsList[Integer]@
-- (any value satisfying both IS a @ConsList[Integer]@; the flexible atom constrains nothing
-- beyond membership). The type-level predicate merge cannot unify DISTINCT classes, so the
-- same-class pair only meets inside the engine after member expansion
-- (@ConsList[$_A] ∧ ConsList[Integer]@); the goal must resolve that pair, or the member
-- candidate generated from the flexible atom keeps its unbound slot and — under the sound
-- flexible-slot cover — manufactures an escape witness (@Cons[$_A]@ "escaping"
-- @∀ConsList[Integer]@) that no value satisfying BOTH atoms has. Surfaced as a
-- 'verifySchemeActLowers' Grows-by on typeApply.ct's element-changing map
-- (@Functor[$_A] ∧ Maybe[Integer] ⊄ ∀Maybe[Integer]@).
propConjMemberClassSubtype :: IO Prgms -> Property
propConjMemberClassSubtype ioPrgms = property $ do
  prgms <- lift ioPrgms
  prgm <- case [v | (k, v) <- H.toList prgms, "containers" `isInfixOf` k] of
    (prgm:_) -> return prgm
    []       -> fail "containers.ct program not found"
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData, teNames} = mkTypeEnv prgm
  consListPt <- case [cpt | (CGClass (_, cpt, cts, _), _, _) <- graphToNodes cgData, "ConsList" `isInfixOf` nameStr (ptName cpt), not (null cts)] of
    (cpt:_) -> return cpt
    []      -> fail "ConsList class not found"
  bagPt <- case [cpt | (CGClass (_, cpt, _, _), _, _) <- graphToNodes cgData, "Bag" `isInfixOf` nameStr (ptName cpt)] of
    (cpt:_) -> return cpt
    []      -> fail "Bag class not found"
  intT <- case [n | n <- S.toList teNames, "Integer" `isInfixOf` nameStr n] of
    (n:_) -> return $ singletonType $ partialVal n
    []    -> fail "Integer type not found"
  tKey <- case H.keys (ptVars consListPt) of
    (k:_) -> return k
    []    -> fail "ConsList class has no type parameter"
  bKey <- case H.keys (ptVars bagPt) of
    (k:_) -> return k
    []    -> fail "Bag class has no type parameter"
  -- An UNBOUND external variable, named unlike the class parameters: flexible (binds per
  -- value), the reading an authored @$_A@ has.
  let flexA = TypeVar (TVVar (partialKey "A")) TVExt
  let consListInt = consListPt{ptVars=H.singleton tKey intT}
  let conjOver outer = UnionType (Just (predsAnd (PredsOne (PredClass outer)) (PredsOne (PredClass consListInt)), H.empty)) H.empty []
  let conj = conjOver bagPt{ptVars=H.singleton bKey flexA}
  -- The same conjunction with the outer atom UNBOUND (a bare membership atom, the form a
  -- parameterless isa-entry produces).
  let conjBare = conjOver bagPt{ptVars=H.empty}
  let narrowExpanded = expandClassPartial typeEnv H.empty consListInt
  annotate $ printf "conj = %s" (show conj)
  annotate $ printf "conjBare = %s" (show conjBare)
  annotate $ printf "narrowExpanded = %s" (show narrowExpanded)
  assert $ isSubtypeOf typeEnv conj narrowExpanded
  assert $ isSubtypeOf typeEnv conjBare narrowExpanded

typeTests :: TestTree
typeTests = withResource findPrgms (const $ pure ()) tests
  where
    tests ioPrgms = testGroup "TypeTests" [
      HG.testProperty "propCompactNoChanges" (p $ propCompactNoChanges gPrgm)
      , HG.testProperty "propCompactIdempotent" (p $ propCompactIdempotent gPrgm)
      , HG.testProperty "propSubtypeByUnion" (p $ propSubtypeByUnion gPrgm)
      , HG.testProperty "propSubtypeByIntersection" (p $ propSubtypeByIntersection gPrgm)
      , HG.testProperty "propUnionReflexive" (p $ propUnionReflexive gPrgm)
      , HG.testProperty "propUnionCommutative" (p $ propUnionCommutative gPrgm)
      , HG.testProperty "propIntersectionReflexive" (p $ propIntersectionReflexive gPrgm)
      , HG.testProperty "propIntersectionCommutative" (p $ propIntersectionCommutative gPrgm)
      , HG.testProperty "propIntersectionDistributesUnion" (p $ propIntersectionDistributesUnion gPrgm)
      , HG.testProperty "propUnionDistributesIntersection" (p $ propUnionDistributesIntersection gPrgm)
      , HG.testProperty "propDifferenceShrinks" (p $ propDifferenceShrinks gPrgm)
      , HG.testProperty "propUnionWithComplement" (p $ propUnionWithComplement gPrgm)
      , HG.testProperty "propIntersectionWithComplement" (p $ propIntersectionWithComplement gPrgm)
      , HG.testProperty "propComplementInverse" (p $ propComplementInverse gPrgm)
      , HG.testProperty "propDifferenceIdentity" (p $ propDifferenceIdentity gPrgm)
      , HG.testProperty "propDifferenceSubset" (p $ propDifferenceSubset gPrgm)
      -- , HG.testProperty "propIntersectByDifference" (p $ propIntersectByDifference gPrgm)
      -- , HG.testProperty "propDifferenceByIntersection" (p $ propDifferenceByIntersection gPrgm)
      , HG.testProperty "propParamClassCovariant" (p $ propParamClassCovariant gPrgm)
      , HG.testProperty "propSubtypeUnionVarSplit" (withDiscards 1000 $ p $ propSubtypeUnionVarSplit gPrgm)
      , HG.testProperty "propClassExpansionLeafSubtype" (withDiscards 1000 $ p $ propClassExpansionLeafSubtype gPrgm)
      , HG.testProperty "propClassGraphMatchesPredClass" (withDiscards 1000 $ p $ propClassGraphMatchesPredClass gPrgm)
      , HG.testProperty "propParamClassWideNotNarrow" (withTests 1 $ propParamClassWideNotNarrow ioPrgms)
      , HG.testProperty "propParamClassIntersectAbsorb" (withTests 1 $ propParamClassIntersectAbsorb ioPrgms)
      , HG.testProperty "propConjMemberClassSubtype" (withTests 1 $ propConjMemberClassSubtype ioPrgms)
                                        ]
      where gPrgm = ggPrgm <$> ioPrgms
    p prop = prop
    -- p prop = withTests 100000 prop

retest :: IO ()
retest = defaultMain $ localOption (TP.TestPattern $ Just $ TT.ERE "propCompactIdempotent") $ localOption (HedgehogReplay $ Just ("1:a2", Seed 10220680066336263475 12463056215188250787)) typeTests

main :: IO ()
main = defaultMain typeTests
