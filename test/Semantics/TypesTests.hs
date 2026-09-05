module Semantics.TypesTests where

import           Common.TestCommon         (findCt)
import           Control.Exception         (evaluate)
import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO)
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
import           System.Timeout            (timeout)
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

-- | Reads a whole-type bare variable ('TVVar'\/'TVArg', or a 'TVApplied' over one) to its value
-- under the environment (an unbound one to ⊤). It deliberately does NOT descend into partials: a
-- variable inside a partial's arg\/var slot is resolved correctly by the operations themselves as
-- they thread the environment through each nested scope (a 'TVExt' slot reads the outer binding),
-- whereas a bare top-level variable is the one shape the meet mishandles (see 'intersectR').
readTopVar :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
readTopVar typeEnv env (TypeVar (TVApplied b ovr) loc i) = applyTypeVarOverrides (fmap (readTopVar typeEnv env) ovr) (readTopVar typeEnv env (TypeVar b loc i))
-- The reference reading of an occurrence: @⟦var⟧ ∩ ⟦inner⟧@.
readTopVar typeEnv env (TypeVar v _ i)                    = meetInner typeEnv env (vaenvLookup env v) (readTopVar typeEnv env i)
readTopVar _ _   t                                = t

-- | Intersection for the property tests, threading a seed 'TypeVarArgEnv'. A bare top-level variable
-- operand is read to its value first ('readTopVar'), because 'intersectTypesWithVarEnv' returns a
-- variable meet as the bare VARIABLE while recording the narrowing in its (here discarded) env — the
-- mergeVarEnvVal doctrine: @$a ∩ b@ keeps @$a@ symbolic and so reads standalone as @$a@, deliberately
-- wider so a call-site lambda's parameter can be refined later ('FnInputBound'). The set-algebra laws
-- are about denotations, so a top-level variable is read to its value first; nested variables are
-- left for the meet, which resolves them exactly. Union, complement and the subtype\/equality checks
-- resolve a bound variable inline (via 'vaenvLookup'), so they take the environment directly.
intersectR :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Type
intersectR typeEnv env a b = intersectTypesEnv typeEnv env (readTopVar typeEnv env a) (readTopVar typeEnv env b)

-- | Difference threading a seed 'TypeVarArgEnv' (@a − b = a ∩ ¬b@). @complementTypeEnv@ resolves a
-- bare bound @b@ inline, and a nested variable stays symbolic for the meet to resolve.
diffR :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Type
diffR typeEnv env a b = intersectR typeEnv env a (complementTypeEnv typeEnv env b)

propCompactNoChanges :: IO GenPrgm -> Property
propCompactNoChanges gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  compacted <- timed "compact a" $ compactType typeEnv vaenv a
  annotate $ printf "compacted = %s" (show compacted)
  assertTimed "a = compact a" $ isEqTypeWithEnvComplete typeEnv vaenv a compacted

propCompactIdempotent :: IO GenPrgm -> Property
propCompactIdempotent gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  compact1 <- timed "compact a" $ compactType typeEnv vaenv a
  compact2 <- timed "compact (compact a)" $ compactType typeEnv vaenv compact1
  annotate $ printf "compact once to %s" (show compact1)
  annotate $ printf "compact twice to %s" (show compact2)
  eqTimed "compact idempotent" compact1 compact2



propSubtypeByUnion :: IO GenPrgm -> Property
propSubtypeByUnion gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  subtype <- timed "a ⊆ b" $ isSubtypeOfWithEnv typeEnv vaenv a b
  annotate $ printf "subtype = %s" (show subtype)
  ab <- timed "a∪b" $ unionTypesWithEnv typeEnv vaenv a b
  byUnion <- timed "a∪b = b" $ isEqTypeWithEnvComplete typeEnv vaenv ab b
  annotate $ printf "a∪b = %s" (show ab)
  annotate $ printf "byUnion = %s" (show byUnion)
  eqTimed "subtype by union" subtype byUnion

propSubtypeByIntersection :: IO GenPrgm -> Property
propSubtypeByIntersection gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  subtype <- timed "a ⊆ b" $ isSubtypeOfWithEnv typeEnv vaenv a b
  annotate $ printf "subtype = %s" (show subtype)
  ab <- timed "a∩b" $ intersectR typeEnv vaenv a b
  byIntersection <- timed "a∩b = a" $ isEqTypeWithEnvComplete typeEnv vaenv ab a
  annotate $ printf "a∩b = %s" (show ab)
  annotate $ printf "byIntersection = %s" (show byIntersection)
  eqTimed "subtype by intersection" subtype byIntersection

propUnionReflexive :: IO GenPrgm -> Property
propUnionReflexive gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  ab <- timed "a∪b" $ unionTypesWithEnv typeEnv vaenv a b
  ba <- timed "b∪a" $ unionTypesWithEnv typeEnv vaenv b a
  assertTimed "a∪b = b∪a" $ isEqTypeWithEnvComplete typeEnv vaenv ab ba

propUnionCommutative :: IO GenPrgm -> Property
propUnionCommutative gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm
  abc <- timed "a∪b∪c" $ unionAllTypesWithEnv typeEnv vaenv [a, b, c]
  cba <- timed "c∪b∪a" $ unionAllTypesWithEnv typeEnv vaenv [c, b, a]
  assertTimed "a∪b∪c = c∪b∪a" $ isEqTypeWithEnvComplete typeEnv vaenv abc cba

propIntersectionReflexive :: IO GenPrgm -> Property
propIntersectionReflexive gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  ab <- timed "a∩b" $ intersectR typeEnv vaenv a b
  ba <- timed "b∩a" $ intersectR typeEnv vaenv b a
  assertTimed "a∩b = b∩a" $ isEqTypeWithEnvComplete typeEnv vaenv ab ba

propIntersectionCommutative :: IO GenPrgm -> Property
propIntersectionCommutative gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm

  abc <- timed "a∩b∩c" $ foldr1 (intersectR typeEnv vaenv) [a, b, c]
  annotate $ printf "a∩b∩c = %s" (show abc)

  cba <- timed "c∩b∩a" $ foldr1 (intersectR typeEnv vaenv) [c, b, a]
  annotate $ printf "c∩b∩a = %s" (show cba)

  fwd <- timed "a∩b∩c ⊆ c∩b∩a" $ isSubtypeOfWithEnv typeEnv vaenv abc cba
  bwd <- timed "c∩b∩a ⊆ a∩b∩c" $ isSubtypeOfWithEnv typeEnv vaenv cba abc
  annotate $ printf "a∩b∩c ⊆ c∩b∩a = %s" (show fwd)
  annotate $ printf "c∩b∩a ⊆ a∩b∩c = %s" (show bwd)

  assertTimed "a∩b∩c = c∩b∩a" $ isEqTypeWithEnvComplete typeEnv vaenv abc cba

propIntersectionDistributesUnion :: IO GenPrgm -> Property
propIntersectionDistributesUnion gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm
  bc <- timed "b∪c" $ unionTypesWithEnv typeEnv vaenv b c
  f <- timed "a∩(b∪c)" $ intersectR typeEnv vaenv a bc
  annotate $ printf "f = %s" (show f)
  ab <- timed "a∩b" $ intersectR typeEnv vaenv a b
  ac <- timed "a∩c" $ intersectR typeEnv vaenv a c
  d <- timed "(a∩b)∪(a∩c)" $ unionTypesWithEnv typeEnv vaenv ab ac
  annotate $ printf "a∩b = %s" (show ab)
  annotate $ printf "a∩c = %s" (show ac)
  annotate $ printf "d = %s" (show d)
  assertTimed "a∩(b∪c) = (a∩b)∪(a∩c)" $ isEqTypeWithEnvComplete typeEnv vaenv f d

propUnionDistributesIntersection :: IO GenPrgm  -> Property
propUnionDistributesIntersection gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  c <- forAll $ genType prgm
  bc <- timed "b∩c" $ intersectR typeEnv vaenv b c
  f <- timed "a∪(b∩c)" $ unionTypesWithEnv typeEnv vaenv a bc
  annotate $ printf "f = %s" (show f)
  ab <- timed "a∪b" $ unionTypesWithEnv typeEnv vaenv a b
  ac <- timed "a∪c" $ unionTypesWithEnv typeEnv vaenv a c
  annotate $ printf "a∪b = %s" (show ab)
  annotate $ printf "a∪c = %s" (show ac)
  d <- timed "(a∪b)∩(a∪c)" $ intersectR typeEnv vaenv ab ac
  annotate $ printf "d = %s" (show d)
  assertTimed "a∪(b∩c) = (a∪b)∩(a∪c)" $ isEqTypeWithEnvComplete typeEnv vaenv f d

propDifferenceShrinks :: IO GenPrgm -> Property
propDifferenceShrinks gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  b <- forAll $ HG.choice [genType prgm, genRelatedType prgm a]
  diff' <- timed "a-b" $ diffR typeEnv vaenv a b
  annotate $ printf "diff' = %s" (show diff')
  assertTimed "a-b ⊆ a" $ isSubtypeOfWithEnv typeEnv vaenv diff' a

-- | The meet is a LOWER BOUND: @a∩b ⊆ a@ and @a∩b ⊆ b@.
--
-- With 'propUnionGrows' these are the two most basic lattice laws, and the suite had neither —
-- only the compound 'propIntersectionDistributesUnion' \/ 'propUnionDistributesIntersection' and
-- the 'propIntersectionCommutative' fold, which reach a broken meet only indirectly. That costs
-- twice over: a distributivity failure reports only that two sides DIFFER, without saying which
-- one is wrong, and it needs three operands, so Hedgehog shrinks to a three-operand
-- counterexample where a two-operand one would name the defect. The 2026-08 meet defect
-- (Types.hs:5287 returning the left occurrence verbatim, dropping the right operand's
-- occurrence-inner and so returning something WIDER than an operand) surfaced through
-- distributivity after a 47-shrink path; this law states it directly.
--
-- 'genRelatedType' matters here as it does for 'propDifferenceShrinks': two independently drawn
-- types usually meet to ∅, which satisfies the law trivially.
propIntersectionShrinks :: IO GenPrgm -> Property
propIntersectionShrinks gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  b <- forAll $ HG.choice [genType prgm, genRelatedType prgm a]
  ab <- timed "a∩b" $ intersectR typeEnv vaenv a b
  annotate $ printf "a∩b = %s" (show ab)
  assertTimed "a∩b ⊆ a" $ isSubtypeOfWithEnv typeEnv vaenv ab a
  assertTimed "a∩b ⊆ b" $ isSubtypeOfWithEnv typeEnv vaenv ab b

-- | The join is an UPPER BOUND: @a ⊆ a∪b@ and @b ⊆ a∪b@. The dual of 'propIntersectionShrinks'
-- and the other half of the missing pair; a union that DROPS an operand's refinement is the same
-- defect class seen from the other side.
propUnionGrows :: IO GenPrgm -> Property
propUnionGrows gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  b <- forAll $ HG.choice [genType prgm, genRelatedType prgm a]
  ab <- timed "a∪b" $ unionTypesWithEnv typeEnv vaenv a b
  annotate $ printf "a∪b = %s" (show ab)
  assertTimed "a ⊆ a∪b" $ isSubtypeOfWithEnv typeEnv vaenv a ab
  assertTimed "b ⊆ a∪b" $ isSubtypeOfWithEnv typeEnv vaenv b ab

-- A∪A=U
propUnionWithComplement :: IO GenPrgm -> Property
propUnionWithComplement gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  a' <- timed "¬a" $ complementTypeEnv typeEnv vaenv a
  annotate $ printf "a' = %s" (show a')
  combined <- timed "a∪¬a" $ unionTypesWithEnv typeEnv vaenv a a'
  annotate $ printf "combined = %s" (show combined)
  assertTimed "a∪¬a = ⊤" $ isEqTypeWithEnvComplete typeEnv vaenv combined PTopType

-- A∩A=∅
propIntersectionWithComplement :: IO GenPrgm -> Property
propIntersectionWithComplement gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  a' <- timed "¬a" $ complementTypeEnv typeEnv vaenv a
  annotate $ printf "a' = %s" (show a')
  combined <- timed "a∩¬a" $ intersectR typeEnv vaenv a a'
  annotate $ printf "combined = %s" (show combined)
  assertTimed "a∩¬a = ∅" $ isEqTypeWithEnvComplete typeEnv vaenv combined BottomType

-- A^c^c=A
propComplementInverse :: IO GenPrgm -> Property
propComplementInverse gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  a' <- timed "¬a" $ complementTypeEnv typeEnv vaenv a
  annotate $ printf "a' = %s" (show a')
  a'' <- timed "¬¬a" $ complementTypeEnv typeEnv vaenv a'
  annotate $ printf "a'' = %s" (show a'')
  assertTimed "¬¬a = a" $ isEqTypeWithEnvComplete typeEnv vaenv a a''

-- A-A=∅
propDifferenceIdentity :: IO GenPrgm -> Property
propDifferenceIdentity gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  a' <- timed "¬a" $ complementTypeEnv typeEnv vaenv a
  annotate $ printf "a' = %s" (show a')
  a'' <- timed "a-a" $ diffR typeEnv vaenv a a
  annotate $ printf "a-a = %s" (show a'')
  assertTimed "a-a = ∅" $ isEqTypeWithEnvComplete typeEnv vaenv a'' BottomType

-- A+B-A ⊆ B
propDifferenceSubset :: IO GenPrgm -> Property
propDifferenceSubset gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  b <- forAll $ HG.choice [genType prgm, genRelatedType prgm a]
  intersected <- timed "a∩b" $ intersectR typeEnv vaenv a b
  let overlapping = intersected /= BottomType
  annotate $ printf "does a overlaps b? %s" (show overlapping)
  union <- timed "a∪b" $ unionTypesWithEnv typeEnv vaenv a b
  annotate $ printf "a + b = %s" (show union)
  sub <- timed "a+b-a" $ diffR typeEnv vaenv union a
  annotate $ printf "a+b-a = %s" (show sub)
  if overlapping
    then assertTimed "a+b-a ⊆ b" $ isSubtypeOfWithEnv typeEnv vaenv sub b
    else assertTimed "a+b-a = b" $ isEqTypeWithEnvComplete typeEnv vaenv sub b


-- | Deep-force a stage's result under a wall-clock budget: a case that exceeds it fails the
-- property, so hedgehog SHRINKS toward a minimal pathological input (a livelocked op otherwise
-- hangs the suite with no reported counterexample; deep-forcing via 'show' reaches every thunk).
forceTimed :: (Show a, Monad m, MonadTest m, MonadIO m) => String -> a -> m ()
forceTimed stage x = do
  r <- evalIO $ timeout 5000000 (evaluate (length (show x)))
  case r of
    Just _  -> pure ()
    Nothing -> do
      footnote $ printf "TIMEOUT (5s) forcing stage: %s" stage
      failure

-- | 'forceTimed' that returns the value, for binding a stage's result:
-- @v <- timed "a∩b" $ intersectR …@. EVERY property below routes its set operations through
-- 'timed'\/'assertTimed'\/'eqTimed' rather than a bare @let@\/@assert@\/@===@, so a livelocked
-- operation FAILS (and hedgehog shrinks it to a minimal reproducing input) instead of spinning
-- forever with no counterexample. A silent livelock here once burned hours of wall clock: the
-- suite pinned a core at multi-GB residency with nothing to debug, and even survived its own
-- @timeout@ (SIGTERM is ignored while the RTS spins in a non-allocating loop — use
-- @timeout -s KILL@ around the suite as a backstop).
timed :: (Show a, Monad m, MonadTest m, MonadIO m) => String -> a -> m a
timed stage x = forceTimed stage x >> return x

-- | 'assert' under the force budget (see 'timed').
assertTimed :: (Monad m, MonadTest m, MonadIO m) => String -> Bool -> m ()
assertTimed stage b = forceTimed stage b >> assert b

-- | '===' under the force budget on both sides (see 'timed').
eqTimed :: (Eq a, Show a, Monad m, MonadTest m, MonadIO m) => String -> a -> a -> m ()
eqTimed stage x y = do
  forceTimed (stage ++ " lhs") x
  forceTimed (stage ++ " rhs") y
  x === y

-- A∩B=A-(A-B)
propIntersectByDifference :: IO GenPrgm -> Property
propIntersectByDifference gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  b <- forAll $ HG.choice [genType prgm, genRelatedType prgm a]
  let intersect' = intersectR typeEnv vaenv a b
  forceTimed "intersect a.b" intersect'
  annotate $ printf "intersect' = %s" (show intersect')
  let subDiff' = diffR typeEnv vaenv a b
  forceTimed "a-b" subDiff'
  let diff' = diffR typeEnv vaenv a subDiff'
  forceTimed "a-(a-b)" diff'
  annotate $ printf "a-b' = %s" (show subDiff')
  annotate $ printf "diff' = a-(a-b) = %s" (show diff')
  assertTimed "a∩b = a-(a-b)" $ isEqTypeWithEnvComplete typeEnv vaenv intersect' diff'

-- A-B=A-(A∩B)
propDifferenceByIntersection :: IO GenPrgm -> Property
propDifferenceByIntersection gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  b <- forAll $ HG.choice [genType prgm, genRelatedType prgm a]
  diff' <- timed "a-b" $ diffR typeEnv vaenv a b
  annotate $ printf "diff' = %s" (show diff')
  subInter' <- timed "a∩b" $ intersectR typeEnv vaenv a b
  withInter' <- timed "a-(a∩b)" $ diffR typeEnv vaenv a subInter'
  annotate $ printf "a∩b' = %s" (show subInter')
  annotate $ printf "withInter' = a-(a∩b) = %s" (show withInter')
  assertTimed "a-b = a-(a∩b)" $ isEqTypeWithEnvComplete typeEnv vaenv diff' withInter'

-- P[$T=(A+B)] should be recognized as equal to P[$T=A] + P[$T=B]
propSubtypeUnionVarSplit :: IO GenPrgm -> Property
propSubtypeUnionVarSplit gPrgm = property $ do
  gPrgm' <- lift gPrgm
  -- Pick a program 'genPartialType' can actually draw from, by retrying INSIDE the generator.
  -- Drawing one that cannot and letting the draw discard spends the property's discard budget:
  -- at 10000 tests that budget ran out and the property reported "gave up", a FAIL saying
  -- nothing about the algebra.
  prgm <- forAll $ HG.filter prgmHasPartial gPrgm'

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
  assertTimed "P[A+B] = P[A]+P[B]" $ isEqType typeEnv joinedType splitType

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
        in UnionType (Just (PredsOne (PredClass cP), H.empty, [])) H.empty []
  let cNarrow = mkClassOver a
  let cWide = mkClassOver wider
  annotate $ printf "a = %s" (show a)
  annotate $ printf "a∪b = %s" (show wider)
  annotate $ printf "C[a] = %s" (show cNarrow)
  annotate $ printf "C[a∪b] = %s" (show cWide)
  cDiff <- timed "C[a] − C[a∪b]" $ differenceTypeEnv typeEnv cNarrow cWide
  annotate $ printf "C[a] − C[a∪b] = %s" (show cDiff)
  assertTimed "C[a] ⊆ C[a∪b]" $ isSubtypeOf typeEnv cNarrow cWide
  assertTimed "C[a] − C[a∪b] = ∅" $ isEqType typeEnv cDiff BottomType

-- | Every leaf of a class's (materialized) member expansion is a subtype of the class — both
-- as expanded (covers class-named members like @Nil@ reached through @Eq@'s @ConsList[Eq]@) and
-- with its declared fields erased to ⊤ (a data value's fields are constrained by its data
-- definition, so @Cons(head=⊤, tail=⊤)@ has the same values as @Cons@ and stays inside
-- @∀ConsList@). Regression for the symbolic engine manufacturing escape witnesses (e.g. a Cons
-- with a non-ConsList tail) that no real value has.
-- | Whether a program carries a class node whose (sealed, members) satisfy the predicate.
--
-- Drawn through 'HG.filter', this picks a QUALIFYING program by retrying INSIDE the generator
-- rather than spending the property's discard budget: roughly a third of generated programs and
-- one of the premade files carry no class at all, and the class laws are vacuous for those. A
-- property that draws-then-rejects instead reports "gave up after N discards" once the count is
-- raised — a FAIL that says nothing about the algebra, and the reason #40 was twice triaged as a
-- class/negation gap when neither property had ever found a counterexample.
prgmHasClass :: (Bool -> [Type] -> Bool) -> Prgm Expr () -> Bool
prgmHasClass f (Prgm _ (ClassGraph cg) _) = any match (graphToNodes cg)
  where match (CGClass (sealed, _, cts, _), _, _) = f sealed cts
        match _                                   = False

propClassExpansionLeafSubtype :: IO GenPrgm -> Property
propClassExpansionLeafSubtype gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll $ HG.filter (prgmHasClass (\_ cts -> not (null cts))) gPrgm'
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData} = mkTypeEnv prgm
  -- Draw from the QUALIFYING classes rather than drawing one and rejecting it. A class whose
  -- expansion is not a positive union cannot satisfy this law's premise, and rejecting it after
  -- the draw spends the discard budget without testing anything: at 3000 iterations that budget
  -- ran out and the property reported "gave up" — a FAIL carrying no information about the
  -- algebra, which is how it was twice mistaken for a class/negation gap.
  let candidates = [ (cpt, splitUnionType leafs)
                   | (CGClass (_, cpt, cts, _), _, _) <- graphToNodes cgData, not (null cts)
                   , UnionType Nothing leafs _ <- [expandClassPartial typeEnv H.empty cpt]
                   , not (H.null leafs) ]
  when (null candidates) discard
  (cpt, leaves) <- forAll $ HG.element candidates
  let clsT = classPartial cpt
  leaf <- forAll $ HG.element leaves
  let erased = leaf{ptArgs = fmap (const PTopType) (ptArgs leaf)}
  annotate $ printf "class = %s" (show clsT)
  annotate $ printf "leaf = %s" (show leaf)
  annotate $ printf "erased = %s" (show erased)
  assertTimed "leaf ⊆ class" $ isSubtypeOf typeEnv (singletonType leaf) clsT
  assertTimed "erased leaf ⊆ class" $ isSubtypeOf typeEnv (singletonType erased) clsT

propClassGraphMatchesPredClass :: IO GenPrgm -> Property
propClassGraphMatchesPredClass gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll $ HG.filter (prgmHasClass (\sealed _ -> sealed)) gPrgm'

  -- Skip programs with no objects (like empty or error programs)
  let Prgm{prgmObjMap} = prgm
  when (nullObjectMap prgmObjMap) discard

  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData} = mkTypeEnv prgm
  -- SEALED classes only, chosen BEFORE the draw. For an unsealed class the law does not hold
  -- (a future file may add implementations), so drawing one and rejecting it afterwards spends
  -- the discard budget on a case that was never testable — the "gave up after N discards" FAIL.
  let classNodes = [n | n@(CGClass (True, _, _, _), _, _) <- graphToNodes cgData]

  -- Discard if no classes
  when (null classNodes) discard

  -- Pick a random class
  (cgNode, className, _) <- forAll $ HG.element classNodes
  case cgNode of
    CGClass (_sealed, classPartialType, constituentTypes, _doc) -> do
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
          assertTimed "empty class = placeholder" $ isEqType typeEnv cls expectedPlaceholder
        _ -> do
          -- Non-empty class should equal the union of constituent types
          unionConstituents <- timed "union of constituents" $ unionAllTypes typeEnv constituentTypes
          annotate $ printf "Union of constituents: %s" (show unionConstituents)
          assertTimed "class = union of constituents" $ isEqType typeEnv cls unionConstituents

    CGType -> discard

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
  let clsOver elemT = UnionType (Just (PredsOne (PredClass clsPt{ptVars=H.singleton tKey elemT}), H.empty, [])) H.empty []
  let wide = clsOver PTopType
  let narrow = clsOver intT
  let bare = classPartial clsPt
  let expanded = expandClassPartial typeEnv H.empty clsPt
  annotate $ printf "wide = %s" (show wide)
  annotate $ printf "narrow = %s" (show narrow)
  annotate $ printf "bare = %s" (show bare)
  annotate $ printf "expanded = %s" (show expanded)
  -- Covariance holds upward: a narrower element bound is within the wider.
  assertTimed "narrow ⊆ wide" $ isSubtypeOf typeEnv narrow wide
  -- Soundness downward: the wide form contains values the narrow form excludes.
  assertTimed "wide ⊄ narrow" $ not (isSubtypeOf typeEnv wide narrow)
  assertTimed "bare ⊄ narrow" $ not (isSubtypeOf typeEnv bare narrow)
  -- The class's own member expansion (flexible parameters) stays within the class.
  assertTimed "expanded ⊆ bare" $ isSubtypeOf typeEnv expanded bare

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
  let clsOver elemT = UnionType (Just (PredsOne (PredClass clsPt{ptVars=H.singleton tKey elemT}), H.empty, [])) H.empty []
  -- The variable is deliberately NAMED like the class's own parameter key (an external @$_T@
  -- meeting a class whose parameter is also @/T@) — the shape a completed data field takes.
  let flexVar = TypeVar (TVVar tKey) TVExt PTopType
  let narrow = clsOver intT
  let flexed = clsOver flexVar
  -- The meet's standalone reading: the intersection records a var's binding in the returned
  -- env while the type keeps the var, so resolve the discovered bindings back in. This is
  -- exactly what env-holding consumers ('updateTypeProp') do; the regression pinned here was
  -- 'mergePredsAnd' WIPING an earlier unification's binding, leaving nothing to resolve.
  let resolvedIntersect a b =
        let (venv, t) = intersectTypesWithVarEnv typeEnv H.empty a b
            newBindings = H.filter (/= PTopType) venv
        in if H.null newBindings then t else substituteKeep typeEnv newBindings t
  met <- timed "flexed ∩ narrow" $ resolvedIntersect flexed narrow
  annotate $ printf "flexed = %s" (show flexed)
  annotate $ printf "narrow = %s" (show narrow)
  annotate $ printf "flexed ∩ narrow = %s" (show met)
  -- The meet must not widen: it stays within the concrete-bounded form.
  assertTimed "met ⊆ narrow" $ isSubtypeOf typeEnv met narrow
  -- And the concrete-bounded form stays within the meet (the meet is exactly the narrow form).
  assertTimed "narrow ⊆ met" $ isSubtypeOf typeEnv narrow met
  -- The same meet through a CONJUNCTION with a second class (the shape a completed data field
  -- takes: the field's class with an unresolved element ∧ a supertype class with the bound):
  -- @(ConsList[$_v] ∧ List[Int]) ∩ (ConsList[Int] ∧ List[Int])@ must also keep the bound.
  listPt <- case [cpt | (CGClass (_, cpt, _, _), _, _) <- graphToNodes cgData, "List" `isInfixOf` nameStr (ptName cpt), not ("ConsList" `isInfixOf` nameStr (ptName cpt))] of
    (cpt:_) -> return cpt
    []      -> fail "List class not found"
  lKey <- case H.keys (ptVars listPt) of
    (k:_) -> return k
    []    -> fail "List class has no type parameter"
  let conj elemA elemB = UnionType (Just (predsAnd (PredsOne (PredClass clsPt{ptVars=H.singleton tKey elemA})) (PredsOne (PredClass listPt{ptVars=H.singleton lKey elemB})), H.empty, [])) H.empty []
  let flexConj = conj flexVar intT
  let narrowConj = conj intT intT
  metConj <- timed "flexConj ∩ narrowConj" $ resolvedIntersect flexConj narrowConj
  annotate $ printf "flexConj = %s" (show flexConj)
  annotate $ printf "narrowConj = %s" (show narrowConj)
  annotate $ printf "flexConj ∩ narrowConj = %s" (show metConj)
  assertTimed "metConj ⊆ narrowConj" $ isSubtypeOf typeEnv metConj narrowConj
  assertTimed "narrowConj ⊆ metConj" $ isSubtypeOf typeEnv narrowConj metConj

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
  let flexA = TypeVar (TVVar (partialKey "A")) TVExt PTopType
  let consListInt = consListPt{ptVars=H.singleton tKey intT}
  let conjOver outer = UnionType (Just (predsAnd (PredsOne (PredClass outer)) (PredsOne (PredClass consListInt)), H.empty, [])) H.empty []
  let conj = conjOver bagPt{ptVars=H.singleton bKey flexA}
  -- The same conjunction with the outer atom UNBOUND (a bare membership atom, the form a
  -- parameterless isa-entry produces).
  let conjBare = conjOver bagPt{ptVars=H.empty}
  narrowExpanded <- timed "expand ConsList[Int]" $ expandClassPartial typeEnv H.empty consListInt
  annotate $ printf "conj = %s" (show conj)
  annotate $ printf "conjBare = %s" (show conjBare)
  annotate $ printf "narrowExpanded = %s" (show narrowExpanded)
  assertTimed "conj ⊆ ConsList[Int]" $ isSubtypeOf typeEnv conj narrowExpanded
  assertTimed "conjBare ⊆ ConsList[Int]" $ isSubtypeOf typeEnv conjBare narrowExpanded

-- | The ¬-over-recursive-class meet terminates and stays exact — the minimal repro of the
-- 2026-08 value-level livelock: @(⊤|¬EqCO) ∩ (Cons + Nil canonical expansion)@ diverged by
-- materializing the negation into subtraction leafs that re-introduce it through the
-- recursive @tail@ slot, bit-identically forever. The meet must instead attach the
-- negation symbolically ('partialAddPreds'), drop the uninhabited @Nil{¬EqCO}@ (@Nil@ IS
-- an @EqCO@ member through @every(ConsList[$T -> EqCO], isa= [EqCO])@), and the result
-- must sit inside the positive operand, be disjoint from @∀EqCO@, and re-meet to itself.
propNegRecursiveClassMeet :: IO Prgms -> Property
propNegRecursiveClassMeet ioPrgms = property $ do
  prgms <- lift ioPrgms
  prgm <- case [v | (k, v) <- H.toList prgms, "recursiveClassBounds" `isInfixOf` k] of
    (prgm:_) -> return prgm
    []       -> fail "recursiveClassBounds.ct program not found"
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData} = mkTypeEnv prgm
  eqCOPt <- case [cpt | (CGClass (_, cpt, _, _), _, _) <- graphToNodes cgData, "EqCO" `isInfixOf` nameStr (ptName cpt)] of
    (cpt:_) -> return cpt
    []      -> fail "EqCO class not found"
  consListPt <- case [cpt | (CGClass (_, cpt, cts, _), _, _) <- graphToNodes cgData, "ConsList" `isInfixOf` nameStr (ptName cpt), not (null cts)] of
    (cpt:_) -> return cpt{ptVars=H.empty}
    []      -> fail "ConsList class not found"
  let notEqCO = UnionType (Just (PredsNot (PredsOne (PredClass eqCOPt)), H.empty, [])) H.empty []
  let eqCOTop = UnionType (Just (PredsOne (PredClass eqCOPt), H.empty, [])) H.empty []
  let lists = expandClassPartial typeEnv H.empty consListPt
  met <- timed "(⊤|¬EqCO) ∩ lists" $ intersectTypes typeEnv notEqCO lists
  annotate $ printf "lists = %s" (show lists)
  annotate $ printf "met = %s" (show met)
  assertTimed "met ⊆ lists" $ isSubtypeOf typeEnv met lists
  assertTimed "met ∩ ∀EqCO = ∅" $ isBottomType (intersectTypes typeEnv met eqCOTop)
  met2 <- timed "re-meet" $ intersectTypes typeEnv notEqCO met
  eqTimed "re-meet idempotent" met met2

-- The drop/has/vars parity triple: 'hasOutOfScopeTVExt' must be exactly "would
-- 'dropOutOfScopeTVExt' change the type", and 'outOfScopeTVExtVars' must agree on emptiness.
-- Pins the historically bug-prone family whose erase-to-⊤ became erase-to-inner.
propOutOfScopeParity :: IO GenPrgm -> Property
propOutOfScopeParity gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  vaenv <- forAll $ genVaenv prgm
  t <- forAll $ genType prgm
  dropped <- timed "drop" $ dropOutOfScopeTVExt vaenv t
  annotate $ printf "dropped = %s" (show dropped)
  hasOutOfScopeTVExt vaenv t === (dropped /= t)
  S.null (outOfScopeTVExtVars vaenv t) === not (hasOutOfScopeTVExt vaenv t)

-- Occurrence-reading soundness: an occurrence (@var ∩ inner@) is a subtype of its reference
-- reading, and of its inner alone.
propOccurrenceReadingSound :: IO GenPrgm -> Property
propOccurrenceReadingSound gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  o <- forAll $ genTypeVar prgm
  let readO = readTopVar typeEnv vaenv o
  annotate $ printf "read = %s" (show readO)
  assertTimed "o ⊆ read(o)" $ isSubtypeOfWithEnv typeEnv vaenv o readO
  case o of
    TypeVar _ _ i -> assertTimed "o ⊆ inner" $ isSubtypeOfWithEnv typeEnv vaenv o i
    _             -> return ()

-- | 'ptVars' is a BINDER: a 'TVInt' reference to a variable the enclosing partial declares
-- denotes that variable's binding, not ⊤. So @F[/E=B](.. $/E ..)@ and @F[/E=B](.. B ..)@ are the
-- SAME set, and every operation must decide them identically — both directions of both provers.
--
-- The failure mode this pins down is one-sided and silent. A traversal that descends from a
-- partial into its parts without extending the environment with that partial's own 'ptVars'
-- falls through to 'vaenvLookup', which defaults an unbound name to ⊤. Because ⊤ is the top of
-- the lattice, forgetting the binder can only WIDEN: such a prover over-approximates membership
-- and under-approximates difference, so it reports a genuine narrowing as a no-op. The provers
-- disagree about which pairs are equal, and a solver that asks one for "did this change?" and
-- another for "is this still narrowing?" can then accept an update forever
-- (@core\/set.ct@'s @allIn@ oscillated between the two spellings until the epoch limit).
--
-- 'genOwnVarBoundPair' places the reference at the whole slot, inside a nested positive partial's
-- dimension, and inside a CLASS PREDICATE's dimension; only the last reaches
-- 'complementTypeEnv'\/'predImpliesAtom'.
propOwnVarBindingRespected :: IO GenPrgm -> Property
propOwnVarBindingRespected gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll $ HG.filter prgmHasPlacement gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  (ref, sub) <- forAll $ genOwnVarBoundPair prgm
  annotate $ printf "ref = %s" (show ref)
  annotate $ printf "sub = %s" (show sub)
  assertTimed "ref ⊆ sub" $ isSubtypeOfWithEnv typeEnv vaenv ref sub
  assertTimed "sub ⊆ ref" $ isSubtypeOfWithEnv typeEnv vaenv sub ref
  refMinusSub <- timed "ref-sub" $ snd $ differenceTypeWithEnv typeEnv vaenv ref sub
  annotate $ printf "ref - sub = %s" (show refMinusSub)
  assertTimed "ref - sub = ∅" $ isBottomType refMinusSub
  subMinusRef <- timed "sub-ref" $ snd $ differenceTypeWithEnv typeEnv vaenv sub ref
  annotate $ printf "sub - ref = %s" (show subMinusRef)
  assertTimed "sub - ref = ∅" $ isBottomType subMinusRef

-- | A declared subclass edge with a parameter mapping — @every(ConsList[$T -> $_T], isa= [/Bag])@,
-- recorded under @Bag[$T]@ as the member template @∀ConsList[/T=$_/T]@ whose @$_/T@ names Bag's own
-- @/T@ — asserts @ConsList[T=k] ⊆ Bag[T=k]@ for EVERY @k@. So it proves @∀ConsList[/T=a] ⊆ ∀Bag[/T=b]@
-- exactly when @a ⊆ b@, and never for an unrelated or wider left element. The bounded declared-edge
-- fast path ('classDeclaredSubclassOfBounded') compared the left side against the RAW template (every
-- @$_/T@ read as ⊤) and proved the edge for ANY instantiation of the right side; the typechecker then
-- judged @Node(left=⊤) ≡ Node(left=∀Map[/K=Ord] ∪ …)@, refused a genuine requirement narrowing as a
-- no-op, and unrolled core/map.ct's recursive @insert@ result one level per epoch. The subtype walk
-- and the difference oracle must agree ('verifyScheme' backs one with the other).
propParamIsaEdgeCovariant :: IO Prgms -> Property
propParamIsaEdgeCovariant ioPrgms = property $ do
  prgms <- lift ioPrgms
  prgm <- case [v | (k, v) <- H.toList prgms, "containers" `isInfixOf` k] of
    (prgm:_) -> return prgm
    []       -> fail "containers.ct program not found"
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData, teNames} = mkTypeEnv prgm
  let findClass n = case [cpt | (CGClass (_, cpt, _, _), _, _) <- graphToNodes cgData, n `isInfixOf` nameStr (ptName cpt)] of
        (cpt:_) -> return cpt
        []      -> fail $ printf "%s class not found" n
  let findData n = case [nm | nm <- S.toList teNames, n `isInfixOf` nameStr nm] of
        (nm:_) -> return $ singletonType $ partialVal nm
        []     -> fail $ printf "%s type not found" n
  consPt <- findClass "ConsList"
  bagPt <- findClass "Bag"
  intT <- findData "Integer"
  pointT <- findData "Point"
  tKey <- case H.keys (ptVars consPt) of
    (k:_) | H.member k (ptVars bagPt) -> return k
    _ -> fail "ConsList and Bag do not share a type parameter"
  let clsOver cpt elemT = UnionType (Just (PredsOne (PredClass cpt{ptVars=H.singleton tKey elemT}), H.empty, [])) H.empty []
  let bare cpt = UnionType (Just (PredsOne (PredClass cpt{ptVars=H.empty}), H.empty, [])) H.empty []
  let sub a b = isSubtypeOf typeEnv a b
  let diffEmpty a b = isBottomType $ snd $ differenceTypeWithEnv typeEnv H.empty a b
  annotate $ printf "ConsList[Integer] = %s" (show (clsOver consPt intT))
  annotate $ printf "Bag[Integer] = %s" (show (clsOver bagPt intT))
  assertTimed "ConsList[Integer] ⊆ Bag[Integer]" $ sub (clsOver consPt intT) (clsOver bagPt intT)
  assertTimed "ConsList[Integer] ⊆ Bag" $ sub (clsOver consPt intT) (bare bagPt)
  assertTimed "ConsList[Point] ⊄ Bag[Integer]" $ not $ sub (clsOver consPt pointT) (clsOver bagPt intT)
  assertTimed "ConsList[⊤] ⊄ Bag[Integer]" $ not $ sub (clsOver consPt PTopType) (clsOver bagPt intT)
  assertTimed "ConsList ⊄ Bag[Integer]" $ not $ sub (bare consPt) (clsOver bagPt intT)
  assertTimed "ConsList[Integer] − Bag[Integer] = ∅" $ diffEmpty (clsOver consPt intT) (clsOver bagPt intT)
  assertTimed "ConsList[Point] − Bag[Integer] ≠ ∅" $ not $ diffEmpty (clsOver consPt pointT) (clsOver bagPt intT)
  assertTimed "ConsList[⊤] − Bag[Integer] ≠ ∅" $ not $ diffEmpty (clsOver consPt PTopType) (clsOver bagPt intT)

-- | Class membership reads a member's fields DECLARATION-BOUNDED, as the partial subtype does:
-- @Cons[/T=Integer](head=Integer ∪ Point, ..)@ denotes @head=Integer@ (no constructible @Cons@
-- holds a head outside its declared @$T@), so it is a @ConsList[/T=Integer]@ — the two provers
-- must agree, or a value proven inside the class's own member template is refused by the
-- class. A genuinely wider element parameter is still refused.
propDeclClampClassMembership :: IO Prgms -> Property
propDeclClampClassMembership ioPrgms = property $ do
  prgms <- lift ioPrgms
  prgm <- case [v | (k, v) <- H.toList prgms, "containers" `isInfixOf` k] of
    (prgm:_) -> return prgm
    []       -> fail "containers.ct program not found"
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData, teNames} = mkTypeEnv prgm
  let findClass n = case [cpt | (CGClass (_, cpt, _, _), _, _) <- graphToNodes cgData, n `isInfixOf` nameStr (ptName cpt)] of
        (cpt:_) -> return cpt
        []      -> fail $ printf "%s class not found" n
  let findName n = case [nm | nm <- S.toList teNames, n `isInfixOf` nameStr nm] of
        (nm:_) -> return nm
        []     -> fail $ printf "%s type not found" n
  consListPt <- findClass "ConsList"
  consN <- findName "Cons"
  intT <- singletonType . partialVal <$> findName "Integer"
  pointT <- singletonType . partialVal <$> findName "Point"
  tKey <- case H.keys (ptVars consListPt) of
    (k:_) -> return k
    []    -> fail "ConsList has no type parameter"
  (headKey, tailKey) <- case [(hk, tk) | PartialType{ptArgs=dArgs} <- typeGraphData typeEnv consN
                                       , hk <- H.keys dArgs, "head" `isInfixOf` nameStr (pkName hk)
                                       , tk <- H.keys dArgs, "tail" `isInfixOf` nameStr (pkName tk)] of
    (ks:_) -> return ks
    []     -> fail "Cons declares no head/tail"
  let consList elemT = UnionType (Just (PredsOne (PredClass consListPt{ptVars=H.singleton tKey elemT}), H.empty, [])) H.empty []
  let cons elemT h t = singletonType (partialVal consN){ptVars=H.singleton tKey elemT, ptArgs=H.fromList [(headKey, h), (tailKey, t)]}
  let wide = unionTypes typeEnv intT pointT
  let sub a b = isSubtypeOf typeEnv a b
  let diffEmpty a b = isBottomType $ snd $ differenceTypeWithEnv typeEnv H.empty a b
  let clamped = cons intT wide (consList wide)
  let clean = cons intT intT (consList intT)
  let wider = cons wide pointT (consList wide)
  annotate $ printf "clamped = %s" (show clamped)
  assertTimed "clean ⊆ ConsList[Integer]" $ sub clean (consList intT)
  assertTimed "clamped ⊆ ConsList[Integer]" $ sub clamped (consList intT)
  assertTimed "clamped ⊆ clean" $ sub clamped clean
  assertTimed "clamped − ConsList[Integer] = ∅" $ diffEmpty clamped (consList intT)
  assertTimed "wider ⊄ ConsList[Integer]" $ not $ sub wider (consList intT)
  assertTimed "wider − ConsList[Integer] ≠ ∅" $ not $ diffEmpty wider (consList intT)

-- | A constant excluded from a class top is irrelevant to a data leaf: @Cons(..)@ never holds a
-- constant, so @Cons ⊆ (U_EqCO − {-100}) ∪ Cons|¬EqCO@ exactly as @Cons ⊆ U_EqCO ∪ Cons|¬EqCO@
-- (every Cons is either an EqCO or not). The distributive law found the excluded constant
-- pushing the decision out of the engine's fragment, where the materialized cover could not
-- split the leaf between the overlapping class top and its predicated sibling. Fixture:
-- recursiveClassBounds.ct (@every(ConsList[$T -> EqCO], isa= [EqCO])@).
propExcludedConstantIrrelevantToData :: IO Prgms -> Property
propExcludedConstantIrrelevantToData ioPrgms = property $ do
  prgms <- lift ioPrgms
  prgm <- case [v | (k, v) <- H.toList prgms, "recursiveClassBounds" `isInfixOf` k] of
    (prgm:_) -> return prgm
    []       -> fail "recursiveClassBounds.ct program not found"
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData, teNames} = mkTypeEnv prgm
  let findClass n = case [cpt | (CGClass (_, cpt, _, _), _, _) <- graphToNodes cgData, n `isInfixOf` nameStr (ptName cpt)] of
        (cpt:_) -> return cpt
        []      -> fail $ printf "%s class not found" n
  let findName n = case [nm | nm <- S.toList teNames, n `isInfixOf` nameStr nm] of
        (nm:_) -> return nm
        []     -> fail $ printf "%s type not found" n
  eqcoPt <- findClass "EqCO"
  consListPt <- findClass "ConsList"
  consN <- findName "Cons"
  (headKey, tailKey) <- case [(hk, tk) | PartialType{ptArgs=dArgs} <- typeGraphData typeEnv consN
                                       , hk <- H.keys dArgs, "head" `isInfixOf` nameStr (pkName hk)
                                       , tk <- H.keys dArgs, "tail" `isInfixOf` nameStr (pkName tk)] of
    (ks:_) -> return ks
    []     -> fail "Cons declares no head/tail"
  tKey <- case H.keys (ptVars consListPt) of
    (k:_) -> return k
    []    -> fail "ConsList has no type parameter"
  let eqco = PredsOne (PredClass eqcoPt{ptVars=H.empty})
  let eqcoTop = UnionType (Just (eqco, H.empty, [])) H.empty []
  let eqcoTopMinus = UnionType (Just (eqco, H.empty, [CInt (-100)])) H.empty []
  let notEqco = UnionType (Just (PredsNot eqco, H.empty, [])) H.empty []
  let consList elemT = UnionType (Just (PredsOne (PredClass consListPt{ptVars=H.singleton tKey elemT}), H.empty, [])) H.empty []
  let cons = singletonType (partialVal consN){ptVars=H.singleton tKey PTopType, ptArgs=H.fromList [(headKey, TypeVar (TVVar tKey) TVInt PTopType), (tailKey, consList PTopType)]}
  let consNot = intersectTypes typeEnv cons notEqco
  let sub a b = isSubtypeOf typeEnv a b
  let diffEmpty a b = isBottomType $ snd $ differenceTypeWithEnv typeEnv H.empty a b
  annotate $ printf "Cons|¬EqCO = %s" (show consNot)
  assertTimed "Cons ⊆ EqCO ∪ Cons|¬EqCO" $ sub cons (unionTypes typeEnv eqcoTop consNot)
  assertTimed "Cons ⊆ (EqCO − {-100}) ∪ Cons|¬EqCO" $ sub cons (unionTypes typeEnv eqcoTopMinus consNot)
  assertTimed "Cons − ((EqCO − {-100}) ∪ Cons|¬EqCO) = ∅" $ diffEmpty cons (unionTypes typeEnv eqcoTopMinus consNot)
  assertTimed "Cons ⊄ (EqCO − {-100})" $ not $ sub cons eqcoTopMinus
  assertTimed "(EqCO − {-100}) ∪ Cons ≡ (EqCO − {-100}) ∪ Cons|¬EqCO" $
    isEqType typeEnv (unionTypes typeEnv eqcoTopMinus cons) (unionTypes typeEnv eqcoTopMinus consNot)

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
      , HG.testProperty "propIntersectionShrinks" (p $ propIntersectionShrinks gPrgm)
      , HG.testProperty "propUnionGrows" (p $ propUnionGrows gPrgm)
      , HG.testProperty "propUnionWithComplement" (p $ propUnionWithComplement gPrgm)
      , HG.testProperty "propIntersectionWithComplement" (p $ propIntersectionWithComplement gPrgm)
      , HG.testProperty "propComplementInverse" (p $ propComplementInverse gPrgm)
      , HG.testProperty "propDifferenceIdentity" (p $ propDifferenceIdentity gPrgm)
      , HG.testProperty "propDifferenceSubset" (p $ propDifferenceSubset gPrgm)
      , HG.testProperty "propIntersectByDifference" (p $ propIntersectByDifference gPrgm)
      , HG.testProperty "propDifferenceByIntersection" (p $ propDifferenceByIntersection gPrgm)
      , HG.testProperty "propParamClassCovariant" (p $ propParamClassCovariant gPrgm)
      , HG.testProperty "propSubtypeUnionVarSplit" (withDiscards 1000 $ p $ propSubtypeUnionVarSplit gPrgm)
      , HG.testProperty "propClassExpansionLeafSubtype" (withDiscards 1000 $ p $ propClassExpansionLeafSubtype gPrgm)
      , HG.testProperty "propClassGraphMatchesPredClass" (withDiscards 1000 $ p $ propClassGraphMatchesPredClass gPrgm)
      , HG.testProperty "propParamClassWideNotNarrow" (withTests 1 $ propParamClassWideNotNarrow ioPrgms)
      , HG.testProperty "propParamClassIntersectAbsorb" (withTests 1 $ propParamClassIntersectAbsorb ioPrgms)
      , HG.testProperty "propConjMemberClassSubtype" (withTests 1 $ propConjMemberClassSubtype ioPrgms)
      , HG.testProperty "propNegRecursiveClassMeet" (withTests 1 $ propNegRecursiveClassMeet ioPrgms)
      , HG.testProperty "propParamIsaEdgeCovariant" (withTests 1 $ propParamIsaEdgeCovariant ioPrgms)
      , HG.testProperty "propDeclClampClassMembership" (withTests 1 $ propDeclClampClassMembership ioPrgms)
      , HG.testProperty "propExcludedConstantIrrelevantToData" (withTests 1 $ propExcludedConstantIrrelevantToData ioPrgms)
      , HG.testProperty "propOutOfScopeParity" (p $ propOutOfScopeParity gPrgm)
      , HG.testProperty "propOccurrenceReadingSound" (p $ propOccurrenceReadingSound gPrgm)
      , HG.testProperty "propOwnVarBindingRespected" (withDiscards 1000 $ p $ propOwnVarBindingRespected gPrgm)
                                        ]
      where gPrgm = ggPrgm <$> ioPrgms
    p prop = prop
    -- p prop = withTests 100000 prop

retest :: IO ()
retest = defaultMain $ localOption (TP.TestPattern $ Just $ TT.ERE "propCompactIdempotent") $ localOption (HedgehogReplay $ Just ("1:a2", Seed 10220680066336263475 12463056215188250787)) typeTests

main :: IO ()
main = defaultMain typeTests
