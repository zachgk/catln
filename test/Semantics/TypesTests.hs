{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Semantics.TypesTests where

import           Common.TestCommon         (findCt)
import           Control.Exception         (AllocationLimitExceeded (..),
                                            evaluate, finally, try)
import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader      (ReaderT (..), ask)
import           Control.Monad.Trans       (lift)
import           CRes
import           CtService
import           Data.Graph                (graphFromEdges)
import qualified Data.HashMap.Strict       as H
import qualified Data.HashSet              as S
import           Data.Int                  (Int64)
import           Data.IORef
import           Data.List                 (intercalate, isInfixOf, isSuffixOf,
                                            sortOn)
import           Data.Maybe
import           GHC.Conc                  (disableAllocationLimit,
                                            enableAllocationLimit,
                                            setAllocationCounter)
import           GHC.Stack                 (HasCallStack, withFrozenCallStack)
import           Hedgehog
import qualified Hedgehog.Gen              as HG
import           MapMeta                   (clearMetaDat, mapMetaPrgm)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           System.Timeout            (timeout)
import           Test.Tasty
import           Test.Tasty.Hedgehog       as HG
import           Test.Tasty.HUnit          (assertFailure, testCase)
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

-- | A fixture program read on its own, outside the generator pool of 'findPrgms': a unit case
-- ('Unit') states its one claim against it without every generated law drawing from it too.
loadUnitPrgm :: String -> IO (Prgm Expr ())
loadUnitPrgm fileName = do
  prgm <- runCResT $ do
    ctss <- lift $ ctssBaseFiles testCtssConfig [fileName]
    tprgm <- getTPrgm ctss
    return $ mconcat $ map (mapMetaPrgm clearMetaDat . fst3) (graphToNodes tprgm)
  return $ fromCRes prgm

-- | The program a property draws: one of the fixture files or a generated program, each with
-- the same weight.
ggPrgm :: Prgms -> GenPrgm
ggPrgm = ggPrgmFor noNeed

-- | 'ggPrgm' held to a 'PrgmNeed', so that EVERY draw can state the property's law: a fixture
-- file is offered only when it meets the need, and 'genPrgmWith' builds the need into the
-- program it generates. Nothing is drawn and then rejected. A property that drew a program and
-- rejected it (@HG.filter@ over the program generator, or @discard@ after the draw) charged the
-- rejection to its discard budget, and once the iteration count was raised it reported "gave
-- up after N discards" — a FAIL that says nothing about the algebra: 'propExtensionKeepsProofs'
-- discarded every draw of @containers.ct@ that way (a quarter of its draws), and #40 twice
-- triaged give-ups of the class laws as class\/negation gaps. The suite now runs at hedgehog's
-- default discard limit.
--
-- No fixture meeting a need is a static fact about @test\/Semantics\/code\/@ and fails loudly.
ggPrgmFor :: PrgmNeed -> Prgms -> GenPrgm
ggPrgmFor need prgms = HG.choice [HG.element qualifying, genPrgmWith need]
  where
    qualifying = case [p | (_, p) <- sortOn fst (H.toList prgms), prgmSatisfies need p] of
      [] -> error "ggPrgmFor: no fixture program in test/Semantics/code/ meets the need"
      ps -> ps

-- | Reads a whole-type bare variable ('TVVar'\/'TVArg', or a 'TVApplied' over one) to its value
-- under the environment (an unbound one to ⊤). It deliberately does NOT descend into partials: a
-- variable inside a partial's arg\/var slot is resolved correctly by the operations themselves as
-- they thread the environment through each nested scope (a 'TVExt' slot reads the outer binding),
-- whereas a bare top-level variable is the one shape the meet mishandles (see 'intersectR').
readTopVar :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
readTopVar typeEnv env (TypeVar (TVApplied b ovr) loc i) = applyTypeVarOverrides typeEnv (fmap (readTopVar typeEnv env) ovr) (readTopVar typeEnv env (TypeVar b loc i))
-- The reference reading of an occurrence: @⟦var⟧ ∩ ⟦inner⟧@.
readTopVar typeEnv env (TypeVar v _ i)                    = meetInner typeEnv env (vaenvLookup typeEnv env v) (readTopVar typeEnv env i)
readTopVar _ _   t                                = t

-- | Intersection for the property tests, threading a seed 'TypeVarArgEnv'. A bare top-level variable
-- operand is read to its value first ('readTopVar'), because 'intersectTypesWithVarEnv' returns a
-- variable meet as the bare VARIABLE while recording the narrowing in its (here discarded) env — the
-- mergeVarEnvVal doctrine: @$a ∩ b@ keeps @$a@ symbolic and so reads standalone as @$a@, deliberately
-- wider so a call-site lambda's parameter can be refined later. The set-algebra laws
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
  annotate $ printf "f ⊆ d = %s" (show (isSubtypeOfWithEnvComplete typeEnv vaenv f d))
  annotate $ printf "d ⊆ f = %s" (show (isSubtypeOfWithEnvComplete typeEnv vaenv d f))
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
  annotate $ printf "f ⊆ d = %s" (show (isSubtypeOfWithEnvComplete typeEnv vaenv f d))
  annotate $ printf "d ⊆ f = %s" (show (isSubtypeOfWithEnvComplete typeEnv vaenv d f))
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
  aAb <- timed "a ⊆ a∪b" $ isSubtypeOfWithEnv typeEnv vaenv a ab
  bAb <- timed "b ⊆ a∪b" $ isSubtypeOfWithEnv typeEnv vaenv b ab
  annotate $ printf "a⊆a∪b=%s b⊆a∪b=%s" (show aAb) (show bAb)
  assert aAb
  assert bAb

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
-- | What fraction of 'genType''s output is each region of the 'Type' product. Asserted on ONE
-- property, because a distribution is a property of the generator and not of any single law, and
-- because each 'cover' costs an evaluation per draw.
--
-- 'label' and 'classify' only print; 'cover' FAILS, which is the point — a generator change that
-- quietly stops producing a whole region of the corpus is otherwise invisible: the generators
-- draw every shape by construction ('Testing.Generation.GenSpec'), so nothing downstream rejects
-- a collapsed region, and it would go unnoticed until a law happened to depend on it.
--
-- Minimums are roughly half the measured rate of the mixture the properties actually see
-- ('ggPrgm': half premade, half 'genPrgm', so the leaf-free @empty@ program is an eighth of every
-- draw), so they cannot flake at the default 100 iterations. Measured on that mixture, sizes 0-9
-- and 50-99 agreeing to within 0.6pp: plain leaf union 35.7-36.3%, subtractive top 27.9%
-- (⊤ 6.0 + predicate 16.3 + pred-free 0.8 + mixed 4.8), bare variable 11.1%, constants 19.4%,
-- non-exact arg mode 13.7-14.9%.
coverTypeShapes :: Type -> PropertyT IO ()
coverTypeShapes a = do
  cover 15 "plain positive leaf union" $ case a of
    UnionType Nothing ls [] -> not (H.null ls)
    _                       -> False
  cover 10 "subtractive top" $ case a of
    UnionType Just{} _ _ -> True
    _                    -> False
  -- 1%, not 4%: a bare variable is the rarest of the five shapes (genType draws one in about
  -- one draw in twenty, and 'cover' compares the OBSERVED percentage with no statistical
  -- allowance), so at the suite's default 100 tests a 4% target failed on an otherwise green
  -- run that drew two. The floor still states the shape must be reached.
  cover 1 "bare type variable" $ case a of
    TypeVar{} -> True
    _         -> False
  cover 6 "constants" $ case a of
    UnionType _ _ cs -> not (null cs)
    _                -> False
  cover 4 "non-exact arg mode" $ case a of
    UnionType _ ls _ -> any ((/= PtArgExact) . ptArgMode) (splitUnionType ls)
    _                -> False

propComplementInverse :: IO GenPrgm -> Property
propComplementInverse gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  coverTypeShapes a
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


-- | What a check needs of the monad it runs in, so one set of budgeted checks ('forceTimed',
-- 'timed', 'assertTimed', 'eqTimed') serves both a generated law (hedgehog's 'PropertyT', whose
-- failure reports the shrunk counterexample) and a unit case ('Unit', a tasty-hunit test whose
-- failure reports the case's notes).
class MonadIO m => MonadCheck m where
  -- | Record context to show beside a failure.
  note :: HasCallStack => String -> m ()
  -- | Fail the check with a message.
  failWith :: HasCallStack => String -> m a
  -- | Run an action whose exception fails the check instead of escaping it.
  checkIO :: HasCallStack => IO a -> m a
  -- | Fail unless the two sides are equal, showing both.
  checkEq :: (HasCallStack, Eq a, Show a) => String -> a -> a -> m ()

instance MonadCheck (PropertyT IO) where
  note = withFrozenCallStack annotate
  failWith msg = withFrozenCallStack $ footnote msg >> failure
  checkIO = withFrozenCallStack evalIO
  checkEq stage x y = withFrozenCallStack $ do
    unless (x == y) $ footnote stage
    x === y

-- | A unit case: 'IO' carrying the notes recorded so far, so a failing check reports them the way
-- a failing property reports its annotations. 'unitCase' runs it as a tasty-hunit test; an
-- exception escaping it is the test's error, as under HUnit.
newtype Unit a = Unit (ReaderT (IORef [String]) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadFail Unit where
  fail = failWith

instance MonadCheck Unit where
  note n = Unit $ ask >>= \notes -> liftIO (modifyIORef notes (n :))
  failWith msg = Unit $ do
    notes <- ask >>= liftIO . readIORef
    liftIO $ assertFailure $ intercalate "\n" (reverse notes ++ [msg])
  checkIO = liftIO
  checkEq stage x y = unless (x == y) $ failWith $ printf "%s: the sides differ\n  lhs: %s\n  rhs: %s" stage (show x) (show y)

-- | A unit case as a tasty-hunit test.
unitCase :: TestName -> Unit () -> TestTree
unitCase name (Unit m) = testCase name $ newIORef [] >>= runReaderT m

-- | Deep-force a stage's result under a wall-clock budget: a case that exceeds it fails the
-- check, so under hedgehog the property SHRINKS toward a minimal pathological input (a livelocked
-- op otherwise hangs the suite with no reported counterexample; deep-forcing via 'show' reaches
-- every thunk).
--
-- The stage is also held to an allocation budget: a case whose meet blows up can fill the heap
-- before the clock runs out, and heap exhaustion ends the whole suite with no counterexample
-- (@propIntersectByDifference@ did, at 3000 cases). An allocation limit is thrown to the forcing
-- thread as an exception, which fails the check like the timeout does.
forceTimed :: (HasCallStack, Show a, MonadCheck m) => String -> a -> m ()
forceTimed stage x = do
  r <- checkIO $ try $ timeout 5000000 $ do
    setAllocationCounter stageAllocationBudget
    enableAllocationLimit
    n <- evaluate (length (show x)) `finally` disableAllocationLimit
    return n
  case r of
    Right (Just _) -> pure ()
    Right Nothing -> failWith $ printf "TIMEOUT (5s) forcing stage: %s" stage
    Left AllocationLimitExceeded -> failWith $ printf "ALLOCATION LIMIT (%d bytes) forcing stage: %s" stageAllocationBudget stage

-- | The bytes one stage may allocate while it is forced ('forceTimed'), a quarter of the suite's
-- heap: past it a case is a runaway, not a slow one.
stageAllocationBudget :: Int64
stageAllocationBudget = 1500 * 1024 * 1024

-- | 'forceTimed' that returns the value, for binding a stage's result:
-- @v <- timed "a∩b" $ intersectR …@. EVERY property and unit case below routes its set operations
-- through 'timed'\/'assertTimed'\/'eqTimed' rather than a bare @let@\/@assert@\/@===@, so a
-- livelocked operation FAILS (and hedgehog shrinks it to a minimal reproducing input) instead of
-- spinning forever with no counterexample. A silent livelock here once burned hours of wall clock:
-- the suite pinned a core at multi-GB residency with nothing to debug, and even survived its own
-- @timeout@ (SIGTERM is ignored while the RTS spins in a non-allocating loop — use
-- @timeout -s KILL@ around the suite as a backstop).
timed :: (HasCallStack, Show a, MonadCheck m) => String -> a -> m a
timed stage x = forceTimed stage x >> return x

-- | Assert under the force budget (see 'timed'); a failure names the stage.
assertTimed :: (HasCallStack, MonadCheck m) => String -> Bool -> m ()
assertTimed stage b = forceTimed stage b >> unless b (failWith $ printf "FAILED: %s" stage)

-- | Equality under the force budget on both sides (see 'timed').
eqTimed :: (HasCallStack, Eq a, Show a, MonadCheck m) => String -> a -> a -> m ()
eqTimed stage x y = do
  forceTimed (stage ++ " lhs") x
  forceTimed (stage ++ " rhs") y
  checkEq stage x y
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
propSubtypeUnionVarSplit :: IO Prgms -> Property
propSubtypeUnionVarSplit ioPrgms = property $ do
  prgms <- lift ioPrgms
  -- A program 'genPartialType' can draw from ('objectNeed').
  prgm <- forAll $ ggPrgmFor objectNeed prgms

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
-- | What the algebra PROVES about a pair of types: @a ⊆ b@, @a ∩ b = ∅@ and @a − b = ∅@. A refusal
-- is not a claim, so only the 'True' answers are facts to be checked elsewhere.
data ProvedFacts = ProvedFacts { pfSubtype :: Bool, pfMeetEmpty :: Bool, pfDiffEmpty :: Bool }
  deriving (Eq, Show)

provedFacts :: TypeEnv (ObjArrTypeGraph Expr ()) -> TypeVarArgEnv -> Type -> Type -> PropertyT IO ProvedFacts
provedFacts typeEnv vaenv a b = do
  sub <- timed "a ⊆ b" $ isSubtypeOfWithEnv typeEnv vaenv a b
  ab <- timed "a∩b" $ intersectR typeEnv vaenv a b
  aMinusB <- timed "a−b" $ intersectR typeEnv vaenv a (complementTypeEnv typeEnv vaenv b)
  meetEmpty <- timed "a∩b = ∅" $ provedEmpty ab
  diffEmpty <- timed "a−b = ∅" $ provedEmpty aMinusB
  annotate $ printf "a∩b = %s" (show ab)
  annotate $ printf "a−b = %s" (show aMinusB)
  return (ProvedFacts sub meetEmpty diffEmpty)
  where
    provedEmpty t = isBottomType (compactType typeEnv vaenv t) || isSubtypeOfWithEnv typeEnv vaenv t BottomType

-- | Every fact proved in the first reading also holds in the second: the second may prove MORE, never
-- less. A refusal that turns into a proof is fine; a proof that turns into a refusal is not.
assertFactsKept :: String -> ProvedFacts -> ProvedFacts -> PropertyT IO ()
assertFactsKept reading before kept = do
  classify "a ⊆ b proved" (pfSubtype before)
  classify "a ∩ b = ∅ proved" (pfMeetEmpty before)
  classify "a − b = ∅ proved" (pfDiffEmpty before)
  annotate $ printf "%s: before = %s" reading (show before)
  annotate $ printf "%s: after  = %s" reading (show kept)
  assertTimed (reading ++ ": a ⊆ b kept") (not (pfSubtype before) || pfSubtype kept)
  assertTimed (reading ++ ": a ∩ b = ∅ kept") (not (pfMeetEmpty before) || pfMeetEmpty kept)
  assertTimed (reading ++ ": a − b = ∅ kept") (not (pfDiffEmpty before) || pfDiffEmpty kept)

-- | Open-world soundness: a later file may create a NEW type and declare it a member of an UNSEALED
-- class (@every(X isa C)@), and everything proved before that declaration must still hold after it
-- — that is what lets a program be compiled in stages. The extension adds exactly the class-graph
-- node the declaration desugars to, merged into @C@'s existing node.
--
-- Only a new type joins. A type already in scope with no @every@ into a class never gains one
-- (Zach's ruling, 2026-10-03; 'knownMemberName'), so a proof that reads such a type's memberships
-- from the declarations is not one a later file can break, and the extension never names one.
--
-- The extension is what @data(ExtensionMember)@ followed by @every(ExtensionMember, isa= [C])@
-- desugars to: the member's OBJECT and its @CGType@ node ('desTypeDef'), and a membership node
-- merged into @C@'s ('desClassDef'; 'mergeClassPartials' keeps @C@'s declared partial and
-- unions the members). The object matters: 'teNames' and 'typeGraphData' are read from the
-- object map, so without it the new type is outside the universe and 'knownMemberName' does
-- not know it — a weaker world than the one a later file creates. The member is nullary, so
-- for a PARAMETERIZED class it is a member at every parameter, exactly as @containers.ct@'s
-- @every(ConsList, isa= [/List])@ joins @List[$T]@; choosing parameter bindings for a
-- parameterized member is a different question. Every unsealed class of the program is drawn
-- from ('openClassNeed' guarantees one).
propExtensionKeepsProofs :: IO Prgms -> Property
propExtensionKeepsProofs ioPrgms = property $ do
  prgms <- lift ioPrgms
  prgm <- forAll $ ggPrgmFor openClassNeed prgms
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cg, teNames} = mkTypeEnv prgm
  let openClasses = [ cpt | (CGClass False cpt _ _, _, _) <- graphToNodes cg ]
  cls <- forAll $ HG.element openClasses
  let member = head [ nm | i <- [0 :: Int ..], let nm = mkName (printf "/ExtensionMember%d" i), not (S.member nm teNames) ]
  let memberObj = ObjArr (Just (Value (emptyMetaT (typeVal member)) member)) TypeObj Nothing [] Nothing
      extObjs = objectMapFromList [memberObj]
      extension = Prgm extObjs (classGraphFromObjs extObjs <> ClassGraph (graphFromEdges
        [ (CGClass False (partialVal (ptName cls)) [typeVal member] Nothing
          , PClassName (ptName cls)
          , [PTypeName member]) ])) []
  let prgmExt = prgm <> extension
  let typeEnvExt = mkTypeEnv prgmExt
  annotate $ printf "extension: every(%s isa %s)" (show member) (show (ptName cls))
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  before <- provedFacts typeEnv vaenv a b
  extended <- provedFacts typeEnvExt vaenv a b
  assertFactsKept "extended program" before extended

-- | The open world never proves more than the closed one: whatever holds for every program that
-- might extend this one holds for this program as declared ('tePrgmEnv'), where every class has
-- exactly its declared members. An open-world proof the closed world cannot reproduce was proved
-- from something other than the declarations.
propOpenProofsHoldClosed :: IO GenPrgm -> Property
propOpenProofsHoldClosed gPrgm = property $ do
  gPrgm' <- lift gPrgm
  prgm <- forAll gPrgm'
  let typeEnv = mkTypeEnv prgm
  let typeEnvClosed = typeEnv{tePrgmEnv = True}
  vaenv <- forAll $ genVaenv prgm
  a <- forAll $ genType prgm
  b <- forAll $ genType prgm
  open <- provedFacts typeEnv vaenv a b
  closed <- provedFacts typeEnvClosed vaenv a b
  assertFactsKept "closed world" open closed

propClassExpansionLeafSubtype :: IO Prgms -> Property
propClassExpansionLeafSubtype ioPrgms = property $ do
  prgms <- lift ioPrgms
  prgm <- forAll $ ggPrgmFor memberClassNeed prgms
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData} = mkTypeEnv prgm
  -- Draw from the QUALIFYING classes, chosen before the draw: those with members whose
  -- expansion is a positive union. A class whose expansion is a predicate top or a variable
  -- (@Option[$T]@'s @$_T@ member, @EqCS@'s @ConsList[$T -> /EqCS]@, @List@\/@Bag@ reached
  -- through @every@ templates) cannot state this law's premise and is not drawn — a coverage
  -- limit of the law as written, not a discard; a generated class has nullary members, so
  -- every generated program qualifies, and each fixture with a member class has one.
  let candidates = [ (cpt, splitUnionType leafs)
                   | (CGClass _ cpt cts _, _, _) <- graphToNodes cgData, not (null cts)
                   , UnionType Nothing leafs _ <- [expandClassPartial typeEnv H.empty cpt]
                   , not (H.null leafs) ]
  (cpt, leaves) <- forAll $ HG.element candidates
  let clsT = classPartial cpt
  leaf <- forAll $ HG.element leaves
  let erased = leaf{ptArgs = fmap (const PTopType) (ptArgs leaf)}
  annotate $ printf "class = %s" (show clsT)
  annotate $ printf "leaf = %s" (show leaf)
  annotate $ printf "erased = %s" (show erased)
  assertTimed "leaf ⊆ class" $ isSubtypeOf typeEnv (singletonType leaf) clsT
  assertTimed "erased leaf ⊆ class" $ isSubtypeOf typeEnv (singletonType erased) clsT

propClassGraphMatchesPredClass :: IO Prgms -> Property
propClassGraphMatchesPredClass ioPrgms = property $ do
  prgms <- lift ioPrgms
  prgm <- forAll $ ggPrgmFor sealedClassNeed prgms
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData} = mkTypeEnv prgm
  -- SEALED classes only, chosen BEFORE the draw ('sealedClassNeed' guarantees one). For an
  -- unsealed class the law does not hold: a future file may add implementations.
  let classNodes = [ (classPartialType, constituentTypes, className)
                   | (CGClass True classPartialType constituentTypes _, className, _) <- graphToNodes cgData ]
  (classPartialType, constituentTypes, className) <- forAll $ HG.element classNodes
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

-- | A parameterized class over ⊤ must not imply the same class over a narrower element:
-- @∀ConsList[⊤]@ contains a list of ANY element (e.g. a list of lists), which
-- @∀ConsList[Integer]@ excludes. The symbolic engine once claimed this implication: class
-- members enumerate with an uninstantiated parameter left flexible, and the ∀-cover escape
-- check ('partialCoversSat') bound the flexible slot on demand toward being covered, so no
-- member ever escaped the narrower class. Also pins the sound directions: covariance upward,
-- and the class's member expansion staying within the class (flexible-vs-flexible cover).
unitParamClassWideNotNarrow :: IO Prgms -> Unit ()
unitParamClassWideNotNarrow ioPrgms = do
  prgms <- liftIO ioPrgms
  prgm <- case [v | (k, v) <- H.toList prgms, "containers" `isInfixOf` k] of
    (prgm:_) -> return prgm
    []       -> fail "containers.ct program not found"
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData, teNames} = mkTypeEnv prgm
  clsPt <- case [cpt | (CGClass _ cpt cts _, _, _) <- graphToNodes cgData, "ConsList" `isInfixOf` nameStr (ptName cpt), not (null cts)] of
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
  note $ printf "wide = %s" (show wide)
  note $ printf "narrow = %s" (show narrow)
  note $ printf "bare = %s" (show bare)
  note $ printf "expanded = %s" (show expanded)
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
unitParamClassIntersectAbsorb :: IO Prgms -> Unit ()
unitParamClassIntersectAbsorb ioPrgms = do
  prgms <- liftIO ioPrgms
  prgm <- case [v | (k, v) <- H.toList prgms, "containers" `isInfixOf` k] of
    (prgm:_) -> return prgm
    []       -> fail "containers.ct program not found"
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData, teNames} = mkTypeEnv prgm
  clsPt <- case [cpt | (CGClass _ cpt cts _, _, _) <- graphToNodes cgData, "ConsList" `isInfixOf` nameStr (ptName cpt), not (null cts)] of
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
  note $ printf "flexed = %s" (show flexed)
  note $ printf "narrow = %s" (show narrow)
  note $ printf "flexed ∩ narrow = %s" (show met)
  -- The meet must not widen: it stays within the concrete-bounded form.
  assertTimed "met ⊆ narrow" $ isSubtypeOf typeEnv met narrow
  -- And the concrete-bounded form stays within the meet (the meet is exactly the narrow form).
  assertTimed "narrow ⊆ met" $ isSubtypeOf typeEnv narrow met
  -- The same meet through a CONJUNCTION with a second class (the shape a completed data field
  -- takes: the field's class with an unresolved element ∧ a supertype class with the bound):
  -- @(ConsList[$_v] ∧ List[Int]) ∩ (ConsList[Int] ∧ List[Int])@ must also keep the bound.
  listPt <- case [cpt | (CGClass _ cpt _ _, _, _) <- graphToNodes cgData, "List" `isInfixOf` nameStr (ptName cpt), not ("ConsList" `isInfixOf` nameStr (ptName cpt))] of
    (cpt:_) -> return cpt
    []      -> fail "List class not found"
  lKey <- case H.keys (ptVars listPt) of
    (k:_) -> return k
    []    -> fail "List class has no type parameter"
  let conj elemA elemB = UnionType (Just (predsAnd (PredsOne (PredClass clsPt{ptVars=H.singleton tKey elemA})) (PredsOne (PredClass listPt{ptVars=H.singleton lKey elemB})), H.empty, [])) H.empty []
  let flexConj = conj flexVar intT
  let narrowConj = conj intT intT
  metConj <- timed "flexConj ∩ narrowConj" $ resolvedIntersect flexConj narrowConj
  note $ printf "flexConj = %s" (show flexConj)
  note $ printf "narrowConj = %s" (show narrowConj)
  note $ printf "flexConj ∩ narrowConj = %s" (show metConj)
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
unitConjMemberClassSubtype :: IO Prgms -> Unit ()
unitConjMemberClassSubtype ioPrgms = do
  prgms <- liftIO ioPrgms
  prgm <- case [v | (k, v) <- H.toList prgms, "containers" `isInfixOf` k] of
    (prgm:_) -> return prgm
    []       -> fail "containers.ct program not found"
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData, teNames} = mkTypeEnv prgm
  consListPt <- case [cpt | (CGClass _ cpt cts _, _, _) <- graphToNodes cgData, "ConsList" `isInfixOf` nameStr (ptName cpt), not (null cts)] of
    (cpt:_) -> return cpt
    []      -> fail "ConsList class not found"
  bagPt <- case [cpt | (CGClass _ cpt _ _, _, _) <- graphToNodes cgData, "Bag" `isInfixOf` nameStr (ptName cpt)] of
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
  note $ printf "conj = %s" (show conj)
  note $ printf "conjBare = %s" (show conjBare)
  note $ printf "narrowExpanded = %s" (show narrowExpanded)
  assertTimed "conj ⊆ ConsList[Int]" $ isSubtypeOf typeEnv conj narrowExpanded
  assertTimed "conjBare ⊆ ConsList[Int]" $ isSubtypeOf typeEnv conjBare narrowExpanded

-- | The ¬-over-recursive-class meet terminates and stays exact — the minimal repro of the
-- 2026-08 value-level livelock: @(⊤|¬EqCO) ∩ (Cons + Nil canonical expansion)@ diverged by
-- materializing the negation into subtraction leafs that re-introduce it through the
-- recursive @tail@ slot, bit-identically forever. The meet must instead attach the
-- negation symbolically ('partialAddPreds'), drop the uninhabited @Nil{¬EqCO}@ (@Nil@ IS
-- an @EqCO@ member through @every(ConsList[$T -> EqCO], isa= [EqCO])@), and the result
-- must sit inside the positive operand, be disjoint from @∀EqCO@, and re-meet to itself.
unitNegRecursiveClassMeet :: IO Prgms -> Unit ()
unitNegRecursiveClassMeet ioPrgms = do
  prgms <- liftIO ioPrgms
  prgm <- case [v | (k, v) <- H.toList prgms, "recursiveClassBounds" `isInfixOf` k] of
    (prgm:_) -> return prgm
    []       -> fail "recursiveClassBounds.ct program not found"
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData} = mkTypeEnv prgm
  eqCOPt <- case [cpt | (CGClass _ cpt _ _, _, _) <- graphToNodes cgData, "EqCO" `isInfixOf` nameStr (ptName cpt)] of
    (cpt:_) -> return cpt
    []      -> fail "EqCO class not found"
  consListPt <- case [cpt | (CGClass _ cpt cts _, _, _) <- graphToNodes cgData, "ConsList" `isInfixOf` nameStr (ptName cpt), not (null cts)] of
    (cpt:_) -> return cpt{ptVars=H.empty}
    []      -> fail "ConsList class not found"
  let notEqCO = UnionType (Just (PredsNot (PredsOne (PredClass eqCOPt)), H.empty, [])) H.empty []
  let eqCOTop = UnionType (Just (PredsOne (PredClass eqCOPt), H.empty, [])) H.empty []
  let lists = expandClassPartial typeEnv H.empty consListPt
  met <- timed "(⊤|¬EqCO) ∩ lists" $ intersectTypes typeEnv notEqCO lists
  note $ printf "lists = %s" (show lists)
  note $ printf "met = %s" (show met)
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
  let typeEnv = mkTypeEnv prgm
  dropped <- timed "drop" $ dropOutOfScopeTVExt typeEnv vaenv t
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
propOwnVarBindingRespected :: IO Prgms -> Property
propOwnVarBindingRespected ioPrgms = property $ do
  prgms <- lift ioPrgms
  prgm <- forAll $ ggPrgmFor placementNeed prgms
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

-- | A 'TVParent' reference resolves through its IMMEDIATELY enclosing partial's binding, and
-- through nothing else: not the like-named parameter of the nested partial it is the value of (the
-- shadowing scope it is written across), and not a like-named ambient binding. So
-- @B[/v=K](slot = N[/v=$^/v])@ and @B[/v=K](slot = N[/v=K])@ are ONE set — both directions of
-- both provers, as 'propOwnVarBindingRespected' asks of the internal reference — and they stay
-- one set when the ambient env binds @/v@ to a decoy, the capture 'TypeVarLoc' says must never
-- happen. The pair is the core idiom @Cons[$T](tail -> ConsList[/T=$^/T])@ with the parent's
-- parameter bound.
propParentVarBindingRespected :: IO Prgms -> Property
propParentVarBindingRespected ioPrgms = property $ do
  prgms <- lift ioPrgms
  prgm <- forAll $ ggPrgmFor fieldPlacementNeed prgms
  let typeEnv = mkTypeEnv prgm
  vaenv0 <- forAll $ genVaenv prgm
  (ref, sub, alt, varName) <- forAll $ genParentVarBoundPair prgm
  -- Two thirds of the draws bind the parent's parameter NAME in the ambient env to something
  -- else, so a reader that resolves the parent reference by name against the ambient layer
  -- (instead of the enclosing partial's binding) reads the decoy and the pair comes apart.
  decoy <- forAll $ HG.frequency [(2, Just <$> genGroundLeafBound prgm), (1, return Nothing)]
  let vaenv = maybe vaenv0 (\d -> H.insert (extKey (TVVar varName)) d vaenv0) decoy
  classify "decoy bound in the ambient env" (isJust decoy)
  annotate $ printf "vaenv = %s" (show vaenv)
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
  -- The reference reads as its BINDING, not as a flexible slot that matches anything: against
  -- the third member, whose nested parameter holds an unrelated bound, it answers exactly as
  -- its binding does. The coverage guard keeps the check from being satisfied on related bounds
  -- alone, where both answers agree for the wrong reason.
  annotate $ printf "alt = %s" (show alt)
  subAlt <- timed "sub⊆alt" $ isSubtypeOfWithEnv typeEnv vaenv sub alt
  altSub <- timed "alt⊆sub" $ isSubtypeOfWithEnv typeEnv vaenv alt sub
  cover 20 "alt unrelated to sub" (not subAlt && not altSub)
  refAlt <- timed "ref⊆alt" $ isSubtypeOfWithEnv typeEnv vaenv ref alt
  altRef <- timed "alt⊆ref" $ isSubtypeOfWithEnv typeEnv vaenv alt ref
  annotate $ printf "sub⊆alt=%s alt⊆sub=%s ref⊆alt=%s alt⊆ref=%s" (show subAlt) (show altSub) (show refAlt) (show altRef)
  assertTimed "ref ⊆ alt ⇔ sub ⊆ alt" $ refAlt == subAlt
  assertTimed "alt ⊆ ref ⇔ alt ⊆ sub" $ altRef == altSub

-- | A declared subclass edge with a parameter mapping — @every(ConsList[$T -> $_T], isa= [/Bag])@,
-- recorded under @Bag[$T]@ as the member template @∀ConsList[/T=$_/T]@ whose @$_/T@ names Bag's own
-- @/T@ — asserts @ConsList[T=k] ⊆ Bag[T=k]@ for EVERY @k@. So it proves @∀ConsList[/T=a] ⊆ ∀Bag[/T=b]@
-- exactly when @a ⊆ b@, and never for an unrelated or wider left element. The bounded declared-edge
-- fast path ('classDeclaredSubclassOfBounded') compared the left side against the RAW template (every
-- @$_/T@ read as ⊤) and proved the edge for ANY instantiation of the right side; the typechecker then
-- judged @Node(left=⊤) ≡ Node(left=∀Map[/K=Ord] ∪ …)@, refused a genuine requirement narrowing as a
-- no-op, and unrolled core/map.ct's recursive @insert@ result one level per epoch. The subtype walk
-- and the difference oracle must agree ('verifyScheme' backs one with the other).
unitParamIsaEdgeCovariant :: IO Prgms -> Unit ()
unitParamIsaEdgeCovariant ioPrgms = do
  prgms <- liftIO ioPrgms
  prgm <- case [v | (k, v) <- H.toList prgms, "containers" `isInfixOf` k] of
    (prgm:_) -> return prgm
    []       -> fail "containers.ct program not found"
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData, teNames} = mkTypeEnv prgm
  let findClass n = case [cpt | (CGClass _ cpt _ _, _, _) <- graphToNodes cgData, n `isInfixOf` nameStr (ptName cpt)] of
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
  note $ printf "ConsList[Integer] = %s" (show (clsOver consPt intT))
  note $ printf "Bag[Integer] = %s" (show (clsOver bagPt intT))
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
unitDeclClampClassMembership :: IO Prgms -> Unit ()
unitDeclClampClassMembership ioPrgms = do
  prgms <- liftIO ioPrgms
  prgm <- case [v | (k, v) <- H.toList prgms, "containers" `isInfixOf` k] of
    (prgm:_) -> return prgm
    []       -> fail "containers.ct program not found"
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData, teNames} = mkTypeEnv prgm
  let findClass n = case [cpt | (CGClass _ cpt _ _, _, _) <- graphToNodes cgData, n `isInfixOf` nameStr (ptName cpt)] of
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
  note $ printf "clamped = %s" (show clamped)
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
-- | A constant met with a leaf that holds it by shape, under a value predicate nothing decides at
-- it, is the constant within that predicate: @{3} ∩ /Integer | P@ is the point leaf
-- @/Integer | PredConstEq 3 ∧ P@, as the meet with the top spelling of the same set
-- (@⊤ | P ∧ PredRel Integer@) gives it where @Integer@ is declared. It was ∅, the constant read as
-- outside @P@, so a refined argument given a literal (@f(a= 3)@ for @a -> Integer_nonNeg@) was
-- refused at tree build.
unitConstantWithinValuePredicate :: IO Prgms -> Unit ()
unitConstantWithinValuePredicate ioPrgms = do
  prgms <- liftIO ioPrgms
  prgm <- case [v | (k, v) <- H.toList prgms, "recursiveClassBounds" `isInfixOf` k] of
    (prgm:_) -> return prgm
    []       -> fail "recursiveClassBounds.ct program not found"
  let typeEnv = mkTypeEnv prgm
  let e = (partialVal "/undecidedProp"){ptArgs = H.singleton (partialKey "this") (singletonType (partialVal "/$this"))}
  let refinedLeaf = singletonType intLeaf{ptPreds = PredsOne (PredExpr e)}
  let c = constantType (CInt 3)
  let viaLeaf = intersectTypes typeEnv c refinedLeaf
  note $ printf "viaLeaf = %s" (show viaLeaf)
  assertTimed "{3} ∩ leaf ≠ ∅" $ not (isBottomType viaLeaf)
  assertTimed "{3} ∩ leaf = leaf ∩ {3}" $ isEqType typeEnv viaLeaf (intersectTypes typeEnv refinedLeaf c)
  assertTimed "{3} ∩ leaf ⊆ {3}" $ isSubtypeOf typeEnv viaLeaf c
  assertTimed "{3} ∩ leaf ⊆ leaf" $ isSubtypeOf typeEnv viaLeaf refinedLeaf

unitExcludedConstantIrrelevantToData :: IO Prgms -> Unit ()
unitExcludedConstantIrrelevantToData ioPrgms = do
  prgms <- liftIO ioPrgms
  prgm <- case [v | (k, v) <- H.toList prgms, "recursiveClassBounds" `isInfixOf` k] of
    (prgm:_) -> return prgm
    []       -> fail "recursiveClassBounds.ct program not found"
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData, teNames} = mkTypeEnv prgm
  let findClass n = case [cpt | (CGClass _ cpt _ _, _, _) <- graphToNodes cgData, n `isInfixOf` nameStr (ptName cpt)] of
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
  note $ printf "Cons|¬EqCO = %s" (show consNot)
  assertTimed "Cons ⊆ EqCO ∪ Cons|¬EqCO" $ sub cons (unionTypes typeEnv eqcoTop consNot)
  assertTimed "Cons ⊆ (EqCO − {-100}) ∪ Cons|¬EqCO" $ sub cons (unionTypes typeEnv eqcoTopMinus consNot)
  assertTimed "Cons − ((EqCO − {-100}) ∪ Cons|¬EqCO) = ∅" $ diffEmpty cons (unionTypes typeEnv eqcoTopMinus consNot)
  assertTimed "Cons ⊄ (EqCO − {-100})" $ not $ sub cons eqcoTopMinus
  assertTimed "(EqCO − {-100}) ∪ Cons ≡ (EqCO − {-100}) ∪ Cons|¬EqCO" $
    isEqType typeEnv (unionTypes typeEnv eqcoTopMinus cons) (unionTypes typeEnv eqcoTopMinus consNot)

-- | A cover pattern's pointer slot reads through the pattern's OWN parameter bound, in both
-- provers: an arm @bap[/M=∀Mon](fn=$M, m=$M)@ holds only values whose @fn@ result is a @Mon@,
-- so a definition @bap[/M=∀Mon](fn=⊤, m=$M)@ is not inside it. The symbolic cover read the
-- arm's @fn=$M@ as a flexible (unbound) variable — covering every result — while the
-- materialized prover read it as @∀Mon@, so @def ⊆ arm@ was refused while @def − arm@ compacted
-- to ∅ (the leaf's @¬PredRel arm@ refinement was decided "always true"). Surfaced as a
-- "Definition Outside Declaration" with an empty undeclared part once core's [bindap] carried
-- a second arm.
--
-- The fixture sits outside the generator pool (@test/Semantics/unit/@): the general laws fail
-- on its overloaded arms in ways this fix does not touch (e.g. @propDifferenceSubset@ keeps a
-- @/bap[..](fn=/bap[/B=⊤]|¬Mon, ..)@ leaf of @a+b−a@ outside @b@, and meeting the two arms
-- spells @/m=$/M∩{Fun ∧ Mon}@), pending their own fixes.
unitOwnParamPointerCover :: Unit ()
unitOwnParamPointerCover = do
  prgm <- liftIO $ loadUnitPrgm "test/Semantics/unit/overloadedHO.ct"
  let typeEnv = mkTypeEnv prgm
  let Prgm{prgmObjMap=ObjectMap om} = prgm
  arms <- case [omiDecls omi | (n, omi) <- H.toList om, "bap" `isInfixOf` nameStr n] of
    (ds@(_:_:_):_) -> return $ map (getMetaType . getExprMeta . oaObjExpr) ds
    _              -> fail "bap does not declare two arms"
  let monArm = [a | a <- arms, "Mon" `isInfixOf` show a]
  (monPt, monArmT) <- case monArm of
    [a@(UnionType Nothing leafs [])] | [pt] <- splitUnionType leafs -> return (pt, a)
    _ -> fail "no single-leaf Mon arm"
  -- The definition, spelled as inference completes core's mmap arm definition: only the
  -- parameter its arguments use (@/M@, its element read as ⊤) and the @fn@ result widened to ⊤.
  fnKey <- case [k | k <- H.keys (ptArgs monPt), not (S.null (pkArgs k))] of
    (k:_) -> return k
    []    -> fail "the Mon arm has no higher-order argument"
  mKey <- case [k | k <- H.keys (ptVars monPt), "M" `isInfixOf` nameStr (pkName k)] of
    (k:_) -> return k
    []    -> fail "the Mon arm has no /M parameter"
  let monTop = dropOutOfScopeTVExt typeEnv H.empty <$> H.lookup mKey (ptVars monPt)
  let defT = singletonType monPt{ptVars = maybe H.empty (H.singleton mKey) monTop, ptArgs = H.insert fnKey PTopType (ptArgs monPt)}
  let armRel = TopType H.empty (PredsOne (PredRel monPt))
  let armsT = unionAllTypes typeEnv arms
  let sub a b = isSubtypeOf typeEnv a b
  let diffEmpty a b = isBottomType $ snd $ differenceTypeWithEnv typeEnv H.empty a b
  note $ printf "def = %s" (show defT)
  note $ printf "Mon arm = %s" (show monArmT)
  forM_ [(False, "open world", typeEnv), (True, "closed world", typeEnv{tePrgmEnv=True})] $ \(_, world, te) -> do
    let subW a b = isSubtypeOf te a b
    let diffEmptyW a b = isBottomType $ snd $ differenceTypeWithEnv te H.empty a b
    assertTimed (printf "def ⊄ Mon arm (%s)" (world :: String)) $ not $ subW defT monArmT
    assertTimed (printf "def ⊄ ~Mon arm, the arm as a relation (%s)" world) $ not $ subW defT armRel
    assertTimed (printf "def − Mon arm ≠ ∅ (%s)" world) $ not $ diffEmptyW defT monArmT
    assertTimed (printf "def ⊆ arms ⇔ def − arms = ∅ (%s)" world) $ subW defT armsT == diffEmptyW defT armsT
  assertTimed "Mon arm ⊆ Mon arm" $ sub monArmT monArmT
  assertTimed "Mon arm − Mon arm = ∅" $ diffEmpty monArmT monArmT

-- | A leaf carrying a class predicate is a subtype of its own meet with that class, in both the
-- open world and the closed one the declaration coverage checks read in. In the closed world the
-- meet expands the predicate through the conditional membership
-- @every(ConsList[$T -> EqCO], isa= [EqCO])@, spelling @Cons(head=⊤, tail=⊤) | EqCO@ as
-- @Cons[/T=∀EqCO](head=⊤, tail=⊤)@. The subtype check read the leaf's absent @/T@ as ⊤ and
-- answered False for @l ⊆ l ∩ EqCO@ while @l − (l ∩ EqCO)@ was ∅. core/maybe.ct's @equal@ and
-- @compareTo@ arms were the first to meet it, once their declarations checked them. Fixture:
-- recursiveClassBounds.ct.
unitPredicatedLeafWithinOwnMeet :: IO Prgms -> Unit ()
unitPredicatedLeafWithinOwnMeet ioPrgms = do
  prgms <- liftIO ioPrgms
  prgm <- case [v | (k, v) <- H.toList prgms, "recursiveClassBounds" `isInfixOf` k] of
    (prgm:_) -> return prgm
    []       -> fail "recursiveClassBounds.ct program not found"
  let typeEnv0@TypeEnv{teClassGraph=ClassGraph cgData, teNames} = mkTypeEnv prgm
  eqcoPt <- case [cpt | (CGClass _ cpt _ _, _, _) <- graphToNodes cgData, "EqCO" `isInfixOf` nameStr (ptName cpt)] of
    (cpt:_) -> return cpt
    []      -> fail "EqCO class not found"
  consN <- case [nm | nm <- S.toList teNames, "Cons" `isInfixOf` nameStr nm, not ("ConsList" `isInfixOf` nameStr nm)] of
    (nm:_) -> return nm
    []     -> fail "Cons type not found"
  (headKey, tailKey) <- case [(hk, tk) | PartialType{ptArgs=dArgs} <- typeGraphData typeEnv0 consN
                                       , hk <- H.keys dArgs, "head" `isInfixOf` nameStr (pkName hk)
                                       , tk <- H.keys dArgs, "tail" `isInfixOf` nameStr (pkName tk)] of
    (ks:_) -> return ks
    []     -> fail "Cons declares no head/tail"
  let eqco = PredsOne (PredClass eqcoPt{ptVars=H.empty})
  let eqcoTop = UnionType (Just (eqco, H.empty, [])) H.empty []
  let leaf = singletonType (partialVal consN){ptArgs=H.fromList [(headKey, PTopType), (tailKey, PTopType)], ptPreds=eqco}
  forM_ [("open", typeEnv0), ("closed", typeEnv0{tePrgmEnv=True})] $ \(world, typeEnv) -> do
    let met = intersectTypes typeEnv leaf eqcoTop
    note $ printf "%s world: l ∩ EqCO = %s" (world :: String) (show met)
    assertTimed (world ++ ": l ⊆ EqCO") $ isSubtypeOf typeEnv leaf eqcoTop
    assertTimed (world ++ ": l ⊆ l ∩ EqCO") $ isSubtypeOf typeEnv leaf met
    assertTimed (world ++ ": l ∩ EqCO ⊆ l") $ isSubtypeOf typeEnv met leaf
    assertTimed (world ++ ": l − (l ∩ EqCO) = ∅") $ isBottomType $ snd $ differenceTypeWithEnv typeEnv H.empty leaf met

-- | An outer variable nested in a slot is not the partial's like-named parameter. A law body
-- @testGroup[$T -> Group](t -> $T)= t ++ inverse(t= t)@ meets @operator++(l=$_T, r=inverse(t=$_T))@
-- against @operator++[$T -> Semi](l -> $T, r -> $T)@: the @$_T@ inside @inverse@ names the law's
-- variable, and the meet descending into @operator++@'s arguments read it as @operator++@'s own
-- @/T@ — whose binding it was itself feeding (@/T = inverse(t=$_T)@), so the reading never ended
-- (a build of core ran out of memory). Renaming the outer variable must not change the meet.
unitOuterVarNotCaptured :: Unit ()
unitOuterVarNotCaptured = do
  prgm <- liftIO $ loadUnitPrgm "test/Semantics/unit/linkMeet.ct"
  let typeEnv = mkTypeEnv prgm
  let Prgm{prgmObjMap=ObjectMap om} = prgm
  declT <- case [ getMetaType (getExprMeta (oaObjExpr d)) | (n, omi) <- H.toList om, "++" `isInfixOf` nameStr n, d <- omiDecls omi ] of
    (d:_) -> return d
    _     -> fail "no ++ declaration"
  invName <- case [ n | (n, _) <- H.toList om, "inverse" `isInfixOf` nameStr n ] of
    (n:_) -> return n
    _     -> fail "no inverse"
  plusName <- case [ n | (n, _) <- H.toList om, "++" `isInfixOf` nameStr n ] of
    (n:_) -> return n
    _     -> fail "no ++"
  let outer n = TypeVar (TVVar (partialKey n)) TVExt PTopType
  let callOver n = singletonType (partialVal plusName){ptArgs = H.fromList
        [ (partialKey "l", outer n)
        , (partialKey "r", singletonType (partialVal invName){ptArgs = H.singleton (partialKey "t") (outer n)}) ]}
  let envFor n = ambientEnv $ H.singleton (TVVar (partialKey n)) (classPartial (partialVal "/Grp"))
  note $ printf "decl = %s" (show declT)
  forM_ [(H.empty, H.empty), (envFor "T", envFor "X")] $ \(envT, envX) -> do
    mT <- timed "meet over $_T" $ intersectTypesEnv typeEnv envT (callOver "T") declT
    mX <- timed "meet over $_X" $ intersectTypesEnv typeEnv envX (callOver "X") declT
    note $ printf "over T: %s" (show mT)
    note $ printf "over X: %s" (show mX)
    -- The same meet up to the outer variable's name.
    let renamed = renameOuter mX
        renameOuter (UnionType m ls cs) = UnionType m (joinUnionType (map renameLeaf (splitUnionType ls))) cs
        renameOuter t = t
        renameLeaf p = p{ptVars = fmap renameIn (ptVars p), ptArgs = fmap renameIn (ptArgs p)}
        renameIn (TypeVar (TVVar k) TVExt i) | k == partialKey "X" = TypeVar (TVVar (partialKey "T")) TVExt (renameIn i)
        renameIn t@(UnionType{}) = renameOuter t
        renameIn t = t
    eqTimed "meet over $_T = meet over $_X, renamed" mT renamed

-- | A data type joining a parameterized class at a fixed parameter —
-- @data(PtRel, isa= [Rel[$T -> Pt]])@, as core's instance relations do — is recorded under the
-- class as @PtRel[/T=Pt]@, where @/T@ is the CLASS's parameter: PtRel declares none. The record's
-- binding decides which instantiations hold the member (@Rel[/T=Y]@ does exactly when @Pt ⊆ Y@)
-- and does not constrain PtRel's own values. Read as PtRel's own dimension, a plain @PtRel@ read
-- its missing @/T@ as ⊤ and was in no instantiation at all, not even the bare class — so every
-- instance arm of core's @eq@ and @ord@ (@= /Data/MaybeEqv@) fell outside its declared result.
-- Fixture: unit/dataIsaParamClass.ct.
unitDataIsaBindsClassParam :: Unit ()
unitDataIsaBindsClassParam = do
  prgm <- liftIO $ loadUnitPrgm "test/Semantics/unit/dataIsaParamClass.ct"
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData, teNames} = mkTypeEnv prgm
  relPt <- case [cpt | (CGClass _ cpt _ _, _, _) <- graphToNodes cgData, "Rel" `isInfixOf` nameStr (ptName cpt)] of
    (cpt:_) -> return cpt
    []      -> fail "Rel not found"
  let findData n = case [nm | nm <- S.toList teNames, ("/" ++ n) `isSuffixOf` nameStr nm] of
        (nm:_) -> return $ singletonType $ partialVal nm
        []     -> fail $ printf "%s type not found" n
  ptT <- findData "Pt"
  qtT <- findData "Qt"
  ptRel <- findData "PtRel"
  tKey <- case H.keys (ptVars relPt) of
    (k:_) -> return k
    _     -> fail "Rel has no parameter"
  let relOver t = UnionType (Just (PredsOne (PredClass relPt{ptVars=H.singleton tKey t}), H.empty, [])) H.empty []
  let bare = UnionType (Just (PredsOne (PredClass relPt{ptVars=H.empty}), H.empty, [])) H.empty []
  forM_ [("open", typeEnv), ("closed", typeEnv{tePrgmEnv=True})] $ \(world, te) -> do
    let sub a b = isSubtypeOf te a b
    let diffEmpty a b = isBottomType $ snd $ differenceTypeWithEnv te H.empty a b
    let checks = [ ("PtRel ⊆ Rel", sub ptRel bare)
                 , ("PtRel ⊆ Rel[Pt]", sub ptRel (relOver ptT))
                 , ("PtRel ⊆ Rel[⊤]", sub ptRel (relOver PTopType))
                 , ("PtRel − Rel[⊤] = ∅", diffEmpty ptRel (relOver PTopType))
                 , ("PtRel − Rel = ∅", diffEmpty ptRel bare)
                 , ("PtRel ⊄ Rel[Qt]", not $ sub ptRel (relOver qtT))
                 , ("PtRel − Rel[Qt] ≠ ∅", not $ diffEmpty ptRel (relOver qtT)) ]
    forM_ checks $ \(n, ok) -> note $ printf "%s world: %s -> %s" (world :: String) (n :: String) (show ok)
    assertTimed (world ++ " world: every check") $ all snd checks

-- | A meet never leaves its first operand. A call to @fold[$B](fn(acc -> $_B) -> $_B, ..)@ holds
-- its @fn@ slot as the input object @fn[/B=$^B](acc=$/B)@, whose @acc@ is whatever the call binds
-- @$B@ to; with the call's @/B=X@ that is @X@. Met with the same call spelling @fn@ concretely as
-- @fn[/B=X+Y](acc=X+Y)@ (which contains it), the result must still be within the first: core's
-- @sum@ stored such a meet as its requirement, and reading the pointer as ⊤ there gave the
-- concrete @X+Y@ back, which the narrowing check refused as a widening.
-- | A variable linked only through a sibling variable's bound is bound by the meet. @f[$M ->
-- Mon[$T -> $_A], $A](m -> $M, ..)@ met with a call binding @/M@ to a @Bx[/T=X]@ binds @/A=X@:
-- @$_A@ in @/M@'s bound names @f@'s own @/A@, so the element the call's @/M@ carries is @/A@'s
-- value. Left at ⊤, the lambda @f@ is given (@fn(v -> $_A)@) reads its @v@ as ⊤ (core's @mmap@
-- over a list typed @fn(v)= [v, v]@ as a list of anything).
unitSiblingBoundLinks :: Unit ()
unitSiblingBoundLinks = do
  prgm <- liftIO $ loadUnitPrgm "test/Semantics/unit/siblingBound.ct"
  let typeEnv@TypeEnv{teNames} = mkTypeEnv prgm
  let Prgm{prgmObjMap=ObjectMap om} = prgm
  declT <- case [ getMetaType (getExprMeta (oaObjExpr d)) | (n, omi) <- H.toList om, "/f" `isSuffixOf` nameStr n, d <- omiDecls omi ] of
    (t:_) -> return t
    []    -> fail "f not declared"
  declLeaf <- case declT of
    UnionType Nothing leafs [] | [p] <- splitUnionType leafs -> return p
    _ -> fail "decl is not one leaf"
  let findName n = case [nm | nm <- S.toList teNames, ("/" ++ n) `isSuffixOf` nameStr nm] of
        (nm:_) -> return nm
        []     -> fail $ printf "%s not found" n
  xN <- findName "X"
  bxN <- findName "Bx"
  let x = singletonType (partialVal xN)
  (mKey, aKey) <- case (filter (("/M" `isSuffixOf`) . nameStr . pkName) (H.keys (ptVars declLeaf)), filter (("/A" `isSuffixOf`) . nameStr . pkName) (H.keys (ptVars declLeaf))) of
    ([m], [a]) -> return (m, a)
    _          -> fail "f's parameters not found"
  argM <- case [ k | k <- H.keys (ptArgs declLeaf), "/m" `isSuffixOf` nameStr (pkName k) ] of
    [k] -> return k
    _   -> fail "no m arg"
  valueKey <- case [ k | PartialType{ptArgs=dArgs} <- typeGraphData typeEnv bxN, k <- H.keys dArgs ] of
    (k:_) -> return k
    []    -> fail "Bx declares no field"
  tKey <- case [ k | PartialType{ptVars=dVars} <- typeGraphData typeEnv bxN, k <- H.keys dVars ] of
    (k:_) -> return k
    []    -> fail "Bx declares no parameter"
  let bx = singletonType (partialVal bxN){ptVars = H.singleton tKey x, ptArgs = H.singleton valueKey x}
  let call = singletonType (partialVal (ptName declLeaf)){ptVars = H.singleton mKey bx, ptArgs = H.insert argM bx (ptArgs declLeaf)}
  note $ printf "decl = %s" (show declT)
  note $ printf "call = %s" (show call)
  let met = intersectTypesEnv typeEnv H.empty call declT
  note $ printf "call ∩ decl = %s" (show met)
  aVal <- case maybeGetSingleton met of
    Just p  -> return (H.lookupDefault PTopType aKey (ptVars p))
    Nothing -> fail "meet is not one partial"
  note $ printf "/A = %s" (show aVal)
  assertTimed "/A ⊆ X" $ isSubtypeOf typeEnv aVal x

-- | 'unitSiblingBoundLinks' with the call's @/M@ a class top of a SUBCLASS (@∀Opt[/T=X]@, every
-- @Opt@ a @Mon@), as core's @bindap@ over a @Maybe[$T -> Integer]@: the meet keeps @/M@ within
-- @∀Opt[/T=X]@ and binds @/A=X@, also when the call spells @/M@ only through @m@ and its lambda
-- slot as a call does (@fn(v=X)@). That call is within its own meet: the portion of it the
-- declaration accepts is all of it. The meet's input object keeps its hop parameter
-- (@fn[/A=$^A](v=X)@), and the call's unspelled one reads as the same link
-- ('unspelledSubParam'); read as ⊤ it fell outside the bound @/A=X@, so a requirement could not
-- choose the declaration's arrow and @bindap@'s result widened to @Monad[/T=⊤] && Maybe[/T=⊤]@.
unitSiblingBoundClassTop :: Unit ()
unitSiblingBoundClassTop = do
  prgm <- liftIO $ loadUnitPrgm "test/Semantics/unit/siblingBoundClass.ct"
  let typeEnv@TypeEnv{teNames, teClassGraph=ClassGraph cgData} = mkTypeEnv prgm
  let Prgm{prgmObjMap=ObjectMap om} = prgm
  declT <- case [ getMetaType (getExprMeta (oaObjExpr d)) | (n, omi) <- H.toList om, "/f" `isSuffixOf` nameStr n, d <- omiDecls omi ] of
    (t:_) -> return t
    []    -> fail "f not declared"
  declLeaf <- case declT of
    UnionType Nothing leafs [] | [p] <- splitUnionType leafs -> return p
    _ -> fail "decl is not one leaf"
  xN <- case [nm | nm <- S.toList teNames, "/X" `isSuffixOf` nameStr nm] of
    (nm:_) -> return nm
    []     -> fail "X not found"
  optPt <- case [cpt | (CGClass _ cpt _ _, _, _) <- graphToNodes cgData, "/Opt" `isSuffixOf` nameStr (ptName cpt)] of
    (cpt:_) -> return cpt
    []      -> fail "Opt class not found"
  let x = singletonType (partialVal xN)
  (mKey, aKey) <- case (filter (("/M" `isSuffixOf`) . nameStr . pkName) (H.keys (ptVars declLeaf)), filter (("/A" `isSuffixOf`) . nameStr . pkName) (H.keys (ptVars declLeaf))) of
    ([m], [a]) -> return (m, a)
    _          -> fail "f's parameters not found"
  argM <- case [ k | k <- H.keys (ptArgs declLeaf), "/m" `isSuffixOf` nameStr (pkName k) ] of
    [k] -> return k
    _   -> fail "no m arg"
  let optX = UnionType (Just (PredsOne (PredClass optPt{ptVars = fmap (const x) (ptVars optPt)}), H.empty, [])) H.empty []
  let call = singletonType (partialVal (ptName declLeaf)){ptVars = H.singleton mKey optX, ptArgs = H.insert argM optX (ptArgs declLeaf)}
  note $ printf "decl = %s" (show declT)
  note $ printf "call = %s" (show call)
  let met = intersectTypesEnv typeEnv H.empty call declT
  note $ printf "call ∩ decl = %s" (show met)
  (mVal, aVal) <- case maybeGetSingleton met of
    Just p  -> return (H.lookupDefault PTopType mKey (ptVars p), H.lookupDefault PTopType aKey (ptVars p))
    Nothing -> fail "meet is not one partial"
  note $ printf "/M = %s, /A = %s" (show mVal) (show aVal)
  assertTimed "/M ⊆ ∀Opt[/T=X]" $ isSubtypeOf typeEnv mVal optX
  assertTimed "/A ⊆ X" $ isSubtypeOf typeEnv aVal x
  -- The same call spelled as its act is: @/M@ not among its parameters, bound only through the
  -- argument it types (@m -> $M@) — the reach query's meet of a call with the declaration.
  -- The call's lambda slot as a call spells it: the input object with its parameter at the
  -- value the call gives it and no hop parameter (@fn(v=X)@, where the declaration spells
  -- @fn[/A=$^A](v=$/A)@).
  (fnKey, fnObj) <- case [ (k, o) | (k, v) <- H.toList (ptArgs declLeaf), not (S.null (pkArgs k)), Just o <- [maybeGetSingleton v] ] of
    [kv] -> return kv
    _    -> fail "no higher-order arg"
  let callFn = singletonType fnObj{ptVars = H.empty, ptArgs = fmap (const x) (ptArgs fnObj)}
  let callArg = singletonType (partialVal (ptName declLeaf)){ptVars = H.singleton aKey x, ptArgs = H.insert fnKey callFn (H.insert argM optX (ptArgs declLeaf))}
  let metArg = intersectTypesEnv typeEnv H.empty callArg declT
  note $ printf "argument-only call ∩ decl = %s" (show metArg)
  (mVal', aVal') <- case maybeGetSingleton metArg of
    Just p  -> return (H.lookupDefault PTopType mKey (ptVars p), H.lookupDefault PTopType aKey (ptVars p))
    Nothing -> fail "argument-only meet is not one partial"
  note $ printf "argument-only /M = %s, /A = %s" (show mVal') (show aVal')
  assertTimed "argument-only: /M ⊆ ∀Opt[/T=X]" $ isSubtypeOf typeEnv (slotAtPoint typeEnv H.empty (fromJust (maybeGetSingleton metArg)) mVal') optX
  -- The meet is the portion of the call the declaration accepts, and it accepts all of it: the
  -- call is within its own meet (the coverage a requirement's choice of arrow is guarded by).
  assertTimed "argument-only: call ⊆ call ∩ decl" $ isSubtypeOf typeEnv callArg metArg

-- | A call meeting the union of two declarations' objects, each bounding its variable by a class
-- linking a sibling (core's @bindap@ arms over @Monad@ and @Functor@): the call binds the sibling
-- from its @m@ (@/A=X@), which is what its lambda's parameter reads. The call spells no variable
-- of its own, so both declarations accept the same calls and their portions are one set.
unitSiblingBoundOverloads :: Unit ()
unitSiblingBoundOverloads = do
  prgm <- liftIO $ loadUnitPrgm "test/Semantics/unit/siblingBoundOverload.ct"
  let typeEnv@TypeEnv{teNames, teClassGraph=ClassGraph cgData} = mkTypeEnv prgm
  let Prgm{prgmObjMap=ObjectMap om} = prgm
  declTs <- case [ getMetaType (getExprMeta (oaObjExpr d)) | (n, omi) <- H.toList om, "/f" `isSuffixOf` nameStr n, d <- omiDecls omi ] of
    ts@(_:_:_) -> return ts
    _          -> fail "f is not declared twice"
  let declU = unionAllTypes typeEnv declTs
  declLeaf <- case declTs of
    (UnionType Nothing leafs []:_) | (p:_) <- splitUnionType leafs -> return p
    _ -> fail "decl is not a leaf"
  xN <- case [nm | nm <- S.toList teNames, "/X" `isSuffixOf` nameStr nm] of
    (nm:_) -> return nm
    []     -> fail "X not found"
  optPt <- case [cpt | (CGClass _ cpt _ _, _, _) <- graphToNodes cgData, "/Opt" `isSuffixOf` nameStr (ptName cpt)] of
    (cpt:_) -> return cpt
    []      -> fail "Opt class not found"
  let x = singletonType (partialVal xN)
  let optX = UnionType (Just (PredsOne (PredClass optPt{ptVars = fmap (const x) (ptVars optPt)}), H.empty, [])) H.empty []
  argM <- case [ k | k <- H.keys (ptArgs declLeaf), "/m" `isSuffixOf` nameStr (pkName k) ] of
    [k] -> return k
    _   -> fail "no m arg"
  (fnKey, fnObj) <- case [ (k, o) | (k, v) <- H.toList (ptArgs declLeaf), not (S.null (pkArgs k)), Just o <- [maybeGetSingleton v] ] of
    [kv] -> return kv
    _    -> fail "no higher-order arg"
  let callFn = singletonType fnObj{ptVars = H.empty, ptArgs = fmap (const PTopType) (ptArgs fnObj)}
  let call = singletonType (partialVal (ptName declLeaf)){ptArgs = H.fromList [(argM, optX), (fnKey, callFn)]}
  note $ printf "decls = %s" (show declU)
  note $ printf "call = %s" (show call)
  let met = intersectTypesEnv typeEnv H.empty call declU
  note $ printf "call ∩ decls = %s" (show met)
  pieces <- case met of
    UnionType Nothing leafs [] -> return (splitUnionType leafs)
    _                          -> fail "meet is not a union of partials"
  note $ printf "pieces = %s" (show pieces)
  forM_ pieces $ \p -> do
    let aVal = head ([ v | (k, v) <- H.toList (ptVars p), "/A" `isSuffixOf` nameStr (pkName k) ] ++ [PTopType])
    note $ printf "piece %s: /A = %s" (show p) (show aVal)
    assertTimed "each piece binds /A=X" $ isSubtypeOf typeEnv aVal x

-- | Rebinding a data value's parameter ('applyTypeVarOverrides', the @$F[$T -> $_B]@ reading)
-- re-derives the fields that parameter governs: @Bx[/T=X](/value=X)@ with @T@ rebound to @Y@
-- holds a @Y@, and is not the @Bx[/T=Y](/value=X)@ no declaration admits.
unitAppliedDataFields :: Unit ()
unitAppliedDataFields = do
  prgm <- liftIO $ loadUnitPrgm "test/Semantics/unit/appliedDataFields.ct"
  let typeEnv@TypeEnv{teNames} = mkTypeEnv prgm
  let findName n = case [nm | nm <- S.toList teNames, ("/" ++ n) `isSuffixOf` nameStr nm] of
        (nm:_) -> return nm
        []     -> fail $ printf "%s not found" n
  x <- singletonType . partialVal <$> findName "X"
  y <- singletonType . partialVal <$> findName "Y"
  bxN <- findName "Bx"
  bxDecl <- case typeGraphData typeEnv bxN of
    (d:_) -> return d
    []    -> fail "Bx declares no fields"
  tVar <- case [ k | k <- H.keys (ptVars bxDecl), "/T" `isSuffixOf` nameStr (pkName k) ] of
    [k] -> return k
    _   -> fail "Bx has no T"
  let bxAt t = singletonType bxDecl{ptVars = H.singleton tVar t, ptArgs = fmap (const t) (ptArgs bxDecl)}
  let applied = applyTypeVarOverrides typeEnv (H.singleton tVar y) (bxAt x)
  note $ printf "Bx at X = %s" (show (bxAt x))
  note $ printf "rebound to Y = %s" (show applied)
  assertTimed "the rebound value is inhabited" $ not (isBottomType (intersectTypes typeEnv applied (bxAt y)))
  assertTimed "the rebound value is a Bx at Y" $ isSubtypeOf typeEnv applied (bxAt y)
  assertTimed "the rebound value holds no X" $ isBottomType (intersectTypes typeEnv applied (bxAt x))

-- | Two overloads whose continuation slots differ only in which declaration parameter they name
-- ('compactJoinPartials' joins them into one leaf): the union contains each of them.
unitLinkedSlotUnion :: Unit ()
unitLinkedSlotUnion = do
  prgm <- liftIO $ loadUnitPrgm "test/Semantics/unit/linkedSlotUnion.ct"
  let typeEnv = mkTypeEnv prgm
  let Prgm{prgmObjMap=ObjectMap om} = prgm
  declTs <- case [ getMetaType (getExprMeta (oaObjExpr d)) | (n, omi) <- H.toList om, "/bap" `isSuffixOf` nameStr n, d <- omiDecls omi ] of
    ts@[_, _] -> return ts
    _         -> fail "bap is not declared twice"
  let declU = unionAllTypes typeEnv declTs
  note $ printf "decls = %s" (show declTs)
  note $ printf "union = %s" (show declU)
  forM_ declTs $ \d -> assertTimed "each declaration is within the union" $ isSubtypeOf typeEnv d declU

-- | A parameter value's parent reference names a sibling ('comparedPartialsEnv'): with the
-- sibling bound, an argument spelled through the parameter's pointer (@lst=$/F@) is the same set
-- as the argument spelling the value out (@lst=Bx[/T=$^/A](..)@), both ways.
unitSiblingParamValue :: Unit ()
unitSiblingParamValue = do
  prgm <- liftIO $ loadUnitPrgm "test/Semantics/unit/siblingParamValue.ct"
  let typeEnv@TypeEnv{teNames} = mkTypeEnv prgm
  let Prgm{prgmObjMap=ObjectMap om} = prgm
  armLeaf <- case [ p | (n, omi) <- H.toList om, "/fm" `isSuffixOf` nameStr n, d <- omiDefs omi
                      , Just p <- [maybeGetSingleton (getMetaType (getExprMeta (oaObjExpr d)))]
                      , "Bx[" `isInfixOf` show p ] of
    [p] -> return p
    _   -> fail "no single Bx arm"
  let keyOf m n = case [ k | k <- H.keys m, ("/" ++ n) `isSuffixOf` nameStr (pkName k) ] of
        [k] -> return k
        _   -> fail $ printf "no %s" n
  lstK <- keyOf (ptArgs armLeaf) "lst"
  fK <- keyOf (ptVars armLeaf) "F"
  aK <- keyOf (ptVars armLeaf) "A"
  x <- case [nm | nm <- S.toList teNames, "/X" `isSuffixOf` nameStr nm] of
    (nm:_) -> return $ singletonType $ partialVal nm
    []     -> fail "X not found"
  let spelled = armLeaf{ptVars = H.insert aK x (ptVars armLeaf)}
  let pointed = spelled{ptArgs = H.insert lstK (TypeVar (TVVar fK) TVInt PTopType) (ptArgs spelled)}
  note $ printf "spelled = %s" (show spelled)
  note $ printf "pointed = %s" (show pointed)
  assertTimed "pointed within spelled" $ isSubtypeOf typeEnv (singletonType pointed) (singletonType spelled)
  assertTimed "spelled within pointed" $ isSubtypeOf typeEnv (singletonType spelled) (singletonType pointed)

-- | A continuation input naming a parameter its data pins is read as the data pins it
-- ('pinnedSlotsOk'): the element-passing and the whole-passing overloads are incomparable. And a
-- meet keeps the input's link where the other side spells the same set out
-- ('intersectPartialsBaseN'.'subUnion'): @a ⊆ b ⇒ a ∩ b = a@.
unitPinnedInputSlot :: Unit ()
unitPinnedInputSlot = do
  prgm <- liftIO $ loadUnitPrgm "test/Semantics/unit/pinnedInputSlot.ct"
  let typeEnv = mkTypeEnv prgm
  let Prgm{prgmObjMap=ObjectMap om} = prgm
  (d1, d2) <- case [ getMetaType (getExprMeta (oaObjExpr d)) | (n, omi) <- H.toList om, "/bap" `isSuffixOf` nameStr n, d <- omiDecls omi ] of
    [a, b] -> return (a, b)
    _      -> fail "bap is not declared twice"
  let decls = [d1, d2]
  note $ printf "decls = %s" (show decls)
  assertTimed "neither declaration within the other" $ not (isSubtypeOf typeEnv d1 d2) && not (isSubtypeOf typeEnv d2 d1)
  forM_ decls $ \d -> do
    leaf <- maybe (fail "decl is not one leaf") return (maybeGetSingleton d)
    (fnKey, fnObj) <- case [ (k, o) | (k, v) <- H.toList (ptArgs leaf), not (S.null (pkArgs k)), Just o <- [maybeGetSingleton v] ] of
      [kv] -> return kv
      _    -> fail "no single higher-order arg"
    -- The slot with its references read out to the values they name: a parent reference to the
    -- declaration's parameter, and the input's own pointer through its parameter to the same.
    let outer (TypeVar (TVVar n) TVParent _) = H.lookupDefault PTopType n (ptVars leaf)
        outer t                              = t
        spell (TypeVar (TVVar n) TVInt _) = outer (H.lookupDefault PTopType n (ptVars fnObj))
        spell t                           = outer t
    let spelledFn = singletonType fnObj{ptVars = fmap spell (ptVars fnObj), ptArgs = fmap spell (ptArgs fnObj)}
    let spelled = singletonType leaf{ptArgs = H.insert fnKey spelledFn (ptArgs leaf)}
    note $ printf "spelled = %s" (show spelled)
    when (isSubtypeOf typeEnv d spelled) $
      assertTimed "a meet keeps the operand within the other" $ isEqType typeEnv (intersectTypes typeEnv d spelled) d

unitHoSlotParentMeet :: Unit ()
unitHoSlotParentMeet = do
  prgm <- liftIO $ loadUnitPrgm "test/Semantics/unit/hoSlotParentMeet.ct"
  let typeEnv@TypeEnv{teNames} = mkTypeEnv prgm
  let Prgm{prgmObjMap=ObjectMap om} = prgm
  declT <- case [ getMetaType (getExprMeta (oaObjExpr d)) | (n, omi) <- H.toList om, "/fold" `isSuffixOf` nameStr n, d <- omiDecls omi ] of
    (t:_) -> return t
    []    -> fail "fold not declared"
  declLeaf <- case declT of
    UnionType Nothing leafs [] | [p] <- splitUnionType leafs -> return p
    _ -> fail "decl is not one leaf"
  let findData n = case [nm | nm <- S.toList teNames, ("/" ++ n) `isSuffixOf` nameStr nm] of
        (nm:_) -> return $ singletonType $ partialVal nm
        []     -> fail $ printf "%s type not found" n
  x <- findData "X"
  y <- findData "Y"
  let xy = unionTypes typeEnv x y
  (fnKey, fnSlot) <- case [ (k, v) | (k, v) <- H.toList (ptArgs declLeaf), not (S.null (pkArgs k)) ] of
    [kv] -> return kv
    _    -> fail "no higher-order arg"
  initKey <- case [ k | k <- H.keys (ptArgs declLeaf), "/init" `isSuffixOf` nameStr (pkName k) ] of
    [k] -> return k
    _   -> fail "no init arg"
  bName <- case H.keys (ptVars declLeaf) of
    [b] -> return b
    _   -> fail "fold has not one parameter"
  fnObj <- case maybeGetSingleton fnSlot of
    Just o  -> return o
    Nothing -> fail "fn slot is not one input object"
  note $ printf "declared fn slot = %s" (show fnSlot)
  let call fn = singletonType declLeaf{ptVars = H.singleton bName x, ptArgs = H.fromList [(fnKey, fn), (initKey, x)]}
  let concreteFn = singletonType fnObj{ptVars = fmap (const xy) (ptVars fnObj), ptArgs = fmap (const xy) (ptArgs fnObj)}
  let a = call fnSlot
  let b = call concreteFn
  note $ printf "a = %s" (show a)
  note $ printf "b = %s" (show b)
  forM_ [("open", typeEnv), ("closed", typeEnv{tePrgmEnv=True})] $ \(world, te) -> do
    let met = intersectTypesEnv te H.empty a b
    note $ printf "%s world: a ∩ b = %s" (world :: String) (show met)
    assertTimed (world ++ ": a ⊆ b") $ isSubtypeOfWithEnv te H.empty a b
    assertTimed (world ++ ": a ∩ b ⊆ a") $ isSubtypeOfWithEnv te H.empty met a || isBottomType (snd (differenceTypeWithEnv te H.empty met a))

-- | A class member met with its parent class at a parameter: every @BinTree[$K -> $_K, ..]@ is a
-- @Map[$K -> $_K, ..]@, so @Node ∩ ∀Map[/K=Kx]@ holds only nodes keyed by @Kx@. Core's @Map@ arms
-- match @m= Node(left, key, value, right)@ against a declared @m -> Map[$K -> $_MK]@ and read the
-- pattern's fields from that meet, so each field read must carry the parameter: @key@ within @Kx@
-- and @left@ within @∀BinTree[/K=Kx]@. @Map@ is open, but @Node@ is already in scope, so no later
-- file makes it a @Map@ any other way ('knownMemberName'). Inference reads the open world; the
-- closed world relaxes a member's fields to ⊤ as match targets, so its field reads are not asked.
unitMemberParentParams :: Unit ()
unitMemberParentParams = do
  prgm <- liftIO $ loadUnitPrgm "test/Semantics/unit/memberParentParams.ct"
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData, teNames} = mkTypeEnv prgm
  let classPt n = case [cpt | (CGClass _ cpt _ _, _, _) <- graphToNodes cgData, ("/" ++ n) `isSuffixOf` nameStr (ptName cpt)] of
        (cpt:_) -> return cpt
        []      -> fail $ printf "%s class not found" n
  let findName n = case [nm | nm <- S.toList teNames, ("/" ++ n) `isSuffixOf` nameStr nm] of
        (nm:_) -> return nm
        []     -> fail $ printf "%s not found" n
  mapPt <- classPt "Map"
  treePt <- classPt "BinTree"
  kx <- singletonType . partialVal <$> findName "Kx"
  nodeN <- findName "Node"
  ky <- singletonType . partialVal <$> findName "Ky"
  let atK t cpt = do
        kVar <- case [ k | k <- H.keys (ptVars cpt), "/K" `isSuffixOf` nameStr (pkName k) ] of
          [k] -> return k
          _   -> fail $ printf "%s has no K" (show (ptName cpt))
        return $ UnionType (Just (PredsOne (PredClass cpt{ptVars=H.insert kVar t (fmap (const PTopType) (ptVars cpt))}), H.empty, [])) H.empty []
  mapKx <- atK kx mapPt
  treeKx <- atK kx treePt
  treeKy <- atK ky treePt
  nodeArgs <- case typeGraphData typeEnv nodeN of
    (PartialType{ptArgs=dArgs}:_) -> return (H.keys dArgs)
    []                            -> fail "Node declares no fields"
  let argKey n = case [ k | k <- nodeArgs, ("/" ++ n) `isSuffixOf` nameStr (pkName k) ] of
        [k] -> return k
        _   -> fail $ printf "Node has no %s" n
  keyK <- argKey "key"
  leftK <- argKey "left"
  let node = singletonType (partialVal nodeN){ptArgs=H.fromList [ (k, PTopType) | k <- nodeArgs ]}
  let met = intersectTypes typeEnv node mapKx
  note $ printf "Node ∩ Map[K=Kx] = %s" (show met)
  assertTimed "Node ∩ Map[K=Kx] ⊆ Node" $ isSubtypeOf typeEnv met node
  assertTimed "Node ∩ Map[K=Kx] ⊆ Map[K=Kx]" $ isSubtypeOf typeEnv met mapKx
  leafs <- case met of
    UnionType Nothing ls [] | not (H.null ls) -> return (splitUnionType ls)
    _ -> fail "the meet is not a union of nodes"
  forM_ leafs $ \l -> do
    -- A field's parent references name the leaf, so it is read in the leaf's own scope.
    let readField k = substituteTVExtScoped typeEnv (ambientEnv $ joinVarArgEnv (ptVars l) H.empty) H.empty (fromMaybe PTopType (typeGetArg k l))
    note $ printf "key = %s, left = %s" (show (readField keyK)) (show (readField leftK))
    assertTimed "key ⊆ Kx" $ isSubtypeOf typeEnv (readField keyK) kx
    assertTimed "left ⊆ BinTree[K=Kx]" $ isSubtypeOf typeEnv (readField leftK) treeKx
    -- Read as the parameter, not as a free spelling that any binding would also admit.
    assertTimed "key ⊄ Ky" $ not $ isSubtypeOf typeEnv (readField keyK) ky
    assertTimed "left ⊄ BinTree[K=Ky]" $ not $ isSubtypeOf typeEnv (readField leftK) treeKy

-- | An arm spelled without the parameter its declaration ties to its argument is the same set as
-- one spelled with it: @eq(this=∀Peano)@ and @eq[/T=∀Peano](this=∀Peano)@ under
-- @eq[$T](this -> $T)@. Declaration coverage read them differently: with every arm spelled
-- without its @/T@, @eq[/T=∀Eq](this=$/T)@ was not within their union, though it is within the
-- union spelled with it. Fixture: unit/coverDerivedParam.ct.
unitCoverDerivedParam :: Unit ()
unitCoverDerivedParam = do
  prgm <- liftIO $ loadUnitPrgm "test/Semantics/unit/coverDerivedParam.ct"
  let typeEnv = (mkTypeEnv prgm){tePrgmEnv=True}
  let Prgm{prgmObjMap=ObjectMap om} = prgm
  omi <- case [ o | (n, o) <- H.toList om, "/eq" `isSuffixOf` nameStr n ] of
    [o] -> return o
    _   -> fail "no single eq"
  declT <- case [ getMetaType (getExprMeta (oaObjExpr d)) | d <- omiDecls omi ] of
    [d] -> return d
    _   -> fail "no single eq declaration"
  let defTs = [ getMetaType (getExprMeta (oaObjExpr d)) | d <- omiDefs omi ]
  let unspelled (UnionType m ls cs) = UnionType m (joinUnionType [ l{ptVars=H.empty} | l <- splitUnionType ls ]) cs
      unspelled t = t
  note $ printf "decl = %s" (show declT)
  note $ printf "defs = %s" (show defTs)
  forM_ defTs $ \d -> eqTimed' ("respelled " ++ show d) typeEnv d (unspelled d)
  assertTimed "decl ⊆ ∪ defs" $ isSubtypeOf typeEnv declT (unionAllTypes typeEnv defTs)
  assertTimed "decl ⊆ ∪ defs without their /T" $ isSubtypeOf typeEnv declT (unionAllTypes typeEnv (map unspelled defTs))
  -- As a page adding Peano to Eq spells them: its own arm without /T, the earlier pages' arms with.
  let peanoOnly d = if "Peano" `isInfixOf` show d then unspelled d else d
  let mixed = unionAllTypes typeEnv (map peanoOnly defTs)
  let atClass c (UnionType m ls cs) = UnionType m (joinUnionType [ l{ptVars=fmap (const (classPartial (partialVal c))) (ptVars l)} | l <- splitUnionType ls ]) cs
      atClass _ t = t
  -- Each class's piece of the declaration is within the union on its own, as is their union.
  forM_ ["/Pg/Peano", "/Lib/Str", "/Lib/Lst"] $ \c ->
    assertTimed (printf "decl at %s ⊆ ∪ defs, the Peano arm without its /T" (nameStr c)) $ isSubtypeOf typeEnv (atClass c declT) mixed
  assertTimed "decl ⊆ ∪ defs, the Peano arm without its /T" $ isSubtypeOf typeEnv declT mixed
  where
    eqTimed' msg te a b = assertTimed msg (isEqType te a b)

-- | A class's member at a concrete parameter is within the class at ⊤. A member declared with a
-- field naming its own parameter (@MkSet[$K -> $_K, ..](map -> Map[$K -> $_K, ..])@) spells that
-- field with a parent reference (@map=∀Map[/K=$^K, ..]@), and the class at ⊤ expands to the member
-- at ⊤ with the reference unresolved. The symbolic engine took the unresolved reference for one
-- unknown binding the member's @∀Map[/K=Z]@ escapes, so @MkSet[/K=Z](..) ⊆ ∀MapSet[/K=⊤]@ failed
-- while @MkSet[/K=Z](..) ⊆ ∀MapSet[/K=Z] ⊆ ∀MapSet[/K=⊤]@ held — core's @Set@ requirements then
-- failed their narrowing check once a meet spelled a set as its member.
unitMemberWithinClassAtTop :: Unit ()
unitMemberWithinClassAtTop = do
  prgm <- liftIO $ loadUnitPrgm "test/Semantics/unit/memberWithinClassAtTop.ct"
  let typeEnv@TypeEnv{teClassGraph=ClassGraph cgData, teNames} = mkTypeEnv prgm
  let classPt n = case [cpt | (CGClass _ cpt _ _, _, _) <- graphToNodes cgData, ("/" ++ n) `isSuffixOf` nameStr (ptName cpt)] of
        (cpt:_) -> return cpt
        []      -> fail $ printf "%s class not found" n
  let findName n = case [nm | nm <- S.toList teNames, ("/" ++ n) `isSuffixOf` nameStr nm] of
        (nm:_) -> return nm
        []     -> fail $ printf "%s not found" n
  setPt <- classPt "Set"
  msPt <- classPt "MapSet"
  z <- singletonType . partialVal <$> findName "Z"
  mkN <- findName "MkSet"
  let top cpt = UnionType (Just (PredsOne (PredClass cpt), H.empty, [])) H.empty []
  let atParam n t cpt = top cpt{ptVars=H.mapWithKey (\k _ -> if ("/" ++ n) `isSuffixOf` nameStr (pkName k) then t else PTopType) (ptVars cpt)}
  -- The member as the class at ⊤ spells it, then at a concrete parameter.
  mkArgs <- case typeGraphData typeEnv mkN of
    (PartialType{ptArgs=dArgs}:_) -> return (H.keys dArgs)
    []                            -> fail "MkSet declares no fields"
  let mk = singletonType (partialVal mkN){ptArgs=H.fromList [ (k, PTopType) | k <- mkArgs ]}
  memberTop <- case intersectTypes typeEnv mk (atParam "K" PTopType msPt) of
    UnionType Nothing ls [] | [l] <- splitUnionType ls -> return l
    t -> fail $ printf "MkSet ∩ MapSet[K=⊤] is not one leaf: %s" (show t)
  kVar <- case [ k | k <- H.keys (ptVars memberTop), "/K" `isSuffixOf` nameStr (pkName k) ] of
    [k] -> return k
    _   -> fail "MkSet has no K"
  let memberZ = singletonType memberTop{ptVars=H.insert kVar z (ptVars memberTop)}
  note $ printf "member at Z = %s" (show memberZ)
  let checks = [ ("member ⊆ MapSet[K=Z]", memberZ, atParam "K" z msPt)
               , ("member ⊆ MapSet[K=⊤]", memberZ, atParam "K" PTopType msPt)
               , ("member ⊆ Set[T=Z]", memberZ, atParam "T" z setPt)
               , ("member ⊆ Set[T=⊤]", memberZ, atParam "T" PTopType setPt) ]
  forM_ checks $ \(n, a, b) -> do
    note $ printf "%s: symbolic %s" (n :: String) (show (typeCoveredSymbolic typeEnv H.empty ProveEmpty a b))
    assertTimed n $ isSubtypeOf typeEnv a b

-- | A function declaration whose arguments are its own parameter (@(_ :: $T ++ _ :: $T)[$T ->
-- Semigroup]@) holds an application to two members of the parameter's bound, and not one to a type
-- outside it. The application is spelled without the parameter (@op(l=Sa, r=Sa)@), so its @/T@ is
-- what its arguments are, as a data value's is what its pointer-declared fields are
-- ('ptVarEff'). Read from data declarations alone, the application's @/T@ stayed ⊤ and every
-- application fell outside its own declaration: core's String arm of @++@ outside the generic
-- @++@, as soon as the String declaration became a refinement and no longer covered it itself.
-- Fixture: unit/pointerArgsMember.ct.
unitFunctionParamFromArgs :: Unit ()
unitFunctionParamFromArgs = do
  prgm <- liftIO $ loadUnitPrgm "test/Semantics/unit/pointerArgsMember.ct"
  let typeEnv@TypeEnv{teNames} = mkTypeEnv prgm
  let Prgm{prgmObjMap=ObjectMap om} = prgm
  declT <- case [ getMetaType (getExprMeta (oaObjExpr d)) | (n, omi) <- H.toList om, "/op" `isSuffixOf` nameStr n, d <- omiDecls omi ] of
    (t:_) -> return t
    []    -> fail "op not declared"
  opName <- case [ n | (n, _) <- H.toList om, "/op" `isSuffixOf` nameStr n ] of
    (n:_) -> return n
    []    -> fail "op not found"
  let findData n = case [nm | nm <- S.toList teNames, ("/" ++ n) `isSuffixOf` nameStr nm] of
        (nm:_) -> return $ singletonType $ partialVal nm
        []     -> fail $ printf "%s type not found" n
  sa <- findData "Sa"
  sb <- findData "Sb"
  declLeaf <- case declT of
    UnionType Nothing leafs [] | [p] <- splitUnionType leafs -> return p
    _ -> fail "decl is not one leaf"
  let keyNamed k = case [ ak | ak <- H.keys (ptArgs declLeaf), k `isSuffixOf` nameStr (pkName ak) ] of
        (ak:_) -> return ak
        []     -> fail ("no arg " ++ k)
  lk <- keyNamed "/l"
  rk <- keyNamed "/r"
  let app a b = singletonType (partialVal opName){ptArgs = H.fromList [(lk, a), (rk, b)]}
  note $ printf "decl = %s" (show declT)
  forM_ [("open", typeEnv), ("closed", typeEnv{tePrgmEnv=True})] $ \(world, te) -> do
    let sub a b = isSubtypeOf te a b
    let diffEmpty a b = isBottomType $ snd $ differenceTypeWithEnv te H.empty a b
    let checks = [ ("op(Sa, Sa) ⊆ decl", sub (app sa sa) declT)
                 , ("op(Sa, Sa) − decl = ∅", diffEmpty (app sa sa) declT)
                 , ("op(Sb, Sb) ⊄ decl", not $ sub (app sb sb) declT)
                 , ("op(Sb, Sb) − decl ≠ ∅", not $ diffEmpty (app sb sb) declT) ]
    forM_ checks $ \(n, ok) -> do
      forceTimed (world ++ ": " ++ n) ok
      note $ printf "%s world: %s -> %s" (world :: String) n (show ok)
    assertTimed (world ++ " world: every check") $ all snd checks

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
      , HG.testProperty "propSubtypeUnionVarSplit" (p $ propSubtypeUnionVarSplit ioPrgms)
      , HG.testProperty "propClassExpansionLeafSubtype" (p $ propClassExpansionLeafSubtype ioPrgms)
      , HG.testProperty "propClassGraphMatchesPredClass" (p $ propClassGraphMatchesPredClass ioPrgms)
      , unitCase "unitParamClassWideNotNarrow" (unitParamClassWideNotNarrow ioPrgms)
      , unitCase "unitParamClassIntersectAbsorb" (unitParamClassIntersectAbsorb ioPrgms)
      , unitCase "unitConjMemberClassSubtype" (unitConjMemberClassSubtype ioPrgms)
      , unitCase "unitNegRecursiveClassMeet" (unitNegRecursiveClassMeet ioPrgms)
      , unitCase "unitParamIsaEdgeCovariant" (unitParamIsaEdgeCovariant ioPrgms)
      , unitCase "unitDeclClampClassMembership" (unitDeclClampClassMembership ioPrgms)
      , unitCase "unitExcludedConstantIrrelevantToData" (unitExcludedConstantIrrelevantToData ioPrgms)
      , unitCase "unitConstantWithinValuePredicate" (unitConstantWithinValuePredicate ioPrgms)
      , unitCase "unitOwnParamPointerCover" unitOwnParamPointerCover
      , unitCase "unitDataIsaBindsClassParam" unitDataIsaBindsClassParam
      , unitCase "unitFunctionParamFromArgs" unitFunctionParamFromArgs
      , unitCase "unitHoSlotParentMeet" unitHoSlotParentMeet
      , unitCase "unitSiblingBoundLinks" unitSiblingBoundLinks
      , unitCase "unitSiblingBoundClassTop" unitSiblingBoundClassTop
      , unitCase "unitSiblingBoundOverloads" unitSiblingBoundOverloads
      , unitCase "unitAppliedDataFields" unitAppliedDataFields
      , unitCase "unitLinkedSlotUnion" unitLinkedSlotUnion
      , unitCase "unitSiblingParamValue" unitSiblingParamValue
      , unitCase "unitPinnedInputSlot" unitPinnedInputSlot
      , unitCase "unitMemberParentParams" unitMemberParentParams
      , unitCase "unitMemberWithinClassAtTop" unitMemberWithinClassAtTop
      , unitCase "unitPredicatedLeafWithinOwnMeet" (unitPredicatedLeafWithinOwnMeet ioPrgms)
      , unitCase "unitOuterVarNotCaptured" unitOuterVarNotCaptured
      , unitCase "unitCoverDerivedParam" unitCoverDerivedParam
      , HG.testProperty "propOutOfScopeParity" (p $ propOutOfScopeParity gPrgm)
      , HG.testProperty "propOccurrenceReadingSound" (p $ propOccurrenceReadingSound gPrgm)
      , HG.testProperty "propOwnVarBindingRespected" (p $ propOwnVarBindingRespected ioPrgms)
      , HG.testProperty "propParentVarBindingRespected" (p $ propParentVarBindingRespected ioPrgms)
      , HG.testProperty "propExtensionKeepsProofs" (p $ propExtensionKeepsProofs ioPrgms)
      , HG.testProperty "propOpenProofsHoldClosed" (p $ propOpenProofsHoldClosed gPrgm)
                                        ]
      where gPrgm = ggPrgm <$> ioPrgms
    -- Every property runs at hedgehog's default discard limit: no generator draws and rejects
    -- ('ggPrgmFor', 'Testing.Generation.GenSpec'), so a "gave up" is a generator regression.
    p prop = prop
    -- p prop = withTests 100000 prop

retest :: IO ()
retest = defaultMain $ localOption (TP.TestPattern $ Just $ TT.ERE "propCompactIdempotent") $ localOption (HedgehogReplay $ Just ("1:a2", Seed 10220680066336263475 12463056215188250787)) typeTests

main :: IO ()
main = defaultMain typeTests
