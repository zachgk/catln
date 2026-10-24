-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.SatGoal
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- The goals of the symbolic satisfiability engine ('SatGoal', 'SatAtom') and their
-- normalization into disjunctive normal form. The engine itself is in
-- "Semantics.Types.Algebra.Sat".
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Semantics.Types.SatGoal where

import           Control.Monad             (foldM)
import           Data.Hashable
import qualified Data.HashMap.Strict       as H
import qualified Data.HashSet              as S
import           Data.List                 (partition)
import qualified Data.List                 as L
import           Data.Maybe
import           GHC.Generics
import           Semantics.Types.ArgModes
import           Semantics.Types.Construct
import           Semantics.Types.Data
import           Semantics.Types.Name
import           Semantics.Types.Preds
import           Semantics.Types.Vars


-- ============================================================================
-- * Symbolic class-predicate satisfiability
-- ============================================================================
-- Decides inhabitation (satisfiability) of a conjunction of class/relation/
-- concrete-shape constraints on a single value directly against the class
-- graph, without materializing member unions ('expandPredicates' /
-- 'expandClassPartial') and re-expanding recursive classes.
--
-- Recursion through the class graph (e.g. a class @Eq@ whose parameterized
-- members @ConsList[$T -> Eq]@ / @Maybe[$T -> Eq]@ re-reference the class in
-- their element) is handled by a goal stack (least fixpoint): a goal that
-- recurs on the current derivation path can produce no /finite/ witness - and
-- Catln values are finite - so that path is reported uninhabited instead of
-- being re-expanded. Every query therefore terminates in time polynomial in
-- the class-graph size, where the materializing algebra re-expands the
-- recursive universe combinatorially.
--
-- The engine is CONSERVATIVE TOWARDS SATISFIABILITY: whenever a constraint
-- falls outside the decidable fragment (a 'PredExpr', constants, an unhandled
-- type shape, an oversized DNF) it answers 'True' ("may be inhabited").
-- Callers may therefore only rely on a 'False' answer ("provably empty"),
-- which is exactly the polarity the emptiness/implication/coverage decisions
-- need.

-- | Which ANSWER of a satisfiability decision the caller relies on. Not a knob: it selects
-- which direction of the decision must be sound, and therefore whether enumerating the
-- CURRENT members of an open (unsealed, non-whole-program) class is legitimate.
--
-- * 'ProveWitness' — the caller relies on SAT (@∃@ a value). Enumeration is sound: a member
--   found now IS a member, and a future file adding another cannot unmake it.
-- * 'ProveEmpty' — the caller relies on UNSAT (@∀@ values). Enumeration is UNSOUND in the
--   open world: a future file may add exactly the member that makes the goal satisfiable.
--
-- These are genuinely two different queries and both are memoized under this key.
data SatUse
  = ProveWitness
  | ProveEmpty
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | One conjunct constraining a single value in a satisfiability goal.
data SatAtom
  = SatClass PartialType   -- ^ the value has class C (a 'PredClass')
  | SatRel PartialType     -- ^ the value isa R (a 'PredRel')
  | SatLeafs [PartialType] -- ^ the value matches one of these concrete partials
  | SatOpaque TypePredicate -- ^ the value satisfies a predicate the engine knows nothing of — a value
                           -- predicate ('PredExpr') or a constant equality ('PredConstEq'): a
                           -- propositional variable it can refute against its own negation
                           -- ('satDNFConj', 'satGoalRaw') and otherwise drops before enumeration
                           -- ('satGoalRaw'); a goal weakened that way yields no witness, only "may be
                           -- inhabited" ('satGoalsDecided')
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | A satisfiability goal: a single value satisfying all of 'sgPos' and none of 'sgNeg'.
data SatGoal = SatGoal { sgPos :: [SatAtom], sgNeg :: [SatAtom] }
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | The hypothesis stack of goals on the current derivation path (see 'satGoal').
type SatHyps = S.HashSet SatGoal

-- | A member shape produced by enumerating a class or relation.
data SatCand
  = CandLeaf PartialType -- ^ a concrete member shape
  | CandGoal SatGoal     -- ^ a member described by further predicates (e.g. a subclass edge)

satGoalNorm :: SatGoal -> SatGoal
satGoalNorm (SatGoal pos neg) = SatGoal (L.sort (L.nub pos)) (L.sort (L.nub neg))

mergeSatGoals :: SatGoal -> SatGoal -> SatGoal
mergeSatGoals (SatGoal p1 n1) (SatGoal p2 n2) = SatGoal (p1 ++ p2) (n1 ++ n2)

-- | Bound on DNF size during conversion; a larger formula is out of fragment.
satDNFCap :: Int
satDNFCap = 64

-- | Cartesian conjunction of per-conjunct DNFs, 'Nothing' when it exceeds 'satDNFCap'.
-- Each step normalizes the combined disjunction — all three rewrites are exact, and without
-- them a tautology-heavy product (e.g. negating the DNF of @¬(A∧B) ∧ ¬(A∧C)@, whose raw
-- product is choicesᵍᵒᵃˡˢ) overflows the cap that a collapsed equivalent stays far under:
--   * dedup atoms within a goal ('satGoalNorm'; conjunction is idempotent),
--   * drop goals requiring and forbidding the same atom (unsatisfiable, removable from a
--     disjunction),
--   * drop subsumed goals (a goal whose literal set contains another goal's has only a
--     subset of its models, so the disjunction is unchanged without it).
satDNFConj :: [[SatGoal]] -> Maybe [SatGoal]
satDNFConj = foldl step (Just [SatGoal [] []])
  where
    step macc gs = do
      acc <- macc
      let combined = simplify [mergeSatGoals g1 g2 | g1 <- acc, g2 <- gs]
      if length combined > satDNFCap then Nothing else Just combined
    simplify = subsume . L.nub . filter (not . refuted) . map satGoalNorm
    refuted (SatGoal pos neg) = any (`elem` neg) pos
    subsume gs = [g | g <- gs, not (any (\g' -> g' /= g && g' `weakerThan` g) gs)]
    -- g' is weaker than g when its literals are a subset of g's (after 'satGoalNorm' dedup)
    weakerThan (SatGoal p1 n1) (SatGoal p2 n2) = p1 `subsetOf` p2 && n1 `subsetOf` n2
    subsetOf xs ys = all (`elem` ys) xs

-- | DNF of a predicate tree over 'SatAtom' literals under a polarity; 'Nothing' outside the
-- fragment (a constant in a union). A value predicate or a constant equality enters as the opaque 'SatOpaque'.
predsSatDNF :: Bool -> TypePredicates -> Maybe [SatGoal]
predsSatDNF pol (PredsOne p) = do
  a <- case p of
    PredClass c   -> Just (SatClass c)
    PredRel r     -> Just (SatRel r)
    PredExpr _    -> Just (SatOpaque p)
    PredConstEq _ -> Just (SatOpaque p)
  Just [if pol then SatGoal [a] [] else SatGoal [] [a]]
predsSatDNF pol (PredsNot p) = predsSatDNF (not pol) p
predsSatDNF True (PredsAnd ps) = mapM (predsSatDNF True) ps >>= satDNFConj
predsSatDNF False (PredsAnd ps) = concat <$> mapM (predsSatDNF False) ps

-- | 'predsSatDNF' for the DISPATCH provers ('partialsOverlapSat', 'partialCoversSat',
-- 'dispatchInFragment'), which keep a value predicate out of their fragment: dispatch on a
-- refinement goes to the materialized accumulation, where the predicate is decided on the value
-- ('TreeBuild.completeTreeSet'); an opaque atom here would read as "may escape" and refuse an arm.
predsSatDNFNoExpr :: Bool -> TypePredicates -> Maybe [SatGoal]
predsSatDNFNoExpr pol preds
  | predsHasExpr preds = Nothing
  | otherwise = predsSatDNF pol preds

-- | Whether a formula holds a value predicate ('PredExpr') anywhere.
predsHasExpr :: TypePredicates -> Bool
predsHasExpr (PredsOne PredExpr{}) = True
predsHasExpr (PredsOne _)          = False
predsHasExpr (PredsAnd ps)         = any predsHasExpr ps
predsHasExpr (PredsNot p)          = predsHasExpr p

-- | DNF of "a value is a member of this type"; 'Nothing' outside the fragment.
-- A 'TVInt' variable is local to its enclosing partial (e.g. a class member's own
-- parameter): it is instantiable per value and never refers to the ambient 'vaenv', so it
-- carries no constraint here. An OUTER variable ('TVParent' or 'TVExt') resolves through the
-- env the caller hands in, which carries the parent's bindings as well as the ambient ones
-- (see 'TVOuter').
typeSatDNF :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Maybe [SatGoal]
typeSatDNF _ _ PTopType = Just [SatGoal [] []]
typeSatDNF typeEnv vaenv (TypeVar _ TVInt i) = satVarOccDNF typeEnv vaenv (Just [SatGoal [] []]) i -- partial-local var: no ambient constraint
typeSatDNF typeEnv vaenv tv@(TypeVar v TVOuter i) = satVarOccDNF typeEnv vaenv varDNF i
  where varDNF = case vaenvLookup typeEnv vaenv v of
          looked | looked == tv -> Just [SatGoal [] []] -- unbound/self-bound var: no constraint
          looked                -> typeSatDNF typeEnv (H.delete (extKey v) vaenv) looked -- delete to break var reference cycles
typeSatDNF _ _ (UnionType _ _ (_:_)) = Nothing -- constants: out of fragment
typeSatDNF _ _ (UnionType (Just (_, _, _:_)) _ _) = Nothing -- excluded constants: out of fragment
-- A leaf enters as one atom, but the engine decides it SLOT BY SLOT
-- ('partialCoversSat'.'elemCovered' via 'typesSatDiff'), so a leaf is only in the fragment when
-- its slots are. Admitting a leaf whose slot is out of the fragment would turn "I cannot tell"
-- into a definite negative: the slot comparison would report the pair as differing, which
-- 'elemCovered' reads as a PROVEN escape, and 'typeCoveredSymbolic' would then answer
-- @Just False@ and short-circuit the materialized fallback that decides it. The pairs this
-- matters for differ in spelling but not in denotation (@/P[/A=$_v∩{c}]@ against @/P[/A={c}]@
-- with @$_v@ unbound); syntactically equal leaves cover each other before any slot is read.
--
-- A leaf's own predicates (@Cons|¬C@, the meet of a leaf with a predicate top) are LIFTED out of
-- the leaf into the goal as their own atoms: the leaf is @Cons ∧ ¬C@, and only as separate atoms
-- does its negation split into @¬Cons ∨ C@. As one opaque atom, @Cons ⊆ U_C ∪ Cons|¬C@ — every
-- Cons is a C or is not — would be undecidable. Predicate-free leaves stay grouped in one
-- 'SatLeafs' atom; a predicated leaf becomes its own goal.
typeSatDNF typeEnv vaenv (UnionType Nothing leafs []) = case splitUnionType leafs of
  [] -> Just [] -- bottom: unsatisfiable
  ls | all (satLeafInFragment typeEnv vaenv) ls -> do
         let (predicated, plain) = partition (\l -> ptPreds l /= PredsNone) ls
         lifted <- mapM liftLeaf predicated
         Just $ [SatGoal [SatLeafs plain] [] | not (null plain)] ++ concat lifted
     | otherwise             -> Nothing
  where
    liftLeaf l = do
      predGoals <- predsSatDNF True (ptPreds l)
      satDNFConj [[SatGoal [SatLeafs [l{ptPreds=PredsNone}]] []], predGoals]
-- An EXCLUDED predicated leaf (@U − Cons|¬C@) is likewise lifted: excluding @Cons ∧ ¬C@ is
-- @¬Cons ∨ C@, a disjunction, so the top's goals split — one branch excludes the bare leaf, the
-- other conjoins the predicates' negation. Predicate-free exclusions stay one negative
-- 'SatLeafs' atom.
typeSatDNF typeEnv vaenv (UnionType (Just (preds, negLeafs, [])) pos []) = do
  topGoals <- predsSatDNF True preds
  let (predicatedNegs, plainNegs) = partition (\l -> ptPreds l /= PredsNone) (splitUnionType negLeafs)
  let negAtoms = [SatLeafs plainNegs | not (null plainNegs)]
  let topGoals' = map (\g -> g{sgNeg = sgNeg g ++ negAtoms}) topGoals
  topGoals'' <- foldM excludeLifted topGoals' predicatedNegs
  posGoals <- typeSatDNF typeEnv vaenv (UnionType Nothing pos [])
  Just (topGoals'' ++ posGoals)
  where
    excludeLifted goals l = do
      notPreds <- predsSatDNF True (PredsNot (ptPreds l))
      withoutPreds <- satDNFConj [goals, notPreds]
      Just $ map (\g -> g{sgNeg = sgNeg g ++ [SatLeafs [l{ptPreds=PredsNone}]]}) goals ++ withoutPreds

-- | Whether the engine can decide a leaf: it compares leaves SLOT BY SLOT, so a leaf is in the
-- fragment only when every slot is.
satLeafInFragment :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> Bool
satLeafInFragment typeEnv vaenv PartialType{ptVars, ptArgs} =
  all (isJust . typeSatDNF typeEnv vaenv) (H.elems ptVars ++ H.elems ptArgs)

-- | DNF of an OCCURRENCE @⟦var⟧ ⊓ ⟦inner⟧@: the variable's own constraint conjoined with the
-- occurrence-local inner's. Dropping the inner would read the occurrence as the bare variable,
-- which for an unbound one is ⊤: @$_a∩{X}@ would enter the engine as "no constraint at all", and
-- a cover query would find an escape witness outside @X@ that no value of the occurrence has.
--
-- The inner is read under the CALLER's environment, not the variable's: it is a type written at
-- the occurrence site, not part of whatever the variable is bound to.
satVarOccDNF :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Maybe [SatGoal] -> Type -> Maybe [SatGoal]
satVarOccDNF _ _ varDNF PTopType = varDNF -- knowledge-free occurrence: the variable alone
satVarOccDNF typeEnv vaenv varDNF i = do
  vd <- varDNF
  innerDNF <- typeSatDNF typeEnv vaenv i
  satDNFConj [vd, innerDNF]

-- | Enumerate the shapes matching a relation (mirrors 'expandRelPartial'): every known
-- type or vaenv-arg name the relative name matches (carrying the relation's args), plus
-- every matching class (as a class constraint). The NAME universe is declaration-bounded
-- (closed-world, the settled relation-type semantics), but a class route inherits the
-- class's own open/closed-world standing: under 'ProveEmpty', a relation matching an UNSEALED
-- class is unenumerable ('Nothing' → conservatively satisfiable), exactly as
-- 'classSatCandidates' gates the class itself — a future member of the open class matches
-- the relation, so a closed-world enumeration would prove implications
-- (e.g. @~List ⟹ ∀Bag@) that new declarations can break.
relSatCandidates :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> SatUse -> PartialType -> Maybe [SatCand]
relSatCandidates typeEnv vaenv satUse relPartial
  | satUse == ProveEmpty && not (tePrgmEnv typeEnv) && any unsealedClass matchingClassNames = Nothing
  | otherwise = Just (argCands ++ tpCands ++ clsCands)
  where
    relName = ptName relPartial
    matchingClassNames = relativeNameFilter relName (listClassNames typeEnv)
    unsealedClass n = case H.lookup n (scClasses (teSatCache typeEnv)) of
      Just ClassSatInfo{csiSealed} -> not csiSealed
      Nothing                      -> True
    argCands = [CandLeaf relPartial{ptName=n} | n <- relativeNameFilter relName (map pkName (H.keys (snd (splitVarArgEnv (envAmbient vaenv)))))]
    -- Arg-keyset feasibility mirrors 'expandRelPartial': an uninhabitable grafted candidate
    -- (keys no object of the name declares) admits nothing, so dropping it is sound in both
    -- polarities and keeps the two surfaces in agreement.
    tpCands = [CandLeaf relPartial{ptName=n} | n <- filter (relArgKeysFeasible typeEnv relPartial) (relativeNameFilter relName (scTypeNames (teSatCache typeEnv)))]
    -- Carry the relation's own type arguments onto the class candidate (@isa ConsList[/T=∀ToString]@
    -- ⇒ @ConsList[/T=∀ToString]@, not a bare @ConsList@ whose Top element would spuriously escape a
    -- @DShow@ bound), mirroring the arg/type candidates above which already reuse @relPartial@.
    clsCands = [CandGoal (SatGoal [SatClass relPartial{ptName=n}] []) | n <- matchingClassNames]

-- | The classes a relative name matches (the class routes of a 'PredRel'). The relation's own type
-- arguments/element vars are carried onto each class partial (@isa ConsList[/T=∀ToString]@ routes to
-- @ConsList[/T=∀ToString]@, not a bare @ConsList@ whose Top element would ignore the bound), mirroring
-- 'relSatCandidates.clsCands'. A bare relation yields bare class partials.
relMatchingClasses :: TypeGraph tg => TypeEnv tg -> PartialType -> [PartialType]
relMatchingClasses typeEnv relPartial = [relPartial{ptName=n} | n <- relativeNameFilter (ptName relPartial) (listClassNames typeEnv)]

-- | Declaration-level implication between two atoms: the non-recursive fast paths of
-- 'predImpliesAtom' (same-name and declared @isa@ edges), which are sound in the OPEN world -
-- a declared edge holds for every future member, so @a1 ∧ ¬a2@ is unsatisfiable whatever the
-- enumeration gate says. Used to refute a 'satGoal' whose positive atom implies a negative one
-- (e.g. @Fish ∧ ¬Animal@ with @every(Fish isa Animal)@) without enumerating the class - the
-- gated enumeration otherwise reads such a goal as conservatively satisfiable. Never consults
-- the engine, so it is recursion-safe inside 'satGoal'.
satAtomImpliesDeclared :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> SatAtom -> SatAtom -> Bool
satAtomImpliesDeclared typeEnv vaenv a1 a2 = case (a1, a2) of
  -- hasClass C1[..] ⟹ hasClass C2 for the same name or a declared subclass edge; the implied
  -- class must be PLAIN (an element bound would need a covariance check).
  (SatClass c1, SatClass c2)
    | ptName c1 == ptName c2 -> slotsImplied c1 c2
    | otherwise -> plainPartial c2 && classDeclaredSubclassOf typeEnv (ptName c1) (ptName c2)
  -- hasClass C ⟹ isa R when R's name path is a suffix of C's, or C is a declared subclass of R
  -- (@hasClass C ⟹ isa C ⟹ isa R@) - the same declared-edge route 'predImpliesAtom' PredClass/PredRel
  -- carries. Sound in the open world independent of the enumeration gate.
  (SatClass c, SatRel r) -> plainPartial r
    && (relativeNameMatches (ptName r) (ptName c) || classDeclaredSubclassOf typeEnv (ptName c) (ptName r))
  -- isa R ⟹ hasClass C for the same plain name (either absoluteness direction) or a declared
  -- edge ('predImpliesAtom' PredRel/PredClass fast path).
  (SatRel r, SatClass c) -> plainPartial r && plainPartial c
    && (relativeNameMatches (ptName r) (ptName c) || relativeNameMatches (ptName c) (ptName r)
        || classDeclaredSubclassOf typeEnv (ptName r) (ptName c))
  -- isa R1 ⟹ isa R2 when R2's name path is a suffix of R1's, or by a declared edge.
  (SatRel r1, SatRel r2) -> plainPartial r2
    && (relativeNameMatches (ptName r2) (ptName r1)
        || (plainPartial r1 && classDeclaredSubclassOf typeEnv (ptName r1) (ptName r2)))
  _ -> False
  where
    -- Same class, covariant slots decided without the engine: every slot the implied class
    -- spells is either ⊤ (no constraint) or spelled identically on the implying side
    -- (@List[/T=x] ⟹ List[/T=⊤]@, @List[/T=x] ⟹ List[/T=x]@). A free external variable with no
    -- knowledge of its own denotes ⊤, read as 'vaenvLookup' reads it.
    slotsImplied c1 c2 = slotOk (ptVars c1) (ptVars c2) && slotOk (ptArgs c1) (ptArgs c2)
    slotOk s1 s2 = and [slotTop v || H.lookup k s1 == Just v | (k, v) <- H.toList s2]
    slotTop PTopType = True
    slotTop (TypeVar b TVExt PTopType) = case vaenvLookup typeEnv vaenv b of
      PTopType              -> True
      TypeVar b' _ PTopType -> b' == b
      _                     -> False
    slotTop _ = False

-- | Whether a partial is within the fragment of the dispatch-coverage queries below
-- ('satLeafEscapesAll' and its wrappers): no 'PredExpr' predicates and no constant
-- components anywhere in its var\/arg elements. An out-of-fragment dispatch falls back to
-- the materialized accumulation in 'completeTreeSet'.
dispatchInFragment :: PartialType -> Bool
dispatchInFragment PartialType{ptVars, ptArgs, ptPreds} =
  predsInFrag ptPreds && all elemInFrag (H.elems ptVars) && all elemInFrag (H.elems ptArgs)
  where
    predsInFrag = isJust . predsSatDNFNoExpr True
    elemInFrag PTopType = True
    elemInFrag TypeVar{} = True
    elemInFrag (UnionType _ _ (_:_)) = False -- constants: out of fragment
    elemInFrag (UnionType (Just (_, _, _:_)) _ _) = False -- excluded constants: out of fragment
    elemInFrag (UnionType mtop leafs []) =
      maybe True (\(preds, _, _) -> predsInFrag preds) mtop
      && all dispatchInFragment (splitUnionType leafs)
