-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.Algebra.Sat
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- The symbolic satisfiability engine ('satGoal'): whether some value can satisfy a conjunction of
-- type atoms, and the emptiness and coverage questions built on it.
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Semantics.Types.Algebra.Sat where

import                          Control.Exception               (evaluate)
import                          Control.Monad                   (foldM)
import                          Data.Hashable                   (Hashed, hashed)
import                qualified Data.HashMap.Strict             as H
import                qualified Data.HashSet                    as S
import                          Data.IORef                      (IORef,
                                                                 atomicModifyIORef',
                                                                 newIORef,
                                                                 readIORef)
import                qualified Data.List                       as L
import                          Data.Maybe
import {-# SOURCE #-}           Semantics.Types.Algebra.Compact (compactType)
import                          Semantics.Types.Algebra.Expand
import {-# SOURCE #-}           Semantics.Types.Algebra.SetOps  (intersectTypesEnv,
                                                                 intersectTypesWithVarEnv,
                                                                 meetInner,
                                                                 unionAllTypesWithEnv)
import                          Semantics.Types.Algebra.Subst
import {-# SOURCE #-}           Semantics.Types.Algebra.Subtype (isSubtypeOfWithEnv)
import                          Semantics.Types.ArgModes
import                          Semantics.Types.Construct
import                          Semantics.Types.Data
import                          Semantics.Types.Memo
import                          Semantics.Types.Name
import                          Semantics.Types.Preds
import                          Semantics.Types.SatGoal
import                          Semantics.Types.Vars
import                          System.IO.Unsafe                (unsafePerformIO)


-- | Whether a goal carries an opaque atom ('SatOpaque').
satGoalHasOpaque :: SatGoal -> Bool
satGoalHasOpaque (SatGoal pos neg) = any isOpaque pos || any isOpaque neg
  where isOpaque SatOpaque{} = True
        isOpaque _           = False

-- | A goal without its opaque atoms: weaker, so unsatisfiable only where the goal is.
satGoalStripOpaque :: SatGoal -> SatGoal
satGoalStripOpaque (SatGoal pos neg) = SatGoal (filter (not . isOpaque) pos) (filter (not . isOpaque) neg)
  where isOpaque SatOpaque{} = True
        isOpaque _           = False

-- | Three-valued answer to "is some goal of the disjunction satisfiable?" ('satGoal' on each):
-- @Just False@ when none is (every goal is refuted, opaque atoms stripped or not — a weaker goal
-- unsatisfiable is the goal unsatisfiable), @Just True@ when a goal WITHOUT opaque atoms is
-- satisfiable (a witness the engine can vouch for), and 'Nothing' when only goals carrying
-- opaque atoms are: the engine answered for the goal without them, which says nothing of the
-- goal itself. Consumers whose negative is final ('typeCoveredSymbolic', 'predsEmptySymbolic')
-- read 'Nothing' as a refusal and go on to the provers that decide a value predicate on a value.
satGoalsDecided :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> SatUse -> [SatGoal] -> Maybe Bool
satGoalsDecided typeEnv vaenv satUse goals
  | any (\g -> not (satGoalHasOpaque g) && satGoal typeEnv vaenv satUse S.empty g) goals = Just True
  | any (satGoal typeEnv vaenv satUse S.empty) goals = Nothing
  | otherwise = Just False

-- | Resolve same-class (and same-relation) POSITIVE atoms in a goal against each other: a
-- goal's positive atoms all constrain the ONE witness value, so two atoms over the same
-- class meet into a single atom, with the meet's own discovered bindings resolved back into
-- it (the standalone reading, as 'mergePredsAnd'; @Maybe[$_A] ∧ Maybe[Int] → Maybe[Int]@).
-- Such a pair cannot be unified at the type level (the predicate merge only sees DISTINCT
-- classes there, e.g. @Functor[$_A] ∧ Maybe[Int]@) — it arises here, when class-member
-- expansion ('classSatCandidates') rewrites the outer class into a member atom. Without the
-- meet, candidates generated from the flexible atom keep an unbound slot and manufacture
-- escape witnesses no value satisfying BOTH atoms has (@Some[$_A]@ "escaping" @∀Maybe[Int]@
-- — a false SAT). The discoveries are NOT propagated to the goal's
-- other atoms: an unbound flexible slot constrains nothing beyond class membership, so the
-- collapsed pair is exact, while pinning the variable elsewhere would strengthen those
-- atoms towards a false UNSAT — the polarity ('satGoal' is conservative-to-SAT; UNSAT
-- claims prove subtypes) the engine must never err towards.
satGoalUnifyPos :: TypeVarArgEnv -> SatGoal -> SatGoal
satGoalUnifyPos vaenv goal@(SatGoal pos neg)
  | length (L.nub keys) == length keys = goal
  | otherwise = satGoalNorm (SatGoal (foldl step [] pos) neg)
  where
    keys = [(k, ptName p) | Just (k, p) <- map atomKP pos]
    atomKP (SatClass p) = Just (0 :: Int, p)
    atomKP (SatRel p)   = Just (1, p)
    atomKP SatLeafs{}   = Nothing
    atomKP SatOpaque{}  = Nothing
    mkAtom 0 p = SatClass p
    mkAtom _ p = SatRel p
    step kept atom = case atomKP atom of
      Nothing -> kept ++ [atom]
      Just (k, p) -> case break (sameKN k p) kept of
        (pre, ex:post) | Just (_, ep) <- atomKP ex ->
          case unifyAtomVars ep p of
            Just m  -> pre ++ mkAtom k m : post
            Nothing -> kept ++ [atom]
        _ -> kept ++ [atom]
    sameKN k p a = case atomKP a of
      Just (k', p') -> k' == k && ptName p' == ptName p
      Nothing       -> False
    -- Slot-wise meet of two same-name atoms, resolvable only when each shared slot pair has
    -- a side that constrains nothing (⊤ or an unbound\/partial-local variable): take the
    -- other side. Anything else (two distinct concrete bounds, predicates, args) bails to
    -- keeping both atoms, which the enumeration handles. Deliberately NOT the full
    -- 'intersectPartials': the type-algebra meet re-enters engine decisions from inside
    -- 'satGoal', which is both a recursion hazard (no goal-stack guard protects this entry
    -- normalization) and a hot-path cost.
    unifyAtomVars a b
      | ptPreds a /= PredsNone || ptPreds b /= PredsNone = Nothing
      | not (H.null (ptArgs a) && H.null (ptArgs b)) = Nothing
      | otherwise = do
          vars <- sequenceA (H.unionWith meetSlot (fmap Just (ptVars a)) (fmap Just (ptVars b)))
          Just a{ptVars=vars}
    meetSlot mx my = do
      x <- mx
      y <- my
      meet2 x y
    meet2 x y
      | x == y = Just x
      | slotFree x = Just y
      | slotFree y = Just x
      | otherwise = Nothing
    slotFree PTopType = True
    slotFree (TypeVar _ TVInt _) = True
    slotFree (TypeVar v TVOuter _) = case outerBinding vaenv v of
      Unbound    -> True
      SelfLink _ -> True
      BoundTop   -> True -- bound to ⊤: constrains nothing, exactly as an unbound one
      BoundVar _ -> False
      Bound _    -> False
    slotFree _ = False

-- | The independent ways to falsify a goal: one falsified positive or one satisfied negative.
satGoalNegChoices :: SatGoal -> [SatGoal]
satGoalNegChoices (SatGoal pos neg) = [SatGoal [] [p] | p <- pos] ++ [SatGoal [n] [] | n <- neg]

-- | SAT of a conjunction of member-of-type constraints; conservative towards 'True'.
typesConjSat :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> SatUse -> SatHyps -> [Type] -> Bool
typesConjSat typeEnv vaenv satUse hyps ts = case mapM (typeSatDNF typeEnv vaenv) ts >>= satDNFConj of
  -- Outside the fragment, as in 'typesSatDiff': an unconditional "satisfiable" would be read by
  -- the engine as a PROVEN overlap, so a slot it cannot encode (a variable bound to a leaf with a
  -- constant slot) would overlap even a disjoint one (@⊤ − X@) and wrongly count a leaf as
  -- excluded. The materialized meet decides it; a meet that does not reduce to ∅ stays
  -- "satisfiable".
  Nothing    -> case ts of
    [] -> True
    _  -> not (isBottomType (compactType typeEnv vaenv (foldr1 (intersectTypesEnv typeEnv vaenv) ts)))
  Just goals -> any (satGoal typeEnv vaenv satUse hyps) goals

-- | SAT of @t1 ∧ ¬t2@ (a value of @t1@ escaping @t2@); conservative towards 'True'.
typesSatDiff :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> SatUse -> SatHyps -> Type -> Type -> Bool
typesSatDiff typeEnv vaenv satUse hyps t1 t2 = case res of
  -- Outside the fragment the engine cannot tell, and "differs" would be read by every caller as
  -- a PROVEN escape. With @$_a@ unbound, @F[/i=⊤ − F[/i=$_a∩{c}]]@ and @F[/i=⊤ − F[/i={c}]]@ are
  -- equal sets, but the constant puts the slot outside the fragment and a "differs" there would
  -- manufacture a witness. The pair is asked of the materialized prover instead, which is sound
  -- in the direction the callers rely on.
  Nothing    -> not (isSubtypeOfWithEnv typeEnv vaenv t1 t2)
  Just goals -> any (satGoal typeEnv vaenv satUse hyps) goals
  where
    res = do
      d1 <- typeSatDNF typeEnv vaenv t1
      d2 <- typeSatDNF typeEnv vaenv t2
      -- ¬(∨ d2) = ∧ ¬g2, and each ¬g2 is a disjunction of single-literal flips
      negConj <- satDNFConj (map satGoalNegChoices d2)
      satDNFConj [d1, negConj]

-- | Enumerate a class's member shapes from the class graph. 'Nothing' when the class is
-- unknown, or when the caller relies on UNSAT ('ProveEmpty') and the class is neither sealed nor in the closed
-- world ('tePrgmEnv') - the open-world gate of 'predImpliesAtom' (an unenumerable class
-- must be treated as possibly containing anything).
classSatCandidates :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> SatUse -> PartialType -> Maybe [SatCand]
classSatCandidates typeEnv vaenv satUse PartialType{ptName, ptVars=classVarsP} =
  case H.lookup ptName (scClasses (teSatCache typeEnv)) of
    Just ClassSatInfo{csiSealed=sealed, csiVarsDecl=classVarsDecl, csiTypes=classTypes}
      | satUse == ProveEmpty && not (sealed || tePrgmEnv typeEnv) -> Nothing
      | null classTypes -> Just [CandLeaf (classPlaceholderLeaf ptName)]
      | otherwise -> concat <$> mapM candsOfMember classTypes
      where
        classVars = H.unionWith (intersectTypesEnv typeEnv vaenv) classVarsP (cleanDeclSelfRefs classVarsDecl)
        candsOfMember (TypeVar (TVVar t) _ _) = case H.lookup t classVars of
          Just v  -> candsOfType v
          Nothing -> Nothing
        candsOfMember (TypeVar (TVArg _) _ _) = Nothing
        -- A member partial whose name is itself a CLASS (e.g. @ConsList[/T=Eq]@ as a member of @Eq@)
        -- is emitted as a class-predicate goal, not a bare leaf: 'partialCoversSat' requires an exact
        -- name match, so a leaf named @ConsList@ never covers a value shaped as one of its
        -- CONSTRUCTORS (@Nil@/@Cons@). Routing it through 'satGoal' expands the class to its
        -- constructors (with the member's element bound applied), so e.g. @Nil@ is recognised as an
        -- @Eq@ (an empty list vacuously satisfies the @[/T=Eq]@ element bound). A non-class member
        -- (a concrete data type) stays a completed leaf.
        candsOfMember (UnionType Nothing p []) = Just (map mkMemberCand (mapMaybe (classSideMember typeEnv vaenv classVars) (splitUnionType p)))
        candsOfMember t = candsOfType t
        mkMemberCand tp@PartialType{ptName=tpNm}
          | H.member tpNm (scClasses (teSatCache typeEnv)) = CandGoal (SatGoal [SatClass (mapClassPartial tp)] [])
          | otherwise = CandLeaf (mapClassPartial tp)
        -- Apply the class's instantiated arguments to a class-typed member (e.g. @List@'s
        -- @ConsList[/T=$_T]@ member) before enumerating it, mirroring 'mapClassPartial'/'substMemberVar'
        -- on the positive-partial members: otherwise the member's element parameter stays free and the
        -- enumeration widens it (@List[/T=Integer]@ would enumerate as @ConsList[/T=$_T]@).
        -- EXIT substitution ('substituteExit'): a ⊤-bound parameter substitutes the ⊤ it denotes.
        -- Keeping the @$_T@ spelling instead (the informative-only variant) would leak it into the
        -- recursive member query, whose own 'classVars' meet runs under the QUERY's ambient env,
        -- where a like-named ambient binding (@/T=∀Ord@) would capture the class-scope spelling and
        -- harden the member's element, deciding @Cons[/T=⊤] ⊆ ∀List[/T=⊤]@ False.
        -- Flexibility is NOT lost: each 'classSatCandidates' level re-derives it from its own
        -- uninstantiated parameters ('substMemberVar's ⊤ path and the cover checks' free reads).
        candsOfType t = map CandGoal <$> typeSatDNF typeEnv vaenv (substituteExit typeEnv ((ambientEnv $ joinVarArgEnv classVars H.empty)) t)
        -- Substitute only informative bindings: an uninstantiated parameter (bound to
        -- 'PTopType', the declaration default) leaves the member's own type variable in
        -- place, keeping it FLEXIBLE for the cover checks - parity with the enumeration
        -- paths, which check raw member declarations whose variables bind on demand.
        informativeVars = H.filter (/= PTopType) classVars
        substMemberVar (TypeVar (TVVar v) _ _) = case H.lookup v informativeVars of
          Just b  -> b
          -- An uninstantiated class parameter is flexible per value — the ⊤ it denotes. The
          -- class scope the @$_T@ spelling names is exiting here, and fabricating a
          -- partial-local self-entry (@/T=$/T@) instead is a producer error form (a
          -- parameter is a constant, another variable, or a parent link — never itself).
          Nothing -> PTopType
        substMemberVar t = substituteVarsWithVarEnv typeEnv informativeVars t
        -- The member leaves the class scope here, so its declared fields/preds are
        -- instantiated at expansion EXIT ('substitutePartialExit', as 'expandClassPartial'
        -- does): a nested self-entry in a declared field (@tail=∀ConsList[/T=$/T]@) receives
        -- the class's instantiated element rather than staying a class-scope spelling that a
        -- meet detached from this scope widens to ⊤. The member's own 'ptVars' use
        -- 'substMemberVar' instead — an uninstantiated parameter must stay FLEXIBLE
        -- (partial-local) for the cover checks.
        mapClassPartial tp = (substitutePartialExit typeEnv ((ambientEnv $ joinVarArgEnv classVars H.empty)) tp){ptVars = fmap substMemberVar (ptVars tp)}
    _ -> Nothing

-- | Interprets a concrete witness shape under its data declaration. Every actual value of a
-- data type satisfies the declaration's field types (a @Cons@ value's @tail@ IS a @ConsList@
-- by construction), so a ⊤-valued declared field carries no constraint of its own but the
-- value universe does. Substitute the declared field type (with the shape's own informative
-- param bindings applied) before the engine reasons about what the shape can escape; taking a
-- ⊤ field at face value manufactures escape witnesses no real value has (e.g.
-- @Cons(head=⊤, tail=⊤)@ "escaping" @∀ConsList@ through a non-list tail, wrongly refuting
-- @Cons(head=⊤, tail=⊤) ⊆ ∀ConsList@). Only /present/ ⊤-valued fields are tightened — the
-- value-set is unchanged, so this is a sound normalization for witness candidates (the
-- covering sup side must keep its authored ⊤ fields: narrowing a sup could flip a proven
-- cover to unproven). A bare argless partial is left alone: it may denote the unapplied
-- constructor (an arg-powerset partial application), whose value set has no fields at all.
satValueShape :: TypeGraph tg => TypeEnv tg -> PartialType -> PartialType
satValueShape typeEnv p@PartialType{ptName, ptVars, ptArgs}
  | H.null ptArgs = p
  | otherwise = case [dArgs | PartialType{ptArgs=dArgs} <- typeGraphData typeEnv ptName, not (H.null dArgs)] of
      (dArgs:_) -> p{ptArgs = H.mapWithKey (tightenField dArgs) ptArgs}
        where
          tightenField dArgs' k PTopType | Just ft <- H.lookup k dArgs' = interpretField ft
          tightenField _ _ t = t
          interpretField = substituteVarsWithVarEnv typeEnv informativeVars
          informativeVars = H.filter (/= PTopType) ptVars
      _ -> p

-- | Whether a relation's /name route/ applies to a shape: the relative name matches AND the
-- shape's name is a known type name. Parity with 'expandRelPartial', whose type candidates
-- come only from 'teNames' - a name outside the universe is not matched by a relation.
relNameAdmits :: TypeEnv tg -> PartialType -> PartialType -> Bool
relNameAdmits TypeEnv{teNames} r m =
  relativeNameMatches (ptName r) (ptName m) && S.member (ptName m) teNames

-- | Whether a partial is uninhabited under DECLARATION-BOUNDING: a field value disjoint from the
-- field's declared type admits no constructible value (@Cons(tail=X)@ for an @X@ that is no list
-- denotes ∅ — every @Cons@ tail is declared @∀ConsList@), making the partial set-equal to the
-- empty set. Only a
-- concrete non-⊤ value differing from its declared bound pays the disjointness meet; ⊤ and
-- symbolic (variable) values, and a value equal to its bound, are trivially inside, so
-- member-form and field-completed partials decide without any meet.
--
-- A declared bound that is a PARAMETER REFERENCE (@head -> $T@) is instantiated before the test:
-- the partial's own binding for that parameter is what bounds the field HERE, so
-- @Cons[/T=Integer](head=/String)@ is uninhabited even though the raw bound @$T@ says nothing.
-- The instantiation ('freeTVIntToTopDeep') reads a reference the partial does not bind as ⊤, and
-- the meet resolves any remaining variable through the ambient env, so an unresolvable bound
-- still decides "inside" — but the meet's own pointer clamp ('intersectPartialsBase'.'varBoundFor')
-- reads the bound this way, and a reader that stopped at the bare reference would call inhabited
-- what the meet calls ∅.
partialUninhabitedByDecl :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> Bool
partialUninhabitedByDecl typeEnv vaenv p@PartialType{ptName, ptArgs}
  | H.null ptArgs = False
  | null decls = False
  | otherwise = any argEmptyInAllDecls (H.toList (requiredArgsOf p))
  where
    decls = [dArgs | PartialType{ptArgs=dArgs} <- typeGraphData typeEnv ptName, not (H.null dArgs)]
    argEmptyInAllDecls (k, v) =
      let bounds = mapMaybe (H.lookup k) decls
      in not (null bounds) && all (clampEmpty v) bounds
    clampEmpty PTopType _  = False
    clampEmpty TypeVar{} _ = False
    clampEmpty _ PTopType  = False
    clampEmpty v d | v == d = False
                   | otherwise = case freeTVIntToTopDeep typeEnv vaenv p d of
                       PTopType -> False
                       d'       -> v /= d' && isBottomType (intersectTypesEnv typeEnv vaenv v d')

-- | Whether a partial is uninhabited because one of its arg\/var SLOT VALUES is itself empty: no
-- constructible value has an ∅-typed field or type-parameter (@Cons[/T=∅]@, @Data(field=∅)@ denote
-- ∅). Each slot is read under the ambient env ('substituteTVExt' resolves a slot's own external
-- variable — a @$_c@ bound to a semantically-empty @Top(#a ∧ #b)@) and compacted, so it recognises
-- exactly what 'compactType' collapses. 'compactType' already reduces such a partial to ∅, but the
-- standalone subtype\/emptiness path never re-compacts its inputs, so without this INTERSECTION (which
-- keeps the slot symbolic) and DIFFERENCE (which resolves the variable to ∅) disagree on the
-- partial's emptiness — breaking @a∩b = a−(a−b)@ when the intersection's result is denotationally ∅.
--
-- Each slot is read in the partial's OWN scope: an internal pointer in a slot (@value=$/T@)
-- names THIS partial's parameter, never a like-named ambient variable. Read detached (the
-- walker's top-level fallback for a pointer no scope binds is the ambient env, the encoder's
-- sentinel rule), @Some[/T=~Nil](value=$/T)@ under an env binding @/T@ to an empty type would
-- read its @value@ as that type and be judged uninhabited — and so a subtype of everything.
--
-- The scope is every parameter the partial BINDS ('partialBindsParam'), spelled or not: an
-- unspelled declared parameter is ⊤ (see 'PartialType'), and a pointer to it reads ⊤, not a
-- like-named ambient binding. A spread's declared floor ('spreadFloor') writes @head=$/T@ into a
-- @Cons..@ that spells no @/T@; read through an ambient @/T@ whose value holds @Cons..@ again,
-- the emptiness test would compact that value, whose absorb check asks about @Cons..@, and the
-- question would never settle.
partialHasEmptySlot :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> Bool
partialHasEmptySlot typeEnv vaenv p@PartialType{ptVars} =
  any slotEmpty (H.elems ptVars ++ H.elems (requiredArgsOf p))
  where
    ownScope = joinVarArgEnv (H.union ptVars (declaredParamsAtTop typeEnv p)) H.empty
    slotEmpty v = isBottomType (compactType typeEnv vaenv (substituteTVExtScoped typeEnv (ambientEnv $ ownScope) vaenv v))

-- | A partial that denotes ∅ for either reason: a field disjoint from its declared bound
-- ('partialUninhabitedByDecl') or an intrinsically-empty slot value ('partialHasEmptySlot').
partialUninhabited :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> Bool
partialUninhabited typeEnv vaenv p =
  partialUninhabitedByDecl typeEnv vaenv p || partialHasEmptySlot typeEnv vaenv p || partialPredExprDecidedFalse typeEnv vaenv p

-- | Whether a POSITIVE type denotes ∅ because every one of its leafs is uninhabited
-- ('partialUninhabited'). @∅@ is a subset of everything, but the subtype oracle walks its
-- operands as written and never re-compacts them, so a leaf that 'compactType' would collapse
-- (@/Pair[/A=∅]@, or a parameter slot holding an occurrence whose variable value and inner are
-- disjoint) otherwise reads as inhabited there while the meet and the union agree it is empty —
-- and @a ⊆ a∪b@ fails on the very leaf the union dropped. Callers use it as the LAST disjunct so
-- the per-leaf compaction is paid only on an otherwise-negative answer.
positiveLeafsAllUninhabited :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialLeafs -> Bool
positiveLeafsAllUninhabited typeEnv vaenv leafs =
  not (H.null leafs) && all (partialUninhabited typeEnv vaenv) (splitUnionType leafs)

-- | SAT(a ∧ b) for two concrete shapes; conservative towards 'True'.
--
-- Only the SHARED 'ptVars' keys need comparing. An unspelled parameter is ⊤ (see 'PartialType')
-- and @⊤ ⊓ x@ is inhabited for every inhabited @x@, so a dimension only one side constrains can
-- never refute an overlap. That equivalence is specific to this ∃ direction: the ∀ directions
-- ('partialCoversSat', 'isSubPartialOfWithEnv') must ask about every key the other side spells.
partialsOverlapSat :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> SatUse -> SatHyps -> PartialType -> PartialType -> Bool
partialsOverlapSat typeEnv vaenv satUse hyps a0 b0 =
  ptName a == ptName b
  && argModesOverlap a b
  && all (elemSat vaenv) (H.elems (H.intersectionWith (,) (ptVars a) (ptVars b)))
  && all (elemSat argEnv) (H.elems (H.intersectionWith (,) (ptArgs a) (ptArgs b)))
  && predsOk
  where
    -- Both sides read with their spread floor ('spreadFloor'), as 'partialCoversSat' and the
    -- meet read them. A bare @/P..@ is @/P@ with at least its DECLARED fields, so it does not
    -- overlap the argless @/P@. Admission and cover must agree about every value: were this
    -- compared on literal modes, 'satAdmits' would let the argless witness into @/P..@ while
    -- 'partialCoversSat' found it outside, manufacturing an escape no value has.
    a = spreadFloor typeEnv a0
    b = spreadFloor typeEnv b0
    elemSat env (ta, tb) = typesConjSat typeEnv env satUse hyps [ta, tb]
    -- Fields in the partials' own scope, parameter values in the containing one, as in
    -- 'partialCoversSat' ('shadowBoundParams').
    argEnv = shadowBoundParams typeEnv [a0, b0] vaenv
    predsOk = case predsAnd (ptPreds a) (ptPreds b) of
      PredsNone -> True
      preds -> case predsSatDNFNoExpr True preds of
        Nothing  -> True
        Just dnf -> any (\g -> satGoal typeEnv vaenv satUse hyps
                          (mergeSatGoals g (SatGoal [SatLeafs [a{ptPreds=PredsNone}], SatLeafs [b{ptPreds=PredsNone}]] []))) dnf

-- | Everything a field of a partial can hold, by what its name's data objects declare there: the
-- UNION of the field's declared type across all of them, instantiated in the partial's own scope.
-- 'Nothing' when no data object of the name declares the field.
--
-- This is the clamp 'isSubPartialOfWithEnv' and 'partialCoversSat' re-read a sub's field through:
-- no constructible value holds a field outside what is declared for it. It has to be the union.
-- A name can have more than one data object -- a value a page declares is one, beside the
-- declaration it is a value of -- and a value is built from any of them, so a field may hold what
-- any of them allows. Clamping by each object in turn would read a field as empty whenever it
-- missed one object's field (a task's @state=True@ against another page's task with
-- @state=False@), and an empty field makes the whole sub empty.
declaredArgBound :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> PartialType -> ArgName -> Maybe Type
declaredArgBound typeEnv vaenv sub k = case bounds of
  [] -> Nothing
  _  -> Just $ unionAllTypesWithEnv typeEnv vaenv bounds
  where bounds = [freeTVIntToTopDeep typeEnv vaenv sub d | PartialType{ptArgs=dArgs} <- typeGraphData typeEnv (ptName sub), Just d <- [H.lookup k dArgs]]

-- | A family of supers identical to the sub at every var/arg slot but ONE jointly covers the
-- sub when the union of their values at that slot covers the sub's value there — e.g.
-- @fluffy(∀Animal) ⊆ fluffy(Animal∧¬Fish) ∪ fluffy(∀Fish)@, the pair the exact union's
-- negLeaf reduction produces alongside an untouched original. Erasing the slot to ⊤ on both
-- sides delimits the family; the arg MODE is normalised out of that structural match and
-- handled by 'argModeCovers' instead — a super whose mode is WIDER than the sub's (e.g. the
-- @..@/'PtArgAny' leaf a difference leaves beside an exact one) still covers the sub's arg-sets
-- at the matching slot value, so excluding it on a mode mismatch would miss the joint cover.
-- The slot cover is decided through the type-level union, so it is whatever the algebra proves.
-- The COVER primitive is the caller's: the materialized prover passes its own subtype check,
-- the symbolic one the engine's ('typesSatDiff'), so a single rule serves both instead of two
-- copies that must be kept in agreement.
slotUnionCovers :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> (Type -> Type -> Bool) -> PartialType -> [PartialType] -> Bool
slotUnionCovers typeEnv vaenv covers sub sup =
  any (coverSlot ptArgs (\p m -> p{ptArgs=m})) (slotKeys ptArgs)
  || any (coverSlot ptVars (\p m -> p{ptVars=m})) (slotKeys ptVars)
  where
    -- The supers' keys are tried only when the sub spells NOTHING on this axis. That is the
    -- case the rule exists for (a bare leaf against a family that partitions the dimension);
    -- trying them in general re-runs the family scan and its type-level union for keys the sub
    -- already answers for, at a large cost.
    --
    -- "Spells nothing" is judged on what the sub DENOTES: a parameter spelled ⊤ is no
    -- constraint (see 'PartialType'), so @/P[/a=⊤]@ is the bare @/P@ here, and the supers'
    -- partition of another dimension @/d@ is still found.
    slotKeys get | H.null (get subN) = L.nub (concatMap (H.keys . get) sup)
                 | otherwise         = H.keys (get subN)
    subN = sub{ptVars = H.filter (/= PTopType) (ptVars sub)}
    -- The dimension may be spelled only on the SUPER side: a parameter the sub leaves
    -- unspelled is ⊤ there (see 'PartialType'), so a family of supers that partitions that
    -- dimension still covers it: a bare @/x@ has no keys at all, yet @/x ⊆ /x[/p=A] ∪
    -- /x[/p=⊤−A]@ must hold exactly as it does for the spelled-out @/x[/p=⊤]@.
    coverSlot get set k =
        let sv = H.lookupDefault PTopType k (get sub)
            -- The family is delimited by equality OUTSIDE the slot, and that equality must be
            -- of what the partials DENOTE: a parameter spelled ⊤ is the same set as one left
            -- unspelled (see 'PartialType'), so ⊤-valued parameters are dropped before
            -- comparing: @/P[/a=⊤, /d=X]@ belongs to the same family as @/P[/d=Y]@, and a family
            -- partitioning @/d@ must be found whichever way the members spell @/a@. This is the
            -- sub-side twin of the super-side rule on 'slotKeys' above. Only PARAMETERS: an
            -- unspelled ARG is absent, not ⊤, so the arg map is compared as written.
            dropTopVars p = p{ptVars = H.filter (/= PTopType) (ptVars p)}
            erase p = dropTopVars (set p (H.insert k PTopType (get p))){ptArgMode = PtArgExact}
            -- Normalise both modes before 'argModeCovers': an exact super does not literally
            -- cover a sub carrying a vacuous @..@\/'PtArgAny' (@subUb@=False) — even though that
            -- @..@ admits no arg-set beyond the sub's own declared keys — so without this the
            -- family would be empty and a joint cover such as @f(x=∀A).. ⊆ f(x=∀B) ∪ f(x=A∧¬B)@
            -- would be missed. Every other sub-partial path applies the same normalisation.
            family = [v | supP <- sup, Just v <- [H.lookup k (get supP)]
                        , erase supP == erase sub
                        , argModeCovers (normalizePtArgMode typeEnv supP) (normalizePtArgMode typeEnv sub)]
        in length family > 1 && covers sv (unionAllTypesWithEnv typeEnv vaenv family)

-- | A partial's parameters as its VALUES carry them. A parameter the DECLARATION ties to a field
-- (@head -> $T@) is that field's value on every value of this shape, so @Cons[/T=⊤](head=W)@ and
-- @Cons[/T=W](head=$/T)@ spell the same set (a ??-interval piece and its enumerated difference
-- form respell the same narrowing on the slot vs the var dimension) and must compare equal.
--
-- The field's value is read in the partial's OWN scope: a parent reference inside it
-- (@head=Cons[/T=$^/T](..)@) names this partial's parameter, and met as written it would be out
-- of scope — a flexible ⊤ — so @Cons[/T=B](head=Cons[/T=$^/T](..))@ and
-- @Cons[/T=B](head=Cons[/T=B](..))@, one set, would derive different parameters.
--
-- Derived from the partial ALONE, never from a comparison's shared env: reading the other side's
-- binding back would narrow this partial to it and vacuously prove @Cons(head=$/T) ⊆
-- Cons[/T=X](head=X)@ for any @X@.
--
-- A function's declaration ties its parameter to its arguments the same way a data type's ties
-- it to fields: @(_ :: $T ++ _ :: $T)[$T -> Semigroup]@ makes an application's @/T@ what its
-- arguments are. So the declarations' input templates ('typeGraphArrowTemplates') are read
-- beside the data declarations. Read from the data alone, an application spelled without the
-- parameter (@++(l=String, r=String)@) kept it at ⊤, and so fell outside its own declaration's
-- @/T=∀Semigroup@ whatever its arguments were.
ptVarEff :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> TypeVarName -> Type -> Type
ptVarEff typeEnv vaenv p k v = foldr (intersectTypesEnv typeEnv vaenv) v
  [ substituteTVExtScoped typeEnv ((ambientEnv $ joinVarArgEnv (ptVars p) H.empty)) vaenv av
  | (ak, av) <- H.toList (ptArgs p), not (isTypeVar av)
  , dp <- typeGraphData typeEnv (ptName p) ++ typeGraphArrowTemplates typeEnv (ptName p)
  , Just (TypeVar (TVVar k') TVInt _) <- [H.lookup ak (ptArgs dp)], k' == k ]

-- | The partial re-spelled with its effective parameters ('ptVarEff'). This is the spelling a
-- DECLARED FIELD BOUND must be instantiated in: the bound is a template over the partial's own
-- parameters (@tail -> ConsList[$T]@), so reading it against the LITERAL @/T=⊤@ of a shape whose
-- @head@ already pins @/T@ to @/Integer@ would clamp @tail@ to @∀ConsList[⊤]@ — no clamp at all.
-- Read in the effective spelling, @Cons[/T=⊤](head=/Integer, tail=~ConsList)@ and
-- @Cons[/T=/Integer](head=/Integer, tail=∀ConsList[/T=/Integer])@ are the same set, as they
-- must be.
withEffectivePtVars :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType
withEffectivePtVars typeEnv vaenv p = p{ptVars = H.mapWithKey (ptVarEff typeEnv vaenv p) (ptVars p)}

-- | Proven cover of a shape by another: every value matching @sub@ matches @sup@.
-- Conservative towards 'False' (unproven cover), the safe direction for escape checks.
-- Mirrors 'isSubPartialOfWithEnv'. A sub-side element that is an unresolved type variable is
-- flexible - it can be bound to the super's element (as 'intersectTypesWithVarEnv' would) - so
-- it does not defeat the cover.
--
-- This is a ∀-cover, so a sup-side parameter the sub does not spell is checked against the sub's
-- ⊤ there (see 'PartialType'): @/Some@ is covered by @/Some[/T=/Nil]@ only if @⊤ ⊆ /Nil@, i.e.
-- never. Skipping it would make the two mutually cover, hence equal. 'ptArgs' compares shared
-- keys only — unspelled args are 'ptArgMode' business, decided by 'argModeCovers'.
partialCoversSat :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> SatUse -> SatHyps -> PartialType -> PartialType -> Bool
partialCoversSat typeEnv vaenv satUse hyps sup0 sub0 =
  ptName sup == ptName sub
  && argModeCovers (normalizePtArgMode typeEnv sup) (normalizePtArgMode typeEnv sub)
  && all varCovered (H.elems (H.intersectionWith (,) subVarsFull (ptVars sup)))
  && and (H.elems (H.intersectionWithKey argCovered (ptArgs sub) (ptArgs sup)))
  && supPredsCover
  where
    -- Both sides read with their spread floor ('spreadFloor') for parity with
    -- 'isSubPartialOfWithEnv', so the floor takes part in the cover like any listed field;
    -- the keyset check is mode-normalized ('normalizePtArgMode'): a vacuous interval bound
    -- must not defeat a cover (@/x..@ with no larger declared arg-set is one set with @/x@).
    sup = spreadFloor typeEnv sup0
    sub = spreadFloor typeEnv sub0
    -- A field's value set is DECLARATION-BOUNDED (the same clamp 'isSubPartialOfWithEnv'
    -- applies): when the literal cover fails, the sub value is re-read met with the field's
    -- declared type instantiated in the sub's own scope. A materialized member spelling such
    -- as @Cons[/T=Integer](head=∀Ord, ..)@ (an arm body's module-generic presentation met
    -- with a concrete call's element) denotes @head=Integer@ and IS a @ConsList[/T=Integer]@,
    -- as the partial subtype ('isSubPartialOfWithEnv') also decides.
    argCovered k tSub tSup = elemCoveredIn argEnv (tSub, tSup)
      || maybe False (\d -> elemCoveredIn argEnv (intersectTypesEnv typeEnv argEnv tSub d, tSup)) (declaredArgBound typeEnv argEnv subEff k)
    -- The bound is a template over the SUB's own parameters, instantiated in the spelling its
    -- values actually carry ('withEffectivePtVars').
    subEff = withEffectivePtVars typeEnv argEnv sub
    -- FIELDS sit inside the partials, so the parameters they bind are theirs there and never an
    -- enclosing scope's ('shadowBoundParams'); PARAMETER values sit at the containing scope and
    -- keep the incoming env.
    argEnv = shadowBoundParams typeEnv [sup0, sub0] vaenv
    -- Unspelled sub-side parameters materialize as ⊤ so every sup key is actually asked about.
    -- Under projection they stay unspelled: there a ⊤-valued parameter carries no dispatch
    -- information, which is what 'varCovered' states for the spelled-⊤ case below.
    --
    -- Each is then read DECLARATION-BOUNDED through the sub's pointer-declared args, as
    -- 'isSubPartialOfWithEnv'.'subVarEff' reads it: a field declared as the bare parameter
    -- (@item -> $T@) makes a value's @/T@ that field's value. Without it an unspelled @/T@
    -- stays ⊤ here while the materialized prover reads it as the field, and the two disagree:
    -- @Has(item=Pair(..))@ is proved inside @Has[/T=Pair](item=$T)@ there and escapes it here,
    -- so a destructuring @Has(item= Pair(a, b))@ is reported outside its declaration
    -- @m -> Opt[$T -> Pair]@ while the undeclared part is ∅.
    --
    -- The reading is 'ptVarEff' itself, shared with the materialized prover rather than copied:
    -- a field's value is read in the sub's OWN scope, so a parent reference inside it
    -- (@head=Cons[/T=$^/T](..)@) names the sub's parameter, and the two spellings of one value
    -- derive the same parameter.
    subVarsFull = H.mapWithKey (ptVarEff typeEnv vaenv sub) subVarsSpelled
    subVarsSpelled | teArgModeProjection typeEnv = ptVars sub
                   | otherwise                   = H.union (ptVars sub) (H.mapWithKey unspelledSubParam (ptVars sup))
    -- A sub-side slot holding an internal POINTER (@/head=$T@, an arg or var naming one of
    -- the partial's own parameters, 'TVInt') denotes that parameter's binding: read it
    -- through before deciding (single step, as 'compactOwnVarRefs' materializes). SUB side
    -- only — the sub is the witness VALUE, whose pointers denote its actual content (an
    -- exact reading; engine-made member shapes never pass through 'compactType', and a
    -- var-free but predicate-bounded binding is never materialized at all, leaving
    -- field-resolved and param-resolved forms of the SAME value otherwise incomparable —
    -- the 'verifySchemeActLowers' trap 'compactOwnVarRefs' documents).
    subSlot t@(TypeVar (TVVar n) TVInt _) = case H.lookup n (ptVars sub) of
      Just b | b /= PTopType, b /= t -> b
      _                              -> t
    subSlot t = t
    -- A sup-side pointer is the cover PATTERN's own parameter, and the pattern holds a value
    -- only where that parameter can take it: @bap[/M=∀Mon](fn=$M, m=$M)@ covers a value whose
    -- @fn@ result is a @Mon@, never one whose result is ⊤. Left symbolic it read as a free
    -- variable — flexible, covering every value — while 'isSubPartialOfWithEnv' reads it
    -- through the same bound ('readOwnScope'), so the two provers split: @def ⊆ arm@ was
    -- refused while @def − arm@ compacted to ∅ (unitOwnParamPointerCover). An unbound or
    -- ⊤-bound parameter stays the flexible variable it is.
    supSlot t@(TypeVar (TVVar n) TVInt i) = case H.lookup n (ptVars sup) of
      Just b | b /= PTopType, b /= t -> meetInner typeEnv vaenv b i
      _                              -> t
    supSlot t = t
    -- Under projection (matching/dispatch consumers, 'teArgModeProjection'), a Top-valued type
    -- parameter on the value side carries no information — it is equivalent to the parameter
    -- being unspelled. A definition whose element
    -- parameter could not be narrowed (e.g. a mapped element fed by a higher-order argument)
    -- completes to an explicit @[T=Top]@, and must dispatch exactly like the key-free form.
    -- The precise set algebra (projection off) keeps Top ⊄ concrete, which is why an unspelled
    -- parameter is materialized as ⊤ there rather than skipped.
    varCovered (PTopType, _) | teArgModeProjection typeEnv = True
    varCovered p             = elemCoveredIn vaenv p
    -- This is the ∀-cover primitive (every sub value within sup — the escape/coverage
    -- direction), so under the precise algebra a flexible slot is covered only by a cover
    -- slot that itself admits every binding (⊤ or another flexible variable): its values
    -- range over every binding (@Maybe[$T]@ contains @Some(Some(5))@), and treating it as
    -- covered by a CONSTRAINED slot denies escapes the engine's conservative-to-SAT polarity
    -- requires (it would make @Maybe[⊤] ⟹ Maybe[Integer]@ hold). Binding-on-demand belongs to the
    -- ∃-overlap direction ('partialsOverlapSat', the admit checks). Under projection
    -- (matching/dispatch consumers) a flexible slot on the value side behaves like the
    -- Top-valued parameter above: it binds per value and carries no dispatch information.
    elemCoveredIn env (tSub0, tSup0)
      | isFlexibleVar env tSub = tSup == PTopType || isFlexibleVar env tSup || teArgModeProjection typeEnv
      | otherwise = not (typesSatDiff typeEnv env satUse hyps tSub tSup)
      where tSub = resolveSelfRefs sub (subSlot tSub0)
            tSup = readOwnTop sup (resolveSelfRefs sup (supSlot tSup0))
    -- A parameter value's internal ('TVInt') spelling names a variable of the partial itself —
    -- 'tagParentVarRefs' spells a reference to the enclosing partial 'TVParent' — so it is never
    -- read through the ambient env.
    -- Resolve a slot's SELF-referential recursion variable through its own partial's @ptVars@. A
    -- recursive field like @tail -> ConsList[/T=$_T]@ names the partial's own parameter @/T@ but is
    -- typed EXTERNAL ('TVExt', @$_T@) rather than the internal pointer ('TVInt', @$T@) that
    -- @head -> $T@ uses, so it does not resolve through @ptVars@ automatically. When one side's
    -- element was concretized (e.g. an enumerated class member with @tail=ConsList[/T=Integer]@) and
    -- the other kept the free form, the comparison spuriously fails; resolving both through their own
    -- @ptVars@ (@substituteKeep@ leaves genuinely-free vars untouched) restores the symmetry.
    -- The partial's own bindings are the SCOPE of its slot values (what a parent reference in
    -- them names), not an ambient env: an ambient spelling there is not the partial's to resolve.
    resolveSelfRefs p t = substituteKeepScoped typeEnv ((ambientEnv $ joinVarArgEnv (ptVars p) H.empty)) H.empty t
    -- A cover PATTERN's own parameter left at ⊤ admits every binding, so a field value that names
    -- it is the field at any binding: @MkSet[/K=⊤](map=∀Map[/K=$^K])@ covers a set whose map has
    -- any key type, as its member @MkSet[/K=Z](map=∀Map[/K=$^K])@ does. A parent reference whose
    -- parameter is bound to ⊤ stays symbolic in a scoped read, and the engine took that spelling
    -- for one unknown binding the sub's @∀Map[/K=Z]@ escapes — so a class's member at a concrete
    -- parameter fell outside the class at ⊤, while the materialized prover reads the spelling as
    -- covering it. The references at the value's own level name the pattern itself, and are read
    -- detached ('detachLevelParentRefs') only when every outer reference in the value names one
    -- of the pattern's ⊤-bound parameters: any other names a binding the pattern does hold, and
    -- reading it as ⊤ would widen the pattern. That includes a parameter bound to a SELF-LINK
    -- (@/T ↦ $_T@, a recursive member's element tied to its enclosing list's): the link is the
    -- binding, and reading it as ⊤ let a @Cons[/T=Entry](.., tail=ConsList[/T=Entry])@ arm
    -- pattern be absorbed into a @tail=ConsList[/T=⊤]@ spelling.
    readOwnTop p t
      | not (S.null refs), refs `S.isSubsetOf` H.keysSet ownTop = detachLevelParentRefs t
      | otherwise = t
      where ownTop = H.filter (== PTopType) (ptVars p)
            refs = typeOuterVarRefs t
    -- FLEXIBLE means "ranges over every binding". An occurrence CARRYING KNOWLEDGE is not that:
    -- @$_b ∩ {X}@ ranges only within @X@, so calling it flexible would demand a ⊤ (or equally
    -- flexible) cover slot and refuse the true @$_b∩{X} ⊆ X@, manufacturing an escape witness
    -- for a value that cannot escape. This is the same rule as
    -- 'isSubPartialOfWithEnv'.'flexibleVar'; the two must agree or the symbolic and materialized
    -- provers contradict each other.
    isFlexibleVar _ (TypeVar _ _ i) | i /= PTopType = False
    isFlexibleVar _ (TypeVar _ TVInt _) = True -- partial-local var: bindable per value
    isFlexibleVar env (TypeVar v TVOuter _) = case outerBinding env v of
      Unbound     -> True
      SelfLink bI -> bI == PTopType -- a knowledge-carrying self-link is information (as 'flexibleVar')
      BoundTop    -> True -- bound to Top: unconstrained, so still bindable per value
      BoundVar _  -> False
      Bound _     -> False
    isFlexibleVar _ _ = False
    -- sup's own predicates must also hold of every sub value: sub ∧ ¬preds must be unsatisfiable
    supPredsCover = case ptPreds sup of
      PredsNone -> True
      preds -> case predsSatDNFNoExpr False preds of
        Nothing  -> False
        Just dnf -> not (any (\g -> satGoal typeEnv vaenv satUse hyps
                               (mergeSatGoals g (SatGoal [SatLeafs [sub{ptPreds=PredsNone}]] []))) dnf)

-- | SAT(m ∧ atom): can a value of concrete shape @m@ satisfy the atom? Conservative towards 'True'.
satAdmits :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> SatUse -> SatHyps -> PartialType -> SatAtom -> Bool
satAdmits typeEnv vaenv satUse hyps m atom = case atom of
  SatOpaque _ -> True -- opaque: may admit (stripped before the engine reads a goal, 'satGoalRaw')
  SatLeafs ls -> any (partialsOverlapSat typeEnv vaenv satUse hyps m) ls
  -- A shape named by a type already in scope has its memberships fixed by what is declared now
  -- ('knownMemberName'): an open class gains new types, never this one. So the class's current
  -- members decide whether the shape is one, as they would a sealed class's, where the open-world
  -- gate otherwise refuses to enumerate them and admits it — the meet reads @#noCore ∩ (⊤|¬Bag)@
  -- as @#noCore@, and the cover @#noCore ⊆ ⊤|¬Bag@ must agree.
  SatClass c -> case classSatCandidates typeEnv vaenv satUse c of
    Just cands -> any candAdmits cands
    Nothing | knownMemberName typeEnv (ptName m)
            , Just cands <- classSatCandidates typeEnv vaenv ProveWitness c -> any candAdmits cands
            | otherwise -> True
  SatRel r -> nameRoute || classRoute
    where
      nameRoute = relNameAdmits typeEnv r m
                  && partialsOverlapSat typeEnv vaenv satUse hyps m r{ptName = ptName m}
      classRoute = any (\c -> satAdmits typeEnv vaenv satUse hyps m (SatClass c)) (relMatchingClasses typeEnv r)
  where
    candAdmits (CandLeaf m') = partialsOverlapSat typeEnv vaenv satUse hyps m m'
    candAdmits (CandGoal g) = satGoal typeEnv vaenv satUse hyps (mergeSatGoals g (SatGoal [SatLeafs [m]] []))

-- | SAT(m ∧ ¬atom): can a value of concrete shape @m@ escape the atom? Conservative towards 'True'
-- (an escape is only denied when cover is proven). The shape is first split into its atomic
-- pieces (per union-valued var/arg component - 'splitPartialByUnionVarArgs'), since each piece
-- may be covered by a /different/ member of the atom; a cover need not be single-member.
-- A whole-shape cover ('satEscapesAtomic' on the unsplit shape) is checked first: cover of
-- the whole denies escape for every piece, and stays precise where the piece split would
-- exceed 'satDNFCap' (e.g. an applied operator whose arg slots each carry a member union) —
-- an oversized split otherwise reads as a conservative escape, which the coverage decisions
-- ('typeCoveredSymbolic') would surface as a definitive not-covered.
satEscapes :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> SatUse -> SatHyps -> PartialType -> SatAtom -> Bool
satEscapes typeEnv vaenv satUse hyps m0 atom
  | not (satEscapesAtomic typeEnv vaenv satUse hyps m0 atom) = False
  -- Splittability is decided on the shape AS WRITTEN: resolving its links can only turn a slot
  -- into a union by copying a var's value there, and a union-valued var splits the shape by
  -- itself, so this misses no split — and it keeps the resolution off the path of every
  -- union-free shape, which is most of them.
  | [_] <- splitPartialByUnionVarArgs m0 = True -- no union-valued components: the whole-shape answer is final
  | otherwise = case splitPartialByUnionVarArgs mLinked of
      [_] -> True
      -- Oversized split: conservative. Checked WITHOUT 'length' — the split is a cartesian
      -- product over the union-valued slots, so forcing its full spine to count it can
      -- itself be exponential; 'drop' materializes at most @satDNFCap + 1@ pieces.
      pieces | not (null (drop satDNFCap pieces)) -> True
      pieces -> any (\m -> satEscapesAtomic typeEnv vaenv satUse hyps m atom) pieces
  where
    -- The links are read as their VALUES before the shape is taken apart, the rule
    -- 'isSubtypeOfWithEnv'.'subLinked' states and for the same reason: a reference naming a
    -- slot this split is about to replace (@Cons[/T=X](head=Cons[/T=$^/T])@) denotes X, so the
    -- shape is @Cons[/T=X](head=Cons[/T=X])@ — every combination of an outside piece with an
    -- inside one. Split with the link left symbolic, each piece would read the reference as its
    -- OWN component (the diagonal, a proper subset): with @X = A + B@ the pieces are
    -- @Cons[/T=A](head=Cons[/T=A])@ and @Cons[/T=B](head=Cons[/T=B])@, which miss
    -- @Cons[/T=A](head=Cons[/T=B])@, so "every piece is covered" would be no cover of the shape.
    -- The shape's own var slots are the scope of its arg values; the ambient env is not consulted.
    mLinked = m0{ptArgs = fmap (substituteKeepScoped typeEnv ((ambientEnv $ joinVarArgEnv (ptVars m0) H.empty)) H.empty) (ptArgs m0)}

-- | 'satEscapes' for a shape with no union-valued vars/args.
satEscapesAtomic :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> SatUse -> SatHyps -> PartialType -> SatAtom -> Bool
satEscapesAtomic typeEnv vaenv satUse hyps m atom = case atom of
  SatOpaque _ -> False -- opaque: no escape proven (stripped before the engine reads a goal, 'satGoalRaw')
  -- A cover need not be by ONE leaf: a family of leafs that differ from the shape only at one
  -- slot covers it jointly when their values there cover the shape's ('slotUnionCovers', the
  -- rule 'isSubtypeOfWithEnv'.'coveredBySlotUnion' applies, decided here with the engine's own
  -- cover primitive). @f(x=A∪B)@ is covered by @f(x=∀A) + f(x=∀B)@ and by no one of them, and
  -- the engine must agree with the materialized prover that it does not escape.
  SatLeafs ls -> not (any (\l -> partialCoversSat typeEnv vaenv satUse hyps l m) ls
                      || slotUnionCovers typeEnv vaenv (\a b -> not (typesSatDiff typeEnv vaenv satUse hyps a b)) m ls)
  -- Negative atoms are enumerated regardless of the positive gate (parity with
  -- 'predImpliesAtom', which expands the implied class unconditionally).
  SatClass c -> case classSatCandidates typeEnv vaenv ProveWitness c of
    Nothing    -> True
    Just cands -> not (any candCovers cands)
  SatRel r -> not (nameCovered || classCovered)
    where
      nameCovered = relNameAdmits typeEnv r m
                    && partialCoversSat typeEnv vaenv satUse hyps r{ptName = ptName m} m
      classCovered = any (\c -> not (satEscapes typeEnv vaenv satUse hyps m (SatClass c))) (relMatchingClasses typeEnv r)
  where
    candCovers (CandLeaf m') = partialCoversSat typeEnv vaenv satUse hyps m' m
    -- m is covered by a predicate-described member iff m ∧ ¬member-goal is unsatisfiable
    candCovers (CandGoal g) =
      not (any (\ng -> satGoal typeEnv vaenv satUse hyps (mergeSatGoals ng (SatGoal [SatLeafs [m]] [])))
             (satGoalNegChoices g))

-- | THE SAT-GOAL MEMO. 'satGoalRaw' is a pure function of the env (identified by its
-- 'teQueryCacheKey' token plus the behavior flags a same-token env can vary in), the var
-- env, the gate, the hypothesis path, and the goal — but its candidate enumeration
-- re-derives shared subgoals per candidate, so a DAG-shaped goal space would be explored as a
-- tree, re-deciding the same subgoal under identical hypotheses once per path to it.
-- Memoizing on the full input tuple collapses the tree to the DAG; entries are exact (same
-- inputs, same pure result — hypothesis-path dependence is IN the key), and the table keeps
-- the three most-recently-used token generations like the reach-query memo in "Semantics"
-- (the static per-page env's, the epoch reaches env's, and the current merged reaches env's
-- tokens rotate).
type SatMemoKey = (Int, (Bool, Bool, Bool, Bool, Bool), [(VarKey, Type)], SatUse, [SatGoal], SatGoal)

-- | 'Nothing' marks a key whose decision is IN FLIGHT: a lookup hitting it is a derivation
-- that returned to its own goal through a helper chain the hypothesis stack does not thread
-- (the escape\/admit helpers recurse between themselves without a 'satGoal' entry). The
-- engine's cycle rule applies unchanged — a goal that recurs on its own derivation path can
-- produce no finite witness — so the re-entry answers 'False' exactly as the hypothesis
-- guard would have, had the path carried the stack. Without the marker such a cycle would
-- re-derive the same goals without end while the hypothesis depth stays fixed.
{-# NOINLINE satMemoRef #-}
satMemoRef :: IORef [(Int, H.HashMap (Hashed SatMemoKey) (Bool, ReadDeps))]
satMemoRef = unsafePerformIO (newIORef [])

-- | The keys whose decision is currently BEING computed, on each thread's evaluation stack
-- ('InFlight'). Never capped: its size is bounded by the recursion depth, and dropping a marker
-- mid-derivation would silently re-open the very cycle the marker refutes.
{-# NOINLINE satInFlightRef #-}
satInFlightRef :: InFlight (Hashed SatMemoKey)
satInFlightRef = unsafePerformIO newInFlight

-- | Is some (finite) value able to satisfy the goal? Conservative towards 'True'.
-- 'satUse' says which answer the caller relies on. Under 'ProveEmpty' a positive class
-- generator may only be enumerated when the class is sealed or 'tePrgmEnv' is set; an
-- unenumerable positive class makes the goal conservatively satisfiable.
--
-- The polarity is INHERITED by every sub-decision reached from here ('checkCand',
-- 'checkCandLeaf'), because each of those answers' FALSE direction is what produces this
-- goal's UNSAT: a sub-decision that enumerated an open class closed-world would manufacture
-- the very non-membership 'ProveEmpty' forbids, and the SAME goal would decide two ways by call
-- site. For an unsealed @Bag@, @predsEmpty (Bag ∧ Option ∧ ¬ConsList)@ must not read empty on the
-- strength of Bag's current members: a future @isa Bag@ declaration may inhabit it.
--
-- The one justified flip is in 'satEscapesAtomic', where the enumeration searches for a member
-- that COVERS the witness: a member found now really is a member, so the cover it proves cannot
-- be unmade, and a member not yet declared only costs precision (an unproven cover reads as an
-- escape, the conservative side).
satGoal :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> SatUse -> SatHyps -> SatGoal -> Bool
satGoal typeEnv vaenv satUse hyps goal = case teQueryCacheKey typeEnv of
  Nothing -> satGoalRaw typeEnv vaenv satUse hyps goal
  Just tok -> unsafePerformIO $ do
    -- Hashed once, for the lookup, the in-flight mark and the store.
    key <- evaluate (hashed (tok, (teDisableCompact typeEnv, tePrgmEnv typeEnv, teArgModeProjection typeEnv, teInSatDecision typeEnv, teDebug typeEnv), L.sortOn fst (H.toList vaenv), satUse, L.sort (S.toList hyps), goal))
    -- The value table keeps the THREE most-recently-used token generations, exactly like
    -- the reach-query memo ('queryMemoRef'): hypothesis-path dependence is IN the key, so
    -- entries are reusable ACROSS top-level decisions under the same env token — an epoch
    -- asks the same handful of goals from many top-level decisions. Token eviction bounds the
    -- table to the recent envs' distinct decisions.
    gens0 <- readIORef satMemoRef
    case lookup tok gens0 >>= H.lookup key of
      Just (r, deps) -> noteReadDeps deps >> return r
      Nothing -> do
        -- An in-flight re-entry is a cyclic derivation, which has no finite witness.
        inFlightOn satInFlightRef key (noteCycleCut False) $ do
          (r, deps) <- memoReading typeEnv $ evaluate (satGoalRaw typeEnv vaenv satUse hyps goal)
          atomicModifyIORef' satMemoRef $ \gens2 ->
            let mTok = fromMaybe H.empty (lookup tok gens2)
                rest = filter ((/= tok) . fst) gens2
            in ((tok, H.insert key (r, deps) mTok) : take 2 rest, ())
          return r

satGoalRaw :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> SatUse -> SatHyps -> SatGoal -> Bool
satGoalRaw typeEnv0 vaenv satUse hyps goal0
  -- @X ∧ ¬X@: an atom required positively and negatively at once is contradictory, whatever
  -- the atom means. The enumeration below cannot be trusted to detect this (a candidate on
  -- which BOTH the admit and the escape check are conservative reads as a witness - e.g.
  -- @Ord ∧ Eq ∧ ¬Eq@ found "satisfiable" through a parameterized class member), so decide it
  -- syntactically first — on the goal AS POSED, its opaque value-predicate atoms included
  -- ('SatOpaque'): @P ∧ ¬P@ is empty whatever @P@ means. Everything below reads the goal without
  -- them: the engine knows nothing of a value predicate, and a goal weakened by dropping one is
  -- unsatisfiable only where the goal is, the direction the engine's answer is relied on for.
  | any (`elem` sgNeg goal0) (sgPos goal0) = False
  | goal `S.member` hyps = False -- recursive goal: no finite witness along this path
  | any (`elem` sgNeg goal) (sgPos goal) = False
  -- A positive atom implying a negative one by declaration (@Fish ∧ ¬Animal@ under
  -- @every(Fish isa Animal)@) is contradictory in the open world, independent of the gate.
  | or [satAtomImpliesDeclared typeEnv vaenv p n | p <- sgPos goal, n <- sgNeg goal] = False
  -- A negative atom that ALREADY denotes the whole universe (⊤) makes the goal UNSAT whatever the
  -- positive constraints are: the value must AVOID that negative, but ⊤ leaves nowhere to go. The
  -- canonical case is a sealed class with a bare type-variable member (@Option = [$_T, None]@): the
  -- @$_T@ member admits EVERY value, enumerating as the trivially-satisfiable candidate
  -- @SatGoal [] []@, so @∀Option ≡ ⊤@ and no value is @¬Option@. This decides both @⊤ ⊆ ∀Option@
  -- (empty positive) and @∀Bag ⊆ ∀Option@ (positive @Bag@ that the open-world gate cannot itself
  -- enumerate) — without it the generator path reads the goal as conservatively satisfiable and
  -- reports the subtype False, breaking @a ⊆ b ⟺ a∪b = b@. Only a PROVABLY universal negative flips
  -- to UNSAT; a proper (sub-⊤) negative leaves an escape route, so the conservative-to-SAT default
  -- stands. Non-recursive (a nested universal-class member reads conservatively as an escape) —
  -- sound, since the miss is toward SAT.
  | any negAtomCoversUniverse (sgNeg goal) = False
  | otherwise = case sgPos goal of
      [] -> True -- no positive constraint: the open universe escapes any finite proper negatives
      _  -> case chosenGen of
        Nothing         -> True -- no enumerable generator: conservative
        Just (_, cands) -> any checkCand cands
  where
    -- The decision root marks its entire body ('teInSatDecision'); no answer depends on the
    -- mark. Internal engine recursion stays hypothesis-guarded, and a nested decision that
    -- re-poses a goal still being decided is stopped by the in-flight marker in 'satGoal'.
    typeEnv = typeEnv0{teInSatDecision=True}
    goal = satGoalUnifyPos vaenv (satGoalNorm (satGoalStripOpaque goal0))
    hyps' = S.insert goal hyps

    negAtomCoversUniverse (SatClass c) = maybe False (any candUniversal) (classSatCandidates typeEnv vaenv satUse c)
    -- A relation negative is universal through its CLASS route (@¬~Option@ is the @isa@
    -- spelling of the same universal class; @hasClass C ⟹ isa C@ per
    -- 'satAtomImpliesDeclared') — parity with 'satAdmits'/'satEscapesAtomic', which consult
    -- 'relMatchingClasses' for both polarities, so @⊤|¬(PredRel Option)@ and
    -- @⊤|¬(PredClass Option)@ are both empty.
    negAtomCoversUniverse (SatRel r)   = any (negAtomCoversUniverse . SatClass) (relMatchingClasses typeEnv r)
    negAtomCoversUniverse _            = False
    candUniversal (CandGoal (SatGoal [] [])) = True
    candUniversal _                          = False

    -- Prefer the most concrete generator: concrete leafs, then a class, then a relation. Among
    -- those, take the first that can actually ENUMERATE: a class the caller's polarity forbids
    -- enumerating yields no candidates, so a co-present enumerable atom generates instead (e.g.
    -- the SEALED @ConsList@ deciding @Monad[Integer] ∧ ConsList ∧ ¬DShow@ that the OPEN @Monad@
    -- generator only reports conservatively satisfiable). Skipping is sound because the skipped
    -- atom still reaches 'satAdmits' in 'checkCandLeaf' UNDER THE SAME POLARITY, which under
    -- 'ProveEmpty' refuses the same enumeration and answers "admits" conservatively — so the
    -- witness survives and the goal reads satisfiable. It is NOT sound to claim the skipped atom
    -- is thereby ENFORCED: enforcing it would mean concluding non-membership from an open class's
    -- current members, which is exactly what 'ProveEmpty' forbids.
    orderedAtoms = let (leafs, nonLeafs) = L.partition isLeafs (sgPos goal)
                       (classes, rels)   = L.partition isClass nonLeafs
                   in leafs ++ classes ++ rels
    chosenGen = listToMaybe [(a, cands) | a <- orderedAtoms, Just cands <- [genCandidates a]]
    restPos = case chosenGen of
      Just (g, _) -> L.delete g (sgPos goal)
      Nothing     -> sgPos goal
    isLeafs SatLeafs{} = True
    isLeafs _          = False
    isClass SatClass{} = True
    isClass _          = False

    -- Witness candidates are interpreted under their data declarations ('satValueShape'): the
    -- escape checks below must reason over real values, not the leaf's face-value ⊤ fields.
    -- An arg-set INTERVAL leaf (e.g. @/Cons..@) is the union of its exact arg-set pieces
    -- ('ptIntervalArgKeys', declaration-bounded), and each piece may be covered by a DIFFERENT
    -- negative (@/Cons(head,tail)@ by a class, the partial-application pieces by leafs) - a
    -- joint cover the atomic leaf hides. Decompose into the pieces, mirroring the
    -- 'coveredByArgSets' decomposition of the positive-partial subtype.
    genCandidates (SatLeafs ls) = Just (map (CandLeaf . satValueShape typeEnv) (concatMap intervalPieces ls))
      where
        intervalPieces l = case ptIntervalArgKeys typeEnv lN of
          Just kss@(_:_:_) -> map (ptAtArgKeys lN) kss
          _                -> [lN]
          where lN = normalizePtArgMode typeEnv l
    genCandidates (SatClass c)  = classSatCandidates typeEnv vaenv satUse c
    genCandidates (SatOpaque _)   = Nothing -- opaque: not enumerable (stripped before the engine reads a goal)
    -- A relation's type-route candidates come from the name universe ('scTypeNames'); an env
    -- without one (e.g. the reduced env of the class-graph 'Semigroup' merge) makes the
    -- enumeration silently incomplete - a goal with no candidates would read as refuted when
    -- the engine merely knows nothing. Treat it as unenumerable (conservatively satisfiable).
    genCandidates (SatRel r)
      | null (scTypeNames (teSatCache typeEnv)) = Nothing
      | otherwise = relSatCandidates typeEnv vaenv satUse r

    rest = SatGoal restPos (sgNeg goal)

    checkCand (CandGoal g) = satGoal typeEnv vaenv satUse hyps' (mergeSatGoals g rest)
    checkCand (CandLeaf m)
      | ptPreds m /= PredsNone = case predsSatDNF True (ptPreds m) of
          Nothing  -> True -- member predicates outside the fragment: conservative
          Just dnf -> any (\g -> let g' = mergeSatGoals g rest
                                  in not (goalContradictory g') && checkCandLeaf g' m{ptPreds=PredsNone}) dnf
      | otherwise = checkCandLeaf rest m
    -- The contradiction rules the goal itself was checked against at entry, applied to a
    -- goal the witness's OWN predicates were merged into: a witness carrying @X@ against a
    -- negative @¬X@ (or a declared consequence of @X@) has no value, whatever the per-atom
    -- admit\/escape checks can decide.
    goalContradictory g = any (`elem` sgNeg g) (sgPos g)
      || or [satAtomImpliesDeclared typeEnv vaenv p n | p <- sgPos g, n <- sgNeg g]
    -- The admit and escape checks INHERIT 'satUse': both answers' FALSE direction feeds this
    -- goal's UNSAT, so under 'ProveEmpty' neither may enumerate an open class closed-world one
    -- level down. The cover check of a witness's slot ('partialCoversSat' → 'typesSatDiff')
    -- would otherwise prove @∀ConsList[/T=∀Ord] ⊆ ∀Eq@ from @Ord@'s current members while
    -- @∀Ord ⊆ ∀Eq@ itself stays unproven.
    --
    -- A candidate that denotes ∅ IS NO WITNESS: a shape with an empty slot value, or a field
    -- outside its declared bound, has no value to offer ('partialUninhabited' — the same rule
    -- 'isSubtypeOfWithEnv'.'isSubPartial' applies on the materialized side, and the same one
    -- 'compactType' applies when it deletes such a leaf). Otherwise the two provers would split
    -- on exactly the leaf the compaction deletes: @Some[/T=$_b∩{X}](value=$/T)@ with @$_b@ bound
    -- to a type disjoint from @X@ has an empty element and compacts to ∅, so a union containing
    -- it must read as covered wherever its other leafs are.
    checkCandLeaf r m = case foldM refineByPos m (sgPos r) of
        Nothing -> False
        Just mR -> not (partialUninhabited typeEnv vaenv mR)
                   && all (satAdmits typeEnv vaenv satUse hyps' mR) (sgPos r)
                   && all (satEscapes typeEnv vaenv satUse hyps' mR) (sgNeg r)
      where
        -- Refine the witness by each co-occurring positive atom it must ALSO satisfy: meeting a
        -- @Cons@ witness with @Monad[Integer]@'s same-named member @Cons[Integer]@ pins its otherwise
        -- free element before the escape checks. Without this a @Cons[$_T]@ generated from @ConsList@
        -- spuriously escapes @DShow@ though every value satisfying @Monad[Integer] ∧ ConsList@ has
        -- element Integer (a @ToString@). Only refine when the meet is a SINGLE leaf of the witness's
        -- own name — an element-preserving pinning, never an open-world emptiness conclusion (an
        -- unrelated class\/leaf yields ∅ or a multi-leaf union, which is left as the witness for
        -- 'satAdmits' to reject). Co-occurring positive LEAF atoms need the same meet: the generator
        -- enumerates a witness from ONE leaf atom while 'satAdmits' only checks OVERLAP against the
        -- others, so a witness broader than the true joint value (e.g. a @fluffy(this=Animal)@ chosen
        -- for @fluffy(this=Fish) ∧ fluffy(this=Animal)@) would escape a negative the refined
        -- (@this=Fish@) value cannot — a false SAT that breaks @A ⊆ A∪B@ (covariance).
        --
        -- A meet with a LEAF atom, or with a relation's shape by its name route, is between
        -- concrete leafs of the witness's own name, so when it is ∅ no value satisfies both and the
        -- candidate is no witness ('Nothing'). Only the SLOTS would otherwise be checked, one at a
        -- time: @Cons[/T=⊤](tail=X)@ overlaps @Cons[/T=K]@ on @/T@, yet a tail outside
        -- @ConsList[K]@ has no value in @Cons[/T=K]@, whose declared tail is bounded by its @/T@.
        -- A CLASS expansion is the class's current members, which the open world may extend, so
        -- its empty meet concludes nothing and leaves the witness for 'satAdmits'.
        --
        -- Witness refinement only ever accepts a single-leaf meet named @ptName acc@
        -- ('refineMeet'), so the class expansion is restricted to that one name — the
        -- dropped members could only meet to ∅.
        refineByPos acc (SatClass c) = Just (refineMeet acc (expandClassPartialWithinNames typeEnv vaenv (S.singleton (ptName acc)) c))
        refineByPos acc (SatLeafs ls) = refineMeetExact acc (UnionType Nothing (joinUnionType ls) [])
        refineByPos acc (SatOpaque _)   = Just acc -- opaque: refines nothing
        -- A relation is satisfied through its name route OR a matching class (a disjunction),
        -- so the witness may only be refined when a SINGLE route is available: name route
        -- only (refine by the relation's shape at the witness's name — @~Cons[T=Eq](…)@
        -- pins the witness's slots) or exactly one matching class with the name route
        -- inapplicable. Refining through a live disjunction would over-constrain the
        -- witness towards a false UNSAT.
        refineByPos acc (SatRel r') = case (relNameAdmits typeEnv r' acc, relMatchingClasses typeEnv r') of
          (True, [])   -> refineMeetExact acc (singletonType r'{ptName = ptName acc})
          (False, [c]) -> Just (refineMeet acc (expandClassPartialWithinNames typeEnv vaenv (S.singleton (ptName acc)) c))
          _            -> Just acc
        refineMeet acc other = fromMaybe acc (refineMeetExact acc other)
        refineMeetExact acc other =
          case snd $ intersectTypesWithVarEnv typeEnv vaenv (singletonType acc) other of
            met | isBottomType met -> Nothing
            UnionType Nothing leafs [] | [acc'] <- splitUnionType leafs, ptName acc' == ptName acc -> Just acc'
            _ -> Just acc

-- | Engine-backed subtype: @Just (t1 ⊆ t2)@, decided as emptiness of @t1 ∧ ¬t2@, when both
-- types are within the decidable fragment. 'satUse' gates enumeration of t1's classes
-- (the open-world coverage gate).
typeCoveredSymbolic :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> SatUse -> Type -> Type -> Maybe Bool
typeCoveredSymbolic typeEnv vaenv satUse t1 t2 = do
  d1 <- typeSatDNF typeEnv vaenv t1
  d2 <- typeSatDNF typeEnv vaenv t2
  negConj <- satDNFConj (map satGoalNegChoices d2)
  goals <- satDNFConj [d1, negConj]
  -- Covered when every escape goal is refuted; an escape the engine can vouch for refutes the
  -- cover; an escape resting on an opaque value predicate decides nothing ('satGoalsDecided').
  not <$> satGoalsDecided typeEnv vaenv satUse goals

-- | Engine-backed implication between two class/relation atoms: @a1 ⟹ a2@ decided as emptiness
-- of @a1 ∧ ¬a2@. 'satUse' gates enumeration of a1's class (the open-world gate).
-- A memberless (but enumerable) class on the left implies anything vacuously - parity with
-- the enumerate-members check, which is vacuously true on an empty member list (the class
-- placeholder inhabitant is a satisfiability device, not a counterexample to implication).
satAtomImplies :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> SatUse -> SatAtom -> SatAtom -> Bool
satAtomImplies typeEnv vaenv satUse a1 a2
  | SatClass c1 <- a1
  , Just ClassSatInfo{csiSealed=sealed, csiTypes=[]} <- H.lookup (ptName c1) (scClasses (teSatCache typeEnv))
  , sealed || tePrgmEnv typeEnv = True
  -- In-flight decision: only the engine-free declared-edge check ('satAtomImpliesDeclared'
  -- is recursion-safe by construction), else implication unproven (see 'teInSatDecision').
  | otherwise = not (satGoal typeEnv vaenv satUse S.empty (SatGoal [a1] [a2]))
