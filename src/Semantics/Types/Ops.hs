-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.Ops
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Convenience entry points to the type algebra that run in a default environment or combine
-- several operations.
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Semantics.Types.Ops where

import           Data.Bifunctor                  (Bifunctor (first))
import qualified Data.HashMap.Strict             as H
import qualified Data.HashSet                    as S
import qualified Data.List                       as L
import           Data.Maybe
import           Debug.Trace                     (trace)
import           Semantics.Types.Algebra.Compact
import           Semantics.Types.Algebra.Expand
import           Semantics.Types.Algebra.Preds
import           Semantics.Types.Algebra.Sat
import           Semantics.Types.Algebra.SetOps
import           Semantics.Types.Algebra.Subst
import           Semantics.Types.Algebra.Subtype
import           Semantics.Types.Construct
import           Semantics.Types.Data
import           Semantics.Types.Name
import           Semantics.Types.SatGoal
import           Semantics.Types.Vars
import           Text.Printf


-- | Narrow each partial's present fields to their /concrete/ declared type. A destructure pattern
-- (@#md(text)@) leaves its field's actual at Top; the declaration @#md(text -> String)@ says the
-- field IS a @String@, so an LHS input actually is one. This adds no missing field: it tightens
-- an /already-present/ field to its declared type. Only a 'typeFullyConcrete' declared field type is carried — a parameter-typed field
-- (@v -> $A@, or a still-parameterized @tail -> ConsList[$T]@) is left as inference has it, so no
-- free type parameter is reintroduced. Recurses into nested args/vars.
completeConcreteFields :: (TypeGraph tg) => TypeEnv tg -> Type -> Type
completeConcreteFields typeEnv = onUnion
  where
    onUnion (UnionType mPreds leafs consts) = UnionType (fmap (mapTopNegLeafs onLeafs) mPreds) (onLeafs leafs) consts
    onUnion t = t
    onLeafs = joinUnionType . map onPartial . splitUnionType
    onPartial p@PartialType{ptName, ptArgs, ptVars} =
      let recArgs = fmap onUnion ptArgs
          p1 = p{ptArgs = recArgs, ptVars = fmap onUnion ptVars}
      in case [dArgs | PartialType{ptArgs=dArgs} <- typeGraphData typeEnv ptName, not (H.null dArgs)] of
           (dArgs:_) -> p1{ptArgs = H.mapWithKey (narrowField dArgs) recArgs}
           []        -> p1
    narrowField dArgs k v = case H.lookup k dArgs of
      Just dt | typeFullyConcrete dt -> intersectTypesEnv typeEnv H.empty v dt
      _                              -> v

-- | 'expandPredicates' under the restricted-intersection-operand contract of
-- 'expandClassPartialWithinNames': only POSITIVE class\/relation atoms are name-restricted.
-- A 'PredsNot' (or any atom under it) takes the full unrestricted path — restricting a
-- complemented expansion WIDENS the result, which is unsound in every consumer — and a
-- 'PredExpr' has no name-local reading, so it keeps the full path too.
expandPredicatesWithinNames :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> S.HashSet TypeName -> TypePredicates -> Type
expandPredicatesWithinNames _       _     _  PredsNone    = PTopType
expandPredicatesWithinNames typeEnv vaenv ns (PredsOne (PredClass clss)) = expandClassPartialWithinNames typeEnv vaenv ns clss
expandPredicatesWithinNames typeEnv vaenv ns (PredsOne (PredRel rel))    = expandRelPartialWithinNames typeEnv vaenv ns rel
expandPredicatesWithinNames typeEnv vaenv _  p@(PredsOne PredExpr{})     = expandPredicates typeEnv vaenv p
expandPredicatesWithinNames typeEnv vaenv _  p@(PredsOne PredConstEq{})  = expandPredicates typeEnv vaenv p
expandPredicatesWithinNames typeEnv vaenv ns (PredsAnd ps) = case map (expandPredicatesWithinNames typeEnv vaenv ns) ps of
  [] -> BottomType
  ts -> foldr1 (intersectTypesEnv typeEnv vaenv) ts
expandPredicatesWithinNames typeEnv vaenv _  p@PredsNot{}  = expandPredicates typeEnv vaenv p

expandClassPartialWithNamesFull :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> S.HashSet TypeName -> (PartialLeafs, Bool)
expandClassPartialWithNamesFull = expandClassPartialWithNamesFullAux S.empty

-- | SAT(m ∧ ¬n₁ ∧ … ∧ ¬nₖ): can a single value of shape @m@ escape ALL of @negs@ jointly?
-- The joint-cover decision needed by dispatch coverage ('completeTreeSet' in "TreeBuild"):
-- options that each match only part of a class-\/union-typed position can still jointly
-- cover it (e.g. @Nil@ and @Cons@ arms covering a @~ConsList@ argument). 'satEscapes'
-- checks each negative independently and cannot see such joint covers; this instead
-- refines @m@ by enumerating one discriminating top-level var\/arg position at a time
-- (union components, class members, relation candidates) and requiring every case to be
-- covered. Recursion through the class graph terminates via the goal stack exactly as in
-- 'satGoal': a repeated goal has no finite witness, so it cannot escape. Conservative
-- towards 'True' (escape unproven ⇒ possibly escapes), the safe polarity for coverage
-- (unproven cover ⇒ not covered).
satLeafEscapesAll :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> SatHyps -> PartialType -> [PartialType] -> Bool
satLeafEscapesAll typeEnv0 vaenv hyps m negs0
  | goal `S.member` hyps = False -- recursive goal: no finite witness along this path
  | any (\n -> partialCoversSat typeEnv vaenv ProveWitness hyps' n m) negs = False
  | null negs = satGoal typeEnv vaenv ProveWitness hyps (SatGoal [SatLeafs [m]] []) -- inhabited ⇒ escapes
  | otherwise = case refinements of
      []        -> True -- no discriminating position is refinable: conservative escape
      (cases:_) -> any (\m' -> satLeafEscapesAll typeEnv vaenv hyps' m' negs) cases
  where
    -- Decision root: mark the entire body, exactly as in 'satGoal'. This root has its own
    -- algebra re-entries that never pass through 'satGoal' ('classSatCandidates'' class-var
    -- meet in 'enumAtom', and through it the 'compactType' → 'typeGraphQueryWithReason' →
    -- 'mostSpecificPromises' route back to a fresh 'satLeafEscapesAll').
    typeEnv = typeEnv0{teInSatDecision=True}
    negs = filter (partialsOverlapSat typeEnv vaenv ProveWitness hyps m) negs0
    goal = satGoalNorm (SatGoal [SatLeafs [m]] [SatLeafs [n] | n <- negs])
    hyps' = S.insert goal hyps

    -- Refinement at the first discriminating position: a var\/arg of m that some negative
    -- genuinely constrains (m's element there can escape the negative's), enumerated into
    -- exhaustive cases. An empty case list means the position (hence m) is uninhabited.
    refinements = mapMaybe refineAt (map (first TVVar) (H.toList (ptVars m)) ++ map (first TVArg) (H.toList (ptArgs m)))
    refineAt (k, tk)
      | any constrains negs = do
          cases <- enumTypeCases tk
          Just [typeSetAux k c m | c <- cases]
      | otherwise = Nothing
      where
        constrains n = case elemAt k n of
          Just nk -> typesSatDiff typeEnv vaenv ProveWitness hyps' tk nk
          Nothing -> False
    elemAt (TVVar k) PartialType{ptVars} = H.lookup k ptVars
    elemAt (TVArg k) PartialType{ptArgs} = H.lookup k ptArgs
    elemAt (TVApplied k _) _ = error $ printf "Unexpected TVApplied %s used as a position in elemAt" (show k)

    -- Exhaustive top-level cases of a position's type: union components, class members
    -- (via 'classSatCandidates', gated closed-world like the outer coverage decisions), or
    -- relation candidates. 'Nothing' when not enumerable (then another position may be).
    -- Subtracted 'negLeafs' and non-generator conjuncts are dropped, which only ENLARGES
    -- the enumerated set - sound for cover proofs, conservative for escapes.
    enumTypeCases (UnionType Nothing leafs []) = case splitUnionType leafs of
      ls@(_:_:_) -> Just (map singletonType ls)
      _          -> Nothing -- single leaf: no top-level refinement available
    enumTypeCases (UnionType (Just (preds, _, [])) pos []) = do
      dnf <- predsSatDNF True preds
      satPos <- case dnf of
        [SatGoal ps _] -> Just ps
        _              -> Nothing
      topCases <- enumAtomCases satPos
      Just (topCases ++ map singletonType (splitUnionType pos))
    enumTypeCases _ = Nothing

    -- Enumerate the first enumerable atom of a positive conjunction, preferring a class
    -- (the most precise generator); remaining conjuncts are dropped (enlarging).
    enumAtomCases atoms = listToMaybe (mapMaybe enumAtom (L.sortOn atomPref atoms))
    atomPref SatClass{}  = 0 :: Int
    atomPref SatRel{}    = 1
    atomPref SatLeafs{}  = 2
    atomPref SatOpaque{} = 3
    enumAtom (SatClass c) = classSatCandidates typeEnv vaenv ProveEmpty c >>= mapM candCase
    enumAtom (SatRel r)   = relSatCandidates typeEnv vaenv ProveEmpty r >>= mapM candCase
    enumAtom (SatLeafs _) = Nothing -- cannot appear in a preds DNF
    enumAtom (SatOpaque _)  = Nothing -- opaque: not enumerable
    candCase (CandLeaf tp) = Just (singletonType tp)
    candCase (CandGoal (SatGoal [a] [])) = case a of
      SatClass c  -> Just (classPartial c)
      SatRel r    -> Just (TopType H.empty (PredsOne (PredRel r)))
      SatLeafs ls -> Just (UnionType Nothing (joinUnionType ls) [])
      SatOpaque q -> Just (TopType H.empty (PredsOne q))
    candCase CandGoal{} = Nothing -- a compound member goal: not expressible as a case type

-- | Engine-backed completion check of 'completeTreeSet': every value of @full@ is matched
-- by some accumulated option. covered ⟺ UNSAT(full ∧ ¬opt₁ ∧ … ∧ ¬optₖ).
dispatchCoveredSymbolic :: TypeGraph tg => TypeEnv tg -> PartialType -> [PartialType] -> Bool
dispatchCoveredSymbolic typeEnv full opts = not (satLeafEscapesAll typeEnv H.empty S.empty full opts)

-- | Engine-backed accumulation check of 'completeTreeSet': @opt@ matches some value of
-- @full@ that no accumulated option matches yet. SAT(full ∧ opt ∧ ¬acc₁ ∧ … ∧ ¬accₖ).
dispatchAddsSymbolic :: TypeGraph tg => TypeEnv tg -> PartialType -> PartialType -> [PartialType] -> Bool
dispatchAddsSymbolic typeEnv full opt accOpts =
  case intersectTypes typeEnv (singletonType full) (singletonType opt) of
    UnionType Nothing leafs [] -> any (\l -> satLeafEscapesAll typeEnv H.empty S.empty l accOpts) (splitUnionType leafs)
    _ -> True -- out-of-fragment intersection: conservatively adds

-- | Checks if one type contains another type. In set terminology, it is equivalent to subset or equal to ⊆.
isSubtypeOf :: TypeGraph tg => TypeEnv tg -> Type -> Type -> Bool
isSubtypeOf typeEnv = isSubtypeOfWithEnv typeEnv H.empty

-- | Checks if one type contains another type. In set terminology, it is equivalent to subset or equal to ⊆.
isSubtypePartialOf :: TypeGraph tg => TypeEnv tg -> PartialType -> Type -> Bool
isSubtypePartialOf typeEnv subPartial = isSubtypeOf typeEnv (singletonType subPartial)

isEqType :: TypeGraph tg => TypeEnv tg -> Type -> Type -> Bool
isEqType TypeEnv{teDebug=True} a b | trace (printf "[EQ] %s = %s" (show a) (show b)) False = undefined
isEqType _ a b | a == b = True
isEqType typeEnv a b = isSubtypeOf typeEnv a b && isSubtypeOf typeEnv b a

-- | Set-semantic equality via 'isSubtypeOfWithEnvComplete' — the specification-level
-- relation, complete for the partition covers the plain checker cannot see. NOT for use
-- inside the algebra's own meets/compaction (the split's meets would re-enter it).
isEqTypeWithEnvComplete :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Bool
isEqTypeWithEnvComplete _ _ a b | a == b = True
isEqTypeWithEnvComplete typeEnv vaenv a b = isSubtypeOfWithEnvComplete typeEnv vaenv a b && isSubtypeOfWithEnvComplete typeEnv vaenv b a

unionTypes :: TypeGraph tg => TypeEnv tg -> Type -> Type -> Type
unionTypes typeEnv = unionTypesWithEnv typeEnv H.empty

-- | Takes the 'intersectTypes' of many types
intersectAllTypes :: (TypeGraph tg, Foldable f) => TypeEnv tg -> f Type -> Type
intersectAllTypes _ types | null types = BottomType
intersectAllTypes typeEnv types = foldr1 (intersectTypes typeEnv) types

-- | Takes the intersection of two 'Type' (∩).
intersectTypes :: (TypeGraph tg) => TypeEnv tg -> Type -> Type -> Type
intersectTypes typeEnv = intersectTypesEnv typeEnv H.empty

-- | The env-dropping difference: the same resolve-the-meet-back reading as 'intersectTypesEnv'
-- (@$a − X@ discovers @$a ↦ ¬X@, which the standalone result must carry rather than the bare var).
differenceTypeEnv :: (TypeGraph tg) => TypeEnv tg -> Type -> Type -> Type
differenceTypeEnv typeEnv t1 t2 = intersectTypesEnv typeEnv H.empty t1 (complementTypeEnv typeEnv H.empty t2)

-- | Takes the powerset of a 'Type' with the powerset of the arguments in the type.
powersetType :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
powersetType _ _ PTopType = PTopType
-- A predicate universe stays symbolic when its class is open, so it has no member leafs to take
-- the arg-subsets of. The powerset is only ever used as an UPPER bound (the arg-mode transform
-- is met into the destination), so ⊤ is the sound reading of that component.
powersetType _ _ (UnionType (Just _) _ _) = PTopType
powersetType _ _ t@(TypeVar _ _ _) = t
powersetType typeEnv vaenv (UnionType Nothing partials []) =
  compactType typeEnv vaenv $ UnionType Nothing partials' []
  where
    -- Every subset of the args may be present: that is exactly the arg-mode interval
    -- [∅, args] — lower-unbounded (no arg required), with the source's upper bound kept
    -- (an upper-unbounded source still permits extra args; an upper-bounded one caps at
    -- @args@). Representing the powerset as this single interval partial avoids enumerating
    -- the 2^|args| subsets, which otherwise bloats the union and every compaction over it.
    partials' = joinUnionType $ map asInterval $ splitUnionType partials
    asInterval (PartialType name vars args _ (PtArgMode _ ub _)) = PartialType name vars args PredsNone (PtArgMode False ub S.empty)
powersetType _ _ t = error $ printf "powersetType: unexpected case %s" (show t)

-- | 'substituteTVExt' over one partial. SCOPES NEST: the partial's own (substituted) bindings
-- SHADOW the caller's inside its args and predicates — a @$/T@ there resolves to the sibling
-- parameter, while a reference the partial does not bind (@right=∀Tree[/K=$^/MK]@ inside a
-- @Node[/K, /V]@, the enclosing declaration's parameter) resolves to what the caller supplied,
-- exactly as the algebra reads it ('comparedPartialsEnv'). Dropping the caller's bindings at the
-- arg boundary instead would erase every such reference to ⊤ (an env miss reads the occurrence as
-- its inner), so a value spelled with its declaration's parameters would lose them at every call
-- site.
substitutePartialTVExt :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType
substitutePartialTVExt typeEnv = substitutePartialTVExtScoped typeEnv H.empty

-- | Deep-substitute the given FOREIGN frame bindings throughout a type: every FREE reference
-- to a bound name is replaced wherever it occurs — nested arg slot types and class-predicate
-- dims included. Which references are free is the loc's business ('substituteFreeDeepScoped'):
-- an ambient one always, a parent one never, an internal one unless the partial in view binds
-- it. Unlike 'substituteKeep' the walk carries NO own-parameter resolution into the descent: a
-- recursive member template's self-pointer (@tail=∀ConsList[/T=$/T]@) sits under its own
-- @/T@ binder and stays symbolic, so the substitution cannot unroll a recursive spelling.
-- Callers pass CLOSED (var-free) bindings only — see 'typeHasTypeVars'.
--
-- This is the frame-closure step for content ADOPTED into a stored ACTUAL from a
-- requirement spelled in another frame (the discovered-binding resolve in
-- 'arrowConstrainUbs'; the ACT channel is frame-local): an enclosing call's @$/MK@ inside an
-- adopted @Node[..](left=∀Tree[/K=$/MK], ..)@ must materialize while the discovering meet's
-- bindings are still in hand — once stored past the constraint the frame is gone, and the
-- reference would name nothing any value can satisfy.
substituteFreeDeep :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
substituteFreeDeep typeEnv = substituteFreeDeepScoped typeEnv H.empty

-- | 'substituteFreeDeep' with the SCOPE in view beside the free bindings. The scope is the
-- partial the position sits one level inside (its own parameter names; the values are not
-- consulted here) and it decides which references are the partial's own rather than free:
--
--   * an ambient reference ('TVExt') is never a partial's own — it substitutes wherever the
--     free bindings name it, even under a like-named parameter;
--   * a parent reference ('TVParent') names the scope partial's parameter, so it is never free
--     and always stays a pointer;
--   * an internal reference ('TVInt') is the partial's own where the scope binds it (@/head=$T@
--     under a partial declaring @/T@) and free otherwise — an encoder-spelled declaration-frame
--     pointer (@∀Tree[/K=$/MK]@ under a function's frame binding @/MK@) is exactly what this
--     closure exists to materialize.
--
-- The top-level scope is EMPTY: a reference not bound by any partial in view is free by
-- definition, so unlike the keep walker this one does not seed the scope with the ambient env.
-- Descent REPLACES the scope with the partial's own names (its 'ptVars' values sit at the
-- containing level and keep the scope they arrived with).
substituteFreeDeepScoped :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypeVarArgEnv -> Type -> Type
substituteFreeDeepScoped _typeEnv _ env t | H.null env = t
substituteFreeDeepScoped typeEnv scope env (TypeVar (TVApplied b overrides) loc i) =
  applyTypeVarOverrides typeEnv (fmap (substituteFreeDeepScoped typeEnv scope env) overrides) (substituteFreeDeepScoped typeEnv scope env (TypeVar b loc i))
substituteFreeDeepScoped typeEnv scope env (TypeVar v loc@TVExt i) = substituteFreeLeaf typeEnv scope env v loc i True
substituteFreeDeepScoped typeEnv scope env (TypeVar v loc@TVParent i) = substituteFreeLeaf typeEnv scope env v loc i False
substituteFreeDeepScoped typeEnv scope env (TypeVar v loc@TVInt i) = substituteFreeLeaf typeEnv scope env v loc i (isNothing (envLookupAt scope TVInt v))
substituteFreeDeepScoped typeEnv scope env (UnionType mtop leafs consts) =
  UnionType (fmap goTop mtop) (joinUnionType $ map (goPartial scope) $ splitUnionType leafs) consts
  where
    goTop (preds, negLeafs, ncs) =
      (mapTypePreds (mapTypePred (goPartial scope)) preds, joinUnionType $ map (goPartial scope) $ splitUnionType negLeafs, ncs)
    goPartial sc p@PartialType{ptVars, ptArgs, ptPreds} = p{
        ptVars  = fmap (substituteFreeDeepScoped typeEnv sc env) ptVars,
        ptArgs  = fmap (substituteFreeDeepScoped typeEnv innerScope env) ptArgs,
        ptPreds = mapTypePreds (mapTypePred (goPartial innerScope)) ptPreds
        }
      where innerScope = ambientEnv (joinVarArgEnv ptVars H.empty)

-- | The leaf rule of 'substituteFreeDeepScoped': a FREE reference the bindings name is
-- replaced (@binding ∩ inner@); any other keeps its (substituted) inner along with the reference.
substituteFreeLeaf :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypeVarArgEnv -> TypeVarAux -> TypeVarLoc -> Type -> Bool -> Type
substituteFreeLeaf typeEnv scope env v loc i free = case envLookupAt env loc v of
  Just b | free, b /= PTopType -> meetInner typeEnv env b i'
  _                            -> TypeVar v loc i'
  where i' = substituteFreeDeepScoped typeEnv scope env i
