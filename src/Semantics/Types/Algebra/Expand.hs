-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.Algebra.Expand
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Class and relative-name expansion: reading a class or relative partial as the union of its
-- members.
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Semantics.Types.Algebra.Expand where

import                          Control.Exception               (evaluate)
import                          Data.Hashable                   (Hashed, hashed)
import                qualified Data.HashMap.Strict             as H
import                qualified Data.HashSet                    as S
import                          Data.IORef                      (IORef,
                                                                 atomicModifyIORef',
                                                                 newIORef,
                                                                 readIORef)
import                qualified Data.List                       as L
import                          Data.Maybe
import {-# SOURCE #-}           Semantics.Types.Algebra.SetOps  (differencePartialLeafs,
                                                                 intersectPartialLeafsWithVarEnv,
                                                                 intersectTypesEnv,
                                                                 unionAllTypesWithEnv,
                                                                 unionAllTypesWithEnvOnce)
import                          Semantics.Types.Algebra.Subst
import {-# SOURCE #-}           Semantics.Types.Algebra.Subtype (isSubtypeOfWithEnv)
import                          Semantics.Types.ArgModes
import                          Semantics.Types.Construct
import                          Semantics.Types.Data
import                          Semantics.Types.Memo
import                          Semantics.Types.Name
import                          Semantics.Types.Preds
import                          Semantics.Types.Vars
import                          System.IO.Unsafe                (unsafePerformIO)
import                          Text.Printf
import                          Utils


-- | Returns only the 'PartialLeafs' of a type whose names are in 'allowedNames'.
expandTypeWithNames :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> S.HashSet TypeName -> PartialLeafs
expandTypeWithNames typeEnv vaenv t allowedNames = fst $ expandTypesWithNamesFull typeEnv vaenv t allowedNames

-- | Like 'expandTypeWithNames', but also returns a 'Bool' indicating whether the expansion
-- is approximate or has components outside 'allowedNames'. 'True' means the expansion is
-- approximate and the result may be incomplete. 'False' means the expansion is exact and all
-- components are within 'allowedNames'.
expandTypesWithNamesFull :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> S.HashSet TypeName -> (PartialLeafs, Bool)
expandTypesWithNamesFull = expandTypesWithNamesFullAux S.empty

-- | 'expandTypesWithNamesFull' threading the CYCLE GUARD's visited set: the class names
-- whose expansions are on the CURRENT path. The class graph can be cyclic through top-shaped
-- members (@Eq@'s members may include @∀Ord@, whose expansion leads back to @Eq@), and this
-- family — unlike the plain expansion, which keeps Just-top members symbolic — materializes
-- THROUGH them, so an unguarded walk would recurse forever. A re-entered class contributes
-- nothing here ('expandClassPartialWithNamesFullAux''s guard): its direct members were already
-- enumerated at its first entry on this path, so the cut computes the membership transitive
-- closure — the same rule as the reaches walk's @rVisited@ and the sat engine's in-flight
-- markers.
expandTypesWithNamesFullAux :: (TypeGraph tg) => S.HashSet TypeName -> TypeEnv tg -> TypeVarArgEnv -> Type -> S.HashSet TypeName -> (PartialLeafs, Bool)
expandTypesWithNamesFullAux _visited _ _ (UnionType Nothing partials []) allowedNames =
  (filtered, hasOutside)
  where
    filtered = H.filterWithKey (\k _ -> S.member k allowedNames) partials
    hasOutside = any (\k -> not $ S.member k allowedNames) (H.keys partials)
expandTypesWithNamesFullAux visited typeEnv vaenv (TypeVar v _ _) allowedNames =
  expandTypesWithNamesFullAux visited typeEnv vaenv (H.lookupDefault PTopType (extKey v) vaenv) allowedNames
expandTypesWithNamesFullAux _visited TypeEnv{teNames} _ PTopType allowedNames =
  (topFilteredLeafs, not $ S.isSubsetOf teNames allowedNames)
  where
    topFilteredLeafs = joinUnionType $ map ((\p -> p{ptArgMode=PtArgUnbounded}) . partialVal) $ S.toList allowedNames
-- Constants carry no partial-name information; drop them before filtering (both the outer
-- positive constants and a Just-top's excluded constants — this computes leaf expansions only,
-- and a point exclusion never removes a leaf)
expandTypesWithNamesFullAux visited typeEnv vaenv (UnionType mTop leafs (_:_)) allowedNames =
  expandTypesWithNamesFullAux visited typeEnv vaenv (UnionType mTop leafs []) allowedNames
expandTypesWithNamesFullAux visited typeEnv vaenv (UnionType (Just (preds, negLeafs, _:_)) leafs []) allowedNames =
  expandTypesWithNamesFullAux visited typeEnv vaenv (UnionType (Just (preds, negLeafs, [])) leafs []) allowedNames
expandTypesWithNamesFullAux visited typeEnv@TypeEnv{teNames} vaenv (TopType negPartials preds) allowedNames =
  let (leafs, hasOutside) = expandPredsWithNamesFull preds
  in (subtractNeg leafs, hasOutside)
  where
    topFilteredLeafs = joinUnionType $ map ((\p -> p{ptArgMode=PtArgUnbounded}) . partialVal) $ S.toList allowedNames
    hasUniverseOutside = not $ S.isSubsetOf teNames allowedNames

    expandPredsWithNamesFull :: TypePredicates -> (PartialLeafs, Bool)
    expandPredsWithNamesFull (PredsAnd []) = (topFilteredLeafs, hasUniverseOutside)
    expandPredsWithNamesFull (PredsAnd ps) =
      let mapped = map expandPredsWithNamesFull ps
          (venv, leafs) = foldl1 combine (map ((vaenv,) . fst) mapped)
          combine (ve, aLeafs) (_, bLeafs) = intersectPartialLeafsWithVarEnv typeEnv ve aLeafs bLeafs
          -- Intersecting one predicate's members against another's meets an external var on one
          -- side (e.g. @Some[/T=$_B]@ from a @PredClass Maybe[/T=$_B]@) with the other side's
          -- bound by recording the meet in the returned env while the leaf keeps the var pointer.
          -- That env goes no further than this expansion, so resolve the newly recorded meets
          -- back into the leaves - otherwise the pointer later resolves to Top and the expansion
          -- is wider than the predicate it came from. Still-unresolved vars stay symbolic
          -- ('substituteKeep'); the same resolve-the-meet-back rule as 'mergeVarEnvVal'.
          newBindings = discoveredBindings venv vaenv
          leafs' = case substituteKeep typeEnv newBindings (UnionType Nothing leafs []) of
            UnionType Nothing l [] -> l
            _                      -> leafs
      -- Approximate when ANY conjunct is: a member a conjunct's expansion is missing — an open
      -- class's member a later file declares ('openOutside'), a cyclic class's cut — may be in
      -- the intersection, whatever the other conjuncts enumerate in full. Combined with 'all',
      -- a conjunct complete within the names made the whole conjunction read exact, and
      -- @~fluffy ∧ ~Vertebrate@ within @{fluffy}@ expanded to NOTHING, exactly: the empty exact
      -- expansion proved @⊤ | ~fluffy ∧ ~Vertebrate ∧ ¬P@ inside any type (the engine refusing
      -- over the opaque @¬P@), and a meet with it read empty, where the open @Vertebrate@ may yet
      -- gain a member named @fluffy@ — the closed-world decision 'openOutside' exists to prevent.
      -- A sealed conjunct's members outside the names cost no proof: the exactness-gated routes
      -- ('predsPositiveSealed') decide such a conjunction by its full expansion.
      in (leafs', any snd mapped)
    -- De Morgan: ¬(p1 ∧ ... ∧ pn) = ¬p1 ∨ ... ∨ ¬pn.
    -- Compute union of complements directly instead of topFiltered - intersect(expand(pi)),
    -- which avoids approximation errors from PtArgAny/PtArgExact subtraction.
    expandPredsWithNamesFull (PredsNot (PredsAnd ps)) =
      let negPs = map predsNot ps
          mapped = map expandPredsWithNamesFull negPs
          leafs = foldl (H.unionWith S.union) H.empty (map fst mapped)
      in (leafs, any snd mapped)
    -- The arg-mode interval algebra cannot represent every residual (e.g. @n.. − n@ — "n with
    -- a nonempty argument set" — for a name whose realizable argument sets are open-world),
    -- and 'differencePartialLeafs' then leaves the universe leaf UNCHANGED: a wrong-side
    -- approximation that would let a consumer read the un-subtracted name as the exact
    -- complement, keeping a leaf the negated predicate excludes. Detect the silent no-op — a
    -- name whose leafs survive identically although the subtracted side overlaps them — and
    -- report the expansion as approximate-and-empty instead, so consumers fall to their
    -- symbolic paths (which subtract the atom forms exactly).
    --
    -- The complement is taken against @inner@'s members, which are all of @inner@ only when its
    -- positive atoms are enumerable ('predsPositiveSealed': a data name, or a class 'tePrgmEnv'
    -- or its declaration closes; a negation inside @inner@ is this case again, exact on its
    -- own). An open class's members are the ones declared SO FAR, and the universe minus them is
    -- wider than @¬inner@: for an unsealed class @C@ with no members yet it would be every name,
    -- so a leaf @X..@ would be proved inside @⊤|¬C@ although a later file may make @X@ a @C@.
    -- Such a complement keeps @¬inner@ attached to each name instead (@X..|¬C@) — exact, as a
    -- current member is in @inner@ anyway.
    expandPredsWithNamesFull (PredsNot inner)
      | not (predsPositiveSealed typeEnv inner) =
          (joinUnionType $ map (`partialAddPreds` PredsNot inner) $ splitUnionType topFilteredLeafs, hasUniverseOutside)
    expandPredsWithNamesFull (PredsNot inner) =
      let (innerLeafs, _) = expandPredsWithNamesFull inner
      in case differencePartialLeafs typeEnv vaenv topFilteredLeafs innerLeafs of
           UnionType Nothing result []
             | not (any (subtractionNoOp result innerLeafs) (H.keys innerLeafs)) ->
                 (result, hasUniverseOutside)
             | otherwise -> (H.empty, True)
           _ -> (topFilteredLeafs, hasUniverseOutside)
      where
        subtractionNoOp res inners n =
          H.lookup n res == H.lookup n topFilteredLeafs
            && not (isBottomType (intersectTypesEnv typeEnv vaenv
                      (UnionType Nothing (H.filterWithKey (\k _ -> k == n) topFilteredLeafs) [])
                      (UnionType Nothing (H.filterWithKey (\k _ -> k == n) inners) [])))
    expandPredsWithNamesFull (PredsOne (PredClass clss)) =
      expandClassPartialWithNamesFullAux visited typeEnv vaenv clss allowedNames
    expandPredsWithNamesFull (PredsOne (PredRel rel)) =
      case expandRelPartial typeEnv vaenv rel of
        UnionType Nothing leafs [] ->
          (H.filterWithKey (\k _ -> S.member k allowedNames) leafs,
           any (\k -> not $ S.member k allowedNames) (H.keys leafs))
        t -> expandTypesWithNamesFullAux visited typeEnv vaenv t allowedNames
    -- A constant equality within the names: the constant's primitive leaf carrying the atom
    -- (@/Float | PredConstEq c@, the one value @c@) when that name is allowed, and nothing
    -- otherwise — never an empty expansion that would empty a conjunction it joins
    -- (@/Float ∩ (⊤ | PredConstEq c ∧ ¬P)@ is the constant within @¬P@, not ∅). The value is a
    -- single point of its name, so the expansion is exact for that name.
    expandPredsWithNamesFull (PredsOne (PredConstEq c))
      | S.member (ptName cLeaf) allowedNames = (joinUnionType [cLeaf{ptPreds = PredsOne (PredConstEq c)}], hasUniverseOutside)
      | otherwise = (H.empty, True)
      where cLeaf = constantPartialType c
    -- A value predicate is decided per value shape where a leaf carries it (see
    -- 'expandPredicate'): within the names, it is each name with the predicate attached, which
    -- compaction then decides leaf by leaf ('compactFalsePredExprLeafs'). The ALLOWED names, not
    -- the names the environment lists: @⊤ | P@ holds every value of every name, a primitive or a
    -- type no file in scope declares included (open world), and reading such a name as outside
    -- the universe made @/Float ∩ (⊤ | P)@ empty wherever @Float@ went undeclared.
    expandPredsWithNamesFull (PredsOne (PredExpr e)) =
      (joinUnionType $ map (`partialAddPreds` PredsOne (PredExpr e)) $ splitUnionType topFilteredLeafs, hasUniverseOutside)

    subtractNeg :: PartialLeafs -> PartialLeafs
    subtractNeg leafs
      | H.null negPartials = leafs
      | otherwise =
          let negFiltered = H.filterWithKey (\k _ -> S.member k allowedNames) negPartials
          in case differencePartialLeafs typeEnv vaenv leafs negFiltered of
               UnionType Nothing result [] -> result
               _                           -> leafs
-- Mixed type (both top and pos non-empty): distribute expansion over union
expandTypesWithNamesFullAux visited typeEnv vaenv (UnionType (Just topData) pos []) allowedNames
  | not (H.null pos) =
      let (topLeafs, topHasOutside) = expandTypesWithNamesFullAux visited typeEnv vaenv (UnionType (Just topData) H.empty []) allowedNames
          (posLeafs, posHasOutside) = expandTypesWithNamesFullAux visited typeEnv vaenv (UnionType Nothing pos []) allowedNames
      in (H.unionWith S.union topLeafs posLeafs, topHasOutside || posHasOutside)
expandTypesWithNamesFullAux _ _ _ t _ = error $ printf "expandTypesWithNamesFull: unexpected type %s" (show t)

expandClassPartialWithNamesFullAux :: (TypeGraph tg) => S.HashSet TypeName -> TypeEnv tg -> TypeVarArgEnv -> PartialType -> S.HashSet TypeName -> (PartialLeafs, Bool)
-- CYCLE GUARD (see 'expandTypesWithNamesFullAux'): a class re-entered on its own
-- expansion path contributes nothing further — its direct members were enumerated at its
-- first entry, so the cut yields the membership transitive closure. Reported as
-- approximate ('True'): a PARAMETERIZED cyclic class re-entered under different
-- instantiated arguments could contribute members the first entry's substitution did not,
-- so exactness-dependent consumers must not rely on the cut expansion.
expandClassPartialWithNamesFullAux visited _ _ PartialType{ptName} _
  | S.member ptName visited = (H.empty, True)
expandClassPartialWithNamesFullAux visited typeEnv@TypeEnv{teClassGraph=ClassGraph cg} vaenv PartialType{ptName, ptVars=classVarsP} allowedNames =
  case graphLookup (PClassName ptName) cg of
    Just (CGClass sealed PartialType{ptVars=classVarsDecl} classTypes _) -> openOutside sealed $ case classTypes of
      [] ->
        let leafs = joinUnionType [classPlaceholderLeaf ptName]
        in (H.filterWithKey (\k _ -> S.member k allowedNames) leafs,
            any (\k -> not $ S.member k allowedNames) (H.keys leafs))
      _  ->
        let mapped = map mapClassTypeWithNamesFull classTypes
            leafs = foldl (H.unionWith S.union) H.empty (map fst mapped)
        in (leafs, any snd mapped)
      where
        -- An UNSEALED class has members beyond the ones declared so far — any later file may
        -- add one under any name — so outside the closed-world program view its expansion always
        -- has content outside the requested names, whatever today's members are. Reported exact,
        -- it would let a cover be proved from the current members alone: @∀Bag ⊆ Cons + Nil@
        -- because Bag's only members so far are lists.
        openOutside sealedC (leafs, outside) = (leafs, outside || not (sealedC || tePrgmEnv typeEnv))
        classVars = H.unionWith (intersectTypesEnv typeEnv vaenv) classVarsP (cleanDeclSelfRefs classVarsDecl)
        visited' = S.insert ptName visited
        -- A class member that is itself a (parameterized) class - e.g. @List@'s sole member
        -- @ConsList[/T=$_T]@ - must receive this class's instantiated arguments before being
        -- re-expanded, or the nested member's element parameter stays the raw member variable
        -- (@$_T@) instead of the class argument (e.g. @Integer@). The sibling 'TypeVar' case below
        -- already looks the argument up; this mirrors it for the nested-class form. EXIT
        -- substitution ('substituteExit'): the class scope the @$_T@ spelling names ceases to
        -- exist here, so a ⊤-bound parameter substitutes the ⊤ it denotes — the keep-symbolic
        -- variant would leak the dangling spelling into the re-expansion's 'classVarsP', where a
        -- like-named enclosing scope later captures it.
        mapClassTypeWithNamesFull (TopType negPartials ps) =
          expandTypesWithNamesFullAux visited' typeEnv vaenv (substituteExit typeEnv ((ambientEnv $ joinVarArgEnv classVars H.empty)) (TopType negPartials ps)) allowedNames
        mapClassTypeWithNamesFull (TypeVar (TVVar t) _ _) = case H.lookup t classVars of
          Just v  -> expandTypesWithNamesFullAux visited' typeEnv vaenv (intersectTypesEnv typeEnv vaenv v (H.lookupDefault PTopType t classVars)) allowedNames
          Nothing -> error $ printf "Unknown var %s in expandPartial" (show t)
        mapClassTypeWithNamesFull (TypeVar (TVArg t) _ _) = error $ printf "Arg %s found in expandPartial" (show t)
        mapClassTypeWithNamesFull (UnionType Nothing p []) =
          let fullList = splitUnionType p
              filteredList = filter (\PartialType{ptName=n} -> S.member n allowedNames) fullList
              hasOut = any (\PartialType{ptName=n} -> not $ S.member n allowedNames) fullList
          in (joinUnionType $ map mapClassPartial (mapMaybe (classSideMember typeEnv vaenv classVars) filteredList), hasOut)
        mapClassTypeWithNamesFull t = expandTypesWithNamesFullAux visited' typeEnv vaenv t allowedNames
        -- Substitute this class's instantiated arguments through the member at expansion EXIT
        -- ('substituteExit' resolves EVERY external 'TVExt' reference — the class scope the
        -- references name ceases to exist here, so a ⊤-bound parameter substitutes the ⊤ it
        -- denotes rather than keeping the dangling spelling — while every 'TVInt' field stays
        -- tied to the member's own now-substituted 'ptVars'), so a field like
        -- @tail -> ConsList[/T=$_T]@ receives the class argument rather than a free variable that
        -- a like-named variable of the receiving scope later captures. In the closed-world
        -- program-coverage view ('tePrgmEnv') the member is a match /target/ whose field types are
        -- supplied by the value being matched, so its explicitly-written fields are then relaxed
        -- to 'PTopType' (keys only) — preserving them would spuriously hold a destructured
        -- definition's binding field (which merely binds and ignores the value) outside its
        -- declaration. The true field types always remain in the data graph.
        mapClassPartial tp@PartialType{ptArgs}
          | tePrgmEnv typeEnv = base{ptArgs=fmap (const PTopType) ptArgs}
          | otherwise         = base
          where base = substitutePartialExit typeEnv ((ambientEnv $ joinVarArgEnv classVars H.empty)) tp
    r -> error $ printf "Unknown class %s in expandPartial. Found %s" (show (PClassName ptName)) (show r)

expandClassPartial :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> Type
expandClassPartial = expandClassPartialAux Nothing

-- | 'expandClassPartial' restricted for use as an INTERSECTION OPERAND against a positive
-- ('Nothing'-top) type whose leaf names lie within the allowed set: positive leaf members
-- outside the names are dropped — they could only meet to ∅, since 'intersectPartialsBase'
-- refuses any name mismatch (the same argument 'intersectTypesWithVarEnv' states at its own
-- name-restricted expansion clause). Class-typed (Just-top) members stay SYMBOLIC exactly as
-- in 'expandClassPartial', so the meet machinery's lazier materialization decides them
-- identically. NOT a general expansion: the result equals 'expandClassPartial' only modulo
-- that intersection — never store it, never use it under negation/complement or on the sub
-- side of a subtype check.
expandClassPartialWithinNames :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> S.HashSet TypeName -> PartialType -> Type
expandClassPartialWithinNames typeEnv vaenv allowedNames = expandClassPartialAux (Just allowedNames) typeEnv vaenv

-- | Token-keyed memo for 'expandClassPartialAux', mirroring the 'objArrGraphQuery' reach-query
-- memo: same soundness story (token-per-immutable-env), same generational retention (the
-- static per-page, epoch reaches, and merged reaches tokens rotate), same in-flight-sat bypass
-- (which costs sharing and changes no answer). 'compactOverlapping' \/
-- 'compactPartialsWithClassPred' re-expand the SAME small classes (@Boolean@, @Eq@, ...) through
-- per-leaf-pair subtype checks many thousands of times per epoch, while the expansion is a pure
-- function of the key below.
type ExpandClassMemoKey = (Int, (Bool, Bool, Bool), Maybe (S.HashSet TypeName), PartialType, [(VarKey, Type)])

{-# NOINLINE expandClassMemoRef #-}
expandClassMemoRef :: IORef [(Int, H.HashMap (Hashed ExpandClassMemoKey) (Type, ReadDeps))]
expandClassMemoRef = unsafePerformIO (newIORef [])

expandClassPartialAux :: (TypeGraph tg) => Maybe (S.HashSet TypeName) -> TypeEnv tg -> TypeVarArgEnv -> PartialType -> Type
expandClassPartialAux mAllowed typeEnv vaenv pp = case teQueryCacheKey typeEnv of
  Nothing -> expandClassPartialAuxRaw mAllowed typeEnv vaenv pp
  Just _ | teInSatDecision typeEnv -> expandClassPartialAuxRaw mAllowed typeEnv vaenv pp
  Just tok -> unsafePerformIO $ do
    -- Hashed once, for the lookup and the store both.
    key <- evaluate (hashed (tok, (teDisableCompact typeEnv, tePrgmEnv typeEnv, teArgModeProjection typeEnv), mAllowed, pp, L.sortOn fst (H.toList vaenv)))
    gens <- readIORef expandClassMemoRef
    case lookup tok gens >>= H.lookup key of
      Just (r, deps) -> noteReadDeps deps >> return r
      Nothing -> do
        -- Forced through, so every read it makes is made here ('withReadFrame').
        (r, deps) <- memoReading typeEnv $ do
          let r = expandClassPartialAuxRaw mAllowed typeEnv vaenv pp
          _ <- evaluate (forceType r)
          return r
        atomicModifyIORef' expandClassMemoRef $ \gens2 ->
          let mTok = fromMaybe H.empty (lookup tok gens2)
              rest = filter ((/= tok) . fst) gens2
          in ((tok, H.insert key (r, deps) mTok) : take 2 rest, ())
        return r

-- | A class's member record read at one instantiation of the class, or 'Nothing' where that
-- instantiation does not hold the member.
--
-- A member recorded with a dimension its own data declaration does not declare
-- (@data(IntOrdering, isa= [Ordering[$T -> Integer]])@ records @IntOrdering[/T=Integer]@) carries
-- the CLASS's parameter binding, not one of its own: @IntOrdering@ is the @Ordering@ over
-- @Integer@. So the binding decides which instantiations hold the member — under the covariant
-- reading, @Ordering[/T=Y]@ does exactly when @Integer ⊆ Y@ — and it never constrains the
-- member's values, so the member is read without it. Read as the member's own dimension, a value
-- spelled without it (@/Data/MaybeEqv@, the body of every instance arm) read the missing @/T@ as
-- ⊤ and fell outside its own class. Every reading of a class's members goes through here — the
-- class expansions ('expandClassPartialAuxRaw', 'expandClassPartialWithNamesFull') and the
-- symbolic engine's member candidates ('classSatCandidates') — so the provers agree.
classSideMember :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> H.HashMap TypeVarName Type -> PartialType -> Maybe PartialType
classSideMember typeEnv vaenv classVars m@PartialType{ptName=memberName, ptVars=memberVars}
  | H.null classSide = Just m
  | all admitted (H.toList classSide) = Just m{ptVars = H.difference memberVars classSide}
  | otherwise = Nothing
  where
    classSide = case typeGraphData typeEnv memberName of
      []    -> H.empty
      decls -> H.filterWithKey (\k _ -> not (any (H.member k . ptVars) decls)) memberVars
    admitted (k, b) = isSubtypeOfWithEnv typeEnv vaenv b (H.lookupDefault PTopType k classVars)

expandClassPartialAuxRaw :: (TypeGraph tg) => Maybe (S.HashSet TypeName) -> TypeEnv tg -> TypeVarArgEnv -> PartialType -> Type
expandClassPartialAuxRaw mAllowed typeEnv@TypeEnv{teClassGraph=ClassGraph cg} vaenv PartialType{ptName, ptVars=classVarsP} = expanded
  where
    className = PClassName ptName
    -- The allowed-name filters: 'keepPartials' on a member list, 'keepType' on an expanded
    -- var-bound member. Just-top (symbolic) and 'TypeVar' results pass through whole —
    -- restricting under a predicate top would need complement reasoning, and the meet
    -- machinery already name-restricts those lazily.
    keepPartials = case mAllowed of
      Nothing -> id
      Just ns -> filter (\PartialType{ptName=memberName} -> S.member memberName ns)
    keepType t = case (mAllowed, t) of
      (Just _, UnionType Nothing p cs) -> UnionType Nothing (joinUnionType $ keepPartials $ splitUnionType p) cs
      _                                -> t
    expanded = case graphLookup className cg of
      Just (CGClass _ PartialType{ptVars=classVarsDecl} classTypes _) -> case classTypes of
        [] -> singletonType $ classPlaceholderLeaf ptName
        -- Single-compaction union: member templates carry no explicit recursion (the
        -- canonical recursive reference is the predicate/top form), so the union-folding
        -- compaction clauses cannot fire mid-fold and the once-compacted result is
        -- spelling-identical to the pairwise fold.
        _  -> unionAllTypesWithEnvOnce typeEnv vaenv $ map mapClassType classTypes
        where
          classVars = H.unionWith (intersectTypesEnv typeEnv vaenv) classVarsP (cleanDeclSelfRefs classVarsDecl)
          mapClassType (TypeVar (TVVar t) _ _) = case H.lookup t classVars of
            Just v -> keepType $ expandNeg $ intersectTypesEnv typeEnv vaenv v (H.lookupDefault PTopType t classVars)
            Nothing -> error $ printf "Unknown var %s in expandPartial" (show t)
          mapClassType (TypeVar (TVArg t) _ _) = error $ printf "Arg %s found in expandPartial" (show t)
          mapClassType (TypeVar (TVApplied t _) _ _) = error $ printf "Applied var %s found in expandPartial" (show t)
          -- Name-filter BEFORE the exit substitution: 'substitutePartialExit' never renames
          -- a member, so filtering on declared names is equivalent and skips the
          -- substitution cost for dropped members.
          mapClassType (UnionType Nothing p []) = UnionType Nothing (joinUnionType $ mapMaybe mapClassMember $ keepPartials $ splitUnionType p) []
          mapClassType t@(UnionType Nothing _ _) = keepType t
          -- A class-typed member (@List@'s @∀ConsList[/T=$_T]@) exits the class scope here:
          -- substitute its external references totally ('substituteExit'), or the dangling
          -- spelling leaks and a like-named enclosing scope captures it.
          mapClassType t@(UnionType (Just _) _ _) = substituteExit typeEnv ((ambientEnv $ joinVarArgEnv classVars H.empty)) t
          expandNeg t@(UnionType (Just _) _ _) = t
          expandNeg (TypeVar tv _ _) = H.lookupDefault PTopType (extKey tv) vaenv
          expandNeg t              = t
          mapClassPartial = substitutePartialExit typeEnv ((ambientEnv $ joinVarArgEnv classVars H.empty))
          mapClassMember m = mapClassPartial <$> classSideMember typeEnv vaenv classVars m
      r -> error $ printf "Unknown class %s in expandPartial. Found %s" (show className) (show r)

expandRelPartial :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> Type
expandRelPartial = expandRelPartialAux Nothing

-- | 'expandRelPartial' under the same restricted-intersection-operand contract as
-- 'expandClassPartialWithinNames': the concrete-name candidates ('tpNames'\/'fromArgs') are
-- filtered to the allowed set — a dropped candidate is a positive leaf of another name and
-- meets to ∅ — while the class-route candidates ('clsNames', symbolic 'classPartial' tops)
-- are kept whole, since restricting under a predicate top is not name-local. This is where
-- the whole-universe 'teNames' scan dies for single-name consumers.
expandRelPartialWithinNames :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> S.HashSet TypeName -> PartialType -> Type
expandRelPartialWithinNames typeEnv vaenv allowedNames = expandRelPartialAux (Just allowedNames) typeEnv vaenv

expandRelPartialAux :: (TypeGraph tg) => Maybe (S.HashSet TypeName) -> TypeEnv tg -> TypeVarArgEnv -> PartialType -> Type
expandRelPartialAux mAllowed typeEnv@TypeEnv{teNames} vaenv relPartial = result
  where
    result = unionAllTypesWithEnv typeEnv vaenv (UnionType Nothing (joinUnionType fromArgs) [] : tpNames ++ clsNames)
    relName = ptName relPartial
    keepNames = case mAllowed of
      Nothing -> id
      Just ns -> filter (`S.member` ns)
    fromArgs = map (\n -> relPartial{ptName=n}) $ keepNames $ relativeNameFilter relName $ map pkName $ H.keys $ snd $ splitVarArgEnv (envAmbient vaenv)
    tpNames = map (\n -> singletonType relPartial{ptName=n}) $ filter (relArgKeysFeasible typeEnv relPartial) $ keepNames $ relativeNameFilter relName $ S.toList teNames
    clsNames = map (setArgMode H.empty (ptArgMode relPartial) . classPartial . partialVal) $ relativeNameFilter relName $ listClassNames typeEnv

-- | The value SET a partial's value denotes standalone: read via 'substituteTVExt' under
-- the partial's OWN var\/arg scope over the ambient env — the codebase's canonical reading
-- (the same one 'mergeVarEnvVal' applies to every stored env binding), so forms produced
-- here are structurally comparable with the intersection's. A top-level reference reads
-- through the partial's slot (@head=$/T@ with @/T=X@ denotes @X@), a nested partial's
-- references read through ITS own slots, and an unbound reference reads as ⊤.
resolveOwnScopeRefs :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> Type -> Type
resolveOwnScopeRefs typeEnv vaenv p = substituteTVExtScoped typeEnv (ptVarArg p) vaenv

-- | The instances of a class-predicate bound that a value witnesses.
--
-- A variable bounded by a CLASS (@$M -> Monad@) ranges over that class's INSTANCES, not over
-- their constructors. Evidence that an argument matched @Cons@ therefore says @$M@ is the
-- @ConsList@ instance — not that it is @Cons@. Narrowing to the constructor would force every
-- OTHER occurrence of the variable to that same constructor, which for a definition arm means
-- the sibling @fn(v) -> $M@ of @mmap(m= Nil, fn(v))@ having to return @Nil@ while the recursive
-- caller passes one returning @Cons@ — a dispatch that then matches nothing.
--
-- A bound that is not a class witnesses no instance and the evidence stands as written, which
-- is every var whose bound is ⊤ or a plain type.
boundClassInstancesFor :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Type
boundClassInstancesFor typeEnv@TypeEnv{teClassGraph=ClassGraph cg} vaenv bound v = case hits of
  [] -> v
  hs -> unionAllTypesWithEnv typeEnv vaenv hs
  where
    classAtoms = case bound of
      UnionType (Just (preds, _, _)) _ _ -> case tryPredsToList preds of
        Just as -> [cp | PredClass cp <- as]
        Nothing -> []
      _ -> []
    -- Membership is decided on NAMES alone, walked through the class graph with a visited set.
    -- This runs INSIDE the meet, so it must not re-enter the type algebra: an intersection test
    -- here recurses back into 'intersectPartialsBase' through the class expansion and does not
    -- terminate.
    headNames t = case t of
      UnionType (Just (preds, _, _)) pos _ ->
        [ptName cp | Just as <- [tryPredsToList preds], PredClass cp <- as] ++ map ptName (splitUnionType pos)
      UnionType Nothing pos _ -> map ptName (splitUnionType pos)
      _                       -> []
    coveredNames seen n
      | S.member n seen = S.empty
      | otherwise = case graphLookup (PClassName n) cg of
          Just CGClass{cgcExtends} ->
            S.insert n $ S.unions [ coveredNames (S.insert n seen) n' | m <- cgcExtends, n' <- headNames m ]
          _ -> S.singleton n
    evidenceNames = S.fromList (headNames v)
    hits = [ m | cp <- classAtoms
               , Just CGClass{cgcExtends} <- [graphLookup (PClassName (ptName cp)) cg]
               , m <- cgcExtends
               , not (S.null (S.intersection evidenceNames (S.unions (map (coveredNames S.empty) (headNames m))))) ]
