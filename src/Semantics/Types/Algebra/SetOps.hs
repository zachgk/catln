-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.Algebra.SetOps
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- The set operations on types: union, intersection, difference and complement.
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns    #-}

module Semantics.Types.Algebra.SetOps where

import           Control.Exception               (evaluate)
import qualified Data.HashMap.Strict             as H
import qualified Data.HashSet                    as S
import qualified Data.List                       as L
import           Data.Maybe
import           Data.Zip
import           Debug.Trace                     (trace)
import           Prelude                         hiding (unzip)
import           Semantics.Types.Algebra.Compact
import           Semantics.Types.Algebra.Expand
import           Semantics.Types.Algebra.Preds
import           Semantics.Types.Algebra.Subst
import           Semantics.Types.Algebra.Subtype
import           Semantics.Types.ArgModes
import           Semantics.Types.Construct
import           Semantics.Types.Data
import           Semantics.Types.Memo
import           Semantics.Types.Name
import           Semantics.Types.Preds
import           Semantics.Types.SatGoal
import           Semantics.Types.Vars
import           System.IO.Unsafe                (unsafePerformIO)
import           Text.Printf
import           Utils


-- | Whether a predicate formula negates ('PredsNot') an atom over a RECURSIVE class
-- membership ('classRecursiveMembership') anywhere. Meets against such formulas stay
-- symbolic ('partialAddPreds') instead of materializing (see the ¬-containing clause of
-- 'intersectTypesWithVarEnv'): their materialization re-introduces the formula in a
-- recursive member slot and never converges, while a non-recursive negation materializes
-- finitely and keeps its existing exact spelling.
predsHasRecNegAtom :: TypeGraph tg => TypeEnv tg -> TypePredicates -> Bool
predsHasRecNegAtom typeEnv = goPos
  where
    goPos (PredsAnd ps) = any goPos ps
    goPos (PredsNot p)  = goNeg p
    goPos _             = False
    -- Under a negation: any class/rel atom over a recursive membership triggers (deeper
    -- negations flip back to the positive side, where only another negation can trigger).
    goNeg (PredsAnd ps) = any goNeg ps
    goNeg (PredsNot p)  = goPos p
    goNeg (PredsOne a)  = case a of
      PredClass p   -> classRecursiveMembership typeEnv (ptName p)
      PredRel p     -> classRecursiveMembership typeEnv (ptName p)
      PredExpr _    -> False
      PredConstEq _ -> False

-- | Whether a formula mentions an unsealed class (outside the closed-world 'tePrgmEnv') at any
-- polarity: a class\/rel atom whose class is open. Its current members do not bound it, so a
-- meet can only carry it symbolically.
predsHasOpenAtom :: TypeGraph tg => TypeEnv tg -> TypePredicates -> Bool
predsHasOpenAtom typeEnv
  | tePrgmEnv typeEnv = const False
  | otherwise = go
  where
    go (PredsAnd ps) = any go ps
    go (PredsNot p)  = go p
    go PredsNone     = False
    go (PredsOne a)  = case a of
      PredClass c   -> openClass c
      PredRel r     -> any openClass (relMatchingClasses typeEnv r)
      PredExpr _    -> False
      PredConstEq _ -> False
    openClass c = not (predsSealed typeEnv (PredsOne (PredClass c)))

-- | Whether class @c@'s membership structure is recursive: the class-reference graph
-- reachable from @c@ — through its members' names and every class\/rel atom appearing at
-- any depth in their var\/arg slots (e.g. @Cons@'s @tail=∀ConsList[$T]@) — revisits a
-- class on the current path. Covers both directly self-bounded memberships
-- (@class(Eq, [.., ConsList[$T -> Eq]])@) and interlocked ones reached through a
-- shared recursive container.
classRecursiveMembership :: TypeGraph tg => TypeEnv tg -> TypeName -> Bool
classRecursiveMembership typeEnv = go S.empty
  where
    go path n
      | S.member n path = True
      | otherwise = case H.lookup n (scClasses (teSatCache typeEnv)) of
          Just ClassSatInfo{csiTypes} -> any (go (S.insert n path)) (S.toList (S.unions (map typeRefNamesDeep csiTypes)))
          Nothing                     -> False

-- | Whether a type parameter of a name is tied to SEVERAL of its values' positions. A type is the
-- set of its possible values, and a parameter such a declaration ties to several fields
-- (@Cons[T](head -> $T, tail -> ConsList[$T])@) constrains every one of them: @Cons[/T=X]@ is the
-- lists whose EVERY element is in @X@. It is not a coordinate each value has one point of, so
-- @Cons[/T=⊤] − Cons[/T=K]@ is the lists with SOME element outside @K@, not @Cons[/T=⊤−K]@ (the
-- lists with every element outside it), and narrowing one of its fields narrows only that field.
-- A parameter tied to a single field (@Some(value -> $T)@) is that field's value, a coordinate.
paramSharedByPositions :: (TypeGraph tg) => TypeEnv tg -> TypeName -> TypeVarName -> Bool
paramSharedByPositions typeEnv name n = S.size (paramPositions typeEnv name n) > 1

-- | The fields a name's declarations tie a type parameter to: a field declared as the parameter
-- itself (@head -> $T@) or as a type referencing it (@tail -> ConsList[$T]@).
paramPositions :: (TypeGraph tg) => TypeEnv tg -> TypeName -> TypeVarName -> S.HashSet ArgName
paramPositions typeEnv name n = S.fromList [k | d <- typeGraphData typeEnv name, (k, v) <- H.toList (ptArgs d), refs v]
  where
    refs (TypeVar (TVVar n') TVInt _) = n' == n
    refs v                            = S.member n (typeOuterVarRefs v)

-- | Takes the union of two types (∪).
unionTypesWithEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Type
unionTypesWithEnv TypeEnv{teDebug=True} _ l r | trace (printf "[UNION] %s ∪ %s" (show l) (show r)) False = PTopType
unionTypesWithEnv _ _ PTopType _ = PTopType
unionTypesWithEnv _ _ _ PTopType = PTopType
unionTypesWithEnv _ _ t BottomType = t
unionTypesWithEnv _ _ BottomType t = t
unionTypesWithEnv _ _ t1 t2 | t1 == t2 = t1
-- TypeVar ∪ t → resolve the var in vaenv. An unconstrained var is unknown, i.e. 'PTopType'
-- (matching 'vaenvLookup' and the TypeVar handling in 'isSubtypeOfWithEnv'/'complementTypeEnv').
unionTypesWithEnv typeEnv vaenv (TypeVar v _ i) t = unionTypesWithEnv typeEnv vaenv (intersectTypesEnv typeEnv vaenv (vaenvLookup typeEnv vaenv v) i) t
unionTypesWithEnv typeEnv vaenv t v@TypeVar{} = unionTypesWithEnv typeEnv vaenv v t
-- Structured union: separate handling for positive and top components.
-- The outer posLeafs/posConsts of both sides always merge positively; a 'Just' top is carried
-- SYMBOLICALLY (never expanded to its materialized members — for an unsealed class the expansion
-- is a strictly smaller set, so expanding changes the union). 'compactType' canonicalises the
-- result (dropping positive members the top already covers and cancelling re-added exclusions).
-- Top components: same preds → neg leafs intersect; PredsNone ∪ anything → PredsNone with intersected negs;
-- different preds → De Morgan normalization: p1∨p2 = ¬(¬p1∧¬p2) via predsNot/predsAnd (flat, depth 1).
unionTypesWithEnv typeEnv vaenv t1@(UnionType mTop1 pos1 cs1) t2@(UnionType mTop2 pos2 cs2) =
  debugTrace typeEnv msg result
  where
    pos'  = unionPartialLeafs [pos1, pos2]
    cs'   = L.nub (cs1 ++ cs2)
    -- The part of a set of negated (excluded) leafs that the OTHER operand does not re-add:
    -- @(U − N) ∪ P = (U − (N − P)) ∪ P@, so a positive leaf covering part of an excluded region
    -- folds that part out of the exclusion. Difference distributes over the union of negated leafs
    -- (@(⋃ Nᵢ) − P = ⋃ (Nᵢ − P)@), so each leaf is subtracted SEPARATELY: differencing the negated
    -- set as one union would let 'compactType' fold an over-unrolled recursive set (e.g. an unrolled
    -- @ConsList@) to its @~C@ rel-form — a Just-top, not the leaf set the exclusion needs. A single
    -- leaf never triggers that fold ('foldRecursiveType' needs >1 top-level leaf), so each per-leaf
    -- difference stays positive.
    exclOutside negs other
      | H.null negs = negs
      | otherwise = unionPartialLeafs
          [ case snd $ differenceTypeWithEnv typeEnv vaenv (singletonType negLeaf) other of
              UnionType Nothing ls _ -> ls
              d -> error $ printf "unionTypesWithEnv: non-positive neg-leaf difference %s" (show d)
          | negLeaf <- splitUnionType negs ]
    result = case (mTop1, mTop2) of
      (Nothing, Nothing) ->
        compactType typeEnv vaenv $ UnionType Nothing pos' cs'
      -- Fold the Nothing side's positive leafs out of the Just side's negated leafs so the mixed
      -- orders produce the SAME negLeafs as the (Just,Just) order below (which already subtracts via
      -- 'exclOutside'). Otherwise @(⊤ − X) ∪ Y@ with @Y ⊆ X@ would keep the unfolded negLeaf @X@ in
      -- one association order and the folded @X − Y@ in another, two valid representations
      -- 'isEqType' cannot equate.
      (Nothing, Just (p2, n2, nc2)) ->
        compactType typeEnv vaenv $ UnionType (Just (p2, exclOutside n2 t1, nc2)) pos' cs'
      (Just (p1, n1, nc1), Nothing) ->
        compactType typeEnv vaenv $ UnionType (Just (p1, exclOutside n1 t2, nc1)) pos' cs'
      (Just (p1, n1, nc1), Just (p2, n2, nc2)) ->
        -- The union's predicate p1∨p2, kept syntactically stable across solver epochs: equal or
        -- one-sided-universal preds collapse directly, and when one predicate region implies the
        -- other the union is just the wider side (semantically equal to the De Morgan fallback,
        -- but avoids nesting another !(!p1 && !p2) layer per union - growth
        -- 'verifySchemeActLowers' cannot order). Otherwise De Morgan with predsNot/predsAnd
        -- normalization: p1∨p2 = ¬(¬p1 ∧ ¬p2), kept flat (depth 1).
        let preds' | p1 == p2 = p1
                   | p1 == PredsNone || p2 == PredsNone = PredsNone
                   | predImplies typeEnv vaenv p1 p2 = p2
                   | predImplies typeEnv vaenv p2 p1 = p1
                   | otherwise = predsNot $ predsAnd (predsNot p1) (predsNot p2)
            -- A constant one side excludes, placed against each side: proven outside both
            -- ('constantOutsideType') it stays excluded ((U−nc1) ∪ (U−nc2) excludes only nc1 ∩ nc2);
            -- proven inside one ('constantInType' — a side that re-adds it positively included) it
            -- is in the union; held by a side only WITHIN its value predicates
            -- ('constantWithinTop') it is in the union exactly there, so it leaves the top and comes
            -- back as the point within them (@/Char | PredConstEq c ∧ P@): @(⊤|P) ∪ (⊤ − {c})@ holds
            -- @c@ iff @P@ does, where a bare @⊤@ read it as held outright.
            placed = [ (c, placement c) | c <- L.nub (nc1 ++ nc2) ]
            placement c
              | all (constantOutsideType typeEnv vaenv c) [t1, t2] = Nothing
              | any (constantInType typeEnv vaenv c) [t1, t2] = Just Nothing
              | otherwise = Just (Just [ (constantPartialType c){ptPreds = mkAnd (PredsOne (PredConstEq c) : lits)}
                                       | side <- [t1, t2], Just lits <- [withinSide c side] ])
            -- The literals a side holds the constant within, or none when it is proven out of it.
            -- A side whose top is decided but whose positive part may hold the constant is read as
            -- holding it ('constantInType' above does not catch that), the wider reading.
            withinSide c side = case side of
              UnionType (Just td) _ _ | not (constantOutsideType typeEnv vaenv c side) -> case constantWithinTop typeEnv vaenv c td of
                Just lits@(_:_) -> Just lits
                _               -> Nothing
              _ -> Nothing
            nc' = [ c | (c, pl) <- placed, pl /= Just Nothing ]
            withinLeafs = joinUnionType (concat [ ls | (_, Just (Just ls)) <- placed ])
            -- Excluded leaf regions: with the SAME predicate universe, n1 ∩ n2 is exact. With
            -- differing predicates it is not - an n1 leaf lying OUTSIDE U_p2 is in neither side
            -- (e.g. (U − /aaaaa) ∪ (U_{isa aaaab} − ∅): the /aaaaa leaf meets no n2 leaf yet must
            -- stay excluded, else the union widens to ⊤). The exact exclusion mirrors the constant
            -- rule: the part of each side's excluded leafs the OTHER side does not contain,
            -- (n1 − t2) ∪ (n2 − t1). A leaf−type difference yields positive leafs (plus re-added
            -- points of the other side's excluded constants, which the nc' rule already accounts
            -- for exactly: such a point survives there iff it is in neither side).
            negLeafs'
              | p1 == p2 = snd $ intersectPartialLeafsWithVarEnv typeEnv vaenv n1 n2
              | otherwise = unionPartialLeafs [exclOutside n1 t2, exclOutside n2 t1]
        in compactType typeEnv vaenv $
             UnionType (Just (preds', negLeafs', nc')) (H.unionWith S.union pos' withinLeafs) cs'
    msg = printf "[UNION] %s ∪ %s = %s" (show $ UnionType mTop1 pos1 cs1) (show $ UnionType mTop2 pos2 cs2) (show result)

-- | Takes the 'unionTypes' of many types
unionAllTypesWithEnv :: (TypeGraph tg, Foldable f) => TypeEnv tg -> TypeVarArgEnv -> f Type -> Type
unionAllTypesWithEnv typeEnv vaenv = foldr (unionTypesWithEnv typeEnv vaenv) BottomType

-- | 'unionAllTypesWithEnv' with ONE final 'compactType' instead of one per fold step: the
-- positive 'Nothing'-top operands are merged raw ('unionPartialLeafs' + constant nub) and
-- compacted once, and only the remaining operands (predicate tops, type vars — where the
-- pairwise 'preds'' absorption and var resolution genuinely need 'unionTypesWithEnv') fold
-- pairwise in their original relative order on top of that base. Set-equal to
-- 'unionAllTypesWithEnv' (every intermediate compaction is semantics-preserving and the
-- final compaction sees the same raw leafs); NOT guaranteed spelling-identical when a
-- pairwise fold prefix would have triggered a union-folding compaction mid-fold — callers
-- that store the result must gate adoption on a golden audit.
unionAllTypesWithEnvOnce :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> [Type] -> Type
unionAllTypesWithEnvOnce typeEnv vaenv ts = foldr (unionTypesWithEnv typeEnv vaenv) posBase others
  where
    isPosUnion (UnionType Nothing _ _) = True
    isPosUnion _                       = False
    (poss, others) = L.partition isPosUnion ts
    posBase = compactType typeEnv vaenv $ UnionType Nothing
                (unionPartialLeafs [p | UnionType Nothing p _ <- poss])
                (L.nub (concat [c | UnionType Nothing _ c <- poss]))

-- | Takes the 'unionTypes' of many types
unionAllTypes :: (TypeGraph tg, Foldable f) => TypeEnv tg -> f Type -> Type
unionAllTypes typeEnv = unionAllTypesWithEnv typeEnv H.empty

intersectPartialsBase :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> Maybe (TypeVarArgEnv, [PartialType])
intersectPartialsBase _ _ PartialType{ptName=aName} PartialType{ptName=bName} | aName /= bName = Nothing
-- Both operands are read with their spread floor ('spreadFloor') first, so a spread's declared
-- floor meets as listed fields; the modes are left as written (a spread still admits extras).
--
-- A higher-order argument's input object reads its parent references from the parameters' own
-- slots as they are before the arguments meet ('intersectPartialsBaseN'.'readInputObjParents').
-- A parameter the DATA arguments pin through a sibling's bound (@fn(v -> $_A)@ beside
-- @m -> $F@, @$F -> Functor[$T -> $_A]@) is only learned from that meet, so the input object was
-- met with it still ⊤: @bindap@'s fmap arm met with a call whose lambda takes @∀Maybe[Integer]@
-- read @fn[/A=∀Maybe[Integer]]@ and accepted, though at @m= Maybe[Integer]@ the arm passes the
-- @Integer@ element. The data decides what the input is ('pinnedSlotsOk' reads it the same way),
-- so the meet is taken again with what the first one pinned, and the input object meets it.
intersectPartialsBase typeEnv vaenv a0 b0 = do
  let a = spreadFloor typeEnv a0
      b = spreadFloor typeEnv b0
  r@(_, ps) <- intersectPartialsBaseN typeEnv vaenv a b
  let pinned = H.fromList [ (n, t) | n <- S.toList (inputParentRefs a `S.union` inputParentRefs b)
                                   , all (unresolvedAt n) [a, b]
                                   , Just ts <- [mapM (H.lookup n . ptVars) ps]
                                   , not (null ts), all (\t -> not (isTypeVar t) && t /= PTopType) ts
                                   , let t = unionAllTypes typeEnv ts ]
  if H.null pinned
    then return r
    else intersectPartialsBaseN typeEnv vaenv a{ptVars = H.unionWith (intersectTypesEnv typeEnv vaenv) pinned (ptVars a)} b
  where
    -- The parameters a partial's higher-order input objects name by a parent reference.
    inputParentRefs p = S.fromList [ n | (k, UnionType Nothing leafs _) <- H.toList (ptArgs p), not (S.null (pkArgs k))
                                       , l <- splitUnionType leafs, TypeVar (TVVar n) TVParent _ <- H.elems (ptVars l) ]
    unresolvedAt n p = H.lookupDefault PTopType n (ptVars p) == PTopType

intersectPartialsBaseN :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> Maybe (TypeVarArgEnv, [PartialType])
intersectPartialsBaseN typeEnv vaenv aP@(PartialType name' aVars aArgs aPreds (PtArgMode _ aUb _)) bP@(PartialType _ bVars bArgs bPreds (PtArgMode _ bUb _)) = do
  -- The parameter meets read no binding (they are seeded empty of VALUES), but a parent reference
  -- in a parameter value (@Map[/V=$^MV]@ in an argument of @foldEntries[$MV]@) names the partial
  -- whose argument this one sits in, so that partial's parameters are its 'TVParent' layer — keys
  -- only, read as ⊤. Its meets are then recorded under that layer rather than the ambient one,
  -- and 'parentToOwn' hands them back as the enclosing partial's own on return.
  let parentSeed = H.fromList [ (VarKey TVParent v, PTopType) | (VarKey TVInt v, _) <- H.toList vaenv ]
  -- A parameter's value may name a SIBLING parameter, as a parent reference from the partial
  -- nested in it (@f[/M=∀Mon[/T=$^A], /A=⊤]@ — 'siblingRefsToParent'): the parameters are one
  -- system, not independent meets. So the var meets also see this partial's own parameters, as
  -- the layer a nested partial's parent references name, and what they learn about them is met
  -- into those parameters' slots: a call's @/M=Bx[/T=X]@ against @∀Mon[/T=$^A]@ binds @/A=X@.
  -- Those readings are this partial's own and are not handed to the caller.
  let ownKeys = S.fromList [ VarKey TVInt (TVVar n) | n <- H.keys aVars ++ H.keys bVars ]
  let ownSeed = H.fromList [ (k, PTopType) | k <- S.toList ownKeys ]
  (varsVaenvs0, vars0) <- unzip <$> intersectMapVars (H.union parentSeed ownSeed) aVars bVars
  let ownDiscovered = H.fromListWith (intersectTypesEnv typeEnv vaenv)
        [ (n, t) | env <- H.elems varsVaenvs0, (k@(VarKey TVInt (TVVar n)), t) <- H.toList env, S.member k ownKeys, t /= PTopType, not (isTypeVar t) ]
  vars' <- traverse (\(n, v) -> case H.lookup n ownDiscovered of
                        Just d -> let v' = intersectTypesEnv typeEnv vaenv v d in if isBottomType v' then Nothing else Just (n, v')
                        Nothing -> Just (n, v)) (H.toList vars0) >>= Just . H.fromList
  let varsVaenvs = fmap (H.filterWithKey (\k _ -> not (S.member k ownKeys))) varsVaenvs0
  -- Args (unlike vars) are intersected without killing the partial when a value intersects to
  -- bottom: such an arg simply cannot be present. For a *required* arg (a lower-bounded side
  -- lists it) that makes the partial uninhabited; for an *optional* arg (a lower-unbounded mode)
  -- it just drops the arg, leaving the smaller arg-sets of the interval valid.
  -- A pointer occurrence's realizable value-set is bounded by its var's DECLARED bound (the
  -- met var slot): @r -> $T@ under @[/T: ∀Eq]@ realizes @evidence ∩ ∀Eq@ — bare evidence would
  -- DROP the declared requirement (@r=mempty@ giving @∀Monoid@ must store @∀Monoid ∩ ∀Eq@).
  --
  -- The slot is read through 'substituteTVExt', so a slot bound to an EXTERNAL occurrence
  -- (@/T=$_v@) bounds the pointer by that occurrence's ambient VALUE: @Cons[/T=$_v](head=$/T)@
  -- with @v ↦ /Cons@ has a @/Cons@-valued head, and meeting it against a disjoint head must
  -- empty the partial. 'subUnion'\'s pointer clause deliberately does not consult the ambient
  -- env under the pointer's own name (a like-named var of the enclosing scope is a DIFFERENT
  -- var), so this is the only route by which the partial's OWN binding reaches it. Reading such
  -- a slot as "no bound" instead lets the arg meet report the other side verbatim and stamp it
  -- over the parameter the partial already spells, answering with a partial that contradicts
  -- its own operands.
  --
  -- A slot that is STILL a var after resolution is a flexible per-value pointer
  -- ('subUnionVar') and adds no bound.
  -- A slot that is a parent reference (@fn[/F=$^/F]@, a continuation input naming the enclosing
  -- partial's parameter) is bounded by that parameter's binding ('parentRefReading').
  let varBoundFor n = case fmap (substituteTVExt typeEnv vaenv) (H.lookup n vars') of
        Just t@(TypeVar _ TVParent _) | Just b <- parentRefReading typeEnv vaenv t -> b
        Just b | not (isTypeVar b) -> b
        _                          -> PTopType
  -- The nested arg meets run under the ambient env EXTENDED with this partial's own met var
  -- bindings: a TVInt reference to THIS partial's parameter sitting inside a nested value
  -- (@binsert[/MK=Integer](m= …Node[/K=$/MK]…)@ — the parser spells a declaration parameter's
  -- nested occurrence 'TVInt' context-free) is resolvable only through the enclosing partial's
  -- bindings, which the plain ambient env does not carry.
  --
  -- A slot is read through 'substituteTVExt' first, for the reason 'varBoundFor' gives: a slot
  -- bound to an EXTERNAL occurrence contributes its ambient value. What still adds no reading,
  -- after resolution: ⊤, and a value that is STILL a bare var (a genuine unresolved link —
  -- defaulting it would read as ⊤).
  let deepVaenv = H.union (H.fromList [ (VarKey TVInt (TVVar n), r')
                                      | (n, v) <- H.toList vars'
                                      , let r = substituteTVExt typeEnv vaenv v
                                      , let r' = if isTypeVar r then PTopType else r ])
                          -- The fields read this partial's parameters at their OWN location, so an
                          -- enclosing scope's like-named variable is a different entry: an outer
                          -- @$_T@ nested in a field is not this partial's @/T@ (a law body's
                          -- @t ++ inverse(t= t)@ met against @operator++[$T]@ read the law's
                          -- variable as @operator++@'s own and fed the parameter its own binding).
                          -- A parameter whose slot is still a variable or ⊤ reads ⊤ there.
                          vaenv
  -- A higher-order argument's input object reaches this partial's variables through its own
  -- parameters, spelled as parent references (@fn[/B=$^B](acc=$/B)@ — 'hopInputObject'). Its
  -- parameter slots are met in the nested meet, under no scope that says which partial a parent
  -- reference names, so there the reference reads as ⊤ ('substituteTVExt' keeps a parent reference
  -- symbolic at the top) and the meet with a concrete slot (@fn[/B=X+Y](acc=X+Y)@) answered that
  -- slot: outside the first operand, whose @acc@ is @/B@'s @X@. So the input object's parent
  -- references are read here, against this partial's own resolved bindings, before the meet.
  let ownResolved = H.fromList [ (TVVar n, r) | (n, v) <- H.toList vars', let r = substituteTVExt typeEnv vaenv v
                                              , not (isTypeVar r), r /= PTopType ]
  let readInputObjParents k t
        | S.null (pkArgs k) = t
        | otherwise = case t of
            UnionType Nothing leafs cs -> UnionType Nothing (joinUnionType (map onInputObj (splitUnionType leafs))) cs
            _                          -> t
        where
          -- A hop parameter carries the enclosing partial's binding in to the object's arguments,
          -- so the arguments that point at it read that binding (@fn[/A=$^A](v=$/A)@ under
          -- @/A=Integer@ reads @v=$/A∩{Integer}@), and the parameter itself is read as that binding
          -- while staying the parent reference: it is the object's link to the enclosing partial,
          -- and an input object that does not spell it has the same link ('unspelledSubParam').
          -- Replaced by the bare binding it was an input object a call's own @fn(v=Integer)@ is
          -- not within (the call's unspelled @/A@ reads ⊤), though the two accept the same values.
          onInputObj p = p{ ptVars = H.mapWithKey (\n pv -> case (pv, H.lookup n resolved) of
                                                       (TypeVar v TVParent _, Just b) -> TypeVar v TVParent b
                                                       _                              -> pv) (ptVars p)
                          , ptArgs = fmap readHop (ptArgs p) }
            where
              resolved = H.fromList [ (n, meetInner typeEnv vaenv b i) | (n, TypeVar v TVParent i) <- H.toList (ptVars p), Just b <- [H.lookup v ownResolved] ]
              -- The argument stays the pointer, carrying the binding as its inner: it reads the
              -- binding, and a meet it lies within keeps it ('subUnion''s pointer clause) rather
              -- than answering the binding's spelling, which is wider than the pointer.
              readHop (TypeVar v@(TVVar n) TVInt i) | Just b <- H.lookup n resolved = TypeVar v TVInt (meetInner typeEnv vaenv b i)
              readHop x = x
  let argsMap = H.unionWith (subUnionArg varBoundFor) (fmap (deepVaenv,) (H.mapWithKey readInputObjParents aArgs)) (fmap (deepVaenv,) (H.mapWithKey readInputObjParents bArgs))
  let args' = fmap snd argsMap
  -- An arg whose met value denotes ∅ cannot be present (see the arg-mode reasoning below). The
  -- value is judged as the rest of the algebra judges a slot ('partialHasEmptySlot'): RESOLVED
  -- under its env and compacted, not as written. A value that is empty only once its variable
  -- resolves (@$_b ∩ {X}@ with @b@ bound to a type disjoint from @X@) would otherwise stay in the
  -- interval as an inhabited optional arg, and a later compaction would collapse the WHOLE
  -- partial to ∅, where only the arg-sets containing that arg are empty.
  let argDenotesEmpty (env, v) = isBottomType v
        || (not (isTypeVar v) && v /= PTopType && isBottomType (compactType typeEnv env (substituteTVExt typeEnv env v)))
  let bottomKeys = H.keysSet $ H.filter argDenotesEmpty argsMap
  let liveArgsMap = H.filterWithKey (\k _ -> not (S.member k bottomKeys)) argsMap
  -- The args' envs are seeded with the whole incoming vaenv (extended with this partial's met
  -- vars — 'deepVaenv'), so they carry the caller's ambient bindings alongside the meets
  -- discovered here. The discovered bindings must be kept on the
  -- partial's ptVars - they are returned no other way, and they carry a callee's type-var
  -- resolution (@unwrap[/T=Integer]@ from meeting @/b@'s @Box[/T=$_/T]@ with @Box[/T=Integer]@,
  -- which later resolves the passthrough output @$_T@). An ambient binding that merely passed
  -- through unchanged is dropped: it lives in the caller's env, and stamping it onto every
  -- intersected partial invents foreign var slots (an operator's /B appearing on a Cons leaf).
  -- "Passed through" is judged against the SEED env ('deepVaenv'): the met-var extensions above
  -- are seeded, not discovered, so they must not read as fresh discoveries here.
  let ambientVars = fst $ splitVarArgEnv (envByName deepVaenv)
  -- Only a binding some arg value REFERENCES may be hoisted: the reference marks the name as
  -- belonging to THIS partial's scope. The arg meets also record under names with no such
  -- reference — a nested partial's local var resolving per value, or a
  -- class predicate's own var KEY (@∀Monad[/T=..]@ meeting a member binds the class's @/T@) —
  -- and stamping those onto this partial captures a like-named var of an unrelated scope
  -- (@operator==[$T]@ gaining @/T=Integer@ from its operand's element).
  --
  -- HOW an arg references the name decides the set operation combining its discoveries:
  --
  --   * An external ('TVExt') reference NESTED in the value (@unwrap@'s @/b@ valued
  --     @Box[/T=$_T]@) sits in a var slot: var application is EXACT, so every such occurrence
  --     names the same binding and the discoveries MEET ('exactNamesAt').
  --   * A partial-local ('TVInt') reference that is the arg's DIRECT value (a declaration's own
  --     field pointer — @==(l=$T, r=$T)@, @Id(/value=$T)@) is a MEMBERSHIP position: it says
  --     that argument's value is a member of @$T@, an existential LOWER bound on the var.
  --     Discoveries therefore JOIN across occurrences — @==(l=True, r=False)@ binds
  --     @/T=True∪False@, not the uninhabited @True∩False@ ('pointerNamesAt'). The join carries
  --     information only once EVERY occurrence has resolved to evidence: an unresolved
  --     occurrence contributes ⊤ (its future value is unbounded), which keeps the hoist
  --     monotone as occurrences resolve one solver epoch at a time. The joined binding still
  --     MEETS the var slot's declared bound (@/T: ∀Eq@) in the 'venvs'' merge below — the
  --     upper-bound side. (Cross-element @==@ such as @5 == "hi"@ therefore passes inference
  --     with @/T=Integer∪String@ and fails later at instance selection/TreeBuild; making it a
  --     type error would need refinement-precise declarations like
  --     @:$T.equal[$T -> Eq](other -> Eq) | $T.eq == other.eq@ — future type-system work.)
  --
  -- A 'TVInt' reference nested INSIDE an embedded partial (@/l=Cons[$T]@) names the inner
  -- partial's scope and is not hoisted at all.
  let origValuesAt k = mapMaybe (H.lookup k) [aArgs, bArgs]
  -- Every outer reference an arg value carries ('typeOuterVarRefs' reads 'TVOuter' — 'TVParent'
  -- as well as 'TVExt') is a name the meet may discover a binding for, and the discovery is
  -- stamped onto this partial. That is only right when the reference names THIS partial's
  -- parameter (@Cons(tail=∀ConsList[/T=$^/T])@ — the declaration's own linked occurrence); a
  -- value that reached the slot still spelled for another level (@insert@'s result
  -- @Map[/V=$^/MV]@ arriving in @Node@'s @right@ slot) would have @/MV@ stamped onto @Node@
  -- as @Node[/MV=$^/V]@, an invented slot re-derived under a fresh spelling every epoch. So
  -- the rule is not a filter here but at the boundary the value crosses: the slot install
  -- reads such a reference as the ⊤ it denotes before it enters ('updateTypePropInner',
  -- 'detachForeignParentRefs'). Class expansion still relocates a class parameter's value into
  -- the member's arg slots one level deeper than it was written ('substitutePartialExit'),
  -- which is the remaining producer of such a spelling.
  -- Only a reference that names THIS partial counts: a parent reference at the slot's own layer.
  -- An external ('TVExt') reference names the ambient scope, never this partial's parameter, even
  -- when the names coincide — stamping its discovery here is the capture the location-keyed
  -- environment exists to prevent (a law's outer @$_T@ inside @operator++(l=$_T, ..)@ stamping
  -- @operator++@'s own @/T@).
  let exactNamesAt k = S.unions $ map typeLayerVarRefs $ origValuesAt k
  let pointerNamesAt k = S.fromList [n | TypeVar (TVVar n) TVInt _ <- origValuesAt k]
  -- A hoisted binding is stored in its canonical 'substituteTVExt' reading (the same one
  -- 'mergeVarEnvVal' gives every merged env binding), so a binding that only passed through
  -- ONE env is structurally comparable with one that met across several — and with the
  -- difference side's 'resolveOwnScopeRefs' reading. It is read under the ambient @vaenv@ (as
  -- 'resolveOwnScopeRefs' also is), so an external ('TVExt') reference NESTED in the binding
  -- resolves to its ambient value rather than widening to ⊤ (@Cons[/T = P[/a=$_a]]@ with
  -- @$_a@ bound keeps @/a@ at its value); with no ambient binding this is identical to reading
  -- under the empty env. A binding that IS a bare var is a genuine link and stays symbolic (the
  -- reading would default an unbound one to ⊤).
  let canonBinding t@TypeVar{} = t
      canonBinding t           = substituteTVExt typeEnv vaenv t
  -- "Passed through unchanged" is decided on the canonical readings of BOTH sides. An arg env
  -- that merged gives its entry the canonical reading while the ambient entry is the slot's raw
  -- reading, and the reading is not idempotent (it descends into the value a second time: a
  -- member's own @/head=$/T@ reads as that member's @/T@), so comparing a canonical reading
  -- with a raw one would treat an entry that merely passed through as a DISCOVERY. Hoisting it
  -- would materialize the slot's ambient binding into the partial (@Cons[/T=$_c]@ becoming
  -- @Cons[/T=⟦c⟧]@) and relocate that value's own free @$_T@ under the partial's @/T@, where the
  -- next read captures it.
  let ambientCanon = fmap canonBinding ambientVars
      passedThrough n v = H.lookup n ambientCanon == Just (canonBinding v)
  -- A discovery is stamped onto this partial only under a name that is one of ITS parameters —
  -- spelled on either operand, or declared on its data or arrow templates (@unwrap[$T]@'s @/T@).
  -- An external reference nested in an arg value can name something else entirely: the free
  -- @$_a@ of @Cons(tail=Cons[/T=$_a∩Integer])@ is a variable of the enclosing scope, and
  -- stamping its discovery onto the outer @Cons@ invents a @/a@ parameter @Cons@ does not have.
  let ownParams = S.unions $ H.keysSet aVars : H.keysSet bVars
        : [H.keysSet (ptVars t) | t <- typeGraphData typeEnv name' ++ typeGraphArrowTemplates typeEnv name']
  let exactEnvAt k env = fmap canonBinding $ H.filterWithKey (\n v -> n `S.member` exactNamesAt k && n `S.member` ownParams && not (passedThrough n v)) $ fst $ splitVarArgEnv env
  -- What arg @k@'s meet discovered for name @n@: a fresh env entry with actual content. ⊤
  -- carries no information, a var-valued entry is a link rather than evidence, and an entry
  -- identical to the ambient one merely passed through — none resolves the occurrence. Any
  -- other value set is evidence: a function-typed value is as much a data object as declared
  -- data (and declared data is as rewritable — arrows exist between data objects too), so
  -- the algebra draws no distinction between them.
  --
  -- Three outcomes, not two: 'Nothing' WITHHOLDS the whole join (an occurrence that is merely
  -- unresolved may still resolve to something that widens it, so no partial join is monotone),
  -- @Just Nothing@ is VACUOUS (the occurrence constrains nothing and does not withhold, as an
  -- empty container does at a linked slot), and @Just (Just v)@ is evidence.
  let pointerNamed n = any (S.member n . pointerNamesAt) (H.keys liveArgsMap)
  let discoveryAt k n = case H.lookup k liveArgsMap >>= H.lookup n . fst . splitVarArgEnv . envByName . fst of
        Just PTopType                    -> Nothing
        -- A link the enclosing scope passed in ('pointerMeetsLink') is that scope's variable as
        -- the value: evidence that resolves where the link does. Any other var is unresolved.
        Just v@(TypeVar _ TVExt _) | pointerNamed n -> Just (Just v)
        Just TypeVar{}                   -> Nothing
        Just v | not (passedThrough n v) -> Just (Just (canonBinding v))
        _                                -> Nothing
  -- The pointer join for an arg-set piece ranges over that piece's occurrences of the name:
  -- an occurrence on an arg OUTSIDE the piece does not constrain it (the ∅ piece of
  -- @Pair(first=$A, second=$B)??@ keeps @/A@ at its declared bound even when the @first@
  -- piece's meet discovered a binding — 'bindingOptArgs' enumerates the pieces so each
  -- carries exactly its own join). Within a piece, an occurrence present but UNRESOLVED
  -- (including an argument the other side has not applied yet — a required key contributed
  -- by the object alone) withholds the join, which keeps the piece's binding monotone as
  -- the application completes.
  -- A DIRECT declared occurrence whose slot already holds CONTENT is still an occurrence:
  -- its pointer spelling (@this=$T@) was consumed by an earlier meet — one that correctly
  -- withheld the stamp because the occurrence set was incomplete — so its contribution is
  -- read from the content itself, exactly as 'tightenDeclBoundedDims' reads a data value's
  -- occupants ("the pointer spelling ... is consumed the moment content lands there").
  -- Without it the join would range over the still-pointer-spelled occurrences alone and record
  -- a partial join as the var's binding (@compareTo(this=Nothing, other=Some)@ stamping
  -- @/T=Some@ from @other@ alone). Content that is ⊤-carrying or
  -- var-carrying withholds the join (its future narrowing could widen the true join), the
  -- same monotone-withhold as an unresolved pointer discovery. Only the templates' DIRECT
  -- pointer occurrences read content this way; a LINKED occurrence (@tail -> ConsList[$T]@)
  -- reaches the join by its own route ('linkedKsOf' below).
  let exactEnvOf k = maybe H.empty (exactEnvAt k . envByName . fst) (H.lookup k liveArgsMap)
  let declTmpls = typeGraphData typeEnv name' ++ typeGraphArrowTemplates typeEnv name'
  -- A LINKED occurrence of this partial's OWN parameter: the declaration spells the arg as a
  -- container carrying the parameter in one of its own slots — @tail -> ConsList[$T]@ is spelled
  -- @∀ConsList[/T=$^/T]@, a PARENT ('TVParent') reference naming @Cons@'s @/T@ from a nested
  -- position. Such an arg's elements are MEMBERS of the var exactly as a direct @head -> $T@
  -- occurrence's value is, so its contribution belongs in the pointer JOIN: a spine whose
  -- elements differ takes @/T@ from every cell rather than from its head alone. (An EXTERNAL
  -- 'TVExt' reference names an enclosing OBJECT's scope instead and keeps meeting through
  -- 'exactEnvAt' — var application is exact — so only the parent-scope spelling joins here.)
  let selfPartials t = case t of
        UnionType mtop pos _ -> splitUnionType pos ++ maybe [] (\(preds, _, _) -> predsPartials preds) mtop
        _                    -> []
      predsPartials (PredsOne p)  = [predPartialOf p]
      predsPartials (PredsAnd ps) = concatMap predsPartials ps
      predsPartials (PredsNot p)  = predsPartials p
      predPartialOf (PredExpr p)    = p
      predPartialOf (PredClass p)   = p
      predPartialOf (PredRel p)     = p
      predPartialOf (PredConstEq c) = constantPartialType c
  let linkedParamsOf n t = [ pn | pt <- selfPartials t
                           , (pn, TypeVar (TVVar n') TVParent _) <- H.toList (ptVars pt), n' == n ]
  -- A higher-order argument's INPUT OBJECT links its parameters to the declaration's variables too
  -- (@fn[/MV=$^MV](v=$/MV)@), but those parameters are where the variable's values are GIVEN — the
  -- lambda's @v@ takes whatever the call binds @$MV@ to — so they are no evidence of it. Joined as
  -- occurrences, the lambda's still-⊤ parameter withheld @$MV@'s binding, which was the only thing
  -- that could ever type the parameter. Only the argument's RESULT is evidence ('hoResultKsOf').
  let linkedKsOf n ks = L.nub [ (k, pn) | tmpl <- declTmpls, (k, tv) <- H.toList (ptArgs tmpl)
                              , k `S.member` ks, S.null (pkArgs k), pn <- linkedParamsOf n tv ]
  -- A HIGHER-ORDER argument's declared RESULT (@fn(acc -> $_B) -> $_B@) is an occurrence of the
  -- variable too, but not one the object holds: the slot holds the input object, and the result
  -- is the arrow the argument adds to the environment ('hoSlotResult'). Read through that arrow,
  -- a call's lambda gives the content its body reaches — evidence, joined like any occurrence;
  -- a definition's own argument gives back the declared variable itself (its arrow returns the
  -- declaration's @$_B@), which says nothing about the binding — vacuous, as the arm's other
  -- occurrences are then the only evidence (@mmap(m= Cons(..), fn(v))@ binding @$M@ from @m@).
  -- An arrow the environment does not hold yet, or one reaching unresolved content, WITHHOLDS:
  -- its result may still widen the join, and stamping without it narrowed @foldEntries@'s @$B@
  -- to the @Nil@ of @init@ alone while the lambda's @Cons@ was still on its way. An occurrence
  -- nested inside the result (@fn(..) -> List[$T -> $_B]@) withholds too.
  let hoResultKsOf n ks = [ (k, t) | (k, t) <- typeGraphDeclaredArgResults typeEnv name', k `S.member` ks, n `S.member` typeOuterVarRefs t ]
  let resultAt (k, t) = case fmap (hoSlotResult typeEnv vaenv k . snd) (H.lookup k liveArgsMap) of
        Just TypeVar{} -> Just Nothing
        Just r | not (typeHasVars r), not (typeContainsBareTop r), TypeVar{} <- t -> Just (Just r)
        _ -> Nothing
  -- Whether a template spells an argument (not a higher-order argument's input object) as the
  -- parameter itself.
  let directOccurrenceOf n tmpl = or [ n' == n | (k, TypeVar (TVVar n') _ _) <- H.toList (ptArgs tmpl), S.null (pkArgs k) ]
  let pointerJoinFor ks =
        let ksL = S.toList ks
            joinFor n =
              let ptrKs = filter (\k -> n `S.member` pointerNamesAt k) ksL
                  contentKs = [ k | tmpl <- declTmpls, (k, TypeVar (TVVar n') TVInt _) <- H.toList (ptArgs tmpl)
                              , n' == n, k `S.member` ks, k `notElem` ptrKs ]
                  contentAt k = case fmap snd (H.lookup k liveArgsMap) of
                    Just v | not (typeHasVars v), not (typeContainsBareTop v) -> Just v
                    _                                                        -> Nothing
                  -- BOTH readings of the occurrence feed the join: the arg's own binding at the
                  -- linked slot, and whatever the arg's meet discovered for the name through the
                  -- external-reference env. The latter carries the DECLARED bound a nested
                  -- spelling supplies (@tags -> ConsList[$T -> String]@ gives @String@ where the
                  -- literal alone gives the constant @"x"@), so dropping it rather than joining it
                  -- would read a list's element type off its first element.
                  --
                  -- Content still carrying bare ⊤ withholds the whole join — its future narrowing
                  -- could only make the true join WIDER, so stamping a partial join now would not
                  -- be monotone — the same withhold the direct occurrences take. A resolved arg
                  -- carrying no such slot (@Nil@ under @∀ConsList[/T=..]@) is vacuous: an empty
                  -- container constrains no element, so it contributes nothing WITHOUT withholding.
                  -- A linked slot still holding a VARIABLE (@tail=Cons[/T=$^/T]@, the occurrence
                  -- linked back to the very parameter being joined) is unresolved in the same way:
                  -- its elements are whatever the stamp will say, so stamping from the other
                  -- occurrences alone would narrow it: @Cons(head=Integer, tail=…)@ stamping
                  -- @/T=Integer@ would turn an unconstrained tail into an Integer list.
                  linkedAt (k, pn) = case fmap snd (H.lookup k liveArgsMap) of
                    Nothing -> Just []
                    Just v | typeContainsBareTop v -> Nothing
                           | any (maybe False isTypeVar . H.lookup pn . ptVars) (selfPartials v) -> Nothing
                           | otherwise -> Just ([ b | pt <- selfPartials v, Just b <- [H.lookup pn (ptVars pt)]
                                                , not (isTypeVar b), b /= PTopType ]
                                             ++ [ b | Just b <- [H.lookup n (exactEnvOf k)]
                                                , not (isTypeVar b), b /= PTopType ])
              in case ((++) <$> mapM (`discoveryAt` n) ptrKs <*> mapM resultAt (hoResultKsOf n ks), mapM contentAt (L.nub contentKs), mapM linkedAt (linkedKsOf n ks)) of
                (Just mvals, Just cvals, Just lvalss)
                  | let vals = catMaybes mvals
                  , let lvals = concat lvalss
                  , not (null (vals ++ cvals ++ lvals))
                  , let joined = unionAllTypesWithEnv typeEnv vaenv (vals ++ cvals ++ lvals)
                  -- A vacuous occurrence (a definition's own higher-order result, which is the
                  -- variable itself) is missing from the join, which so lacks whatever that
                  -- occurrence will really hold. Reading
                  -- the evidence at INSTANCE granularity is what keeps the binding wide enough
                  -- for it while still naming the instance the arm selected.
                  , let joined' = if any isNothing mvals
                                    then boundClassInstancesFor typeEnv vaenv (varBoundFor n) joined
                                    else joined
                  -> Just (n, joined')
                _ -> Nothing
        -- The names considered are the DIRECT pointer occurrences': a linked occurrence AUGMENTS
        -- a join that a direct occurrence already establishes, and does not start one of its own.
        -- Ranging over linked names too would stamp a var no direct occurrence stamps — the
        -- content of a single-element literal ("x") standing in for the element type its
        -- declaration gives (@String@), which is a narrowing, not a widening.
        --
        -- A parameter the declaration gives NO direct occurrence is the exception: its linked
        -- occurrences are all it has, so they start its join. @foldr[$A, $B](lst -> List[$T ->
        -- $_A], fn(acc -> $_B, v -> $_A) -> $_B, ..)@ has @$A@ only inside @lst@ (and at the
        -- lambda's @v@, where it is given rather than evidenced), and without its join the
        -- lambda's @v@ never learned the list's element type.
            linkedOnlyNames = S.fromList [ n | n <- L.nub (concatMap (H.keys . ptVars) declTmpls)
                                             , not (any (directOccurrenceOf n) declTmpls)
                                             , not (null (linkedKsOf n ks)) ]
        in H.fromList $ mapMaybe joinFor $ S.toList $ S.unions (map pointerNamesAt ksL) <> linkedOnlyNames
  -- A name an arg contributes to the pointer JOIN through a LINKED (parent-scope) occurrence must
  -- not ALSO meet through the exact-reference env: both readings describe the SAME occurrence, and
  -- the meet would pull the join straight back down (@/T = (K/A + K/B) ⊓ K/B = K/B@ — the head's
  -- element lost again), which is precisely what the join exists to prevent. The occurrence is a
  -- membership position, so the join is its reading. Every OTHER name on the arg — an external
  -- 'TVExt' reference naming an enclosing object's scope — meets through the exact env.
  let exactEnvOfJoined ks k = H.filterWithKey (\n _ -> null [() | (k', _) <- linkedKsOf n ks, k' == k]) (exactEnvOf k)
  -- EVERY hoisted stamp — the pointer-occurrence join and the external-reference discoveries
  -- alike — is gated on every DECLARED occurrence of the name supporting it
  -- ('varStampSupported'): the declaration ties e.g. a recursive @tail@'s content to the var,
  -- and stamping past unresolved content there is inexact (it launders the content's bare ⊤
  -- into the binding's value through the instantiated declared bound). The difference
  -- machinery obeys the same rule ('narrowPiece'), so both op
  -- families spell the unsupported case identically.
  let hoistFor ks = H.filterWithKey (\n _ -> varStampSupported typeEnv name' [aArgs, bArgs] ks args' (TVVar n)) $
        mergeAllVarEnvs typeEnv $ pointerJoinFor ks : map (exactEnvOfJoined ks) (S.toList ks)
  let argsVarBindings = hoistFor $ H.keysSet liveArgsMap
  -- A parameter the arguments bind (@m -> $/M@ binding @/M@ to the call's @m@) reaches its value
  -- only here, after the var meets read its siblings off its bound ('ownDiscovered' above). So the
  -- bound is read again against the bound value: what @/M=∀Opt[/T=X]@ says against
  -- @∀Mon[/T=$^A]@ is that @/A=X@. Only the bound's own parent references are read, so this binds
  -- siblings and nothing else.
  let withSiblings vs = H.mapWithKey (\n v -> case H.lookup n (siblingsOf vs) of
                                               Just d -> intersectTypesEnv typeEnv vaenv v d
                                               Nothing -> v) vs
      siblingsOf vs = H.fromListWith (intersectTypesEnv typeEnv vaenv)
        [ (n', t)
        | (n, v) <- H.toList vs, Just b <- [H.lookup n vars'], v /= b
        , let (env, _) = intersectTypesWithVarEnv typeEnv (H.union parentSeed ownSeed) v b
        , (k@(VarKey TVInt (TVVar n')), t) <- H.toList env, S.member k ownKeys, n' /= n
        , t /= PTopType, not (isTypeVar t) ]
  let venvs' = withSiblings $ mergeAllVarEnvs typeEnv [argsVarBindings, vars']
  let preds' = predsAnd aPreds bPreds
  -- The two arg-sets each constrain the instance args to an interval over the
  -- arg-name subset lattice (see 'PtArgMode'). Their intersection is the interval
  -- [reqKeys, capKeys]: the required (lower) keys are the union of each side's
  -- required keys ('ptRequiredArgs'); the cap (upper) keys are the intersection of each side's cap.
  -- Every listed key is typed where present, by the meet of what each side lists for it.
  -- Bottom-valued keys can never be present, so they are removed from the cap (and make the
  -- partial uninhabited if required - see the 'partials'' guard below).
  let okKeys = H.keysSet args' `S.difference` bottomKeys
  let reqKeys = ptRequiredArgs aP `S.union` ptRequiredArgs bP
  let ub' = aUb || bUb
  let capKeys = ((if aUb then H.keysSet aArgs else okKeys) `S.intersection` (if bUb then H.keysSet bArgs else okKeys)) `S.difference` bottomKeys
  let restrict ks = H.filterWithKey (\k _ -> k `S.member` ks) args'
  let mkPartial lb' ub'' ks = PartialType name' venvs' (restrict ks) preds' (PtArgMode lb' ub'' S.empty)
  -- A var binding hoisted from an OPTIONAL arg's meet (the arg is outside reqKeys) holds
  -- only for the arg-sets CONTAINING that arg — stamping it on the whole interval would
  -- narrow the arg-sets missing the arg (e.g. @/T=X@ from a @head@ meet constraining the
  -- argless piece). No single ptVars fits, so such an interval is enumerated with each
  -- arg-set carrying exactly the bindings ITS OWN args' meets discovered.
  let bindingOptArgs = S.filter (\k -> not (H.null (hoistFor (S.singleton k)))) (okKeys `S.difference` reqKeys)
  let venvsFor ks = withSiblings $ mergeAllVarEnvs typeEnv [hoistFor ks, vars']
  let mkPartialAt ks = PartialType name' (venvsFor ks) (restrict ks) preds' PtArgExact
  -- Upper-unbounded: the keys outside reqKeys that either side lists are OPTIONAL — an instance
  -- may lack one, and where it has one it is typed by the meet ('PtArgMode'). One that met ∅ is
  -- one no instance has: it is kept, typed ∅, since unlisted it would be an extra of any value.
  -- An optional arg whose meet bound a var is split as above: a piece where it is present, with
  -- its bindings, and one where it is absent, without them.
  let unboundedAt present =
        let absent = bindingOptArgs `S.difference` present
            typed = H.union (H.fromList [(k, BottomType) | k <- S.toList (bottomKeys `S.union` absent)]) (restrict okKeys)
            req = reqKeys `S.union` present
        in canonicalArgOpt typeEnv $ intervalLowerBound typeEnv $ PartialType name' (venvsFor (req `S.union` (okKeys `S.difference` bindingOptArgs))) typed preds' (PtArgMode (not (S.null req)) False (H.keysSet typed `S.difference` req))
  let partials'
        | not (S.null (reqKeys `S.intersection` bottomKeys)) = [] -- a required arg is uninhabited
        | ub' = if reqKeys `S.isSubsetOf` capKeys -- bounded interval [reqKeys, capKeys]
            then if reqKeys == capKeys
              then [mkPartial True True capKeys]         -- single arg-set: exact
              else if S.null reqKeys && S.null bindingOptArgs
                then [mkPartial False True capKeys]      -- [∅, capKeys]: powerset
                -- strict bounded interval (or optional-arg bindings): enumerate each
                -- arg-set as an exact partial with its own arg-derived bindings
                else [mkPartialAt (reqKeys `S.union` S.fromList extra) | extra <- powerset $ S.toList (capKeys `S.difference` reqKeys)]
            else []                                       -- infeasible: required exceeds cap
        | otherwise = [unboundedAt (S.fromList present) | present <- powerset (S.toList bindingOptArgs)]
  if null partials' then Nothing else Just (parentToOwn (mergeAllVarEnvs typeEnv varsVaenvs), partials')
  where
    -- What a parameter meet recorded under the 'TVParent' layer names the ENCLOSING partial's
    -- parameter, which in the caller's frame is its own layer ('varKeyIn' at 'TVInt'); an entry
    -- still the ⊤ the seed gave it says nothing.
    parentToOwn env = H.fromList
      [ (k', t)
      | (k, t) <- H.toList env
      , let k' = case k of
              VarKey TVParent v -> varKeyIn vaenv TVInt v
              _                 -> k
      , not (vkLoc k == TVParent && t == PTopType) ]
    -- intersectMap unions so that all typeVars from either a or b are kept
    -- The key maps are UNIONed: a parameter only one side spells is ⊤ on the other (see
    -- 'PartialType'), and @⊤ ⊓ x = x@, so the spelled value survives unchanged.
    intersectMapVars vev a b = traverse subValidate $ H.unionWith subUnionVar (fmap (vev,) a) (fmap (vev,) b)
    -- A bare partial-local ('TVInt') reference stored in a VAR slot is a pointer within the
    -- partial itself (a partial-local element var, flexible per value): it is
    -- flexible per value and must neither read nor record ambient bindings under its name — a
    -- like-named var of the enclosing scope is a DIFFERENT var. The meet is the other side.
    -- EXCEPT a name this partial does NOT declare: that is an ENCLOSING scope's parameter,
    -- TVInt-spelled because the parser spells a bare @$U@ context-free (the scope axis lives in
    -- the descent rule, not the leaf rule — see 'substituteTVExt'). Its ambient reading is the
    -- slot's meaning: when the reading already lies within the other side, KEEP the symbolic
    -- link (the frame-portable spelling — taking the other side would replace the call frame's
    -- narrow reading with another frame's materialized bound, e.g. @Node[/K=$/MK]@ under
    -- @MK↦Integer@ met with an arm-frame @Node[/K=∀Ord]@ must stay @$/MK@, not widen to @∀Ord@);
    -- otherwise the meet of the reading with the other side. An unbound/⊤/var reading keeps the
    -- flexible-local behavior.
    declaredVarKeys = S.union (H.keysSet aVars) (H.keysSet bVars)
    hoppedVars = S.fromList [ w | TypeVar (TVVar w) TVParent _ <- H.elems aVars ++ H.elems bVars ]
    ambientReading n = case H.lookup (extKey (TVVar n)) vaenv of
      Just r | not (isTypeVar r), r /= PTopType -> Just r
      _                                         -> Nothing
    subUnionVar (aVaenv, a@(TypeVar (TVVar n) TVInt _)) (bVaenv, b)
      | not (S.member n declaredVarKeys)
      , Just r <- ambientReading n
      = let env = mergeAllVarEnvs typeEnv [aVaenv, bVaenv]
        in if isSubtypeOfWithEnv typeEnv vaenv r b then (env, a) else (env, intersectTypesEnv typeEnv vaenv r b)
    subUnionVar (aVaenv, a) (bVaenv, b@(TypeVar (TVVar n) TVInt _))
      | not (isTypeVar a)
      , not (S.member n declaredVarKeys)
      , Just r <- ambientReading n
      = let env = mergeAllVarEnvs typeEnv [aVaenv, bVaenv]
        in if isSubtypeOfWithEnv typeEnv vaenv r a then (env, b) else (env, intersectTypesEnv typeEnv vaenv r a)
    subUnionVar (aVaenv, TypeVar _ TVInt i) (bVaenv, b) = (mergeAllVarEnvs typeEnv [aVaenv, bVaenv], intersectTypesEnv typeEnv vaenv b i)
    subUnionVar (aVaenv, a) (bVaenv, TypeVar _ TVInt i) = (mergeAllVarEnvs typeEnv [aVaenv, bVaenv], intersectTypesEnv typeEnv vaenv a i)
    -- A slot holding an EXTERNAL occurrence denotes its ambient binding (met with the
    -- occurrence's own inner — 'Type'), so the meet of two such slots is the meet of their
    -- VALUES. The var meets are seeded with the empty env ('intersectMapVars'), unlike the ARG
    -- meets ('deepVaenv'), so without this clause both sides would read as ⊤ ('vaenvLookup'
    -- defaults a miss to ⊤) and a disjointness between the two bindings would be lost:
    -- @P[/v=$_b] ∩ P[/v=$_a]@ with @b@ and @a@ bound to disjoint types must be ∅, and must be
    -- the same whichever operand comes first.
    --
    -- The rule is the one the 'TVInt' clauses above already apply to an enclosing scope's
    -- parameter: when the reading already lies within the other side, KEEP the symbolic link
    -- (the frame-portable spelling — materializing it would replace the call frame's narrow
    -- reading with another frame's bound); otherwise the meet of the reading with the other side.
    --
    -- 'TVParent' is deliberately NOT included. A parent reference names the nearest enclosing
    -- partial's parameter and is never capturable by a like-named ambient binding
    -- ('TypeVarLoc'); reading one through the ambient env is the capture 'genParentVarBoundPair'
    -- exists to catch.
    subUnionVar (aVaenv, a) (bVaenv, b)
      | Just r <- outerReading a
      = let env = mergeAllVarEnvs typeEnv [aVaenv, bVaenv]
        in if isSubtypeOfWithEnv typeEnv vaenv r b then (env, a) else (env, intersectTypesEnv typeEnv vaenv a b)
    subUnionVar (aVaenv, a) (bVaenv, b)
      | Just r <- outerReading b
      = let env = mergeAllVarEnvs typeEnv [aVaenv, bVaenv]
        in if isSubtypeOfWithEnv typeEnv vaenv r a then (env, b) else (env, intersectTypesEnv typeEnv vaenv a b)
    -- A parent reference reads the ENCLOSING partial's binding, which that partial's meet passes
    -- down under its own key ('deepVaenv'), never an ambient one: the same keep-the-link rule then
    -- applies (a continuation input's @/F=$^/F@ met with the spelled-out @/F=∀Fun[..]@ it lies
    -- within stays the link).
    subUnionVar (aVaenv, a) (bVaenv, b)
      | inputObject
      , Just r <- parentReading a
      , isSubtypeOfWithEnv typeEnv vaenv r b
      = (mergeAllVarEnvs typeEnv [aVaenv, bVaenv], a)
    subUnionVar (aVaenv, a) (bVaenv, b)
      | inputObject
      , Just r <- parentReading b
      , isSubtypeOfWithEnv typeEnv vaenv r a
      = (mergeAllVarEnvs typeEnv [aVaenv, bVaenv], b)
    subUnionVar a b = subUnion (const PTopType) a b
    parentReading = parentRefReading typeEnv vaenv
    -- Within an input object a pointer met with what it does not lie within stays the pointer,
    -- carrying the meet as its inner, and records no binding: the input object's parameters are
    -- what the declaration passes, not occurrences of the variable it names (the data pins that),
    -- so a lambda's @fn(v= ∀Ord)@ does not bind @filter@'s @/A@ to @∀Ord@.
    -- The pointer- and link-keeping clauses are a higher-order argument's INPUT OBJECT's
    -- (@fn[/F=$^/F](v=$/F)@ — a name no data type, class or arrow declares): there the link is what a
    -- call binds, and spelling it out loses which value the continuation is given. A data
    -- partial's or a call's pointers keep the established reading: a call's arguments and a
    -- pattern's fields take the values the meet finds (@Cons[/T=∀String](head=∀String)@), which
    -- is what their bindings and the trees built from them read.
    -- An input object's parameters are its hops: parent references to the enclosing partial's
    -- (@fn[/A=$^/A]@). That is checked as well as the name, since not every env a meet runs in
    -- carries the arrow templates that would rule out a call's own name.
    inputObject = any isHop (H.elems aVars ++ H.elems bVars)
      && null (typeGraphData typeEnv name') && null (typeGraphArrowTemplates typeEnv name') && not namesClass
    isHop (TypeVar _ TVParent _) = True
    isHop _                      = False
    namesClass = case teClassGraph typeEnv of
      ClassGraph cg -> case graphLookup (PClassName name') cg of
        Just CGClass{} -> True
        _              -> False
    keepsPointer env reading other = inputObject && reading /= PTopType && isSubtypeOfWithEnv typeEnv env reading other
    -- The BINDING an external occurrence resolves to, or Nothing when the variable is unbound,
    -- bound to ⊤ (indistinguishable from unbound) or bound to another variable — in each of
    -- those the occurrence adds no reading and the flexible per-value behaviour of 'subUnion' is
    -- right. Exactly 'ambientReading', keyed on the occurrence's own aux so a 'TVArg'-auxed
    -- reference resolves too.
    --
    -- The occurrence's own inner is deliberately NOT met in here, for the reason the 'TVInt'
    -- clauses above leave it out as well: an inner can hold free variables of ITS scope, and
    -- meeting it against an env that does not bind them resolves them to ⊤ and widens the
    -- occurrence (@Cons[/T=$_a∩{∀List[/T=$_T]}]@ would become @Cons[/T=$_a∩{∀List[/T=⊤]}]@).
    -- The reading is used only
    -- to decide WHETHER the link may stay symbolic (@r ⊆ other@ is sufficient for that, since
    -- the occurrence denotes @r ∩ inner ⊆ r@); the meet itself is taken on the whole occurrence,
    -- which carries its inner.
    outerReading (TypeVar aux TVExt _) = case H.lookup (extKey aux) vaenv of
      Just r | not (isTypeVar r), r /= PTopType -> Just r
      _                                         -> Nothing
    outerReading _                              = Nothing
    -- A bare partial-local ('TVInt') pointer as an ARG value is the declaration's own field
    -- linkage (@l -> $T@). Like 'subUnionVar' for var slots, it must neither read nor record
    -- the AMBIENT binding under its name — a like-named var of the enclosing scope is a
    -- DIFFERENT var (the generic var clause of 'intersectTypesWithVarEnv' would meet the
    -- evidence against the scope's binding and hide the discovery under the pre-existing
    -- key). Its meet with the other side is that side clamped to the var's declared bound:
    -- the slot keeps the occurrence's realizable set (@A∩B ⊆ A@ requires it even when the
    -- var-level join withholds — @++(/l=Integer) ∩ ++[/T=∀Semigroup](/l=$/T, /r=$/T)@ must
    -- keep @/l=Integer@, and a RAW applied value keeps its shapes with the bound's predicate
    -- distributed onto them rather than reading as the bare bound). The clamped value is
    -- recorded under the pointer's name for the pointer-occurrence join ('pointerJoinFor',
    -- via 'discoveryAt').
    subUnion _ (aVaenv, a@(TypeVar (TVVar _) TVInt _)) (bVaenv, b)
      | a == b = (mergeAllVarEnvs typeEnv [aVaenv, bVaenv], a)
    -- A pointer whose reading (its variable's bound met with its inner) already lies within the
    -- other side is that side's meet with it unchanged, so it stays a pointer, as the 'subUnionVar'
    -- clauses keep a link: replaced by its evidence, a continuation input's @v=$/F@ met with the
    -- spelled-out @v=∀Fun[..]@ would become the wider spelling, though it lies within it. A kept
    -- pointer is no new evidence, so it records no binding (one recorded would be resolved back
    -- into the slot by the enclosing meet, spelling the pointer out after all).
    subUnion varBound (aVaenv, a@(TypeVar (TVVar n) TVInt i)) (bVaenv, b)
      | not (isTypeVar b), b /= PTopType = let env = mergeAllVarEnvs typeEnv [aVaenv, bVaenv]
                                               reading = intersectTypesEnv typeEnv env (varBound n) i
                                               ev = intersectTypesEnv typeEnv env b reading
                                           in if keepsPointer env reading b then (env, a)
                                              else if inputObject then (env, TypeVar (TVVar n) TVInt ev)
                                              else (H.insert (VarKey TVInt (TVVar n)) ev env, ev)
    subUnion varBound (aVaenv, a) (bVaenv, b@(TypeVar (TVVar n) TVInt i))
      | not (isTypeVar a), a /= PTopType = let env = mergeAllVarEnvs typeEnv [aVaenv, bVaenv]
                                               reading = intersectTypesEnv typeEnv env (varBound n) i
                                               ev = intersectTypesEnv typeEnv env a reading
                                           in if keepsPointer env reading a then (env, b)
                                              else if inputObject then (env, TypeVar (TVVar n) TVInt ev)
                                              else (H.insert (VarKey TVInt (TVVar n)) ev env, ev)
    -- The slot VALUES are read in the ambient scope the partial sits in: an external reference
    -- inside one (@x=⊤ − P[/v=$_a]@) denotes its ambient binding, as the 'subUnionVar' clauses
    -- above read it; met under the empty seed alone it would read as unbound, so @P[/v=⊤]@ would
    -- fall inside the excluded leaf whatever @a@ is bound to. The ambient env is only READ: what
    -- the slot returns is still its own discoveries — the seed's entries, new bindings, and an
    -- ambient binding the meet narrowed.
    --
    -- The field meet's freshly-discovered CONCRETE bindings are resolved back into its TYPE (the
    -- same resolve-meets-back rule 'intersectTypesEnv' applies at the top level). A recursive-tail
    -- field @tail -> ∀ConsList[/T=$_T]@ met with a concrete @Cons[/T=Integer]@ discovers
    -- @/T ↦ Integer@ but 'intersectTypesWithVarEnv' returns the bare var; taking @snd@ alone would
    -- leave the deep tail free, falling to the requirement's declared bound (@Ord@) and
    -- abstracting a concrete list's element at depth. Concrete-only, so a genuine var link (a
    -- polymorphic parameter tying positions together) is left as a var.
    subUnion _ (aVaenv, a) (bVaenv, b) =
      let inEnv = mergeAllVarEnvs typeEnv [aVaenv, bVaenv]
          -- Only the ambient bindings the values name EXTERNALLY are read, and none a value also
          -- names as a parent reference: the meet keys a reference by name, so an ambient @/T@
          -- in view would capture a slot's @$^/T@ (the enclosing partial's parameter), as
          -- 'foldDiscovered' documents.
          readEnv = H.union inEnv (externalReadEnv vaenv [a, b])
          (outEnv, t) = intersectTypesWithVarEnv typeEnv readEnv a b
          -- A binding the meet DISCOVERED: a fresh key, or one the environment held only as the ⊤ a
          -- partial's own parameter is seeded with ('intersectPartialsBaseN') that the meet narrowed.
          newBindings = H.filter (not . isTypeVar) $ discoveredBindings outEnv readEnv
          ambientUnchanged = H.difference vaenv inEnv
          ownEnv = H.differenceWith (\o r -> if o == r then Nothing else Just o) outEnv ambientUnchanged
      in (ownEnv, if H.null newBindings then t else substituteKeepReading typeEnv outEnv newBindings t)
    -- An EXTERNAL occurrence against the pointer is content too: a value the enclosing scope
    -- passes as a link to one of its own variables (@idf(/x=$_/Q)@ against @idf[/T](/x=$/T)@ —
    -- a call in a definition body, passing the definition's @y -> $Q@). Read as the ⊤ its
    -- binding may still be, the meet would keep the pointer and bind @/T@ to nothing, and the
    -- arrow's result (@-> $T@) would lose the caller's @$Q@. It is clamped to the var's declared
    -- bound as any content is, by meeting the bound into the occurrence's inner, and recorded
    -- under the pointer's name for the join, where a link is evidence ('discoveryAt').
    -- Argument slots only: a parameter slot's pointer is the partial's own per-value variable
    -- ('subUnionVar'), never a declaration's field linkage.
    -- A link to the variable one of this partial's parameters is a parent reference to is that
    -- parameter's own variable: a higher-order argument's input object hops the declaration's
    -- @$A@ in as its @/A=$^A@, and its @v -> $_A@ names the same @$A@ (@steps[$A](fn(v -> $_A))@).
    -- Read as content it would bind the parameter to itself. A link that merely shares a
    -- parameter's NAME is another scope's variable like any other ('unitOuterVarNotCaptured').
    subUnionArg varBound (aVaenv, TypeVar (TVVar n) TVInt i) (bVaenv, TypeVar w TVExt j)
      | foreignLink w = pointerMeetsLink varBound n i aVaenv bVaenv w j
    subUnionArg varBound (aVaenv, TypeVar w TVExt j) (bVaenv, TypeVar (TVVar n) TVInt i)
      | foreignLink w = pointerMeetsLink varBound n i aVaenv bVaenv w j
    subUnionArg varBound a b = subUnion varBound a b
    foreignLink (TVVar w)       = not (S.member w hoppedVars)
    foreignLink TVArg{}         = True
    foreignLink (TVApplied b _) = foreignLink b
    subValidate (vev, subTp) = if isBottomType subTp then Nothing else Just (vev, subTp)
    -- The link is kept as it is when what it reads already lies within the bound (the
    -- frame-portable spelling, as 'subUnionVar' keeps one): the clamp says nothing more of it.
    -- Otherwise the bound narrows the occurrence's inner, and a reading disjoint from it leaves
    -- the slot empty.
    pointerMeetsLink varBound n i aVaenv bVaenv w j =
      let env = mergeAllVarEnvs typeEnv [aVaenv, bVaenv]
          clamp = intersectTypesEnv typeEnv env (varBound n) i
          reading = fromMaybe PTopType (outerReading (TypeVar w TVExt j))
          inner | isSubtypeOfWithEnv typeEnv vaenv reading clamp = j
                | otherwise = meetInner typeEnv env j clamp
          ev = TypeVar w TVExt inner
      in if isBottomType inner || isBottomType (intersectTypesEnv typeEnv vaenv reading clamp)
           then (env, BottomType)
           else (H.insert (VarKey TVInt (TVVar n)) ev env, ev)

intersectPartials :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> (TypeVarArgEnv, [PartialType])
intersectPartials typeEnv vaenv a b = case intersectPartialsBase typeEnv vaenv a b of
  Just (vaenv', partials') -> (vaenv', partials')
  Nothing                  -> (vaenv, [])

intersectPartialLeafsWithVarEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialLeafs -> PartialLeafs -> (TypeVarArgEnv, PartialLeafs)
intersectPartialLeafsWithVarEnv typeEnv vaenv aPartials bPartials = (vaenv', type')
  where
    intersected = H.intersectionWith (\as bs -> [intersectPartials typeEnv vaenv a b | a <- as, b <- bs]) (splitUnionTypeByName aPartials) (splitUnionTypeByName bPartials)
    -- The result env must EXTEND the given one, not replace it: a caller folding several
    -- intersections (e.g. 'expandPredsWithNamesFull' over a predicate conjunction) threads the
    -- returned env into the next step, and a binding recorded by an earlier meet (@/B ↦ Integer@)
    -- must still constrain a later meet of the same var (@$_/B ∩ ∀Eq@) - shared keys merge by
    -- intersection in 'mergeVarEnvs', so knowledge only narrows.
    -- The pairwise intersections, however, are the ALTERNATIVES of distributing ∩ over the
    -- unions (d ∩ (l1 + l2) = d∩l1 + d∩l2), so a var bound by several inhabited alternatives
    -- holds their JOIN across pairs - meeting them would fabricate an empty binding whenever
    -- the alternatives disagree, e.g. a raw applied term kept alongside its settled sibling
    -- binding an output-contract var ($_A ∩ /operator+(..) in one leaf, $_A ∩ Integer in the
    -- other). Positions within one alternative still meet (inside 'intersectPartialsBase'),
    -- and an uninhabited alternative binds nothing.
    liveEnvs = [e | (e, ps) <- concat (H.elems intersected), not (null ps)]
    joinedAlternatives = case liveEnvs of
      []  -> H.empty
      [e] -> e
      es  -> foldr1 (H.unionWith (unionTypesWithEnv typeEnv vaenv)) es
    vaenv' = mergeAllVarEnvs typeEnv [vaenv, joinedAlternatives]
    -- A pairwise leaf meet can bind an external var while its leaf keeps the var pointer
    -- (e.g. @Some[/T=operator+..] ∩ Some[/T=$_/T]@ records @$_/T ↦ operator+..@ yet leaves the leaf
    -- as @Some[/T=$_/T]@, WIDER than the operand). Resolve each pair's freshly recorded bindings
    -- back into its OWN leafs so the result is the true meet — per pair (not the joined @vaenv'@),
    -- since each pair is a distinct distributed alternative with its own binding. @vaenv'@ itself is
    -- returned unchanged for the caller's downstream threading.
    resolvePairLeafs (e, ps)
      | H.null nb = ps
      | otherwise = case substituteKeep typeEnv nb (UnionType Nothing (joinUnionType ps) []) of
                      UnionType Nothing leafs' _ -> splitUnionType leafs'
                      _                          -> ps
      where nb = discoveredBindings e vaenv
    type' = joinUnionTypeByName $ fmap (concatMap resolvePairLeafs) intersected

-- |
-- Takes the intersection of two 'Type'.
-- It uses the 'TypeVarEnv' for type variable arguments and determines any possible changes to the surrounding 'TypeVarEnv'.
intersectTypesWithVarEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> (TypeVarArgEnv, Type)
intersectTypesWithVarEnv TypeEnv{teDebug=True} _ l r | trace (printf "[INTERSECT] %s ∩ %s" (show l) (show r)) False = undefined
intersectTypesWithVarEnv _ vaenv PTopType t = (vaenv, t)
intersectTypesWithVarEnv _ vaenv t PTopType = (vaenv, t)
-- ⊥ absorbs: @X ∩ ⊥ = ⊥@ for any @X@. Must precede the 'TypeVar' clause below, which would
-- otherwise intercept @$a ∩ ⊥@ and record @$a ↦ ⊥@ while returning the bare @$a@ — reading as ⊤
-- once the env is dropped (a self-difference @$a − $a = $a ∩ ¬$a@ then wrongly keeps the var).
intersectTypesWithVarEnv _ vaenv _ BottomType = (vaenv, BottomType)
intersectTypesWithVarEnv _ vaenv BottomType _ = (vaenv, BottomType)
intersectTypesWithVarEnv _ vaenv t1 t2 | t1 == t2 = (vaenv, t1)
intersectTypesWithVarEnv _ vaenv t1@(UnionType (Just top1) p1 []) (UnionType (Just top2) _ _)
  | H.null p1, top1 == top2 = (vaenv, t1)
intersectTypesWithVarEnv _ vaenv (UnionType (Just top1) _ _) t2@(UnionType (Just top2) p2 [])
  | H.null p2, top1 == top2 = (vaenv, t2)
-- Two occurrences of the SAME variable in the SAME scope: the meet is that variable bounded by
-- BOTH occurrences' inners. An occurrence denotes @⟦var⟧ ⊓ ⟦inner⟧@, so
-- @(v ⊓ i) ⊓ (v ⊓ i') = v ⊓ (i ⊓ i')@ — the inners meet and the variable stays symbolic, exactly
-- as the general clause below keeps it.
--
-- Without this the general clause returns its LEFT operand verbatim, silently discarding the
-- right operand's inner. That is not merely imprecise: when the discarded occurrence carried the
-- NARROWER inner the result is WIDER than an operand, so the meet is unsound and its answer
-- depends on argument order.
--
--   vaenv = {a ↦ /X(f=⊤)}
--   $_a∩{/X} ∩ $_a  =  $_a∩{/X}      (correct)
--   $_a ∩ $_a∩{/X}  =  $_a           (WRONG — wider than the right operand)
--
-- which would propagate to @a ∩ (b∪c) ⊄ a@ through the mixed-distribute clause below, since that
-- clause can reach the meet with the operands in the losing order.
--
-- Scope class is part of the identity: a 'TVInt' and a 'TVExt' spelling of one NAME are different
-- variables (partial-local vs ambient), so both the aux and the location must match. A mismatched
-- pair falls through to the general clause unchanged.
intersectTypesWithVarEnv typeEnv vaenv (TypeVar v loc i) (TypeVar v' loc' i')
  | v == v' && loc == loc' =
      let inner' = meetInner typeEnv vaenv i i'
      in if isBottomType inner' then (vaenv, BottomType) else (vaenv, TypeVar v loc inner')
-- TypeVar: constrain in vaenv.  Treats 'TVVar' and 'TVArg' uniformly: both look up in vaenv,
-- and an unknown key is recorded as a fresh constraint.  TVInt/TVExt is informational here:
-- a TVInt that reaches this top level was introduced as a fresh sentinel by an upstream
-- caller (e.g. a constituent's prop ref) and is constrained like any other reference.
-- A binding equal to the var itself (a self-referential entry an unconstrained var may
-- acquire) carries no constraint: treat it as absent and bind to @t@, rather than
-- intersecting the binding with @t@ - which would re-enter this clause unchanged forever.
-- Must precede the mixed-distribute clauses: the var's constraint is the WHOLE other
-- operand; distributing @$a ∩ (top ∪ pos)@ would record @$a@ against each union ALTERNATIVE
-- separately and the env merge would MEET them ('mergeVarEnvs'), narrowing the binding to
-- @top ∩ pos@.
--
-- The occurrence's own inner takes part in the EMPTINESS decision but is not folded into the
-- result: an occurrence denotes @⟦var⟧ ⊓ ⟦inner⟧@, so the meet is empty as soon as
-- @binding ⊓ inner ⊓ t@ is, even when @binding ⊓ t@ alone is inhabited. Leaving the inner out
-- of that test lets an occurrence survive a meet with a type it cannot possibly hold, and a
-- self-difference @x − x@ (which is @x ∩ ¬x@, where the complement DOES resolve the inner)
-- then returns @x@ instead of ∅. The returned occurrence keeps its inner untouched rather than
-- absorbing @t@: narrowing it here would make the meet a producer of occurrence-local knowledge
-- on every constraint step, which grows without bound in the solver.
intersectTypesWithVarEnv typeEnv vaenv tv@(TypeVar v loc inner) t
  | Just l <- H.lookup kv vaenv, not (isSelfVar v l) =
      let (e, i) = intersectTypesWithVarEnv typeEnv vaenv l t
      in if isBottomType i || innerRejects i then (vaenv, BottomType)
         else case t of
           -- The other operand is a DIFFERENT variable: the meet is @⟦v⟧ ⊓ ⟦w⟧@ bounded by
           -- both inners, and must read so once the env is dropped whichever operand came
           -- first. Returning @v@ bare would read as @v@'s own binding — wider than the meet
           -- unless @⟦v⟧ ⊆ ⟦w⟧@ — and which var survived would depend on operand order.
           --
           --   ⟦w⟧ ⊆ ⟦v⟧  (the meet left w's binding as it was)  → w's occurrence
           --   ⟦v⟧ ⊆ ⟦w⟧  (w's binding narrowed to all of v's)    → v's occurrence
           --   neither                                             → v's occurrence bounded by
           --                                                         the narrowed binding
           TypeVar w lw j
             | let wk = varKeyIn vaenv lw w, wk /= kv, TypeVar w' lw' _ <- i, varKeyIn vaenv lw' w' == wk ->
                 let bothInners = meetInner typeEnv vaenv inner j
                     wNew = fromMaybe PTopType (H.lookup (varKeyIn vaenv lw w) e)
                     (occ, bound)
                       | bindingUnchanged e lw w = (TypeVar w lw, bothInners)
                       | isSubtypeOfWithEnv typeEnv vaenv l wNew = (TypeVar v loc, bothInners)
                       | otherwise = (TypeVar v loc, meetInner typeEnv vaenv bothInners wNew)
                 in if isBottomType bound then (vaenv, BottomType) else (H.insert kv i e, occ bound)
           _ | recursive i || innerSelf -> (e, onOccurrence)
           _ -> (H.insert kv i e, tv)
  | innerRejects t = (vaenv, BottomType)
  -- An UNBOUND @v@ against a different variable: @⟦v⟧ = ⊤ ⊇ ⟦w⟧@, so the meet is @w@'s
  -- occurrence bounded by both inners — the unbound case of the rule above. Returning @v@ would
  -- read as ⊤ bounded by its inner once the env (which holds only the link @v ↦ $w@) is dropped,
  -- so @$_b∩{¬~M} ∩ $_a@ would answer @⊤−M@ while @$_a ∩ $_b∩{¬~M}@ answers @$_a∩{⊤−M}@.
  | TypeVar w lw j <- t, varKeyIn vaenv lw w /= kv =
      let bound = meetInner typeEnv vaenv inner j
      in if isBottomType bound then (vaenv, BottomType) else (H.insert kv t vaenv, TypeVar w lw bound)
  | recursive t || innerSelf = (vaenv, onOccurrence)
  | otherwise = (H.insert kv t vaenv, tv)
  where kv = varKeyIn vaenv loc v
        -- Two references are the same variable when they read the same entry ('varKeyIn'): an
        -- outer @$_T@ and a partial's own @$/T@ share a name and nothing else.
        isSelfVar _ (TypeVar var' loc' _) = varKeyIn vaenv loc' var' == kv
        isSelfVar _ _                     = False
        -- A value that mentions @v@ itself is a recursive EQUATION for @v@, not a binding: resolving
        -- it re-meets the value, which holds @v@ again, and unfolds forever (@b ↦ P[/x=$_b∩{..}] + …@,
        -- as a slot union holding @$_b@ produces). 'comparedPartialsEnv' refuses the same shape.
        -- The meet is still exact without installing it: it is @⟦v⟧ ⊓ inner ⊓ t@, which is the
        -- occurrence with @t@ met into its inner, and the env keeps the binding it had. Only an
        -- EXTERNAL reference re-enters the env: a like-named partial-local pointer
        -- (@Cons[/T=⊤](head=$/T)@ as @/T@'s value) names that partial's own parameter, and treating
        -- it as recursive would keep ordinary bindings out of the env.
        recursive x = vkLoc kv == TVExt && S.member v (typeExtVarAuxes x)
        -- The occurrence's own inner reading @v@ back is the same equation from the other side: a
        -- binding installed for @v@ is then read at the inner's position too, where the other
        -- operand does not apply. Meeting @$_c∩{P[/x=$_c∩{~Q}]..}@ with @P[/x=~Q]..@ would bind
        -- @c@ to the whole @P[/x=~Q]..@, then read the inner's @$_c∩{~Q}@ as @P.. ∩ ~Q = ∅@ and
        -- empty a meet that is inhabited.
        innerSelf = vkLoc kv == TVExt && S.member v (typeExtVarAuxes inner)
        onOccurrence = TypeVar v loc (meetInner typeEnv vaenv inner t)
        -- The meet can only narrow @w@'s binding, so "unchanged" is the reverse inclusion.
        -- Compared as SETS: the meet may respell an equal binding (@X..@ as @X[/p=⊤]@).
        -- An unbound @w@ never qualifies: the recursive meet binds it to @l@, and the
        -- @⟦v⟧ ⊆ ⟦w⟧@ case then keeps @v@.
        bindingUnchanged e lw w = case (H.lookup (varKeyIn vaenv lw w) vaenv, H.lookup (varKeyIn vaenv lw w) e) of
          (Just b, Just b') -> b == b' || isSubtypeOfWithEnv typeEnv vaenv b b'
          _                 -> False
        -- A ⊤ inner adds no knowledge, so the test is skipped on the (hot) knowledge-free
        -- occurrence. Every other inner is MET — including one mentioning an ambient variable,
        -- whose meet can re-enter this clause (a var reachable from its own inner). Skipping such
        -- an inner would change the ANSWER, not just bound the work: the complement resolves the
        -- occurrence, so @a ∩ ¬a@ would keep a leaf instead of reducing to ∅. The re-entry itself
        -- is what is guarded, by the marker 'innerRejectsMeet' carries — a meet that recurs on its
        -- own derivation path adds no knowledge, so it answers "does not reject", and only where a
        -- cycle actually occurs.
        innerRejects = innerRejectsMeet typeEnv vaenv v inner
intersectTypesWithVarEnv typeEnv vaenv t tv@TypeVar{} = intersectTypesWithVarEnv typeEnv vaenv tv t
-- Mixed type (both top and pos non-empty): distribute intersection over union
-- (top ∪ pos) ∩ t = (top ∩ t) ∪ (pos ∩ t)
intersectTypesWithVarEnv typeEnv vaenv (UnionType (Just topData) pos cs) t2
  | not (H.null pos) || not (null cs) =
      let (vaenv1, topResult) = intersectTypesWithVarEnv typeEnv vaenv (UnionType (Just topData) H.empty []) t2
          (vaenv2, posResult) = intersectTypesWithVarEnv typeEnv vaenv (UnionType Nothing pos cs) t2
          vaenv' = mergeAllVarEnvs typeEnv [vaenv1, vaenv2]
          -- A component intersection can meet an external var with the other component's bound,
          -- recording the meet in its returned env while its leaf keeps the var pointer (e.g.
          -- @Some[/T=$_B]@ with @/B ↦ (Top|Eq)@ recorded). The recombining union resolves var
          -- pointers through the env it is GIVEN, which does not hold those fresh meets - the
          -- pointer would widen to Top. Resolve the newly recorded meets back into the components
          -- first; still-unresolved vars stay symbolic ('substituteKeep', as 'mergeVarEnvVal').
          newBindings = discoveredBindings vaenv' vaenv
          resolveNew | H.null newBindings = id
                     | otherwise = substituteKeep typeEnv newBindings
      in (vaenv', unionTypesWithEnv typeEnv vaenv (resolveNew topResult) (resolveNew posResult))
intersectTypesWithVarEnv typeEnv vaenv t1 t2@(UnionType (Just _) pos cs)
  | not (H.null pos) || not (null cs) = intersectTypesWithVarEnv typeEnv vaenv t2 t1
-- Top ∩ Top → union excluded sets (leafs and constants) and merge predicates
intersectTypesWithVarEnv typeEnv vaenv (UnionType (Just (ps1, np1, nc1)) (H.null -> True) []) (UnionType (Just (ps2, np2, nc2)) (H.null -> True) []) =
  let (vaenv', preds') = mergePredsAnd typeEnv vaenv ps1 ps2
  in (vaenv', compactType typeEnv vaenv' $ UnionType (Just (preds', unionPartialLeafs [np1, np2], L.nub $ nc1 ++ nc2)) H.empty [])
-- PosPartials with constants ∩ Top: handle constant exclusion before leafs logic
intersectTypesWithVarEnv typeEnv vaenv (UnionType Nothing posLeafs posConsts) (UnionType (Just (preds, negLeafs, negConsts)) (H.null -> True) [])
  | not (null posConsts) || not (null negConsts) =
      let (vaenv', baseResult) = intersectTypesWithVarEnv typeEnv vaenv
                                   (UnionType Nothing posLeafs [])
                                   (UnionType (Just (preds, negLeafs, [])) H.empty [])
          -- A constant is in the top, out of it, or within the top's VALUE predicates, which are
          -- decided neither way on it ('constantWithinTop'): kept as the leaf that spells exactly
          -- that, the constant within the predicates (@/Float | PredConstEq c ∧ P@) — never dropped
          -- (a meet is exact or wider, and dropping it read @c ∉ P@) nor kept bare (which read
          -- @c ∈ P@, and lost the predicate the value is held to).
          placed = [ (c, lits) | c <- posConsts, Just lits <- [constantWithinTop typeEnv vaenv c (preds, negLeafs, negConsts)] ]
          survivingConsts = [ c | (c, []) <- placed ]
          withinLits = [ (constantPartialType c){ptPreds = mkAnd (PredsOne (PredConstEq c) : lits)} | (c, lits@(_:_)) <- placed ]
      -- @(posLeafs ∪ posConsts) ∩ t2 = (posLeafs ∩ t2) ∪ (posConsts ∩ t2)@: rejoin the surviving
      -- constants onto the leaf part. 'unionTypesWithEnv' handles every shape of @baseResult@ — it
      -- drops a constant already covered by baseResult's leafs and folds it onto a Just-top's
      -- positive constants when the leafs compacted to a rel-form (an over-unrolled recursive set),
      -- which a bare @baseResult@ return would silently drop.
          -- The leaf part was met against the top WITHOUT its excluded constants, so they are
          -- taken out of it again: a leaf holding one keeps every point but it
          -- ('excludeConstants'), so @/Integer ∩ (⊤ − {c})@ is @/Integer|¬PredConstEq(c)@, not all
          -- of @/Integer@.
          leafPart = excludeConstants negConsts baseResult
      in if null survivingConsts && null withinLits
           then (vaenv', leafPart)
           else (vaenv', unionTypesWithEnv typeEnv vaenv' leafPart (UnionType Nothing (joinUnionType withinLits) survivingConsts))
-- PosPartials ∩ Top(negLeafs, PredsNone) when negLeafs non-empty → subtract
intersectTypesWithVarEnv typeEnv vaenv (UnionType Nothing posPartials []) (UnionType (Just (PredsNone, negPartials, [])) (H.null -> True) [])
  | not (H.null negPartials) =
      (vaenv, compactType typeEnv vaenv $ differencePartialLeafs typeEnv vaenv posPartials negPartials)
-- PosPartials ∩ a DISJUNCTIVE predicate universe: DISTRIBUTE over the disjuncts.
-- 'unionTypesWithEnv' spells @p₁ ∨ p₂@ as @¬(¬p₁ ∧ ¬p₂)@ (De Morgan, kept flat), so a union's
-- predicate reaches the meet as ONE negated conjunction. Meeting a leaf against that formula as
-- a WHOLE would hand the disjunction to the per-leaf Bool oracles, which decide it with a
-- different prover than the per-disjunct meets use; @Cons ∩ (∀A ∪ (⊤ − ∀B))@ must equal
-- @(Cons ∩ ∀A) ∪ (Cons ∩ (⊤ − ∀B))@ whatever either prover can decide.
--
-- @X ∩ (Q₁ ∨ … ∨ Qₙ) = (X∩Q₁) ∪ … ∪ (X∩Qₙ)@ is an exact set identity — no world assumption and
-- no enumeration — and computing the meet that way makes it agree with the union BY
-- CONSTRUCTION, rather than by both provers happening to answer alike. It is also strictly more
-- precise: each disjunct meets the leaf in its own right, so a disjunct the oracles cannot
-- decide on the whole formula does not cost the others their answer.
--
-- Well-founded: each @¬qᵢ@ carries one ¬-level less than the formula, and the union's
-- normalization keeps these flat (depth 1), so the disjuncts are atoms or single negations that
-- the clauses below decide directly — a recursive negation among them still reaches the
-- terminal attach clause. Placed BEFORE that clause, which would otherwise take the whole
-- formula (a disjunction holding one recursive negated atom) and attach it undistributed.
intersectTypesWithVarEnv typeEnv vaenv t1@(UnionType Nothing partials []) (UnionType (Just (PredsNot (PredsAnd qs), (H.null -> True), [])) (H.null -> True) [])
  | not (H.null partials), length qs >= 2 =
      let meetDisjunct (va, acc) q =
            let (va', r) = intersectTypesWithVarEnv typeEnv va t1 (UnionType (Just (predsNot q, H.empty, [])) H.empty [])
            in (va', unionTypesWithEnv typeEnv va' acc r)
      in L.foldl' meetDisjunct (vaenv, BottomType) qs
-- PosPartials ∩ pure formula top NEGATING A RECURSIVE CLASS MEMBERSHIP: the formula is a
-- symbolic refinement — attach it to each leaf ('partialAddPreds', the same exact
-- leaf-refinement spelling the positive 'tryPredsToList' arm below uses) and let the Bool
-- oracles (implication, emptiness, the sat engine) decide it per-leaf later. Such a
-- formula must NEVER materialize into member leafs during a meet: the subtraction leafs
-- it builds are self-similar to the recursive class's members — the residual reappears in
-- the @tail@ slot of @every(ConsList[$T -> Eq])@-style members and re-poses this same
-- meet unchanged, so @(⊤|¬Eq) ∩ ConsList-members@ would never converge. Every materializing
-- surface (the meet, the difference's escape pieces, the subtype fallback) can re-route that
-- loop, so the meet itself must be TERMINAL for these formulas. A NON-recursive negation
-- stays on the materialized path below: its expansion is finitely deep (the reference DAG's
-- height), and that path's spellings are the ones the rest of the algebra decides exactly.
--
-- An OPEN class atom ('predsHasOpenAtom') is attached the same way: its current members do
-- not bound it, so materializing it would decide membership closed-world. Unless every leaf is a
-- type already in scope ('knownMemberName'): no later file adds such a type to a class, so the
-- class's current members restricted to the leaves' names are exact, and the expansion below
-- carries the class's parameters into the leaves' fields.
intersectTypesWithVarEnv typeEnv vaenv (UnionType Nothing partials []) (UnionType (Just (preds, (H.null -> True), [])) (H.null -> True) [])
  | predsHasRecNegAtom typeEnv preds || (predsHasOpenAtom typeEnv preds && not (all (knownMemberName typeEnv) (H.keys partials))) =
      (vaenv, compactType typeEnv vaenv $ UnionType Nothing (joinUnionType $ map (`partialAddPreds` preds) $ splitUnionType partials) [])
-- PosPartials ∩ Top(_, preds) when negLeafs empty → expand/filter
intersectTypesWithVarEnv typeEnv vaenv t1@(UnionType Nothing partials []) t2@(UnionType (Just (preds, negPartials, [])) (H.null -> True) _)
  | H.null negPartials =
      case expandTypesWithNamesFull typeEnv vaenv t2 (H.keysSet partials) of
        -- When filteredLeafs is non-empty, use it directly regardless of hasOutside.
        -- Elements of t2 outside pos's names cannot affect the intersection result,
        -- so filteredLeafs (elements of t2 within pos's names) is sufficient.
        (filteredLeafs, _) | not (H.null filteredLeafs) ->
          intersectTypesWithVarEnv typeEnv vaenv t1 (UnionType Nothing filteredLeafs [])
        (filteredLeafs, False) -> intersectTypesWithVarEnv typeEnv vaenv t1 (UnionType Nothing filteredLeafs [])
        -- filteredLeafs is empty but hasOutside=True: t2 might still constrain t1 symbolically.
        -- Symbolic: add preds to each partial when compactPartialsWithClassPred can resolve them
        -- (requires no PredsNot, which tryPredsToList signals by returning Nothing).
        -- Otherwise expand preds symbolically within pp's names (no expandPredicates needed).
        (_, True) -> case tryPredsToList preds of
          Just _  -> (vaenv, compactType typeEnv vaenv $ UnionType Nothing (joinUnionType $ map (`partialAddPreds` preds) $ splitUnionType partials) [])
          Nothing -> case preds of
            -- pp ∩ Top(¬inner, ∅) = pp - expand(inner within pp)
            PredsNot inner ->
              let innerLeafs = expandTypeWithNames typeEnv vaenv (UnionType (Just (inner, H.empty, [])) H.empty []) (H.keysSet partials)
              in (vaenv, compactType typeEnv vaenv $ differencePartialLeafs typeEnv vaenv partials innerLeafs)
            -- General: pp ∩ Top(preds, ∅) = pp ∩ expand(preds within pp)
            _ ->
              let predsLeafs = expandTypeWithNames typeEnv vaenv (UnionType (Just (preds, H.empty, [])) H.empty []) (H.keysSet partials)
              in intersectTypesWithVarEnv typeEnv vaenv t1 (UnionType Nothing predsLeafs [])
-- PosPartials ∩ Top(preds, negLeafs) with both preds non-trivial and negLeafs non-empty.
-- Split the top as Top(preds, ∅) ∩ Top(PredsNone, negLeafs) and intersect in sequence:
-- (pos ∩ U_preds) - negLeafs. The first step routes to the negLeafs-empty clause above and the
-- second to the PredsNone-subtract clause, both of which yield positive-partial forms, so this
-- terminates. (Without this clause the pair falls through to the unhandled-case error below.)
intersectTypesWithVarEnv typeEnv vaenv t1@(UnionType Nothing _ []) (UnionType (Just (preds, negPartials, ncs)) (H.null -> True) [])
  | not (H.null negPartials) && preds /= PredsNone =
      let (vaenv', inner) = intersectTypesWithVarEnv typeEnv vaenv t1 (UnionType (Just (preds, H.empty, ncs)) H.empty [])
      in intersectTypesWithVarEnv typeEnv vaenv' inner (UnionType (Just (PredsNone, negPartials, [])) H.empty [])
-- Top ∩ PosPartials → flip
intersectTypesWithVarEnv typeEnv vaenv t1@(UnionType (Just _) (H.null -> True) _) t2@(UnionType Nothing _ _) =
  intersectTypesWithVarEnv typeEnv vaenv t2 t1
-- PosPartials ∩ PosPartials → intersect leafs and constants
intersectTypesWithVarEnv typeEnv vaenv (UnionType Nothing aPartials aConsts) (UnionType Nothing bPartials bConsts) =
  debugTrace typeEnv msg (vaenv', compactType typeEnv vaenv result)
  where
    (vaenv', partials') = intersectPartialLeafsWithVarEnv typeEnv vaenv aPartials bPartials
    -- Constants that appear in both sides, or whose parent partial is in the other side's leafs
    aConsts' = filter (\c -> c `elem` bConsts || constantCoveredByLeafs c bPartials) aConsts
    bConsts' = filter (\c -> c `elem` aConsts || constantCoveredByLeafs c aPartials) bConsts
    -- Also remove constants already covered by the intersected partial leafs
    mergedConsts = L.nub $ filter (not . (`constantCoveredByLeafs` partials')) (aConsts' ++ bConsts')
    -- A constant the other side's leaf holds by its shape but under predicates left at the
    -- constant (@{3} ∩ /Integer | P@ for a value predicate @P@) is that constant within them: the
    -- point leaf @/Integer | PredConstEq 3 ∧ P@, as the meet with the top spelling of the same set
    -- (@⊤ | P ∧ PredRel Integer@) gives it. Dropped, the constant read as outside @P@, which
    -- nothing decided.
    pointLeafs = joinUnionType
      [ l{ptPreds = predsAnd (PredsOne (PredConstEq c)) ps}
      | (cs, otherConsts, others) <- [(aConsts, bConsts, bPartials), (bConsts, aConsts, aPartials)]
      , c <- cs, c `notElem` otherConsts, not (constantCoveredByLeafs c others)
      , l <- splitUnionType others
      , Just ps <- [predsAtConst c (ptPreds l)], ps /= PredsNone
      , constantCoveredByLeafs c (joinUnionType [l{ptPreds = PredsNone}]) ]
    result = UnionType Nothing (unionPartialLeafs [partials', pointLeafs]) mergedConsts
    msg = printf "[INTERSECT] %s ∩ %s = %s" (show $ UnionType Nothing aPartials aConsts) (show $ UnionType Nothing bPartials bConsts) (show result)
intersectTypesWithVarEnv _ _ t1 t2 = error $ printf "intersectTypesWithVarEnv: unhandled case %s ∩ %s" (show t1) (show t2)

-- | The env-dropping meet. 'intersectTypesWithVarEnv' returns a var-meet as the bare VARIABLE while
-- recording the tightened binding in its returned env (@Integer ∩ $_T@ → @$_T@ with @$_T ↦ Integer@),
-- so the meet's occurrences stay linked. Dropping that env verbatim would leave the WIDER free var —
-- a recursive-tail @ConsList[/T=$_T]@ reading as Top, so a @/T=Integer@ list would meet a @/T=Eq@
-- requirement to the abstract @Eq@. So the meet's freshly discovered bindings are resolved back into
-- the result before the env is discarded ('foldDiscovered', the 'mergeVarEnvVal' resolve-meets-back
-- rule) — but ONLY the CONCRETE ones. A binding to another
-- variable is genuine var LINKING (an @operator+[$T]@ tying its operands and result together); keeping
-- that link a var is the whole point of returning the var, and substituting it would collapse the
-- polymorphic operator. A binding to a concrete type is a true narrowing that the free var would
-- otherwise lose.
intersectTypesEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Type
intersectTypesEnv typeEnv vaenv t1 t2 = uncurry (foldDiscovered typeEnv vaenv) (intersectTypesWithVarEnv typeEnv vaenv t1 t2)

-- | Read a meet's result standalone: fold the bindings the meet DISCOVERED (its returned env
-- @vaenv'@ against the env it was given) back into the result, so dropping the env loses
-- nothing. The rule behind 'intersectTypesEnv', shared with every caller that runs the
-- env-returning meet itself and then keeps only the type ('differencePartialLeafs'.'subtractVar').
foldDiscovered :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypeVarArgEnv -> Type -> Type
foldDiscovered typeEnv vaenv vaenv' t =
  let -- The bindings this meet DISCOVERED: a var that was free, and a bound var the meet
      -- genuinely narrowed. The second matters as much as the first — an occurrence of an
      -- already-bound @$_b@ met with @∀C@ keeps the bare @$_b@ while the narrowing lives in the
      -- dropped env, so without it the result would read @b@'s old, wider binding. "Narrowed" is
      -- decided as SETS: the meet also re-inserts bindings it merely respelled (@X..@ as
      -- @X[/p=⊤]@), and substituting every such respelling would grow the type without bound.
      --
      -- The two kinds fold back differently. A FRESH binding folds through 'substituteKeep',
      -- whose top-level scope is the ambient env: the result is a fragment whose parent is the
      -- object the env registers. A NARROWED binding folds into the var's EXTERNAL occurrences
      -- only: the meet keys a parent reference by name too, so a detached slot value's @$^/T@
      -- narrows the ambient @/T@ — a different variable — and folding that back would rewrite
      -- the partial's own parameter. With the scope left empty a parent reference reads nothing
      -- and stays symbolic.
      concrete = H.filter (not . isTypeVar) vaenv'
      fresh = discoveredBindings concrete vaenv
      -- Only a var the result still mentions externally can be folded, and deciding
      -- "narrowed" costs a subtype test per binding. Asked of every re-inserted binding it
      -- would cascade: the test meets, the meets re-merge and respell the env, and the
      -- respelled bindings are tested again.
      mentioned = typeExtVarAuxes t
      narrowedB = H.filterWithKey (\k b' -> S.member (vkVar k) mentioned && narrowed k b') (H.intersection concrete vaenv)
      narrowed k b' = case H.lookup k vaenv of
        Nothing -> False
        Just b  -> b /= b' && not (isSubtypeOfWithEnv typeEnv vaenv b b')
      foldBack t'
        | H.null narrowedB = t'
        | otherwise = substituteKeepScopedR typeEnv readEnv H.empty narrowedB t'
      -- Folding a binding into an occurrence meets it with the occurrence's inner, and that meet
      -- reads the references inside both in the ambient scope ('externalReadEnv'). Read in the
      -- folded bindings alone, @c ↦ P[/x=$_b]..@ would read its @$_b@ as unbound, and the slot
      -- would widen past the operand.
      readEnv = externalReadEnv vaenv' (t : H.elems fresh ++ H.elems narrowedB)
  in case t of
      _ | H.null fresh && H.null narrowedB -> t
      -- A meet returned as a bare top-level var IS that var's discovered binding — whatever
      -- the var's loc. 'substituteKeep' would keep a 'TVInt' (inside a partial that names a
      -- partial-scoped parameter), but at the RESULT's top level the var is this operation's
      -- own operand and its standalone reading would wrongly widen to ⊤.
      -- ...but the occurrence denotes @⟦v⟧ ⊓ ⟦inner⟧@, so its VALUE is the discovered
      -- binding MET WITH THE INNER, not the binding alone. Returning @b@ bare would drop the
      -- inner and widen the meet past an operand: with @c@ unbound, @$_c∩{A} ∩ (⊤−B)@ binds
      -- @c ↦ ⊤−B@, and the meet is @A−B@, not @⊤−B@.
      --
      -- An inner mentioning type variables is left alone, for the same reason
      -- 'intersectTypesWithVarEnv'.'innerRejects' skips it: meeting it re-enters the var
      -- clause and a var reachable from its own inner loops.
      TypeVar v _ i
        | Just b <- H.lookup (extKey v) fresh ->
            if i == PTopType || innerOpen v i then b else intersectTypesEnv typeEnv vaenv b i
      _ -> foldBack (if H.null fresh then t else substituteKeepReading typeEnv readEnv fresh t)

-- | Meets in flight inside 'innerRejectsMeet', for the re-entry guard it documents. Keyed
-- WITHOUT the environment: a cycle whose env grows each lap would never re-hit an env-keyed
-- entry, and the marker exists to stop exactly that.
{-# NOINLINE innerRejectsInFlightRef #-}
innerRejectsInFlightRef :: InFlight (TypeVarAux, Type, Type)
innerRejectsInFlightRef = unsafePerformIO newInFlight

-- | Whether an occurrence's own inner rules the meet out: an occurrence denotes
-- @⟦var⟧ ⊓ ⟦inner⟧@, so @$v∩{I} ⊓ t@ is empty as soon as @I ⊓ t@ is, whatever the binding says.
-- The inner is met under the ambient env like any other type; a re-entry on the same
-- (variable, inner, operand) answers 'False' — no finite derivation proves emptiness along a
-- path that re-poses its own question, and "does not reject" is the conservative direction for
-- a meet.
innerRejectsMeet :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypeVarAux -> Type -> Type -> Bool
innerRejectsMeet typeEnv vaenv v inner other
  | inner == PTopType = False
  | otherwise = unsafePerformIO $ do
      let k = (v, inner, other)
      inFlightOn innerRejectsInFlightRef k (noteCycleCut False) $
        evaluate (isBottomType (intersectTypesEnv typeEnv vaenv inner other))

differencePartialLeafs :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialLeafs -> PartialLeafs -> Type
differencePartialLeafs typeEnv vaenv posPartialLeafs negPartialLeafs = UnionType Nothing (joinUnionTypeByName $ H.differenceWith differencePartials (splitUnionTypeByName posPartialLeafs) (splitUnionTypeByName negPartialLeafs)) []
  where
    -- Whether an escape piece carries a ¬-over-recursive-class formula in a var/arg slot
    -- (a top's predicates, or an attach-refined leaf, at any depth) — the spelling the
    -- symbolic-attach clause of 'differencePartial' exists to avoid.
    escapeHasRecNeg :: PartialType -> Bool
    escapeHasRecNeg PartialType{ptVars, ptArgs} = any slotRecNeg (H.elems ptVars ++ H.elems ptArgs)
      where
        slotRecNeg (UnionType mTop pos _) =
          maybe False (\(preds, negLeafs, _) -> predsHasRecNegAtom typeEnv preds || any leafRecNeg (splitUnionType negLeafs)) mTop
          || any leafRecNeg (splitUnionType pos)
        slotRecNeg _ = False
        leafRecNeg p = predsHasRecNegAtom typeEnv (ptPreds p) || escapeHasRecNeg p
    differencePartials :: [PartialType] -> [PartialType] -> Maybe [PartialType]
    differencePartials poss negs = case concatMap (`differencePartialNegs` negs) (keepIrredundant poss) of
      []  -> Nothing
      res -> Just res

    -- A pos leaf a SIBLING of the same name already contains contributes nothing to the
    -- difference: @(P ∪ Q) − N = P − N@ when @Q ⊆ P@. Exact — and it leaves the subtraction on
    -- the spelling that subtracts CLEANLY. The two spellings of one set do not subtract alike: a
    -- list written to a fixed depth (@Cons(tail=(Cons(tail=∀ConsList) + Nil))@) spells its tail
    -- concretely, so subtracting a neg whose tail is the bare @∀ConsList[/T=$^/T]@ produces a
    -- per-slot escape carrying a ¬-over-recursive-class formula and falls to the symbolic
    -- attach below, while the equal @Cons(tail=∀ConsList[/T=$^/T])@ beside it subtracts on the
    -- element dimension alone and reduces. Keeping both would leave @a − (a−b)@ as an opaque
    -- @leaf|¬(PredRel …)@ that no longer compares with the meet's @a∩b@. Mutually-contained
    -- leafs keep the FIRST, so one always survives.
    keepIrredundant :: [PartialType] -> [PartialType]
    keepIrredundant [p] = [p]
    keepIrredundant ps = [p | (i, p) <- ips, not (any (absorbs i p) ips)]
      where
        ips = Prelude.zip [(0 :: Int) ..] ps
        absorbs i p (j, q) = j /= i
          && isSubPartialOfWithEnv typeEnv vaenv p q
          && (j < i || not (isSubPartialOfWithEnv typeEnv vaenv q p))

    differencePartialNegs :: PartialType -> [PartialType] -> [PartialType]
    differencePartialNegs pos = foldr (\n ps -> concatMap (`differencePartial` n) ps) [pos]

    -- Exact difference on the arg-set intervals (see 'PtArgMode'), on mode-normalized
    -- partials ('normalizePtArgMode' — so a vacuous bound never reads as extra arg-sets):
    --   * identical or contained pos ⟹ ∅;
    --   * disjoint intervals ('argModesOverlap' fails) ⟹ pos unchanged;
    --   * neg interval covering the whole pos interval ('argModeCovers'), pos
    --     lower-bounded ⟹ only the per-arg\/var value escapes remain ('partialEscapes').
    --     Lower-bounded pos only: escaping via an arg's value requires the arg PRESENT,
    --     which a lower bound guarantees; a lower-unbounded pos admits arg-sets missing
    --     the arg (vacuously inside neg), so it enumerates instead;
    --   * PARTIAL interval overlap: enumerate the pos interval's arg-sets
    --     ('ptIntervalArgKeys' — finite via the declared arg-sets) as exact partials and
    --     recurse; each exact piece is decided by the clauses above (an exact pos interval
    --     is a single point: it is covered by neg or disjoint from it, never partial).
    --   * Only an upper-unbounded pos of an UNKNOWN name (open world: arg-sets not
    --     enumerable) falls back to keeping pos unchanged.
    differencePartial :: PartialType -> PartialType -> [PartialType]
    differencePartial posRaw negRaw = go (normalizePtArgMode typeEnv posRaw) (normalizePtArgMode typeEnv negRaw)
      where
        go p1 p2 | p1 == p2 = []
        -- @p1 ⊆ p2 ⟹ p1 − p2 = ∅@. The syntactic @p1 == p2@ above only catches identical partials;
        -- this also cancels a partial contained in the neg up to variable renaming or field widening —
        -- e.g. a @Cons[/T=$_a](head=$x, tail=…)@ minus the class member @Cons[/T=$_b](head=$y, tail=…)@,
        -- whose free element\/field variables all read as ⊤, so the pos IS the member. Without this the
        -- per-field 'partialEscapes' produces spurious variable-vs-variable escape leafs (@$x − $y@ never
        -- proves ∅) and the difference wrongly keeps a partial its own subtype relation calls covered.
        -- Uses the precise partial subtype (this env has 'teArgModeProjection' False).
        go p1 p2 | isSubPartialOfWithEnv typeEnv vaenv p1 p2 = []
        go p1 p2 | not (argModesOverlap p1 p2) = [p1]
        -- A neg REFINED by predicates (@Cons(..)|¬EqC@, the ¬-over-recursive-class attach
        -- spelling) excludes only the instances satisfying them; the clauses below read the
        -- neg's args\/vars alone and would subtract the FULL bare neg. Exact decomposition:
        -- @p1 − (p2' ∧ Q) = (p1 − p2') ∪ ((p1 ∩ p2') ∧ ¬Q)@ with @p2'@ the bare neg. The
        -- overlap comes from 'intersectPartials' with its new env bindings resolved back
        -- (the same env-dropping recipe as 'intersectTypesEnv', so both op families spell
        -- the met pieces identically), and @¬Q@ attaches for the enclosing compact to
        -- decide. The recursive @go@ runs on the PredsNone bare neg, so this clause cannot
        -- re-enter.
        go p1 p2 | ptPreds p2 /= PredsNone =
          let p2' = p2{ptPreds=PredsNone}
              (venv, overlapRaw) = intersectPartials typeEnv vaenv p1 p2'
              newBindings = H.filter (not . isTypeVar) $ discoveredBindings venv vaenv
              overlap = case substituteKeep typeEnv newBindings (UnionType Nothing (joinUnionType overlapRaw) []) of
                UnionType Nothing ls [] -> splitUnionType ls
                _                       -> overlapRaw
          in go p1 p2' ++ map (`partialAddPreds` predsNot (ptPreds p2)) overlap
        -- A subtraction whose per-slot escapes would carry a ¬-over-recursive-class formula
        -- subtracts SYMBOLICALLY instead: attach @¬(PredRel neg)@. Such escapes put the
        -- ¬-formula INTO var\/arg slots (@head=(⊤ | ¬EqC)@, @tail=…|¬ConsList[Eq]@) — a
        -- spelling position the terminal-attach design never produces, which the meet
        -- route's attach spelling ('partialAddPreds' @¬EqC@) cannot be compared against,
        -- splitting the @A∩B = A−(A−B)@ \/ @A−B = A−(A∩B)@ bridges. The gate is decided on
        -- the RESULT ('escapeHasRecNeg'): a neg merely referencing a recursive class often
        -- subtracts cleanly (its slot differences resolve to plain types), and replacing
        -- that decidable exact spelling with an opaque symbolic one breaks @A+B−A ⊆ B@-style
        -- laws on unrelated inputs. @PredRel neg@ is exact on THIS leaf's value set: every
        -- value here carries the shared name (the leafs subtract name-grouped), so the
        -- relation's name route reduces to matching the neg's shape — guarded on the name
        -- not doubling as a class name, where the relation's class route would widen the
        -- excluded set.
        --
        -- A value of the pos escapes the neg at some arg or var: at an arg the neg requires and it
        -- lacks ('absentEscapes'), or at one it has, valued outside the neg's ('partialEscapes',
        -- whose arg pieces require the arg). That accounts for every escape when the neg admits
        -- unlisted args (its unlisted args constrain nothing) or covers the pos's arg-sets (it
        -- admits every unlisted arg the pos may have).
        go p1 p2 | argModeCovers p2 p1 || not (ptArgUb (ptArgMode p2)) = case absentEscapes p1 p2 of
          Nothing -> [p1]
          Just absents ->
            let escapes = partialEscapes typeEnv vaenv p1 p2
            in if ptName p2 `notElem` listClassNames typeEnv && any escapeHasRecNeg escapes
                 then [partialAddPreds p1 (predsNot (PredsOne (PredRel p2)))]
                 else absents ++ escapes
        go p1 p2 | Just kss <- ptIntervalArgKeys typeEnv p1 = concatMap ((`go` p2) . ptAtArgKeys p1) kss
        go p1 _ = [p1]
        -- The pos values lacking an arg the neg requires: for each such arg the pos does not
        -- require, the pos with it absent (typed ∅, 'PtArgMode'). 'Nothing' where the pos never
        -- has one (upper-bounded and not listing it): then no value of the pos is in the neg.
        absentEscapes p1 p2 = traverse absentAt (S.toList (ptRequiredArgs p2 `S.difference` ptRequiredArgs p1))
          where
            absentAt k
              | H.member k (ptArgs p1) = Just $ canonicalArgOpt typeEnv p1{ptArgs=H.insert k BottomType (ptArgs p1)}
              | ptArgUb (ptArgMode p1) = Nothing
              | otherwise = Just $ withOptionalArgs typeEnv (S.insert k (ptArgOpt (ptArgMode p1))) p1{ptArgs=H.insert k BottomType (ptArgs p1)}

-- | The pos instances escaping neg on some per-arg\/var type — @p1 − p2@ as a union of pieces, one
-- per position of @p2@ that a value of @p1@ can leave (see 'differencePartialLeafs', which spells the
-- difference with them). The pos instances escaping neg on some per-arg\/var type: for each neg arg\/var, the
-- pos partial with that element narrowed to its difference from neg's. A ⊤-valued neg
-- element excludes nothing; a neg arg that pos does not constrain is skipped.
partialEscapes :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> [PartialType]
partialEscapes typeEnv vaenv p1 p2Spelled@PartialType{ptVars=negVars} =
  mapMaybe (\a@(k, _) -> requireArg typeEnv k <$> subtractArg a) (H.toList negArgs) ++ mapMaybe subtractVar (H.toList negVars)
  where
    -- A parameter tied to several positions ('paramSharedByPositions') subtracts through
    -- those positions, not as a coordinate: the neg excludes the values whose every position
    -- lies in its bound, so a value escapes it at whichever position leaves the bound. The
    -- neg's positions it does not spell hold the declaration's template, read at the neg's own
    -- parameter (@Cons[/T=K]..@ has @head@ in @K@ and @tail@ in @ConsList[K]@).
    --
    -- Only where the pos has EVERY position of the parameter. A partial application missing
    -- some (@Cons[/T=X](head=..)@ before its @tail@, or a bare @Cons[/T=X]@) still carries the
    -- parameter as a bound on what it will accept there, so on that dimension the parameter is
    -- subtracted as written.
    shared n = paramSharedByPositions typeEnv (ptName p1) n
               && all (`H.member` ptArgs p1) (S.toList (paramPositions typeEnv (ptName p1) n))
    sharedAux (TVVar n) = shared n
    sharedAux _         = False
    sharedArg x y = any sharedRefs [x, y]
    sharedRefs t = any sharedAux (S.toList (typeTVIntRefs t)) || any shared (S.toList (typeOuterVarRefs t))
    sharedNegVars = [n | (n, v) <- H.toList negVars, v /= PTopType, shared n]
    p2 = p2Spelled{ptArgs = H.union (ptArgs p2Spelled) declaredPositions}
    negArgs = ptArgs p2
    declaredPositions = H.fromList
      [ (k, v)
      | not (null sharedNegVars)
      , d <- take 1 (typeGraphData typeEnv (ptName p1))
      , (k, v) <- H.toList (ptArgs d)
      , H.member k (ptArgs p1)
      , any (\n -> case v of
                    TypeVar (TVVar n') TVInt _ -> n' == n
                    _                          -> S.member n (typeOuterVarRefs v)) sharedNegVars ]
    -- A neg value subtracts as the value SET it denotes: its own-scope references read
    -- through the NEG partial's slots ('resolveOwnScopeRefs' — @head=$/T@ under the
    -- neg's @/T=X@ excludes @X@, not the ambient reading of a bare @$/T@).
    negValSet = resolveOwnScopeRefs typeEnv vaenv p2
    -- The pos side must denote its value SET the same way the neg side does, or the two
    -- spellings of one set compare unequal and the subtraction invents an escape. The two
    -- positions a reference can sit in want DIFFERENT treatment, though:
    --
    --   TOP LEVEL (@/head=$/T@, the arg value IS the reference). Kept as a pointer, and
    --   deliberately: this is the dimension 'subtractVar' subtracts on, and 'narrowPiece'
    --   folds the discovered narrowing back onto the var slot. Resolving it here would
    --   subtract on the arg instead and lose that machinery.
    --
    --   NESTED (@/second=/Pair[/B=$/A]@, the reference is inside a nested partial). No
    --   var-slot machinery reaches it, so leaving it symbolic while the neg side spells it
    --   concretely would be pure asymmetry, and the difference of the two spellings a
    --   spurious escape: @/Pair[/A=X](/second=/Pair[/B=$/A])@ minus the same partial writing
    --   that nested slot as @X@ must be empty — the nested @/Pair@ does not spell @/A@, so its
    --   @$/A@ reads against the enclosing @/A=X@, and the two operands denote one set.
    --
    -- So a bare reference is kept as a pointer, and anything else resolves through the pos
    -- partial's OWN slots at depth.
    --
    -- Own slots and NOT the ambient env: resolving ambient vars at every depth would
    -- materialise the whole nesting at a large cost, and the asymmetry this corrects is
    -- entirely about the partial's own parameters.
    posValSet v@TypeVar{} = freeTVIntToTop p1 (substituteKeep typeEnv (ptVarArg p1) v)
    posValSet v           = substituteTVExtScoped typeEnv (ptVarArg p1) H.empty v
    -- A value escaping the neg on an arg has the arg: the piece requires it ('requireArg' above).
    -- One the pos does not list, a pos without an upper bound may still carry unlisted, with any
    -- value: it escapes where that value is outside the neg's.
    subtractArg (argName, negArgValRaw) = case H.lookup argName (ptArgs p1) of
      Nothing | ptArgUb (ptArgMode p1) -> Nothing
              | otherwise -> case negValSet negArgValRaw of
                  PTopType  -> Nothing
                  negArgVal -> case differenceTypeWithEnv typeEnv vaenv PTopType negArgVal of
                    (_, BottomType)   -> Nothing
                    (outEnv, argVal') -> narrowPieceAt (Just argName) p1{ptArgs=H.insert argName argVal' (ptArgs p1)} outEnv
      -- IDENTICAL linked forms on both sides (@tail=∀ConsList[/T=$/T]@) whose every
      -- reference the POS partial itself binds differ only through the two partials'
      -- own var scopes; those dimensions are exactly what 'subtractVar' subtracts, so
      -- the arg contributes no separate escape (emitting one duplicates the var escape
      -- as an unrecognizable symbolic residue). A reference the pos does NOT bind
      -- reads as ⊤ on the pos side only — a real per-arg escape — so it must subtract.
      Just posArgVal | posArgVal == negArgValRaw
                     , all ((`H.member` ptVarArg p1) . extKey) (S.toList (typeTVIntRefs posArgVal))
                     , not (sharedRefs posArgVal) -> Nothing
      -- The pos value subtracts as the value SET it denotes: a free reference reads as
      -- ⊤ ('freeTVIntToTop' — @Cons(head=$/T)@ with no @/T@ slot escapes on @⊤ − neg@),
      -- while a DECLARED-var reference subtracts on the var slot via 'narrowPiece'.
      -- A reference NESTED in the value is instead read through the pos partial's own
      -- slots ('posValSet'), the same reading the neg side gets: both sides must spell a
      -- bound parameter the same way or the slot difference cannot cancel. A recursive
      -- @tail -> ∀ConsList[/T=$^/T]@ under @/T=$_a@ otherwise leaves the pos at @$^/T@
      -- while the neg reads @$_a@, and the difference of the two spellings is a ¬-formula
      -- over the class rather than the plain type they both denote — which then trips the
      -- symbolic-attach gate below and loses the exact answer entirely.
      -- A position of a SHARED parameter ('paramSharedByPositions') is not subtracted on the
      -- parameter, so no var-slot machinery reads its references: each side is read as the
      -- value set it denotes under its own partial ('freeTVIntToTopDeep'), a parameter the
      -- partial binds to ⊤ reading as ⊤ at any depth — @tail=∀ConsList[/T=$^/T]@ under
      -- @/T=⊤@ is every list, and left symbolic it would be free to take the neg's @K@.
      Just posArgVal | sharedArg posArgVal negArgValRaw ->
        case freeTVIntToTopDeep typeEnv vaenv p2 negArgValRaw of
          PTopType  -> Nothing
          negArgVal -> case differenceTypeWithEnv typeEnv vaenv (freeTVIntToTopDeep typeEnv vaenv p1 posArgVal) negArgVal of
            (_, BottomType)   -> Nothing
            (outEnv, argVal') -> narrowPieceAt (Just argName) p1{ptArgs=H.insert argName argVal' (ptArgs p1)} outEnv
      Just posArgVal -> case negValSet negArgValRaw of
        PTopType  -> Nothing -- a ⊤-valued neg element excludes nothing
        negArgVal -> case differenceTypeWithEnv typeEnv vaenv (posValSet posArgVal) negArgVal of
          (_, BottomType)    -> Nothing
          (outEnv, argVal')  -> narrowPieceAt (Just argName) p1{ptArgs=H.insert argName argVal' (ptArgs p1)} outEnv
    -- An arg valued by a var reference (@head=$/T@) subtracts on the partial's own var
    -- slot: the escape's narrowing comes back only as an env binding (the env-dropping
    -- wrappers return the bare var — the same resolve-meets-back rule as
    -- 'intersectTypesEnv'), so fold the discovered CONCRETE bindings onto the piece's
    -- DECLARED 'ptVars' (a name the partial does not declare is another scope's var and
    -- is dropped). A var slot narrowed to ∅ makes the escape piece uninhabited.
    -- The stamp obeys the same occurrence-support rule as the intersect's pointer-join
    -- ('varStampSupported'): a binding whose declared occurrences still hold bare-⊤
    -- content is withheld from 'ptVars', and its narrowing stays ON the subtracted
    -- slot instead (the bare pointer resolves to the discovered value) — the two op
    -- families then spell the unsupported case identically, keeping @A∩B = A−(A−B)@.
    -- Subtracting an ARG stamps only the variables that arg holds as a DIRECT pointer
    -- occurrence (@head -> $T@). A LINKED occurrence (@tail -> ConsList[$T]@) contributes to
    -- a variable by joining with the direct ones and never starts a binding on its own — the
    -- intersection's pointer join ('pointerJoinFor') withholds on an unresolved direct
    -- occurrence exactly so — so its narrowing stays on the subtracted slot. Stamped here, a
    -- tail narrowed to Integer lists would pin @/T=Integer@ and with it the head, which the
    -- meet of the same operands leaves free.
    narrowPiece = narrowPieceAt Nothing
    narrowPieceAt mArg piece outEnv
      | H.null varBindings = Just piece
      | any isBottomType (H.elems vars') = Nothing
      | otherwise = Just piece{ptVars=vars', ptArgs=fmap resolveWithheld (ptArgs piece)}
      where
        newBindings = H.filter (not . isTypeVar) $ H.difference outEnv vaenv
        varBindings = H.filterWithKey (\k _ -> k `H.member` ptVars piece && directAt k) $ fst $ splitVarArgEnv (envAmbient newBindings)
        directAt n = case mArg of
          Nothing -> True
          Just argName -> any (\args -> case H.lookup argName args of
                                   Just (TypeVar (TVVar n') TVInt _) -> n' == n
                                   _                                 -> False)
                              (ptArgs p1 : map ptArgs (typeGraphData typeEnv (ptName p1) ++ typeGraphArrowTemplates typeEnv (ptName p1)))
        supported n = not (shared n) && varStampSupported typeEnv (ptName piece) [ptArgs piece] (H.keysSet (ptArgs piece)) (ptArgs piece) (TVVar n)
        (stamped, withheld) = (H.filterWithKey (\n _ -> supported n) varBindings, H.filterWithKey (\n _ -> not (supported n)) varBindings)
        vars' = H.unionWith (intersectTypesEnv typeEnv vaenv) (ptVars piece) stamped
        resolveWithheld t@(TypeVar (TVVar n) TVInt _) = fromMaybe t (H.lookup n withheld)
        resolveWithheld t = t
    -- A neg type-parameter the pos partial does not spell still subtracts: the pos side is ⊤
    -- on that dimension (see 'PartialType') and escapes any neg slot that is a proper subset.
    -- Skipping it would make @P − P[/T=X]@ empty for every X, so @A−(A−B)@ would return A
    -- where @A∩B@ narrows.
    --
    -- Two conditions keep the subtraction honest, and both are load-bearing.
    --
    -- The neg slot must denote a PROPER subset. A neg slot that itself denotes ⊤ — a bare
    -- @$T@ that is unbound, ⊤-bound or var-valued — leaves no escape on that dimension, and
    -- subtracting it anyway manufactures a bottom-element leaf: the universe @Cons{}@ minus
    -- the class member @Cons(…, /T=$T)@ gives @Cons[/T = ⊤−$T = ∅]@, which then empties every
    -- intersection against it (a partial-application subset @Cons(head)@ reading as ⊆ its
    -- class by difference while intersect/subtype correctly read it as disjoint). The test is
    -- the same "only a CONCRETE binding resolves" one 'partialCoversSat'.'ambientRef' and
    -- 'isSubPartialOfWithEnv'.'flexibleVar' use.
    --
    -- The parameter must be one the pos partial's DECLARATION has ('declaresVar'). This also
    -- bounds the cost: only declared dimensions can produce an escape piece, and the pieces
    -- multiply per dimension.
    subtractVar (_, PTopType) = Nothing
    subtractVar (n, _) | shared n = Nothing
    subtractVar (varName, negVarVal) = case H.lookup varName (ptVars p1) of
      -- A var-valued pos slot (a flexible self-entry @/T=$/T@) subtracts like a
      -- var-referencing arg: the difference returns the bare var and records the
      -- narrowing only in the env, so fold it back via 'narrowPiece' (@$/T − (⊤−X)@
      -- narrows the piece to @/T=X@ rather than keeping the full flexible piece).
      Just posVarVal -> case differenceTypeWithEnv typeEnv vaenv posVarVal negVarVal of
        (_, BottomType)   -> Nothing
        -- ...and a var the env ALREADY binds narrows there too, not only a fresh one
        -- ('foldDiscovered'): @Pair[/B=$_a](/second=$/B) − Pair[/B=X](…)@ with @a@ bound must
        -- narrow the piece below @a@'s binding, not keep it and subtract nothing.
        -- Resolve the narrowing back into the SLOT, the same resolve-meets-back rule
        -- 'intersectTypesEnv' applies: a slot holding an EXTERNAL occurrence (@/T=$_b@)
        -- comes back as the bare var with its narrowing only in the env, recorded under
        -- the AMBIENT name @/b@ — which is not one of this partial's own parameters, so
        -- 'narrowPiece' below (which folds only the partial's own names) drops it and the
        -- escape piece reads exactly as the unsubtracted original. Folding the discovered
        -- CONCRETE bindings into the slot keeps the narrowing where the piece can carry it;
        -- a binding to another variable stays a link, as everywhere else.
        (outEnv, varVal') ->
          narrowPiece p1{ptVars=H.insert varName (foldDiscovered typeEnv vaenv outEnv varVal') (ptVars p1)} outEnv
      Nothing
        | declaresVar varName, negProperSubset negVarVal ->
            case differenceTypeWithEnv typeEnv vaenv PTopType (negValSet negVarVal) of
              (_, BottomType)   -> Nothing
              (outEnv, varVal') -> narrowPiece p1{ptVars=H.insert varName varVal' (ptVars p1)} outEnv
        | otherwise -> Nothing
    -- Only a parameter the pos partial's declaration has is a dimension of its values. A neg
    -- spelling can carry names the pos name never declares (a class member's own binder, an
    -- enclosing scope's parameter); those are not unspelled-at-⊤ but not dimensions of @p1@
    -- at all, as @/Nil@ has no element dimension inside @ConsList[$T]@.
    --
    -- With NO declaration in the graph the name's dimensions are unknown, and the neg's own
    -- spelling is the only evidence there is. Both partials carry the SAME name here (the
    -- leafs subtract name-grouped), so a parameter the neg spells IS a dimension of that
    -- name; reading "no declaration" as "no dimensions" instead drops every escape and
    -- makes @a − b@ empty for a @b@ that is a proper subset of @a@. Declarations are what
    -- bound the cost, so the enumeration stays bounded wherever the graph has one.
    declaresVar n = case typeGraphData typeEnv (ptName p1) of
      []    -> True
      decls -> any (H.member n . ptVars) decls
    -- Does the neg parameter denote a proper subset of ⊤? A knowledge-carrying occurrence
    -- (@$_a∩{X}@) always does; a bare variable does only when the env binds it to a concrete
    -- value — a ⊤-bound, self-bound or var-valued entry stays bindable per value, i.e. ⊤.
    negProperSubset t0 = case negValSet t0 of
      PTopType                 -> False
      t@(TypeVar v _ PTopType) -> case vaenvLookup typeEnv vaenv v of
        PTopType                  -> False
        b | b == t || isTypeVar b -> False
          | otherwise             -> True
      _                        -> True

-- | Difference of two types (t1 − t2 = t1 ∩ ¬t2).
differenceTypeWithEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> (TypeVarArgEnv, Type)
differenceTypeWithEnv TypeEnv{teDebug=True} _ t1 t2 | trace (printf "[DIFF] %s - %s" (show t1) (show t2)) False = undefined
differenceTypeWithEnv typeEnv vaenv t1 t2 = intersectTypesWithVarEnv typeEnv vaenv t1 (complementTypeEnv typeEnv vaenv t2)

-- | Complement of a type (¬t).
complementTypeEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
-- ¬(PosPartials leafs consts): the universe minus those leafs and constants
complementTypeEnv _ _ (UnionType Nothing leafs consts) = UnionType (Just (PredsNone, leafs, consts)) H.empty []
-- ¬PTopType = BottomType
complementTypeEnv _ _ PTopType = BottomType
-- ¬(top ∪ pos ∪ cs) = ¬top ∩ ¬(pos ∪ cs)
complementTypeEnv typeEnv vaenv (UnionType (Just topData) pos cs)
  | not (H.null pos) || not (null cs) =
      intersectTypesEnv typeEnv vaenv
        (complementTypeEnv typeEnv vaenv (UnionType (Just topData) H.empty []))
        (complementTypeEnv typeEnv vaenv (UnionType Nothing pos cs))
-- ¬(U_preds − negPartials − negConsts) = ¬U_preds ∪ negPartials ∪ negConsts
complementTypeEnv typeEnv vaenv (UnionType (Just (preds, negPartials, negConsts)) (H.null -> True) [])
  | H.null negPartials && null negConsts = notPreds
  | preds == PredsNone = UnionType Nothing negPartials negConsts
  | otherwise = unionTypesWithEnv typeEnv vaenv (UnionType Nothing negPartials negConsts) notPreds
  where
    notPreds = UnionType (Just (predsNot preds, H.empty, [])) H.empty []
complementTypeEnv typeEnv vaenv (TypeVar v _ i) = complementTypeEnv typeEnv vaenv (intersectTypesEnv typeEnv vaenv (vaenvLookup typeEnv vaenv v) i)
complementTypeEnv _ _ t = error $ printf "complementTypeEnv: unhandled case %s" (show t)

-- | Meets a resolved variable value with an occurrence's INNER (the @var ∩ inner@ reading).
-- The ⊤ short-circuits make a knowledge-free occurrence (@inner = PTopType@) read exactly as
-- the bare variable. The general clause needs the REAL 'TypeEnv': a class-predicate inner
-- (@∀Bag@) cannot be met by a class-graph-blind meet ("Unknown class … in expandPartial"), so
-- the whole substitute\/scope family threads the environment through to here.
meetInner :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Type
meetInner _ _ b PTopType    = b
meetInner _ _ PTopType i    = i
meetInner typeEnv vaenv b i = intersectTypesEnv typeEnv vaenv b i
