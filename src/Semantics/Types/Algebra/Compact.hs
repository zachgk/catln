-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.Algebra.Compact
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Compaction: rewriting a type into a canonical, smaller spelling of the same set ('compactType').
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns    #-}

module Semantics.Types.Algebra.Compact where

import                          Data.Either
import                qualified Data.HashMap.Strict             as H
import                qualified Data.HashSet                    as S
import                qualified Data.List                       as L
import                          Data.Maybe
import                          Semantics.Types.Algebra.Expand
import                          Semantics.Types.Algebra.Preds
import {-# SOURCE #-}           Semantics.Types.Algebra.SetOps  (intersectTypesEnv,
                                                                 unionAllTypes,
                                                                 unionAllTypesWithEnv,
                                                                 unionTypesWithEnv)
import                          Semantics.Types.Algebra.Subtype
import                          Semantics.Types.ArgModes
import                          Semantics.Types.Construct
import                          Semantics.Types.Data
import                          Semantics.Types.Name
import                          Semantics.Types.Preds
import                          Semantics.Types.SatGoal
import                          Semantics.Types.Vars
import                          Text.Printf
import                          Utils


-- | A conjunction with two value-predicate atoms that are one form ('predExprSameForm') carries the
-- form once, as the merged spelling. Guarded by a cheap scan for two value atoms, so the common
-- conjunctions never pay the comparison.
mergeSameFormPredExprs :: TypePredicates -> TypePredicates
mergeSameFormPredExprs preds
  | length [() | PredsOne (PredExpr _) <- conj] < 2 = preds
  | kept == conj                                     = preds
  | otherwise                                        = mkAnd kept
  where
    conj = predsConjuncts preds
    kept = foldl step [] conj
    step acc a@(PredsOne (PredExpr e)) = go [] acc
      where
        go pre []                                = pre ++ [a]
        go pre (PredsOne (PredExpr e') : post)
          | Just m <- predExprSameForm e' e      = pre ++ PredsOne (PredExpr m) : post
        go pre (x : post)                        = go (pre ++ [x]) post
    step acc a = acc ++ [a]

-- | Whether a formula is empty by declaration alone: every disjunct of its DNF requires an atom
-- both positively and negatively, or requires a positive atom that a declared edge makes imply one
-- of its negated atoms (@~Lizard ∧ ¬~Animal@ under @every(Lizard isa Animal)@). Syntactic — no
-- enumeration and no engine search — so it holds in the open world and is cheap enough to ask of
-- every predicated leaf a compaction sees.
predsDeclContradictory :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> TypePredicates -> Bool
predsDeclContradictory typeEnv vaenv preds = case predsSatDNF True preds of
  Just goals@(_:_) -> all contradictory goals
  _                -> False
  where
    contradictory g = any (`elem` sgNeg g) (sgPos g)
      || or [satAtomImpliesDeclared typeEnv vaenv p n | p <- sgPos g, n <- sgNeg g]

-- | Takes constants out of a type: each is dropped from the listed constants, a leaf that may hold
-- it keeps every point but it (@l|¬PredConstEq c@), and a predicate top excludes it as a point.
--
-- A leaf MAY hold the constant unless it is proven not to: by its shape (a primitive leaf of
-- another type, a data leaf) or by its own constant equalities read at the constant
-- ('predsAtConst'). There is no environment here to decide a class atom or a value predicate, so
-- a leaf whose formula is undecided at the constant is excluded from like any other: read as
-- holding the constant only when proven to ('constantCoveredByLeafs'), a leaf within a value
-- predicate kept the excluded point, and the point leaf @/Char | PredConstEq c ∧ ¬P@ survived
-- @⊤ − {c}@ whole. An exclusion that removes nothing is dropped again where the environment can
-- tell ('compactConstEqLeafs').
excludeConstants :: [Constant] -> Type -> Type
excludeConstants [] t = t
excludeConstants cs (UnionType mtop leafs consts) =
  UnionType (fmap onTop mtop) (joinUnionType (map onLeaf (splitUnionType leafs))) (filter (`notElem` cs) consts)
  where
    onTop (preds, negLeafs, negConsts) = (preds, negLeafs, L.nub (negConsts ++ cs))
    onLeaf l = foldl (\l' c -> if mayHold c l'
                                  then l'{ptPreds = predsAnd (ptPreds l') (PredsNot (PredsOne (PredConstEq c)))}
                                  else l') l cs
    mayHold c l = isJust (predsAtConst c (ptPreds l)) && constantCoveredByLeafs c (joinUnionType [l{ptPreds = PredsNone}])
excludeConstants _ t = t

-- | Whether a formula holds a 'PredConstEq' atom anywhere.
predsHasConstEq :: TypePredicates -> Bool
predsHasConstEq (PredsOne PredConstEq{}) = True
predsHasConstEq (PredsOne _)             = False
predsHasConstEq (PredsAnd ps)            = any predsHasConstEq ps
predsHasConstEq (PredsNot p)             = predsHasConstEq p

-- | Canonicalises the 'PredConstEq' conjuncts of a union's leafs, returning the leafs and the
-- constants they became:
--
--   * a leaf conjoining @PredConstEq c@ is at most the one value @c@: it becomes that constant
--     when the rest of the leaf holds it, and nothing otherwise — except that a VALUE predicate
--     beside it ('isValuePredLiteral') is decided neither way on the constant, so the leaf stays,
--     spelled as the constant within those predicates alone (@/Float | PredConstEq c ∧ P@), unless
--     the union lists @c@ itself, which it is then inside of;
--   * a @¬PredConstEq c@ conjunct excludes nothing from a leaf that cannot hold @c@ (by shape, or
--     by the rest of its formula read at @c@), and nothing from a union that lists @c@ itself
--     (@(l − {c}) ∪ {c} = l@), so it drops.
--
-- So the same set has one spelling: a constant is a constant, and an exclusion is spelled only
-- where it removes a point.
compactConstEqLeafs :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> [Constant] -> PartialLeafs -> (PartialLeafs, [Constant])
compactConstEqLeafs typeEnv vaenv consts leafs
  | not (any (predsHasConstEq . ptPreds) ls) = (leafs, [])
  | otherwise = (joinUnionType [l | Left l <- res], [c | Right c <- res])
  where
    ls = splitUnionType leafs
    res = mapMaybe onLeaf ls
    -- The leaf's formula read at the constant ('predsAtConstant'): false, and the leaf is empty;
    -- true, and it is the constant; left over value predicates, and it is the constant within
    -- them — unless the union lists the constant itself, which it is then inside of.
    onLeaf l = case [c | PredsOne (PredConstEq c) <- conjs (ptPreds l)] of
      c:_ | constantCoveredByLeafs c (joinUnionType [l{ptPreds = PredsNone}]) -> case predsAtConstant typeEnv vaenv c (ptPreds l) of
              Nothing        -> Nothing
              Just PredsNone -> Just (Right c)
              Just r | c `elem` consts -> Just (Right c)
                     | otherwise       -> Just (Left l{ptPreds = rebuild (PredsOne (PredConstEq c) : conjs r)})
          | otherwise -> Nothing
      [] -> Just (Left l{ptPreds = rebuild (filter (not . vacuous l) (conjs (ptPreds l)))})
    -- An exclusion is vacuous on a leaf that cannot hold the constant: by shape, or because the
    -- rest of its formula is false at it ('predsAtConstant' — a @/Char | ~Even@ holds no @'a'@).
    vacuous l q@(PredsNot (PredsOne (PredConstEq c))) =
      c `elem` consts || not (constantCoveredByLeafs c (joinUnionType [l{ptPreds = PredsNone}]))
        || isNothing (predsAtConstant typeEnv vaenv c (rebuild (filter (/= q) (conjs (ptPreds l)))))
    vacuous _ _ = False
    conjs (PredsAnd xs) = concatMap conjs xs
    conjs x             = [x]
    rebuild []  = PredsNone
    rebuild [q] = q
    rebuild qs  = mkAnd qs

-- | Where a constant stands relative to a predicate top @(preds, negLeafs, negConsts)@: 'Nothing'
-- when it is outside — a class or relation atom it does not satisfy, an excluded constant, an
-- excluded leaf holding it — @Just []@ when it is inside, and @Just lits@ when its membership
-- turns on VALUE predicates decided neither way on it ('isValuePredLiteral'): the top's own value
-- literals, and the negation of what an excluded leaf holding the constant's type still requires
-- of it (@⊤ − (/Char | PredConstEq c ∧ ¬P)@ holds @c@ exactly where @¬P@ fails). The constant is
-- then the leaf @/Char | PredConstEq c ∧ lits@, how a meet spells it ('intersectTypesWithVarEnv').
-- 'constantInType' is the proven-membership reading of the same question.
constantWithinTop :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Constant -> (TypePredicates, PartialLeafs, [Constant]) -> Maybe [TypePredicates]
constantWithinTop typeEnv vaenv c (preds, negLeafs, negConsts)
  | c `elem` negConsts = Nothing
  | otherwise = do
      residual <- predsAtConstant typeEnv vaenv c preds
      excluded <- foldr excluding (Just []) (splitUnionType negLeafs)
      let lits = predsConjuncts residual ++ excluded
      -- Within @¬P@ by the top and within @P@ by an excluded leaf (@⊤|¬P − (/Integer | PredConstEq
      -- c ∧ ¬P)@): the constant is outside, whatever @P@ means.
      if any (\l -> predsNot l `elem` lits) lits then Nothing else Just lits
  where
    -- An excluded leaf read at the constant: one whose shape cannot hold @c@, or whose formula is
    -- false at it, excludes nothing; one holding @c@ outright excludes it; one holding @c@ up to
    -- value predicates excludes it exactly where they hold, so the constant stays within their
    -- negation.
    excluding _ Nothing    = Nothing
    excluding l (Just acc)
      | not (constantCoveredByLeafs c (joinUnionType [l{ptPreds = PredsNone}])) = Just acc
      | otherwise = case predsAtConstant typeEnv vaenv c (ptPreds l) of
          Nothing        -> Just acc
          Just PredsNone -> Nothing
          Just r         -> Just (predsNot r : acc)

-- |
-- Join partials by checking if one is a subset of another (redundant) and removing it.
-- TODO: This currently joins only with matching names. More matches could improve the effectiveness of the compaction, but slows down the code significantly
compactOverlapping :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialLeafs -> PartialLeafs
compactOverlapping typeEnv vaenv = joinUnionTypeByName . fmap ((aux . reverse) . aux) . splitUnionTypeByName
  where
    -- Tests each partial against all following partials to check if it is already handled by it
    aux [] = []
    aux (partial:rest) = if any (partialAlreadyCovered partial) rest
      then aux rest
      else partial : aux rest

    -- checks if a partial is covered by the candidate from rest
    partialAlreadyCovered PartialType{ptArgMode=PtArgMode _ False _} PartialType{ptArgMode=PtArgMode _ False _} = False -- Don't count coverings between upper-unbounded partials because it loses type information
    partialAlreadyCovered partial restPartial = isSubtypeOfWithEnv typeEnv vaenv (singletonType partial) (singletonType restPartial)

-- |
-- Joins partials with only one difference between their args or vars. Then, it can join the two partials into one partial
-- TODO: Should check if preds are suitable for joining
compactJoinPartials :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialLeafs -> PartialLeafs
compactJoinPartials typeEnv vaenv partials = joinUnionType $ concat $ H.elems $ fmap joinMatchArgPartials $ H.fromListWith (++) $ map prepGroupJoinable $ splitUnionType partials
  where
    -- group partials by name, argSet, and varSet to check for joins
    prepGroupJoinable partial@PartialType{ptName, ptArgs, ptVars, ptArgMode} = ((ptName, H.keysSet ptArgs, H.keysSet ptVars, ptArgMode), [partial])

    -- Checks pairs of tuples for joins
    joinMatchArgPartials []     = []
    joinMatchArgPartials (p:ps) = joinMatchArgPartialsAux p ps []
    joinMatchArgPartialsAux curPartial [] tried = curPartial:joinMatchArgPartials tried
    joinMatchArgPartialsAux curPartial (toTry:toTrys) tried = case tryJoin curPartial toTry of
      Just joined -> joinMatchArgPartialsAux joined toTrys tried
      Nothing     -> joinMatchArgPartialsAux curPartial toTrys (toTry:tried)

    -- if two partials differ by only one arg or var, joins them else Nothing
    -- 'numDifferences'/'differingPairs' compare shared keys only, which is safe here because
    -- 'prepGroupJoinable' keys the group on @H.keysSet ptVars@: two partials whose parameter key
    -- sets differ are never in the same group and never reach 'tryJoin', so a one-sided key
    -- cannot be miscounted as no difference. Within a group the key sets are equal, where
    -- 'H.unionWith' and 'H.intersectionWith' agree. Spellings that differ only by an unspelled
    -- vs ⊤-valued parameter (@/Some@ vs @/Some[/T=⊤]@) land in different groups and are merged
    -- by 'partialAlreadyCovered' absorption, which covers them both ways through
    -- 'isSubtypeOfWithEnv'.
    -- The join is exact only when the differing slot's union is itself exactly representable:
    -- with a bare type-var on one side (a partial-local pointer such as @/m=$/M@ against a
    -- concrete @/m=Cons(..)@), 'unionTypes' resolves the var through the (empty) env to ⊤,
    -- so the joined leaf would be strictly WIDER than the two originals. Keep such leaves
    -- separate instead — an unmerged union is always exact.
    -- An argument slot sits one level inside its partial, so its references to the partial's
    -- parameters (@fn(v)=/fn[/A=$_/A](..)@) are joined in that scope ('comparedPartialsEnv'), as
    -- they are compared. Joined under the outer env alone, they would be out of scope there and
    -- read as ⊤, so two slots that differ only in which parameter they name would collapse into one.
    -- A parameter slot sits at the partial's own level and is joined under the outer env.
    -- A higher-order input naming one of its partial's parameters (@fn[/A=$^/A](v=$/A)@) reads
    -- through that parameter — at every instance of the data, where the data pins it
    -- ('pinnedInputSlot') — which the slot alone does not carry, so its union on its own is not
    -- exact either: the two leafs are kept apart.
    tryJoin p1@(PartialType name1 vars1 args1 PredsNone mode1) p2@(PartialType _ vars2 args2 PredsNone _) = if numDifferences args1 args2 + numDifferences vars1 vars2 == 1
         && all exactSlotJoin (differingPairs args1 args2 ++ differingPairs vars1 vars2)
         && not (or [ inputNamesParams p (k, v) | (k, (v1, v2)) <- H.toList (H.intersectionWith (,) args1 args2), v1 /= v2, (p, v) <- [(p1, v1), (p2, v2)] ])
      then Just $ PartialType name1 (joinMap vaenv vars1 vars2) (joinMap (comparedPartialsEnv typeEnv vaenv p1 p2) args1 args2) PredsNone mode1
      else Nothing
    -- Excluded middle: two leafs identical but for one conjunct held positively on one and
    -- negated on the other are together the leaf without it (@X|P∧q ∪ X|P∧¬q = X|P@). Left
    -- apart, the pair is a spelling of @X|P@ no single-leaf subtype check recognises, so a cover
    -- that holds by the split (a slot partition, a difference) read as refused.
    tryJoin p1@PartialType{ptPreds=preds1} p2@PartialType{ptPreds=preds2}
      | p1{ptPreds=PredsNone} == p2{ptPreds=PredsNone}
      , Just rest <- complementaryConj (conjs preds1) (conjs preds2)
      = Just p1{ptPreds = rebuild rest}
    tryJoin _ _ = Nothing
    conjs PredsNone     = []
    conjs (PredsAnd qs) = concatMap conjs qs
    conjs q             = [q]
    rebuild []  = PredsNone
    rebuild [q] = q
    rebuild qs  = mkAnd qs
    complementaryConj cs1 cs2 = case (cs1 L.\\ cs2, cs2 L.\\ cs1) of
      ([q1], [q2]) | q2 == predsNot q1 || q1 == predsNot q2 -> Just (L.intersect cs1 cs2)
      _ -> Nothing

    inputNamesParams p (k, v) = not (S.null (pkArgs k)) && not (S.null (S.intersection (typeOuterVarRefs v) (H.keysSet (ptVars p))))
    numDifferences m1 m2 = sum $ fromEnum <$> H.intersectionWith (/=) m1 m2
    differingPairs m1 m2 = filter (uncurry (/=)) $ H.elems $ H.intersectionWith (,) m1 m2
    exactSlotJoin (v1, v2) = not (isTypeVar v1) && not (isTypeVar v2)
    joinMap env = H.unionWith (unionTypesWithEnv typeEnv env)

-- | Processes partials that have a 'PredClass' predicate. Takes an already-split list of
-- partials (the fused 'compactPartialLeafs' pipeline splits once up front) and produces the
-- merged 'PartialLeafs'. 'unionPartialLeafs' is an order-independent merge, so the result is
-- the same set regardless of the input list's order.
compactPartialsWithClassPred :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> [PartialType] -> PartialLeafs
compactPartialsWithClassPred typeEnv vaenv partials = unionPartialLeafs $ map tryAux partials
  where
    tryAux :: PartialType -> PartialLeafs
    -- Split the formula's TOP-LEVEL conjuncts: positive class/rel atoms are expanded and
    -- intersected back in; everything else (expr atoms and ¬-conjuncts) stays attached on
    -- the partial and rides through the meet. A ¬-conjunct does not suppress expansion
    -- of its positive siblings — @Cons₂{¬EqC ∧ ~Cons}@ must reach the same ∅ the
    -- association order @~Cons ∩ (Cons₂{¬EqC})@ computes, and @leaf{¬X ∧ EqC}@ the same
    -- expanded @leafEqC{¬X}@ the meet route spells.
    tryAux partial
      | isPredsContradictory (ptPreds partial) || predsDeclContradictory typeEnv vaenv (ptPreds partial) = H.empty
      | otherwise = if null classPreds && null relPreds
      then joinUnionType [partial]
      -- Expand the partial's own class/rel preds and intersect them back in.
      -- Expansions restricted to the partial's OWN name: the fold is
      -- @partial' ∩ E₁ ∩ … ∩ Eₖ@ with a single-name positive leaf, so a member of any
      -- other name meets to ∅ under every association order, and Just-top members stay
      -- symbolic ('expandClassPartialWithinNames') exactly as the full expansion keeps them.
      else asLeafs $ foldr1 (intersectTypesEnv typeEnv vaenv) (singletonType partial' : map (expandRelPartialWithinNames typeEnv vaenv ownName) relPreds ++ map (expandClassPartialWithinNames typeEnv vaenv ownName) classPreds)
      where
        -- Take the positive class/rel atom conjuncts out of partial. Only ENUMERABLE ones are
        -- expanded: an unsealed class (or a relation reaching one) has members beyond those
        -- declared so far, so outside 'tePrgmEnv' its expansion is its current members, not
        -- the class. Such an atom stays attached to the partial as a conjunct, which is exact.
        -- The expansion here is restricted to the partial's own name, so a partial whose
        -- memberships are already fixed ('knownMemberName') expands every atom.
        (allClassPreds, allRelPreds, residConjs0) = splitConjs (conjsOf (ptPreds partial))
        ownKnown = tePrgmEnv typeEnv || knownMemberName typeEnv (ptName partial)
        (relPreds, openRelPreds)
          | ownKnown = (allRelPreds, [])
          | otherwise = L.partition (all (\c -> predsSealed typeEnv (PredsOne (PredClass c))) . relMatchingClasses typeEnv) allRelPreds
        (classPreds, openClassPreds)
          = L.partition (\c -> ownKnown || predsSealed typeEnv (PredsOne (PredClass c))) allClassPreds
        residConjs = map (PredsOne . PredClass) openClassPreds ++ map (PredsOne . PredRel) openRelPreds ++ residConjs0
        partial' = partial{ptPreds = rebuildConjs residConjs}
        ownName = S.singleton (ptName partial')

        -- A constant the meet produced (a leaf @/Char | PredConstEq c ∧ …@ whose predicates all
        -- held, compacted to the point @c@) is that leaf again: the one spelling leafs have for
        -- a point.
        asLeafs (UnionType Nothing leafs consts) = H.unionWith S.union leafs (joinUnionType [ (constantPartialType c){ptPreds = PredsOne (PredConstEq c)} | c <- consts ])
        asLeafs (UnionType (Just (_, negLeafs, _)) posLeafs []) = H.unionWith S.union negLeafs posLeafs  -- shouldn't normally occur
        asLeafs t = error $ printf "compactPartialsWithClassPred: unexpected type %s" (show t)

        conjsOf (PredsAnd ps) = concatMap conjsOf ps
        conjsOf q             = [q]
        splitConjs :: [TypePredicates] -> ([PartialType], [PartialType], [TypePredicates])
        splitConjs [] = ([], [], [])
        splitConjs (q:qs) = case q of
          PredsOne (PredClass p') -> (p':classes', rels', resid')
          PredsOne (PredRel p')   -> (classes', p':rels', resid')
          _                       -> (classes', rels', q:resid')
          where
            (classes', rels', resid') = splitConjs qs
        rebuildConjs []  = PredsNone
        rebuildConjs [q] = q
        rebuildConjs qs  = mkAnd qs

-- | Checks if a partial has predicates that turn the partial into a 'bottomType' (e.g. a partial with contradictory preds such as isClass A and not isClass A).
-- Operates on a list of partials in the fused pipeline (no intermediate join); each partial is mapped/dropped independently.
compactDisconnectedPreds :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> [PartialType] -> [PartialType]
compactDisconnectedPreds typeEnv vaenv = mapMaybe aux
  where
    -- NOTE: deliberately NOT decided by the symbolic engine. These predicates constrain the
    -- eventual term-rewritten value of a call-shaped partial (e.g. @isLess | PredClass Boolean@),
    -- not the partial's own name, so name-membership emptiness does not apply here.
    aux partial@PartialType{ptPreds=PredsNone} = Just partial
    aux partial@PartialType{ptPreds} = case intersectTypesEnv typeEnv{teDisableCompact=True} vaenv (singletonType partialWithoutPreds) (TopType H.empty ptPreds) of

                                        -- | If the intersection of the partial with its preds has no overlap then it is a bottom type
                                        intersection | isBottomType intersection -> Nothing

                                        -- | If the intersection of the partial with its preds fully overlaps, then the preds are redundant.
                                        -- A meet that keeps the predicates attached hands back the partial itself, which
                                        -- lies within the partial without them (it is that partial narrowed), so only
                                        -- the other inclusion is asked.
                                        intersection | intersection == singletonType partial -> if isSubtypeOfWithEnv typeEnv vaenv (singletonType partialWithoutPreds) intersection then Just partialWithoutPreds else Just partial
                                        intersection | isEqTypeWithEnv typeEnv vaenv intersection (singletonType partialWithoutPreds) -> Just partialWithoutPreds

                                        _ -> Just partial
      where
        partialWithoutPreds = partial{ptPreds = PredsNone}

-- | Removes partials which contain a type variable that is the 'bottomType', because then the whole partial is a 'bottomType'.
-- Operates on a list of partials in the fused pipeline (a pure filter; no join).
compactBottomType :: [PartialType] -> [PartialType]
compactBottomType = filter (not . containsBottomPartialType)

-- | Materializes a partial-local variable reference in an argument (@/v=$T@, 'TVInt') when the
-- partial's own 'ptVars' resolves that variable to a fully concrete type: @Box[T=Integer](v=$T)@
-- → @Box[T=Integer](v=Integer)@. Two orders of resolving the same value otherwise produce
-- incomparable syntactic forms — field-resolved @Box[T=$_T](v=Integer)@ vs param-resolved
-- @Box[T=Integer](v=$T)@ — which 'verifySchemeActLowers' cannot order, so an equivalent re-write
-- is rejected as a grow. Only a fully concrete binding is materialized: a variable-form binding
-- is a live link that must stay symbolic (e.g. recursive element linkage), and a Top or
-- predicate-form binding is a bound, not a resolution.
compactOwnVarRefs :: PartialType -> PartialType
compactOwnVarRefs p@PartialType{ptVars, ptArgs} = p{ptArgs = fmap subst ptArgs}
  where
    subst t@(TypeVar (TVVar v) TVInt _) = case H.lookup v ptVars of
      Just b | typeFullyConcrete b -> b
      _                            -> t
    subst t = t

-- The per-leaf stages (from 'compactBottomType' through 'compactPartialsWithClassPred') are
-- fused over a single 'splitUnionType': all but the last are pure per-element list
-- filters/maps and 'compactPartialsWithClassPred' ends in the order-independent
-- 'unionPartialLeafs' merge, so the 'PartialLeafs' it produces is identical regardless of
-- intermediate list order. Fusing avoids intermediate join+split round-trips (each of which
-- re-hashes every partial); the order-sensitive 'compactJoinPartials' \/ 'compactOverlapping'
-- stages then run on joined 'PartialLeafs'.
compactPartialLeafs :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialLeafs -> PartialLeafs
compactPartialLeafs typeEnv vaenv =
  compactOverlapping typeEnv vaenv
  . compactJoinPartials typeEnv vaenv
  -- ¬-conjunct reduction FIRST: once a vacuous negation is stripped, the leaf's remaining
  -- positive preds become visible to 'compactPartialsWithClassPred' (a ¬-carrying formula
  -- is outside its 'tryPredsToList' shape, so a redundant positive conjunct would
  -- otherwise survive on exactly the leafs this stage rewrites).
  . compactPartialsWithClassPred typeEnv vaenv
  . compactNegPredLeafs typeEnv vaenv
  . compactModedPredLeafs typeEnv
  . compactDisconnectedPreds typeEnv vaenv
  . map compactOwnVarRefs
  . compactFalsePredExprLeafs typeEnv vaenv
  . compactSameFormPredExprLeafs
  . compactBottomType
  . splitUnionType

-- | Splits a NON-exact-mode leaf carrying decidable predicate atoms into its
-- 'ptIntervalArgKeys' exact pieces (predicates preserved on each piece). A moded leaf's
-- predicate cannot be decided as a whole — @Cons(..)??@ spans arg-set pieces on both
-- sides of a membership atom (the exact-args form IS a @ConsList@ member while the
-- partial-application forms are NOT), so any whole-leaf decision ('compactNegPredLeafs',
-- the class-pred expansion) over-keeps or over-drops some piece. Distributing the
-- predicate over the interval is exact, and each exact piece is then decided by the
-- per-leaf rules, reaching the same canonical form as the materialized-meet route. A
-- leaf whose predicates hold only 'PredExpr' content is left moded: a value predicate is
-- decided on the leaf as it is spelled ('compactFalsePredExprLeafs'), never by decomposing
-- it, so the undecomposed moded spelling stays canonical.
compactModedPredLeafs :: (TypeGraph tg) => TypeEnv tg -> [PartialType] -> [PartialType]
compactModedPredLeafs typeEnv = concatMap decompose
  where
    decompose p
      | predsHasClassRelAtom (ptPreds p)
      , p' <- normalizePtArgMode typeEnv p
      , ptArgMode p' /= PtArgExact
      , Just kss <- ptIntervalArgKeys typeEnv p'
      = map (ptAtArgKeys p') kss
      | otherwise = [p]

-- | Canonicalizes leafs carrying a NEGATED predicate conjunct (the symbolic refinement the
-- ¬-over-recursive-class meet attaches — see 'intersectTypesWithVarEnv'). Each top-level
-- @¬Q@ conjunct of a leaf's 'ptPreds' is decided against the BARE leaf (sound for both
-- rules: the leaf's other refinements only shrink it):
--
--   * leaf ⊆ U_Q ⟹ the refined leaf is uninhabited (@leaf ∧ ¬Q = ∅@) — drop the LEAF
--     (@Nil{¬EqC}@ when @Nil@ is an @EqC@ member);
--   * leaf ∩ U_Q = ∅ ⟹ the @¬Q@ conjunct is vacuous — drop the CONJUNCT
--     (@Point{¬(N ∧ Bag)}@ when @Point@ provably cannot satisfy @N ∧ Bag@).
--
-- Without these reductions the two spellings of one set (bare vs vacuously-refined) split
-- 'isEqType' and the distributivity\/bridge laws. Both questions recurse only into
-- POSITIVE-formula subtype\/meet machinery (one ¬-level is stripped per step, so a nested
-- negation bottoms out at the formula's finite depth).
compactNegPredLeafs :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> [PartialType] -> [PartialType]
compactNegPredLeafs typeEnv vaenv = mapMaybe (reduceNegPredLeaf typeEnv vaenv)

-- | Truth value of a predicate subformula relative to one leaf: decided for every value of
-- the leaf, decided for none, or left residual.
data LeafPredSimp = LeafPredTrue | LeafPredFalse | LeafPredResid TypePredicates

-- | The per-leaf reduction of 'compactNegPredLeafs': simplify the leaf's formula by deciding
-- subformulas against the BARE leaf (sound at any depth and polarity — a decision holds for
-- EVERY value of the leaf, so substituting the constant is pointwise-exact; the leaf's other
-- refinements only shrink it):
--
--   * leaf ⊆ U_q ⟹ @q@ is 'True' of every leaf value — substitute truth
--     (@Nil{¬EqC}@ when @Nil@ is an @EqC@ member reduces to 'False': drop the LEAF);
--   * leaf ∩ U_q = ∅ ⟹ @q@ holds of no leaf value — substitute falsity
--     (@Cons₂{¬EqC ∧ ~Cons}@: the argless-exact @~Cons@ is disjoint from the exact
--     two-arg leaf, so the whole conjunction is 'False' and the leaf drops;
--     @Cons₂{¬(EqC ∧ ¬~Integer)}@: @~Integer@ is 'False' on @Cons@ values, collapsing
--     the formula to the meet route's @¬EqC@ spelling).
--
-- Decisions are attempted at each formula node holding a class\/rel atom (a pure-'PredExpr'
-- subformula is undecidable either way — see 'compactDisconnectedPreds' on why membership
-- emptiness does not apply to expression predicates); an undecided node recurses
-- structurally. Only leafs with a ¬ somewhere are processed: positive-only formulas are the
-- canonical domain of 'compactPartialsWithClassPred'. Both questions recurse only into
-- POSITIVE-formula subtype\/meet machinery (one ¬-level is stripped per step, so a nested
-- negation bottoms out at the formula's finite depth); without these reductions the two
-- spellings of one set (bare vs vacuously-refined) split 'isEqType' and the
-- distributivity\/bridge laws.
reduceNegPredLeaf :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> PartialType -> Maybe PartialType
reduceNegPredLeaf typeEnv vaenv p
  | not (hasNegConj (ptPreds p)) = Just p
  | otherwise = case simplify (ptPreds p) of
      LeafPredFalse   -> Nothing
      LeafPredTrue    -> Just p{ptPreds = PredsNone}
      LeafPredResid f -> Just p{ptPreds = f}
  where
    bare = singletonType p{ptPreds=PredsNone}
    qTop q = UnionType (Just (q, H.empty, [])) H.empty []
    decide q
      | not (predsHasClassRelAtom q) = Nothing
      | otherwise = decideInFlight (p{ptPreds=PredsNone}, q) $
          if isSubtypeOfWithEnv typeEnv vaenv bare (qTop q) then Just True
          else if isBottomType (intersectTypesEnv typeEnv vaenv bare (qTop q)) then Just False
          else Nothing
    -- 'decide' is attempted only where the tested formula is ¬-SMALLER than the formula
    -- being reduced: at atoms (a ¬-free test) and at each 'PredsNot' operand (one ¬-level
    -- stripped), a well-founded descent. Deciding the WHOLE formula at the root or an And
    -- node would re-pose this same reduction from inside its own test (the meet attaches the
    -- formula to the bare leaf and compacts it) — a livelock, not just a cost.
    simplify f@(PredsOne _)            = decideSimp f
    simplify (PredsAnd ps)             = conjSimp (map simplify ps)
    simplify (PredsNot q@(PredsOne _)) = negSimp (decideSimp q)
    simplify (PredsNot q)              = negSimp $ case decide q of
      Just True  -> LeafPredTrue
      Just False -> LeafPredFalse
      Nothing    -> simplify q
    decideSimp q = case decide q of
      Just True  -> LeafPredTrue
      Just False -> LeafPredFalse
      Nothing    -> LeafPredResid q
    negSimp LeafPredTrue      = LeafPredFalse
    negSimp LeafPredFalse     = LeafPredTrue
    negSimp (LeafPredResid q) = LeafPredResid (predsNot q)
    conjSimp rs
      | any isFalseSimp rs = LeafPredFalse
      | null resids        = LeafPredTrue
      | otherwise          = LeafPredResid (rebuildConjs resids)
      where resids = [q | LeafPredResid q <- rs]
    isFalseSimp LeafPredFalse = True
    isFalseSimp _             = False
    hasNegConj (PredsAnd ps) = any hasNegConj ps
    hasNegConj (PredsNot _)  = True
    hasNegConj (PredsOne _)  = False
    rebuildConjs []  = PredsNone
    rebuildConjs [q] = q
    rebuildConjs qs  = mkAnd qs

-- | Drops a leaf whose value predicate is decided FALSE on it: @S(n=Z)|$this.isZ == True@ holds no
-- value, as the meet of an argument with a refinement it is outside. A predicate decided true is
-- left on the leaf: dropping it would respell the leaf, and a spelling must not depend on which
-- graph decided it — the type checker verifies a narrowing under the environment of the typed
-- dependencies alone ('TypeCheck.Common.verifyScheme'), where a predicate over the program being
-- checked is not decidable, and a leaf respelled where it is would be reported there as growth.
-- Emptied, a leaf is a narrowing under every environment; a predicate that holds answers the
-- subtype question instead ('isSubPartialOfWithEnv'). Only a positive conjunction is decided: a
-- negated value predicate is left to the formula rules.
compactFalsePredExprLeafs :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> [PartialType] -> [PartialType]
compactFalsePredExprLeafs typeEnv vaenv = filter (not . partialPredExprDecidedFalse typeEnv vaenv)

-- | A leaf's two spellings of one value-predicate atom are the one atom ('mergeSameFormPredExprs'),
-- as on a top.
compactSameFormPredExprLeafs :: [PartialType] -> [PartialType]
compactSameFormPredExprLeafs = map merge
  where
    merge l@PartialType{ptPreds=preds@PredsAnd{}} = l{ptPreds = mergeSameFormPredExprs preds}
    merge l                                       = l

-- |
-- Used to simplify and reduce the size of a 'Type'.
-- TODO: This should merge type partials into class partials
--
-- A type compaction produced is compact under every environment ('typeCompacted'), so it is
-- handed back as it is, and so is every compacted type nested in one being compacted: a meet or
-- union over compacted operands walks only what it built itself. One holding a value predicate is
-- not flagged: the predicate is decided through arrows a later epoch may have narrowed ('Type').
compactType :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
compactType TypeEnv{teDisableCompact=True} _ t = t
compactType _ _ t | typeCompacted t = t
compactType typeEnv vaenv t
  | holdsPredExpr t' = t'
  | otherwise = markCompacted t'
  where t' = compactTypeRaw typeEnv vaenv t

compactTypeRaw :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
-- Idempotence fast paths for already-canonical trivial types. 'compactType' is provably the
-- identity on each of these, yet the set algebra re-compacts them constantly — most calls are
-- on types with at most one leaf, many on the empty 'BottomType'. Short-circuiting here avoids
-- rebuilding the same structure through the whole pipeline.
--   * 'BottomType' (empty Nothing-top): 'compactPartialLeafs' on empty is empty, 'foldRecursiveType' leaves it.
--   * a single predicate-free partial with no args/vars: nothing to filter, join, overlap, or recurse into.
compactTypeRaw _ _ t@BottomType = t
compactTypeRaw _ _ t@(UnionType Nothing pos [])
  | [PartialArgsOption vars args PredsNone _] <- concatMap S.toList (H.elems pos)
  , H.null vars
  , H.null args = t
-- Canonical scope form: a NESTED external reference not bound by the ambient env denotes ⊤ —
-- its scope has exited ('dropOutOfScopeTVExt'). Normalizing here, in the shared canonicalizer,
-- makes every op's result carry the SAME spelling for the same set: an expansion-materialized
-- @[/T=⊤]@ and a difference-side symbolic @[/T=$_/T]@ otherwise denote equal sets that
-- 'isEqType' cannot join, breaking the intersect/difference laws. Guarded by a cheap scan;
-- the rewrite's output has no such references, so this clause re-enters at most once.
compactTypeRaw typeEnv vaenv t@UnionType{}
  | hasOutOfScopeTVExt vaenv t = compactType typeEnv vaenv (dropOutOfScopeTVExt typeEnv vaenv t)
-- Contradictory top: the universe filtered by an unsatisfiable predicate is empty, so
-- (∅ - neg) ∪ pos = pos. Two detectors: the syntactic 'isPredsContradictory' (e.g.
-- PredsAnd[p, ¬p], which arises during difference/subtype - @t ∩ ¬t@ on a predicated top yields
-- @p ∧ ¬p@; leaving it un-reduced falsely reports monotonicity growth in
-- 'verifySchemeActLowers'), and class-graph disjointness for a conjunction of >=2 PredClass
-- atoms (e.g. two distinct unsealed empty classes expand to distinct placeholders) which the
-- syntactic check cannot see - consulting 'predsEmpty', the same oracle the
-- placeholder/expansion paths use, so the symbolic conjunction agrees with its expansion
-- (without it, 'isSubtypeOf X BottomType' is false for an empty class conjunction, breaking
-- distributivity). Must precede the empty-neg/empty-pos clauses below, whose catch-alls would
-- otherwise return such a top unchanged. 'predsEmpty' itself gates to >=2-atom conjunctions and
-- decides symbolically first, so this stays cheap and non-recursive for any atom mix (e.g. two
-- distinct PredRel atoms - a value has ONE name, so the conjunction is empty).
compactTypeRaw typeEnv vaenv (UnionType (Just (preds, _, _)) pos cs)
  | isPredsContradictory preds || predsEmpty typeEnv vaenv preds = compactType typeEnv vaenv (UnionType Nothing pos cs)
-- Class-graph unsatisfiable top: a negated conjunct implied by the rest (@p ∧ ¬q = ∅@ when @p ⟹ q@),
-- e.g. @PredClass C[/T=a] ∧ ¬PredClass C[/T=b]@ with @a ⊆ b@. The syntactic 'isPredsContradictory'
-- above only sees identical-atom @X ∧ ¬X@; this uses 'predImplies' to also reduce a covariant
-- parameterized-class pair — the difference @C[a] − C[b]@ when @a ⊆ b@ (which otherwise leaves an
-- unreduced @C[a] ∧ ¬C[b]@ residue). Gated on a present negation so the common all-positive
-- conjunctions never pay the 'predImplies' pass.
compactTypeRaw typeEnv vaenv (UnionType (Just (PredsAnd ps, _, _)) pos cs)
  | any isNeg ps
  , or [ predImplies typeEnv vaenv (mkAnd rest) q | (PredsNot q, rest) <- picks ps ] =
      compactType typeEnv vaenv (UnionType Nothing pos cs)
  where
    isNeg (PredsNot _) = True
    isNeg _            = False
    -- each element paired with the list of all the others
    picks = go []
      where go _   []       = []
            go pre (x:post) = (x, pre ++ post) : go (pre ++ [x]) post
-- PredsAnd: unit propagation into a negated conjunct. Within @… ∧ r ∧ ¬(q₁ ∧ … ∧ qₙ)@ a
-- sub-conjunct @qᵢ@ that itself appears as an ambient conjunct is 'True' throughout the
-- region, so @¬(q₁ ∧ …)@ ≡ @¬(q₁ ∧ … without qᵢ)@ — exact pointwise. The @a−(a−b)@ chain
-- builds exactly this shape: @¬E ∧ ¬(N ∧ ¬E)@ (complementing a difference re-imports the
-- ambient refinement), which must reach the @¬E ∧ ¬N@ spelling the direct meet produces.
-- Dropping ALL of a negation's sub-conjuncts cannot happen here: that requires the ambient
-- to imply the whole negated formula, which the class-graph-unsatisfiable clause above
-- already reduced to ∅. Placed BEFORE the exclusion-canonicalization clause below (which
-- returns terminally for a no-change exclusion set and would strand tops that carry
-- exclusions with an unpropagated formula).
compactTypeRaw typeEnv vaenv (UnionType (Just (PredsAnd ps, negPartials, ncs)) posLeafs cs)
  | any isNegConjunct ps
  , let ps' = [unitProp x rest | (x, rest) <- conjPicks ps]
  , ps' /= ps =
      compactType typeEnv vaenv $ UnionType (Just (mkAnd ps', negPartials, ncs)) posLeafs cs
  where
    isNegConjunct (PredsNot _) = True
    isNegConjunct _            = False
    conjPicks = go []
      where go _   []       = []
            go pre (x:post) = (x, pre ++ post) : go (pre ++ [x]) post
    unitProp (PredsNot q) ambient
      | qs <- conjsOf q
      , kept <- filter (`notElem` ambient) qs
      , not (null kept)
      , length kept < length qs = predsNot (mkAnd kept)
    unitProp x _ = x
    conjsOf (PredsAnd xs) = concatMap conjsOf xs
    conjsOf x             = [x]
-- Two spellings of one value-predicate atom in a top's conjunction are the one atom
-- ('mergeSameFormPredExprs'): a declared refinement met with the same condition written on the body.
compactTypeRaw typeEnv vaenv (UnionType (Just (preds@PredsAnd{}, negPartials, ncs)) posLeafs cs)
  | preds' <- mergeSameFormPredExprs preds
  , preds' /= preds =
      compactType typeEnv vaenv $ UnionType (Just (preds', negPartials, ncs)) posLeafs cs
-- Pure ¬(single relation) top: re-express as the universe minus the relation's expansion.
-- Matched on exactly this shape (no catch-all) so other pure-top predicates flow on to the
-- ¬(unsatisfiable) and PredsAnd-simplification clauses below.
compactTypeRaw typeEnv vaenv t@(UnionType (Just (PredsNot (PredsOne (PredRel rp)), negPartials, [])) posLeafs [])
  | H.null negPartials && H.null posLeafs = case expandRelPartial typeEnv vaenv rp of
      (UnionType Nothing rp' []) -> UnionType (Just (PredsNone, rp', [])) H.empty []
      _               -> t
-- An excluded leaf that is ONE CONSTANT within value predicates the top itself states (how a meet
-- spells a constant met with the top, @/Integer | PredConstEq c ∧ P@ under @⊤ | P@ —
-- 'intersectTypesWithVarEnv') excludes exactly the point @c@: it is spelled as the excluded
-- constant, so @⊤|P − {c}@ and @⊤|P − (/Integer | PredConstEq c ∧ P)@ are one type. A residual
-- the top does not state (a predicate of its own) keeps the leaf: it excludes @c@ only where that
-- holds.
compactTypeRaw typeEnv vaenv (UnionType (Just (preds, negLeafs, negConsts)) posLeafs cs)
  | not (H.null negLeafs)
  , (asConsts@(_:_), keptLeafs) <- partitionEithers (map constOrLeaf (splitUnionType negLeafs)) =
      compactType typeEnv vaenv $ UnionType (Just (preds, joinUnionType keptLeafs, L.nub (negConsts ++ asConsts))) posLeafs cs
  where
    topConjs = predsConjuncts preds
    constOrLeaf l = case [c | PredsOne (PredConstEq c) <- predsConjuncts (ptPreds l)] of
      [c] | Just ps <- predsAtConst c (ptPreds l)
          , all (`elem` topConjs) (predsConjuncts ps)
          , constantCoveredByLeafs c (joinUnionType [l{ptPreds = PredsNone}]) -> Left c
      _ -> Right l
-- Top component with non-trivial preds and some exclusion (negated partials and/or excluded
-- constants), no positive leafs. This is the residual @U_preds − negPartials − negConsts@ that a
-- relation/class-top difference (@t1 ∩ ¬t2@) produces. Canonicalise it:
--  * Drop any exclusion disjoint from the predicated domain — excluding something not in the set is a
--    no-op (a negLeaf whose intersection with U_preds is ∅, or a negConst not a member of U_preds).
--    This is what lets @~/Cons − /Nil@ collapse back to @~/Cons@. Use the intersection check
--    (A ∩ B = ∅) rather than a subtype check for the leafs since the latter can fail to prove
--    disjointness when the ¬B expansion is approximate (hasOutside=True).
--  * If the SURVIVING negated partials cover the ENTIRE predicated domain (@U_preds ⊆ negPartials@)
--    the whole thing is empty. This is what makes @~/Cons − /Cons..@ reduce: it is
--    @Top(PredRel /Cons, {/Cons..})@ and @~/Cons ⊆ /Cons..@, so ∅ (without this the difference stays
--    an unreduced symbolic subtraction). 'isSubtypeOfWithEnv' is sound (no false positives), so this
--    never empties a genuinely inhabited residual.
compactTypeRaw typeEnv vaenv t@(UnionType (Just (preds, negPartials, negConsts)) posLeafs [])
  | H.null posLeafs && preds /= PredsNone
  , not (H.null negPartials) || not (null negConsts) =
      if not (H.null keptNegLeafs) && isSubtypeOfWithEnv typeEnv vaenv predUniverse (UnionType Nothing keptNegLeafs [])
        then BottomType
      else if keptNegLeafs == negPartials && keptNegConsts == negConsts
        then t
      else compactType typeEnv vaenv $ UnionType (Just (preds, keptNegLeafs, keptNegConsts)) H.empty []
  where
    predUniverse  = TopType H.empty preds
    keptNegLeafs  = joinUnionType $ filter negLeafIntersects $ splitUnionType negPartials
    -- An excluded constant drops only when it is PROVEN outside the domain ('constantWithinTop'
    -- answers 'Nothing'): one whose membership turns on a value predicate may be in it, and its
    -- exclusion then removes a point (@⊤|P − {c}@ is not @⊤|P@).
    keptNegConsts = filter (\c -> isJust (constantWithinTop typeEnv vaenv c (preds, H.empty, []))) negConsts
    negLeafIntersects p = not $ isBottomType $ intersectTypesEnv typeEnv vaenv (singletonType p) predUniverse
-- ¬(unsatisfiable) = everything: top component is U, simplify to PredsNone (same oracle pair
-- as the pure-top clause above).
compactTypeRaw typeEnv vaenv (UnionType (Just (PredsNot p, negLeafs, ncs)) posLeafs cs)
  | isPredsContradictory p || predsEmpty typeEnv vaenv p = compactType typeEnv vaenv (UnionType (Just (PredsNone, negLeafs, ncs)) posLeafs cs)
-- Cancel pos/neg overlap: an excluded region the positive side re-adds is a vacuous exclusion
-- ((T − neg' − C) ∪ pos' ∪ C = (T − neg') ∪ pos' ∪ C), so the common part drops from the NEG
-- side unconditionally. Its positive listing drops only when the reduced top already contains
-- it (e.g. (U − {X}) ∪ {X} = U for X inside U); a piece outside the predicate universe must
-- survive positively (dropping both sides would LOSE it: (U¬A − A) ∪ A = U¬A ∪ A, not U¬A).
compactTypeRaw typeEnv vaenv (UnionType (Just (preds, negLeafs, ncs)) posLeafs cs)
  | not (H.null posLeafs), not (H.null negLeafs)
  , let common = H.filter (not . S.null) $ H.intersectionWith S.intersection posLeafs negLeafs
  , not (H.null common) =
      let subtractLeafs leafs sub = H.filter (not . S.null) $ H.differenceWith (\a b -> Just (S.difference a b)) leafs sub
          negLeafs' = subtractLeafs negLeafs common
          topOnly = UnionType (Just (preds, negLeafs', ncs)) H.empty []
          redundantPos = joinUnionType $ filter (\p -> isSubtypeOfWithEnv typeEnv vaenv (singletonType p) topOnly) $ splitUnionType common
      in compactType typeEnv vaenv $ UnionType (Just (preds, negLeafs', ncs)) (subtractLeafs posLeafs redundantPos) cs
-- A positive leaf that is ONE CONSTANT within predicates (@/Integer | PredConstEq c ∧ L@, how a
-- meet or a union spells a constant held within a predicate) beside a top, when @L@ is exactly
-- what the top's own formula requires at that constant ('predsAtConstant'): the leaf re-adds the
-- point the top holds there, so it is redundant — and if the top excludes @c@, the two cancel
-- (@(⊤|¬P − {c}) ∪ (/Integer | PredConstEq c ∧ ¬P)@ is @⊤|¬P@). The dual of the excluded-leaf
-- rule above, so the two spellings a union's operand order produces are one.
compactTypeRaw typeEnv vaenv (UnionType (Just (preds, negLeafs, negConsts)) posLeafs cs)
  | not (H.null posLeafs)
  , (reAdded@(_:_), keptLeafs) <- partitionEithers (map constOrLeaf (splitUnionType posLeafs)) =
      compactType typeEnv vaenv $ UnionType (Just (preds, negLeafs, filter (`notElem` reAdded) negConsts)) (joinUnionType keptLeafs) cs
  where
    constOrLeaf l = case [c | PredsOne (PredConstEq c) <- predsConjuncts (ptPreds l)] of
      [c] | constantCoveredByLeafs c (joinUnionType [l{ptPreds = PredsNone}])
          , Just leafAtC <- predsAtConst c (ptPreds l)
          , Just topAtC <- predsAtConstant typeEnv vaenv c preds
          , sameConjuncts leafAtC topAtC
          , not (constantCoveredByLeafs c negLeafs) -> Left c
      _ -> Right l
    sameConjuncts x y = L.sort (predsConjuncts x) == L.sort (predsConjuncts y)
-- Cancel constants across the positive and subtractive components:
--  * a positive constant also excluded by the top (c ∈ negConsts) whose point lies in the
--    predicated region (U − negLeafs) cancels out of BOTH lists ((U − … − {c}) ∪ {c} restores the
--    point); if the point lies outside that region the exclusion is vacuous and drops while the
--    positive listing survives;
--  * a positive constant the top or the positive leafs already contain is redundant and drops.
compactTypeRaw typeEnv vaenv (UnionType (Just (preds, negLeafs, negConsts)) posLeafs posConsts)
  | not (null posConsts)
  , let (negConsts', posConsts') = foldr cancelConst (negConsts, []) posConsts
  , (negConsts', posConsts') /= (negConsts, posConsts) =
      compactType typeEnv vaenv $ UnionType (Just (preds, negLeafs, negConsts')) posLeafs posConsts'
  where
    predRegion = UnionType (Just (preds, negLeafs, [])) H.empty []
    cancelConst c (ncs, pcs)
      | c `elem` ncs = if constantInType typeEnv vaenv c predRegion
                         then (L.delete c ncs, pcs)
                         else (L.delete c ncs, c : pcs)
      | constantInType typeEnv vaenv c (UnionType (Just (preds, negLeafs, ncs)) H.empty [])
        || constantCoveredByLeafs c posLeafs = (ncs, pcs)
      | otherwise = (ncs, c : pcs)
-- PredsAnd: remove PredRel atoms dominated by a PredClass atom in the same conjunction.
-- e.g. PredsAnd[PredClass /Boolean, PredRel Boolean] → PredsOne (PredClass /Boolean)
-- Soundness: expandRelPartial r includes classPartial c in its expansion, so U_PredClass c ⊆ U_PredRel r
-- when r's name path is a suffix of c's name path.
compactTypeRaw typeEnv vaenv (UnionType (Just (PredsAnd ps, negPartials, ncs)) posLeafs cs)
  | not (null predClassAtoms)
  , let simplified = filter (not . isDominatedByPredClass) ps
  , length simplified < length ps =
      -- Recurse: might simplify further (e.g. single-element PredsAnd → PredsOne)
      compactType typeEnv vaenv $ UnionType (Just (mkAnd simplified, negPartials, ncs)) posLeafs cs
  where
    predClassAtoms = [c | PredsOne (PredClass c) <- ps]
    -- A PredRel r is dominated if some PredClass c in the conjunction has r's path as a suffix
    isDominatedByPredClass (PredsOne (PredRel r)) =
      any (\c -> relativeNameMatches (ptName r) (ptName c)) predClassAtoms
    isDominatedByPredClass _                      = False
-- PredsAnd: drop a bare PredClass conjunct that is a declared SUPERCLASS of a bare sibling
-- (@hasClass Sub ⟹ hasClass Super@, so @Sub ∧ Super = Sub@) - e.g. @PredClass Ord ∧ PredClass Eq →
-- PredClass Ord@ since @Ord isa Eq@. Uses the fast class-graph 'classDeclaredSubclassOf' (not
-- the sat engine). Restricted to arg/var-free (bare) class atoms so no element bound is lost (a
-- parameterized @Ord[X] ∧ Eq[Y]@ would need @X ⊆ Y@, which the name-only edge does not check).
-- Without this an element bound like @Cons[/T=(Ord∧Eq)]@ stays un-canonical and re-derives each
-- epoch, blocking convergence. Positive-only complement of the negation-gated clause below.
compactTypeRaw typeEnv vaenv (UnionType (Just (PredsAnd ps, negPartials, ncs)) posLeafs cs)
  | length bareClasses >= 2
  , let simplified = filter (not . isSuperclassOfSibling) ps
  , length simplified < length ps =
      compactType typeEnv vaenv $ UnionType (Just (mkAnd simplified, negPartials, ncs)) posLeafs cs
  where
    bareClasses = [c | PredsOne (PredClass c) <- ps, plainPartial c]
    isSuperclassOfSibling (PredsOne (PredClass q))
      | plainPartial q =
          any (\p -> ptName p /= ptName q && classDeclaredSubclassOf typeEnv (ptName p) (ptName q)) bareClasses
    isSuperclassOfSibling _ = False
-- Any formula: the parameterized counterpart of the clause above, at every depth - drop a
-- PredClass conjunct @q@ that a sibling PredClass @p@ implies through a declared @isa@ edge with
-- its element bounds checked ('classDeclaredSubclassOfBounded'), so @Maybe[/T=R] ∧ Functor[/T=R]@
-- is @Maybe[/T=R]@ under @every(Maybe[$T -> $_T], isa= [Functor])@. Such a pair is what a meet of a
-- value with a class-bounded variable builds (@m -> $F@ with @$F -> Functor[$T -> $_A]@, given a
-- Maybe), and what a declared result like @$F[$T -> $_B]@ instantiates; it then reaches a union's
-- disjunction uncompacted, which is why the rewrite looks inside negations too (an equivalent
-- subformula is equivalent in any context). Keeping the implied atom copied every element bound
-- into both atoms, which doubled the type at each level of a nested bind chain. Only a
-- VARIABLE-FREE @q@ is dropped: its set is fixed, so a later narrowing of @p@ keeps @p ⟹ q@, and
-- no variable link @q@ carries is lost. A mutually implying pair keeps one atom. A conjunction
-- with nothing to drop is left as it was, so no other formula is respelled.
compactTypeRaw typeEnv vaenv (UnionType (Just (preds, negPartials, ncs)) posLeafs cs)
  | Just preds' <- dropImpliedClassConjuncts typeEnv vaenv preds =
      compactType typeEnv vaenv $ UnionType (Just (preds', negPartials, ncs)) posLeafs cs
-- PredsAnd: drop a conjunct implied by a sibling (@p ∧ q = p@ when @p ⟹ q@), so a disjunction
-- conjunct absorbed by a stronger sibling collapses - e.g. with @Ord isa Eq@,
-- @PredClass Eq ∧ (PredRel Eq ∨ PredRel Ord) → PredClass Eq@ (the element bound a parameterized
-- type accumulates when its constructor is a member of two related classes). Gated to conjunctions
-- containing a 'PredsNot' (the disjunction/complement form) so the common atom-only conjunctions
-- never pay the pairwise 'predImplies'.
compactTypeRaw typeEnv vaenv (UnionType (Just (PredsAnd ps, negPartials, ncs)) posLeafs cs)
  | any isPredsNotConjunct ps
  , let simplified = dropImpliedConjuncts ps
  , length simplified < length ps =
      compactType typeEnv vaenv $ UnionType (Just (mkAnd simplified, negPartials, ncs)) posLeafs cs
  where
    isPredsNotConjunct (PredsNot _) = True
    isPredsNotConjunct _            = False
    -- Drop @q@ when a distinct sibling @p@ implies it; keep one of any mutually-implying pair. Uses
    -- only the cheap /syntactic/ implication (@p ∧ ¬q@ contradictory) - never the expansion-based
    -- 'predsEmpty' - so this hot-path clause stays cheap. The enhanced 'isPredsContradictory'
    -- (which knows @hasClass c ⟹ isa c@) is enough for the disjunction-absorption this targets.
    cheapImplies p q = isPredsContradictory (predsAnd p (predsNot q))
    dropImpliedConjuncts = go []
      where
        go kept [] = reverse kept
        go kept (q:rest)
          | any (\p -> p /= q && cheapImplies p q) (kept ++ rest) = go kept rest
          | otherwise = go (q:kept) rest
-- Mixed Just-top + pos: strip a positive leaf's ¬Q conjunct made vacuous by the
-- accompanying predicate top: @(U_P ∪ (leaf ∧ ¬Q ∧ rest)) = (U_P ∪ (leaf ∧ rest))@ when
-- @Q ⟹ P@ and the top carries no exclusions — the leaf values satisfying @Q@ lie inside
-- @U_P@ already, so refining them out of the leaf changes the union by nothing. The meet
-- route builds exactly this form (@(a∪b) ∩ a@ distributes @a@'s ¬-refinement onto @b@'s
-- leafs) while the union route keeps the bare leaf; stripping restores the union spelling.
compactTypeRaw typeEnv vaenv (UnionType (Just (preds, (H.null -> True), [])) pos cs)
  | preds /= PredsNone
  , not (H.null pos)
  , let pos' = joinUnionType (map stripLeaf (splitUnionType pos))
  , pos' /= pos =
      compactType typeEnv vaenv $ UnionType (Just (preds, H.empty, [])) pos' cs
  where
    stripLeaf p = case filter (not . vacuousNeg p) (conjsOf (ptPreds p)) of
      kept | length kept < length (conjsOf (ptPreds p)) -> p{ptPreds = rebuildConjs kept}
      _    -> p
    -- @¬Q@ is vacuous when @leaf ∧ Q ⊆ U_P@, i.e. @leaf ∧ Q ∧ ¬P = ∅@. The cheap
    -- formula-level implication decides the syntactic cases; the meet-based emptiness is
    -- the SAME nested (closed-world-over-current-members) decision the other association
    -- orders reach when they drop the refinement inside their own meets — using the gated
    -- 'predsEmpty' here instead would leave this route's spelling refined while the
    -- other's is bare, splitting distributivity on interlocked open/sealed class pairs.
    vacuousNeg p (PredsNot q) = q == preds || predImplies typeEnv vaenv q preds
      || isBottomType (intersectTypesEnv typeEnv vaenv (singletonType p{ptPreds=PredsNone})
                        (UnionType (Just (predsAnd q (predsNot preds), H.empty, [])) H.empty []))
    vacuousNeg _ _            = False
    conjsOf (PredsAnd xs) = concatMap conjsOf xs
    conjsOf x             = [x]
    rebuildConjs []  = PredsNone
    rebuildConjs [q] = q
    rebuildConjs qs  = mkAnd qs
-- Mixed Just-top + pos: absorb a positive leaf that the predicate region already covers
-- (@L ⊆ ∀preds − negs@ makes @L@ redundant in @(∀preds − negs) ∪ pos@). A recursive class
-- re-expanded during solving accretes exactly this form one level deeper each epoch
-- (@∀ConsList ∪ Nil ∪ Cons(tail=∀ConsList)@); absorbing restores the finite predicate form so
-- the solver converges — the mixed-form complement of 'foldRecursiveType', which folds pure
-- positive unions. Candidates are name-gated to the declared constituents of the predicate's
-- class atoms so unrelated leaves never pay the subtype check.
compactTypeRaw typeEnv vaenv (UnionType (Just topData@(preds, _, _)) pos cs)
  | not (H.null pos)
  , not (S.null constituentNames) || not (null topClassAtoms)
  , ((_:_), kept) <- L.partition absorbable (splitUnionType pos) =
      compactType typeEnv vaenv $ UnionType (Just topData) (joinUnionType kept) cs
  where
    constituentNames = case tryPredsToList preds of
      Just ps -> S.unions [classConstituentNames typeEnv (ptName c) | PredClass c <- ps]
      Nothing -> S.empty
    -- A leaf's RECURSIVE self-referential field can spuriously fail the strict subtype: the
    -- over-unrolled tail is @List@-topped while the class expansion of @Cons@ declares its tail
    -- @ConsList@ (and @List ⊋ ConsList@), so @Cons(tail=∀List) ⊄ Cons(tail=∀ConsList)@. Widen ONLY
    -- the recursive self-referential fields to ⊤ for the check ('relaxDeclaredRecursiveArgs', which
    -- keys off the DECLARATION so it applies even when the field's value is a complex union) — a
    -- sound over-approximation (@leaf ⊆ relaxed leaf@, so @relaxed ⊆ top ⟹ leaf ⊆ top@) that ignores
    -- the recursive field. NON-recursive fields are kept, so a concrete member (@IntOrdering(x=5)@)
    -- is absorbed into @∀Ord@ only when it genuinely is one — widening every field would fold
    -- concrete members into their class top, erasing structure eval dispatch keys on.
    -- Only fold a leaf whose type parameters are fully CONCRETE (e.g. @Cons[/T=Integer]@ from
    -- @rangeFrom@'s over-unrolling recursion): its structure is redundant once the class top holds
    -- the same element. A leaf with an ABSTRACT element (@Cons[/T=∀Ord]@, a test value or generic
    -- list) is left alone — folding it into its class top erases the constructor structure eval
    -- dispatch keys on. Widen the arg values to ⊤ for the membership check (soundness:
    -- @leaf ⊆ leaf{args=⊤}@) since membership is by class + element, not by recursive-field value.
    absorbable l
      | carriesTopAtom l = isSubtypeOfWithEnv typeEnv vaenv (singletonType l) (UnionType (Just topData) H.empty [])
      | otherwise = S.member (makeAbsoluteName (ptName l)) constituentNames
                   && all typeFullyConcrete (H.elems (ptVars l))
                   && isSubtypeOfWithEnv typeEnv vaenv (singletonType l{ptArgs = fmap (const PTopType) (ptArgs l)}) (UnionType (Just topData) H.empty [])
    topClassAtoms = case tryPredsToList preds of
      Just ps -> [a | a@PredClass{} <- ps]
      Nothing -> []
    carriesTopAtom l = case tryPredsToList (ptPreds l) of
      Just lps -> any (`elem` lps) topClassAtoms
      Nothing  -> False
-- Top component catch-all (includes PTopType and TopType np PredsNone); preserve constants.
-- Also compact the pos component for structural canonicalization (ensures union commutativity
-- holds structurally: different union orderings that mix Just and Nothing forms produce
-- equivalent pos components after compaction).
compactTypeRaw typeEnv vaenv (UnionType (Just topData) pos cs)
  | (pos', eqCs@(_:_)) <- compactConstEqLeafs typeEnv vaenv cs pos = compactType typeEnv vaenv (UnionType (Just topData) pos' (L.nub (cs ++ eqCs)))
  | (pos', []) <- compactConstEqLeafs typeEnv vaenv cs pos, pos' /= pos = compactType typeEnv vaenv (UnionType (Just topData) pos' cs)
compactTypeRaw typeEnv vaenv (UnionType (Just topData) pos cs)
  | not (H.null pos) =
      -- Recurse into each leaf's nested arg/var types (as the Nothing-pos branch does), so a nested
      -- bound like @Cons[/T=(Ord∧Eq)]@ canonicalises (→ @Cons[/T=∀Ord]@) instead of re-deriving an
      -- un-reduced form each epoch and blocking convergence.
      -- A changed pos re-enters: the clauses above read the pos (the pure-top respellings need
      -- it empty, absorption and overlap cancellation read its leafs), so a leaf that compacts
      -- to ∅ or to another spelling must be seen by them, or a second compaction would.
      let argCompacted = joinUnionType $ map (compactPartialArgs typeEnv vaenv) $ splitUnionType pos
          pos' = compactSettleMerged typeEnv vaenv argCompacted
      in if pos' /= pos
           then compactType typeEnv vaenv (UnionType (Just topData) pos' cs)
           else UnionType (Just topData) pos' cs
compactTypeRaw _ _ t@(UnionType (Just _) _ _) = t
-- TypeVar: an occurrence denotes @var ∩ inner@, so compact the inner and reduce the whole
-- occurrence to ⊥ whenever that meet is empty — either because the inner alone is, or because
-- the inner and the variable's value in this environment are disjoint. The second case is the
-- one a plain structural walk misses: the occurrence still LOOKS inhabited, so it survives
-- operations (a meet against ⊤, a union) that never consult the environment, and an
-- uninhabited slot then keeps its enclosing partial alive.
-- The ⊤-inner fast path is exact (compact ⊤ = ⊤) and keeps the hot knowledge-free case free.
-- The environment test is skipped when either side still mentions type variables: the meet
-- would re-enter this clause on a var reachable from its own value, and a variable-valued
-- binding (including the self-referential entry an unconstrained var acquires) carries no
-- concrete value to contradict.
compactTypeRaw _ _ t@(TypeVar _ _ PTopType) = t
compactTypeRaw typeEnv vaenv (TypeVar v loc i) = case compactType typeEnv vaenv i of
  i'@BottomType -> i'
  i' | envRejects i' -> BottomType
     | otherwise     -> TypeVar v loc i'
  where
    envRejects i'
      | typeHasTypeVars i' = False
      | l <- vaenvLookup typeEnv vaenv v, not (typeHasTypeVars l) =
          isBottomType $ intersectTypesEnv typeEnv vaenv l i'
      | otherwise = False
-- PosPartials with constants: compact leafs, remove constants covered by leafs
compactTypeRaw typeEnv vaenv (UnionType Nothing partials0 consts0) =
  let (partials, eqConsts) = compactConstEqLeafs typeEnv vaenv consts0 partials0
      consts = L.nub (consts0 ++ eqConsts)
      -- Compact the nested argument/variable types (bottom-up) so a recursive arg already
      -- in canonical predicate form is recognised one level up.
      argCompacted = joinUnionType $ map (compactPartialArgs typeEnv vaenv) $ splitUnionType partials
      compactedLeafs = compactSettleMerged typeEnv vaenv argCompacted
      -- Remove constants whose parent partial type is already in the compacted leafs
      filteredConsts = filter (\c -> not $ constantCoveredByLeafs c compactedLeafs) consts
  in foldRecursiveType typeEnv vaenv $ UnionType Nothing compactedLeafs filteredConsts

-- | The leaf stage of compaction, run to a fixpoint on the leafs it CREATES.
--
-- The stage merges same-named leafs ('compactJoinPartials'), and a merged leaf's slot is a NEW
-- value — the union of the merged leafs' values — which the per-field canonicalisation
-- ('compactPartialArgs') never saw. @Cons(tail=∀ConsList) + Cons(tail=Cons(tail=∀ConsList) + Nil)@
-- merges to @Cons(tail = ∀ConsList ∪ Cons(tail=∀ConsList))@, whose tail still spells a member
-- beside the class top that covers it, and only a SECOND 'compactType' would fold it
-- ('foldNestedMembers'). Compaction must be idempotent, so a leaf the stage created gets its
-- fields canonicalised too, and the stage runs again while that changes anything — a folded
-- field can make two leafs equal outside one slot and so mergeable. Leafs the stage passed
-- through are already canonical and are not redone.
--
-- EVERY caller of the leaf stage settles: the merge is the same merge whether the leafs sit in a
-- bare positive union or beside a predicate top.
compactSettleMerged :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> PartialLeafs -> PartialLeafs
compactSettleMerged typeEnv vaenv leafs
  -- The stage handed its input back (the common case): no leaf is new, so there is nothing to
  -- settle, which is what the membership test below would find leaf by leaf.
  | merged == leafs = merged
  | fresh' == fresh = merged
  | otherwise = compactSettleMerged typeEnv vaenv (joinUnionType (kept ++ fresh'))
  where
    merged = compactPartialLeafs typeEnv vaenv leafs
    before = S.fromList (splitUnionType leafs)
    (fresh, kept) = L.partition (not . (`S.member` before)) (splitUnionType merged)
    fresh' = map (compactPartialArgs typeEnv vaenv) fresh

-- | Recurse 'compactType' into a partial's argument and variable types. Compaction
-- otherwise only operates on the top level of a union, so without this a recursive type
-- nested inside an argument (e.g. the @tail@ of a @Cons@) is never canonicalised.
compactPartialArgs :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType
compactPartialArgs typeEnv vaenv p = tightenDeclBoundedDims typeEnv p
  -- Self-ref capping is applied ONLY to type-VARIABLE slots ('ptVars'), never argument slots
  -- ('ptArgs'). A recursive value's structure lives in its ARGUMENTS — a @ConsList@'s @tail=ConsList@,
  -- a @reverseOnto@'s accumulator @acc@ — and is load-bearing. The spurious over-unrolling to cap is a
  -- non-class arrow's result re-nesting into its own type PARAMETER (@operator++@'s @/T@), which no
  -- value structure depends on.
  --
  -- An argument's DIRECT free reference ('freeTVIntToTop': bound neither by this partial nor the
  -- ambient env) denotes ⊤, so the canonical form spells it ⊤ — the TVInt analogue of the
  -- out-of-scope-TVExt normalization in 'compactType'. A met slot otherwise keeps one side's raw
  -- spelling (@value=Cons[/a=$_/a](head=$/T)@ with @$/T@ free in the Cons) while the hoisted
  -- binding stores the ⊤-read form, and the two spellings of the same set never compare equal
  -- (the nested free ref instead CAPTURES a like-named var of the comparison's enclosing scope).
  -- Bound references — a declaration's own field pointer — are untouched.
  --
  -- A DATA partial's field value additionally folds a nested member leaf into the class top that
  -- covers it ('foldNestedMembers'): a data field's declared type is the class, and no dispatch
  -- keys on a value's nested structure, so the depth-1 predicate form is its canonical spelling.
  { ptArgs = fmap (foldField . compactType typeEnv vaenv . freeTVIntToTop p) (ptArgs p)
  , ptVars = fmap (compactType typeEnv vaenv . settleSelfRefRedundant typeEnv vaenv pName) (ptVars p)
  }
  where
    pName = makeAbsoluteName (ptName p)
    foldField | null (typeGraphData typeEnv pName) = id
              | otherwise                          = foldNestedMembers typeEnv vaenv

-- | Canonical form of a DATA partial's FIELD value: a member leaf the field's class top already
-- covers folds into that top. A recursive result re-derived during solving arrives at a
-- consumption point one level deeper each epoch — @Node[K=Ord](left = ∀Tree[K=Ord] ∪
-- Node[K=Ord](left = ∀Tree[K=Ord] ∪ Node[K=Ord](…)))@ — and every nested level is
-- set-theoretically redundant: a full member with a covered element lies inside the class top
-- (@L ⊆ ∀C ⟹ ∀C ∪ L = ∀C@), so the finite form is the depth-1 expansion with each recursive
-- field at its predicate ('hasExplicitRecursion' names the predicate form the canonical
-- recursive reference). The top-level mixed-form absorb in 'compactType' deliberately keeps a
-- leaf with an ABSTRACT element (eval's dispatch keys on a point's own constructor structure),
-- and a CALL partial's argument slot keeps its members too (a @put(m= ∀Tree ∪ Node(…))@ slot is
-- what selects the @m= Node@ arm); neither concern reaches a data value's own field, whose
-- declared type is the class and whose value no dispatch keys on. Unfolded, the nested levels
-- would carry the same ⊤-valued parameter under different spellings from different producers
-- (@$^/MV@, @$^/V@, ⊤ — the class expansion's template, a declaration bound, a meet), which
-- re-derivation rotates between epochs. Only a leaf named by the top's declared constituents and
-- provably a member (the same membership test as the absorb: class + element, the field values
-- widened to ⊤) folds; anything else is kept verbatim.
foldNestedMembers :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
foldNestedMembers typeEnv vaenv (UnionType (Just topData@(preds, _, _)) pos cs)
  | not (H.null pos)
  , not (S.null constituentNames)
  , ((_:_), kept) <- L.partition (leafCoveredByTop typeEnv vaenv topData constituentNames) (splitUnionType pos) =
      UnionType (Just topData) (joinUnionType kept) cs
  where constituentNames = predClassConstituentNames typeEnv preds
foldNestedMembers _ _ t = t

-- | The declared constituent names of a predicate's class atoms — the only names that can be
-- members of the predicate region by declaration.
predClassConstituentNames :: TypeGraph tg => TypeEnv tg -> TypePredicates -> S.HashSet TypeName
predClassConstituentNames typeEnv preds = case tryPredsToList preds of
  Just ps -> S.unions [classConstituentNames typeEnv (ptName c) | PredClass c <- ps]
  Nothing -> S.empty

-- | Whether a leaf is redundant beside a predicate top: named by the top's declared constituents
-- and a member of the region by class and element. The arg values are widened to ⊤ for the
-- check (sound: @leaf ⊆ leaf{args=⊤}@), since membership is by class + element, not by
-- recursive-field value — a leaf's recursive field can otherwise spuriously fail the strict
-- subtype (an over-unrolled tail @List@-topped where the member declares @ConsList@).
leafCoveredByTop :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> (TypePredicates, PartialLeafs, [Constant]) -> S.HashSet TypeName -> PartialType -> Bool
leafCoveredByTop typeEnv vaenv topData constituentNames l =
  S.member (makeAbsoluteName (ptName l)) constituentNames
    && isSubtypeOfWithEnv typeEnv vaenv (singletonType l{ptArgs = fmap (const PTopType) (ptArgs l)}) (UnionType (Just topData) H.empty [])

-- | Canonical form: DECLARATION-BOUNDED DIM TIGHTENING. A data partial's type-variable dimension
-- canonically absorbs what its declared occurrences already pin: a value spelled
-- @Cons[/T=∀Ord](head=Integer, tail=Cons[/T=Integer](…))@ tightens its element dimension to
-- @Integer@ — the stored bound met with the JOIN of the head content (a pointer occurrence
-- @head -> $T@ in the data declaration) and the tail's element dimension (a linked occurrence
-- @tail -> ConsList[$T -> $_T]@). The spellings denote the SAME set: a value matches through some
-- instantiation @τ ⊆ dim@ admitting every occupant, each occupant lies inside its occurrence's
-- contribution, so @τ@ met with the join still admits them all and no value is lost. The tightened
-- dim is what every consumer reads: a list whose content is all @Integer@ presents as
-- @List[Integer]@, not as the @List[∀Ord]@ of its declared bound.
--
-- Without this the dimension STARVES: the intersect's binding discovery needs the pointer spelling
-- (@head=$/T@) on one side of a meet, which is consumed the moment content lands there, so a dim
-- inherited from a declared bound (@∀Ord@, @⊤@) can never narrow again no matter how precise the
-- content becomes.
--
-- Strictly conservative — any gate failing keeps the dim unchanged:
--   * exactly one data definition for the name, 'PtArgExact' value, and the dim spelled ⊤ or as a
--     var-free union (a 'TypeVar' dim is a live link and is never touched);
--   * every template argument either provably cannot reference the variable, or is a supported
--     occurrence shape — the bare pointer @$T@ ('TVInt') or a pure class application linking
--     @$_T@ ('TVExt') — with the value's content present (a missed occurrence would tighten below
--     the true join, so anything else aborts);
--   * contributions must be var-free, so the meet cannot depend on the ambient env and the
--     canonical spelling is stable across op contexts; a pointer occurrence contributes its
--     content verbatim, a linked occurrence contributes the content's linked dimension (∅ for a
--     bare atom member — @Nil@ pins no elements);
--   * the meet applies only when it genuinely narrows (the subtype guard), which also makes the
--     rewrite idempotent: the tightened dim lies inside the join, so a second pass skips.
tightenDeclBoundedDims :: TypeGraph tg => TypeEnv tg -> PartialType -> PartialType
tightenDeclBoundedDims typeEnv p
  | H.null (ptVars p) || ptArgMode p /= PtArgExact = p
  | [tmpl] <- typeGraphData typeEnv (ptName p) = p{ptVars = H.mapWithKey (tightenDim tmpl) (ptVars p)}
  | otherwise = p
  where
    tightenDim tmpl k d
      | dimEligible d
      , k `H.member` ptVars tmpl
      , Just contribss <- mapM (occurrenceContrib k) (H.toList (ptArgs tmpl))
      , contribs@(_:_) <- concat contribss
      , let joined = unionAllTypes typeEnv contribs
      -- An EMPTY join means no occupant pins the dimension at all (every occurrence's
      -- content is a bare atom member — a wrapper over @Lf@\/@Nil@), which is an ABSENCE of
      -- information, not an empty dimension: the value exists with the variable free.
      -- Tightening to ∅ would manufacture an ∅-valued type parameter, which
      -- 'partialHasEmptySlot' (rightly, for an OCCUPIED parameter like @Cons[/T=∅]@) reads
      -- as the empty set — collapsing an inhabited value like the empty
      -- @MkSet(map=Leaf)@ to ⊥. A PARTIAL join (@Nil@ beside a content-bearing occupant)
      -- still contributes its ∅ harmlessly through the union.
      , not (isBottomType joined)
      , not (isSubtypeOfWithEnv typeEnv H.empty d joined)
      = compactType typeEnv H.empty $ intersectTypesEnv typeEnv H.empty d joined
      | otherwise = d

    dimEligible PTopType      = True
    dimEligible t@UnionType{} = not (typeHasVars t)
    dimEligible _             = False

    -- Contributions of one template argument: @Nothing@ aborts the dim's tightening entirely,
    -- @Just []@ is a non-occurrence, @Just cs@ are the occurrence's element contributions.
    occurrenceContrib k (argName, tmplVal) = case tmplVal of
      TypeVar (TVVar k') TVInt _ | k' == k -> do
        content <- H.lookup argName (ptArgs p)
        if typeHasVars content then Nothing else Just [content]
      _ | outerVarName tmplVal == Just k -> Nothing
      TypeVar{} -> Just []
      _ | Just links <- pureClassLinks k tmplVal ->
            if null links
              then Just []
              else do
                content <- H.lookup argName (ptArgs p)
                mapM (`linkedContrib` content) links
        | typeHasVars tmplVal -> Nothing -- may reference the variable in an unsupported shape
        | otherwise -> Just []

    -- @Just innerKeys@ when the template argument is a pure class application (@∀C[..]@ — a
    -- predicate-only top with no args) whose parameters are @$_k@ links or var-free; the inner
    -- keys name the class's own parameters where the content's dimension is read. @Nothing@ for
    -- any other shape (the caller then decides by 'typeHasVars').
    pureClassLinks k (UnionType (Just (PredsOne (PredClass q), negs, ncs)) pos consts)
      | H.null negs, null ncs, H.null pos, null consts, H.null (ptArgs q)
      , all linkOrClosed (H.elems (ptVars q))
      = Just [k' | (k', v) <- H.toList (ptVars q), outerVarName v == Just k]
      where linkOrClosed t = isJust (outerVarName t) || not (typeHasVars t)
    pureClassLinks _ _ = Nothing

    -- The element dimension a linked occurrence's content pins at the class parameter @innerK@:
    -- the dim of every content leaf, or ∅ for a bare atom member (no parameters, no fields —
    -- @Nil@ pins no elements). Any leaf whose dimension is absent-but-structured, var-carrying,
    -- or any non-union content shape aborts.
    linkedContrib innerK content = case content of
      UnionType Nothing leafs [] ->
        unionAllTypes typeEnv <$> mapM leafDim (splitUnionType leafs)
      UnionType (Just (PredsOne (PredClass q), negs, ncs)) pos consts
        | H.null negs, null ncs, H.null pos, null consts -> leafDim q
      _ -> Nothing
      where
        leafDim l = case H.lookup innerK (ptVars l) of
          Just e | not (typeHasVars e) -> Just e
                 | otherwise -> Nothing
          Nothing | H.null (ptVars l) && H.null (ptArgs l) -> Just BottomType
                  | otherwise -> Nothing

-- | Drop an over-unrolled DIRECT self-referential leaf from a partial's own nested argument\/variable
-- union when its settled form is already covered by a non-self sibling. An arrow's raw applied form is
-- kept in the reaches for eval dispatch, but when it is tied back into one of its OWN parameters — a
-- non-class arrow (@operator++ -> $T@) whose raw result feeds its own @/T@ — it re-nests one level per
-- constraint epoch and never reaches a fixpoint ("Reached runConstraints limit"). Two signals gate the
-- drop, and BOTH are required to avoid removing anything real:
--
--   * SELF-REFERENCE — the leaf's name equals @pName@, the enclosing partial's name (the immediate
--     cycle @operator++[/T=(operator++ + …)]@). A NON-self recursive tail (@Cons(tail=rangeFrom(..))@,
--     a different name) is the genuine structure of a recursive list and is left alone.
--   * COVERAGE — the leaf REDUCES (through its arrows) to a type already present among the non-self
--     sibling leaves, so removing it loses nothing. A self leaf that does not reduce, or whose reduced
--     form is not covered, is a real value and is kept.
--
-- Applied only at NESTED positions (via 'compactPartialArgs'), so the top-level raw forms eval's
-- dispatch keying still needs are untouched.
settleSelfRefRedundant :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> TypeName -> Type -> Type
settleSelfRefRedundant typeEnv vaenv pName (UnionType Nothing pos consts)
  | not (null selfRefs)
  = UnionType Nothing (joinUnionType (others ++ filter keep selfRefs)) consts
  where
    leaves = splitUnionType pos
    (selfRefs, others) = L.partition (\l -> makeAbsoluteName (ptName l) == pName) leaves
    coveredType = UnionType Nothing (joinUnionType others) consts
    keep l = let sl = singletonType l
                 reduced = case typeGraphQueryWithReason typeEnv vaenv l of
                             []    -> sl
                             dests -> unionAllTypesWithEnv typeEnv vaenv [d | (_, _, d) <- dests]
             in reduced == sl || not (isSubtypeOfWithEnv typeEnv vaenv reduced coveredType)
settleSelfRefRedundant _ _ _ t = t

-- | Fold an explicit union of partials back into a recursive class predicate when the
-- union is semantically equal to that class's expansion (the inverse of
-- 'expandRelPartial'). Recursive types are otherwise unrolled one level deeper each
-- constraint epoch (@ConsList ≡ Nil + Cons(tail=ConsList)@), producing syntactically
-- distinct but semantically equal representations that never reach a fixpoint. Folding
-- restores the finite @PredRel@ form so the solver converges.
--
-- With a NON-trivial joined element (@~ConsList[/T=⊤−A]@ from differencing) the finite form is
-- the class member EXPANSION (@Cons[/T=E](head=$T, tail=∀ConsList[/T=$T]) + Nil@) rather than the bare
-- rel predicate: the element then rides on the members' own fields, where the covariance-aware leaf
-- algebra reads it, instead of on a @PredRel@'s 'ptVars' — which 'expandRelPartial' and the
-- symbolic engine ('relMatchingClasses' notwithstanding) do not covariance-check, so a folded
-- @~ConsList[/T=E]@ subtrahend silently drops @E@ and empties a difference. The expansion is still
-- finite (its recursive @tail@ is the @∀ConsList@ predicate, not an explicit unrolling), so
-- convergence is preserved; the bare-element case keeps the minimal @~C@ predicate.
foldRecursiveType :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
foldRecursiveType typeEnv@TypeEnv{teClassGraph=ClassGraph cg} vaenv t@(UnionType Nothing leafs [])
  | length leafList > 1 -- only fold a genuine union; a single partial is already minimal
  , hasExplicitRecursion typeEnv leafList -- only fold an over-unrolled form, never the canonical depth-1 expansion
  , all leafExactlyMember leafList -- every leaf is exactly a member (see 'leafExactlyMember')
  , (relP:_) <- foldCandidates
  = if H.null joinedVars
      then TopType H.empty (PredsOne (PredRel (withElem relP)))
      else expandClassPartial typeEnv vaenv (withElem relP)
  where
    leafList = splitUnionType leafs
    -- Fold gate against NARROWING: the union folds to @~C@ only if it equals the class expansion
    -- @U_C@, which requires each leaf to be exactly its member — an exact-mode partial. A leaf with
    -- a non-vacuous @??@ mode ('ptArgLb'=False) also admits partial-application forms (a @Cons@
    -- missing @head@\/@tail@), so it is strictly WIDER than the bare member and the union overshoots
    -- @U_C@; folding to @~C@ would then drop those forms. 'normalizePtArgMode' first collapses a
    -- vacuous @..@ (extra args allowed, but the member declares no larger arg-set) to exact, so a
    -- real member-expansion leaf — always exact or vacuously widened — still folds.
    -- A leaf carrying its own predicate refinement (e.g. the symbolic @¬EqC@ a
    -- ¬-containing meet attaches) is strictly NARROWER than the bare member, so folding
    -- it to @~C@ would DROP the refinement and widen the union (@(⊤|!EqC) ∩ lists@ read
    -- as ALL lists). Only a predicate-free leaf is exactly its member.
    leafExactlyMember l = ptPreds l == PredsNone && ptArgMode (normalizePtArgMode typeEnv l) == PtArgExact
    tNames = leafNameSet t
    -- @t@ is an (over-)unrolling of a sealed class @C@ when its top-level constructor names
    -- are exactly @C@'s declared constituents (from @class(C,[..])@ in the class graph).
    -- Sealed ⇒ @U_C@ is exactly that union, so folding is sound and independent of the
    -- open/closed-world setting. A structural name match sidesteps a deep 'isEqType', which
    -- cannot prove @Nil+Cons(tail=Nil+Cons(..)) ≡ Nil+Cons(tail=~C)@. 'leafsWithinMembers'
    -- gives @t ⊆ U_C@ and 'leafsCoverMembers' the reverse @U_C ⊆ t@ — together @t ≡ U_C@.
    matchingClassNames = [ fromPartialName cKey
                         | (CGClass sealed _ classTypes _, cKey, _) <- graphToNodes cg
                         , sealed
                         , S.unions (map leafNameSet classTypes) == tNames
                         , leafsWithinMembers classTypes
                         , leafsCoverMembers (fromPartialName cKey) classTypes ]
    -- Fold gate: each leaf must be EXACTLY its class member, so the union equals @U_C@. Two ways a
    -- same-named leaf can differ from the member and make the union miss @U_C@:
    --   * WIDENING — a key NOT declared by the member, with a narrowing (non-⊤) value, makes the leaf
    --     strictly smaller than the bare member (@Integer(/r=Integer)@ from @5 - 2@, whose @Integer@
    --     member declares no fields), so the union under-covers and folding to @~C@ would widen it.
    --     ⊤-valued extra keys do not narrow and are permitted.
    --   * INCOMPLETENESS — a leaf MISSING a declared member field (a bare @/Cons@ with arg-set @{}@, or
    --     @Cons(head)@ without @tail@) is a partial-application form, DISJOINT from the field-complete
    --     member, so folding the union to @~C@ would drop it. A genuine over-unrolling carries every
    --     declared field (a @Cons@'s @head@ AND @tail@), so this never rejects a real recursive fold.
    leafsWithinMembers classTypes = all leafOk leafList
      where
        memberArgKeys = H.fromListWith S.union
          [ (ptName m, H.keysSet (ptArgs m)) | ct <- classTypes, m <- memberPartials ct ]
        memberPartials (UnionType _ pos _) = splitUnionType pos
        memberPartials _                   = []
        leafOk l =
          let declKeys = H.lookupDefault S.empty (ptName l) memberArgKeys
          in declKeys `S.isSubsetOf` H.keysSet (ptArgs l) -- field-complete (no missing declared field)
             && null [() | (k, v) <- H.toList (ptArgs l), not (S.member k declKeys), v /= PTopType]
    -- Coverage gate — the reverse of 'leafsWithinMembers': the union must EQUAL @U_C@, not merely
    -- sit within it. A RECURSIVE member field (declared with the class's own type, e.g. @Cons@'s
    -- @tail -> ∀ConsList@) must, in every union leaf, be a FULL reference to the class — the
    -- @∀C@/@~C@ predicate, ⊤ (declaration-bounded ≡ @∀C@), or a nested union that itself covers
    -- @U_C@. A leaf whose recursive field is a PROPER subset (e.g. @Cons(tail=Nil)@, produced when
    -- a difference narrows the @tail@) leaves @U_C@ uncovered, so folding to @~C@ would WIDEN it
    -- back to the whole class: @Cons(tail=Nil)+Nil@ (the lists of length at most one) must not
    -- fold to @~ConsList@ (ALL lists). A narrowed non-recursive field VALUE is checked
    -- the same way ('declFieldCovers'): a pointer-spelled element rides on 'joinedVars' into the
    -- folded @~C[/T=E]@, but a directly narrowed value (@head=⊤−X@ from a difference, var still
    -- ⊤) rides nothing and folding would widen it back. Purely structural (no
    -- 'isSubtypeOf'): terminates on the finite value structure and never depends on the engine
    -- proving a deep recursive equivalence, so it cannot silently disable a genuine fold.
    leafsCoverMembers cName classTypes = covers Nothing leafList
      where
        memberNameSet = S.unions (map leafNameSet classTypes)
        -- per member name, the arg keys whose DECLARED type references this class (the recursive fields)
        recKeysByMember = H.fromListWith S.union
          [ (ptName m, S.fromList [k | (k, fv) <- H.toList (ptArgs m), fieldRefsClass fv])
          | ct <- classTypes, m <- memberPartials ct ]
        memberArgKeys = H.fromListWith S.union
          [ (ptName m, H.keysSet (ptArgs m)) | ct <- classTypes, m <- memberPartials ct ]
        memberDeclArgs = H.fromListWith (++)
          [ ((ptName m, k), [dv]) | ct <- classTypes, m <- memberPartials ct, (k, dv) <- H.toList (ptArgs m) ]
        memberPartials (UnionType _ pos _) = splitUnionType pos
        memberPartials _                   = []
        -- @mExpect@ threads the ENCLOSING level's element bindings: the fold to @~C[E]@ claims
        -- every level's element is E, but a difference narrows only the TOP level
        -- (@Cons[/T=⊤−X](head=$/T, tail=⟨full ConsList⟩)@ — deep elements stay flexible), and
        -- folding that would narrow the deep levels too, a different set. Each nested level's
        -- element must therefore AGREE with its encloser's ('elemAgrees'); the top level has no
        -- encloser ('Nothing').
        covers mExpect leaves = S.fromList (map ptName leaves) == memberNameSet && all (leafCovers mExpect) leaves
        leafCovers mExpect l = maybe True (elemAgreesLeaf (ptVars l)) mExpect
                               && all (\k -> fieldCovers (ptVars l) (H.lookupDefault PTopType k (ptArgs l)))
                                      (S.toList (H.lookupDefault S.empty (ptName l) recKeysByMember))
                               && all (declFieldCovers l)
                                      (S.toList (H.lookupDefault S.empty (ptName l) memberArgKeys
                                                 `S.difference` H.lookupDefault S.empty (ptName l) recKeysByMember))
        -- A NON-recursive declared field's stored value must cover the member's: ⊤, or a value
        -- whose reading under the leaf's own scope equals the declared spelling's reading (the
        -- pointer @head=$/T@ and its bound element both qualify). A properly narrowed value
        -- (@head=⊤−X@) makes the leaf a proper subset of its member — reject the fold.
        declFieldCovers l k = case H.lookup k (ptArgs l) of
          Nothing       -> True
          Just PTopType -> True
          Just v -> any (\dv -> readSlot l v == readSlot l dv) (H.lookupDefault [] (ptName l, k) memberDeclArgs)
        readSlot l sv@(TypeVar (TVVar n) TVInt _) = maybe sv resolveFlex (H.lookup n (ptVars l))
        readSlot _ sv = resolveFlex sv
        fieldCovers _ PTopType                 = True -- ⊤ tail ≡ ∀C[E-linked] (declaration-bounded)
        fieldCovers ownVars (UnionType Nothing pos []) = covers (Just ownVars) (splitUnionType pos) -- nested explicit unrolling
        fieldCovers ownVars (UnionType (Just (preds, negLeafs, [])) p [])
          | H.null negLeafs && H.null p        = predsCover ownVars preds -- the ∀C / ~C predicate form
        fieldCovers _ _                        = False
        predsCover ownVars (PredsOne (PredClass c)) = relativeNameMatches (ptName c) cName && elemAgreesPred (ptVars c) ownVars
        predsCover ownVars (PredsOne (PredRel r))   = relativeNameMatches (ptName r) cName && elemAgreesPred (ptVars r) ownVars
        predsCover _ _                              = False
        -- A member LEAF without the class-param key has no element dimension (@Nil@) — vacuous;
        -- a key it does carry must resolve equal to the encloser's. A name that declares no such
        -- parameter has no dimension to read as ⊤ at all, which is why this differs from an
        -- unspelled parameter of a name that does declare it. A PRED's unspelled key IS ⊤ (an
        -- element-free @∀C@ admits every element), so it is compared over both key sets.
        elemAgreesLeaf inner outer = and [resolveFlex v == resolveFlexAt k outer | (k, v) <- H.toList inner]
        elemAgreesPred inner outer = and [resolveFlexAt k inner == resolveFlexAt k outer | k <- H.keys inner ++ H.keys outer]
        resolveFlexAt k m = maybe PTopType resolveFlex (H.lookup k m)
        -- A flexible spelling (a partial-local var, or an env-unbound external) denotes ⊤.
        resolveFlex (TypeVar _ TVInt _) = PTopType
        resolveFlex (TypeVar tv TVOuter _) = case outerBinding vaenv tv of
          Unbound    -> PTopType
          SelfLink _ -> PTopType
          BoundTop   -> PTopType
          BoundVar _ -> PTopType
          Bound b    -> b
        resolveFlex v = v
        fieldRefsClass (UnionType (Just (preds, _, _)) pos _) = predRefsName preds || anyMentions pos
        fieldRefsClass (UnionType Nothing pos _)              = anyMentions pos
        fieldRefsClass _                                      = False
        predRefsName (PredsOne (PredClass c)) = relativeNameMatches (ptName c) cName
        predRefsName (PredsOne (PredRel r))   = relativeNameMatches (ptName r) cName
        predRefsName _                        = False
        anyMentions pos = any (\p -> relativeNameMatches (ptName p) cName) (splitUnionType pos)
    -- Prefer a 'PredRel' partial nested within @t@ (it carries the right type-var bindings,
    -- e.g. @ConsList[T=$_T]@); otherwise fall back to the bare class partial. This also
    -- catches recursions that bottom out in 'TopType' rather than an explicit @~C@.
    nestedRels = L.nub $ concatMap relPartialsIn leafList
    foldCandidates = [ rp
                     | cName <- matchingClassNames
                     , rp <- case filter (\r -> relativeNameMatches (ptName r) cName) nestedRels of
                               (r:_) -> [r]
                               []    -> [partialVal cName] ]
    -- Keep the element on the folded relation: join each type parameter across the union's leaves
    -- (@Nil + Cons[/T=Integer] + Cons[/T=∀ToString]@ folds to @~ConsList[/T=∀ToString]@, not a bare
    -- @~ConsList@ whose Top element would spuriously escape a @DShow@ bound). The join across the
    -- constructors is exactly the class parameter's value; 'leafsWithinMembers' has already ruled
    -- out any leaf var not declared by a member, so this invents no foreign key. A FLEXIBLE
    -- spelling (a self-entry @/T=$/T@ or env-unbound external) denotes no element constraint —
    -- joining it as an element would make a pure-flexible unrolling miss the minimal bare-@~C@
    -- form below.
    joinedVars = H.fromListWith (unionTypesWithEnv typeEnv vaenv)
      [ (k, v) | l <- leafList, (k, v) <- H.toList (ptVars l), v /= PTopType, not (flexibleSpelling v) ]
    flexibleSpelling (TypeVar _ TVInt _) = True
    flexibleSpelling (TypeVar tv TVOuter _) = case outerBinding vaenv tv of
      Unbound    -> True
      SelfLink _ -> True
      BoundTop   -> False
      BoundVar _ -> True
      Bound _    -> False
    flexibleSpelling _ = False
    withElem rp = rp{ptVars = H.union joinedVars (ptVars rp)}
foldRecursiveType _ _ t = t

-- | True when the union is unrolled at least one extra level: some partial has, within its
-- arguments, an /explicit/ (non-predicate) occurrence of a partial whose name is one of the
-- union's own names. The canonical depth-1 expansion (e.g. @Nil + Cons(tail=~ConsList)@,
-- whose recursive tail is the @PredRel@ form) does NOT qualify, so it is preserved for
-- dispatch; only genuinely over-unrolled chains are folded back to the predicate.
hasExplicitRecursion :: (TypeGraph tg) => TypeEnv tg -> [PartialType] -> Bool
hasExplicitRecursion typeEnv leafList = any partialRecurses leafList
  where
    -- An unrolling nests a constructor inside its OWN recursive field — a @Cons@ within a
    -- @Cons@'s @tail@ (every over-unrolled level carries the recursive constructor at some
    -- depth). A same-CLASS partial that is merely an ELEMENT value — in a VAR slot
    -- (@ConsList[/T=Cons(..)]@) or a non-recursive arg (@head=Nil@), lists of lists — is not
    -- an unrolling: counting it would re-fold the canonical depth-1 expansion of such an element
    -- forever (fold → expand → 'compactOwnVarRefs' re-materializes the element into the
    -- pointer arg → fold …, a livelock).
    -- An ELEMENT field is one the declaration types by a bare parameter pointer (@head -> $T@):
    -- whatever it holds is a value of the element type, never a level of the recursion, so it
    -- is skipped by declaration rather than by the value's name. Judged by the value alone, a
    -- list of argument-less @Cons@ values would livelock the same way: 'tightenDeclBoundedDims'
    -- materialises the pointer @head=$/T@ as the element @Cons@ under @/T=Cons@, which would
    -- read as an unrolling, fold, and re-expand to @∀ConsList[/T=Cons]@.
    partialRecurses p = any (argHasName (ptName p)) [v | (k, v) <- H.toList (ptArgs p), not (S.member k (elemKeys p))]
    elemKeys p = S.fromList [k | dp <- typeGraphData typeEnv (ptName p), (k, TypeVar (TVVar _) TVInt _) <- H.toList (ptArgs dp)]
    argHasName encl (UnionType Nothing pos _) = any (\p' -> ptName p' == encl || partialRecurses p') (splitUnionType pos)
    argHasName _ _ = False -- predicate/top forms are the canonical recursive reference, not an unrolling


-- | The formula with every PredClass conjunct a sibling implies through a bounded declared edge
-- removed, at any depth of its conjunctions and negations, or 'Nothing' when none is (see the
-- 'compactTypeRaw' clause using it).
dropImpliedClassConjuncts :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicates -> Maybe TypePredicates
dropImpliedClassConjuncts typeEnv vaenv = go
  where
    go (PredsNot p) = PredsNot <$> go p
    go (PredsAnd ps) =
      let inner = map (\p -> fromMaybe p (go p)) ps
          kept = dropFrom [] inner
      in if length kept < length ps || inner /= ps then Just (mkAnd kept) else Nothing
    go _ = Nothing
    dropFrom kept [] = reverse kept
    dropFrom kept (q:rest)
      | impliedBySibling q (kept ++ rest) = dropFrom kept rest
      | otherwise = dropFrom (q:kept) rest
    impliedBySibling (PredsOne (PredClass q)) siblings
      | not (partialMentionsVars q) =
          any (\p -> ptName p /= ptName q && not (plainPartial p && plainPartial q) && classDeclaredSubclassOfBounded typeEnv vaenv p q)
              [p | PredsOne (PredClass p) <- siblings]
    impliedBySibling _ _ = False
