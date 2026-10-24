-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.Algebra
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- The type-set algebra: union, intersection, difference, subtyping, compaction, class expansion,
-- substitution and the satisfiability engine. These operations are one mutually recursive group.
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns    #-}
-- 'isSubtypeOfWithEnv' pairs many view-pattern clauses; the default coverage-checker budget
-- (30 models) is too small for it and degrades the check with a warning.
{-# OPTIONS_GHC -fmax-pmcheck-models=100 #-}

module Semantics.Types.Algebra where

import           Control.Exception         (evaluate)
import           Control.Monad             (foldM, when)
import           Data.Bifunctor            (Bifunctor (first))
import           Data.Either
import           Data.Hashable
import qualified Data.HashMap.Strict       as H
import qualified Data.HashSet              as S
import           Data.IORef                (IORef, atomicModifyIORef', newIORef,
                                            readIORef)
import           Data.List                 (partition)
import qualified Data.List                 as L
import           Data.Maybe
import           Data.Zip
import           Debug.Trace               (trace)
import           Prelude                   hiding (unzip)
import           Semantics.Types.ArgModes
import           Semantics.Types.Construct
import           Semantics.Types.Data
import           Semantics.Types.Memo
import           Semantics.Types.Name
import           Semantics.Types.Preds
import           Semantics.Types.SatGoal
import           Semantics.Types.Vars
import           System.IO.Unsafe          (unsafePerformIO)
import           Text.Printf
import           Utils


-- | Conjoin two top-component predicate formulae, UNIFYING same-class atoms instead of
-- accumulating them. When both sides assert @PredClass C[..]@ (or @PredRel C[..]@) for the same
-- class @C@, the two class 'PartialType's are intersected (threading the var env) into one atom:
-- e.g. @PredClass C[/T=∀Ord] ∧ PredClass C[/T=$_/T]@ becomes @PredClass C[/T=$_/T]@ with @$_/T ↦
-- ∀Ord@ recorded in the returned env. Unlike a post-hoc 'compactType' (which is 'Type -> Type' and
-- cannot record the binding, so it would have to DROP a bound), this keeps every bound: the tighter
-- one survives as a monotonic var binding rather than a lost conjunct. Without it a recursive
-- parameterized type grows a redundant same-class conjunct each epoch and never converges. A pair
-- that does not intersect to a single partial is left as separate conjuncts.
mergePredsAnd :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicates -> TypePredicates -> (TypeVarArgEnv, TypePredicates)
mergePredsAnd typeEnv@TypeEnv{teClassGraph=ClassGraph cg} vaenv0 ps1 ps2 = (vaenvF, rebuild keptF)
  where
    (vaenvS, keptS) = foldl step (vaenv0, []) (conjOf ps1 ++ conjOf ps2)
    -- After merging same-name atoms, propagate an element bound carried by a superCLASS atom
    -- into a subclass atom's free element via the declared (element-preserving) @isa@ edge:
    -- @ConsList[$_T] ∧ List[Integer]@ becomes @ConsList[Integer] ∧ List[Integer]@. The two
    -- conjuncts then agree, so a recursive element does not oscillate free/bound across epochs
    -- (a 'verifySchemeActLowers' monotonicity failure). Sound because @every(ConsList[$T] isa
    -- List[$T])@ makes ConsList's element the same as List's - the superclass's enumerated member
    -- of the subclass's name carries exactly that mapping, so meeting against it only refines.
    (vaenvF, keptF) = pinIsa vaenvS keptS
    pinIsa v atoms
      | length classPs < 2 = (v, atoms)
      | otherwise          = L.mapAccumL pinAtom v atoms
      where
        classPs = [c | PredsOne (PredClass c) <- atoms]
        pinAtom v' (PredsOne (PredClass ca)) =
          let (v'', ca') = foldl pinBy (v', ca) [cb | cb <- classPs, ptName cb /= ptName ca]
          in (v'', PredsOne (PredClass ca'))
        pinAtom v' other = (v', other)
        -- Pinning reads cb's member of ca's name as the ONLY way a ca is a cb. That is the
        -- declaration's own statement when the member merely FORWARDS cb's parameters
        -- (@every(ConsList[$T] isa List[$T])@), and a closed-world one when it RESTRICTS them
        -- (@every(ConsList[$T -> Eq] isa Eq)@: a list is an @Eq@ when its elements are). An
        -- unsealed cb can gain another member of that name, so outside 'tePrgmEnv' the meet
        -- @Eq ∧ ConsList@ is not @Eq ∧ ConsList[/T=∀Eq]@ — a later file may declare every
        -- @ConsList@ an @Eq@ — and pinning is sound only for a sealed cb or a forwarding member.
        pinBy (v', ca) cb
          | not (pinSound cb (ptName ca)) = (v', ca)
          | otherwise = case classSatCandidates typeEnv v' ProveWitness cb of
          Just cands -> case [m | Just m <- map candClassPartial cands, ptName m == ptName ca] of
            (m:_) -> case intersectPartials typeEnv v' ca m of
              (vd, [r]) -> (mergeAllVarEnvs typeEnv [v', vd], r)
              _         -> (v', ca)
            []    -> (v', ca)
          Nothing -> (v', ca)
        pinSound cb caName = tePrgmEnv typeEnv || predsSealed typeEnv (PredsOne (PredClass cb)) || forwardingMember cb caName
        forwardingMember cb caName = case graphLookup (PClassName (ptName cb)) cg of
          Just (CGClass _ _ members _) ->
            let named = [ m | t <- members, m <- membersOf t, relativeNameMatches caName (ptName m) || relativeNameMatches (ptName m) caName ]
            in not (null named) && all (all isTypeVar . H.elems . ptVars) named
          _ -> False
        membersOf t = case t of
          UnionType _ leafs _ -> splitUnionType leafs ++ maybe [] pure (maybeGetClassSingleton t)
          _                   -> []
        candClassPartial (CandGoal (SatGoal [SatClass m] [])) = Just m
        candClassPartial (CandLeaf m)                         = Just m
        candClassPartial _                                    = Nothing
    conjOf (PredsAnd xs) = concatMap conjOf xs
    conjOf PredsNone     = []
    conjOf p             = [p]
    atomKC (PredsOne (PredClass c)) = Just (0 :: Int, c)
    atomKC (PredsOne (PredRel   r)) = Just (1 :: Int, r)
    atomKC _                        = Nothing
    mkAtom 0 p = PredsOne (PredClass p)
    mkAtom _ p = PredsOne (PredRel   p)
    -- Each unification EXTENDS the accumulated env with the meet's discoveries
    -- ('intersectPartials' returns the discovered bindings only): a later step that
    -- discovers nothing must not wipe an earlier step's binding (@ConsList[$_T] ∧
    -- ConsList[Int]@ records @$_T ↦ Int@, which a following @List[Int] ∧ List[Int]@
    -- unification would otherwise erase, leaving the kept flexible form unresolvable).
    step (v, kept) atom = case atomKC atom of
      Nothing     -> (v, kept ++ [atom])
      Just (k, p) -> case break (sameKN k p) kept of
        (pre, ex:post) | Just (_, ep) <- atomKC ex ->
          case intersectPartials typeEnv v ep p of
            (v', [m]) -> (mergeAllVarEnvs typeEnv [v, v'], pre ++ mkAtom k m : post)
            _         -> (v, kept ++ [atom])
        _ -> (v, kept ++ [atom])
    sameKN k p a = case atomKC a of
      Just (k', p') -> k == k' && ptName p' == ptName p
      Nothing       -> False
    rebuild []  = PredsNone
    rebuild [p] = p
    rebuild xs  = mkAnd xs

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

-- | Class-graph-aware unsatisfiability: 'True' when no type satisfies @preds@, decided by expanding
-- against the class graph and checking for emptiness. Complements the purely syntactic
-- 'isPredsContradictory' (which only sees structural A∧¬A contradictions) with class-graph
-- disjointness - e.g. two distinct classes whose members\/placeholders do not overlap, so
-- @PredClass C ∧ PredClass D@ is empty. Sound by construction: it only reports 'True' when the
-- expansion is definitively bottom; predicates that do not fully expand (e.g. a 'PredRel' or
-- 'PredExpr') leave a symbolic, non-bottom expansion and so return 'False' (conservative).
predsEmpty :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicates -> Bool
-- Only conjunctions of >=2 atoms can be empty by class-graph disjointness (a single atom or a
-- negation is satisfiable on its own). Restricting the (relatively expensive) expansion to that
-- case keeps the common single-predicate subtype checks free.
predsEmpty typeEnv vaenv preds@(PredsAnd ps) | length ps >= 2 =
  -- Where the symbolic engine refuses (constants, an oversized formula, a goal resting on an
  -- opaque value predicate): decide by materialized expansion — only where the expansion is
  -- EXACT ('predsPositiveSealed', the gate 'isSubtypeOfWithEnv' already applies to its own
  -- expansion fallback). An unsealed class expands to its members so far, and a conjunction
  -- empty on those alone (@Bag ∧ ~length ∧ ¬P@, with the engine refusing over @¬P@) is not empty
  -- in the open world, where a later file may add the member that inhabits it.
  fromMaybe (expansionExact && isBottomType (expandPredicates typeEnv vaenv preds))
    (predsEmptySymbolic typeEnv vaenv preds)
  where
    expansionExact = tePrgmEnv typeEnv || predsPositiveSealed typeEnv preds
predsEmpty _       _     _                                    = False

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

-- | Expands a 'TypePredicates' value into a 'Type' by resolving class, relation, and expression predicates.
expandPredicates :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicates -> Type
expandPredicates _       _     PredsNone    = PTopType
expandPredicates typeEnv vaenv (PredsOne p) = expandPredicate typeEnv vaenv p
-- A value predicate's expansion is symbolic, a top carrying it ('expandPredicate'), and so is a
-- negated one's (the complement of that top). Meeting the conjuncts' expansions would meet two
-- such tops: 'intersectTypesWithVarEnv' merges them into one top and compacts it, and compaction
-- asks whether the conjunction is empty ('predsEmpty') by expanding it — this very expansion,
-- without end (a body's condition met with a declared refinement spelled the same way, @a |
-- a.isZ == True@ under @-> Nat_isZ@; the union of two refinements, which is the complement of
-- the meet of their negations). The value-predicate LITERALS, of either polarity, are attached
-- as they are to the expansion of the other conjuncts, which is what the meet of the tops is. A
-- literal beside its own negation constrains the one value both ways, so the conjunction is
-- empty whatever the predicate means.
expandPredicates typeEnv vaenv conj@(PredsAnd ps)
  | null literals = case map (expandPredicates typeEnv vaenv) ps of
      [] -> BottomType
      ts -> foldr1 (intersectTypesEnv typeEnv vaenv) ts
  | any (\e -> e `elem` negated) positive = BottomType
  | otherwise = attach othersExpanded (mkAnd literals)
  where
    (literals, others) = partition isValuePredLiteral (predsConjuncts conj)
    othersExpanded = case others of
      [] -> PTopType
      _  -> expandPredicates typeEnv vaenv (mkAnd others)
    positive = [e | PredsOne (PredExpr e) <- literals]
    negated  = [e | PredsNot (PredsOne (PredExpr e)) <- literals]
    attach BottomType _ = BottomType
    -- A class top whose members are enumerable in full ('predsPositiveSealed') is attached per
    -- member, so a literal decided on each member's shape decides the whole
    -- ('compactFalsePredExprLeafs'): every Mammal is fluffy, so @⊤ | Mammal ∧ ¬fluffy@ is
    -- empty, which the symbolic top cannot show. An open class, or one the expansion cannot
    -- exhaust, keeps the literals on the top.
    --
    -- A sealed class whose members already reach every value — one of them is its own unbounded
    -- parameter (@Option[$T]@'s @$_T@) — expands to every name of the program, each with any
    -- arguments: the universe, enumerated. Attached per member there, the literals are the
    -- symbolic top again spelled once per name, and every later meet, compaction and emptiness
    -- question about it is asked once per name too (a subtype question over such a top made
    -- ~500 compactions of a 49-name union). The universe a value predicate filters is kept
    -- symbolic, as 'expandPredicate' keeps it.
    attach t@(TopType _ tps) atoms
      | predsPositiveSealed typeEnv tps
      , (leafs, False) <- expandTypesWithNamesFull typeEnv vaenv t (teNames typeEnv)
      , not (enumeratesUniverse leafs) =
          compactType typeEnv vaenv (typeAddPreds (UnionType Nothing leafs []) atoms)
      | otherwise = typeAddPreds t atoms
    attach t@(UnionType Nothing _ []) atoms = typeAddPreds t atoms
    attach t atoms                        = intersectTypesEnv typeEnv vaenv t (TopType H.empty atoms)
    enumeratesUniverse leafs =
      let anyArgs = S.fromList [ ptName l | l <- splitUnionType leafs, l == (partialVal (ptName l)){ptArgMode=PtArgUnbounded} ]
      in teNames typeEnv `S.isSubsetOf` anyArgs
expandPredicates typeEnv vaenv (PredsNot p)  = complementTypeEnv typeEnv vaenv $ expandPredicates typeEnv vaenv p

expandPredicate :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicate -> Type
expandPredicate typeEnv vaenv (PredClass clss) = expandClassPartial typeEnv vaenv clss
expandPredicate typeEnv vaenv (PredRel rel)    = expandRelPartial typeEnv vaenv rel
-- A constant equality holds of exactly that constant.
expandPredicate _ _ (PredConstEq c) = constantType c
-- A value predicate has no enumeration of its own: it is decided per value shape, where a leaf
-- carries it ('compactFalsePredExprLeafs', 'isSubPartialOfWithEnv'), so its expansion is the
-- universe it filters, kept symbolic.
expandPredicate _ _ (PredExpr e)     = TopType H.empty (PredsOne $ PredExpr e)

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

-- | Returns 'True' if every type satisfying @p1@ also satisfies @p2@, i.e. U_p1 ⊆ U_p2.
-- Sound but potentially incomplete: 'False' does not guarantee the predicates are incomparable.
predImplies :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicates -> TypePredicates -> Bool
-- Structural base cases
predImplies _       _     _  PredsNone = True   -- anything ⊆ top
predImplies _       _     p1 p2 | p1 == p2 = True  -- reflexivity
-- @⊤ ⟹ p2@ iff p2 holds for every value, i.e. p2 is a TAUTOLOGY (its negation is unsatisfiable).
-- Usually False (a proper predicate excludes something), but a @¬(unsatisfiable)@ universe — e.g.
-- @¬(A ∧ B)@ for two disjoint classes — denotes ⊤, so @⊤ ⟹ it@. Deciding this through the
-- 'predsEmpty' oracle keeps 'predImplies' consistent with 'compactType', which reduces such a
-- universe to 'PredsNone', so a super carrying the un-reduced universe (@sub ⊆ (⊤|¬(A∧B)) − …@) is
-- decided the same as its reduced form. Sound — 'predsEmpty' is conservative-to-inhabited, so this
-- only answers True when p2 is PROVABLY tautological.
predImplies typeEnv vaenv PredsNone p2 = predsEmpty typeEnv vaenv (predsNot p2)
-- Logical connectives
predImplies typeEnv vaenv p1@(PredsAnd ps) p2      = any (\p -> predImplies typeEnv vaenv p p2) ps || allCheck || cancellationCheck || directContradictCheck || jointEmptyCheck
  -- if any conjunct alone implies p2, the intersection does too
  -- if p2 is a conjunction, p1 implies p2 iff p1 implies each conjunct of p2
  -- also: (X₁ ∨ X₂ ∨ ... ∨ q) ∧ ¬X₁ ∧ ¬X₂ ∧ ... ⊆ q  (disjunction cancellation)
  -- direct contradiction: p1 → ¬q iff p1 ∧ q = ∅
  where
    allCheck = case p2 of
      PredsAnd qs -> all (predImplies typeEnv vaenv p1) qs
      _           -> False
    -- Definitional fallback @p1 ⊆ p2 ⟺ p1 ∧ ¬p2 = ∅@, deciding the WHOLE conjunction jointly (via
    -- 'predsEmpty'\/the SAT engine) rather than conjunct-by-conjunct. A joint constraint the atoms do
    -- not carry alone is then seen: @Monad[Integer] ∧ ConsList ⊆ DShow@ holds because the two atoms
    -- together pin the list element to Integer (a @ToString@), which neither @Monad[Integer]@ nor a
    -- bare @ConsList@ implies by itself. Last in the chain, so the cheap syntactic routes win first.
    jointEmptyCheck = case p2 of
      PredsOne{} -> predsEmpty typeEnv vaenv (predsAnd p1 (predsNot p2))
      _          -> False
    -- A leftover disjunct @x@ is cancellable when @¬x@ is literally present, or when the
    -- conjunction is incompatible with @x@ by the class graph (@p1 ∧ x = ∅@, so @x@ can never
    -- co-occur) - e.g. @Boolean ∧ (Animal ∨ ~r)@ implies @~r@ because @Boolean ∧ Animal = ∅@.
    cancellationCheck = or
      [ all (\x -> PredsNot x `elem` ps || predsEmpty typeEnv vaenv (predsAnd p1 x)) xs
      | PredsNot (PredsAnd nots) <- ps
      , Just inners <- [mapM extractNot nots]
      , (q_inner, xs) <- choices inners
      , predImplies typeEnv vaenv q_inner p2
      ]
    directContradictCheck = case p2 of
      PredsNot inner -> isPredsContradictory (predsAnd p1 inner) || predsEmpty typeEnv vaenv (predsAnd p1 inner)
      _              -> False
    extractNot (PredsNot x) = Just x
    extractNot _            = Nothing
    choices []     = []
    choices (y:ys) = (y, ys) : map (\(z, zs) -> (z, y:zs)) (choices ys)
predImplies typeEnv vaenv p1 (PredsAnd ps)      = all (predImplies typeEnv vaenv p1) ps
  -- p1 must imply every conjunct
predImplies typeEnv vaenv (PredsNot inner) (PredsNot outer) = predImplies typeEnv vaenv outer inner
  -- contravariance: ¬A ⊆ ¬B ↔ B ⊆ A
predImplies typeEnv vaenv p1 (PredsNot p2)      = isPredsContradictory (predsAnd p1 p2) || predsEmpty typeEnv vaenv (predsAnd p1 p2)
  -- U_p1 ⊆ ¬U_p2 ↔ U_p1 ∩ U_p2 = ∅
-- !(¬a₁ ∧ ¬a₂ ∧ ...) = a₁ ∨ a₂ ∨ ...: a disjunction implies q iff every disjunct implies q
predImplies typeEnv vaenv (PredsNot (PredsAnd nots)) q2
  = case mapM extractNot nots of
      Just inners -> all (\inner -> predImplies typeEnv vaenv inner q2) inners
      Nothing     -> False
  where
    extractNot (PredsNot x) = Just x
    extractNot _            = Nothing
predImplies _       _     (PredsNot _) _        = False  -- conservative
-- Atom cases
predImplies typeEnv vaenv (PredsOne a1) (PredsOne a2) = predImpliesAtom typeEnv vaenv a1 a2

-- | 'classDeclaredSubclassOf' where BOTH sides may carry element bounds. A declared member
-- of the target's class node is a TEMPLATE spelled in the target class's own parameters: the
-- desugarer records @every(Tree[$K -> $_K], isa= [Map[$K -> $_K]])@ under @Map[$K]@ as the
-- member @∀Tree[/K=$_/K]@, whose @$_/K@ names Map's own @/K@ (the same convention the class
-- expansion reads — 'expandClassPartialWithNamesFullAux'). So the edge asserts
-- @Tree[K=k] ⊆ Map[K=k]@ for EVERY @k@, and whether it proves @c1 ⟹ c2@ depends on @c2@'s
-- instantiation: each member is first INSTANTIATED at @c2@'s bounds ('substitutePartialExit'
-- under @c2@'s vars, exactly as expansion instantiates a member before use), and only then
-- compared. A member spelled @Tree[/K=$_/K]@ under @Map[/K=∀Ord]@ becomes @Tree[/K=∀Ord]@, so
-- @Tree[/K=Integer] ⟹ Map[/K=∀Ord]@ holds while @Tree[/K=∀Show] ⟹ Map[/K=∀Ord]@ does not.
-- Compared against the raw template instead, every @$_/K@ would read as ⊤ and the edge would
-- prove @c1 ⟹ c2@ for any instantiation of @c2@.
--
-- The DIRECT step: @c1@'s name matches the instantiated member and each bound the member
-- declares covers @c1@'s corresponding bound (covariant, absent = ⊤ — the same rule as the
-- same-name class covariance in 'predImpliesAtom', under the same 'comparedPartialsEnv').
-- The TRANSITIVE step continues into the instantiated member as the next target, so bounds
-- compose through a chain of parameterized edges by the same substitution; a class re-entered
-- on its own walk stops (False — conservative). A plain @c2@ instantiates every member
-- parameter at ⊤. NOT used inside the SAT engine's declared-edge check — 'directMatch' runs
-- algebra code ('isSubtypeOfWithEnv'), and 'satAtomImpliesDeclared' must stay engine- and
-- algebra-free.
classDeclaredSubclassOfBounded :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> Bool
classDeclaredSubclassOfBounded typeEnv@TypeEnv{teClassGraph=ClassGraph cg} vaenv c1 c2 = go S.empty c2
  where
    go visited target@PartialType{ptName=cName}
      | S.member cName visited = False
      | otherwise = case graphLookup (PClassName cName) cg of
          Just (CGClass _ PartialType{ptVars=classVarsDecl} classTypes _) ->
            let classVars = H.unionWith (intersectTypesEnv typeEnv vaenv) (ptVars target) (cleanDeclSelfRefs classVarsDecl)
                subs = [ substitutePartialExit typeEnv ((ambientEnv $ joinVarArgEnv classVars H.empty)) sc
                       | t <- classTypes, Just sc <- [maybeGetClassSingleton t] ]
            in any (\sc -> directMatch sc || go (S.insert cName visited) sc) subs
          _ -> False
    directMatch sc = relativeNameMatches (ptName c1) (ptName sc)
      && all (\(k, v) -> isSubtypeOfWithEnv typeEnv vaenv' (H.lookupDefault PTopType k (ptVars c1)) v) (H.toList (ptVars sc))
      && all (\(k, v) -> isSubtypeOfWithEnv typeEnv vaenv' (H.lookupDefault PTopType k (ptArgs c1)) v) (H.toList (ptArgs sc))
      where vaenv' = comparedPartialsEnv typeEnv vaenv c1 sc

predImpliesAtom :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicate -> TypePredicate -> Bool
-- A declared subclass edge @every(c1 isa c2)@ proves @c1 ⊆ c2@ directly, without enumerating
-- @c1@'s members (which can recurse through a recursive parameterized member - see
-- 'classDeclaredSubclassOf').
predImpliesAtom typeEnv vaenvB (PredClass c1) (PredClass c2)
  | classDeclaredSubclassOf typeEnv (ptName c1) (ptName c2) = True
  -- The bounded direct edge: @Tree[K→Ord] ⟹ Eq@ by the var-carrying member the name-only
  -- walk skips, without the engine route below.
  | classDeclaredSubclassOfBounded typeEnv vaenvB c1 c2 = True
-- Same class, covariant element bound: @hasClass C[/T=a] ⟹ hasClass C[/T=b]@ when @a ⊆ b@. A value
-- whose class-C element is within @a@ is also within the wider @b@. This is the parameterized-class
-- subtype rule for a shared class name with a type-var vs. a wider element bound (e.g. @Eqv[$T] ⟹
-- Eqv[⊤]@, @Functor[Integer] ⟹ Functor[$A]@), which the name-only 'classDeclaredSubclassOf' above
-- and the member-enumeration below never compare. Checked covariantly over each bound @c2@ declares,
-- with an unspelled @c1@ var/arg defaulting to ⊤ (see 'PartialType'): every bound @c2@ declares
-- must be asked about, or @C ⟹ C[/T=∀Ord]@ holds vacuously when @c1@ omits @/T@ and the tighter
-- bound drops out of an intersection like @C[/T=∀Ord] ∧ C@.
-- Slots are compared under 'comparedPartialsEnv', NOT the bare ambient env: a @$/E@ in either
-- side's slot names that side's own 'ptVars' binding, and reading it as ⊤ would widen the
-- comparison into vacuously proving @C[/T=⊤] ⟹ C[/T=$/E]@.
predImpliesAtom typeEnv vaenv (PredClass c1) (PredClass c2)
  | ptName c1 == ptName c2
  , let vaenv' = comparedPartialsEnv typeEnv vaenv c1 c2
  , all (\(k, v) -> isSubtypeOfWithEnv typeEnv vaenv' (H.lookupDefault PTopType k (ptVars c1)) v) (H.toList (ptVars c2))
  , all (\(k, v) -> isSubtypeOfWithEnv typeEnv vaenv' (H.lookupDefault PTopType k (ptArgs c1)) v) (H.toList (ptArgs c2)) = True
-- PredClass: c1 implies c2 only when c1 is sealed (all members known) and each member is in U_c2.
-- Decided symbolically (emptiness of @c1 ∧ ¬c2@ against the class graph); the engine's positive
-- gate reproduces the sealed/'tePrgmEnv' enumeration gate.
predImpliesAtom typeEnv vaenv (PredClass c1) (PredClass c2) =
  satAtomImplies typeEnv vaenv ProveEmpty (SatClass c1) (SatClass c2)
-- PredRel: for the SAME relation entity (r2's name path is a suffix of r1's — one relation, spelled
-- relatively) the implication is EXACTLY covariance of r1's args AND element vars into r2's, decided
-- BOTH ways here. The 'ptVars' check mirrors the PredClass\/PredClass covariance above (an element
-- bound sits in 'ptVars', not 'ptArgs'): a 'foldRecursiveType' result carries the joined element on
-- the relation (@~ConsList[/T=⊤−A]@), and covariance must decide @~ConsList[/T=⊤] ⟹
-- ~ConsList[/T=⊤−A]@ FALSE — a list of anything is not a list of non-@A@s — rather than fall
-- through to the name-blind engine below, which would read @isa r ∧ ¬isa r@ (same name, differing
-- element) as a contradiction and prove the implication. A DIFFERENT-named r2 (a superclass) still
-- routes to the declared-edge\/engine cases.
predImpliesAtom typeEnv vaenv (PredRel r1) (PredRel r2)
  | relativeNameMatches (ptName r2) (ptName r1) = all checkArg (H.keys (ptArgs r2)) && all checkVar (H.keys (ptVars r2))
  -- A declared subclass edge @every(r1 isa r2)@ proves @isa r1 ⟹ isa r2@ directly, avoiding the
  -- 'expandRelPartial' below (which unrolls r1's members and recurses through a recursive
  -- parameterized member). Restricted to plain (arg/var-free) relations, matching the nullary
  -- class edges (@Ord isa Eq@) it targets - see 'classDeclaredSubclassOf'.
  | plainPartial r1 && plainPartial r2 && classDeclaredSubclassOf typeEnv (ptName r1) (ptName r2) = True
  -- Bounded-left form of the same edge (the right side must still be plain — the walk
  -- targets nullary edges).
  | plainPartial r2 && classDeclaredSubclassOfBounded typeEnv vaenv r1 r2 = True
  -- Symbolic emptiness of @r1 ∧ ¬r2@ replaces expanding both relations (which for a recursive
  -- parameterized class universe re-expands the element class combinatorially). Gated open-world
  -- ('ProveEmpty'), like every other kind of implication here: @isa r1 ⟹ isa r2@ must NOT be proved by
  -- enumerating an UNSEALED @r1@'s CURRENT members (@~List ⟹ ~Bag@ because List's only member so far
  -- is a Bag member is broken by any future @isa List@ declaration). Deciding it closed-world here
  -- while the gated 'predsEmpty' oracle refuses the same enumeration would make the two disagree, and
  -- @(~List ∩ ¬Bag)@ would collapse to ∅ on one route but not the other. A declared @isa@ edge still
  -- implies (the 'classDeclaredSubclassOf' fast path above), and a SEALED @r1@ is still enumerated
  -- (the gate only refuses an unsealed, non-'tePrgmEnv' class).
  | otherwise = satAtomImplies typeEnv vaenv ProveEmpty (SatRel r1) (SatRel r2)
  -- Slots are compared under 'comparedPartialsEnv', NOT the bare ambient env: a @$/E@ in either
  -- side's slot names that side's own 'ptVars' binding, and reading it as ⊤ would widen the
  -- comparison into vacuously proving @r[/items=⊤] ⟹ r[/items=$/E]@. A declaration that binds
  -- @/E@ to @∀Ord@ and spells its @/items@ dimension @$/E@ relies on this, and the difference
  -- ('differenceTypeWithEnv') must read the slot the same way the subtype walk does.
  where vaenv' = comparedPartialsEnv typeEnv vaenv r1 r2
        checkArg k = isSubtypeOfWithEnv typeEnv vaenv'
                       (H.lookupDefault PTopType k (ptArgs r1))
                       (ptArgs r2 H.! k)
        checkVar k = isSubtypeOfWithEnv typeEnv vaenv'
                       (H.lookupDefault PTopType k (ptVars r1))
                       (ptVars r2 H.! k)
-- A value predicate implies another only as the SAME FORM ('predExprSameForm'): a form is not a
-- set, and read as one through 'isSubPartialOfWithEnv' two different predicates were each within
-- the other (their @$this@ operand names no type, so both read empty), which made @¬P ⟹ ¬Q@ by
-- contraposition and dropped a region from every union of their negations.
predImpliesAtom _ _ (PredExpr e1) (PredExpr e2) = isJust (predExprSameForm e1 e2)
-- PredClass c implies PredRel r when r's name is a suffix of c's name (then
-- expandRelPartial r includes classPartial c, so U_PredClass c ⊆ U_PredRel r), or - like
-- the PredClass\/PredClass case - when c is sealed and each of its members is in U_PredRel r
-- (e.g. c is a subclass of the class that r names).
predImpliesAtom typeEnv vaenv (PredClass c) (PredRel r)
  | relativeNameMatches (ptName r) (ptName c) = True
  -- A declared subclass edge @every(c isa r)@ proves @hasClass c ⟹ isa r@: membership in class @c@
  -- gives @isa c@ (the same-name route above), and the edge gives @isa c ⟹ isa r@. Sound in the open
  -- world (a declared edge holds for every future member), so an UNSEALED empty class - an isa-only
  -- @Lizard@ - still implies its declared superclass relation @~Animal@ WITHOUT enumeration, which the
  -- gated 'satAtomImplies' below (rightly) refuses for an unenumerable open class. Mirrors the
  -- PredRel/PredRel and PredRel/PredClass 'classDeclaredSubclassOf' fast paths; only @r@ need be plain
  -- (a narrower element bound on @c@ only shrinks the left).
  | plainPartial r && classDeclaredSubclassOf typeEnv (ptName c) (ptName r) = True
  -- Bounded-left form: @hasClass BinTree[K→Ord,V→Eq] ⟹ isa Eq@ by the var-carrying edge.
  | plainPartial r && classDeclaredSubclassOfBounded typeEnv vaenv c r = True
  | otherwise = satAtomImplies typeEnv vaenv ProveEmpty (SatClass c) (SatRel r)
-- PredRel r implies PredClass c when every type matching r is a member of c.
-- Fast path (mirrors the @PredClass c ⟹ PredRel r@ same-name case above): for the SAME plain class
-- name, @isa c ⟹ hasClass c@ holds directly — a value whose type is a member of class @c@ has class
-- @c@. This avoids expanding BOTH sides via 'expandRelPartial'/'expandClassPartial', which for a
-- recursive parameterized class (e.g. @Eq@ with @ConsList[Eq]@/@Maybe[Eq]@ members) re-expands the
-- element class unboundedly. Restricted to arg/var-free partials naming a real class.
predImpliesAtom typeEnv vaenv (PredRel r) (PredClass c)
  | plainPartial r, plainPartial c
  , sameClass || classDeclaredSubclassOf typeEnv (ptName r) (ptName c) = True
  -- Bounded-left form of the declared-edge route only (the same-class route keeps its
  -- plain-r guard: @isa c ⟹ hasClass c@ is a same-entity reading, not an edge).
  | plainPartial c, classDeclaredSubclassOfBounded typeEnv vaenv r c = True
  -- Gated ('ProveEmpty') like the other outermost implication decisions: a relation naming an
  -- UNSEALED class must not be enumerated closed-world here — @~List ⟹ ∀Bag@ via the current
  -- members is broken by any future @isa List@ declaration, and deciding it True while the
  -- gated 'predsEmpty' oracle refuses the same enumeration would make a union drop the relation
  -- in one associativity order but not the other.
  | otherwise = satAtomImplies typeEnv vaenv ProveEmpty (SatRel r) (SatClass c)
  where
    -- Same named entity (either absoluteness direction): @isa c ⟹ hasClass c@.
    sameClass = relativeNameMatches (ptName r) (ptName c) || relativeNameMatches (ptName c) (ptName r)
-- Cross-kind: conservative False
predImpliesAtom _ _ _ _ = False

-- | Returns 'True' if every type satisfying @preds@ is a member of @partials@, i.e. U_preds ⊆ partials.
-- Expands @preds@ restricted to the names present in @partials@; if the expansion is exact
-- (no elements outside those names) and the result is a subset of @partials@, returns 'True'.
-- When the restricted expansion is inexact ('hasOutside = True'), conservative 'False': the
-- name-materialized expansion does not exhaust U (constants and open-world values carry no
-- partial name), so an empty full-name expansion must NOT be read as @U_preds = ∅@ — e.g.
-- @¬(PredRel n)@ subtracts every known name yet still contains every constant. A genuinely
-- contradictory conjunction is decided by 'isPredsContradictory'/'predsEmpty' at the call site.
predImpliesPartials :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicates -> PartialLeafs -> Bool
predImpliesPartials typeEnv vaenv preds partials
  | not (predsPositiveAtoms preds) = False
  | otherwise =
      case expandTypesWithNamesFull typeEnv vaenv (TopType H.empty preds) (H.keysSet partials) of
        (expanded, False) -> isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing expanded []) (UnionType Nothing partials [])
        (_,        True)  -> False

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

expandClassPartialWithNamesFull :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> S.HashSet TypeName -> (PartialLeafs, Bool)
expandClassPartialWithNamesFull = expandClassPartialWithNamesFullAux S.empty

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
expandClassMemoRef :: IORef [(Int, H.HashMap ExpandClassMemoKey (Type, ReadDeps))]
expandClassMemoRef = unsafePerformIO (newIORef [])

expandClassPartialAux :: (TypeGraph tg) => Maybe (S.HashSet TypeName) -> TypeEnv tg -> TypeVarArgEnv -> PartialType -> Type
expandClassPartialAux mAllowed typeEnv vaenv pp = case teQueryCacheKey typeEnv of
  Nothing -> expandClassPartialAuxRaw mAllowed typeEnv vaenv pp
  Just _ | teInSatDecision typeEnv -> expandClassPartialAuxRaw mAllowed typeEnv vaenv pp
  Just tok -> unsafePerformIO $ do
    let key = (tok, (teDisableCompact typeEnv, tePrgmEnv typeEnv, teArgModeProjection typeEnv), mAllowed, pp, L.sortOn fst (H.toList vaenv))
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

-- | The value SET a partial's value denotes standalone: read via 'substituteTVExt' under
-- the partial's OWN var\/arg scope over the ambient env — the codebase's canonical reading
-- (the same one 'mergeVarEnvVal' applies to every stored env binding), so forms produced
-- here are structurally comparable with the intersection's. A top-level reference reads
-- through the partial's slot (@head=$/T@ with @/T=X@ denotes @X@), a nested partial's
-- references read through ITS own slots, and an unbound reference reads as ⊤.
resolveOwnScopeRefs :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> Type -> Type
resolveOwnScopeRefs typeEnv vaenv p = substituteTVExtScoped typeEnv (ptVarArg p) vaenv

-- | 'freeTVIntToTop' applied DEEP: every reference the given partial does not bind reads as ⊤
-- at ANY depth, including class-predicate parameters (@∀ConsList[/T=$/T]@ under a partial with
-- no @/T@ slot reads @∀ConsList[/T=⊤]@ — the union over all instantiations). Used to
-- instantiate a DECLARED field type under a specific partial: the template's free parameters
-- must NOT stay symbolic, lest they resolve through a like-named binding in a shared env
-- during a subtype check (the same leak 'freeTVIntToTop' guards against, at depth). This
-- covers EXTERNAL ('TVExt') template spellings too — a declaration's recursive tail is stored
-- as @∀ConsList[/T=$_/T]@, and a symbolic @$_/T@ surviving instantiation would be captured by a
-- like-named binding of an ENCLOSING unrolling level: in a nested list, the outer cell's
-- @/T=Integer@ would make an inner cell's ⊤ head read Integer. A template reference is
-- therefore resolved SCOPE-EXACTLY: a declaration template's variables belong to the
-- declaration's own scope, so a reference the partial itself binds substitutes that binding
-- immediately (a self-referential spelling — @/T ↦ $_/T@, an unbound declared parameter —
-- reads ⊤) and every other reference reads ⊤, NEVER the ambient env — a like-named ambient var
-- is a DIFFERENT scope's var, and both keeping the reference symbolic and reading it from the
-- ambient env would let an enclosing level's binding capture the template.
-- References INSIDE a substituted binding do resolve ambient-or-⊤: the binding is the
-- partial's own content, which legitimately lives in the ambient scope. A reference a NESTED
-- partial itself links is also widened — a sound over-approximation of the field's value set,
-- which for a clamp read only makes the check more conservative.
freeTVIntToTopDeep :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> Type -> Type
freeTVIntToTopDeep typeEnv vaenv p = goT
  where
    own = ptVarArg p
    -- The occurrence's inner is occurrence-carried CONTENT, so it takes the binding-content
    -- walk 'goB' (not the template walk) and meets into every resolution of the reference.
    -- A binding that is itself a parent reference (@Node[/V=$^/MV]@) is spelled at the partial's
    -- level and has no spelling in the template position one level in, so the reference is kept
    -- as the hop through the partial's own parameter ('keepsHop', as 'substituteTVExtScoped').
    -- Substituted, the declared @left -> Tree[$V -> $^V]@ would read @∀Tree[/V=$^/MV]@, naming
    -- an @/MV@ of the wrong partial.
    goT (TypeVar v loc i) = case H.lookup (extKey v) own of
      Just (TypeVar v' _ _) | v' == v -> goB i
      Just b | keepsHop b             -> TypeVar v loc (goB i)
             | otherwise              -> meetInner typeEnv vaenv (goB b) (goB i)
      Nothing                         -> goB i
    goT (UnionType mtop pos consts) = UnionType (fmap (goTop goPartialT) mtop) (goLeafs goPartialT pos) consts
    goB (TypeVar v loc i) | H.member (extKey v) vaenv = TypeVar v loc (goB i)
                        | otherwise        = goB i
    goB (UnionType mtop pos consts) = UnionType (fmap (goTop goPartialB) mtop) (goLeafs goPartialB pos) consts
    goTop gp (preds, negLeafs, ncs) = (mapTypePreds (mapTypePred gp) preds, goLeafs gp negLeafs, ncs)
    goLeafs gp = joinUnionType . map gp . splitUnionType
    goPartialT pt = pt{ ptVars  = fmap goT (ptVars pt)
                      , ptArgs  = fmap goT (ptArgs pt)
                      , ptPreds = mapTypePreds (mapTypePred goPartialT) (ptPreds pt) }
    goPartialB pt = pt{ ptVars  = fmap goB (ptVars pt)
                      , ptArgs  = fmap goB (ptArgs pt)
                      , ptPreds = mapTypePreds (mapTypePred goPartialB) (ptPreds pt) }

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
satMemoRef :: IORef [(Int, H.HashMap SatMemoKey (Bool, ReadDeps))]
satMemoRef = unsafePerformIO (newIORef [])

-- | The keys whose decision is currently BEING computed, on each thread's evaluation stack
-- ('InFlight'). Never capped: its size is bounded by the recursion depth, and dropping a marker
-- mid-derivation would silently re-open the very cycle the marker refutes.
{-# NOINLINE satInFlightRef #-}
satInFlightRef :: InFlight SatMemoKey
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
    let key = (tok, (teDisableCompact typeEnv, tePrgmEnv typeEnv, teArgModeProjection typeEnv, teInSatDecision typeEnv, teDebug typeEnv), L.sortOn fst (H.toList vaenv), satUse, L.sort (S.toList hyps), goal)
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

-- | Engine-backed emptiness of a predicate conjunction ('predsEmpty' fast path):
-- @Just isEmpty@ when the predicates are within the decidable fragment.
--
-- The satisfiability check runs under the open-world gate ('ProveEmpty'), so a
-- conjunction of distinct /open/ classes (e.g. @Functor[Eq] ∧ Eq@) is treated as satisfiable
-- rather than empty: a future file may declare a type inhabiting both, so the conjunction is
-- not provably disjoint. The gate itself folds in 'tePrgmEnv' ('classSatCandidates'), so in the
-- closed-world coverage environment ('tePrgmEnv' set) enumeration is still exhaustive and a
-- genuinely uninhabited conjunction is reported empty. Only classes provably disjoint by the
-- class graph remain empty in the open world.
predsEmptySymbolic :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> TypePredicates -> Maybe Bool
predsEmptySymbolic typeEnv vaenv preds = do
  goals <- predsSatDNF True preds
  not <$> satGoalsDecided typeEnv vaenv ProveEmpty goals

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

-- | The loose /projection/ arg-keyset relation used by matching\/dispatch consumers (see
-- 'teArgModeProjection'). Returns 'True' when @sub@ definitely cannot project onto @super@
-- on the arg-set dimension. The 'PtArgExact'\/'PtArgAny' combinations are handled by the first
-- four cases, and any combination involving a lower-unbounded mode falls back to the interval
-- relation ('argModeCovers'). Crucially, an 'Any'\/upper-unbounded source projecting onto an
-- 'Exact' requirement is feasible iff its listed args ⊆ the requirement's args (extra args are
-- ignored).
looseArgModeRejects :: PartialType -> PartialType -> Bool
looseArgModeRejects sub@PartialType{ptArgs=subArgs, ptArgMode=subMode} super@PartialType{ptArgs=superArgs, ptArgMode=superMode} =
     (subMode == PtArgExact && superMode == PtArgExact && subKeys /= superKeys)
  || (subMode == PtArgAny && superMode == PtArgAny && not (superKeys `isSubsetOf` subKeys))
  || (superMode == PtArgExact && not (subKeys `isSubsetOf` superKeys))
  || (subMode == PtArgExact && superMode == PtArgAny && S.null subKeys && not (S.null superKeys))
  || (intervalOnly && not (argModeCovers super sub))
  where
    subKeys = H.keysSet subArgs
    superKeys = H.keysSet superArgs
    -- A lower-unbounded mode, or one with optional args ('ptArgOpt'), is none of the four named
    -- combinations above, so it is judged by the interval relation.
    intervalOnly = not (ptArgLb subMode) || not (ptArgLb superMode) || not (S.null (ptArgOpt subMode)) || not (S.null (ptArgOpt superMode))

-- | The environment for comparing two same-name partials slot by slot.
--
-- 'ptVars' is a BINDER: a partial-local ('TVInt') reference inside a leaf names THAT leaf's
-- variable. Comparing the leaves' slots under the bare ambient environment reads such a
-- reference through 'vaenvLookup', which defaults an unbound name to ⊤ — and since ⊤ is the top
-- of the lattice, forgetting the binder can only WIDEN. A prover that forgets therefore
-- over-approximates membership and under-approximates difference: it proves @C[/T=⊤] ⟹
-- C[/T=$/E]@ for a partial that binds @/E@ to something well below ⊤.
--
-- So the partials' OWN var/arg slots take precedence over the ambient env (left-biased union),
-- and a name both bind reads as the meet. An ambient @/T ↦ ∀Group@ (the surrounding law
-- declaration's parameter) must not shadow a compared leaf's own @[/T=∀Eq]@ when resolving its
-- @$/T@ arg slots; ambient bindings still resolve names the leaves do not declare.
--
-- THE RULE, both halves. A traversal that descends through a binder must resolve the references
-- below it CORRECTLY — neither too leniently nor too conservatively:
--
--   * Too LENIENT is reading the reference as ⊤ (the 'vaenvLookup' default). It widens, and the
--     widening is silent, because an unbound variable legitimately denotes ⊤ — a missing binding
--     and a genuinely free variable are the same map miss.
--   * Too CONSERVATIVE is baking the variable's DECLARED BOUND onto the occurrence and treating
--     that as the answer. @$/E@ under @[/E=∀Ord]@ is not "some Ord": it is the SAME type as every
--     other occurrence of @/E@ in this partial, and at a call site that instantiates it, that type
--     is @Integer@, not @∀Ord@. Freezing it at the declared bound discards the instantiation and
--     the cross-occurrence link — the very things that make it a variable rather than a type — and
--     the loss simply resurfaces later as @∀Ord@ where @Integer@ was required.
--
-- A variable's value is CONTEXTUAL, so it cannot be pre-computed onto the occurrence and must be
-- threaded. (Contrast a variable's SCOPE CLASS — 'TVInt'\/'TVParent'\/'TVExt' — which is lexical,
-- is assigned once at desugar, and is deliberately never re-derived here.) Every descent owes the
-- correct resolution; there is no representation that excuses a traversal from doing it.
comparedPartialsEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> TypeVarArgEnv
comparedPartialsEnv typeEnv vaenv a b = H.union (ambientEnv $ (H.filterWithKey installable merged)) (shadowBoundParams typeEnv [a, b] vaenv)
  where
    merged = H.union
      (H.mapWithKey (\k -> installVar . readSiblings k) mergedVars)
      (H.unionWith (intersectTypesEnv typeEnv argEnv) (readArgs a) (readArgs b))
    mergedVars = H.unionWith (intersectTypesEnv typeEnv vaenv) (fst (splitVarArgs a)) (fst (splitVarArgs b))
    -- A parent reference in a parameter's value that names ANOTHER parameter of the compared
    -- partials names that sibling (@$F -> Fun[$T -> $_A]@ spells @/F=∀Fun[/T=$^/A]@), as the
    -- meet reads a bound ('intersectPartialsBaseN'.'withSiblings'). Read as the enclosing scope,
    -- as 'installVar' reads the rest, it is ⊤ whenever that scope does not bind the name, while the
    -- same spelling in an argument slot reads the sibling's value ('readArgs'): @lst=$/F@ against
    -- @lst=Bx[/T=$^/A]@ then compared @Bx[/T=⊤]@ with @Bx[/T=/A's value]@. A reference to the
    -- parameter's OWN name cannot be a sibling and is left to 'installVar'. The sibling's value is
    -- read with its own references detached, so two parameters naming each other cannot loop.
    readSiblings (TVVar n) = goSib
      where
        goSib (TypeVar (TVVar n') TVParent i)
          | n' /= n, Just sib <- H.lookup (TVVar n') mergedVars = meetInner typeEnv vaenv (installVar sib) (goSib i)
        goSib (TypeVar v loc i) = TypeVar v loc (goSib i)
        goSib (UnionType mtop pos consts) = UnionType (fmap onTop mtop) (joinUnionType $ map onLeaf $ splitUnionType pos) consts
        onTop (preds, negLeafs, ncs) = (mapTypePreds (mapTypePred onLeaf) preds, joinUnionType $ map onLeaf $ splitUnionType negLeafs, ncs)
        onLeaf q@PartialType{ptVars=qVars} = q{ptVars = fmap goSib qVars}
    readSiblings _ = id
    splitVarArgs p = (joinVarArgEnv (ptVars p) H.empty, joinVarArgEnv H.empty (ptArgs p))
    -- An ARGUMENT value sits one level inside its partial, so its references name that partial's
    -- parameters: a pointer (@head=$/T@) or a parent reference in a class atom (the declared
    -- @tail=∀ConsList[/T=$^/T]@) reads the partial's own parameter value, and a parameter it
    -- declares but does not spell is the ⊤ an unspelled parameter denotes. Each side is read in
    -- its OWN scope before the two meet, and the meet runs with every parameter the partials
    -- bind shadowed ('shadowBoundParams'), as the arguments themselves are compared. Read as a
    -- fragment of the object instead ('substituteTVExt'), a like-named ambient binding would
    -- capture the reference, and the installed binding would carry the ambient value.
    argEnv = shadowBoundParams typeEnv [a, b] vaenv
    readArgs p = fmap (substituteTVExtScoped typeEnv ((ambientEnv $ ownScope p)) argEnv) (snd (splitVarArgs p))
    ownScope p = joinVarArgEnv (H.union (fmap installVar (ptVars p)) (declaredParamsAtTop typeEnv p)) H.empty
    -- Rule one at the binder. The bindings installed here are what the partials' ARGUMENTS are
    -- compared under, one level in; a var slot's own value sits at the partial's level, so a parent
    -- reference in it names the scope the partial sits in, which is the incoming env. That scope is
    -- out of reach inside the arguments, so the reference is replaced by its value from there, and
    -- one the incoming env does not bind reads as the ⊤ a detached reference denotes. Installed
    -- verbatim instead, it would resolve inside the arguments through whatever binds the NAME
    -- there — a nested partial's like-named parameter — and a chain of such bindings can close
    -- on itself: @/MV ↦ $^/V@ beside @/V ↦ $^/MV@ would send 'isSubtypeOfWithEnv' round the
    -- pair forever.
    installVar v = detachLevelParentRefs (substituteTVExtScoped typeEnv vaenv vaenv v)
    -- A binding whose VALUE still mentions the name being bound is a recursive equation, not a
    -- value, and installing it changes the operands mid-comparison: the slot value here is derived
    -- from @a@ and @b@ themselves, so an occurrence of the same name INSIDE @a@ then resolves to a
    -- function of @a@ rather than to whatever it denoted. A parameterized class compared against a
    -- union of itself is exactly that shape — @/T@ bound to @a ⊓ (a∪b)@ while @a@ carries a
    -- @$_/T@ of its own (@length(this -> ∀List[/T=$_/T])@) — and installing it would make the
    -- covariance rule refuse a genuine @a ⊆ a∪b@. Leaving the key unbound keeps the occurrence
    -- symbolic and reads it IDENTICALLY
    -- on both sides, which is what a comparison needs; the guard mirrors 'isSelfVar' in
    -- 'intersectTypesWithVarEnv' and 'cleanDeclSelfRefs' on the declaration path.
    -- Tested on the RAW slot values, not on 'merged': the meet and 'substituteTVExt' have already
    -- rewritten a self-reference by then (an unbound @$_/T@ becomes ⊤), so the merged value no
    -- longer shows the cycle that makes the binding unsafe to install.
    -- The mention may be INTERNAL as well as outer. A free 'TVInt' — one no partial in view
    -- declares ('typeFreeTVIntRefs') — is read through the ambient env by the same var-slot-value
    -- rule the leaf readers implement ('partialCoversSat'.'ambientRef'), so installing a binding
    -- whose value carries one under that very name captures it. Covariance of a parameterized
    -- class is decided by comparing its two elements under this env, and an element carrying a
    -- free @$/T@ of its own would read as the element itself: @C[/T=Some(value=$/T)]@ against
    -- @C[/T=Some(value=⊤)]@ would compare a value that contains itself. The element's own name
    -- space is not the class's.
    --
    -- A 'TVParent' mention is NOT a self-reference: it names the ENCLOSING partial's variable, a
    -- different variable that happens to share the name (@Pair[/A=$^/A]@ nested in an outer
    -- @Pair@), and 'installVar' has already replaced it with that variable's value from the
    -- incoming env, so the binding is installable.
    installable k@(TVVar n) _ = not $ any mentions $ mapMaybe (H.lookup (extKey k)) [ptVarArg a, ptVarArg b]
      where mentions v = S.member n (typeExtVarRefs v) || S.member n (typeFreeTVIntRefs v)
    installable _ _ = True

-- | A private helper for 'isSubPartialOfWithEnv' that checks while ignore class expansions
isSubPartialOfWithEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> Bool
isSubPartialOfWithEnv TypeEnv{teDebug=True} _ l r | trace (printf "[SUBPARTIAL] %s ⊆ %s" (show l) (show r)) False = undefined
isSubPartialOfWithEnv _ _ PartialType{ptName=subName} PartialType{ptName=superName} | subName /= superName = False
-- A sub partial's own positive class and rel predicates are read the way the meet and
-- 'compactType' read them ('compactPartialsWithClassPred'), which expands each through its
-- membership and meets it back in. Where membership is conditional on a parameter, that turns
-- the predicate into the parameter's bound: under @every(ConsList[$T -> EqCO], isa= [EqCO])@ in
-- the closed world, @Cons(head=⊤, tail=⊤) | EqCO@ is @Cons[/T=∀EqCO](head=⊤, tail=⊤)@. Read
-- as spelled instead, the sub's absent @/T@ counted as ⊤, so that pair compared unequal one way
-- while their difference was ∅. It was found as core/maybe.ct's @equal@ arms falling outside
-- the declaration their meet had just been shown inside. The expansion is the compacted form of
-- the same set, and its leaves no longer carry the expanded atoms, so each is decided on its own.
isSubPartialOfWithEnv typeEnv vaenv sub0 super0
  | predsHasClassRelAtom (ptPreds sub0)
  , let expanded = splitUnionType (compactPartialsWithClassPred typeEnv vaenv [sub0])
  , expanded /= [sub0]
  = all (\sub -> isSubPartialOfWithEnv typeEnv vaenv sub super0) expanded
-- Both sides are read with their spread floor ('spreadFloor') before anything else looks at
-- them: the floor is then a listed field for the arg comparison and the clamp below, not
-- only for the keyset check.
isSubPartialOfWithEnv typeEnv vaenv sub0 super0 = isSubPartialOfWithEnvN typeEnv vaenv (spreadFloor typeEnv sub0) (spreadFloor typeEnv super0)

-- | One materialized sub-partial goal: the two partials, plus the env they are read under and
-- the flags that change the reading. Compared by 'Eq' (the 'Hashable' instances on types are
-- deliberately shallow), so a hit is the same goal, never a collision.
type SubPartialKey = (Bool, Bool, [(VarKey, Type)], PartialType, PartialType)

-- | The sub-partial goals whose answer is currently BEING computed, on each thread's evaluation
-- stack ('InFlight') — the materialized counterpart of 'satInFlightRef'.
--
-- The symbolic engine terminates on a recursive class because its hypothesis stack refutes a
-- goal that recurs on its own derivation path. The MATERIALIZED path — the fallback every
-- engine entry takes when its operands leave the decidable fragment, for example when either
-- carries a constant — needs the same rule, or a self-similar comparison walks forever:
-- @Cons[/T=X] ⊆ Cons[/T=∀Eq]@ asks its @tail@ slots, @∀ConsList[/T=X] ⊆ ∀ConsList[/T=∀Eq]@
-- expands to the members, and the @Cons@ member poses the first goal again, one level down and
-- never equal to a previous level, so the structural @t1 == t2@ short-circuit never fires.
--
-- The re-entry answers FALSE — cover unproven along this derivation path — which is the
-- engine's own polarity ('satInFlightRef': a goal that recurs can produce no finite witness)
-- and the algebra's standing convention that an unproven cover is not a cover. The coinductive
-- reading (assume the goal, answer True) is WRONG for these callers: the meet routes that ask
-- "is this sub covered" then drop an operand's slot, so @∀Eq ∩ Cons[/T=$_a]@ would come back
-- as @Cons[/T=∀Eq]@ — wider than an operand.
--
-- Never reset and never capped, for the reason 'satInFlightRef' gives: its size is bounded by
-- the recursion depth, and dropping a marker mid-derivation would re-open the cycle it refutes.
{-# NOINLINE subPartialInFlightRef #-}
subPartialInFlightRef :: InFlight SubPartialKey
subPartialInFlightRef = unsafePerformIO newInFlight

isSubPartialOfWithEnvN :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> Bool
isSubPartialOfWithEnvN typeEnv vaenv sub super = unsafePerformIO $ do
  let k = (tePrgmEnv typeEnv, teArgModeProjection typeEnv, L.sort (H.toList vaenv), sub, super)
  inFlightOn subPartialInFlightRef k (noteCycleCut False) $
    evaluate (isSubPartialOfWithEnvNRaw typeEnv vaenv sub super)

isSubPartialOfWithEnvNRaw :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> Bool
-- Projection (loose) keyset feasibility, used by matching\/dispatch consumers via
-- 'teArgModeProjection' (TreeBuild, Eval, scheme verification): an 'Any'\/upper-unbounded
-- source may project onto an 'Exact' requirement by ignoring extra args. This looseness is
-- genuine projection (not an interval relation)
-- and is intentionally NOT applied to the precise set algebra below.
isSubPartialOfWithEnvNRaw typeEnv _ sub super
  | teArgModeProjection typeEnv
  , looseArgModeRejects sub super = False
-- Keyset feasibility on the arg-set interval (see 'PtArgMode'): sub ⊆ super requires
-- super's lower\/upper bound to hold for every arg-set allowed by sub ('argModeCovers'),
-- on mode-normalized partials so a vacuous bound does not read as extra arg-sets
-- (e.g. @/length(this=..)..@ IS @/length(this=..)@ — see 'normalizePtArgMode').
isSubPartialOfWithEnvNRaw typeEnv _ sub super
  | not (teArgModeProjection typeEnv)
  , not (argModeCovers (normalizePtArgMode typeEnv super) (normalizePtArgMode typeEnv sub)) = False
isSubPartialOfWithEnvNRaw typeEnv vaenv sub@PartialType{ptVars=subVars, ptArgs=subArgs, ptPreds=subPreds} super@PartialType{ptVars=superVars, ptArgs=superArgs, ptPreds=superPreds} = debugTrace typeEnv msg result
  where
    vaenv' = comparedPartialsEnv typeEnv vaenv super sub
    -- A sub-side FREE reference reads as ⊤ ('freeTVIntToTop'): it must not resolve through
    -- the shared env's like-named SUPER binding, which would narrow the sub's value set to
    -- the super's and wrongly prove e.g. @Cons(head=$/T) ⊆ Cons[/T=X](head=X)@.
    -- A field's value set is DECLARATION-BOUNDED: no constructible value holds a field outside
    -- its declared type (@Cons(tail=⊤)@ admits no tail outside @∀ConsList@), so when the literal
    -- comparison fails the sub value is re-read clamped by the declared field type (the clamp
    -- only shrinks the sub side, so checking it as a fallback equals always clamping). This is
    -- what makes the field-completed @Cons(head=⊤, tail=⊤)@ a subtype of the class member form
    -- @Cons[/T=$/T](head=$/T, tail=∀ConsList[/T=$/T])@ — the same set under the declaration.
    -- The declared bound is instantiated with 'freeTVIntToTopDeep' under the SUB's scope: its
    -- free template parameters read ⊤ rather than staying symbolic and resolving through the
    -- shared env's SUPER binding (which would wrongly prove
    -- @Cons(head=$/T) ⊆ Cons[/T=X](head=X)@ again through the clamp).
    -- Both sides' FREE references read as the ⊤ they denote ('freeTVIntToTop') — a 'TVInt'
    -- resolves against its own partial's scope only, so a free one must not resolve through
    -- the shared @vaenv'@'s like-named entry (which holds the COMPARED partials' var meet, a
    -- different scope) on either side.
    -- An UNBOUND ambient reference ('TVExt' with no binding in the ambient env, at any depth)
    -- reads as the ⊤ it denotes BEFORE the compared partials' own bindings are installed
    -- ('freeTVExtToTopDeep'): left symbolic, a nested call's free parameter
    -- (@Some[/T=X](value= length(this= ∀List[/T=$_/T]))@) would resolve through @vaenv'@ to the
    -- compared @Some@'s @/T@ — capture by a binder the external spelling explicitly steps past
    -- — and the subtype would read the reference differently from the meet, which reads it
    -- ambiently. A BOUND ambient reference keeps resolving through
    -- the env, and the partials' own ('TVInt'\/'TVParent') references still read the installed
    -- bindings.
    hasAll sb sp = and $ H.elems $ H.intersectionWithKey argOk (fmap (freeExt . freeTVIntToTop sub) sb) (fmap (freeExt . freeTVIntToTop super) sp)
      where
        argOk k sbVal spVal = isSubtypeOfWithEnv typeEnv vaenv' sbVal spVal
          || maybe False (\d -> isSubtypeOfWithEnv typeEnv vaenv' (intersectTypesEnv typeEnv vaenv' sbVal d) spVal) (declaredArgBound typeEnv vaenv subEff k)
    -- Type parameters: mirrors 'partialCoversSat'. Under projection a Top-valued parameter on
    -- the sub side is no constraint, and is equivalent to the parameter being absent.
    --
    -- Outside projection a super-side parameter the sub omits is compared against the sub's ⊤
    -- there (see 'PartialType'). Skipping it proves @/Some ⊆ /Some[/T=/Nil]@ vacuously, and since
    -- the converse genuinely holds the two become equal — which absorbs @/Some ∪ /Some[/T=X]@ to
    -- the narrow side and puts @a∩(b∪c)@ strictly below @(a∩b)∪(a∩c)@. Under projection a
    -- ⊤-valued parameter carries no dispatch information, so the skip is right there.
    -- A sub-side slot still FLEXIBLE after reading through the sub's own scope (a self-entry
    -- @/T=$/T@ or an env-unbound var) ranges over every binding, so it is a subtype only of a
    -- super slot that itself admits every binding (⊤ or another flexible variable) — resolving
    -- it through the shared @vaenv'@ would read the SUPER-met binding back and vacuously prove
    -- e.g. @Cons[/T=$/T](head=$/T) ⊆ Cons[/T=⊤−X](head=$/T)@ ('partialCoversSat'.'elemCovered'
    -- is the settled mirror of this rule).
    -- The sub's var value is first read DECLARATION-BOUNDED through its pointer-declared
    -- args: a field declared as the bare parameter (@head -> $T@) makes a value's @/T@ that
    -- field's value, so @Cons[/T=⊤](head=W)@ and @Cons[/T=W](head=$/T)@ spell the same set
    -- (a ??-interval piece and its enumerated difference form respell the same narrowing on
    -- the slot vs the var dimension) and must compare equal.
    -- The field's value is read in the sub's OWN scope first ('ptVarEff'): a parent reference
    -- inside it (@head=Cons[/T=$^/T](..)@) names the sub's parameter.
    subVarEff = ptVarEff typeEnv vaenv sub
    subEff = withEffectivePtVars typeEnv vaenv sub
    hasAllVars sb sp = and $ H.elems $ H.intersectionWith varOk (H.mapWithKey subVarEff (fmap freeExt sbFull)) (fmap freeExt sp)
      where sbFull | teArgModeProjection typeEnv = sb
                   | otherwise                   = H.union sb (H.mapWithKey unspelledSubParam sp)
    freeExt = freeTVExtToTopDeep vaenv
    varOk PTopType _ | teArgModeProjection typeEnv = True
    varOk a b
      | not (teArgModeProjection typeEnv)
      , flexibleVar (readOwnScope sub a) = b == PTopType || flexibleVar (readOwnScope super b)
    -- A PARAMETER value sits at the scope containing the partial, so it is compared under the
    -- incoming env, not under @vaenv'@ (the partials' own scope, which their FIELDS are read in).
    -- The two share a key space: read under @vaenv'@, a parent reference @$^/A@ in a nested
    -- @Pair[/A=$^/A]@ would look up the nested pair's OWN met @/A@ instead of the enclosing
    -- binding.
    --
    -- Within that containing-level reading, a 'TVInt' at the top of the value still names the
    -- partial ITSELF (a sibling @Pair[/B=~Pair, /A=$/B]@, or a self-entry): the register rule puts
    -- the value at the partial's level, which is where its own parameters live. So the value is
    -- first stepped through the partial's own spelled parameters ('readOwnScope'.'step') and then
    -- compared under the incoming env, which is where a 'TVParent' finds the enclosing binding.
    varOk a b        = isSubtypeOfWithEnv typeEnv vaenv (stepOwnParam sub a) (stepOwnParam super b)
    stepOwnParam p t@(TypeVar (TVVar n) TVInt i) = case H.lookup n (ptVars p) of
      Just v | v /= PTopType, v /= t -> meetInner typeEnv vaenv v i
      _                              -> t
    stepOwnParam _ t = t
    -- Read a var slot through its OWN partial's scope: a bare internal pointer steps to its
    -- binding ('partialCoversSat'.'subSlot'), then self-spelled external refs resolve through
    -- the partial's @ptVars@ ('partialCoversSat'.'resolveSelfRefs').
    readOwnScope p t0 = substituteKeepScoped typeEnv ((ambientEnv $ joinVarArgEnv (ptVars p) H.empty)) H.empty (step t0)
      where
        step t@(TypeVar (TVVar n) TVInt i) = case H.lookup n (ptVars p) of
          Just v | v /= PTopType, v /= t -> meetInner typeEnv vaenv v i
          Just _                         -> t
          -- Unspelled: the partial's own flexible ⊤ ('TypeVarLoc': a 'TVInt' names a variable of
          -- the same partial). An enclosing scope's variable is spelled 'TVParent'
          -- ('tagParentVarRefs'), so there is nothing to read from the ambient env here.
          Nothing                        -> t
        step t = t
    -- FLEXIBLE means "ranges over every binding", which is why a flexible sub slot is a subtype
    -- only of a super slot that itself admits everything. An occurrence CARRYING KNOWLEDGE is not
    -- that: @$_b ∩ {X}@ ranges only within @X@. Calling it flexible demands the super side be ⊤ or
    -- another variable and so REJECTS the true @$_b∩{X} ⊆ X@ — too conservative, and inconsistent
    -- with 'compactType' and the meet, which read an occurrence as @⟦var⟧ ⊓ ⟦inner⟧@.
    -- A knowledge-carrying occurrence therefore falls through to 'isSubtypeOfWithEnv', which
    -- applies that same reading. Holds for BOTH locations: the inner bounds the per-value binding
    -- of a 'TVInt' exactly as it bounds an external one.
    flexibleVar (TypeVar _ _ i) | i /= PTopType = False
    -- A knowledge-free occurrence is bindable per value; a self-link env entry counts flexible
    -- only when its inner is ⊤ — a knowledge-carrying self-link is information.
    flexibleVar (TypeVar _ TVInt _) = True -- partial-local var: bindable per value
    flexibleVar (TypeVar v TVOuter _) = case outerBinding vaenv v of
      Unbound     -> True
      SelfLink bI -> bI == PTopType
      BoundTop    -> True -- bound to Top: unconstrained, so still bindable per value
      BoundVar _  -> False
      Bound _     -> False
    flexibleVar _ = False
    result = hasAll subArgs superArgs && unlistedOk && predsOk && hasAllVars subVars superVars && pinnedSlotsOk typeEnv vaenv sub super
    msg = printf "[SUBPARTIAL] %s ⊆ %s = %s" (show sub) (show super) (show result)
    -- An arg the super lists and the sub does not: a sub without an upper bound may still carry
    -- it, unlisted, so with any value its declaration allows, which the super's must then admit.
    -- (A super requiring it was already refused by 'argModeCovers'; one listing it as optional, or
    -- without a lower bound, types it only where present.) Under projection an unlisted arg is
    -- ignored, as the keyset relation there ignores it.
    unlistedOk = teArgModeProjection typeEnv || ptArgUb (ptArgMode sub)
      || and [ isSubtypeOfWithEnv typeEnv vaenv' (fromMaybe PTopType (declaredArgBound typeEnv vaenv subEff k)) (freeExt (freeTVIntToTop super spVal))
             | (k, spVal) <- H.toList (H.difference superArgs subArgs) ]

    predsOk = hasAllPreds superPreds subPreds || leafCtxPredsCover
    -- Leaf-contextual predicate implication: the formula-vs-formula oracle ('hasAllPreds')
    -- is leaf-blind, but two spellings need only agree on THIS leaf's value set
    -- (@¬~Cons[T=Eq](…)@ ⟺ @¬EqC@ on @Cons@-shaped values — the difference route's
    -- symbolic-neg attach vs the meet route's class attach — while neither implies the
    -- other universally). Decide coverage of the sub leaf (with its own predicates) by the
    -- super's predicate universe symbolically.
    --
    -- When the engine cannot encode the LEAF (a constant in a slot: @Cons[/T=3]@ against
    -- @⊤|~List@), the question is still an ordinary subtype question over predicates that
    -- expand into leafs, so it goes to the whole subtype relation, which expands them within
    -- the leaf's name. When the engine cannot encode the PREDICATES (a 'PredExpr'), expansion
    -- has nothing more to offer: the atom stays attached to every leaf it expands to and the
    -- question comes back here. A value predicate is then decided on the sub leaf itself
    -- ('predExprHoldsOn'): the sub is within the super's predicate when the predicate holds of
    -- every value of its shape, or when the sub already carries the same predicate. The rest of
    -- the super's conjunction goes the way it did; one value predicate left undecided keeps the
    -- whole unproven ('False'). A negated value predicate is not split out — the formula is
    -- decided whole, as before.
    leafCtxPredsCover = superPreds /= PredsNone
      && case typeCoveredSymbolic typeEnv vaenv' ProveEmpty (singletonType sub) superTop of
           Just covered -> covered
           -- Without a value predicate the formula is asked exactly as spelled: a respelling here
           -- would change the key the recursion's cycle cuts are made on.
           Nothing | null superExprAtoms -> isJust (predsSatDNF True superPreds) && isSubtypeOfWithEnv typeEnv vaenv' (singletonType sub) superTop
                   | otherwise -> all exprAtomHolds superExprAtoms && (superRest == PredsNone || (isJust (predsSatDNF True superRest) && isSubtypeOfWithEnv typeEnv vaenv' (singletonType sub) superRestTop))
    superTop = UnionType (Just (superPreds, H.empty, [])) H.empty []
    superRestTop = UnionType (Just (superRest, H.empty, [])) H.empty []
    (superExprAtoms, superRest) = case tryPredsToList superPreds of
      Just atoms -> ([e | PredExpr e <- atoms], case [a | a <- atoms, not (isPredExpr a)] of
                                                  []   -> PredsNone
                                                  [a]  -> PredsOne a
                                                  rest -> mkAnd (map PredsOne rest))
      Nothing    -> ([], superPreds)
    isPredExpr PredExpr{} = True
    isPredExpr _          = False
    subExprAtoms = maybe [] (\atoms -> [e | PredExpr e <- atoms]) (tryPredsToList subPreds)
    exprAtomHolds e = e `elem` subExprAtoms || predExprHoldsOn typeEnv vaenv' sub e == Just True

    -- @hasAllPreds needed given@: the sub's formula @given@ implies the super's @needed@.
    hasAllPreds :: TypePredicates -> TypePredicates -> Bool
    hasAllPreds (PredsAnd needs) given = all (`hasAllPreds` given) needs
    hasAllPreds need@PredsOne{} (PredsAnd gives) = any (hasAllPreds need) gives
    hasAllPreds (PredsOne need) (PredsOne give) = predImpliesAtom typeEnv vaenv' give need
    hasAllPreds p PredsNone = p == PredsNone
    -- Formulas beyond the atom/conjunction shapes above (a 'PredsNot' anywhere — e.g. a De
    -- Morgan disjunction a ¬-containing meet attached to a leaf): decide by the general
    -- predicate-implication oracle, which handles negation soundly (conservative 'False' when
    -- unproven).
    hasAllPreds need give = predImplies typeEnv vaenv' give need

-- | What a parent reference held in a parameter slot reads, where the meet can tell: the
-- enclosing partial's binding, which that partial's meet passes down under its own key
-- ('intersectPartialsBaseN'.'deepVaenv'), or else the inner it was read into before the meet
-- ('intersectPartialsBaseN'.'readInputObjParents'). Never an ambient binding of the same name,
-- which is a different variable.
parentRefReading :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Maybe Type
parentRefReading typeEnv vaenv (TypeVar (TVVar m) TVParent i) = case H.lookup (VarKey TVInt (TVVar m)) vaenv of
  Just r | not (isTypeVar r), r /= PTopType -> Just (meetInner typeEnv vaenv r i)
  _ | i /= PTopType, not (isTypeVar i)     -> Just i
  _                                         -> Nothing
parentRefReading _ _ _ = Nothing

-- | The continuation-input half of 'isSubPartialOfWithEnvNRaw'. A higher-order argument's input
-- object (@fn(v -> $_A)@) is what the partial promises to PASS its continuation, so a parameter
-- it names reads as the DATA arguments pin it — @A@ is @m@'s element — and an occurrence there
-- never widens it. Read as its value instead, an unbound @A@ is ⊤ and every input fits: the ap
-- form @fn(v -> $_F)@ would read as within the fmap form @fn(v -> $_A)@ though, at @m= Opt[X]@,
-- one passes the @Opt@ and the other its @X@, and the meet (which pins @A= X@) then finds the
-- two disjoint, so the union absorbing one into the other erases an arm.
--
-- The inclusion must hold at every instance, so it is decided at a generic one: each of the
-- sub's data-pinned parameters still unbound is given a fresh opaque type (a skolem, a name no
-- program defines). The super is met with that instance, its continuation inputs left open,
-- which pins the super's parameters from the data exactly as a call does; those values are set
-- on the super, which otherwise keeps its own spelling (the meet respells an input's pointer as
-- its bound, which a pointer is not compared to), and the instance must lie within it.
pinnedSlotsOk :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> Bool
pinnedSlotsOk typeEnv vaenv sub super
  | not (any (pinnedInputSlot super) (H.toList (ptArgs super))) = True
  | null freePinned && null classPinned = True
  | otherwise = case intersectTypesEnv typeEnv vaenv (singletonType super) (singletonType subOpen) of
      UnionType Nothing leafs [] | pieces@(_:_) <- splitUnionType leafs ->
        let pinnedVal n = unionAllTypes typeEnv [ H.lookupDefault PTopType n (ptVars p) | p <- pieces ]
            superPinned = super{ptVars = H.mapWithKey (\n v -> if S.member n (dataPinnedVars super) then pinnedVal n else v) (ptVars super)}
        in isSubtypeOfWithEnv typeEnv vaenv (singletonType subInst) (singletonType superPinned)
      _ -> False
  where
    isHo k = not (S.null (pkArgs k))
    pinnedNames = dataPinnedNames sub
    freePinned = [ n | n <- S.toList pinnedNames, maybe True (== PTopType) (H.lookup n (ptVars sub)) ]
    -- A pinned parameter bounded by a class whose own parameter is left ⊤ (@/F=∀Fun[/T=⊤]@, where
    -- the arm does not name the element) ranges over every element too: that parameter is given a
    -- skolem of its own.
    classPinned = [ n | n <- S.toList pinnedNames, Just v <- [H.lookup n (ptVars sub)], v /= PTopType, freeClassParams v ]
    freeClassParams (UnionType (Just (PredsOne (PredClass q), _, _)) _ _) = PTopType `elem` H.elems (ptVars q)
    freeClassParams _ = False
    skolemizeClass n (UnionType (Just (PredsOne (PredClass q), negs, ncs)) pos consts) =
      UnionType (Just (PredsOne (PredClass q{ptVars = H.mapWithKey (\k v -> if v == PTopType then skolemAt (pkName n) k else v) (ptVars q)}), negs, ncs)) pos consts
    skolemizeClass _ v = v
    skolemAt base k = singletonType $ partialVal $ mkName ("/$skolem" ++ nameStr base ++ nameStr (pkName k))
    skolem n = singletonType $ partialVal $ mkName ("/$skolem" ++ nameStr (pkName n))
    subInst = sub{ptVars = flip (foldr (\n -> H.adjust (skolemizeClass n) n)) classPinned $ foldr (\n -> H.insert n (skolem n)) (ptVars sub) freePinned}
    subOpen = subInst{ptArgs = H.mapWithKey (\k v -> if isHo k then PTopType else v) (ptArgs subInst)}

-- | The names of a partial's parameters a value in one of its slots refers to: its outer
-- references, and a direct pointer (@m=$/F@). A pointer nested in a partial inside the value names
-- that partial's own parameter (@Bx(value=$/T)@), not this one's.
slotParamRefs :: Type -> S.HashSet TypeVarName
slotParamRefs t@(TypeVar (TVVar n) TVInt _) = S.insert n (typeOuterVarRefs t)
slotParamRefs t                             = typeOuterVarRefs t

-- | The parameter names a partial's DATA arguments pin: those the data arguments (the ones that
-- are not a higher-order input) name, closed over the parameters their values name in turn
-- (@m -> $F@ with @$F -> Fun[$T -> $_A]@ pins @F@ and @A@). A name may be one the partial does
-- not spell, which is the ⊤ an unspelled parameter denotes.
dataPinnedNames :: PartialType -> S.HashSet TypeVarName
dataPinnedNames p = closeOver (S.unions [ slotParamRefs v | (k, v) <- H.toList (ptArgs p), S.null (pkArgs k) ])
  where closeOver s = let s' = S.union s (S.unions [ slotParamRefs v | (n, v) <- H.toList (ptVars p), S.member n s ])
                      in if s' == s then s else closeOver s'

-- | 'dataPinnedNames' among the parameters the partial spells.
dataPinnedVars :: PartialType -> S.HashSet TypeVarName
dataPinnedVars p = S.intersection (H.keysSet (ptVars p)) (dataPinnedNames p)

-- | Whether an argument of the partial is a higher-order input that names a parameter the
-- partial's data arguments pin ('pinnedSlotsOk'): its reading depends on the data, so it cannot
-- be decided, or unioned, on its own.
pinnedInputSlot :: PartialType -> (ArgName, Type) -> Bool
pinnedInputSlot p (k, v) = not (S.null (pkArgs k)) && not (S.null (S.intersection (typeOuterVarRefs v) (dataPinnedVars p)))

-- | Whether a constant's parent partial type is covered by the given positive leafs.
constantCoveredByLeafs :: Constant -> PartialLeafs -> Bool
constantCoveredByLeafs c leafs = isSubtypeOfWithEnv emptyTypeEnv' H.empty (singletonType $ constantPartialType c) (UnionType Nothing leafsAtC [])
  where
    -- A leaf's 'PredConstEq' atoms are decided for this one constant first: @/Integer|¬PredConstEq(c)@
    -- holds every integer but @c@, which the primitive-leaf test alone cannot tell apart. Only a
    -- leaf whose formula is then TRUE at the constant — nothing left — can be shown to hold it:
    -- there is no environment here to decide a class atom or a value predicate against (the
    -- constant within one, @/Char | PredConstEq c ∧ ¬EqCO@, is how a meet spells it), and asking
    -- the empty environment to expand a class it does not know is an error, not a refusal.
    leafsAtC = joinUnionType [l{ptPreds = PredsNone} | l <- splitUnionType leafs, Just PredsNone <- [predsAtConst c (ptPreds l)]]

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

-- | A predicate formula read at one constant, every atom the constant decides decided: a constant
-- equality by the constant ('predsAtConst'), a class or relation atom by whether the constant's
-- primitive type is within it (or provably outside it), and a value predicate left as it is — the
-- constant neither satisfies nor fails it until it is evaluated. 'Nothing' when the formula is
-- false at the constant, @Just PredsNone@ when true, and otherwise the formula over what is left
-- undecided: @¬(Q ∧ ¬~pd)@ at a @Float@ is @¬Q@, since a @Float@ is no @pd@.
predsAtConstant :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Constant -> TypePredicates -> Maybe TypePredicates
predsAtConstant typeEnv vaenv c = go
  where
    cLeaf = singletonType (constantPartialType c)
    go (PredsOne (PredConstEq c')) = if c == c' then Just PredsNone else Nothing
    go p@(PredsOne PredExpr{}) = Just p
    -- A class or relation atom in the computation's own world ('constantInType' says why).
    go p@(PredsOne _)
      | isSubtypeOfWithEnv typeEnv vaenv cLeaf (TopType H.empty p) = Just PredsNone
      | isBottomType (intersectTypesEnv typeEnv vaenv cLeaf (TopType H.empty p)) = Nothing
      | otherwise = Just p
    go (PredsAnd ps) = mkAndOrNone . filter (/= PredsNone) <$> mapM go ps
    go (PredsNot p) = case go p of
      Nothing        -> Just PredsNone
      Just PredsNone -> Nothing
      Just q         -> Just (predsNot q)
    mkAndOrNone [] = PredsNone
    mkAndOrNone qs = mkAnd qs

-- | Whether a constant is PROVEN outside a type — the other polarity of 'constantInType', which
-- proves membership. A constant whose membership turns on an undecided predicate is neither:
-- reading "not proven in" as "out" excluded the constants of @⊤ | ¬Lizard@ from it, and so
-- proved @⊤ | ¬Lizard ⊆ ⊤ − {c}@ with @c@ a @Float@ the open world has not placed.
constantOutsideType :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Constant -> Type -> Bool
constantOutsideType _ _ _ PTopType = False
constantOutsideType typeEnv vaenv c (UnionType Nothing leafs cs) =
  c `notElem` cs && all leafExcludes (splitUnionType leafs)
  where
    -- A leaf of another name cannot hold the constant; one of its name holds it unless its
    -- formula is false at it.
    leafExcludes l = not (constantCoveredByLeafs c (joinUnionType [l{ptPreds = PredsNone}])) || isNothing (predsAtConstant typeEnv vaenv c (ptPreds l))
constantOutsideType typeEnv vaenv c (UnionType (Just top) posLeafs posConsts) =
  constantOutsideType typeEnv vaenv c (UnionType Nothing posLeafs posConsts) && isNothing (constantWithinTop typeEnv vaenv c top)
constantOutsideType _ _ _ _ = False

-- | Checks if a constant value is a member of a type.
-- A constant is in a type if it appears in the constant list, or its parent partial type is in the type.
constantInType :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Constant -> Type -> Bool
constantInType _ _ _ PTopType = True
constantInType _ _ c (UnionType Nothing leafs cs) = c `elem` cs || constantCoveredByLeafs c leafs
-- Just-top: c is in the type iff it is a positive member (posConsts/posLeafs), or it is in the
-- predicated universe and not excluded (by negConsts or negLeafs)
constantInType typeEnv vaenv c (UnionType (Just (preds, negLeafs, negConsts)) posLeafs posConsts) =
  c `elem` posConsts
  || constantCoveredByLeafs c posLeafs
  || ( c `notElem` negConsts
       && not (constantCoveredByLeafs c negLeafs)
       -- Decided in the WORLD of the computation around it, as every other membership is: a
       -- closed-world reading here (once the rule) proved a constant outside a value predicate
       -- the open world leaves undecided (@fluffy(this= Float)@ with @Float@ no @Animal@), so a
       -- union dropped the constant the open-world meet had just kept within the predicate.
       && isSubtypeOfWithEnv typeEnv vaenv (singletonType $ constantPartialType c) (TopType H.empty preds)
     )
constantInType _ _ _ _ = False

-- | Token-keyed memo for 'isSubtypeOfWithEnv', the same shape as the 'satGoal' and
-- 'expandClassPartialAux' memos: keyed by the env's identity token plus the behavior flags a
-- same-token env can vary in, the var env, and the two types; the table keeps the three
-- most-recently-used token generations; and a query under an in-flight sat decision
-- ('teInSatDecision') bypasses it, as the reach-query memo does.
--
-- A failing subtype check tries every way the sub could still be covered, and each way asks
-- again about the fields. Two of those ways re-ask about the same field in another spelling --
-- the field clamped to its declared type ('isSubPartialOfWithEnv'), and the sub with its class
-- tops unrolled a level ('expandedSub') -- so a check that fails at the bottom of a nested value
-- re-asks the same few questions a number of times that doubles with each level of nesting. A
-- list literal's call type is exactly that: one @fmap@ over twelve elements spent twenty seconds
-- deciding whether the call's scheme had changed ('TypeCheck.Common.eqScheme'), asking 478
-- distinct questions 66,987 times at seven. Remembered, each is asked once, and the time is
-- linear in the depth.
--
-- A re-entrant query is not served its own answer, because none is published until it has
-- been computed: it recomputes, and only a finished 'Bool' is ever stored. And only an answer
-- no cycle cut touched ('cycleCutCountRef'): a cut answers for the derivation path it happened
-- on, so an answer computed while one fired may depend on which questions were in flight above
-- it, and storing it would let the path it was first reached on decide it for every later one.
-- Traced ('teDebug') queries bypass it so the trace shows every question asked.
--
-- The key is forced to a value before it is compared or stored, as 'Semantics.objArrGraphQuery'
-- forces its answers. A key holds its question's types and var-arg env, and the env is lazy in its
-- values: 'TypeCheck.Common.setDescriptor' resolves an entry only where a check reads it, and an
-- entry can be a stored scheme still being worked out further up the stack. Kept as a thunk, a
-- later question whose key hashed alike compared against it and forced that scheme from inside
-- its own evaluation, which GHC ends as @<<loop>>@ (every page importing core, once core/char.ct
-- took Char out of core/string.ct). The questions themselves never needed those entries: with the
-- memo bypassed the same pages compile. Stored keys are now values, so a comparison forces
-- nothing that is still being worked out.
type SubtypeMemoKey = (Int, (Bool, Bool, Bool), [(VarKey, Type)], Type, Type)

{-# NOINLINE subtypeMemoRef #-}
subtypeMemoRef :: IORef [(Int, H.HashMap SubtypeMemoKey (Bool, ReadDeps))]
subtypeMemoRef = unsafePerformIO (newIORef [])

-- | Checks if one type contains another type. In set terminology, it is equivalent to subset or equal to ⊆.
isSubtypeOfWithEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Bool
isSubtypeOfWithEnv typeEnv@TypeEnv{teDebug=True} vaenv t1 t2 = isSubtypeOfWithEnvRaw typeEnv vaenv t1 t2
isSubtypeOfWithEnv _ _ _ PTopType = True
isSubtypeOfWithEnv _ _ t1 t2 | t1 == t2 = True
isSubtypeOfWithEnv typeEnv vaenv t1 t2 = case teQueryCacheKey typeEnv of
  Nothing -> isSubtypeOfWithEnvRaw typeEnv vaenv t1 t2
  Just _ | teInSatDecision typeEnv -> isSubtypeOfWithEnvRaw typeEnv vaenv t1 t2
  Just tok -> unsafePerformIO $ do
    let vaenvList = L.sortOn fst (H.toList vaenv)
    _ <- evaluate (forceType t1 `seq` forceType t2 `seq` foldr (\(_, v) acc -> forceType v `seq` acc) () vaenvList)
    let key = (tok, (teDisableCompact typeEnv, tePrgmEnv typeEnv, teArgModeProjection typeEnv), vaenvList, t1, t2)
    gens <- readIORef subtypeMemoRef
    case lookup tok gens >>= H.lookup key of
      Just (r, deps) -> noteReadDeps deps >> return r
      Nothing -> do
        cutsBefore <- readIORef cycleCutCountRef
        (r, deps) <- memoReading typeEnv $ evaluate (isSubtypeOfWithEnvRaw typeEnv vaenv t1 t2)
        cutsAfter <- readIORef cycleCutCountRef
        when (cutsAfter == cutsBefore) $
          atomicModifyIORef' subtypeMemoRef $ \gens2 ->
            let mTok = fromMaybe H.empty (lookup tok gens2)
                rest = filter ((/= tok) . fst) gens2
            in ((tok, H.insert key (r, deps) mTok) : take 2 rest, ())
        return r

isSubtypeOfWithEnvRaw :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Bool
isSubtypeOfWithEnvRaw TypeEnv{teDebug=True} _ l r | trace (printf "[SUBTYPE] %s ⊆ %s" (show l) (show r)) False = undefined
isSubtypeOfWithEnvRaw _ _ _ PTopType = True
isSubtypeOfWithEnvRaw _ _ t1 t2 | t1 == t2 = True
-- A type var resolves to its 'vaenv' binding. A var bound to itself (a self-referential
-- entry, which an unconstrained var may acquire during intersection) carries no information,
-- so treat it as 'PTopType' - exactly as an unbound var already resolves via 'vaenvLookup'.
-- Following it verbatim would re-enter this clause with identical arguments forever.
-- The occurrence reads @var ∩ inner@: on the LEFT the inner supplies a sound upper bound
-- even when the lookup is ⊤ (the point of occurrence-local knowledge); on the RIGHT both
-- conjuncts must admit t1 (@t ⊆ A∩B ⟺ t ⊆ A ∧ t ⊆ B@). A ⊤ inner reduces both to the
-- plain-variable rules via the meet's and the ⊤-rhs identity clauses.
isSubtypeOfWithEnvRaw typeEnv vaenv (TypeVar v _ i) t2 = case vaenvLookup typeEnv vaenv v of
  TypeVar v' _ _ | v' == v -> isSubtypeOfWithEnv typeEnv vaenv (meetInner typeEnv vaenv PTopType i) t2
  looked                   -> isSubtypeOfWithEnv typeEnv vaenv (intersectTypesEnv typeEnv vaenv looked i) t2
isSubtypeOfWithEnvRaw typeEnv vaenv t1 (TypeVar v _ i) = case vaenvLookup typeEnv vaenv v of
  TypeVar v' _ _ | v' == v -> isSubtypeOfWithEnv typeEnv vaenv t1 i
  looked                   -> isSubtypeOfWithEnv typeEnv vaenv t1 looked && isSubtypeOfWithEnv typeEnv vaenv t1 i
-- ⊤ ⊆ a positive union: never (open world - a fresh type name or constant kind escapes any
-- finite positive listing). A 'Just'-top rhs is NOT decided here: its subtractive component may
-- be vacuous (e.g. tautological preds ¬(A∧B) with A∧B unsatisfiable, making it ⊤), which the
-- predicate clauses below decide exactly (⊤ is itself the pure top with 'PredsNone').
isSubtypeOfWithEnvRaw _ _ PTopType (UnionType Nothing _ _) = False
-- A leaf conjoining @PredConstEq c@ is at most the one value @c@, so it is within @t2@ whenever @c@
-- is ('constantInType'): the constant within a value predicate (@/Float | PredConstEq c ∧ P@, how
-- a meet spells it) is inside the constant @c@ and inside every type holding @c@. A guard: a
-- membership not proven this way falls through to the clauses below (@… ∧ P ⊆ ⊤|P@ is proved by
-- its predicates there).
isSubtypeOfWithEnvRaw typeEnv vaenv (UnionType Nothing subLeafs []) t2
  | not (null constEqLeafs)
  , all (\c -> constantInType typeEnv vaenv c t2) constEqLeafs
  , null others || isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing (joinUnionType others) []) t2 = True
  where
    (constEqLeafs, others) = partitionEithers [ maybe (Right l) Left (leafConstEq l) | l <- splitUnionType subLeafs ]
    leafConstEq l = listToMaybe [c | PredsOne (PredConstEq c) <- predsConjuncts (ptPreds l)]
-- Mixed type on left (top + positive leafs/constants): (top ∪ pos ∪ cs) ⊆ t2 iff top ⊆ t2 ∧ (pos ∪ cs) ⊆ t2
isSubtypeOfWithEnvRaw typeEnv vaenv (UnionType (Just topData) pos cs) t2
  | not (H.null pos) || not (null cs) =
      isSubtypeOfWithEnv typeEnv vaenv (UnionType (Just topData) H.empty []) t2
      && isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing pos cs) t2
-- PosPartials ⊆ pure FORMULA top (a conjunction or negation, no exclusions): decompose the
-- formula by exact set identities instead of materializing its members —
--   @X ⊆ U_{P∧Q} ⟺ X ⊆ U_P ∧ X ⊆ U_Q@   (an intersection of supersets)
--   @X ⊆ U_{¬Q}  ⟺ X ∩ U_Q = ∅@          (complement disjointness)
-- The engine still answers first; this is the fallback outside its fragment. Materializing
-- instead would not terminate on a formula whose recursive class atom re-introduces the same
-- formula in a member's recursive slot (the tail of @every(ConsList[$T -> Eq])@-style members):
-- the expansion's internal slot subtype checks would re-pose this same subtype unchanged.
-- Decomposition bottoms out: each conjunct is smaller, and the ¬-rule's meet runs against a
-- POSITIVE atom, whose recursive slot meets unify same-class tops slot-wise ('mergePredsAnd')
-- rather than re-expanding.
-- Sound both ways: the (∧) rule is set-exact given its sub-answers; the (¬) rule's meet is
-- exact-or-wider (the mergeVarEnvVal doctrine), so a proven-∅ is truly ∅ and a nonempty
-- meet answers subtype-unproven, the safe polarity.
isSubtypeOfWithEnvRaw typeEnv vaenv t1@(UnionType Nothing subLeafs []) t2@(UnionType (Just (PredsAnd ps, (H.null -> True), [])) (H.null -> True) []) =
  fromMaybe
    (all (\p -> isSubtypeOfWithEnv typeEnv vaenv t1 (UnionType (Just (p, H.empty, [])) H.empty [])) ps)
    (typeCoveredSymbolic typeEnv vaenv ProveEmpty t1 t2)
  || positiveLeafsAllUninhabited typeEnv vaenv subLeafs
-- The meet decides emptiness by what it denotes, not how it spells it: a leaf the meet keeps
-- can be uninhabited on its own (a slot whose occurrence and binding are disjoint), which is ∅
-- as surely as a leaf the meet dropped. A union is then covered exactly when each of its leafs
-- is.
isSubtypeOfWithEnvRaw typeEnv vaenv t1@(UnionType Nothing subLeafs []) t2@(UnionType (Just (PredsNot q, (H.null -> True), [])) (H.null -> True) []) =
  fromMaybe
    (meetEmpty (intersectTypesEnv typeEnv vaenv t1 (UnionType (Just (q, H.empty, [])) H.empty [])))
    (typeCoveredSymbolic typeEnv vaenv ProveEmpty t1 t2)
  || positiveLeafsAllUninhabited typeEnv vaenv subLeafs
  where
    meetEmpty m@(UnionType Nothing mLeafs []) = isBottomType m || positiveLeafsAllUninhabited typeEnv vaenv mLeafs
    meetEmpty m                               = isBottomType m
-- PosPartials ⊆ NegPartials: filter by name.
-- filteredLeafs is always exact for names in allowedNames; hasOutside only means the type
-- has additional elements outside those names, which cannot affect subtype checking here.
-- Constants on the super side are irrelevant to a sub made only of data leaves: a constant
-- value belongs to its primitive type alone, so a data leaf (@Cons(..)@) neither reaches a
-- positive constant nor loses anything to an excluded one — @Cons ⊆ (U_C − {c}) ∪ pos@ is
-- exactly @Cons ⊆ U_C ∪ pos@. Dropping them keeps the super inside the engine's constant-free
-- fragment; the materialized expansion's leaf-wise cover cannot split a data leaf between an
-- overlapping class top and a predicated sibling (@Cons ⊆ U_C ∪ Cons|¬C@). Only a sub with no
-- primitive-typed leaf drops them; a primitive leaf (@Integer@) does hold constants and keeps
-- the full super.
isSubtypeOfWithEnvRaw typeEnv vaenv t1@(UnionType Nothing subPartials []) (UnionType (Just (preds, negLeafs, _:_)) pos cs)
  | not (any leafHoldsConstants (splitUnionType subPartials)) =
      isSubtypeOfWithEnv typeEnv vaenv t1 (UnionType (Just (preds, negLeafs, [])) pos cs)
isSubtypeOfWithEnvRaw typeEnv vaenv t1@(UnionType Nothing subPartials []) (UnionType (Just topData) pos (_:_))
  | not (any leafHoldsConstants (splitUnionType subPartials)) =
      isSubtypeOfWithEnv typeEnv vaenv t1 (UnionType (Just topData) pos [])
isSubtypeOfWithEnvRaw typeEnv vaenv t1@(UnionType Nothing subPartials []) t2@(UnionType (Just (preds2, negLeafs2, negConsts2)) _ _) =
  -- Symbolic decision (emptiness of @t1 ∧ ¬t2@ against the class graph) avoids materializing
  -- the predicate expansion; fall back to expansion outside the fragment. The expansion reads
  -- names and drops the super's excluded constants; they are put back ('excludeConstants'), since
  -- a sub reaching this clause may hold constants (the clauses above strip them only when it
  -- cannot) — a point leaf @/Char | P ∧ PredConstEq c@ is not inside @⊤ − {c}@.
  fromMaybe
    (let (filteredLeafs, _) = expandTypesWithNamesFull typeEnv vaenv t2 (H.keysSet subPartials)
     in isSubtypeOfWithEnv typeEnv vaenv t1 (excludeConstants negConsts2 (UnionType Nothing filteredLeafs [])))
    (typeCoveredSymbolic typeEnv vaenv ProveEmpty t1 t2)
  || positiveLeafsAllUninhabited typeEnv vaenv subPartials
  || leafPredsImplyTop
  || posPartCovers
  || topPartCovers
  || leafwise
  || disjointFromExcluded
  || disjunctSplit
  where
    -- A leaf refined by a DISJUNCTION — @¬(q₁ ∧ … ∧ qₙ)@, the De Morgan spelling a union of
    -- predicate tops produces (@/Char | ¬(¬P₁ ∧ ¬P₂) ∧ PredConstEq c@, the point within
    -- @P₁ ∨ P₂@) — is the union of the leaf refined by each disjunct: @X | ¬(q₁ ∧ q₂) ∧ R =
    -- X | ¬q₁ ∧ R ∪ X | ¬q₂ ∧ R@, an exact identity. Decided piecewise, a piece covered by the
    -- super's predicate top and another by its leafs are both seen, where the whole formula was
    -- proved by none of the routes above (the meet distributes a leaf over a disjunctive universe
    -- the same way, 'intersectTypesWithVarEnv'; the positive-union clause below has its own
    -- 'coveredByDisjuncts'). Last, so a leaf the routes above prove whole never pays the split;
    -- each piece carries a smaller formula, so the split bottoms out.
    disjunctSplit = any (isJust . leafDisjunction) subLs
      && isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing (joinUnionType (concatMap splitLeaf subLs)) []) t2
    subLs = splitUnionType subPartials
    splitLeaf l = case leafDisjunction l of
      Just (qs, rest) -> [ l{ptPreds = mkAnd (predsNot q : rest)} | q <- qs ]
      Nothing         -> [l]
    leafDisjunction l = listToMaybe [ (qs, pre ++ post) | (PredsNot (PredsAnd qs@(_:_:_)), pre, post) <- picks (predsConjuncts (ptPreds l)) ]
    picks xs = [ (x, take i xs, drop (i + 1) xs) | (i, x) <- L.zip [0 :: Int ..] xs ]
    -- ...and the rhs's predicate part alone likewise: it is a subset of the rhs, and a pure top
    -- is decided by the clauses built for it (a negated formula by its meet) where the mixed
    -- expansion above is not. So a sub inside @⊤|¬(~A ∧ ~B)@ stays inside when a positive leaf
    -- such as @/Integer@ is added beside that top.
    topPartCovers = case t2 of
      UnionType (Just topData) pos2 cs2 | not (H.null pos2) || not (null cs2) ->
        isSubtypeOfWithEnv typeEnv vaenv t1 (UnionType (Just topData) H.empty [])
      _ -> False
    -- Against the universe minus leafs alone the question is exactly a meet: @t1 ⊆ ⊤ − n2 ⟺
    -- t1 ∩ n2 = ∅@, and the meet is exact or wider, so an empty one is proof. The routes above
    -- compare slot by slot and do not carry a variable-valued slot through:
    -- @Cons[/T=$_c∩{X}](head=$/T)..@ against an excluded @Cons[/T=⊤−X](..)@ is disjoint by the
    -- @/T@ slot, which the meet sees.
    disjointFromExcluded = case t2 of
      UnionType (Just (PredsNone, n2, [])) pos2 [] | H.null pos2, not (H.null n2) ->
        case intersectTypesEnv typeEnv vaenv t1 (UnionType Nothing n2 []) of
          m@(UnionType Nothing mLeafs []) -> isBottomType m || positiveLeafsAllUninhabited typeEnv vaenv mLeafs
          m                               -> isBottomType m
      _ -> False
    -- The rhs's positive leafs alone, when they cover the sub, prove the cover: they are a
    -- subset of the rhs. The expansion above reads them together with the predicate universe's
    -- members, a mixture in which the positive-union rules (a slot union across leafs) no
    -- longer see the family they need.
    posPartCovers = case t2 of
      UnionType (Just _) pos2 _ | not (H.null pos2) -> isSubtypeOfWithEnv typeEnv vaenv t1 (UnionType Nothing pos2 [])
      _ -> False
    -- Each leaf of a positive union is covered on its own, and different leafs can take different
    -- routes — one by its own predicates, another by the union's positive part — which no
    -- whole-union rule above sees. A leaf whose slot holds a union is such a union too, one piece
    -- per member ('splitPartialByUnionVarArgs'): @f(x=∀A ∪ ∀B)@ is covered when each of
    -- @f(x=∀A)@ and @f(x=∀B)@ is. Only several pieces split, and a piece has no union slot left to
    -- split, so the per-piece subgoal does not re-enter this rule.
    leafwise = case concatMap splitPartialByUnionVarArgs (splitUnionType subPartials) of
      ls@(_:_:_) -> all (\l -> isSubtypeOfWithEnv typeEnv vaenv (singletonType l) t2) ls
      _          -> False
    -- A leaf whose own predicates imply the predicate universe's is inside it, when the
    -- universe's excluded leafs take nothing from it (each is disjoint from the leaf — an empty
    -- meet is a proof, the meet being exact or wider): the formula alone decides membership,
    -- with no expansion. The expansion above reads an OPEN class through its current members
    -- only, so it cannot see that @ek|~C ⊆ ~C@ for a name that is not (yet) a member; and an
    -- excluded leaf of another name carrying a value predicate (@⊤|Lizard − (True | fluffy($this))@)
    -- is outside the symbolic engine's fragment, so without this a leaf it cannot touch
    -- (@fluffy | Lizard@) went unproven.
    -- ...and its excluded constants take nothing from it either: a point leaf
    -- (@/Char | P ∧ PredConstEq c@) is not inside @⊤ − {c}@ ('constantOutsideType').
    leafPredsImplyTop =
      all (\l -> ptPreds l /= PredsNone && predImplies typeEnv vaenv (ptPreds l) preds2 && all (disjointLeafs l) (splitUnionType negLeafs2)
                 && all (\c -> constantOutsideType typeEnv vaenv c (singletonType l)) negConsts2) (splitUnionType subPartials)
    disjointLeafs l n = isBottomType (intersectTypesEnv typeEnv vaenv (singletonType l) (singletonType n))
-- PosPartials-with-constants ⊆ t2: partials check + each constant must be in t2
-- (Only for PosPartials: in NegPartials, cs are excluded constants, not members)
isSubtypeOfWithEnvRaw typeEnv vaenv (UnionType Nothing leafs cs@(_:_)) t2 =
  isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing leafs []) t2
  && all (\c -> constantInType typeEnv vaenv c t2) cs
-- Pure top ⊆ MIXED top: fold t2's positive components into t1's subtraction and recurse with a
-- pure rhs (the set identity @A ⊆ B ∪ C ⟺ A − C ⊆ B@, exact for any C): the pos2 leaf regions
-- join t1's excluded leafs and the cs2 points its excluded constants — both exactly
-- representable on a 'Just'-top. (t1 is always a pure top when both sides are 'Just'-tops:
-- a mixed left was split by the clause above.)
isSubtypeOfWithEnvRaw typeEnv vaenv (UnionType (Just (p1, n1, nc1)) (H.null -> True) []) (UnionType (Just top2) pos2 cs2)
  | not (H.null pos2) || not (null cs2) =
      isSubtypeOfWithEnv typeEnv vaenv
        (UnionType (Just (p1, H.unionWith S.union n1 pos2, L.nub (nc1 ++ cs2))) H.empty [])
        (UnionType (Just top2) H.empty [])
-- Pure top ⊆ pure top with EXCLUDED constants on the rhs: @A ⊆ B − cs ⟺ A ⊆ B ∧ ∀c∈cs. c ∉ A@
-- (exact). Stripping nc2 up front keeps the rhs inside the symbolic engine's constant-free
-- fragment for the clauses below.
isSubtypeOfWithEnvRaw typeEnv vaenv t1@(UnionType (Just _) (H.null -> True) []) (UnionType (Just (p2, n2, nc2@(_:_))) (H.null -> True) []) =
  all (\c -> constantOutsideType typeEnv vaenv c t1) nc2
  && isSubtypeOfWithEnv typeEnv vaenv t1 (TopType n2 p2)
-- Pure NegPartials-top ⊆ pure NegPartials-top, SAME predicates: (U_p − n1 − nc1) ⊆ (U_p − n2)
-- holds EXACTLY iff (U_p ∩ n2) ⊆ n1. Only the part of n2 inside the shared predicate universe
-- matters — an excluded leaf outside U_p removes nothing from either side, and one that differs
-- from an n1 leaf only in arg-mode/field-completion still excludes the same U_p members. This is
-- what makes @∀Bag − /Cons..@ and @∀Bag − /Cons[full]@ compare equal (both exclude
-- @∀Bag ∩ /Cons.. = /Cons[full]@). A point exclusion (nc1) can never cover the leaf region
-- (U_p ∩ n2) − n1 (leaf regions are not finite constant sets), so the region check ignores nc1.
-- Matched only for the fully-reduced form — no positive leafs/constants on either side (mixed
-- lefts split, mixed rights folded above) and no rhs excluded constants (stripped above) —
-- where the identity is exact.
isSubtypeOfWithEnvRaw typeEnv vaenv (UnionType (Just (p1, n1, _)) (H.null -> True) []) (UnionType (Just (p2, n2, [])) (H.null -> True) [])
  -- The cover is a positive-union question, and @n1@ may cover jointly only through a split on a
  -- predicate one of its leafs carries (@length(this=⊤)|¬~length(this=∀List)@ beside
  -- @length(this=∀List)@), so ask it with the complete relation — one excluded-middle split, its
  -- halves decided by the plain relation, so the subgoal stays a positive-leaf question.
  | p1 == p2 = isSubtypeOfWithEnvComplete typeEnv vaenv (intersectTypesEnv typeEnv vaenv (TopType H.empty p1) (UnionType Nothing n2 [])) (UnionType Nothing n1 [])
-- Pure NegPartials-top ⊆ pure NegPartials-top, DIFFERING predicates: (U_p1-n1-nc1) ⊆ (U_p2-n2).
-- Both sides are fully reduced here (mixed lefts split, mixed rights folded, rhs excluded
-- constants stripped, same-preds pairs decided, by the clauses above). The excluded constants
-- of t1 are captured by the @t1@ binding and consulted by the fallbacks below (not ignored);
-- the cheap guards decide the common cases: sufficient iff predImplies p1 p2 and n2 ⊆ n1;
-- otherwise the symbolic decision.
isSubtypeOfWithEnvRaw typeEnv vaenv t1@(UnionType (Just (p1, n1, _)) (H.null -> True) []) t2@(UnionType (Just (p2, n2, [])) (H.null -> True) [])
  | isPredsContradictory p1 = True  -- t1 = ∅ ⊆ anything
  | (isPredsContradictory (predsAnd p1 (predsNot p2)) || predImplies typeEnv vaenv p1 p2)
    && n2CoveredInP1 = True
    -- U_p1 ⊆ U_p2 (syntactically: p1 ∧ ¬p2 = ∅, or by 'predImplies') and U_p1 ∩ n2 ⊆ n1, so
    -- (U_p1 - n1 - nc1) ⊆ (U_p2 - n2). The universe implication alone is NOT sufficient:
    -- an n2 leaf inside U_p1 but not in n1 stays in t1 while t2 excludes it.
  | predsEmpty typeEnv vaenv p1 = True  -- t1 = ∅ by class-graph disjointness ⊆ anything (fallback: expansion is costly)
  -- The SUBTRACTION can be what brings t1 inside t2, with no implication between the universes
  -- at all: @(U_p1 − n1) ⊆ (U_p2 − n2)@ holds as soon as @n1@ already excludes everything
  -- @U_p1@ has OUTSIDE @U_p2@ — that residue is @U_(p1 ∧ ¬p2)@, and asking whether @n1@ covers
  -- it is a plain 'Just'-top ⊆ positive-union query.
  --
  -- The union reaches this shape by association alone: folding the same three operands the
  -- other way spells one side's negation as a @PredRel@ over the whole name and the other's as
  -- explicit excluded leafs. Both are correct, and both denote the same set once the shared
  -- positive leaf is added back, but the guard above only proves the case where @U_p1@ is
  -- already inside @U_p2@.
  --
  -- A NON-EMPTY @n2@ needs one more thing: a t1 value surviving @n1@ could otherwise land in
  -- @n2@, which the residue test does not check. It cannot when @n2 ⊆ n1@ — t1 already excludes
  -- everything t2 does — and the pair is then exact: @v ∈ U_p1 − n1@ is in @U_p2@ by the residue
  -- test and outside @n2@ by this one. So @⊤ − (N + P)@ and @(⊤|¬R) − N@, which may spell one
  -- set with the shared @N@ excluded on both sides, compare as that set.
  --
  -- Only when something IS subtracted: with an empty @n1@ the reasoning cannot apply (nothing
  -- was removed), and firing anyway would run a full subtype query on every such comparison —
  -- the overwhelmingly common shape.
  | not (H.null n1)
  , H.null n2 || n2CoveredInP1
  , isSubtypeOfWithEnv typeEnv vaenv
      (UnionType (Just (predsAnd p1 (predsNot p2), H.empty, [])) H.empty [])
      (UnionType Nothing n1 [])
  = True
  -- Symbolic decision (emptiness of @t1 ∧ ¬t2@); the engine's positive gate reproduces the
  -- 'tePrgmEnv'/'predsSealed' enumeration gate of the re-expression fallback. Decided on t1
  -- WITHOUT its excluded constants (which are outside the DNF fragment): dropping them only
  -- widens t1, so a proven cover is sound — and it is exact, because with differing predicates
  -- t2's missing set inside U_p1 is a union of name-keyed leaf regions and nc2 points (both
  -- name-level), never exactly a finite set of excluded constants, so re-adding nc1's points
  -- cannot flip an uncovered t1 to covered.
  | otherwise = fromMaybe oldReexpress
      (typeCoveredSymbolic typeEnv vaenv ProveEmpty (UnionType (Just (p1, n1, [])) H.empty []) t2)
  where
    -- Only the part of @n2@ inside @U_p1@ can exclude a t1 value, so the cover @n1@ owes is
    -- @U_p1 ∩ n2 ⊆ n1@ — the same-predicates clause's question. Asking @n2 ⊆ n1@ outright
    -- demands more: @n1 = f(x=∀A − B..)|¬~C + f(x=B..)@ covers @n2 = f(x=∀A)@ only inside @¬~C@,
    -- but with @p1 = ¬~C@ that is all of @U_p1@, so @(⊤|¬~C) − n1 ⊆ ⊤ − n2@ holds. The plain
    -- test is the cheap sufficient case and goes first.
    n2CoveredInP1 = isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing n2 []) (UnionType Nothing n1 [])
      || isSubtypeOfWithEnvComplete typeEnv vaenv (intersectTypesEnv typeEnv vaenv (TopType H.empty p1) (UnionType Nothing n2 [])) (UnionType Nothing n1 [])
    -- Re-express U_p1 - n1 as a positive type and recurse. Guarded on progress: when the predicate
    -- expands to the universe (e.g. p1 = PredsNone) the difference reproduces the same complement,
    -- which would recurse forever, so require the rewrite to differ from t1 (else conservative False).
    -- Guarded on EXACTNESS as the other expansion fallbacks are ('predsPositiveSealed'): a
    -- relation over a data name (@~#noCore@, two names in scope) is enumerated in full though no
    -- class is sealed, and a value predicate carried along is transparent.
    oldReexpress
      | tePrgmEnv typeEnv || predsPositiveSealed typeEnv p1
      , let t1' = snd $ differenceTypeWithEnv typeEnv vaenv (expandPredicates typeEnv vaenv p1) (UnionType Nothing n1 [])
      , t1' /= t1 = isSubtypeOfWithEnv typeEnv vaenv t1' t2
      | otherwise = False
-- Pure top ⊆ (PosPartials ∪ constants): the rhs constant points cannot help cover the leaf
-- regions of the predicate universe (leaf regions are not finite constant sets — the same
-- postulate as the nc1 treatment above), so decide against the positive leafs alone. Keeps
-- the rhs inside the symbolic engine's constant-free fragment for the clause below.
isSubtypeOfWithEnvRaw typeEnv vaenv t1@(UnionType (Just _) (H.null -> True) []) (UnionType Nothing superPartials (_:_)) =
  isSubtypeOfWithEnv typeEnv vaenv t1 (UnionType Nothing superPartials [])
-- NegPartials ⊆ PosPartials: filter t1 by superPartials names.
-- hasOutside=True: t1 has elements outside superPartials names.
--   True if preds are syntactically contradictory (t1 = ∅).
--   True if predImpliesPartials establishes U_preds ⊆ superPartials (negLeafs removal can only shrink).
--   Otherwise False (conservative: can't prove subtype without knowing predicate members).
isSubtypeOfWithEnvRaw typeEnv vaenv t1@(UnionType (Just (preds, negLeafs, _)) (H.null -> True) []) t2@(UnionType Nothing superPartials []) =
  -- The predicate region (U_preds - negLeafs) must be covered by t2; the excluded constants only
  -- shrink t1 further, and a positive union cannot cover all-but-points of a leaf region, so the
  -- region check without them is exact. (Positive leaves and constants of t1 are split off by the
  -- "mixed type on left" branch above.)
  predsCovered
  where
   -- Symbolic decision first (emptiness of @(U_preds − negLeafs) ∧ ¬t2@ against the class
   -- graph); the materialized chain remains as the out-of-fragment fallback. Gated open-world
   -- ('ProveEmpty', matching 'predsEmptySymbolic' and the sealed-only 'predsCoveredExpand'
   -- fallback below): an UNSEALED class in @preds@ must NOT be enumerated closed-world here, or the
   -- cover would prove @Top(Bag ∧ ¬Nil) ⊆ Cons@ (Bag's only non-Nil member so far IS a Cons) —
   -- broken by any future @isa Bag@ declaration, and inconsistent with the open-world emptiness
   -- oracle. In the closed-world coverage environment ('tePrgmEnv') the gate still enumerates.
   -- Only a symbolic PROOF short-circuits. A symbolic refusal falls through to the materialized
   -- expansion, which is a second, independent prover — not a formatting of the same answer.
   --
   -- The engine reports "not covered" by exhibiting an escape witness, and it builds that witness
   -- from the leaf-cover check alone, without the goal's own NEGATIVE atoms in hand. Against a
   -- cover leaf carrying a refinement it can therefore offer a value the goal already excludes:
   -- for @(~ConsList ∧ ¬C) ⊆ (Cons[…]|¬C) ∪ Nil@ it answers @Just False@ on the strength of
   -- "a Cons that IS a C", which the goal's own @¬C@ forbids. The expansion decides that query.
   --
   -- One prover's inability to decide must never present itself as a decision. Here the engine
   -- is IN fragment but incomplete, so the fragment boundary cannot catch it — only refusing to
   -- treat its negative as final can.
   predsCovered = case typeCoveredSymbolic typeEnv vaenv ProveEmpty (UnionType (Just (preds, negLeafs, [])) H.empty []) t2 of
     Just True                       -> True
     Just False | not expansionExact -> False
     _                               -> predsCoveredExpand
   -- A symbolic refusal is only ever INCOMPLETENESS where the expansion is entitled to decide.
   -- When the POSITIVE atoms are unsealed the refusal is the open-world gate doing real work:
   -- @∀/Bag ⊆ (/Cons + /Nil)@ must stay False however many of Bag's current members happen to be
   -- ConsLists, and enumerating Bag to "prove" it is exactly the violation this clause's gate
   -- exists to prevent.
   expansionExact = tePrgmEnv typeEnv || predsPositiveSealed typeEnv preds
   predsCoveredExpand = case expandTypesWithNamesFull typeEnv vaenv t1 (H.keysSet superPartials) of
    (_, True)      | isPredsContradictory preds -> True
    (_, True)      | predImpliesPartials typeEnv vaenv preds superPartials -> True
                   -- U_preds ⊆ superPartials implies (U_preds - negLeafs) ⊆ superPartials
    (_, True)      | predsEmpty typeEnv vaenv preds -> True  -- U_preds = ∅ by class-graph disjointness (fallback: costly)
    -- (U_preds - negLeafs) ⊆ t2  ⟺  U_preds ⊆ (t2 ∪ negLeafs)  [set identity]. Recursing isSubtypeOf
    -- on the expanded member union re-expresses the predicate as a positive type WITHOUT
    -- 'differenceTypeWithEnv', and decomposes it per-member through the positive/mixed branches: a
    -- member whose recursive element matches t2's reflexively short-circuits rather than re-expanding.
    -- Guarded on progress: when the expansion reproduces t1 (e.g. preds = PredsNone giving
    -- 'PTopType') recursing would not progress, so give a conservative False. Guarded on
    -- EXACTNESS as the symbolic refusal above is ('expansionExact'): the recursion is sound when
    -- every positive atom is enumerated in full — a data name's extent is its own values, and a
    -- value predicate is carried symbolically — while an unsealed class at negative polarity
    -- only makes the expansion wider, which a cover it still passes is sound for.
    (_, True)      | expansionExact
                   , let expanded = expandPredicates typeEnv vaenv preds
                   , expanded /= t1
                   -> isSubtypeOfWithEnv typeEnv vaenv expanded
                        (unionTypesWithEnv typeEnv vaenv t2 (UnionType Nothing negLeafs []))
    (_, True)      -> False
    -- An exact-flagged expansion exhausts the top only for positive-atom predicates
    -- ('predsPositiveAtoms'): a 'PredsNone'/negated top also contains constants of unlisted
    -- kinds and open-world values outside every declared name, so it is never inside a finite
    -- positive union (the same open-world rule as @⊤ ⊆ positive union = never@).
    (leafs, False) | predsPositiveAtoms preds -> isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing leafs []) t2
                   | otherwise -> False
-- PosPartials ⊆ PosPartials: check partials structurally
isSubtypeOfWithEnvRaw typeEnv vaenv (UnionType Nothing subPartials []) (UnionType Nothing superPartials []) =
  debugTrace typeEnv msg result
  where
    supersByName = splitUnionTypeByName superPartials
    subsByName   = splitUnionTypeByName subPartials
    -- A sub group with no same-named super can still be covered when every one of its leaves is
    -- uninhabited by declaration-bounding ('partialUninhabitedByDecl' — the empty set is a subset
    -- of anything); an inhabited leaf requires a same-named super.
    result = and (H.elems (H.mapWithKey subGroupCovered subsByName))
    subGroupCovered n subs = case H.lookup n supersByName of
      Just sups -> all (isSubPartial sups) subs
      Nothing   -> all (partialUninhabited typeEnv vaenv) subs
    isSubPartial sup sub = directCheck || coveredByArgSets || coveredBySlotUnion sub sup || coveredBySplit (splitPartialByUnionVarArgs subLinked) || coveredBySplit (splitPartialByUnionVarArgs expandedSub) || partialUninhabited typeEnv vaenv sub || coveredBySlotPartition sub sup || coveredByDisjuncts || coveredByNegPred || coveredByNegRelEscapes || coveredByOptionalSplit
      where
        directCheck = any (isSubPartialOfWithEnv typeEnv vaenv sub) sup
        -- A leaf with an optional arg ('PtArgMode') is the leaf without it (typed ∅) and the leaf
        -- requiring it, so it is covered when both are: @F(x, y?=Y)..@ is within
        -- @F(x, y?=∅).. + F(x, y=Y)..@ though within neither. Each piece has one fewer optional arg
        -- that may be present, so the split ends. Only a lower-bounded leaf's optional args
        -- ('ptArgOpt') split: a leaf without a lower bound is an interval of arg-sets, which
        -- 'coveredByArgSets' already enumerates.
        coveredByOptionalSplit = case [ k | k <- S.toList (ptArgOpt (ptArgMode sub)), not (isBottomType (H.lookupDefault PTopType k (ptArgs sub))) ] of
          k:_ -> let supT = UnionType Nothing (joinUnionType sup) []
                     absent = withOptionalArgs typeEnv (S.insert k (ptArgOpt (ptArgMode sub))) sub{ptArgs=H.insert k BottomType (ptArgs sub)}
                 in all (\piece -> isSubtypeOfWithEnv typeEnv vaenv (singletonType piece) supT) [absent, requireArg typeEnv k sub]
          []  -> False
        -- A leaf refined by a DISJUNCTION is the union of the leaf refined by each disjunct:
        -- @l|¬(q₁ ∧ … ∧ qₙ) = ⋃ᵢ l|¬qᵢ@ (De Morgan, exact), so it is covered when every such leaf
        -- is. Each is compacted first, which meets a positive relation or class atom into the
        -- leaf (@Cons|~Cons[/T=A](…)@ is @Cons[/T=A](…)@) and compares it as the plain leaf it is.
        -- Asked through the negated conjunction whole ('coveredByNegPred'), the complement of
        -- each atom is materialized into per-slot escapes, and the cover then needs a joint
        -- partition across several slots that no route finds. Each disjunct is a smaller formula,
        -- so this does not re-fire on its own subgoals indefinitely.
        --
        -- The disjunction may be one conjunct among others, which each piece keeps:
        -- @l|¬(q₁ ∧ q₂) ∧ R = l|¬q₁ ∧ R ∪ l|¬q₂ ∧ R@. The point a constant met with a union of two
        -- predicate tops gives, @/Integer | ¬(¬P ∧ ¬Q) ∧ PredConstEq c@, is covered by the two
        -- points @/Integer | P ∧ PredConstEq c@ and @/Integer | Q ∧ PredConstEq c@ that the meet
        -- distributed over the union gives ('propIntersectionDistributesUnion').
        coveredByDisjuncts = case subDisjunction of
          Just (qs, rest) ->
            let supT = UnionType Nothing (joinUnionType sup) []
            in all (\q -> isSubtypeOfWithEnv typeEnv vaenv (compactType typeEnv vaenv (singletonType sub{ptPreds = mkAnd (predsNot q : rest)})) supT) qs
          Nothing -> False
        subDisjunction = listToMaybe [ (qs, pre ++ post) | (PredsNot (PredsAnd qs@(_:_:_)), pre, post) <- conjPicks (predsConjuncts (ptPreds sub)) ]
        conjPicks xs = [ (x, take i xs, drop (i + 1) xs) | (i, x) <- L.zip [0 :: Int ..] xs ]
        -- A leaf with a NEGATED predicate is its bare shape minus the predicate's members:
        -- @l|¬q ⊆ S ⟺ l ⊆ S ∪ (l ∩ q)@, exactly. Compared as written, @l|¬q@ matches no single
        -- super when a difference has already cut @l@ into the pieces outside @q@:
        -- @Cons|¬~Cons[/T=~Integer] ⊆ Cons[/T=⊤−Integer] + …@. Only the negated form: a future
        -- @isa@ only shrinks @¬q@, so the cover stays valid in the open world, while a
        -- POSITIVE predicate's expansion is exactly the closed-world read 'predsPositiveSealed'
        -- forbids. The subgoal's sub is predicate-free, so this route does not re-fire on it.
        -- A leaf outside a relation's SAME-NAMED shape (@l|¬~n@, the spelling a difference gives
        -- @l − n@ when its pieces carry a recursive negation) IS the difference @l − n@, whose
        -- pieces are the values leaving @n@ at one of its positions ('partialEscapes'). Each piece
        -- covered covers the leaf: @Cons|¬~Cons[/T=K]@ is the lists with a head outside @K@ or a
        -- tail outside @ConsList[K]@, and a super spelling those two cases covers it though no
        -- single super leaf does. Exact under the conditions the difference itself decomposes on:
        -- the relation names no class (whose class route would widen what it excludes), and its
        -- arg-set interval covers the leaf's, which is lower-bounded.
        coveredByNegRelEscapes = case ptPreds sub of
          PredsNot (PredsOne (PredRel n))
            | ptName n == ptName sub
            , ptName n `notElem` listClassNames typeEnv
            , argModeCovers nN bareN
            , ptArgLb (ptArgMode bareN) ->
                isSubPartialOfWithEnv typeEnv vaenv bareN nN
                || all (\e -> isSubtypeOfWithEnv typeEnv vaenv (singletonType e) (UnionType Nothing (joinUnionType sup) []))
                       (partialEscapes typeEnv vaenv bareN nN)
            where bareN = normalizePtArgMode typeEnv sub{ptPreds = PredsNone}
                  nN = normalizePtArgMode typeEnv n
          _ -> False
        coveredByNegPred = case ptPreds sub of
          PredsNot q ->
            let bare = singletonType sub{ptPreds = PredsNone}
                inQ = intersectTypesEnv typeEnv vaenv bare (expandPredicates typeEnv vaenv q)
            in isSubtypeOfWithEnv typeEnv vaenv bare (unionTypesWithEnv typeEnv vaenv (UnionType Nothing (joinUnionType sup) []) inQ)
          _ -> False
        -- The split routes below take a var slot apart into pieces. A parent reference in an
        -- ARG value that names that slot (@Cons[/T=X](head=Cons[/T=$^/T])@) denotes the slot's
        -- VALUE (rule one of the register machine, 'substitutePartialTVExtScoped'), so the sub
        -- is @Cons[/T=X](head=Cons[/T=X])@ — every combination of a piece outside and a piece
        -- inside. Split with the link left symbolic, each piece would read the reference as
        -- ITSELF (@Cons[/T=p](head=Cons[/T=p])@), the diagonal — a proper subset of the sub, and
        -- a cover proved of it is no cover of the sub. So the links are read as their values
        -- before any slot is taken apart — the sub's own
        -- var slots are the scope of its arg values, the ambient env is not consulted (as
        -- 'partialCoversSat'.'resolveSelfRefs' reads them).
        subLinked = sub{ptArgs = fmap (substituteKeepScoped typeEnv ((ambientEnv $ joinVarArgEnv (ptVars sub) H.empty)) H.empty) (ptArgs sub)}
        -- A moded sub leaf IS the union of its arg-set interval's exact pieces
        -- ('ptIntervalArgKeys'); it is covered when every piece is — each piece may be
        -- covered by a DIFFERENT super leaf (e.g. @/x(a=A)??@ ⊆ @/x ∪ /x(a=A)@), a joint
        -- cover no single-leaf check sees. Only a genuinely multi-piece interval recurses
        -- (an exact leaf is its own single piece, already decided by 'directCheck').
        coveredByArgSets = case ptIntervalArgKeys typeEnv subN of
          Just kss@(_:_:_) -> all (isSubPartial sup . ptAtArgKeys subN) kss
          _                -> False
          where subN = normalizePtArgMode typeEnv sub
        -- A genuinely split sub is covered when every split component is covered by some super.
        -- STREAMED: the split of a partial with several union-valued slots is the CARTESIAN
        -- PRODUCT of the slots' members — exponential in the slot count — so it is never counted
        -- with 'length'. Two cells are matched to establish non-triviality and 'all' consumes the
        -- rest incrementally, so a failing cover stops at the first uncovered piece.
        coveredBySplit splits = case splits of
          (_:_:_) -> all (\s -> any (isSubPartialOfWithEnv typeEnv vaenv s) sup) splits
          _       -> False
        expandedSub = subLinked{ptArgs = fmap expandArg (ptArgs subLinked), ptVars = fmap expandArg (ptVars subLinked)}
          -- The global (predicate) component and the positive partials of a 'UnionType' are
          -- joined by union; the predicate does not constrain the partials. So expanding a
          -- @Just(preds)@ arg to /just/ @expand(preds) - negLeafs@ would silently drop the
          -- union-joined positive leaves (and constants), under-approximating the arg and
          -- producing a false-positive subtype (e.g. @m(⊤)@, a leaf outside @preds@, would
          -- vanish). Union the partial component back in so the expansion is faithful.
          -- The slot is a POSITIVE position of the sub: expanding its class atoms to their
          -- current members is the open-world enumeration every other re-expression here
          -- gates ('predsCoveredExpand', 'oldReexpress', 'topRewriteCover'), so an unsealed
          -- positive atom stays symbolic — a future member of @∀List@ need not be a @Bag@, so
          -- @Cons[/T=∀List] ⊆ Cons[/T=∀Bag]@ is unproven, though every CURRENT member of
          -- @List@ may be a @Bag@.
          -- A negated atom only shrinks the slot, so its expansion widens the sub and a
          -- cover proved of the wider sub still holds ('predsPositiveSealed').
          where expandArg t@(UnionType (Just (preds, negLeafs, _)) pos cs)
                  | tePrgmEnv typeEnv || predsPositiveSealed typeEnv preds =
                      unionTypesWithEnv typeEnv vaenv
                        (snd $ differenceTypeWithEnv typeEnv vaenv (expandPredicates typeEnv vaenv preds) (UnionType Nothing negLeafs []))
                        (UnionType Nothing pos cs)
                  | otherwise = t
                expandArg t = t
    coveredBySlotUnion sub sup = slotUnionCovers typeEnv vaenv (isSubtypeOfWithEnv typeEnv vaenv) sub sup
    -- A super family that PARTITIONS one slot covers the sub jointly even when its members also
    -- differ from the sub at OTHER slots, because those other slots may be REFERENCES to the
    -- partitioned one. A diagonal declaration is exactly that shape: @ord[/T=∀Ord](/this=$/T)@
    -- against its per-member definitions @ord[/T=Integer](/this=Integer)@ and
    -- @ord[/T=Float](/this=Float)@. Erasing @/T@ leaves @/this@ still differing, so
    -- 'coveredBySlotUnion' (which delimits its family by equality outside the slot) finds no
    -- family, though the difference oracle reads the same query as ∅; a declaration-coverage
    -- check must not report a declaration unfulfilled with an EMPTY undefined part.
    --
    -- Restricting the sub's slot to one super's value at it resolves the sub's own references to
    -- that slot ('readOwnScope' reads @$/T@ under the restricted @ptVars@), so each piece is
    -- decided by the ordinary sub-partial check. Exact in both directions: the supers' values at
    -- the slot cover the sub's value there, so the pieces cover the sub, and each piece is
    -- covered by the super it was cut from.
    --
    -- LAST in the chain, and only for a genuine family (two or more same-named supers): it runs a
    -- type-level union, a cover check and a meet per super, which is real work to spend on every
    -- comparison the cheaper routes already decide.
    coveredBySlotPartition sub sup
      | length sup < 2 = False
      | otherwise = any (partitionSlot True ptVars (\p m -> p{ptVars=m})) (partKeys ptVars)
                 || any (partitionSlot False ptArgs (\p m -> p{ptArgs=m})) (partKeys ptArgs)
      where
        -- The partitioned dimension may be spelled only on the super side (an unspelled
        -- parameter is ⊤ on the sub, see 'PartialType'), so the supers' keys count too.
        partKeys get = L.nub (H.keys (get sub) ++ concatMap (H.keys . get) sup)
        partitionSlot isVars get set k
          -- A variable slot has no value to cut into pieces here: it stands for whatever its
          -- binding says, which the scope-resolving routes above already read.
          | isTypeVar sv = False
          -- An UNSPELLED slot is cut only on the PARAMETER axis, where it denotes ⊤ (see
          -- 'PartialType'). An unspelled ARG is ABSENT, not ⊤: writing the key in to cut it
          -- changes the leaf's ARG-SET, and the pieces then answer for values the sub does not
          -- contain — a bare @/P@ is not covered by @/P(x=A) + /P(x=⊤−A)@, which both carry an
          -- @x@ argument it does not have. An arg SPELLED ⊤ is present, and is cut like any
          -- other value: @/w(x=⊤)|P@ is covered by @/w(x=A) + /w(x=⊤−A)|P@ piece by piece.
          | not isVars, not (H.member k (get sub)) = False
          -- A ⊤ PARAMETER slot IS cut. It carries no constraint of its own, but the supers may
          -- still partition it — @/P@ against @/P[/v=X](x=⊤)@ beside @/P[/v=⊤−X]@ — and no other
          -- route reaches that case: 'coveredBySlotUnion' delimits its family by equality OUTSIDE
          -- the slot, which these two supers fail on their args.
          --
          -- The cover need not use EVERY super: one whose value spans the slot but whose piece
          -- it does not cover (a ⊤-valued sibling constrained elsewhere) only takes part if it
          -- has to. Only the supers that cover their own piece contribute, and those values must
          -- cover the sub's — each piece is inside its super and the pieces fill the sub, so the
          -- cover is exact. So @Cons[/T=⊤](…)@ is covered by
          -- @Cons[/T=~Integer](…) + Cons[/T=⊤−Integer](…) + Cons[/T=⊤](head=⊤−Integer, …)@ through
          -- its first two leafs, though the third does not cover its own piece.
          | otherwise =
              isSubtypeOfWithEnv typeEnv env sv (unionAllTypesWithEnv typeEnv env supVals)
              && isSubtypeOfWithEnv typeEnv env sv
                   (unionAllTypesWithEnv typeEnv env [v | (supP, v) <- Prelude.zip sup supVals, pieceCovered supP])
          where
            -- Each slot value is read in the scope it sits in before it is cut, as every other
            -- slot comparison here reads it:
            --
            --   * a PARAMETER value sits at the scope containing its partial, so its outer
            --     references are read there ('substituteKeepScoped' under the incoming env, the
            --     reading 'comparedPartialsEnv' gives a var slot): @Cons[/T=$^/T]@ inside an
            --     enclosing @/T=X@ partitions the slot as @X@ does. Cut as written, the piece
            --     @v ∩ $^/T@ would be the bare reference — the meet keeps a variable symbolic and
            --     records the narrowing only in its env — which reads as all of @X@;
            --   * an ARG value sits one level inside its partial, so a parent reference in it (the
            --     declared @tail=∀ConsList[/T=$^/T]@) names THAT partial's parameter and is read
            --     through its own slots ('partialCoversSat'.'resolveSelfRefs'), and the values are
            --     compared with every parameter the partials bind shadowed ('shadowBoundParams').
            --     Read under the incoming env instead, a like-named ambient binding would capture
            --     the reference.
            --
            -- A partial-local ('TVInt') spelling names the partial itself and is kept.
            --
            -- A PARAMETER is read at its effective value ('ptVarEff'): one its declaration ties
            -- to an argument is what that argument is, spelled or not. A definition
            -- @eq(this=∀Peano)@ under @eq[$T](this -> $T)@ is @eq[/T=∀Peano](this=∀Peano)@, and so
            -- holds the @∀Peano@ piece of the declaration's @/T@; read as the ⊤ an unspelled
            -- parameter is on its own, its piece was the whole slot, which it does not cover, and
            -- a declaration every definition fulfilled was reported unfulfilled.
            slotVal p
              | isVars    = substituteKeepScoped typeEnv vaenv vaenv (ptVarEff typeEnv vaenv p k raw)
              | otherwise = substituteKeepScoped typeEnv ((ambientEnv $ joinVarArgEnv (ptVars p) H.empty)) H.empty raw
              where raw = H.lookupDefault PTopType k (get p)
            env | isVars    = vaenv
                | otherwise = shadowBoundParams typeEnv (sub : sup) vaenv
            sv = slotVal sub
            supVals = map slotVal sup
            pieceCovered supP =
              let piece = intersectTypesEnv typeEnv env sv (slotVal supP)
              in isBottomType piece
                 || isSubPartialOfWithEnv typeEnv vaenv (set sub (H.insert k piece (get sub))) supP
    msg = printf "[SUBTYPE] %s ⊆ %s = %s" (show $ UnionType Nothing subPartials []) (show $ UnionType Nothing superPartials []) (show result)
-- partials ⊆ (superLeafs ∪ constants): constants in supertype don't help cover partials
isSubtypeOfWithEnvRaw typeEnv vaenv t1@(UnionType Nothing _ []) (UnionType Nothing superLeafs (_:_)) =
  isSubtypeOfWithEnv typeEnv vaenv t1 (UnionType Nothing superLeafs [])
-- Unreachable by clause flow: every 'Just'-top pairing is decomposed above (mixed left split →
-- mixed right folded → rhs excluded constants stripped → pure pairs decided), which the
-- coverage checker cannot see through the view patterns and guards. A genuine flow hole must
-- fail loudly here rather than silently mis-decide a subtype.
isSubtypeOfWithEnvRaw _ _ t1 t2 = error $ printf "Unreachable isSubtypeOfWithEnv arguments %s ⊆ %s" (show t1) (show t2)

-- | Checks if one type contains another type. In set terminology, it is equivalent to subset or equal to ⊆.
isSubtypeOf :: TypeGraph tg => TypeEnv tg -> Type -> Type -> Bool
isSubtypeOf typeEnv = isSubtypeOfWithEnv typeEnv H.empty

-- | Checks if one type contains another type. In set terminology, it is equivalent to subset or equal to ⊆.
isSubtypePartialOf :: TypeGraph tg => TypeEnv tg -> PartialType -> Type -> Bool
isSubtypePartialOf typeEnv subPartial = isSubtypeOf typeEnv (singletonType subPartial)

-- | 'isSubtypeOfWithEnv' extended with the excluded-middle predicate case-split:
-- @X ⊆ S ⟺ X∧Q ⊆ S ∧ X∧¬Q ⊆ S@ (an exact partition — @Q ∪ ¬Q = ⊤@; the meets are
-- exact-or-wider, so proving the wider halves proves the exact ones), with @Q@ drawn from
-- the ¬-conjuncts of the super side's leafs. This proves the joint cover the per-leaf
-- chain cannot see when the partition halves live at DIFFERENT structural levels: a class
-- membership pushed INTO the slots on one super (@Cons[T=∀EqC](tail=∀ConsList[EqC])@ —
-- elementwise membership) beside its complement kept as a LEAF predicate on another
-- (@Cons[T=⊤]{¬EqC}@) — exactly the shape @a∩(¬Q ∪ ∀Q)@ distributes to. A SEPARATE entry
-- rather than a clause of 'isSubtypeOfWithEnv' itself: the split runs meets, and a
-- meet-invoking rule inside the universal cover chain would re-enter through every nested
-- compaction without bound. Consumers are the EQUALITY-level
-- checks ('isEqTypeWithEnv'), whose internal meets here use only the plain machinery, so
-- termination is structural; re-splitting on a @Q@ the sub half already carries is
-- blocked, bounding the recursion by the super's finite ¬-conjunct set.
isSubtypeOfWithEnvComplete :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Bool
isSubtypeOfWithEnvComplete typeEnv vaenv t1 t2 = isSubtypeOfWithEnv typeEnv vaenv t1 t2 || predSplitCover
  where
    predSplitCover = case (t1, t2) of
      -- Just-top sub vs a positive union: rewrite the top into its EXACT positive
      -- materialization and recurse — the shape one association order of a meet keeps
      -- symbolic (@⊤ | ¬EqC ∧ ~ConsList@) while the other materializes
      -- (@Cons{¬EqC}@, Nil dropped). The positive rel\/class conjuncts expand exhaustively
      -- when sealed (or under 'tePrgmEnv'); the ¬-conjuncts are then reduced per-member by
      -- the MEET (whose exact negLeaf reduction drops an uninhabited member like
      -- @Nil ∧ ¬EqC@, which a per-leaf uninhabitedness proof cannot). The recursion lands
      -- on a positive union, where the excluded-middle split below applies — and only
      -- there (a non-positive rewrite answers cover-unproven), so the top-rewrite runs at
      -- most once per query.
      (UnionType (Just (preds, negLeafs, [])) (H.null -> True) [], UnionType Nothing _ [])
        | H.null negLeafs -> topRewriteCover preds
      -- Constants on either side: each sub constant is decided by membership, and the data
      -- leafs are split without them — a constant never covers a data leaf nor lands in one,
      -- unless a sub leaf is a primitive type holding constants (then the plain answer stands).
      (UnionType Nothing subPartials subCs, UnionType Nothing superPartials supCs)
        | not (null subCs && null supCs)
        , not (any leafHoldsConstants (splitUnionType subPartials)) ->
            all (\c -> constantInType typeEnv vaenv c t2) subCs
            && (H.null subPartials
                || isSubtypeOfWithEnvComplete typeEnv vaenv (UnionType Nothing subPartials []) (UnionType Nothing superPartials []))
      (UnionType Nothing subPartials [], UnionType Nothing superPartials []) ->
        all leafCovered (splitUnionType subPartials)
        where
          supT = UnionType Nothing superPartials []
          -- A candidate is the WHOLE formula under a ¬-conjunct (an atom or a conjunction —
          -- the split law holds for any predicate formula Q).
          candidates = L.nub [inner | supP <- splitUnionType superPartials
                                    , PredsNot inner <- conjList (ptPreds supP)]
          leafCovered sub = isSubtypeOfWithEnv typeEnv vaenv (singletonType sub) supT
                            || any (splitOnPred sub) candidates
          -- ONE split level, halves checked with the PLAIN subtype: recursing the complete
          -- relation on a half diverges — @subQ ∧ Q@ meets a recursive conditional member
          -- (@ConsList[T→EqC]@) one level deeper each round, so the spelling never reaches
          -- a fixpoint and the progress guard never trips. A deeper nested partition stays
          -- cover-unproven, the safe polarity.
          splitOnPred sub q
            | q `elem` subConjs || PredsNot q `elem` subConjs = False
            | otherwise =
                let subT'   = singletonType sub
                    subQ    = snd $ intersectTypesWithVarEnv typeEnv vaenv subT' (qTop q)
                    subNotQ = snd $ intersectTypesWithVarEnv typeEnv vaenv subT' (qTop (PredsNot q))
                in subQ /= subT' && subNotQ /= subT'
                   && isSubtypeOfWithEnv typeEnv vaenv subQ t2
                   && isSubtypeOfWithEnv typeEnv vaenv subNotQ t2
            where subConjs = conjList (ptPreds sub)
      _ -> False
    -- Fuel-bounded: each rewrite step peels one symbolic layer (@~C@ expands to the class
    -- top @∀C@, which expands to the member union), so a small fuel covers the real chains
    -- while any pathological formula loop answers cover-unproven.
    topRewriteCover = go (3 :: Int)
      where
        go fuel preds
          | fuel <= 0 = False
          | otherwise =
              let conjs = conjList preds
                  posAtoms = [q | q@(PredsOne PredClass{}) <- conjs] ++ [q | q@(PredsOne PredRel{}) <- conjs]
                  notFs    = [q | q@PredsNot{} <- conjs]
                  others   = length conjs - length posAtoms - length notFs
                  posConj  = mkAnd posAtoms
                  expanded = expandPredicates typeEnv vaenv posConj
                  restricted = case notFs of
                    [] -> expanded
                    _  -> snd $ intersectTypesWithVarEnv typeEnv vaenv expanded (qTop (mkAnd notFs))
              in others == 0 && not (null posAtoms)
                 && (tePrgmEnv typeEnv || predsSealed typeEnv posConj)
                 && cover fuel restricted
        cover fuel t = case t of
          BottomType -> True
          UnionType Nothing _ [] -> isSubtypeOfWithEnvComplete typeEnv vaenv t t2
          -- A pure top again (the ¬-conjuncts from the meet ride along in the formula):
          -- peel another layer.
          UnionType (Just (p', nl, [])) (H.null -> True) [] | H.null nl -> go (fuel - 1) p'
          -- Mixed: the positive part and the top part must each cover.
          UnionType (Just (p', nl, [])) pos [] | H.null nl ->
            isSubtypeOfWithEnvComplete typeEnv vaenv (UnionType Nothing pos []) t2
              && go (fuel - 1) p'
          _ -> False
    conjList (PredsAnd ps) = concatMap conjList ps
    conjList q             = [q]
    qTop t = UnionType (Just (t, H.empty, [])) H.empty []

isEqType :: TypeGraph tg => TypeEnv tg -> Type -> Type -> Bool
isEqType TypeEnv{teDebug=True} a b | trace (printf "[EQ] %s = %s" (show a) (show b)) False = undefined
isEqType _ a b | a == b = True
isEqType typeEnv a b = isSubtypeOf typeEnv a b && isSubtypeOf typeEnv b a

-- | The compiler's own equality: fast and sound but INCOMPLETE (per-leaf covers only — a
-- False may mean "unproven"). Used inside the algebra's own recursion (compaction,
-- 'eqScheme' convergence), where a miss costs a respelling or an epoch, never wrongness.
-- Set-semantic equality ASSERTIONS (the algebra laws) must use 'isEqTypeWithEnvComplete'.
isEqTypeWithEnv :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Bool
isEqTypeWithEnv _ _ a b | a == b = True
isEqTypeWithEnv typeEnv vaenv a b = isSubtypeOfWithEnv typeEnv vaenv a b && isSubtypeOfWithEnv typeEnv vaenv b a

-- | Set-semantic equality via 'isSubtypeOfWithEnvComplete' — the specification-level
-- relation, complete for the partition covers the plain checker cannot see. NOT for use
-- inside the algebra's own meets/compaction (the split's meets would re-enter it).
isEqTypeWithEnvComplete :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Bool
isEqTypeWithEnvComplete _ _ a b | a == b = True
isEqTypeWithEnvComplete typeEnv vaenv a b = isSubtypeOfWithEnvComplete typeEnv vaenv a b && isSubtypeOfWithEnvComplete typeEnv vaenv b a

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

                                        -- | If the intersection of the partial with its preds fully overlaps, then the preds are redundant
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
compactType :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
compactType TypeEnv{teDisableCompact=True} _ t = t
-- Idempotence fast paths for already-canonical trivial types. 'compactType' is provably the
-- identity on each of these, yet the set algebra re-compacts them constantly — most calls are
-- on types with at most one leaf, many on the empty 'BottomType'. Short-circuiting here avoids
-- rebuilding the same structure through the whole pipeline.
--   * 'BottomType' (empty Nothing-top): 'compactPartialLeafs' on empty is empty, 'foldRecursiveType' leaves it.
--   * a single predicate-free partial with no args/vars: nothing to filter, join, overlap, or recurse into.
compactType _ _ t@BottomType = t
compactType _ _ t@(UnionType Nothing pos [])
  | [PartialArgsOption vars args PredsNone _] <- concatMap S.toList (H.elems pos)
  , H.null vars
  , H.null args = t
-- Canonical scope form: a NESTED external reference not bound by the ambient env denotes ⊤ —
-- its scope has exited ('dropOutOfScopeTVExt'). Normalizing here, in the shared canonicalizer,
-- makes every op's result carry the SAME spelling for the same set: an expansion-materialized
-- @[/T=⊤]@ and a difference-side symbolic @[/T=$_/T]@ otherwise denote equal sets that
-- 'isEqType' cannot join, breaking the intersect/difference laws. Guarded by a cheap scan;
-- the rewrite's output has no such references, so this clause re-enters at most once.
compactType typeEnv vaenv t@UnionType{}
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
compactType typeEnv vaenv (UnionType (Just (preds, _, _)) pos cs)
  | isPredsContradictory preds || predsEmpty typeEnv vaenv preds = compactType typeEnv vaenv (UnionType Nothing pos cs)
-- Class-graph unsatisfiable top: a negated conjunct implied by the rest (@p ∧ ¬q = ∅@ when @p ⟹ q@),
-- e.g. @PredClass C[/T=a] ∧ ¬PredClass C[/T=b]@ with @a ⊆ b@. The syntactic 'isPredsContradictory'
-- above only sees identical-atom @X ∧ ¬X@; this uses 'predImplies' to also reduce a covariant
-- parameterized-class pair — the difference @C[a] − C[b]@ when @a ⊆ b@ (which otherwise leaves an
-- unreduced @C[a] ∧ ¬C[b]@ residue). Gated on a present negation so the common all-positive
-- conjunctions never pay the 'predImplies' pass.
compactType typeEnv vaenv (UnionType (Just (PredsAnd ps, _, _)) pos cs)
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
compactType typeEnv vaenv (UnionType (Just (PredsAnd ps, negPartials, ncs)) posLeafs cs)
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
compactType typeEnv vaenv (UnionType (Just (preds@PredsAnd{}, negPartials, ncs)) posLeafs cs)
  | preds' <- mergeSameFormPredExprs preds
  , preds' /= preds =
      compactType typeEnv vaenv $ UnionType (Just (preds', negPartials, ncs)) posLeafs cs
-- Pure ¬(single relation) top: re-express as the universe minus the relation's expansion.
-- Matched on exactly this shape (no catch-all) so other pure-top predicates flow on to the
-- ¬(unsatisfiable) and PredsAnd-simplification clauses below.
compactType typeEnv vaenv t@(UnionType (Just (PredsNot (PredsOne (PredRel rp)), negPartials, [])) posLeafs [])
  | H.null negPartials && H.null posLeafs = case expandRelPartial typeEnv vaenv rp of
      (UnionType Nothing rp' []) -> UnionType (Just (PredsNone, rp', [])) H.empty []
      _               -> t
-- An excluded leaf that is ONE CONSTANT within value predicates the top itself states (how a meet
-- spells a constant met with the top, @/Integer | PredConstEq c ∧ P@ under @⊤ | P@ —
-- 'intersectTypesWithVarEnv') excludes exactly the point @c@: it is spelled as the excluded
-- constant, so @⊤|P − {c}@ and @⊤|P − (/Integer | PredConstEq c ∧ P)@ are one type. A residual
-- the top does not state (a predicate of its own) keeps the leaf: it excludes @c@ only where that
-- holds.
compactType typeEnv vaenv (UnionType (Just (preds, negLeafs, negConsts)) posLeafs cs)
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
compactType typeEnv vaenv t@(UnionType (Just (preds, negPartials, negConsts)) posLeafs [])
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
compactType typeEnv vaenv (UnionType (Just (PredsNot p, negLeafs, ncs)) posLeafs cs)
  | isPredsContradictory p || predsEmpty typeEnv vaenv p = compactType typeEnv vaenv (UnionType (Just (PredsNone, negLeafs, ncs)) posLeafs cs)
-- Cancel pos/neg overlap: an excluded region the positive side re-adds is a vacuous exclusion
-- ((T − neg' − C) ∪ pos' ∪ C = (T − neg') ∪ pos' ∪ C), so the common part drops from the NEG
-- side unconditionally. Its positive listing drops only when the reduced top already contains
-- it (e.g. (U − {X}) ∪ {X} = U for X inside U); a piece outside the predicate universe must
-- survive positively (dropping both sides would LOSE it: (U¬A − A) ∪ A = U¬A ∪ A, not U¬A).
compactType typeEnv vaenv (UnionType (Just (preds, negLeafs, ncs)) posLeafs cs)
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
compactType typeEnv vaenv (UnionType (Just (preds, negLeafs, negConsts)) posLeafs cs)
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
compactType typeEnv vaenv (UnionType (Just (preds, negLeafs, negConsts)) posLeafs posConsts)
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
compactType typeEnv vaenv (UnionType (Just (PredsAnd ps, negPartials, ncs)) posLeafs cs)
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
compactType typeEnv vaenv (UnionType (Just (PredsAnd ps, negPartials, ncs)) posLeafs cs)
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
-- PredsAnd: drop a conjunct implied by a sibling (@p ∧ q = p@ when @p ⟹ q@), so a disjunction
-- conjunct absorbed by a stronger sibling collapses - e.g. with @Ord isa Eq@,
-- @PredClass Eq ∧ (PredRel Eq ∨ PredRel Ord) → PredClass Eq@ (the element bound a parameterized
-- type accumulates when its constructor is a member of two related classes). Gated to conjunctions
-- containing a 'PredsNot' (the disjunction/complement form) so the common atom-only conjunctions
-- never pay the pairwise 'predImplies'.
compactType typeEnv vaenv (UnionType (Just (PredsAnd ps, negPartials, ncs)) posLeafs cs)
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
compactType typeEnv vaenv (UnionType (Just (preds, (H.null -> True), [])) pos cs)
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
compactType typeEnv vaenv (UnionType (Just topData@(preds, _, _)) pos cs)
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
compactType typeEnv vaenv (UnionType (Just topData) pos cs)
  | (pos', eqCs@(_:_)) <- compactConstEqLeafs typeEnv vaenv cs pos = compactType typeEnv vaenv (UnionType (Just topData) pos' (L.nub (cs ++ eqCs)))
  | (pos', []) <- compactConstEqLeafs typeEnv vaenv cs pos, pos' /= pos = compactType typeEnv vaenv (UnionType (Just topData) pos' cs)
compactType typeEnv vaenv (UnionType (Just topData) pos cs)
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
compactType _ _ t@(UnionType (Just _) _ _) = t
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
compactType _ _ t@(TypeVar _ _ PTopType) = t
compactType typeEnv vaenv (TypeVar v loc i) = case compactType typeEnv vaenv i of
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
compactType typeEnv vaenv (UnionType Nothing partials0 consts0) =
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
compactSettleMerged typeEnv vaenv leafs =
  let merged = compactPartialLeafs typeEnv vaenv leafs
      before = S.fromList (splitUnionType leafs)
      (fresh, kept) = L.partition (not . (`S.member` before)) (splitUnionType merged)
      fresh' = map (compactPartialArgs typeEnv vaenv) fresh
  in if fresh' == fresh then merged else compactSettleMerged typeEnv vaenv (joinUnionType (kept ++ fresh'))

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

unionTypes :: TypeGraph tg => TypeEnv tg -> Type -> Type -> Type
unionTypes typeEnv = unionTypesWithEnv typeEnv H.empty

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

-- | Takes the 'intersectTypes' of many types
intersectAllTypes :: (TypeGraph tg, Foldable f) => TypeEnv tg -> f Type -> Type
intersectAllTypes _ types | null types = BottomType
intersectAllTypes typeEnv types = foldr1 (intersectTypes typeEnv) types

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

-- | Takes the intersection of two 'Type' (∩).
intersectTypes :: (TypeGraph tg) => TypeEnv tg -> Type -> Type -> Type
intersectTypes typeEnv = intersectTypesEnv typeEnv H.empty

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

-- | The env-dropping difference: the same resolve-the-meet-back reading as 'intersectTypesEnv'
-- (@$a − X@ discovers @$a ↦ ¬X@, which the standalone result must carry rather than the bare var).
differenceTypeEnv :: (TypeGraph tg) => TypeEnv tg -> Type -> Type -> Type
differenceTypeEnv typeEnv t1 t2 = intersectTypesEnv typeEnv H.empty t1 (complementTypeEnv typeEnv H.empty t2)

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

-- |
-- Combines two 'TypeVarEnv' to form the one applying the knowledge from both
-- It takes the union of all variables from either, and shared variables combine knowledge by intersection
mergeVarEnvs :: (TypeGraph tg, Eq k, Hashable k) => TypeEnv tg -> H.HashMap k Type -> H.HashMap k Type -> H.HashMap k Type
mergeVarEnvs typeEnv = H.unionWith mergeVarEnvVal
  where
    -- Intersecting @a ∩ b@ where one side is a concrete type and the other an unbound
    -- external var returns the var (recording the binding in the returned env), so plain
    -- 'intersectTypes' - which discards that env - would keep the wider var-form and lose the
    -- concrete. Apply the intersection's own bindings back to the meet so the resolved
    -- (narrower) type is what the merged env stores. Those bindings are ALL that is applied: no
    -- ambient env is in view here, so a reference they do not bind is unknown rather than the ⊤
    -- an ambient miss denotes, and it is kept ('substituteKeep'), so an env value naming another
    -- env entry (@c ↦ Pair[/B=$_a]..@) keeps its @$_a@ across merges.
    mergeVarEnvVal a b = let (venv, t) = intersectTypesWithVarEnv typeEnv H.empty a b
                         in substituteKeep typeEnv venv t

-- | Applies 'mergeVarEnvs' to many 'TypeVarEnv'
mergeAllVarEnvs :: (TypeGraph tg, Foldable f, Eq k, Hashable k) => TypeEnv tg -> f (H.HashMap k Type) -> H.HashMap k Type
mergeAllVarEnvs typeEnv = foldr (mergeVarEnvs typeEnv) H.empty

-- | Meets a resolved variable value with an occurrence's INNER (the @var ∩ inner@ reading).
-- The ⊤ short-circuits make a knowledge-free occurrence (@inner = PTopType@) read exactly as
-- the bare variable. The general clause needs the REAL 'TypeEnv': a class-predicate inner
-- (@∀Bag@) cannot be met by a class-graph-blind meet ("Unknown class … in expandPartial"), so
-- the whole substitute\/scope family threads the environment through to here.
meetInner :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Type
meetInner _ _ b PTopType    = b
meetInner _ _ PTopType i    = i
meetInner typeEnv vaenv b i = intersectTypesEnv typeEnv vaenv b i

-- | Replaces type variables in a 'Type' using the provided 'TypeVarArgEnv'.
-- 'TVVar' and 'TVArg' are handled uniformly via 'vaenvLookup'.  The 'TVInt'/'TVExt' axis
-- is reflected in the descent logic, not the leaf rule: descending into a partial pushes
-- the partial's own bindings onto the env, so a 'TVInt' inside the partial's components
-- resolves to a sibling's bound value, and a 'TVExt' resolves to whatever the caller
-- supplied (and that the partial's bindings did not shadow).
substituteTVExt :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
-- The top-level scope is EMPTY: a type read here may be a slot value pulled out of its partial
-- (@Cons@'s @tail=∀ConsList[/T=$^/T]@ on its own), whose parent is that partial and not the
-- object the ambient env registers. Seeding the scope with the ambient env instead would read
-- such a reference by NAME against the frame: under an @equal[$T]@ frame, a list member
-- template's @/T@ would become the list type itself. A parent reference at the top stays
-- symbolic; only the keep walker below reads the predicate of a top union as the object's
-- fragment.
substituteTVExt typeEnv = substituteTVExtScoped typeEnv H.empty

-- | 'substituteTVExt' carrying the SCOPE layer explicitly: the bindings of the partial a
-- reference sits one level inside.
--
-- A declaration reaches an enclosing parameter STEP BY STEP, one level per hop, each mediated by
-- the partial between:
--
-- > class(ConsList[$T], [Cons[$T -> $_T](head -> $T, tail -> ConsList[$T -> $^T]), Nil])
--
-- @Cons@'s own @/T@ comes from the class; @head@'s @$T@ names @Cons@'s @/T@ from @Cons@'s own arg
-- position; and the nested @ConsList@'s @/T@ names @CONS@'s @/T@ — not the class's again. So at a
-- position inside a partial's arguments, a 'TVInt' written there and a 'TVParent' one level
-- further in BOTH name that partial, and both resolve against this layer.
--
-- The layer is REPLACED at each argument boundary, never unioned: descending into a partial's
-- args makes that partial's bindings the scope, and its 'ptVars' VALUES sit at its own level and
-- keep the layer they arrived with. The AMBIENT environment is threaded through untouched and is
-- the only thing a 'TVExt' consults.
--
-- Resolving a 'TVParent' against the ambient environment instead — against the rule stated at
-- 'TypeVarLoc' — would FLATTEN the chain: the hop that names @Cons@'s @/T@ would resolve to the
-- class's value directly, so @tail=∀ConsList[/T=$^/T]@ would become @tail=∀ConsList[/T=Integer]@
-- and the reference would stop tracking the parameter it was written to follow.
substituteTVExtScoped :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypeVarArgEnv -> Type -> Type
substituteTVExtScoped typeEnv scope vaenv (UnionType Nothing partials []) =
  UnionType Nothing (joinUnionType $ map (substitutePartialTVExtScoped typeEnv scope vaenv) $ splitUnionType partials) []
-- Also substitute inside the predicate ('Just') component: a class/rel predicate such as
-- @PredClass ConsList[/T=$_B]@ carries external vars that must resolve the same way as those in
-- the positive leaves (e.g. an arrow whose result type is an abstract-class type like
-- @ConsList[$T -> $_B]@ needs @$_B@ bound from the matched argument, or the result stays
-- abstract). Only vars that are actually bound in @vaenv@ are replaced; an external var absent
-- from the env is *kept* (not dropped to Top), so a recursive predicate like
-- @PredClass ConsList[/T=$_T]@ stays linked.
-- The POSITIVE leaves beside the predicate are leaves like any other and take the plain-union
-- substitution: a definition arm's result presented at a call site is this mixed form
-- (@∀Map[..] ∪ Node[..](right=∀Tree[/K=$^/MK, ..])@ — the declared result beside the
-- constructed value), and leaving its leaves unsubstituted would carry the callee's own
-- parameter spellings into the caller, where they read as ⊤ and the value no longer proves a
-- member of its own class.
-- The predicate partials follow the DESCENT's scope, like the positive leaves beside them. A
-- class atom inside an ARG value names the enclosing partial's parameters, so reading it
-- through the ambient env would resolve its parent reference by NAME: the declared
-- @tail=∀ConsList[/T=$^/T]@ of a @Cons[/T=X]@ nested in a field would read as @∀ConsList[/T=D]@
-- for whatever the ambient env happens to bind @/T@ to.
--
-- Reading a recursive declaration this exactly has a consequence for bounds derived from it. A
-- recursive data field is declared over a SELF-LINK to its partial's own element
-- (@tail -> ConsList[$T -> $_T]@), so a value such as @Cons[/T=⊤](head=$/T, tail=∀ConsList[/T=$_/T])@
-- survives this read verbatim rather than generalising to the member shape. A call-site bound
-- built from a lambda's current result — @foldEntries(fn(acc -> $_B, …) -> $_B, init -> $_B)@
-- binds the accumulator from the lambda whose RESULT is that same variable — is then a
-- self-referential equation, and publishing it as the next input bound would add one
-- constructor level per epoch. A lambda's parameter is now typed by the call's own requirement on
-- its @fn@ slot, which the object-universe bound closes at the class ('relaxRecursiveSelfArgs').
substituteTVExtScoped typeEnv scope vaenv (UnionType (Just (preds, negLeafs, negConsts)) posLeafs consts) =
  UnionType (Just (predsSub preds, negLeafs, negConsts))
            (joinUnionType $ map (substitutePartialTVExtScoped typeEnv scope vaenv) $ splitUnionType posLeafs) consts
  where
    -- A DESCENT supplies the enclosing partial's bindings, and the predicate partials are one
    -- level inside it exactly as the positive leaves beside them are: read them under that
    -- scope. At the walker's own ENTRY the scope is empty and the fragment rule applies
    -- instead — the fragment's parent is the object, whose variables the ambient env registers
    -- ('substitutePartialKeep').
    predsSub | H.null scope = mapTypePreds (mapTypePred (substitutePartialKeep typeEnv vaenv))
             | otherwise    = mapTypePreds (mapTypePred (substitutePartialKeepScoped typeEnv scope vaenv))
substituteTVExtScoped typeEnv scope vaenv (TypeVar (TVApplied b overrides) loc i) =
  applyTypeVarOverrides typeEnv (fmap (substituteTVExtScoped typeEnv scope vaenv) overrides) (substituteTVExtScoped typeEnv scope vaenv (TypeVar b loc i))
-- The occurrence reads @var ∩ inner@: an env miss (⊤ lookup) therefore resolves to the
-- (substituted) inner rather than ⊤.
substituteTVExtScoped typeEnv scope vaenv (TypeVar v TVExt i) =
  meetInner typeEnv vaenv (vaenvLookup typeEnv vaenv v) (substituteTVExtScoped typeEnv scope vaenv i)
-- A 'TVParent' consults the SCOPE layer and nothing else — never the AMBIENT environment, which
-- is a different scope and would resolve the hop to whatever binds the name there (the flattening).
-- A miss leaves it SYMBOLIC: the layer is supplied by the descent, so its absence means this
-- traversal did not carry the parent rather than that the reference denotes ⊤, and erasing on that
-- basis loses a bound the reference was tracking (@Node[/K=$^/MK]@ under @[/MK=∀Ord]@ reading
-- @/K=⊤@). A caller that genuinely reads a value DETACHED from its parent says so by detaching it
-- first ('detachParentRefs'); it is the caller that knows, not the leaf.
--
-- A binding that is ITSELF a parent reference (@Node[/K=$^/FK]@'s @/K@, the enclosing
-- declaration's parameter one hop out) is spelled at the scope partial's level; the position being
-- substituted is one level further in, where that spelling names the scope partial instead. The
-- reference has no spelling there other than the hop through the scope partial's own parameter,
-- so the hop is kept ('keepsHop'): @key=$/K@ stays the pointer rather than flattening to
-- @key=$^/FK@, which would name an @/FK@ of @Node@ — a parameter @Node@ does not have.
substituteTVExtScoped typeEnv scope vaenv (TypeVar v loc@TVParent i) = case vaenvLookupAt typeEnv scope TVParent v of
  PTopType -> TypeVar v loc i'
  b | keepsHop b -> TypeVar v loc i'
    | otherwise  -> meetInner typeEnv vaenv b i'
  where i' = substituteTVExtScoped typeEnv scope vaenv i
-- A 'TVInt' names the partial whose slot it directly is, which is this layer. At the TOP level
-- there is no such partial: a reference that reaches there was introduced as a fresh sentinel by
-- an upstream caller ('TypeCheck.Encode' spells an arg link that way), so it falls back to the
-- ambient environment that registered it. That fallback is for a name the layer has NO entry for:
-- a parameter the layer declares but binds to ⊤ (@Some[/T=⊤](value=$/T)@) is declared, and its
-- pointer erases to the inner it reads @var ∩ inner@ with — never through the ambient env, whose
-- like-named entry is another scope's variable (read that way, @Some[/T=⊤](value=$/T)@ under an
-- env binding @/T@ to ∅ would be judged uninhabited).
substituteTVExtScoped typeEnv scope vaenv (TypeVar v loc@TVInt i) = case envLookupAt scope TVInt v of
  Nothing -> meetInner typeEnv vaenv (vaenvLookup typeEnv vaenv v) i'
  Just b | b == PTopType -> meetInner typeEnv vaenv PTopType i'
         | keepsHop b    -> TypeVar v loc i'
         | otherwise     -> meetInner typeEnv vaenv b i'
  where i' = substituteTVExtScoped typeEnv scope vaenv i
substituteTVExtScoped _typeEnv _ _ t = t

-- | Whether a scope binding must be read THROUGH the reference that names it rather than
-- substituted for it: it carries a parent reference at its own level, which has no spelling one
-- level further in (see 'substituteTVExtScoped').
keepsHop :: Type -> Bool
keepsHop b = detachLevelParentRefs b /= b

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

substitutePartialTVExtScoped :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypeVarArgEnv -> PartialType -> PartialType
substitutePartialTVExtScoped typeEnv scope vaenv partial@PartialType{ptVars, ptArgs, ptPreds} = partial{
    ptVars  = ptVars',
    ptArgs  = fmap (substituteTVExtScoped typeEnv (ambientEnv $ innerScope) vaenv) ptArgs,
    ptPreds = mapTypePreds (mapTypePred (substitutePartialTVExtScoped typeEnv (ambientEnv $ innerScope) vaenv)) ptPreds
    }
  where
    -- The partial's own values sit at ITS level, so they keep the scope they arrived with.
    ptVars' = fmap (substituteTVExtScoped typeEnv scope vaenv) ptVars
    -- Its arguments are one level in, where its own bindings are the scope. REPLACED, not unioned:
    -- the layer names exactly one partial, and the hop that reaches further out was already taken.
    -- The layer is every parameter the partial BINDS, an unspelled declared one at the ⊤ it
    -- denotes ('declaredParamsAtTop'): a @Cons..@ spells no @/T@ yet its @tail=∀ConsList[/T=$^/T]@
    -- names Cons's own, and an empty layer would read as the walker's entry, where the fragment
    -- rule resolves the reference against a like-named ambient binding.
    innerScope = joinVarArgEnv (H.union ptVars' (declaredParamsAtTop typeEnv partial)) H.empty

-- | Substitute vars within a predicate partial, but *only* when the env resolves an external
-- ('TVExt') var to a concrete (non-Top) type. An external var absent from the env — or bound only
-- to Top — is kept, so a recursive predicate like @PredClass ConsList[/T=$_T]@ stays linked rather
-- than widening to Top. Internal ('TVInt') vars are always kept: they name the partial's own
-- parameters and resolve through its @ptVars@, not the outer scope.
substitutePartialKeep :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType
-- A partial read here is the object's fragment (a predicate of a point's top union, above all),
-- so its own var values' parent is the object: the ambient env, as for 'substituteKeep'. Seeded
-- with ∅ instead, a predicate such as @∀ConsList[/T=$^/T]@ would stay symbolic where the object
-- binds @/T@.
substitutePartialKeep typeEnv vaenv = substitutePartialKeepScoped typeEnv vaenv vaenv

-- | 'substitutePartialKeep' with the two layers of 'substituteTVExtScoped': the SCOPE (the
-- partial the position sits one level inside, what a 'TVParent' names) and the AMBIENT env (what
-- a 'TVExt' names). Descending into the args and predicates REPLACES the scope with this
-- partial's own (substituted) bindings and leaves the ambient env alone. Unioning the bindings
-- over one env instead would capture a like-named ambient reference under the partial's own
-- parameter and let a parent reference fall through to the ambient env by name — the two
-- conflations the 'TypeVarLoc' split prevents.
substitutePartialKeepScoped :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypeVarArgEnv -> PartialType -> PartialType
substitutePartialKeepScoped typeEnv scope vaenv = substitutePartialKeepScopedR typeEnv vaenv scope vaenv

-- | 'substitutePartialKeepScoped' whose meets read @readEnv@ (see 'substituteKeepReading').
substitutePartialKeepScopedR :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypeVarArgEnv -> TypeVarArgEnv -> PartialType -> PartialType
substitutePartialKeepScopedR typeEnv readEnv scope vaenv partial@PartialType{ptVars, ptArgs, ptPreds} = partial{
    ptVars  = ptVars',
    ptArgs  = fmap (substituteKeepScopedR typeEnv readEnv (ambientEnv $ innerScope) vaenv) ptArgs,
    ptPreds = mapTypePreds (mapTypePred (substitutePartialKeepScopedR typeEnv readEnv (ambientEnv $ innerScope) vaenv)) ptPreds
    }
  where
    -- The partial's own values sit at ITS level, so they keep the scope they arrived with.
    ptVars' = fmap (substituteKeepScopedR typeEnv readEnv scope vaenv) ptVars
    innerScope = joinVarArgEnv ptVars' H.empty

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

-- | Substitute a class member template's EXTERNAL references at expansion EXIT: the references
-- name the CLASS object's scope, which ceases to exist as the member leaves it, so every
-- 'TVExt' reference resolves TOTALLY — to its env binding when present (a concrete value, or a
-- link of the RECEIVING scope the application itself supplied), and to the ⊤ it denotes when
-- absent — ⊤-bound included, unlike 'substitutePartialKeep'. A class-scope spelling surviving
-- the exit is a dangling name: a like-named variable of the receiving scope would capture it (in
-- a nested list, an enclosing level's @/T=Integer@ would make an inner cell's ⊤ head read
-- Integer). 'TVInt' references are kept — they
-- are the member's own declaration linkage, resolved against its own 'ptVars'.
substituteExit :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
substituteExit typeEnv vaenv (TypeVar (TVApplied b overrides) loc i) =
  applyTypeVarOverrides typeEnv (fmap (substituteExit typeEnv vaenv) overrides) (substituteExit typeEnv vaenv (TypeVar b loc i))
substituteExit typeEnv vaenv (TypeVar v loc@TVExt i) = case H.lookup (extKey v) vaenv of
  Just b  -> meetInner typeEnv vaenv b i'
  Nothing -> case v of
    TVVar _ -> i'  -- the occurrence reads var ∩ inner: erasing the dead var leaves the inner
    _       -> TypeVar v loc i'
  where i' = substituteExit typeEnv vaenv i
substituteExit typeEnv vaenv (TypeVar v loc@TVParent i) = TypeVar v loc (substituteExit typeEnv vaenv i)
substituteExit typeEnv vaenv (TypeVar v loc@TVInt i) = TypeVar v loc (substituteExit typeEnv vaenv i)
substituteExit typeEnv vaenv (UnionType Nothing partials consts) =
  UnionType Nothing (joinUnionType $ map (substitutePartialExit typeEnv vaenv) $ splitUnionType partials) consts
substituteExit typeEnv vaenv (UnionType (Just (preds, negLeafs, negConsts)) posLeafs consts) =
  UnionType (Just (mapTypePreds (mapTypePred (substitutePartialExit typeEnv vaenv)) preds, negLeafs, negConsts)) posLeafs consts

substitutePartialExit :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType
substitutePartialExit typeEnv vaenv partial@PartialType{ptVars, ptArgs, ptPreds} = partial{
    ptVars  = H.mapWithKey exitVar ptVars,
    ptArgs  = fmap (substituteExit typeEnv vaenv) ptArgs,
    ptPreds = mapTypePreds (mapTypePred (substitutePartialExit typeEnv vaenv)) ptPreds
    }
  where
    -- A SELF-REFERENTIAL parameter entry (@/T ↦ $/T@ or @/T ↦ $_/T@, either loc) is the
    -- declaration's unbound-parameter spelling, and at exit it receives the exiting scope's
    -- binding for that name. Instantiated, the binding lands: kept literal, the member would
    -- drop the class application's value — the @[/T=⊤−X]@ element restriction a difference
    -- computed, or the @Integer@ of @ConsList[/T=Integer]@, whose expansion would widen to
    -- any-element members. Uninstantiated (⊤), by loc: a 'TVExt' self-spelling references the
    -- EXITING class scope and materializes to the ⊤ it denotes; a 'TVInt' self-spelling is
    -- partial-LOCAL — its scope is the member partial itself, which travels with it rather
    -- than exiting — and stays verbatim as the flexible per-value parameter the engine binds on
    -- demand ('classSatCandidates'.'substMemberVar' parity), read as ⊤-equivalent wherever
    -- content is required ('freeTVIntToTopDeep').
    -- A self-entry is a producer ERROR form (a parameter is spelled as a constant, another
    -- variable, or a parent @$_T@ — never itself); these clauses read one where it arrives.
    exitVar k (TypeVar (TVVar k') loc i) | k' == k, loc /= TVParent = case (vaenvLookup typeEnv vaenv (TVVar k), loc) of
      (PTopType, TVInt) -> TypeVar (TVVar k') loc i'
      (PTopType, _)     -> i'  -- var ∩ inner: the dead self-spelling erases to the inner
      (b, _)            -> meetInner typeEnv vaenv b i'
      where i' = substituteExit typeEnv vaenv i
    exitVar _ v = substituteExit typeEnv vaenv v

-- | See 'substitutePartialKeep': substitutes only concrete external bindings, keeping every other
-- var unchanged.
substituteKeep :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
-- The type read here is a FRAGMENT: a slot value or a point's type with the object that
-- encloses it left implicit. Its parent is that object, whose own variables are what the
-- ambient env registers ('TypeCheck.Encode'), so the top-level scope IS the ambient env — the
-- same seeding 'classifyParentVarRefs' applies to an annotation fragment. Descent still
-- replaces it with each partial's own bindings. Seeded with ∅, a detached @∀ConsList[/T=$^/T]@
-- would stay symbolic where the object binds @/T@.
substituteKeep typeEnv vaenv = substituteKeepScoped typeEnv vaenv vaenv

-- | 'substituteKeep' of the bindings @binds@ with its meets READ in @readEnv@: a binding met
-- into an occurrence's inner ('meetInner') is a meet like any other, and the references inside
-- either side denote their ambient bindings. A caller folding back only the bindings a meet
-- DISCOVERED passes that meet's ambient here; reading the meets in the discoveries alone read
-- every other reference as unbound.
substituteKeepReading :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypeVarArgEnv -> Type -> Type
substituteKeepReading typeEnv readEnv binds = substituteKeepScopedR typeEnv readEnv binds binds

-- | 'substituteKeep' with a SCOPE layer beside the ambient env (see
-- 'substitutePartialKeepScoped'). A 'TVExt' reads the ambient env and nothing else; a
-- 'TVParent' reads the scope and nothing else — a miss leaves it symbolic, since the layer is
-- supplied by the descent and its absence means this traversal did not carry the parent, not
-- that the reference denotes ⊤; a 'TVInt' is kept.
substituteKeepScoped :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypeVarArgEnv -> Type -> Type
substituteKeepScoped typeEnv scope vaenv = substituteKeepScopedR typeEnv vaenv scope vaenv

-- | 'substituteKeepScoped' whose meets read @readEnv@ (see 'substituteKeepReading').
substituteKeepScopedR :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypeVarArgEnv -> TypeVarArgEnv -> Type -> Type
substituteKeepScopedR typeEnv readEnv scope vaenv (TypeVar (TVApplied b overrides) loc i) =
  applyTypeVarOverrides typeEnv (fmap (substituteKeepScopedR typeEnv readEnv scope vaenv) overrides) (substituteKeepScopedR typeEnv readEnv scope vaenv (TypeVar b loc i))
-- A concrete binding resolves the occurrence: @binding ∩ inner@. A kept occurrence keeps its
-- (substituted) inner along with the reference. A binding that still carries a hop
-- ('keepsHop' — @Cons[/T=$^/A]@'s own @/T@) is not flattened into the reference naming it: the
-- field's @tail=∀ConsList[/T=$^/T]@ would become @/T=$^/A@, a reference spelled for the
-- partial's level moved one level in.
substituteKeepScopedR typeEnv readEnv scope vaenv (TypeVar v loc@TVExt i) = keepOrMeet (H.lookup (extKey v) vaenv)
  where keepOrMeet = substituteKeepLeaf typeEnv readEnv scope vaenv v loc i
substituteKeepScopedR typeEnv readEnv scope vaenv (TypeVar v loc@TVParent i) = keepOrMeet (envLookupAt scope TVParent v)
  where keepOrMeet = substituteKeepLeaf typeEnv readEnv scope vaenv v loc i
substituteKeepScopedR typeEnv readEnv scope vaenv (TypeVar v loc@TVInt i) = TypeVar v loc (substituteKeepScopedR typeEnv readEnv scope vaenv i)
substituteKeepScopedR typeEnv readEnv scope vaenv (UnionType Nothing partials []) =
  UnionType Nothing (joinUnionType $ map (substitutePartialKeepScopedR typeEnv readEnv scope vaenv) $ splitUnionType partials) []
substituteKeepScopedR typeEnv readEnv scope vaenv (UnionType (Just (preds, negLeafs, negConsts)) posLeafs consts) =
  UnionType (Just (mapTypePreds (mapTypePred (substitutePartialKeepScopedR typeEnv readEnv scope vaenv)) preds, negLeafs, negConsts)) posLeafs consts
substituteKeepScopedR _typeEnv _ _ _ t = t

-- | The leaf rule shared by the two outer locs of 'substituteKeepScoped', applied to the binding
-- the loc's own layer holds (if any).
substituteKeepLeaf :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypeVarArgEnv -> TypeVarArgEnv -> TypeVarAux -> TypeVarLoc -> Type -> Maybe Type -> Type
substituteKeepLeaf typeEnv readEnv scope vaenv v loc i mb = case mb of
  Just b | not (isTop b), not (keepsHop b) -> meetInner typeEnv readEnv b i'
  _                                        -> TypeVar v loc i'
  where
    isTop PTopType = True
    isTop _        = False
    i' = substituteKeepScopedR typeEnv readEnv scope vaenv i

-- | Replaces 'TVExt' 'TVVar' references in a 'Type' using the provided 'TypeVarEnv'.
-- Thin wrapper over 'substituteTVExt'.
substituteVarsWithVarEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarEnv -> Type -> Type
substituteVarsWithVarEnv typeEnv venv = substituteTVExt typeEnv ((ambientEnv $ joinVarArgEnv venv H.empty))

typeGetAux :: TypeVarAux -> PartialType -> Maybe Type
typeGetAux (TVVar v) p   = Just $ H.lookupDefault PTopType v $ ptVars p
typeGetAux (TVArg v) p   = typeGetArg v p
-- A re-applied variable is not read here: rebinding a data value's parameter re-derives the fields
-- it governs from the value's declaration ('applyTypeVarOverrides'), which this env-free accessor
-- cannot consult, so the reference is left for an env-carrying consumer to resolve.
typeGetAux TVApplied{} _ = Nothing

-- | Gets an arg from a partial type, fully resolved against the partial's own bindings.
-- When the arg name is absent, falls back to 'PTopType' for upper-unbounded partials
-- (where the arg may still be present) and 'Nothing' for upper-bounded ones.
typeGetArg :: ArgName -> PartialType -> Maybe Type
typeGetArg argName partial@PartialType{ptArgs, ptVars, ptArgMode} = case H.lookup argName ptArgs of
  Nothing -> if ptArgUb ptArgMode then Nothing else Just PTopType
  Just arg -> case arg of
    t@(UnionType (Just _) _ _) -> Just t
    TypeVar (TVArg a) _ _ | a == argName -> error $ printf "Found getArg cycle looking for %s in %s" (show argName) (show partial)
    -- A link to the scope an arrow's result is being computed for ('callerScopeName' — @idf(/x=$_/Q)@
    -- read for @ident@'s @/Q@) is never one of the partial's own variables, and stays that link:
    -- read through the partial it would be the ⊤ of a parameter the partial has not got, and the
    -- link the caller resolves would be gone.
    t'@(TypeVar (TVVar n) TVExt _) | isCallerScopeName n -> Just t'
    t'@(TypeVar v _ _) -> case typeGetAux v partial of
      Just t'' -> Just t''
      Nothing  -> Just t'
    UnionType Nothing partialLeafs [] -> Just $ UnionType Nothing (joinUnionType $ map substitutePartial $ splitUnionType partialLeafs) []
      where
        substitutePartial p@PartialType{ptVars=vs} = p{ptVars = fmap subst vs}
        -- Accessor-local resolution: this raw read carries no 'TypeEnv', so a slot value
        -- containing occurrence-local knowledge is KEPT whole — its @var ∩ inner@ meet is
        -- DEFERRED to env-carrying consumers rather than approximated. For a knowledge-free
        -- value, 'meetInner''s ⊤ short-circuits guarantee the empty env below is never
        -- consulted.
        subst t | typeHasInnerKnowledge t = t
                | otherwise = substituteVarsWithVarEnv emptyTypeEnv' ptVars t
    t -> Just t

-- | 'typeGetArg' tolerating 'PartialKey' phase drift. An argument key computed from a resolved
-- type ('partialToKey', e.g. via 'inExprSingleton') carries the type's accreted variable names in
-- 'pkVars', while the same logical argument may be stored under an encode-time structural key
-- without them (a call site's higher-order argument @/fn(/v)@ vs the declaration's
-- @/fn[/F,..](/v)@). When the exact key is absent, fall back to the unique stored argument that
-- matches by name and applied-argument names, ignoring 'pkVars'.
typeGetArgLoose :: ArgName -> PartialType -> Maybe Type
typeGetArgLoose argName partial@PartialType{ptArgs}
  | H.member argName ptArgs = typeGetArg argName partial
  | otherwise = case filter matches (H.keys ptArgs) of
      [k] -> typeGetArg k partial
      _   -> typeGetArg argName partial
  where matches k = pkName k == pkName argName && pkArgs k == pkArgs argName

-- | What a higher-order argument's slot RETURNS: its input object resolved through the
-- environment's arrows (the callee's local argument arrows, or the caller's lambda). A slot that
-- does not hold an input object is already the result and is returned unchanged; an input object
-- no arrow in the environment accepts returns ⊤ — its result is unknown here, not the object.
hoSlotResult :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> ArgName -> Type -> Type
--
-- What an argument returns is never an unreduced call of an argument named like itself (the rule
-- 'NoReturnArg' applies to an arm's result). A lambda forwarding an enclosing parameter of the
-- same name (@allBy(lst, fn(v))= ..fmap(.., fn(v)= fn(v= v))@) has a result that keeps the raw
-- @fn(/v=..)@ form beside its reach, as any term may stay unrewritten. That raw form names the
-- OUTER argument, whose reach is already in the union, but in the call's environment the
-- lambda is the arrow named @fn@, so it would resolve back to itself. It is dropped.
hoSlotResult typeEnv vaenv argName t = case hoSlotInputObject argName t of
  Just inObj -> case typeGraphQueryWithReason typeEnv vaenv inObj of
    []    -> PTopType
    dests -> dropSelfCalls $ unionAllTypesWithEnv typeEnv vaenv [dest | (_, _, dest) <- dests]
  Nothing -> t
  where
    dropSelfCalls (UnionType mTop leafs consts) =
      UnionType mTop (joinUnionType [l | l <- splitUnionType leafs, isNothing (hoSlotInputObject argName (singletonType l))]) consts
    dropSelfCalls r = r

-- | A slot's value read in the POINT's currency: a var-slot reference to a parameter of the slot's
-- partial (@∀Tree[/V=$^/V]@ under @Node[/V=$^/MV]@) is replaced by the partial's binding, which
-- is a point-level value already; a parameter the partial leaves UNRESOLVED reads as the ⊤ it
-- denotes — unbound, bound only to its own spelling, or bound to exactly its DECLARED bound
-- (@get[/MK=∀Ord]@ is the declaration's instantiation, not a resolution: read as a value it would
-- turn the slot's @Map[/K=$^/MK]@ into @Map[/K=∀Ord]@, and the actual's @/K=Integer@ could never
-- narrow @/MK@ — the same vacuous-occurrence rule the arg meet's pointer join applies). References inside
-- a nested partial's arguments name that partial and are untouched; a var-slot VALUE sits at its
-- partial's level, so the walk continues through it. A bare pointer slot is read through the var by
-- the slot read itself.
slotAtPoint :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> Type -> Type
slotAtPoint typeEnv vaenv PartialType{ptName=supName, ptVars=supVars} = level
  where
    declBounds = H.unions [ cleanDeclSelfRefs (ptVars t) | t <- typeGraphData typeEnv supName ++ typeGraphArrowTemplates typeEnv supName ]
    level (UnionType mtop pos consts) = UnionType (fmap onTop mtop) (joinUnionType $ map onLeaf $ splitUnionType pos) consts
    level t = t
    onTop (preds, negLeafs, ncs) = (mapTypePreds (mapTypePred onLeaf) preds, joinUnionType $ map onLeaf $ splitUnionType negLeafs, ncs)
    onLeaf p@PartialType{ptVars} = p{ptVars = fmap inVarSlot ptVars}
    inVarSlot (TypeVar (TVVar y) loc i) | loc /= TVExt = case H.lookup y supVars of
      Just b | b /= PTopType, not (selfNamed b), H.lookup y declBounds /= Just b -> meetInner typeEnv vaenv b i
      _                                                                          -> level i
      where selfNamed (TypeVar (TVVar y') _ _) = y' == y
            selfNamed _                        = False
    inVarSlot t = level t

updateTypeProp :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> TypeVarAux -> Type -> (TypeVarArgEnv, Type, Type)
updateTypeProp typeEnv vaenv superType propName subType =
  let (vaenv', superType', subType') = updateTypePropInner typeEnv vaenv superType propName subType
  in (vaenv', restoreLostTVExtVars superType superType', subType')

updateTypePropInner :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> TypeVarAux -> Type -> (TypeVarArgEnv, Type, Type)
updateTypePropInner typeEnv vaenv superType propName subType = case superType of
    PTopType -> (vaenv, PTopType, subType)
    TypeVar v _ _ -> do
      let (vaenv', superType', subType') = updateTypeProp typeEnv vaenv (H.lookupDefault PTopType (extKey v) vaenv) propName subType
      (H.insert (extKey v) superType' vaenv', superType, subType')
    t@(UnionType (Just _) _ _) ->
      (vaenv, t, subType)
    UnionType Nothing supPartials [] -> do
      let supPartialList = splitUnionType supPartials
      -- The slot AS STORED, without pointer resolution: a slot holding a partial-internal var
      -- pointer (@/fn(..)=$B@, like a data definition's @/head=$T@) must be recognized as one
      -- and updated through the partial's OWN var; the resolving read ('typeGetAux' ->
      -- 'typeGetArg') follows the pointer to the var's current (often Top) binding, and
      -- writing an update against that would clobber the pointer.
      let rawSlot (TVVar k) p   = H.lookup k (ptVars p)
          rawSlot (TVArg k) p   = H.lookup k (ptArgs p)
          rawSlot TVApplied{} _ = Nothing
      -- The value STORED into the slot exits the sub's scope: an external reference the
      -- constraint scope does not bind names a scope that ceases to exist here and reads as
      -- the ⊤ it denotes ('dropOutOfScopeTVExt'); an in-scope reference stays a link. The
      -- value RETURNED to the sub point keeps its own scope's refs.
      -- The slot's partial-internal var, whether stored directly or read through the pointer.
      let slotOwnVar sup = case (rawSlot propName sup, typeGetAux propName sup) of
            (Just (TypeVar (TVVar v) TVInt _), _) -> Just v
            (_, Just (TypeVar (TVVar v) TVInt _)) -> Just v
            _                                     -> Nothing
      -- The resolving slot read, with an ABSENT arg slot read at its DECLARED bound rather
      -- than the bare ⊤ fallback of 'typeGetArg'. The occurrence's realizable value-set is
      -- declaration-bounded — no constructible value holds a field outside its declared type —
      -- the same clamp the partial subtype applies through its declared-bound fallback. The
      -- stored spellings @#assert..@ and @#assert(test=∀Boolean, msg=String)??@ denote the
      -- SAME set (which is why a respelling never commits — 'eqScheme' reads it as a no-op),
      -- so the projection must read the declared bound out of the name-only spelling itself
      -- (an argument slot requirement, e.g. an annotation's @test -> ∀Boolean@, stays ⊤
      -- otherwise). Free template parameters of the declared bound read ⊤ ('freeTVIntToTopDeep').
      -- A slot PRESENT AS ⊤ is the same spelling: an object pattern's binder
      -- (@:Node[$K -> Ord, $V -> Eq](left, key, value, right).equal(other)@) stores
      -- @Node[/K=∀Ord, /V=∀Eq](left=⊤, ..)@, which denotes @left=∀Tree[/K=∀Ord, /V=∀Eq]@ and
      -- is judged equal to it, so no narrowing ever respells the store — read literally, the
      -- binder would be ⊤ and the body's @Node(left= left, ..)@ would be constructed ⊤-slotted.
      -- Only a slot with no positive content reads through the declaration: a spelled value
      -- keeps its own forms (an unsettled call form in a slot is the settle machinery's to
      -- decide, not a projection's).
      --
      -- ⊤ is only the KNOWLEDGE-FREE case of the shape this applies to. A slot holding any pure
      -- TOP — a predicate/class form with no positive leaf of its own (@left=∀Tree[/K=⊤]@) —
      -- is likewise bounded by the declaration and nothing else, and reading it literally
      -- answers with whatever that top happens to spell. Restricted to a literal ⊤, the
      -- declaration would stop being consulted the moment anything wrote a wider-than-declared
      -- top into the slot, so a field derived before the partial's own parameters narrowed would
      -- stay stale and contradict the parameter its field is declared to follow
      -- (@Node[/K=∀Ord, /V=∀Eq](left=∀Tree[/K=⊤, /V=⊤])@). Meeting the bound in is
      -- denotation-preserving — no constructible value holds a field outside its declared type
      -- — and it re-derives every time it is read, so it tracks the parameters as they narrow.
      -- A slot with POSITIVE leaves still keeps its own forms: pruning a raw applied form is
      -- the settle machinery's decision, not a projection's.
      let boundedTop (UnionType Just{} pos consts) = H.null pos && null consts
          boundedTop _                             = False
      let slotRead sup = case (rawSlot propName sup, typeGetAux propName sup) of
            (raw, Just supProp) | TVArg k <- propName, raw == Nothing || maybe False boundedTop raw ->
              case [freeTVIntToTopDeep typeEnv vaenv sup d | PartialType{ptArgs=dArgs} <- typeGraphData typeEnv (ptName sup), Just d <- [H.lookup k dArgs]] of
                [] -> Just supProp
                ds -> Just $ intersectTypesEnv typeEnv vaenv supProp (unionAllTypesWithEnv typeEnv vaenv ds)
            (_, r) -> r
      -- A slot already PRESENT as a pure top is re-read through the declaration on every
      -- projection ('slotRead'); storing that reading back is what makes the tightening stick.
      -- Only an existing pure-top slot is rewritten — never one with positive leaves (whose forms
      -- are the settle machinery's), and never a slot the partial does not have (which would
      -- invent one).
      let tightenSup sup = case (rawSlot propName sup, slotRead sup) of
            (Just raw, Just r) | boundedTop raw, r /= raw -> typeSetAux propName r sup
            _                                            -> sup
      -- The value returned to the SUB point EXITS the sup partial's frame: a reference to one
      -- of @sup@'s own vars (@Node[/K=$/MK](left=∀Tree[/K=$/MK], ..)@ projected out of
      -- @mapEqual[/MK=∀Ord, /MV=∀Eq](/a=..)@) names a frame the sub point does not carry —
      -- stored there it would be permanently dangling (no later constraint sees the binding),
      -- a type no value can satisfy. Materialize the frame on the way out: substitute @sup@'s
      -- CLOSED (var-free — see 'typeHasTypeVars') var bindings deep into the sub-bound copy
      -- ('substituteFreeDeep'; the ACT channel is frame-local). The copy stored back into
      -- @sup@'s own slot keeps its pointers — inside the frame the link is the meaning
      -- ('dropOutOfScopeTVExt' extension below).
      let closedSupEnv PartialType{ptVars=sv} = H.fromList
            [ (TVVar k, b) | (k, b) <- H.toList sv, b /= PTopType, not (typeHasTypeVars b) ]
      let closeFrame sup t = let e = closedSupEnv sup in if H.null e then t else substituteFreeDeep typeEnv (ambientEnv $ e) t
      -- The slot and the point are spelled in DIFFERENT scopes: a reference in the slot names
      -- @sup@ (@∀Tree[/V=$^/V]@), a reference on the point names the scope the point sits in
      -- (@∀Map[/V=$^/MV]@, the enclosing declaration), and the same spelling means a different
      -- variable on each side. So no single environment can carry a meet of the two as written:
      -- the point's @$^/MV@ stored into the slot would read as an @/MV@ OF @sup@ (a parameter it
      -- does not have), and the slot's @$^/V@ handed to the point would read as an @/V@ of the
      -- declaration. Each side is therefore read in its own scope first, in the POINT's currency:
      -- the slot's references to @sup@'s parameters are replaced by @sup@'s bindings (point-level
      -- values already) or by the ⊤ an unbound parameter denotes ('slotAtPoint'); the point's
      -- references resolve through the ambient env. The point is the slot's fragment, so a PARENT
      -- reference at its level names @sup@: one naming a parameter @sup@ declares is spelled right
      -- and stays; one naming anything else was written for another level and has moved — a
      -- result @Map[/V=$^/MV]@ arriving at @Node@'s @right@ slot — and reads as the ⊤ it denotes
      -- ('detachForeignParentRefs'), since stored it would spell a @/MV@ of @Node@. An unbound
      -- EXTERNAL reference stays symbolic: the ambient scope is the same on both sides. The two
      -- readings meet, and each side then meets that result with its OWN original, which puts its
      -- own variables back (@Tree[/V=⊤] ∩ Tree[/V=$^/V]@ keeps the link). Only values cross the
      -- boundary; spellings never do.
      let concreteUnder = slotAtPoint typeEnv vaenv
      let declaredParamsOf PartialType{ptName=n, ptVars=vs} = case typeGraphData typeEnv n ++ typeGraphArrowTemplates typeEnv n of
            [] -> Nothing -- open world: nothing says which names the partial binds
            ds -> Just (S.unions (H.keysSet vs : map (H.keysSet . ptVars) ds))
      let concreteAtUnder sups = maybe id detachForeignParentRefs (S.unions <$> mapM declaredParamsOf sups) . substituteTVExtScoped typeEnv vaenv vaenv
      -- A side's original re-met with the concrete meet, with the meet's fresh discoveries folded
      -- back in: a bare variable met against a concrete value keeps the variable in the type and
      -- records the value only in the returned env ('intersectTypesEnv' drops it), so the
      -- evidence @Map[/K=Integer]@ brought to a slot @Map[/K=$^/MK]@ would otherwise vanish and
      -- the call's @/MK@ never narrow. Resolving the discoveries is the same resolve-meets-back
      -- rule 'intersectTypesWithVarEnv' applies.
      let resolvedMeetIn env orig m =
            let (env', t) = intersectTypesWithVarEnv typeEnv env orig m
                fresh = discoveredBindings env' env
            in if H.null fresh then t else substituteKeep typeEnv fresh t
      let resolvedMeet = resolvedMeetIn vaenv
      -- The slot's own original is re-met INSIDE @sup@: its parent references name @sup@'s
      -- parameters, whose values are passed under their own keys as a partial meet passes them
      -- down ('intersectPartialsBaseN'.'deepVaenv'), so a link the concrete meet lies within is
      -- kept ('parentRefReading') rather than spelled out.
      let slotScope PartialType{ptVars=sv} = H.union (H.fromList [ (VarKey TVInt (TVVar k), b) | (k, b) <- H.toList sv, b /= PTopType, not (isTypeVar b) ]) vaenv
      -- A higher-order argument's slot holds its INPUT OBJECT, whose parameters are annotated by
      -- the enclosing declaration's variables (@fn(/acc=$_B, /k=$_MK, /v=$_MV)@). Those are bound
      -- per call, by the call meeting the declaration, so the object is stored with its in-scope
      -- references lifted to hops ('hopInputObject') rather than read concretely (an unbound
      -- @$_MV@ as ⊤, @$_MK@ as its bound @∀Ord@), which left nothing for a call to bind.
      let slotValue sup supProp evSup met = case propName of
            TVArg k | supProp == PTopType, isJust (hoSlotInputObject k subType) -> hopInputObject vaenv subType
            _ -> resolvedMeetIn (slotScope sup) evSup met
      let intersectedPartials sup@PartialType{ptVars=supVars} sub0 = case (slotOwnVar sup, slotRead sup) of
            (Just v, _) -> do
              -- A pointer slot names a MEMBERSHIP position (@this -> $T@): a single
              -- occurrence's evidence must NOT meet-narrow the var — a sibling occurrence may
              -- still widen the witness (@:Some.equal(other= Nothing)@: narrowing @/T@ to
              -- @Some@ from @this@ empties @other=Nothing@ at the next universe meet). The
              -- var narrows only where EVERY occurrence is visible at once — the
              -- pointer-occurrence join in 'intersectPartialsBase'. Here the child is just
              -- clamped by the var's current bound and the parent stays untouched.
              -- The var's value sits at the point's level already.
              let supVar = H.lookupDefault PTopType v supVars
              let newProp = intersectTypesEnv typeEnv vaenv supVar sub0
              Just (sup, closeFrame sup newProp)
            (_, Just supProp) -> do
              let met = intersectTypesEnv typeEnv vaenv (concreteUnder sup supProp) (concreteAtUnder [sup] sub0)
              -- The evidence the two spellings meeting AS WRITTEN discover for their variables —
              -- kept only where it is a VALUE (no scope reference: @/MK ↦ Integer@, never
              -- @/V ↦ $^/MV@) — and folded into each side before its re-meet. The concrete
              -- readings above erase a slot variable before it can meet the entering value's
              -- isa-related predicate: @Map[/K=$^/MK]@ read as @Map[/K=⊤]@ would never learn
              -- @/MK ↦ Integer@ from an actual @Map[/K=∀Ord] ∧ Tree[/K=Integer]@, whose
              -- same-class predicate only offers the bound, and the call's @/MK@ would stay at
              -- @∀Ord@. A value crosses the boundary either way; a spelling never does.
              let evidence = H.filter concreteEvidence $ H.difference (fst (intersectTypesWithVarEnv typeEnv vaenv supProp sub0)) vaenv
                  concreteEvidence v = not (isTypeVar v) && v /= PTopType && detachParentRefs v == v
                  withEvidence t | H.null evidence = t
                                 | otherwise       = substituteKeep typeEnv evidence t
              if isBottomType met
                then Nothing
                -- The stored value sits INSIDE @sup@'s scope (scopes nest through partials), so
                -- the drop's in-scope set extends with @sup@'s own var names — a slot value
                -- @∀ConsList[/T=$_/T]@ under @insertStep[/T=∀Ord]@ keeps its link. The value is
                -- detached from @sup@ here, so the enclosing scope must be supplied explicitly.
                else Just ( typeSetAux propName (dropOutOfScopeTVExt typeEnv (H.union (ambientEnv $ joinVarArgEnv supVars H.empty) vaenv) (slotValue sup supProp (withEvidence supProp) met)) sup
                          , closeFrame sup (resolvedMeet (withEvidence sub0) met) )
            (_, Nothing) -> Nothing
      let tightenedSuper = compactType typeEnv vaenv $ UnionType Nothing (joinUnionType (map tightenSup supPartialList)) []
      case subType of
        UnionType Nothing subPartials [] -> do
          let subPartialList = splitUnionType subPartials
          let (supPartialList', subPartialList') = unzip $ catMaybes $ [intersectedPartials sup (singletonType sub) | sup <- supPartialList, sub <- subPartialList]
          (vaenv, compactType typeEnv vaenv $ UnionType Nothing (joinUnionType supPartialList') [], unionAllTypesWithEnv typeEnv vaenv subPartialList')
        TypeVar v _ _ -> do
          -- The meet stored as the var's refined binding must be its STANDALONE reading: the
          -- intersection records a met external var in its returned env while the type keeps
          -- the var (@∀ConsList[Ord] ∩ ∀ConsList[$_T]@ reads as @∀ConsList[$_T]@ alone — WIDER
          -- than the existing binding, a monotonicity violation when this constraint's scope
          -- does not bind @$_T@). Resolve the meet's fresh discoveries back in, mirroring the
          -- prop intersection above ('intersectedPartials').
          -- The same two-scope reading as 'intersectedPartials': the slots read concretely under
          -- their partials, the var's binding concretely under the ambient env, the meet re-met
          -- with the binding to keep its variables; the slot install re-meets it with each slot.
          let subBinding = H.lookupDefault PTopType (extKey v) vaenv
          let argVarLink = case (v, subBinding) of
                (TVArg _, TypeVar vn@(TVVar _) loc _) | loc /= TVParent, H.member (extKey vn) vaenv -> Just (TypeVar vn TVExt PTopType)
                _ -> Nothing
          let met = intersectTypesEnv typeEnv vaenv (concreteAtUnder supPartialList subBinding) (unionAllTypesWithEnv typeEnv vaenv $ mapMaybe (\sup -> concreteUnder sup <$> slotRead sup) supPartialList)
          let tp' = resolvedMeet subBinding met
          let vaenv' = H.insert (extKey v) tp' vaenv
          let updateSuperPartial p = case (rawSlot propName p, typeGetAux propName p) of
                -- Slot is an internal pointer: keep the pointer and leave the var untouched —
                -- a single occurrence must not meet-narrow a membership var (see
                -- 'intersectedPartials' above; the join in 'intersectPartialsBase' narrows it).
                (Just (TypeVar (TVVar _) TVInt _), _) -> Just p
                (_, Nothing) -> Nothing
                (_, Just supTp) | supTp == subType -> Just $ typeSetAux propName subType p
                -- A partial-internal ('TVInt') sub is BY DEFINITION a reference into this
                -- object's own var scope — the desugared @(l :: $T == r :: $T)@ arg arrM is
                -- @$/T@, and so is a declaration's own-parameter argument annotation,
                -- function-typed ones included (@fmap[..$B](fn(v -> $_A) -> $_B)@'s @/fn@
                -- slot holds the result @$/B@, as 'classifyParentVarRefs' spells it) — so store
                -- the pointer
                -- unconditionally (the var slot itself may only arrive with a later VarApply
                -- merge; a pointer without its var reads ⊤ until then). Falling through would
                -- store the var's resolved BOUND (@∀Eq@) and sever the arg↔var linkage that
                -- lets call-site argument evidence bind the object's var (@==(l=Integer,...)@
                -- discovering @/T=Integer@ for dispatch).
                (_, Just PTopType) | TVArg _ <- propName, TypeVar (TVVar vn) TVInt subI2 <- subType ->
                  Just $ typeSetAux propName (TypeVar (TVVar vn) TVInt subI2) p
                -- The same link one scope out: an external reference to a variable the
                -- constraint's scope binds — a nested pattern's parameter annotated by the
                -- enclosing declaration's variable (@fm[$A](.., fn(v -> $_A) -> $_B)@'s @fn(v)@
                -- pattern) — is that declaration's variable, not its current bound. Resolved to
                -- the bound (⊤ for an unconstrained @$A@) the pattern lost the link the call's
                -- element evidence binds @$A@ through, and the declared input object reached the
                -- call as @fn(/v=⊤)@. An external reference the scope does not bind still exits
                -- as the ⊤ it denotes (the frame-exit rule above).
                (_, Just PTopType) | TVArg _ <- propName, TypeVar (TVVar vn) TVExt subI2 <- subType, H.member (extKey (TVVar vn)) vaenv ->
                  Just $ typeSetAux propName (TypeVar (TVVar vn) TVExt subI2) p
                -- The same link reached through an ARGUMENT: a definition's parameter typed by one
                -- of the definition's own variables (@bap[$M](m -> $M, ..)@ binds @/m ↦ $/M@) is
                -- that variable wherever the body uses it, so the slot it fills (@mmap(m= m)@)
                -- holds the variable as the enclosing scope's reference — not its current bound,
                -- which made the body's result the bound's (@mmap@'s @$M@ read as @∀Monad@) and
                -- left the definition promising "some Monad" rather than its own @$M@. Once
                -- stored, the link is kept.
                (_, Just supTp) | TVArg _ <- propName, Just lnk <- argVarLink, supTp == PTopType || supTp == lnk ->
                  Just $ typeSetAux propName lnk p
                (_, Just supTp) -> Just $ typeSetAux propName (resolvedMeet supTp met) p
          let superType' = compactType typeEnv vaenv $ UnionType Nothing (joinUnionType $ mapMaybe updateSuperPartial $ splitUnionType supPartials) []
          (vaenv', superType', subType)
        PTopType -> do
          -- The frame-exit closure applies to the ⊤-sub SEEDING write too — this is the
          -- FIRST projection a fresh sub point receives, and delivering the raw slot here
          -- plants the dangling frame-var spelling every later epoch then preserves.
          let sub' = case mapMaybe (\sup -> concreteUnder sup <$> slotRead sup) supPartialList of
                []       -> PTopType
                supProps -> compactType typeEnv vaenv $ unionAllTypesWithEnv typeEnv vaenv supProps
          -- The declaration-bounded reading is not only for the value handed to the sub: the slot
          -- the super STORES is bounded the same way, so tighten it in place. Left alone, a slot
          -- filled before the partial's parameters narrowed keeps its stale spelling for good
          -- while every READ of it reports the tighter bound — the two disagree, and it is the
          -- stored one that reaches Decode and TreeBuild.
          (vaenv, tightenedSuper, sub')
        _ -> do
          let (supPartialList', subPartialList') = unzip $ catMaybes $ [intersectedPartials sup subType | sup <- supPartialList]
          (vaenv, compactType typeEnv vaenv $ UnionType Nothing (joinUnionType supPartialList') [], compactType typeEnv vaenv $ unionAllTypesWithEnv typeEnv vaenv subPartialList')
    t -> (vaenv, t, subType)
