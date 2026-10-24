-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.Algebra.Preds
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Predicate operations that need the type algebra: expanding predicates, deciding implication
-- between them, and whether a set of predicates is empty.
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Semantics.Types.Algebra.Preds where

import                qualified Data.HashMap.Strict             as H
import                qualified Data.HashSet                    as S
import                          Data.List                       (partition)
import                qualified Data.List                       as L
import                          Data.Maybe
import {-# SOURCE #-}           Semantics.Types.Algebra.Compact (compactType)
import                          Semantics.Types.Algebra.Expand
import                          Semantics.Types.Algebra.Sat
import {-# SOURCE #-}           Semantics.Types.Algebra.SetOps  (complementTypeEnv,
                                                                 intersectPartials,
                                                                 intersectTypesEnv)
import                          Semantics.Types.Algebra.Subst
import {-# SOURCE #-}           Semantics.Types.Algebra.Subtype (comparedPartialsEnv,
                                                                 isSubtypeOfWithEnv)
import                          Semantics.Types.Construct
import                          Semantics.Types.Data
import                          Semantics.Types.Name
import                          Semantics.Types.Preds
import                          Semantics.Types.SatGoal
import                          Semantics.Types.Vars
import                          Utils


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
-- A relation or class implies a value predicate when the predicate holds of every value in it.
-- The predicate is decided per value shape ('predExprHoldsOn', where a leaf meets it in
-- 'isSubPartialOfWithEnv'), so the universe is re-expressed as what it expands to and that is
-- asked against the predicate, the same re-expression the subtype relation makes of a top
-- ('isSubtypeOfWithEnvRaw''s @oldReexpress@). Without it the two answered @isa Dog ⟹ fluffy@
-- differently: the subtype relation proved @~Dog ⊆ (⊤|fluffy)@ while the implication refused it,
-- so their union kept the De Morgan spelling @¬(¬~Dog ∧ ¬fluffy)@ that the implication then
-- could not equate to @⊤|fluffy@. Gated on the same exactness: only an atom whose extent is
-- fixed ('predsPositiveSealed' — a data name or a sealed class) or the closed world may be read
-- through its current members, as a future member of an open class need not satisfy the
-- predicate. The expansion must differ from the atom's own universe, so each step reads a more
-- concrete spelling (relation, then class members, then leafs).
predImpliesAtom typeEnv vaenv a1 (PredExpr e)
  | not (isPredExprAtom a1)
  , tePrgmEnv typeEnv || predsPositiveSealed typeEnv (PredsOne a1)
  , let expanded = expandPredicates typeEnv vaenv (PredsOne a1)
  , expanded /= TopType H.empty (PredsOne a1)
  = isSubtypeOfWithEnv typeEnv vaenv expanded (TopType H.empty (PredsOne (PredExpr e)))
  where
    isPredExprAtom PredExpr{} = True
    isPredExprAtom _          = False
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
