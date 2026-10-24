-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.Preds
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Structural operations on 'TypePredicates' that do not need the type algebra, and the
-- decision of a value predicate on a leaf ('predExprHoldsOn').
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Semantics.Types.Preds where

import           Control.Applicative       ((<|>))
import           Control.Exception         (evaluate)
import           Control.Monad             (guard)
import           CtConstants
import qualified Data.HashMap.Strict       as H
import qualified Data.HashSet              as S
import           Data.IORef                (IORef, atomicModifyIORef', newIORef,
                                            readIORef)
import qualified Data.List                 as L
import           Data.Maybe
import           Semantics.Types.Construct
import           Semantics.Types.Data
import           Semantics.Types.Memo
import           Semantics.Types.Name
import           System.IO.Unsafe          (unsafePerformIO)
import           Text.Printf
import           Utils


-- | 'mapTypePred' in a monad, for a mapping that can fail.
mapTypePredM :: (Monad m) => (PartialType -> m PartialType) -> TypePredicate -> m TypePredicate
mapTypePredM f (PredExpr p)    = PredExpr <$> f p
mapTypePredM _ p@PredConstEq{} = return p
mapTypePredM f (PredClass p)   = PredClass <$> f p
mapTypePredM f (PredRel p)     = PredRel <$> f p

-- | 'mapTypePreds' in a monad, for a mapping that can fail.
mapTypePredsM :: (Monad m) => (TypePredicate -> m TypePredicate) -> TypePredicates -> m TypePredicates
mapTypePredsM f (PredsOne p)  = PredsOne <$> f p
mapTypePredsM f (PredsAnd ps) = PredsAnd <$> mapM (mapTypePredsM f) ps
mapTypePredsM f (PredsNot p)  = PredsNot <$> mapTypePredsM f p

predsAnd :: TypePredicates -> TypePredicates -> TypePredicates
predsAnd (PredsAnd as) (PredsAnd bs) = mkAnd (as ++ bs)
predsAnd (PredsAnd ps) p             = mkAnd (p:ps)
predsAnd p (PredsAnd ps)             = mkAnd (p:ps)
predsAnd a b                         = mkAnd [a, b]

-- | Normalize a list of predicates into a 'TypePredicates', deduplicating and
-- collapsing singleton lists so 'PredsAnd [p] = p'.
mkAnd :: [TypePredicates] -> TypePredicates
mkAnd ps = case uniq ps of
  [p] -> p
  ps' -> PredsAnd ps'

predsNot :: TypePredicates -> TypePredicates
predsNot (PredsNot p) = p
predsNot PredsNone    = PredsNone
predsNot p            = PredsNot p

-- | Whether a leaf named @n@ has its class memberships fixed by what is declared now. An open
-- class can gain members, but only NEW types: a type already in scope with no @every@ into a class
-- (or one only at some parameters) gains none later (Zach's ruling, 2026-10-03). So an open class
-- read against such a leaf is decided by its current members restricted to the leaf's name, as a
-- sealed one is: @Node ∩ ∀Map[/K=Kx]@ is @Node[/K=Kx]@ through @every(BinTree[$K..], isa=
-- [Map[$K..]])@, not a @Node|Map[/K=Kx]@ whose fields read ⊤. A class's placeholder member, and a
-- leaf named as a class, stand for the members not yet declared and stay open. So does a call
-- form: the ruling is about TYPES, and a function's call joins classes by what it rewrites to as
-- well as by an @every@ naming it (core's @DShowCall@ members), so only a name with a data
-- definition ('typeGraphData') counts. A PRIMITIVE holding constants ('leafHoldsConstants') is in
-- scope in every program — its constants are — whether or not a file spells @data(Integer)@, so
-- its memberships are fixed too: a @Float@ constant is no @Fish@ in a program declaring no such
-- thing, in the open world as in the closed.
knownMemberName :: (TypeGraph tg) => TypeEnv tg -> TypeName -> Bool
knownMemberName typeEnv n =
  leafHoldsConstants (partialVal n)
  || (S.member n (teNames typeEnv) && nameStr n /= classPlaceholderStr && not (H.member n (scClasses (teSatCache typeEnv)))
    && not (null (typeGraphData typeEnv n)))

-- | Every name a type refers to, at any depth: its leaves, their slots, and their predicates.
typeRefNamesDeep :: Type -> S.HashSet TypeName
typeRefNamesDeep (UnionType mTop pos _) = S.unions $
  maybe S.empty topRefNames mTop : map partialRefNamesDeep (splitUnionType pos)
  where topRefNames (preds, negLeafs, _) = S.union (predsRefNamesDeep preds) (S.unions (map partialRefNamesDeep (splitUnionType negLeafs)))
typeRefNamesDeep _ = S.empty

-- | Every name a partial refers to, at any depth ('typeRefNamesDeep').
partialRefNamesDeep :: PartialType -> S.HashSet TypeName
partialRefNamesDeep PartialType{ptName, ptVars, ptArgs, ptPreds} = S.unions $
  S.singleton ptName : predsRefNamesDeep ptPreds : map typeRefNamesDeep (H.elems ptVars ++ H.elems ptArgs)

predsRefNamesDeep :: TypePredicates -> S.HashSet TypeName
predsRefNamesDeep PredsNone     = S.empty
predsRefNamesDeep (PredsAnd ps) = S.unions (map predsRefNamesDeep ps)
predsRefNamesDeep (PredsNot p)  = predsRefNamesDeep p
predsRefNamesDeep (PredsOne a)  = partialRefNamesDeep (predAtomPartial a)
  where
    predAtomPartial (PredClass p)   = p
    predAtomPartial (PredRel p)     = p
    predAtomPartial (PredExpr p)    = p
    predAtomPartial (PredConstEq c) = constantPartialType c

-- | Returns True if the predicates are syntactically contradictory, i.e. contain both p and ¬p.
-- Uses iterative unit propagation: ¬(A∧B) with A known → derives ¬B, allowing multi-step
-- contradiction detection. Limited to 3 rounds to bound complexity.
isPredsContradictory :: TypePredicates -> Bool
isPredsContradictory (PredsAnd ps) = go (3 :: Int) ps
  where
    go 0 _  = False
    go n ps'
      | any (\p -> PredsNot p `elem` ps') ps' = True
      | otherwise =
          let derived = L.nub $ classImplied ps' ++ concatMap (unitPropDerive ps') ps'
              ps'' = L.nub $ ps' ++ derived
          in if length ps'' > length ps'
             then go (n - 1) ps''
             else False
    -- @hasClass c ⟹ isa c@ (same name): a known @PredClass c@ also makes @PredRel c@ known. Lets
    -- contradiction detection see e.g. @PredClass Eq ∧ ¬(PredClass Eq ∧ PredRel Eq)@ as empty - the
    -- shape an element bound takes when a constructor is a member of two related classes (Eq, Ord).
    classImplied known = [PredsOne (PredRel c) | PredsOne (PredClass c) <- known]
    -- From ¬(A∧B∧...) with all-but-one known, derive the negation of the remaining.
    -- Use predsNot (not PredsNot) so that ¬(¬A) normalizes to A, enabling contradiction detection.
    unitPropDerive :: [TypePredicates] -> TypePredicates -> [TypePredicates]
    unitPropDerive known (PredsNot (PredsAnd binds)) =
      [predsNot b | b <- binds, let others = filter (/= b) binds, all (`elem` known) others]
    unitPropDerive _ _ = []
isPredsContradictory _             = False

partialAddPreds :: PartialType -> TypePredicates -> PartialType
partialAddPreds partial@PartialType{ptPreds} newPreds = partial{ptPreds = predsAnd newPreds ptPreds}

partialLeafsAddPreds :: PartialLeafs -> TypePredicates -> PartialLeafs
partialLeafsAddPreds partials newPreds = joinUnionType $ map (`partialAddPreds` newPreds) $ splitUnionType partials

typeAddPreds :: Type -> TypePredicates -> Type
typeAddPreds (TopType negLeafs preds) ps = TopType negLeafs (predsAnd ps preds)
typeAddPreds v@TypeVar{} _ = error $ printf "Unimplemented typeAddPreds: %s" (show v)
typeAddPreds (UnionType Nothing leafs []) ps = UnionType Nothing (partialLeafsAddPreds leafs ps) []
typeAddPreds t _ = error $ printf "Unimplemented typeAddPreds: %s" (show t)

clearUnionTypePreds :: Type -> Type
clearUnionTypePreds (UnionType Nothing partials []) = UnionType Nothing (joinUnionType $ map (\p -> p{ptPreds=PredsNone}) $ splitUnionType partials) []
clearUnionTypePreds t = t

-- | The value-predicate atoms ('PredExpr') of a positive conjunction. A formula with a negation in
-- it is decided whole and gives none.
predsExprAtoms :: TypePredicates -> [PartialType]
predsExprAtoms preds = maybe [] (\atoms -> [e | PredExpr e <- atoms]) (tryPredsToList preds)

-- | The value-predicate atoms stated OF THE VALUE — naming @$this@, and no variable of a scope —
-- which are the ones evaluated on a runtime value held to the type ('Eval.valWithinType'). A
-- predicate that does not name the value is a condition on the scope it was written in: an arm's
-- guard or a value's @where@, typed over that scope's arguments (@a.isZ == True@ with @a@ read at
-- its type), which is evaluated where it is written, as the condition it is.
predsValueExprAtoms :: TypePredicates -> [PartialType]
predsValueExprAtoms = filter valueExpr . predsExprAtoms
  where
    valueExpr e = not (typeContainsTypeVar (singletonType e)) && any ((== macroThisKey) . nameStr) (S.toList (partialRefNamesDeep e))

-- | The conjuncts of a formula, nested conjunctions flattened; any other formula is its own one
-- conjunct.
predsConjuncts :: TypePredicates -> [TypePredicates]
predsConjuncts (PredsAnd qs) = concatMap predsConjuncts qs
predsConjuncts q             = [q]

-- | Two value predicates ('PredExpr') that are ONE FORM, and that form spelled once. A value
-- predicate is a form, not a set: its @$this@ stands for the value it is decided on, and its
-- operands are what the form applies — @x == 1@ beside @x == {1, 2}@ are two conditions, neither a
-- narrower set than the other. So two atoms relate only when they are the same application, name
-- for name, where a name written relative matches the resolved name it is a suffix of
-- ('relativeNameMatches'): the parser spells a type property's implicit @True@ as the resolved value
-- with its type set, a written @True@ is the relation of its name ('relTypeVal'). The merged form
-- keeps the resolved spelling at each name, so it is within either spelling under every
-- environment. Compared as sets ('isSubPartialOfWithEnv'), two different predicates were each
-- within the other: the @$this@ operand names no type, so both forms read empty.
predExprSameForm :: PartialType -> PartialType -> Maybe PartialType
predExprSameForm = partial
  where
    partial p q = do
      n <- name (ptName p) (ptName q)
      guard (ptArgMode p == ptArgMode q && ptPreds p == ptPreds q)
      vars <- slots (ptVars p) (ptVars q)
      args <- slots (ptArgs p) (ptArgs q)
      return p{ptName = n, ptVars = vars, ptArgs = args}
    slots a b = do
      guard (H.keysSet a == H.keysSet b)
      H.traverseWithKey (\k t -> typ t (b H.! k)) a
    name n m
      | n == m                  = Just n
      | relativeNameMatches n m = Just m
      | relativeNameMatches m n = Just n
      | otherwise               = Nothing
    typ t u
      | t == u = Just t
    typ (UnionType Nothing ls []) (UnionType Nothing ms [])
      | [l] <- splitUnionType ls, [m] <- splitUnionType ms = singletonType <$> partial l m
    typ (TopType negs (PredsOne (PredRel l))) (TopType negs' (PredsOne (PredRel m)))
      | negs == negs' = (\r -> TopType negs (PredsOne (PredRel r))) <$> partial l m
    -- A name's value beside the relation of its name ('relTypeVal'): the value, resolved.
    typ (UnionType Nothing ls []) (TopType negs (PredsOne (PredRel m)))
      | H.null negs, [l] <- splitUnionType ls = singletonType <$> partial l m
    typ t@TopType{} u@(UnionType Nothing _ []) = typ u t
    typ _ _ = Nothing

-- | A value-predicate LITERAL: a 'PredExpr' atom or its negation. A literal names no member of its
-- own and enumerates nothing: it narrows the set the formula's class and relation atoms describe,
-- whatever it means. So it is transparent to every question about what those atoms enumerate
-- (sealedness, exactness, boundedness — 'predsSealed', 'predsPositiveSealed',
-- 'predsPositiveAtoms') and is carried symbolically through an expansion ('expandPredicates').
isValuePredLiteral :: TypePredicates -> Bool
isValuePredLiteral (PredsOne (PredExpr _))            = True
isValuePredLiteral (PredsNot (PredsOne (PredExpr _))) = True
isValuePredLiteral _                                  = False

-- | A positive conjunction without its value-predicate atoms; a formula with a negation is kept.
predsWithoutExprAtoms :: TypePredicates -> TypePredicates
predsWithoutExprAtoms preds = case tryPredsToList preds of
  Just atoms -> case [a | a <- atoms, not (isExprAtom a)] of
    []   -> PredsNone
    [a]  -> PredsOne a
    rest -> mkAnd (map PredsOne rest)
  Nothing -> preds
  where
    isExprAtom PredExpr{} = True
    isExprAtom _          = False

-- | A type without its value predicates at any depth: what a value is held to by its shape, the
-- predicates being what is decided or evaluated on it besides ('Eval.valWithinType').
typeWithoutPredExprs :: Type -> Type
typeWithoutPredExprs (UnionType mTop leafs consts) = UnionType (fmap top mTop) (joinUnionType (map partialWithoutPredExprs (splitUnionType leafs))) consts
  where top (preds, negs, ncs) = (predsWithoutExprAtoms preds, joinUnionType (map partialWithoutPredExprs (splitUnionType negs)), ncs)
typeWithoutPredExprs (TypeVar v l inner) = TypeVar v l (typeWithoutPredExprs inner)

partialWithoutPredExprs :: PartialType -> PartialType
partialWithoutPredExprs p@PartialType{ptVars, ptArgs, ptPreds} =
  p{ptVars = fmap typeWithoutPredExprs ptVars, ptArgs = fmap typeWithoutPredExprs ptArgs, ptPreds = predsWithoutExprAtoms ptPreds}

-- | Whether a type carries a value predicate at any depth, in a positive conjunction.
typeHasPredExpr :: Type -> Bool
typeHasPredExpr = typeHasExprAtom (not . null . predsExprAtoms)

-- | Whether a type carries a value predicate stated of the value alone ('predsValueExprAtoms') at
-- any depth.
typeHasValuePredExpr :: Type -> Bool
typeHasValuePredExpr = typeHasExprAtom (not . null . predsValueExprAtoms)

typeHasExprAtom :: (TypePredicates -> Bool) -> Type -> Bool
typeHasExprAtom has = typeHas
  where
    typeHas (UnionType mTop leafs _) = maybe False (\(preds, negs, _) -> has preds || any partialHas (splitUnionType negs)) mTop || any partialHas (splitUnionType leafs)
    typeHas (TypeVar _ _ inner) = typeHas inner
    partialHas PartialType{ptVars, ptArgs, ptPreds} = has ptPreds || any typeHas ptVars || any typeHas ptArgs

topTypeAsPartials :: TypeEnv tg ->[PartialType]
topTypeAsPartials TypeEnv{teNames} = map ((\p -> p{ptArgMode=PtArgUnbounded}) . partialVal) $ S.toList teNames

-- | Like 'predsSealed', but only the POSITIVE part must be enumerable, and there must be one:
-- the expansion bounds the universe by the positive atoms, so those must be sealed for it to be
-- exact, while a NEGATED atom's class gaining a member in some future file only SHRINKS
-- @U_preds@ — a cover proved against today's members therefore stays valid.
--
-- @'predsSealed'@ is the wrong test for that: it descends through 'PredsNot' as well, so a
-- conjunction like @~ConsList ∧ ¬∀Eq@ would be refused for the unsealed @Eq@ even though the
-- sealed @ConsList@ is the atom that bounds the set.
predsPositiveSealed :: (TypeGraph tg) => TypeEnv tg -> TypePredicates -> Bool
predsPositiveSealed typeEnv = go
  where
    go (PredsOne p)  = atomEnumerable p
    -- PredsNone = PredsAnd [] ↦ False: ⊤ is not enumerable, and neither is ⊤ narrowed by value
    -- predicates alone ('isValuePredLiteral', transparent here).
    go (PredsAnd ps) = case filter (not . isValuePredLiteral) ps of
      []  -> False
      ps' -> all go ps'
    -- Negated: an unsealed class here only shrinks the set — at ODD depth. Under a second
    -- negation it is positive again (@¬(¬Eq ∧ ~Cons) = Eq ∨ ¬~Cons@ grows with Eq), so the atoms
    -- at positive polarity inside must be enumerable too.
    go (PredsNot q)  = posAtomsEnumerable False q
    posAtomsEnumerable pos (PredsOne p)  = not pos || atomEnumerable p
    posAtomsEnumerable pos (PredsAnd ps) = all (posAtomsEnumerable pos) ps
    posAtomsEnumerable pos (PredsNot q)  = posAtomsEnumerable (not pos) q
    -- An atom over a name that is not a CLASS is a DATA name, and its extent is exactly the
    -- values carrying that name — fixed the moment it is declared, so no later file can widen
    -- it. 'predsSealed' reports False there only because it asks the class graph a class
    -- question and gets no answer; sealedness is simply not the right question for a data name.
    atomEnumerable (PredExpr _)    = False
    atomEnumerable p@(PredClass c) = enumerableName (ptName c) p
    atomEnumerable p@(PredRel c)   = enumerableName (ptName c) p
    atomEnumerable (PredConstEq _) = False
    enumerableName n p = not (nameIsClass n) || predsSealed typeEnv (PredsOne p)
    nameIsClass n = any (relativeNameMatches n) (listClassNames typeEnv)

-- | True when every class referenced by the predicates is sealed, so the predicates may be
-- soundly expanded to their declared constituents irrespective of the open/closed-world
-- ('tePrgmEnv') setting. For a sealed class @C@, @U_C@ is exactly the union of @C@'s declared
-- members, so @~C@ and its expansion are equal regardless of any types defined elsewhere.
predsSealed :: (TypeGraph tg) => TypeEnv tg -> TypePredicates -> Bool
predsSealed typeEnv@TypeEnv{teClassGraph=ClassGraph cg} = go
  where
    go (PredsOne p)  = atomSealed p
    -- PredsNone = PredsAnd [] ↦ False; a value predicate ('isValuePredLiteral') references no
    -- class and is transparent, so ⊤ narrowed by value predicates alone is not sealed either.
    go (PredsAnd ps) = case filter (not . isValuePredLiteral) ps of
      []  -> False
      ps' -> all go ps'
    go (PredsNot p)  = go p
    atomSealed (PredClass c)   = classSealed (ptName c)
    atomSealed (PredRel c)     = classSealed (ptName c)
    atomSealed (PredExpr _)    = False
    atomSealed (PredConstEq _) = False
    -- In the program-as-written ('tePrgmEnv') environment every class is closed to its declared
    -- members, so any class predicate may be soundly expanded to its declared constituents.
    classSealed _ | tePrgmEnv typeEnv = True
    classSealed name = case graphLookup (PClassName name) cg of
      Just (CGClass sealed _ _ _) -> sealed
      _ -> case [ s | (CGClass s _ _ _, cKey, _) <- graphToNodes cg, relativeNameMatches name (fromPartialName cKey) ] of
             (s:_) -> s
             []    -> False

-- | An arg- and var-free partial (a bare name).
plainPartial :: PartialType -> Bool
plainPartial p = H.null (ptArgs p) && H.null (ptVars p)

-- | True when class @c1@ is a transitively-declared subclass of @c2@, established by the
-- @every(c1 isa c2)@ edges that record @c1@ as a class-predicate member of @c2@'s class node.
-- This is the "upward" is-a direction. It follows only /class/ members (those whose member type is
-- a bare 'PredClass'), never data members, so it cannot unroll a recursive parameterized member
-- such as @Maybe[$T -> Ord] ∈ Ord@ - which the "downward" member enumeration in 'predImpliesAtom'
-- does, making @Ord ⊆ Eq@ recurse back through @Maybe[Ord] ⊆ Maybe[Eq] ⊆ Eq@ into @Ord ⊆ Eq@.
-- Sound by definition: @every(c1 isa c2)@ asserts @U_c1 ⊆ U_c2@. Restricted to var-free class
-- members so a parameterized edge (@Ordering[$T] isa Eqv[$T]@) is left to the precise path.
classDeclaredSubclassOf :: (TypeGraph tg) => TypeEnv tg -> TypeName -> TypeName -> Bool
classDeclaredSubclassOf TypeEnv{teClassGraph=ClassGraph cg} c1 c2 = go S.empty c2
  where
    go visited cName
      | S.member cName visited = False
      | otherwise = case graphLookup (PClassName cName) cg of
          Just (CGClass _ _ classTypes _) ->
            let subs = [ ptName sc | t <- classTypes, Just sc <- [maybeGetClassSingleton t], H.null (ptVars sc) ]
            in any (\s -> relativeNameMatches c1 s || go (S.insert cName visited) s) subs
          _ -> False

-- | Whether the predicate conjunction is BOUNDED by at least one positive atom, so that its
-- value set is confined to that atom's name-keyed members and a name-materialized expansion can
-- exhaust it. 'PredsNone' and purely negated forms are open-ended — they contain constants of
-- unlisted kinds and (open-world) values of names not yet declared — so no finite leaf expansion
-- is ever exhaustive of them.
--
-- A negated conjunct alongside a positive one does NOT reopen the universe: @A ∧ ¬B ⊆ A@, so the
-- bound still holds and the extra conjunct only shrinks the set. So @(~ConsList ∧ ¬Eq) ∪ Nil@ is
-- expanded exhaustively, and proved a subtype of @(Cons[…]|¬Eq) ∪ Nil@, which spells the same set.
--
-- Exhaustiveness of the atom's own members is a separate question, already answered by the
-- caller's expansion flag: an unsealed class expands approximately and is rejected there.
--
-- A value predicate bounds nothing by name: @⊤ | $this.isZ == True@ is the universe narrowed by a
-- condition, as open-ended as @⊤@ ('isValuePredLiteral').
predsPositiveAtoms :: TypePredicates -> Bool
predsPositiveAtoms (PredsOne (PredExpr _)) = False
predsPositiveAtoms (PredsOne _)            = True
predsPositiveAtoms (PredsNot _)            = False
predsPositiveAtoms (PredsAnd ps)           = any predsPositiveAtoms ps

-- | A predicate formula read at one constant: each 'PredConstEq' atom is decided (the value is that
-- constant or it is not), giving 'Nothing' when the formula is then false and otherwise the formula
-- of the atoms left ('PredsNone' when none are).
predsAtConst :: Constant -> TypePredicates -> Maybe TypePredicates
predsAtConst c (PredsOne (PredConstEq c')) = if c == c' then Just PredsNone else Nothing
predsAtConst _ p@(PredsOne _)              = Just p
predsAtConst c (PredsAnd ps)               = mkAnd . filter (/= PredsNone) <$> mapM (predsAtConst c) ps
predsAtConst c (PredsNot p)                = case predsAtConst c p of
  Nothing        -> Just PredsNone
  Just PredsNone -> Nothing
  Just q         -> Just (PredsNot q)

-- | Whether a predicate formula holds a class\/rel membership atom anywhere (the decidable
-- fragment of the per-leaf predicate rules; a pure-'PredExpr' formula is undecidable either way).
predsHasClassRelAtom :: TypePredicates -> Bool
predsHasClassRelAtom (PredsOne (PredClass _))   = True
predsHasClassRelAtom (PredsOne (PredRel _))     = True
predsHasClassRelAtom (PredsOne (PredExpr _))    = False
predsHasClassRelAtom (PredsOne (PredConstEq _)) = False
predsHasClassRelAtom (PredsAnd ps)              = any predsHasClassRelAtom ps
predsHasClassRelAtom (PredsNot q)               = predsHasClassRelAtom q

-- | The per-leaf predicate decisions of 'reduceNegPredLeaf' currently being made. Deciding @q@ on a
-- bare leaf asks @leaf ⊆ U_q@; when that is outside the engine's fragment, the answer is read from
-- the materialized meet @leaf ∩ ¬q@, whose compaction reduces the same leaf's @¬q@ — the same
-- decision, one level down. A re-entered decision is left undecided: the conjunct stays attached,
-- which spells the same set, so the marker chooses a spelling and never an answer.
{-# NOINLINE negPredDecideInFlightRef #-}
negPredDecideInFlightRef :: InFlight (PartialType, TypePredicates)
negPredDecideInFlightRef = unsafePerformIO newInFlight

decideInFlight :: (PartialType, TypePredicates) -> Maybe Bool -> Maybe Bool
decideInFlight k decision = unsafePerformIO $
  inFlightOn negPredDecideInFlightRef k (noteCycleCut Nothing) (evaluate decision)
{-# NOINLINE decideInFlight #-}

-- | Whether a leaf's value predicate holds on it ('typeGraphPredExprHolds'), the leaf read without
-- its own predicates: they are what is being decided, and a leaf that carries the predicate is
-- the same values as the bare leaf it holds on. Evaluating the predicate reaches through the
-- graph, whose meets compact the leaves they produce; a leaf carrying this same predicate there
-- would re-pose this decision from inside itself, so a re-entered decision is left undecided
-- ('decideInFlight'), which keeps the leaf as it is.
--
-- The predicate is evaluated with no bindings. A refinement's predicate names @$this@ and
-- functions, nothing of the scope it is read in, so its answer is the same wherever the leaf flows
-- and is remembered per reaches env by the leaf and predicate alone (the var env of the question
-- is not part of the key, which would be a different key at every constraint). One that does
-- refer to the scope — a variable, or a name an argument of the definition shadows, which the
-- reach would otherwise read as the function of that name — is left undecided. One that does not
-- name @$this@ at all (an arm's guard, typed with its arguments' types: @a.head == b.head@ on
-- @Char@s) is the same proposition on every value, and is remembered without the leaf.
--
-- An answer is stored whether or not a cycle cut touched it, unlike a subtype answer
-- ('subtypeMemoRef'). A cut keeps a leaf spelled with its predicate or answers a subtype question
-- conservatively, both of which can only over-approximate the result the predicate reaches; an
-- over-approximation within @True@ or disjoint from it says the same of what it covers, so a cut
-- can make an answer 'Nothing', never a wrong 'Just'.
predExprHoldsOn :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> Maybe Bool
predExprHoldsOn typeEnv vaenv leaf e
  | typeContainsTypeVar (singletonType e) || any shadowed (S.toList names) = Nothing
  | any local (S.toList names) = decide
  | otherwise = case teDecisionKey typeEnv <|> teQueryCacheKey typeEnv of
  Nothing -> decide
  Just tok -> unsafePerformIO $ do
    _ <- evaluate (forcePartialType bare `seq` forcePartialType e)
    let key = (tok, (teDisableCompact typeEnv, tePrgmEnv typeEnv, teArgModeProjection typeEnv), keyLeaf, e)
    gens <- readIORef predDecideMemoRef
    case lookup tok gens >>= H.lookup key of
      Just (r, deps) -> noteReadDeps deps >> return r
      Nothing -> do
        (r, deps) <- memoReading typeEnv $ do
          r <- evaluate decide
          _ <- evaluate (maybe () (`seq` ()) r)
          return r
        -- More generations than the query memos keep: the per-epoch tokens rotate past the
        -- page's static env, whose decisions are asked throughout, and the maps are small.
        atomicModifyIORef' predDecideMemoRef $ \gens2 ->
          let mTok = fromMaybe H.empty (lookup tok gens2)
              rest = filter ((/= tok) . fst) gens2
          in ((tok, H.insert key (r, deps) mTok) : take 7 rest, ())
        return r
  where
    bare = leaf{ptPreds=PredsNone}
    names = partialRefNamesDeep e
    shadowed n = H.member (extKey (TVArg (partialKey n))) vaenv
    local n = any (\l -> relativeNameMatches n l || relativeNameMatches l n) (S.toList (teLocalNames typeEnv))
    mentionsThis = any ((== macroThisKey) . nameStr) (S.toList names)
    keyLeaf = if mentionsThis then Just bare else Nothing
    inFlightLeaf = fromMaybe e keyLeaf
    decide = decideInFlight (inFlightLeaf, PredsOne (PredExpr e)) (typeGraphPredExprHolds typeEnv H.empty bare e)

type PredDecideMemoKey = (Int, (Bool, Bool, Bool), Maybe PartialType, PartialType)

{-# NOINLINE predDecideMemoRef #-}
predDecideMemoRef :: IORef [(Int, H.HashMap PredDecideMemoKey (Maybe Bool, ReadDeps))]
predDecideMemoRef = unsafePerformIO (newIORef [])

-- | Collect the 'PredRel' partials nested anywhere within a partial's args/vars.
relPartialsIn :: PartialType -> [PartialType]
relPartialsIn PartialType{ptArgs, ptVars} = concatMap relPartialsInType (H.elems ptArgs ++ H.elems ptVars)

relPartialsInType :: Type -> [PartialType]
relPartialsInType (UnionType mtop pos _) = topRels ++ concatMap relPartialsIn (splitUnionType pos)
  where topRels = case mtop of
          Just (preds, _, _) -> predRelPartials preds
          Nothing            -> []
relPartialsInType _ = []

predRelPartials :: TypePredicates -> [PartialType]
predRelPartials (PredsOne (PredRel p)) = [p]
predRelPartials (PredsAnd ps)          = concatMap predRelPartials ps
predRelPartials (PredsNot p)           = predRelPartials p
predRelPartials _                      = []
