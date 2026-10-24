-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.ArgModes
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Argument modes ('PtArgMode'): which arguments a partial requires, allows or leaves open, and
-- how two modes overlap or cover each other.
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns    #-}

module Semantics.Types.ArgModes where

import           CtConstants
import qualified Data.HashMap.Strict       as H
import qualified Data.HashSet              as S
import qualified Data.List                 as L
import           Data.Maybe
import           Semantics.Types.Construct
import           Semantics.Types.Data
import           Semantics.Types.Name
import           Text.Printf
import           Utils


containsBottomType :: Type -> Bool
containsBottomType BottomType = True
containsBottomType (UnionType (Just _) _ _) = False
containsBottomType (TypeVar _ _ _) = False
containsBottomType (UnionType Nothing leafs _) = any containsBottomPartialType $ splitUnionType leafs

-- | Whether a partial is empty for an empty slot: a type parameter, or an arg every instance has
-- ('requiredArgsOf'). An optional arg typed ∅ is one an instance does not have ('PtArgMode').
containsBottomPartialType :: PartialType -> Bool
containsBottomPartialType p@PartialType{ptVars} = any containsBottomType (requiredArgsOf p) || any containsBottomType ptVars

-- | Whether a relative reference's applied arg keys are realizable under a candidate name:
-- some declared arg-set of the name covers them ("every realizable arg-set is a subset of a
-- declared one" — 'declaredArgKeys'; partial application never adds args). Grafting the
-- reference's args onto every name-matched candidate would otherwise fabricate uninhabitable
-- forms: a relative @insert(m=…, key=…)@ also matches a nested @/sort./insert@ whose objects
-- declare only @(x, lst)@, and the grafted @/sort./insert(m=…, key=…)@ admits no value but would
-- ride every union built from the reference, where dispatch finds no arrow for it.
-- The check is purely structural (parse-time arg-name sets, never solving-state types), so
-- it is stable across epochs; an undeclared name (open world) is never refused.
relArgKeysFeasible :: TypeGraph tg => TypeEnv tg -> PartialType -> TypeName -> Bool
relArgKeysFeasible typeEnv relPartial n = case declaredArgKeys typeEnv n of
  Nothing  -> True
  Just kss -> any (H.keysSet (ptArgs relPartial) `S.isSubsetOf`) kss

-- | The args every instance of a partial has: where the lower bound is set, the listed args less
-- the optional ones ('PtArgMode').
ptRequiredArgs :: PartialType -> S.HashSet ArgName
ptRequiredArgs PartialType{ptArgs, ptArgMode=PtArgMode lb _ opt}
  | lb = H.keysSet ptArgs `S.difference` opt
  | otherwise = S.empty

-- | The listed args every instance of a partial has, with their types ('ptRequiredArgs'). Only
-- these can make a partial empty: an optional arg typed ∅ is one an instance does not have.
requiredArgsOf :: PartialType -> H.HashMap ArgName Type
requiredArgsOf p = H.filterWithKey (\k _ -> S.member k (ptRequiredArgs p)) (ptArgs p)

-- | The listed args an instance of a partial may lack: typed where present ('PtArgMode').
ptOptionalArgs :: PartialType -> S.HashSet ArgName
ptOptionalArgs p = H.keysSet (ptArgs p) `S.difference` ptRequiredArgs p

-- | A partial's mode with these args optional, kept canonical ('PtArgMode'): only listed args,
-- and none without a lower bound.
withOptionalArgs :: TypeGraph tg => TypeEnv tg -> S.HashSet ArgName -> PartialType -> PartialType
withOptionalArgs typeEnv opt p = canonicalArgOpt typeEnv p{ptArgMode=(ptArgMode p){ptArgOpt=opt}}

-- | A partial with this listed arg required: its other listed args keep whether they are.
--
-- A partial with no lower bound is an interval of arg-sets, so it is lower-bounded here on the arg
-- alone ('intervalLowerBound'): the declared fields it does not list stay free, rather than being
-- read in as the floor a spread of the type requires.
requireArg :: TypeGraph tg => TypeEnv tg -> ArgName -> PartialType -> PartialType
requireArg typeEnv k p@PartialType{ptArgMode=PtArgMode lb ub opt}
  | lb = canonicalArgOpt typeEnv p{ptArgMode=PtArgMode True ub (S.delete k opt)}
  | otherwise = canonicalArgOpt typeEnv $ intervalLowerBound typeEnv p{ptArgMode=PtArgMode True ub (S.delete k (H.keysSet (ptArgs p)))}

-- | A partial an operation on arg-set INTERVALS has just given a lower bound: requiring some args of
-- an interval that had no floor, as a difference's piece does (@F(x=X)..??@ less the @F@ lacking
-- @x@) or a meet with a lower-bounded side does.
--
-- Lower-bounded and upper-unbounded is also how a spread is spelled, which reads the declared fields
-- it does not list in as required ('spreadFloor'). Those fields were free in the interval, so they
-- are listed here as optional and typed ⊤ — present or not, with any value — which is what keeps
-- the floor from being read in. A field already listed keeps what it says, and a partial with an
-- upper bound has no floor to read in.
intervalLowerBound :: TypeGraph tg => TypeEnv tg -> PartialType -> PartialType
intervalLowerBound typeEnv p@PartialType{ptName, ptArgs, ptArgMode=PtArgMode lb ub opt}
  | lb && not ub && not (H.null free) = p{ptArgs = H.union ptArgs free, ptArgMode = PtArgMode lb ub (opt `S.union` H.keysSet free)}
  | otherwise = p
  where free = PTopType <$ (declaredFloorArgs typeEnv ptName `H.difference` ptArgs)

-- | Keeps a partial's optional args canonical without changing its value set ('PtArgMode'):
--
--   * an optional arg is a listed one;
--   * without an upper bound, an optional arg typed ⊤ says nothing an unlisted one does not, so
--     it is unlisted — unless it is one of the declared fields a spread reads in as required
--     ('declaredFloorArgs'), where listing it optional is what says the partial does not
--     require it;
--   * with an upper bound, an optional arg typed ∅ is one no instance has, and an unlisted one is
--     no more allowed, so it is unlisted;
--   * a lower bound requiring none of the listed args is no lower bound: every listed arg is
--     optional already, so the set is empty — unless it is a spread's, whose unlisted declared
--     fields it still requires.
canonicalArgOpt :: TypeGraph tg => TypeEnv tg -> PartialType -> PartialType
canonicalArgOpt typeEnv p@PartialType{ptName, ptArgs, ptArgMode=PtArgMode lb ub opt0}
  | S.null opt0 = p
  | not lb = p{ptArgMode=PtArgMode lb ub S.empty}
  | S.null (H.keysSet args' `S.difference` opt), ub || floorListed = p{ptArgs=args', ptArgMode=PtArgMode False ub S.empty}
  | otherwise = p{ptArgs=args', ptArgMode=PtArgMode lb ub opt}
  where
    floorArgs = declaredFloorArgs typeEnv ptName
    floorListed = all (`H.member` args') (H.keys floorArgs)
    listedOpt = opt0 `S.intersection` H.keysSet ptArgs
    vacuous k = case H.lookup k ptArgs of
      Just t | not ub -> t == PTopType && not (H.member k floorArgs)
             | otherwise -> isBottomType t
      Nothing -> True
    dropped = S.filter vacuous listedOpt
    opt = listedOpt `S.difference` dropped
    args' = H.filterWithKey (\k _ -> not (S.member k dropped)) ptArgs

-- | Keyset feasibility of two arg-set intervals intersecting (see 'PtArgMode' and
-- 'intersectPartialsBase'): the intersected interval @[req, cap]@ must be non-empty.
argModesOverlap :: PartialType -> PartialType -> Bool
argModesOverlap a@PartialType{ptArgs=aArgs, ptArgMode=PtArgMode _ aUb _} b@PartialType{ptArgs=bArgs, ptArgMode=PtArgMode _ bUb _} =
  not ub' || reqKeys `S.isSubsetOf` capKeys
  where
    allKeys = H.keysSet aArgs `S.union` H.keysSet bArgs
    reqKeys = ptRequiredArgs a `S.union` ptRequiredArgs b
    ub' = aUb || bUb
    capKeys = (if aUb then H.keysSet aArgs else allKeys) `S.intersection` (if bUb then H.keysSet bArgs else allKeys)

-- | The declared arg-keysets bounding a name's realizable arg-sets, when known.
-- Object declarations come from 'typeGraphArgKeys'. A CLASS name has no object, but its
-- positive-partial form (the class placeholder) is exactly the bare argless reference —
-- a parameterized class carries type parameters in 'ptVars', never 'ptArgs' — so its only
-- realizable arg-set is ∅; likewise the synthetic 'classPlaceholderLeaf' marker name,
-- which is only ever built argless. A name bound only in an argument pattern — a record
-- field, a function-typed parameter — declares the keysets its patterns write
-- ('teBoundArgKeys'). An unknown name (none of these) is open world.
declaredArgKeys :: TypeGraph tg => TypeEnv tg -> TypeName -> Maybe [S.HashSet ArgName]
declaredArgKeys typeEnv name = case typeGraphArgKeys typeEnv name of
  Just kss -> Just kss
  Nothing | typeGraphNameOpen typeEnv name -> Nothing
          | H.member name (scClasses (teSatCache typeEnv)) -> Just [S.empty]
          | nameStr name == classPlaceholderStr -> Just [S.empty]
          | Just kss <- H.lookup name (teBoundArgKeys typeEnv) -> Just kss
          | otherwise -> Nothing

-- | Drops vacuous arg-mode interval bounds (see 'PtArgMode'), canonicalizing the mode
-- without changing the partial's value set:
--
--   * an unset lower bound is vacuous with no listed args (the interval floor is ∅ either way);
--   * an unset upper bound is vacuous when no declared arg-set strictly contains the listed
--     args ('declaredArgKeys'): every realizable arg-set is a subset of some declared one, so
--     no realizable arg-set lies strictly above the listed args and the interval collapses to
--     its floor. This is what makes e.g. @/length(this=..)..@ ('relaxNameBoundArgs' widening)
--     one set with @/length(this=..)@ — @/length@ declares no larger arg-set to relax into.
--
-- The spread widens only the UPPER bound ('PtArgAny'). A lower-bounded, upper-unbounded partial
-- of a DATA type still requires what the data definition declares, so the declared fields it
-- does not list are read in with their declared types and the interval's floor is listed ∪
-- declared:
-- @Plain..@ over @data(Plain(name -> String, other -> String))@ is exactly
-- @Plain(name=String, other=String)@, the values of the type — not also the partial
-- applications @Plain@, @Plain(name)@, @Plain(other)@, which are the exact spellings and the
-- 'PtArgUnbounded' interval. A bare exact name stays the argument-less value it is. This is a
-- READ of what @..@ means, done where the ceiling is read; nothing stores it, and every
-- partial-level operation reads its operands through it at entry.
--
-- An unset lower bound is therefore vacuous with no listed args only under an upper bound:
-- 'PtArgUnbounded' keeps its floor at ∅ rather than becoming the spread.
--
-- An unknown name (open world) keeps its bounds. Exact partials return instantly.
normalizePtArgMode :: TypeGraph tg => TypeEnv tg -> PartialType -> PartialType
normalizePtArgMode typeEnv p0@PartialType{ptName, ptArgMode=PtArgMode lb ub opt}
  | lb' && ub' && S.null opt = if ptArgMode p == PtArgExact then p else p{ptArgMode=PtArgExact}
  | otherwise = canonicalArgOpt typeEnv p{ptArgMode=PtArgMode lb' ub' opt}
  where
    p@PartialType{ptArgs} = spreadFloor typeEnv p0
    -- Read against the CLOSED ceiling: an unbounded @X..??@ of a name declaring no args is closed
    -- to @X??@ (no larger declared arg-set), whose floor ∅ is its only arg-set, so it is exactly @X@.
    lb' = lb || (H.null ptArgs && ub')
    ub' = ub || case declaredArgKeys typeEnv ptName of
      Just kss -> not $ any (H.keysSet ptArgs `strictSubsetOf`) kss
      Nothing  -> False
    strictSubsetOf a b = S.size a < S.size b && a `S.isSubsetOf` b

-- | Reads a spread's declared floor in: a lower-bounded, upper-unbounded partial ('PtArgAny')
-- of a data type lists the declared fields it does not spell, with their declared types (a
-- field every data definition of the name declares; the first spelling is used). The mode is
-- untouched — the spread still admits extra args, which is what an undeclared context
-- argument (@ContextIn(value=.., io=..)@ against @ContextIn(value=$V)..@) relies on; only
-- 'normalizePtArgMode' also closes a vacuous ceiling. Every partial-level operation reads its
-- operands through this at entry, so the floor takes part in the arg comparisons and meets
-- like any listed field.
spreadFloor :: TypeGraph tg => TypeEnv tg -> PartialType -> PartialType
spreadFloor typeEnv p@PartialType{ptName, ptArgs, ptArgMode=PtArgMode lb ub _}
  | lb && not ub, not (H.null floorArgs) = p{ptArgs = H.union ptArgs floorArgs}
  | otherwise = p
  where
    -- Only the fields the partial does not list: one it lists as optional ('ptArgOpt') is a
    -- partial application's, which a value may lack (@Cons(head=X)@ before its @tail@).
    floorArgs = declaredFloorArgs typeEnv ptName `H.difference` ptArgs

-- | The declared fields a spread of a name reads in as its floor ('spreadFloor'): a field every
-- data definition of the name declares, with the type the first definition gives it. None for a
-- name that declares no data.
declaredFloorArgs :: TypeGraph tg => TypeEnv tg -> TypeName -> H.HashMap ArgName Type
declaredFloorArgs typeEnv name = case [dArgs | PartialType{ptArgs=dArgs} <- typeGraphData typeEnv name, not (H.null dArgs)] of
  []   -> H.empty
  d:ds -> foldl (H.intersectionWith const) d ds

-- | The finite list of arg-keysets a partial's arg-mode interval admits, when enumerable:
-- an upper-bounded interval enumerates the subsets of its listed args directly; an
-- upper-unbounded one is bounded by the name's declared arg-sets ('declaredArgKeys' —
-- every realizable arg-set is a subset of a declared one). 'Nothing' only for an
-- upper-unbounded interval on an unknown name (open world).
ptIntervalArgKeys :: TypeGraph tg => TypeEnv tg -> PartialType -> Maybe [S.HashSet ArgName]
ptIntervalArgKeys typeEnv p@PartialType{ptName, ptArgs, ptArgMode=PtArgMode _ ub _}
  | ub = Just $ subsetsBetween lo aKeys
  | otherwise = do
      kss <- declaredArgKeys typeEnv ptName
      return $ L.nub $ concat [subsetsBetween lo ks | ks <- kss, lo `S.isSubsetOf` ks]
  where
    aKeys = H.keysSet ptArgs
    lo = ptRequiredArgs p
    subsetsBetween bottom hi = [bottom `S.union` S.fromList extra | extra <- powerset $ S.toList $ hi `S.difference` bottom]

-- | Restricts a moded partial to one arg-set of its interval: the exact-mode partial with
-- precisely those args, keeping the listed values and leaving unlisted args unconstrained (⊤).
ptAtArgKeys :: PartialType -> S.HashSet ArgName -> PartialType
ptAtArgKeys p@PartialType{ptArgs} ks = p{ptArgs=args', ptArgMode=PtArgExact}
  where args' = H.mapWithKey (\k _ -> H.lookupDefault PTopType k ptArgs) (S.toMap ks)

-- | Keyset coverage: every arg-set allowed by @sub@ is allowed by @sup@
-- (the @negCoversAllPos@ reasoning of 'differencePartialLeafs').
argModeCovers :: PartialType -> PartialType -> Bool
argModeCovers sup@PartialType{ptArgs=supArgs, ptArgMode=PtArgMode _ supUb _} sub@PartialType{ptArgs=subArgs, ptArgMode=PtArgMode _ subUb _} =
  lbOk && ubOk
  where
    supKeys = H.keysSet supArgs
    subKeys = H.keysSet subArgs
    -- Every arg @sup@ requires, @sub@ requires too.
    lbOk = ptRequiredArgs sup `S.isSubsetOf` ptRequiredArgs sub
    ubOk = not supUb || (subUb && subKeys `S.isSubsetOf` supKeys)

-- | Whether a partial BINDS a parameter name: it spells the parameter, or its declaration (data or
-- class) has it. An unspelled parameter is ⊤ (see 'PartialType') — @/Cons@ is @/Cons[/T=⊤]@ — so
-- it is still a binder, flexible per value.
partialBindsParam :: TypeGraph tg => TypeEnv tg -> PartialType -> TypeVarName -> Bool
partialBindsParam typeEnv p n =
  H.member n (ptVars p)
  || any (H.member n . ptVars) (typeGraphData typeEnv (ptName p))
  || maybe False (H.member n . ptVars) (classDeclPartial typeEnv (ptName p))

-- | The parameters a partial's declaration (data or class) has, each at the ⊤ an unspelled
-- parameter denotes.
declaredParamsAtTop :: TypeGraph tg => TypeEnv tg -> PartialType -> H.HashMap TypeVarName Type
declaredParamsAtTop typeEnv p = PTopType <$ H.unions (map ptVars (typeGraphData typeEnv (ptName p) ++ maybe [] pure (classDeclPartial typeEnv (ptName p))))

-- | The declaration partial of a class name, when the name is a class.
classDeclPartial :: TypeEnv tg -> TypeName -> Maybe PartialType
classDeclPartial TypeEnv{teClassGraph=ClassGraph cg} n = case graphLookup (PClassName n) cg of
  Just (CGClass _ p _ _) -> Just p
  _                      -> Nothing

-- | The env a traversal carries INTO a partial: the incoming env with every parameter the partial
-- binds ('partialBindsParam') removed. The traversal then installs what the partial spells.
--
-- A 'TVInt' reference names a variable of the SAME partial ('TypeVarLoc'), and since
-- 'tagParentVarRefs' spells every reference to an enclosing partial 'TVParent', that is true of
-- every 'TVInt' the desugarer produces. So nothing below a partial may read a name it binds from
-- an enclosing scope: a spelled value is installed; an unspelled parameter, or a spelled one whose
-- value cannot be installed (a self-entry @/T=$T@), is the partial's own flexible ⊤, which an
-- unbound name reads as. Carrying the incoming binding through instead would capture the
-- reference: an element @/Cons..@ read through its spread floor carries @head=$/T@, Cons's own
-- @/T@, and compared inside a parameterized class's @C[/T=…]@ it would read the CLASS's element,
-- so whether @C[/T=a] ⟹ C[/T=a∪b]@ holds would depend on what the class parameter is named.
shadowBoundParams :: TypeGraph tg => TypeEnv tg -> [PartialType] -> TypeVarArgEnv -> TypeVarArgEnv
shadowBoundParams typeEnv ps = H.filterWithKey (\(VarKey _ v) -> keep v)
  where keep (TVVar n) _ = not (any (\p -> partialBindsParam typeEnv p n) ps)
        keep _ _         = True

-- | What a cover comparison reads for a parameter the SUB does not spell, given the super's value
-- for it. An unspelled parameter is the ⊤ it denotes ('PartialType'), except a HOP parameter: an
-- input object's link to the enclosing partial's variable of that name (@fn[/A=$^A](v=$/A)@,
-- 'hopInputObject'). It carries the enclosing binding into the object's arguments and says
-- nothing of its own, so an input object that does not spell it (a call's own @fn(v=Integer)@) has
-- the same link: read as ⊤ it would fall outside the super's @$^A@ wherever the enclosing @/A@ is
-- bound, though both accept the same values.
unspelledSubParam :: TypeVarName -> Type -> Type
unspelledSubParam k (TypeVar v@(TVVar k') TVParent _) | k' == k = TypeVar v TVParent PTopType
unspelledSubParam _ _ = PTopType

-- | Spreads a type by making it use PtArgAny mode
spreadType :: TypeVarArgEnv -> Type -> Type
spreadType vaenv = setArgMode vaenv PtArgAny

setArgMode :: TypeVarArgEnv -> PtArgMode -> Type -> Type
setArgMode _ mode (UnionType Nothing leafs []) =
  UnionType Nothing (joinUnionType $ map (\p -> p{ptArgMode=mode}) $ splitUnionType leafs) []
setArgMode vaenv mode (TypeVar v _ _) = setArgMode vaenv mode (fromJust $ H.lookup (extKey v) vaenv)
setArgMode _ _ PTopType = PTopType
setArgMode _ mode (UnionType (Just (PredsOne (PredRel n), negPartials, [])) (H.null -> True) []) =
  UnionType (Just (PredsOne $ PredRel n{ptArgMode=mode}, negPartials, [])) H.empty []
setArgMode _ _ t@(UnionType (Just (PredsOne PredClass{}, _, [])) (H.null -> True) []) = t
setArgMode _ _ t = error $ printf "Unimplemented setArgMode for %s" (show t)
