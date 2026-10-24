-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.Access
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Reading and updating the arguments and type properties of a type ('typeGetArg',
-- 'updateTypeProp').
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Semantics.Types.Access where

import qualified Data.HashMap.Strict             as H
import qualified Data.HashSet                    as S
import           Data.Maybe
import           Data.Zip
import           Prelude                         hiding (unzip)
import           Semantics.Types.Algebra.Compact
import           Semantics.Types.Algebra.SetOps
import           Semantics.Types.Algebra.Subst
import           Semantics.Types.Construct
import           Semantics.Types.Data
import           Semantics.Types.Ops
import           Semantics.Types.Vars
import           Text.Printf


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
