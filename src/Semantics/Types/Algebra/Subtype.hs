-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.Algebra.Subtype
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Subtyping: 'isSubtypeOfWithEnv' and 'isSubPartialOfWithEnv', and the constant coverage they
-- rely on.
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

module Semantics.Types.Algebra.Subtype where

import                          Control.Exception               (evaluate)
import                          Control.Monad                   (when)
import                          Data.Either
import                          Data.Hashable                   (Hashed, hashed)
import                qualified Data.HashMap.Strict             as H
import                qualified Data.HashSet                    as S
import                          Data.IORef                      (IORef,
                                                                 atomicModifyIORef',
                                                                 newIORef,
                                                                 readIORef)
import                qualified Data.List                       as L
import                          Data.Maybe
import                          Debug.Trace                     (trace)
import {-# SOURCE #-}           Semantics.Types.Algebra.Compact (compactPartialsWithClassPred,
                                                                 compactType,
                                                                 constantWithinTop,
                                                                 excludeConstants)
import                          Semantics.Types.Algebra.Expand
import                          Semantics.Types.Algebra.Preds
import                          Semantics.Types.Algebra.Sat
import {-# SOURCE #-}           Semantics.Types.Algebra.SetOps  (differenceTypeWithEnv,
                                                                 intersectTypesEnv,
                                                                 intersectTypesWithVarEnv,
                                                                 meetInner,
                                                                 partialEscapes,
                                                                 unionAllTypes,
                                                                 unionAllTypesWithEnv,
                                                                 unionTypesWithEnv)
import                          Semantics.Types.Algebra.Subst
import                          Semantics.Types.ArgModes
import                          Semantics.Types.Construct
import                          Semantics.Types.Data
import                          Semantics.Types.Memo
import                          Semantics.Types.Name
import                          Semantics.Types.Preds
import                          Semantics.Types.SatGoal
import                          Semantics.Types.Vars
import                          System.IO.Unsafe                (unsafePerformIO)
import                          Text.Printf
import                          Utils


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
type SubPartialKey = (Bool, Bool, TypeVarArgEnv, PartialType, PartialType)

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
  let k = (tePrgmEnv typeEnv, teArgModeProjection typeEnv, vaenv, sub, super)
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
type SubtypeMemoKey = (Int, (Bool, Bool, Bool), TypeVarArgEnv, Type, Type)

{-# NOINLINE subtypeMemoRef #-}
subtypeMemoRef :: IORef [(Int, H.HashMap (Hashed SubtypeMemoKey) (Bool, ReadDeps))]
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
    _ <- evaluate (forceType t1 `seq` forceType t2 `seq` forceTypeMap vaenv)
    -- Hashed once, for the lookup and the store both.
    key <- evaluate (hashed (tok, (teDisableCompact typeEnv, tePrgmEnv typeEnv, teArgModeProjection typeEnv), vaenv, t1, t2))
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

-- | The compiler's own equality: fast and sound but INCOMPLETE (per-leaf covers only — a
-- False may mean "unproven"). Used inside the algebra's own recursion (compaction,
-- 'eqScheme' convergence), where a miss costs a respelling or an epoch, never wrongness.
-- Set-semantic equality ASSERTIONS (the algebra laws) must use 'isEqTypeWithEnvComplete'.
isEqTypeWithEnv :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Bool
isEqTypeWithEnv _ _ a b | a == b = True
isEqTypeWithEnv typeEnv vaenv a b = isSubtypeOfWithEnv typeEnv vaenv a b && isSubtypeOfWithEnv typeEnv vaenv b a
