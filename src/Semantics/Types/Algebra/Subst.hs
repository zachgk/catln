-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.Algebra.Subst
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Type-variable substitution: replacing type variables with their bindings in a 'TypeVarArgEnv'
-- and merging variable environments.
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Semantics.Types.Algebra.Subst where

import                          Data.Hashable
import                qualified Data.HashMap.Strict            as H
import                          Data.Maybe
import {-# SOURCE #-}           Semantics.Types.Algebra.SetOps (intersectTypesWithVarEnv,
                                                                meetInner,
                                                                unionAllTypesWithEnv)
import                          Semantics.Types.ArgModes
import                          Semantics.Types.Construct
import                          Semantics.Types.Data
import                          Semantics.Types.Vars


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
