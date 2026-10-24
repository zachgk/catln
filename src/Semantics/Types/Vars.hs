-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.Vars
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Type-variable scoping helpers that do not need the type algebra: free and outer variable
-- references, parent and sibling references, and self references in declarations.
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Semantics.Types.Vars where

import           Data.Bifunctor            (Bifunctor (first))
import qualified Data.HashMap.Strict       as H
import qualified Data.HashSet              as S
import           Data.List                 (isPrefixOf)
import           Data.Maybe
import           Semantics.Types.Construct
import           Semantics.Types.Data
import           Semantics.Types.Name
import           Semantics.Types.Preds
import           Text.Printf


-- | Splits a partial type by expanding any union-typed vars or args into separate partials.
-- For example, P[$T=(A + B)] splits into [P[$T=A], P[$T=B]].
-- Similarly, P(x=(A + B)) splits into [P(x=A), P(x=B)].
-- A partial with no union-typed vars or args returns [itself] unchanged.
splitPartialByUnionVarArgs :: PartialType -> [PartialType]
splitPartialByUnionVarArgs p@PartialType{ptVars, ptArgs} =
    case findUnion allVarArgs of
      Nothing -> [p]
      Just (keyName, components) -> concatMap splitPartialByUnionVarArgs
        [typeSetAux keyName component p | component <- components]
  where
    allVarArgs = map (first TVVar) (H.toList ptVars) ++ map (first TVArg) (H.toList ptArgs)

    findUnion :: [(TypeVarAux, Type)] -> Maybe (TypeVarAux, [Type])
    findUnion [] = Nothing
    findUnion ((keyName, t):rest) = case unionComponents t of
      Just comps@(_:_:_) -> Just (keyName, comps)
      _                  -> findUnion rest

    -- Splits a union-valued arg\/var into its members as separate single-member types, so an arg
    -- whose value is a union (e.g. from joining two same-key partials during a union) can be matched
    -- component-by-component against the supertypes. Handles the plain positive form and the mixed
    -- 'Just'-pred form (a predicated top plus positive leafs).
    unionComponents :: Type -> Maybe [Type]
    unionComponents (UnionType Nothing leafs []) = Just $ map singletonType $ splitUnionType leafs
    unionComponents (UnionType (Just topData) posLeafs cs)
      | not (H.null posLeafs) = Just $ UnionType (Just topData) H.empty cs : map singletonType (splitUnionType posLeafs)
    -- A disjunctive predicate is a union too, spelled as a formula: @U_{¬(q₁ ∧ … ∧ qₙ)} − N@ is
    -- @⋃ᵢ (U_{¬qᵢ} − N)@ (De Morgan), which is how a union of two classes @∀A ∪ ∀B@ is spelled
    -- (@⊤|¬(¬A ∧ ¬B)@). Splitting it lets @f(x=∀A ∪ ∀B)@ be covered by a type that holds
    -- @f(x=∀A)@ and @f(x=∀B)@ as separate leafs, which a comparison of the whole slot cannot see.
    unionComponents (UnionType (Just (PredsNot (PredsAnd qs@(_:_:_)), negLeafs, negConsts)) posLeafs [])
      | H.null posLeafs = Just [UnionType (Just (predsNot q, negLeafs, negConsts)) H.empty [] | q <- qs]
    unionComponents _ = Nothing

-- | Sets a 'TypeVarAux'-keyed binding inside every partial of a 'Type'.
-- Handles 'UnionType Nothing' (set in each leaf partial) and 'TopType' wrapping
-- a 'PredRel'/'PredClass' (set in the singleton predicate partial).  Var/arg are
-- handled uniformly via 'typeSetAux' (defined later) — the binding location is
-- decided purely by the 'TypeVarAux' tag.
typeSetAuxType :: TypeVarAux -> Type -> Type -> Type
typeSetAuxType aux val (UnionType Nothing leafs []) =
  UnionType Nothing (joinUnionType $ map (typeSetAux aux val) $ splitUnionType leafs) []
typeSetAuxType aux val (TopType negPartials (PredsOne (PredRel p))) =
  TopType negPartials (PredsOne $ PredRel (typeSetAux aux val p))
typeSetAuxType (TVVar k) val (TopType negPartials (PredsOne (PredClass p@PartialType{ptVars}))) =
  TopType negPartials (PredsOne $ PredClass p{ptVars=H.insert k val ptVars})
typeSetAuxType aux val tp = error $ printf "Unimplemented typeSetAuxType for %s.%s = %s" (show tp) (show aux) (show val)

-- | Sets an arg-typed binding inside every partial of a 'Type'.
typeSetArg :: ArgName -> Type -> Type -> Type
typeSetArg argName = typeSetAuxType (TVArg argName)

-- | Sets a var-typed binding inside every partial of a 'Type'.
typeSetVar :: TypeVarName -> Type -> Type -> Type
typeSetVar varName = typeSetAuxType (TVVar varName)

-- | Reads a value DETACHED from the partial its parent references name: every 'TVParent'
-- occurrence, at any depth, reads as the ⊤ it denotes there ('TypeVarLoc' — a parent reference is
-- read as ⊤ exactly when genuinely detached from its parent).
--
-- For a caller holding a declaration's spelling with no enclosing partial in view. Reading
-- @∀List[/T=$^/A]@ that way is what lets a meet against a concrete operand narrow it and recover
-- the binding (@∩ Cons[/T=Integer]@ ⇒ @/A=Integer@); left
-- symbolic the meet keeps the variable and nothing is discovered. 'TVInt' and 'TVExt' references
-- are untouched: they name scopes that are not the one being detached from.
detachParentRefs :: Type -> Type
detachParentRefs (TypeVar _ TVParent i) = detachParentRefs i
detachParentRefs (TypeVar v loc i) = TypeVar v loc (detachParentRefs i)
detachParentRefs (UnionType mtop pos consts) =
  UnionType (fmap onTop mtop) (joinUnionType $ map onLeaf $ splitUnionType pos) consts
  where
    onTop (preds, negLeafs, ncs) =
      (mapTypePreds (mapTypePred onLeaf) preds, joinUnionType $ map onLeaf $ splitUnionType negLeafs, ncs)
    onLeaf p@PartialType{ptVars, ptArgs, ptPreds} = p{
        ptVars  = fmap detachParentRefs ptVars,
        ptArgs  = fmap detachParentRefs ptArgs,
        ptPreds = mapTypePreds (mapTypePred onLeaf) ptPreds
      }

-- | 'detachParentRefs' restricted to the value's OWN level: a bare parent reference, and those in the
-- 'ptVars' values of the value's top partials (which sit at the same level). References inside a
-- partial's arguments name that partial and are left alone.
detachLevelParentRefs :: Type -> Type
detachLevelParentRefs (TypeVar _ TVParent i) = detachLevelParentRefs i
detachLevelParentRefs (TypeVar v loc i) = TypeVar v loc (detachLevelParentRefs i)
detachLevelParentRefs (UnionType mtop pos consts) =
  UnionType (fmap onTop mtop) (joinUnionType $ map onLeaf $ splitUnionType pos) consts
  where
    onTop (preds, negLeafs, ncs) =
      (mapTypePreds (mapTypePred onLeaf) preds, joinUnionType $ map onLeaf $ splitUnionType negLeafs, ncs)
    onLeaf p@PartialType{ptVars} = p{ptVars = fmap detachLevelParentRefs ptVars}

-- | 'detachLevelParentRefs' for the parent references that do NOT name one of the given
-- parameters. A value read at the level of a partial that binds @names@ spells that partial's
-- parameters as parent references, correctly; a parent reference to any OTHER name cannot be
-- naming that partial, so it was written for a different level and has moved — its binder is
-- out of reach and it reads as the ⊤ it denotes.
detachForeignParentRefs :: S.HashSet TypeVarName -> Type -> Type
detachForeignParentRefs names = go
  where
    go (TypeVar (TVVar n) TVParent i) | not (S.member n names) = go i
    go (TypeVar v loc i) = TypeVar v loc (go i)
    go (UnionType mtop pos consts) = UnionType (fmap onTop mtop) (joinUnionType $ map onLeaf $ splitUnionType pos) consts
    onTop (preds, negLeafs, ncs) = (mapTypePreds (mapTypePred onLeaf) preds, joinUnionType $ map onLeaf $ splitUnionType negLeafs, ncs)
    onLeaf p@PartialType{ptVars} = p{ptVars = fmap go ptVars}

-- | Reads a partial's FREE var references as ⊤: a bare partial-local ('TVInt') reference
-- in one of the partial's values names the partial's own var\/arg scope — and ONLY that
-- scope; one it does not bind is free — it constrains nothing, so its value set is ⊤.
-- No ambient consultation: a 'TVInt' never resolves through an environment, and a
-- like-named ambient binding is a DIFFERENT variable. Consulting one would make the canonical
-- form depend on whichever env an operation happens to carry, so one value could be spelled ⊤
-- on one path and @$/T@ on another. Only a direct top-level reference is rewritten: a nested
-- one names the nested partial's own scope.
freeTVIntToTop :: PartialType -> Type -> Type
freeTVIntToTop p t@(TypeVar v TVInt i) | not (H.member (extKey v) (ptVarArg p)) = i  -- var ∩ inner: a free direct reference reads as its inner
                                     | otherwise = t
freeTVIntToTop _ t = t

-- | Reads the UNBOUND ambient ('TVExt') references of a type, at any depth, as their inner
-- (⊤ when knowledge-free): an external reference names the ambient scope and nothing else, so
-- one the ambient env does not bind denotes ⊤ — and must not later resolve through a binding
-- that a compared partial installs for a like-named parameter of its own. A bound reference is
-- left for the env to resolve. 'TVInt' and 'TVParent' references are untouched.
freeTVExtToTopDeep :: TypeVarArgEnv -> Type -> Type
freeTVExtToTopDeep vaenv = go
  where
    go t@(TypeVar v TVExt i) | not (H.member (extKey v) vaenv) = go i
                             | otherwise              = t
    go (TypeVar v loc i) = TypeVar v loc (go i)
    go (UnionType mtop pos consts) = UnionType (fmap goTop mtop) (goLeafs pos) consts
    goTop (preds, negLeafs, ncs) = (mapTypePreds (mapTypePred goPartial) preds, goLeafs negLeafs, ncs)
    goLeafs = joinUnionType . map goPartial . splitUnionType
    goPartial pt = pt{ ptVars  = fmap go (ptVars pt)
                     , ptArgs  = fmap go (ptArgs pt)
                     , ptPreds = mapTypePreds (mapTypePred goPartial) (ptPreds pt) }

-- | All partial-local ('TVInt') references appearing in a type at any depth. Over-collects
-- through nested scopes (a name a nested partial itself binds is still counted), the safe
-- direction for its consumer ('partialEscapes'\'s identical-form skip only becomes rarer).
typeTVIntRefs :: Type -> S.HashSet TypeVarAux
typeTVIntRefs (TypeVar v TVInt _) = S.singleton v
typeTVIntRefs (UnionType mtop pos _) = S.unions $ topRefs ++ map leafRefs (splitUnionType pos)
  where
    topRefs = case mtop of
      Just (preds, negLeafs, _) -> predsRefs preds : map leafRefs (splitUnionType negLeafs)
      Nothing                -> S.empty : []
    leafRefs PartialType{ptVars, ptArgs, ptPreds} =
      S.unions $ predsRefs ptPreds : map typeTVIntRefs (H.elems ptVars ++ H.elems ptArgs)
    predsRefs (PredsOne p)  = leafRefs (predPartial p)
    predsRefs (PredsAnd ps) = S.unions $ map predsRefs ps
    predsRefs (PredsNot p)  = predsRefs p
    predPartial (PredExpr p)    = p
    predPartial (PredClass p)   = p
    predPartial (PredRel p)     = p
    predPartial (PredConstEq c) = constantPartialType c
typeTVIntRefs _ = S.empty

-- | Whether a leaf is uninhabited because a value predicate it carries is decided FALSE on its
-- shape ('predExprHoldsOn'): @Dog | weight($this)@ when @weight@ never reaches @True@ on a @Dog@.
-- 'compactType' drops such a leaf ('compactFalsePredExprLeafs'); the subtype oracle, which never
-- re-compacts its operands, reads it as empty through 'partialUninhabited' so the two agree.
partialPredExprDecidedFalse :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> Bool
partialPredExprDecidedFalse typeEnv vaenv p = any literalFalse (predsConjuncts (ptPreds p))
  where
    -- A value literal of either polarity ('isValuePredLiteral'): @Dog | ¬fluffy($this)@ is as
    -- empty as @Cat | fluffy($this)@ when the predicate is decided the other way on the shape.
    literalFalse (PredsOne (PredExpr e))            = predExprHoldsOn typeEnv vaenv p e == Just False
    literalFalse (PredsNot (PredsOne (PredExpr e))) = predExprHoldsOn typeEnv vaenv p e == Just True
    literalFalse _                                  = False

-- | Whether a type still carries unresolved bare-⊤ content: a bare ⊤ in the type itself, a
-- var\/arg slot value at any depth, or a class\/rel\/expr predicate parameter. A type VARIABLE
-- is a LINK covered by its binding, not an unknown — only bare ⊤ is unresolved content. A
-- predicate-bounded top (@∀ConsList[/T=Integer]@) is determined by its predicates and counts
-- only through a ⊤ inside them. Excluded (negated) leafs are not scanned — a ⊤ there bounds
-- what the type is NOT and holds no unresolved content.
typeContainsBareTop :: Type -> Bool
typeContainsBareTop PTopType = True
typeContainsBareTop (TypeVar _ _ _) = False
typeContainsBareTop (UnionType mtop pos _) = topContains || any partialContains (splitUnionType pos)
  where
    topContains = case mtop of
      Just (preds, _, _) -> predsContains preds
      Nothing            -> False
    predsContains (PredsOne p)  = partialContains (predPartial p)
    predsContains (PredsAnd ps) = any predsContains ps
    predsContains (PredsNot p)  = predsContains p
    predPartial (PredExpr p')   = p'
    predPartial (PredClass p')  = p'
    predPartial (PredRel p')    = p'
    predPartial (PredConstEq c) = constantPartialType c
    partialContains PartialType{ptVars, ptArgs, ptPreds} =
      any typeContainsBareTop (H.elems ptVars ++ H.elems ptArgs) || predsContains ptPreds

-- | Whether a var-binding STAMP on a partial's 'ptVars' is supported by every DECLARED
-- occurrence of the variable: an argument whose declared type (data-graph template, or a
-- nested spelling in the given slot spellings) externally references @n@ ties that slot's
-- content to the var, so a binding stamped while such a slot's met value still carries
-- bare-⊤ content claims resolution the content does not have (a list literal whose tail is
-- still ⊤ would read as Integer-elemented through the instantiated declared bound because
-- its head is an Integer). The join\/narrow is withheld until every occurrence's
-- content supports it; unresolved content only disappears as values narrow, so the
-- withhold is monotone, and the SAME rule gates the intersect's pointer-join and the
-- difference's 'narrowPiece' — both sides spell the unsupported case identically, keeping
-- @A∩B = A−(A−B)@. A vacuous occurrence (@Nil@ under @∀ConsList[/T=..]@) passes.
varStampSupported :: TypeGraph tg => TypeEnv tg -> TypeName -> [H.HashMap ArgName Type] -> S.HashSet ArgName -> H.HashMap ArgName Type -> TypeVarAux -> Bool
varStampSupported typeEnv name' spellings ks metArgs (TVVar n) = declaredOccsPresent && and
  [ not (typeContainsBareTop v)
  | k <- S.toList ks, occAt k, Just v <- [H.lookup k metArgs]]
  where
    -- A higher-order argument's input object only RECEIVES the var's values (see
    -- 'intersectPartialsBase'.'linkedKsOf'), so it holds no content the stamp must wait for.
    occAt k = S.null (pkArgs k) && (any (declRef k) (typeGraphData typeEnv name') || any (spellRef k) spellings)
    declRef k PartialType{ptArgs=dArgs} = maybe False (S.member n . typeOuterVarRefs) (H.lookup k dArgs)
    spellRef k sp = maybe False (S.member n . typeOuterVarRefs) (H.lookup k sp)
    -- A declared DIRECT occurrence of the var (a template field pointer @this=$T@ — a
    -- partial-local 'TVInt' value) that the application has NOT SUPPLIED YET cannot
    -- contribute to the pointer JOIN, and its future value can force the binding WIDER
    -- (the join polarity: direct occurrences are membership positions, lower bounds on
    -- the var). A stamp taken from the present occurrences alone would be read downstream
    -- as the var's exact upper bound and clamp the missing occurrence when it arrives
    -- (@compareTo[$T -> Ord](this -> $T, other -> $T)@ partially applied at @other=Some@
    -- must not record @/T=Some@ — the later @this=Nothing@ is emptied by
    -- declaration-boundedness). So the stamp is withheld until every DIRECT occurrence
    -- the name's templates tie to the var is present in the piece; absence only
    -- disappears as the application completes, so the withhold is monotone, exactly like
    -- the unresolved-content rule above. LINKED ('TVExt') occurrences are exempt — they
    -- contribute by MEET ('exactEnvAt': "var application is EXACT"), and a missing meet
    -- contributor only leaves the stamp WIDER, the sound direction — which also keeps an
    -- arg-subset piece (@Cons(head=..)@ of a @??@-leaf, no @tail@ in its value-set)
    -- stamping from its own args. A name with no template knowledge
    -- (open world, or presentations still resolving) withholds nothing.
    -- A higher-order argument's declared RESULT naming the var is such an occurrence too: until
    -- the argument is given, nothing says what its arrow returns.
    -- Only a template this piece can be an application OF is consulted: one whose arguments
    -- include every argument the piece has. A name declared at more than one shape — unary and
    -- binary @operator-@, each @[$T -> Ring]@ — has templates the piece can never complete
    -- (@a@ is no argument of @operator-(l, r)@), and waiting on their occurrences withheld every
    -- stamp, so no call of either shape bound @$T@ from its arguments.
    declaredOccsPresent = all (`S.member` ks) $
      [ k | tmpl <- typeGraphData typeEnv name' ++ typeGraphArrowTemplates typeEnv name'
          , ks `S.isSubsetOf` H.keysSet (ptArgs tmpl)
          , (k, TypeVar (TVVar n') TVInt _) <- H.toList (ptArgs tmpl), n' == n ]
      ++ [ k | (k, t) <- typeGraphDeclaredArgResults typeEnv name', n `S.member` typeOuterVarRefs t ]
varStampSupported _ _ _ _ _ _ = True

-- | Whether a type mentions any type variable anywhere (dims, args, predicates, exclusions).
typeHasVars :: Type -> Bool
typeHasVars TypeVar{} = True
typeHasVars BottomType = False
typeHasVars (UnionType mtop pos _) =
  maybe False topHasVars mtop || any partialHasVars (splitUnionType pos)
  where
    topHasVars (preds, negs, _) = predsHasVars preds || any partialHasVars (splitUnionType negs)
    predsHasVars (PredsOne pr) = predHasVars pr
    predsHasVars (PredsAnd ps) = any predsHasVars ps
    predsHasVars (PredsNot ps) = predsHasVars ps
    predHasVars (PredExpr q)    = partialHasVars q
    predHasVars (PredClass q)   = partialHasVars q
    predHasVars (PredRel q)     = partialHasVars q
    predHasVars (PredConstEq _) = False
    partialHasVars PartialType{ptVars=vs, ptArgs=as, ptPreds=prs} =
      any typeHasVars vs || any typeHasVars as || predsHasVars prs

-- | Looks up a 'TypeVarAux' in a 'TypeVarArgEnv' (either layer — the ambient env or a scope),
-- returning 'PTopType' on miss. Treats 'TVVar' and 'TVArg' uniformly: both kinds default to
-- 'PTopType' when absent, since an unconstrained variable is unknown (i.e. ⊤). Which layer a
-- reference is looked up in is the loc's business ('TypeVarLoc'); this is only the lookup.
-- A 'TVApplied' reference resolves its base variable, then applies its overrides to the result.
vaenvLookup :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypeVarAux -> Type
vaenvLookup typeEnv vaenv (TVApplied b overrides) = applyTypeVarOverrides typeEnv overrides (vaenvLookup typeEnv vaenv b)
vaenvLookup _ vaenv v = H.lookupDefault PTopType (extKey v) vaenv

-- | Whether an occurrence-local inner is OPEN — meeting it against the ambient env could
-- re-enter the var clause on an ambient variable: a bare variable, a 'TVOuter' reference
-- anywhere inside, or a reference to the occurrence's own variable under any spelling. A
-- closed inner's only variables are partial-local (@Some(value=$/T)@ resolves @$/T@ through
-- @Some@'s own parameters), which the meet reads without consulting the ambient env.
innerOpen :: TypeVarAux -> Type -> Bool
innerOpen _ TypeVar{} = True
innerOpen v i         = typeHasFreeOuterRef i || typeMentionsVar v i

-- | Whether a type has an outer reference no partial inside it binds: any 'TVExt' (it reads the
-- ambient env), or a 'TVParent' outside every partial of the type or naming a parameter its
-- enclosing partial does not declare. The scoping is 'substituteTVExtScoped''s: descending into a
-- partial's args and predicates makes that partial's parameters the layer a parent reference
-- reads, and its 'ptVars' VALUES keep the layer they arrived with. So a recursive field's
-- @tail=∀ConsList[/T=$_/T]@ inside a @Cons[/T=..]@ is bound by that @Cons@, and the value holding
-- it is closed.
typeHasFreeOuterRef :: Type -> Bool
typeHasFreeOuterRef = go Nothing
  where
    go _ (TypeVar _ TVExt _) = True
    go layer (TypeVar (TVVar n) TVParent i) = maybe True (not . S.member n) layer || go layer i
    go _ (TypeVar _ TVParent _) = True
    go layer (TypeVar _ _ i) = go layer i
    go layer (UnionType mtop pos _) = topHas || any (leafHas layer) (splitUnionType pos)
      where topHas = case mtop of
              Just (preds, negLeafs, _) -> predsHas layer preds || any (leafHas layer) (splitUnionType negLeafs)
              Nothing                   -> False
    leafHas layer PartialType{ptVars, ptArgs, ptPreds} =
      any (go layer) (H.elems ptVars) || predsHas layer' ptPreds || any (go layer') (H.elems ptArgs)
      where layer' = Just (S.fromList (H.keys ptVars))
    predsHas layer (PredsOne p)  = leafHas layer (predPartial p)
    predsHas layer (PredsAnd ps) = any (predsHas layer) ps
    predsHas layer (PredsNot p)  = predsHas layer p
    predPartial (PredExpr p)    = p
    predPartial (PredClass p)   = p
    predPartial (PredRel p)     = p
    predPartial (PredConstEq c) = constantPartialType c

-- | Whether a type refers to the variable @v@ (by name, under any location spelling) anywhere.
typeMentionsVar :: TypeVarAux -> Type -> Bool
typeMentionsVar v (TypeVar v' _ i) = sameVarName v v' || typeMentionsVar v i
  where
    sameVarName (TVVar a) (TVVar b) = a == b
    sameVarName (TVArg a) (TVArg b) = a == b
    sameVarName (TVApplied a _) b   = sameVarName a b
    sameVarName a (TVApplied b _)   = sameVarName a b
    sameVarName _ _                 = False
typeMentionsVar v (UnionType mtop leafs _) = any partialHas (splitUnionType leafs) || topHas
  where
    topHas = case mtop of
      Just (preds, negLeafs, _) -> predsHas preds || any partialHas (splitUnionType negLeafs)
      Nothing                   -> False
    predsHas (PredsOne p)  = predHas p
    predsHas (PredsAnd ps) = any predsHas ps
    predsHas (PredsNot ps) = predsHas ps
    predHas (PredClass p)   = partialHas p
    predHas (PredRel p)     = partialHas p
    predHas (PredExpr p)    = partialHas p
    predHas (PredConstEq _) = False
    partialHas PartialType{ptVars, ptArgs, ptPreds} =
      any (typeMentionsVar v) (H.elems ptVars) || any (typeMentionsVar v) (H.elems ptArgs) || predsHas ptPreds

-- | Whether a type contains any 'TypeVar' reference anywhere — var\/arg slot values,
-- class-predicate dims, and negative leaves included. A type without any is CLOSED:
-- substituting it can never introduce a new reference for a later pass to substitute
-- again, so only closed bindings are safe for the cross-epoch 'substituteFreeDeep'
-- frame-closure (a self-referential discovered binding like @/T → Cons[/T=$/T]@ would
-- otherwise embed a fresh occurrence each epoch and grow without bound).
typeHasTypeVars :: Type -> Bool
typeHasTypeVars TypeVar{} = True
typeHasTypeVars (UnionType mtop leafs _) = any partialHas (splitUnionType leafs) || topHas
  where
    topHas = case mtop of
      Just (preds, negLeafs, _) -> predsHas preds || any partialHas (splitUnionType negLeafs)
      Nothing                   -> False
    predsHas (PredsOne p)  = predHas p
    predsHas (PredsAnd ps) = any predsHas ps
    predsHas (PredsNot ps) = predsHas ps
    predHas (PredClass p)   = partialHas p
    predHas (PredRel p)     = partialHas p
    predHas (PredExpr p)    = partialHas p
    predHas (PredConstEq _) = False
    partialHas PartialType{ptVars, ptArgs, ptPreds} =
      any typeHasTypeVars (H.elems ptVars)
        || any typeHasTypeVars (H.elems ptArgs)
        || predsHas ptPreds

-- | Whether any 'TypeVar' occurrence in the type (at any depth, including predicate dims)
-- carries occurrence-local knowledge (a non-⊤ inner). Consumers without a 'TypeEnv' use it
-- to DEFER a knowledge-carrying value's meet instead of approximating it.
typeHasInnerKnowledge :: Type -> Bool
typeHasInnerKnowledge (TypeVar (TVApplied b ovr) loc i) = typeHasInnerKnowledge (TypeVar b loc i) || any typeHasInnerKnowledge (H.elems ovr)
typeHasInnerKnowledge (TypeVar _ _ PTopType) = False
typeHasInnerKnowledge (TypeVar _ _ _) = True
typeHasInnerKnowledge (UnionType mtop leafs _) = topHas || any partialHas (splitUnionType leafs)
  where
    topHas = case mtop of
      Just (preds, negLeafs, _) -> predsHas preds || any partialHas (splitUnionType negLeafs)
      Nothing                   -> False
    predsHas (PredsOne p)  = partialHas (predPartial p)
    predsHas (PredsAnd ps) = any predsHas ps
    predsHas (PredsNot ps) = predsHas ps
    predPartial (PredClass p)   = p
    predPartial (PredRel p)     = p
    predPartial (PredExpr p)    = p
    predPartial (PredConstEq c) = constantPartialType c
    partialHas PartialType{ptVars, ptArgs, ptPreds} =
      any typeHasInnerKnowledge (H.elems ptVars)
        || any typeHasInnerKnowledge (H.elems ptArgs)
        || predsHas ptPreds

-- | The names of all 'TVExt' (object-scope) variable references anywhere in a type. Unlike
-- 'typeOuterVarRefs' this excludes 'TVParent', which names a variable of an enclosing PARTIAL —
-- lexically a different variable from any like-named parameter of the partial it sits in.
typeExtVarRefs :: Type -> S.HashSet TypeVarName
typeExtVarRefs (TypeVar (TVVar n) TVExt i) = S.insert n (typeExtVarRefs i)
typeExtVarRefs (TypeVar _ _ i) = typeExtVarRefs i
typeExtVarRefs (UnionType mtop leafs _) = S.unions $ topRefs ++ map partialRefs (splitUnionType leafs)
  where
    topRefs = case mtop of
      Just (preds, negLeafs, _) -> predsRefs preds : map partialRefs (splitUnionType negLeafs)
      Nothing                   -> []
    predsRefs (PredsOne p')  = partialRefs (predPartial p')
    predsRefs (PredsAnd ps') = S.unions (map predsRefs ps')
    predsRefs (PredsNot p')  = predsRefs p'
    predPartial (PredClass c)   = c
    predPartial (PredRel r)     = r
    predPartial (PredExpr e)    = e
    predPartial (PredConstEq c) = constantPartialType c
    partialRefs PartialType{ptVars=vs, ptArgs=as, ptPreds=ps} =
      S.unions $ predsRefs ps : map typeExtVarRefs (H.elems vs ++ H.elems as)

-- | The references in a value sitting in a partial's arg slot that name THAT partial's parameters:
-- a parent ('TVParent') reference at the slot's own layer. The layer is replaced only at an arg
-- boundary (see 'substituteTVExtScoped'): a reference at the top of the value, or in the parameter
-- VALUES of a partial there, names the slot's partial; one inside a nested partial's ARGS names that
-- nested partial. In @head=Cons(tail=∀ConsList[/T=$^/T])@ the reference names the inner @Cons@'s
-- @/T@, not the outer one, while in @tail=∀ConsList[/T=$^/T]@ it names the outer @Cons@'s. An
-- external ('TVExt') reference names the ambient scope and never the slot's partial, whatever its
-- name.
typeLayerVarRefs :: Type -> S.HashSet TypeVarName
typeLayerVarRefs = go
  where
    go t@(TypeVar _ TVParent i) = maybe id S.insert (outerVarName t) (go i)
    go (TypeVar _ _ i) = go i
    go (UnionType mtop pos _) = S.unions $ topRefs ++ map leafRefs (splitUnionType pos)
      where topRefs = case mtop of
              Just (preds, negLeafs, _) -> predsRefs preds : map leafRefs (splitUnionType negLeafs)
              Nothing                   -> []
    leafRefs PartialType{ptVars} = S.unions (map go (H.elems ptVars))
    predsRefs (PredsOne p)  = leafRefs (predPartial p)
    predsRefs (PredsAnd ps) = S.unions (map predsRefs ps)
    predsRefs (PredsNot p)  = predsRefs p
    predPartial (PredExpr p)    = p
    predPartial (PredClass p)   = p
    predPartial (PredRel p)     = p
    predPartial (PredConstEq c) = constantPartialType c

-- | 'typeExtVarRefs' keyed by the full 'TypeVarAux', so an externally-located 'TVArg' reference
-- counts too — the key a 'TypeVarArgEnv' binds.
typeExtVarAuxes :: Type -> S.HashSet TypeVarAux
typeExtVarAuxes (TypeVar aux TVExt i) = S.insert aux (typeExtVarAuxes i)
typeExtVarAuxes (TypeVar _ _ i) = typeExtVarAuxes i
typeExtVarAuxes (UnionType mtop leafs _) = S.unions $ topRefs ++ map partialRefs (splitUnionType leafs)
  where
    topRefs = case mtop of
      Just (preds, negLeafs, _) -> predsRefs preds : map partialRefs (splitUnionType negLeafs)
      Nothing                   -> []
    predsRefs (PredsOne p')  = partialRefs (predPartial p')
    predsRefs (PredsAnd ps') = S.unions (map predsRefs ps')
    predsRefs (PredsNot p')  = predsRefs p'
    predPartial (PredClass c)   = c
    predPartial (PredRel r)     = r
    predPartial (PredExpr e)    = e
    predPartial (PredConstEq c) = constantPartialType c
    partialRefs PartialType{ptVars=vs, ptArgs=as, ptPreds=ps} =
      S.unions $ predsRefs ps : map typeExtVarAuxes (H.elems vs ++ H.elems as)

-- | The name a querying scope's variable is read under while an arrow's result is computed for it
-- ('Semantics.arrowDestTypeBase'): one no written variable can have, so no variable of the arrow's
-- own is confused with it. 'fromCallerScopeName' names it back.
callerScopeName, fromCallerScopeName :: TypeVarName -> TypeVarName
callerScopeName k@PartialKey{pkName} = k{pkName = mkName (callerScopePrefix ++ nameStr pkName)}
fromCallerScopeName k@PartialKey{pkName}
  | callerScopePrefix `isPrefixOf` nameStr pkName = k{pkName = mkName (drop (length callerScopePrefix) (nameStr pkName))}
  | otherwise = k

isCallerScopeName :: TypeVarName -> Bool
isCallerScopeName PartialKey{pkName} = callerScopePrefix `isPrefixOf` nameStr pkName

callerScopePrefix :: String
callerScopePrefix = "/$caller"

-- | Renames the variable a 'TypeVarAux' names, whichever kind it is.
mapTypeVarAuxName :: (TypeVarName -> TypeVarName) -> TypeVarAux -> TypeVarAux
mapTypeVarAuxName f (TVVar n)         = TVVar (f n)
mapTypeVarAuxName f (TVArg n)         = TVArg (f n)
mapTypeVarAuxName f (TVApplied b ovr) = TVApplied (mapTypeVarAuxName f b) ovr

-- | Renames every external ('TVExt') reference of a type, at any depth and inners included — the
-- references 'typeExtVarAuxes' collects. A re-applied reference's base is renamed when the
-- reference itself is external, and its override values are walked as any other content.
renameExtVarAuxes :: (TypeVarAux -> TypeVarAux) -> Type -> Type
renameExtVarAuxes f = go
  where
    go (TypeVar aux TVExt i) = TypeVar (renameAux aux) TVExt (go i)
    go (TypeVar (TVApplied b ovr) loc i) = TypeVar (TVApplied b (fmap go ovr)) loc (go i)
    go (TypeVar aux loc i) = TypeVar aux loc (go i)
    go (UnionType mtop leafs consts) = UnionType (fmap goTop mtop) (goLeafs leafs) consts
    renameAux (TVApplied b ovr) = TVApplied (renameAux b) (fmap go ovr)
    renameAux aux               = f aux
    goTop (preds, negLeafs, ncs) = (mapTypePreds (mapTypePred goPartial) preds, goLeafs negLeafs, ncs)
    goLeafs = joinUnionType . map goPartial . splitUnionType
    goPartial p@PartialType{ptVars, ptArgs, ptPreds} = p{ ptVars  = fmap go ptVars
                                                       , ptArgs  = fmap go ptArgs
                                                       , ptPreds = mapTypePreds (mapTypePred goPartial) ptPreds }

-- | The part of an ambient env a meet over @ts@ may READ: the bindings of the names the types
-- reference externally, and none a type also names as a parent reference. The meet keys a
-- reference by name, so an ambient @/T@ in view would capture a @$^/T@ naming the enclosing
-- partial's parameter, as 'foldDiscovered' documents.
externalReadEnv :: TypeVarArgEnv -> [Type] -> TypeVarArgEnv
externalReadEnv env ts = H.filterWithKey (\(VarKey _ k) _ -> S.member k extNames && not (S.member k parentNames)) env
  where
    extNames = S.unions (map typeExtVarAuxes ts)
    parentNames = S.unions (map typeParentVarAuxes ts)

-- | 'typeExtVarAuxes' for the 'TVParent' references: every parent reference's aux, at any
-- depth, inners included.
typeParentVarAuxes :: Type -> S.HashSet TypeVarAux
typeParentVarAuxes (TypeVar aux TVParent i) = S.insert aux (typeParentVarAuxes i)
typeParentVarAuxes (TypeVar _ _ i) = typeParentVarAuxes i
typeParentVarAuxes (UnionType mtop leafs _) = S.unions $ topRefs ++ map partialRefs (splitUnionType leafs)
  where
    topRefs = case mtop of
      Just (preds, negLeafs, _) -> predsRefs preds : map partialRefs (splitUnionType negLeafs)
      Nothing                   -> []
    predsRefs (PredsOne p')  = partialRefs (predPartial p')
    predsRefs (PredsAnd ps') = S.unions (map predsRefs ps')
    predsRefs (PredsNot p')  = predsRefs p'
    predPartial (PredClass c)   = c
    predPartial (PredRel r)     = r
    predPartial (PredExpr e)    = e
    predPartial (PredConstEq c) = constantPartialType c
    partialRefs PartialType{ptVars=vs, ptArgs=as, ptPreds=ps} =
      S.unions $ predsRefs ps : map typeParentVarAuxes (H.elems vs ++ H.elems as)

-- | The names of all OUTER type-VARIABLE references ('TVParent' or 'TVExt' — a reference to a
-- variable outside the position it sits at, whichever scope binds it) occurring anywhere in a
-- type: var\/arg slot values of every positive and negative leaf, predicate partials, and an
-- occurrence's inner (it is part of what the occurrence denotes). Used
-- by 'intersectPartialsBase' to decide which arg-meet discoveries genuinely belong to the
-- enclosing partial's scope.
typeOuterVarRefs :: Type -> S.HashSet TypeVarName
typeOuterVarRefs t@(TypeVar _ _ i) = maybe id S.insert (outerVarName t) (typeOuterVarRefs i)
typeOuterVarRefs PTopType = S.empty
typeOuterVarRefs (UnionType mtop leafs _) = S.unions $ topRefs ++ map partialRefs (splitUnionType leafs)
  where
    topRefs = case mtop of
      Just (preds, negLeafs, _) -> predsRefs preds : map partialRefs (splitUnionType negLeafs)
      Nothing                -> []
    predsRefs (PredsOne p')  = predRefs p'
    predsRefs (PredsAnd ps') = S.unions (map predsRefs ps')
    predsRefs (PredsNot p')  = predsRefs p'
    predRefs (PredClass c)   = partialRefs c
    predRefs (PredRel r)     = partialRefs r
    predRefs (PredExpr e)    = partialRefs e
    predRefs (PredConstEq _) = S.empty
    partialRefs PartialType{ptVars=vs, ptArgs=as, ptPreds=ps} =
      S.unions $ predsRefs ps : map typeOuterVarRefs (H.elems vs ++ H.elems as)

-- | A class declaration spells an unbound parameter as a SELF-reference (@/T ↦ $_/T@); read
-- through the application meet (@⊤ ∩ $_/T = $_/T@) it re-emits the dangling class-scope
-- spelling after the scope exits. Clean each self-referential entry to the ⊤ it denotes
-- before the meet.
cleanDeclSelfRefs :: TypeVarEnv -> TypeVarEnv
cleanDeclSelfRefs = H.mapWithKey clean
  where
    clean k v@(TypeVar _ _ i) | outerVarName v == Just k = i  -- var ∩ inner: the self-spelling cleans to its inner
    clean _ v = v

-- | Resolve a recursive value's free element vars (@$_T@) throughout a type, both:
--
--   * SELF-references — a recursive field's @$_T@ naming the partial's OWN @/T@ param (the
--     'resolveSelfRefs' case: @tail -> ConsList[/T=$_T]@), resolved through that partial's
--     @ptVars@; and
--   * AMBIENT references — a top-level raw @Cons[/T=$_T]@ the body constructs, whose @$_T@ names
--     the enclosing DEFINITION's own @/T@ (bound concretely in @ambient@ — @sorted@'s @/T=Ord@).
--
-- Unlike 'substituteKeep' / 'substituteTVExt' this ALSO descends into the positive leaves of a
-- mixed 'Just'-top union — a partially-unrolled recursive tail
-- @UnionType(Just(ConsList[/T=$_T]), Cons[/T=$_T](..))@ carries the free element in BOTH the
-- predicate top and the enumerated leaf, and leaving the leaf unresolved keeps it reading as Top.
-- A var absent from both the partial's @ptVars@ and @ambient@ stays symbolic (a genuinely generic
-- element), so a generic scope is untouched.
--
-- Needed on an Arrow result before intersecting/checking it: an unresolved recursive tail reads
-- as Top, so the meet of two otherwise-concrete @Cons[/T=Ord]@ leaves re-derives the outer element
-- from it and collapses @/T@ to Top (a 'verifySchemeActLowers' widening), and the result fails the
-- declaration's requirement check. Leaves with no @/T@ param (raw applied forms kept for eval
-- dispatch) resolve nothing.
resolveTypeSelfRefs :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
-- Two layers, as 'substituteKeepScoped': an ambient reference reads the env, a parent reference
-- reads the SCOPE (the partial the position sits one level inside, its own resolved slots), and
-- descent replaces the scope. The top is a fragment whose parent is the object, so the scope
-- seeds with the env (the keep walker's rule; the one caller, 'arrowConstrainUbs', passes ∅).
-- Keeping the layers apart is what stops an ambient spelling being captured by a like-named own
-- slot, and a parent reference whose own slot is unresolved falling through to the env by name.
resolveTypeSelfRefs typeEnv vaenv = go vaenv vaenv
  where
    go scope env (UnionType mtop pos consts) =
      UnionType (fmap (goTop scope env) mtop) (joinUnionType $ map (goLeaf scope env) $ splitUnionType pos) consts
    go scope env (TypeVar (TVApplied b overrides) loc i) =
      applyTypeVarOverrides typeEnv (fmap (go scope env) overrides) (go scope env (TypeVar b loc i))
    -- Only a CONCRETE binding resolves a reference ('isResolvable'). A partial's own var slot
    -- may itself be a reference (@Node[/V=$^/MV]@, an arm's result read under its
    -- declaration): substituted into the field it would spell @right=∀Tree[/V=$^/MV]@, a
    -- reference written for Node's level moved one level in, where it names a @/MV@ Node
    -- does not declare. The field keeps its own spelling (@$^/V@) and reaches the binding
    -- through Node's slot.
    go _     env t@(TypeVar v TVExt _) = resolvedIn env TVExt v t
    go scope _   t@(TypeVar v TVParent _) = resolvedIn scope TVParent v t
    go _     _   t                        = t
    resolvedIn layer loc v t = case envLookupAt layer loc v of
      Just b | isResolvable b -> b
      _                       -> t
    goTop scope env (preds, negLeafs, ncs) =
      (mapTypePreds (mapTypePred (goLeaf scope env)) preds, joinUnionType $ map (goLeaf scope env) $ splitUnionType negLeafs, ncs)
    goLeaf scope env p@PartialType{ptVars, ptArgs, ptPreds} = p{
        ptVars  = ptVars',
        ptArgs  = fmap (go innerScope env) ptArgs,
        ptPreds = mapTypePreds (mapTypePred (goLeaf innerScope env)) ptPreds
      }
      where
        -- The partial's own values sit at ITS level and keep the scope they arrived with; its
        -- args and predicates are one level in, where its (resolved) own slots are the scope.
        ptVars'    = fmap (go scope env) ptVars
        innerScope = ambientEnv (joinVarArgEnv ptVars' H.empty)
    isResolvable (TypeVar _ _ _) = False
    isResolvable PTopType        = False
    isResolvable _               = True

-- | Names an external reference that resolves to the IMMEDIATELY enclosing partial's variables
-- as the parent reference it is ('TVParent'), so the ambient scope rules stop reading it as a
-- foreign name. Everything else keeps the 'TVExt' the parser gave it.
--
-- A 'TVParent' reaches exactly ONE level out, because that is the only depth a descent can
-- preserve: descending into a partial's argument replaces the parent scope with that partial's
-- own variables (a reference to the old parent is resolved to its value on the way in), so a
-- reference naming a variable two levels out has no spelling. The scope set is therefore
-- REPLACED at each argument boundary, not accumulated: descending into a partial's ARG slots
-- (and predicates) makes that partial's own var names the parent set, while its 'ptVars' VALUES
-- sit at the partial's own level and keep the set they arrived with. A name the immediately
-- enclosing partial does not bind stays 'TVExt' and resolves through the ambient environment,
-- which is where a declaration's own variables are registered ('TypeCheck.Encode').
--
-- Accumulating instead would produce references that no descent can resolve — @Node(left =
-- ∀Tree[/V=$^/MV])@, whose @/MV@ belongs to the surrounding declaration rather than to @Node@.
--
-- Applied where the binder is known and lexical (desugar), never re-derived deep in the type
-- algebra, where a like-named ambient variable would be captured.
tagParentVarRefs :: S.HashSet TypeVarAux -> Type -> Type
tagParentVarRefs = onType
  where
    onType sc (UnionType mtop pos consts) =
      UnionType (fmap (onTop sc) mtop) (joinUnionType $ map (onLeaf sc) $ splitUnionType pos) consts
    onType _ t = t -- a bare top-level variable has no parent in view
    onTop sc (preds, negLeafs, ncs) =
      (mapTypePreds (mapTypePred (onLeaf sc)) preds, joinUnionType $ map (onLeaf sc) $ splitUnionType negLeafs, ncs)
    onLeaf sc p@PartialType{ptVars, ptArgs, ptPreds} = p{
        ptVars  = H.mapWithKey (\k -> siblingRefs k . onVarSlot sc k) ptVars,
        ptArgs  = H.mapWithKey (\k -> onDirect sc' . hopInput sc' k) ptArgs,
        ptPreds = mapTypePreds (mapTypePred (onLeaf sc')) ptPreds
      }
      where
        sc' = S.fromList (map TVVar (H.keys ptVars))
        -- A parameter's value may name a SIBLING parameter of the same partial: the bound of
        -- @f[$M -> Mon[$T -> $_A], $A]@'s @$M@ names @f@'s own @$A@. The reference sits on the
        -- partial nested in the value (@Mon@), whose immediately enclosing partial is @f@, so it is a
        -- parent reference (@/M=∀Mon[/T=$^A]@): read as the ambient @$_A@ it is not @f@'s @/A@ at
        -- all, and the meet could never bind @/A@ from what the call's @/M@ is. Only a sibling: a
        -- reference to anything else in a bound names the scope containing the binder and keeps
        -- its spelling, and a reference to the parameter's own name (@Cons[$T -> $_T]@) is that
        -- containing scope's variable of the same name.
        siblingRefs k = siblingRefsToParent (S.fromList [TVVar n | n <- H.keys ptVars, n /= k])
    -- A DIRECT slot value names the partial it is a slot of, and that is what 'TVInt' spells
    -- (@/Cons(/head=$/T)@); there is no separate parent for it to name. So a bare outer
    -- spelling in that position naming a parameter the partial binds (@Just[$T](val -> $_T)@,
    -- which means @Just[$T](val -> $T)@) is the partial's OWN parameter and is spelled
    -- 'TVInt' — a partial-local pointer that survives the template being carried into a foreign
    -- frame, where an ambient @$_/T@ is a foreign name read as ⊤ ('dropOutOfScopeTVExt') and the
    -- field's link to @/T@ would be lost (@defInt(x= Just(val -> Integer))@ must type @/x@ with
    -- its @/T=Integer@). It is NOT a parent reference: 'TVParent' never consults the ambient env and
    -- has no parent in view from a bare annotation. 'classifyParentVarRefs' applies the same rule
    -- to the object's own direct argument annotations, so the declaration has one spelling. The
    -- set is still carried inward, because a partial NESTED in the slot does have this one as its
    -- parent (@/tail=∀ConsList[/T=$^/T]@).
    -- A PARAMETER value sits at its partial's own level, so a bare internal spelling there that
    -- names a variable of the immediately enclosing partial is that partial's variable — a parent
    -- reference — and not this partial's: @unwrap[$U](b -> Box[$T -> $U])@'s @/T=$U@ is
    -- @unwrap@'s @/U@. The parser spells a bare @$U@ context-free as 'TVInt', and left so it would
    -- break the 'TVInt' contract (a variable of the SAME partial). The self-entry @/T=$T@ names
    -- its own slot and stays 'TVInt'.
    -- A HIGHER-ORDER argument's slot holds its input object (@fn(acc -> $_B)@ in
    -- @foldEntries[$B]@), whose parameters name the enclosing partial's variables from one level
    -- further in than a parent reference reaches. The input object is a pattern rather than data,
    -- so it can carry those variables itself: each one it references becomes a parameter of the
    -- object that is a parent reference to the enclosing one (@fn[/B=$^B](acc=$/B)@), and the
    -- descent below spells the uses as references to that parameter. The declaration then has
    -- one spelling, and an occurrence of @$B@ inside the input object is visible to the
    -- enclosing partial as a linked occurrence ('intersectPartialsBase').
    hopInput sc k v@(UnionType Nothing leafs [])
      | isJust (hoSlotInputObject k v) = UnionType Nothing (joinUnionType (map (hopLeaf sc) (splitUnionType leafs))) []
    hopInput _ _ v = v
    hopLeaf sc p@PartialType{ptVars, ptArgs} = p{ptVars = H.union ptVars hops}
      where
        refs = [ n | v@(TVVar n) <- S.toList (S.unions (map typeExtVarAuxes (H.elems ptArgs))), S.member v sc, not (H.member n ptVars) ]
        hops = H.fromList [ (n, TypeVar (TVVar n) TVParent PTopType) | n <- refs ]
    onVarSlot sc k (TypeVar v@(TVVar n) TVInt i) | n /= k, S.member v sc = TypeVar v TVParent (onSlot sc i)
    onVarSlot sc _ t = onSlot sc t
    onSlot sc (TypeVar v@(TVVar _) TVExt i) | S.member v sc = TypeVar v TVParent i
    onSlot sc (TypeVar (TVApplied b ovr) loc i) = case onSlot sc (TypeVar b loc i) of
      TypeVar b' loc' i' -> reapplyTypeVar (fmap (onSlot sc) ovr) b' loc' i'
      t                  -> error $ printf "tagParentVarRefs re-spelled the variable %s as %s" (show b) (show t)
    onSlot sc (UnionType mtop pos consts) =
      UnionType (fmap (onTop sc) mtop) (joinUnionType $ map (onLeaf sc) $ splitUnionType pos) consts
    onSlot _ t = t
    onDirect sc (TypeVar v@(TVVar _) TVExt i) | S.member v sc = TypeVar v TVInt (onSlot sc i)
    onDirect sc (TypeVar v loc i) = TypeVar v loc (onSlot sc i)
    onDirect sc t                 = onSlot sc t

-- | A parameter's value entering its partial's slot: the references it makes to SIBLING
-- parameters (@sib@) from the partials nested at its top (@Mon[/T=$_A]@ as @f@'s @/M@) name the
-- slot's partial, one level out, so they are spelled as parent references (@Mon[/T=$^A]@).
-- References deeper in, or to any other variable, keep their spelling ('tagParentVarRefs' gives
-- the one-level rule).
siblingRefsToParent :: S.HashSet TypeVarAux -> Type -> Type
siblingRefsToParent = mapNestedVarRefs toParent
  where toParent sib (TypeVar v TVExt i) | S.member v sib = TypeVar v TVParent i
        toParent _ t = t

-- | The inverse of 'siblingRefsToParent': a parameter's value leaving its partial's slot for a
-- frame in which the partial's parameters are the AMBIENT variables (the declaration's own
-- constraint scope), so a parent reference to a sibling is the ambient reference to it.
siblingRefsToAmbient :: S.HashSet TypeVarAux -> Type -> Type
siblingRefsToAmbient = mapNestedVarRefs toAmbient
  where toAmbient sib (TypeVar v TVParent i) | S.member v sib = TypeVar v TVExt i
        toAmbient _ t = t

-- | Rewrites the variable references in the parameter values of the partials at a type's top
-- (and in its predicate tops' partials), one level in and no deeper.
mapNestedVarRefs :: (S.HashSet TypeVarAux -> Type -> Type) -> S.HashSet TypeVarAux -> Type -> Type
mapNestedVarRefs f sib t0
  | S.null sib = t0
  | otherwise = onType t0
  where
    onType (UnionType mtop pos consts) = UnionType (fmap onTop mtop) (joinUnionType $ map onLeaf $ splitUnionType pos) consts
    onType t = t
    onTop (preds, negLeafs, ncs) = (mapTypePreds (mapTypePred onLeaf) preds, joinUnionType $ map onLeaf $ splitUnionType negLeafs, ncs)
    onLeaf p = p{ptVars = fmap (f sib) (ptVars p)}

-- | 'tagParentVarRefs' for a producer holding a 'PartialType' directly — the class graph's own
-- declaration and member templates — rather than a 'Type'. The partial is the outermost thing in
-- view, so it has no parent of its own; only the references nested inside it can be parent
-- references.
tagParentVarRefsPartial :: S.HashSet TypeVarAux -> PartialType -> PartialType
tagParentVarRefsPartial sc p = case tagParentVarRefs sc (singletonType p) of
  UnionType Nothing leafs [] | [p'] <- splitUnionType leafs -> p'
  _                                                         -> p

-- | Reads a type's NESTED out-of-scope external ('TVExt' 'TVVar') references as ⊤. A stored
-- point type may reference only variables its own constraint scope binds: a graph-presented
-- result carries the source declarations' own parameter spellings (@mmap@'s @$_A@, an
-- instance's @$_T@), which are meaningless names in a foreign scope — each denotes ⊤ (an
-- unbound var constrains nothing), yet keeping the NAME makes denotationally-equal forms
-- compare unequal, so accumulations flip between spellings across epochs ('compactOverlapping'
-- dedups an arbitrary one) and 'verifySchemeActLowers' reports a spurious grow. An IN-scope
-- variable stays symbolic (the mergeVarEnvVal doctrine — its binding may still resolve), as
-- does a bare top-level variable (same doctrine) and every 'TVArg'/'TVInt' reference (arg
-- links and partial-local pointers are structural, not ambient names).
--
-- The in-scope set is the ambient env's keys and NOTHING else. A reference to an enclosing
-- partial's own parameter is spelled 'TVParent' by desugar ('tagParentVarRefs') and is never
-- read here, so descending into a partial's arg slots does not extend the set with the
-- partial's parameter names: an ambient spelling that merely SHARES a name with the partial it
-- sits in is a foreign name like any other.
dropOutOfScopeTVExt :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
dropOutOfScopeTVExt typeEnv vaenv = onType (S.fromList (H.keys (envAmbient vaenv)))
  where
    onType sc (UnionType mtop pos consts) =
      UnionType (fmap (onTop sc) mtop) (joinUnionType $ map (onLeaf sc) $ splitUnionType pos) consts
    onType _ t = t -- a bare top-level variable stays symbolic
    onTop sc (preds, negLeafs, ncs) =
      (mapTypePreds (mapTypePred (onLeaf sc)) preds, joinUnionType $ map (onLeaf sc) $ splitUnionType negLeafs, ncs)
    onLeaf sc p@PartialType{ptVars, ptArgs, ptPreds} = p{
        ptVars  = fmap (onSlot sc) ptVars,
        ptArgs  = fmap (onSlot sc) ptArgs,
        ptPreds = mapTypePreds (mapTypePred (onLeaf sc)) ptPreds
      }
    -- Only an AMBIENT reference is read as ⊤ here. A 'TVParent' names a variable of a partial
    -- it sits inside, not of this scope, so it is none of the ambient env's business — erasing it
    -- would drop a declaration's own arg-slot pointer (@Box[/T=$_/V]@) on every descent.
    -- The occurrence reads @var ∩ inner@, so erasing a GENUINELY out-of-scope var leaves the
    -- (processed) inner. That is exact HERE and only here: a var no scope binds denotes ⊤, so
    -- @⊤ ∩ inner = inner@ is the reading, not an approximation of it. Do not generalize it — an
    -- inner is not a substitute for resolving a var that IS bound. Carrying a declared bound on
    -- the occurrence would freeze the variable at that bound and lose its instantiation
    -- ('comparedPartialsEnv' states the rule); a traversal that reaches a bound variable still
    -- owes it the correct contextual value.
    onSlot sc (TypeVar v@(TVVar _) TVExt i) | not (S.member v sc) = onSlot sc i
                                            | otherwise           = TypeVar v TVExt (onSlot sc i)
    onSlot sc (TypeVar (TVApplied b ovr) loc i) = case onSlot sc (TypeVar b loc i) of
      PTopType -> PTopType
      base     -> applyTypeVarOverrides typeEnv (fmap (onSlot sc) ovr) base
    onSlot sc (UnionType mtop pos consts) =
      UnionType (fmap (onTop sc) mtop) (joinUnionType $ map (onLeaf sc) $ splitUnionType pos) consts
    onSlot _ t = t

-- | Whether 'dropOutOfScopeTVExt' would change the type: some NESTED external ('TVExt'
-- 'TVVar') reference is not bound by the ambient env. A cheap early-exit scan so the common
-- reference-free case pays no rewrite ('compactType' consults this on every union).
hasOutOfScopeTVExt :: TypeVarArgEnv -> Type -> Bool
hasOutOfScopeTVExt vaenv = onType (S.fromList (H.keys (envAmbient vaenv)))
  where
    onType sc (UnionType mtop pos _) = maybe False (onTop sc) mtop || any (onLeaf sc) (splitUnionType pos)
    onType _ _ = False
    onTop sc (preds, negLeafs, _) = predsHas sc preds || any (onLeaf sc) (splitUnionType negLeafs)
    predsHas sc (PredsOne p)  = onLeaf sc (predPartial p)
    predsHas sc (PredsAnd ps) = any (predsHas sc) ps
    predsHas sc (PredsNot p)  = predsHas sc p
    predPartial (PredExpr p)    = p
    predPartial (PredClass p)   = p
    predPartial (PredRel p)     = p
    predPartial (PredConstEq c) = constantPartialType c
    -- Mirrors 'dropOutOfScopeTVExt': the in-scope set is the ambient env's keys at every depth.
    onLeaf sc PartialType{ptVars, ptArgs, ptPreds} =
      any (onSlot sc) (H.elems ptVars) || any (onSlot sc) (H.elems ptArgs) || predsHas sc ptPreds
    -- Only an AMBIENT reference counts, matching 'dropOutOfScopeTVExt': a 'TVParent' names a
    -- variable of a partial it sits inside, not of this scope, and is never erased.
    onSlot sc (TypeVar v@(TVVar _) TVExt i)   = not (S.member v sc) || onSlot sc i
    onSlot sc (TypeVar (TVApplied b ovr) loc i) = onSlot sc (TypeVar b loc i) || any (onSlot sc) (H.elems ovr)
    onSlot sc t@UnionType{}                 = onType sc t
    onSlot _ _                              = False

-- | The NESTED external ('TVExt' 'TVVar') references not bound by the ambient env — the vars
-- 'dropOutOfScopeTVExt' would read as the ⊤ they denote. Same traversal as
-- 'hasOutOfScopeTVExt'. Used to find an object's IMPLICIT type variables (externals introduced
-- through usage in its annotations rather than an explicit application) so encode can register
-- them in the object's own constraint scope.
outOfScopeTVExtVars :: TypeVarArgEnv -> Type -> S.HashSet TypeVarAux
outOfScopeTVExtVars vaenv = onType (S.fromList (H.keys (envAmbient vaenv)))
  where
    onType sc (UnionType mtop pos _) = maybe S.empty (onTop sc) mtop <> S.unions (map (onLeaf sc) (splitUnionType pos))
    onType _ _ = S.empty
    onTop sc (preds, negLeafs, _) = predsVars sc preds <> S.unions (map (onLeaf sc) (splitUnionType negLeafs))
    predsVars sc (PredsOne p)  = onLeaf sc (predPartial p)
    predsVars sc (PredsAnd ps) = S.unions (map (predsVars sc) ps)
    predsVars sc (PredsNot p)  = predsVars sc p
    predPartial (PredExpr p)    = p
    predPartial (PredClass p)   = p
    predPartial (PredRel p)     = p
    predPartial (PredConstEq c) = constantPartialType c
    -- Same in-scope set at every depth as 'dropOutOfScopeTVExt'.
    onLeaf sc PartialType{ptVars, ptArgs, ptPreds} =
      S.unions (map (onSlot sc) (H.elems ptVars)) <> S.unions (map (onSlot sc) (H.elems ptArgs)) <> predsVars sc ptPreds
    -- Only an AMBIENT reference is collected: a 'TVParent' resolves through the partial it sits
    -- inside, so it is not one of the object's implicit variables.
    onSlot sc (TypeVar v@(TVVar _) TVExt i) = (if S.member v sc then S.empty else S.singleton v) <> onSlot sc i
    onSlot sc (TypeVar (TVApplied b ovr) loc i) = onSlot sc (TypeVar b loc i) <> S.unions (map (onSlot sc) (H.elems ovr))
    onSlot sc t@UnionType{} = onType sc t
    onSlot _ _ = S.empty

-- | Relate an arrow's declared result to the type variables the source bound. A method whose
-- result is a class parameterized by the RECEIVER's own type — @:$T.eq[$T -> Eq] -> Eqv[$T]@ — loses
-- that link at desugar: a bare @Eqv[$T]@ becomes @Eqv[/T=Top]@ (the var reads as the parameter NAME,
-- not a value bound to it), so 'arrowDestType's 'substituteTVExt' finds no @$/T@ occurrence to fill
-- and the element stays Top. Re-link each Top class-parameter slot whose NAME is a type variable the
-- source bound ('vaenv') to that binding — @Eqv[/T=Top]@ under @/T ↦ Integer@ becomes
-- @Eqv[/T=Integer]@, carrying the element the receiver determines. A slot with no matching bound var
-- stays Top (a genuinely open parameter), and a non-Top slot is left to 'substituteTVExt' (it already
-- holds its value or a resolvable var).
relateArrowVars :: TypeVarArgEnv -> Type -> Type
relateArrowVars vaenv = go
  where
    go (UnionType mtop pos consts) =
      UnionType (fmap goTop mtop) (joinUnionType $ map goLeaf $ splitUnionType pos) consts
    go t = t
    goTop (preds, negLeafs, ncs) =
      (mapTypePreds (mapTypePred goLeaf) preds, joinUnionType $ map goLeaf $ splitUnionType negLeafs, ncs)
    goLeaf p@PartialType{ptVars, ptArgs, ptPreds} = p{
        ptVars  = H.mapWithKey relateVar ptVars,
        ptArgs  = fmap go ptArgs,
        ptPreds = mapTypePreds (mapTypePred goLeaf) ptPreds
      }
    relateVar k PTopType = case H.lookup (extKey (TVVar k)) vaenv of
      Just b | b /= PTopType -> b
      _                      -> PTopType
    relateVar _ v = go v

-- | The INPUT OBJECT a higher-order argument's slot holds, if it holds one. A higher-order
-- argument (@fn(v -> $_A) -> $M@) is an Obj/Arrow pair in the environment of the function it is
-- passed into: its slot in the call's partial is the object @fn(/v=..)@ — named as the argument,
-- over exactly the argument's own parameter names — and its result is the arrow's, found through
-- the environment rather than stored in any partial.
hoSlotInputObject :: ArgName -> Type -> Maybe PartialType
hoSlotInputObject argName t = case maybeGetSingleton t of
  -- Only an argument written with parameters (@fn(v)@) is higher-order here; a zero-parameter
  -- one (@fn()@) is not yet told apart from a plain argument by its key.
  Just p | not (S.null (pkArgs argName))
         , makeAbsoluteName (ptName p) == makeAbsoluteName (pkName argName)
         , H.keysSet (ptArgs p) == pkArgs argName -> Just p
  _ -> Nothing

-- | A higher-order input object spelled to reach its enclosing object's variables one hop at a
-- time. A declared @fn(v -> $_MV)@ sits in a slot of @foldEntries[/MV]@, so its parameter
-- annotation names a variable two levels out — past the @fn@ pattern to the object it is an
-- argument of — which no traversal carries (a 'TVExt' reads the ambient env, where a caller's
-- frame never binds the callee's @/MV@). Spelled as @fn[/MV=$^MV](v=$MV)@ instead, each step is
-- one the algebra resolves: @fn@'s own @/MV@ is a parent reference to the declaration's (bound
-- when a call meets the declaration), and @v@ is an internal pointer to @fn@'s @/MV@. Only
-- references to variables the scope binds (@inScope@) are lifted; any other stays as it is.
hopInputObject :: TypeVarArgEnv -> Type -> Type
hopInputObject inScope (UnionType Nothing leafs []) = UnionType Nothing (joinUnionType (map hopLeaf (splitUnionType leafs))) []
  where
    hopLeaf p@PartialType{ptVars, ptArgs} = p{ptVars = H.union ptVars hops, ptArgs = fmap toPointer ptArgs}
      where
        refs = [ n | TypeVar (TVVar n) TVExt _ <- H.elems ptArgs, H.member (extKey (TVVar n)) inScope, not (H.member n ptVars) ]
        hops = H.fromList [ (n, TypeVar (TVVar n) TVParent PTopType) | n <- refs ]
        toPointer (TypeVar (TVVar n) TVExt i) | n `elem` refs = TypeVar (TVVar n) TVInt i
        toPointer t = t
hopInputObject _ t = t

typeSetAux :: TypeVarAux -> Type -> PartialType -> PartialType
typeSetAux (TVVar k) v p@PartialType{ptVars} = p{ptVars=H.insert k v ptVars}
typeSetAux (TVArg k) v p@PartialType{ptArgs} = p{ptArgs=H.insert k v ptArgs}
typeSetAux (TVApplied k _) _ _ = error $ printf "Unexpected TVApplied %s used as a position in typeSetAux" (show k)

-- | Undo a widening of a type variable back to Top. Wherever @old@ carried a variable at a nested
-- argument or type-parameter position and @new@ replaced it with Top, keep @old@'s variable. A
-- resolution to a concrete (narrower) type in @new@ is left untouched — only a loss to Top is
-- reverted. This lets 'updateTypeProp' preserve the cross-scope link an explicit external variable
-- carries (e.g. a higher-order argument's @fn(/v=$_A)@ tied to a container element) rather than
-- dropping it to Top during intersection.
--
-- An OUTER ('TVOuter') reference is always such a link. An INTERNAL one ('TVInt') is the
-- partial's own pointer: resolving through its own @ptVars@ to ⊤ denotes ⊤, so showing as ⊤ is
-- the same reading.
--
-- This walk is THE detection of a lost link: 'TypeCheck.Common.typeVarLost' is "this
-- restoration changes something", so the actual side (restore the lost slots) and the
-- requirement sides (refuse the write) of 'updateSchemeProp' answer one question.
restoreLostTVExtVars :: Type -> Type -> Type
restoreLostTVExtVars = go
  where
    go :: Type -> Type -> Type
    go old new = case (old, new) of
      (_, PTopType) | isJust (outerVarRef old) -> old
      (UnionType otop oldLeafs _, UnionType ntop newLeafs ncs) ->
        UnionType (fixTop <$> ntop) (joinUnionType (map (fixPartial (byName oldLeafs)) (splitUnionType newLeafs))) ncs
        where
          -- The predicate component carries the same references (a class atom's parameter), and
          -- it is where a class-typed slot keeps its spelling: the @∀ConsList[/T=$/E]@ a
          -- recursive field holds is a predicate atom, not a positive leaf. Restore it against
          -- the old type's own top, atom-by-name — in POSITIVE positions only. Under a negation
          -- (a @PredsNot@ atom, an excluded leaf) a reference replaced by ⊤ excludes MORE, which
          -- narrows the type: a legal lowering, not a lost link, and restoring it would widen a
          -- narrowing (@¬Tree[/V=⊤]@ back to @¬Tree[/V=$_/MV]@).
          fixTop (nPreds, nNegLeafs, nncs) = (fixPos nPreds, nNegLeafs, nncs)
            where oPreds = maybe PredsNone (\(op', _, _) -> op') otop
                  -- The old atoms to restore from, positive positions only as well: a negated
                  -- old atom is not the spelling a positive new atom of the same name lost.
                  oldPredPartials = H.fromListWith (++) [(ptName q, [q]) | q <- posPartials oPreds]
                  posPartials (PredsAnd ps) = concatMap posPartials ps
                  posPartials PredsNot{}    = []
                  posPartials (PredsOne pr) = predsPartials (PredsOne pr)
                  fixPos (PredsAnd ps) = PredsAnd (map fixPos ps)
                  fixPos p@PredsNot{}  = p
                  fixPos (PredsOne pr) = PredsOne (mapTypePred (fixPartial oldPredPartials) pr)
          byName = splitUnionTypeByName
          restoreKey oldMap k nv = maybe nv (\ov -> go ov nv) (H.lookup k oldMap)
          fixPartial oldByName np = case H.lookup (ptName np) oldByName of
            Just [op] -> np { ptArgs = H.mapWithKey (restoreKey (ptArgs op)) (ptArgs np)
                            , ptVars = H.mapWithKey (restoreKey (ptVars op)) (ptVars np) }
            _         -> np
      _ -> new
    predsPartials :: TypePredicates -> [PartialType]
    predsPartials (PredsOne (PredExpr q))    = [q]
    predsPartials (PredsOne (PredClass q))   = [q]
    predsPartials (PredsOne (PredRel q))     = [q]
    predsPartials (PredsOne (PredConstEq _)) = []
    predsPartials (PredsAnd ps)              = concatMap predsPartials ps
    predsPartials (PredsNot ps)              = predsPartials ps
