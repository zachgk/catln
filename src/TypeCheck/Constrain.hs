--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Constrain
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines the 'runConstraints'. It executes the set
-- of type variable constraints against the type variables until
-- the variables converge. It tries to execute each constraint
-- and then determines if it is changing the variable.
--------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module TypeCheck.Constrain where

import qualified Data.HashMap.Strict as H
import qualified Data.IntMap.Strict  as IM
import           Data.List           (intercalate, partition)
import           Data.Maybe
import qualified Data.Zip            as Z

import           Control.Monad.State
import           Semantics           (unsafeFreshQueryToken)
import           Semantics.Types
import           Text.Printf
import           TypeCheck.Common
import           TypeCheck.Show
import           TypeCheck.TypeUtils

-- | Checks if a scheme is solved because it's upper bound and lower bound are equal
isSolved :: Scheme -> Bool
isSolved _                                 = False

-- | Updates a 'VarMeta' point to a new 'Scheme' value
setScheme :: FEnv -> VConstraint -> (RConstraint, RConstraint) -> VarMeta -> Scheme -> String -> FEnv
setScheme env con (oldCon, newCon) p scheme baseMsg = setDescriptor env con p (checkScheme (fmap normalizeScope scheme)) (msg "" "")
  where
    -- A stored point type may reference only variables its own constraint scope binds: nested
    -- external spellings from foreign declarations denote ⊤ and are read as ⊤
    -- ('dropOutOfScopeTVExt'), so denotationally-equal forms cannot flip between spellings
    -- across epochs. In-scope variables (and bare top-level ones) stay symbolic.
    scopeKeys = fmap (const PTopType) (constraintVarArgEnv con)
    normalizeScope s@SType{stypeAct=act, stypeReq=req, stypeReqDecl=reqDecl} =
      s{stypeAct=dropOutOfScopeTVExt scopeKeys act, stypeReq=dropOutOfScopeTVExt scopeKeys req, stypeReqDecl=dropOutOfScopeTVExt scopeKeys reqDecl}
    -- checkScheme (TypeCheckResult _ (SType ub _ desc)) | containsBottomType ub = error $ msg desc $ printf "Actual type contains bottomType: %s" (show ub)
    checkScheme (TypeCheckResult notes (SType ub _ _ _ desc)) | containsBottomType ub = TypeCheckResE (mkTracedTypeCheckError env p (msg desc "Actual type contains bottomType") : notes)
    checkScheme (TypeCheckResult notes (SType _ req _ _ desc)) | containsBottomType req = TypeCheckResE (mkTracedTypeCheckError env p (msg desc "Required type contains bottomType") : notes)
    checkScheme (TypeCheckResult notes (SType _ _ reqDecl _ desc)) | containsBottomType reqDecl = TypeCheckResE (mkTracedTypeCheckError env p (msg desc "Required declaration type contains bottomType") : notes)
    -- NOTE: the @act ⊆ req@ invariant is NOT enforced here. During inference a requirement (req)
    -- may legitimately run ahead of the still-widening actual (act) — e.g. an 'ArrowTo' dest that
    -- requires @Integer@ while its actual is still the wider @Integer ∪ Float@ reach. The invariant
    -- is checked once, after convergence, in 'TypeCheck.Decode.toMeta'.
    checkScheme s = s

    msg :: String -> String -> String
    msg desc problem = printf "Scheme failed check at setScheme %s(point %s): \n\t %s - %s \n\t Constraining:%s\n\t\tto: %s\n" baseMsg (show p) problem desc (show oldCon) (show newCon)

setSchemeAct :: FEnv -> VConstraint -> (RConstraint, RConstraint) -> VarMeta -> Type -> String -> FEnv
setSchemeAct env con cons p t msg = setScheme env con cons p scheme' ("Act " ++ msg)
  where
    scheme = descriptor env p
    scheme' = fmap (\s -> s{stypeAct=t}) scheme

setSchemeReq :: FEnv -> VConstraint -> (RConstraint, RConstraint) -> VarMeta -> Type -> String -> FEnv
setSchemeReq env con cons p t msg = setScheme env con cons p scheme' ("Req " ++ msg)
  where
    scheme = descriptor env p
    scheme' = fmap (\s -> s{stypeReq=t}) scheme

setSchemeActReq :: FEnv -> VConstraint -> (RConstraint, RConstraint) -> SchemeActReq -> VarMeta -> Type -> String -> FEnv
setSchemeActReq env con cons SchemeAct p t msg = setSchemeAct env con cons p t msg
setSchemeActReq env con cons SchemeReq p t msg = setSchemeReq env con cons p t msg

setSchemeConVaenv :: FEnv -> VConstraint -> (RConstraint, RConstraint) -> SchemeActReq -> TypeVarArgEnv -> String -> FEnv
setSchemeConVaenv env con cons actOrReq types msg = foldr (\(m, t) env' -> setSchemeActReq env' con cons actOrReq m t msg) env (H.elems $ Z.zip (constraintVarArgEnv con) types)

-- | Tries to join two 'SType' as equal to each other and returns their updated values
equalizeSTypes :: FEnv -> TypeVarArgEnv -> (SType, SType) -> (TypeVarArgEnv, SType, SType)
equalizeSTypes FEnv{feTypeEnv} vaenv (stype1@SType{stypeAct=act1, stypeReq=req1, stypeReqDecl=reqDecl1}, stype2@SType{stypeAct=act2, stypeReq=req2, stypeReqDecl=reqDecl2}) = do
  let (vaenv'1, actBoth) = intersectTypesWithVarEnv feTypeEnv vaenv act1 act2
  let (vaenv'2, reqBoth) = intersectTypesWithVarEnv feTypeEnv vaenv req1 req2
  -- Equalized points denote the same term, so both decl-origin requirement spellings apply.
  -- The meet's var discoveries are NOT folded into the returned env: the decl channel is a
  -- passenger here and must not perturb the actual-side var resolution. Identical spellings
  -- skip the meet (a per-epoch respell of the same set — the decl channel's own comparisons
  -- are spelling-insensitive, so nothing downstream depends on the respelled form).
  let reqDeclBoth | reqDecl1 == reqDecl2 = reqDecl1
                  | otherwise            = intersectTypesEnv feTypeEnv vaenv reqDecl1 reqDecl2
  let vaenv' = H.unionWith (intersectTypes feTypeEnv) vaenv'1 vaenv'2
  (vaenv', stype1{stypeAct=actBoth, stypeReq=reqBoth, stypeReqDecl=reqDeclBoth}, stype2{stypeAct=actBoth, stypeReq=reqBoth, stypeReqDecl=reqDeclBoth})


-- | A helper for the 'PropEq' 'Constraint' that applies to both the actual and required types.
-- After the meets, the slot settle ('settlePropSlot') drops settle-forced unsettled forms from
-- the super's actual slot and tightens its requirement slot to the declared-bound meet.
updateSchemeProp :: FEnv -> STypeVarArgEnv -> SType -> TypeVarAux -> SType -> (STypeVarArgEnv, SType, SType)
updateSchemeProp env@FEnv{feTypeEnv} vaenv super@SType{stypeAct=superAct, stypeReq=superReq, stypeReqDecl=superReqDecl} propName sub@SType{stypeAct=subAct, stypeReq=subReq, stypeReqDecl=subReqDecl} = (vaenv', super{stypeAct=superAct'', stypeReq=superReq'', stypeReqDecl=superReqDecl''}, sub{stypeAct=subAct', stypeReq=subReq', stypeReqDecl=subReqDecl'})
  where
    (actVaenv', superAct', subAct') = updateTypeProp feTypeEnv (fmap stypeAct vaenv) superAct propName subAct
    -- A requirement-side projection never commits a respell that LOSES a type-var spelling
    -- (@∀Map[/K=$/MK, /V=$/MV]@ meeting a sub whose frame reads the pointers as ⊤ would
    -- store @∀Map[/K=⊤, /V=⊤]@ — read WIDER, failing 'verifySchemeReqLowers'): the same
    -- 'typeVarLost' guard the reverse narrowing applies. The refused projection re-runs next
    -- epoch once the sub's spelling resolves; the actual side is untouched (its meets only
    -- gain spellings here).
    (reqVaenv', superReq'0, subReq'0) = updateTypeProp feTypeEnv (fmap stypeReq vaenv) superReq propName subReq
    superReq' | typeVarLost superReq superReq'0 = superReq
              | otherwise                       = superReq'0
    subReq' | typeVarLost subReq subReq'0 = subReq
            | otherwise                   = subReq'0
    -- The slot projection is a spelled-structure relation, so the decl channel flows through it
    -- too — this is how a pattern's declared field content reaches a binding variable's point.
    -- Fully-inert channel (both sides ⊤) skips the projection outright.
    (declVaenv', superReqDecl'0, subReqDecl'0)
      | superReqDecl == PTopType && subReqDecl == PTopType = (fmap stypeReqDecl vaenv, superReqDecl, subReqDecl)
      | otherwise = updateTypeProp feTypeEnv (fmap stypeReqDecl vaenv) superReqDecl propName subReqDecl
    superReqDecl' | typeVarLost superReqDecl superReqDecl'0 = superReqDecl
                  | otherwise                               = superReqDecl'0
    subReqDecl' | typeVarLost subReqDecl subReqDecl'0 = subReqDecl
                | otherwise                           = subReqDecl'0
    (superAct'', superReq'', superReqDecl'') = settlePropSlot env (fmap stypeAct vaenv) propName superAct' superReq' superReqDecl'
    vaenv' = H.intersectionWith (\t s -> s{stypeReqDecl=t}) declVaenv' $ H.intersectionWith (\t s -> s{stypeReq=t}) reqVaenv' $ H.intersectionWith (\t s -> s{stypeAct=t}) actVaenv' vaenv

-- | A helper for the 'AddArg' 'Constraint'
addArgToType :: FEnv -> TypeVarArgEnv -> Type -> TypeVarAux -> Maybe Type
addArgToType _ _ PTopType _ = Nothing
addArgToType _ _ (UnionType (Just _) _ _) _ = Nothing
-- A type-variable source is a point's own top-level type, where no partial encloses it: the
-- parent and scope layers are both the constraint's env there (the fragment rule of
-- 'substituteKeep'), so every loc resolves through the same lookup. A miss is the ⊤ an unbound
-- variable denotes ('vaenvLookup'), which the ⊤ clause above already reads as "not known yet"
-- (the constraint is retried each epoch), and a 'TVApplied' reference resolves through its
-- base — a keyed lookup could never hit it. Measured 2026-09-18 (build/scratch/addargtrace.py,
-- three core + five integration files): no type variable of any loc reaches this clause.
addArgToType env vaenv (TypeVar v _ _) newArg = addArgToType env vaenv (vaenvLookup vaenv v) newArg
addArgToType FEnv{feTypeEnv} _ (UnionType Nothing partials []) newArg = Just $ unionAllTypes feTypeEnv $ map singletonType kept
  where
    grafted = map fromPartial $ splitUnionType partials
    -- An applied arg can REFUTE a name-ambiguity candidate: a relative reference expands to
    -- every matching name (@insert@ ⇒ @/Data/Map/insert + /sorted./Data/List/insert@), and
    -- the application then grafts its args onto every candidate — but no value of a name can
    -- carry arg keys outside the name's declared arg-sets ('declaredArgKeys' — partial
    -- application never adds args), so a candidate whose REQUIRED keyset (lower-bounded mode)
    -- no declared arg-set covers admits no value. Keeping it rides every downstream ACT union:
    -- convergence slows past the epoch budget and TreeBuild's followup chains fail on the
    -- grafted form, silently dropping real dispatch arms ("Failed match in eval callTree").
    -- ARROW-ONLY names only: a DATA-named leaf with extra keys is a legitimate raw
    -- method-style form (@Integer(/r=Integer)@ from @5 - 2@), and an undeclared name is open
    -- world. Purely structural (parse-time arg-name sets), so the decision is epoch-stable;
    -- if every candidate would drop, keep them all (never publish an empty union).
    argsRefuted p@PartialType{ptArgMode=PtArgMode lb _} =
      lb && null (typeGraphData feTypeEnv (ptName p)) && not (relArgKeysFeasible feTypeEnv p (ptName p))
    kept = case filter (not . argsRefuted) grafted of
      [] -> grafted
      ps -> ps
    fromPartial partial@PartialType{ptVars, ptArgs} = case newArg of
      TVArg newArg' -> partial{ptArgs=H.insertWith (unionTypes feTypeEnv) newArg' PTopType ptArgs}
      TVVar newArg' -> partial{ptVars=H.insertWith (unionTypes feTypeEnv) newArg' PTopType ptVars}
      TVApplied k _ -> error $ printf "Unexpected TVApplied %s used as an arg name in addArgToType" (show k)
addArgToType _ _ _ _ = Nothing

-- | A helper for the 'AddArg' 'Constraint'
addArgToScheme :: FEnv -> STypeVarArgEnv -> SType -> TypeVarAux -> SType -> SType
addArgToScheme env@FEnv{feTypeEnv} vaenv SType{stypeAct=srcAct, stypeReq=srcReq, stypeReqDecl=srcReqDecl} newArgName dest@SType{stypeAct=destAct, stypeReq=destReq, stypeReqDecl=destReqDecl} = dest{stypeAct=destAct', stypeReq=destReq', stypeReqDecl=destReqDecl'}
  where
    destAct' = case addArgToType env (fmap stypeAct vaenv) srcAct newArgName of
      Just addDestAct -> snd $ intersectTypesWithVarEnv feTypeEnv (fmap stypeAct vaenv) destAct addDestAct
      Nothing         -> destAct
    destReq' = case addArgToType env (fmap stypeReq vaenv) srcReq newArgName of
      Just addDestReq -> intersectTypesEnv feTypeEnv (fmap stypeReq vaenv) destReq addDestReq
      Nothing         -> destReq
    destReqDecl' = case addArgToType env (fmap stypeReqDecl vaenv) srcReqDecl newArgName of
      Just addDestReqDecl -> intersectTypesEnv feTypeEnv (fmap stypeReqDecl vaenv) destReqDecl addDestReqDecl
      Nothing             -> destReqDecl

-- | A helper for the 'AddArg' 'Constraint'
-- | A helper for the 'AddInferArg' 'Constraint'. The grafted source is met INTO the destination
-- under the constraint's own var env, as 'addArgToScheme' does: the source carries the
-- declaration's in-scope external references (@bindap[/M=∀Monad[/T=$_A]]@), and a meet that
-- does not know the scope reads every such reference as the ⊤ it would denote unbound
-- (@/M=∀Monad[/T=⊤]@) — an erasure the destination then keeps for good, since it is a
-- same-set respelling no later write can distinguish. It went unnoticed only while the
-- destination was still ⊤ at the first graft, where the meet is the identity.
addInferArgToScheme :: FEnv -> STypeVarArgEnv -> SType -> SType -> SType
addInferArgToScheme env@FEnv{feTypeEnv} vaenv SType{stypeAct=srcAct, stypeReq=srcReq, stypeReqDecl=srcReqDecl} dest@SType{stypeAct=destAct, stypeReq=destReq, stypeReqDecl=destReqDecl} = dest{stypeAct=destAct', stypeReq=destReq', stypeReqDecl=destReqDecl'}
  where
    destAct' = case addInferArgToType env (fmap stypeAct vaenv) srcAct of
      Just addDestAct -> snd $ intersectTypesWithVarEnv feTypeEnv (fmap stypeAct vaenv) destAct addDestAct
      Nothing         -> destAct
    destReq' = case addInferArgToType env (fmap stypeReq vaenv) srcReq of
      Just addDestReq -> intersectTypesEnv feTypeEnv (fmap stypeReq vaenv) destReq addDestReq
      Nothing         -> destReq
    destReqDecl' = case addInferArgToType env (fmap stypeReqDecl vaenv) srcReqDecl of
      Just addDestReqDecl -> intersectTypesEnv feTypeEnv (fmap stypeReqDecl vaenv) destReqDecl addDestReqDecl
      Nothing             -> destReqDecl

stypeConstraint :: SConstraint -> TypeCheckResult RConstraint
stypeConstraint = mapMCon id

updateCOVarArgEnv :: STypeVarArgEnv -> CVarArgEnv SType -> CVarArgEnv SType
updateCOVarArgEnv oVaenv' ioVaenv = H.intersectionWith (\(si, _) so -> (si, so)) ioVaenv oVaenv'

updateCOVarArgEnvAct :: TypeVarArgEnv -> CVarArgEnv SType -> CVarArgEnv SType
updateCOVarArgEnvAct oVaenv' ioVaenv = H.intersectionWith (\(si, so) oAct' -> (si, so{stypeAct=oAct'})) ioVaenv oVaenv'

computeConstraint :: FEnv -> RConstraint -> (Bool, RConstraint)
computeConstraint env con@(Constraint _ vaenv (EqualsKnown i p tp)) = (True, con{conVaenv = updateCOVarArgEnvAct vaenv' vaenv, conDat=EqualsKnown i stype' tp})
  where
    (vaenv', stype', _) = equalizeSTypes env (fmap (stypeAct . snd) vaenv) (p, SType tp tp tp Nothing "")
computeConstraint env con@(Constraint _ vaenv (EqPoints i p1 p2)) = (False, con{conVaenv = updateCOVarArgEnvAct vaenv' vaenv, conDat=EqPoints i stype' stype'})
  where
    (vaenv', stype', _) = equalizeSTypes env (fmap (stypeAct . snd) vaenv) (p1, p2)
computeConstraint FEnv{feTypeEnv} con@(Constraint _ vaenv (BoundedByKnown i p@SType{stypeAct=act, stypeReq=req, stypeReqDecl=reqDecl} boundTp)) = (True, con{conDat=BoundedByKnown i (p{stypeAct=act', stypeReq=req', stypeReqDecl=reqDecl'}) boundTp})
  where
    vaenv' = fmap (stypeAct . snd) vaenv
    boundTp' = relaxNameBoundArgs act $ case boundTp of
      TypeVar v _ _                                   -> H.lookupDefault PTopType v vaenv'
      UnionType (Just (preds, negLeafs, _)) _ _ ->
        snd $ differenceTypeWithEnv feTypeEnv vaenv' (expandPredicates feTypeEnv vaenv' preds) (UnionType Nothing negLeafs [])
      t                                             -> t
    -- Per-channel bound: when the channel is a positive union, the predicate expansion is
    -- restricted to the channel's own leaf names — the dropped members are name-disjoint
    -- from every channel leaf, and 'differenceTypeWithEnv' never renames a leaf, so the
    -- meet result is unchanged ('expandPredicatesWithinNames''s operand contract). Any
    -- other channel shape (⊤, a type var, a predicate top) keeps the full shared bound —
    -- there the expansion IS the stored meet content and restriction would narrow it.
    boundFor chanT = case (boundTp, chanT) of
      (UnionType (Just (preds, negLeafs, _)) _ _, UnionType Nothing chanLeafs _) | not (H.null chanLeafs) ->
        relaxNameBoundArgs act $ snd $ differenceTypeWithEnv feTypeEnv vaenv' (expandPredicatesWithinNames feTypeEnv vaenv' (H.keysSet chanLeafs) preds) (UnionType Nothing negLeafs [])
      _ -> boundTp'
    act' = intersectTypesEnv feTypeEnv vaenv' act (boundFor act)
    req' = intersectTypesEnv feTypeEnv (fmap (stypeReq . snd) vaenv) req (boundFor req)
    -- A known bound is a spelled type (annotation seeds enter here), so it is decl-origin.
    reqDecl' = intersectTypesEnv feTypeEnv (fmap (stypeReqDecl . snd) vaenv) reqDecl (boundFor reqDecl)
computeConstraint _ con@(Constraint _ _ (BoundedByObjs _ SType{stypeReq=PTopType} _)) = (False, con)
computeConstraint FEnv{feTypeEnv, feCompletedAllObjsKey, feCompletedAllObjs} con@(Constraint _ vaenv (BoundedByObjs i p@SType{stypeReq=pReq, stypeReqDecl=pReqDecl} objMapBoundUb)) = (False, con{conDat=BoundedByObjs i (p{stypeReq=pReq', stypeReqDecl=pReqDecl'}) objMapBoundUb})
  where
    vaenv' = fmap (stypeAct . snd) vaenv
    argsBoundUb = setArgMode vaenv' PtArgExact $ powersetType feTypeEnv vaenv' $ UnionType Nothing (joinUnionType $ map partialToType $ H.keys $ snd $ splitVarArgEnv $ constraintVarArgEnv con) []
    -- Also offer the /completed/ form of each bound object: a nested param-form annotation
    -- (e.g. @Box[T]@ inside @unwrap(b=Box[T])@ for @data Box[T](v -> T)@) gets its declared fields
    -- restored to @Box[T](v=$T)@ (see 'completeDataPartials'), so a field-form value @Box(v=...)@
    -- being bounded can reconcile with it. The original (uncompleted) forms are kept in the union
    -- too, so a param-form value still matches without widening.
    -- 'objMapBoundUb' is the 'feUnionAllObjs' descriptor (substituted uniformly by 'showCon'), so its
    -- completion is identical across all 'BoundedByObjs' constraints whose bound equals the epoch's
    -- cached universe 'feCompletedAllObjsKey'. Reuse the per-epoch cache EXACTLY in that case (the
    -- common one — a converged universe), and recompute only if the bound differs (e.g. the universe
    -- shifted mid-epoch), so the result is byte-identical to the uncached computation.
    objMapBoundUb' | objMapBoundUb == feCompletedAllObjsKey = feCompletedAllObjs
                   | otherwise = unionTypes feTypeEnv objMapBoundUb (completeDataPartials feTypeEnv objMapBoundUb)
    -- A type is REQUIRED to be a declared/defined object, but that does not mean it ACTUALLY is one:
    -- being one of the known objects is only a requirement. So narrow only the requirement against
    -- the object-universe bound and leave the actual untouched.
    --
    -- Relax each bound object's self-referential field (one that recurses back to a type the
    -- object participates in) so a destructured value whose corresponding field is still an
    -- unconstrained pattern variable can reconcile with it (see 'relaxRecursiveSelfArgs').
    -- The objects universe is closed-world (only currently-declared data objects), but an OPEN
    -- (unsealed) class may gain instances in a future file, so a value typed by one is genuinely
    -- inhabitable and must survive the bound rather than empty. Admit the requirement's OWN
    -- open-class predicate components (with their real element bounds) as inhabitable bound members
    -- ('openClassPart pReq'): an open-class requirement (e.g. a @Functor[Eq]@ law argument with no
    -- in-scope instance) must survive the object-universe bound rather than empty. Sealed classes are
    -- excluded — they equal their declared members, already in the universe.
    --
    -- Only narrow a *concrete* requirement. A Top requirement meets the whole universe to the whole
    -- universe (every known object) — not a narrowing, and a performance blowup — so leave it be
    -- (the 'stypeReq=PTopType' guard above already short-circuits it). A named requirement instead
    -- meets only its name-matching objects, tightening the requirement's arg fields (e.g.
    -- @/f(/a=Top)@ narrows to @/f(/a=<declared>)@).
    -- The narrowing is applied per requirement channel — the accumulated 'stypeReq' and the
    -- decl-origin 'stypeReqDecl' — each against its own var-env and its own open-class
    -- components ('openClassPart'). Narrowing spelled requirement content against declared
    -- objects preserves the decl channel's origin purity: no actual-derived input enters.
    narrowChannel chanVaenv chanReq = chanReq'
      where
        boundUbReq = relaxRecursiveSelfArgs feTypeEnv $ unionTypes feTypeEnv (unionTypes feTypeEnv objMapBoundUb' (openClassPart feTypeEnv chanReq)) argsBoundUb
        boundNames = case boundUbReq of
          UnionType _ bLeafs _ -> map ptName $ splitUnionType bLeafs
          _                    -> []
        -- The universe bound is open-world on NAMES: a requirement leaf whose name matches no
        -- object in the universe (a compiler annotation whose compile.ct declaration is not in
        -- this import closure, or any name declared outside the current program) offers nothing
        -- to tighten against, so it passes through unchanged rather than emptying the requirement.
        -- A name the universe DOES know meets its name-matching objects as before.
        nameKnown l = any (relativeNameMatches (ptName l)) boundNames
        chanReq' | chanReq == PTopType = chanReq
                 | otherwise           = case chanReq of
                     UnionType mTop leafs consts ->
                       let (knownLeafs, unknownLeafs) = partition nameKnown (splitUnionType leafs)
                           metKnown = [ snd $ intersectTypesWithVarEnv feTypeEnv chanVaenv (singletonType l) boundUbReq
                                      | l <- knownLeafs ]
                           metTopConsts = snd $ intersectTypesWithVarEnv feTypeEnv chanVaenv (UnionType mTop H.empty consts) boundUbReq
                       in unionAllTypes feTypeEnv (UnionType Nothing (joinUnionType unknownLeafs) [] : metTopConsts : metKnown)
                     _ -> snd $ intersectTypesWithVarEnv feTypeEnv chanVaenv chanReq boundUbReq
    reqChanVaenv = fmap (stypeReq . snd) vaenv
    declChanVaenv = fmap (stypeReqDecl . snd) vaenv
    pReq' = narrowChannel reqChanVaenv pReq
    -- Exact-input memo: the two channels are identical until an echo writer diverges them
    -- (most points, most epochs), and 'narrowChannel' is a pure function of (vaenv, req) —
    -- reuse the req result only when BOTH inputs match, so no cross-channel contamination.
    pReqDecl' | pReqDecl == pReq && declChanVaenv == reqChanVaenv = pReq'
              | otherwise = narrowChannel declChanVaenv pReqDecl
computeConstraint FEnv{feTypeEnv} con@(Constraint _ vaenv (NoReturnArg i p@SType{stypeAct=act@(UnionType Nothing _ _)})) = (True, con{conVaenv=updateCOVarArgEnvAct vaenv'' vaenv, conDat=NoReturnArg i p{stypeAct=act'}})
  where
    vaenv' = fmap (stypeAct . snd) vaenv
    argsBoundUb = setArgMode vaenv' PtArgExact $ powersetType feTypeEnv vaenv' $ UnionType Nothing (joinUnionType $ map partialToType $ H.keys $ snd $ splitVarArgEnv $ constraintVarArgEnv con) []
    (vaenv'', act') = differenceTypeWithEnv feTypeEnv vaenv' act argsBoundUb
computeConstraint FEnv{feTypeEnv} con@(Constraint _ vaenv (NoReturnArg i p@SType{stypeAct=UnionType (Just topData) posLeafs consts}))
  | not (H.null posLeafs) = (True, con{conVaenv=updateCOVarArgEnvAct vaenv'' vaenv, conDat=NoReturnArg i p{stypeAct=act'}})
  where
    vaenv' = fmap (stypeAct . snd) vaenv
    argsBoundUb = setArgMode vaenv' PtArgExact $ powersetType feTypeEnv vaenv' $ UnionType Nothing (joinUnionType $ map partialToType $ H.keys $ snd $ splitVarArgEnv $ constraintVarArgEnv con) []
    (vaenv'', posResult) = differenceTypeWithEnv feTypeEnv vaenv' (UnionType Nothing posLeafs []) argsBoundUb
    posLeafs' = case posResult of
      UnionType Nothing pos' _ -> pos'
      _                        -> posLeafs
    act' = UnionType (Just topData) posLeafs' consts
computeConstraint _ con@(Constraint _ _ NoReturnArg{}) = (False, con)
-- An input argument actually IS whatever it is required to be, AND VICE VERSA: a declaration LHS is
-- an input, so its actual and requirement denote the same thing and must agree. Meet the two and
-- assign it to BOTH bounds. Pulling the actual down to the requirement gives it the requirement's
-- object-universe bound ('BoundedByObjs' now narrows only the requirement); pulling the requirement
-- down to the actual carries an ANNOTATION written on the LHS (e.g. @Cons[$T -> Integer]@, whose
-- @/T=Integer@ arrives on the actual) into the requirement, so a destructured field @/head=$T@ then
-- resolves through @/T@ to @Integer@ in the requirement rather than staying Top - which was leaving
-- the field binding's own requirement Top, so its EqReqAct meet could not narrow anything. As uses
-- (and the ArrowTo reverse propagation) keep narrowing, this re-runs each epoch (returns False, not
-- "done"); the meet only shrinks either bound (@act ∩ req ⊆ act@, @⊆ req@), so both stay monotone
-- and a bare 'TypeVar' bound is preserved by the intersection.
computeConstraint FEnv{feTypeEnv} con@(Constraint _ vaenv (EqReqAct i p@SType{stypeAct=act, stypeReq=req})) = (False, con{conDat=EqReqAct i p{stypeAct=meet, stypeReq=meet}})
  where
    vaenv' = fmap (stypeAct . snd) vaenv
    -- A destructure pattern's field carries only the value inference gives it, which for a bare
    -- pattern variable (@#md(text)@) is Top: the data/annotation declaration says the field IS its
    -- declared type (@text -> String@), so an LHS input actually is that. 'BoundedByObjs' would
    -- narrow it via the object universe, but now only touches the requirement — and an open-class
    -- bound on the pattern's slot (@dshow(s -> DShow)@) re-widens the requirement's field, leaving
    -- the meet Top and building @Show/MD(text=Top)@. So complete each partial's concrete declared
    -- fields onto the actual directly (option 1): only fields whose declared type is fully concrete
    -- are carried (a parameter-typed field @v -> $A@ stays as inference left it, so no free
    -- parameter is reintroduced). This narrows the field on both bounds through the meet below.
    actC = completeConcreteFields feTypeEnv act
    -- An LHS input's actual and requirement denote the same thing, so meet them and set BOTH: pulling
    -- the actual down to the requirement gives it the requirement's object-universe bound
    -- ('BoundedByObjs' now narrows only the requirement); pulling the requirement down to the actual
    -- carries an LHS ANNOTATION (@Cons[$T -> Integer]@, whose @/T=Integer@ lands on the actual) into
    -- the requirement so a destructured field @/head=$T@ resolves through @/T@ there. Resolve the
    -- meet's freshly-discovered bindings back into the result: meeting a narrow actual
    -- (@∀ConsList[/T=∀Eq]@) with a requirement carrying a bare recursion var (@∀ConsList[/T=$_T]@)
    -- records @$_T ↦ ∀Eq@ but keeps @$_T@ in the type, which unresolved reads WIDER and re-widens it
    -- (a 'verifySchemeActLowers' failure), the trap 'intersectPartialsBase'/'arrowConstrainUbs'
    -- resolve back. Still-unresolved vars stay symbolic ('substituteKeep').
    (ivaenv, meet0) = intersectTypesWithVarEnv feTypeEnv vaenv' actC req
    -- Resolve the meet's freshly-discovered bindings back into the result. Meeting a concretized
    -- actual (@∀ConsList[/T=∀Eq]@) with a requirement carrying the recursion element var
    -- (@∀ConsList[/T=$_T]@) records @/T ↦ ∀Eq@ but keeps @$_T@ (which is literally @TVVar "/T"@,
    -- referring to the enclosing Cons's own element) in the type — unresolved it reads WIDER and the
    -- requirement re-widens (@verifySchemeReqLowers@) and the actual re-widens
    -- (@verifySchemeActLowers@). Resolving is exactly correct: the tail's element IS the enclosing
    -- element. 'substituteKeep' leaves any still-unresolved var symbolic.
    newBindings = H.difference ivaenv vaenv'
    meet1 | H.null newBindings = meet0
          | otherwise          = substituteKeep feTypeEnv newBindings meet0
    -- The meet once had to restore an outer var spelling it widened to ⊤ at a nested slot (a
    -- declaration's implicit-through-usage element link, @b -> Box[$T -> $_T]@, unbound in the
    -- constraint's vaenv). Measured 2026-09-18 on list, map, set, mapRecursion and boxUnwrap:
    -- that restoration fired zero times — 'substituteKeep' keeps such a spelling symbolic
    -- ("still-unresolved vars stay symbolic") — so the meet is stored as computed. The one
    -- place a loss still happens is the property install ('updateTypeProp' restores it there).
    meet = meet1
computeConstraint env con@(Constraint _ _ (ArrowTo i src dest)) = case arrowConstrainUbs env con src dest of
    TypeCheckResult _ (srcAct', srcReq', destAct', destRT') -> (False, con{conDat=ArrowTo i src{stypeAct=srcAct', stypeReq=srcReq'} dest{stypeAct=destAct', stypeTree=destRT'}})
    TypeCheckResE _ -> (True, con)
computeConstraint env con@(Constraint _ vaenv (PropEq i (super, name) sub)) = (False, con{conVaenv=updateCOVarArgEnv vaenv' vaenv, conDat=PropEq i (super', name) sub'})
  where
    (vaenv', super', sub') = updateSchemeProp env (fmap snd vaenv) super name sub
computeConstraint env@FEnv{feTypeEnv} con@(Constraint _ _ (FnInputBound i (callP, fnKey, inKey) inp@SType{stypeAct=inpAct})) = (False, con{conDat=FnInputBound i (callP, fnKey, inKey) inp{stypeAct=inpAct'}})
  where
    inpAct' = case fnArgInputBound env (stypeAct callP) fnKey inKey of
      Just b  -> intersectTypes feTypeEnv inpAct b
      Nothing -> inpAct
computeConstraint env con@(Constraint _ vaenv (AddArg i (src, newArgName) dest)) = (False, con{conDat=AddArg i (src, newArgName) dest'})
  where
    dest' = addArgToScheme env (fmap snd vaenv) src newArgName dest
computeConstraint env con@(Constraint _ vaenv (AddInferArg i src dest)) = (False, con{conDat=AddInferArg i src dest'})
  where
    dest' = addInferArgToScheme env (fmap snd vaenv) src dest
computeConstraint FEnv{feTypeEnv} con@(Constraint _ vaenv (SetArgMode i mode src@SType{stypeAct=srcAct, stypeReq=srcReq, stypeReqDecl=srcReqDecl} dest@SType{stypeAct=destAct, stypeReq=destReq, stypeReqDecl=destReqDecl})) = (False, con{conDat=SetArgMode i mode src dest{stypeAct=destAct', stypeReq=destReq', stypeReqDecl=destReqDecl'}})
  where
    vaenv' = fmap (stypeAct . snd) vaenv
    argModeType venv t = if mode
      then powersetType feTypeEnv venv t
      else setArgMode venv PtArgAny t
    destAct' = intersectTypes feTypeEnv (argModeType vaenv' srcAct) destAct
    -- The arg-mode transform is an upper bound on the destination, so it applies to the required
    -- type as well as the actual (both are upper bounds — see 'SType'); mirror it on each side.
    destReq' = intersectTypes feTypeEnv (argModeType (fmap (stypeReq . snd) vaenv) srcReq) destReq
    destReqDecl' = intersectTypes feTypeEnv (argModeType (fmap (stypeReqDecl . snd) vaenv) srcReqDecl) destReqDecl
computeConstraint FEnv{feTypeEnv} con@(Constraint _ vaenv (ConWhere i base cond res)) = (False, con{conDat=ConWhere i base cond res'}) -- TODO Currently using temp equals relationship, switch to actual constraint
  where
    vaenv' = fmap (stypeAct . snd) vaenv
    tp'' = case maybeGetSingleton (stypeAct cond) of
      Just tp' -> intersectTypesEnv feTypeEnv vaenv' (stypeAct res) $ typeAddPreds (stypeAct base) (PredsOne $ PredExpr tp')
      Nothing -> stypeAct res
    -- The where condition refines an upper bound on the result, so it applies to the required type
    -- as well as the actual (both are upper bounds — see 'SType'); refine each side against its own
    -- ambient var-env, keyed on the same condition value.
    tpReq'' = case maybeGetSingleton (stypeAct cond) of
      Just tp' -> intersectTypesEnv feTypeEnv (fmap (stypeReq . snd) vaenv) (stypeReq res) $ typeAddPreds (stypeReq base) (PredsOne $ PredExpr tp')
      Nothing -> stypeReq res
    tpReqDecl'' = case maybeGetSingleton (stypeAct cond) of
      Just tp' -> intersectTypesEnv feTypeEnv (fmap (stypeReqDecl . snd) vaenv) (stypeReqDecl res) $ typeAddPreds (stypeReqDecl base) (PredsOne $ PredExpr tp')
      Nothing -> stypeReqDecl res
    res' = res{stypeAct=tp'', stypeReq=tpReq'', stypeReqDecl=tpReqDecl''}
computeConstraint env@FEnv{feTypeEnv} con@(Constraint _ vaenv (UnionOf i parent children)) = (False, con{conVaenv=updateCOVarArgEnvAct vaenv' vaenv, conDat=UnionOf i parentST' children})
  where
    chAct = unionAllTypesWithEnv feTypeEnv H.empty $ filter (not . isTypeVar) $ map stypeAct children
    chReq = unionAllTypesWithEnv feTypeEnv H.empty $ filter (not . isTypeVar) $ map stypeReq children
    chReqDecl = unionAllTypesWithEnv feTypeEnv H.empty $ filter (not . isTypeVar) $ map stypeReqDecl children
    actVaenv = fmap (stypeAct . snd) vaenv
    (vaenv', parentST', _) = equalizeSTypes env actVaenv (parent, SType (compactType feTypeEnv actVaenv chAct) chReq chReqDecl Nothing "")

saveConstraint :: VConstraint -> RConstraint -> RConstraint -> FEnv -> FEnv
saveConstraint con@(Constraint _ _ (UnionOf _ parentPnt _)) oldCon newCon@(Constraint _ vaenv' (UnionOf _ parent' _)) env = env''
  -- TODO Delete special case of saveConstraint for UnionOf by fixing the bug
  where
    env' = setScheme env con (oldCon, newCon) parentPnt (return parent') "UnionOf"
    env'' = setSchemeConVaenv env' con (oldCon, newCon) SchemeAct (fmap (stypeAct . snd) vaenv') "EqualsKnown env"
saveConstraint vals old new env = saveDat $ saveVaenv env
  where
    saveDat en = foldr (\(p, v) e -> setScheme e vals (old, new) p (pure v) (show v)) en (zip (constraintDatMetas $ conDat vals) (constraintDatMetas $ conDat new))
    saveVaenv en = foldr saveVaenvPair en (H.elems $ H.intersectionWith (,) (conVaenv vals) (conVaenv new))
    saveVaenvPair ((pIn, pOut), (vIn, vOut)) en = en''
      where
        en' = setScheme en vals (old, new) pIn (pure vIn) (show vIn)
        en'' = setScheme en' vals (old, new) pOut (pure vOut) (show vOut)

-- |
-- This takes a constraint and tries to apply it in the environment.
-- It will return the updated environment and a boolean that is true if the constraint is done.
-- If it is done, it can be safely removed and no longer needs to be executed.
executeConstraint :: Int -> VConstraint -> StateT FEnv TypeCheckResult Bool
executeConstraint cid con = do
  modify $ startConstraint con
  env@FEnv{feEnvStamp} <- get
  let conMemo = feConMemo env
  case stypeConstraint $ showCon env con of
    TypeCheckResE _ -> return True
    TypeCheckResult _ con' -> do
      -- Skip-if-unchanged: 'computeConstraint' is a pure function of the resolved view
      -- @con'@ plus, for the kinds that read the per-epoch env caches, those caches
      -- ('feReachesTypeEnv' for ArrowTo/PropEq/FnInputBound via 'mkReachesEnv' /
      -- 'settlePropSlot' / 'fnArgInputBound'; the universe is already substituted INTO a
      -- BoundedByObjs view by 'showCon', and 'feCompletedAllObjs' is an exact function of
      -- it). An ArrowTo additionally resolves its 'conOs' object-side arg entries
      -- ('conOaResolveKey'). When every component of that key equals the last execution's,
      -- the recomputed stores would all be refused as unchanged by 'setScheme' — a provable
      -- no-op, so skip the compute and save entirely and repeat the prune decision.
      let stampKey = case conDat con of
            ArrowTo{}      -> feEnvStamp
            PropEq{}       -> feEnvStamp
            FnInputBound{} -> feEnvStamp
            _              -> 0
      let oaKey = case conDat con of
            ArrowTo{} -> conOaResolveKey env con
            _         -> []
      -- 'conOs' is CONSTANT for a given constraint (the same cid always resolves the same
      -- object list), so it is zeroed out of the memoized view rather than structurally
      -- compared every execution — its object expressions carry every meta and source
      -- position, and comparing them was a third of a fragmented solve. The parts of the
      -- objects a compute actually reads are covered separately ('conOaResolveKey').
      let conMemoKey = con'{conOs=[]}
      case IM.lookup cid conMemo of
        Just (ConMemo memoStamp memoCon memoOa memoPrune)
          | memoStamp == stampKey && memoCon == conMemoKey && memoOa == oaKey -> return memoPrune
        _ -> do
          let (prune, con'') = computeConstraint env con'
          modify $ saveConstraint con con' con''
          modify $ \e -> e{feConMemo = IM.insert cid (ConMemo stampKey conMemoKey oaKey prune) (feConMemo e)}
          return prune

-- |
-- Applies the constraints continuously to update the environment.
-- It should eventually converge after which applying the constraints no longer has any effect.
-- This function also accepts a limit 'Integer' to indicate a maximum number of times a constraint can be used.
-- A 'TypeCheckError' will be thrown if it has not converged by the time the limit is reached.
-- Returns whether the constraints CONVERGED (a limit failure or an aborting env build returns
-- 'False' after noting its error), so a per-fragment solve loop can stop before dependents
-- solve against a half-settled fragment.
runConstraints :: Integer -> [VConstraint] -> StateT FEnv TypeCheckResult Bool
runConstraints limit cons = do
  -- Fresh skip-memo per constraint set: memo keys are positions in THIS list.
  modify $ \e -> e{feConMemo = IM.empty}
  modify startConstrainSolve
  runConstraintsIdx limit (zip [0..] cons)

runConstraintsIdx :: Integer -> [(Int, VConstraint)] -> StateT FEnv TypeCheckResult Bool
runConstraintsIdx _ [] = return True
runConstraintsIdx 0 _ = do
  env@FEnv{feUnionAllObjs} <- get
  if (stypeAct <$> descriptor env feUnionAllObjs) == pure PTopType
    then lift $ TypeCheckResult [GenTypeCheckError Nothing $ printf "Reached runConstraints limit without determining the TopTop (unionAllObjs)"] ()
    -- What was still changing is whatever the instruments watching this solve can say about it,
    -- which for the one every typecheck carries ('TypeCheck.Instruments.LastEpochs') is the last
    -- completed epoch. A solve watched by nothing says only that it ran out.
    else lift $ TypeCheckResult [GenTypeCheckError Nothing $ printf "Reached runConstraints limit with still changing constraints: \n\n%s" (intercalate "\n\n" $ instrumentDiagnostics env)] ()
  return False
runConstraintsIdx limit cons = do
  modify startConstrainEpoch
  env <- get
  -- Compute the constraint-independent reaches type graph once for this epoch and cache it, so
  -- the (many) 'arrowConstrainUbs'/'mkReachesEnv' calls in this epoch reuse it instead of each
  -- rebuilding the whole type graph. See the TODO on 'buildTypeEnv'.
  --
  -- A failed 'setScheme' check or verification stores a 'TypeCheckResE' in the failing point,
  -- and 'buildTypeEnv' reads every object's points, so lifting its result directly would abort
  -- the whole 'StateT' computation here on the following epoch - discarding 'feInstruments'
  -- (leaving 'catln typetrace' with nothing to write) along with all other typecheck artifacts. Instead,
  -- demote the failure to recoverable error notes and stop constraining: decoding still surfaces
  -- the poisoned points' errors, and consumers reject the program via 'failOnErrorNotes'.
  case buildTypeEnv env of
    TypeCheckResE errNotes -> do
      lift $ TypeCheckResult errNotes ()
      return False
    TypeCheckResult notes (reachesVPart, reachesTypeEnv) -> do
      lift $ TypeCheckResult notes ()
      -- Every 'BoundedByObjs' constraint is bounded by the same 'feUnionAllObjs' descriptor
      -- (substituted uniformly by 'showCon'), so the completion @unionTypes u (completeDataPartials u)@
      -- in 'computeConstraint' is identical across all of them. Compute it once per epoch, keyed by the
      -- universe value @u@ so 'computeConstraint' can reuse it EXACTLY (only when its bound equals @u@).
      let u = maybe BottomType stypeAct $ tcreToMaybe $ descriptor env $ feUnionAllObjs env
      -- The completion is an exact function of @u@: reuse last epoch's when @u@ is unchanged.
      let uUnchanged = u == feCompletedAllObjsKey env
      let completedAllObjs = if uUnchanged
            then feCompletedAllObjs env
            else unionTypes (feTypeEnv env) u (completeDataPartials (feTypeEnv env) u)
      -- Bump the env stamp iff a per-epoch env input changed: constraint kinds that read
      -- these caches only skip re-execution within an unchanged stamp (see 'feConMemo').
      -- Only the LIVE (V-side) part of the reaches graph is compared: the typechecked side
      -- changes only between solve fragments, where the fresh universe point already makes
      -- 'uUnchanged' False on the fragment's first epoch.
      let envUnchanged = uUnchanged && reachesVPart == feReachesVPart env
      -- The input-arrow name set is a pure function of the reaches graph: reuse it within an
      -- unchanged env, recompute otherwise (same policy as the completed-universe cache).
      let inputArrowNames = if envUnchanged then feInputArrowNames env else typeGraphInputEmbeddedNames reachesTypeEnv
      -- The reach-query memo's identity: an unchanged graph KEEPS the previous epoch's env
      -- value with its existing token (the memo stays warm across quiet epochs); a changed
      -- graph is stamped with a fresh process-unique token, which also retires the previous
      -- generation's entries ('objArrGraphQuery' resets on a new token).
      let reachesTypeEnv' = if envUnchanged
            then feReachesTypeEnv env
            else reachesTypeEnv{teQueryCacheKey = Just (unsafeFreshQueryToken (teTypeGraph reachesTypeEnv))}
      modify $ \e -> e{feReachesTypeEnv = reachesTypeEnv', feReachesVPart = reachesVPart,
                       feCompletedAllObjsKey = u, feCompletedAllObjs = completedAllObjs,
                       feInputArrowNames = inputArrowNames,
                       feEnvStamp = if envUnchanged then feEnvStamp e else feEnvStamp e + 1}
      constraintsToPrune <- mapM (uncurry executeConstraint) cons
      updated <- gets feUpdatedDuringEpoch
      let cons' = mapMaybe (\(icon, shouldPrune) -> if shouldPrune then Nothing else Just icon) $ zip cons constraintsToPrune
      if not updated then return True else do
        modify nextConstrainEpoch
        -- ALTERNATE the traversal direction each epoch.
        --
        -- Two flows run through the same constraint set in OPPOSITE directions: actuals travel
        -- forward (a leaf's type outward to the application containing it) and requirements
        -- travel backward (a consumer's demand inward to what supplies it). A constraint sees
        -- whatever the constraints before it in this epoch already wrote, so ONE order is the
        -- good order for one flow and the pessimal order for the other. Alternating gives each
        -- flow its good order every other epoch instead of starving one of them.
        --
        -- Starving the requirement channel is not merely slow, it is WRONG: a requirement
        -- computed from partial knowledge is locked in by the narrowing invariant
        -- ('verifySchemeReqLowers'), so a demand derived before its siblings resolved cannot be
        -- widened back. Encoding in dataflow order WITHOUT this alternation over-narrowed
        -- requirements on calls through a class top (core's @insert(m= insert(m= ...))@, whose
        -- requirement must round-trip between @/Data/Map@ and its @BinTree@ members) — 70 of 574
        -- tests, against 2 on the unordered list.
        --
        -- The index paired with each constraint is its 'feConMemo' key and rides along, so
        -- reversing the running order leaves the skip-memo keys untouched.
        runConstraintsIdx (limit - 1) (reverse cons')
