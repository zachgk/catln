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
import           Data.Maybe
import qualified Data.Zip            as Z

import           Control.Monad.State
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
setScheme env con (oldCon, newCon) p scheme baseMsg = setDescriptor env con p (checkScheme scheme) (msg "" "")
  where
    -- checkScheme (TypeCheckResult _ (SType ub _ desc)) | containsBottomType ub = error $ msg desc $ printf "Actual type contains bottomType: %s" (show ub)
    checkScheme (TypeCheckResult notes (SType ub _ _ desc)) | containsBottomType ub = TypeCheckResE (mkTracedTypeCheckError env p (msg desc "Actual type contains bottomType") : notes)
    checkScheme (TypeCheckResult notes (SType _ req _ desc)) | containsBottomType req = TypeCheckResE (mkTracedTypeCheckError env p (msg desc "Required type contains bottomType") : notes)
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
equalizeSTypes FEnv{feTypeEnv} vaenv (stype1@SType{stypeAct=act1, stypeReq=req1}, stype2@SType{stypeAct=act2, stypeReq=req2}) = do
  let (vaenv'1, actBoth) = intersectTypesWithVarEnv feTypeEnv vaenv act1 act2
  let (vaenv'2, reqBoth) = intersectTypesWithVarEnv feTypeEnv vaenv req1 req2
  let vaenv' = H.unionWith (intersectTypes feTypeEnv) vaenv'1 vaenv'2
  (vaenv', stype1{stypeAct=actBoth, stypeReq=reqBoth}, stype2{stypeAct=actBoth, stypeReq=reqBoth})


-- | A helper for the 'PropEq' 'Constraint' that applies to both the actual and required types
updateSchemeProp :: FEnv -> STypeVarArgEnv -> SType -> TypeVarAux -> SType -> (STypeVarArgEnv, SType, SType)
updateSchemeProp FEnv{feTypeEnv} vaenv super@SType{stypeAct=superAct, stypeReq=superReq} propName sub@SType{stypeAct=subAct, stypeReq=subReq} = (vaenv', super{stypeAct=superAct', stypeReq=superReq'}, sub{stypeAct=subAct', stypeReq=subReq'})
  where
    (actVaenv', superAct', subAct') = updateTypeProp feTypeEnv (fmap stypeAct vaenv) superAct propName subAct
    (reqVaenv', superReq', subReq') = updateTypeProp feTypeEnv (fmap stypeReq vaenv) superReq propName subReq
    vaenv' = H.intersectionWith (\t s -> s{stypeReq=t}) reqVaenv' $ H.intersectionWith (\t s -> s{stypeAct=t}) actVaenv' vaenv

-- | A helper for the 'AddArg' 'Constraint'
addArgToType :: FEnv -> TypeVarArgEnv -> Type -> TypeVarAux -> Maybe Type
addArgToType _ _ PTopType _ = Nothing
addArgToType _ _ (UnionType (Just _) _ _) _ = Nothing
addArgToType env vaenv (TypeVar v _) newArg = case H.lookup v vaenv of
  Just t  -> addArgToType env vaenv t newArg
  Nothing -> error $ printf "Unknown type in addArgToType: %s" (show v)
addArgToType FEnv{feTypeEnv} _ (UnionType Nothing partials []) newArg = Just $ unionAllTypes feTypeEnv $ mapMaybe fromPartial $ splitUnionType partials
  where
    fromPartial partial@PartialType{ptVars, ptArgs} = case newArg of
      TVArg newArg' -> Just $ singletonType partial{ptArgs=H.insertWith (unionTypes feTypeEnv) newArg' PTopType ptArgs}
      TVVar newArg' -> Just $ singletonType partial{ptVars=H.insertWith (unionTypes feTypeEnv) newArg' PTopType ptVars}
addArgToType _ _ _ _ = Nothing

-- | A helper for the 'AddArg' 'Constraint'
addArgToScheme :: FEnv -> STypeVarArgEnv -> SType -> TypeVarAux -> SType -> SType
addArgToScheme env@FEnv{feTypeEnv} vaenv SType{stypeAct=srcAct, stypeReq=srcReq} newArgName dest@SType{stypeAct=destAct, stypeReq=destReq} = dest{stypeAct=destAct', stypeReq=destReq'}
  where
    destAct' = case addArgToType env (fmap stypeAct vaenv) srcAct newArgName of
      Just addDestAct -> snd $ intersectTypesWithVarEnv feTypeEnv (fmap stypeAct vaenv) destAct addDestAct
      Nothing         -> destAct
    destReq' = case addArgToType env (fmap stypeReq vaenv) srcReq newArgName of
      Just addDestReq -> intersectTypesEnv feTypeEnv (fmap stypeReq vaenv) destReq addDestReq
      Nothing         -> destReq

-- | A helper for the 'AddArg' 'Constraint'
addInferArgToScheme :: FEnv -> STypeVarArgEnv -> SType -> SType -> SType
addInferArgToScheme env@FEnv{feTypeEnv} vaenv SType{stypeAct=srcAct, stypeReq=srcReq} dest@SType{stypeAct=destAct, stypeReq=destReq} = dest{stypeAct=destAct', stypeReq=destReq'}
  where
    destAct' = case addInferArgToType env (fmap stypeAct vaenv) srcAct of
      Just addDestAct -> intersectTypes feTypeEnv destAct addDestAct
      Nothing         -> destAct
    destReq' = case addInferArgToType env (fmap stypeAct vaenv) srcReq of
      Just addDestReq -> intersectTypes feTypeEnv destReq addDestReq
      Nothing         -> destReq

stypeConstraint :: SConstraint -> TypeCheckResult RConstraint
stypeConstraint = mapMCon id

updateCOVarArgEnv :: STypeVarArgEnv -> CVarArgEnv SType -> CVarArgEnv SType
updateCOVarArgEnv oVaenv' ioVaenv = H.intersectionWith (\(si, _) so -> (si, so)) ioVaenv oVaenv'

updateCOVarArgEnvAct :: TypeVarArgEnv -> CVarArgEnv SType -> CVarArgEnv SType
updateCOVarArgEnvAct oVaenv' ioVaenv = H.intersectionWith (\(si, so) oAct' -> (si, so{stypeAct=oAct'})) ioVaenv oVaenv'

computeConstraint :: FEnv -> RConstraint -> (Bool, RConstraint)
computeConstraint env con@(Constraint _ vaenv (EqualsKnown i p tp)) = (True, con{conVaenv = updateCOVarArgEnvAct vaenv' vaenv, conDat=EqualsKnown i stype' tp})
  where
    (vaenv', stype', _) = equalizeSTypes env (fmap (stypeAct . snd) vaenv) (p, SType tp tp Nothing "")
computeConstraint env con@(Constraint _ vaenv (EqPoints i p1 p2)) = (False, con{conVaenv = updateCOVarArgEnvAct vaenv' vaenv, conDat=EqPoints i stype' stype'})
  where
    (vaenv', stype', _) = equalizeSTypes env (fmap (stypeAct . snd) vaenv) (p1, p2)
computeConstraint FEnv{feTypeEnv} con@(Constraint _ vaenv (BoundedByKnown i p@SType{stypeAct=act, stypeReq=req} boundTp)) = (True, con{conDat=BoundedByKnown i (p{stypeAct=act', stypeReq=req'}) boundTp})
  where
    vaenv' = fmap (stypeAct . snd) vaenv
    boundTp' = relaxNameBoundArgs act $ case boundTp of
      TypeVar v _                                   -> H.lookupDefault PTopType v vaenv'
      UnionType (Just (preds, negLeafs)) _ _ ->
        snd $ differenceTypeWithEnv feTypeEnv vaenv' (expandPredicates feTypeEnv vaenv' preds) (UnionType Nothing negLeafs [])
      t                                             -> t
    act' = intersectTypesEnv feTypeEnv vaenv' act boundTp'
    req' = intersectTypesEnv feTypeEnv (fmap (stypeReq . snd) vaenv) req boundTp'
computeConstraint _ con@(Constraint _ _ (BoundedByObjs _ SType{stypeAct=PTopType} _)) = (False, con)
computeConstraint FEnv{feTypeEnv, feCompletedAllObjsKey, feCompletedAllObjs} con@(Constraint _ vaenv (BoundedByObjs i p@SType{stypeAct=pAct, stypeReq=pReq} objMapBoundUb)) = (False, con{conDat=BoundedByObjs i (p{stypeAct=pAct', stypeReq=pReq'}) objMapBoundUb})
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
    -- Relax each bound object's self-referential field (one that recurses back to a type the
    -- object participates in) so a destructured value whose corresponding field is still an
    -- unconstrained pattern variable can reconcile with it (see 'relaxRecursiveSelfArgs').
    -- The objects universe is closed-world (only currently-declared data objects), but an OPEN
    -- (unsealed) class may gain instances in a future file, so a value typed by one is genuinely
    -- inhabitable and must survive the bound rather than empty. Admit the value's OWN open-class
    -- predicate components (with their real element bounds) as inhabitable bound members: the
    -- intersection then preserves exactly those components (a foreign class-top with an
    -- unconstrained element would instead let the value's element widen). Sealed classes are
    -- excluded — they equal their declared members, already in the universe.
    boundUb = relaxRecursiveSelfArgs feTypeEnv $ unionTypes feTypeEnv (unionTypes feTypeEnv objMapBoundUb' (openClassPart feTypeEnv pAct)) argsBoundUb

    -- A partially applied tuple would not be a raw type on the unionObj,
    -- but a subset of the arguments in that type. The object-universe bound is an upper bound, so it
    -- applies to the required type as well as the actual (both are upper bounds — see 'SType'), just
    -- as 'BoundedByKnown' narrows both; each side is resolved against its own ambient var-env.
    (_, pAct') = intersectTypesWithVarEnv feTypeEnv vaenv' pAct boundUb
    -- Only narrow a *concrete* requirement. A Top requirement meets the whole universe to the whole
    -- universe (every known object) — not a narrowing, and a performance blowup — so leave it be
    -- (mirroring the 'stypeAct=PTopType' guard above). A named requirement instead meets only its
    -- name-matching objects, tightening the requirement's arg fields (e.g. @/f(/a=Top)@ narrows to
    -- @/f(/a=<declared>)@). If the meet empties the requirement — e.g. a @(Top | Functor && Eq)@ law
    -- requirement whose type variables resolve differently under the required var-env than under the
    -- actual — keep the un-narrowed requirement: a requirement is an upper bound, so widening it is
    -- always sound, whereas narrowing it to ∅ would spuriously reject a valid program ('checkScheme'
    -- rejects a required type containing bottomType).
    narrowedReq = snd $ intersectTypesWithVarEnv feTypeEnv (fmap (stypeReq . snd) vaenv) pReq boundUb
    pReq' | pReq == PTopType               = pReq
          | containsBottomType narrowedReq = pReq
          | otherwise                      = narrowedReq
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
-- An input argument actually is only whatever it is required to be, so pull its actual down to its
-- requirement. As uses (and the ArrowTo reverse propagation) keep narrowing the requirement, this
-- re-runs each epoch (returns False, not "done"); both bounds only shrink, so the actual stays
-- monotone (@act ∩ req ⊆ act@) and a bare 'TypeVar' requirement is preserved by the intersection.
computeConstraint FEnv{feTypeEnv} con@(Constraint _ vaenv (EqReqAct i p@SType{stypeAct=act, stypeReq=req})) = (False, con{conDat=EqReqAct i p{stypeAct=act'}})
  where
    vaenv' = fmap (stypeAct . snd) vaenv
    act' = intersectTypesEnv feTypeEnv vaenv' act req
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
computeConstraint FEnv{feTypeEnv} con@(Constraint _ vaenv (SetArgMode i mode src@SType{stypeAct=srcAct, stypeReq=srcReq} dest@SType{stypeAct=destAct, stypeReq=destReq})) = (False, con{conDat=SetArgMode i mode src dest{stypeAct=destAct', stypeReq=destReq'}})
  where
    vaenv' = fmap (stypeAct . snd) vaenv
    argModeType venv t = if mode
      then powersetType feTypeEnv venv t
      else setArgMode venv PtArgAny t
    destAct' = intersectTypes feTypeEnv (argModeType vaenv' srcAct) destAct
    -- The arg-mode transform is an upper bound on the destination, so it applies to the required
    -- type as well as the actual (both are upper bounds — see 'SType'); mirror it on each side.
    destReq' = intersectTypes feTypeEnv (argModeType (fmap (stypeReq . snd) vaenv) srcReq) destReq
computeConstraint FEnv{feTypeEnv} con@(Constraint _ vaenv (ConWhere i base cond res)) = (False, con{conDat=ConWhere i base cond res'}) -- TODO Currently using temp equals relationship, switch to actual constraint
  where
    vaenv' = fmap (stypeAct . snd) vaenv
    tp'' = case maybeGetSingleton (stypeAct cond) of
      Just tp' -> intersectTypesEnv feTypeEnv vaenv' (stypeAct res) $ typeAddPreds (stypeAct base) (PredsOne $ PredExpr tp')
      Nothing -> stypeAct res
    res' = res{stypeAct=tp''}
computeConstraint env@FEnv{feTypeEnv} con@(Constraint _ vaenv (UnionOf i parent children)) = (False, con{conVaenv=updateCOVarArgEnvAct vaenv' vaenv, conDat=UnionOf i parentST' children})
  where
    chAct = unionAllTypesWithEnv feTypeEnv H.empty $ filter (not . isTypeVar) $ map stypeAct children
    chReq = unionAllTypesWithEnv feTypeEnv H.empty $ filter (not . isTypeVar) $ map stypeReq children
    actVaenv = fmap (stypeAct . snd) vaenv
    (vaenv', parentST', _) = equalizeSTypes env actVaenv (parent, SType (compactType feTypeEnv actVaenv chAct) chReq Nothing "")

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
executeConstraint :: VConstraint -> StateT FEnv TypeCheckResult Bool
executeConstraint con = do
  modify $ startConstraint con
  env <- get
  case stypeConstraint $ showCon env con of
    TypeCheckResE _ -> return True
    TypeCheckResult _ con' -> do
      let (prune, con'') = computeConstraint env con'
      modify $ saveConstraint con con' con''
      return prune

-- |
-- Applies the constraints continuously to update the environment.
-- It should eventually converge after which applying the constraints no longer has any effect.
-- This function also accepts a limit 'Integer' to indicate a maximum number of times a constraint can be used.
-- A 'TypeCheckError' will be thrown if it has not converged by the time the limit is reached.
runConstraints :: Integer -> [VConstraint] -> StateT FEnv TypeCheckResult ()
runConstraints _ [] = return ()
runConstraints 0 _ = do
  env@FEnv{feTrace, feUnionAllObjs} <- get
  -- _ <- trace (printf "Trace 908: %s" (show $ showTraceConstrainPnt env 908)) (return ())
  if (stypeAct <$> descriptor env feUnionAllObjs) == pure PTopType
    then lift $ TypeCheckResult [GenTypeCheckError Nothing $ printf "Reached runConstraints limit without determining the TopTop (unionAllObjs)"] ()
    else lift $ TypeCheckResult [GenTypeCheckError Nothing $ printf "Reached runConstraints limit with still changing constraints: \n\n%s" (show $ showTraceConstrainEpoch env $ head $ tail $ tcEpochs feTrace)] ()
runConstraints limit cons = do
  env <- get
  -- Compute the constraint-independent reaches type graph once for this epoch and cache it, so
  -- the (many) 'arrowConstrainUbs'/'mkReachesEnv' calls in this epoch reuse it instead of each
  -- rebuilding the whole type graph. See the TODO on 'buildTypeEnv'.
  --
  -- A failed 'setScheme' check or verification stores a 'TypeCheckResE' in the failing point,
  -- and 'buildTypeEnv' reads every object's points, so lifting its result directly would abort
  -- the whole 'StateT' computation here on the following epoch - discarding 'feTrace' (leaving
  -- 'catln typetrace' with nothing to write) along with all other typecheck artifacts. Instead,
  -- demote the failure to recoverable error notes and stop constraining: decoding still surfaces
  -- the poisoned points' errors, and consumers reject the program via 'failOnErrorNotes'.
  case buildTypeEnv env of
    TypeCheckResE errNotes -> lift $ TypeCheckResult errNotes ()
    TypeCheckResult notes reachesTypeEnv -> do
      lift $ TypeCheckResult notes ()
      -- Every 'BoundedByObjs' constraint is bounded by the same 'feUnionAllObjs' descriptor
      -- (substituted uniformly by 'showCon'), so the completion @unionTypes u (completeDataPartials u)@
      -- in 'computeConstraint' is identical across all of them. Compute it once per epoch, keyed by the
      -- universe value @u@ so 'computeConstraint' can reuse it EXACTLY (only when its bound equals @u@).
      let u = maybe BottomType stypeAct $ tcreToMaybe $ descriptor env $ feUnionAllObjs env
      let completedAllObjs = unionTypes (feTypeEnv env) u (completeDataPartials (feTypeEnv env) u)
      modify $ \e -> e{feReachesTypeEnv = reachesTypeEnv, feCompletedAllObjsKey = u, feCompletedAllObjs = completedAllObjs}
      constraintsToPrune <- mapM executeConstraint cons
      updated <- gets feUpdatedDuringEpoch
      let cons' = mapMaybe (\(con, shouldPrune) -> if shouldPrune then Nothing else Just con) $ zip cons constraintsToPrune
      when updated $ do
        modify nextConstrainEpoch
        runConstraints (limit - 1) cons'
