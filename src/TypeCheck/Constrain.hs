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

import           Semantics.Prgm
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
setScheme env@FEnv{feTypeEnv} con (oldCon, newCon) p scheme baseMsg = setDescriptor env con p (checkScheme scheme) (msg "" "")
  where
    -- checkScheme (TypeCheckResult _ (SType ub _ desc)) | containsBottomType ub = error $ msg desc $ printf "Actual type contains bottomType: %s" (show ub)
    checkScheme (TypeCheckResult notes (SType ub _ _ desc)) | containsBottomType ub = TypeCheckResE (mkTracedTypeCheckError env p (getMetaPos p) (msg desc "Actual type contains bottomType") : notes)
    checkScheme (TypeCheckResult notes (SType _ req _ desc)) | containsBottomType req = TypeCheckResE (mkTracedTypeCheckError env p (getMetaPos p) (msg desc "Required type contains bottomType") : notes)
    checkScheme (TypeCheckResult notes (SType act req _ desc)) | not (isSubtypeOfWithEnv feTypeEnv (fmap stypeAct $ fromJust $ tcreToMaybe $ descriptorConVaenv env con) act req) = TypeCheckResE (mkTracedTypeCheckError env p (getMetaPos p) (msg desc "Act is not less than reqe") : notes)
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
updateSchemeProp :: FEnv -> STypeVarArgEnv -> SType -> TypeVarAux -> SType -> (SType, SType)
updateSchemeProp FEnv{feTypeEnv} vaenv super@SType{stypeAct=superAct, stypeReq=superReq} propName sub@SType{stypeAct=subAct, stypeReq=subReq} = (super{stypeAct=superAct', stypeReq=superReq'}, sub{stypeAct=subAct', stypeReq=subReq'})
  where
    (_actVaenv', superAct', subAct') = updateTypeProp feTypeEnv (fmap stypeAct vaenv) superAct propName subAct
    (_reqVaenv', superReq', subReq') = updateTypeProp feTypeEnv (fmap stypeReq vaenv) superReq propName subReq

-- | A helper for the 'AddArg' 'Constraint'
addArgToType :: FEnv -> TypeVarArgEnv -> Type -> TypeVarAux -> Maybe Type
addArgToType _ _ PTopType _ = Nothing
addArgToType env@FEnv{feTypeEnv} vaenv t@TopType{} newArg = case expandType feTypeEnv vaenv t of
  t'@UnionType{} -> addArgToType env vaenv t' newArg
  _              -> Nothing
addArgToType env vaenv (TypeVar v _) newArg = case H.lookup v vaenv of
  Just t  -> addArgToType env vaenv t newArg
  Nothing -> error $ printf "Unknown type in addArgToType: %s" (show v)
addArgToType FEnv{feTypeEnv} _ (UnionType partials) newArg = Just $ unionAllTypes feTypeEnv $ mapMaybe fromPartial $ splitUnionType partials
  where
    fromPartial partial@PartialType{ptArgs} = case newArg of
      TVArg newArg' -> Just $ singletonType partial{ptArgs=H.insertWith (unionTypes feTypeEnv) newArg' PTopType ptArgs}
      TVVar newArg' -> Just $ singletonType partial{ptVars=H.insertWith (unionTypes feTypeEnv) newArg' PTopType ptArgs}

-- | A helper for the 'AddArg' 'Constraint'
addArgToScheme :: FEnv -> STypeVarArgEnv -> SType -> TypeVarAux -> SType -> SType
addArgToScheme env@FEnv{feTypeEnv} vaenv SType{stypeAct=srcAct, stypeReq=srcReq} newArgName dest@SType{stypeAct=destAct, stypeReq=destReq} = dest{stypeAct=destAct', stypeReq=destReq'}
  where
    destAct' = case addArgToType env (fmap stypeAct vaenv) srcAct newArgName of
      Just addDestAct -> intersectTypesEnv feTypeEnv (fmap stypeAct vaenv) destAct addDestAct
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

stypeConstraintDat :: SConstraintDat -> TypeCheckResult RConstraintDat
stypeConstraintDat (EqualsKnown i p t) = do
  p' <- p
  return $ EqualsKnown i p' t
stypeConstraintDat (EqPoints i p1 p2) = do
  p1' <- p1
  EqPoints i p1' <$> p2
stypeConstraintDat (BoundedByKnown i p t) = do
  p' <- p
  return $ BoundedByKnown i p' t
stypeConstraintDat (BoundedByObjs i p t) = do
  p' <- p
  return $ BoundedByObjs i p' t
stypeConstraintDat (NoReturnArg i p) = NoReturnArg i <$> p
stypeConstraintDat (ArrowTo i p1 p2) = do
  p1' <- p1
  ArrowTo i p1' <$> p2
stypeConstraintDat (PropEq i (p1, name) p2) = do
  p1' <- p1
  PropEq i (p1', name) <$> p2
stypeConstraintDat (AddArg i (p1, argName) p2) = do
  p1' <- p1
  AddArg i (p1', argName) <$> p2
stypeConstraintDat (AddInferArg i p1 p2) = do
  p1' <- p1
  AddInferArg i p1' <$> p2
stypeConstraintDat (SetArgMode i m p1 p2) = do
  p1' <- p1
  SetArgMode i m p1' <$> p2
stypeConstraintDat (ConWhere i p1 p2 p3) = do
  p1' <- p1
  p2' <- p2
  ConWhere i p1' p2' <$> p3
stypeConstraintDat (UnionOf i p1 p2s) = do
  p1' <- p1
  p2s' <- sequence p2s
  return $ UnionOf i p1' p2s'

stypeConstraint :: SConstraint -> TypeCheckResult RConstraint
stypeConstraint (Constraint oa vaenv dat) = do
  vaenv' <- mapM both vaenv
  dat' <- stypeConstraintDat dat
  return $ Constraint oa vaenv' dat'
  where
    both (a, b) = do
      a' <- a
      b' <- b
      return (a', b')


updateCOVarArgEnvAct :: TypeVarArgEnv -> CVarArgEnv SType -> CVarArgEnv SType
updateCOVarArgEnvAct oVaenv' ioVaenv = H.intersectionWith (\(si, so) oAct' -> (si, so{stypeAct=oAct'})) ioVaenv oVaenv'

computeConstraint :: FEnv -> RConstraint -> (Bool, RConstraint)
computeConstraint env con@(Constraint _ vaenv (EqualsKnown i p tp)) = (True, con{conVaenv = updateCOVarArgEnvAct vaenv' vaenv, conDat=EqualsKnown i stype' tp})
  where
    (vaenv', stype', _) = equalizeSTypes env (fmap (stypeAct . snd) vaenv) (p, SType tp tp Nothing "")
computeConstraint _ con@(Constraint _ _ (EqPoints _ p1 p2)) | p1 == p2 = (True, con)
computeConstraint env con@(Constraint _ vaenv (EqPoints i p1 p2)) = (False, con{conVaenv = updateCOVarArgEnvAct vaenv' vaenv, conDat=EqPoints i stype' stype'})
  where
    (vaenv', stype', _) = equalizeSTypes env (fmap (stypeAct . snd) vaenv) (p1, p2)
computeConstraint FEnv{feTypeEnv} con@(Constraint _ vaenv (BoundedByKnown i p@SType{stypeAct=act, stypeReq=req} boundTp)) = (True, con{conDat=BoundedByKnown i (p{stypeAct=act', stypeReq=req'}) boundTp})
  where
    boundTp' = expandType feTypeEnv (fmap (stypeAct . snd) vaenv) boundTp
    act' = intersectTypesEnv feTypeEnv (fmap (stypeAct . snd) vaenv) act boundTp'
    req' = intersectTypesEnv feTypeEnv (fmap (stypeReq . snd) vaenv) req boundTp'
computeConstraint _ con@(Constraint _ _ (BoundedByObjs _ SType{stypeAct=PTopType} _)) = (False, con)
computeConstraint FEnv{feTypeEnv} con@(Constraint _ vaenv (BoundedByObjs i p@SType{stypeAct=pAct} objMapBoundUb)) = (False, con{conDat=BoundedByObjs i (p{stypeAct=pAct'}) objMapBoundUb})
  where
    vaenv' = fmap (stypeAct . snd) vaenv
    argsBoundUb = setArgMode vaenv' PtArgExact $ powersetType feTypeEnv vaenv' $ UnionType $ joinUnionType $ map partialToType $ H.keys $ snd $ splitVarArgEnv $ constraintVarArgEnv con
    boundUb = unionTypes feTypeEnv objMapBoundUb argsBoundUb

    -- A partially applied tuple would not be a raw type on the unionObj,
    -- but a subset of the arguments in that type
    (_, pAct') = intersectTypesWithVarEnv feTypeEnv vaenv' pAct boundUb
computeConstraint _ con@(Constraint _ _ (NoReturnArg _ SType{stypeAct=PTopType})) = (False, con)
computeConstraint FEnv{feTypeEnv} con@(Constraint _ vaenv (NoReturnArg i p@SType{stypeAct=act})) = (True, con{conDat=NoReturnArg i p{stypeAct=act'}})
  where
    vaenv' = fmap (stypeAct . snd) vaenv
    argsBoundUb = setArgMode vaenv' PtArgExact $ powersetType feTypeEnv vaenv' $ UnionType $ joinUnionType $ map partialToType $ H.keys $ snd $ splitVarArgEnv $ constraintVarArgEnv con
    act' = differenceTypeWithEnv feTypeEnv vaenv' act argsBoundUb
computeConstraint env con@(Constraint _ _ (ArrowTo i src dest)) = case arrowConstrainUbs env con (stypeAct src) (stypeAct dest) of
    TypeCheckResult _ (src', dest', destRT') -> (False, con{conDat=ArrowTo i src{stypeAct=src'} dest{stypeAct=dest', stypeTree=destRT'}})
    TypeCheckResE _ -> (True, con)
computeConstraint env con@(Constraint _ vaenv (PropEq i (super, name) sub)) = (False, con{conDat=PropEq i (super', name) sub'})
  where
    (super', sub') = updateSchemeProp env (fmap snd vaenv) super name sub
computeConstraint env con@(Constraint _ vaenv (AddArg i (src, newArgName) dest)) = (False, con{conDat=AddArg i (src, newArgName) dest'})
  where
    dest' = addArgToScheme env (fmap snd vaenv) src newArgName dest
computeConstraint env con@(Constraint _ vaenv (AddInferArg i src dest)) = (False, con{conDat=AddInferArg i src dest'})
  where
    dest' = addInferArgToScheme env (fmap snd vaenv) src dest
computeConstraint FEnv{feTypeEnv} con@(Constraint _ vaenv (SetArgMode i mode src@SType{stypeAct=srcAct} dest@SType{stypeAct=destAct})) = (False, con{conDat=SetArgMode i mode src dest{stypeAct=destAct'}})
  where
    vaenv' = fmap (stypeAct . snd) vaenv
    fromSrc = if mode
      then powersetType feTypeEnv vaenv' srcAct
      else setArgMode vaenv' PtArgAny srcAct
    destAct' = intersectTypes feTypeEnv fromSrc destAct
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

saveConstraint :: FEnv -> VConstraint -> RConstraint -> RConstraint -> FEnv
saveConstraint env con@(Constraint _ _ (UnionOf _ parentPnt _)) oldCon newCon@(Constraint _ vaenv' (UnionOf _ parent' _)) = env''
  -- TODO Delete special case of saveConstraint for UnionOf by fixing the bug
  where
    env' = setScheme env con (oldCon, newCon) parentPnt (return parent') "UnionOf"
    env'' = setSchemeConVaenv env' con (oldCon, newCon) SchemeAct (fmap (stypeAct . snd) vaenv') "EqualsKnown env"
saveConstraint env vals old new = saveDat $ saveVaenv env
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
executeConstraint :: FEnv -> VConstraint -> (Bool, FEnv)
executeConstraint env con = case stypeConstraint $ showCon env con of
  TypeCheckResE _ -> (True, env)
  TypeCheckResult _ con' -> (prune, env')
    where
      (prune, con'') = computeConstraint env con'
      env' = saveConstraint env con con' con''

-- | Calls 'executeConstraint' for a list of constraints
executeConstraints :: FEnv -> [VConstraint] -> ([Bool], FEnv)
executeConstraints env [] = ([], env)
executeConstraints env1 (c:cs) = (prune:res, env4)
  where
    env2 = startConstraint c env1
    (prune, env3) = executeConstraint env2 c
    (res, env4) = executeConstraints env3 cs

-- |
-- Applies the constraints continuously to update the environment.
-- It should eventually converge after which applying the constraints no longer has any effect.
-- This function also accepts a limit 'Integer' to indicate a maximum number of times a constraint can be used.
-- A 'TypeCheckError' will be thrown if it has not converged by the time the limit is reached.
runConstraints :: Integer -> FEnv -> [VConstraint] -> TypeCheckResult FEnv
runConstraints _ env [] = return env
-- runConstraints 0 env _ | trace (printf "Trace 908: %s" (show $ showTraceConstrainPnt env 908)) False = undefined
runConstraints 0 env@FEnv{feUnionAllObjs} _ | (stypeAct <$> descriptor env feUnionAllObjs) == pure PTopType = TypeCheckResult [GenTypeCheckError Nothing $ printf "Reached runConstraints limit without determining the TopTop (unionAllObjs)"] env
runConstraints 0 env@FEnv{feTrace} _ = TypeCheckResult [GenTypeCheckError Nothing $ printf "Reached runConstraints limit with still changing constraints: \n\n%s" (show $ showTraceConstrainEpoch env $ head $ tail $ tcEpochs feTrace)] env
runConstraints limit env cons = do
  let (constraintsToPrune, env'@FEnv{feUpdatedDuringEpoch}) = executeConstraints env cons
  let cons' = mapMaybe (\(con, shouldPrune) -> if shouldPrune then Nothing else Just con) $ zip cons constraintsToPrune
  if feUpdatedDuringEpoch
    then runConstraints (limit - 1) (nextConstrainEpoch env') cons'
    else return env'
