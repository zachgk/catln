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

import           Data.List           (intercalate)
import           Data.Tuple.Sequence
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
setScheme :: FEnv -> VConstraint -> VarMeta -> Scheme -> String -> FEnv
setScheme env@FEnv{feTypeEnv} con p scheme baseMsg = setDescriptor env con p (checkScheme scheme) (msg "" "")
  where
    -- checkScheme (TypeCheckResult _ (SType ub _ desc)) | containsBottomType ub = error $ msg desc $ printf "Actual type contains bottomType: %s" (show ub)
    checkScheme (TypeCheckResult notes (SType ub _ desc)) | containsBottomType ub = TypeCheckResE (mkTracedTypeCheckError env p (getMetaPos p) (msg desc "Actual type contains bottomType") : notes)
    checkScheme (TypeCheckResult notes (SType _ req desc)) | containsBottomType req = TypeCheckResE (mkTracedTypeCheckError env p (getMetaPos p) (msg desc "Required type contains bottomType") : notes)
    checkScheme (TypeCheckResult notes (SType act req desc)) | not (isSubtypeOfWithEnv feTypeEnv (fmap stypeAct $ fromJust $ tcreToMaybe $ descriptorConVaenv env con) act req) = TypeCheckResE (mkTracedTypeCheckError env p (getMetaPos p) (msg desc "Act is not less than reqe") : notes)
    checkScheme s = s

    msg :: String -> String -> String
    msg desc problem = printf "Scheme failed check at setScheme %s(point %s): \n\t %s - %s" baseMsg (show p) problem desc

setSchemeAct :: FEnv -> VConstraint -> VarMeta -> Type -> String -> FEnv
setSchemeAct env con p t msg = setScheme env con p scheme' ("Act " ++ msg)
  where
    scheme = descriptor env p
    scheme' = fmap (\(SType _ lb d) -> SType t lb d) scheme

setSchemeReq :: FEnv -> VConstraint -> VarMeta -> Type -> String -> FEnv
setSchemeReq env con p t msg = setScheme env con p scheme' ("Req " ++ msg)
  where
    scheme = descriptor env p
    scheme' = fmap (\(SType a _ d) -> SType a t d) scheme

setSchemeActReq :: FEnv -> VConstraint -> SchemeActReq -> VarMeta -> Type -> String -> FEnv
setSchemeActReq env con SchemeAct p t msg = setSchemeAct env con p t msg
setSchemeActReq env con SchemeReq p t msg = setSchemeReq env con p t msg

setSchemeConVaenv :: FEnv -> VConstraint -> SchemeActReq -> TypeVarArgEnv -> String -> FEnv
setSchemeConVaenv env con actOrReq types msg = foldr (\(m, t) env' -> setSchemeActReq env' con actOrReq m t msg) env (H.elems $ Z.zip (constraintVarArgEnv con) types)

-- | Tries to join two 'SType' as equal to each other and returns their updated values
equalizeSTypes :: FEnv -> TypeVarArgEnv -> (SType, SType) -> (TypeVarArgEnv, SType, SType)
equalizeSTypes FEnv{feTypeEnv} vaenv (SType act1 req1 desc1, SType act2 req2 desc2) = do
  let (vaenv'1, actBoth) = intersectTypesWithVarEnv feTypeEnv vaenv act1 act2
  let (vaenv'2, reqBoth) = intersectTypesWithVarEnv feTypeEnv vaenv req1 req2
  let vaenv' = H.unionWith (intersectTypes feTypeEnv) vaenv'1 vaenv'2
  (vaenv', SType actBoth reqBoth desc1, SType actBoth reqBoth desc2)


-- | A helper for the 'PropEq' 'Constraint' that applies to both the actual and required types
updateSchemeProp :: FEnv -> STypeVarArgEnv -> SType -> TypeVarAux -> SType -> (SType, SType)
updateSchemeProp FEnv{feTypeEnv} vaenv (SType superAct superReq superDesc) propName (SType subAct subReq subDesc) = (SType superAct' superReq' superDesc, SType subAct' subReq' subDesc)
  where
    (_actVaenv', superAct', subAct') = updateTypeProp feTypeEnv (fmap stypeAct vaenv) superAct propName subAct
    (_reqVaenv', superReq', subReq') = updateTypeProp feTypeEnv (fmap stypeReq vaenv) superReq propName subReq

-- | A helper for the 'AddArg' 'Constraint'
addArgToType :: FEnv -> TypeVarArgEnv -> Type -> ArgName -> Maybe Type
addArgToType _ _ (TopType _) _ = Nothing
addArgToType env vaenv (TypeVar v _) newArg = case H.lookup v vaenv of
  Just t  -> addArgToType env vaenv t newArg
  Nothing -> error $ printf "Unknown type in addArgToType: %s" (show v)
addArgToType FEnv{feTypeEnv} _ (UnionType partials) newArg = Just $ unionAllTypes feTypeEnv $ mapMaybe fromPartial $ splitUnionType partials
  where
    fromPartial partial@PartialType{ptArgs} = Just $ singletonType partial{ptArgs=H.insertWith (unionTypes feTypeEnv) newArg topType ptArgs}

-- | A helper for the 'AddArg' 'Constraint'
addArgToScheme :: FEnv -> STypeVarArgEnv -> SType -> ArgName -> SType -> SType
addArgToScheme env@FEnv{feTypeEnv} vaenv (SType srcAct srcReq _) newArgName (SType destAct destReq destDesc) = SType destAct' destReq' destDesc
  where
    destAct' = case addArgToType env (fmap stypeAct vaenv) srcAct newArgName of
      Just addDestAct -> intersectTypes feTypeEnv destAct addDestAct
      Nothing         -> destAct
    destReq' = case addArgToType env (fmap stypeReq vaenv) srcReq newArgName of
      Just addDestReq -> intersectTypes feTypeEnv destReq addDestReq
      Nothing         -> destReq

-- | A helper for the 'AddArg' 'Constraint'
addInferArgToScheme :: FEnv -> STypeVarArgEnv -> SType -> SType -> SType
addInferArgToScheme env@FEnv{feTypeEnv} vaenv (SType srcAct srcReq _) (SType destAct destReq destDesc) = SType destAct' destReq' destDesc
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
stypeConstraintDat (PowersetTo i p1 p2) = do
  p1' <- p1
  PowersetTo i p1' <$> p2
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
    (vaenv', stype', _) = equalizeSTypes env (fmap (stypeAct . snd) vaenv) (p, SType tp tp "")
computeConstraint _ con@(Constraint _ _ (EqPoints _ p1 p2)) | p1 == p2 = (True, con)
computeConstraint env con@(Constraint _ vaenv (EqPoints i p1 p2)) = (False, con{conVaenv = updateCOVarArgEnvAct vaenv' vaenv, conDat=EqPoints i stype' stype'})
  where
    (vaenv', stype', _) = equalizeSTypes env (fmap (stypeAct . snd) vaenv) (p1, p2)
computeConstraint FEnv{feTypeEnv} con@(Constraint _ vaenv (BoundedByKnown i p@SType{stypeAct=act, stypeReq=req} boundTp)) = (True, con{conDat=BoundedByKnown i (p{stypeAct=act', stypeReq=req'}) boundTp})
  where
    boundTp' = expandType feTypeEnv (fmap (stypeAct . snd) vaenv) boundTp
    act' = intersectTypesEnv feTypeEnv (fmap (stypeAct . snd) vaenv) act boundTp'
    req' = intersectTypesEnv feTypeEnv (fmap (stypeReq . snd) vaenv) req boundTp'
computeConstraint _ con@(Constraint _ _ (BoundedByObjs _ SType{stypeAct=TopType []} _)) = (False, con)
computeConstraint FEnv{feTypeEnv} con@(Constraint _ vaenv (BoundedByObjs i p@SType{stypeAct=pAct} objMapBoundUb)) = (False, con{conDat=BoundedByObjs i (p{stypeAct=pAct'}) objMapBoundUb})
  where
    vaenv' = fmap (stypeAct . snd) vaenv
    argsBoundUb = setArgMode vaenv' PtArgExact $ powersetType feTypeEnv vaenv' $ UnionType $ joinUnionType $ map partialToType $ H.keys $ snd $ splitVarArgEnv $ constraintVarArgEnv con
    boundUb = unionTypes feTypeEnv objMapBoundUb argsBoundUb

    -- A partially applied tuple would not be a raw type on the unionObj,
    -- but a subset of the arguments in that type
    (_, pAct') = intersectTypesWithVarEnv feTypeEnv vaenv' pAct boundUb
computeConstraint env con@(Constraint _ _ (ArrowTo i src dest)) = (False, con{conDat=ArrowTo i src{stypeAct=src'} dest{stypeAct=dest'}})
  where
    (src', dest') = fromJust $ tcreToMaybe $ arrowConstrainUbs env con (stypeAct src) (stypeAct dest)
computeConstraint env con@(Constraint _ vaenv (PropEq i (super, name) sub)) = (False, con{conDat=PropEq i (super', name) sub'})
  where
    (super', sub') = updateSchemeProp env (fmap snd vaenv) super name sub
computeConstraint env con@(Constraint _ vaenv (AddArg i (src, newArgName) dest)) = (False, con{conDat=AddArg i (src, newArgName) dest'})
  where
    dest' = addArgToScheme env (fmap snd vaenv) src newArgName dest
computeConstraint env con@(Constraint _ vaenv (AddInferArg i src dest)) = (False, con{conDat=AddInferArg i src dest'})
  where
    dest' = addInferArgToScheme env (fmap snd vaenv) src dest
computeConstraint FEnv{feTypeEnv} con@(Constraint _ vaenv (PowersetTo i src@SType{stypeAct=srcAct} dest@SType{stypeAct=destAct})) = (False, con{conDat=PowersetTo i src dest{stypeAct=destAct'}})
  where
    destAct' = intersectTypes feTypeEnv (powersetType feTypeEnv (fmap (stypeAct . snd) vaenv) srcAct) destAct
computeConstraint env@FEnv{feTypeEnv} con@(Constraint _ vaenv (UnionOf i parent children)) = (False, con{conVaenv=updateCOVarArgEnvAct vaenv' vaenv, conDat=UnionOf i parentST' children})
  where
    chAct = unionAllTypesWithEnv feTypeEnv H.empty $ map stypeAct children
    chReq = unionAllTypesWithEnv feTypeEnv H.empty $ map stypeReq children
    actVaenv = fmap (stypeAct . snd) vaenv
    (vaenv', parentST', _) = equalizeSTypes env actVaenv (parent, SType (compactType feTypeEnv actVaenv chAct) chReq "")

saveConstraint :: FEnv -> VConstraint -> RConstraint -> FEnv
saveConstraint env vals new = saveDat $ saveVaenv env
  where
    saveDat en = foldr (\(p, v) e -> setScheme e vals p (pure v) (show v)) en (zip (constraintDatMetas $ conDat vals) (constraintDatMetas $ conDat new))
    saveVaenv en = foldr saveVaenvPair en (H.elems $ H.intersectionWith (,) (conVaenv vals) (conVaenv new))
    saveVaenvPair ((pIn, pOut), (vIn, vOut)) en = en''
      where
        en' = setScheme en vals pIn (pure vIn) (show vIn)
        en'' = setScheme en' vals pOut (pure vOut) (show vOut)

-- |
-- This takes a constraint and tries to apply it in the environment.
-- It will return the updated environment and a boolean that is true if the constraint is done.
-- If it is done, it can be safely removed and no longer needs to be executed.
executeConstraint :: FEnv -> VConstraint -> (Bool, FEnv)
executeConstraint env@FEnv{feTypeEnv} con@(Constraint _ _ (UnionOf _ parentPnt childrenM)) = do
  let parentScheme = descriptor env parentPnt
  let tcresChildrenSchemes = fmap (descriptor env) childrenM
  case sequenceT (parentScheme, sequence tcresChildrenSchemes, descriptorConVaenv env con) of
    TypeCheckResE _ -> (True, env)
    TypeCheckResult notes (parentSType, childrenSchemes, vaenv) -> do
      let chAct = unionAllTypesWithEnv feTypeEnv H.empty $ map stypeAct childrenSchemes
      let chReq = unionAllTypesWithEnv feTypeEnv H.empty $ map stypeReq childrenSchemes
      let (vaenv', parentST', _) = equalizeSTypes env (fmap stypeAct vaenv) (parentSType, SType (compactType feTypeEnv (fmap stypeAct vaenv) chAct) chReq "")
      let parentScheme' = TypeCheckResult notes parentST'
      let env' = setScheme env con parentPnt parentScheme' "UnionOf"
      let env'' = setSchemeConVaenv env' con SchemeAct vaenv' "EqualsKnown env"
      (isSolved parentScheme', env'')
executeConstraint env con = case stypeConstraint $ showCon env con of
  TypeCheckResE _ -> (True, env)
  TypeCheckResult _ con' -> (prune, env')
    where
      (prune, con'') = computeConstraint env con'
      env' = saveConstraint env con con''

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
runConstraints 0 env@FEnv{feTrace} _ = TypeCheckResult [GenTypeCheckError Nothing $ printf "Reached runConstraints limit with still changing constraints: \n\t%s" (intercalate "\n\t" $ map show $ showTraceConstrainEpoch env $ head $ tail feTrace)] env
runConstraints limit env cons = do
  let (constraintsToPrune, env'@FEnv{feUpdatedDuringEpoch}) = executeConstraints env cons
  let cons' = mapMaybe (\(con, shouldPrune) -> if shouldPrune then Nothing else Just con) $ zip cons constraintsToPrune
  if feUpdatedDuringEpoch
    then runConstraints (limit - 1) (nextConstrainEpoch env') cons'
    else return env'
