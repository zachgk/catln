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
setScheme :: FEnv -> Constraint -> VarMeta -> Scheme -> String -> FEnv
setScheme env@FEnv{feClassGraph} con p scheme baseMsg = setDescriptor env con p (checkScheme scheme) (msg "" "")
  where
    -- checkScheme (TypeCheckResult _ (SType ub _ desc)) | isBottomType ub = error $ msg' ++ desc
    checkScheme (TypeCheckResult notes (SType ub _ desc)) | isBottomType ub = TypeCheckResE (mkTracedTypeCheckError env p (getMetaPos p) (msg desc "Actual type is bottomType") : notes)
    checkScheme (TypeCheckResult notes (SType _ req desc)) | isBottomType req = TypeCheckResE (mkTracedTypeCheckError env p (getMetaPos p) (msg desc "Required type is bottomType") : notes)
    checkScheme (TypeCheckResult notes (SType act req desc)) | not (isSubtypeOf feClassGraph act req) = TypeCheckResE (mkTracedTypeCheckError env p (getMetaPos p) (msg desc "Required type is bottomType") : notes)
    checkScheme s = s

    msg :: String -> String -> String
    msg desc problem = printf "Scheme failed check at setScheme %s(point %s): \n\t %s - %s" baseMsg (show p) problem desc

setSchemeAct :: FEnv -> Constraint -> VarMeta -> Type -> String -> FEnv
setSchemeAct env con p t msg = setScheme env con p scheme' ("Act " ++ msg)
  where
    scheme = descriptor env p
    scheme' = fmap (\(SType _ lb d) -> SType t lb d) scheme

setSchemeReq :: FEnv -> Constraint -> VarMeta -> Type -> String -> FEnv
setSchemeReq env con p t msg = setScheme env con p scheme' ("Req " ++ msg)
  where
    scheme = descriptor env p
    scheme' = fmap (\(SType a _ d) -> SType a t d) scheme

setSchemeActReq :: FEnv -> Constraint -> SchemeActReq -> VarMeta -> Type -> String -> FEnv
setSchemeActReq env con SchemeAct p t msg = setSchemeAct env con p t msg
setSchemeActReq env con SchemeReq p t msg = setSchemeReq env con p t msg

setSchemeConVaenv :: FEnv -> Constraint -> SchemeActReq -> TypeVarArgEnv -> String -> FEnv
setSchemeConVaenv env con actOrReq types msg = foldr (\(m, t) env' -> setSchemeActReq env' con actOrReq m t msg) env (H.elems $ Z.zip (constraintVarArgEnv con) types)

-- | Tries to join two 'SType' as equal to each other and returns their updated values
equalizeSTypes :: FEnv -> TypeVarArgEnv -> (SType, SType) -> (TypeVarArgEnv, SType, SType)
equalizeSTypes FEnv{feClassGraph} vaenv (SType act1 req1 desc1, SType act2 req2 desc2) = do
  let (vaenv'1, actBoth) = intersectTypesWithVarEnv feClassGraph vaenv act1 act2
  let (vaenv'2, reqBoth) = intersectTypesWithVarEnv feClassGraph vaenv req1 req2
  let vaenv' = H.unionWith (intersectTypes feClassGraph) vaenv'1 vaenv'2
  (vaenv', SType actBoth reqBoth desc1, SType actBoth reqBoth desc2)


-- | A helper for the 'PropEq' 'Constraint' that applies to both the actual and required types
updateSchemeProp :: FEnv -> Constraint -> SType -> TypeVarAux -> SType -> (FEnv, Scheme, Scheme)
updateSchemeProp env1@FEnv{feClassGraph} con (SType superAct superReq superDesc) propName (SType subAct subReq subDesc) = (env1, pure $ SType superAct' superReq' superDesc, pure $ SType subAct' subReq' subDesc)
  where
    TypeCheckResult _ vaenv = descriptorConVaenv env1 con
    (_actVaenv', superAct', subAct') = updateTypeProp feClassGraph (fmap stypeAct vaenv) superAct propName subAct
    (_reqVaenv', superReq', subReq') = updateTypeProp feClassGraph (fmap stypeReq vaenv) superReq propName subReq

-- | A helper for the 'AddArg' 'Constraint'
addArgToType :: FEnv -> Type -> ArgName -> Maybe Type
addArgToType _ (TopType _) _ = Nothing
addArgToType _ TypeVar{} _ = error "addArgToType TypeVar"
addArgToType FEnv{feClassGraph} (UnionType partials) newArg = Just $ UnionType partials'
  where
    partials' = joinUnionType $ map fromPartial $ splitUnionType partials
    fromPartial partial@PartialType{ptArgs} = partial{ptArgs=H.insertWith (unionTypes feClassGraph) newArg topType ptArgs}

-- | A helper for the 'AddArg' 'Constraint'
addArgToScheme :: FEnv -> SType -> ArgName -> SType -> SType
addArgToScheme env@FEnv{feClassGraph} (SType srcAct srcReq _) newArgName (SType destAct destReq destDesc) = SType destAct' destReq' destDesc
  where
    destAct' = case addArgToType env srcAct newArgName of
      Just addDestAct -> intersectTypes feClassGraph destAct addDestAct
      Nothing         -> destAct
    destReq' = case addArgToType env srcReq newArgName of
      Just addDestReq -> intersectTypes feClassGraph destReq addDestReq
      Nothing         -> destReq

-- | A helper for the 'AddInferArg' 'Constraint'
addInferArgToType :: FEnv -> Type -> Maybe Type
addInferArgToType _ (TopType _) = Nothing
addInferArgToType _ TypeVar{} = error "addInferArgToType TypeVar"
addInferArgToType env@FEnv{feClassGraph} (UnionType partials) = Just $ unionAllTypes feClassGraph partials'
  where
    partials' = map (inferArgFromPartial env) $ splitUnionType partials

-- | A helper for the 'AddArg' 'Constraint'
addInferArgToScheme :: FEnv -> SType -> SType -> SType
addInferArgToScheme env@FEnv{feClassGraph} (SType srcAct srcReq _) (SType destAct destReq destDesc) = SType destAct' destReq' destDesc
  where
    destAct' = case addInferArgToType env srcAct of
      Just addDestAct -> intersectTypes feClassGraph destAct addDestAct
      Nothing         -> destAct
    destReq' = case addInferArgToType env srcReq of
      Just addDestReq -> intersectTypes feClassGraph destReq addDestReq
      Nothing         -> destReq

-- |
-- This takes a constraint and tries to apply it in the environment.
-- It will return the updated environment and a boolean that is true if the constraint is done.
-- If it is done, it can be safely removed and no longer needs to be executed.
executeConstraint :: FEnv -> Constraint -> (Bool, FEnv)
executeConstraint env con@(EqualsKnown _ pnt tp) = case sequenceT (descriptor env pnt, descriptorConVaenv env con) of
  TypeCheckResult notes (stype, vaenv) -> do
    let (vaenv', stype', _) = equalizeSTypes env (fmap stypeAct vaenv) (stype, SType tp tp "")
    let scheme' = TypeCheckResult notes stype'
    let env' = setScheme env con pnt scheme' "EqualsKnown"
    let env'' = setSchemeConVaenv env' con SchemeAct vaenv' "EqualsKnown env"
    (True, env'')
  TypeCheckResE{} -> (True, env)
executeConstraint env (EqPoints _ (Meta _ _ (VarMetaDat p1 _)) (Meta _ _ (VarMetaDat p2 _))) | p1 == p2 = (True, env)
executeConstraint env1 con@(EqPoints _ p1 p2) = case sequenceT (descriptor env1 p1, descriptor env1 p2, descriptorConVaenv env1 con) of
  TypeCheckResult notes (s1, s2, vaenv) -> do
    let (_, s1', s2') = equalizeSTypes env1 (fmap stypeAct vaenv) (s1, s2)
    let env2 = setScheme env1 con p1 (TypeCheckResult notes s1') "EqPoints"
    let env3 = setScheme env2 con p2 (return s2') "EqPoints"
    (isSolved $ return s1', env3)
  TypeCheckResE _ -> (True, env1)
executeConstraint env@FEnv{feClassGraph} con@(BoundedByKnown _ subPnt boundTp) = do
  let subScheme = descriptor env subPnt
  case subScheme of
    TypeCheckResE _ -> (True, env)
    TypeCheckResult _ (SType act req desc) -> do
      let act' = intersectTypes feClassGraph act boundTp
      let req' = intersectTypes feClassGraph req boundTp
      let scheme' = pure $ SType act' req' desc
      let env' = setScheme env con subPnt scheme' "BoundedByKnown"
      (True, env')
executeConstraint env@FEnv{feUnionAllObjs, feClassGraph} con@(BoundedByObjs _ pnt) = do
  let scheme = pointUb env pnt
  let boundScheme = pointUb env feUnionAllObjs
  case sequenceT (scheme, boundScheme) of
    TypeCheckResE _ -> (True, env)
    TypeCheckResult _ (TopType [], _) -> (False, env)
    TypeCheckResult _ (ub, boundUb) -> do
      -- A partially applied tuple would not be a raw type on the unionObj,
      -- but a subset of the arguments in that type
      let ub' = intersectTypes feClassGraph ub boundUb
      -- let env' = setScheme env pnt scheme' $ printf "BoundedByObjs for %s\nBound: %s\n" (show ub) (show boundUb)
      let env' = setSchemeAct env con pnt ub' $ printf "BoundedByObjs for %s\n" (show ub)
      (False, env')
executeConstraint env con@(ArrowTo _ srcPnt destPnt) = do
  let srcScheme = pointUb env srcPnt
  let destScheme = pointUb env destPnt
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> (True, env)
    TypeCheckResult _ (srcUb, destUb) -> do
      let constrained = arrowConstrainUbs env con srcUb srcPnt destUb destPnt
      case constrained of
        TypeCheckResult _ (srcUb', destUb') -> do
          let env' = setSchemeAct env con srcPnt srcUb' "ArrowTo src"
          let env'' = setSchemeAct env' con destPnt destUb' "ArrowTo dest"
          (False, env'')
        TypeCheckResE _ -> (True, env)
executeConstraint env con@(PropEq _ (superPnt, propName) subPnt) = do
  let superScheme = descriptor env superPnt
  let subScheme = descriptor env subPnt
  case sequenceT (superScheme, subScheme) of
    TypeCheckResE _ -> (True, env)
    (TypeCheckResult _ _) ->
      case sequenceT (superScheme, subScheme) of
        TypeCheckResult _ (superSType, subSType) -> do
          let (env2, superScheme', subScheme') = updateSchemeProp env con superSType propName subSType
          let env3 = setScheme env2 con superPnt superScheme' (printf "PropEq super (%s)" (show propName))
          let env4 = setScheme env3 con subPnt subScheme' (printf "PropEq sub (%s)" (show propName))
          (isSolved subScheme, env4)
        TypeCheckResE _ -> (True, env)
executeConstraint env con@(AddArg _ (srcPnt, newArgName) destPnt) = do
  let srcScheme = descriptor env srcPnt
  let destScheme = descriptor env destPnt
  let checkName = printf "AddArg %s" (show newArgName)
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> (True, env)
    TypeCheckResult _ (src, dest) -> do
      let dest' = pure $ addArgToScheme env src newArgName dest
      let env' = setScheme env con destPnt dest' checkName
      (False, env')
executeConstraint env con@(AddInferArg _ srcPnt destPnt) = do
  let srcScheme = descriptor env srcPnt
  let destScheme = descriptor env destPnt
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> (True, env)
    TypeCheckResult _ (src, dest) -> do
      let dest' = pure $ addInferArgToScheme env src dest
      let env' = setScheme env con destPnt dest' "AddInferArg dest"
      (False, env')
executeConstraint env@FEnv{feClassGraph} con@(PowersetTo _ srcPnt destPnt) = do
  let srcScheme = pointUb env srcPnt
  let destScheme = pointUb env destPnt
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> (True, env)
    TypeCheckResult _ (ub1, ub2) -> do
      let ub2' = intersectTypes feClassGraph (powersetType feClassGraph ub1) ub2
      let env' = setSchemeAct env con destPnt ub2' "PowersetTo"
      (False, env')
executeConstraint env@FEnv{feClassGraph} con@(UnionOf _ parentPnt childrenM) = do
  let parentScheme = descriptor env parentPnt
  let tcresChildrenSchemes = fmap (descriptor env) childrenM
  case sequenceT (parentScheme, sequence tcresChildrenSchemes, descriptorConVaenv env con) of
    TypeCheckResE _ -> (True, env)
    TypeCheckResult notes (parentSType, childrenSchemes, vaenv) -> do
      let accumulateChild (SType ub lb _) (accUb, accLb) = (unionTypes feClassGraph ub accUb, unionTypes feClassGraph lb accLb)
      let (chUb, chLb) = foldr accumulateChild (bottomType, bottomType) childrenSchemes
      let (vaenv', parentST', _) = equalizeSTypes env (fmap stypeAct vaenv) (parentSType, SType (compactType feClassGraph chUb) chLb "")
      let parentScheme' = TypeCheckResult notes parentST'
      let env' = setScheme env con parentPnt parentScheme' "UnionOf"
      let env'' = setSchemeConVaenv env' con SchemeAct vaenv' "EqualsKnown env"
      (isSolved parentScheme', env'')

-- | Calls 'executeConstraint' for a list of constraints
executeConstraints :: FEnv -> [Constraint] -> ([Bool], FEnv)
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
runConstraints :: Integer -> FEnv -> [Constraint] -> TypeCheckResult FEnv
runConstraints _ env [] = return env
runConstraints 0 env@FEnv{feTrace} _ = TypeCheckResult [GenTypeCheckError Nothing $ printf "Reached runConstraints limit with still changing constraints: \n\t%s" (intercalate "\n\t" $ map show $ showTraceConstrainEpoch env $ head $ tail feTrace)] env
runConstraints limit env cons = do
  let (constraintsToPrune, env'@FEnv{feUpdatedDuringEpoch}) = executeConstraints env cons
  let cons' = mapMaybe (\(con, shouldPrune) -> if shouldPrune then Nothing else Just con) $ zip cons constraintsToPrune
  if feUpdatedDuringEpoch
    then runConstraints (limit - 1) (nextConstrainEpoch env') cons'
    else return env'
