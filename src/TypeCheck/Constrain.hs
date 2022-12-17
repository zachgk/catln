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

import           Data.Tuple.Sequence
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           TypeCheck.Common
import           TypeCheck.Show
import           TypeCheck.TypeGraph

-- | Checks if a scheme is solved because it's upper bound and lower bound are equal
isSolved :: Scheme -> Bool
isSolved (TypeCheckResult _ (SType a b _)) = a == b
isSolved _                                 = False

-- | Updates a 'VarMeta' point to a new 'Scheme' value
setScheme :: FEnv -> VarMeta -> Scheme -> String -> FEnv
setScheme env p scheme msg = setDescriptor env p (checkScheme scheme) msg
  where msg' = printf "Scheme failed check at setScheme %s(point %s): upper bound is bottomType - " msg (show p)
        -- checkScheme (TypeCheckResult _ (SType ub _ desc)) | isBottomType ub = error $ msg' ++ desc
        checkScheme (TypeCheckResult notes (SType ub _ desc)) | isBottomType ub = TypeCheckResE (GenTypeCheckError (getMetaPos p) (msg' ++ desc) : notes)
        checkScheme s = s

-- | Tries to join two 'SType' as equal to each other and returns their updated values
equalizeSTypes :: FEnv -> (SType, SType) -> String -> TypeCheckResult (SType, SType)
equalizeSTypes env@FEnv{feClassGraph} (SType ub1 lb1 desc1, SType ub2 lb2 desc2) d = do
  let lbBoth = unionTypes feClassGraph lb1 lb2
  ubBoth <- tryIntersectTypes env ub1 ub2 $ "equalizeSTypes(" ++ d ++ ")"
  if isSubtypeOf feClassGraph lbBoth ubBoth
    then return (SType ubBoth lbBoth desc1, SType ubBoth lbBoth desc2)
    else TypeCheckResE [GenTypeCheckError Nothing (printf "Type mismatched: %s is not a subtype of %s" (show lbBoth) (show ubBoth))]

-- | A helper for the 'PropEq' 'Constraint'
updateSchemeProp :: FEnv -> (VarMeta, SType) -> ArgName -> (VarMeta, SType) -> (FEnv, Scheme, Scheme)
updateSchemeProp env@FEnv{feClassGraph} (superM, superScheme@(SType superUb superLb superDesc)) propName (subM, subScheme@(SType subUb subLb subDesc)) = case (superUb, subUb) of
    (TopType, _) -> wrapUbs (TopType, subUb)
    (TypeVar v, _) -> do
      let (TypeCheckResult _ superM') = resolveTypeVar v superM
      let (TypeCheckResult _ super') = pure superM' >>= descriptor env
      let (env2, super'', sub'') = updateSchemeProp env (superM, super') propName (subM, subScheme)
      let env3 = setScheme env2 superM' super'' "PropEq var super"
      (env3, pure superScheme, sub'')
    (UnionType supPartials, TypeVar{}) -> do
      let supPartialList = splitUnionType supPartials
      let intersectedPartials sup@PartialType{ptArgs=supArgs} = Just (sup{ptArgs=H.insert propName subUb supArgs})
      let supPartialList' = catMaybes $ [intersectedPartials sup | sup <- supPartialList]
      wrapUbs (compactType feClassGraph $ UnionType $ joinUnionType supPartialList', subUb)
    (UnionType supPartials, TopType) -> do
      let supPartialList = splitUnionType supPartials
      let sub' = unionAllTypes feClassGraph $ mapMaybe (typeGetArg propName) supPartialList
      wrapUbs (superUb, sub')
    (UnionType supPartials, UnionType subPartials) -> do
      let supPartialList = splitUnionType supPartials
      let subPartialList = splitUnionType subPartials
      let intersectedPartials sup@PartialType{ptArgs=supArgs, ptVars=supVars} sub = case H.lookup propName supArgs of
            Just (TypeVar (TVVar v)) -> do
              let supVar = H.lookupDefault TopType v supVars
              let newProp = intersectTypes feClassGraph supVar (singletonType sub)
              Just (sup{ptVars=H.insert v newProp supVars}, newProp)
            Just (TypeVar TVArg{}) -> error $ printf "Not yet implemented"
            Just supProp -> do
              let newProp = intersectTypes feClassGraph supProp (singletonType sub)
              if isBottomType newProp
                then Nothing
                else Just (sup{ptArgs=H.insert propName newProp supArgs}, newProp)
            Nothing -> Nothing
      let (supPartialList', subPartialList') = unzip $ catMaybes $ [intersectedPartials sup sub | sup <- supPartialList, sub <- subPartialList]
      wrapUbs (compactType feClassGraph $ UnionType $ joinUnionType supPartialList', unionAllTypes feClassGraph subPartialList')
  where
    wrapUbs (superUb', subUb') = (env, return $ SType superUb' superLb superDesc, return $ SType subUb' subLb subDesc)

-- | A helper for the 'VarEq' 'Constraint'
updateSchemeVar :: FEnv -> SType -> TypeVarName -> SType -> (Scheme, Scheme)
updateSchemeVar FEnv{feClassGraph} (SType superUb superLb superDesc) varName (SType subUb subLb subDesc) = (return $ SType superUb' superLb superDesc, return $ SType subUb' subLb subDesc)
  where
    (superUb', subUb') = case (superUb, subUb) of
      (TopType, sub) -> (TopType, sub)
      (UnionType supPartials, TopType) -> do
        let supPartialList = splitUnionType supPartials
        let getVar PartialType{ptVars=supVars} = H.lookup varName supVars
        let sub = unionAllTypes feClassGraph $ mapMaybe getVar supPartialList
        (superUb, sub)
      (UnionType supPartials, UnionType subPartials) -> do
        let supPartialList = splitUnionType supPartials
        let subPartialList = splitUnionType subPartials
        let intersectedPartials sup@PartialType{ptVars=supVars} sub = case H.lookup varName supVars of
              Just supVar -> do
                let newVar = intersectTypes feClassGraph supVar (singletonType sub)
                if isBottomType newVar
                  then Nothing
                  else Just (sup{ptVars=H.insert varName newVar supVars}, newVar)
              Nothing -> Just (sup{ptVars=H.insert varName (singletonType sub) supVars}, singletonType sub)
        let (supPartialList', subPartialList') = unzip $ catMaybes $ [intersectedPartials sup sub | sup <- supPartialList, sub <- subPartialList]
        (compactType feClassGraph $ UnionType $ joinUnionType supPartialList', unionAllTypes feClassGraph subPartialList')
      (UnionType supPartials, subT@TypeVar{}) -> do
        let supPartialList = splitUnionType supPartials
        let intersectedPartials sup@PartialType{ptVars=supVars} = case H.lookup varName supVars of
              Just supVar -> do
                if supVar == subT
                  then Just sup
                  else Nothing
              Nothing -> Nothing
        let supPartialList' = catMaybes [intersectedPartials sup | sup <- supPartialList]
        (compactType feClassGraph $ UnionType $ joinUnionType supPartialList', subT)
      (sup, sub) -> error $ printf "Unsupported updateSchemeVar Ub (%s).%s = %s" (show sup) varName (show sub)

-- | A helper for the 'AddArg' 'Constraint'
addArgToType :: FEnv -> Type -> ArgName -> Maybe Type
addArgToType _ TopType _ = Nothing
addArgToType _ TypeVar{} _ = error "addArgToType TypeVar"
addArgToType FEnv{feClassGraph} (UnionType partials) newArg = Just $ UnionType partials'
  where
    partials' = joinUnionType $ map fromPartial $ splitUnionType partials
    fromPartial partial@PartialType{ptArgs} = partial{ptArgs=H.insertWith (unionTypes feClassGraph) newArg TopType ptArgs}

-- | A helper for the 'AddInferArg' 'Constraint'
addInferArgToType :: FEnv -> Type -> Maybe Type
addInferArgToType _ TopType = Nothing
addInferArgToType _ TypeVar{} = error "addInferArgToType TypeVar"
addInferArgToType env@FEnv{feClassGraph} (UnionType partials) = Just $ unionAllTypes feClassGraph partials'
  where
    partials' = map (inferArgFromPartial env) $ splitUnionType partials

-- |
-- This takes a constraint and tries to apply it in the environment.
-- It will return the updated environment and a boolean that is true if the constraint is done.
-- If it is done, it can be safely removed and no longer needs to be executed.
executeConstraint :: FEnv -> Constraint -> (Bool, FEnv)
executeConstraint env (EqualsKnown pnt tp) = case descriptor env pnt of
  (TypeCheckResult notes stype) -> case equalizeSTypes env (stype, SType tp tp "") "executeConstraint EqualsKnown" of
    TypeCheckResult notes2 (stype', _) -> do
      let scheme' = TypeCheckResult (notes ++ notes2) stype'
      let env' = setScheme env pnt scheme' "EqualsKnown"
      (True, env')
    TypeCheckResE _ -> (True, env)
  TypeCheckResE{} -> (True, env)
executeConstraint env (EqPoints (Meta _ _ (VarMetaDat p1 _ _ _)) (Meta _ _ (VarMetaDat p2 _ _ _))) | p1 == p2 = (True, env)
executeConstraint env1 (EqPoints p1 p2) = case sequenceT (descriptorResolve env1 p1, descriptorResolve env1 p2) of
  TypeCheckResult notes ((p1', s1), (p2', s2)) -> case equalizeSTypes env1 (s1, s2) "executeConstraint EqPoints" of
    TypeCheckResult notes2 (s1', s2') -> do
      let env2 = setScheme env1 p1' (TypeCheckResult (notes ++ notes2) s1') "EqPoints"
      let env3 = setScheme env2 p2' (return s2') "EqPoints"
      (isSolved $ return s1', env3)
    TypeCheckResE notes2 -> do
      let res = TypeCheckResE (notes ++ notes2)
      let env2 = setScheme env1 p1 res "EqPoints"
      let env3 = setScheme env2 p2 res "EqPoints"
      (True, env3)
  TypeCheckResE _ -> (True, env1)
executeConstraint env (BoundedByKnown subPnt boundTp) = do
  let subScheme = descriptor env subPnt
  case subScheme of
    TypeCheckResE _ -> (True, env)
    TypeCheckResult _ (SType ub lb description) -> do
      let subScheme' = fmap (\ub' -> SType ub' lb description) (tryIntersectTypes env ub boundTp "executeConstraint BoundedByKnown")
      let env' = setScheme env subPnt subScheme' "BoundedByKnown"
      (True, env')
executeConstraint env@FEnv{feUnionAllObjs, feClassGraph} (BoundedByObjs pnt) = do
  let scheme = descriptor env pnt
  let boundScheme = descriptor env feUnionAllObjs
  case sequenceT (scheme, boundScheme) of
    TypeCheckResE _ -> (True, env)
    TypeCheckResult _ (SType TopType _ _, _) -> (False, env)
    TypeCheckResult _ (SType ub lb desc, SType boundUb _ _) -> do
      -- A partially applied tuple would not be a raw type on the unionObj,
      -- but a subset of the arguments in that type
      let ub' = intersectTypes feClassGraph ub boundUb
      let scheme' = return $ SType ub' lb desc
      -- let env' = setScheme env pnt scheme' $ printf "BoundedByObjs for %s\nBound: %s\n" (show ub) (show boundUb)
      let env' = setScheme env pnt scheme' $ printf "BoundedByObjs for %s\n" (show ub)
      (isSolved scheme', env')
executeConstraint env (ArrowTo srcPnt destPnt) = do
  let srcScheme = descriptor env srcPnt
  let destScheme = descriptor env destPnt
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> (True, env)
    TypeCheckResult _ (SType srcUb srcLb srcDesc, SType destUb destLb destDesc) -> do
      let constrained = arrowConstrainUbs env srcUb srcPnt destUb destPnt
      case constrained of
        TypeCheckResult _ (srcUb', destUb') -> do
          let srcScheme' = return $ SType srcUb' srcLb srcDesc
          let destScheme' = return $ SType destUb' destLb destDesc
          let env' = setScheme env srcPnt srcScheme' "ArrowTo src"
          let env'' = setScheme env' destPnt destScheme' "ArrowTo dest"
          (isSolved destScheme', env'')
        TypeCheckResE _ -> (True, env)
executeConstraint env (PropEq (superPnt, propName) subPnt) = do
  let superScheme = descriptor env superPnt
  let subScheme = descriptor env subPnt
  case sequenceT (superScheme, subScheme) of
    TypeCheckResE _ -> (True, env)
    (TypeCheckResult _ _) ->
      case sequenceT (superScheme, subScheme) of
        TypeCheckResult _ (superSType, subSType) -> do
          let (env2, superScheme', subScheme') = updateSchemeProp env (superPnt, superSType) propName (subPnt, subSType)
          let env3 = setScheme env2 superPnt superScheme' (printf "PropEq super (%s)" propName)
          let env4 = setScheme env3 subPnt subScheme' (printf"PropEq sub (%s)" propName)
          (isSolved subScheme, env4)
        TypeCheckResE _ -> (True, env)
executeConstraint env (VarEq (superPnt, varName) subPnt) = do
  let superScheme = descriptor env superPnt
  let subScheme = descriptor env subPnt
  case sequenceT (superScheme, subScheme) of
    TypeCheckResE _ -> (True, env)
    (TypeCheckResult _ _) ->
      case sequenceT (superScheme, subScheme) of
        TypeCheckResult _ (superSType, subSType) -> do
          let (superScheme', subScheme') = updateSchemeVar env superSType varName subSType
          let env' = setScheme env superPnt superScheme' "VarEq super"
          let env'' = setScheme env' subPnt subScheme' "VarEq sub"
          (isSolved subScheme, env'')
        TypeCheckResE _ -> (True, env)
executeConstraint env (AddArg (srcPnt, newArgName) destPnt) = do
  let srcScheme = descriptor env srcPnt
  let destScheme = descriptor env destPnt
  let checkName = printf "AddArg %s" (show newArgName)
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> (True, env)
    TypeCheckResult _ (SType TopType _ _, _) -> (False, env)
    TypeCheckResult notes (SType srcUb _ _, SType destUb destLb destDesc) ->
      case addArgToType env srcUb newArgName of
        Just destUb' ->
          case tryIntersectTypes env destUb' destUb checkName of
            TypeCheckResult notes2 destUb'' -> do
              let destScheme' = TypeCheckResult (notes ++ notes2) (SType destUb'' destLb destDesc)
              let env' = setScheme env destPnt destScheme' checkName
              (isSolved srcScheme || isSolved destScheme, env')
            TypeCheckResE notes2 -> do
              let res = TypeCheckResE (notes ++ notes2)
              let env' = setScheme env destPnt res checkName
              (True, env')
        Nothing -> (False, env)
executeConstraint env (AddInferArg srcPnt destPnt) = do
  let srcScheme = descriptor env srcPnt
  let destScheme = descriptor env destPnt
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> (True, env)
    TypeCheckResult _ (SType TopType _ _, _) -> (False, env)
    TypeCheckResult notes (SType srcUb _ _, SType destUb destLb destDesc) ->
      case addInferArgToType env srcUb of
        Just destUb' ->
          case tryIntersectTypes env destUb' destUb "AddInferArg intersect" of
            TypeCheckResult notes2 destUb'' -> do
              let destScheme' = TypeCheckResult (notes ++ notes2) (SType destUb'' destLb destDesc)
              let env' = setScheme env destPnt destScheme' "AddInferArg dest"
              (isSolved srcScheme || isSolved destScheme, env')
            TypeCheckResE notes2 -> do
              let res = TypeCheckResE (notes ++ notes2)
              let env' = setScheme env destPnt res "AddInferArg error"
              (True, env')
        Nothing -> (False, env)
executeConstraint env@FEnv{feClassGraph} (PowersetTo srcPnt destPnt) = do
  let srcScheme = descriptor env srcPnt
  let destScheme = descriptor env destPnt
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> (True, env)
    TypeCheckResult _ (SType ub1 _ _, SType ub2 lb2 description2) -> do
      let destScheme' = fmap (\ub -> SType ub lb2 description2) (tryIntersectTypes env (powersetType feClassGraph ub1) ub2 "executeConstraint PowersetTo")
      let env' = setScheme env destPnt destScheme' "PowersetTo"
      (isSolved destScheme', env')
executeConstraint env@FEnv{feClassGraph} (UnionOf parentPnt childrenM) = do
  let parentScheme = descriptor env parentPnt
  let tcresChildrenSchemes = fmap (descriptor env) childrenM
  case sequenceT (parentScheme, sequence tcresChildrenSchemes) of
    TypeCheckResE _ -> (True, env)
    TypeCheckResult notes (parentSType, childrenSchemes) -> do
      let accumulateChild (SType ub lb _) (accUb, accLb) = (unionTypes feClassGraph ub accUb, unionTypes feClassGraph lb accLb)
      let (chUb, chLb) = foldr accumulateChild (bottomType, bottomType) childrenSchemes
      case equalizeSTypes env (parentSType, SType (compactType feClassGraph chUb) chLb "") "executeConstraint UnionOf" of
        TypeCheckResult notes2 (parentST', _) -> do
          let parentScheme' = TypeCheckResult (notes ++ notes2) parentST'
          let env' = setScheme env parentPnt parentScheme' "UnionOf"
          (isSolved parentScheme', env')
        TypeCheckResE notes2 -> do
          let res = TypeCheckResE (notes ++ notes2)
          let env' = setScheme env parentPnt res "UnionOf"
          (True, env')

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
runConstraints 0 env@FEnv{feTrace} _ = TypeCheckResult [GenTypeCheckError Nothing $ printf "Reached runConstraints limit with still changing constraints: %s" (show $ showTraceConstrainEpoch env $ head $ tail feTrace)] env
runConstraints limit env cons = do
  let (constraintsToPrune, env'@FEnv{feUpdatedDuringEpoch}) = executeConstraints env cons
  let cons' = mapMaybe (\(con, shouldPrune) -> if shouldPrune then Nothing else Just con) $ zip cons constraintsToPrune
  if feUpdatedDuringEpoch
    then runConstraints (limit - 1) (nextConstrainEpoch env') cons'
    else return env'
