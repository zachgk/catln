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
import           Syntax
import           Syntax.Types
import           Text.Printf
import           TypeCheck.Common
import           TypeCheck.Show
import           TypeCheck.TypeGraph

isSolved :: Scheme -> Bool
isSolved (TypeCheckResult _ (SType a b _)) = a == b
isSolved _                                 = False

setScheme :: FEnv -> VarMeta -> Scheme -> String -> FEnv
setScheme env p scheme msg = setDescriptor env p (checkScheme scheme) msg
  where msg' = printf "Scheme failed check at setScheme %s(point %s): upper bound is bottomType - " msg (show p)
        -- checkScheme (TypeCheckResult _ (SType ub _ desc)) | isBottomType ub = error $ msg' ++ desc
        checkScheme (TypeCheckResult notes (SType ub _ desc)) | isBottomType ub = TypeCheckResE (GenTypeCheckError (getMetaPos p) (msg' ++ desc) : notes)
        checkScheme s = s

equalizeSTypes :: FEnv -> (SType, SType) -> String -> TypeCheckResult (SType, SType)
equalizeSTypes env@FEnv{feClassMap} (SType ub1 lb1 desc1, SType ub2 lb2 desc2) d = do
  let lbBoth = unionTypes feClassMap lb1 lb2
  ubBoth <- tryIntersectTypes env ub1 ub2 $ "equalizeSTypes(" ++ d ++ ")"
  if isSubtypeOf feClassMap lbBoth ubBoth
    then return (SType ubBoth lbBoth desc1, SType ubBoth lbBoth desc2)
    else TypeCheckResE [GenTypeCheckError Nothing (printf "Type mismatched: %s is not a subtype of %s" (show lbBoth) (show ubBoth))]

updateSchemeProp :: FEnv -> (VarMeta, SType) -> ArgName -> (VarMeta, SType) -> (FEnv, Scheme, Scheme)
updateSchemeProp env@FEnv{feClassMap} (superM, superScheme@(SType superUb superLb superDesc)) propName (subM, subScheme@(SType subUb subLb subDesc)) = case (superUb, subUb) of
    (TopType, _) -> wrapUbs (TopType, subUb)
    (TypeVar v, _) -> do
      let (TypeCheckResult _ superM') = resolveTypeVar v superM
      let (TypeCheckResult _ super') = pure superM' >>= descriptor env
      let (env2, super'', sub'') = updateSchemeProp env (superM, super') propName (subM, subScheme)
      let env3 = setScheme env2 superM' super'' "PropEq var super"
      (env3, pure superScheme, sub'')
    (UnionType supPartials, TypeVar{}) -> do
      let supPartialList = splitUnionType supPartials
      let intersectPartials sup@PartialType{ptArgs=supArgs} = Just (sup{ptArgs=H.insert propName subUb supArgs})
      let supPartialList' = catMaybes $ [intersectPartials sup | sup <- supPartialList]
      wrapUbs (compactType feClassMap $ UnionType $ joinUnionType supPartialList', subUb)
    (UnionType supPartials, TopType) -> do
      let supPartialList = splitUnionType supPartials
      let sub' = unionAllTypes feClassMap $ mapMaybe (typeGetArg propName) supPartialList
      wrapUbs (superUb, sub')
    (UnionType supPartials, UnionType subPartials) -> do
      let supPartialList = splitUnionType supPartials
      let subPartialList = splitUnionType subPartials
      let intersectPartials sup@PartialType{ptArgs=supArgs, ptVars=supVars} sub = case H.lookup propName supArgs of
            Just (TypeVar (TVVar v)) -> do
              let supVar = H.lookupDefault TopType v supVars
              let newProp = intersectTypes feClassMap supVar (singletonType sub)
              Just (sup{ptVars=H.insert v newProp supVars}, newProp)
            Just (TypeVar TVArg{}) -> error $ printf "Not yet implemented"
            Just supProp -> do
              let newProp = intersectTypes feClassMap supProp (singletonType sub)
              if isBottomType newProp
                then Nothing
                else Just (sup{ptArgs=H.insert propName newProp supArgs}, newProp)
            Nothing -> Nothing
      let (supPartialList', subPartialList') = unzip $ catMaybes $ [intersectPartials sup sub | sup <- supPartialList, sub <- subPartialList]
      wrapUbs (compactType feClassMap $ UnionType $ joinUnionType supPartialList', unionAllTypes feClassMap subPartialList')
  where
    wrapUbs (superUb', subUb') = (env, return $ SType superUb' superLb superDesc, return $ SType subUb' subLb subDesc)

updateSchemeVar :: FEnv -> SType -> TypeVarName -> SType -> (Scheme, Scheme)
updateSchemeVar FEnv{feClassMap} (SType superUb superLb superDesc) varName (SType subUb subLb subDesc) = (return $ SType superUb' superLb superDesc, return $ SType subUb' subLb subDesc)
  where
    (superUb', subUb') = case (superUb, subUb) of
      (TopType, sub) -> (TopType, sub)
      (UnionType supPartials, TopType) -> do
        let supPartialList = splitUnionType supPartials
        let getVar PartialType{ptVars=supVars} = H.lookup varName supVars
        let sub = unionAllTypes feClassMap $ mapMaybe getVar supPartialList
        (superUb, sub)
      (UnionType supPartials, UnionType subPartials) -> do
        let supPartialList = splitUnionType supPartials
        let subPartialList = splitUnionType subPartials
        let intersectPartials sup@PartialType{ptVars=supVars} sub = case H.lookup varName supVars of
              Just supVar -> do
                let newVar = intersectTypes feClassMap supVar (singletonType sub)
                if isBottomType newVar
                  then Nothing
                  else Just (sup{ptVars=H.insert varName newVar supVars}, newVar)
              Nothing -> Just (sup{ptVars=H.insert varName (singletonType sub) supVars}, singletonType sub)
        let (supPartialList', subPartialList') = unzip $ catMaybes $ [intersectPartials sup sub | sup <- supPartialList, sub <- subPartialList]
        (compactType feClassMap $ UnionType $ joinUnionType supPartialList', unionAllTypes feClassMap subPartialList')
      (UnionType supPartials, subT@TypeVar{}) -> do
        let supPartialList = splitUnionType supPartials
        let intersectPartials sup@PartialType{ptVars=supVars} = case H.lookup varName supVars of
              Just supVar -> do
                if supVar == subT
                  then Just sup
                  else Nothing
              Nothing -> Nothing
        let supPartialList' = catMaybes [intersectPartials sup | sup <- supPartialList]
        (compactType feClassMap $ UnionType $ joinUnionType supPartialList', subT)
      (sup, sub) -> error $ printf "Unsupported updateSchemeVar Ub (%s).%s = %s" (show sup) varName (show sub)

addArgToType :: FEnv -> Type -> ArgName -> Maybe Type
addArgToType _ TopType _ = Nothing
addArgToType _ TypeVar{} _ = error "addArgToType TypeVar"
addArgToType FEnv{feClassMap} (UnionType partials) newArg = Just $ UnionType partials'
  where
    partials' = joinUnionType $ map fromPartial $ splitUnionType partials
    fromPartial partial@PartialType{ptArgs} = partial{ptArgs=H.insertWith (unionTypes feClassMap) newArg TopType ptArgs}

addInferArgToType :: FEnv -> Type -> Maybe Type
addInferArgToType _ TopType = Nothing
addInferArgToType _ TypeVar{} = error "addInferArgToType TypeVar"
addInferArgToType env@FEnv{feClassMap} (UnionType partials) = Just $ unionAllTypes feClassMap partials'
  where
    partials' = map (inferArgFromPartial env) $ splitUnionType partials

-- returns updated (pruned) constraints and boolean if schemes were updated
executeConstraint :: FEnv -> Constraint -> ([Constraint], Bool, FEnv)
executeConstraint env (EqualsKnown pnt tp) = case descriptor env pnt of
  (TypeCheckResult notes stype) -> case equalizeSTypes env (stype, SType tp tp "") "executeConstraint EqualsKnown" of
    TypeCheckResult notes2 (stype', _) -> do
      let scheme' = TypeCheckResult (notes ++ notes2) stype'
      let env' = setScheme env pnt scheme' "EqualsKnown"
      ([], True, env')
    TypeCheckResE _ -> ([], False, env)
  TypeCheckResE{} -> ([], False, env)
executeConstraint env (EqPoints (VarMeta p1 _ _) (VarMeta p2 _ _)) | p1 == p2 = ([], False, env)
executeConstraint env1 cons@(EqPoints p1 p2) = case sequenceT (descriptor env1 p1, descriptor env1 p2) of
  TypeCheckResult notes (s1, s2) -> case equalizeSTypes env1 (s1, s2) "executeConstraint EqPoints" of
    TypeCheckResult notes2 (s1', s2') -> do
      let env2 = setScheme env1 p1 (TypeCheckResult (notes ++ notes2) s1') "EqPoints"
      let env3 = setScheme env2 p2 (return s2') "EqPoints"
      ([cons | not (isSolved $ return s1')], s1 /= s1' || s2 /= s2', env3)
    TypeCheckResE notes2 -> do
      let res = TypeCheckResE (notes ++ notes2)
      let env2 = setScheme env1 p1 res "EqPoints"
      let env3 = setScheme env2 p2 res "EqPoints"
      ([], True, env3)
  TypeCheckResE _ -> ([], False, env1)
executeConstraint env (BoundedByKnown subPnt boundTp) = do
  let subScheme = descriptor env subPnt
  case subScheme of
    TypeCheckResE _ -> ([], False, env)
    TypeCheckResult _ (SType ub lb description) -> do
      let subScheme' = fmap (\ub' -> SType ub' lb description) (tryIntersectTypes env ub boundTp "executeConstraint BoundedByKnown")
      let env' = setScheme env subPnt subScheme' "BoundedByKnown"
      ([], subScheme /= subScheme', env')
executeConstraint env@FEnv{feUnionAllObjs, feUnionTypeObjs, feClassMap} cons@(BoundedByObjs bnd pnt) = do
  let scheme = descriptor env pnt
  let boundPnt = case bnd of
        BoundAllObjs  -> feUnionAllObjs
        BoundTypeObjs -> feUnionTypeObjs
  let boundScheme = descriptor env boundPnt
  case sequenceT (scheme, boundScheme) of
    TypeCheckResE _ -> ([], False, env)
    TypeCheckResult _ (SType TopType _ _, _) -> ([cons], False, env)
    TypeCheckResult _ (SType ub lb desc, SType boundUb _ _) -> do
      -- A partially applied tuple would not be a raw type on the unionObj,
      -- but a subset of the arguments in that type
      let ub' = intersectTypes feClassMap ub boundUb
      let scheme' = return $ SType ub' lb desc
      let env' = setScheme env pnt scheme' $ printf "BoundedByObjs for %s" (show ub)
      ([cons | not (isSolved scheme')], scheme /= scheme', env')
executeConstraint env cons@(ArrowTo srcPnt destPnt) = do
  let srcScheme = descriptor env srcPnt
  let destScheme = descriptor env destPnt
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> ([], False, env)
    TypeCheckResult _ (SType srcUb srcLb srcDesc, SType destUb destLb destDesc) -> do
      let constrained = arrowConstrainUbs env srcUb srcPnt destUb destPnt
      case constrained of
        TypeCheckResult _ (srcUb', destUb') -> do
          let srcScheme' = return $ SType srcUb' srcLb srcDesc
          let destScheme' = return $ SType destUb' destLb destDesc
          let env' = setScheme env srcPnt srcScheme' "ArrowTo src"
          let env'' = setScheme env' destPnt destScheme' "ArrowTo dest"
          ([cons | not (isSolved destScheme')], srcScheme /= srcScheme' || destScheme /= destScheme', env'')
        TypeCheckResE _ -> ([], False, env)
executeConstraint env cons@(PropEq (superPnt, propName) subPnt) = do
  let superScheme = descriptor env superPnt
  let subScheme = descriptor env subPnt
  case sequenceT (superScheme, subScheme) of
    TypeCheckResE _ -> ([], False, env)
    (TypeCheckResult _ _) ->
      case sequenceT (superScheme, subScheme) of
        TypeCheckResult _ (superSType, subSType) -> do
          let (env2, superScheme', subScheme') = updateSchemeProp env (superPnt, superSType) propName (subPnt, subSType)
          let env3 = setScheme env2 superPnt superScheme' (printf "PropEq super (%s)" propName)
          let env4 = setScheme env3 subPnt subScheme' (printf"PropEq sub (%s)" propName)
          ([cons | not (isSolved subScheme)], subScheme /= subScheme' || superScheme /= superScheme', env4)
        TypeCheckResE _ -> ([], False, env)
executeConstraint env cons@(VarEq (superPnt, varName) subPnt) = do
  let superScheme = descriptor env superPnt
  let subScheme = descriptor env subPnt
  case sequenceT (superScheme, subScheme) of
    TypeCheckResE _ -> ([], False, env)
    (TypeCheckResult _ _) ->
      case sequenceT (superScheme, subScheme) of
        TypeCheckResult _ (superSType, subSType) -> do
          let (superScheme', subScheme') = updateSchemeVar env superSType varName subSType
          let env' = setScheme env superPnt superScheme' "VarEq super"
          let env'' = setScheme env' subPnt subScheme' "VarEq sub"
          ([cons | not (isSolved subScheme)], subScheme /= subScheme' || superScheme /= superScheme', env'')
        TypeCheckResE _ -> ([], False, env)
executeConstraint env cons@(AddArg (srcPnt, newArgName) destPnt) = do
  let srcScheme = descriptor env srcPnt
  let destScheme = descriptor env destPnt
  let checkName = printf "AddArg %s" (show newArgName)
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> ([], False, env)
    TypeCheckResult _ (SType TopType _ _, _) -> ([cons], False, env)
    TypeCheckResult notes (SType srcUb _ _, SType destUb destLb destDesc) ->
      case addArgToType env srcUb newArgName of
        Just destUb' ->
          case tryIntersectTypes env destUb' destUb checkName of
            TypeCheckResult notes2 destUb'' -> do
              let destScheme' = TypeCheckResult (notes ++ notes2) (SType destUb'' destLb destDesc)
              let env' = setScheme env destPnt destScheme' checkName
              ([cons | not (isSolved srcScheme || isSolved destScheme)], destScheme /= destScheme', env')
            TypeCheckResE notes2 -> do
              let res = TypeCheckResE (notes ++ notes2)
              let env' = setScheme env destPnt res checkName
              ([], True, env')
        Nothing -> ([cons], False, env)
executeConstraint env cons@(AddInferArg srcPnt destPnt) = do
  let srcScheme = descriptor env srcPnt
  let destScheme = descriptor env destPnt
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> ([], False, env)
    TypeCheckResult _ (SType TopType _ _, _) -> ([cons], False, env)
    TypeCheckResult notes (SType srcUb _ _, SType destUb destLb destDesc) ->
      case addInferArgToType env srcUb of
        Just destUb' ->
          case tryIntersectTypes env destUb' destUb "AddInferArg intersect" of
            TypeCheckResult notes2 destUb'' -> do
              let destScheme' = TypeCheckResult (notes ++ notes2) (SType destUb'' destLb destDesc)
              let env' = setScheme env destPnt destScheme' "AddInferArg dest"
              ([cons | not (isSolved srcScheme || isSolved destScheme)], destScheme /= destScheme', env')
            TypeCheckResE notes2 -> do
              let res = TypeCheckResE (notes ++ notes2)
              let env' = setScheme env destPnt res "AddInferArg error"
              ([], True, env')
        Nothing -> ([cons], False, env)
executeConstraint env@FEnv{feClassMap} cons@(PowersetTo srcPnt destPnt) = do
  let srcScheme = descriptor env srcPnt
  let destScheme = descriptor env destPnt
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> ([], False, env)
    TypeCheckResult _ (SType ub1 _ _, SType ub2 lb2 description2) -> do
      let destScheme' = fmap (\ub -> SType ub lb2 description2) (tryIntersectTypes env (compactType feClassMap $ powersetType ub1) ub2 "executeConstraint PowersetTo")
      let env' = setScheme env destPnt destScheme' "PowersetTo"
      ([cons | not (isSolved destScheme')], destScheme /= destScheme', env')
executeConstraint env@FEnv{feClassMap} cons@(UnionOf parentPnt childrenM) = do
  let parentScheme = descriptor env parentPnt
  let tcresChildrenSchemes = fmap (descriptor env) childrenM
  case sequenceT (parentScheme, sequence tcresChildrenSchemes) of
    TypeCheckResE _ -> ([], False, env)
    TypeCheckResult notes (parentSType, childrenSchemes) -> do
      let accumulateChild (SType ub lb _) (accUb, accLb) = (unionTypes feClassMap ub accUb, unionTypes feClassMap lb accLb)
      let (chUb, chLb) = foldr accumulateChild (bottomType, bottomType) childrenSchemes
      case equalizeSTypes env (parentSType, SType chUb chLb "") "executeConstraint UnionOf" of
        TypeCheckResult notes2 (parentST', _) -> do
          let parentScheme' = TypeCheckResult (notes ++ notes2) parentST'
          let env' = setScheme env parentPnt parentScheme' "UnionOf"
          ([cons | not (isSolved parentScheme')], parentScheme /= parentScheme', env')
        TypeCheckResE notes2 -> do
          let res = TypeCheckResE (notes ++ notes2)
          let env' = setScheme env parentPnt res "UnionOf"
          ([], True, env')

executeConstraints :: FEnv -> [Constraint] -> ([([Constraint], Bool)], FEnv)
executeConstraints env [] = ([], env)
executeConstraints env1 (c:cs) = ((a, b):res, env4)
  where
    env2 = startConstraint c env1
    (a, b, env3) = executeConstraint env2 c
    (res, env4) = executeConstraints env3 cs

runConstraints :: Integer -> FEnv -> [Constraint] -> TypeCheckResult FEnv
runConstraints _ env [] = return env
runConstraints 0 env cons = do
  let (res, env') = executeConstraints env cons
  let consChangedList = mapMaybe (\(con, isChanged) -> if isChanged then Just con else Nothing) res
  let showCons = showConstraints env' $ concat consChangedList
  TypeCheckResult [GenTypeCheckError Nothing $ "Reached runConstraints limit with still changing constraints: " ++ show showCons] env
runConstraints limit env cons = do
  let (res, env') = executeConstraints env cons
  let (consList, changedList) = unzip res
  let cons' = concat consList
  if not (or changedList)
    then return env'
    else runConstraints (limit - 1) (nextConstrainEpoch env') cons'
