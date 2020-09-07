--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Constrain
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module TypeCheck.Constrain where

import           Data.Maybe
import qualified Data.HashMap.Strict as H

import           Syntax.Types
import           TypeCheck.Common
import           TypeCheck.TypeGraph
import           TypeCheck.Show
import           Data.Tuple.Sequence
import           Text.Printf

isSolved :: Scheme -> Bool
isSolved (TypeCheckResult _ (SType a b _)) = a == b
isSolved _ = False

checkScheme :: String -> Scheme -> Scheme
-- checkScheme msg (TypeCheckResult _ (SType ub _ desc)) | isBottomType ub = error $ "Scheme failed check at " ++ msg ++ ": upper bound is bottomType - " ++ desc
checkScheme msg (TypeCheckResult notes (SType ub _ desc)) | isBottomType ub = TypeCheckResE (GenTypeCheckError ("Scheme failed check at " ++ msg ++ ": upper bound is bottomType - " ++ desc) : notes)
checkScheme _ scheme = scheme

setScheme :: FEnv -> Pnt -> Scheme -> String -> FEnv
setScheme env p scheme msg = setDescriptor env p (checkScheme msg' scheme)
  where msg' = printf "setScheme %s" msg

findSVar :: FEnv -> Scheme -> TypeCheckResult SplitSType
findSVar env scheme = scheme >>= aux
  where aux (SType ub lb desc) = return (ub, lb, desc)
        aux (SVar _ p) = findSVar env (descriptor env p)

equalizeSTypes :: FEnv -> (SplitSType, SplitSType) -> String -> TypeCheckResult (SType, SType)
equalizeSTypes env@(FEnv _ _ (_, _, classMap) _) ((ub1, lb1, desc1), (ub2, lb2, desc2)) d = do
  let lbBoth = unionType classMap lb1 lb2
  ubBoth <- tryIntersectTypes env ub1 ub2 $ "equalizeSTypes(" ++ d ++ ")"
  if hasType classMap lbBoth ubBoth
    then return (SType ubBoth lbBoth desc1, SType ubBoth lbBoth desc2)
    else TypeCheckResE [GenTypeCheckError (printf "Type mismatched: %s is not a subtype of %s" (show lbBoth) (show ubBoth))]

updateSchemeProp :: FEnv -> SplitSType -> ArgName -> SplitSType -> (Scheme, Scheme)
updateSchemeProp (FEnv _ _ (_, _, classMap) _) (superUb, superLb, superDesc) propName (subUb, subLb, subDesc) = (return $ SType superUb' superLb superDesc, return $ SType subUb' subLb subDesc)
  where
    (superUb', subUb') = case (superUb, subUb) of
      (TopType, sub) -> (TopType, sub)
      (SumType supPartials, TopType) -> do
        let supPartialList = splitPartialLeafs supPartials
        let getProp (_, _, _, supArgs) = H.lookup propName supArgs
        let sub = unionTypes classMap $ mapMaybe getProp supPartialList
        (superUb, sub)
      (SumType supPartials, SumType subPartials) -> do
        let supPartialList = splitPartialLeafs supPartials
        let subPartialList = splitPartialLeafs subPartials
        let intersectPartials (supName, supVars, supProps, supArgs) sub = case H.lookup propName supArgs of
              Just supProp -> do
                let newProp = intersectTypes classMap supProp (singletonType sub)
                if isBottomType newProp
                  then Nothing
                  else Just ((supName, supVars, supProps, H.insert propName newProp supArgs), newProp)
              Nothing -> Nothing
        let (supPartialList', subPartialList') = unzip $ catMaybes $ [intersectPartials sup sub | sup <- supPartialList, sub <- subPartialList]
        (SumType $ joinPartialLeafs supPartialList', unionTypes classMap subPartialList')
      (sup, sub) -> error $ printf "Unsupported updateSchemeProp Ub (%s).%s = %s" (show sup) propName (show sub)

updateSchemeVar :: FEnv -> SplitSType -> TypeVarName -> SplitSType -> (Scheme, Scheme)
updateSchemeVar (FEnv _ _ (_, _, classMap) _) (superUb, superLb, superDesc) varName (subUb, subLb, subDesc) = (return $ SType superUb' superLb superDesc, return $ SType subUb' subLb subDesc)
  where
    (superUb', subUb') = case (superUb, subUb) of
      (TopType, sub) -> (TopType, sub)
      (SumType supPartials, TopType) -> do
        let supPartialList = splitPartialLeafs supPartials
        let getVar (_, supVars, _, _) = H.lookup varName supVars
        let sub = unionTypes classMap $ mapMaybe getVar supPartialList
        (superUb, sub)
      (SumType supPartials, SumType subPartials) -> do
        let supPartialList = splitPartialLeafs supPartials
        let subPartialList = splitPartialLeafs subPartials
        let intersectPartials (supName, supVars, supProps, supArgs) sub = case H.lookup varName supVars of
              Just supVar -> do
                let newVar = intersectTypes classMap supVar (singletonType sub)
                if isBottomType newVar
                  then Nothing
                  else Just ((supName, H.insert varName newVar supVars, supProps, supArgs), newVar)
              Nothing -> Just ((supName, H.insert varName (singletonType sub) supVars, supProps, supArgs), singletonType sub)
        let (supPartialList', subPartialList') = unzip $ catMaybes $ [intersectPartials sup sub | sup <- supPartialList, sub <- subPartialList]
        (SumType $ joinPartialLeafs supPartialList', unionTypes classMap subPartialList')
      (sup, sub) -> error $ printf "Unsupported updateSchemeVar Ub (%s).%s = %s" (show sup) varName (show sub)

addArgToType :: FEnv -> Type -> ArgName -> Maybe Type
addArgToType _ TopType _ = Nothing
addArgToType _ TypeVar{} _ = error "addArgToType TypeVar"
addArgToType (FEnv _ _ (_, _, classMap) _) (SumType partials) newArg = Just $ SumType partials'
  where
    partials' = joinPartialLeafs $ map fromPartial $ splitPartialLeafs partials
    fromPartial (partialName, partialVars, partialProps, partialArgs) = (partialName, partialVars, partialProps, H.insertWith (unionType classMap) newArg TopType partialArgs)

addInferArgToType :: FEnv -> Type -> Maybe Type
addInferArgToType _ TopType = Nothing
addInferArgToType _ TypeVar{} = error "addInferArgToType TypeVar"
addInferArgToType env@(FEnv _ _ (_, _, classMap) _) (SumType partials) = Just $ unionTypes classMap partials'
  where
    partials' = map (inferArgFromPartial env) $ splitPartialLeafs partials

-- returns updated (pruned) constraints and boolean if schemes were updated
executeConstraint :: FEnv -> Constraint -> ([Constraint], Bool, FEnv)
executeConstraint env (EqualsKnown pnt tp) = case descriptor env pnt of
  (TypeCheckResult notes oldSType) -> case oldSType of
    (SType ub lb desc) -> case equalizeSTypes env ((ub, lb, desc), (tp, tp, "")) "executeConstraint EqualsKnown" of
      TypeCheckResult notes2 (stype', _) -> do
        let scheme' = TypeCheckResult (notes ++ notes2) stype'
        let env' = setScheme env pnt scheme' "EqualsKnown"
        ([], True, env')
      TypeCheckResE _ -> ([], False, env)
    (SVar _ pnt') -> executeConstraint env (EqualsKnown pnt' tp)
  TypeCheckResE{} -> ([], False, env)
executeConstraint env (EqPoints p1 p2) | p1 == p2 = ([], False, env)
executeConstraint env1 cons@(EqPoints p1 p2) = case sequenceT (descriptor env1 p1, descriptor env1 p2) of
  TypeCheckResult _ (var@(SVar _ p1'), _) -> do
    let (_, _, env2) = executeConstraint env1 (EqPoints p1' p2)
    let env3 = setScheme env2 p2 (return var) "EqPoints"
    ([],  True, env3)
  TypeCheckResult _ (_, var@(SVar _ p2')) -> do
    let (_, _, env2) = executeConstraint env1 (EqPoints p1 p2')
    let env3 = setScheme env2 p1 (return var) "EqPoints"
    ([],  True, env3)
  TypeCheckResult notes (s1@(SType ub1 lb1 desc1), s2@(SType ub2 lb2 desc2)) -> case equalizeSTypes env1 ((ub1, lb1, desc1), (ub2, lb2, desc2)) "executeConstraint EqPoints" of
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
    TypeCheckResult _ (SVar _ subPnt') -> executeConstraint env (BoundedByKnown subPnt' boundTp)
    TypeCheckResult _ (SType ub lb description) -> do
      let subScheme' = fmap (\ub' -> SType ub' lb description) (tryIntersectTypes env ub boundTp "executeConstraint BoundedByKnown")
      let env' = setScheme env subPnt subScheme' "BoundedByKnown"
      ([], subScheme /= subScheme', env')
executeConstraint env@(FEnv _ _ ((unionAllObjs, unionTypeObjs), _, classMap) _) cons@(BoundedByObjs bnd pnt) = do
  let scheme = descriptor env pnt
  let unionPnt = case bnd of
        BoundAllObjs -> unionAllObjs
        BoundTypeObjs -> unionTypeObjs
  let unionScheme = descriptor env unionPnt
  case sequenceT (scheme, unionScheme) of
    TypeCheckResE _ -> ([], False, env)
    TypeCheckResult _ (SType TopType _ _, _) -> ([cons], False, env)
    TypeCheckResult _ (SVar _ pnt', _) -> executeConstraint env (BoundedByObjs bnd pnt')
    TypeCheckResult _ (_, SVar{}) -> error "bound point is type var"
    TypeCheckResult _ (SType ub lb desc, SType objsUb _ _) -> do
      -- A partially applied tuple would not be a raw type on the unionObj,
      -- but a subset of the arguments in that type
      let ub' = intersectTypes classMap ub objsUb
      let scheme' = if isBottomType ub'
            then TypeCheckResE [GenTypeCheckError $ printf "Failed to BoundByObjs for %s: \n\t%s" desc (show ub)]
            else return $ SType ub' lb desc
      let env' = setScheme env pnt scheme' "BoundedByObjs"
      ([cons | not (isSolved scheme')], scheme /= scheme', env')
executeConstraint env cons@(ArrowTo srcPnt destPnt) = do
  let srcScheme = descriptor env srcPnt
  let destScheme = descriptor env destPnt
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> ([], False, env)
    TypeCheckResult _ (SVar _ srcPnt', _) -> executeConstraint env (ArrowTo srcPnt' destPnt)
    TypeCheckResult _ (_, SVar _ destPnt') -> executeConstraint env (ArrowTo srcPnt destPnt')
    TypeCheckResult _ (SType srcUb srcLb srcDesc, SType destUb destLb destDesc) -> do
      let constrained = arrowConstrainUbs env srcUb destUb
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
    (TypeCheckResult _ (SVar _ superPnt', _)) -> executeConstraint env (PropEq (superPnt', propName) subPnt)
    (TypeCheckResult _ _) ->
      case sequenceT (findSVar env superScheme, findSVar env subScheme) of
        TypeCheckResult _ (superSType, subSType) -> do
          let (superScheme', subScheme') = updateSchemeProp env superSType propName subSType
          let env' = setScheme env superPnt superScheme' "PropEq super"
          let env'' = setScheme env' subPnt subScheme' "PropEq sub"
          ([cons | not (isSolved subScheme)], subScheme /= subScheme' || superScheme /= superScheme', env'')
        TypeCheckResE _ -> ([], False, env)
executeConstraint env cons@(VarEq (superPnt, varName) subPnt) = do
  let superScheme = descriptor env superPnt
  let subScheme = descriptor env subPnt
  case sequenceT (superScheme, subScheme) of
    TypeCheckResE _ -> ([], False, env)
    (TypeCheckResult _ (SVar _ superPnt', _)) -> executeConstraint env (VarEq (superPnt', varName) subPnt)
    (TypeCheckResult _ _) ->
      case sequenceT (findSVar env superScheme, findSVar env subScheme) of
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
    TypeCheckResult _ (SVar _ srcPnt', _) -> executeConstraint env (AddArg (srcPnt', newArgName) destPnt)
    TypeCheckResult _ (_, SVar _ destPnt') -> executeConstraint env (AddArg (srcPnt, newArgName) destPnt')
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
    TypeCheckResult _ (SVar _ srcPnt', _) -> executeConstraint env (AddInferArg srcPnt' destPnt)
    TypeCheckResult _ (_, SVar _ destPnt') -> executeConstraint env (AddInferArg srcPnt destPnt')
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
executeConstraint env cons@(PowersetTo srcPnt destPnt) = do
  let srcScheme = descriptor env srcPnt
  let destScheme = descriptor env destPnt
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> ([], False, env)
    TypeCheckResult _ (SVar _ srcPnt', _) -> executeConstraint env (PowersetTo srcPnt' destPnt)
    TypeCheckResult _ (_, SVar _ destPnt') -> executeConstraint env (PowersetTo srcPnt destPnt')
    TypeCheckResult _ (SType ub1 _ _, SType ub2 lb2 description2) -> do
      let destScheme' = fmap (\ub -> SType ub lb2 description2) (tryIntersectTypes env (powersetType ub1) ub2 "executeConstraint PowersetTo")
      let env' = setScheme env destPnt destScheme' "PowersetTo"
      ([cons | not (isSolved destScheme')], destScheme /= destScheme', env')
executeConstraint env@(FEnv _ _ (_, _, classMap) _) cons@(UnionOf parentPnt childrenPnts) = do
  let parentScheme = descriptor env parentPnt
  let tcresChildrenSchemes = fmap (descriptor env) childrenPnts
  case sequenceT (parentScheme, sequence tcresChildrenSchemes) of
    TypeCheckResE _ -> ([], False, env)
    TypeCheckResult _ (SVar _ parentPnt', _) -> executeConstraint env (UnionOf parentPnt' childrenPnts)
    TypeCheckResult notes (SType pub plb pdesc, childrenSchemes) -> do
      let (chUb, chLb) = (\(ub, lb) -> (ub, lb)) $ foldr (\(SType ub1 lb1 _) (ub2, lb2) -> (unionType classMap ub1 ub2, unionType classMap lb1 lb2)) (bottomType, bottomType) childrenSchemes
      case equalizeSTypes env ((pub, plb, pdesc), (chUb, chLb, "")) "executeConstraint UnionOf" of
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
executeConstraints env (c:cs) = ((a, b):res, env'')
  where
    (a, b, env') = executeConstraint env c
    (res, env'') = executeConstraints env' cs

runConstraints :: Integer -> FEnv -> [Constraint] -> TypeCheckResult FEnv
runConstraints _ env [] = return env
runConstraints 0 env cons = do
  let (res, env') = executeConstraints env cons
  let consChangedList = mapMaybe (\(con, isChanged) -> if isChanged then Just con else Nothing) res
  let showCons = showConstraints env' $ concat consChangedList
  TypeCheckResE [GenTypeCheckError $ "Reached runConstraints limit with still changing constraints: " ++ show showCons]
runConstraints limit env cons = do
  let (res, env') = executeConstraints env cons
  let (consList, changedList) = unzip res
  let cons' = concat consList
  if not (or changedList)
    then return env'
    else runConstraints (limit - 1) env' cons'
