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
import qualified Data.HashSet as S

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
checkScheme msg (TypeCheckResult notes (SType ub _ desc)) | ub == bottomType = TypeCheckResE (GenTypeCheckError ("Scheme failed check at " ++ msg ++ ": upper bound is bottomType - " ++ desc) : notes)
checkScheme _ scheme = scheme

setScheme :: FEnv -> Pnt -> Scheme -> String -> FEnv
setScheme env p scheme msg = setDescriptor env p (checkScheme msg scheme)

equalizeSTypes :: (SplitSType, SplitSType) -> String -> TypeCheckResult (SType, SType)
equalizeSTypes ((ub1, lb1, desc1), (ub2, lb2, desc2)) d = do
  let lbBoth = unionType lb1 lb2
  ubBoth <- tryIntersectTypes ub1 ub2 $ "equalizeSTypes(" ++ d ++ ")"
  if hasType lbBoth ubBoth
    then return (SType ubBoth lbBoth desc1, SType ubBoth lbBoth desc2)
    else TypeCheckResE [GenTypeCheckError (printf "Type mismatched: %s is not a subtype of %s" (show lbBoth) (show ubBoth))]

getSchemeProp :: FEnv -> Scheme -> ArgName -> TypeCheckResult SplitSType
getSchemeProp env inScheme propName = do
  inScheme' <- inScheme
  case inScheme' of
    (SVar _ p) -> do
      let inScheme'' = descriptor env p
      getSchemeProp env inScheme'' propName
    (SType ub lb desc) -> do
      let ub' = getTypeProp ub
      let lb' = getTypeProp lb
      case checkScheme (printf "getSchemeProp with prop %s from %s" propName (show inScheme)) $ return $ SType ub' lb' desc of
        TypeCheckResult notes _ -> TypeCheckResult notes (ub', lb', desc)
        TypeCheckResE notes -> TypeCheckResE notes
  where
    getTypeProp :: Type -> Type
    getTypeProp TopType = TopType
    getTypeProp TypeVar{} = error "getSchemeProp getTypeProp TypeVar"
    getTypeProp (SumType partials) = case getPartials partials of
      TopType -> TopType
      TypeVar{} -> error "getSchemeProp getPartials TypeVar"
      (SumType partials') -> SumType partials'
    getPartials :: PartialLeafs -> Type
    getPartials partials = unionTypes $ mapMaybe (H.lookup propName . snd) $ concatMap S.toList $ H.elems partials

setSchemeProp :: FEnv -> Scheme -> ArgName -> Scheme -> Scheme
setSchemeProp env scheme propName pscheme = do
  scheme' <- scheme
  pscheme' <- pscheme
  case (scheme', pscheme') of
    (SVar _ p', _) -> do
      let scheme'' = descriptor env p'
      setSchemeProp env scheme'' propName pscheme
    (_, SVar _ p') -> do
      let pscheme'' = descriptor env p'
      setSchemeProp env scheme propName pscheme''
    (SType ub lb desc, SType pub _ _) -> checkScheme ("setSchemeProp " ++ propName) $ return $ SType (compactType $ setTypeUbProp ub pub) (compactType $ setTypeLbProp lb) desc
  where
    setTypeUbProp :: Type -> Type -> Type
    setTypeUbProp TopType _ = TopType
    setTypeUbProp TypeVar{} _ = error "setSchemeProp setTypeUbProp TypeVar"
    setTypeUbProp (SumType ubPartials) pub = SumType (joinPartialLeafs $ mapMaybe (setPartialsUb pub) $ splitPartialLeafs ubPartials)
    setPartialsUb TopType partial = Just partial
    setPartialsUb pub (partialName, partialVars, partialArgs) = case H.lookup propName partialArgs of
      Just partialArg -> let partialArg' = intersectTypes partialArg pub
                          in if partialArg' == bottomType
                                then Nothing
                                else Just (partialName, partialVars, H.insert propName partialArg' partialArgs)
      Nothing -> Nothing
    setTypeLbProp tp = tp -- TODO: Should set with union?

addArgsToType :: Type -> S.HashSet ArgName -> Maybe Type
addArgsToType TopType _ = Nothing
addArgsToType TypeVar{} _ = error "addArgsToType TypeVar"
addArgsToType (SumType partials) newArgs = Just $ SumType partials'
  where
    partialUpdate = H.fromList $ map (,TopType) $ S.toList newArgs
    partials' = joinPartialLeafs $ map fromPartial $ splitPartialLeafs partials
    fromPartial (partialName, partialVars, partialArgs) = (partialName, partialVars, H.unionWith unionType partialArgs partialUpdate)

-- returns updated (pruned) constraints and boolean if schemes were updated
executeConstraint :: FEnv -> Constraint -> ([Constraint], Bool, FEnv)
executeConstraint env (EqualsKnown pnt tp) = case descriptor env pnt of
  (TypeCheckResult notes oldSType) -> case oldSType of
    (SType ub lb desc) -> case equalizeSTypes ((ub, lb, desc), (tp, tp, "")) "executeConstraint EqualsKnown" of
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
  TypeCheckResult notes (s1@(SType ub1 lb1 desc1), s2@(SType ub2 lb2 desc2)) -> case equalizeSTypes ((ub1, lb1, desc1), (ub2, lb2, desc2)) "executeConstraint EqPoints" of
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
      let subScheme' = fmap (\ub' -> SType ub' lb description) (tryIntersectTypes ub boundTp "executeConstraint BoundedByKnown")
      let env' = setScheme env subPnt subScheme' "BoundedByKnown"
      ([], subScheme /= subScheme', env')
executeConstraint env@(FEnv _ _ ((unionAllObjs, unionTypeObjs), _) _) cons@(BoundedByObjs bnd pnt) = do
  let scheme = descriptor env pnt
  let unionPnt = case bnd of
        BoundAllObjs -> unionAllObjs
        BoundTypeObjs -> unionTypeObjs
  let unionScheme = descriptor env unionPnt
  case sequenceT (scheme, unionScheme) of
    TypeCheckResE _ -> ([], False, env)
    TypeCheckResult _ (SVar _ pnt', _) -> executeConstraint env (BoundedByObjs bnd pnt')
    TypeCheckResult _ (_, SVar{}) -> error "bound point is type var"
    TypeCheckResult _ (SType ub lb desc, SType objsUb _ _) -> do
      -- A partially applied tuple would not be a raw type on the unionObj,
      -- but a subset of the arguments in that type
      let ub' = intersectTypes ub objsUb
      let scheme' = if ub' == bottomType
            then TypeCheckResE [GenTypeCheckError $ printf "Failed to BoundByObjs for %s: \n\t%s \n\twith \n\t%s" desc (show ub) (show objsUb)]
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
    (TypeCheckResult _ (_, SVar _ subPnt')) -> executeConstraint env (PropEq (superPnt, propName) subPnt')
    (TypeCheckResult notes (SType{}, SType ub2 lb2 desc2)) -> do
      let maybeSuperPropSType = getSchemeProp env superScheme propName
      case maybeSuperPropSType of
        TypeCheckResult notes2 superPropSType -> do
          case equalizeSTypes (superPropSType, (ub2, lb2, desc2)) $ printf "executeConstraint PropEq %s" propName of
            TypeCheckResult notes3 (subST', superST') -> do
              let subScheme' = TypeCheckResult (notes ++ notes2 ++ notes3) subST'
              let superPropScheme' = return superST'
              let superScheme' = setSchemeProp env superScheme propName superPropScheme'
              let env' = setScheme env subPnt subScheme' "PropEq sub"
              let env'' = setScheme env' superPnt superScheme' "PropEq super"
              ([cons | not (isSolved subScheme')], subScheme /= subScheme' || superScheme /= superScheme', env'')
            TypeCheckResE notes3 -> do
              let res = TypeCheckResE (notes ++ notes2 ++ notes3)
              let env' = setScheme env subPnt res "PropEq sub"
              let env'' = setScheme env' superPnt res "PropEq super"
              ([], True, env'')
        TypeCheckResE notes2 -> do
          let res = TypeCheckResE (notes ++ notes2)
          let env' = setScheme env superPnt res "PropEq super"
          ([], True, env')
executeConstraint env cons@(AddArgs (srcPnt, newArgNames) destPnt) = do
  let srcScheme = descriptor env srcPnt
  let destScheme = descriptor env destPnt
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> ([], False, env)
    TypeCheckResult _ (SVar _ srcPnt', _) -> executeConstraint env (AddArgs (srcPnt', newArgNames) destPnt)
    TypeCheckResult _ (_, SVar _ destPnt') -> executeConstraint env (AddArgs (srcPnt, newArgNames) destPnt')
    TypeCheckResult _ (SType TopType _ _, _) -> ([cons], False, env)
    TypeCheckResult notes (SType srcUb _ _, SType destUb destLb destDesc) ->
      case addArgsToType srcUb newArgNames of
        Just destUb' ->
          case tryIntersectTypes destUb' destUb "AddArgs" of
            TypeCheckResult notes2 destUb'' -> do
              let destScheme' = TypeCheckResult (notes ++ notes2) (SType destUb'' destLb destDesc)
              let env' = setScheme env destPnt destScheme' "AddArgs"
              ([cons | not (isSolved srcScheme || isSolved destScheme)], destScheme /= destScheme', env')
            TypeCheckResE notes2 -> do
              let res = TypeCheckResE (notes ++ notes2)
              let env' = setScheme env destPnt res "AddArgs"
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
      let destScheme' = fmap (\ub -> SType ub lb2 description2) (tryIntersectTypes (powersetType ub1) ub2 "executeConstraint PowersetTo")
      let env' = setScheme env destPnt destScheme' "PowersetTo"
      ([cons | not (isSolved destScheme')], destScheme /= destScheme', env')
executeConstraint env cons@(UnionOf parentPnt childrenPnts) = do
  let parentScheme = descriptor env parentPnt
  let tcresChildrenSchemes = fmap (descriptor env) childrenPnts
  case sequenceT (parentScheme, sequence tcresChildrenSchemes) of
    TypeCheckResE _ -> ([], False, env)
    TypeCheckResult _ (SVar _ parentPnt', _) -> executeConstraint env (UnionOf parentPnt' childrenPnts)
    TypeCheckResult notes (SType pub plb pdesc, childrenSchemes) -> do
      let (chUb, chLb) = (\(ub, lb) -> (ub, lb)) $ foldr (\(SType ub1 lb1 _) (ub2, lb2) -> (unionType ub1 ub2, unionType lb1 lb2)) (bottomType, bottomType) childrenSchemes
      case equalizeSTypes ((pub, plb, pdesc), (chUb, chLb, "")) "executeConstraint UnionOf" of
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
