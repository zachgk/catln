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
checkScheme msg (TypeCheckResult notes (SType ub _ desc)) | ub == bottomType = TypeCheckResE (GenTypeCheckError ("Scheme failed check at " ++ msg ++ ": upper bound is bottomType - " ++ show (S.toList desc)) : notes)
checkScheme _ scheme = scheme

setScheme :: FEnv -> Pnt -> Scheme -> String -> FEnv
setScheme env p scheme msg = setDescriptor env p (checkScheme msg scheme)

modifyScheme :: FEnv -> Pnt -> (Scheme -> Scheme) -> String -> FEnv
modifyScheme env p f msg = setDescriptor env p (checkScheme msg $ f $ descriptor env p)

equalizeBounds :: FEnv -> (Scheme, Scheme) -> String -> Scheme
equalizeBounds env inSchemes d = do
  inSchemes' <- sequenceT inSchemes
  case inSchemes' of
    (SVar _ p1, scheme2) -> do
      let scheme1 = descriptor env p1
      equalizeBounds env (scheme1, return scheme2) d
    (scheme1, SVar _ p2) -> do
      let scheme2 = descriptor env p2
      equalizeBounds env (return scheme1, scheme2) d
    (SType ub1 lb1 desc1, SType ub2 lb2 _) -> do
      let lbBoth = unionType lb1 lb2
      ubBoth <- tryIntersectTypes ub1 ub2 $ "equalizeBounds(" ++ d ++ ")"
      if hasType lbBoth ubBoth
        then return $ SType ubBoth lbBoth desc1
        else TypeCheckResE [GenTypeCheckError $ concat ["Type Mismatched: ", show lbBoth, " is not a subtype of ", show ubBoth]]

equalizeSchemes :: FEnv -> (Scheme, Scheme) -> String -> Scheme
equalizeSchemes env inSchemes d = do
  inSchemes' <- sequenceT inSchemes
  case inSchemes' of
    (SVar _ p1, scheme2) -> do
      let scheme1 = descriptor env p1
      equalizeSchemes env (scheme1, return scheme2) d
    (scheme1, SVar _ p2) -> do
      let scheme2 = descriptor env p2
      equalizeSchemes env (return scheme1, scheme2) d
    (SType ub1 lb1 desc1, SType ub2 lb2 desc2) -> do
      let lbBoth = unionType lb1 lb2
      ubBoth <- tryIntersectTypes ub1 ub2 $ "equalizeSchemes(" ++ d ++ ")"
      let descBoth = S.union desc1 desc2
      if hasType lbBoth ubBoth
        then return $ SType ubBoth lbBoth descBoth
        else TypeCheckResE [GenTypeCheckError $ concat ["Type Mismatched: ", show lbBoth, " is not a subtype of ", show ubBoth]]



getSchemeProp :: FEnv -> Scheme -> ArgName -> Scheme
getSchemeProp env inScheme propName = do
  inScheme' <- inScheme
  case inScheme' of
    (SVar _ p) -> do
      let inScheme'' = descriptor env p
      getSchemeProp env inScheme'' propName
    (SType ub lb desc) -> checkScheme (printf "getSchemeProp with prop %s from %s" propName (show inScheme)) $ return $ SType (getTypeProp ub ) (getTypeProp lb) desc
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
executeConstraint env (EqualsKnown pnt tp) = ([], True, env')
  where
    f oldScheme = equalizeSchemes env (oldScheme, return $ SType tp tp $ S.singleton "") "executeConstraint EqualsKnown"
    env' = modifyScheme env pnt f "EqualslKnown"
executeConstraint env1 cons@(EqPoints p1 p2) = ([cons | not (isSolved s')], s1 /= s' || s2 /= s', env3)
  where
    s1 = descriptor env1 p1
    s2 = descriptor env1 p2
    s' = equalizeSchemes env1 (s1, s2) "executeConstraint EqPoints"
    env2 = setScheme env1 p1 s' "EqPoints p1"
    env3 = setScheme env2 p2 s' "EqPoints p2"
executeConstraint env cons@(BoundedBy subPnt parentPnt) = do
  let subScheme = descriptor env subPnt
  let parentScheme = descriptor env parentPnt
  case sequenceT (subScheme, parentScheme) of
    TypeCheckResE _ -> ([], False, env)
    TypeCheckResult _ (SVar _ subPnt', _) -> executeConstraint env (BoundedBy subPnt' parentPnt)
    TypeCheckResult _ (_, SVar _ parentPnt') -> executeConstraint env (BoundedBy subPnt parentPnt')
    TypeCheckResult _ (SType ub1 lb1 description, SType ub2 _ _) -> do
      let subScheme' = fmap (\ub -> SType ub lb1 description) (tryIntersectTypes ub1 ub2 "executeConstraint BoundedBy")
      let env' = setScheme env subPnt subScheme' "BoundedBy"
      ([cons | not (isSolved subScheme')], subScheme /= subScheme', env')
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
            then TypeCheckResE [GenTypeCheckError $ printf "Failed to BoundByObjs for %s: \n\t%s \n\twith \n\t%s" (show $ S.toList desc) (show ub) (show objsUb)]
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
    TypeCheckResult{} -> do
      let superPropScheme = getSchemeProp env superScheme propName
      let scheme' = equalizeBounds env (subScheme, superPropScheme) $ printf "executeConstraint PropEq %s" propName
      let superScheme' = setSchemeProp env superScheme propName scheme'
      let env' = setScheme env subPnt scheme' "PropEq sub"
      let env'' = setScheme env' superPnt superScheme' "PropEq super"
      ([cons | not (isSolved scheme')], subScheme /= scheme' || superScheme /= superScheme', env'')
executeConstraint env cons@(AddArgs (srcPnt, newArgNames) destPnt) = do
  let srcScheme = descriptor env srcPnt
  let destScheme = descriptor env destPnt
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> ([], False, env)
    TypeCheckResult _ (SVar _ srcPnt', _) -> executeConstraint env (AddArgs (srcPnt', newArgNames) destPnt)
    TypeCheckResult _ (_, SVar _ destPnt') -> executeConstraint env (AddArgs (srcPnt, newArgNames) destPnt')
    TypeCheckResult _ (SType TopType _ _, _) -> ([cons], False, env)
    TypeCheckResult _ (SType srcUb _ _, SType _ destLb destDesc) ->
      case addArgsToType srcUb newArgNames of
        Just destUb' -> do
          let destScheme' = equalizeSchemes env (destScheme, return $ SType destUb' destLb destDesc) $ printf "executeConstraint AddArgs %s" (show $ S.toList newArgNames)
          let env' = setScheme env destPnt destScheme' "AddArgs"
          ([cons | not (isSolved srcScheme || isSolved destScheme)], destScheme /= destScheme', env')
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
    TypeCheckResult _ (_, childrenSchemes) -> do
      let childrenScheme = (\(ub, lb) -> return $ SType ub lb (S.singleton "")) $ foldr (\(SType ub1 lb1 _) (ub2, lb2) -> (unionType ub1 ub2, unionType lb1 lb2)) (bottomType, bottomType) childrenSchemes
      let parentScheme' = equalizeBounds env (parentScheme, childrenScheme) "executeConstraint UnionOf"
      let env' = setScheme env parentPnt parentScheme' "UnionOf"
      ([cons | not (isSolved parentScheme')], parentScheme /= parentScheme', env')

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
