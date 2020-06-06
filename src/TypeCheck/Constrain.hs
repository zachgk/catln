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
import           Control.Monad.ST

import           Syntax.Types
import           TypeCheck.Common
import           TypeCheck.TypeGraph
import           TypeCheck.Show
import           Data.UnionFind.ST
import           Data.Tuple.Sequence
import           Text.Printf

isSolved :: Scheme -> Bool
isSolved (TypeCheckResult _ (SType a b _)) = a == b
isSolved _ = False

checkScheme :: String -> Scheme -> Scheme
checkScheme msg (TypeCheckResult notes (SType ub _ desc)) | ub == bottomType = TypeCheckResE (GenTypeCheckError ("Scheme failed check at " ++ msg ++ ": upper bound is bottomType - " ++ desc) : notes)
checkScheme _ scheme = scheme

equalizeBounds :: (Scheme, Scheme) -> String -> Scheme
equalizeBounds inSchemes d = do
  (SType ub1 lb1 desc1, SType ub2 lb2 _) <- sequenceT inSchemes
  let lbBoth = unionType lb1 lb2
  ubBoth <- tryIntersectTypes ub1 ub2 $ "equalizeBounds(" ++ d ++ ")"
  if hasType lbBoth ubBoth
    then return $ SType ubBoth lbBoth desc1
    else TypeCheckResE [GenTypeCheckError $ concat ["Type Mismatched: ", show lbBoth, " is not a subtype of ", show ubBoth]]

equalizeSchemes :: (Scheme, Scheme) -> String -> Scheme
equalizeSchemes inSchemes d = do
  (SType ub1 lb1 desc1, SType ub2 lb2 desc2) <- sequenceT inSchemes
  let lbBoth = unionType lb1 lb2
  ubBoth <- tryIntersectTypes ub1 ub2 $ "equalizeSchemes(" ++ d ++ ")"
  let descBoth = if desc1 == desc2
         then desc1
         else "(" ++ desc1 ++ "," ++ desc2 ++ ")"
  if hasType lbBoth ubBoth
    then return $ SType ubBoth lbBoth descBoth
    else TypeCheckResE [GenTypeCheckError $ concat ["Type Mismatched: ", show lbBoth, " is not a subtype of ", show ubBoth]]



getSchemeProp :: Scheme -> ArgName -> Scheme
getSchemeProp inScheme propName = do
  (SType ub lb desc) <- inScheme
  return $ SType (getTypeProp ub ) (getTypeProp lb) desc
  where
    getTypeProp :: Type -> Type
    getTypeProp TopType = TopType
    getTypeProp (SumType partials) = case getPartials partials of
      TopType -> TopType
      (SumType partials') -> SumType partials'
    getPartials :: PartialLeafs -> Type
    getPartials partials = unionTypes $ mapMaybe (H.lookup propName . snd) $ concatMap S.toList $ H.elems partials

setSchemeProp :: Scheme -> ArgName -> Scheme -> Scheme
setSchemeProp scheme propName pscheme = do
  (SType ub lb desc) <- scheme
  (SType pub _ _) <- pscheme
  checkScheme ("setSchemeProp " ++ propName) $ return $ SType (compactType $ setTypeUbProp ub pub) (compactType $ setTypeLbProp lb) desc
  where
    setTypeUbProp :: Type -> Type -> Type
    setTypeUbProp TopType _ = TopType
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
addArgsToType (SumType partials) newArgs = Just $ SumType partials'
  where
    partialUpdate = H.fromList $ map (,TopType) $ S.toList newArgs
    partials' = joinPartialLeafs $ map fromPartial $ splitPartialLeafs partials
    fromPartial (partialName, partialVars, partialArgs) = (partialName, partialVars, H.unionWith unionType partialArgs partialUpdate)

-- returns updated (pruned) constraints and boolean if schemes were updated
executeConstraint :: TypeEnv s -> Constraint s -> ST s ([Constraint s], Bool)
executeConstraint _ (EqualsKnown pnt tp) = modifyDescriptor pnt (\oldScheme -> equalizeSchemes (oldScheme, return $ SType tp tp "") "executeConstraint EqualsKnown") >> return ([], True)
executeConstraint _ (EqPoints p1 p2) = union' p1 p2 (\s1 s2 -> return (equalizeSchemes (s1, s2) "executeConstraint EqPoints")) >> return ([], True)
executeConstraint _ cons@(BoundedBy subPnt parentPnt) = do
  subScheme <- descriptor subPnt
  parentScheme <- descriptor parentPnt
  case sequenceT (subScheme, parentScheme) of
    TypeCheckResE _ -> return ([], False)
    TypeCheckResult _ (SType ub1 lb1 description, SType ub2 _ _) -> do
      let subScheme' = fmap (\ub -> SType ub lb1 description) (tryIntersectTypes ub1 ub2 "executeConstraint BoundedBy")
      setDescriptor subPnt subScheme'
      return ([cons | not (isSolved subScheme')], subScheme /= subScheme')
executeConstraint _ (BoundedByKnown subPnt boundTp) = do
  subScheme <- descriptor subPnt
  case subScheme of
    TypeCheckResE _ -> return ([], False)
    TypeCheckResult _ (SType ub lb description) -> do
      let subScheme' = fmap (\ub' -> SType ub' lb description) (tryIntersectTypes ub boundTp "executeConstraint BoundedBy")
      setDescriptor subPnt subScheme'
      return ([], subScheme /= subScheme')
executeConstraint ((unionAllObjs, unionTypeObjs), _) cons@(BoundedByObjs bnd pnt) = do
  scheme <- descriptor pnt
  let unionPnt = case bnd of
        BoundAllObjs -> unionAllObjs
        BoundTypeObjs -> unionTypeObjs
  unionScheme <- descriptor unionPnt
  case sequenceT (scheme, unionScheme) of
    TypeCheckResE _ -> return ([], False)
    TypeCheckResult _ (SType ub lb desc, SType objsUb _ _) -> do
      -- A partially applied tuple would not be a raw type on the unionObj,
      -- but a subset of the arguments in that type
      let ub' = intersectTypes ub objsUb
      let scheme' = if ub' == bottomType
            then TypeCheckResE [GenTypeCheckError $ printf "Failed to BoundByObjs for %s: %s %s" desc (show ub) (show objsUb)]
            else return $ SType ub' lb desc
      setDescriptor pnt scheme'
      return ([cons | not (isSolved scheme')], scheme /= scheme')
executeConstraint typeEnv cons@(ArrowTo srcPnt destPnt) = do
  srcScheme <- descriptor srcPnt
  destScheme <- descriptor destPnt
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> return ([], False)
    TypeCheckResult _ (SType srcUb srcLb srcDesc, SType destUb destLb destDesc) -> do
      constrained <- arrowConstrainUbs typeEnv srcUb destUb
      case constrained of
        TypeCheckResult _ (srcUb', destUb') -> do
          let srcScheme' = return $ SType srcUb' srcLb srcDesc
          let destScheme' = return $ SType destUb' destLb destDesc
          setDescriptor srcPnt srcScheme'
          setDescriptor destPnt destScheme'
          return ([cons | not (isSolved destScheme')], srcScheme /= srcScheme' || destScheme /= destScheme')
        TypeCheckResE _ -> return ([], False)
executeConstraint _ cons@(PropEq (superPnt, propName) subPnt) = do
  superScheme <- descriptor superPnt
  subScheme <- descriptor subPnt
  case sequenceT (superScheme, subScheme) of
    TypeCheckResE _ -> return ([], False)
    TypeCheckResult{} -> do
      let superPropScheme = getSchemeProp superScheme propName
      let scheme' = equalizeBounds (subScheme, superPropScheme) "executeConstraint PropEq"
      let superScheme' = setSchemeProp superScheme propName scheme'
      setDescriptor subPnt scheme'
      setDescriptor superPnt superScheme'
      return ([cons | not (isSolved scheme')], subScheme /= scheme' || superScheme /= superScheme')
executeConstraint _ cons@(AddArgs (srcPnt, newArgNames) destPnt) = do
  srcScheme <- descriptor srcPnt
  destScheme <- descriptor destPnt
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> return ([], False)
    TypeCheckResult _ (SType TopType _ _, _) -> return ([cons], False)
    TypeCheckResult _ (SType srcUb _ _, SType _ destLb destDesc) ->
      case addArgsToType srcUb newArgNames of
        Just destUb' -> do
          let destScheme' = equalizeSchemes (destScheme, return $ SType destUb' destLb destDesc) "executeConstraint AddArgs"
          setDescriptor destPnt destScheme'
          return ([cons | not (isSolved srcScheme || isSolved destScheme)], destScheme /= destScheme')
        Nothing -> return ([cons], False)
executeConstraint _ cons@(PowersetTo srcPnt destPnt) = do
  srcScheme <- descriptor srcPnt
  destScheme <- descriptor destPnt
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> return ([], False)
    TypeCheckResult _ (SType ub1 _ _, SType ub2 lb2 description2) -> do
      let destScheme' = fmap (\ub -> SType ub lb2 description2) (tryIntersectTypes (powersetType ub1) ub2 "executeConstraint PowersetTo")
      setDescriptor destPnt destScheme'
      return ([cons | not (isSolved destScheme')], destScheme /= destScheme')
executeConstraint _ cons@(UnionOf parentPnt childrenPnts) = do
  parentScheme <- descriptor parentPnt
  tcresChildrenSchemes <- mapM descriptor childrenPnts
  case sequenceT (parentScheme, sequence tcresChildrenSchemes) of
    TypeCheckResE _ -> return ([], False)
    TypeCheckResult _ (_, childrenSchemes) -> do
      let childrenScheme = (\(ub, lb) -> return $ SType ub lb "") $ foldr (\(SType ub1 lb1 _) (ub2, lb2) -> (unionType ub1 ub2, unionType lb1 lb2)) (bottomType, bottomType) childrenSchemes
      let parentScheme' = equalizeBounds (parentScheme, childrenScheme) "executeConstraint UnionOf"
      setDescriptor parentPnt parentScheme'
      return ([cons | not (isSolved parentScheme')], parentScheme /= parentScheme')

runConstraints :: Integer -> TypeEnv s -> [Constraint s] -> ST s (Either [TypeCheckError] ())
runConstraints _ _ [] = return $ return ()
runConstraints 0 typeEnv cons = do
  res <- mapM (executeConstraint typeEnv) cons
  let consChangedList = mapMaybe (\(con, isChanged) -> if isChanged then Just con else Nothing) res
  showCons <- showConstraints $ concat consChangedList
  return $ Left [GenTypeCheckError $ "Reached runConstraints limit with still changing constraints: " ++ show showCons]
runConstraints limit typeEnv cons = do
  res <- mapM (executeConstraint typeEnv) cons
  let (consList, changedList) = unzip res
  let cons' = concat consList
  if not (or changedList)
    then return $ return ()
    else runConstraints (limit - 1) typeEnv cons'
