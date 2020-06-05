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
import           Data.UnionFind.ST
import           Data.Tuple.Sequence
import           Text.Printf

isSolved :: Scheme -> Bool
isSolved (TypeCheckResult _ (SType a b _)) = a == b
isSolved _ = False

checkScheme :: String -> Scheme -> Scheme
checkScheme msg (TypeCheckResult notes (SType ub _ desc)) | ub == rawBottomType = TypeCheckResE (GenTypeCheckError ("Scheme failed check at " ++ msg ++ ": upper bound is rawBottomType - " ++ desc) : notes)
checkScheme _ scheme = scheme

equalizeBounds :: (Scheme, Scheme) -> String -> Scheme
equalizeBounds inSchemes d = do
  (SType ub1 lb1 desc1, SType ub2 lb2 _) <- sequenceT inSchemes
  let lbBoth = unionRawTypes lb1 lb2
  ubBoth <- tryIntersectRawTypes ub1 ub2 $ "equalizeBounds(" ++ d ++ ")"
  if hasRawType lbBoth ubBoth
    then return $ SType ubBoth lbBoth desc1
    else TypeCheckResE [GenTypeCheckError $ concat ["Type Mismatched: ", show lbBoth, " is not a subtype of ", show ubBoth]]

equalizeSchemes :: (Scheme, Scheme) -> String -> Scheme
equalizeSchemes inSchemes d = do
  (SType ub1 lb1 desc1, SType ub2 lb2 desc2) <- sequenceT inSchemes
  let lbBoth = unionRawTypes lb1 lb2
  ubBoth <- tryIntersectRawTypes ub1 ub2 $ "equalizeSchemes(" ++ d ++ ")"
  let descBoth = if desc1 == desc2
         then desc1
         else "(" ++ desc1 ++ "," ++ desc2 ++ ")"
  if hasRawType lbBoth ubBoth
    then return $ SType ubBoth lbBoth descBoth
    else TypeCheckResE [GenTypeCheckError $ concat ["Type Mismatched: ", show lbBoth, " is not a subtype of ", show ubBoth]]



getSchemeProp :: Scheme -> Name -> Scheme
getSchemeProp inScheme propName = do
  (SType ub lb desc) <- inScheme
  return $ SType (getRawTypeProp ub ) (getRawTypeProp lb) desc
  where
    getRawTypeProp :: RawType -> RawType
    getRawTypeProp RawTopType = RawTopType
    getRawTypeProp (RawSumType leafs partials) = case getPartials partials of
      RawTopType -> RawTopType
      (RawSumType partialLeafs partials') -> RawSumType (S.union partialLeafs $ S.fromList $ mapMaybe getLeafProp $ S.toList leafs) partials'
    getLeafProp :: RawLeafType -> Maybe RawLeafType
    getLeafProp (RawLeafType _ leafArgs) = H.lookup propName leafArgs
    getPartials :: RawPartialLeafs -> RawType
    getPartials partials = unionRawTypesList $ mapMaybe (H.lookup propName) $ concatMap S.toList (H.elems partials)

setSchemeProp :: Scheme -> Name -> Scheme -> Scheme
setSchemeProp scheme propName pscheme = do
  (SType ub lb desc) <- scheme
  (SType pub _ _) <- pscheme
  checkScheme ("setSchemeProp " ++ propName) $ return $ SType (compactRawType $ setRawTypeUbProp ub pub) (compactRawType $ setRawTypeLbProp lb) desc
  where
    setRawTypeUbProp :: RawType -> RawType -> RawType
    setRawTypeUbProp RawTopType _ = RawTopType
    setRawTypeUbProp (RawSumType ubLeafs ubPartials) pub = RawSumType (S.fromList $ mapMaybe (setLeafUbProp pub) $ S.toList ubLeafs) (joinPartialLeafs $ mapMaybe (setPartialsUb pub) $ splitPartialLeafs ubPartials)
    setLeafUbProp pub ubLeaf@(RawLeafType _ leafArgs) = case (H.lookup propName leafArgs, pub) of
      (Nothing, _) -> Nothing
      (Just leafArg, RawSumType pubLeafs _) -> if S.member leafArg pubLeafs
        then Just ubLeaf
        else Nothing
      (Just{} , RawTopType) -> Just ubLeaf
    setPartialsUb RawTopType partial = Just partial
    setPartialsUb pub (partialName, partialArgs) = case H.lookup propName partialArgs of
      Just partialArg -> let partialArg' = intersectRawTypes partialArg pub
                          in if partialArg' == rawBottomType
                                then Nothing
                                else Just (partialName, H.insert propName partialArg' partialArgs)
      Nothing -> Nothing
    setRawTypeLbProp tp = tp -- TODO: Should set with union?

addArgsToRawType :: RawType -> S.HashSet Name -> Maybe RawType
addArgsToRawType RawTopType _ = Nothing
addArgsToRawType (RawSumType leafs partials) newArgs = Just $ RawSumType S.empty (H.unionWith S.union partialsFromLeafs partialsFromPartials)
  where
    partialUpdate = H.fromList $ map (,RawTopType) $ S.toList newArgs
    partialsFromLeafs = foldr (H.unionWith S.union . partialFromLeaf) H.empty $ S.toList leafs
    partialFromLeaf (RawLeafType leafName leafArgs) = H.singleton leafName (S.singleton (H.unionWith unionRawTypes partialUpdate $ fmap (\leafArg -> RawSumType (S.singleton leafArg) H.empty) leafArgs))
    partialsFromPartials = joinPartialLeafs $ map fromPartial $ splitPartialLeafs partials
    fromPartial (partialName, partialArgs) = (partialName, H.unionWith unionRawTypes partialArgs partialUpdate)

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
      let subScheme' = fmap (\ub -> SType ub lb1 description) (tryIntersectRawTypes ub1 ub2 "executeConstraint BoundedBy")
      setDescriptor subPnt subScheme'
      return ([cons | not (isSolved subScheme')], subScheme /= subScheme')
executeConstraint _ (BoundedByKnown subPnt boundTp) = do
  subScheme <- descriptor subPnt
  case subScheme of
    TypeCheckResE _ -> return ([], False)
    TypeCheckResult _ (SType ub lb description) -> do
      let subScheme' = fmap (\ub' -> SType ub' lb description) (tryIntersectRawTypes ub boundTp "executeConstraint BoundedBy")
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
      let ub' = intersectRawTypes ub objsUb
      let scheme' = if ub' == rawBottomType
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
    TypeCheckResult _ (SType RawTopType _ _, _) -> return ([cons], False)
    TypeCheckResult _ (SType srcUb _ _, SType _ destLb destDesc) ->
      case addArgsToRawType srcUb newArgNames of
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
      let destScheme' = fmap (\ub -> SType ub lb2 description2) (tryIntersectRawTypes (powersetRawType ub1) ub2 "executeConstraint PowersetTo")
      setDescriptor destPnt destScheme'
      return ([cons | not (isSolved destScheme')], destScheme /= destScheme')
executeConstraint _ cons@(UnionOf parentPnt childrenPnts) = do
  parentScheme <- descriptor parentPnt
  tcresChildrenSchemes <- mapM descriptor childrenPnts
  case sequenceT (parentScheme, sequence tcresChildrenSchemes) of
    TypeCheckResE _ -> return ([], False)
    TypeCheckResult _ (_, childrenSchemes) -> do
      let childrenScheme = (\(ub, lb) -> return $ SType ub lb "") $ foldr (\(SType ub1 lb1 _) (ub2, lb2) -> (unionRawTypes ub1 ub2, unionRawTypes lb1 lb2)) (rawBottomType, rawBottomType) childrenSchemes
      let parentScheme' = equalizeBounds (parentScheme, childrenScheme) "executeConstraint UnionOf"
      setDescriptor parentPnt parentScheme'
      return ([cons | not (isSolved parentScheme')], parentScheme /= parentScheme')

runConstraints :: Integer -> TypeEnv s -> [Constraint s] -> ST s (Either [TypeCheckError] ())
runConstraints _ _ [] = return $ return ()
runConstraints 0 _ _ = return $ Left [GenTypeCheckError "Reached runConstraints limit"]
runConstraints limit typeEnv cons = do
  res <- mapM (executeConstraint typeEnv) cons
  let (consList, changedList) = unzip res
  let cons' = concat consList
  if not (or changedList)
    then return $ return ()
    else runConstraints (limit - 1) typeEnv cons'
