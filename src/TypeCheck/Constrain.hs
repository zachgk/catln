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

import           Syntax
import           TypeCheck.Common
import           TypeCheck.Show (showCon)
import           TypeCheck.TypeGraph (reaches, boundSchemeByGraphObjects)
import           Data.UnionFind.ST

isSolved :: Scheme -> Bool
isSolved (Right (SType a b _)) = a == b
isSolved _ = False

tryIntersectRawTypes :: RawType -> RawType -> String -> TypeCheckResult RawType
tryIntersectRawTypes a b desc= let c = intersectRawTypes a b
                            in if c == rawBottomType
                                  then Left [GenTypeCheckError $ "Failed to intersect(" ++ desc ++ "): " ++ show a ++ " --- " ++ show b]
                                  else Right c


checkScheme :: String -> Scheme -> Scheme
checkScheme msg (Right (SType ub _ desc)) | ub == rawBottomType = Left [GenTypeCheckError $ "Scheme failed check at " ++ msg ++ ": upper bound is rawBottomType - " ++ desc]
checkScheme _ scheme = scheme

equalizeBounds :: (Scheme, Scheme) -> String -> Scheme
equalizeBounds (_, Left s) _ = Left s
equalizeBounds (Left s, _) _ = Left s
equalizeBounds (Right (SType ub1 lb1 desc1), Right (SType ub2 lb2 _)) d =
  let lbBoth = unionRawTypes lb1 lb2
      tryUbBoth = tryIntersectRawTypes ub1 ub2 $ "equalizeSchemes(" ++ d ++ ")"
   in case tryUbBoth of
           Right ubBoth -> if hasRawType lbBoth ubBoth
             then return $ SType ubBoth lbBoth desc1
             else Left [GenTypeCheckError $ concat ["Type Mismatched: ", show lbBoth, " is not a subtype of ", show ubBoth]]
           Left errors -> Left errors

equalizeSchemes :: (Scheme, Scheme) -> String -> Scheme
equalizeSchemes (_, Left s) _ = Left s
equalizeSchemes (Left s, _) _ = Left s
equalizeSchemes (Right (SType ub1 lb1 desc1), Right (SType ub2 lb2 desc2)) d =
  let lbBoth = unionRawTypes lb1 lb2
      tryUbBoth = tryIntersectRawTypes ub1 ub2 $ "equalizeSchemes(" ++ d ++ ")"
      descBoth = if desc1 == desc2
         then desc1
         else "(" ++ desc1 ++ "," ++ desc2 ++ ")"
   in case tryUbBoth of
           Right ubBoth -> if hasRawType lbBoth ubBoth
             then return $ SType ubBoth lbBoth descBoth
             else Left [GenTypeCheckError $ concat ["Type Mismatched: ", show lbBoth, " is not a subtype of ", show ubBoth]]
           Left errors -> Left errors


lowerUb :: RawType -> RawType -> RawType
lowerUb ub@(RawSumType ubLeafs ubPartials) lb | S.size ubLeafs == 1 && H.null ubPartials = unionRawTypes ub lb
lowerUb _ lb = lb


getSchemeProp :: Scheme -> Name -> Scheme
getSchemeProp Left{} _ = error "get prop of Left"
getSchemeProp (Right (SType ub lb desc)) propName = Right $ SType (getRawTypeProp ub ) (getRawTypeProp lb) desc
  where
    getRawTypeProp :: RawType -> RawType
    getRawTypeProp RawTopType = RawTopType
    getRawTypeProp (RawSumType leafs partials) = case getPartials partials of
      RawTopType -> RawTopType
      (RawSumType partialLeafs partials') -> RawSumType (S.union partialLeafs $ S.fromList $ mapMaybe getLeafProp $ S.toList leafs) partials'
    getLeafProp :: RawLeafType -> Maybe RawLeafType
    getLeafProp (RawLeafType _ leafArgs) = H.lookup propName leafArgs
    getPartials :: RawPartialLeafs -> RawType
    getPartials partials = joinPartials $ mapMaybe (H.lookup propName) $ concat $ H.elems partials
    joinPartials :: [RawType] -> RawType
    joinPartials = foldr unionRawTypes rawBottomType

setSchemeProp :: Scheme -> Name -> Scheme -> Scheme
setSchemeProp err@Left{} _ _ = err
setSchemeProp _ _ err@Left{} = err
setSchemeProp (Right (SType ub lb desc)) propName (Right (SType pub _ _)) = checkScheme ("setSchemeProp " ++ propName) $ Right $ SType (compactRawType $ setRawTypeUbProp ub) (compactRawType $ setRawTypeLbProp lb) desc
  where
    setRawTypeUbProp :: RawType -> RawType
    setRawTypeUbProp RawTopType = RawTopType
    setRawTypeUbProp (RawSumType ubLeafs ubPartials) = RawSumType (S.fromList $ mapMaybe setLeafUbProp $ S.toList ubLeafs) (H.mapMaybe setPartialsUb ubPartials)
    setLeafUbProp  ubLeaf@(RawLeafType _ leafArgs) = case (H.lookup propName leafArgs, pub) of
      (Nothing, _) -> Nothing
      (Just leafArg, RawSumType pubLeafs _) -> if S.member leafArg pubLeafs
        then Just ubLeaf
        else Nothing
      (Just{} , RawTopType) -> Just ubLeaf
    setPartialsUb partials = case mapMaybe setPartialUb partials of
      [] -> Nothing
      partials' -> Just partials'
    setPartialUb partialArgs = case H.lookup propName partialArgs of
      Just partialArg -> let tryPartialArg' = tryIntersectRawTypes partialArg pub "setSchemeProp"
                          in case tryPartialArg' of
                               Right partialArg' -> if partialArg' == rawBottomType
                                                      then Nothing
                                                      else Just $ H.insert propName partialArg' partialArgs
                               Left _ -> Nothing
      Nothing -> Nothing
    setRawTypeLbProp tp = tp -- TODO: Should set with union?

addArgsToRawType :: RawType -> S.HashSet Name -> Maybe RawType
addArgsToRawType RawTopType _ = Nothing
addArgsToRawType (RawSumType leafs partials) newArgs = Just $ RawSumType S.empty (H.unionWith (++) partialsFromLeafs partialsFromPartials)
  where
    partialUpdate = H.fromList $ map (,RawTopType) $ S.toList newArgs
    partialsFromLeafs = foldr (H.unionWith (++) . partialFromLeaf) H.empty $ S.toList leafs
    partialFromLeaf (RawLeafType leafName leafArgs) = H.singleton leafName [H.union partialUpdate $ fmap (\leafArg -> RawSumType (S.singleton leafArg) H.empty) leafArgs]
    partialsFromPartials = fmap (map fromPartial) partials
    fromPartial = H.union partialUpdate

-- returns updated (pruned) constraints and boolean if schemes were updated
executeConstraint :: TypeGraph s -> Constraint s -> ST s ([Constraint s], Bool)
executeConstraint _ (EqualsKnown pnt tp) = modifyDescriptor pnt (\oldScheme -> equalizeSchemes (oldScheme, Right $ SType tp tp "") "executeConstraint EqualsKnown") >> return ([], True)
executeConstraint _ (EqPoints p1 p2) = union' p1 p2 (\s1 s2 -> return (equalizeSchemes (s1, s2) "executeConstraint EqPoints")) >> return ([], True)
executeConstraint _ cons@(BoundedBy subPnt parentPnt) = do
  subScheme <- descriptor subPnt
  parentScheme <- descriptor parentPnt
  case (subScheme, parentScheme) of
    (_, Left _) -> return ([], False)
    (Left _, _) -> return ([], False)
    (Right (SType ub1 lb1 description), Right (SType ub2 _ _)) -> do
      let subScheme' = fmap (\ub -> SType ub lb1 description) (tryIntersectRawTypes ub1 ub2 "executeConstraint BoundedBy")
      setDescriptor subPnt subScheme'
      return ([cons | not (isSolved subScheme')], subScheme /= subScheme')
executeConstraint typeGraph cons@(ArrowTo srcPnt destPnt) = do
  srcScheme <- descriptor srcPnt
  destScheme <- descriptor destPnt
  case (srcScheme, destScheme) of
    (Left _, _) -> return ([], False)
    (_, Left _) -> return ([], False)
    (Right (SType srcUb _ _), Right (SType destUb destLb destDescription)) -> do
      maybeDestUbByGraph <- reaches typeGraph srcUb
      case maybeDestUbByGraph of
        Just destUbByGraph -> do
          let destScheme' = case tryIntersectRawTypes destUb destUbByGraph "executeConstraint ArrowTo" of
                Right destUb' -> let destLb' = lowerUb destUb' destLb
                                  in Right $ SType destUb' destLb' destDescription
                Left errors -> Left errors
          setDescriptor destPnt destScheme'
          return ([cons | not (isSolved destScheme')], destScheme /= destScheme')
        Nothing -> return ([], False) -- remove constraint if found Left
executeConstraint typeGraph cons@(PropEq (superPnt, propName) subPnt) = do
  superScheme <- descriptor superPnt
  subScheme <- descriptor subPnt
  case (superScheme, subScheme) of
    (Left _, _) -> return ([], False)
    (_, Left _) -> return ([], False)
    (Right{}, Right{}) -> do
      let superPropScheme = getSchemeProp superScheme propName
      let scheme' = equalizeBounds (subScheme, superPropScheme) "executeConstraint PropEq"
      superSchemeBound <- boundSchemeByGraphObjects typeGraph $ setSchemeProp superScheme propName scheme'
      let superScheme' = checkScheme "PropEq superScheme'" superSchemeBound
      setDescriptor subPnt scheme'
      setDescriptor superPnt superScheme'
      return ([cons | not (isSolved scheme')], subScheme /= scheme' || superScheme /= superScheme')
executeConstraint _ cons@(AddArgs (srcPnt, newArgNames) destPnt) = do
  srcScheme <- descriptor srcPnt
  destScheme <- descriptor destPnt
  case (srcScheme, destScheme) of
    (Left _, _) -> return ([], False)
    (_, Left _) -> return ([], False)
    (Right (SType srcUb _ _), Right (SType _ destLb destDesc)) ->
      case addArgsToRawType srcUb newArgNames of
        Just destUb' -> do
          let destScheme' = equalizeSchemes (destScheme, Right $ SType destUb' destLb destDesc) "executeConstraint AddArgs"
          setDescriptor destPnt destScheme'
          return ([], True)
        Nothing -> return ([cons], False)
executeConstraint _ cons@(UnionOf parentPnt childrenPnts) = do
  parentScheme <- descriptor parentPnt
  tcresChildrenSchemes <- mapM descriptor childrenPnts
  case (parentScheme, sequence tcresChildrenSchemes) of
    (Left _, _) -> return ([], False)
    (_, Left _) -> return ([], False)
    (Right{}, Right childrenSchemes) -> do
      let childrenScheme = (\(ub, lb) -> Right $ SType ub lb "") $ foldr (\(SType ub1 lb1 _) (ub2, lb2) -> (unionRawTypes ub1 ub2, unionRawTypes lb1 lb2)) (rawBottomType, rawBottomType) childrenSchemes
      let parentScheme' = equalizeBounds (parentScheme, childrenScheme) "executeConstraint UnionOf"
      setDescriptor parentPnt parentScheme'
      return ([cons | not (isSolved parentScheme')], parentScheme /= parentScheme')

abandonConstraints :: Constraint s -> ST s TypeCheckError
abandonConstraints con = do
  scon <- showCon con
  return $ AbandonCon scon

runConstraints :: Integer -> TypeGraph s -> [Constraint s] -> ST s (Either [TypeCheckError] ())
runConstraints _ _ [] = return $ Right ()
runConstraints 0 _ _ = return $ Left [GenTypeCheckError "Reached runConstraints limit"]
runConstraints limit typeGraph cons = do
  res <- mapM (executeConstraint typeGraph) cons
  let (consList, changedList) = unzip res
  let cons' = concat consList
  if not (or changedList)
    then do
      constraintErrors <- mapM abandonConstraints cons
      return $ Left constraintErrors
    else runConstraints (limit - 1) typeGraph cons'
