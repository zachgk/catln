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
import           TypeCheck.TypeGraph (reaches)
import           Data.UnionFind.ST

isSolved :: Scheme -> Bool
isSolved (Right (SType a b _)) = a == b
isSolved _ = False

tryIntersectRawTypes :: RawType -> RawType -> String -> TypeCheckResult RawType
tryIntersectRawTypes a b desc= let c = intersectRawTypes a b
                            in if c == rawBottomType
                                  then Left [GenTypeCheckError $ "Failed to intersect(" ++ desc ++ "): " ++ show a ++ " --- " ++ show b]
                                  else Right c


equalizeSchemes :: (Scheme, Scheme) -> String -> Scheme
equalizeSchemes (_, Left s) _ = Left s
equalizeSchemes (Left s, _) _ = Left s
equalizeSchemes (Right (SType ub1 lb1 desc1), Right (SType ub2 lb2 desc2)) d = let lbBoth = unionRawTypes lb1 lb2
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
setSchemeProp (Right (SType ub lb desc)) propName (Right (SType pub _ _)) = Right $ SType (compactRawType $ setRawTypeUbProp ub) (compactRawType $ setRawTypeLbProp lb) desc
  where
    setRawTypeUbProp :: RawType -> RawType
    setRawTypeUbProp RawTopType = RawTopType
    setRawTypeUbProp (RawSumType ubLeafs ubPartials) = RawSumType (S.fromList $ mapMaybe (setLeafUbProp ubLeafs) $ S.toList ubLeafs) (H.mapMaybe setPartialsUb ubPartials)
    setLeafUbProp ubLeafs (RawLeafType leafName leafArgs) = case H.lookup propName leafArgs of
      Just leafArg -> if S.member leafArg ubLeafs
        then Just $ RawLeafType leafName (H.insert propName leafArg leafArgs)
        else Nothing
      Nothing -> Nothing
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

executeConstraint :: TypeGraph s -> Constraint s -> ST s [Constraint s]
executeConstraint _ (EqualsKnown pnt tp) = modifyDescriptor pnt (\oldScheme -> equalizeSchemes (oldScheme, Right $ SType tp tp "") "executeConstraint EqualsKnown") >> return []
executeConstraint _ (EqPoints p1 p2) = union' p1 p2 (\s1 s2 -> return (equalizeSchemes (s1, s2) "executeConstraint EqPoints")) >> return []
executeConstraint _ cons@(BoundedBy subPnt parentPnt) = do
  subScheme <- descriptor subPnt
  parentScheme <- descriptor parentPnt
  case (subScheme, parentScheme) of
    (_, Left _) -> return []
    (Left _, _) -> return []
    (Right (SType ub1 lb1 description), Right (SType ub2 _ _)) -> do
      let subScheme' = fmap (\ub -> SType ub lb1 description) (tryIntersectRawTypes ub1 ub2 "executeConstraint BoundedBy")
      setDescriptor subPnt subScheme'
      return [cons | not (isSolved subScheme')]
executeConstraint typeGraph cons@(ArrowTo srcPnt destPnt) = do
  srcScheme <- descriptor srcPnt
  destScheme <- descriptor destPnt
  case (srcScheme, destScheme) of
    (Left _, _) -> return []
    (_, Left _) -> return []
    (Right (SType srcUb _ _), Right (SType destUb destLb destDescription)) -> do
      maybeDestUbByGraph <- reaches typeGraph srcUb
      case maybeDestUbByGraph of
        Just destUbByGraph -> do
          let destScheme' = case tryIntersectRawTypes destUb destUbByGraph "executeConstraint ArrowTo" of
                Right destUb' -> let destLb' = lowerUb destUb' destLb
                                  in Right $ SType destUb' destLb' destDescription
                Left errors -> Left errors
          setDescriptor destPnt destScheme'
          return [cons | not (isSolved destScheme')]
        Nothing -> return [] -- remove constraint if found Left
executeConstraint _ cons@(PropEq (superPnt, propName) subPnt) = do
  superScheme <- descriptor superPnt
  subScheme <- descriptor subPnt
  case (superScheme, subScheme) of
    (Left _, _) -> return []
    (_, Left _) -> return []
    (Right{}, Right{}) -> do
      let superPropScheme = getSchemeProp superScheme propName
      let scheme' = equalizeSchemes (superPropScheme, subScheme) "executeConstraint PropEq"
      let superScheme' = setSchemeProp superScheme propName scheme'
      setDescriptor subPnt scheme'
      setDescriptor superPnt superScheme'
      return [cons | not (isSolved scheme')]
executeConstraint _ cons@(AddArgs (srcPnt, newArgNames) destPnt) = do
  srcScheme <- descriptor srcPnt
  destScheme <- descriptor destPnt
  case (srcScheme, destScheme) of
    (Left _, _) -> return []
    (_, Left _) -> return []
    (Right (SType srcUb _ _), Right (SType _ destLb destDesc)) ->
      case addArgsToRawType srcUb newArgNames of
        Just destUb' -> do
          setDescriptor destPnt $ equalizeSchemes (Right $ SType destUb' destLb destDesc, destScheme) "executeConstraint AddArgs"
          return []
        Nothing -> return [cons]

abandonConstraints :: Constraint s -> ST s TypeCheckError
abandonConstraints con = do
  scon <- showCon con
  return $ AbandonCon scon

runConstraints :: TypeGraph s -> [Constraint s] -> ST s (Either [TypeCheckError] ())
runConstraints _ [] = return $ Right ()
runConstraints typeGraph cons = do
  res <- mapM (executeConstraint typeGraph) cons
  let cons' = concat res
  if cons == cons'
    then do
      constraintErrors <- mapM abandonConstraints cons
      return $ Left constraintErrors
    else runConstraints typeGraph cons'
