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
isSolved (SType a b _) = a == b
isSolved _ = False

equalizeSchemes :: (Scheme, Scheme) -> Scheme
equalizeSchemes (_, SCheckError s) = SCheckError s
equalizeSchemes (SCheckError s, _) = SCheckError s
equalizeSchemes (SType ub1 lb1 desc1, SType ub2 lb2 desc2) = let lbBoth = unionRawTypes lb1 lb2
                                                                 ubBoth = intersectRawTypes ub1 ub2
                                                                 descBoth = if desc1 == desc2
                                                                   then desc1
                                                                   else "(" ++ desc1 ++ "," ++ desc2 ++ ")"
                                                              in if hasRawType lbBoth ubBoth
                                                                    then SType ubBoth lbBoth descBoth
                                                                    else SCheckError $ concat ["Type Mismatched: ", show lbBoth, " is not a subtype of ", show ubBoth]

lowerUb :: RawType -> RawType -> RawType
lowerUb ub@(RawSumType ubLeafs ubPartials) lb | S.size ubLeafs == 1 && H.null ubPartials = unionRawTypes ub lb
lowerUb _ lb = lb


getSchemeProp :: Scheme -> Name -> Scheme
getSchemeProp SCheckError{} _ = error "get prop of SCheckError"
getSchemeProp (SType ub lb desc) propName = SType (getRawTypeProp ub ) (getRawTypeProp lb) desc
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
setSchemeProp SCheckError{} _ _ = error "set prop of SCheckError"
setSchemeProp _ _ SCheckError{} = error "set prop to SCheckError"
setSchemeProp (SType ub lb desc) propName (SType pub _ _) = SType (setRawTypeUbProp ub) (setRawTypeLbProp lb) desc
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
      Just partialArg -> let partialArg' = intersectRawTypes partialArg pub
                          in if partialArg' == rawBottomType
                                then Nothing
                                else Just $ H.insert propName partialArg' partialArgs
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
executeConstraint _ (EqualsKnown pnt tp) = modifyDescriptor pnt (\oldScheme -> equalizeSchemes (oldScheme, SType tp tp "")) >> return []
executeConstraint _ (EqPoints p1 p2) = union' p1 p2 (\s1 s2 -> return (equalizeSchemes (s1, s2))) >> return []
executeConstraint _ cons@(BoundedBy subPnt parentPnt) = do
  subScheme <- descriptor subPnt
  parentScheme <- descriptor parentPnt
  case (subScheme, parentScheme) of
    (_, SCheckError _) -> return []
    (SCheckError _, _) -> return []
    (SType ub1 lb1 description, SType ub2 _ _) -> do
      let subScheme' = SType (intersectRawTypes ub1 ub2) lb1 description
      setDescriptor subPnt subScheme'
      return [cons | not (isSolved subScheme')]
executeConstraint typeGraph cons@(ArrowTo srcPnt destPnt) = do
  srcScheme <- descriptor srcPnt
  destScheme <- descriptor destPnt
  case (srcScheme, destScheme) of
    (SCheckError _, _) -> return []
    (_, SCheckError _) -> return []
    (SType srcUb _ _, SType destUb destLb destDescription) -> do
      maybeDestUbByGraph <- reaches typeGraph srcUb
      -- Commenting out reachedBy and usages until it is deemed necessary
      -- srcUbByGraph <- reachedBy typeGraph destUb
      case maybeDestUbByGraph of
        Just destUbByGraph -> do
          let destUb' = intersectRawTypes destUb destUbByGraph
          -- let srcUb' = intersectRawTypes srcUb srcUbByGraph
          let destLb' = lowerUb destUb' destLb
          -- let srcLb' = lowerUb srcUb' srcLb
          -- let srcScheme' = SType srcUb' srcLb'
          let destScheme' = SType destUb' destLb' destDescription
          -- setDescriptor srcPnt srcScheme'
          setDescriptor destPnt destScheme'
          -- return [cons | not (isSolved srcScheme' || isSolved destScheme')]
          return [cons | not (isSolved destScheme')]
        Nothing -> return [] -- remove constraint if found SCheckError
executeConstraint _ cons@(PropEq (superPnt, propName) subPnt) = do
  superScheme <- descriptor superPnt
  subScheme <- descriptor subPnt
  case (superScheme, subScheme) of
    (SCheckError _, _) -> return []
    (_, SCheckError _) -> return []
    (SType{}, SType{}) -> do
      let superPropScheme = getSchemeProp superScheme propName
      let scheme' = equalizeSchemes (superPropScheme, subScheme)
      let superScheme' = setSchemeProp superScheme propName scheme'
      setDescriptor subPnt scheme'
      setDescriptor superPnt superScheme'
      return [cons | not (isSolved scheme')]
executeConstraint _ cons@(AddArgs (srcPnt, newArgNames) destPnt) = do
  srcScheme <- descriptor srcPnt
  destScheme <- descriptor destPnt
  case (srcScheme, destScheme) of
    (SCheckError _, _) -> return []
    (_, SCheckError _) -> return []
    (SType srcUb _ _, SType _ destLb destDesc) ->
      case addArgsToRawType srcUb newArgNames of
        Just destUb' -> do
          setDescriptor destPnt $ equalizeSchemes (SType destUb' destLb destDesc, destScheme)
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
