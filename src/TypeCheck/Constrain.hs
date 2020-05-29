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
    getPartials partials = joinPartials $ mapMaybe (H.lookup propName) $ concat $ H.elems partials
    joinPartials :: [RawType] -> RawType
    joinPartials = foldr unionRawTypes rawBottomType

setSchemeProp :: Scheme -> Name -> Scheme -> Scheme
setSchemeProp scheme propName pscheme = do
  (SType ub lb desc) <- scheme
  (SType pub _ _) <- pscheme
  checkScheme ("setSchemeProp " ++ propName) $ return $ SType (compactRawType $ setRawTypeUbProp ub pub) (compactRawType $ setRawTypeLbProp lb) desc
  where
    setRawTypeUbProp :: RawType -> RawType -> RawType
    setRawTypeUbProp RawTopType _ = RawTopType
    setRawTypeUbProp (RawSumType ubLeafs ubPartials) pub = RawSumType (S.fromList $ mapMaybe (setLeafUbProp pub) $ S.toList ubLeafs) (H.mapMaybe (setPartialsUb pub) ubPartials)
    setLeafUbProp pub ubLeaf@(RawLeafType _ leafArgs) = case (H.lookup propName leafArgs, pub) of
      (Nothing, _) -> Nothing
      (Just leafArg, RawSumType pubLeafs _) -> if S.member leafArg pubLeafs
        then Just ubLeaf
        else Nothing
      (Just{} , RawTopType) -> Just ubLeaf
    setPartialsUb pub partials = case mapMaybe (setPartialUb pub) partials of
      [] -> Nothing
      partials' -> Just partials'
    setPartialUb pub partialArgs = case H.lookup propName partialArgs of
      Just partialArg -> let tryPartialArg' = tryIntersectRawTypes partialArg pub "setSchemeProp"
                          in case tryPartialArg' of
                               TypeCheckResult _ partialArg' -> if partialArg' == rawBottomType
                                                      then Nothing
                                                      else Just $ H.insert propName partialArg' partialArgs
                               TypeCheckResE _ -> Nothing
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
executeConstraint typeGraph cons@(ArrowTo srcPnt destPnt) = do
  srcScheme <- descriptor srcPnt
  destScheme <- descriptor destPnt
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> return ([], False)
    TypeCheckResult _ (SType srcUb srcLb srcDesc, SType destUb destLb destDesc) -> do
      constrained <- arrowConstrainUbs typeGraph srcUb destUb
      case constrained of
        TypeCheckResult _ (srcUb', destUb') -> do
          let srcScheme' = return $ SType srcUb' srcLb srcDesc
          let destScheme' = return $ SType destUb' destLb destDesc
          setDescriptor srcPnt srcScheme'
          setDescriptor destPnt destScheme'
          return ([cons | not (isSolved destScheme')], srcScheme /= srcScheme' || destScheme /= destScheme')
        TypeCheckResE _ -> return ([], False)
executeConstraint typeGraph cons@(PropEq (superPnt, propName) subPnt) = do
  superScheme <- descriptor superPnt
  subScheme <- descriptor subPnt
  case sequenceT (superScheme, subScheme) of
    TypeCheckResE _ -> return ([], False)
    TypeCheckResult{} -> do
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
  case sequenceT (srcScheme, destScheme) of
    TypeCheckResE _ -> return ([], False)
    TypeCheckResult _ (SType srcUb _ _, SType _ destLb destDesc) ->
      case addArgsToRawType srcUb newArgNames of
        Just destUb' -> do
          let destScheme' = equalizeSchemes (destScheme, return $ SType destUb' destLb destDesc) "executeConstraint AddArgs"
          setDescriptor destPnt destScheme'
          return ([], True)
        Nothing -> return ([cons], False)
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
runConstraints limit typeGraph cons = do
  res <- mapM (executeConstraint typeGraph) cons
  let (consList, changedList) = unzip res
  let cons' = concat consList
  if not (or changedList)
    then return $ return ()
    else runConstraints (limit - 1) typeGraph cons'
