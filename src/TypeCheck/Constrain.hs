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
import           Control.Monad
import qualified Data.HashMap.Strict as H
import Data.Hashable (Hashable)
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
                                                              in if hasRawType lbBoth ubBoth
                                                                    then SType ubBoth lbBoth ("(" ++ desc1 ++ "," ++ desc2 ++ ")")
                                                                    else SCheckError $ concat ["Type Mismatched: ", show lbBoth, " is not a subtype of ", show ubBoth]

mapSequence :: (Eq b, Hashable b) => H.HashMap a (H.HashMap b c) -> H.HashMap b (H.HashMap a c)
mapSequence m = H.fromList $ map (\b -> (b, mapForB b)) $ S.toList bKeySet
  where
    intersections [] = error "mapSequence with no maps"
    intersections (firstSet:sets) = foldr S.intersection firstSet sets
    bKeySet = intersections $ H.elems $ H.map H.keysSet m
    mapForB b = H.mapMaybe (H.lookup b) m

tupleCrossProductTypes :: Name -> H.HashMap String RawType -> Maybe RawType
tupleCrossProductTypes name parts = do
  partLeafs <- mapM fromSum parts
  return $ RawSumType $ S.fromList $ map (RawLeafType name) $ mapM S.toList partLeafs
  where fromSum (RawSumType leafs) = Just leafs
        fromSum RawTopType = Nothing

tupleConstrainSumWith :: ((H.HashMap String RawLeafType, RawType) -> (H.HashMap String RawLeafType, RawType)) -> (S.HashSet RawLeafType, H.HashMap String RawType) -> (RawType, H.HashMap String RawType)
tupleConstrainSumWith constrainArg (wholeUnmatched, parts) = (whole', parts')
  where
    extractWhole (RawLeafType productName leafs) = if H.keysSet leafs == H.keysSet parts then Just (productName, leafs) else Nothing
    whole = mapSequence $ H.fromList $ mapMaybe extractWhole $ S.toList wholeUnmatched
    joined = H.intersectionWith (,) whole parts
    constrained = H.map constrainArg joined
    whole' = RawSumType $ S.fromList $ map (uncurry RawLeafType) $ H.toList $ mapSequence $ H.map fst constrained
    parts' = H.map snd constrained

-- constrain by intersection
tupleConstrainUb :: (RawType, H.HashMap String RawType) -> (RawType, H.HashMap String RawType)
tupleConstrainUb (RawTopType, parts) = (RawTopType, parts)
tupleConstrainUb (RawSumType wholeUnparsed, parts) = tupleConstrainSumWith constrainArg (wholeUnparsed, parts)
  where
    constrainArg :: (H.HashMap String RawLeafType, RawType) -> (H.HashMap String RawLeafType, RawType)
    constrainArg (whole, RawTopType) = (whole, RawSumType $ S.fromList $ H.elems whole)
    constrainArg (whole, RawSumType partLeafs) = let leafs = S.intersection (S.fromList $ H.elems whole) partLeafs
                                                  in (H.filter (`S.member` leafs) whole, RawSumType leafs)

-- constrain by union
tupleConstrainLb :: (RawType, H.HashMap String RawType) -> (RawType, H.HashMap String RawType)
tupleConstrainLb (RawTopType, parts) = (RawTopType, parts)
tupleConstrainLb (RawSumType wholeUnparsed, parts) = tupleConstrainSumWith constrainArg (wholeUnparsed, parts)
  where
    constrainArg :: (H.HashMap String RawLeafType, RawType) -> (H.HashMap String RawLeafType, RawType)
    constrainArg (_, RawTopType) = error "Constrain lb with RawTopType"
    constrainArg (whole, RawSumType partLeafs) = let leafs = S.union (S.fromList $ H.elems whole) partLeafs
                                                  in (whole, RawSumType leafs)
lowerUb :: RawType -> RawType -> RawType
lowerUb ub@(RawSumType ubLeafs) lb | S.size ubLeafs == 1 = unionRawTypes ub lb
lowerUb _ lb = lb

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
executeConstraint _ cons@(IsTupleOf wholePnt partPnts) = do
  wholeScheme <- descriptor wholePnt
  partSchemes <- mapM descriptor partPnts
  case (wholeScheme, partSchemes) of
    (SCheckError _, _) -> return []
    _ | H.null (H.filter schemeError partSchemes) -> return []
    (SType wholeUb wholeLb wholeDescription, _) -> do
      let (partUbs, partLbs, partDescriptions) = unzip3 $ map (\(argName, SType ub lb d) -> ((argName, ub), (argName, lb), (argName, d))) $ H.toList partSchemes
      let (wholeUb', partUbs') = tupleConstrainUb (wholeUb, H.fromList partUbs)
      let (wholeLb', partLbs') = tupleConstrainLb (wholeLb, H.fromList partLbs)
      let wholeScheme' = SType wholeUb' wholeLb' wholeDescription
      let partSchemes' = H.intersectionWith (\f x -> f x) (H.intersectionWith SType partUbs' partLbs') (H.fromList partDescriptions)
      setDescriptor wholePnt wholeScheme'
      forM_ (H.intersectionWith (,) partPnts partSchemes') $ uncurry setDescriptor
      return [cons | not (isSolved wholeScheme')]
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
