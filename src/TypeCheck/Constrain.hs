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
import qualified Data.HashSet as S
import           Control.Monad.ST

import           Syntax
import           TypeCheck.Common
import           TypeCheck.TypeGraph (reaches)
import           Data.UnionFind.ST

isSolved :: Scheme -> Bool
isSolved (SType a b) = a == b
isSolved _ = False

equalizeSchemes :: (Scheme, Scheme) -> Scheme
equalizeSchemes (_, SCheckError s) = SCheckError s
equalizeSchemes (SCheckError s, _) = SCheckError s
equalizeSchemes (SType lb1 ub1, SType lb2 ub2) = let lbBoth = unionRawTypes lb1 lb2
                                                     ubBoth = intersectRawTypes ub1 ub2
                                                  in if hasRawType lbBoth ubBoth
                                                        then SType lbBoth ubBoth
                                                        else SCheckError $ concat ["Type Mismatched: ", show lbBoth, " is not a subtype of ", show ubBoth]

tupleCrossProductTypes :: [RawType] -> Maybe RawType
tupleCrossProductTypes parts = do
  partLeafs <- mapM fromSum parts
  return $ RawSumType $ S.fromList $ map RawProdType $ mapM S.toList partLeafs
  where fromSum (RawSumType leafs) = Just leafs
        fromSum RawTopType = Nothing
        fromSum RawBottomType = Nothing

tupleConstrainSumWith :: (([RawLeafType], RawType) -> ([RawLeafType], RawType)) -> (S.HashSet RawLeafType, [RawType]) -> (RawType, [RawType])
tupleConstrainSumWith constrain (wholeUnparsed, parts) = (whole', parts')
  where
    extractWhole (RawProdType leafs) = if length leafs == length parts then Just leafs else Nothing
    extractWhole _ = Nothing
    wholeSplit = sequence $ mapMaybe extractWhole $ S.toList wholeUnparsed
    (wholeSplit', parts') = unzip $ zipWith (curry constrain) wholeSplit parts
    whole' = RawSumType $ S.fromList $ map RawProdType $ sequence wholeSplit'

-- constrain by intersection
tupleConstrainUb :: (RawType, [RawType]) -> (RawType, [RawType])
tupleConstrainUb (RawTopType, parts) = (fromMaybe RawTopType $ tupleCrossProductTypes parts, parts)
tupleConstrainUb (RawBottomType, parts) = (RawBottomType, parts)
tupleConstrainUb (RawSumType wholeUnparsed, parts) = tupleConstrainSumWith constrain (wholeUnparsed, parts)
  where
    constrain (whole, RawTopType) = (whole, RawSumType $ S.fromList whole)
    constrain (_, RawBottomType) = error "Constrain ub with RawBottomType"
    constrain (whole, RawSumType partLeafs) = let leafs = S.intersection (S.fromList whole) partLeafs
                                                  in (S.toList leafs, RawSumType leafs)

-- constrain by union
tupleConstrainLb :: (RawType, [RawType]) -> (RawType, [RawType])
tupleConstrainLb (RawTopType, parts) = (RawTopType, parts)
tupleConstrainLb (RawBottomType, parts) = (fromMaybe RawBottomType $ tupleCrossProductTypes parts, parts)
tupleConstrainLb (RawSumType wholeUnparsed, parts) = tupleConstrainSumWith constrain (wholeUnparsed, parts)
  where
    constrain (_, RawTopType) = error "Constrain lb with RawTopType"
    constrain (whole, RawBottomType) = (whole, RawSumType $ S.fromList whole)
    constrain (whole, RawSumType partLeafs) = let leafs = S.union (S.fromList whole) partLeafs
                                                  in (S.toList leafs, RawSumType leafs)

lowerUb :: RawType -> RawType -> RawType
lowerUb ub@(RawSumType ubLeafs) lb | S.size ubLeafs == 1 = unionRawTypes ub lb
lowerUb _ lb = lb

executeConstraint :: TypeGraph s -> Constraint s -> ST s [Constraint s]
executeConstraint _ (EqualsKnown pnt tp) = modifyDescriptor pnt (\oldScheme -> equalizeSchemes (oldScheme, SType tp tp)) >> return []
executeConstraint _ (EqPoints p1 p2) = union' p1 p2 (\s1 s2 -> return (equalizeSchemes (s1, s2))) >> return []
executeConstraint _ cons@(BoundedBy subPnt parentPnt) = do
  subScheme <- descriptor subPnt
  parentScheme <- descriptor parentPnt
  case (subScheme, parentScheme) of
    (_, SCheckError _) -> return []
    (SCheckError _, _) -> return []
    (SType lb1 ub1, SType _ ub2) -> do
      let subScheme' = SType lb1 (intersectRawTypes ub1 ub2)
      setDescriptor subPnt subScheme'
      return [cons | not (isSolved subScheme')]
executeConstraint _ cons@(IsTupleOf wholePnt partPnts) = do
  wholeScheme <- descriptor wholePnt
  partSchemes <- mapM descriptor partPnts
  case (wholeScheme, partSchemes) of
    (_, pts) | not $ null [p | p@SCheckError{} <- pts] -> return []
    (SCheckError _, _) -> return []
    (SType wholeUb wholeLb, _) -> do
      let (partUbs, partLbs) = unzip $ map (\(SType ub lb) -> (ub, lb)) partSchemes
      let (wholeUb', partUbs') = tupleConstrainUb (wholeUb, partUbs)
      let (wholeLb', partLbs') = tupleConstrainLb (wholeLb, partLbs)
      let wholeScheme' = SType wholeUb' wholeLb'
      let partSchemes' = zipWith SType partUbs' partLbs'
      setDescriptor wholePnt wholeScheme'
      forM_ (zip partPnts partSchemes') $ uncurry setDescriptor
      return [cons | not (isSolved wholeScheme')]
executeConstraint typeGraph cons@(ArrowTo srcPnt destPnt) = do
  srcScheme <- descriptor srcPnt
  destScheme <- descriptor destPnt
  case (srcScheme, destScheme) of
    (SCheckError _, _) -> return []
    (_, SCheckError _) -> return []
    (SType srcUb srcLb, SType destUb destLb) -> do
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
          let destScheme' = SType destUb' destLb'
          -- setDescriptor srcPnt srcScheme'
          setDescriptor destPnt destScheme'
          -- return [cons | not (isSolved srcScheme' || isSolved destScheme')]
          return [cons | not (isSolved destScheme')]
        Nothing -> return []


abandonConstraints :: Constraint s -> ST s ()
abandonConstraints EqualsKnown{} = error "Bad Type equality"
abandonConstraints EqPoints{} = error "Bad point equality"
abandonConstraints BoundedBy{} = error "Bad Bounded By"
abandonConstraints IsTupleOf{} = error "Bad Tuple Of"
abandonConstraints (ArrowTo srcPnt destPnt) = do
  subScheme <- descriptor srcPnt
  parentScheme <- descriptor destPnt
  case (subScheme, parentScheme) of
    -- (SKnown _, SUnknown) -> setDescriptor destPnt $ SCheckError "Failed to unify hasType"
    (_, _) -> error "Uknown abandon constraint failure"

runConstraints :: TypeGraph s -> [Constraint s] -> ST s ()
runConstraints _ [] = return ()
runConstraints typeGraph cons = do
  res <- mapM (executeConstraint typeGraph) cons
  let cons' = concat res
  if cons == cons'
    then mapM_ abandonConstraints cons
    else runConstraints typeGraph cons'
