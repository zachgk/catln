--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.TypeGraph
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module TypeCheck.TypeGraph where

import qualified Data.HashMap.Strict           as H
import qualified Data.HashSet                  as S
import           Control.Monad.ST
import           Data.UnionFind.ST
import           Data.Tuple.Sequence
import           Data.Maybe

import           Syntax.Types
import           Syntax.Prgm
import           TypeCheck.Common

buildUnionObj :: [VObject s] -> ST s (UnionObj s, [Constraint s])
buildUnionObj objs = do
  unionAllObjs <- fresh $ TypeCheckResult [] $ SType RawTopType rawBottomType "unionAllObjs"
  unionTypeObjs <- fresh $ TypeCheckResult [] $ SType RawTopType rawBottomType "unionTypeObjs"
  unionAllObjsPs <- fresh $ TypeCheckResult [] $ SType RawTopType rawBottomType "unionAllObjsPs"
  unionTypeObjsPs <- fresh $ TypeCheckResult [] $ SType RawTopType rawBottomType "unionTypeObjsPs"
  let constraints = [unionObjs unionAllObjs objs, unionObjs unionTypeObjs $ filterTypes objs, PowersetTo unionAllObjs unionAllObjsPs, PowersetTo unionTypeObjs unionTypeObjsPs]
  return ((unionAllObjsPs, unionTypeObjsPs), constraints)
                    where
                      unionObjs pnt os = UnionOf pnt $ map (\(Object m _ _ _) -> m) os
                      filterTypes = filter (\(Object _ basis _ _) -> basis == TypeObj)

buildTypeGraph :: VObjectMap s -> TypeGraph s
buildTypeGraph = foldr addArrows H.empty
  where
    addArrows (obj, arrows) acc = foldr (addArrow obj) acc arrows
    addArrow (Object objM _ name _) (Arrow arrM _ _ _) graph = graph'
      where graph' = H.insertWith (++) name [(objM, arrM)] graph

buildTypeEnv :: VObjectMap s -> ST s (TypeEnv s, [Constraint s])
buildTypeEnv objMap = do
  let typeGraph = buildTypeGraph objMap
  (unionObj, cons) <- buildUnionObj (map fst objMap)
  return ((unionObj, typeGraph), cons)

ubFromScheme :: Scheme -> TypeCheckResult RawType
ubFromScheme (TypeCheckResult _ (SType ub _ _))  = return ub
ubFromScheme (TypeCheckResE notes) = TypeCheckResE notes

reachesLeaf :: TypeEnv s -> RawLeafType -> ST s (TypeCheckResult RawType)
reachesLeaf (_, graph) leaf@(RawLeafType leafName _) = do
  let typePnts = H.lookupDefault [] leafName graph
  schemes <- mapM fromTypePnts typePnts
  return $ fmap (joinDestTypes . mapMaybe tryArrows) (mapM sequenceT schemes)
  where
    fromTypePnts (p1, p2) = do
      s1 <- descriptor p1
      s2 <- descriptor p2
      return (ubFromScheme s1, ubFromScheme s2)
    tryArrows (check, dest) = if hasRawLeaf leaf check
      then Just dest
      else Nothing
    idReach = RawSumType (S.singleton leaf) H.empty
    joinDestTypes destTypes = unionRawTypes (idReach:destTypes)

reachesPartial :: TypeEnv s -> RawPartialType -> ST s (TypeCheckResult RawType)
reachesPartial (_, graph) partial@(partialName, _) = do
  let typePnts = H.lookupDefault [] partialName graph
  schemes <- mapM fromTypePnts typePnts
  return $ fmap (joinDestTypes . mapMaybe tryArrows) (mapM sequenceT schemes)
  where
    fromTypePnts (p1, p2) = do
      s1 <- descriptor p1
      s2 <- descriptor p2
      return (ubFromScheme s1, ubFromScheme s2)
    tryArrows (check, dest) = if hasRawPartial partial check
      then Just dest
      else Nothing
    idReach = RawSumType S.empty (joinPartialLeafs [partial])
    joinDestTypes destTypes = unionRawTypes (idReach:destTypes)

reaches :: TypeEnv s -> RawType -> ST s (TypeCheckResult RawType)
reaches _     RawTopType            = return $ return RawTopType
reaches typeEnv (RawSumType srcLeafs srcPartials) = do
  resultsByLeafs <- mapM (reachesLeaf typeEnv) $ S.toList srcLeafs
  resultsByPartials <- mapM (reachesPartial typeEnv) $ splitPartialLeafs srcPartials
  return $ unionRawTypes <$> sequence (resultsByPartials ++ resultsByLeafs)


arrowConstrainUbs :: TypeEnv s -> RawType -> RawType -> ST s (TypeCheckResult (RawType, RawType))
arrowConstrainUbs _ RawTopType dest = return $ return (RawTopType, dest)
arrowConstrainUbs typeEnv (RawSumType srcLeafs srcPartials) dest = do
  let srcLeafList = S.toList srcLeafs
  leafMap <- sequence $ H.fromList $ zip srcLeafList $ map (reachesLeaf typeEnv) srcLeafList
  let srcPartialList = splitPartialLeafs srcPartials
  partialMap <- sequence $ H.fromList $ zip srcPartialList $ map (reachesPartial typeEnv) srcPartialList
  let leafMap' = H.filter (`hasRawType` dest) <$> sequence leafMap
  let partialMap' = H.filter (`hasRawType` dest) <$> sequence partialMap
  return $ do
    (leafMap'', partialMap'') <- sequenceT (leafMap', partialMap')
    let (srcLeafList', destByLeaf) = unzip $ H.toList leafMap''
    let (srcPartialList', destByPartial) = unzip $ H.toList partialMap''
    let srcLeafs' = S.fromList srcLeafList'
    let srcPartials' = joinPartialLeafs srcPartialList'
    let destByGraph = unionRawTypes (destByLeaf ++ destByPartial)
    dest' <- tryIntersectRawTypes dest destByGraph "executeConstraint ArrowTo"
    return (RawSumType srcLeafs' srcPartials', dest')
