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
import           Control.Monad.ST
import           Data.UnionFind.ST
import           Data.Tuple.Sequence
import           Data.Maybe

import           Syntax.Types
import           Syntax.Prgm
import           TypeCheck.Common

buildUnionObj :: [VObject s] -> ST s (UnionObj s, [Constraint s])
buildUnionObj objs = do
  unionAllObjs <- fresh $ TypeCheckResult [] $ SType TopType bottomType "unionAllObjs"
  unionTypeObjs <- fresh $ TypeCheckResult [] $ SType TopType bottomType "unionTypeObjs"
  unionAllObjsPs <- fresh $ TypeCheckResult [] $ SType TopType bottomType "unionAllObjsPs"
  unionTypeObjsPs <- fresh $ TypeCheckResult [] $ SType TopType bottomType "unionTypeObjsPs"
  let constraints = [unionObjs unionAllObjs objs, unionObjs unionTypeObjs $ filterTypes objs, PowersetTo unionAllObjs unionAllObjsPs, PowersetTo unionTypeObjs unionTypeObjsPs]
  return ((unionAllObjsPs, unionTypeObjsPs), constraints)
                    where
                      unionObjs pnt os = UnionOf pnt $ map (\(Object m _ _ _ _) -> getPnt m) os
                      filterTypes = filter (\(Object _ basis _ _ _) -> basis == TypeObj)

buildTypeGraph :: VObjectMap s -> TypeGraph s
buildTypeGraph = foldr addArrows H.empty
  where
    addArrows (obj, arrows) acc = foldr (addArrow obj) acc arrows
    addArrow (Object objM _ name _ _) (Arrow arrM _ _ _) graph = graph'
      where graph' = H.insertWith (++) name [(getPnt objM, getPnt arrM)] graph

buildTypeEnv :: VObjectMap s -> ST s (TypeEnv s, [Constraint s])
buildTypeEnv objMap = do
  let typeGraph = buildTypeGraph objMap
  (unionObj, cons) <- buildUnionObj (map fst objMap)
  return ((unionObj, typeGraph), cons)

ubFromScheme :: Scheme -> TypeCheckResult Type
ubFromScheme (TypeCheckResult _ (SType ub _ _))  = return ub
ubFromScheme (TypeCheckResE notes) = TypeCheckResE notes

reachesPartial :: TypeEnv s -> PartialType -> ST s (TypeCheckResult Type)
reachesPartial (_, graph) partial@(partialName, _, _) = do
  let typePnts = H.lookupDefault [] partialName graph
  schemes <- mapM fromTypePnts typePnts
  return $ fmap (joinDestTypes . mapMaybe tryArrows) (mapM sequenceT schemes)
  where
    fromTypePnts (p1, p2) = do
      s1 <- descriptor p1
      s2 <- descriptor p2
      return (ubFromScheme s1, ubFromScheme s2)
    tryArrows (check, dest) = if hasPartial partial check
      then Just dest -- TODO: May need to propagate partialVars into the dest here
      else Nothing
    idReach = SumType (joinPartialLeafs [partial])
    joinDestTypes destTypes = unionTypes (idReach:destTypes)

reaches :: TypeEnv s -> Type -> ST s (TypeCheckResult Type)
reaches _     TopType            = return $ return TopType
reaches _     TypeVar{}            = error "reaches TypeVar"
reaches typeEnv (SumType srcPartials) = do
  resultsByPartials <- mapM (reachesPartial typeEnv) $ splitPartialLeafs srcPartials
  return $ unionTypes <$> sequence resultsByPartials


arrowConstrainUbs :: TypeEnv s -> Type -> Type -> ST s (TypeCheckResult (Type, Type))
arrowConstrainUbs _ TopType dest = return $ return (TopType, dest)
arrowConstrainUbs _ TypeVar{} _ = error "arrowConstrainUbs typeVar"
arrowConstrainUbs typeEnv (SumType srcPartials) dest = do
  let srcPartialList = splitPartialLeafs srcPartials
  partialMap <- sequence $ H.fromList $ zip srcPartialList $ map (reachesPartial typeEnv) srcPartialList
  let partialMap' = H.filter (`hasType` dest) <$> sequence partialMap
  return $ do
    partialMap'' <- partialMap'
    let (srcPartialList', destByPartial) = unzip $ H.toList partialMap''
    let srcPartials' = joinPartialLeafs srcPartialList'
    let destByGraph = unionTypes destByPartial
    dest' <- tryIntersectTypes dest destByGraph "executeConstraint ArrowTo"
    return (SumType srcPartials', dest')
