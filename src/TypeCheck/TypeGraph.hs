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
import           Syntax
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

buildTypeEnv :: FEnv s -> VObjectMap s -> ST s (TypeEnv s, [Constraint s])
buildTypeEnv (FEnv _ typeGraph _ _) objMap = do
  (unionObj, cons) <- buildUnionObj (map fst objMap)
  return ((unionObj, typeGraph), cons)

ubFromScheme :: Scheme -> TypeCheckResult Type
ubFromScheme (TypeCheckResult _ (SType ub _ _))  = return ub
ubFromScheme (TypeCheckResE notes) = TypeCheckResE notes

reachesPartial :: TypeEnv s -> PartialType -> ST s (TypeCheckResult Type)
reachesPartial (_, graph) partial@(partialName, _, partialArgs) = do
  let typeArrows = H.lookupDefault [] partialName graph
  schemes <- mapM tryArrow typeArrows
  return $ joinDestTypes . catMaybes <$> sequence schemes
  where
    tryArrow (Object (objP, _) _ _ _ objArgs, Arrow (arrP, arrPreT) _ _ _) = do
      objScheme <- descriptor objP
      arrScheme <- descriptor arrP
      return $ sequenceT (ubFromScheme objScheme, ubFromScheme arrScheme) >>= \(objUb, arrUb) -> return $ if hasPartial partial objUb
        then case preTypedToTypeVar arrPreT of
               Just typeVar -> case H.elems $ H.intersectionWith const partialArgs $ H.filter (\((_, PreTyped (TypeVar t)), _) -> t == typeVar) objArgs of
                 [] -> Just arrUb
                 -- if the result is a type variable then it should be the intersection of all type variable args in the partial
                 partialAtArgs -> Just $ intersectAllTypes partialAtArgs
               Nothing -> Just arrUb
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
