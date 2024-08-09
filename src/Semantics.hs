--------------------------------------------------------------------
-- |
-- Module    :  Semantics
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module contains miscellaneous and general syntax elements.
-- It also contains a the instances of program metadata and some
-- general syntax-based utilities.
--------------------------------------------------------------------

module Semantics where

import qualified Data.HashMap.Strict as H

import           Data.Graph          (graphFromEdges)
import qualified Data.HashSet        as S
import           Data.Maybe
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf

newtype ObjArrTypeGraph e m = ObjArrTypeGraph (H.HashMap TypeName [ObjArr e m])
  deriving (Show)
instance (ExprClass e, MetaDat m, Show m, Show (e m)) => TypeGraph (ObjArrTypeGraph e m) where
  typeGraphMerge (ObjArrTypeGraph a) (ObjArrTypeGraph b) = ObjArrTypeGraph (H.unionWith (++) a b)
  typeGraphQueryWithReason typeEnv@TypeEnv{teTypeGraph=ObjArrTypeGraph tg} vaenv partial@PartialType{ptName} = concatMap tryTArrow $ H.lookupDefault [] ptName tg
    where
      tryTArrow :: (ExprClass e, MetaDat m, Show m, Show (e m)) => ObjArr e m -> [(String, Type)]
      tryTArrow oa@ObjArr{oaArr=Just{}} = do
        -- It is possible to send part of a partial through the arrow, so must compute the valid part
        -- If none of it is valid, then there is Nothing
        let potentialSrc@(UnionType potSrcLeafs) = intersectTypesEnv typeEnv vaenv (singletonType partial) (getMetaType $ getExprMeta $ oaObjExpr oa)
        if not (isBottomType potentialSrc)
          then map (printf "using %s" (show oa),) $ joinPartialDestTypes [arrowDestType typeEnv potentialSrcPartial oa | potentialSrcPartial <- splitUnionType potSrcLeafs]
          else []
      tryTArrow ObjArr{oaArr=Nothing} = []

      joinPartialDestTypes :: [[Type]] -> [Type]
      joinPartialDestTypes [] = [BottomType]
      joinPartialDestTypes (pdt1:restPdt) = [unionTypes typeEnv pdt1Poss restPdt' | pdt1Poss <- pdt1, restPdt' <- joinPartialDestTypes restPdt]

mkTypeEnv :: (ExprClass e, Show m, Show (e m), MetaDat m) => Prgm e m -> TypeEnv (ObjArrTypeGraph e m)
mkTypeEnv (objMap, classGraph, _) = TypeEnv classGraph (ObjArrTypeGraph $ H.fromListWith (++) $ map typeGraphItem objMap) typeNames
  where
    typeNames = S.fromList $ map makeAbsoluteName $ concatMap getAllObjArrNames objMap
    typeGraphItem oa = (makeAbsoluteName $ oaObjPath oa, [oa])

emptyMetaM :: (MetaDat m) => Meta m -> Meta m
emptyMetaM m = emptyMetaN{getMetaPos=getMetaPos m}

emptyMetaE :: (ExprClass e, MetaDat m) => e m -> Meta m
emptyMetaE e = emptyMetaM (getExprMeta e)

exprWithMeta :: (Show m) => Meta m -> Expr m -> Expr m
exprWithMeta m (CExpr _ c)        = CExpr m c
exprWithMeta m (Value _ n)        = Value m n
exprWithMeta m (HoleExpr _ h)     = HoleExpr m h
exprWithMeta m (EWhere _ base cond) = EWhere m base cond
exprWithMeta m (TupleApply _ b a) = TupleApply m b a
exprWithMeta _ e                  = error $ printf "exprWithMeta with unsupported expr %s" (show e)

-- | Returns the EWhere conditions in an expression
exprWhereConds :: Expr m -> [Expr m]
exprWhereConds CExpr{} = []
exprWhereConds Value{} = []
exprWhereConds HoleExpr{} = []
exprWhereConds (AliasExpr b _) = exprWhereConds b
exprWhereConds (EWhere _ _ c) = [c]
exprWhereConds (TupleApply _ (_, b) (EAppArg ObjArr{oaArr=Nothing})) = exprWhereConds b
exprWhereConds (TupleApply _ (_, b) (EAppArg ObjArr{oaArr=Just (me, _)})) = exprWhereConds b ++ maybe [] exprWhereConds me
exprWhereConds (TupleApply _ (_, b) EAppVar{}) = exprWhereConds b
exprWhereConds (TupleApply _ (_, b) (EAppSpread a)) = exprWhereConds b ++ exprWhereConds a

exprWithMetaType :: (Show m) => Type -> Expr m -> Expr m
exprWithMetaType t e = exprWithMeta (mWithType t (getExprMeta e)) e

classGraphFromObjs :: (ExprClass e, MetaDat m, Show m, Show (e m)) => ObjectMap e m -> ClassGraph
classGraphFromObjs objMap = ClassGraph $ graphFromEdges $ map (\oa -> (CGType, PTypeName (oaObjPath oa), [])) objMap

mapExprPath :: (Show m) => ((Meta m, TypeName) -> Expr m) -> Expr m -> Expr m
mapExprPath f (Value m n) = f (m, n)
mapExprPath f (EWhere m b c) = EWhere m (mapExprPath f b) c
mapExprPath f (TupleApply m (bm, be) a) = TupleApply m (bm, mapExprPath f be) a
mapExprPath _ e = error $ printf "Unexpected expr to mapExprPath: %s" (show e)

mapOAObjExpr :: (MetaDat m, ExprClass e, Show (e m)) => (e m -> e m) -> ObjArr e m -> ObjArr e m
mapOAObjExpr f oa@ObjArr{oaObj=Just e}  = oa{oaObj = Just (f e)}
mapOAObjExpr _ oa@ObjArr{oaObj=Nothing} = oa

mapOAArrExpr :: (MetaDat m, ExprClass e, Show (e m)) => (e m -> e m) -> ObjArr e m -> ObjArr e m
mapOAArrExpr f oa@ObjArr{oaArr=Just (Just e, m)} = oa{oaArr = Just (Just (f e), m)}
mapOAArrExpr _ oa                           = oa

exprVarArgsWithSrcs :: (TypeGraph tg, MetaDat m, Show m) => TypeEnv tg -> [(Expr m, PartialType)] -> PossArgMetaMapWithSrc m
exprVarArgsWithSrcs typeEnv os = joinAllPossVarArgMetaWithSrc typeEnv (map (uncurry (exprVarArgsWithSrc typeEnv)) os)

exprVarArgsWithObjSrcs :: (TypeGraph tg, MetaDat m, Show m) => TypeEnv tg -> [(PartialType, ObjArr Expr m)] -> PossArgMetaMapWithSrc m
exprVarArgsWithObjSrcs typeEnv os = exprVarArgsWithSrcs typeEnv $ map (\(src, obj) -> (oaObjExpr obj, src)) os

arrowDestType :: (TypeGraph tg, ExprClass e, Show m, Show (e m), MetaDat m) => TypeEnv tg -> PartialType -> ObjArr e m -> [Type]
arrowDestType typeEnv src oa@ObjArr{oaArr=Just (oaArrExpr, oaM)} = map (arrowDestTypeForVaenv . fmap snd) $ exprVarArgsWithSrc typeEnv (oaObjExpr oa) (fromJust $ maybeGetSingleton $ substituteVars $ singletonType src)
  where
    arrowDestTypeForVaenv vaenv = case oaArrExpr of
      -- _ | trace (printf "arrowDestType from %s with %s to %s.\n\t\tVanev: %s" (show src) (show oa) (show joined) (show vaenv)) False -> undefined
      Just n | isJust (maybeExprPath n) -> maybe joined substitute (H.lookup (TVArg $ inExprSingleton n) vaenv)
      _                              -> joined
      where
        substitute = substituteWithVarArgEnv vaenv
        expr' = fmap (substitute . getExprType) oaArrExpr
        arr' = substitute $ getMetaType oaM
        joined = fromMaybe arr' expr'
arrowDestType _ _ oa@ObjArr{oaArr=Nothing} = error $ printf "Unexpected call to arrowDestType with type obj (which has no arrow to find the dest type of): %s" (show oa)

metaTypeVar :: Meta m -> Maybe TypeVarAux
metaTypeVar m = case getMetaType m of
  TypeVar v _ -> Just v
  _           -> Nothing

isSubtypeOfWithObjSrcs :: (TypeGraph tg, Show m, MetaDat m) => TypeEnv tg -> [(PartialType, ObjArr Expr m)] -> Type -> Type -> Bool
isSubtypeOfWithObjSrcs typeEnv objSrcs subType supType = any (\vaenv -> isSubtypeOfWithEnv typeEnv (fmap snd vaenv) subType supType) (exprVarArgsWithObjSrcs typeEnv objSrcs)
