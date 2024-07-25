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
  typeGraphQuery typeEnv@TypeEnv{teTypeGraph=ObjArrTypeGraph tg} partial@PartialType{ptName} = mapMaybe tryTArrow $ H.lookupDefault [] ptName tg
    where
      tryTArrow oa@ObjArr{oaArr=Just{}} = do
        -- It is possible to send part of a partial through the arrow, so must compute the valid part
        -- If none of it is valid, then there is Nothing
        let potentialSrc@(UnionType potSrcLeafs) = intersectTypes typeEnv (singletonType partial) (getMetaType $ getExprMeta $ oaObjExpr oa)
        if not (isBottomType potentialSrc)
          then Just $ unionAllTypes typeEnv [arrowDestType typeEnv potentialSrcPartial oa | potentialSrcPartial <- splitUnionType potSrcLeafs]
          else Nothing
      tryTArrow ObjArr{oaArr=Nothing} = Nothing

mkTypeEnv :: (ExprClass e, Show m, Show (e m), MetaDat m) => Prgm e m -> TypeEnv (ObjArrTypeGraph e m)
mkTypeEnv (objMap, classGraph, _) = TypeEnv classGraph (ObjArrTypeGraph $ H.fromListWith (++) $ map typeGraphItem objMap) typeNames
  where
    typeNames = S.fromList $ map makeAbsoluteName $ concatMap getAllObjArrNames objMap
    typeGraphItem oa = (makeAbsoluteName $ oaObjPath oa, [oa])

labelPosM :: String -> Meta m -> Meta m
labelPosM s (Meta t pos ext) = Meta t (labelPos s pos) ext

mWithType :: Type -> Meta m -> Meta m
mWithType t (Meta _ p d) = Meta t p d

labelPos :: String -> CodeRange -> CodeRange
labelPos s (Just (p1, p2, sPrefix)) = Just (p1, p2, label')
  where label' = case sPrefix of
          [] -> s
          _  -> printf "%s-%s" sPrefix s
labelPos _ Nothing = Nothing

emptyMetaM :: (MetaDat m) => String -> Meta m -> Meta m
emptyMetaM s m = Meta PTopType (labelPos s $ getMetaPos m) emptyMetaDat

emptyMetaE :: (ExprClass e, MetaDat m) => String -> e m -> Meta m
emptyMetaE s e = emptyMetaM s (getExprMeta e)

exprWithMeta :: (Show m) => Meta m -> Expr m -> Expr m
exprWithMeta m (CExpr _ c)        = CExpr m c
exprWithMeta m (Value _ n)        = Value m n
exprWithMeta m (HoleExpr _ h)     = HoleExpr m h
exprWithMeta m (EWhere _ base cond) = EWhere m base cond
exprWithMeta m (TupleApply _ b a) = TupleApply m b a
exprWithMeta m (VarApply _ b n v) = VarApply m b n v
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
exprWhereConds (TupleApply _ (_, b) (EAppSpread a)) = exprWhereConds b ++ exprWhereConds a
exprWhereConds (VarApply _ b _ _) = exprWhereConds b

exprWithMetaType :: (Show m) => Type -> Expr m -> Expr m
exprWithMetaType t e = exprWithMeta (mWithType t (getExprMeta e)) e

classGraphFromObjs :: (ExprClass e, MetaDat m, Show m, Show (e m)) => ObjectMap e m -> ClassGraph
classGraphFromObjs objMap = ClassGraph $ graphFromEdges $ map (\oa -> (CGType, PTypeName (oaObjPath oa), [])) objMap

mapOAObjExpr :: (MetaDat m, ExprClass e, Show (e m)) => (e m -> e m) -> ObjArr e m -> ObjArr e m
mapOAObjExpr f oa@ObjArr{oaObj=Just e}  = oa{oaObj = Just (f e)}
mapOAObjExpr _ oa@ObjArr{oaObj=Nothing} = oa

mapOAArrExpr :: (MetaDat m, ExprClass e, Show (e m)) => (e m -> e m) -> ObjArr e m -> ObjArr e m
mapOAArrExpr f oa@ObjArr{oaArr=Just (Just e, m)} = oa{oaArr = Just (Just (f e), m)}
mapOAArrExpr _ oa                           = oa

exprVarArgsWithSrcs :: (MetaDat m, Show m) => TypeEnv tg -> [(Expr m, PartialType)] -> ArgMetaMapWithSrc m
exprVarArgsWithSrcs typeEnv os = H.unions (map (uncurry (exprVarArgsWithSrc typeEnv)) os)

exprVarArgsWithObjSrcs :: (MetaDat m, Show m) => TypeEnv tg -> [(PartialType, ObjArr Expr m)] -> ArgMetaMapWithSrc m
exprVarArgsWithObjSrcs typeEnv os = exprVarArgsWithSrcs typeEnv $ map (\(src, obj) -> (oaObjExpr obj, src)) os

arrowDestType :: (ExprClass e, Show m, Show (e m), MetaDat m) => TypeEnv tg -> PartialType -> ObjArr e m -> Type
arrowDestType typeEnv src oa@ObjArr{oaArr=Just (oaArrExpr, oaM)} = case oaArrExpr of
  Just n | isJust (maybeExprPath n) -> maybe joined substitute (H.lookup (TVArg $ inExprSingleton n) vaenv)
  _                              -> joined
  where
    vaenv = snd <$> exprVarArgsWithSrc typeEnv (oaObjExpr oa) (fromJust $ maybeGetSingleton $ substituteVars $ singletonType src)
    substitute = substituteWithVarArgEnv vaenv
    expr' = fmap (substitute . getExprType) oaArrExpr
    arr' = substitute $ getMetaType oaM
    joined = fromMaybe arr' expr'
arrowDestType _ _ oa@ObjArr{oaArr=Nothing} = error $ printf "Unexpected call to arrowDestType with type obj (which has no arrow to find the dest type of): %s" (show oa)

metaTypeVar :: Meta m -> Maybe TypeVarAux
metaTypeVar m = case getMetaType m of
  TypeVar v _ -> Just v
  _           -> Nothing

type MetaVarArgEnv m = H.HashMap TypeVarAux (Meta m)

isSubtypePartialOfWithObjSrc :: (Show m, MetaDat m) => TypeEnv tg -> PartialType -> ObjArr Expr m -> PartialType -> Type -> Bool
isSubtypePartialOfWithObjSrc typeEnv os obj sub = isSubtypeOfWithObjSrc typeEnv os obj (singletonType sub)

isSubtypeOfWithObjSrc :: (Show m, MetaDat m) => TypeEnv tg -> PartialType -> ObjArr Expr m -> Type -> Type -> Bool
isSubtypeOfWithObjSrc typeEnv srcType obj = isSubtypeOfWithEnv typeEnv (snd <$> exprVarArgsWithSrc typeEnv (oaObjExpr obj) srcType)

isSubtypeOfWithObjSrcs :: (Show m, MetaDat m) => TypeEnv tg -> [(PartialType, ObjArr Expr m)] -> Type -> Type -> Bool
isSubtypeOfWithObjSrcs typeEnv objSrcs = isSubtypeOfWithEnv typeEnv (snd <$> exprVarArgsWithObjSrcs typeEnv objSrcs)

isSubtypePartialOfWithMetaEnv :: TypeEnv tg -> MetaVarArgEnv m -> PartialType -> Type -> Bool
isSubtypePartialOfWithMetaEnv typeEnv vaenv sub = isSubtypeOfWithEnv typeEnv (metaToTypeEnv vaenv) (singletonType sub)
  where
    metaToTypeEnv = fmap getMetaType

isSubtypeOfWithMetaEnv :: TypeEnv tg -> MetaVarArgEnv m -> Type -> Type -> Bool
isSubtypeOfWithMetaEnv typeEnv vaenv = isSubtypeOfWithEnv typeEnv (metaToTypeEnv vaenv)
  where
    metaToTypeEnv = fmap getMetaType
