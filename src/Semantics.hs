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
import           Data.Maybe
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf


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
exprWithMeta m (EWhere base cond) = EWhere (exprWithMeta m base) cond
exprWithMeta m (TupleApply _ b a) = TupleApply m b a
exprWithMeta m (VarApply _ b n v) = VarApply m b n v
exprWithMeta _ e                  = error $ printf "exprWithMeta with unsupported expr %s" (show e)

-- | Returns the EWhere conditions in an expression
exprWhereConds :: Expr m -> [Expr m]
exprWhereConds CExpr{} = []
exprWhereConds Value{} = []
exprWhereConds HoleExpr{} = []
exprWhereConds (AliasExpr b _) = exprWhereConds b
exprWhereConds (EWhere _ c) = [c]
exprWhereConds (TupleApply _ (_, b) ObjArr{oaArr=Nothing}) = exprWhereConds b
exprWhereConds (TupleApply _ (_, b) ObjArr{oaArr=Just (me, _)}) = exprWhereConds b ++ maybe [] exprWhereConds me
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

-- |
-- The 'exprVarArgsWithSrc' is similar to the 'exprArgs' function.
-- It differs in that it accepts an additional partial type that is equivalent to the expression and it pull the corresponding parts of the partial matching the args in the expression
-- TODO: Make nonLinear by changing signature to H.HashMap ArgName ([Meta m], Type)
type ArgMetaMapWithSrc m = H.HashMap TypeVarAux ([(Expr m, Meta m)], Type)
exprVarArgsWithSrc :: (MetaDat m, Show m) => TypeEnv -> Expr m -> PartialType -> ArgMetaMapWithSrc m
exprVarArgsWithSrc _ CExpr{} _ = H.empty
exprVarArgsWithSrc _ Value{} _ = H.empty
exprVarArgsWithSrc _ HoleExpr{} _ = H.empty
exprVarArgsWithSrc typeEnv (EWhere base _) src = exprVarArgsWithSrc typeEnv base src
exprVarArgsWithSrc typeEnv (AliasExpr base n) src = H.insert (TVArg $ inExprSingleton n) ([(n, getExprMeta base)], singletonType src) (exprVarArgsWithSrc typeEnv base src)
exprVarArgsWithSrc typeEnv (VarApply _ b n m) src = H.insert (TVVar n) ([(Value (emptyMetaT $ partialToTypeSingleton n) (pkName n), m)], H.lookupDefault PTopType n (ptVars src)) $ exprVarArgsWithSrc typeEnv b src
exprVarArgsWithSrc typeEnv (TupleApply _ (_, be) arg) src = H.union (exprVarArgsWithSrc typeEnv be src) (fromArg arg)
  where
    fromArg ObjArr{oaObj=Just obj, oaArr=Just (Just e, _)} = case typeGetArg (inExprSingleton obj) src of
      Just (UnionType srcArg) -> mergeMaps $ map (exprVarArgsWithSrc typeEnv e) $ splitUnionType srcArg
      Just t@TopType{} -> (,t) <$> exprVarArgs e
      _ -> H.empty
    fromArg ObjArr{oaArr=Just (Just e, _)} = exprVarArgsWithSrc typeEnv e src
    fromArg ObjArr {oaObj=Just obj, oaArr=Just (Nothing, arrM)} = case typeGetArg (inExprSingleton obj) src of
      Just srcArg -> H.singleton (TVArg $ inExprSingleton obj) ([(obj, arrM)], srcArg)
      Nothing     -> H.empty
    fromArg oa = error $ printf "Invalid oa %s" (show oa)

    mergeMaps [] = H.empty
    mergeMaps (x:xs) = foldr (H.unionWith (\(m1s, t1) (m2s, t2) -> (m1s ++ m2s, unionTypes typeEnv t1 t2))) x xs

exprVarArgsWithSrcs :: (MetaDat m, Show m) => TypeEnv -> [(Expr m, PartialType)] -> ArgMetaMapWithSrc m
exprVarArgsWithSrcs typeEnv os = H.unions (map (uncurry (exprVarArgsWithSrc typeEnv)) os)

exprVarArgsWithObjSrcs :: (MetaDat m, Show m) => TypeEnv -> [(PartialType, ObjArr Expr m)] -> ArgMetaMapWithSrc m
exprVarArgsWithObjSrcs typeEnv os = exprVarArgsWithSrcs typeEnv $ map (\(src, obj) -> (oaObjExpr obj, src)) os

-- fullDest means to use the greatest possible type (after implicit).
-- Otherwise, it uses the minimal type that *must* be reached
arrowDestType :: (Show m, MetaDat m) => Bool -> TypeEnv -> PartialType -> ObjArr Expr m -> Type
arrowDestType fullDest typeEnv src oa@ObjArr{oaArr=Just (oaArrExpr, oaM)} = case oaArrExpr of
  Just n | isJust (maybeExprPath n) -> fromMaybe joined (H.lookup (TVArg $ inExprSingleton n) vaenv)
  _                              -> joined
  where
    vaenv = snd <$> exprVarArgsWithSrc typeEnv (oaObjExpr oa) (fromJust $ maybeGetSingleton $ substituteVars $ singletonType src)
    substitute = substituteWithVarArgEnv vaenv
    expr' = fmap (substitute . getExprType) oaArrExpr
    arr' = substitute $ getMetaType oaM
    joined = if fullDest
      then unionTypes typeEnv (fromMaybe BottomType expr') arr'
      else intersectTypes typeEnv (fromMaybe PTopType expr') arr'
arrowDestType _ _ _ oa@ObjArr{oaArr=Nothing} = error $ printf "Unexpected call to arrowDestType with type obj (which has no arrow to find the dest type of): %s" (show oa)

metaTypeVar :: Meta m -> Maybe TypeVarAux
metaTypeVar m = case getMetaType m of
  TypeVar v _ -> Just v
  _           -> Nothing

type MetaVarArgEnv m = H.HashMap TypeVarAux (Meta m)

isSubtypePartialOfWithObjSrc :: (Show m, MetaDat m) => TypeEnv -> PartialType -> ObjArr Expr m -> PartialType -> Type -> Bool
isSubtypePartialOfWithObjSrc typeEnv os obj sub = isSubtypeOfWithObjSrc typeEnv os obj (singletonType sub)

isSubtypeOfWithObjSrc :: (Show m, MetaDat m) => TypeEnv -> PartialType -> ObjArr Expr m -> Type -> Type -> Bool
isSubtypeOfWithObjSrc typeEnv srcType obj = isSubtypeOfWithEnv typeEnv (snd <$> exprVarArgsWithSrc typeEnv (oaObjExpr obj) srcType)

isSubtypeOfWithObjSrcs :: (Show m, MetaDat m) => TypeEnv -> [(PartialType, ObjArr Expr m)] -> Type -> Type -> Bool
isSubtypeOfWithObjSrcs typeEnv objSrcs = isSubtypeOfWithEnv typeEnv (snd <$> exprVarArgsWithObjSrcs typeEnv objSrcs)

isSubtypePartialOfWithMetaEnv :: TypeEnv -> MetaVarArgEnv m -> PartialType -> Type -> Bool
isSubtypePartialOfWithMetaEnv typeEnv vaenv sub = isSubtypeOfWithEnv typeEnv (metaToTypeEnv vaenv) (singletonType sub)
  where
    metaToTypeEnv = fmap getMetaType

isSubtypeOfWithMetaEnv :: TypeEnv -> MetaVarArgEnv m -> Type -> Type -> Bool
isSubtypeOfWithMetaEnv typeEnv vaenv = isSubtypeOfWithEnv typeEnv (metaToTypeEnv vaenv)
  where
    metaToTypeEnv = fmap getMetaType
