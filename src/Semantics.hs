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

import           Data.Hashable
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

getExprType :: (ExprClass e) => e m -> Type
getExprType = getMetaType . getExprMeta

emptyMetaM :: String -> Meta m -> Meta m
emptyMetaM = labelPosM

emptyMetaE :: (ExprClass e) => String -> e m -> Meta m
emptyMetaE s e = labelPosM s $ getExprMeta e

exprAppliedArgsMap :: (ExprClass e, Show m) => e m -> H.HashMap ArgName (Meta m, Maybe (e m))
exprAppliedArgsMap = H.fromList . mapMaybe mapArg . exprAppliedArgs
  where
    mapArg ObjArr{oaObj=Just (GuardExpr oe _), oaArr=(Just (GuardExpr ae _), oaM)} = Just (exprPath oe, (oaM, Just ae))
    mapArg ObjArr{oaObj=Just (GuardExpr oe _)} = case exprPathM oe of
      (n, m) -> Just (n, (m, Nothing))
    mapArg _ = Nothing

exprWithMeta :: Meta m -> Expr m -> Expr m
exprWithMeta m (CExpr _ c)        = CExpr m c
exprWithMeta m (Value _ n)        = Value m n
exprWithMeta m (Arg _ n)          = Arg m n
exprWithMeta m (HoleExpr _ h)     = HoleExpr m h
exprWithMeta m (TupleApply _ b a) = TupleApply m b a
exprWithMeta m (VarApply _ b n v) = VarApply m b n v
exprWithMeta _ _                  = error "exprWithMeta with unsupported expr"

exprWithMetaType :: Type -> Expr m -> Expr m
exprWithMetaType t e = exprWithMeta (mWithType t (getExprMeta e)) e

-- objExpr version that uses objDupExpr
-- objExpr :: (Show m, MetaDat m) => Object e m -> e m
-- objExpr Object{objDupExpr} = objDupExpr

objExpr :: (Show m, MetaDat m) => Object Expr m -> Expr m
objExpr Object{deprecatedObjM=m, deprecatedObjVars=vars, deprecatedObjArgs=args, deprecatedObjPath=path, objDupExpr} = exprWithMeta m $ applyArgs $ applyVars $ Value emptyMetaN path
  where
    applyVars b = foldr (\(varName,  _) b' -> applyVar (varName, fromJust $ H.lookup varName vars) b') b (exprAppliedOrdVars objDupExpr)
    applyVar (varName, varVal) b = VarApply (emptyMetaE "var" b) b varName varVal

    applyArgs b = foldr (applyArg . ((\argName -> (argName, fromJust $ H.lookup argName args)) . oaObjPath)) b (exprAppliedArgs objDupExpr)
    applyArg (argName, (argM, Just argVal)) b = if H.null (deprecatedObjArgs argVal)
      then TupleApply (emptyMetaM "tupleApplyArg" argM) (emptyMetaE "appArg" b, b) (ObjArr (Just (GuardExpr (Arg emptyMetaN argName) Nothing)) ArgObj Nothing [] (Just (GuardExpr (Arg (deprecatedObjM argVal) (deprecatedObjPath argVal)) Nothing), argM))
      else TupleApply (emptyMetaM "tupleApplyArg" argM) (emptyMetaE "appArg" b, b) (ObjArr (Just (GuardExpr (Arg emptyMetaN argName) Nothing)) ArgObj Nothing [] (Just (GuardExpr (objExpr argVal) Nothing), argM))
    applyArg (argName, (argM, Nothing)) b = TupleApply (emptyMetaM "tupleApplyArg" argM) (emptyMetaE "appArg" b, b) (ObjArr (Just (GuardExpr (Arg argM argName) Nothing)) ArgObj Nothing [] (Nothing, emptyMetaM "argArr" argM))

objPath :: (Show m, MetaDat m) => Object Expr m -> TypeName
objPath = exprPath . objExpr

objM :: (Show m, MetaDat m) => Object Expr m -> Meta m
objM = getExprMeta . objExpr

objAppliedArgsMap :: (Show m, MetaDat m) => Object Expr m -> H.HashMap ArgName (Meta m, Maybe (Expr m))
objAppliedArgsMap = exprAppliedArgsMap . objExpr

objAppliedVars :: (Show m, MetaDat m) => Object Expr m -> H.HashMap TypeVarName (Meta m)
objAppliedVars = exprAppliedVars . objExpr

objArgs :: (Show m, MetaDat m) => Object Expr m -> H.HashMap ArgName [Meta m]
objArgs = exprArgs . objExpr

oaObjExpr :: (MetaDat m, ExprClass e, Show m, Show (e m)) => ObjArr e m -> e m
oaObjExpr ObjArr{oaObj=Just (GuardExpr e _)} = e
oaObjExpr oa = error $ printf "oaObjExpr with no input expression: %s" (show oa)

mapOAObjExpr :: (MetaDat m, ExprClass e, Show (e m)) => (e m -> e m) -> ObjArr e m -> ObjArr e m
mapOAObjExpr f oa@ObjArr{oaObj=Just (GuardExpr e g)} = oa{oaObj = Just (GuardExpr (f e) g)}
mapOAObjExpr _ oa@ObjArr{oaObj=Nothing} = oa

mapOAArrExpr :: (MetaDat m, ExprClass e, Show (e m)) => (e m -> e m) -> ObjArr e m -> ObjArr e m
mapOAArrExpr f oa@ObjArr{oaArr=(Just (GuardExpr e g), m)} = oa{oaArr = (Just (GuardExpr (f e) g), m)}
mapOAArrExpr _ oa = oa

oaObjPath :: (MetaDat m, ExprClass e, Show m, Show (e m)) =>ObjArr e m -> TypeName
oaObjPath = exprPath . oaObjExpr

exprToObj :: (MetaDat m, Show m, Hashable m) => ObjectBasis -> Maybe String -> Expr m -> Object Expr m
exprToObj basis doc e@(Value m path) = Object m basis H.empty H.empty doc e path
exprToObj basis doc e@(Arg m path) = Object m basis H.empty H.empty doc e path
exprToObj basis doc e@(TupleApply m' (_, baseExpr) arg@ObjArr{oaArr=argArr}) = baseObj{deprecatedObjM=m', deprecatedObjArgs=H.insert argName' argVal' (deprecatedObjArgs baseObj), objDupExpr=e}
  where
    baseObj = exprToObj basis doc baseExpr
    argName' = exprPath $ oaObjExpr arg
    argVal' = case argArr of
      (Nothing, _)    -> (getExprMeta $ oaObjExpr arg, Nothing)
      (Just (GuardExpr (HoleExpr holeM _) _), _) -> (holeM, Nothing)
      (Just (GuardExpr (AliasExpr HoleExpr{} argExpr@(Arg m argName)) _), _) -> (m, Just (Object m MatchObj H.empty H.empty Nothing argExpr argName))
      (Just (GuardExpr argE _), _) -> (getExprMeta argE, Just $ exprToObj basis Nothing argE)
exprToObj basis doc e@(VarApply m' baseExpr name varM) = baseObj{deprecatedObjM=m', deprecatedObjVars=H.insert name varM (deprecatedObjVars baseObj), objDupExpr=e}
  where
    baseObj = exprToObj basis doc baseExpr
exprToObj _ _ e = error $ printf "Not yet implemented exprToObj: %s" (show e)

asExprObjectMapItem :: (Show m, MetaDat m) => ObjectMapItem Expr m -> ExprObjectMapItem Expr m
asExprObjectMapItem (obj@Object{objBasis, objDoc}, annots, Just (Arrow m guard expr)) = ObjArr (Just (GuardExpr (objExpr obj) guard)) objBasis objDoc annots (fmap (`GuardExpr` Nothing) expr, m)
asExprObjectMapItem (obj@Object{objBasis, objDoc, deprecatedObjM}, annots, Nothing) = ObjArr (Just (GuardExpr (objExpr obj) Nothing)) objBasis objDoc annots (Nothing, deprecatedObjM)

toExprPrgm :: (MetaDat m, Show m) => Prgm Expr m -> ExprPrgm Expr m
toExprPrgm (objMap, classGraph, annots) = (map asExprObjectMapItem objMap, classGraph, annots)

fromExprObjectMapItem :: (MetaDat m, Show m, Hashable m) => ExprObjectMapItem Expr m -> ObjectMapItem Expr m
fromExprObjectMapItem (ObjArr (Just (GuardExpr obj guard)) basis doc as arr) = (exprToObj basis doc obj, as, arr')
  where
    arr' = case arr of
      (Just (GuardExpr e _), m)         -> Just $ Arrow m guard (Just e)
      (_, m) | isJust guard             -> Just $ Arrow m guard Nothing
      (_, m) | getMetaType m == topType -> Nothing
      (_, m)                            -> Just $ Arrow m guard Nothing
fromExprObjectMapItem oa = error $ printf "fromExprObjectMapItem with no input expression: %s" (show oa)

fromExprPrgm :: (MetaDat m, Show m, Hashable m) => ExprPrgm Expr m -> Prgm Expr m
fromExprPrgm (objMap, classGraph, annots) = (map fromExprObjectMapItem objMap, classGraph, annots)

type ArgMetaMap m = H.HashMap ArgName [Meta m]

-- |
-- The 'exprVarArgsWithSrc' is similar to the 'exprArgs' function.
-- It differs in that it accepts an additional partial type that is equivalent to the expression and it pull the corresponding parts of the partial matching the args in the expression
-- TODO: Make nonLinear by changing signature to H.HashMap ArgName ([Meta m], Type)
type ArgMetaMapWithSrc m = H.HashMap TypeVarAux ([Meta m], Type)
exprVarArgsWithSrc :: (Show m) => ClassGraph -> Expr m -> PartialType -> ArgMetaMapWithSrc m
exprVarArgsWithSrc _ CExpr{} _ = H.empty
exprVarArgsWithSrc _ Value{} _ = H.empty
exprVarArgsWithSrc _ HoleExpr{} _ = H.empty
exprVarArgsWithSrc _ Arg{} _ = H.empty
exprVarArgsWithSrc classGraph (AliasExpr base (Arg m n)) src = H.insert (TVArg n) ([m], singletonType src) (exprVarArgsWithSrc classGraph base src)
exprVarArgsWithSrc classGraph (AliasExpr base (Value m n)) src = H.insert (TVArg n) ([m], singletonType src) (exprVarArgsWithSrc classGraph base src)
exprVarArgsWithSrc classGraph (AliasExpr base alias) src = H.union (exprVarArgsWithSrc classGraph base src) (exprVarArgsWithSrc classGraph alias src)
exprVarArgsWithSrc classGraph (VarApply _ b n m) src = H.insert (TVVar n) ([m], H.lookupDefault topType n (ptVars src)) $ exprVarArgsWithSrc classGraph b src
exprVarArgsWithSrc classGraph (TupleApply _ (_, be) arg) src = H.union (exprVarArgsWithSrc classGraph be src) (fromArg arg)
  where
    fromArg ObjArr{oaObj=Just (GuardExpr obj _), oaArr=(Just (GuardExpr e _), _)} = case typeGetArg (exprPath obj) src of
      Just (UnionType srcArg) -> mergeMaps $ map (exprVarArgsWithSrc classGraph e) $ splitUnionType srcArg
      Just t@TopType{} -> (,t) <$> exprVarArgs e
      _ -> H.empty
    fromArg ObjArr{oaArr=(Just (GuardExpr e _), _)} = exprVarArgsWithSrc classGraph e src
    fromArg ObjArr {oaObj=Just (GuardExpr obj _), oaArr=(Nothing, _)} = case typeGetArg (exprPath obj) src of
      Just srcArg -> case exprPathM obj of
        (n, m) -> H.singleton (TVArg n) ([m], srcArg)
      Nothing     -> H.empty
    fromArg oa = error $ printf "Invalid oa %s" (show oa)

    mergeMaps [] = H.empty
    mergeMaps (x:xs) = foldr (H.intersectionWith (\(m1s, t1) (m2s, t2) -> (m1s ++ m2s, unionTypes classGraph t1 t2))) x xs

-- fullDest means to use the greatest possible type (after implicit).
-- Otherwise, it uses the minimal type that *must* be reached
arrowDestType :: (Show m, MetaDat m) => Bool -> ClassGraph -> PartialType -> ObjArr Expr m -> Type
arrowDestType fullDest classGraph src oa@ObjArr{oaArr=(oaArrExpr, oaM)} = case oaArrExpr >>= getExprArg . rgeExpr of
  Just _ -> fromMaybe (error "Unfound expr") expr'
  _      -> joined
  where
    vaenv = snd <$> exprVarArgsWithSrc classGraph (oaObjExpr oa) ((\(UnionType pl) -> head $ splitUnionType pl) $ substituteVars $ singletonType src)
    substitute = substituteWithVarArgEnv vaenv
    expr' = fmap (substitute . getExprType . rgeExpr) oaArrExpr
    arr' = substitute $ getMetaType oaM
    joined = if fullDest
      then unionTypes classGraph (fromMaybe bottomType expr') arr'
      else intersectTypes classGraph (fromMaybe topType expr') arr'

metaTypeVar :: Meta m -> Maybe TypeVarAux
metaTypeVar m = case getMetaType m of
  TypeVar v _ -> Just v
  _           -> Nothing

type MetaVarArgEnv m = H.HashMap TypeVarAux (Meta m)

isSubtypePartialOfWithObj :: (Show m, MetaDat m) => ClassGraph -> Object Expr m -> PartialType -> Type -> Bool
isSubtypePartialOfWithObj classGraph obj sub = isSubtypeOfWithObj classGraph obj (singletonType sub)

isSubtypeOfWithObj :: (Show m, MetaDat m) => ClassGraph -> Object Expr m -> Type -> Type -> Bool
isSubtypeOfWithObj classGraph obj = isSubtypeOfWithEnv classGraph (joinVarArgEnv (getMetaType <$> objAppliedVars obj) (unionAllTypes classGraph . fmap getMetaType <$> exprArgs (objExpr obj)))

isSubtypeOfWithObjSrc :: (Show m, MetaDat m) => ClassGraph -> PartialType -> ObjArr Expr m -> Type -> Type -> Bool
isSubtypeOfWithObjSrc classGraph srcType obj = isSubtypeOfWithEnv classGraph (snd <$> exprVarArgsWithSrc classGraph (oaObjExpr obj) srcType)

isSubtypePartialOfWithMetaEnv :: ClassGraph -> MetaVarArgEnv m -> PartialType -> Type -> Bool
isSubtypePartialOfWithMetaEnv classGraph vaenv sub = isSubtypeOfWithEnv classGraph (metaToTypeEnv vaenv) (singletonType sub)
  where
    metaToTypeEnv = fmap getMetaType

isSubtypeOfWithMetaEnv :: ClassGraph -> MetaVarArgEnv m -> Type -> Type -> Bool
isSubtypeOfWithMetaEnv classGraph vaenv = isSubtypeOfWithEnv classGraph (metaToTypeEnv vaenv)
  where
    metaToTypeEnv = fmap getMetaType
