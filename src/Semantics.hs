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

import           Data.Maybe
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           Utils


getMetaType :: Meta m -> Type
getMetaType (Meta t _ _) = t

getMetaPos :: Meta m -> CodeRange
getMetaPos (Meta _ pos _) = pos

labelPosM :: String -> Meta m -> Meta m
labelPosM s (Meta t pos ext) = Meta t (labelPos s pos) ext

emptyMetaN :: (MetaDat m) => Meta m
emptyMetaN = Meta TopType Nothing emptyMetaDat

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

exprPath :: (ExprClass e) => e m -> TypeName
exprPath = fromMaybe (error "No exprPath found") . maybeExprPath

exprAppliedArgsMap :: (ExprClass e) => e m -> H.HashMap ArgName (Meta m, Maybe (e m))
exprAppliedArgsMap = H.fromList . mapMaybe fromTupleArg . exprAppliedArgs
  where
    fromTupleArg (TupleArgI m n)    = Just (n, (m, Nothing))
    fromTupleArg (TupleArgIO m n a) = Just (n, (m, Just a))
    fromTupleArg TupleArgO{}        = Nothing

exprWithMeta :: Meta m -> Expr m -> Expr m
exprWithMeta m (CExpr _ c)        = CExpr m c
exprWithMeta m (Value _ n)        = Value m n
exprWithMeta m (Arg _ n)          = Arg m n
exprWithMeta m (HoleExpr _ h)     = HoleExpr m h
exprWithMeta m (TupleApply _ b a) = TupleApply m b a
exprWithMeta m (VarApply _ b n v) = VarApply m b n v
exprWithMeta _ _                  = error "exprWithMeta with unsupported expr"

objExpr :: (Show m, MetaDat m) => Object m -> Expr m
objExpr Object{objBasis, deprecatedObjM=m, deprecatedObjVars=vars, deprecatedObjArgs=args, deprecatedObjPath=path} = exprWithMeta m $ applyArgs $ applyVars $ mkBase emptyMetaN path
  where
    mkBase = case objBasis of
      MatchObj -> Arg
      _        -> Value

    applyVars b = foldr applyVar b $ H.toList vars
    applyVar (varName, varVal) b = VarApply (emptyMetaE "" b) b varName varVal

    applyArgs b = foldr applyArg b $ H.toList args
    applyArg (argName, (argM, Just argVal)) b = if H.null (deprecatedObjArgs argVal)
      then TupleApply (emptyMetaM "tupleApplyArg" argM) (emptyMetaE "appArg" b, b) (TupleArgIO argM argName (Arg (deprecatedObjM argVal) (deprecatedObjPath argVal)))
      else TupleApply (emptyMetaM "tupleApplyArg" argM) (emptyMetaE "appArg" b, b) (TupleArgIO argM argName (objExpr argVal))
    applyArg (argName, (argM, Nothing)) b = TupleApply (emptyMetaM "tupleApplyArg" argM) (emptyMetaE "appArg" b, b) (TupleArgI argM argName)

objPath :: (Show m, MetaDat m) => Object m -> TypeName
objPath = exprPath . objExpr

objM :: (Show m, MetaDat m) => Object m -> Meta m
objM = getExprMeta . objExpr

objAppliedArgs :: (Show m, MetaDat m) => Object m -> [TupleArg Expr m]
objAppliedArgs = exprAppliedArgs . objExpr

objAppliedArgsMap :: (Show m, MetaDat m) => Object m -> H.HashMap ArgName (Meta m, Maybe (Expr m))
objAppliedArgsMap = exprAppliedArgsMap . objExpr

objAppliedVars :: (Show m, MetaDat m) => Object m -> H.HashMap TypeVarName (Meta m)
objAppliedVars = exprAppliedVars . objExpr

objArgs :: (Show m, MetaDat m) => Object m -> H.HashMap ArgName (Meta m)
objArgs = exprArgs . objExpr

eobjPath :: (MetaDat m, ExprClass e) => ExprObject e m -> TypeName
eobjPath = exprPath . eobjExpr

exprToObj :: (MetaDat m, Show m) => ObjectBasis -> Maybe String -> Expr m -> Object m
exprToObj basis doc (Value m path) = Object m basis H.empty H.empty doc path
exprToObj basis doc (TupleApply m' (_, baseExpr) arg) = baseObj{deprecatedObjM=m', deprecatedObjArgs=H.insert argName' argVal' (deprecatedObjArgs baseObj)}
  where
    baseObj = exprToObj basis doc baseExpr
    (argName', argVal') = case arg of
      (TupleArgI m n)    -> (n, (m, Nothing))
      (TupleArgIO _ n (HoleExpr holeM _)) -> (n, (holeM, Nothing))
      (TupleArgIO _ n (AliasExpr HoleExpr{} (Arg m argName))) -> (n, (m, Just (Object m MatchObj H.empty H.empty Nothing argName)))
      (TupleArgIO m n a) -> (n, (m, Just $ exprToObj basis Nothing a))
      TupleArgO{} -> error "Found TupleArgO in exprToObj"
exprToObj basis doc (VarApply m' baseExpr name varM) = baseObj{deprecatedObjM=m', deprecatedObjVars=H.insert name varM (deprecatedObjVars baseObj)}
  where
    baseObj = exprToObj basis doc baseExpr
exprToObj _ _ e = error $ printf "Not yet implemented exprToObj: %s" (show e)

asExprObjMap :: (Show m, MetaDat m) => ObjectMap Expr m -> ExprObjectMap Expr m
asExprObjMap = map asExprObjectMapItem
  where
    asExprObjectMapItem (obj, annots, arr) = (asExprObject obj, annots, arr)
    asExprObject obj@Object{objBasis, objDoc} = ExprObject objBasis objDoc (objExpr obj)

fromExprPrgm :: (MetaDat m, Show m) => ExprPrgm Expr m -> Prgm Expr m
fromExprPrgm (objMap, classGraph, annots) = (map fromExprObjectMapItem objMap, classGraph, annots)
  where
    fromExprObjectMapItem :: (MetaDat m, Show m) => ExprObjectMapItem Expr m -> ObjectMapItem Expr m
    fromExprObjectMapItem (obj, ann, arr) = (fromExprObject obj, ann, arr)

    fromExprObject :: (MetaDat m, Show m) => ExprObject Expr m -> Object m
    fromExprObject (ExprObject basis doc expr) = exprToObj basis doc expr

type ArgMetaMap m = H.HashMap ArgName (Meta m)
-- |
-- The 'formArgMetaMap' produces a map from the argument name to argument meta.
-- In an object where arguments are themselves objects, it would match the names used in those subobjects.
-- For example, in the object real(c=Complex(a, b)), the matched arguments are a and b.
formArgMetaMap :: (Show m, MetaDat m) => Object m -> ArgMetaMap m
formArgMetaMap = exprArgs . objExpr

type ArgMetaMapWithSrc m = H.HashMap ArgName (Meta m, Type)
-- |
-- The 'formArgMetaMapWithSrc' is similar to the 'formArgMetaMap' function.
-- It differs in that it accepts an additional partial type that is matched by the object and matches against that partial type.
formArgMetaMapWithSrc :: (Show m, MetaDat m) => ClassGraph -> Object m -> PartialType  -> ArgMetaMapWithSrc m
formArgMetaMapWithSrc _ (Object m _ _ args _ path) src | H.null args = H.singleton path (m, singletonType src)
formArgMetaMapWithSrc classGraph Object{deprecatedObjArgs} PartialType{ptArgs=srcArgs} = H.foldr (H.unionWith unionCombine) H.empty $ H.mapWithKey fromArg deprecatedObjArgs
  where
    unionCombine _ _ = error "Duplicate var matched"
    fromArg k (m, Nothing) = case H.lookup k srcArgs of
      Just srcArg -> H.singleton k (m, srcArg)
      Nothing     -> H.empty
    fromArg k (_, Just arg) = case H.lookup k srcArgs of
      Just (UnionType srcArg) -> mergeMaps $ map (formArgMetaMapWithSrc classGraph arg) $ splitUnionType srcArg
      Just TopType -> (,TopType) <$> formArgMetaMap arg
      Just _ -> H.empty
      Nothing -> H.empty
    mergeMaps [] = H.empty
    mergeMaps (x:xs) = foldr (H.intersectionWith (\(m1, t1) (_, t2) -> (m1, unionTypes classGraph t1 t2))) x xs

formVarMap :: ClassGraph -> Type -> TypeVarEnv
formVarMap classGraph (UnionType partialLeafs) = unionsWith (unionTypes classGraph) $ map ptVars $ splitUnionType partialLeafs
formVarMap _ _ = error $ printf "Unknown formVarMap"

-- fullDest means to use the greatest possible type (after implicit).
-- Otherwise, it uses the minimal type that *must* be reached
arrowDestType :: (ExprClass e, Show m, MetaDat m) => Bool -> ClassGraph -> PartialType -> Object m -> Arrow e m -> Type
arrowDestType fullDest classGraph src obj (Arrow arrM _ maybeExpr) = case mapM getExprArg maybeExpr of
  Just (Just _) -> fromMaybe (error "Unfound expr") expr'
  _             -> joined
  where
    varEnv = formVarMap classGraph $ intersectTypes classGraph (getMetaType $ objM obj) (singletonType src)
    argEnv = snd <$> formArgMetaMapWithSrc classGraph obj ((\(UnionType pl) -> head $ splitUnionType pl) $ substituteVars $ singletonType src)
    substitute = substituteVarsWithVarEnv varEnv . substituteArgsWithArgEnv argEnv
    expr' = fmap (substitute . getExprType) maybeExpr
    arr' = substitute $ getMetaType arrM
    joined = if fullDest
      then unionTypes classGraph (fromMaybe bottomType expr') arr'
      else intersectTypes classGraph (fromMaybe TopType expr') arr'

metaTypeVar :: Meta m -> Maybe TypeVarAux
metaTypeVar m = case getMetaType m of
  TypeVar v -> Just v
  _         -> Nothing


isSubtypePartialOfWithObj :: (Show m, MetaDat m) => ClassGraph -> Object m -> PartialType -> Type -> Bool
isSubtypePartialOfWithObj classGraph obj sub = isSubtypeOfWithObj classGraph obj (singletonType sub)

isSubtypeOfWithObj :: (Show m, MetaDat m) => ClassGraph -> Object m -> Type -> Type -> Bool
isSubtypeOfWithObj classGraph obj = isSubtypeOfWithEnv classGraph (getMetaType <$> objAppliedVars obj) (getMetaType <$> formArgMetaMap obj)

isSubtypeOfWithObjSrc :: (Show m, MetaDat m) => ClassGraph -> PartialType -> Object m -> Type -> Type -> Bool
isSubtypeOfWithObjSrc classGraph srcType obj = isSubtypeOfWithEnv classGraph (getMetaType <$> objAppliedVars obj) (snd <$> formArgMetaMapWithSrc classGraph obj srcType )

isSubtypeOfWithMaybeObj :: (Show m, MetaDat m) => ClassGraph -> Maybe (Object m) -> Type -> Type -> Bool
isSubtypeOfWithMaybeObj classGraph (Just obj) = isSubtypeOfWithObj classGraph obj
isSubtypeOfWithMaybeObj classGraph Nothing    = isSubtypeOf classGraph
