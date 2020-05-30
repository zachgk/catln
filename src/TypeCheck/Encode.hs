--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Encode
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module TypeCheck.Encode where

import           Control.Monad
import           Control.Monad.ST
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet          as S
import           Data.UnionFind.ST

import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           TypeCheck.Common
import           TypeCheck.TypeGraph (buildTypeEnv)

makeBaseFEnv :: ST s (FEnv s)
makeBaseFEnv = return $ FEnv [] H.empty []

fromMetaP :: FEnv s -> PreMeta -> String -> ST s (VarMeta s, Pnt s, FEnv s)
fromMetaP env (PreTyped mt) description = do
  p <- fresh (TypeCheckResult [] $ SType mt rawBottomType description)
  return (p, p, env)

fromMeta :: FEnv s -> PreMeta -> String -> ST s (VarMeta s, FEnv s)
fromMeta env m description = do
  (m', _, env') <- fromMetaP env m description
  return (m', env')

mapMWithFEnv :: FEnv s -> (FEnv s -> a -> ST s (b, FEnv s)) -> [a] -> ST s ([b], FEnv s)
mapMWithFEnv env f = foldM f' ([], env)
  where f' (acc, e) a = do
          (b, e') <- f e a
          return (b:acc, e')

mapMWithFEnvMap :: (Eq k, Hashable k) => FEnv s -> (FEnv s -> a -> ST s (b, FEnv s)) -> H.HashMap k a -> ST s (H.HashMap k b, FEnv s)
mapMWithFEnvMap env f hmap = do
  (res, env2) <- mapMWithFEnv env f' (H.toList hmap)
  return (H.fromList res, env2)
  where
    f' e (k, a) = do
      (b, e2) <- f e a
      return ((k, b), e2)

mapMWithFEnvMapWithKey :: (Eq k, Hashable k) => FEnv s -> (FEnv s -> (k, a) -> ST s ((k, b), FEnv s)) -> H.HashMap k a -> ST s (H.HashMap k b, FEnv s)
mapMWithFEnvMapWithKey env f hmap = do
  (res, env2) <- mapMWithFEnv env f' (H.toList hmap)
  return (H.fromList res, env2)
  where
    f' e (k, a) = do
      ((k2, b), e2) <- f e (k, a)
      return ((k2, b), e2)

fromExpr :: VArgMetaMap s -> FEnv s -> PExpr -> ST s (VExpr s, FEnv s)
fromExpr _ env (CExpr m (CInt i)) = do
  (m', p, env') <- fromMetaP env m ("Constant int " ++ show i)
  return (CExpr m' (CInt i), addConstraints env' [EqualsKnown p rintType])
fromExpr _ env (CExpr m (CFloat f)) = do
  (m', p, env') <- fromMetaP env m ("Constant float " ++ show f)
  return (CExpr m' (CFloat f), addConstraints env' [EqualsKnown p rfloatType])
fromExpr _ env (CExpr m (CStr s)) = do
  (m', p, env') <- fromMetaP env m ("Constant str " ++ s)
  return (CExpr m' (CStr s), addConstraints env' [EqualsKnown p rstrType])
fromExpr _ env1 (Value m name) = do
  (m', p, env2) <- fromMetaP env1 m ("Value " ++ name)
  case fLookup env2 name of
    (Nothing, _) -> error $ "Could not find value " ++ name
    (Just lookupM, env3) ->
      return (Value m' name, addConstraints env3 [EqPoints p (getPnt lookupM)])
fromExpr objArgs env1 (Arg m name) = do
  (m', p, env2) <- fromMetaP env1 m ("Arg " ++ name)
  case H.lookup name objArgs of
    Nothing -> error $ "Could not find arg " ++ name
    Just lookupArg ->
      return (Arg m' name, addConstraints env2 [EqPoints p (getPnt lookupArg)])
fromExpr objArgs env1 (TupleApply m (baseM, baseExpr) args) = do
  (m', p, env2) <- fromMetaP env1 m "TupleApply Meta"
  (baseM', baseP, env3) <- fromMetaP env2 baseM "TupleApply BaseMeta"
  (baseExpr', env4) <- fromExpr objArgs env3 baseExpr
  (args', env5) <- mapMWithFEnvMap env4 (fromExpr objArgs) args
  convertExprMetas <- mapM (\_ -> fresh (TypeCheckResult [] $ SType RawTopType rawBottomType "Tuple converted expr meta")) args
  let arrowArgConstraints = H.elems $ H.intersectionWith ArrowTo (fmap getPntExpr args') convertExprMetas
  let tupleConstraints = H.elems $ H.mapWithKey (\name ceMeta -> PropEq (p, name) ceMeta) convertExprMetas
  let constraints = [ArrowTo (getPntExpr baseExpr') baseP, AddArgs (baseP, H.keysSet args) p, BoundedByObjs BoundAllObjs p] ++ arrowArgConstraints ++ tupleConstraints
  let env6 = addConstraints env5 constraints
  return (TupleApply m' (baseM', baseExpr') args', env6)

fromAnnot :: VArgMetaMap s -> FEnv s -> PCompAnnot -> ST s (VCompAnnot s, FEnv s)
fromAnnot objArgs env1 (CompAnnot name args) = do
  (args', env2) <- mapMWithFEnvMap env1 (fromExpr objArgs) args
  return (CompAnnot name args', env2)

fromGuard :: VArgMetaMap s -> FEnv s -> PGuard -> ST s (VGuard s, FEnv s)
fromGuard objArgs env1 (IfGuard expr) =  do
  (expr', env2) <- fromExpr objArgs env1 expr
  bool <- fresh $ TypeCheckResult [] $ SType rboolType rawBottomType "bool"
  return (IfGuard expr', addConstraints env2 [ArrowTo (getExprMeta expr') bool])
fromGuard _ env ElseGuard = return (ElseGuard, env)
fromGuard _ env NoGuard = return (NoGuard, env)

fromArrow :: VObject s -> FEnv s -> PArrow -> ST s (VArrow s, FEnv s)
fromArrow obj@(Object _ _ objName _) env1 (Arrow m annots aguard maybeExpr) = do
  (m', p, env2) <- fromMetaP env1 m ("Arrow result from " ++ show objName)
  let argMetaMap = formArgMetaMap obj
  (annots', env3) <- mapMWithFEnv env2 (fromAnnot argMetaMap) annots
  (aguard', env4) <- fromGuard argMetaMap env3 aguard
  case maybeExpr of
    Just expr -> do
      (vExpr, env5) <- fromExpr argMetaMap env4 expr
      let env6 = addConstraints env5 [ArrowTo (getPntExpr vExpr) p]
      let arrow' = Arrow m' annots' aguard' (Just vExpr)
      return (arrow', env6)
    Nothing -> return (Arrow m' annots' aguard' Nothing, env4)

fromObjectMap :: FEnv s -> (VObject s, [PArrow]) -> ST s ((VObject s, [VArrow s]), FEnv s)
fromObjectMap env1 (obj, arrows) = do
  (arrows', env2) <- mapMWithFEnv env1 (fromArrow obj) arrows
  return ((obj, arrows'), env2)

addObjArg :: VarMeta s -> String -> FEnv s -> (Name, PObjArg) -> ST s ((Name, VObjArg s), FEnv s)
addObjArg objM prefix env (n, (m, maybeSubObj)) = do
  let prefix' = prefix ++ "." ++ n
  (m', env2) <- fromMeta env m prefix'
  let env3 = addConstraints env2 [PropEq (getPnt objM, n) m', BoundedByObjs BoundTypeObjs m']
  case maybeSubObj of
    Just subObj -> do
      (subObj'@(Object subM _ _ _), env4) <- fromObject prefix' env3 subObj
      return ((n, (m', Just subObj')), addConstraints env4 [ArrowTo subM m'])
    Nothing -> return ((n, (m', Nothing)), env3)

fromObject :: String -> FEnv s -> PObject -> ST s (VObject s, FEnv s)
fromObject prefix env (Object m basis name args) = do
  let prefix' = prefix ++ "." ++ name
  (m', env1) <- fromMeta env m prefix'
  (args', env2) <- mapMWithFEnvMapWithKey env1 (addObjArg m' prefix') args
  let obj' = Object m' basis name args'
  (objValue, env3) <- fromMeta env2 (PreTyped $ RawSumType (S.singleton (RawLeafType name H.empty)) H.empty) ("objValue" ++ name)
  let env4 = fInsert env3 name objValue
  let env5 = addConstraints env4 [BoundedByObjs BoundAllObjs m']
  let env6 = addConstraints env5 [BoundedByKnown m' (RawSumType S.empty (H.singleton name [fmap (const RawTopType) args])) | basis /= PatternObj]
  return (obj', env6)

-- Add all of the objects first for various expressions that call other top level functions
fromObjectArrows :: FEnv s -> (PObject, [PArrow]) -> ST s ((VObject s, [PArrow]), FEnv s)
fromObjectArrows env (obj, arrows) = do
  (obj', env1) <- fromObject "Object" env obj
  return ((obj', arrows), env1)

fromPrgm :: FEnv s -> PPrgm -> ST s (VPrgm s, TypeEnv s, FEnv s)
fromPrgm env1 (objMap1, classMap) = do
  (objMap2, env2) <- mapMWithFEnv env1 fromObjectArrows $ H.toList objMap1
  (objMap3, env3) <- mapMWithFEnv env2 fromObjectMap objMap2
  (typeEnv, typeEnvConstraints) <- buildTypeEnv objMap3
  let env4 = addConstraints env3 typeEnvConstraints
  return ((objMap3, classMap), typeEnv, env4)
