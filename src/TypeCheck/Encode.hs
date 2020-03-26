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

import           Syntax
import           TypeCheck.Common
import           TypeCheck.TypeGraph (buildTypeGraph)

makeBaseFEnv :: ST s (FEnv s)
makeBaseFEnv = return $ FEnv [] H.empty []

fReplaceMap :: FEnv s -> FEnv s -> FEnv s
fReplaceMap (FEnv cons _ errs1) (FEnv _ pmap errs2) = FEnv cons pmap (errs1 ++ errs2)

fromMetaP :: FEnv s -> PreMeta -> String -> ST s (VarMeta s, Pnt s, FEnv s)
fromMetaP env (PreTyped mt) description = do
  p <- fresh (Right $ SType mt rawBottomType description)
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

fromExpr :: FEnv s -> PExpr -> ST s (VExpr s, FEnv s)
fromExpr env (CExpr m (CInt i)) = do
  (m', p, env') <- fromMetaP env m ("Constant int " ++ show i)
  return (CExpr m' (CInt i), addConstraints env' [EqualsKnown p rintType])
fromExpr env (CExpr m (CFloat f)) = do
  (m', p, env') <- fromMetaP env m ("Constant float " ++ show f)
  return (CExpr m' (CFloat f), addConstraints env' [EqualsKnown p rfloatType])
fromExpr env (CExpr m (CStr s)) = do
  (m', p, env') <- fromMetaP env m ("Constant str " ++ s)
  return (CExpr m' (CStr s), addConstraints env' [EqualsKnown p rstrType])
fromExpr env1 (Value m name) = do
  (m', p, env2) <- fromMetaP env1 m ("Value " ++ name)
  case fLookup env2 name of
    (Nothing, _) -> error $ "Could not find value " ++ name
    (Just lookupM, env3) ->
      return (Value m' name, addConstraints env3 [EqPoints p (getPnt lookupM)])
fromExpr env1 (TupleApply m (baseM, baseExpr) args) = do
  (m', p, env2) <- fromMetaP env1 m "TupleApply Meta"
  (baseM', baseP, env3) <- fromMetaP env2 baseM "TupleApply BaseMeta"
  (baseExpr', env4) <- fromExpr env3 baseExpr
  (args', env5) <- mapMWithFEnvMap env4 fromExpr args
  convertExprMetas <- mapM (\_ -> fresh (Right $ SType RawTopType rawBottomType "Tuple converted expr meta")) args
  let arrowArgConstraints = H.elems $ H.intersectionWith ArrowTo (fmap getPntExpr args') convertExprMetas
  let tupleConstraints = H.elems $ H.mapWithKey (\name ceMeta -> PropEq (p, name) ceMeta) convertExprMetas
  let constraints = [ArrowTo (getPntExpr baseExpr') baseP, AddArgs (baseP, H.keysSet args) p] ++ tupleConstraints ++ arrowArgConstraints
  let env6 = addConstraints env5 constraints
  return (TupleApply m' (baseM', baseExpr') args', env6)

fromAnnot :: FEnv s -> PCompAnnot -> ST s (VCompAnnot s, FEnv s)
fromAnnot env1 (CompAnnot name args) = do
  (args', env2) <- mapMWithFEnvMap env1 fromExpr args
  return (CompAnnot name args', env2)

arrowAddScope :: FEnv s -> VObject s -> ST s (FEnv s)
arrowAddScope env1 (Object _ _ args) = foldM aux env1 $ H.toList args
  where aux :: FEnv s -> (String, VarMeta s) -> ST s (FEnv s)
        aux e (n, m) = return $ fInsert e n m

fromArrow :: VObject s -> FEnv s -> PArrow -> ST s (VArrow s, FEnv s)
fromArrow obj@(Object _ objName _) env1 (Arrow m annots maybeExpr) = do
  env2 <- arrowAddScope env1 obj
  (m', p, env3) <- fromMetaP env2 m ("Arrow result from " ++ show objName)
  (annots', env4) <- mapMWithFEnv env3 fromAnnot annots
  case maybeExpr of
    Just expr -> do
      (vExpr, env5) <- fromExpr env4 expr
      let env6 = addConstraints env5 [ArrowTo (getPntExpr vExpr) p]
      let arrow' = Arrow m' annots' (Just vExpr)
      return (arrow', fReplaceMap env6 env1)
    Nothing -> return (Arrow m' annots' Nothing, fReplaceMap env4 env1)

fromObjectMap :: FEnv s -> (VObject s, [PArrow]) -> ST s ((VObject s, [VArrow s]), FEnv s)
fromObjectMap env1 (obj, arrows) = do
  (arrows', env2) <- mapMWithFEnv env1 (fromArrow obj) arrows
  return ((obj, arrows'), env2)

addObjArg :: FEnv s -> (Name, PreMeta) -> ST s ((Name, VarMeta s), FEnv s)
addObjArg env (n, m) = do
  (m', env2) <- fromMeta env m ("Object argument " ++ n)
  return ((n, m'), env2)

-- Add all of the objects first for various expressions that call other top level functions
fromObject :: FEnv s -> (PObject, [PArrow]) -> ST s ((VObject s, [PArrow]), FEnv s)
fromObject env (Object m name args, arrows) = do
  (m', env1) <- fromMeta env m ("Object " ++ name ++ "")
  (args', env2) <- mapMWithFEnvMapWithKey env1 addObjArg args
  let obj' = Object m' name args'
  (objValue, env3) <- fromMeta env2 (PreTyped $ RawSumType (S.singleton (RawLeafType name H.empty)) H.empty) ("objValue" ++ name)
  let env4 = fInsert env3 name objValue
  return ((obj', arrows), env4)

fromPrgm :: FEnv s -> PPrgm -> ST s (VPrgm s, TypeGraph s, FEnv s)
fromPrgm env1 (objMap1, classMap) = do
  (objMap2, env2) <- mapMWithFEnv env1 fromObject $ H.toList objMap1
  (objMap3, env3) <- mapMWithFEnv env2 fromObjectMap objMap2
  (env4, typeGraph) <- buildTypeGraph env3 objMap3
  return ((objMap3, classMap), typeGraph, env4)
