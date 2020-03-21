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
import           Data.UnionFind.ST

import           Syntax
import           TypeCheck.Common
import           TypeCheck.TypeGraph (buildTypeGraph)

makeBaseFEnv :: ST s (FEnv s)
makeBaseFEnv = do
  let env1 = FEnv [] H.empty []
  let ops = [ ("+", rintType, H.fromList [("l", rintType), ("r", rintType)])
            , ("-", rintType, H.fromList [("l", rintType), ("r", rintType)])
            , ("*", rintType, H.fromList [("l", rintType), ("r", rintType)])
            , (">", rboolType, H.fromList [("l", rintType), ("r", rintType)])
            , ("<", rboolType, H.fromList [("l", rintType), ("r", rintType)])
            , (">=", rboolType, H.fromList [("l", rintType), ("r", rintType)])
            , ("<=", rboolType, H.fromList [("l", rintType), ("r", rintType)])
            , ("==", rboolType, H.fromList [("l", rintType), ("r", rintType)])
            , ("!=", rboolType, H.fromList [("l", rintType), ("r", rintType)])
            , ("&", rboolType, H.fromList [("l", rboolType), ("r", rboolType)])
            , ("~", rboolType, H.singleton "a" rboolType)
            ]
  foldM f env1 ops
  where f e (opName, retType, args) = do
          p <- fresh (SType retType rawBottomType ("Runtime operator " ++ opName))
          -- pargs <- forM args (fresh . (`SType` rawBottomType))
          pargs <- forM args (\arg -> fresh (SType arg rawBottomType ("Runtime operator " ++ opName ++ " argument " ++ show arg)))
          return $ fInsert e opName (Object p opName pargs)

addConstraints :: FEnv s -> [Constraint s] -> FEnv s
addConstraints (FEnv oldCons defMap errs) newCons = FEnv (newCons ++ oldCons) defMap errs

fInsert :: FEnv s -> String -> VObject s -> FEnv s
fInsert (FEnv cons pmap errs) k v = FEnv cons (H.insert k v pmap) errs

fReplaceMap :: FEnv s -> FEnv s -> FEnv s
fReplaceMap (FEnv cons _ errs1) (FEnv _ pmap errs2) = FEnv cons pmap (errs1 ++ errs2)

fromMetaP :: FEnv s -> PreMeta -> String -> ST s (VarMeta s, Pnt s, FEnv s)
fromMetaP env (PreTyped mt) description = do
  p <- fresh (SType mt rawBottomType description)
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
    (Just (Object om _ _), env3) ->
      return (Value m' name, addConstraints env3 [EqPoints (getPnt om) p])
fromExpr env1 (TupleApply m (baseM, baseExpr) args) = do
  (m', p, env2) <- fromMetaP env1 m "TupleApply Meta"
  (baseM', baseP, env3) <- fromMetaP env2 baseM "TupleApply BaseMeta"
  (baseExpr', env4) <- fromExpr env3 baseExpr
  (args', env5) <- mapMWithFEnvMap env4 fromExpr args
  convertExprMetas <- mapM (\_ -> fresh (SType RawTopType rawBottomType "Tuple converted expr meta")) args
  let arrowArgConstraints = H.elems $ H.intersectionWith ArrowTo (fmap getPntExpr args') convertExprMetas
  let constraints = [ArrowTo (getPntExpr baseExpr') baseP, IsTupleOf p convertExprMetas] ++ arrowArgConstraints -- TODO: Add constraints
  let env6 = addConstraints env5 constraints
  return (TupleApply m' (baseM', baseExpr') args', env6)
-- fromExpr env1 (Tuple m name exprs) = do
--   (m', p, env2) <- fromMetaP env1 m ("Tuple " ++ name)
--   case fLookup env2 name of
--     (Nothing, _)          -> error $ "Could not find tuple object " ++ name
--     (Just (Object om _ _), env3) -> do
--       (exprs', env4) <- mapMWithFEnvMap env3 fromExpr exprs
--       convertExprMetas <- mapM (\_ -> fresh (SType RawTopType rawBottomType "Tuple converted expr meta")) exprs
--       let arrowArgConstraints = H.elems $ H.intersectionWith ArrowTo (fmap getPntExpr exprs') convertExprMetas
--       let constraints = [BoundedBy p (getPnt om), IsTupleOf (getPnt m') convertExprMetas] ++ arrowArgConstraints
--       let env5 = addConstraints env4 constraints
--       return (Tuple m' name exprs', env5)

fromAnnot :: FEnv s -> PCompAnnot -> ST s (VCompAnnot s, FEnv s)
fromAnnot env1 (CompAnnot name args) = do
  (args', env2) <- mapMWithFEnvMap env1 fromExpr args
  return (CompAnnot name args', env2)

arrowAddScope :: FEnv s -> VObject s -> ST s (FEnv s)
arrowAddScope env1 (Object meta _ args) = do
  env2 <- foldM aux env1 $ H.toList args
  let env3 = addConstraints env2 [IsTupleOf meta args]
  return env3
  where aux :: FEnv s -> (String, VarMeta s) -> ST s (FEnv s)
        aux e (n, m) = return $ fInsert e n (Object m n H.empty)

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
fromObjectMap env1 (obj@(Object _ name _), arrows) = do
  let env2 = fInsert env1 name obj
  (arrows', env3) <- mapMWithFEnv env2 (fromArrow obj) arrows
  return ((obj, arrows'), env3)

addObjArg :: FEnv s -> (Name, PreMeta) -> ST s ((Name, VarMeta s), FEnv s)
addObjArg env (n, m) = do
  (m', env2) <- fromMeta env m ("Object argument " ++ n)
  return ((n, m'), env2)

-- Add all of the objects first for various expressions that call other top level functions
fromObject :: FEnv s -> (PObject, [PArrow]) -> ST s ((VObject s, [PArrow]), FEnv s)
fromObject env (Object m name args, arrows) = do
  (m', env1) <- fromMeta env m ("Object " ++ name ++ show args)
  (args', env2) <- mapMWithFEnvMapWithKey env1 addObjArg args
  let obj' = Object m' name args'
  let env3 = fInsert env2 name obj'
  return ((obj', arrows), env3)

fromPrgm :: FEnv s -> PPrgm -> ST s (VPrgm s, TypeGraph s, FEnv s)
fromPrgm env1 (objMap1, classMap) = do
  (objMap2, env2) <- mapMWithFEnv env1 fromObject $ H.toList objMap1
  (objMap3, env3) <- mapMWithFEnv env2 fromObjectMap objMap2
  (env4, typeGraph) <- buildTypeGraph env3 objMap3
  return ((objMap3, classMap), typeGraph, env4)
