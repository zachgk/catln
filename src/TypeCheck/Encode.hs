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
-- makeBaseFEnv = return $ FEnv [] H.empty []
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
          p <- fresh (SType retType RawBottomType)
          pargs <- forM args (fresh . (`SType` RawBottomType))
          return $ fInsert e opName (Object p opName pargs)

addConstraints :: FEnv s -> [Constraint s] -> FEnv s
addConstraints (FEnv oldCons defMap errs) newCons = FEnv (newCons ++ oldCons) defMap errs

fInsert :: FEnv s -> String -> VObject s -> FEnv s
fInsert (FEnv cons pmap errs) k v = FEnv cons (H.insert k v pmap) errs

fReplaceMap :: FEnv s -> FEnv s -> FEnv s
fReplaceMap (FEnv cons _ errs1) (FEnv _ pmap errs2) = FEnv cons pmap (errs1 ++ errs2)

fromMetaP :: FEnv s -> PreMeta -> ST s (VarMeta s, Pnt s, FEnv s)
fromMetaP env (PreTyped mt) = do
  p <- fresh (SType mt RawBottomType)
  return (p, p, env)

fromMeta :: FEnv s -> PreMeta -> ST s (VarMeta s, FEnv s)
fromMeta env m = do
  (m', _, env') <- fromMetaP env m
  return (m', env')

mapMWithFEnv :: FEnv s -> (FEnv s -> a -> ST s (b, FEnv s)) -> [a] -> ST s ([b], FEnv s)
mapMWithFEnv env f = foldM f' ([], env)
  where f' (acc, e) a = do
          (b, e') <- f e a
          return (b:acc, e')

mapMWithFEnvMap :: (Eq k, Hashable k) => FEnv s -> (FEnv s -> a -> ST s (b, FEnv s)) -> H.HashMap k a -> ST s (H.HashMap k b, FEnv s)
mapMWithFEnvMap env f map = do
  (res, env2) <- mapMWithFEnv env f' (H.toList map)
  return (H.fromList res, env2)
  where
    f' e (k, a) = do
      (b, e2) <- f e a
      return ((k, b), e2)

mapMWithFEnvMapWithKey :: (Eq k, Hashable k) => FEnv s -> (FEnv s -> (k, a) -> ST s ((k, b), FEnv s)) -> H.HashMap k a -> ST s (H.HashMap k b, FEnv s)
mapMWithFEnvMapWithKey env f map = do
  (res, env2) <- mapMWithFEnv env f' (H.toList map)
  return (H.fromList res, env2)
  where
    f' e (k, a) = do
      ((k2, b), e2) <- f e (k, a)
      return ((k2, b), e2)

fromExpr :: FEnv s -> PExpr -> ST s (VExpr s, FEnv s)
fromExpr env (CExpr m (CInt i)) = do
  (m', p, env') <- fromMetaP env m
  return (CExpr m' (CInt i), addConstraints env' [EqualsKnown p rintType])
fromExpr env (CExpr m (CFloat f)) = do
  (m', p, env') <- fromMetaP env m
  return (CExpr m' (CFloat f), addConstraints env' [EqualsKnown p rfloatType])
fromExpr env (CExpr m (CStr s)) = do
  (m', p, env') <- fromMetaP env m
  return (CExpr m' (CStr s), addConstraints env' [EqualsKnown p rstrType])
fromExpr env1 (Tuple m name exprs) = do
  (m', p, env2) <- fromMetaP env1 m
  case fLookup env2 name of
    (Nothing, _)          -> error $ "Could not find tuple object " ++ name
    (Just (Object om _ _), env3) -> do
      (exprs', env4) <- mapMWithFEnvMap env3 fromExpr exprs
      convertExprMetas <- mapM (\_ -> fresh (SType RawTopType RawBottomType)) exprs
      let arrowArgConstraints = H.elems $ H.intersectionWith ArrowTo (fmap getPntExpr exprs') convertExprMetas
      let constraints = [BoundedBy p (getPnt om), IsTupleOf (getPnt m') convertExprMetas] ++ arrowArgConstraints
      let env5 = addConstraints env4 constraints
      return (Tuple m' name exprs', env5)

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
fromArrow obj env1 (Arrow m annots maybeExpr) = do
  env2 <- arrowAddScope env1 obj
  (m', p, env3) <- fromMetaP env2 m
  (annots', env4) <- mapMWithFEnv env3 fromAnnot annots
  case maybeExpr of
    Just expr -> do
      (vExpr, env5) <- fromExpr env4 expr
      let env6 = addConstraints env5 [ArrowTo (getPntExpr vExpr) p]
      let arrow' = Arrow m' annots' (Just vExpr)
      return (arrow', fReplaceMap env6 env1)
    Nothing -> return (Arrow m' annots' Nothing, fReplaceMap env4 env1)

fromObjectMap :: FEnv s -> (VObject s, [PArrow]) -> ST s ((VObject s, [VArrow s]), FEnv s)
fromObjectMap env1 (obj@(Object m name args), arrows) = do
  let env2 = fInsert env1 name obj
  (arrows', env3) <- mapMWithFEnv env2 (fromArrow obj) arrows
  return ((obj, arrows'), env3)

addObjArg :: FEnv s -> (Name, PreMeta) -> ST s ((Name, VarMeta s), FEnv s)
addObjArg env (n, m) = do
  (m', env2) <- fromMeta env m
  return ((n, m'), env2)

-- Add all of the objects first for various expressions references other top level functions
fromObject :: FEnv s -> (PObject, [PArrow]) -> ST s ((VObject s, [PArrow]), FEnv s)
fromObject env (Object m name args, arrows) = do
  (m', env1) <- fromMeta env m
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
