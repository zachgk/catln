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
import qualified Data.HashMap.Strict as H
import           Data.UnionFind.ST

import           Syntax
import           TypeCheck.Common

makeBaseFEnv :: ST s (FEnv s)
makeBaseFEnv = return $ FEnv [] H.empty []
-- makeBaseFEnv = do
--   let env1 = FEnv [] H.empty []
--   let ops = [ ("+", rintType, [rintType, rintType])
--             , ("-", rintType, [rintType, rintType])
--             , ("*", rintType, [rintType, rintType])
--             , (">", rboolType, [rintType, rintType])
--             , ("<", rboolType, [rintType, rintType])
--             , (">=", rboolType, [rintType, rintType])
--             , ("<=", rboolType, [rintType, rintType])
--             , ("==", rboolType, [rintType, rintType])
--             , ("!=", rboolType, [rintType, rintType])
--             , ("&", rboolType, [rboolType, rboolType])
--             , ("~", rboolType, [rboolType])
--             ]
--   foldM f env1 ops
--   where f e (opName, retType, args) = do
--           p <- fresh (SType RawBottomType retType)
--           pargs <- forM args (fresh . SType RawBottomType )
--           return $ fInsert e opName (PntFun p pargs)

getPnt :: VarMeta s -> Pnt s
getPnt x = x

addErr :: FEnv s -> String -> FEnv s
addErr (FEnv cons pmap errs) newErr = FEnv cons pmap (newErr:errs)

addConstraints :: FEnv s -> [Constraint s] -> FEnv s
addConstraints (FEnv oldCons defMap errs) newCons = FEnv (newCons ++ oldCons) defMap errs

fLookup :: FEnv s -> String -> (Maybe (VObject s), FEnv s)
fLookup env@(FEnv _ pmap _) k = case H.lookup k pmap of
  Just v  -> (Just v, env)
  Nothing -> (Nothing, addErr env ("Failed to lookup " ++ k))

fInsert :: FEnv s -> String -> VObject s -> FEnv s
fInsert (FEnv cons pmap errs) k v = FEnv cons (H.insert k v pmap) errs

fReplaceMap :: FEnv s -> FEnv s -> FEnv s
fReplaceMap (FEnv cons _ errs1) (FEnv _ pmap errs2) = FEnv cons pmap (errs1 ++ errs2)

fromMetaP :: FEnv s -> PreMeta -> ST s (VarMeta s, Pnt s, FEnv s)
fromMetaP env (PreTyped mt) = do
  p <- fresh (SType RawBottomType mt)
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
fromExpr env (Var m name) = do
  (m', p, env') <- fromMetaP env m
  let env'' = case fLookup env' name of
              (Nothing, e)          -> e
              (Just (Object om _ _), e) -> addConstraints e [EqPoints p (getPnt om)]
   in return (Var m' name, env'')
fromExpr env1 (Tuple m name exprs) = do
  (m', p, env2) <- fromMetaP env1 m
  case fLookup env2 name of
    (Nothing, _)          -> error $ "Could not find tuple object " ++ name
    (Just (Object om _ _), e) -> do
      (exprVals, env3) <- mapMWithFEnv e fromExpr (map snd exprs)
      let exprs' = zip (map fst exprs) exprVals
      let env4 = addConstraints env3 [BoundedBy p (getPnt om), IsTupleOf (getPnt m') (map (getPnt . getExprMeta) exprVals)]
      return (Tuple m' name exprs', env4)

arrowAddScope :: FEnv s -> VObject s -> ST s (FEnv s)
arrowAddScope env1 (Object meta _ args) = do
  env2 <- foldM aux env1 args
  let argMetas = map snd args
  let env3 = addConstraints env2 [IsTupleOf meta argMetas]
  return env3
  where aux :: FEnv s -> (String, VarMeta s) -> ST s (FEnv s)
        aux e (n, m) = return $ fInsert e n (Object m n [])

fromArrow :: FEnv s -> PArrow -> ST s (VArrow s, FEnv s)
fromArrow env1 (Arrow m objName expr) = case fLookup env1 objName of
    (Nothing, _) -> error $ "Failed to find object " ++ objName
    (Just obj, env2) -> do
      env3 <- arrowAddScope env2 obj
      (vExpr, env4) <- fromExpr env3 expr
      (m', p, env5) <- fromMetaP env4 m
      let env6 = addConstraints env5 [ArrowTo (getPnt $ getExprMeta vExpr) p]
      let arrow' = Arrow m' objName vExpr
      return (arrow', fReplaceMap env6 env1)

addObjArg :: FEnv s -> (Name, PreMeta) -> ST s ((Name, VarMeta s), FEnv s)
addObjArg env (n, m) = do
  (m', env2) <- fromMeta env m
  return ((n, m'), env2)

addObject :: FEnv s -> PObject -> ST s (VObject s, FEnv s)
addObject env (Object m name args) = do
  (m', env1) <- fromMeta env m
  (args', env2) <- mapMWithFEnv env1 addObjArg args
  let obj' = Object m' name args'
  let env3 = fInsert env2 name obj'
  return (obj', env3)


fromPrgm :: FEnv s -> PPrgm -> ST s (VPrgm s, FEnv s)
fromPrgm env (objects, arrows) = do
  (objs', env') <- mapMWithFEnv env addObject objects
  (arrows', env'') <- mapMWithFEnv env' fromArrow arrows
  return ((objs', arrows'), env'')
