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
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as H
import qualified Data.IntMap.Lazy as IM

import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           TypeCheck.Common
import           TypeCheck.TypeGraph (buildTypeEnv)

-- represents how a specified variables corresponds to the known types.
-- It could be a lower bound, upper bound, or exact bound
data TypeBound = BUpper | BLower | BEq
  deriving (Eq)

makeBaseFEnv :: ClassMap -> FEnv
makeBaseFEnv classMap = FEnv IM.empty [] ((0, 0), H.empty, classMap) H.empty

fromMetaNoObj :: FEnv -> TypeBound -> PreMeta -> String -> TypeCheckResult (VarMeta, FEnv)
fromMetaNoObj env bound m description  = do
  let (p, env') = case bound of
        BUpper -> fresh env (TypeCheckResult [] $ SType (getMetaType m) bottomType description)
        BLower -> fresh env (TypeCheckResult [] $ SType TopType (getMetaType m) description)
        BEq -> fresh env (TypeCheckResult [] $ SType (getMetaType m) (getMetaType m) description)
  return (VarMeta p m, env')

fromMeta :: FEnv -> TypeBound -> VObject -> PreMeta -> String -> TypeCheckResult (VarMeta, FEnv)
fromMeta env bound obj@(Object _ _ _ objVars _) m description  = case metaTypeVar m of
  Just tv@(TVVar varName) -> case H.lookup varName objVars of
    Just objVarM -> do
      let (p, env') = fresh env (TypeCheckResult [] $ SVar tv (getPnt objVarM))
      return (VarMeta p m, env')
    Nothing -> error $ "Unknown obj var: " ++ varName
  Just tv@(TVArg argName) -> case H.lookup argName $ formArgMetaMap obj of
    Just objArgM -> do
      let (p, env') = fresh env (TypeCheckResult [] $ SVar tv (getPnt objArgM))
      return (VarMeta p m, env')
    Nothing -> error $ "Unknown obj arg: " ++ argName
  Nothing -> fromMetaNoObj env bound m description

mapMWithFEnv :: FEnv -> (FEnv -> a -> TypeCheckResult (b, FEnv)) -> [a] -> TypeCheckResult ([b], FEnv)
mapMWithFEnv env f = foldM f' ([], env)
  where f' (acc, e) a = do
          (b, e') <- f e a
          return (b:acc, e')

mapMWithFEnvMap :: (Eq k, Hashable k) => FEnv -> (FEnv -> a -> TypeCheckResult (b, FEnv)) -> H.HashMap k a -> TypeCheckResult (H.HashMap k b, FEnv)
mapMWithFEnvMap env f hmap = do
  (res, env2) <- mapMWithFEnv env f' (H.toList hmap)
  return (H.fromList res, env2)
  where
    f' e (k, a) = do
      (b, e2) <- f e a
      return ((k, b), e2)

mapMWithFEnvMapWithKey :: (Eq k, Hashable k) => FEnv -> (FEnv -> (k, a) -> TypeCheckResult ((k, b), FEnv)) -> H.HashMap k a -> TypeCheckResult (H.HashMap k b, FEnv)
mapMWithFEnvMapWithKey env f hmap = do
  (res, env2) <- mapMWithFEnv env f' (H.toList hmap)
  return (H.fromList res, env2)
  where
    f' e (k, a) = do
      ((k2, b), e2) <- f e (k, a)
      return ((k2, b), e2)

fromExpr :: VArgMetaMap -> VObject -> FEnv -> PExpr -> TypeCheckResult (VExpr, FEnv)
fromExpr _ obj env (CExpr m (CInt i)) = do
  (m', env') <- fromMeta env BUpper obj m ("Constant int " ++ show i)
  return (CExpr m' (CInt i), addConstraints env' [EqualsKnown (getPnt m') intType])
fromExpr _ obj env (CExpr m (CFloat f)) = do
  (m', env') <- fromMeta env BUpper obj m ("Constant float " ++ show f)
  return (CExpr m' (CFloat f), addConstraints env' [EqualsKnown (getPnt m') floatType])
fromExpr _ obj env (CExpr m (CStr s)) = do
  (m', env') <- fromMeta env BUpper obj m ("Constant str " ++ s)
  return (CExpr m' (CStr s), addConstraints env' [EqualsKnown (getPnt m') strType])
fromExpr _ obj env1 (Value m name) = do
  (m', env2) <- fromMeta env1 BUpper obj m ("Value " ++ name)
  lookupM <- fLookup env2 name
  return (Value m' name, addConstraints env2 [EqPoints (getPnt m') (getPnt lookupM)])
fromExpr objArgs obj env1 (Arg m name) = do
  (m', env2) <- fromMeta env1 BUpper obj m ("Arg " ++ name)
  let varM = PreTyped $ TypeVar $  TVArg name
  (varM', env3) <- fromMeta env2 BUpper obj varM $ "ArgVar " ++ name
  case H.lookup name objArgs of
    Nothing -> error $ "Could not find arg " ++ name
    Just lookupArg -> return (Arg varM' name, addConstraints env3 [EqPoints (getPnt m') (getPnt lookupArg)])
fromExpr objArgs obj env1 (TupleApply m (baseM, baseExpr) args) = do
  (m', env2) <- fromMeta env1 BUpper obj m "TupleApply Meta"
  (baseM', env3) <- fromMeta env2 BUpper obj baseM "TupleApply BaseMeta"
  (baseExpr', env4) <- fromExpr objArgs obj env3 baseExpr
  (args', env5) <- mapMWithFEnvMap env4 (fromExpr objArgs obj) args
  (convertExprMetas, env6) <- mapMWithFEnvMap env5 (\e _ -> return $ fresh e (TypeCheckResult [] $ SType TopType bottomType "Tuple converted expr meta")) args
  let arrowArgConstraints = H.elems $ H.intersectionWith ArrowTo (fmap getPntExpr args') convertExprMetas
  let tupleConstraints = H.elems $ H.mapWithKey (\name ceMeta -> PropEq (getPnt m', name) ceMeta) convertExprMetas
  let constraints = [ArrowTo (getPntExpr baseExpr') (getPnt baseM'), AddArgs (getPnt baseM', H.keysSet args) (getPnt m'), BoundedByObjs BoundAllObjs (getPnt m')] ++ arrowArgConstraints ++ tupleConstraints
  let env7 = addConstraints env6 constraints
  return (TupleApply m' (baseM', baseExpr') args', env7)

fromAnnot :: VArgMetaMap -> VObject -> FEnv -> PCompAnnot -> TypeCheckResult (VCompAnnot, FEnv)
fromAnnot objArgs obj env1 (CompAnnot name args) = do
  (args', env2) <- mapMWithFEnvMap env1 (fromExpr objArgs obj) args
  return (CompAnnot name args', env2)

fromGuard :: VArgMetaMap -> VObject -> FEnv -> PGuard -> TypeCheckResult (VGuard, FEnv)
fromGuard objArgs obj env1 (IfGuard expr) =  do
  (expr', env2) <- fromExpr objArgs obj env1 expr
  let (bool, env3) = fresh env2 $ TypeCheckResult [] $ SType boolType bottomType "ifGuardBool"
  return (IfGuard expr', addConstraints env3 [ArrowTo (getPnt $ getExprMeta expr') bool])
fromGuard _ _ env ElseGuard = return (ElseGuard, env)
fromGuard _ _ env NoGuard = return (NoGuard, env)

fromArrow :: VObject -> FEnv -> PArrow -> TypeCheckResult (VArrow, FEnv)
fromArrow obj@(Object _ _ objName objVars _) env1 (Arrow m annots aguard maybeExpr) = do
  (m', env2) <- fromMeta env1 BUpper obj m ("Arrow result from " ++ show objName)
  let argMetaMap = formArgMetaMap obj
  (annots', env3) <- mapMWithFEnv env2 (fromAnnot argMetaMap obj) annots
  (aguard', env4) <- fromGuard argMetaMap obj env3 aguard
  case maybeExpr of
    Just expr -> do
      (vExpr, env5) <- fromExpr argMetaMap obj env4 expr
      let env6 = case metaTypeVar m of
            Just (TVVar typeVarName) -> case H.lookup typeVarName objVars of
              Just varM -> addConstraints env5 [ArrowTo (getPntExpr vExpr) (getPnt varM)]
              Nothing -> error "unknown type fromArrow"
            Just TVArg{} -> error "Bad TVArg in fromArrow"
            Nothing -> addConstraints env5 [ArrowTo (getPntExpr vExpr) (getPnt m')]
      let arrow' = Arrow m' annots' aguard' (Just vExpr)
      let env7 = fAddTypeGraph env6 objName (obj, arrow')
      return (arrow', env7)
    Nothing -> do
      let arrow' = Arrow m' annots' aguard' Nothing
      let env5 = fAddTypeGraph env4 objName (obj, arrow')
      return (arrow', env5)

fromObjectMap :: FEnv -> (VObject, [PArrow]) -> TypeCheckResult ((VObject, [VArrow]), FEnv)
fromObjectMap env1 (obj, arrows) = do
  (arrows', env2) <- mapMWithFEnv env1 (fromArrow obj) arrows
  return ((obj, arrows'), env2)

fromObjVar :: String -> FEnv -> (TypeVarName, PreMeta) -> TypeCheckResult ((TypeVarName, VarMeta), FEnv)
fromObjVar prefix env1 (varName, m) = do
  (m', env2) <- fromMetaNoObj env1 BUpper m (prefix ++ "." ++ varName)
  return ((varName, m'), env2)

addObjArg :: VObject -> VarMeta -> String -> H.HashMap TypeVarName VarMeta -> FEnv -> (TypeName, PObjArg) -> TypeCheckResult ((TypeName, VObjArg), FEnv)
addObjArg fakeObj objM prefix varMap env (n, (m, maybeSubObj)) = do
  let prefix' = prefix ++ "." ++ n
  -- requires a fakeObj to pull the type variables from
  let argBound = case maybeSubObj of
        Just{} -> BUpper
        Nothing -> BEq
  (m', env2) <- fromMeta env argBound fakeObj m prefix'
  let env3 = addConstraints env2 [PropEq (getPnt objM, n) (getPnt m'), BoundedByObjs BoundTypeObjs (getPnt m')]
  let env4 = case H.lookup n varMap of
        Just varM -> addConstraints env3 [EqPoints (getPnt m') (getPnt varM)]
        Nothing -> env3
  case maybeSubObj of
    Just subObj -> do
      (subObj'@(Object subM _ _ _ _), env5) <- fromObject prefix' True env4 subObj
      return ((n, (m', Just subObj')), addConstraints env5 [ArrowTo (getPnt subM) (getPnt m')])
    Nothing -> return ((n, (m', Nothing)), env4)

-- Remove constraints on args from the main meta in an Object.
-- Those constraints are still applied by constraints with the args.
-- However, typeVars only work correctly from the arg version and not the main meta
clearMetaArgTypes :: PreMeta -> PreMeta
clearMetaArgTypes (PreTyped (SumType partials)) = PreTyped $ SumType $ joinPartialLeafs $ map clearPartialTypeArgs $ splitPartialLeafs partials
  where clearPartialTypeArgs (partialName, partialVars, partialArgs) = (partialName, partialVars, fmap (const TopType) partialArgs)
clearMetaArgTypes p = p

fromObject :: String -> Bool -> FEnv -> PObject -> TypeCheckResult (VObject, FEnv)
fromObject prefix isObjArg env (Object m basis name vars args) = do
  let prefix' = prefix ++ "." ++ name
  (m', env1) <- fromMetaNoObj env BUpper (clearMetaArgTypes m) prefix'
  (vars', env2) <- mapMWithFEnvMapWithKey env1 (fromObjVar prefix') vars
  let fakeObjForArgs = Object m' basis name vars' H.empty
  (args', env3) <- mapMWithFEnvMapWithKey env2 (addObjArg fakeObjForArgs m' prefix' vars') args
  let obj' = Object m' basis name vars' args'
  (objValue, env4) <- fromMeta env3 BUpper obj' (PreTyped $ SumType $ joinPartialLeafs [(PTypeName name, H.empty, H.empty)]) ("objValue" ++ name)
  let env5 = fInsert env4 name objValue
  let env6 = addConstraints env5 [BoundedByObjs BoundAllObjs (getPnt m') | isObjArg]
  let env7 = addConstraints env6 [BoundedByKnown (getPnt m') (SumType $ joinPartialLeafs [(PTypeName name, fmap (const TopType) vars, fmap (const TopType) args)]) | basis == FunctionObj]
  return (obj', env7)

-- Add all of the objects first for various expressions that call other top level functions
fromObjectArrows :: FEnv -> (PObject, [PArrow]) -> TypeCheckResult ((VObject, [PArrow]), FEnv)
fromObjectArrows env (obj, arrows) = do
  (obj', env1) <- fromObject "Object" False env obj
  return ((obj', arrows), env1)

fromPrgm :: FEnv -> PPrgm -> TypeCheckResult (VPrgm, FEnv)
fromPrgm env1 (objMap1, classMap) = do
  (objMap2, env2) <- mapMWithFEnv env1 fromObjectArrows $ H.toList objMap1
  (objMap3, env3) <- mapMWithFEnv env2 fromObjectMap objMap2
  let env4 = buildTypeEnv env3 objMap3
  return ((objMap3, classMap), env4)
