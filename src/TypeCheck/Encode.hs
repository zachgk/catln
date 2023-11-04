--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Encode
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module encodes before type checking to extract all of the
-- types into isolated variables and all of the constraints that
-- relate the variables together. It prepares for typechecking.
--------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module TypeCheck.Encode where

import           Control.Monad
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as H
import qualified Data.IntMap.Lazy    as IM
import           Prelude             hiding (unzip)

import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           TypeCheck.Common
import           TypeCheck.TypeUtils (addUnionObjToEnv)

-- represents how a specified variables corresponds to the known types.
-- It could be a lower bound, upper bound, or exact bound
data TypeBound = BUpper | BLower | BEq
  deriving (Eq)

data EncodeState
  = EncodeOut !(Maybe VObject) !VMetaVarArgEnv -- ^ Used for outputs including in definitions (with object) and global annots (without obj)
  | EncodeIn !VMetaVarArgEnv -- ^ Used for inputs
  deriving (Eq, Ord, Show)

isEncodeOut :: EncodeState -> Bool
isEncodeOut EncodeOut{} = True
isEncodeOut EncodeIn{}  = False

asEncodeIn :: EncodeState -> EncodeState
asEncodeIn (EncodeOut _ vaenv) = EncodeIn vaenv
asEncodeIn est@EncodeIn{}      = est

encodeObj :: EncodeState -> Maybe VObject
encodeObj (EncodeOut obj _) = obj
encodeObj EncodeIn{}        = Nothing

encodeVarArgs :: EncodeState -> VMetaVarArgEnv
encodeVarArgs (EncodeOut _ vaenv) = vaenv
encodeVarArgs (EncodeIn vaenv)    = vaenv

makeBaseFEnv :: ClassGraph -> FEnv
makeBaseFEnv classGraph = FEnv{
  fePnts = IM.empty,
  feCons = [],
  feUnionAllObjs = Meta topType Nothing (VarMetaDat 0 Nothing),
  feVTypeGraph = H.empty,
  feTTypeGraph = H.empty,
  feUpdatedDuringEpoch = False,
  feClassGraph = classGraph,
  feDefMap = H.empty,
  feTrace = [[]]
  }

mkVarMetaDat :: EncodeState -> Pnt -> VarMetaDat
mkVarMetaDat (EncodeOut obj _) p = VarMetaDat p obj
mkVarMetaDat (EncodeIn _) p      = VarMetaDat p Nothing

fromMeta :: FEnv -> TypeBound -> EncodeState -> PreMeta -> String -> TypeCheckResult (VarMeta, FEnv)
fromMeta env bound est m description  = do
  let m' = case est of
        -- For in meta, use clearMetaArgTypes
        EncodeIn{}  -> clearMetaArgTypes m

        -- For arrow meta, can use directly
        EncodeOut{} -> m
  let tp = getMetaType m'
  let (p, env') = case bound of
        BUpper -> fresh env (TypeCheckResult [] $ SType tp topType description)
        BLower -> fresh env (TypeCheckResult [] $ SType topType tp description)
        BEq    -> fresh env (TypeCheckResult [] $ SType tp tp description)
  return (mapMetaDat (\_ -> mkVarMetaDat est p) m', env')

-- TODO: This might reverse the list to return.
mapMWithFEnv :: FEnv -> (FEnv -> a -> TypeCheckResult (b, FEnv)) -> [a] -> TypeCheckResult ([b], FEnv)
mapMWithFEnv env f = foldM f' ([], env)
  where f' (acc, e) a = do
          (b, e') <- f e a
          return (b:acc, e')

mapMWithFEnvMaybe :: FEnv -> (FEnv -> a -> TypeCheckResult (b, FEnv)) -> Maybe a -> TypeCheckResult (Maybe b, FEnv)
mapMWithFEnvMaybe env _ Nothing = return (Nothing, env)
mapMWithFEnvMaybe env f (Just a) = do
  (b, env') <- f env a
  return (Just b, env')

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

fromExpr :: EncodeState -> FEnv -> PExpr -> TypeCheckResult (VExpr, FEnv)
fromExpr est env (CExpr m c) = do
  (m', env') <- fromMeta env BUpper est m ("Constant " ++ show c)
  return (CExpr m' c, addConstraints env' [EqualsKnown (encodeVarArgs est) m' (constantType c)])
fromExpr est@EncodeOut{} env1 (Value m name) = do
  (m', env2) <- fromMeta env1 BUpper est m ("Value " ++ name)
  lookupVal <- fLookup env2 (encodeObj est) name
  lookupConstraints <- case lookupVal of
    DefVar lookupM      -> return [EqPoints (encodeVarArgs est) m' lookupM]
    DefKnown lookupType -> return [BoundedByKnown (encodeVarArgs est) m' lookupType]
  return (Value m' name, addConstraints env2 lookupConstraints)
fromExpr est@EncodeIn{} env1 (Value m name) = do
  -- let m' = mWithType (typeVal $ PTypeName name) m
  (m'', env2) <- fromMeta env1 BUpper est m ("Value " ++ name)
  return (Value m'' name, env2)
-- fromObjExpr varEnv argEnv env1 (Value m name) = do
--   (m', env2) <- fromMeta env1 BUpper Nothing varEnv argEnv m ("Value " ++ name)
--   (objValue, env3) <- fromMeta env2 BUpper Nothing varEnv argEnv (Meta (typeVal (PRelativeName name)) (labelPos "objValue" $ getMetaPos m') emptyMetaDat) ("objValue" ++ name)
--   let env4 = fInsert env3 name (DefVar objValue)
--   return (Value m' name, env4)
fromExpr est@EncodeOut{} env1 (Arg m name) = do
  (m', env2) <- fromMeta env1 BUpper est m ("Arg " ++ name)
  let varM = Meta (TypeVar $ TVArg TVInt name) (getMetaPos m) emptyMetaDat
  (varM', env3) <- fromMeta env2 BUpper est varM $ "ArgVar " ++ name
  let argEnv = snd $ splitVarArgEnv $ encodeVarArgs est
  case H.lookup name argEnv of
    Nothing -> error $ printf "Could not find arg %s with argEnv %s and obj %s" name (show argEnv) (show $ encodeObj est)
    Just lookupArgs -> return (Arg varM' name, addConstraints env3 [EqPoints (encodeVarArgs est) m' lookupArgs])
fromExpr est@EncodeIn{} env1 (Arg m name) = do
  (m', env2) <- fromMeta env1 BUpper est m ("Arg " ++ name)
  return (Arg m' name, env2)
fromExpr est env1 (HoleExpr m hole) = do
  (m', env2) <- fromMeta env1 BUpper est m ("Hole " ++ show hole)
  return (HoleExpr m' hole, env2)
fromExpr est env1 (AliasExpr base alias) = do
  (base', env2) <- fromExpr est env1 base
  (alias', env3) <- fromExpr est env2 alias
  let env4 = addConstraints env3 [EqPoints (encodeVarArgs est) (getExprMeta base') (getExprMeta alias')]
  return (AliasExpr base' alias', env4)
fromExpr est env1 (TupleApply m (baseM, baseExpr) arg@ObjArr{oaObj, oaAnnots, oaArr}) = do
  (m', env2) <- fromMeta env1 BUpper est m $ printf "Tuple %s(%s) Meta" (show baseExpr) (show arg)
  (baseM', env3) <- fromMeta env2 BUpper est baseM $ printf "Tuple %s(%s) BaseMeta" (show baseExpr) (show arg)
  (baseExpr', env4) <- fromExpr est env3 baseExpr
  (oaObj', env5) <- case oaObj of
    Just (GuardExpr inExpr guardExpr) -> do
      (inExpr', env5a) <- fromExpr (asEncodeIn est) env4 inExpr
      (guardExpr', env5b) <- fromGuard est env5a guardExpr
      -- TODO: Add constraint that input expr' refined by guardExpr'
      -- For outputs, should instead verify in decode that it refined (is subtype)
      return (Just $ GuardExpr inExpr' guardExpr', env5b)
    Nothing -> return (Nothing, env4)
  (oaAnnots', env6) <- mapMWithFEnv env5 (fromExpr est) oaAnnots
  (oaArr', env7) <- case oaArr of
    Just (arrGuardExpr, arrM) -> do
      (arrM', env6a) <- fromMeta env6 BUpper est arrM $ printf "Tuple oaM %s(%s)" (show baseExpr) (show arg)
      case arrGuardExpr of
        Just (GuardExpr argExpr guardExpr) -> do
          (argExpr', env6b) <- fromExpr est env6a argExpr
          (guardExpr', env6c) <- fromGuard est env6b guardExpr
          return (Just (Just (GuardExpr argExpr' guardExpr'), arrM'), env6c)
        Nothing -> return (Just (Nothing, arrM'), env6a)
    Nothing -> return (Nothing, env6)
  let constraints = case (est, oaObj', oaArr') of
        (EncodeOut{}, Just (GuardExpr obj Nothing), Just (Just (GuardExpr argExpr' Nothing), arrM')) ->
          -- Output with (x=x)
          [
            ArrowTo (encodeVarArgs est) (getExprMeta baseExpr') baseM',
                        AddArg (encodeVarArgs est) (baseM', exprPath obj) m',
                        BoundedByObjs (encodeVarArgs est) m',
                        ArrowTo (encodeVarArgs est) (getExprMeta argExpr') arrM',
                        EqPoints (encodeVarArgs est) (getExprMeta obj) arrM',
                        PropEq (encodeVarArgs est) (m', TVArg TVInt $ exprPath obj) arrM'
                        ]
        (EncodeOut{}, Nothing, Just (Just (GuardExpr argExpr' Nothing), arrM')) ->
          -- Output with (x) infer
          [
            ArrowTo (encodeVarArgs est) (getExprMeta baseExpr') baseM',
                        AddInferArg (encodeVarArgs est) baseM' m',
                        BoundedByObjs (encodeVarArgs est) m',
                        ArrowTo (encodeVarArgs est) (getExprMeta argExpr') arrM'
                        ]
        (EncodeIn{}, Just (GuardExpr obj Nothing), Just (Just (GuardExpr argExpr' Nothing), arrM')) ->
          -- Input with (x=x)
          [
            EqPoints (encodeVarArgs est) (getExprMeta baseExpr') baseM',
                     AddArg (encodeVarArgs est) (baseM', exprPath obj) m',
                     EqPoints (encodeVarArgs est) (getExprMeta argExpr') arrM',
                     EqPoints (encodeVarArgs est) (getExprMeta obj) arrM',
                     PropEq (encodeVarArgs est) (m', TVArg TVInt $ exprPath obj) arrM'
                    ]
        (EncodeIn{}, Just (GuardExpr obj Nothing), Just (Nothing, arrM')) ->
          -- Input with (x -> T)
          [
         -- EqPoints (getExprMeta baseExpr') baseM',
                     AddArg (encodeVarArgs est) (baseM', exprPath obj) m',
                     EqPoints (encodeVarArgs est) (getExprMeta obj) arrM',
                     PropEq (encodeVarArgs est) (m', TVArg TVInt $ exprPath obj) (getExprMeta obj)
                    ]
        (EncodeIn{}, Just (GuardExpr obj Nothing), Nothing) ->
          -- Input with (x) matchable
          [
         -- EqPoints (encodeVarArgs est) (getExprMeta baseExpr') baseM',
                     AddArg (encodeVarArgs est) (baseM', exprPath obj) m',
                     PropEq (encodeVarArgs est) (m', TVArg TVInt $ exprPath obj) (getExprMeta obj)
                    ]
        _ -> error $ printf "Invalid fromExpr in %s mode for %s" (show est) (show arg)
  let env8 = addConstraints env7 constraints
  let arg' = arg{
        oaObj=oaObj',
        oaArr=oaArr',
        oaAnnots=oaAnnots'
        }
  return (TupleApply m' (baseM', baseExpr') arg', env8)
fromExpr est env1 (VarApply m baseExpr varName varVal) = do
  let baseName = printf "VarApply %s<%s = %s>" (show baseExpr) varName (show varVal) :: String
  (m', env2) <- fromMeta env1 BUpper est m $ printf "%s Meta" baseName
  (baseExpr', env3) <- fromExpr est env2 baseExpr
  (varVal', env4) <- fromMeta env3 BUpper est varVal $ printf "%s val" baseName
  let constraints = case est of
        EncodeOut{} -> [
                      ArrowTo (encodeVarArgs est) (getExprMeta baseExpr') m',
                      PropEq (encodeVarArgs est) (m', TVVar TVInt varName) varVal',
                      BoundedByObjs (encodeVarArgs est) m'
                       ]
        EncodeIn{} -> [
                      -- EqPoints (getExprMeta baseExpr') m',
                      PropEq (encodeVarArgs est) (m', TVVar TVInt varName) varVal'
                      ]
  let env5 = addConstraints env4 constraints
  return (VarApply m' baseExpr' varName varVal', env5)

fromGuard :: EncodeState -> FEnv -> Maybe PExpr -> TypeCheckResult (Maybe VExpr, FEnv)
fromGuard est env1 (Just expr) =  do
  (expr', env2) <- fromExpr est env1 expr
  let (bool, env3) = fresh env2 $ TypeCheckResult [] $ SType topType boolType "ifGuardBool"
  let bool' = Meta boolType (labelPos "bool" $ getMetaPos $ getExprMeta expr) (mkVarMetaDat est bool)
  return (Just expr', addConstraints env3 [ArrowTo (encodeVarArgs est) (getExprMeta expr') bool'])
fromGuard _ env Nothing = return (Nothing, env)

fromArrow :: EncodeState -> FEnv -> PArrow -> TypeCheckResult (VArrow, FEnv)
fromArrow est env1 (Arrow m aguard maybeExpr) = do
  -- User entered type is not an upper bound, so start with TopType always. The true use of the user entered type is that the expression should have an arrow that has a reachesTree cut that is within the user entered type.
  let Just obj@Object{deprecatedObjVars=objVars} = encodeObj est
  (mUserReturn', env2) <- fromMeta env1 BUpper est m (printf "Specified result from %s" (show $ objPath obj))
  (aguard', env3) <- fromGuard est env2 aguard
  case maybeExpr of
    Just expr -> do
      (m', env4) <- fromMeta env3 BUpper est (Meta topType (labelPos "res" $ getMetaPos m) emptyMetaDat) $ printf "Arrow result from %s" (show $ objPath obj)
      (vExpr, env5) <- fromExpr est env4 expr
      let env6 = case metaTypeVar m of
            Just (TVVar TVInt typeVarName) -> case suffixLookupInDict typeVarName objVars of
              Just varM -> addConstraints env5 [ArrowTo (encodeVarArgs est) (getExprMeta vExpr) varM]
              Nothing -> error "unknown type fromArrow"
            Just TVVar{} -> error "Bad TVVar (TVExt) in fromArrow"
            Just TVArg{} -> error "Bad TVArg in fromArrow"
            Nothing -> addConstraints env5 [ArrowTo (encodeVarArgs est) (getExprMeta vExpr) m', ArrowTo (encodeVarArgs est) (getExprMeta vExpr) mUserReturn']
      let arrow' = Arrow m' aguard' (Just vExpr)
      let env7 = fAddVTypeGraph env6 (objPath obj) (obj, arrow')
      return (arrow', env7)
    Nothing -> do
      let arrow' = Arrow mUserReturn' aguard' Nothing
      let env4 = fAddVTypeGraph env3 (objPath obj) (obj, arrow')
      return (arrow', env4)

fromObjectMap :: FEnv -> (VObject, VMetaVarArgEnv, [PCompAnnot], Maybe PArrow) -> TypeCheckResult ((VObject, [VCompAnnot], Maybe VArrow), FEnv)
fromObjectMap env1 (obj, vaenv, annots, arrow) = do
  let est = EncodeOut (Just obj) vaenv
  (arrow', env2) <- mapMWithFEnvMaybe env1 (fromArrow est) arrow
  (annots', env3) <- mapMWithFEnv env2 (fromExpr est) annots
  return ((obj, annots', arrow'), env3)

fromObjVar :: VarMeta -> String -> EncodeState -> FEnv -> (TypeVarName, PreMeta) -> TypeCheckResult ((TypeVarName, VarMeta), FEnv)
fromObjVar oM prefix est env1 (varName, m) = do
  (m', env2) <- fromMeta env1 BUpper est m (prefix ++ "." ++ varName)
  let env3 = addConstraints env2 [PropEq (encodeVarArgs est) (oM, TVVar TVInt varName) m']
  return ((varName, m'), env3)

addObjArg :: VarMeta -> String -> EncodeState -> FEnv -> (TypeName, PObjArg) -> TypeCheckResult ((TypeName, VObjArg), FEnv)
addObjArg oM prefix est env (n, (m, maybeSubObj)) = do
  let prefix' = prefix ++ "." ++ n
  -- requires a fakeObj to pull the type variables from
  let argBound = case maybeSubObj of
        Just{}  -> BUpper
        Nothing -> BEq
  (m', env2) <- fromMeta env argBound est m prefix'
  let env3 = addConstraints env2 [PropEq (encodeVarArgs est) (oM, TVArg TVInt n) m', BoundedByObjs (encodeVarArgs est) m']
  let env4 = case suffixLookupInDict n (snd $ splitVarArgEnv $ encodeVarArgs est) of
        Just varM -> addConstraints env3 [EqPoints (encodeVarArgs est) m' varM]
        Nothing   -> env3
  case maybeSubObj of
    Just subObj -> do
      (subObj'@Object{deprecatedObjM=subM}, env5) <- fromObjectRec prefix' True est env4 subObj
      return ((n, (m', Just subObj')), addConstraints env5 [ArrowTo (encodeVarArgs est) subM m'])
    Nothing -> return ((n, (m', Nothing)), env4)

-- Remove constraints on args from the main meta in an Object.
-- Those constraints are still applied by constraints with the args.
-- However, typeVars only work correctly from the arg version and not the main meta
-- Likewise, replace the type vars equal to vars with top type for now
clearMetaArgTypes :: PreMeta -> PreMeta
clearMetaArgTypes (Meta (UnionType partials) pos md) = Meta (UnionType $ joinUnionType $ map clearPartialTypeArgs $ splitUnionType partials) (labelPos "clear" pos) md
  where
    clearPartialTypeArgs partial@PartialType{ptArgs} = partial{ptArgs=fmap (const topType) ptArgs}
clearMetaArgTypes p = p

fromObjectRec :: String -> Bool -> EncodeState -> FEnv -> PObject -> TypeCheckResult (VObject, FEnv)
fromObjectRec prefix isObjArg est env (Object m basis vars args doc expr path) = do
  let prefix' = prefix ++ "." ++ path
  (m', env1) <- fromMeta env BUpper est (clearMetaArgTypes m) prefix'
  (vars', env2) <- mapMWithFEnvMapWithKey env1 (fromObjVar m' prefix' est) vars
  (args', env3) <- mapMWithFEnvMapWithKey env2 (addObjArg m' prefix' est) args
  (expr', env4) <- fromExpr est env3 expr
  let obj' = Object m' basis vars' args' doc expr' path
  (objValue, env5) <- fromMeta env4 BUpper est (Meta (typeVal (PTypeName path)) (labelPos "objValue" $ getMetaPos m) emptyMetaDat) ("objValue" ++ path)
  let env6 = fInsert env5 path (DefVar objValue)
  let env7 = addConstraints env6 [BoundedByObjs (encodeVarArgs est) m' | isObjArg]
  let env8 = addConstraints env7 [BoundedByKnown (encodeVarArgs est) m' (singletonType (PartialType (PTypeName path) (fmap (const topType) vars) (fmap (const topType) args) [] PtArgExact)) | basis == FunctionObj || basis == PatternObj]
  return (obj', env8)

fromObject ::  FEnv -> PObject -> TypeCheckResult (VObject, VMetaVarArgEnv, FEnv)
fromObject env1 obj = do
  let envEst = EncodeIn H.empty -- An EncodeState for computing varEnv and argEnv
  (varEnv, env2) <- mapMWithFEnvMap env1 (\e m -> fromMeta e BUpper envEst m "var") $ exprAppliedVars $ objExpr obj
  (argEnv, env3) <- mapMWithFEnvMap env2 (\e m -> fromMeta e BUpper envEst m "arg") $ fmap head $ exprArgs $ objExpr obj

  let vaenv = joinVarArgEnv varEnv argEnv
  let est = EncodeIn vaenv
  (obj', env4) <- fromObjectRec "Object" False est env3 obj
  return (obj', vaenv, env4)

-- Add all of the objects first for various expressions that call other top level functions
fromObjects :: FEnv -> (PObject, [PCompAnnot], Maybe PArrow) -> TypeCheckResult ((VObject, VMetaVarArgEnv, [PCompAnnot], Maybe PArrow), FEnv)
fromObjects env (obj, annots, arrow) = do
  (obj', vaenv, env1) <- fromObject env obj
  return ((obj', vaenv, annots, arrow), env1)

fromPrgm :: FEnv -> (PPrgm, [(VObject, VMetaVarArgEnv)]) -> TypeCheckResult (VPrgm, FEnv)
fromPrgm env1 ((objMap1, classGraph, annots), vobjs) = do
  let objMap2 = zipWith (\(_, oans, arrows) (vobj, vaenv) -> (vobj, vaenv, oans, arrows)) (reverse objMap1) vobjs
  (objMap3, env2) <- mapMWithFEnv env1 fromObjectMap objMap2
  (annots', env3) <- mapMWithFEnv env2 (fromExpr (EncodeOut Nothing H.empty)) annots
  return ((objMap3, classGraph, annots'), env3)

-- Add all of the objects first for various expressions that call other top level functions, from all programs
prepObjPrgm :: FEnv -> PPrgm -> TypeCheckResult ((PPrgm, [(VObject, VMetaVarArgEnv)]), FEnv)
prepObjPrgm env1 pprgm@(objMap1, _, _) = do
  (objMap2, env2) <- mapMWithFEnv env1 fromObjects objMap1
  return ((pprgm, map (\(obj, vaenv, _, _) -> (obj, vaenv)) objMap2), env2)

addTypeGraphArrow :: TObject -> FEnv -> TArrow -> TypeCheckResult ((), FEnv)
addTypeGraphArrow obj env arr = return ((), fAddTTypeGraph env (objPath obj) (obj, arr))

addTypeGraphObjects :: FEnv -> TObjectMapItem -> TypeCheckResult ((), FEnv)
addTypeGraphObjects env (obj, _, arrow) = do
  let objValue = typeVal (PTypeName (objPath obj))
  let env' = fInsert env (objPath obj) (DefKnown objValue)
  (_, env'') <- mapMWithFEnvMaybe env' (addTypeGraphArrow obj) arrow
  return ((), env'')

addTypeGraphPrgm :: FEnv -> TPrgm -> TypeCheckResult ((), FEnv)
addTypeGraphPrgm env (objMap, _, _) = do
  (_, env') <- mapMWithFEnv env addTypeGraphObjects objMap
  return ((), env')

fromPrgms :: FEnv -> [PPrgm] -> [TPrgm] -> TypeCheckResult ([VPrgm], FEnv)
fromPrgms env1 pprgms tprgms = do
  (_, env2) <- mapMWithFEnv env1 addTypeGraphPrgm tprgms
  (pprgmsWithVObjs, env3) <- mapMWithFEnv env2 prepObjPrgm pprgms
  (vprgms, env4) <- mapMWithFEnv env3 fromPrgm pprgmsWithVObjs
  let (vjoinObjMap, _, _) = mergePrgms vprgms
  let (tjoinObjMap, _, _) = mergePrgms tprgms
  let env5 = addUnionObjToEnv env4 vjoinObjMap tjoinObjMap
  return (vprgms, env5)
