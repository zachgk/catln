--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.TypeUtils
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module handles some type utilities for type checking.
-- It supports the 'ArrowTo' constraint to compute resulting types through the typeGraph
-- This module also computes what the Any
-- types are which result from joining the types of all objects.
--------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module TypeCheck.TypeUtils where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S

import           Control.Monad
import           Control.Monad.State
import           Data.Bifunctor      (Bifunctor (bimap))
import           Data.List           (intercalate)
import           Data.UUID           (nil)
import           MapMeta
import           Semantics
import           Semantics.Prgm
import           Semantics.TypeGraph
import           Semantics.Types
import           Text.Printf
import           TypeCheck.Common
import           TypeCheck.Show
import           Utils

-- |
-- The object precedence is used to avoid increasing the scope of objects accidentally.
-- For example, if a data type is defined, functions using that data type shouldn't change the valid arguments to it
-- Especially, not specifying bounds should not turn them into TopType.
-- Similarly, matches or patterns are less effective then functions.
-- TODO May need to differentiate top level of functions from inner levels
objectPrecedence :: (Show m, Show (e m)) => ObjArr e m -> [Int]
objectPrecedence ObjArr{oaBasis=TypeObj}=                             [1]
objectPrecedence ObjArr{oaBasis=FunctionObj, oaArr=Nothing}           = [2, 1] -- Type objects have priority
objectPrecedence ObjArr{oaBasis=FunctionObj, oaArr=Just (Nothing, _)} = [2, 2] -- Declaration objects have priority [2,2], better than definitions
objectPrecedence ObjArr{oaBasis=FunctionObj, oaArr=Just (Just{}, _)}  = [2, 3] -- Definition objects have priority [2,3]
objectPrecedence ObjArr{oaBasis=PatternObj}                           = [3]
objectPrecedence ObjArr{oaBasis=MatchObj}                             =   [4]
objectPrecedence ObjArr{oaBasis=ArgObj}                               =   [5]

-- | Finds the 'objectPrecedence' for all types
buildPrecedenceMap :: (Show m, MetaDat m) => ObjectMap Expr m -> H.HashMap TypeName [Int]
buildPrecedenceMap = fmap (minimum . map objectPrecedence) . H.fromListWith (++) . map (\oa -> (oaObjPath oa, [oa]))

-- |
-- Prunes an objectMap by precendence. If two objects share the same precendence, only the bigger one(s) will be kept.
-- This is used to ensure that the type of an object can't be changed by other usages, such as a data object by functions using that data
filterBestPrecedence :: (Show m, MetaDat m) => H.HashMap TypeName [Int] -> ObjectMap Expr m -> ObjectMap Expr m
filterBestPrecedence precedenceMap = filter (\oa -> objectPrecedence oa == H.lookupDefault (error "Could not find obj in union") (oaObjPath oa) precedenceMap)


-- | This creates 'feUnionAllObjs' and adds it to the 'FEnv'
addUnionObjToEnv :: VObjectMap -> TObjectMap -> StateT FEnv TypeCheckResult ()
addUnionObjToEnv vobjMap tobjMap = do
  FEnv{feTypeEnv} <- get
  let vobjMapRec = concatMap getRecursiveObjs vobjMap
  let tobjMapRec = concatMap getRecursiveObjs tobjMap

  -- Finds the best precedence for all each object name
  let vePrecedenceMap = buildPrecedenceMap vobjMapRec
  let tPrecedenceMap = buildPrecedenceMap tobjMapRec
  let precedenceMap = H.unionWith min vePrecedenceMap tPrecedenceMap

  -- Filter the objects to only those with the best precedence
  let vobjs' = filterBestPrecedence precedenceMap vobjMapRec
  let tobjs' = filterBestPrecedence precedenceMap tobjMapRec

  let vobjMetas = map (getExprMeta . oaObjExpr) vobjs'
  let tobjMetas = map (getMetaType . getExprMeta . oaObjExpr) tobjs'

  -- Builds vars to use for union and union powerset
  unionAllObjs <- fresh $ TypeCheckResult [] $ SType PTopType PTopType Nothing "unionAllObjs"
  unionAllObjsPs <- fresh $ TypeCheckResult [] $ SType PTopType PTopType Nothing "unionAllObjsPs"

  let mkVarMeta p = Meta PTopType Nothing nil (VarMetaDat (Just p) Nothing)

  -- Build a variable to store union of tobjs
  let typecheckedAllType = unionAllTypes feTypeEnv $ filter (not . isTypeVar) tobjMetas
  when (typecheckedAllType == PTopType) $ fail $ printf "Failed to deteremine dependency TopType. Found undetermined dependent objects: \n\t%s" (intercalate "\n\t" $ map (show . snd) $ filter ((==) PTopType . fst) $ zip tobjMetas tobjs')
  typecheckedAllObjs <- fresh $ TypeCheckResult [] $ SType typecheckedAllType PTopType Nothing "typecheckedAll"
  let typecheckedAllObjs' = mkVarMeta typecheckedAllObjs

  -- Builds metas to use for union and union powerset
  let unionAllObjs' = mkVarMeta unionAllObjs
  let unionAllObjsPs' = mkVarMeta unionAllObjsPs

  let constraints = [
        UnionOf 1 unionAllObjs' (typecheckedAllObjs' : vobjMetas),
        SetArgMode 2 True unionAllObjs' unionAllObjsPs'
        ]
  modify (\env -> env{feUnionAllObjs=unionAllObjsPs'})
  modify startConstrainBlock
  modify $ addConstraints constraints
  modify $ endConstraintBlock Nothing H.empty

-- | A helper for the 'AddInferArg' 'Constraint'
addInferArgToType :: FEnv -> TypeVarArgEnv -> Type -> Maybe Type
addInferArgToType _ _ PTopType = Nothing
addInferArgToType env@FEnv{feTypeEnv} vaenv t@TopType{} = addInferArgToType env vaenv (expandType feTypeEnv vaenv t)
addInferArgToType env vaenv (TypeVar t _) = case H.lookup t vaenv of
  Just t' -> addInferArgToType env vaenv t'
  Nothing -> error $ printf "Failed to find %s in addInferArgToType" (show t)
addInferArgToType env@FEnv{feTypeEnv} vaenv (UnionType partials) = Just $ unionAllTypes feTypeEnv partials'
  where
    partials' = map (addInferArgToPartial env vaenv) $ splitUnionType partials

addInferArgToPartial :: FEnv -> TypeVarArgEnv -> PartialType -> Type
addInferArgToPartial FEnv{feVTypeGraph, feTTypeGraph, feTypeEnv} _ partial@PartialType{ptName=name, ptArgs} = do
  let vtypeArrows = H.lookupDefault [] name feVTypeGraph
  let vTypes = unionAllTypes feTypeEnv $ map tryArrow vtypeArrows

  let ttypeArrows = H.lookupDefault [] name feTTypeGraph
  let tTypes = unionAllTypes feTypeEnv $ map tryArrow ttypeArrows

  unionTypes feTypeEnv vTypes tTypes
  where
    tryArrow :: (MetaDat m, Show m) => ObjArr Expr m -> Type
    tryArrow oa = if H.keysSet ptArgs `isSubsetOf` H.keysSet (exprAppliedArgsMap $ oaObjExpr oa)
      then UnionType $ joinUnionType $ map addArg $ S.toList $ S.difference (H.keysSet $ exprAppliedArgsMap $ oaObjExpr oa) (H.keysSet ptArgs)
      else BottomType
    addArg arg = partial{ptArgs=H.insertWith (unionTypes feTypeEnv) arg PTopType ptArgs}

-- TODO Look into making this run every epoch rather than per call to mkReachesEnv
buildTypeEnv :: FEnv -> TypeCheckResult FEnvTypeEnv
buildTypeEnv env@FEnv{feTypeEnv, feVTypeGraph, feTTypeGraph} = do

  -- Env (typeGraph) from variables
  feVTypeGraph' <- forM feVTypeGraph $ \objArrs -> do
    forM objArrs $ \voa -> do
      soa <- showObjArr env voa
      let soa' = mapMetaObjArr clearMetaDat Nothing soa
      return soa'

  let ttypeGraph' = H.map (map (mapMetaObjArr clearMetaDat Nothing)) feTTypeGraph
  return feTypeEnv{teTypeGraph=ObjArrTypeGraph $ H.unionWith (++) feVTypeGraph' ttypeGraph'}

mkReachesEnv :: FEnv -> RConstraint -> TypeCheckResult (ReachesEnv (ObjArrTypeGraph Expr ()))
mkReachesEnv env (Constraint maybeConOa stypeVaenv _) = do

  let vaenv = fmap (bimap stypeAct stypeAct) stypeVaenv

  -- Env (typeGraph) from args
  -- TODO Remove the call to head below to support nonLinear args
  let argVaenv = H.unions $ map (fmap head . snd . splitVarArgEnv . exprVarArgs . oaObjExpr) maybeConOa
  argVaenv' <- forM argVaenv $ \(inExpr, outM) -> do
    inExpr' <- showExpr env InputMeta inExpr
    outM' <- showM env ArrMeta outM
    return (mapMeta clearMetaDat InputMeta inExpr', outM')
  let argObjMap = concatMap (\(inExpr, outM) -> [ObjArr (Just inExpr) ArgObj Nothing [] (Just (Nothing, emptyMetaT (substituteWithVarArgEnv (fmap snd vaenv) (getMetaType outM))))]) $ H.elems argVaenv'

  -- final ReachesEnv
  let argTypeEnv = mkTypeEnv $ Prgm argObjMap (classGraphFromObjs argObjMap) []
  feTypeEnv' <- buildTypeEnv env
  return $ ReachesEnv (argTypeEnv <> feTypeEnv') (fmap snd vaenv) S.empty

arrowConstrainUbs :: FEnv -> RConstraint -> Type -> Type -> TypeCheckResult (Type, Type, Maybe ReachesTree)
arrowConstrainUbs env@FEnv{feUnionAllObjs} con PTopType dest@UnionType{} = do
  unionPnt <- descriptor env feUnionAllObjs
  case unionPnt of
    SType{stypeAct=unionUb@UnionType{}} -> do
      (src', dest', destRT') <- arrowConstrainUbs env con unionUb dest
      return (src', dest', destRT')
    _ -> return (PTopType, dest, Nothing)
arrowConstrainUbs _ _ PTopType dest = return (PTopType, dest, Nothing)
arrowConstrainUbs env@FEnv{feTypeEnv} con@Constraint{conVaenv} src@TopType{} dest = do
  arrowConstrainUbs env con (expandType feTypeEnv (fmap (stypeAct . snd) conVaenv) src) dest
arrowConstrainUbs env@FEnv{feTypeEnv} con@Constraint{conVaenv} src@TypeVar{} dest = do
  let src' = expandType feTypeEnv (fmap (stypeAct . snd) conVaenv) src
  case (src', dest) of
    (PTopType, PTopType) -> return (src, src, Nothing)
    _ -> do
      (_, cdest, destRT) <- arrowConstrainUbs env con src' dest
      return (src, cdest, destRT)
arrowConstrainUbs env@FEnv{feTypeEnv} con@Constraint{conVaenv} src@(UnionType srcPartials) dest = do
  reachesEnv <- mkReachesEnv env con
  let reaches' = reachesPartials reachesEnv $ splitUnionType srcPartials
  let vaenv' = fmap (stypeAct . snd) conVaenv
  let destByGraph = unionReachesTree feTypeEnv vaenv' reaches'
  let dest' = intersectTypes feTypeEnv dest destByGraph
  -- TODO Maybe filter srcPartials based on those reaching dest
  return (src, compactType feTypeEnv vaenv' dest', Just reaches')
