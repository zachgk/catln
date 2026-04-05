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
buildPrecedenceMap (ObjectMap objMap) = fmap (minimum . map objectPrecedence . flatObjectMapItem) objMap

-- |
-- Prunes an objectMap by precendence. If two objects share the same precendence, only the bigger one(s) will be kept.
-- This is used to ensure that the type of an object can't be changed by other usages, such as a data object by functions using that data
filterBestPrecedence :: (Show m, MetaDat m) => H.HashMap TypeName [Int] -> ObjectMap Expr m -> ObjectMap Expr m
filterBestPrecedence precedenceMap = filterObjectMap (\oa -> objectPrecedence oa == H.lookupDefault (error $ printf "Could not find obj in union.\n\tObj: %s\n\tKeys: %s" (show oa) (show $ H.keys precedenceMap)) (makeAbsoluteName $ oaObjPath oa) precedenceMap)


-- | This creates 'feUnionAllObjs' and adds it to the 'FEnv'
addUnionObjToEnv :: VObjectMap -> TObjectMap -> StateT FEnv TypeCheckResult ()
addUnionObjToEnv vobjMap tobjMap = do
  FEnv{feTypeEnv} <- get
  let vobjMapRec = mconcat $ map getRecursiveObjs $ flatObjectMap vobjMap
  let tobjMapRec = mconcat $ map getRecursiveObjs $ flatObjectMap tobjMap

  -- Finds the best precedence for all each object name
  let vePrecedenceMap = buildPrecedenceMap vobjMapRec
  let tPrecedenceMap = buildPrecedenceMap tobjMapRec
  let precedenceMap = H.unionWith min vePrecedenceMap tPrecedenceMap

  -- Filter the objects to only those with the best precedence
  let vobjs' = filterBestPrecedence precedenceMap vobjMapRec
  let tobjs' = filterBestPrecedence precedenceMap tobjMapRec

  let vobjMetas = map (getExprMeta . oaObjExpr) $ flatObjectMap vobjs'
  let tobjMetas = map (getMetaType . getExprMeta . oaObjExpr) $ flatObjectMap tobjs'

  -- Builds vars to use for union and union powerset
  unionAllObjs <- fresh $ TypeCheckResult [] $ SType PTopType PTopType Nothing "unionAllObjs"
  unionAllObjsPs <- fresh $ TypeCheckResult [] $ SType PTopType PTopType Nothing "unionAllObjsPs"

  let mkVarMeta p = Meta PTopType Nothing nil (VarMetaDat (Just p) Nothing)

  -- Build a variable to store union of tobjs
  let typecheckedAllType = unionAllTypes feTypeEnv $ filter (not . isTypeVar) tobjMetas
  when (typecheckedAllType == PTopType) $ fail $ printf "Failed to deteremine dependency TopType. Found undetermined dependent objects: \n\t%s" (intercalate "\n\t" $ map (show . snd) $ filter ((==) PTopType . fst) $ zip tobjMetas $ flatObjectMap tobjs')
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
addInferArgToType _ _ (UnionType (Just _) NegPartials _ _) = Nothing
addInferArgToType env vaenv (TypeVar t _) = case H.lookup t vaenv of
  Just t' -> addInferArgToType env vaenv t'
  Nothing -> error $ printf "Failed to find %s in addInferArgToType" (show t)
addInferArgToType env@FEnv{feTypeEnv} vaenv (UnionType Nothing PosPartials partials []) = Just $ unionAllTypes feTypeEnv partials'
  where
    partials' = map (addInferArgToPartial env vaenv) $ splitUnionType partials
addInferArgToType _ _ _ = Nothing

addInferArgToPartial :: FEnv -> TypeVarArgEnv -> PartialType -> Type
addInferArgToPartial FEnv{feVTypeGraph=ObjectMap vtypeGraph, feTTypeGraph=ObjectMap ttypeGraph, feTypeEnv} _ partial@PartialType{ptName=name, ptArgs} = do
  let vtypeArrows = H.lookupDefault mempty name vtypeGraph
  let vTypes = tryOMI vtypeArrows

  let ttypeArrows = H.lookupDefault mempty name ttypeGraph
  let tTypes = tryOMI ttypeArrows

  unionTypes feTypeEnv vTypes tTypes
  where
    tryOMI :: (MetaDat m, Show m) => ObjectMapItem Expr m -> Type
    tryOMI omi = unionAllTypes feTypeEnv $ map tryArrow $ flatObjectMapItem omi

    tryArrow :: (MetaDat m, Show m) => ObjArr Expr m -> Type
    tryArrow oa = if H.keysSet ptArgs `isSubsetOf` H.keysSet (exprAppliedArgsMap $ oaObjExpr oa)
      then UnionType Nothing PosPartials (joinUnionType $ map addArg $ S.toList $ S.difference (H.keysSet $ exprAppliedArgsMap $ oaObjExpr oa) (H.keysSet ptArgs)) []
      else BottomType
    addArg arg = partial{ptArgs=H.insertWith (unionTypes feTypeEnv) arg PTopType ptArgs}

-- TODO Look into making this run every epoch rather than per call to mkReachesEnv
buildTypeEnv :: FEnv -> TypeCheckResult FEnvTypeEnv
buildTypeEnv env@FEnv{feTypeEnv, feVTypeGraph, feTTypeGraph} = do
  feVTypeGraph' <- mapMObjectMap buildVObjArr feVTypeGraph
  let ttypeGraph' = mapMetaObjectMap clearMetaDat feTTypeGraph
  return feTypeEnv{teTypeGraph=ObjArrTypeGraph (feVTypeGraph' <> ttypeGraph')}

  where
    -- Env (typeGraph) from variables
    buildVObjArr :: ObjArr Expr VarMetaDat -> TypeCheckResult (ObjArr Expr ())
    buildVObjArr voa = do
      soa <- showObjArr env voa
      let soa' = mapMetaObjArr clearMetaDat Nothing soa
      return soa'

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
  let argObjMap = objectMapFromList $ concatMap (\(inExpr, outM) -> [ObjArr (Just inExpr) ArgObj Nothing [] (Just (Nothing, emptyMetaT (substituteWithVarArgEnv (fmap snd vaenv) (getMetaType outM))))]) $ H.elems argVaenv'

  -- final ReachesEnv
  let argTypeEnv = mkTypeEnv $ Prgm argObjMap (classGraphFromObjs argObjMap) []
  feTypeEnv' <- buildTypeEnv env
  return $ ReachesEnv (argTypeEnv <> feTypeEnv') (fmap snd vaenv) S.empty

arrowConstrainUbs :: FEnv -> RConstraint -> Type -> Type -> TypeCheckResult (Type, Type, Maybe ReachesTree)
arrowConstrainUbs env@FEnv{feUnionAllObjs} con PTopType dest@(UnionType Nothing PosPartials _ _) = do
  unionPnt <- descriptor env feUnionAllObjs
  case unionPnt of
    SType{stypeAct=unionUb@(UnionType Nothing PosPartials _ _)} -> do
      (src', dest', destRT') <- arrowConstrainUbs env con unionUb dest
      return (src', dest', destRT')
    _ -> return (PTopType, dest, Nothing)
arrowConstrainUbs _ _ PTopType dest = return (PTopType, dest, Nothing)
arrowConstrainUbs _ _ src@(UnionType (Just _) NegPartials _ _) dest = return (src, dest, Nothing)
arrowConstrainUbs env con@Constraint{conVaenv} src@(TypeVar v _) dest = do
  let src' = H.lookupDefault PTopType v (fmap (stypeAct . snd) conVaenv)
  case (src', dest) of
    (PTopType, PTopType) -> return (src, src, Nothing)
    _ -> do
      (_, cdest, destRT) <- arrowConstrainUbs env con src' dest
      return (src, cdest, destRT)
arrowConstrainUbs env@FEnv{feTypeEnv} con@Constraint{conVaenv} src@(UnionType Nothing PosPartials srcPartials consts) dest = do
  reachesEnv <- mkReachesEnv env con
  let allPartials = splitUnionType srcPartials ++ map constantPartialType consts
  let reaches' = reachesPartials reachesEnv allPartials
  let vaenv' = fmap (stypeAct . snd) conVaenv
  let destByGraph = unionReachesTree feTypeEnv vaenv' reaches'
  let dest' = intersectTypes feTypeEnv dest destByGraph
  -- TODO Maybe filter srcPartials based on those reaching dest
  return (src, compactType feTypeEnv vaenv' dest', Just reaches')
arrowConstrainUbs _ _ src dest = return (src, dest, Nothing)
