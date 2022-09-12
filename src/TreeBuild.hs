--------------------------------------------------------------------
-- |
-- Module    :  TreeBuild
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is used to take a rewrite-based program and a desired
-- function or expression to execute and convert it into a fully
-- resolved format for executing. It is run by the interpreter
-- "Eval".
--------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module TreeBuild where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Maybe
import           Text.Printf

import           CRes
import           Control.Monad
import           Eval.Common
import           Syntax
import           Syntax.Prgm
import           Syntax.Types

type TBMeta = Typed
type TBExpr = Expr TBMeta
type TBCompAnnot = CompAnnot TBExpr
type TBObject = Object TBMeta
type TBGuard = Guard TBExpr
type TBArrow = Arrow TBExpr TBMeta
type TBObjectMap = ObjectMap TBExpr TBMeta
type TBPrgm = Prgm TBExpr TBMeta
type TBReplRes = ReplRes TBMeta

type VisitedArrows = S.HashSet ResArrowTree

leafsFromMeta :: TBMeta -> [PartialType]
leafsFromMeta (Typed TopType _) = error "leafFromMeta from TopType"
leafsFromMeta (Typed TypeVar{} _) = error "leafFromMeta from TypeVar"
leafsFromMeta (Typed (UnionType prodTypes) _) = splitUnionType prodTypes

-- Helper to replace matches with a single option with their result
buildMatch :: ResArrowTree -> Type -> H.HashMap PartialType ResArrowTree -> ResArrowTree
buildMatch m tp opts = case H.toList opts of
  [(_, t)] -> t
  _        -> ResArrowMatch m tp opts

buildTBEnv :: ResBuildEnv -> TBPrgm -> TBEnv
buildTBEnv primEnv prgm@(objMap, classGraph, _) = baseEnv
  where
    baseEnv = TBEnv "" (H.union primEnv resEnv) H.empty prgm classGraph
    resEnv = H.fromListWith (++) $ mapMaybe resFromMArrow objMap
    resFromMArrow (obj, annots, marrow) = marrow >>= resFromArrow obj annots
    resFromArrow obj annots arrow@(Arrow _ aguard expr) = case expr of
      Just _ -> Just (objPath obj, [(objLeaf, aguard, \input -> ResEArrow input obj annots arrow) | objLeaf <- leafsFromMeta (objM obj)])
      Nothing -> Nothing

buildExpr :: TBEnv -> ObjSrc -> TBExpr -> CRes ResArrowTree
buildExpr _ _ (CExpr _ c) = case c of
  (CInt i)   -> return $ ConstantArrow $ IntVal i
  (CFloat i) -> return $ ConstantArrow $ FloatVal i
  (CStr i)   -> return $ ConstantArrow $ StrVal i
buildExpr TBEnv{tbVals} _ (Value (Typed (UnionType prodTypes) pos) name) = case splitUnionType prodTypes of
    (_:_:_) -> CErr [MkCNote $ BuildTreeCErr pos $ "Found multiple types for value " ++ name ++ "\n\t" ++ show prodTypes]
    [] -> CErr [MkCNote $ BuildTreeCErr pos $ "Found no types for value " ++ name ++ " with type " ++ show prodTypes]
    [prodType@PartialType{ptName=PTypeName name'}] -> return $ case H.lookup prodType tbVals of
      Just val -> val
      Nothing  -> ResArrowTuple name' H.empty
    e -> error $ printf "Found unexpected value type in buildExpr: %s" (show e)
buildExpr TBEnv{tbClassGraph} (os, obj) (Arg (Typed (TypeVar (TVArg a)) _) name) = return $ ArgArrow (snd $ fromJust $ suffixLookupInDict a $ formArgMetaMapWithSrc tbClassGraph obj os) name
buildExpr _ _ (Arg (Typed tp _) name) = return $ ArgArrow tp name
buildExpr TBEnv{tbClassGraph} _ (TupleApply (Typed tp pos) (Typed baseType _, baseExpr) (TupleArgIO _ argName argExpr)) = case typesGetArg tbClassGraph argName tp of
    Nothing -> CErr [MkCNote $ BuildTreeCErr pos $ printf "Found no types for tupleApply %s with type %s and expr %s" (show baseExpr) (show tp) (show argExpr)]
    Just leafArgs -> do
      let baseBuild = ExprArrow baseExpr (getExprType baseExpr) baseType
      let argVal = ExprArrow argExpr (getExprType argExpr) leafArgs
      return $ ResArrowTupleApply baseBuild argName argVal
buildExpr _ _ e = error $ printf "Bad buildExpr %s" (show e)

envLookupTry ::  TBEnv -> ObjSrc -> VisitedArrows -> (TBExpr, Type) -> PartialType -> Type -> ResArrowTree -> CRes ResArrowTree
envLookupTry TBEnv{tbClassGraph} _ _ _ srcType destType resArrow | isSubtypeOf tbClassGraph (resArrowDestType tbClassGraph srcType resArrow) destType = return resArrow
envLookupTry _ _ visitedArrows _ _ _ resArrow | S.member resArrow visitedArrows = CErr [MkCNote $ BuildTreeCErr Nothing "Found cyclical use of function"]
envLookupTry env@TBEnv{tbClassGraph} objSrc visitedArrows ee srcType destType resArrow = do
  afterArrows <- traverse buildAfterArrows $ splitUnionType newLeafTypes
  return $ buildMatch resArrow destType (H.fromList afterArrows)
  where
    (UnionType newLeafTypes) = resArrowDestType tbClassGraph srcType resArrow
    visitedArrows' = S.insert resArrow visitedArrows
    objSrc' = case resArrow of
          (ResEArrow _ o _ _) -> (srcType, o)
          _                   -> objSrc
    buildAfterArrows = \leafType -> do
      v <- envLookup env objSrc' resArrow ee visitedArrows' leafType destType
      return (leafType, v)

-- This function takes a desired type, arrows with various inputs in sorted order of preference
-- It returns the map of what each arrow should cover for a match tree or a CErr
-- It is used for arrow declarations that require multiple arrows definitions to constitute
-- TODO: This would be drastically improved with type difference
completeTreeSet :: TBEnv -> PartialType -> [(PartialType, ResArrowTree)] -> CRes (H.HashMap PartialType ResArrowTree)
completeTreeSet TBEnv{tbClassGraph} fullPartial = aux H.empty bottomType
  where
    fullType = singletonType fullPartial
    aux accMap accType _ | isSubtypeOf tbClassGraph fullType accType = return accMap
    aux _ accType [] = CErr [MkCNote $ BuildTreeCErr Nothing $ printf "Could not find arrows equaling input %s \n\t Only found %s" (show fullType) (show accType)]
    aux accMap accType ((optType, optTree):opts) = do
      let accType' = intersectTypes tbClassGraph fullType $ unionTypes tbClassGraph accType (singletonType optType)

      -- next opt increases accumulation
      if not (isSubtypeOf tbClassGraph accType' accType)
        -- Add to accumulation
        -- TODO: Should use ((optType - accType) ∩ fullType) for insertion, otherwise order may not be correct in matching and match not precise
        then aux (H.insert optType optTree accMap) accType' opts
        -- Don't add to accumulation
        else aux accMap accType opts

data ArrowGuardGroup
  = NoGuardGroup PartialType ResArrowTree
  | CondGuardGroup [(PartialType, TBExpr, ResArrowTree)] (PartialType, ResArrowTree)
buildGuardArrows :: TBEnv -> ObjSrc -> ResArrowTree -> (TBExpr, Type) -> VisitedArrows -> PartialType -> Type -> [ArrowGuardGroup] -> CRes ResArrowTree
buildGuardArrows env obj input ee visitedArrows srcType destType guards = do
  treeOptions <- catCRes $ map buildGuard guards
  finalTrees <- completeTreeSet env srcType treeOptions
  return $ buildMatch input destType finalTrees
  where
    buildGuard (NoGuardGroup tp tree) = (tp,) <$> ltry tree
    buildGuard (CondGuardGroup ifs (elseTp, elseTree)) = do
      ifTreePairs <- forM ifs $ \(_, ifCond, ifThen@(ResEArrow _ o _ _)) -> do
            ifTree' <- buildExprImp env (srcType, o) ifCond (getExprType ifCond) boolType
            thenTree' <- ltry ifThen
            return ((ifTree', input, o), thenTree')
      elseTree' <- ltry elseTree
      return $ case ifTreePairs of
        [] -> (elseTp, elseTree')
        _  -> (elseTp, ResArrowCond destType ifTreePairs elseTree')
    ltry tree = envLookupTry env obj visitedArrows ee srcType destType tree

groupArrows :: ResArrowTree -> [ResBuildEnvItem] -> CRes [ArrowGuardGroup]
groupArrows = aux ([], Nothing)
  where
    aux ([], Nothing) _ [] = return []
    aux (ifs, Just els) _ [] = return [CondGuardGroup ifs els]
    aux (_, Nothing) _ [] = CErr [MkCNote $ BuildTreeCErr Nothing "No ElseGuards found"]
    aux acc input ((pt, NoGuard, t):bs) = do
      bs' <- aux acc input bs
      return $ NoGuardGroup pt (t input):bs'
    aux (ifs, els) input ((pt, IfGuard it, t):bs) = aux ((pt, it, t input):ifs, els) input bs
    aux (ifs, Nothing) input ((pt, ElseGuard, t):bs) = aux (ifs, Just (pt, t input)) input bs
    aux (_, Just{}) _ ((_, ElseGuard, _):_) = CErr [MkCNote $ BuildTreeCErr Nothing "Multiple ElseGuards found"]

findResArrows :: TBEnv -> PartialType -> Type -> CRes [ResBuildEnvItem]
findResArrows TBEnv{tbName, tbResEnv, tbClassGraph} srcType@PartialType{ptName=PTypeName srcName} destType = case suffixLookupInDict srcName tbResEnv of
  Just resArrowsWithName -> do
    let resArrows = filter (\(arrowType, _, _) -> not $ isBottomType $ intersectTypes tbClassGraph (singletonType srcType) (singletonType arrowType)) resArrowsWithName
    -- TODO: Sort resArrows by priority order before trying
    return resArrows
  Nothing -> CErr [MkCNote $ BuildTreeCErr Nothing $ printf "Failed to find any arrows:\n\tWhen building %s\n\tfrom %s to %s" tbName (show srcType) (show destType)]
findResArrows _ PartialType{ptName=PClassName{}} _ = error "Can't findResArrows for class"
findResArrows _ PartialType{ptName=PRelativeName{}} _ = error "Can't findResArrows for relative name"

envLookup :: TBEnv -> ObjSrc -> ResArrowTree -> (TBExpr, Type) -> VisitedArrows -> PartialType -> Type -> CRes ResArrowTree
envLookup TBEnv{tbClassGraph} _ input _ _ srcType destType | isSubtypePartialOf tbClassGraph srcType destType = return input
envLookup env obj input ee visitedArrows srcType destType = do
  resArrows <- findResArrows env srcType destType
  guards <- groupArrows input resArrows
  buildGuardArrows env obj input ee visitedArrows srcType destType guards

buildImplicit :: TBEnv -> ObjSrc -> TBExpr -> Type -> Type -> CRes ResArrowTree
buildImplicit _ _ expr srcType TopType = return $ ExprArrow expr srcType srcType
buildImplicit _ obj _ TopType destType = error $ printf "Build implicit from top type to %s in %s" (show destType) (show obj)
buildImplicit env objSrc@(_, obj) input (TypeVar (TVVar varName)) destType = case suffixLookupInDict varName $ objAppliedVars obj of
  Just objVarM -> buildImplicit env objSrc input (getMetaType objVarM) destType
  Nothing -> error $ printf "buildImplicit unknown arg %s with obj %s" varName (show objSrc)
buildImplicit env@TBEnv{tbClassGraph} objSrc@(os, obj) input (TypeVar (TVArg argName)) destType = case suffixLookupInDict argName $ formArgMetaMapWithSrc tbClassGraph obj os of
  Just (_, srcType) -> buildImplicit env objSrc input srcType destType
  Nothing -> error $ printf "buildImplicit unknown arg %s with obj %s" argName (show obj)
buildImplicit env obj expr srcType@(UnionType srcTypeLeafs) destType = do
  let wholeInput = ExprArrow expr srcType srcType
  matchVal <- forM (splitUnionType srcTypeLeafs) $ \leafSrcType -> do
    let leafInputType = singletonType leafSrcType
    let leafInput = ExprArrow expr leafInputType leafInputType
    v <- envLookup env obj leafInput (expr, leafInputType) S.empty leafSrcType destType
    return (leafSrcType, v)
  return (buildMatch wholeInput destType (H.fromList matchVal))

-- executes an expression and then an implicit to a desired dest type
buildExprImp :: TBEnv -> ObjSrc -> TBExpr -> Type -> Type -> CRes ResArrowTree
buildExprImp env@TBEnv{tbClassGraph} objSrc@(os, obj) expr exprType destType = do
  res' <- if isSubtypeOfWithObjSrc tbClassGraph os obj exprType destType
    then buildExpr env objSrc expr
    else buildImplicit env objSrc expr exprType destType
  resolveTree env objSrc res'

-- builds all macroArrows and exprArrows into other arrow types
resolveTree :: TBEnv -> ObjSrc -> ResArrowTree -> CRes ResArrowTree
resolveTree env obj (ResEArrow input o annots a) = do
  input' <- resolveTree env obj input
  return $ ResEArrow input' o annots a
resolveTree env obj (PrimArrow input t f) = do
  input' <- resolveTree env obj input
  return $ PrimArrow input' t f
resolveTree env obj (MacroArrow input _ (MacroFunction f)) = do
  input' <- f input (macroData env obj)
  resolveTree env obj input'
resolveTree env obj (ExprArrow e exprType destType) = buildExprImp env obj e exprType destType
resolveTree _ _ a@ConstantArrow{} = return a
resolveTree _ _ a@ArgArrow{} = return a
resolveTree env obj (ResArrowMatch input tp matches) = do
  input' <- resolveTree env obj input
  matches' <- mapM (resolveTree env obj) matches
  return $ ResArrowMatch input' tp matches'
resolveTree env obj (ResArrowCond tp ifs elseTree) = do
  ifs' <- forM ifs $ \((ifCondTree, ifCondInput, ifObj), ifThenTree) -> do
    ifCondTree' <- resolveTree env obj ifCondTree
    ifCondInput' <- resolveTree env obj ifCondInput
    ifThenTree' <- resolveTree env obj ifThenTree
    return ((ifCondTree', ifCondInput', ifObj), ifThenTree')
  elseTree' <- resolveTree env obj elseTree
  return $ ResArrowCond tp ifs' elseTree'
resolveTree env obj (ResArrowTuple name args) = do
  args' <- mapM (resolveTree env obj) args
  return $ ResArrowTuple name args'
resolveTree env obj (ResArrowTupleApply input argName argVal) = do
  input' <- resolveTree env obj input
  argVal' <- resolveTree env obj argVal
  return $ ResArrowTupleApply input' argName argVal'


buildArrow :: TBEnv -> PartialType -> TBObject -> [TBCompAnnot] -> TBArrow -> CRes (Maybe (TBArrow, (ResArrowTree, [ResArrowTree])))
buildArrow _ _ _ _ (Arrow _ _ Nothing) = return Nothing
buildArrow env objPartial obj compAnnots arrow@(Arrow (Typed am _) _ (Just expr)) = do
  let env' = env{tbName = printf "arrow %s" (show obj)}
  let objSrc = (objPartial, obj)
  let am' = case am of
        (TypeVar (TVVar v)) -> case suffixLookupInDict v $ objAppliedVars obj of
          Just m  -> getMetaType m
          Nothing -> error "Bad TVVar in makeBaseEnv"
        (TypeVar (TVArg v)) -> case suffixLookupInDict v $ formArgMetaMap obj of
          Just argMeta -> getMetaType argMeta
          Nothing      -> error "Bad TVArg in makeBaseEnv"
        _ -> am
  resArrowTree <- resolveTree env' objSrc (ExprArrow expr (getExprType expr) am')
  compAnnots' <- forM compAnnots $ \annot -> do
                                       let annotEnv = env{tbName = printf "globalAnnot %s" (show annot)}
                                       let annotType = getExprType annot
                                       resolveTree annotEnv objSrc (ExprArrow annot annotType annotType)
  return $ Just (arrow, (resArrowTree, compAnnots'))

buildRoot :: TBEnv -> TBExpr -> PartialType -> Type -> CRes ResArrowTree
buildRoot env input src dest = do
  let env' = env{tbName = printf "root"}
  let emptyObj = Object (Typed (singletonType src) Nothing) FunctionObj H.empty H.empty Nothing "EmptyObj"
  let objSrc = (src, emptyObj)
  resolveTree env' objSrc (ExprArrow input (getExprType input) dest)
