--------------------------------------------------------------------
-- |
-- Module    :  TreeBuild
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module TreeBuild where

import           Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Maybe
import           Text.Printf

import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           CRes
import           Eval.Common
import Control.Monad

type TBMeta = Typed
type TBExpr = Expr TBMeta
type TBCompAnnot = CompAnnot TBExpr
type TBObject = Object TBMeta
type TBGuard = Guard TBExpr
type TBArrow = Arrow TBExpr TBMeta
type TBObjectMap = ObjectMap TBExpr TBMeta
type TBPrgm = Prgm TBExpr TBMeta
type TBReplRes = ReplRes TBMeta

type VisitedArrows f = S.HashSet (ResArrowTree f)

leafsFromMeta :: TBMeta -> [PartialType]
leafsFromMeta (Typed TopType _) = error "leafFromMeta from TopType"
leafsFromMeta (Typed TypeVar{} _) = error "leafFromMeta from TypeVar"
leafsFromMeta (Typed (SumType prodTypes) _) = splitPartialLeafs prodTypes

-- Helper to replace matches with a single option with their result
buildMatch :: ResArrowTree f -> Type -> H.HashMap PartialType (ResArrowTree f) -> ResArrowTree f
buildMatch m tp opts = case H.toList opts of
  [(_, t)] -> t
  _ -> ResArrowMatch m tp opts

buildTBEnv :: (Eq f, Hashable f) => ResBuildEnv f -> TBPrgm -> TBEnv f
buildTBEnv primEnv prgm@(objMap, classMap, _) = baseEnv
  where
    baseEnv = (H.union primEnv resEnv, H.empty, prgm, classMap)
    resEnv = H.fromListWith (++) $ concatMap resFromArrows objMap
    resFromArrows (obj, arrows) = mapMaybe (resFromArrow obj) arrows
    resFromArrow obj@(Object om _ objName _ _) arrow@(Arrow _ _ aguard expr) = case expr of
      Just _ -> Just (objName, [(objLeaf, aguard, \input -> ResEArrow input obj arrow) | objLeaf <- leafsFromMeta om])
      Nothing -> Nothing

buildExpr :: (Eq f, Hashable f) => TBEnv f -> TBExpr -> CRes (ResArrowTree f)
buildExpr _ (CExpr _ c) = case c of
  (CInt i) -> return $ ConstantArrow $ IntVal i
  (CFloat i) -> return $ ConstantArrow $ FloatVal i
  (CStr i) -> return $ ConstantArrow $ StrVal i
buildExpr (_, valEnv, _, _) (Value (Typed (SumType prodTypes) pos) name) = case splitPartialLeafs prodTypes of
    (_:_:_) -> CErr [MkCNote $ BuildTreeCErr pos $ "Found multiple types for value " ++ name ++ "\n\t" ++ show prodTypes]
    [] -> CErr [MkCNote $ BuildTreeCErr pos $ "Found no types for value " ++ name ++ " with type " ++ show prodTypes]
    [prodType] -> return $ case H.lookup prodType valEnv of
      Just val -> val
      Nothing -> ResArrowTuple name H.empty
buildExpr _ (Arg (Typed tp _) name) = return $ ArgArrow tp name
buildExpr (_, _, _, classMap) (TupleApply (Typed tp pos) (Typed baseType _, baseExpr) argName argExpr) = case typesGetArg classMap argName tp of
    Nothing -> CErr [MkCNote $ BuildTreeCErr pos $ printf "Found no types for tupleApply %s with type %s and expr %s" (show baseExpr) (show tp) (show argExpr)]
    Just leafArgs -> do
      let baseBuild = ExprArrow baseExpr (getMetaType $ getExprMeta baseExpr) baseType
      let argVal = ExprArrow argExpr (getMetaType $ getExprMeta argExpr) leafArgs
      return $ ResArrowTupleApply baseBuild argName argVal
buildExpr _ _ = error "Bad buildExpr"

envLookupTry :: (Eq f, Hashable f) => TBEnv f -> ObjSrc -> VisitedArrows f -> (TBExpr, Type) -> PartialType -> Type -> ResArrowTree f -> CRes (ResArrowTree f)
envLookupTry (_, _, _, classMap) _ _ _ srcType destType resArrow | hasType classMap (resArrowDestType classMap srcType resArrow) destType = return resArrow
envLookupTry _ _ visitedArrows _ _ _ resArrow | S.member resArrow visitedArrows = CErr [MkCNote $ BuildTreeCErr Nothing "Found cyclical use of function"]
envLookupTry env@(_, _, _, classMap) objSrc visitedArrows ee srcType destType resArrow = do
  afterArrows <- traverse buildAfterArrows $ splitPartialLeafs newLeafTypes
  return $ buildMatch resArrow destType (H.fromList afterArrows)
  where
    (SumType newLeafTypes) = resArrowDestType classMap srcType resArrow
    visitedArrows' = S.insert resArrow visitedArrows
    objSrc' = case resArrow of
          (ResEArrow _ o _) -> (srcType, o)
          _ -> objSrc
    buildAfterArrows = \leafType -> do
      v <- envLookup env objSrc' resArrow ee visitedArrows' leafType destType
      return (leafType, v)
buildGuardArrows :: (Eq f, Hashable f) => TBEnv f -> ObjSrc -> ResArrowTree f -> (TBExpr, Type) -> VisitedArrows f -> PartialType -> Type -> ([ResArrowTree f], [(TBExpr, ResArrowTree f)], [ResArrowTree f]) -> CRes (ResArrowTree f)
buildGuardArrows env obj input ee visitedArrows srcType destType guards = case guards of
      ([], [], []) -> CErr [MkCNote $ BuildTreeCErr Nothing $ printf "No arrows found when looking for: %s -> %s" (show $ singletonType srcType) (show destType)]
      (_, _, _:_:_) -> CErr [MkCNote $ BuildTreeCErr Nothing "Multiple ElseGuards"]
      (noGuard, ifGuards, elseGuard) | not (null noGuard) -> case partitionCRes $ map ltry noGuard of
                          (_, resArrowTree:_) -> resArrowTree
                          (errNotes1, _) -> case buildGuardArrows env obj input ee visitedArrows srcType destType ([], ifGuards, elseGuard) of
                            r@CRes{} -> r
                            CErr errNotes2 -> wrapCErr (errNotes1 ++ errNotes2) $ printf "Failed to lookup noGuard arrow: %s -> %s\n\tNoGuard: %s" (show $ singletonType srcType) (show destType) (show noGuard)
      ([], _, []) -> CErr [MkCNote $ BuildTreeCErr Nothing "Missing ElseGuard on envLookup"]
      ([], ifGuards, [elseGuard]) -> do
                                      ifTreePairs <- forM ifGuards $ \(ifCond, ifThen@(ResEArrow _ o _)) -> do
                                            ifTree' <- buildExprImp env (srcType, o) ifCond (getMetaType $ getExprMeta ifCond) boolType
                                            thenTree' <- ltry ifThen
                                            return ((ifTree', input, o), thenTree')
                                      elseTree <- ltry elseGuard
                                      return $ case ifTreePairs of
                                        [] -> elseTree
                                        _ -> ResArrowCond destType ifTreePairs elseTree
      arrows -> CErr [MkCNote $ BuildTreeCErr Nothing $ printf "Unknown arrows found in envLookup: %s" (show arrows)]
  where
    ltry tree = envLookupTry env obj visitedArrows ee srcType destType tree

findResArrows :: (Eq f, Hashable f) => TBEnv f -> PartialType -> Type -> CRes [ResBuildEnvItem f]
findResArrows (resEnv, _, _, classMap) srcType@PartialType{ptName=PTypeName srcName} destType = case H.lookup srcName resEnv of
  Just resArrowsWithName -> do
    let resArrows = filter (\(arrowType, _, _) -> subPartialOf classMap srcType arrowType) resArrowsWithName
    -- TODO: Sort resArrows by priority order before trying
    return resArrows
  Nothing -> CErr [MkCNote $ BuildTreeCErr Nothing $ "Failed to find any arrows from " ++ show srcType ++ " to " ++ show destType]
findResArrows _ PartialType{ptName=PClassName{}} _ = error "Can't findResArrows for class"

envLookup :: (Eq f, Hashable f) => TBEnv f -> ObjSrc -> ResArrowTree f -> (TBExpr, Type) -> VisitedArrows f -> PartialType -> Type -> CRes (ResArrowTree f)
envLookup (_, _, _, classMap) _ input _ _ srcType destType | hasPartial classMap srcType destType = return input
envLookup env obj input ee visitedArrows srcType@PartialType{ptName=PTypeName{}} destType = do
  resArrows <- findResArrows env srcType destType
  let guards = (\(a,b,c) -> (concat a, concat b, concat c)) $ unzip3 $ map (\case
                        (_, NoGuard, a) -> ([a input], [], [])
                        (_, IfGuard ifCond, ifThen) -> ([], [(ifCond, ifThen input)], [])
                        (_, ElseGuard, a) -> ([], [], [a input])
                    ) resArrows
  buildGuardArrows env obj input ee visitedArrows srcType destType guards
envLookup env@(_, _, _, classMap) obj input ee visitedArrows srcType@PartialType{ptName=PClassName{}} destType = do
  let (SumType expanded) = expandClassPartial classMap srcType
  let expanded' = splitPartialLeafs expanded
  expandedTrees <- forM expanded' $ \expandedSrc -> do
    v <- envLookup env obj input ee visitedArrows expandedSrc destType
    return (expandedSrc, v)
  return $ buildMatch input destType $ H.fromList expandedTrees

buildImplicit :: (Eq f, Hashable f) => TBEnv f -> ObjSrc -> TBExpr -> Type -> Type -> CRes (ResArrowTree f)
buildImplicit _ _ expr srcType TopType = return $ ExprArrow expr srcType srcType
buildImplicit _ obj _ TopType destType = error $ printf "Build implicit from top type to %s in %s" (show destType) (show obj)
buildImplicit env objSrc@(_, Object _ _ _ objVars _) input (TypeVar (TVVar varName)) destType = case H.lookup varName objVars of
  Just objVarM -> buildImplicit env objSrc input (getMetaType objVarM) destType
  Nothing -> error $ printf "buildImplicit unknown arg %s with obj %s" varName (show objSrc)
buildImplicit env@(_, _, _, classMap) objSrc@(os, obj) input (TypeVar (TVArg argName)) destType = case H.lookup argName $ formArgMetaMapWithSrc classMap obj os of
  Just (_, srcType) -> buildImplicit env objSrc input srcType destType
  Nothing -> error $ printf "buildImplicit unknown arg %s with obj %s" argName (show obj)
buildImplicit env obj expr srcType@(SumType srcTypeLeafs) destType = do
  let wholeInput = ExprArrow expr srcType srcType
  matchVal <- forM (splitPartialLeafs srcTypeLeafs) $ \leafSrcType -> do
    let leafInputType = singletonType leafSrcType
    let leafInput = ExprArrow expr leafInputType leafInputType
    v <- envLookup env obj leafInput (expr, leafInputType) S.empty leafSrcType destType
    return (leafSrcType, v)
  return (buildMatch wholeInput destType (H.fromList matchVal))

-- executes an expression and then an implicit to a desired dest type
buildExprImp :: (Eq f, Hashable f) => TBEnv f -> ObjSrc -> TBExpr -> Type -> Type -> CRes (ResArrowTree f)
buildExprImp env@(_, _, _, classMap) objSrc@(os, obj) expr exprType destType = do
  res' <- if hasTypeWithObjSrc classMap os obj exprType destType
    then buildExpr env expr
    else buildImplicit env objSrc expr exprType destType
  resolveTree env objSrc res'

-- builds all macroArrows and exprArrows into other arrow types
resolveTree :: (Eq f, Hashable f) => TBEnv f -> ObjSrc -> ResArrowTree f -> CRes (ResArrowTree f)
resolveTree env obj (ResEArrow input o a) = do
  input' <- resolveTree env obj input
  return $ ResEArrow input' o a
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


buildArrow :: (Eq f, Hashable f) => TBEnv f -> PartialType -> TBObject -> TBArrow -> CRes (Maybe (TBArrow, (ResArrowTree f, [ResArrowTree f])))
buildArrow _ _ _ (Arrow _ _ _ Nothing) = return Nothing
buildArrow env objPartial obj@(Object _ _ _ objVars _) arrow@(Arrow (Typed am _) compAnnots _ (Just expr)) = do
  let objSrc = (objPartial, obj)
  let am' = case am of
        (TypeVar (TVVar v)) -> case H.lookup v objVars of
          Just m -> getMetaType m
          Nothing -> error "Bad TVVar in makeBaseEnv"
        (TypeVar (TVArg v)) -> case H.lookup v $ formArgMetaMap obj of
          Just argMeta -> getMetaType argMeta
          Nothing -> error "Bad TVArg in makeBaseEnv"
        _ -> am
  resArrowTree <- resolveTree env objSrc (ExprArrow expr (getMetaType $ getExprMeta expr) am')
  compAnnots' <- forM compAnnots $ \annot -> do
                                       let annotType = getMetaType $ getExprMeta annot
                                       resolveTree env objSrc (ExprArrow annot annotType annotType)
  return $ Just (arrow, (resArrowTree, compAnnots'))

buildRoot :: (Eq f, Hashable f) => TBEnv f -> TBExpr -> PartialType -> Type -> CRes (ResArrowTree f)
buildRoot env input src dest = do
  let emptyObj = Object (Typed (singletonType src) Nothing) FunctionObj "EmptyObj" H.empty H.empty
  let objSrc = (src, emptyObj)
  resolveTree env objSrc (ExprArrow input (getMetaType $ getExprMeta input) dest)
