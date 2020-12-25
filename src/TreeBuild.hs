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
import           Data.Tuple.Sequence
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
type ObjSrc = (PartialType, TBObject)

resArrowDestType :: ClassMap -> PartialType -> ResArrowTree f -> Type
resArrowDestType classMap src (ResEArrow _ obj arr) = arrowDestType False classMap src obj arr
resArrowDestType _ _ (PrimArrow _ tp _) = tp
resArrowDestType _ _ (MacroArrow _ tp _) = tp
resArrowDestType _ _ (ConstantArrow v) = singletonType $ getValType v
resArrowDestType _ _ (ArgArrow tp _) = tp
resArrowDestType _ _ t = error $ printf "Not yet implemented resArrowDestType for %s" (show t)

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

buildExpr :: (Eq f, Hashable f) => TBEnv f -> TBObject -> TBExpr -> CRes (ResArrowTree f)
buildExpr _ _ (CExpr _ c) = case c of
  (CInt i) -> return $ ConstantArrow $ IntVal i
  (CFloat i) -> return $ ConstantArrow $ FloatVal i
  (CStr i) -> return $ ConstantArrow $ StrVal i
buildExpr (_, valEnv, _, _) _ (Value (Typed (SumType prodTypes) pos) name) = case splitPartialLeafs prodTypes of
    (_:_:_) -> CErr [MkCNote $ BuildTreeCErr pos $ "Found multiple types for value " ++ name ++ "\n\t" ++ show prodTypes]
    [] -> CErr [MkCNote $ BuildTreeCErr pos $ "Found no types for value " ++ name ++ " with type " ++ show prodTypes]
    [prodType] -> return $ case H.lookup prodType valEnv of
      Just val -> val
      Nothing -> ResArrowTuple name H.empty
buildExpr _ _ (Arg (Typed tp _) name) = return $ ArgArrow tp name
buildExpr (_, _, _, classMap) (Object _ _ _ objVars _) (TupleApply (Typed (SumType prodTypes) pos) (Typed baseType _, baseExpr) argName argExpr) = case splitPartialLeafs prodTypes of
    [] -> CErr [MkCNote $ BuildTreeCErr pos $ "Found no types for tupleApply " ++ show baseExpr ++ " with type " ++ show prodTypes ++ " and expr " ++ show argExpr]
    leaves -> do
      let baseBuild = ExprArrow baseExpr baseType
      leavesArgs <- mapM getLeafArgs leaves
      let leafArgs = unionTypes classMap leavesArgs
      -- TODO: Currently for each arg it does: execute expr, execute implicit (to any leaf), then match
      -- it should really be execute expr for args, match all args, then implicit all args
      let argVal = ExprArrow argExpr leafArgs
      return $ ResArrowTupleApply baseBuild argName argVal
  where
    getLeafArgs PartialType{ptArgs=leafArgs} = case H.lookup argName leafArgs of
      Just (TypeVar (TVVar v)) -> return $ H.lookupDefault TopType v $ fmap getMetaType objVars
      Just (TypeVar TVArg{}) -> error "Not yet implemented: type arg in getLeafArgs"
      Just leafArg -> return leafArg
      Nothing -> CErr [MkCNote $ BuildTreeCErr pos "buildExpr could not find expected args"]
buildExpr _ _ _ = error "Bad buildExpr"

envLookupTry :: (Eq f, Hashable f) => TBEnv f -> ObjSrc -> VisitedArrows f -> PartialType -> Type -> ResArrowTree f -> CRes (ResArrowTree f)
envLookupTry (_, _, _, classMap) _ _ srcType destType resArrow | hasType classMap (resArrowDestType classMap srcType resArrow) destType = return resArrow
envLookupTry _ _ visitedArrows _ _ resArrow | S.member resArrow visitedArrows = CErr [MkCNote $ BuildTreeCErr Nothing "Found cyclical use of function"]
envLookupTry env@(_, _, _, classMap) objSrc visitedArrows srcType destType resArrow = do
  let (SumType newLeafTypes) = resArrowDestType classMap srcType resArrow
  let visitedArrows' = S.insert resArrow visitedArrows
  let objSrc' = case resArrow of
        (ResEArrow _ o _) -> (srcType, o)
        _ -> objSrc
  let eitherAfterArrows = partitionCRes $ map (\leafType -> (leafType,) <$> envLookup env objSrc' resArrow visitedArrows' leafType destType) $ splitPartialLeafs newLeafTypes
  case eitherAfterArrows of
    ([], afterArrows) -> do
      maybeAfterArrowTrees <- H.fromList <$> sequence afterArrows
      return $ buildMatch resArrow destType maybeAfterArrowTrees
    (errNotes, _) -> wrapCErr errNotes "Failed envLookupTry"

buildGuardArrows :: (Eq f, Hashable f) => TBEnv f -> ObjSrc -> ResArrowTree f -> VisitedArrows f -> PartialType -> Type -> ([ResArrowTree f], [(TBExpr, ResArrowTree f)], [ResArrowTree f]) -> CRes (ResArrowTree f)
buildGuardArrows env obj input visitedArrows srcType destType guards = case guards of
      ([], [], []) -> CErr [MkCNote $ BuildTreeCErr Nothing $ printf "No arrows found when looking for: %s -> %s" (show $ singletonType srcType) (show destType)]
      (_, _, _:_:_) -> CErr [MkCNote $ BuildTreeCErr Nothing "Multiple ElseGuards"]
      (noGuard, ifGuards, elseGuard) | not (null noGuard) -> case partitionCRes $ map ltry noGuard of
                          (_, resArrowTree:_) -> resArrowTree
                          (errNotes1, _) -> case buildGuardArrows env obj input visitedArrows srcType destType ([], ifGuards, elseGuard) of
                            r@CRes{} -> r
                            CErr errNotes2 -> wrapCErr (errNotes1 ++ errNotes2) $ printf "Failed to lookup noGuard arrow: %s -> %s\n\tNoGuard: %s" (show $ singletonType srcType) (show destType) (show noGuard)
      ([], _, []) -> CErr [MkCNote $ BuildTreeCErr Nothing "Missing ElseGuard on envLookup"]
      ([], ifGuards, [elseGuard]) -> do
                                      let maybeIfTreePairs = forM ifGuards $ \(ifCond, ifThen@(ResEArrow _ o _)) -> do
                                            ifTree' <- buildExprImp env (srcType, o) ifCond boolType
                                            thenTree' <- ltry ifThen
                                            return ((ifTree', input, o), thenTree')
                                      let maybeElseTree = ltry elseGuard
                                      case sequenceT (maybeIfTreePairs, maybeElseTree) of
                                        (CRes notes (ifTreePairs, elseTree)) -> CRes notes $ ResArrowCond destType ifTreePairs elseTree
                                        CErr notes -> wrapCErr notes "No valid ifTrees:"
      arrows -> CErr [MkCNote $ BuildTreeCErr Nothing $ printf "Unknown arrows found in envLookup: %s" (show arrows)]
  where
    ltry tree = envLookupTry env obj visitedArrows srcType destType tree

findResArrows :: (Eq f, Hashable f) => TBEnv f -> PartialType -> Type -> CRes [ResBuildEnvItem f]
findResArrows (resEnv, _, _, classMap) srcType@PartialType{ptName=PTypeName srcName} destType = case H.lookup srcName resEnv of
  Just resArrowsWithName -> do
    let resArrows = filter (\(arrowType, _, _) -> subPartialOf classMap srcType arrowType) resArrowsWithName
    -- TODO: Sort resArrows by priority order before trying
    return resArrows
  Nothing -> CErr [MkCNote $ BuildTreeCErr Nothing $ "Failed to find any arrows from " ++ show srcType ++ " to " ++ show destType]
findResArrows _ PartialType{ptName=PClassName{}} _ = error "Can't findResArrows for class"

envLookup :: (Eq f, Hashable f) => TBEnv f -> ObjSrc -> ResArrowTree f -> VisitedArrows f -> PartialType -> Type -> CRes (ResArrowTree f)
envLookup (_, _, _, classMap) _ input _ srcType destType | hasPartial classMap srcType destType = return input
envLookup env obj input visitedArrows srcType@PartialType{ptName=PTypeName{}} destType = do
  resArrows <- findResArrows env srcType destType
  let guards = (\(a,b,c) -> (concat a, concat b, concat c)) $ unzip3 $ map (\case
                        (_, NoGuard, a) -> ([a input], [], [])
                        (_, IfGuard ifCond, ifThen) -> ([], [(ifCond, ifThen input)], [])
                        (_, ElseGuard, a) -> ([], [], [a input])
                    ) resArrows
  buildGuardArrows env obj input visitedArrows srcType destType guards
envLookup env@(_, _, _, classMap) obj input visitedArrows srcType@PartialType{ptName=PClassName{}} destType = do
  let (SumType expanded) = expandClassPartial classMap srcType
  let expanded' = splitPartialLeafs expanded
  expandedTrees <- mapM (\expandedSrc -> envLookup env obj input visitedArrows expandedSrc destType) expanded'
  return $ buildMatch input destType $ H.fromList $ zip expanded' expandedTrees

buildImplicit :: (Eq f, Hashable f) => TBEnv f -> ObjSrc -> ResArrowTree f -> Type -> Type -> CRes (ResArrowTree f)
buildImplicit _ _ input _ TopType = return input
buildImplicit _ obj _ TopType destType = error $ printf "Build implicit from top type to %s in %s" (show destType) (show obj)
buildImplicit env objSrc@(_, Object _ _ _ objVars _) input (TypeVar (TVVar varName)) destType = case H.lookup varName objVars of
  Just objVarM -> buildImplicit env objSrc input (getMetaType objVarM) destType
  Nothing -> error $ printf "buildImplicit unknown arg %s with obj %s" varName (show objSrc)
buildImplicit env@(_, _, _, classMap) objSrc@(os, obj) input (TypeVar (TVArg argName)) destType = case H.lookup argName $ formArgMetaMapWithSrc classMap obj os of
  Just (_, srcType) -> buildImplicit env objSrc input srcType destType
  Nothing -> error $ printf "buildImplicit unknown arg %s with obj %s" argName (show obj)
buildImplicit env obj input (SumType srcType) destType = do
  matchVal <- sequence $ H.fromList $ map aux $ splitPartialLeafs srcType
  return (buildMatch input destType matchVal)
  where
    aux leafSrcType = (leafSrcType,) $ envLookup env obj input S.empty leafSrcType destType

-- executes an expression and then an implicit to a desired dest type
buildExprImp :: (Eq f, Hashable f) => TBEnv f -> ObjSrc -> TBExpr -> Type -> CRes (ResArrowTree f)
buildExprImp env@(_, _, _, classMap) objSrc@(os, obj) expr destType = do
  let exprType = getMetaType (getExprMeta expr)
  res' <- if hasTypeWithObjSrc classMap os obj exprType destType
    then buildExpr env obj expr
    else buildImplicit env objSrc (ExprArrow expr exprType) exprType destType
  resolveTree env objSrc res'

-- builds all macroArrows and exprArrows into other arrow types
resolveTree :: (Eq f, Hashable f) => TBEnv f -> ObjSrc -> ResArrowTree f -> CRes (ResArrowTree f)
resolveTree env obj (ResEArrow input o a) = do
  input' <- resolveTree env obj input
  return $ ResEArrow input' o a
resolveTree env obj (PrimArrow input t f) = do
  input' <- resolveTree env obj input
  return $ PrimArrow input' t f
resolveTree env obj (MacroArrow input _ (MacroFunction f)) = resolveTree env obj $ f input (macroData env)
resolveTree env obj (ExprArrow e destType) = buildExprImp env obj e destType
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
  resArrowTree <- resolveTree env objSrc (ExprArrow expr am')
  compAnnots' <- mapM (\annot -> resolveTree env objSrc (ExprArrow annot (getMetaType $ getExprMeta annot))) compAnnots
  return $ Just (arrow, (resArrowTree, compAnnots'))

buildRoot :: (Eq f, Hashable f) => TBEnv f -> TBExpr -> PartialType -> Type -> CRes (ResArrowTree f)
buildRoot env input src dest = do
  let emptyObj = Object (Typed (singletonType src) Nothing) FunctionObj "EmptyObj" H.empty H.empty
  let objSrc = (src, emptyObj)
  resolveTree env objSrc (ExprArrow input dest)
