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

module TreeBuild where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Maybe
import           Text.Printf

import           Control.Monad
import           CRes
import           Eval.Common
import           Semantics
import           Semantics.Annots
import           Semantics.Prgm
import           Semantics.Types

type TBMetaDat = ()
type TBMeta = Meta ()
type TBExpr = Expr TBMetaDat
type TBCompAnnot = CompAnnot TBExpr
type TBObjArr = ObjArr Expr TBMetaDat
type TBGuard = Maybe TBExpr
type TBObjectMap = ObjectMap Expr TBMetaDat
type TBPrgm = Prgm Expr TBMetaDat

type VisitedArrows = S.HashSet TCallTree

leafsFromMeta :: TBMeta -> [PartialType]
leafsFromMeta (Meta TopType{} _ _) = error "leafFromMeta from TopType"
leafsFromMeta (Meta TypeVar{} _ _) = error "leafFromMeta from TypeVar"
leafsFromMeta (Meta (UnionType prodTypes) _ _) = splitUnionType prodTypes

-- Helper to replace matches with a single option with their result
buildMatch :: Type -> H.HashMap PartialType TCallTree -> TCallTree
buildMatch _tp opts = case H.toList opts of
  [(_, t)] -> t
  _        -> TCMatch opts

buildTBEnv :: ResBuildEnv -> TBPrgm -> TBEnv
buildTBEnv primEnv prgm@(objMap, _, _) = baseEnv
  where
    baseEnv = TBEnv "" (H.union primEnv resEnv) prgm (mkTypeEnv prgm)
    resEnv = H.fromListWith (++) $ mapMaybe resFromArrow objMap

    resFromArrow oa@ObjArr{oaObj=Just obj, oaArr, oaAnnots} = case oaArr of
      _ | getExprType (oaObjExpr oa) == topType -> error $ printf "buildTBEnv failed with a topType input in %s" (show oa)
      (Just _, _) -> Just (oaObjPath oa, [(objLeaf, listToMaybe (exprWhereConds obj), any isElseAnnot oaAnnots, TCObjArr oa) | objLeaf <- leafsFromMeta (getExprMeta $ oaObjExpr oa)])
      (Nothing, _) -> Nothing
    resFromArrow oa = error $ printf "resFromArrow with no input expression: %s" (show oa)

envLookupTry ::  TBEnv -> [ObjSrc] -> VisitedArrows -> PartialType -> Type -> TCallTree -> CRes TCallTree
envLookupTry TBEnv{tbTypeEnv} os _ srcType destType resArrow | isSubtypeOfWithObjSrcs tbTypeEnv os (resArrowDestType tbTypeEnv srcType resArrow) destType = return resArrow
envLookupTry _ _ visitedArrows _ _ resArrow | S.member resArrow visitedArrows = CErr [MkCNote $ BuildTreeCErr Nothing "Found cyclical use of function"]
envLookupTry env@TBEnv{tbTypeEnv} objSrc visitedArrows srcType destType resArrow = do
  newLeafTypes <- case resArrowDestType tbTypeEnv srcType resArrow of
    UnionType t -> return t
    t -> CErr [MkCNote $ BuildTreeCErr Nothing $ printf "Found impossible type %s in envLookupTry" (show t)]
  afterArrows <- traverse buildAfterArrows $ splitUnionType newLeafTypes
  return $ TCSeq resArrow (buildMatch destType (H.fromList afterArrows))
  where
    visitedArrows' = S.insert resArrow visitedArrows
    objSrc' = case resArrow of
          (TCObjArr oa) -> [(srcType, oa)]
          _             -> objSrc
    buildAfterArrows leafType = do
      v <- envLookup env objSrc' visitedArrows' leafType destType
      return (leafType, v)

-- This function takes a desired type, arrows with various inputs in sorted order of preference
-- It returns the map of what each arrow should cover for a match tree or a CErr
-- It is used for arrow declarations that require multiple arrows definitions to constitute
-- TODO: This would be drastically improved with type difference
completeTreeSet :: TBEnv -> PartialType -> [(PartialType, TCallTree)] -> Either Type (H.HashMap PartialType TCallTree)
completeTreeSet TBEnv{tbTypeEnv} fullPartial = aux H.empty bottomType
  where
    fullType = singletonType fullPartial
    aux :: H.HashMap PartialType TCallTree -> Type -> [(PartialType, TCallTree)] -> Either Type (H.HashMap PartialType TCallTree)
    aux accMap accType _ | isSubtypeOf tbTypeEnv fullType accType = return accMap
    aux _ accType [] = Left accType
    aux accMap accType ((optType, optTree):opts) = do
      let accType' = intersectTypes tbTypeEnv fullType $ compactType tbTypeEnv H.empty $ unionTypes tbTypeEnv accType (singletonType optType)

      -- next opt increases accumulation
      if not (isSubtypeOf tbTypeEnv accType' accType)
        -- Add to accumulation
        -- TODO: Should use ((optType - accType) ∩ fullType) for insertion, otherwise order may not be correct in matching and match not precise
        then aux (H.insert optType optTree accMap) accType' opts
        -- Don't add to accumulation
        else aux accMap accType opts

data ArrowGuardGroup
  = NoGuardGroup PartialType TCallTree
  | CondGuardGroup [(TBExpr, TCallTree)] (PartialType, TCallTree)
  deriving Show
buildGuardArrows :: TBEnv -> [ObjSrc] -> VisitedArrows -> PartialType -> Type -> [ArrowGuardGroup] -> CRes TCallTree
buildGuardArrows env obj visitedArrows srcType destType guards = do
  let builtGuards = map buildGuard guards
  treeOptions <- catCRes $ map buildGuard guards
  finalTrees <- case completeTreeSet env srcType treeOptions of
    Left completingAccType -> CErr [MkCNote $ GenMapCErr Nothing (printf "Failed to buildGuardArrows from %s to %s. Could not find enough arrows for the src. Only found %s. Tried guards:" (show srcType) (show destType) (show completingAccType)) (zip (map show guards) (map (fmap show) builtGuards))]
    Right r -> return r
  return $ buildMatch destType finalTrees
  where
    buildGuard :: ArrowGuardGroup -> CRes (PartialType, TCallTree)
    buildGuard (NoGuardGroup tp tree) = do
      tree' <- ltry tree
      return (tp, tree')
    buildGuard (CondGuardGroup [] els) = return els
    buildGuard (CondGuardGroup ifs (elseTp, elseTree)) = do
      ifTreePairs <- forM ifs $ \(ifCond, ifThen@(TCObjArr oa)) -> do
        ifCond' <- toTExprDest env [(srcType, oa)] ifCond (emptyMetaT boolType)
        ifThen' <- ltry ifThen
        return ((ifCond', oa), ifThen')
      elseTree' <- ltry elseTree
      return (elseTp, TCCond destType ifTreePairs elseTree')
      -- return $ TCCond destType ifTreePairs elseTree
    ltry = envLookupTry env obj visitedArrows srcType destType

groupArrows :: [ResBuildEnvItem] -> CRes [ArrowGuardGroup]
groupArrows = aux ([], Nothing)
  where
    aux ([], Nothing) [] = return []
    aux (ifs, Just els) [] = return [CondGuardGroup ifs els]
    aux (_, Nothing) [] = CErr [MkCNote $ BuildTreeCErr Nothing "No ElseGuards found"]
    aux acc ((pt, Nothing, False, t):bs) = do
      bs' <- aux acc bs
      return $ NoGuardGroup pt t:bs'
    aux (ifs, els) ((_, Just it, False, t):bs) = aux ((it, t):ifs, els) bs
    aux (ifs, Nothing) ((pt, Nothing, True, t):bs) = aux (ifs, Just (pt, t)) bs
    aux (_, Just{}) ((_, Nothing, True, _):_) = CErr [MkCNote $ BuildTreeCErr Nothing "Multiple ElseGuards found"]
    aux (_, _) ((_, Just{}, True, _):_) = CErr [MkCNote $ BuildTreeCErr Nothing "ElseGuards with conditions are currently not supported"]

findResArrows :: TBEnv -> [ObjSrc] -> PartialType -> Type -> CRes [ResBuildEnvItem]
findResArrows TBEnv{tbName, tbResEnv, tbTypeEnv} os srcType@PartialType{ptName=srcName} destType = case argArrows ++ globalArrows of
  arrows@(_:_) -> return arrows
  [] -> CErr [MkCNote $ BuildTreeCErr Nothing $ printf "Failed to find any arrows:\n\tWhen building %s\n\tfrom %s to %s" tbName (show srcType) (show destType)]
  where
    globalArrows = case suffixLookupInDict srcName tbResEnv of
      -- TODO: Sort resArrows by priority order before trying
      Just resArrowsWithName -> filter (\(arrowType, _, _, _) -> not $ isBottomType $ intersectTypes tbTypeEnv (singletonType srcType) (singletonType arrowType)) resArrowsWithName
      Nothing -> []
    argArrows :: [ResBuildEnvItem]
    argArrows = case H.lookup (partialToKey srcType) $ snd $ splitVarArgEnv $ exprVarArgsWithObjSrcs tbTypeEnv os of
      Just (_, argArrowType) -> [(srcType, Nothing, False, TCArg argArrowType srcName)]
      Nothing -> []

envLookup :: TBEnv -> [ObjSrc] -> VisitedArrows -> PartialType -> Type -> CRes TCallTree
envLookup TBEnv{tbTypeEnv} os _ srcType destType | isSubtypeOfWithObjSrcs tbTypeEnv os (singletonType srcType) destType = return TCTId
envLookup env obj visitedArrows srcType destType = do
  resArrows <- findResArrows env obj srcType destType
  guards <- groupArrows resArrows
  buildGuardArrows env obj visitedArrows srcType destType guards


buildCallTree :: TBEnv -> [ObjSrc] -> Type -> Type -> CRes TCallTree
buildCallTree TBEnv{tbTypeEnv} os srcType destType | isSubtypeOfWithObjSrcs tbTypeEnv os srcType destType = return TCTId
buildCallTree _ _ _ (TopType []) = return TCTId
buildCallTree _ _ (TopType []) _ = error $ printf "buildCallTree from top type"
buildCallTree env@TBEnv{tbTypeEnv} objSrcs (TypeVar v _) destType = case H.lookup v (exprVarArgsWithObjSrcs tbTypeEnv objSrcs) of
  Just (_, srcType') -> buildCallTree env objSrcs srcType' destType
  Nothing -> error $ printf "Unknown TypeVar %s in buildCallTree" (show v)
buildCallTree env os (UnionType srcLeafs) destType = do
  matchVal <- forM (splitUnionType srcLeafs) $ \srcPartial -> do
    t <- envLookup env os S.empty srcPartial destType
    return (srcPartial, t)
  return $ buildMatch destType $ H.fromList matchVal
buildCallTree _ _ _ _ = error "Unimplemented buildCallTree"

toTExpr :: TBEnv -> [ObjSrc] -> Expr () -> CRes (TExpr ())
toTExpr _ _ (CExpr m (CInt c)) = return $ TCExpr m (IntVal c)
toTExpr _ _ (CExpr m (CFloat c)) = return $ TCExpr m (FloatVal c)
toTExpr _ _ (CExpr m (CStr c)) = return $ TCExpr m (StrVal c)
toTExpr _ _ (CExpr m (CChar c)) = return $ TCExpr m (CharVal c)
toTExpr _ _ (Value m n) = return $ TValue m n
toTExpr _ _ (HoleExpr m h) = return $ THoleExpr m h
toTExpr env os (AliasExpr b a) = do
  b' <- toTExpr env os b
  a' <- toTExpr env os a
  return $ TAliasExpr b' a'
toTExpr env os (EWhere b a) = do
  b' <- toTExpr env os b
  a' <- toTExpr env os a
  return $ TWhere b' a'
toTExpr env os (TupleApply m (bm, be) oa) = do
  be' <- toTExprDest env os be bm
  oa' <- toTEObjArr env os oa
  return $ TTupleApply m (bm, be') oa'
toTExpr env os (VarApply m b n v) = do
  b' <- toTExpr env os b
  return $ TVarApply m b' n v

toTEObjArr :: TBEnv -> [ObjSrc] -> EObjArr -> CRes (ObjArr TExpr ())
toTEObjArr env os oa@ObjArr{oaObj, oaAnnots, oaArr=(arrExpr, arrM)} = do
  oaObj' <- mapM (toTExpr env os) oaObj
  let os' = if isJust oaObj && isJust arrExpr
        then (fromJust $ maybeGetSingleton $ getExprType $ oaObjExpr oa, oa) : os
        else os
  arrExpr' <- forM arrExpr $ \e ->
    toTExprDest env os e arrM
  oaAnnots' <- mapM (toTExpr env os') oaAnnots
  return oa{oaObj=oaObj', oaAnnots=oaAnnots', oaArr=(arrExpr', arrM)}

texprDest :: TBEnv -> [ObjSrc] -> TExpr () -> EvalMeta -> CRes (TExpr ())
texprDest env os e m = do
  ct <- buildCallTree env os (getMetaType $ getExprMeta e) (getMetaType m)
  case ct of
    TCTId -> return e
    TCMacro _ (MacroFunction f) -> do
      e' <- f e (MacroData env os)
      texprDest env os e' m
    _ -> return $ TCalls m e ct

toTExprDest :: TBEnv -> [ObjSrc] -> Expr () -> EvalMeta -> CRes (TExpr ())
-- toTExprDest _ os e m | trace (printf "toExprDest %s to %s \n\twith %s" (show e) (show m) (show os)) False = undefined
toTExprDest env os e m = do
  e' <- toTExpr env os e
  texprDest env os e' m

buildArrow :: TBEnv -> PartialType -> TBObjArr -> CRes (Maybe (TBObjArr, (TExpr TBMetaDat, [TExpr TBMetaDat])))
-- buildArrow _ src oa | trace (printf "buildArrow %s: %s" (show src) (show oa)) False = undefined
buildArrow _ _ ObjArr{oaArr=(Nothing, _)} = return Nothing
buildArrow env objPartial oa@ObjArr{oaAnnots, oaArr=(Just expr, am)} = do
  let env' = env{tbName = printf "arrow %s" (show oa)}
  let objSrc = (objPartial, oa)
  resArrowTree <- toTExprDest env' [objSrc] expr am
  compAnnots' <- forM oaAnnots $ \annot -> do
                                       let annotEnv = env{tbName = printf "globalAnnot %s" (show annot)}
                                       toTExpr annotEnv [objSrc] annot
  return $ Just (oa, (resArrowTree, compAnnots'))

buildRoot :: TBEnv -> TBExpr -> PartialType -> Type -> CRes (TExpr TBMetaDat)
buildRoot env input src dest = do
  let env' = env{tbName = printf "root"}
  let emptyObj = ObjArr (Just input) FunctionObj Nothing [] (Nothing, emptyMetaN)
  let objSrc = (src, emptyObj)
  toTExprDest env' [objSrc] input  (emptyMetaT dest)
