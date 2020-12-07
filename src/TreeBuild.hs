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

resArrowDestType :: ClassMap -> PartialType -> ResArrowTree f -> Type
resArrowDestType classMap src (ResEArrow _ obj arr) = arrowDestType False classMap src obj arr
resArrowDestType _ _ (PrimArrow _ tp _) = tp
resArrowDestType _ _ (MacroArrow _ tp) = tp
resArrowDestType _ _ (ConstantArrow v) = singletonType $ getValType v
resArrowDestType _ _ (ArgArrow tp _) = tp
resArrowDestType _ _ t = error $ printf "Not yet implemented resArrowDestType for %s" (show t)

leafsFromMeta :: TBMeta -> [PartialType]
leafsFromMeta (Typed TopType) = error "leafFromMeta from TopType"
leafsFromMeta (Typed TypeVar{}) = error "leafFromMeta from TypeVar"
leafsFromMeta (Typed (SumType prodTypes)) = splitPartialLeafs prodTypes

makeTBEnv :: (Eq f, Hashable f) => ResBuildEnv f -> TBPrgm -> TBEnv f
makeTBEnv primEnv (objMap, classMap) = baseEnv
  where
    baseEnv = (H.union primEnv resEnv, H.empty, objMap, classMap)
    resEnv = H.fromListWith (++) $ concatMap resFromArrows objMap
    resFromArrows (obj, arrows) = mapMaybe (resFromArrow obj) arrows
    resFromArrow obj@(Object om _ objName _ _) arrow@(Arrow _ _ aguard expr) = case expr of
      Just _ -> Just (objName, [(objLeaf, aguard, \input _ -> ResEArrow input obj arrow) | objLeaf <- leafsFromMeta om])
      Nothing -> Nothing

buildCompAnnot :: (Eq f, Hashable f) => TBEnv f -> TBObject -> TBCompAnnot -> CRes (ResArrowTree f)
buildCompAnnot env obj (CompAnnot "assert" args) = case (H.lookup "test" args, H.lookup "msg" args) of
    (Just test, Just msgExpr) -> do
      test' <- buildExprImp env obj test boolType
      msg' <- buildExprImp env obj msgExpr strType
      return $ ResArrowTuple "assert" (H.fromList [("test", test'), ("msg", msg')])
    (Just test, Nothing) -> do
      test' <- buildExprImp env obj test boolType
      return $ ResArrowTuple "assert" (H.singleton "test" test')
    _ -> CErr [MkCNote $ BuildTreeCErr "Invalid assertion"]
buildCompAnnot _ _ (CompAnnot name _ )= CErr [MkCNote $ BuildTreeCErr $ "Unknown compiler annotation" ++ name]

buildExpr :: (Eq f, Hashable f) => TBEnv f -> TBObject -> TBExpr -> CRes (ResArrowTree f)
buildExpr _ _ (CExpr _ c) = case c of
  (CInt i) -> return $ ConstantArrow $ IntVal i
  (CFloat i) -> return $ ConstantArrow $ FloatVal i
  (CStr i) -> return $ ConstantArrow $ StrVal i
buildExpr (_, valEnv, _, _) _ (Value (Typed (SumType prodTypes)) name) = case splitPartialLeafs prodTypes of
    (_:_:_) -> CErr [MkCNote $ BuildTreeCErr $ "Found multiple types for value " ++ name ++ "\n\t" ++ show prodTypes]
    [] -> CErr [MkCNote $ BuildTreeCErr $ "Found no types for value " ++ name ++ " with type " ++ show prodTypes]
    [prodType] -> return $ case H.lookup prodType valEnv of
      Just val -> val
      Nothing -> ResArrowTuple name H.empty
buildExpr _ _ (Arg (Typed tp) name) = return $ ArgArrow tp name
buildExpr env@(_, _, _, classMap) obj (TupleApply (Typed (SumType prodTypes)) (Typed baseType, baseExpr) argName argExpr) = case splitPartialLeafs prodTypes of
    [] -> CErr [MkCNote $ BuildTreeCErr $ "Found no types for tupleApply " ++ show baseExpr ++ " with type " ++ show prodTypes ++ " and expr " ++ show argExpr]
    leaves -> do
      baseBuild <- buildExprImp env obj baseExpr baseType
      leavesArgs <- mapM getLeafArgs leaves
      let leafArgs = unionTypes classMap leavesArgs
      -- TODO: Currently for each arg it does: execute expr, execute implicit (to any leaf), then match
      -- it should really be execute expr for args, match all args, then implicit all args
      argVal <- buildExprImp env obj argExpr leafArgs
      return $ ResArrowTupleApply baseBuild argName argVal
  where
    getLeafArgs (_, _, _, leafArgs) = case H.lookup argName leafArgs of
      Just leafArg -> return leafArg
      Nothing -> CErr [MkCNote $ BuildTreeCErr "buildExpr could not find expected args"]
buildExpr _ _ _ = error "Bad buildExpr"

envLookupTry :: (Eq f, Hashable f) => TBEnv f -> TBObject -> VisitedArrows f -> PartialType -> Type -> ResArrowTree f -> CRes (ResArrowTree f)
envLookupTry (_, _, _, classMap) _ _ srcType destType resArrow | hasType classMap (resArrowDestType classMap srcType resArrow) destType = return resArrow
envLookupTry _ _ visitedArrows _ _ resArrow | S.member resArrow visitedArrows = CErr [MkCNote $ BuildTreeCErr "Found cyclical use of function"]
envLookupTry (_, valMap, _, _) obj _ _ _ MacroArrow{} = error $ printf "envLookupTry with MacroArrow not yet completed \n\t\t valMap: %s \n\t\t obj: %s" (show valMap) (show obj)
envLookupTry env@(_, _, _, classMap) obj visitedArrows srcType destType resArrow = do
  let (SumType newLeafTypes) = resArrowDestType classMap srcType resArrow
  let visitedArrows' = S.insert resArrow visitedArrows
  let obj' = case resArrow of
        (ResEArrow _ o _) -> o
        _ -> obj
  let eitherAfterArrows = partitionCRes $ map (\leafType -> (leafType,) <$> envLookup env obj' resArrow visitedArrows' leafType destType) $ splitPartialLeafs newLeafTypes
  case eitherAfterArrows of
    ([], afterArrows) -> do
      maybeAfterArrowTrees <- H.fromList <$> sequence afterArrows
      return $ ResArrowMatch resArrow maybeAfterArrowTrees
    (errNotes, _) -> wrapCErr errNotes "Failed envLookupTry"

buildGuardArrows :: (Eq f, Hashable f) => TBEnv f -> TBObject -> ResArrowTree f -> VisitedArrows f -> PartialType -> Type -> ([ResArrowTree f], [(TBExpr, ResArrowTree f)], [ResArrowTree f]) -> CRes (ResArrowTree f)
buildGuardArrows env obj input visitedArrows srcType destType guards = case guards of
      ([], [], []) -> CErr [MkCNote $ BuildTreeCErr $ printf "No arrows found when looking for: %s -> %s" (show $ singletonType srcType) (show destType)]
      (_, _, _:_:_) -> CErr [MkCNote $ BuildTreeCErr "Multiple ElseGuards"]
      (noGuard, ifGuards, elseGuard) | not (null noGuard) -> case partitionCRes $ map ltry noGuard of
                          (_, resArrowTree:_) -> resArrowTree
                          (errNotes1, _) -> case buildGuardArrows env obj input visitedArrows srcType destType ([], ifGuards, elseGuard) of
                            r@CRes{} -> r
                            CErr errNotes2 -> wrapCErr (errNotes1 ++ errNotes2) $ printf "Failed to lookup noGuard arrow: %s -> %s\n\tNoGuard: %s" (show $ singletonType srcType) (show destType) (show noGuard)
      ([], _, []) -> CErr [MkCNote $ BuildTreeCErr "Missing ElseGuard on envLookup"]
      ([], ifGuards, [elseGuard]) -> do
                                      let maybeIfTreePairs = forM ifGuards $ \(ifCond, ifThen@(ResEArrow _ o _)) -> do
                                            ifTree' <- buildExprImp env o ifCond boolType
                                            thenTree' <- ltry ifThen
                                            return ((ifTree', input, o), thenTree')
                                      let maybeElseTree = ltry elseGuard
                                      case sequenceT (maybeIfTreePairs, maybeElseTree) of
                                        (CRes notes (ifTreePairs, elseTree)) -> CRes notes $ ResArrowCond ifTreePairs elseTree
                                        CErr notes -> wrapCErr notes "No valid ifTrees:"
      arrows -> CErr [MkCNote $ BuildTreeCErr $ printf "Unknown arrows found in envLookup: %s" (show arrows)]
  where
    ltry = envLookupTry env obj visitedArrows srcType destType

findResArrows :: (Eq f, Hashable f) => TBEnv f -> PartialType -> Type -> CRes [ResBuildEnvItem f]
findResArrows (resEnv, _, _, classMap) srcType@(PTypeName srcName, _, _, _) destType = case H.lookup srcName resEnv of
  Just resArrowsWithName -> do
    let resArrows = filter (\(arrowType, _, _) -> subPartialOf classMap srcType arrowType) resArrowsWithName
    -- TODO: Sort resArrows by priority order before trying
    return resArrows
  Nothing -> CErr [MkCNote $ BuildTreeCErr $ "Failed to find any arrows from " ++ show srcType ++ " to " ++ show destType]
findResArrows _ (PClassName _, _, _, _) _ = error "Can't findResArrows for class"

envLookup :: (Eq f, Hashable f) => TBEnv f -> TBObject -> ResArrowTree f -> VisitedArrows f -> PartialType -> Type -> CRes (ResArrowTree f)
envLookup (_, _, _, classMap) _ input _ srcType destType | hasPartial classMap srcType destType = return input
envLookup env obj input visitedArrows srcType@(PTypeName _, _, _, _) destType = do
  resArrows <- findResArrows env srcType destType
  let md = macroData env
  let guards = (\(a,b,c) -> (concat a, concat b, concat c)) $ unzip3 $ map (\case
                        (_, NoGuard, a) -> ([a input md], [], [])
                        (_, IfGuard ifCond, ifThen) -> ([], [(ifCond, ifThen input md)], [])
                        (_, ElseGuard, a) -> ([], [], [a input md])
                    ) resArrows
  buildGuardArrows env obj input visitedArrows srcType destType guards
envLookup env@(_, _, _, classMap) obj input visitedArrows srcType@(PClassName _, _, _, _) destType = do
  let (SumType expanded) = expandClassPartial classMap srcType
  let expanded' = splitPartialLeafs expanded
  expandedTrees <- mapM (\expandedSrc -> envLookup env obj input visitedArrows expandedSrc destType) expanded'
  return $ ResArrowMatch input $ H.fromList $ zip expanded' expandedTrees

buildImplicit :: (Eq f, Hashable f) => TBEnv f -> TBObject -> ResArrowTree f -> Type -> Type -> CRes (ResArrowTree f)
buildImplicit _ _ input _ TopType = return input
buildImplicit _ _ _ TopType destType = error $ printf "Build implicit from top type to %s" (show destType)
buildImplicit env obj@(Object _ _ _ objVars _) input (TypeVar (TVVar varName)) destType = case H.lookup varName objVars of
  Just objVarM -> buildImplicit env obj input (getMetaType objVarM) destType
  Nothing -> error $ printf "buildImplicit unknown arg %s with obj %s" varName (show obj)
buildImplicit env obj input (TypeVar (TVArg argName)) destType = case H.lookup argName $ formArgMetaMap obj of
  Just objArgM -> buildImplicit env obj input (getMetaType objArgM) destType
  Nothing -> error $ printf "buildImplicit unknown arg %s with obj %s" argName (show obj)
buildImplicit env obj input (SumType srcType) destType = do
  matchVal <- sequence $ H.fromList $ map aux $ splitPartialLeafs srcType
  return (ResArrowMatch input matchVal)
  where
    aux leafSrcType = (leafSrcType,) $ envLookup env obj input S.empty leafSrcType destType

-- executes an expression and then an implicit to a desired dest type
buildExprImp :: (Eq f, Hashable f) => TBEnv f -> TBObject -> TBExpr -> Type -> CRes (ResArrowTree f)
buildExprImp env obj expr destType = do
  t1 <- buildExpr env obj expr
  let (Typed srcType) = getExprMeta expr
  if srcType == destType then
    return t1
    else buildImplicit env obj t1 srcType destType

buildArrow :: (Eq f, Hashable f) => TBEnv f -> TBObject -> TBArrow -> CRes (Maybe (TBArrow, (ResArrowTree f, [ResArrowTree f])))
buildArrow _ _ (Arrow _ _ _ Nothing) = return Nothing
buildArrow env obj@(Object _ _ _ objVars _) arrow@(Arrow (Typed am) compAnnots _ (Just expr)) = do
  let am' = case am of
        (TypeVar (TVVar v)) -> case H.lookup v objVars of
          Just (Typed t) -> t
          Nothing -> error "Bad TVVar in makeBaseEnv"
        (TypeVar (TVArg v)) -> case H.lookup v $ formArgMetaMap obj of
          Just argMeta -> getMetaType argMeta
          Nothing -> error "Bad TVArg in makeBaseEnv"
        _ -> am
  resArrowTree <- buildExprImp env obj expr am'
  compAnnots' <- mapM (buildCompAnnot env obj) compAnnots
  return $ Just (arrow, (resArrowTree, compAnnots'))

buildResExEnv :: (Eq f, Hashable f) => TBEnv f -> TBPrgm -> CRes (ResExEnv f)
buildResExEnv env (objMap, _) = do
  build' <- mapM buildArrows objMap
  return (H.fromList $ catMaybes $ concat build')
  where buildArrows (obj, arrows) = mapM (buildArrow env obj) arrows

buildRoot :: (Eq f, Hashable f) => ResBuildEnv f -> TBExpr -> PartialType -> Type -> TBPrgm -> CRes (ResArrowTree f, TBEnv f)
buildRoot primEnv input src dest prgm = do
  let env = makeTBEnv primEnv prgm
  let emptyObj = Object (Typed $ singletonType src) FunctionObj "EmptyObj" H.empty H.empty
  input' <- buildExpr env emptyObj input
  rootTree <- envLookup env emptyObj input' S.empty src dest
  return (rootTree, env)
