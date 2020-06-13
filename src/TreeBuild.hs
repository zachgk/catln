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

type TBMeta = Typed
type TBExpr = Expr TBMeta
type TBCompAnnot = CompAnnot TBExpr
type TBObject = Object TBMeta
type TBGuard = Guard TBExpr
type TBArrow = Arrow TBMeta
type TBObjectMap = ObjectMap TBMeta
type TBPrgm = Prgm TBMeta
type TBReplRes = ReplRes TBMeta

type TBEnv f = (ResBuildEnv f, H.HashMap PartialType (ResArrow f))
type VisitedArrows f = S.HashSet (ResArrow f)

resArrowDestType :: PartialType -> ResArrow f -> Type
resArrowDestType (_, _, srcArgs) (ResEArrow (Object _ _ _ _ objArgs) (Arrow (Typed typeVar@TypeVar{}) _ _ (Just expr))) = case H.toList $ H.filter (\(Typed t, _) -> t == typeVar) objArgs of
  ((matchingArgName, _):_) -> case H.lookup matchingArgName srcArgs of
    Just srcArg -> srcArg
    Nothing -> error "Bad arg search in resArrowDestType"
  [] -> (\(Typed t) -> t) $ getExprMeta expr
resArrowDestType _ (ResEArrow _ (Arrow _ _ _ (Just expr))) = (\(Typed t) -> t) $ getExprMeta expr
resArrowDestType _ (ResEArrow _ (Arrow (Typed tp) _ _ Nothing)) = tp
resArrowDestType _ (PrimArrow tp _) = tp
resArrowDestType _ (ConstantArrow CInt{}) = intType
resArrowDestType _ (ConstantArrow CFloat{}) = floatType
resArrowDestType _ (ConstantArrow CStr{}) = strType
resArrowDestType _ (ArgArrow tp _) = tp

leafFromMeta :: TBMeta -> PartialType
leafFromMeta (Typed TopType) = error "leafFromMeta from TopType"
leafFromMeta (Typed TypeVar{}) = error "leafFromMeta from TypeVar"
leafFromMeta (Typed (SumType prodTypes)) = case splitPartialLeafs prodTypes of
  [leafType] -> leafType
  _ -> error $ "Arrow has multiple leaves: " ++ show prodTypes

makeBaseEnv :: (Eq f, Hashable f) => ResBuildEnv f -> TBObjectMap -> CRes (TBEnv f, ResExEnv f)
makeBaseEnv primEnv objMap = fmap (baseEnv,) exEnv
  where
    baseEnv = (H.union primEnv resEnv, H.empty)
    resEnv = H.fromListWith (++) $ concatMap resFromArrows $ H.toList objMap
    resFromArrows :: (TBObject, [TBArrow]) -> [(TypeName, [(PartialType, TBGuard, ResArrow f)])]
    resFromArrows (obj, arrows) = mapMaybe (resFromArrow obj) arrows
    resFromArrow :: TBObject -> TBArrow -> Maybe (TypeName, [(PartialType, TBGuard, ResArrow f)])
    resFromArrow obj@(Object om _ objName _ _) arrow@(Arrow _ _ aguard expr) = fmap (const (objName, [(leafFromMeta om, aguard, ResEArrow obj arrow)])) expr
    exEnv = fmap H.fromList $ sequence $ concatMap exFromArrows $ H.toList objMap
    exFromArrows (obj, arrows) = mapMaybe (exFromArrow obj) arrows
    exFromArrow (Object _ _ _ objVars _) arrow@(Arrow (Typed am) compAnnots _ maybeExpr) = fmap (\expr -> do
            let am' = case am of
                  (TypeVar v) -> case H.lookup v objVars of
                    Just (Typed t) -> t
                    Nothing -> error "Bad typeVar in makeBaseEnv"
                  _ -> am
            resArrowTree <- buildExprImp baseEnv expr am'
            compAnnots' <- mapM (buildCompAnnot baseEnv) compAnnots
            return (arrow, (resArrowTree, compAnnots'))
      ) maybeExpr

buildCompAnnot :: (Eq f, Hashable f) => TBEnv f -> TBCompAnnot -> CRes (ResArrowTree f)
buildCompAnnot env (CompAnnot "assert" args) = case (H.lookup "test" args, H.lookup "msg" args) of
    (Just test, Just msgExpr) -> do
      test' <- buildExprImp env test boolType
      msg' <- buildExprImp env msgExpr strType
      return $ ResArrowTuple "assert" (H.fromList [("test", test'), ("msg", msg')])
    (Just test, Nothing) -> do
      test' <- buildExprImp env test boolType
      return $ ResArrowTuple "assert" (H.singleton "test" test')
    _ -> CErr [BuildTreeCErr "Invalid assertion"]
buildCompAnnot _ (CompAnnot name _ )= CErr [BuildTreeCErr $ "Unknown compiler annotation" ++ name]

buildExpr :: (Eq f, Hashable f) => TBEnv f -> TBExpr -> CRes (ResArrowTree f)
buildExpr _ (CExpr _ c) = return $ ResArrowSingle (ConstantArrow c)
buildExpr (_, valEnv) (Value (Typed (SumType prodTypes)) name) = case splitPartialLeafs prodTypes of
    (_:_:_) -> CErr [BuildTreeCErr $ "Found multiple types for value " ++ name ++ "\n\t" ++ show prodTypes]
    [] -> CErr [BuildTreeCErr $ "Found no types for value " ++ name ++ " with type " ++ show prodTypes]
    [prodType] -> return $ case H.lookup prodType valEnv of
      Just val -> ResArrowSingle val
      Nothing -> ResArrowTuple name H.empty
buildExpr _ (Arg (Typed tp) name) = return $ ResArrowSingle $ ArgArrow tp name
buildExpr env (TupleApply (Typed (SumType prodTypes)) (Typed baseType, baseExpr) argExprs) = case splitPartialLeafs prodTypes of
    (_:_:_) -> CErr [BuildTreeCErr $ "Found multiple types for tupleApply " ++ show baseExpr ++ "\n\t" ++ show prodTypes ++ "\n\t" ++ show argExprs]
    [] -> CErr [BuildTreeCErr $ "Found no types for tupleApply " ++ show baseExpr ++ " with type " ++ show prodTypes ++ " and exprs " ++ show argExprs]
    [(_, _, leafType)] | H.keysSet argExprs `isSubsetOf` H.keysSet leafType -> do
                           baseBuild <- buildExprImp env baseExpr baseType
                           argVals <- mapM (\(valDestType, expr) -> buildExprImp env expr valDestType) $ H.intersectionWith (,) leafType argExprs
                           return $ ResArrowTupleApply baseBuild argVals
    _ -> CErr [BuildTreeCErr $ "Found bad types for tupleApply " ++ show baseExpr]
buildExpr _ _ = error "Bad buildExpr"

envLookupTry :: (Eq f, Hashable f) => TBEnv f -> VisitedArrows f -> PartialType -> Type -> ResArrow f -> CRes (ResArrowTree f)
envLookupTry _ _ srcType destType resArrow | hasType (resArrowDestType srcType resArrow) destType = return $ ResArrowSingle resArrow
envLookupTry _ visitedArrows _ _ resArrow | S.member resArrow visitedArrows = CErr [BuildTreeCErr "Found cyclical use of function"]
envLookupTry env visitedArrows srcType destType resArrow = do
  let (SumType newLeafTypes) = resArrowDestType srcType resArrow
  let visitedArrows' = S.insert resArrow visitedArrows
  let eitherAfterArrows = partitionCRes $ map (\leafType -> (leafType,) <$> envLookup env visitedArrows' leafType destType) $ splitPartialLeafs newLeafTypes
  case eitherAfterArrows of
    ([], afterArrows) -> do
      let maybeAfterArrowTree = H.fromList <$> sequence afterArrows
      fmap (ResArrowCompose (ResArrowSingle resArrow) . ResArrowMatch) maybeAfterArrowTree
    (errNotes, _) -> wrapCErr errNotes "Failed envLookupTry"

buildGuardArrows :: (Eq f, Hashable f) => TBEnv f -> VisitedArrows f -> PartialType -> Type -> ([ResArrow f], [(TBExpr, ResArrow f)], [ResArrow f]) -> CRes (ResArrowTree f)
buildGuardArrows env visitedArrows srcType destType guards = case guards of
      ([], [], []) -> CErr [BuildTreeCErr $ printf "No arrows found on lookup from %s to %s" (show srcType) (show destType)]
      (_, _, _:_:_) -> CErr [BuildTreeCErr "Multiple ElseGuards"]
      (noGuard, ifGuards, elseGuard) | not (null noGuard) -> case partitionCRes $ map ltry noGuard of
                          (_, resArrowTree:_) -> resArrowTree
                          (errNotes1, _) -> case buildGuardArrows env visitedArrows srcType destType ([], ifGuards, elseGuard) of
                            r@CRes{} -> r
                            CErr errNotes2 -> wrapCErr (errNotes1 ++ errNotes2) $ "Failed to lookup noGuard arrow from " ++ show srcType ++ " to " ++ show destType ++ "\n\tNoGuard: " ++ show noGuard
      ([], _, []) -> CErr [BuildTreeCErr "Missing ElseGuard on envLookup"]
      ([], ifGuards, [elseGuard]) | not (null ifGuards) -> do
                                      let maybeIfTreePairs = mapM (\(ifCond, ifThen) -> sequenceT (buildExprImp env ifCond boolType, ltry ifThen)) ifGuards
                                      let maybeElseTree = ltry elseGuard
                                      case sequenceT (maybeIfTreePairs, maybeElseTree) of
                                        (CRes notes (ifTreePairs, elseTree)) -> CRes notes $ ResArrowCond ifTreePairs elseTree
                                        CErr notes -> wrapCErr notes "No valid ifTrees:"
      _ -> CErr [BuildTreeCErr "Unknown arrows found in envLookup"]
  where
    ltry = envLookupTry env visitedArrows srcType destType

envLookup :: (Eq f, Hashable f) => TBEnv f -> VisitedArrows f -> PartialType -> Type -> CRes (ResArrowTree f)
envLookup _ _ srcType destType | srcType `hasPartial` destType = return ResArrowID
envLookup env@(resEnv, _) visitedArrows srcType@(srcName, _, _) destType = case H.lookup srcName resEnv of
  Just resArrowsWithName -> do
    let resArrows = filter (\(arrowType, _, _) -> srcType `subPartialOf` arrowType) resArrowsWithName
    -- TODO: Sort resArrows by priority order before trying
    let guards = (\(a,b,c) -> (concat a, concat b, concat c)) $ unzip3 $ map (\case
                          (_, NoGuard, a) -> ([a], [], [])
                          (_, IfGuard ifCond, ifThen) -> ([], [(ifCond, ifThen)], [])
                          (_, ElseGuard, a) -> ([], [], [a])
                      ) resArrows
    buildGuardArrows env visitedArrows srcType destType guards

  Nothing -> CErr [BuildTreeCErr $ "Failed to find any arrows from " ++ show srcType ++ " to " ++ show destType]

buildImplicit :: (Eq f, Hashable f) => TBEnv f -> Type -> Type -> CRes (ResArrowTree f)
buildImplicit _ TopType _ = error "Build implicit from top type"
buildImplicit _ TypeVar{} _ = error "Build implicit from type var"
buildImplicit env (SumType srcType) destType = do
  matchVal <- sequence $ H.fromList $ map aux $ splitPartialLeafs srcType
  return (ResArrowMatch matchVal)
  where
    aux leafSrcType = (leafSrcType,) $ envLookup env S.empty leafSrcType destType

-- executes an expression and then an implicit to a desired dest type
buildExprImp :: (Eq f, Hashable f) => TBEnv f -> TBExpr -> Type -> CRes (ResArrowTree f)
buildExprImp env expr destType = do
  t1 <- buildExpr env expr
  let (Typed srcType) = getExprMeta expr
  if srcType == destType then
    return t1
    else do
      t2 <- buildImplicit env srcType destType
      return $ ResArrowCompose t1 t2

buildPrgm :: (Eq f, Hashable f) => ResBuildEnv f -> PartialType -> Type -> TBPrgm -> CRes (ResArrowTree f, ResExEnv f)
buildPrgm primEnv src dest (objectMap, _) = do
  (env, exEnv) <- makeBaseEnv primEnv objectMap
  rootTree <- envLookup env S.empty src dest
  return (rootTree, exEnv)
