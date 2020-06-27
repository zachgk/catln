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
resArrowDestType src (ResEArrow obj arr) = arrowDestType src obj arr
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
    exFromArrow obj@(Object _ _ _ objVars _) arrow@(Arrow (Typed am) compAnnots _ maybeExpr) = fmap (\expr -> do
            let am' = case am of
                  (TypeVar (TVVar v)) -> case H.lookup v objVars of
                    Just (Typed t) -> t
                    Nothing -> error "Bad TVVar in makeBaseEnv"
                  (TypeVar (TVArg v)) -> case H.lookup v $ formArgMetaMap obj of
                    Just argMeta -> getMetaType argMeta
                    Nothing -> error "Bad TVArg in makeBaseEnv"
                  _ -> am
            resArrowTree <- buildExprImp baseEnv obj expr am'
            compAnnots' <- mapM (buildCompAnnot baseEnv obj) compAnnots
            return (arrow, (resArrowTree, compAnnots'))
      ) maybeExpr

buildCompAnnot :: (Eq f, Hashable f) => TBEnv f -> TBObject -> TBCompAnnot -> CRes (ResArrowTree f)
buildCompAnnot env obj (CompAnnot "assert" args) = case (H.lookup "test" args, H.lookup "msg" args) of
    (Just test, Just msgExpr) -> do
      test' <- buildExprImp env obj test boolType
      msg' <- buildExprImp env obj msgExpr strType
      return $ ResArrowTuple "assert" (H.fromList [("test", test'), ("msg", msg')])
    (Just test, Nothing) -> do
      test' <- buildExprImp env obj test boolType
      return $ ResArrowTuple "assert" (H.singleton "test" test')
    _ -> CErr [BuildTreeCErr "Invalid assertion"]
buildCompAnnot _ _ (CompAnnot name _ )= CErr [BuildTreeCErr $ "Unknown compiler annotation" ++ name]

buildExpr :: (Eq f, Hashable f) => TBEnv f -> TBObject -> TBExpr -> CRes (ResArrowTree f)
buildExpr _ _ (CExpr _ c) = return $ ResArrowSingle (ConstantArrow c)
buildExpr (_, valEnv) _ (Value (Typed (SumType prodTypes)) name) = case splitPartialLeafs prodTypes of
    (_:_:_) -> CErr [BuildTreeCErr $ "Found multiple types for value " ++ name ++ "\n\t" ++ show prodTypes]
    [] -> CErr [BuildTreeCErr $ "Found no types for value " ++ name ++ " with type " ++ show prodTypes]
    [prodType] -> return $ case H.lookup prodType valEnv of
      Just val -> ResArrowSingle val
      Nothing -> ResArrowTuple name H.empty
buildExpr _ _ (Arg (Typed tp) name) = return $ ResArrowSingle $ ArgArrow tp name
buildExpr env obj (TupleApply (Typed (SumType prodTypes)) (Typed baseType, baseExpr) argExprs) = case splitPartialLeafs prodTypes of
    (_:_:_) -> CErr [BuildTreeCErr $ "Found multiple types for tupleApply " ++ show baseExpr ++ "\n\t" ++ show prodTypes ++ "\n\t" ++ show argExprs]
    [] -> CErr [BuildTreeCErr $ "Found no types for tupleApply " ++ show baseExpr ++ " with type " ++ show prodTypes ++ " and exprs " ++ show argExprs]
    [(_, _, leafType)] | H.keysSet argExprs `isSubsetOf` H.keysSet leafType -> do
                           baseBuild <- buildExprImp env obj baseExpr baseType
                           argVals <- mapM (\(valDestType, expr) -> buildExprImp env obj expr valDestType) $ H.intersectionWith (,) leafType argExprs
                           return $ ResArrowTupleApply baseBuild argVals
    _ -> CErr [BuildTreeCErr $ "Found bad types for tupleApply " ++ show baseExpr]
buildExpr _ _ _ = error "Bad buildExpr"

envLookupTry :: (Eq f, Hashable f) => TBEnv f -> TBObject -> VisitedArrows f -> PartialType -> Type -> ResArrow f -> CRes (ResArrowTree f)
envLookupTry _ _ _ srcType destType resArrow | hasType (resArrowDestType srcType resArrow) destType = return $ ResArrowSingle resArrow
envLookupTry _ _ visitedArrows _ _ resArrow | S.member resArrow visitedArrows = CErr [BuildTreeCErr "Found cyclical use of function"]
envLookupTry env obj visitedArrows srcType destType resArrow = do
  let (SumType newLeafTypes) = resArrowDestType srcType resArrow
  let visitedArrows' = S.insert resArrow visitedArrows
  let obj' = case resArrow of
        (ResEArrow o _) -> o
        _ -> obj
  let eitherAfterArrows = partitionCRes $ map (\leafType -> (leafType,) <$> envLookup env obj' visitedArrows' leafType destType) $ splitPartialLeafs newLeafTypes
  case eitherAfterArrows of
    ([], afterArrows) -> do
      let maybeAfterArrowTree = H.fromList <$> sequence afterArrows
      fmap (ResArrowCompose (ResArrowSingle resArrow) . ResArrowMatch) maybeAfterArrowTree
    (errNotes, _) -> wrapCErr errNotes "Failed envLookupTry"

buildGuardArrows :: (Eq f, Hashable f) => TBEnv f -> TBObject -> VisitedArrows f -> PartialType -> Type -> ([ResArrow f], [(TBExpr, ResArrow f)], [ResArrow f]) -> CRes (ResArrowTree f)
buildGuardArrows env obj visitedArrows srcType destType guards = case guards of
      ([], [], []) -> CErr [BuildTreeCErr $ printf "No arrows found on lookup from %s to %s" (show srcType) (show destType)]
      (_, _, _:_:_) -> CErr [BuildTreeCErr "Multiple ElseGuards"]
      (noGuard, ifGuards, elseGuard) | not (null noGuard) -> case partitionCRes $ map ltry noGuard of
                          (_, resArrowTree:_) -> resArrowTree
                          (errNotes1, _) -> case buildGuardArrows env obj visitedArrows srcType destType ([], ifGuards, elseGuard) of
                            r@CRes{} -> r
                            CErr errNotes2 -> wrapCErr (errNotes1 ++ errNotes2) $ "Failed to lookup noGuard arrow from " ++ show srcType ++ " to " ++ show destType ++ "\n\tNoGuard: " ++ show noGuard
      ([], _, []) -> CErr [BuildTreeCErr "Missing ElseGuard on envLookup"]
      ([], ifGuards, [elseGuard]) | not (null ifGuards) -> do
                                      let maybeIfTreePairs = mapM (\(ifCond, ifThen@(ResEArrow o _)) -> sequenceT (buildExprImp env o ifCond boolType, ltry ifThen)) ifGuards
                                      let maybeElseTree = ltry elseGuard
                                      case sequenceT (maybeIfTreePairs, maybeElseTree) of
                                        (CRes notes (ifTreePairs, elseTree)) -> CRes notes $ ResArrowCond ifTreePairs elseTree
                                        CErr notes -> wrapCErr notes "No valid ifTrees:"
      _ -> CErr [BuildTreeCErr "Unknown arrows found in envLookup"]
  where
    ltry = envLookupTry env obj visitedArrows srcType destType

envLookup :: (Eq f, Hashable f) => TBEnv f -> TBObject -> VisitedArrows f -> PartialType -> Type -> CRes (ResArrowTree f)
envLookup _ _ _ srcType destType | srcType `hasPartial` destType = return ResArrowID
envLookup env@(resEnv, _) obj visitedArrows srcType@(srcName, _, _) destType = case H.lookup srcName resEnv of
  Just resArrowsWithName -> do
    let resArrows = filter (\(arrowType, _, _) -> srcType `subPartialOf` arrowType) resArrowsWithName
    -- TODO: Sort resArrows by priority order before trying
    let guards = (\(a,b,c) -> (concat a, concat b, concat c)) $ unzip3 $ map (\case
                          (_, NoGuard, a) -> ([a], [], [])
                          (_, IfGuard ifCond, ifThen) -> ([], [(ifCond, ifThen)], [])
                          (_, ElseGuard, a) -> ([], [], [a])
                      ) resArrows
    buildGuardArrows env obj visitedArrows srcType destType guards

  Nothing -> CErr [BuildTreeCErr $ "Failed to find any arrows from " ++ show srcType ++ " to " ++ show destType]

buildImplicit :: (Eq f, Hashable f) => TBEnv f -> TBObject -> Type -> Type -> CRes (ResArrowTree f)
buildImplicit _ _ TopType TopType = return ResArrowID
buildImplicit _ _ TopType destType = error $ printf "Build implicit from top type to %s" (show destType)
buildImplicit env obj@(Object _ _ _ objVars _) (TypeVar (TVVar varName)) destType = case H.lookup varName objVars of
  Just objVarM -> buildImplicit env obj (getMetaType objVarM) destType
  Nothing -> error $ printf "buildImplicit unknown arg %s with obj %s" varName (show obj)
buildImplicit env obj (TypeVar (TVArg argName)) destType = case H.lookup argName $ formArgMetaMap obj of
  Just objArgM -> buildImplicit env obj (getMetaType objArgM) destType
  Nothing -> error $ printf "buildImplicit unknown arg %s with obj %s" argName (show obj)
buildImplicit env obj (SumType srcType) destType = do
  matchVal <- sequence $ H.fromList $ map aux $ splitPartialLeafs srcType
  return (ResArrowMatch matchVal)
  where
    aux leafSrcType = (leafSrcType,) $ envLookup env obj S.empty leafSrcType destType

-- executes an expression and then an implicit to a desired dest type
buildExprImp :: (Eq f, Hashable f) => TBEnv f -> TBObject -> TBExpr -> Type -> CRes (ResArrowTree f)
buildExprImp env obj expr destType = do
  t1 <- buildExpr env obj expr
  let (Typed srcType) = getExprMeta expr
  if srcType == destType then
    return t1
    else do
      t2 <- buildImplicit env obj srcType destType
      return $ ResArrowCompose t1 t2

buildPrgm :: (Eq f, Hashable f) => ResBuildEnv f -> PartialType -> Type -> TBPrgm -> CRes (ResArrowTree f, ResExEnv f)
buildPrgm primEnv src dest (objectMap, _) = do
  (env, exEnv) <- makeBaseEnv primEnv objectMap
  let emptyObj = Object (Typed $ SumType $ joinPartialLeafs [src]) FunctionObj "EmptyObj" H.empty H.empty
  rootTree <- envLookup env emptyObj S.empty src dest
  return (rootTree, exEnv)
