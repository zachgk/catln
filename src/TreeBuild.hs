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

import qualified Data.HashMap.Strict as H
import           Data.Maybe
import           Data.Tuple.Sequence
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

type TBEnv f = (ResBuildEnv f, H.HashMap RawPartialType (ResArrow f))

resArrowDestType :: ResArrow f -> RawType
resArrowDestType (ResEArrow _ (Arrow _ _ _ (Just expr))) = (\(Typed t) -> t) $ getExprMeta expr
resArrowDestType (ResEArrow _ (Arrow (Typed tp) _ _ Nothing)) = tp
resArrowDestType (PrimArrow tp _) = tp
resArrowDestType (ConstantArrow CInt{}) = rintType
resArrowDestType (ConstantArrow CFloat{}) = rfloatType
resArrowDestType (ConstantArrow CStr{}) = rstrType
resArrowDestType (ArgArrow tp _) = tp

leafFromMeta :: TBMeta -> RawPartialType
leafFromMeta (Typed RawTopType) = error "leafFromMeta from RawTopType"
leafFromMeta (Typed (RawSumType prodTypes)) = case splitPartialLeafs prodTypes of
  [leafType] -> leafType
  _ -> error $ "Arrow has multiple leaves: " ++ show prodTypes

makeBaseEnv :: ResBuildEnv f -> TBObjectMap -> CRes (TBEnv f, ResExEnv f)
makeBaseEnv primEnv objMap = fmap (baseEnv,) exEnv
  where
    baseEnv = (H.union primEnv resEnv, H.empty)
    resEnv = H.fromListWith (++) $ concatMap resFromArrows $ H.toList objMap
    resFromArrows (obj, arrows) = mapMaybe (resFromArrow obj) arrows
    resFromArrow obj@(Object om _ _ _) arrow@(Arrow _ _ aguard expr) = fmap (const (leafFromMeta om, [(aguard, ResEArrow obj arrow)])) expr
    exEnv = fmap H.fromList $ sequence $ concatMap exFromArrows $ H.toList objMap
    exFromArrows (obj, arrows) = mapMaybe (exFromArrow obj) arrows
    exFromArrow _ arrow@(Arrow (Typed am) compAnnots _ maybeExpr) = fmap (\expr -> do
          resArrowTree <- buildExprImp baseEnv expr am
          compAnnots' <- mapM (buildCompAnnot baseEnv) compAnnots
          return (arrow, (resArrowTree, compAnnots'))
      ) maybeExpr

buildCompAnnot :: TBEnv f -> TBCompAnnot -> CRes (ResArrowTree f)
buildCompAnnot env (CompAnnot "assert" args) = case (H.lookup "test" args, H.lookup "msg" args) of
    (Just test, Just msgExpr) -> do
      test' <- buildExprImp env test rboolType
      msg' <- buildExprImp env msgExpr rstrType
      return $ ResArrowTuple "assert" (H.fromList [("test", test'), ("msg", msg')])
    (Just test, Nothing) -> do
      test' <- buildExprImp env test rboolType
      return $ ResArrowTuple "assert" (H.singleton "test" test')
    _ -> CErr [BuildTreeCErr "Invalid assertion"]
buildCompAnnot _ (CompAnnot name _ )= CErr [BuildTreeCErr $ "Unknown compiler annotation" ++ name]

buildExpr :: TBEnv f -> TBExpr -> CRes (ResArrowTree f)
buildExpr _ (CExpr _ c) = return $ ResArrowSingle (ConstantArrow c)
buildExpr (_, valEnv) (Value (Typed (RawSumType prodTypes)) name) = case splitPartialLeafs prodTypes of
    (_:_:_) -> CErr [BuildTreeCErr $ "Found multiple types for value " ++ name ++ "\n\t" ++ show prodTypes]
    [] -> CErr [BuildTreeCErr $ "Found no types for value " ++ name ++ " with type " ++ show prodTypes]
    [prodType] -> return $ case H.lookup prodType valEnv of
      Just val -> ResArrowSingle val
      Nothing -> ResArrowTuple name H.empty
buildExpr _ (Arg (Typed tp) name) = return $ ResArrowSingle $ ArgArrow tp name
buildExpr env (TupleApply (Typed (RawSumType prodTypes)) (Typed baseType, baseExpr) argExprs) = case splitPartialLeafs prodTypes of
    (_:_:_) -> CErr [BuildTreeCErr $ "Found multiple types for tupleApply " ++ show baseExpr ++ "\n\t" ++ show prodTypes ++ "\n\t" ++ show argExprs]
    [] -> CErr [BuildTreeCErr $ "Found no types for tupleApply " ++ show baseExpr ++ " with type " ++ show prodTypes ++ " and exprs " ++ show argExprs]
    [(_, leafType)] | H.keysSet argExprs `isSubsetOf` H.keysSet leafType -> do
                           baseBuild <- buildExprImp env baseExpr baseType
                           argVals <- mapM (\(valDestType, expr) -> buildExprImp env expr valDestType) $ H.intersectionWith (,) leafType argExprs
                           return $ ResArrowTupleApply baseBuild argVals
    _ -> CErr [BuildTreeCErr $ "Found bad types for tupleApply " ++ show baseExpr]
buildExpr _ _ = error "Bad buildExpr"

envWithVals :: TBEnv f -> H.HashMap RawPartialType (ResArrow f) -> TBEnv f
envWithVals (resEnv, _) vals = (resEnv, vals)

envLookupTry :: TBEnv f -> RawType -> ResArrow f -> CRes (ResArrowTree f)
envLookupTry _ destType resArrow | hasRawType (resArrowDestType resArrow) destType = return $ ResArrowSingle resArrow
envLookupTry env destType resArrow = do
  let (RawSumType newLeafTypes) = resArrowDestType resArrow
  let eitherAfterArrows = partitionCRes $ map (\leafType -> (leafType,) <$> envLookup env leafType destType) $ splitPartialLeafs newLeafTypes
  case eitherAfterArrows of
    ([], afterArrows) -> do
      let maybeAfterArrowTree = H.fromList <$> sequence afterArrows
      fmap (ResArrowCompose (ResArrowSingle resArrow) . ResArrowMatch) maybeAfterArrowTree
    (errNotes, _) -> wrapCErr errNotes "Failed envLookupTry"

buildGuardArrows :: TBEnv f -> RawPartialType -> RawType -> ([ResArrow f], [(TBExpr, ResArrow f)], [ResArrow f]) -> CRes (ResArrowTree f)
buildGuardArrows env srcType destType guards = case guards of
      ([], [], []) -> CErr [BuildTreeCErr "No arrows found on lookup"]
      (_, _, _:_:_) -> CErr [BuildTreeCErr "Multiple ElseGuards"]
      (noGuard, ifGuards, elseGuard) | not (null noGuard) -> case partitionCRes $ map ltry noGuard of
                          (_, resArrowTree:_) -> resArrowTree
                          (errNotes1, _) -> case buildGuardArrows env srcType destType ([], ifGuards, elseGuard) of
                            r@CRes{} -> r
                            CErr errNotes2 -> wrapCErr (errNotes1 ++ errNotes2) $ "Failed to lookup noGuard arrow from " ++ show srcType ++ " to " ++ show destType ++ "\n\tNoGuard: " ++ show noGuard
      ([], _, []) -> CErr [BuildTreeCErr "Missing ElseGuard on envLookup"]
      ([], ifGuards, [elseGuard]) | not (null ifGuards) -> do
                                      let maybeIfTreePairs = mapM (\(ifCond, ifThen) -> sequenceT (buildExprImp env ifCond rboolType, ltry ifThen)) ifGuards
                                      let maybeElseTree = ltry elseGuard
                                      case sequenceT (maybeIfTreePairs, maybeElseTree) of
                                        (CRes notes (ifTreePairs, elseTree)) -> CRes notes $ ResArrowCond ifTreePairs elseTree
                                        CErr notes -> wrapCErr notes "No valid ifTrees:"
      _ -> CErr [BuildTreeCErr "Unknown arrows found in envLookup"]
  where
    ltry = envLookupTry env destType

envLookup :: TBEnv f -> RawPartialType -> RawType -> CRes (ResArrowTree f)
envLookup _ srcType destType | srcType `hasRawPartial` destType = return ResArrowID
envLookup env@(resEnv, _) srcType destType = case H.lookup srcType resEnv of
  Just resArrows -> do
    -- TODO: Sort resArrows by priority order before trying
    let guards = (\(a,b,c) -> (concat a, concat b, concat c)) $ unzip3 $ map (\case
                          (NoGuard, a) -> ([a], [], [])
                          (IfGuard ifCond, ifThen) -> ([], [(ifCond, ifThen)], [])
                          (ElseGuard, a) -> ([], [], [a])
                      ) resArrows
    buildGuardArrows env srcType destType guards

  Nothing -> CErr [BuildTreeCErr $ "Failed to find any arrows from " ++ show srcType ++ " to " ++ show destType]

buildImplicit :: TBEnv f -> RawType -> RawType -> CRes (ResArrowTree f)
buildImplicit _ RawTopType _ = error "Build implicit from top type"
buildImplicit env (RawSumType srcType) destType = do
  matchVal <- sequence $ H.fromList $ map aux $ splitPartialLeafs srcType
  return (ResArrowMatch matchVal)
  where
    aux leafSrcType = (leafSrcType,) $ envLookup env leafSrcType destType

-- executes an expression and then an implicit to a desired dest type
buildExprImp :: TBEnv f -> TBExpr -> RawType -> CRes (ResArrowTree f)
buildExprImp env expr destType = do
  t1 <- buildExpr env expr
  let (Typed srcType) = getExprMeta expr
  if srcType == destType then
    return t1
    else do
      t2 <- buildImplicit env srcType destType
      return $ ResArrowCompose t1 t2

buildPrgm :: ResBuildEnv f -> RawPartialType -> RawType -> TBPrgm -> CRes (ResArrowTree f, ResExEnv f)
buildPrgm primEnv src dest (objectMap, _) = do
  (env, exEnv) <- makeBaseEnv primEnv objectMap
  rootTree <- envLookup env src dest
  return (rootTree, exEnv)
