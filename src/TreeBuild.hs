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
import qualified Data.HashSet          as S
import           Data.Maybe
import           Syntax.Types
import           Syntax

type TBMeta = Typed
type TBCompAnnot = CompAnnot TBMeta
type TBExpr = Expr TBMeta
type TBObject = Object TBMeta
type TBGuard = Guard TBMeta
type TBArrow = Arrow TBMeta
type TBObjectMap = ObjectMap TBMeta
type TBPrgm = Prgm TBMeta
type TBReplRes = ReplRes TBMeta

type TBEnv f = (ResBuildEnv f, H.HashMap LeafType (ResArrow f))

resArrowDestType :: ResArrow f -> Type
resArrowDestType (ResEArrow (Arrow _ _ _ (Just expr))) = (\(Typed t) -> t) $ getExprMeta expr
resArrowDestType (ResEArrow (Arrow (Typed tp) _ _ Nothing)) = tp
resArrowDestType (PrimArrow tp _) = tp
resArrowDestType (ConstantArrow CInt{}) = intType
resArrowDestType (ConstantArrow CFloat{}) = floatType
resArrowDestType (ConstantArrow CStr{}) = strType

leafFromMeta :: TBMeta -> LeafType
leafFromMeta (Typed (SumType prodTypes)) = case S.toList prodTypes of
  [leafType] -> leafType
  _ -> error "Arrow has multiple leaves"

makeBaseEnv :: ResBuildEnv f -> TBObjectMap -> CRes (TBEnv f, ResExEnv f)
makeBaseEnv primEnv objMap = fmap (baseEnv,) exEnv
  where
    baseEnv = (H.union primEnv resEnv, H.empty)
    resEnv = H.fromListWith (++) $ concatMap resFromArrows $ H.toList objMap
    resFromArrows (obj, arrows) = map (resFromArrow obj) arrows
    resFromArrow (Object om _ _) arrow@(Arrow _ _ aguard _) = (leafFromMeta om, [(aguard, ResEArrow arrow)])
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
      test' <- buildExprImp env test boolType
      msg' <- buildExprImp env msgExpr strType
      return $ ResArrowTuple "assert" (H.fromList [("test", test'), ("msg", msg')])
    (Just test, Nothing) -> do
      test' <- buildExprImp env test boolType
      return $ ResArrowTuple "assert" (H.singleton "test" test')
    _ -> CErr [BuildTreeCErr "Invalid assertion"]
buildCompAnnot _ (CompAnnot name _ )= CErr [BuildTreeCErr $ "Unknown compiler annotation" ++ name]

buildExpr :: TBEnv f -> TBExpr -> CRes (ResArrowTree f)
buildExpr _ (CExpr _ c) = return $ ResArrowSingle (ConstantArrow c)
buildExpr (_, valEnv) (Value (Typed (SumType prodTypes)) name) = case S.toList prodTypes of
    (_:_:_) -> CErr [BuildTreeCErr $ "Found multiple types for value " ++ name ++ "\n\t" ++ show prodTypes]
    [] -> CErr [BuildTreeCErr $ "Found no types for value " ++ name ++ " with type " ++ show prodTypes]
    [prodType] -> return $ case H.lookup prodType valEnv of
      Just val -> ResArrowSingle val
      Nothing -> ResArrowTuple name H.empty
buildExpr env (TupleApply (Typed (SumType prodTypes)) (Typed baseType, baseExpr) argExprs) = case S.toList prodTypes of
    (_:_:_) -> CErr [BuildTreeCErr $ "Found multiple types for tupleApply " ++ show baseExpr ++ "\n\t" ++ show prodTypes]
    [] -> CErr [BuildTreeCErr $ "Found no types for tupleApply " ++ show baseExpr ++ " with type " ++ show prodTypes ++ " and exprs " ++ show argExprs]
    [LeafType _ leafType] | H.keysSet argExprs == H.keysSet leafType -> do
                           baseBuild <- buildExprImp env baseExpr baseType
                           argVals <- mapM (\(valDestType, expr) -> buildExprImp env expr (SumType $ S.singleton valDestType)) $ H.intersectionWith (,) leafType argExprs
                           return $ ResArrowTupleApply baseBuild argVals
    _ -> CErr [BuildTreeCErr $ "Found bad types for tupleApply " ++ show baseExpr]

envWithVals :: TBEnv f -> H.HashMap LeafType (ResArrow f) -> TBEnv f
envWithVals (resEnv, _) vals = (resEnv, vals)

envLookupTry :: TBEnv f -> Type -> CRes (ResArrowTree f) -> ResArrow f -> CRes (ResArrowTree f)
envLookupTry _ _ r@CRes{} _ = r
envLookupTry env destType failure resArrow = do
  let (SumType newLeafTypes) = resArrowDestType resArrow
  let eitherAfterArrows = partitionCRes $ map (\leafType -> (leafType,) <$> envLookup env leafType destType) $ S.toList newLeafTypes
  case eitherAfterArrows of
    ([], (notes, afterArrows)) -> do
      let afterArrowTree = H.fromList afterArrows
      CRes notes $ ResArrowCompose (ResArrowSingle resArrow) (ResArrowMatch afterArrowTree)
    (_:_, _) -> failure

envLookup :: TBEnv f -> LeafType -> Type -> CRes (ResArrowTree f)
envLookup _ srcType (SumType destTypes) | S.member srcType destTypes = return ResArrowID
envLookup env@(resEnv, _) srcType destType = case H.lookup srcType resEnv of
  Just guardResArrows -> do
    -- TODO: Sort resArrows by priority order before trying
    -- TODO: Support IfGuard and ElseGuard
    let resArrows = mapMaybe (\case
                                  (NoGuard, resArr) -> Just resArr
                                  _ -> Nothing
                              ) guardResArrows
    let failure = CErr [BuildTreeCErr $ "Failed to lookup arrow for " ++ show (srcType, destType)]
    foldl (envLookupTry env destType) failure resArrows
  Nothing -> CErr [BuildTreeCErr $ "Failed to lookup arrow for " ++ show (srcType, destType)]

buildImplicit :: TBEnv f -> Type -> Type -> CRes (ResArrowTree f)
buildImplicit env (SumType srcType) destType = do
  matchVal <- sequence $ H.fromList $ map aux $ S.toList srcType
  return (ResArrowMatch matchVal)
  where
    aux leafSrcType = (leafSrcType,) $ envLookup env leafSrcType destType

-- executes an expression and then an implicit to a desired dest type
buildExprImp :: TBEnv f -> TBExpr -> Type -> CRes (ResArrowTree f)
buildExprImp env expr destType = do
  t1 <- buildExpr env expr
  let (Typed srcType) = getExprMeta expr
  t2 <- buildImplicit env srcType destType
  return $ ResArrowCompose t1 t2

buildPrgm :: ResBuildEnv f -> LeafType -> Type -> TBPrgm -> CRes (ResArrowTree f, ResExEnv f)
buildPrgm primEnv src dest (objectMap, _) = do
  (env, exEnv) <- makeBaseEnv primEnv objectMap
  rootTree <- envLookup env src dest
  return (rootTree, exEnv)
