--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Decode
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module decodes after type checking is completed to produce
-- the final typechecked program.
--------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module TypeCheck.Decode where

import           Control.Monad.State
import           CtConstants
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Maybe          (mapMaybe)
import           Debug.Trace
import           MapMeta             (MetaType (ArrMeta), clearMetaDat)
import           Semantics
import           Semantics.Annots
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           TypeCheck.Common

toMeta :: VarMeta -> StateT FEnv TypeCheckResult TypedMeta
toMeta  m@(Meta pt pos mid _) = do
  env@FEnv{feTypeEnv} <- get
  case descriptor env m of
    TypeCheckResult notes SType{stypeAct=ub, stypeTree=rt} -> lift $ TypeCheckResult notes $ Meta (intersectTypes feTypeEnv ub pt) pos mid rt
    TypeCheckResE notes -> lift $ TypeCheckResult notes (Meta BottomType pos mid Nothing)

member :: String -> [String ] -> Bool
member x arr = case suffixLookup x arr of
  Just _  -> True
  Nothing -> False

toExpr :: VExpr -> StateT FEnv TypeCheckResult TExpr
toExpr (CExpr m c) = do
  m' <- toMeta m
  return $ CExpr m' c
toExpr (Value m name) = do
  let name' = case maybeGetSingleton $ getMetaType m of
        Just PartialType{ptName=n} -> n
        _                          -> makeAbsoluteName name -- TODO Maybe consider this an exception
  m' <- toMeta m
  return $ Value m' name'
toExpr (HoleExpr m hole) = do
  m' <- toMeta m
  return $ HoleExpr m' hole
toExpr (AliasExpr base alias) = do
  base' <- toExpr base
  alias' <- toExpr alias
  return $ AliasExpr base' alias'
toExpr (EWhere m base cond) = do
  m' <- toMeta m
  base' <- toExpr base
  cond' <- toExpr cond
  return $ EWhere m' base' cond'
toExpr expr@(TupleApply m (baseM, baseExpr) arg) = do
  let mclear = clearMetaDat ArrMeta m
  m' <- toMeta m
  baseM' <- toMeta baseM
  baseExpr' <- toExpr baseExpr
  arg' <- case arg of
    EAppArg a@ObjArr{oaObj, oaArr, oaAnnots} -> do
      oaObj' <- case oaObj of
        Just joaObj -> Just <$> toExpr joaObj
        Nothing -> do
          mArgName <- case (getMetaType baseM', getMetaType m') of
            (UnionType Nothing basePartialLeafs [], UnionType Nothing partialLeafs []) -> case (splitUnionType basePartialLeafs, splitUnionType partialLeafs) of
              ([PartialType{ptArgs=basePartialArgs}], [PartialType{ptArgs}]) -> case S.toList $ S.difference (H.keysSet ptArgs) (H.keysSet basePartialArgs) of
                [argN] -> return $ Just argN
                opts -> lift $ TypeCheckResult [GenTypeCheckError mclear $ printf "Failed argument inference due to multiple arg options %s in %s" (show opts) (show expr)] Nothing
              (base, result) -> lift $ TypeCheckResult [GenTypeCheckError mclear $ printf "Failed argument inference due to multiple types with base %s and result %s in %s" (show base) (show result) (show expr)] Nothing
            (baseM'', m'') -> trace (printf "DEBUG inference fail: baseM=%s m=%s expr=%s" (show baseM'') (show m'') (show expr)) $ lift $ TypeCheckResult [GenTypeCheckError mclear $ printf "Failed argument inference due to non UnionType in baseMeta %s or meta %s in %s" (show baseM'') (show m'') (show expr)] Nothing
          return $ case mArgName of
            Just argName -> Just $ Value (mWithType (singletonType $ partialToType argName) $ emptyMetaM m') (pkName argName)
            Nothing -> Nothing -- Failed argument inference, return nothing and error out
      oaArr' <- forM oaArr $ \(oaArrExpr, oaArrM) -> do
        oaArrExpr' <- mapM toExpr oaArrExpr
        oaArrM' <- toMeta oaArrM
        return (oaArrExpr', oaArrM')
      oaAnnots' <- mapM toExpr oaAnnots
      return $ EAppArg a{oaObj=oaObj', oaArr=oaArr', oaAnnots=oaAnnots'}
    EAppVar vn vm -> EAppVar vn <$> toMeta vm
    EAppSpread a -> EAppSpread <$> toExpr a
  return $ TupleApply m' (baseM', baseExpr') arg'

toObjArr :: VObjArr -> StateT FEnv TypeCheckResult TObjArr
toObjArr oa@ObjArr{oaObj, oaArr, oaAnnots} = do
  oaObj' <- mapM toExpr oaObj
  oaArr' <- forM oaArr $ \(arrE, arrM) -> do
    arrE' <- mapM toExpr arrE
    arrM' <- toMeta arrM
    return (arrE', arrM')
  oaAnnots' <- mapM toExpr oaAnnots
  return oa{oaObj=oaObj', oaArr=oaArr', oaAnnots=oaAnnots'}

toPrgm :: VPrgm -> StateT FEnv TypeCheckResult TPrgm
toPrgm (objMap, classGraph, annots) = do
  objMap' <- mapMObjectMap toObjArr objMap
  annots' <- mapM toExpr annots

  -- Verify declaration/definition input types match after decoding
  let (ObjectMap objMapHash') = objMap'
  _ <- H.traverseWithKey verifyObjMapItem objMapHash'

  return $ Prgm objMap' classGraph annots'

verifyObjMapItem :: TypeName -> ObjectMapItem Expr TypedMetaDat -> StateT FEnv TypeCheckResult ()
verifyObjMapItem name (ObjectMapItem decls refineDecls defs) = do
  FEnv{feTypeEnv} <- get
  let prgmTypeEnv = feTypeEnv{tePrgmEnv = True}
  case (map (getMetaType . getExprMeta . oaObjExpr) decls, map (getMetaType . getExprMeta . oaObjExpr) defs) of
    ([], _) -> pure ()
    (_, []) -> pure ()
    (declInputTypes, defInputTypes) -> do
      let declUnion = unionAllTypes feTypeEnv declInputTypes
      let defUnion = unionAllTypes feTypeEnv defInputTypes

      -- Check each definition against declUnion
      let defsWithLocs = zip defInputTypes (map (getMetaPos . getExprMeta) (mapMaybe oaObj defs))
      forM_ defsWithLocs $ \(defType, defLoc) ->
        unless (isSubtypeOf prgmTypeEnv defType declUnion) $ do
          let undeclared = snd $ differenceTypeWithEnv prgmTypeEnv H.empty defType declUnion
          lift $ TypeCheckResE [DefinitionOutsideDeclaration name defType declUnion undeclared defLoc]

      -- Check each declaration against defUnion, but skip #runtime declarations (they don't need definitions)
      let nonRuntimeDecls = filter (not . hasAnnot runtimeAnnot) decls
      let declsWithLocs = zip (map (getMetaType . getExprMeta . oaObjExpr) nonRuntimeDecls) (map (getMetaPos . getExprMeta) (mapMaybe oaObj nonRuntimeDecls))
      forM_ declsWithLocs $ \(declType, declLoc) ->
        unless (isSubtypeOf prgmTypeEnv declType defUnion) $ do
          let notDefined = snd $ differenceTypeWithEnv prgmTypeEnv H.empty declType defUnion
          lift $ TypeCheckResE [UnfulfilledDeclaration name declType defUnion notDefined declLoc]

      -- Check each refinement declaration against declUnion
      let refineWithLocs = zip
            (map (getMetaType . getExprMeta . oaObjExpr) refineDecls)
            (map (getMetaPos . getExprMeta) (mapMaybe oaObj refineDecls))
      forM_ refineWithLocs $ \(refineType, refineLoc) ->
        unless (isSubtypeOf prgmTypeEnv refineType declUnion) $ do
          let undeclared = snd $ differenceTypeWithEnv prgmTypeEnv H.empty refineType declUnion
          lift $ TypeCheckResE [RefineOutsideDeclaration name refineType declUnion undeclared refineLoc]

toPrgms :: [VPrgm] -> StateT FEnv TypeCheckResult [TPrgm]
toPrgms = mapM toPrgm
