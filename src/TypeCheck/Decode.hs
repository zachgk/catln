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
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Maybe          (mapMaybe)
import           MapMeta             (MetaType (ArrMeta), clearMetaDat)
import           Semantics
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
      -- The argument name learned during type inference: the single argument present in the
      -- tuple-application result meta but not in its base meta.
      let learnedArgName = case (getMetaType baseM', getMetaType m') of
            (UnionType Nothing basePartialLeafs [], UnionType Nothing partialLeafs []) -> case (splitUnionType basePartialLeafs, splitUnionType partialLeafs) of
              ([PartialType{ptArgs=basePartialArgs}], [PartialType{ptArgs}]) -> case S.toList $ S.difference (H.keysSet ptArgs) (H.keysSet basePartialArgs) of
                [argN] -> Right argN
                opts -> Left (printf "Failed argument inference due to multiple arg options %s in %s" (show opts) (show expr) :: String)
              (base, result) -> Left (printf "Failed argument inference due to multiple types with base %s and result %s in %s" (show base) (show result) (show expr) :: String)
            (baseM'', m'') -> Left (printf "Failed argument inference due to non UnionType in baseMeta %s or meta %s in %s" (show baseM'') (show m'') (show expr) :: String)
      let mkArgValue argName = Value (mWithType (singletonType $ partialToType argName) $ emptyMetaM m') (pkName argName)
      let decodeArr = mapM (\(oaArrExpr, oaArrM) -> do
            oaArrExpr' <- mapM toExpr oaArrExpr
            oaArrM' <- toMeta oaArrM
            return (oaArrExpr', oaArrM'))
      (oaObj', oaArr') <- case (oaObj, oaArr) of
        -- An anonymous applied argument (e.g. the call site @f(x)@): finalize the learned
        -- keyword as the object.
        (Nothing, _) -> do
          argName <- case learnedArgName of
            Right n  -> return n
            Left err -> lift $ TypeCheckResE [GenTypeCheckError mclear err]
          arr' <- decodeArr oaArr
          return (Just $ mkArgValue argName, arr')
        -- A destructuring pattern (e.g. @Box(v)@) whose head does not name the argument it
        -- fills: rewrite to the learned keyword form (@b= Box(v)@) so that downstream
        -- TreeBuild and Eval bind by the argument name rather than the pattern's type head.
        (Just joaObj@TupleApply{}, Just (Nothing, _arrM))
          | Right argName <- learnedArgName
          , makeAbsoluteName (pkName argName) /= makeAbsoluteName (exprPath joaObj) -> do
              joaObj' <- toExpr joaObj
              -- The argument slot takes the pattern's own (resolved) type, mirroring the named
              -- form @b= Box(v)@ where the arr slot is unified with the arr value's type.
              return (Just $ mkArgValue argName, Just (Just joaObj', getExprMeta joaObj'))
        -- An explicit object (named argument, or a pattern whose head already names the arg).
        (Just joaObj, _) -> do
          joaObj' <- toExpr joaObj
          arr' <- decodeArr oaArr
          return (Just joaObj', arr')
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
verifyObjMapItem name (ObjectMapItem decls runtimeDecls refineDecls defs) = do
  FEnv{feTypeEnv} <- get
  let prgmTypeEnv = feTypeEnv{tePrgmEnv = True}
  -- A runtime declaration has no body, but it behaves like a definition for coverage: it is
  -- bounded by the declarations (checked against declUnion) and contributes to the definition
  -- coverage that fulfills them (part of defUnion).
  let defLikeOAs = defs ++ runtimeDecls
  case (map (getMetaType . getExprMeta . oaObjExpr) decls, map (getMetaType . getExprMeta . oaObjExpr) defLikeOAs) of
    ([], _) -> pure ()
    (_, []) -> pure ()
    (declInputTypes0, defInputTypes0) -> do
      -- A declaration may write a parameterized type in /param form/ (e.g. @Box[T]@) while a
      -- destructuring definition produces it in /field form/ (e.g. @Box(v=...)@, completed to
      -- @Box[T](v=$T)@). Normalize both to the completed form so coverage compares like with like.
      let declInputTypes = map (completeDataPartials feTypeEnv) declInputTypes0
      let defInputTypes = map (completeDataPartials feTypeEnv) defInputTypes0
      let declUnion = unionAllTypes feTypeEnv declInputTypes
      let defUnion = unionAllTypes feTypeEnv defInputTypes

      -- Check each definition (including runtime declarations) against declUnion
      let defsWithLocs = zip defInputTypes (map (getMetaPos . getExprMeta) (mapMaybe oaObj defLikeOAs))
      forM_ defsWithLocs $ \(defType, defLoc) ->
        unless (isSubtypeOf prgmTypeEnv defType declUnion) $ do
          let undeclared = snd $ differenceTypeWithEnv prgmTypeEnv H.empty defType declUnion
          lift $ TypeCheckResult [DefinitionOutsideDeclaration name defType declUnion undeclared defLoc] ()

      -- Check each declaration against defUnion (which includes runtime declarations)
      let declsWithLocs = zip (map (completeDataPartials feTypeEnv . getMetaType . getExprMeta . oaObjExpr) decls) (map (getMetaPos . getExprMeta) (mapMaybe oaObj decls))
      forM_ declsWithLocs $ \(declType, declLoc) ->
        unless (isSubtypeOf prgmTypeEnv declType defUnion) $ do
          let notDefined = snd $ differenceTypeWithEnv prgmTypeEnv H.empty declType defUnion
          lift $ TypeCheckResult [UnfulfilledDeclaration name declType defUnion notDefined declLoc] ()

      -- Check each refinement declaration against declUnion
      let refineWithLocs = zip
            (map (getMetaType . getExprMeta . oaObjExpr) refineDecls)
            (map (getMetaPos . getExprMeta) (mapMaybe oaObj refineDecls))
      forM_ refineWithLocs $ \(refineType, refineLoc) ->
        unless (isSubtypeOf prgmTypeEnv refineType declUnion) $ do
          let undeclared = snd $ differenceTypeWithEnv prgmTypeEnv H.empty refineType declUnion
          lift $ TypeCheckResult [RefineOutsideDeclaration name refineType declUnion undeclared refineLoc] ()

toPrgms :: [VPrgm] -> StateT FEnv TypeCheckResult [TPrgm]
toPrgms = mapM toPrgm
