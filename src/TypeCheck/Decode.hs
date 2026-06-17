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
  return $ Prgm objMap' classGraph annots'

-- | Detects a /diagonal/ declaration: one whose every argument is typed by a single shared type
-- variable (e.g. @operator==($T l, $T r)@). Returns the variable's bound and the argument names
-- it binds. The arg→variable link is read from the declaration expression's applied args (their
-- arrow metas carry the variable, even though the decoded partial stores each arg as the bound).
diagonalVar :: ObjArr Expr TypedMetaDat -> PartialType -> Maybe (Type, [ArgName])
diagonalVar oa p = case sharedVars of
  [v] | not (H.null (ptArgs p)) && all (`elem` argsForVar v) (H.keys (ptArgs p)) ->
    Just (H.lookupDefault PTopType v (ptVars p), argsForVar v)
  _ -> Nothing
  where
    argVar = mapMaybe argToVar (exprAppliedArgs (oaObjExpr oa))
    argToVar a = case (oaObj a, oaArr a) of
      (Just objE, Just (_, arrM)) -> case getMetaType arrM of
        TypeVar (TVVar v) _ -> Just (inExprSingleton objE, v)
        _                   -> Nothing
      _ -> Nothing
    sharedVars = [v | (v, n) <- H.toList (H.fromListWith (+) [(v, 1 :: Int) | (_, v) <- argVar]), n >= 2]
    argsForVar v = [a | (a, v') <- argVar, v' == v]

-- | The /diagonal projection/ of a definition for the given (shared) argument positions: the
-- intersection of the definition's values at those positions. A diagonal declaration @($T,…,$T)@
-- is fulfilled at a concrete type @t@ iff some definition covers @t@ at /every/ shared position,
-- so the variable's bound must be covered by the union of these projections across definitions.
diagonalProjection :: (TypeGraph tg) => TypeEnv tg -> [ArgName] -> Type -> Type
diagonalProjection typeEnv argNames (UnionType Nothing leafs []) =
  unionAllTypes typeEnv [intersectAllTypes typeEnv (mapMaybe (`typeGetArg` p) argNames) | p <- splitUnionType leafs]
diagonalProjection _ _ _ = BottomType

-- | Universalize /destructuring binding variables/ that appear as argument values in a
-- definition's input type. A pattern like @MyJust(v= lv)@ matches /any/ @MyJust@ regardless
-- of the element, so for declaration-coverage its value at @v@ is the whole field domain,
-- not the singleton of the fresh binding name @lv@ (which would make the definition appear
-- to cover only that one instantiation). A binding variable is an argument whose value is a
-- bare nominal naming nothing defined in the program; concrete pattern values (e.g. @v= 5@,
-- whose value is @Integer@) are defined and so are left precise.
universalizeBindingArgs :: S.HashSet Name -> Type -> Type
universalizeBindingArgs defined = go
  where
    go (UnionType Nothing leafs []) = UnionType Nothing (joinUnionType $ map goPartial $ splitUnionType leafs) []
    go t = t
    goPartial p@PartialType{ptArgs} = p{ptArgs = H.map goArg ptArgs}
    goArg v
      | isBinding v = PTopType
      | otherwise   = go v
    isBinding v = case v of
      UnionType Nothing leafs [] -> case splitUnionType leafs of
        [PartialType{ptName, ptArgs, ptVars}] -> H.null ptArgs && H.null ptVars && not (S.member (makeAbsoluteName ptName) defined)
        _                                     -> False
      _ -> False

-- | The single partial of a type, when it has exactly one.
singleLeaf :: Type -> Maybe PartialType
singleLeaf (UnionType Nothing leafs []) = case splitUnionType leafs of
  [p] -> Just p
  _   -> Nothing
singleLeaf _ = Nothing

verifyObjMapItem :: TypeName -> ObjectMapItem Expr TypedMetaDat -> StateT FEnv TypeCheckResult ()
verifyObjMapItem name (ObjectMapItem decls runtimeDecls refineDecls defs) = do
  FEnv{feTypeEnv, feLocalDefNames} <- get
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

      -- For /coverage/ (does the union of definitions fulfill a declaration?) a destructuring
      -- binding matches any value at its field, so universalize those fields; see
      -- 'universalizeBindingArgs'. This is used only for the fulfillment target below, never for
      -- the definition-within-declaration check, which needs the precise (non-widened) type.
      let ObjArrTypeGraph (ObjectMap objTg) = teTypeGraph feTypeEnv
      let definedNames = (S.map makeAbsoluteName $ H.keysSet objTg) <> feLocalDefNames
      let defInputTypesU = map (universalizeBindingArgs definedNames) defInputTypes
      let defUnionU = unionAllTypes feTypeEnv defInputTypesU

      -- Check each definition (including runtime declarations) against declUnion
      let defsWithLocs = zip defInputTypes (map (getMetaPos . getExprMeta) (mapMaybe oaObj defLikeOAs))
      forM_ defsWithLocs $ \(defType, defLoc) ->
        unless (isSubtypeOf prgmTypeEnv defType declUnion) $ do
          let undeclared = snd $ differenceTypeWithEnv prgmTypeEnv H.empty defType declUnion
          lift $ TypeCheckResult [DefinitionOutsideDeclaration name defType declUnion undeclared defLoc] ()

      -- Check each declaration against defUnion (which includes runtime declarations). Data-type
      -- (TypeObj) declarations are self-defining and need no separate definition, so they are
      -- exempt from the fulfillment check (they still bound definitions via declUnion above).
      let fulfillableDecls = filter ((/= TypeObj) . oaBasis) decls
      forM_ fulfillableDecls $ \oa -> do
        let declType = completeDataPartials feTypeEnv $ getMetaType $ getExprMeta $ oaObjExpr oa
        let declLoc = getMetaPos $ getExprMeta $ oaObjExpr oa
        -- A diagonal declaration (every argument typed by one shared variable, e.g.
        -- @operator==($T l, $T r)@) is fulfilled iff the variable's bound is covered by the
        -- diagonal projection of the definitions, not by the full product. Checking the product
        -- would spuriously demand off-diagonal pairs like @(Integer, Float)@.
        let (checkType, target) = case diagonalVar oa =<< singleLeaf declType of
              Just (varBound, varArgs) -> (varBound, unionAllTypes prgmTypeEnv (map (diagonalProjection prgmTypeEnv varArgs) defInputTypesU))
              Nothing                  -> (declType, defUnionU)
        unless (isSubtypeOf prgmTypeEnv checkType target) $ do
          let notDefined = snd $ differenceTypeWithEnv prgmTypeEnv H.empty checkType target
          lift $ TypeCheckResult [UnfulfilledDeclaration name declType target notDefined declLoc] ()

      -- Check each refinement declaration against declUnion
      let refineWithLocs = zip
            (map (getMetaType . getExprMeta . oaObjExpr) refineDecls)
            (map (getMetaPos . getExprMeta) (mapMaybe oaObj refineDecls))
      forM_ refineWithLocs $ \(refineType, refineLoc) ->
        unless (isSubtypeOf prgmTypeEnv refineType declUnion) $ do
          let undeclared = snd $ differenceTypeWithEnv prgmTypeEnv H.empty refineType declUnion
          lift $ TypeCheckResult [RefineOutsideDeclaration name refineType declUnion undeclared refineLoc] ()

toPrgms :: [VPrgm] -> StateT FEnv TypeCheckResult [TPrgm]
toPrgms vprgms = do
  tprgms <- mapM toPrgm vprgms

  -- Verify declaration/definition input types against the merged program (all modules' object
  -- maps unioned per name). This lets a declaration in one module bound runtime definitions of
  -- the same name in another module (e.g. the generic Eq @operator==@ declaration in algebra.ct
  -- bounds the @#runtime@ Integer/Float implementations in primitives.ct).
  let ObjectMap mergedObjMap = mconcat (map prgmObjMap tprgms)
  _ <- H.traverseWithKey verifyObjMapItem mergedObjMap

  return tprgms
