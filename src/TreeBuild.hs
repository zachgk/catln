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
import           Data.List           (nub, partition)
import           Data.Maybe

import           Text.Printf

import           Control.Monad
import           CRes
import           Data.Either
import           Eval.Common
import           Semantics
import           Semantics.Annots
import           Semantics.Prgm
import           Semantics.TypeGraph
import           Semantics.Types

type TBMetaDat = Maybe ReachesTree
type TBMeta = Meta TBMetaDat
type TBExpr = Expr TBMetaDat
type TBCompAnnot = CompAnnot TBExpr
type TBObjArr = ObjArr Expr TBMetaDat
type TBGuard = Maybe TBExpr
type TBObjectMap = ObjectMap Expr TBMetaDat
type TBPrgm = Prgm Expr TBMetaDat

type VisitedArrows = S.HashSet TCallTree

leafsFromMeta :: TBMeta -> [PartialType]
leafsFromMeta Meta{getMetaType=UnionType Nothing prodTypes consts} = splitUnionType prodTypes ++ map constantPartialType consts
leafsFromMeta m = error $ printf "leafFromMeta with invalid type: %s" (show m)

buildTBEnv :: ResBuildPrims -> TBPrgm -> TBEnv
buildTBEnv primEnv prgm@Prgm{prgmObjMap} = baseEnv
  where
    baseEnv = TBEnv "" resEnv prgm (mkTypeEnv prgm){tePrgmEnv=True, teArgModeProjection=True}
    resEnv = H.fromListWith (++) $ mapMaybe resFromArrow $ flatObjectMap prgmObjMap

    resFromArrow oa@ObjArr{oaObj=Just{}, oaArr, oaAnnots} = case oaArr of
      _ | getExprType (oaObjExpr oa) == PTopType -> error $ printf "buildTBEnv failed with a topType input in %s" (show oa)
      Just (Just _, _) -> Just (oaObjPath oa, [(objLeaf, Just oa, TCObjArr oa) | objLeaf <- leafsFromMeta (getExprMeta $ oaObjExpr oa)])
      Just (Nothing, _) | isJust (getRuntimeAnnot oaAnnots) -> Just (oaObjPath oa, [(objLeaf, Just oa, usePrim oa $ H.lookup (mkName $ fromJust $ getRuntimeAnnot oaAnnots) primEnv) | objLeaf <- leafsFromMeta (getExprMeta $ oaObjExpr oa)])
      Just (Nothing, _) -> Nothing
      Nothing -> Nothing
    resFromArrow oa = error $ printf "resFromArrow with no input expression: %s" (show oa)

    usePrim oa (Just (Left prim)) = TCPrim oa prim
    usePrim oa (Just (Right macro)) = TCMacro oa macro
    usePrim oa Nothing = error $ printf "Missing runtime %s for %s" (show $ getRuntimeAnnot $ oaAnnots oa) (show oa)

-- | arrowFollowup accepts a srcPartial, a destType, and a ResBuildItem that should convert the src to dest
-- | However, it may also be necessary to use several arrows in sequence to go from src to dest
-- | This will check if the single arrow is correct, or whether a sequence is required
-- | It will then return all the ones that correctly go from src to dest (replacing them with the full sequence)
arrowFollowup ::  TBEnv -> [ObjSrc] -> VisitedArrows -> PartialType -> Type -> (ResBuildEnvItem, Type) -> CRes ResBuildEnvItem
arrowFollowup TBEnv{tbTypeEnv} os _ _ destType (item, newSrcType) | isSubtypeOfWithObjSrcs tbTypeEnv os newSrcType destType = return item
-- envLookupTry _ _ _ srcType destType _ | trace (printf "envLookupTry from %s to %s" (show srcType) (show destType)) False = undefined
arrowFollowup _ _ visitedArrows _ _ ((_, _, resArrow), _) | S.member resArrow visitedArrows = CErr [MkCNote $ BuildTreeCErr Nothing "Found cyclical use of function"]
arrowFollowup _ _ _ _ _ (_, PTopType) = CErr [MkCNote $ BuildTreeCErr Nothing "Arrow has indeterminate return type"]
arrowFollowup env objSrc visitedArrows srcType destType ((itemPartial, itemOa, resArrow), newSrcType) = do
  afterArrow <- buildCallTree env objSrc' visitedArrows' newSrcType destType
  let resArrow' = TCSeq resArrow afterArrow
  return (itemPartial, itemOa, resArrow')
  where
    visitedArrows' = S.insert resArrow visitedArrows
    objSrc' = case resArrow of
          (TCObjArr oa) -> [(srcType, oa)]
          _             -> objSrc

-- This function takes a desired src partialType and arrows accepting that partial type in sorted order of preference
-- It returns the combination of arrows necessary to accept the full src partialType
completeTreeSet :: TBEnv -> PartialType -> [ResBuildEnvItem] -> CRes [ResBuildEnvItem]
completeTreeSet TBEnv{tbTypeEnv} fullPartial initialOptions
  | inFragment = engineRes
  | otherwise = oldRes
  where
    -- The engine queries decide coverage/accumulation symbolically against the class graph
    -- (no 'PredExpr'/constants); out-of-fragment dispatch keeps the materialized accumulation.
    inFragment = dispatchInFragment fullPartial && all (\(t, _, _) -> dispatchInFragment t) initialOptions

    -- Engine loop: the accumulation is the plain list of contributing option partials;
    -- coverage and does-this-add are 'satGoal' queries over the original small types.
    engineRes = engineAux [] [] False initialOptions
    engineAux :: [ResBuildEnvItem] -> [PartialType] -> Bool -> [ResBuildEnvItem] -> CRes [ResBuildEnvItem]
    engineAux accArrows accOpts _ _ | dispatchCoveredSymbolic tbTypeEnv fullPartial accOpts = return (reverse accArrows)
    engineAux accArrows _ True [] = return (reverse accArrows) -- see the accHasConds workaround below
    engineAux _ accOpts False [] = CErr [MkCNote $ GenMapCErr Nothing (printf "Failed to handle a condition. Could not find enough arrows for the src. Only found %s. Tried guards:" (show accOpts)) (zip (map show initialOptions) (map (return . show) initialOptions))]
    engineAux accArrows accOpts accHasConds (opt@(optType, optOa, _):opts) =
      if dispatchAddsSymbolic tbTypeEnv fullPartial optType accOpts
        then case optOa of
               -- Non-linear pattern: guarded by the implicit equality of its shared positions,
               -- so it must not count toward coverage (see the materialized loop below).
               (Just oa) | not (null (exprNonLinearArgGroups (oaObjExpr oa))) -> engineAux (opt:accArrows) accOpts True opts
               -- Where-condition guard: counts toward coverage (the accHasConds workaround).
               (Just oa) | not (null (exprWhereConds (oaObjExpr oa))) -> engineAux (opt:accArrows) (optType:accOpts) True opts
               _ -> engineAux (opt:accArrows) (optType:accOpts) accHasConds opts
        else engineAux accArrows accOpts accHasConds opts

    -- Materialized accumulation (out-of-fragment fallback):
    -- TODO: This would be drastically improved with type difference
    oldRes = aux [] BottomType False initialOptions
    fullType = singletonType fullPartial
    aux :: [ResBuildEnvItem] -> Type -> Bool -> [ResBuildEnvItem] -> CRes [ResBuildEnvItem]
    aux accArrows accType _ _ | isSubtypeOf tbTypeEnv fullType accType = return (reverse accArrows) -- Completed the tree set

    -- This line below avoids testing the completeness of the treeset (and the accType) when the accHasConds is True
    -- It is a workaround due to lack of support for refinement types (with predicates in them)
    -- TODO Remove need for this line (and the accHasConds argument) by adding refinement types
    aux accArrows _ True [] = return (reverse accArrows)

    aux _ accType False [] = CErr [MkCNote $ GenMapCErr Nothing (printf "Failed to handle a condition. Could not find enough arrows for the src. Only found %s. Tried guards:" (show accType)) (zip (map show initialOptions) (map (return . show) initialOptions))]
    aux accArrows accType accHasConds (opt@(optType, optOa, _):opts) = do
      let accType' = intersectTypes tbTypeEnv fullType $ compactType tbTypeEnv H.empty $ unionTypes tbTypeEnv accType (singletonType optType)

      -- next opt increases accumulation
      if not (isSubtypeOf tbTypeEnv accType' accType)
        then case optOa of
               -- A non-linear pattern (e.g. @f(a@x, b@x)@) is guarded by the implicit equality of
               -- its shared positions, but — unlike a where-guard — that constraint is not carried
               -- as a predicate in its type. So keep the accumulation UNCHANGED (do not let it count
               -- toward full coverage), which preserves the unconditional fallback that the equality
               -- guard falls through to. The arrow itself is still added.
               (Just oa) | not (null (exprNonLinearArgGroups (oaObjExpr oa))) -> aux (opt:accArrows) accType True opts

               -- For options that are guarded by conditions, don't increase the accumulation because the condition may not pass
               -- This will fix most if/else pieces, but can struggle with multiple conditions that combine to cover everything such as <0 and >=0.
               -- TODO Remove need for this additional check by using refinement types
               (Just oa) | not (null (exprWhereConds (oaObjExpr oa))) -> aux (opt:accArrows) accType' True opts

               -- Increase the a
               -- Add to accumulation and add the arrow
               -- TODO: Should use ((optType - accType) ∩ fullType) for insertion, otherwise order may not be correct in matching and match not precise
               _ -> aux (opt:accArrows) accType' accHasConds opts
        -- Don't add to accumulation
        else aux accArrows accType accHasConds opts

-- | This function will check the available arrows, provided in sorted order.
-- | It will return one that is sufficient, if available.
-- | Otherwise, it will combine them with a 'TCCond' to form a sequence of arrows to check
joinArrowsWithCond :: TBEnv -> PartialType -> Type -> [ResBuildEnvItem] -> CRes TCallTree
joinArrowsWithCond env srcType destType guards = case completeTreeSet env srcType guards of
  -- Static coverage failed even though arrows were found and none of them are guarded by a
  -- where-condition. This is the typeclass/virtual-dispatch case: the source has an abstract
  -- class-bounded region (e.g. a generic @(_ :: $T == _ :: $T)[$T -> Eq]@ applied to an
  -- abstract @∀Eq@ argument, or a recursive arm @lh == rh@ over an abstract element type), so
  -- the concrete instance cannot be chosen at compile time. Emit a runtime 'TCMatch' that
  -- dispatches on the actual value's type to the matching concrete arrow.
  CErr _ | not (null guards) && all conditionFree guards ->
    return $ TCMatch destType $ H.fromList [(t, tree) | (t, _, tree) <- guards]
  res -> res >>= buildGuards
  where
    conditionFree (_, Nothing, _) = True
    conditionFree (_, Just oa, _) = null (exprWhereConds (oaObjExpr oa)) && null (exprNonLinearArgGroups (oaObjExpr oa))
    buildGuards :: [ResBuildEnvItem] -> CRes TCallTree
    buildGuards arrows = do
      ifTreePairs <- forM arrows $ \(_, moa, arrTree) -> do
        case moa of
          Nothing -> return (Nothing, arrTree)
          Just oa -> do
            -- A non-linear pattern (@f(a@x, b@x)@) carries no where-condition but is still guarded
            -- by the implicit equality of its shared positions, which 'Eval' checks from the @ifObj@
            -- recorded here (see 'evalCallTree' for 'TCCond'). So such an arrow is kept as a guarded
            -- branch rather than an unconditional one.
            let ifConds = exprWhereConds (oaObjExpr oa)
            if null ifConds && null (exprNonLinearArgGroups (oaObjExpr oa))
              then return (Just ([], oa), arrTree)
              else do
                ifConds' <- forM ifConds $ \ifCond ->
                  toTExprDest env [(srcType, oa)] ifCond (emptyMetaT boolType)
                return (Just (ifConds', oa), arrTree)
      case ifTreePairs of
        -- TODO Maybe need to check the [(Just{}, t)] case where there is a condition, but it is the only arrow so it has to pass
        [(_, t)] -> return t
        _        -> return $ TCCond destType ifTreePairs

-- | This handles the behavior for the else arrows
-- | It will move them to the end of the sorted list (as the last resort).
-- | However, if there are too many else arrows (>1) it will error.
handleElseArrows :: [ResBuildEnvItem] -> CRes [ResBuildEnvItem]
handleElseArrows items = case partitionEithers $ map splitElse items of
  (is, []) -> return is
  (is, [els]) -> return (is ++ [els])
  (_, tooManyEls) -> CErr [MkCNote $ BuildTreeCErr Nothing $ printf "Multiple ElseGuards found: %s" (show tooManyEls)]
  where
    splitElse item@(_, Nothing, _) = Left item
    splitElse item@(_, Just oa, _) = if hasElseAnnot oa
      then Right item
      else Left item

-- | This should sort arrows in priority order before trying
-- TODO Actually implement the sortArrows behavior
-- Non-linear arrows (e.g. @f(a@x, b@x)@) are moved to the front so their implicit-equality guard
-- is tried before any unconditional catch-all definition it overlaps (which would otherwise be
-- selected first and shadow them). Every other arrow keeps its relative order, so programs without
-- non-linear patterns are unaffected.
sortArrows :: TBEnv -> [ResBuildEnvItem] -> CRes [ResBuildEnvItem]
sortArrows _ = return . uncurry (++) . partition isNonLinear
  where
    isNonLinear (_, Just oa, _) = not $ null $ exprNonLinearArgGroups $ oaObjExpr oa
    isNonLinear _               = False

-- | This will find a single arrow (from the objMap, primitives, macros, or args) from the src type
findResArrows :: TBEnv -> [ObjSrc] -> PartialType -> CRes [ResBuildEnvItem]
findResArrows TBEnv{tbResEnv, tbTypeEnv} os srcType@PartialType{ptName=srcName} = case argArrows ++ globalArrows of
  arrows@(_:_) -> return arrows
  [] -> CErr [MkCNote $ BuildTreeCErr Nothing $ printf "Failed to find any arrows from %s" (show srcType)]
  where
    globalArrows = case suffixLookupInDict srcName tbResEnv of
      Just resArrowsWithName -> filter (\(arrowType, _, _) -> not $ isBottomType $ intersectTypes tbTypeEnv (singletonType srcType) (singletonType arrowType)) resArrowsWithName
      Nothing ->
        let srcBase = reverse $ takeWhile (/= '/') $ reverse (nameStr srcName)
            matchingEntries = concatMap snd $ filter (\(k, _) -> reverse (takeWhile (/= '/') (reverse (nameStr k))) == srcBase) $ H.toList tbResEnv
        -- Replace arrowType with srcType so completeTreeSet can compute a non-empty intersection
        in map (\(_, moa, tree) -> (srcType, moa, tree)) matchingEntries
    argArrows :: [ResBuildEnvItem]
    argArrows = map ((srcType, Nothing,) . (`TCArg` nameStr srcName) . snd) $ mapMaybe (H.lookup $ TVArg $ partialToKey srcType) $ exprVarArgsWithObjSrcs tbTypeEnv os

-- | Builds a call tree to convert something from the partial 'srcType' to 'destType'
buildPartialCallTree :: TBEnv -> [ObjSrc] -> VisitedArrows -> PartialType -> Type -> CRes TCallTree
buildPartialCallTree TBEnv{tbTypeEnv} os _ srcType destType | isSubtypeOfWithObjSrcs tbTypeEnv os (singletonType srcType) destType = return TCTId
buildPartialCallTree env@TBEnv{tbTypeEnv} os visitedArrows srcType destType = do
  wrapCRes (printf "Failed to build arrow tree from %s to %s" (show srcType) (show destType)) $ do
    resArrows <- findResArrows env os srcType
    resArrows' <- sortArrows env resArrows
    resArrows'' <- handleElseArrows resArrows'
    let resArrows''WithPostTypes = concatMap (\item@(_, _, resArrow) -> map (item,) $ resArrowDestType tbTypeEnv srcType resArrow) resArrows''
    let resArrows''' = mapMaybe (cresToMaybe . arrowFollowup env os visitedArrows srcType destType) resArrows''WithPostTypes
    joinArrowsWithCond env srcType destType resArrows'''

-- | Builds a call tree to convert something from 'srcType' to 'destType'
buildCallTree :: TBEnv -> [ObjSrc] -> VisitedArrows -> Type -> Type -> CRes TCallTree
buildCallTree TBEnv{tbTypeEnv} os _ srcType destType | isSubtypeOfWithObjSrcs tbTypeEnv os srcType destType = return TCTId
-- buildCallTree _ os _ srcType destType | trace (printf "buildCallTree from %s to %s\n\t\twith %s" (show srcType) (show destType) (show $ map fst os)) False = undefined
buildCallTree _ _ _ _ PTopType = return TCTId
buildCallTree _ _ _ PTopType _ = error $ printf "buildCallTree from top type"
buildCallTree env@TBEnv{tbTypeEnv} objSrcs visitedArrows (TypeVar v _) destType = case exprVarArgsWithObjSrcs tbTypeEnv objSrcs of
  [vaenv] -> case H.lookup v vaenv of
    Just (_, srcType') -> buildCallTree env objSrcs visitedArrows srcType' destType
    Nothing -> error $ printf "Unknown TypeVar %s in buildCallTree" (show v)
  vaenvs -> error $ printf "Found unhandled zero or multiple vaenvs in builCallTree: %s" (show vaenvs)
-- A source with no concrete leaves but a predicate top (a rel/class form such as @~Boolean@
-- represented as @UnionType (Just (PredRel Boolean, _)) [] []@) carries its members only in the
-- predicate, so expand it to the member union and build from that. Without this it would yield an
-- empty 'TCMatch' that no runtime value can satisfy.
buildCallTree env@TBEnv{tbTypeEnv} os visitedArrows src@(UnionType (Just (preds, _)) srcLeafs consts) destType
  | null (splitUnionType srcLeafs) && null consts
  , let expanded = expandPredicates tbTypeEnv H.empty preds
  , expanded /= src =
      buildCallTree env os visitedArrows expanded destType
buildCallTree env os visitedArrows (UnionType _ srcLeafs consts) destType = do
  let allPartials = splitUnionType srcLeafs ++ map constantPartialType consts
  matchVal <- forM allPartials $ \srcPartial -> do
    t <- buildPartialCallTree env os visitedArrows srcPartial destType
    return (srcPartial, t)
  -- If every source partial resolves to the same call tree, there is nothing to
  -- discriminate at runtime, so use that tree directly instead of a 'TCMatch'. This
  -- collapses the case where a relative call resolves to both a declaration-only
  -- interface arrow and the concrete instance that implements it (they build the same
  -- tree), avoiding a spurious match that a runtime value's argument keys cannot satisfy.
  return $ case nub (map snd matchVal) of
    [t] -> t
    _   -> TCMatch destType $ H.fromList matchVal

toTExpr :: TBEnv -> [ObjSrc] -> Expr TBMetaDat -> CRes (TExpr TBMetaDat)
-- toTExpr _ os e | trace (printf "toTExpr %s \n\twith %s" (show e) (show os)) False = undefined
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
toTExpr env os (EWhere m b a) = do
  b' <- toTExpr env os b
  a' <- toTExpr env os a
  return $ TWhere m b' a'
toTExpr env os (TupleApply m (bm, be) arg) = do
  be' <- case arg of
    EAppVar _ _ -> toTExprDest env os be m
    _           -> toTExprDest env os be bm
  arg' <- case arg of
    EAppArg a     -> EAppArg <$> toTEObjArr env os True a
    EAppVar vn vm -> return $ EAppVar vn vm
    EAppSpread a  -> EAppSpread <$> toTExpr env os a
  return $ TTupleApply m (bm, be') arg'

-- | The Bool marks an ObjArr applied inside an expression ('EAppArg') as opposed to a root
-- definition ('buildRootOA').
toTEObjArr :: TBEnv -> [ObjSrc] -> Bool -> EObjArr -> CRes (ObjArr TExpr TBMetaDat)
-- toTEObjArr _ os _ oa | trace (printf "toTEObjArr %s\n\twith %s" (show oa) (show os)) False = undefined
toTEObjArr env@TBEnv{tbTypeEnv} os inExpr oa@ObjArr{oaObj, oaAnnots, oaArr} = do
  oaObj' <- mapM (toTExpr env os) oaObj
  let os' = case oa of
        ObjArr{oaObj=Just{}, oaArr=Just (Just{}, _)} -> (getSingleton $ getExprType $ oaObjExpr oa, oa) : os
        _ -> os
  -- A call-site lambda's result meta has no consumer bound to drop the raw call partial
  -- 'arrowConstrainUbs' keeps alongside the arrows' results (@fn(v)= v + 1@ infers
  -- @(/operator+(..) + Integer)@), so building the body toward it would short-circuit to
  -- 'TCTId' on the raw leaf and the unreduced tuple would leak out as the lambda's runtime
  -- result. Build the body toward what it settles to instead ('reduceAppliedType'). Only for
  -- an applied argument that is itself function-shaped (it declares parameters) — a plain
  -- value argument (@this= compare(a, b)@) keeps its inferred meta, and a root definition
  -- is not an applied argument at all.
  let isLambda = inExpr && maybe False (not . null . exprAppliedArgs) oaObj
  oaArr' <- forM oaArr $ \(arrExpr, arrM) -> do
    let arrBuildM = if isLambda
          then mWithType (reduceAppliedType tbTypeEnv H.empty (getMetaType arrM)) arrM
          else arrM
    arrExpr' <- forM arrExpr $ \e ->
      toTExprDest env os' e arrBuildM
    return (arrExpr', arrM)
  oaAnnots' <- mapM (toTExpr env os') oaAnnots
  return oa{oaObj=oaObj', oaAnnots=oaAnnots', oaArr=oaArr'}

toTExprDest :: TBEnv -> [ObjSrc] -> Expr TBMetaDat -> EvalMeta -> CRes (TExpr TBMetaDat)
-- toTExprDest _ os e m | trace (printf "toExprDest %s to %s \n\twith %s" (show e) (show (getMetaType m)) (show $ map fst os)) False = undefined
toTExprDest env@TBEnv{tbTypeEnv} os e m = do
  e' <- toTExpr env os e
  let srcT = getMetaType $ getExprMeta e'
  ct <- buildCallTree env os S.empty srcT (getMetaType m)
  -- A reference to an argument of the enclosing definition — bare (@= v@) or applied
  -- (@fn(v= x)@ where @fn@ is a parameter) — is unreduced. When the destination is generic
  -- the dest type is Top, so 'buildCallTree' short-circuits to 'TCTId' — but the argument
  -- must still be resolved (bare) or the closure applied (application) at runtime, or the
  -- raw tuple leaks out as the result value. Force the argument arrow (the tree
  -- 'findResArrows' builds when the destination is typed).
  let argFnTree = case maybeGetSingleton srcT of
        Just sp ->
          case mapMaybe (lookupArgLoose (partialToKey sp)) (exprVarArgsWithObjSrcs tbTypeEnv os) of
            ((_, argT):_) -> Just $ TCArg argT (nameStr (ptName sp))
            []            -> Nothing
        _ -> Nothing
  case ct of
    TCTId | Just at <- argFnTree            -> return $ TCalls m e' at
    TCTId                                   -> return e'
    TCMacro _ (MacroFunction f)             -> fromMacro f
    (TCSeq (TCMacro _ (MacroFunction f)) _) -> fromMacro f
    _                                       -> return $ TCalls m e' ct
  where
    -- The applied-arg partial's key carries accreted 'pkVars' (a declaration's contract
    -- delivered by 'BoundedByObjs') while the venv stores the encode-time structural key
    -- without them — the same 'PartialKey' phase drift 'typeGetArgLoose' tolerates. Fall back
    -- to the unique entry matching by name and applied-argument names.
    lookupArgLoose k venv = case H.lookup (TVArg k) venv of
      Just v  -> Just v
      Nothing -> case [v | (TVArg k', v) <- H.toList venv, pkName k' == pkName k, pkArgs k' == pkArgs k] of
        [v] -> Just v
        _   -> Nothing
    fromMacro f = do
      macroResult <- f e (MacroData env os)
      case macroResult of
        Left val -> return $ TCExpr (emptyMetaT $ singletonType $ getValPartialType val) val
        Right e' -> toTExprDest env os e' m

buildArrow :: TBEnv -> PartialType -> TBObjArr -> CRes (Maybe (TBObjArr, (TExpr TBMetaDat, [TExpr TBMetaDat])))
-- buildArrow _ src oa | trace (printf "buildArrow %s: %s" (show src) (show oa)) False = undefined
buildArrow _ _ ObjArr{oaArr=Nothing} = return Nothing
buildArrow _ _ ObjArr{oaArr=Just (Nothing, _)} = return Nothing
buildArrow env objPartial oa@ObjArr{oaAnnots, oaArr=Just (Just expr, am)} = do
  let env' = env{tbName = printf "arrow %s" (show oa)}
  let objSrc = (objPartial, oa)
  resArrowTree <- toTExprDest env' [objSrc] expr am
  compAnnots' <- forM oaAnnots $ \annot -> do
                                       let annotEnv = env{tbName = printf "globalAnnot %s" (show annot)}
                                       toTExpr annotEnv [objSrc] annot
  return $ Just (oa, (resArrowTree, compAnnots'))

buildRootOA :: TBEnv -> EObjArr -> CRes (ObjArr TExpr TBMetaDat)
buildRootOA env oa = do
  let env' = env{tbName = printf "root"}
  let objSrc = (getSingleton $ getExprType $ oaObjExpr oa, oa)
  toTEObjArr env' [objSrc] False oa

buildRoot :: TBEnv -> TBExpr -> PartialType -> Type -> CRes (TExpr TBMetaDat)
buildRoot env input src dest = do
  let env' = env{tbName = printf "root"}
  let emptyObj = ObjArr (Just input) FunctionObj Nothing [] Nothing
  let objSrc = (src, emptyObj)
  toTExprDest env' [objSrc] input  (emptyMetaT dest)
