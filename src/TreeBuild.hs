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
leafsFromMeta Meta{getMetaType=UnionType prodTypes} = splitUnionType prodTypes
leafsFromMeta m = error $ printf "leafFromMeta with invalid type: %s" (show m)

buildTBEnv :: ResBuildPrims -> TBPrgm -> TBEnv
buildTBEnv primEnv prgm@(objMap, _, _) = baseEnv
  where
    baseEnv = TBEnv "" resEnv prgm (mkTypeEnv prgm)
    resEnv = H.fromListWith (++) $ mapMaybe resFromArrow objMap

    resFromArrow oa@ObjArr{oaObj=Just{}, oaArr, oaAnnots} = case oaArr of
      _ | getExprType (oaObjExpr oa) == PTopType -> error $ printf "buildTBEnv failed with a topType input in %s" (show oa)
      Just (Just _, _) -> Just (oaObjPath oa, [(objLeaf, Just oa, TCObjArr oa) | objLeaf <- leafsFromMeta (getExprMeta $ oaObjExpr oa)])
      Just (Nothing, _) | isJust (getRuntimeAnnot oaAnnots) -> Just (oaObjPath oa, [(objLeaf, Just oa, usePrim oa $ H.lookup (fromJust $ getRuntimeAnnot oaAnnots) primEnv) | objLeaf <- leafsFromMeta (getExprMeta $ oaObjExpr oa)])
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
arrowFollowup ::  TBEnv -> [ObjSrc] -> VisitedArrows -> PartialType -> Type -> ResBuildEnvItem -> CRes ResBuildEnvItem
arrowFollowup TBEnv{tbTypeEnv} os _ srcType destType item@(_, _, resArrow) | isSubtypeOfWithObjSrcs tbTypeEnv os (resArrowDestType tbTypeEnv srcType resArrow) destType = return item
-- envLookupTry _ _ _ srcType destType _ | trace (printf "envLookupTry from %s to %s" (show srcType) (show destType)) False = undefined
arrowFollowup _ _ visitedArrows _ _ (_, _, resArrow) | S.member resArrow visitedArrows = CErr [MkCNote $ BuildTreeCErr Nothing "Found cyclical use of function"]
arrowFollowup env@TBEnv{tbTypeEnv} objSrc visitedArrows srcType destType (itemPartial, itemOa, resArrow) = do
  let newSrcType = resArrowDestType tbTypeEnv srcType resArrow
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
-- TODO: This would be drastically improved with type difference
completeTreeSet :: TBEnv -> PartialType -> [ResBuildEnvItem] -> CRes [ResBuildEnvItem]
completeTreeSet TBEnv{tbTypeEnv} fullPartial initialOptions = aux [] BottomType initialOptions
  where
    fullType = singletonType fullPartial
    aux :: [ResBuildEnvItem] -> Type -> [ResBuildEnvItem] -> CRes [ResBuildEnvItem]
    aux accArrows accType _ | isSubtypeOf tbTypeEnv fullType accType = return (reverse accArrows) -- Completed the tree set
    aux _ accType [] = CErr [MkCNote $ GenMapCErr Nothing (printf "Failed to handle a condition. Could not find enough arrows for the src. Only found %s. Tried guards:" (show accType)) (zip (map show initialOptions) (map (return . show) initialOptions))]
    aux accArrows accType (opt@(optType, optOa, _):opts) = do
      let accType' = intersectTypes tbTypeEnv fullType $ compactType tbTypeEnv H.empty $ unionTypes tbTypeEnv accType (singletonType optType)

      -- next opt increases accumulation
      if not (isSubtypeOf tbTypeEnv accType' accType)
        then case optOa of
               -- For options that are guarded by conditions, don't increase the accumulation because the condition may not pass
               -- This will fix most if/else pieces, but can struggle with multiple conditions that combine to cover everything such as <0 and >=0.
               -- TODO Remove need for this additional check by using refinement types
               (Just oa) | not $ null $ exprWhereConds $ oaObjExpr oa -> aux (opt:accArrows) accType opts

               -- Increase the a
               -- Add to accumulation and add the arrow
               -- TODO: Should use ((optType - accType) ∩ fullType) for insertion, otherwise order may not be correct in matching and match not precise
               _ -> aux (opt:accArrows) accType' opts
        -- Don't add to accumulation
        else aux accArrows accType opts

-- | This function will check the available arrows, provided in sorted order.
-- | It will return one that is sufficient, if available.
-- | Otherwise, it will combine them with a 'TCCond' to form a sequence of arrows to check
joinArrowsWithCond :: TBEnv -> PartialType -> Type -> [ResBuildEnvItem] -> CRes TCallTree
joinArrowsWithCond env srcType destType guards = do
  guards' <- completeTreeSet env srcType guards
  buildGuards guards'
  where
    buildGuards :: [ResBuildEnvItem] -> CRes TCallTree
    buildGuards arrows = do
      ifTreePairs <- forM arrows $ \(_, moa, arrTree) -> do
        case moa of
          Nothing -> return (Nothing, arrTree)
          Just oa -> do
            let ifConds = exprWhereConds (oaObjExpr oa)
            if null ifConds
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
sortArrows :: TBEnv -> [ResBuildEnvItem] -> CRes [ResBuildEnvItem]
sortArrows _ = return

-- | This will find a single arrow (from the objMap, primitives, macros, or args) from the src type
findResArrows :: TBEnv -> [ObjSrc] -> PartialType -> CRes [ResBuildEnvItem]
findResArrows TBEnv{tbName, tbResEnv, tbTypeEnv} os srcType@PartialType{ptName=srcName} = case argArrows ++ globalArrows of
  arrows@(_:_) -> return arrows
  [] -> CErr [MkCNote $ BuildTreeCErr Nothing $ printf "Failed to find any arrows from %s" tbName (show srcType)]
  where
    globalArrows = case suffixLookupInDict srcName tbResEnv of
      Just resArrowsWithName -> filter (\(arrowType, _, _) -> not $ isBottomType $ intersectTypes tbTypeEnv (singletonType srcType) (singletonType arrowType)) resArrowsWithName
      Nothing -> []
    argArrows :: [ResBuildEnvItem]
    argArrows = case H.lookup (partialToKey srcType) $ snd $ splitVarArgEnv $ exprVarArgsWithObjSrcs tbTypeEnv os of
      Just (_, argArrowType) -> [(srcType, Nothing, TCArg argArrowType srcName)]
      Nothing                -> []

-- | Builds a call tree to convert something from the partial 'srcType' to 'destType'
buildPartialCallTree :: TBEnv -> [ObjSrc] -> VisitedArrows -> PartialType -> Type -> CRes TCallTree
buildPartialCallTree TBEnv{tbTypeEnv} os _ srcType destType | isSubtypeOfWithObjSrcs tbTypeEnv os (singletonType srcType) destType = return TCTId
buildPartialCallTree env os visitedArrows srcType destType = do
  wrapCRes (printf "Failed to build arrow tree from %s to %s" (show srcType) (show destType)) $ do
    resArrows <- findResArrows env os srcType
    resArrows' <- sortArrows env resArrows
    resArrows'' <- handleElseArrows resArrows'
    let resArrows''' = mapMaybe (cresToMaybe . arrowFollowup env os visitedArrows srcType destType) resArrows''
    joinArrowsWithCond env srcType destType resArrows'''

-- | Builds a call tree to convert something from 'srcType' to 'destType'
buildCallTree :: TBEnv -> [ObjSrc] -> VisitedArrows -> Type -> Type -> CRes TCallTree
buildCallTree TBEnv{tbTypeEnv} os _ srcType destType | isSubtypeOfWithObjSrcs tbTypeEnv os srcType destType = return TCTId
-- buildCallTree _ os srcType destType | trace (printf "buildCallTree from %s to %s\n\t\twith %s" (show srcType) (show destType) (show os)) False = undefined
buildCallTree _ _ _ _ PTopType = return TCTId
buildCallTree _ _ _ PTopType _ = error $ printf "buildCallTree from top type"
buildCallTree env@TBEnv{tbTypeEnv} objSrcs visitedArrows (TypeVar v _) destType = case H.lookup v (exprVarArgsWithObjSrcs tbTypeEnv objSrcs) of
  Just (_, srcType') -> buildCallTree env objSrcs visitedArrows srcType' destType
  Nothing -> error $ printf "Unknown TypeVar %s in buildCallTree" (show v)
buildCallTree env os visitedArrows (UnionType srcLeafs) destType = do
  matchVal <- forM (splitUnionType srcLeafs) $ \srcPartial -> do
    t <- buildPartialCallTree env os visitedArrows srcPartial destType
    return (srcPartial, t)
  return $ case matchVal of
    [(_, t)] -> t
    _        -> TCMatch $ H.fromList matchVal
buildCallTree _ _ _ _ _ = error "Unimplemented buildCallTree"

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
  be' <- toTExprDest env os be bm
  arg' <- case arg of
    EAppArg a     -> EAppArg <$> toTEObjArr env os a
    EAppVar vn vm -> return $ EAppVar vn vm
    EAppSpread a  -> EAppSpread <$> toTExpr env os a
  return $ TTupleApply m (bm, be') arg'

toTEObjArr :: TBEnv -> [ObjSrc] -> EObjArr -> CRes (ObjArr TExpr TBMetaDat)
-- toTEObjArr _ os oa | trace (printf "toTEObjArr %s\n\twith %s" (show oa) (show os)) False = undefined
toTEObjArr env os oa@ObjArr{oaObj, oaAnnots, oaArr} = do
  oaObj' <- mapM (toTExpr env os) oaObj
  let os' = case oa of
        ObjArr{oaObj=Just{}, oaArr=Just (Just{}, _)} -> (fromJust $ maybeGetSingleton $ getExprType $ oaObjExpr oa, oa) : os
        _ -> os
  oaArr' <- forM oaArr $ \(arrExpr, arrM) -> do
    arrExpr' <- forM arrExpr $ \e ->
      toTExprDest env os' e arrM
    return (arrExpr', arrM)
  oaAnnots' <- mapM (toTExpr env os') oaAnnots
  return oa{oaObj=oaObj', oaAnnots=oaAnnots', oaArr=oaArr'}

texprDest :: TBEnv -> [ObjSrc] -> TExpr TBMetaDat -> EvalMeta -> CRes (TExpr TBMetaDat)
-- texprDest _ os e m | trace (printf "texprDest %s to %s \n\twith %s" (show e) (show m) (show os)) False = undefined
texprDest env os e m = do
  ct <- buildCallTree env os S.empty (getMetaType $ getExprMeta e) (getMetaType m)
  case ct of
    TCTId -> return e
    TCMacro _ (MacroFunction f) -> do
      e' <- f e (MacroData env os)
      texprDest env os e' m
    _ -> return $ TCalls m e ct

toTExprDest :: TBEnv -> [ObjSrc] -> Expr TBMetaDat -> EvalMeta -> CRes (TExpr TBMetaDat)
-- toTExprDest _ os e m | trace (printf "toExprDest %s to %s \n\twith %s" (show e) (show m) (show os)) False = undefined
toTExprDest env os e m = do
  e' <- toTExpr env os e
  texprDest env os e' m

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
  let objSrc = (fromJust $ maybeGetSingleton $ getExprType $ oaObjExpr oa, oa)
  toTEObjArr env' [objSrc] oa

buildRoot :: TBEnv -> TBExpr -> PartialType -> Type -> CRes (TExpr TBMetaDat)
buildRoot env input src dest = do
  let env' = env{tbName = printf "root"}
  let emptyObj = ObjArr (Just input) FunctionObj Nothing [] Nothing
  let objSrc = (src, emptyObj)
  toTExprDest env' [objSrc] input  (emptyMetaT dest)
