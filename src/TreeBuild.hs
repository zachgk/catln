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

-- | Whether a source term is allowed to stand as it is when the destination would accept it.
--
-- 'MayHold' is the ordinary reading and says it may: rewriting is a chain, and a term handed on
-- unrewritten is rewritten by the consumer's own followup. 'MustCall' says a leaf the type graph
-- rewrites has to be applied here, because at this position there will be no followup to do it —
-- see 'toTExprDest', the only place that asks for it, for when that is so. The force applies to
-- the top of one build; once the call is made its followups are 'MayHold' again.
data CallForce
  = MustCall
  | MayHold
  deriving (Eq, Show)

leafsFromMeta :: TBMeta -> [PartialType]
leafsFromMeta Meta{getMetaType=UnionType Nothing prodTypes consts} = splitUnionType prodTypes ++ map constantPartialType consts
leafsFromMeta m = error $ printf "leafFromMeta with invalid type: %s" (show m)

buildTBEnv :: ResBuildPrims -> TBPrgm -> TBEnv
buildTBEnv primEnv prgm@Prgm{prgmObjMap} = baseEnv
  where
    baseEnv = TBEnv "" resEnv prgm tbTypeEnv' untyped
    -- The tree build asks the same set-algebra questions over and over — a dispatch decides one
    -- call site by intersecting the source against every arm of the name, and the arms of a class
    -- method re-expand the same handful of classes each time. Those answers are memoized, but only
    -- for an env carrying an identity token, and 'mkTypeEnv' mints none; without one the build ran
    -- every query raw. Two objects paid for it alone: @/Data/Map/containedIn@ and
    -- @/Data/Map/mapEqual@, whose @==@ over a recursive parameterized type took ~95s each while no
    -- other object in the standard library reached 250ms.
    --
    -- The token's contract is one token per identical immutable env VALUE. This env is built once
    -- here, is never derived from (no site modifies it beyond the flags the memos key on), and
    -- lives for the whole build of one program — exactly the shape the typechecker's per-epoch
    -- reaches env has.
    tbTypeEnv0 = (mkTypeEnv prgm){tePrgmEnv=True, teArgModeProjection=True}
    tbTypeEnv' = tbTypeEnv0{teQueryCacheKey = Just (unsafeFreshQueryToken (teTypeGraph tbTypeEnv0))}
    resEnv = H.fromListWith (++) $ mapMaybe resFromArrow $ flatObjectMap prgmObjMap

    -- | A definition with no type at all, which is what a definition that failed to typecheck leaves
    -- behind: there is nothing to enumerate the leaves of, so there is no arrow to be called at.
    isUntyped oa = getExprType (oaObjExpr oa) == PTopType
    untyped = S.fromList [oaObjPath oa | oa@ObjArr{oaObj=Just{}} <- flatObjectMap prgmObjMap, isUntyped oa]

    resFromArrow oa@ObjArr{oaObj=Just{}, oaArr, oaAnnots} = case oaArr of
      -- Left out rather than raised on. A program is typechecked before it is built, so meeting one
      -- of these means the type errors have already been reported and this is their consequence, not
      -- a new failure to announce — and announcing it by raising took out the whole environment,
      -- including every definition that was fine. So the environment is what could be built, and a
      -- call that wanted this one is told in 'findResArrows' where the name is in hand.
      _ | isUntyped oa -> Nothing
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
arrowFollowup ::  TBEnv -> [ObjSrc] -> VisitedArrows -> Type -> PartialType -> Type -> (ResBuildEnvItem, Type) -> CRes ResBuildEnvItem
arrowFollowup TBEnv{tbTypeEnv} os _ _ _ destType (item, newSrcType) | isSubtypeOfWithObjSrcs tbTypeEnv os newSrcType destType = return item
-- envLookupTry _ _ _ srcType destType _ | trace (printf "envLookupTry from %s to %s" (show srcType) (show destType)) False = undefined
arrowFollowup _ _ visitedArrows _ _ _ ((_, _, resArrow), _) | S.member resArrow visitedArrows = CErr [MkCNote $ BuildTreeCErr Nothing "Found cyclical use of function"]
arrowFollowup _ _ _ _ _ _ (_, PTopType) = CErr [MkCNote $ BuildTreeCErr Nothing "Arrow has indeterminate return type"]
arrowFollowup env objSrc visitedArrows bound srcType destType ((itemPartial, itemOa, resArrow), newSrcType) = do
  afterArrow <- buildCallTree env objSrc' visitedArrows' bound MayHold newSrcType destType
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

    -- Coverage of the source could not be completed. Report what was accumulated and list the
    -- candidate arrows tried — but only mention "Tried guards" when there were candidates, so an
    -- empty 'initialOptions' does not leave a dangling label with nothing beneath it.
    coverageFailure acc = GenMapCErr Nothing msg subs
      where
        subs = zip (map show initialOptions) (map (return . show) initialOptions)
        msg = printf "Failed to handle a condition. Could not find enough arrows for the src. Only found %s.%s"
                acc (if null initialOptions then "" else " Tried guards:" :: String)

    -- Engine loop: the accumulation is the plain list of contributing option partials;
    -- coverage and does-this-add are 'satGoal' queries over the original small types.
    engineRes = engineAux [] [] False initialOptions
    engineAux :: [ResBuildEnvItem] -> [PartialType] -> Bool -> [ResBuildEnvItem] -> CRes [ResBuildEnvItem]
    engineAux accArrows accOpts _ _ | dispatchCoveredSymbolic tbTypeEnv fullPartial accOpts = return (reverse accArrows)
    engineAux accArrows _ True [] = return (reverse accArrows) -- see the accHasConds workaround below
    engineAux _ accOpts False [] = CErr [MkCNote $ coverageFailure (show accOpts)]
    engineAux accArrows accOpts accHasConds (opt@(optType, optOa, _):opts) =
      if dispatchAddsSymbolic tbTypeEnv fullPartial optType accOpts
        then case optOa of
               -- Non-linear pattern: guarded by the implicit equality of its shared positions,
               -- so it must not count toward coverage (see the materialized loop below).
               (Just oa) | not (null (exprNonLinearArgGroups (oaObjExpr oa))) -> engineAux (opt:accArrows) accOpts True opts
               -- Where-condition guard: like a non-linear pattern, the condition may not pass, so
               -- the arrow must not count toward coverage. Counting it let a guarded arm whose
               -- DOMAIN already covers the source complete the tree set on its own, so its
               -- siblings were dropped and 'buildGuards' then emitted the lone survivor
               -- unconditionally — the guard was never evaluated.
               (Just oa) | not (null (exprWhereConds (oaObjExpr oa))) -> engineAux (opt:accArrows) accOpts True opts
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

    aux _ accType False [] = CErr [MkCNote $ coverageFailure (show accType)]
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
               (Just oa) | not (null (exprWhereConds (oaObjExpr oa))) -> aux (opt:accArrows) accType True opts

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
findResArrows TBEnv{tbResEnv, tbTypeEnv, tbUntyped} os srcType@PartialType{ptName=srcName} = case argArrows ++ globalArrows of
  arrows@(_:_) -> return arrows
  -- Said as what it is. The definition is there in the program and has no type, so it was left out
  -- of the environment ('buildTBEnv'); reporting that it could not be found would send whoever reads
  -- this looking for a definition that is in front of them.
  [] | S.member srcName tbUntyped -> CErr [MkCNote $ BuildTreeCErr Nothing $ printf "Cannot call %s, which did not typecheck" (show srcType)]
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

-- | Whether the type graph rewrites this leaf — whether it is a call rather than a value. A data
-- construction and a curried partial application are accepted by no arrow and are already values;
-- see 'reduceAppliedType', which reads the same query the other way round.
typeGraphRewrites :: TBEnv -> PartialType -> Bool
typeGraphRewrites TBEnv{tbTypeEnv} p = not $ null $ typeGraphQueryWithReason tbTypeEnv H.empty p

-- | Builds a call tree to convert something from the partial 'srcType' to 'destType'. The
-- @bound@ is a sound upper bound on the runtime VALUE at this position (⊤ when unknown),
-- carried down the rewrite chain — rewriting preserves the value, so a bound established at an
-- outer call applies to every followup source below it.
buildPartialCallTree :: TBEnv -> [ObjSrc] -> VisitedArrows -> Type -> CallForce -> PartialType -> Type -> CRes TCallTree
buildPartialCallTree env@TBEnv{tbTypeEnv} os _ _ force srcType destType
  | isSubtypeOfWithObjSrcs tbTypeEnv os (singletonType srcType) destType
  , not (force == MustCall && typeGraphRewrites env srcType) = return TCTId
buildPartialCallTree env@TBEnv{tbTypeEnv} os visitedArrows bound _force srcType destType = do
  wrapCRes (printf "Failed to build arrow tree from %s to %s\n\t(bound: %s)\n\t(objSrcs: %s)" (show srcType) (show destType) (show bound) (show $ map fst os)) $ do
    resArrows <- findResArrows env os srcType
    resArrows' <- sortArrows env resArrows
    resArrows'' <- handleElseArrows resArrows'
    -- Refine the value bound with this call's declared result instantiated at this call
    -- ('typeGraphDeclaredResult' — def ⊆ decl, and with the call's linked-variable bindings the
    -- declared @List[/T=$_/T]@ reads e.g. @List[/T=Integer]@), then meet each arrow's presented
    -- dest with it. Stored arm metas are materialized at module level (@insertStep(x=∀Ord…)@,
    -- @Cons[/T=∀Ord](…)@), so without the meet a concrete call's followup (toward e.g. DShow)
    -- works on the module-generic presentation and fails even though every value at this call
    -- is concrete. ONLY the settled (data-defined) leaves are met: meeting a RAW call leaf
    -- attaches the bound as a predicate, which lets the subtype cutoffs treat the still-unrewritten
    -- call as already at the destination — the tree then stops rewriting and the unevaluated
    -- call flows through as a runtime value. Raw leaves instead inherit the bound through the
    -- followup recursion, where their own arrows' settled dests are met in turn.
    let declBound = typeGraphDeclaredResult tbTypeEnv H.empty srcType
        bound' = case (bound, declBound) of
          (PTopType, d) -> d
          (b, PTopType) -> b
          (b, d)        -> intersectTypes tbTypeEnv b d
        -- The meet must be against the bound's data-member EXPANSION: meeting a data leaf with
        -- a class-TOP form only meets the dim and attaches the predicate, leaving materialized
        -- module-level arg contents (@head=∀Ord@) unnarrowed — exactly what the membership
        -- checks then read. Expanding materializes the member templates so the meet narrows the
        -- contents (@head=Integer@); nested levels stay class-formed with the narrowed element
        -- and are decided symbolically. The recursion steps through class-of-class members
        -- (@List → ConsList → Cons/Nil@); the fuel bounds pathological mutual class tops.
        expandToMembers fuel t
          | fuel <= (0 :: Int) = t
          | UnionType (Just (preds, negs, [])) pos [] <- t
          , H.null pos, H.null negs
          , let e = expandPredicates tbTypeEnv H.empty preds
          , e /= t
          = expandToMembers (fuel - 1) e
          | otherwise = t
        boundPost t
          | bound' == PTopType || t == PTopType = t
          | UnionType mTop leafs consts <- t =
              let (raws, settled) = partition (null . typeGraphData tbTypeEnv . ptName) (splitUnionType leafs)
                  metPart = intersectTypes tbTypeEnv (UnionType mTop (joinUnionType settled) consts) (expandToMembers 10 bound')
              in unionTypes tbTypeEnv metPart (UnionType Nothing (joinUnionType raws) [])
          | otherwise = t
    let resArrows''WithPostTypes = concatMap (\item@(_, _, resArrow) -> map ((item,) . boundPost) $ resArrowDestType tbTypeEnv srcType resArrow) resArrows''
    let followups = map (arrowFollowup env os visitedArrows bound' srcType destType) resArrows''WithPostTypes
    let resArrows''' = mapMaybe cresToMaybe followups
    -- When every candidate arrow's followup fails, 'joinArrowsWithCond' is handed an empty guard
    -- set and 'completeTreeSet' can only report a contentless "could not find enough arrows …
    -- Tried guards:" — the actual reasons (each arrow's failed chain toward 'destType') having been
    -- dropped by 'cresToMaybe'. In that case, replace the error with those reasons, prefixed by an
    -- explicit note when 'srcType' simply does not reach 'destType' (the innermost recursion names
    -- the stuck type). Only an EXISTING error is enriched — a success passes through untouched — and
    -- a PARTIAL failure (some arrows still build) is left to 'joinArrowsWithCond', which either
    -- completes coverage or forms a runtime 'TCMatch'.
    case joinArrowsWithCond env srcType destType resArrows''' of
      CErr _ | null resArrows''' && not (null followups) ->
        CErr $ reachHint ++ concat [ns | CErr ns <- followups]
      res -> res
  where
    reachHint
      | reachesTo tbTypeEnv H.empty (singletonType srcType) destType = []
      | otherwise = [MkCNote $ BuildTreeCErr Nothing $ printf "The type %s does not reach the destination %s" (show srcType) (show destType)]

-- | Builds a call tree to convert something from 'srcType' to 'destType'. The @bound@ is a
-- sound upper bound on the runtime value here, threaded down from outer calls
-- (see 'buildPartialCallTree'); pass 'PTopType' at roots.
buildCallTree :: TBEnv -> [ObjSrc] -> VisitedArrows -> Type -> CallForce -> Type -> Type -> CRes TCallTree
-- The two whole-source shortcuts below are the 'MayHold' reading of the source: it may stand as
-- it is because the destination accepts it. Under 'MustCall' the question is settled per leaf in
-- 'buildPartialCallTree', which is the only level that can see whether the type graph rewrites it.
buildCallTree TBEnv{tbTypeEnv} os _ _ MayHold srcType destType | isSubtypeOfWithObjSrcs tbTypeEnv os srcType destType = return TCTId
-- buildCallTree _ os _ _ srcType destType | trace (printf "buildCallTree from %s to %s\n\t\twith %s" (show srcType) (show destType) (show $ map fst os)) False = undefined
buildCallTree _ _ _ _ MayHold _ PTopType = return TCTId
buildCallTree _ _ _ _ _ PTopType _ = error $ printf "buildCallTree from top type"
buildCallTree env@TBEnv{tbTypeEnv} objSrcs visitedArrows bound force (TypeVar v _ i) destType = case exprVarArgsWithObjSrcs tbTypeEnv objSrcs of
  [vaenv] -> case H.lookup v vaenv of
    Just (_, srcType') -> buildCallTree env objSrcs visitedArrows bound force (intersectTypes tbTypeEnv srcType' i) destType
    Nothing -> error $ printf "Unknown TypeVar %s in buildCallTree" (show v)
  vaenvs -> error $ printf "Found unhandled zero or multiple vaenvs in builCallTree: %s" (show vaenvs)
-- A source with no concrete leaves but a predicate top (a rel/class form such as @~Boolean@
-- represented as @UnionType (Just (PredRel Boolean, _)) [] []@) carries its members only in the
-- predicate, so expand it to the member union and build from that. Without this it would yield an
-- empty 'TCMatch' that no runtime value can satisfy.
buildCallTree env@TBEnv{tbTypeEnv} os visitedArrows bound force src@(UnionType (Just (preds, _, _)) srcLeafs consts) destType
  | null (splitUnionType srcLeafs) && null consts
  , let expanded = expandPredicates tbTypeEnv H.empty preds
  , expanded /= src =
      buildCallTree env os visitedArrows bound force expanded destType
buildCallTree env@TBEnv{tbTypeEnv} os visitedArrows bound force (UnionType _ srcLeafs consts) destType = do
  let allPartials = splitUnionType srcLeafs ++ map constantPartialType consts
  let tryBuilds = map (\srcPartial -> (srcPartial, buildPartialCallTree env os visitedArrows bound force srcPartial destType)) allPartials
  -- The type checker keeps an applied term in both its raw and settled forms (@5 + 1@ is
  -- @(/operator+(..) + Integer)@), distributed into sibling container leaves
  -- (@Box[/v=/operator+(..)] + Box[/v=Integer]@). The raw leaf can fail to build: a settled
  -- data construction has no arrows of its own, so nothing accepts the raw field. The built
  -- runtime value settles its computed fields (see 'toTEObjArr'), so such a leaf denotes no
  -- runtime value — drop it when its settled form is covered by the leaves that did build.
  -- A deferred value reference (@/Context(/value=/main)@) is stored raw on purpose but never
  -- has a settled sibling (only an applied term is kept in both forms), so its errors — and
  -- every failure without a covering sibling — still propagate.
  let builtType = unionAllTypes tbTypeEnv [singletonType p | (p, res) <- tryBuilds, isJust (cresToMaybe res)]
  let settledRedundant srcPartial =
        -- Ordered cheapest-first: most failures are not raw applied leaves (their reduction is
        -- the identity), and testing that first avoids forcing the sibling builds in 'builtType'.
        let reduced = reduceAppliedTypeDeep tbTypeEnv H.empty (singletonType srcPartial)
        in reduced /= singletonType srcPartial && not (isBottomType builtType) && isSubtypeOfWithObjSrcs tbTypeEnv os reduced builtType
  matchVal <- fmap catMaybes $ forM tryBuilds $ \(srcPartial, res) -> case res of
    CErr{} | settledRedundant srcPartial -> return Nothing
    _                                    -> Just . (srcPartial,) <$> res
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
    _           -> toTExprDest env os be (baseBuildMeta env os be bm)
  arg' <- case arg of
    EAppArg a     -> do
      let baseIsData = maybe False (not . null . typeGraphData (tbTypeEnv env)) (maybeExprPath be)
      EAppArg <$> toTEObjArr env os True baseIsData a
    EAppVar vn vm -> return $ EAppVar vn vm
    EAppSpread a  -> EAppSpread <$> toTExpr env os a
  return $ TTupleApply m (bm, be') arg'

-- | The destination an application's BASE is built toward.
--
-- A bare name is a term in its own right — the partial application it is before anything has been
-- given to it — so an arrow may be written from it (@f= f(b= 2, c= 3)@, the default-arrow form
-- that gives @b@ and @c@ their defaults; @g= f(b= 2, c= 3)@ under another name is the same thing).
-- That arrow has to fire on the base BEFORE this application's argument goes on top, or the
-- argument lands on an empty tuple and the arrow's arguments are lost (@g(a= 1)@ evaluating to
-- @/f(/a=1)@, with no @/b@ or @/c@ for @f@'s definition to read).
--
-- It does not fire on its own: the base meta holds the arrow's result BESIDE the raw name
-- (@ACT (/g + /f(/b=/N))@ — the raw form is kept deliberately, for eval's dispatch keying), so the
-- base is a subtype of its own destination and 'buildCallTree' short-circuits to 'TCTId'. Build it
-- toward what it settles to instead ('reduceAppliedTypeFix'), which is the same medicine an
-- applied argument already gets for the same reason (see 'arrBuildM' in 'toTEObjArr').
--
-- Only a BARE value. A base that is itself an application is a call still being assembled, and its
-- own arrows must wait for the arguments this level is adding rather than firing an arity short of
-- them. 'mightReduce' is the same cheap name-lookup pre-filter used below: with no res-env
-- arrow for the name there is nothing to reduce, and this runs on every eval tree rebuild.
--
-- AN ARROW DOES NOT REWRITE THE BASE OF ITS OWN BODY, which is the rule
-- 'TypeCheck.TypeUtils.dropSelfBaseReach' states at the type level and 'arrowFollowup' refuses
-- outright ("Found cyclical use of function"). For the default arrow that rule is what makes the
-- form mean anything at all: the @f@ on the right of @f= f(b= 2, c= 3)@ is the bare value the arrow
-- rewrites, so rewriting it again is rewriting the arrow's input into its own output. Reducing it
-- here would do exactly that, and the tree built from it recurses without bound — @evalObjArr@
-- enters the arrow to evaluate its own base, and the build-time 'VisitedArrows' guard cannot see it
-- because each eval-time rebuild ('Eval.Env.evalStartEArrow') starts a fresh one. The enclosing
-- definitions are in hand as 'ObjSrc's, so a leaf naming one of them with its argument keys is left
-- as it is and the rest of the base still reduces.
baseBuildMeta :: TBEnv -> [ObjSrc] -> Expr TBMetaDat -> EvalMeta -> EvalMeta
baseBuildMeta TBEnv{tbTypeEnv, tbResEnv} os Value{} bm = case getMetaType bm of
  UnionType mTop leafs consts | any mightReduce others -> mWithType reduced' bm
    where
      (selfs, others) = partition isSelf (splitUnionType leafs)
      reduced' = case selfs of
        [] -> reduceAppliedTypeFix tbTypeEnv H.empty (getMetaType bm)
        _  -> unionAllTypesWithEnv tbTypeEnv H.empty
                (reduceAppliedTypeFix tbTypeEnv H.empty (UnionType mTop (joinUnionType others) consts)
                 : map singletonType selfs)
  _ -> bm
  where
    mightReduce PartialType{ptName} = isJust (suffixLookupInDict ptName tbResEnv)
    isSelf PartialType{ptName, ptArgs} = S.member (makeAbsoluteName ptName, H.keysSet ptArgs) selfKeys
    -- Named from the DEFINITION's own object, never from the 'ObjSrc' src partial beside it: that
    -- partial is an unforced @getSingleton@ ('toTEObjArr'), and a definition whose object type is a
    -- name ambiguity (@/Data/Algebra/eq + /eq@, which core has and eval tolerates) raises the moment
    -- anything forces it.
    selfKeys = S.fromList [ (makeAbsoluteName (oaObjPath oa), H.keysSet $ snd $ splitVarArgEnv $ exprVarArgs $ oaObjExpr oa)
                          | (_, oa@ObjArr{oaObj=Just{}, oaArr=Just (Just{}, _)}) <- os ]
baseBuildMeta _ _ _ bm = bm

-- | The first Bool marks an ObjArr applied inside an expression ('EAppArg') as opposed to a root
-- definition ('buildRootOA'); the second marks the application's base as a data construction
-- (@Box(v= ..)@) as opposed to a function call.
toTEObjArr :: TBEnv -> [ObjSrc] -> Bool -> Bool -> EObjArr -> CRes (ObjArr TExpr TBMetaDat)
-- toTEObjArr _ os _ _ oa | trace (printf "toTEObjArr %s\n\twith %s" (show oa) (show os)) False = undefined
toTEObjArr env@TBEnv{tbTypeEnv, tbResEnv} os inExpr _inData oa@ObjArr{oaObj, oaAnnots, oaArr} = do
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
    -- A computed value stored in a constructed data field (@Box(v= 5 + 1)@) likewise has no
    -- consumer arrow to apply it — a data field only STORES its value, so the raw tuple would
    -- become the field's runtime value. Build it toward what it settles to as well. A bare
    -- value reference (@/Context(value= main)@, no applied arguments) is a deferred value the
    -- construction applies later, not a term to settle here. 'metaMightReduce' is a cheap
    -- name-lookup pre-filter: only a leaf whose name has res-env arrows can be applied at all,
    -- so for any other meta the reduction is a no-op not worth computing (this runs on every
    -- eval tree rebuild).
    -- Under req-only 'BoundedByObjs' a consumer bound no longer drops the raw call partial
    -- from ANY argument's inferred meta — every applied-term argument keeps its raw form
    -- alongside the settled alternatives, not just data-construction fields. Build every such
    -- argument toward what it settles to: the raw self-leaf in the dest would otherwise
    -- short-circuit 'buildCallTree' to 'TCTId' and leak the unreduced tuple to the consumer
    -- (@#assert(test= 2++3==5)@ receiving the @==@ 'TupleVal'). A bare value reference (no
    -- applied arguments, e.g. @/Context(value= main)@) stays deferred as before.
    let isAppliedTerm = maybe False (not . null . exprAppliedArgs) arrExpr
    let metaMightReduce = case getMetaType arrM of
          UnionType _ leafs _ -> any (\PartialType{ptName} -> isJust (suffixLookupInDict ptName tbResEnv)) (splitUnionType leafs)
          _                   -> False
    -- Root definition bodies included: their result meta keeps the body's raw form too, and
    -- the eval-time lazy arm rebuild ('evalObjArrCaptured') evaluates the body tree directly
    -- with no after-chain — an unreduced body tree returns the raw tuple as the arm's result.
    let arrBuildM = if isLambda || (isAppliedTerm && metaMightReduce)
          then mWithType (reduceAppliedTypeFix tbTypeEnv H.empty (getMetaType arrM)) arrM
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
  ct <- buildCallTree env os S.empty PTopType MayHold srcT (getMetaType m)
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
  -- An applied term may stand unrewritten: rewriting is a chain, and the consumer's own followup
  -- is what rewrites it (@mmap@'s body hands its caller a raw @append(..)@ term, and the caller's
  -- followup applies it). A Top destination is the one position where that cannot happen — it
  -- imposes no contract, so there is no followup to own the rewrite and the applied tuple becomes
  -- the consumer's runtime value with nothing to dispatch on. It is what a declared result spelled
  -- by a declaration variable leaves behind (@gid[$B](x -> $_B) -> $_B@ read in a generic frame IS
  -- Top), so ask for the call here rather than leave a term nothing downstream will reduce.
  --
  -- Reached only after the ordinary build has already answered 'TCTId', so a term that does reduce
  -- toward its destination builds exactly as before, and only after 'argFnTree', so a reference to
  -- an enclosing definition's own argument keeps its 'TCArg'. A bare value reference
  -- (@\/Context(value= main)@) is a deferred value rather than a call, so applied args are required.
  let mustCall = getMetaType m == PTopType && not (null (exprAppliedArgs e)) && case srcT of
        UnionType _ srcLeafs _ -> any (typeGraphRewrites env) (splitUnionType srcLeafs)
        _                      -> False
  case ct of
    TCTId | Just at <- argFnTree            -> return $ TCalls m e' at
    TCTId | mustCall                        -> do
              ct' <- buildCallTree env os S.empty PTopType MustCall srcT (getMetaType m)
              return $ case ct' of
                TCTId -> e'
                _     -> TCalls m e' ct'
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
  -- A definition is built from the one type its object has, which is asked for up front and reported
  -- where there is none. A definition that has no single type is one that did not typecheck, so the
  -- type errors have already been reported and this is their consequence rather than a new failure —
  -- and reporting it keeps every definition that did typecheck, where raising loses all of them.
  --
  -- Asked here rather than left to be raised on wherever it is first looked at. Whether it is looked
  -- at depends on what the definition turns out to need, so leaving it to be raised on makes a
  -- program build or die on the shape of its definitions, which is not something to leave to chance.
  objSrc <- case maybeGetSingleton (getExprType $ oaObjExpr oa) of
    Just objType -> return (objType, oa)
    Nothing      -> CErr [MkCNote $ BuildTreeCErr Nothing $ printf "Cannot build %s, whose object is not one type" (show $ oaObjPath oa)]
  toTEObjArr env' [objSrc] False False oa

buildRoot :: TBEnv -> TBExpr -> PartialType -> Type -> CRes (TExpr TBMetaDat)
buildRoot env input src dest = do
  let env' = env{tbName = printf "root"}
  let emptyObj = ObjArr (Just input) FunctionObj Nothing [] Nothing
  let objSrc = (src, emptyObj)
  toTExprDest env' [objSrc] input  (emptyMetaT dest)

