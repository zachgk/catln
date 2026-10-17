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

-- | Where a build's destination came from.
--
-- 'DestInferred' is the type inference established for the source: an upper bound already proven
-- of its value, which is what an expression is built toward ('toTExprDest'). 'DestRequired' is a
-- destination the build must meet by itself, as every followup's is: a followup converts what an arm
-- presents, and the arm presents its result for every input it accepts, not only this call's.
--
-- The two differ where inference reads a call more precisely than an arm presents. Inference follows
-- a concrete call through the arms it reaches (@Pred(Pred(Zero)) + Succ(Succ(Succ(Zero)))@ down to a
-- call a refinement covers, so @Pos@), while an arm is built once and presents what it returns for
-- any input it accepts (@Pred(i@l) + Succ(i@r)= l + r@ returns any @Peano@). Toward an inferred
-- destination the arm is still the right one to call: the value it returns here is already known to
-- be within the destination. See 'buildPartialCallTree'. Like 'CallForce', it applies to the top of
-- one build; a followup's destination is 'DestRequired'.
data DestBasis
  = DestInferred
  | DestRequired
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
    tbTypeEnv' = stampQueryToken tbTypeEnv0
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
  afterArrow <- buildCallTree env objSrc' visitedArrows' bound MayHold DestRequired newSrcType destType
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
               -- An arm refined by a value predicate the source is not within statically is a
               -- guard too: whether it applies is decided on the value ('Eval.valWithinType').
               _ | runtimeRefined optType -> engineAux (opt:accArrows) accOpts True opts
               _ -> engineAux (opt:accArrows) (optType:accOpts) accHasConds opts
        else engineAux accArrows accOpts accHasConds opts

    -- An arm whose object carries a value predicate, where the source is not within the object
    -- by the type layer's decision: a value of the source may fall outside it, so it counts toward
    -- coverage no more than a guarded arm does, and is dispatched by its predicate at runtime.
    -- The source is read without its own value predicates: a call's argument slot is met with
    -- the parameter's refinement by inference (@f(a= Integer|nonNeg)@ for @f(a= n)@ on every
    -- @Integer@), which is what the value is held to, not what is known of it; what is known is
    -- the shape, and a predicate decided on it (@Z@ within @Nat_isZ@) still answers.
    runtimeRefined optType = typeHasValuePredExpr (singletonType optType) && not (isSubtypeOf tbTypeEnv (typeWithoutPredExprs fullType) (singletonType optType))

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

               -- A refined arm the source is not within is guarded by its predicate (see above).
               _ | runtimeRefined optType -> aux (opt:accArrows) accType True opts

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
joinArrowsWithCond env@TBEnv{tbTypeEnv} srcType destType guards = case completeTreeSet env srcType guards of
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
    -- Whether an arm leaves 'Eval' nothing to decide, asked of the BUILT branch. A condition whose
    -- built type is 'trueType' has no other value it could take, so evaluating it is a formality:
    -- that is what a compile-time proposition reduces to, and the @?->@ guarding @/Context@
    -- ('stack/core/data.ct') is one — its 'arrExists' macro answers it during this build. A real
    -- runtime guard is typed 'boolType' and stays a decision. Read from the built conditions rather
    -- than from the 'ObjArr' because a non-linear arm with no where-condition produces the same
    -- EMPTY condition list as an arm with no guard at all, so the two are told apart by asking the
    -- object for its non-linear groups as well.
    -- An arm refined by a value predicate the source is not within statically has the predicate
    -- to decide on the value as well ('runtimeRefined' in 'completeTreeSet').
    decidesNothing :: Maybe ([TExpr EvalMetaDat], EObjArr) -> Bool
    decidesNothing Nothing = True
    decidesNothing (Just (conds, oa)) =
      all ((== trueType) . getExprType) conds && null (exprNonLinearArgGroups (oaObjExpr oa))
        && not (typeHasValuePredExpr (getExprType (oaObjExpr oa)) && not (isSubtypeOf tbTypeEnv (typeWithoutPredExprs (singletonType srcType)) (getExprType (oaObjExpr oa))))
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
        -- A LONE arm still has to pass its own guard. Being the only arrow left says that no OTHER
        -- arm could apply, not that this one does: a where-condition or a non-linear pattern is a
        -- claim about the VALUE, which dispatch on types cannot settle either way. Returning the
        -- tree bare here ran a guarded arm unconditionally, so a function whose every sibling arm
        -- had been dropped — which 'buildPartialCallTree' does silently when an arm's followup
        -- fails — answered from this arm whatever its guard said, rather than reporting that it
        -- had nothing to run. Only a genuinely unguarded arm may skip the 'TCCond'; a guarded one
        -- becomes a single-branch 'TCCond' so 'Eval' evaluates the condition, and says it found no
        -- matching condition when it does not hold.
        [(cond, t)] | decidesNothing cond -> return t
        -- A LONE arm with nothing to decide but its refinement is the refinement check followed by
        -- the arm ('TCRefine'): a value outside it is reported as outside the refinement, where a
        -- 'TCCond' with no other branch could only say that no branch applied.
        [(Just ([], oa), t)] | null (exprNonLinearArgGroups (oaObjExpr oa)) -> return $ TCSeq (TCRefine (getExprType (oaObjExpr oa))) t
        _ -> return $ TCCond destType ifTreePairs

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
    -- A call of one of the object's arguments (@fn(v= x)@ for a higher-order @fn@) returns
    -- what the argument returns: its input object resolved through the environment's arrows
    -- ('hoSlotResult').
    argArrows = map ((srcType, Nothing,) . (`TCArg` nameStr srcName) . hoSlotResult tbTypeEnv H.empty (partialToKey srcType) . abType) $ mapMaybe (H.lookup $ TVArg $ partialToKey srcType) $ exprVarArgsWithObjSrcs tbTypeEnv os

-- | Whether the type graph rewrites this leaf — whether it is a call rather than a value. A data
-- construction and a curried partial application are accepted by no arrow and are already values;
-- see 'reduceAppliedType', which reads the same query the other way round.
typeGraphRewrites :: TBEnv -> PartialType -> Bool
typeGraphRewrites TBEnv{tbTypeEnv} p = not $ null $ typeGraphQueryWithReason tbTypeEnv H.empty p

-- | Builds a call tree to convert something from the partial 'srcType' to 'destType'. The
-- @bound@ is a sound upper bound on the runtime VALUE at this position (⊤ when unknown),
-- carried down the rewrite chain — rewriting preserves the value, so a bound established at an
-- outer call applies to every followup source below it.
buildPartialCallTree :: TBEnv -> [ObjSrc] -> VisitedArrows -> Type -> CallForce -> DestBasis -> PartialType -> Type -> CRes TCallTree
buildPartialCallTree env@TBEnv{tbTypeEnv} os _ _ force _ srcType destType
  | isSubtypeOfWithObjSrcs tbTypeEnv os (singletonType srcType) destType
  , not (force == MustCall && typeGraphRewrites env srcType) = return TCTId
  -- Within the destination by its shape, held back only by value predicates the type layer left
  -- undecided: the value stays as it is and the predicates are evaluated on it ('TCRefine').
  | Just refine <- refinesTo env os (singletonType srcType) destType
  , not (force == MustCall && typeGraphRewrites env srcType) = refine
buildPartialCallTree env@TBEnv{tbTypeEnv} os visitedArrows bound _force basis srcType destType = do
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
    --
    -- A declared result spelled by a bare declaration variable (@(_ :: $T + _ :: $T) -> $T@) is not
    -- read as a bound, for the reason 'TypeCheck.TypeUtils.declaredCallResult' does not read one:
    -- the call binds the variable to what its arguments are (@/T = Succ(..) + Pred(..)@, the two
    -- operands), and the result is not one of them. Read through that binding it claimed a sum is one
    -- of its operands, which is no bound on the value. The refinements covering the call still bound
    -- it, as their results are spelled for the call they cover.
    let srcItem = case teTypeGraph tbTypeEnv of
          ObjArrTypeGraph (ObjectMap tg) -> H.lookupDefault mempty (ptName srcType) tg
        resultIsDeclVariable = or
          [ isTypeVar (getMetaType m)
          | oa@ObjArr{oaArr=Just (_, m)} <- omiDecls srcItem
          , not (isBottomType (intersectTypes tbTypeEnv (singletonType srcType) (getExprType (oaObjExpr oa)))) ]
        declBound
          | resultIsDeclVariable = foldr (intersectTypes tbTypeEnv) PTopType (coveringRefinementResults tbTypeEnv H.empty srcItem srcType)
          | otherwise = typeGraphDeclaredResult tbTypeEnv H.empty srcType
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
    -- Each arm's result is read from the part of the source the arm ACCEPTS — the source met with
    -- the arm's object — as the type graph's own query reads it ('tryTArrow'). Read from the whole
    -- source, an arm's argument is whatever the source holds there: a call reaching here still
    -- generic (@operator+[/T=∀Add](/l=$T, /r=$T)@, from an argument nothing typed) gave the arm
    -- @(l :: P + P/Z)= l@ the result @∀Add@ — Integer among it — though the arm only ever answers
    -- with a @P@, and the arm was then reported as needed by an Integer call it can never serve.
    let acceptedSrcs itemType = case intersectTypes tbTypeEnv (singletonType srcType) (singletonType itemType) of
          UnionType Nothing leafs [] | not (H.null leafs) -> splitUnionType leafs
          _                                               -> [srcType]
        itemDests itemType resArrow = case acceptedSrcs itemType of
          [accepted] -> resArrowDestType tbTypeEnv accepted resArrow
          accepteds  -> [unionAllTypes tbTypeEnv (concatMap (\accepted -> resArrowDestType tbTypeEnv accepted resArrow) accepteds)]
    let resArrows''WithPostTypes = concatMap (\item@(itemType, _, resArrow) -> map ((item,) . boundPost) $ itemDests itemType resArrow) resArrows''
    let strictFollowups = map (arrowFollowup env os visitedArrows bound' srcType destType) resArrows''WithPostTypes
    let strictKept = mapMaybe cresToMaybe strictFollowups
    -- Toward an inferred destination ('DestInferred'), an arm whose result cannot be shown to reach
    -- it is still called when nothing else answers the inputs it accepts: inference has proven the
    -- value within the destination, more precisely than the arm, built once for every input it
    -- accepts, presents its result. Its followup is built toward the destination joined with the
    -- arm's own settled result instead, so the forms it returns are still rewritten, and only
    -- the precision the arm cannot present is taken from inference.
    --
    -- Only where the call would otherwise be stuck: an arm another kept arm answers is a choice
    -- among results the call is free to make (@mempty@ chosen by the result it is needed as), an arm
    -- whose result cannot meet the destination at all is not one this call's values come from,
    -- and an arm of a function the call did not keep is a relative name that chose another
    -- function (when none is kept, only a call whose arms are all of one function is relaxed).
    let strictKeptFunctions = S.fromList [ oaObjPath koa | (_, Just koa, _) <- strictKept ]
        candidateFunctions = S.fromList [ oaObjPath coa | (_, Just coa, _) <- resArrows'' ]
        relaxable (item@(_, Just oa, resArrow), postType) =
          basis == DestInferred
          && postType /= PTopType
          && not (S.member resArrow visitedArrows)
          && not (isBottomType (intersectTypes tbTypeEnv postType destType))
          && not (answeredBy strictKept item)
          && (S.member (oaObjPath oa) strictKeptFunctions || (null strictKept && S.size candidateFunctions == 1))
        relaxable _ = False
        relaxedDest postType = unionTypes tbTypeEnv destType (settledPart postType)
        relax itemWithPost@(_, postType) strict@CErr{}
          | relaxable itemWithPost = case arrowFollowup env os visitedArrows bound' srcType (relaxedDest postType) itemWithPost of
              relaxed@CRes{} -> relaxed
              CErr _         -> strict
        relax _ strict = strict
    let followups = zipWith relax resArrows''WithPostTypes strictFollowups
    let resArrows''' = mapMaybe cresToMaybe followups
    -- An arm whose result cannot reach the destination is left out of the call, which is how a call
    -- chooses among definitions by the result it needs. That is only a choice where the arm's
    -- inputs are still answered as the arm would answer them: by another arm taking the same
    -- inputs, as one @mempty@ is kept beside another of a different result. An arm whose inputs no
    -- kept arm answers — a guarded arm, whose guard only it applies — would otherwise be skipped
    -- for exactly the values it was written for, and another arm of its function would answer them
    -- instead. That is reported rather than done.
    -- Only among the arms of a function the call kept: where none of a function's arms is kept the
    -- function itself was not chosen, which is a relative name choosing among the functions it
    -- matches (a local binding @input@ beside @\/Python\/Builtins\/input@) and not an arm skipped.
    let keptFunctions = S.fromList [ oaObjPath koa | (_, Just koa, _) <- resArrows''' ]
    let wronglyDropped = [ (oa, postType)
                         | ((item@(_, Just oa, resArrow), postType), CErr _) <- zip resArrows''WithPostTypes followups
                         , S.member (oaObjPath oa) keptFunctions
                         , not (S.member resArrow visitedArrows)
                         , not (reachesTo tbTypeEnv H.empty postType destType)
                         -- A result that cannot meet the destination at all is one no value this
                         -- call needs can come from: an abstract @++@ needing a String rules out the
                         -- Integer instance, whose inputs cannot occur here. Only a result that
                         -- COULD be what the call needs, but cannot be shown to be, is an arm
                         -- skipped for values it answers.
                         , not (isBottomType (intersectTypes tbTypeEnv postType destType))
                         , not (answeredByKept item) ]
        answeredByKept = answeredBy resArrows'''
    unless (null wronglyDropped || null resArrows''') $
      CErr [ MkCNote $ BuildTreeCErr (getMetaPos $ getExprMeta $ oaObjExpr oa) $ printf "The definition %s applies to %s but its result %s cannot be what this call needs, %s, so no other definition answers the values it is written for" (show $ oaObjPath oa) (show srcType) (show postType) (show destType)
           | (oa, postType) <- wronglyDropped ]
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
    -- Whether a kept arm answers the inputs this arm accepts: it covers them, and neither arm only
    -- may apply (a guarded arm's guard is its own, and an else arm is a last resort).
    answeredBy kept (itemPartial, Just oa, _) = not (guarded oa) && any (covers itemPartial) kept
    answeredBy _ _ = True
    guarded oa = not (null (exprWhereConds (oaObjExpr oa)))
    covers itemPartial (keptPartial, Just koa, _) = not (guarded koa) && not (hasElseAnnot koa)
      && isSubtypeOf tbTypeEnv (singletonType itemPartial) (singletonType keptPartial)
    covers _ _ = False
    -- What an arm's presented result holds as values: its data leaves, predicate top and constants,
    -- without the call forms the type graph still rewrites.
    settledPart (UnionType mTop leafs consts) = UnionType mTop (joinUnionType (filter (not . null . typeGraphData tbTypeEnv . ptName) (splitUnionType leafs))) consts
    settledPart t = t
    reachHint
      | reachesTo tbTypeEnv H.empty (singletonType srcType) destType = []
      | otherwise = [MkCNote $ BuildTreeCErr Nothing $ printf "The type %s does not reach the destination %s" (show srcType) (show destType)]

-- | The tree for a source within a destination by its shape but not by the destination's value
-- predicates: the value as it is, with the predicates evaluated on it at runtime ('TCRefine'),
-- which is the fallback a refinement has where the type layer decides nothing. A source the type
-- layer decided OUTSIDE the destination (its meet with it is empty: an @S(n= Z)@ held to
-- @Nat_isZ@) is an error here, as every other source nothing converts is; it is never left to the
-- runtime. 'Nothing' when the shape itself is not within, which the arrows may still reach.
refinesTo :: TBEnv -> [ObjSrc] -> Type -> Type -> Maybe (CRes TCallTree)
refinesTo TBEnv{tbTypeEnv} os srcType destType
  | not (typeHasPredExpr destType) = Nothing
  | not (isSubtypeOfWithObjSrcs tbTypeEnv os srcType (typeWithoutPredExprs destType)) = Nothing
  | isBottomType (intersectTypes tbTypeEnv srcType destType) = Just $ CErr [MkCNote $ BuildTreeCErr Nothing $ printf "The value %s is outside the refinement %s" (show srcType) (show destType)]
  | otherwise = Just $ return $ TCRefine destType

-- | Builds a call tree to convert something from 'srcType' to 'destType'. The @bound@ is a
-- sound upper bound on the runtime value here, threaded down from outer calls
-- (see 'buildPartialCallTree'); pass 'PTopType' at roots.
-- | What the object sources a tree is built for bind: the call's arguments and the definition's
-- variables as this call has them. A binding the sources do not settle (several readings, or ⊤)
-- is left out, so it reads as the declaration states it.
--
-- Only the BINDERS among the sources bind: the definition's object and each lambda's pattern, the
-- objects that take arguments. The stack also holds each call argument being built (@cs= tail@,
-- @head= head@), whose object is the argument's key: read as a binder, @head= head@ would bind
-- @head@ to the slot's declared type and shadow the pattern that bound it.
objSrcsEnv :: (TypeGraph tg) => TypeEnv tg -> [ObjSrc] -> TypeVarArgEnv
objSrcsEnv typeEnv os0 = case exprVarArgsWithObjSrcs typeEnv os of
  [vaenv] -> ambientEnv (H.filter (/= PTopType) (fmap abType vaenv))
  _       -> H.empty
  where os = [ src | src@(_, oa) <- os0, not (null (exprAppliedArgs (oaObjExpr oa))) ]

buildCallTree :: TBEnv -> [ObjSrc] -> VisitedArrows -> Type -> CallForce -> DestBasis -> Type -> Type -> CRes TCallTree
buildCallTree env@TBEnv{tbTypeEnv} os visitedArrows bound force basis srcType destType
  | not (H.null osEnv)
  , srcType' /= srcType || destType' /= destType
  = buildCallTreeAt env os visitedArrows bound force basis srcType' destType'
  where
    -- A definition's types keep its own variables as links (@l.equal(other= r)@ in
    -- @(l :: $T == r :: $T)@ is @equal(this=$_T, other=$_T)@): the tree is built for one call,
    -- whose bindings say what they are here (@T ↦ True@ from @l@). Read unresolved, a link is
    -- the ⊤ it denotes, and every arm of @equal@ accepts it.
    osEnv = objSrcsEnv tbTypeEnv os
    srcType' = substituteTVExt tbTypeEnv osEnv srcType
    destType' = substituteTVExt tbTypeEnv osEnv destType
buildCallTree env os visitedArrows bound force basis srcType destType = buildCallTreeAt env os visitedArrows bound force basis srcType destType

buildCallTreeAt :: TBEnv -> [ObjSrc] -> VisitedArrows -> Type -> CallForce -> DestBasis -> Type -> Type -> CRes TCallTree
-- The two whole-source shortcuts below are the 'MayHold' reading of the source: it may stand as
-- it is because the destination accepts it. Under 'MustCall' the question is settled per leaf in
-- 'buildPartialCallTree', which is the only level that can see whether the type graph rewrites it.
buildCallTreeAt TBEnv{tbTypeEnv} os _ _ MayHold _ srcType destType | isSubtypeOfWithObjSrcs tbTypeEnv os srcType destType = return TCTId
buildCallTreeAt env os _ _ MayHold _ srcType destType | Just refine <- refinesTo env os srcType destType = refine
-- buildCallTree _ os _ _ srcType destType | trace (printf "buildCallTree from %s to %s\n\t\twith %s" (show srcType) (show destType) (show $ map fst os)) False = undefined
buildCallTreeAt _ _ _ _ MayHold _ _ PTopType = return TCTId
buildCallTreeAt _ _ _ _ _ _ PTopType _ = error $ printf "buildCallTree from top type"
buildCallTreeAt env@TBEnv{tbTypeEnv} objSrcs visitedArrows bound force basis (TypeVar v _ i) destType = case exprVarArgsWithObjSrcs tbTypeEnv objSrcs of
  [vaenv] -> case H.lookup v vaenv of
    Just ArgBind{abType=srcType'} -> buildCallTree env objSrcs visitedArrows bound force basis (intersectTypes tbTypeEnv srcType' i) destType
    Nothing -> error $ printf "Unknown TypeVar %s in buildCallTree" (show v)
  vaenvs -> error $ printf "Found unhandled zero or multiple vaenvs in builCallTree: %s" (show $ map (fmap abType) vaenvs)
-- A source with no concrete leaves but a predicate top (a rel/class form such as @~Boolean@
-- represented as @UnionType (Just (PredRel Boolean, _)) [] []@) carries its members only in the
-- predicate, so expand it to the member union and build from that. Without this it would yield an
-- empty 'TCMatch' that no runtime value can satisfy.
buildCallTreeAt env@TBEnv{tbTypeEnv} os visitedArrows bound force basis src@(UnionType (Just (preds, _, _)) srcLeafs consts) destType
  | null (splitUnionType srcLeafs) && null consts
  , let expanded = expandPredicates tbTypeEnv H.empty preds
  , expanded /= src =
      buildCallTree env os visitedArrows bound force basis expanded destType
buildCallTreeAt env@TBEnv{tbTypeEnv} os visitedArrows bound force basis (UnionType _ srcLeafs consts) destType = do
  -- Inference spells a source open-world: a leaf may still carry a class its value is not (yet)
  -- a member of (@rel(this=MyEqv|Eqv)@, kept because a later file could declare @MyEqv isa Eqv@).
  -- This program is the whole program ('tePrgmEnv'), where such a leaf is empty — no runtime value
  -- has that shape, so it takes no arm. Building it anyway offers a runtime match option that a
  -- value of the other, real, leaf can be steered into.
  let inhabited p = not (carriesMembership p) || not (isBottomType (compactType tbTypeEnv H.empty (singletonType p)))
  let allPartials = filter inhabited (splitUnionType srcLeafs) ++ map constantPartialType consts
  let tryBuilds = map (\srcPartial -> (srcPartial, buildPartialCallTree env os visitedArrows bound force basis srcPartial destType)) allPartials
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

-- | Whether a leaf, or a leaf in one of its slots at any depth, carries a class\/relation
-- membership atom attached to it (@X|C@). That is the spelling inference keeps for a value whose
-- membership is still open, and the only one that can be empty in the whole program while
-- inhabited in inference's open world — so only such a leaf is worth compacting before a build.
-- (A predicate TOP in a slot, @∀ConsList@, is the class itself, not an open membership claim.)
carriesMembership :: PartialType -> Bool
carriesMembership PartialType{ptPreds, ptVars, ptArgs} =
  predsHasClassRelAtom ptPreds || any typeCarries (H.elems ptVars) || any typeCarries (H.elems ptArgs)
  where
    typeCarries (UnionType _ leafs _) = any carriesMembership (splitUnionType leafs)
    typeCarries (TypeVar _ _ inner) = typeCarries inner

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
-- A value narrowed by a condition: the condition is built toward 'boolType', as an arm's guard
-- is ('buildGuards'), for 'Eval' to decide on the value.
toTExpr env os (EWhere m b a) = do
  b' <- toTExpr env os b
  a' <- toTExprDest env os a (emptyMetaT boolType)
  return $ TWhere m b' a'
toTExpr env os (TupleApply m (bm, be) arg) = do
  be' <- case arg of
    EAppVar _ _ -> toTExprDestBase env os be m
    _           -> toTExprDestBase env os be (baseBuildMeta env os be bm)
  arg' <- case arg of
    EAppArg a     -> EAppArg <$> toTEObjArr env os True a
    EAppVar vn vm -> return $ EAppVar vn vm
    -- A value spread into a call is read for its arguments, so it has to be a value by then: a
    -- reference to an argument resolved and an applied term called. Built toward ⊤, which is what
    -- 'toTExprDest' does for a destination that imposes nothing on the value but that it be one.
    EAppSpread a  -> EAppSpread <$> toTExprDest env os a (emptyMetaT PTopType)
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

-- | The one type a definition's object has, which is the source partial an 'ObjSrc' carries beside
-- the definition itself.
--
-- Whether a build ever looks at this is decided by what the definition turns out to need, so where
-- the 'ObjSrc' is built ('toTEObjArr') this is left a thunk and raises wherever it is FORCED — a
-- place that has nothing to do with the definition at fault. The message therefore has to name the
-- definition and where it was written, because nothing around the raise will.
--
-- The failure is real when it happens: a definition whose object is a NAME AMBIGUITY (a bare
-- relative name matching two declarations) has no single source to dispatch from. 'buildRootOA'
-- reports the same thing as a 'CRes' error, where it can.
objSrcPartial :: EObjArr -> PartialType
objSrcPartial oa = case maybeGetSingleton objType of
  Just p  -> p
  Nothing -> error $ printf "The object of %s is not one type but %s, so there is no source to build it from%s"
                            (show $ oaObjPath oa) (show objType) writtenAt
  where
    objType = getExprType $ oaObjExpr oa
    writtenAt = maybe "" (printf ", written at %s" . show) (getMetaPos $ getExprMeta $ oaObjExpr oa) :: String

-- | A root definition's object: the pattern its arrow is written from.
--
-- A pattern is matched against the value a call is given, and never evaluated, so the applications
-- along its spine are built as they stand rather than toward a destination. Building each base as a
-- term ('toTExprDest') asked whether it already was what the whole object is, and a base only ever
-- answered with the identity when it did. Where it did not, the build went looking for arrows to
-- rewrite the pattern with, and failed. The generic declaration
-- @(_ :: $T ++ _ :: $T)[$T -> Semigroup]@ of @core\/algebra.ct@ is what showed it: once the String
-- @++@ was declared in its component, 'BoundedByObjs' gave the base @++(l)(r)@ both declarations as
-- its requirement. Decode then kept its actual @++(r=⊤, l=⊤)@, which is no subtype of the object,
-- and the declaration failed to build.
--
-- Only the spine is a pattern here. Each argument is still an 'ObjArr' of its own, built by
-- 'toTEObjArr' as before.
toTObjPattern :: TBEnv -> [ObjSrc] -> Expr TBMetaDat -> CRes (TExpr TBMetaDat)
toTObjPattern env os (TupleApply m (bm, be) arg) = do
  be' <- toTObjPattern env os be
  arg' <- case arg of
    EAppArg a     -> EAppArg <$> toTEObjArr env os True a
    EAppVar vn vm -> return $ EAppVar vn vm
    EAppSpread a  -> EAppSpread <$> toTExpr env os a
  return $ TTupleApply m (bm, be') arg'
-- An arm's guard stays as written on the pattern: the condition 'Eval' decides is built from the
-- arm's own expression by 'buildGuards' ('exprWhereConds'), so building it here toward 'boolType'
-- as a body's @where@ is ('toTExpr') would only repeat that work in the pattern.
toTObjPattern env os (EWhere m b a) = do
  b' <- toTObjPattern env os b
  a' <- toTExpr env os a
  return $ TWhere m b' a'
toTObjPattern env os e = toTExpr env os e

-- | The Bool marks an ObjArr applied inside an expression ('EAppArg') as opposed to a root
-- definition ('buildRootOA').
toTEObjArr :: TBEnv -> [ObjSrc] -> Bool -> EObjArr -> CRes (ObjArr TExpr TBMetaDat)
-- toTEObjArr _ os _ oa | trace (printf "toTEObjArr %s\n\twith %s" (show oa) (show os)) False = undefined
toTEObjArr env@TBEnv{tbTypeEnv, tbResEnv} os inExpr oa@ObjArr{oaObj, oaAnnots, oaArr} = do
  oaObj' <- mapM (if inExpr then toTExpr env os else toTObjPattern env os) oaObj
  let os' = case oa of
        ObjArr{oaObj=Just{}, oaArr=Just (Just{}, _)} -> (objSrcPartial oa, oa) : os
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

-- | The environment a call is built in: the program's, plus the arrows the call's own
-- higher-order arguments add. Each lambda given along the call's application chain
-- (@f(a)(fn(v)= body)@) is an Obj/Arrow pair — object its pattern @fn(/v=..)@, result its body —
-- in scope for this call alone, so the callee's arm resolves the input object its @fn@ slot holds
-- to the lambda's result ('hoSlotResult'). The same per-call scope the typechecker gives a call's
-- constraints ('TypeCheck.Encode.attachCallArrs').
withCallArrows :: TBEnv -> Expr TBMetaDat -> TBEnv
withCallArrows env e = withArgArrows env (callLambdas e)

-- | The environment extended with the arrows a set of lambdas add: each one's object its pattern
-- @fn(/v=..)@, its result its body. A call's lambdas at build time ('withCallArrows'), and at run
-- time the closures a call is given ('Eval.evalObjArrCaptured'), whose arrows an arm's body reaches
-- through the input object each one's slot holds ('hoSlotResult').
withArgArrows :: TBEnv -> [ObjArr Expr TBMetaDat] -> TBEnv
withArgArrows env@TBEnv{tbTypeEnv} lams = case lams of
  []   -> env
  _    -> let arrows = [ ObjArr (Just obj) ArgObj Nothing [] (Just (Nothing, arrM)) | ObjArr{oaObj=Just obj, oaArr=Just (_, arrM)} <- lams ]
              objMap = objectMapFromList arrows
              merged0 = mkTypeEnv (Prgm objMap (classGraphFromObjs objMap) []) <> tbTypeEnv
          in env{tbTypeEnv = withLocalArrowsOf (ObjArrTypeGraph objMap) tbTypeEnv merged0}

-- | The lambdas given along a call's application chain (@f(a)(fn(v)= body)@): each argument whose
-- object is applied to parameters and which has a body.
callLambdas :: Expr TBMetaDat -> [ObjArr Expr TBMetaDat]
callLambdas (TupleApply _ (_, be) arg) = case arg of
  EAppArg oa@ObjArr{oaObj=Just obj, oaArr=Just (Just _, _)} | not (null (exprAppliedArgs obj)) -> oa : callLambdas be
  _ -> callLambdas be
callLambdas _ = []

toTExprDest :: TBEnv -> [ObjSrc] -> Expr TBMetaDat -> EvalMeta -> CRes (TExpr TBMetaDat)
toTExprDest = toTExprDestWith True

-- | 'toTExprDest' for the BASE of an application: a call still being assembled, which nothing
-- dispatches on (the call it becomes reads every argument, its own and its base's), so its
-- arguments are read as the base's own type gives them.
toTExprDestBase :: TBEnv -> [ObjSrc] -> Expr TBMetaDat -> EvalMeta -> CRes (TExpr TBMetaDat)
toTExprDestBase = toTExprDestWith False

toTExprDestWith :: Bool -> TBEnv -> [ObjSrc] -> Expr TBMetaDat -> EvalMeta -> CRes (TExpr TBMetaDat)
-- toTExprDest _ os e m | trace (printf "toExprDest %s to %s \n\twith %s" (show e) (show (getMetaType m)) (show $ map fst os)) False = undefined
-- A hole is not converted toward its destination: it produces no value, failing where a run
-- reaches it, so there is nothing to convert — and its type (⊤) has no arrows to build from.
toTExprDestWith _ env0 os e@HoleExpr{} _ = toTExpr env0 os e
toTExprDestWith readArgRefs env0@TBEnv{tbTypeEnv} os e m = do
  e' <- toTExpr env0 os e
  let env = withCallArrows env0 e
  -- The call is built for the arguments its object sources bind, which TreeBuild knows more
  -- precisely than the inferred type it was typechecked with: each slot valued by a reference to
  -- one of the enclosing definition's arguments is that argument here ('applyArgRefBindings', the
  -- same reading an arm's result is given).
  let srcT0 = getMetaType $ getExprMeta e'
  let srcT | readArgRefs = applyArgRefBindings tbTypeEnv e (exprArgRefBindings (objSrcsEnv tbTypeEnv os) e) srcT0
           | otherwise   = srcT0
  ct <- buildCallTree env os S.empty PTopType MayHold DestInferred srcT (getMetaType m)
  -- A reference to an argument of the enclosing definition — bare (@= v@) or applied
  -- (@fn(v= x)@ where @fn@ is a parameter) — is unreduced. When the destination is generic
  -- the dest type is Top, so 'buildCallTree' short-circuits to 'TCTId' — but the argument
  -- must still be resolved (bare) or the closure applied (application) at runtime, or the
  -- raw tuple leaks out as the result value. Force the argument arrow (the tree
  -- 'findResArrows' builds when the destination is typed).
  let argFnTree = case maybeGetSingleton srcT of
        Just sp ->
          case mapMaybe (lookupArgLoose (partialToKey sp)) (exprVarArgsWithObjSrcs tbTypeEnv os) of
            (ArgBind{abType=argT}:_) -> Just $ TCArg (hoSlotResult tbTypeEnv H.empty (partialToKey sp) argT) (nameStr (ptName sp))
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
  --
  -- The destination is read in this frame: a result spelled as a variable the frame has not bound
  -- (@foldl@'s recursive arm, typed @$_B@) imposes no contract either, exactly as the ⊤ it reads as.
  let destInFrame = substituteTVExt tbTypeEnv (objSrcsEnv tbTypeEnv os) (getMetaType m)
  let mustCall = destInFrame == PTopType && not (null (exprAppliedArgs e)) && case srcT of
        UnionType _ srcLeafs _ -> any (typeGraphRewrites env) (splitUnionType srcLeafs)
        _                      -> False
  case ct of
    TCTId | Just at <- argFnTree            -> return $ TCalls m e' at
    TCTId | mustCall                        -> do
              ct' <- buildCallTree env os S.empty PTopType MustCall DestInferred srcT (getMetaType m)
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
      macroResult <- f e (MacroData env0 os)
      case macroResult of
        Left val -> return $ TCExpr (emptyMetaT $ singletonType $ getValPartialType val) val
        Right e' -> toTExprDest env0 os e' m

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
  toTEObjArr env' [objSrc] False oa

buildRoot :: TBEnv -> TBExpr -> PartialType -> Type -> CRes (TExpr TBMetaDat)
buildRoot env input src dest = do
  let env' = env{tbName = printf "root"}
  let emptyObj = ObjArr (Just input) FunctionObj Nothing [] Nothing
  let objSrc = (src, emptyObj)
  toTExprDest env' [objSrc] input  (emptyMetaT dest)

