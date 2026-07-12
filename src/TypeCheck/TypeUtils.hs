--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.TypeUtils
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module handles some type utilities for type checking.
-- It supports the 'ArrowTo' constraint to compute resulting types through the typeGraph
-- This module also computes what the Any
-- types are which result from joining the types of all objects.
--------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module TypeCheck.TypeUtils where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S

import           Control.Monad
import           Control.Monad.State
import           CtConstants         (runtimeAnnot)
import           Data.Bifunctor      (Bifunctor (bimap))
import           Data.List           (find, intercalate, isInfixOf, nub, partition)
import           Debug.Trace         (trace)
import           Data.Maybe          (mapMaybe)
import           Data.UUID           (nil)
import           MapMeta
import           Semantics
import           Semantics.Annots    (hasAnnot)
import           Semantics.Prgm
import           Semantics.TypeGraph
import           Semantics.Types
import           Text.Printf
import           TypeCheck.Common
import           TypeCheck.Show
import           Utils

-- |
-- The object precedence is used to avoid increasing the scope of objects accidentally.
-- For example, if a data type is defined, functions using that data type shouldn't change the valid arguments to it
-- Especially, not specifying bounds should not turn them into TopType.
-- Similarly, matches or patterns are less effective then functions.
-- TODO May need to differentiate top level of functions from inner levels
objectPrecedence :: (ExprClass e, Show m, Show (e m)) => ObjArr e m -> [Int]
objectPrecedence ObjArr{oaBasis=TypeObj}=                             [1]
objectPrecedence ObjArr{oaBasis=FunctionObj, oaArr=Nothing}           = [2, 1] -- Type objects have priority
objectPrecedence oa@ObjArr{oaBasis=FunctionObj, oaArr=Just (Nothing, _)}
  | hasAnnot runtimeAnnot oa                                          = [2, 3] -- Runtime declarations operate at definition precedence
  | otherwise                                                         = [2, 2] -- Declaration objects have priority [2,2], better than definitions
objectPrecedence ObjArr{oaBasis=FunctionObj, oaArr=Just (Just{}, _)}  = [2, 3] -- Definition objects have priority [2,3]
objectPrecedence ObjArr{oaBasis=PatternObj}                           = [3]
objectPrecedence ObjArr{oaBasis=MatchObj}                             =   [4]
objectPrecedence ObjArr{oaBasis=ArgObj}                               =   [5]

-- | Finds the 'objectPrecedence' for all types
buildPrecedenceMap :: (Show m, MetaDat m) => ObjectMap Expr m -> H.HashMap TypeName [Int]
buildPrecedenceMap (ObjectMap objMap) = fmap (minimum . map objectPrecedence . flatObjectMapItem) objMap

-- |
-- Prunes an objectMap by precendence. If two objects share the same precendence, only the bigger one(s) will be kept.
-- This is used to ensure that the type of an object can't be changed by other usages, such as a data object by functions using that data
filterBestPrecedence :: (Show m, MetaDat m) => H.HashMap TypeName [Int] -> ObjectMap Expr m -> ObjectMap Expr m
filterBestPrecedence precedenceMap = filterObjectMap (\oa -> objectPrecedence oa == H.lookupDefault (error $ printf "Could not find obj in union.\n\tObj: %s\n\tKeys: %s" (show oa) (show $ H.keys precedenceMap)) (makeAbsoluteName $ oaObjPath oa) precedenceMap)


-- | This creates 'feUnionAllObjs' and adds it to the 'FEnv'
addUnionObjToEnv :: VObjectMap -> TObjectMap -> StateT FEnv TypeCheckResult ()
addUnionObjToEnv vobjMap tobjMap = do
  FEnv{feTypeEnv} <- get
  let vobjMapRec = mconcat $ map getRecursiveObjs $ flatObjectMap vobjMap
  let tobjMapRec = mconcat $ map getRecursiveObjs $ flatObjectMap tobjMap

  -- Finds the best precedence for all each object name
  let vePrecedenceMap = buildPrecedenceMap vobjMapRec
  let tPrecedenceMap = buildPrecedenceMap tobjMapRec
  let precedenceMap = H.unionWith min vePrecedenceMap tPrecedenceMap

  -- Filter the objects to only those with the best precedence
  let vobjs' = filterBestPrecedence precedenceMap vobjMapRec
  let tobjs' = filterBestPrecedence precedenceMap tobjMapRec

  let vobjMetas = map (getExprMeta . oaObjExpr) $ flatObjectMap vobjs'
  let tobjMetas = map (getMetaType . getExprMeta . oaObjExpr) $ flatObjectMap tobjs'

  -- Builds vars to use for union and union powerset
  unionAllObjs <- fresh $ TypeCheckResult [] $ SType PTopType PTopType Nothing "unionAllObjs"
  unionAllObjsPs <- fresh $ TypeCheckResult [] $ SType PTopType PTopType Nothing "unionAllObjsPs"

  let mkVarMeta p = Meta PTopType Nothing nil (VarMetaDat (Just p) Nothing)

  -- Build a variable to store union of tobjs
  let typecheckedAllType = unionAllTypes feTypeEnv $ filter (not . isTypeVar) tobjMetas
  when (typecheckedAllType == PTopType) $ fail $ printf "Failed to deteremine dependency TopType. Found undetermined dependent objects: \n\t%s" (intercalate "\n\t" $ map (show . snd) $ filter ((==) PTopType . fst) $ zip tobjMetas $ flatObjectMap tobjs')
  typecheckedAllObjs <- fresh $ TypeCheckResult [] $ SType typecheckedAllType PTopType Nothing "typecheckedAll"
  let typecheckedAllObjs' = mkVarMeta typecheckedAllObjs

  -- Builds metas to use for union and union powerset
  let unionAllObjs' = mkVarMeta unionAllObjs
  let unionAllObjsPs' = mkVarMeta unionAllObjsPs

  let constraints = [
        UnionOf 1 unionAllObjs' (typecheckedAllObjs' : vobjMetas),
        SetArgMode 2 True unionAllObjs' unionAllObjsPs'
        ]
  modify (\env -> env{feUnionAllObjs=unionAllObjsPs'})
  modify startConstrainBlock
  modify $ addConstraints constraints
  modify $ endConstraintBlock Nothing H.empty

-- | A helper for the 'AddInferArg' 'Constraint'
addInferArgToType :: FEnv -> TypeVarArgEnv -> Type -> Maybe Type
addInferArgToType _ _ PTopType = Nothing
addInferArgToType _ _ (UnionType (Just _) _ _) = Nothing
addInferArgToType env vaenv (TypeVar t _) = case H.lookup t vaenv of
  Just t' -> addInferArgToType env vaenv t'
  Nothing -> error $ printf "Failed to find %s in addInferArgToType" (show t)
addInferArgToType env@FEnv{feTypeEnv} vaenv (UnionType Nothing partials []) = Just $ unionAllTypes feTypeEnv partials'
  where
    partials' = map (addInferArgToPartial env vaenv) $ splitUnionType partials
addInferArgToType _ _ _ = Nothing

addInferArgToPartial :: FEnv -> TypeVarArgEnv -> PartialType -> Type
addInferArgToPartial FEnv{feVTypeGraph=ObjectMap vtypeGraph, feTTypeGraph=ObjectMap ttypeGraph, feTypeEnv} _ partial@PartialType{ptName=name, ptArgs} = do
  let vtypeArrows = H.lookupDefault mempty name vtypeGraph
  let vTypes = tryOMI vtypeArrows

  let ttypeArrows = H.lookupDefault mempty name ttypeGraph
  let tTypes = tryOMI ttypeArrows

  unionTypes feTypeEnv vTypes tTypes
  where
    tryOMI :: (MetaDat m, Show m) => ObjectMapItem Expr m -> Type
    tryOMI omi = unionAllTypes feTypeEnv $ map tryArrow $ flatObjectMapItem omi

    tryArrow :: (MetaDat m, Show m) => ObjArr Expr m -> Type
    tryArrow oa = if H.keysSet ptArgs `isSubsetOf` H.keysSet (exprAppliedArgsMap $ oaObjExpr oa)
      then UnionType Nothing (joinUnionType $ map addArg $ S.toList $ S.difference (H.keysSet $ exprAppliedArgsMap $ oaObjExpr oa) (H.keysSet ptArgs)) []
      else BottomType
    addArg arg = partial{ptArgs=H.insertWith (unionTypes feTypeEnv) arg PTopType ptArgs}

-- TODO Look into making this run every epoch rather than per call to mkReachesEnv
buildTypeEnv :: FEnv -> TypeCheckResult FEnvTypeEnv
buildTypeEnv env@FEnv{feTypeEnv, feVTypeGraph, feTTypeGraph} = do
  feVTypeGraph' <- mapMObjectMap buildVObjArr feVTypeGraph
  let ttypeGraph' = mapMetaObjectMap clearMetaDat feTTypeGraph
  return feTypeEnv{teTypeGraph=ObjArrTypeGraph (feVTypeGraph' <> ttypeGraph')}

  where
    -- Env (typeGraph) from variables
    buildVObjArr :: ObjArr Expr VarMetaDat -> TypeCheckResult (ObjArr Expr ())
    buildVObjArr voa = do
      soa <- showObjArr env voa
      let soa' = mapMetaObjArr clearMetaDat Nothing soa
      return soa'

-- | The declared input type of a higher-order argument's parameter, resolved for a call site.
-- For @apply(x= 1, fn(v)= v)@ whose matched declaration is @apply(x -> $_A, fn(v -> $_A) -> $_B)@,
-- the lambda's parameter @v@ is typed by the declaration's @$_A@, substituted with the bindings
-- the call's arguments determine (@x=Integer ⇒ $_A=Integer@). Only a fully concrete result is
-- returned: the lambda's input point is narrowed by resolved knowledge only, never by symbolic
-- variables (writing a symbolic external var into a resolving point oscillates against later
-- epochs). Declarations only ('omiDecls') — an unannotated definition carries no input contract.
fnArgInputBound :: FEnv -> Type -> ArgName -> ArgName -> Maybe Type
fnArgInputBound env callAct fnKey inKey = case callAct of
  UnionType Nothing leafs [] -> case mapMaybe partialBound (splitUnionType leafs) of
    [] -> Nothing
    bs -> Just $ unionAllTypes typeEnv bs
  _ -> Nothing
  where
    typeEnv = feReachesTypeEnv env
    TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} = typeEnv
    -- keys drift between phases in pkVars (see 'typeGetArgLoose'); match name and applied args
    looseKeyMatch a b = a == b || (pkName a == pkName b && pkArgs a == pkArgs b)
    findArg k = find (maybe False (\ob -> looseKeyMatch (inExprSingleton ob) k) . oaObj)
    partialBound partial@PartialType{ptName} =
      case mapMaybe (oaBound partial) (maybe [] omiDecls (H.lookup ptName tg)) of
        [] -> Nothing
        bs -> Just $ unionAllTypes typeEnv bs
    oaBound partial oa = do
      let objArgs = exprAppliedArgs (oaObjExpr oa)
      fnOa <- findArg fnKey objArgs
      fnObj <- oaObj fnOa
      inOa <- findArg inKey (exprAppliedArgs fnObj)
      (_, inM) <- oaArr inOa
      -- the bindings the call's arguments give the declaration's variables
      let venvs = [ fst $ intersectTypesWithVarEnv typeEnv H.empty (getMetaType aM) srcV
                  | argOa <- objArgs
                  , Just aObj <- [oaObj argOa]
                  , Just (_, aM) <- [oaArr argOa]
                  , Just srcV <- [typeGetArgLoose (inExprSingleton aObj) partial] ]
      let venv0 = mergeAllVarEnvs typeEnv venvs
      -- An argument typed by a declaration variable (@lst -> $F@) binds only that variable;
      -- the variable's declared bound (@$F -> Functor[$T -> $_A]@, held in the declaration
      -- object's own var slot) is what relates the bound value to further variables.
      -- Intersect each bound against its binding to recover them (@$_A=Integer@ from
      -- @lst= Cons[/T=Integer]@).
      let declVars = maybe H.empty ptVars (maybeGetSingleton (getMetaType (getExprMeta (oaObjExpr oa))))
      -- An argument typed by a declaration variable (@lst -> $F@) binds only that variable;
      -- the variable's declared bound (@$F -> Functor[$T -> $_A]@, held in the declaration
      -- object's own var slot) is what relates the bound value to further variables.
      -- Intersecting the bound against the binding materializes the class parameter on the
      -- result's data leaves (@Cons[/T=Integer]@ via the data def's field tie), while the
      -- bound's class partial records which external var that parameter points at
      -- (@/T=$_A@); aligning the two by parameter name recovers the binding (@$_A=Integer@).
      let boundVenvs = [ H.singleton (TVVar extV) elemT
                       | (TVVar vn, binding) <- H.toList venv0
                       , Just bound <- [H.lookup vn declVars]
                       , bound /= PTopType
                       , UnionType Nothing rLeafs [] <- [intersectTypes typeEnv bound binding]
                       , (cvn, extV) <- boundClassVarPtrs bound
                       , let elemT = unionAllTypes typeEnv (mapMaybe (H.lookup cvn . ptVars) (splitUnionType rLeafs))
                       , not (isBottomType elemT) ]
      let inTy = substituteTVExt (mergeAllVarEnvs typeEnv (venv0 : boundVenvs)) (getMetaType inM)
      guard (typeFullyConcrete inTy)
      return inTy
    -- The (class parameter name, external var name) pointers of a bound's class partials:
    -- @Functor[$T -> $_A]@ ⇒ [(T, A)], whether the bound is a predicate top or a positive
    -- class-named leaf.
    boundClassVarPtrs bound = concatMap fromPartial (case bound of
      UnionType (Just (preds, _)) posLeafs _ -> predPartials preds ++ splitUnionType posLeafs
      UnionType Nothing posLeafs _           -> splitUnionType posLeafs
      _                                      -> [])
      where
        fromPartial p = [ (vn, an) | (vn, TypeVar (TVVar an) TVExt) <- H.toList (ptVars p) ]
        predPartials preds = case tryPredsToList preds of
          Just ps -> [ p | pr <- ps, p <- predPartial pr ]
          Nothing -> []
        predPartial (PredClass c) = [c]
        predPartial (PredRel r)   = [r]
        predPartial PredExpr{}    = []

-- | Binds a call's declaration-output variable from the call site's destination.
-- For a declaration whose output is a bare declared variable — @mk[$M -> Mk](v -> V) -> $M@ —
-- the call's arguments say nothing about @$M@; only the destination does. Once the destination
-- has resolved, stamp it onto the call partial's variable slot (intersecting any existing
-- binding), so downstream arrow selection can discriminate instance definitions that differ
-- only in that variable's binding (return-type-directed dispatch, e.g. @pure@-style
-- constructors whose instances share an input shape). Mirrors 'fnArgInputBound': declarations
-- only, and only a resolved positive destination is written — a Top, symbolic-variable, or
-- class-predicate destination binds nothing (writing symbolic knowledge into a resolving point
-- oscillates against later epochs). Top-valued /fields/ inside the destination's partials are
-- allowed: a completed annotation restores unbound declared fields as Top (see
-- 'completeDataPartialsTop'), and the partial names still discriminate the instances.
outVarBoundSrc :: FEnv -> Type -> Type -> Type
outVarBoundSrc env dest@(UnionType Nothing destLeafs _) src@(UnionType Nothing srcLeafs srcConsts)
  | not (H.null destLeafs) && not (isBottomType dest) && not (typeContainsTypeVar dest) =
      case UnionType Nothing (joinUnionType $ map bindLeaf $ splitUnionType srcLeafs) srcConsts of
        src' | src' == src -> src
        src'               -> src'
  where
    typeEnv = feReachesTypeEnv env
    TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} = typeEnv
    bindLeaf p = foldr bindVar p (outVarNames p)
    bindVar vn p@PartialType{ptVars} = p{ptVars=H.insertWith (flip (intersectTypes typeEnv)) vn dest ptVars}
    outVarNames p@PartialType{ptName} = nub
      [ vn
      | oa <- maybe [] omiDecls (H.lookup ptName tg)
      , Just (_, outM) <- [oaArr oa]
      , TypeVar (TVVar vn) _ <- [getMetaType outM]
      , Just declObj <- [maybeGetSingleton (getMetaType (getExprMeta (oaObjExpr oa)))]
      , H.member vn (ptVars declObj)
      -- Complete applications only: a partial application's ArrowTo destination is the
      -- further-applied call node, not the result the declaration's output variable stands for.
      , all (`H.member` ptArgs p) (H.keys (ptArgs declObj)) ]
outVarBoundSrc _ _ src = src

mkReachesEnv :: FEnv -> RConstraint -> TypeCheckResult (ReachesEnv (ObjArrTypeGraph Expr ()))
mkReachesEnv env (Constraint maybeConOa stypeVaenv _) = do

  let vaenv = fmap (bimap stypeAct stypeAct) stypeVaenv

  -- Env (typeGraph) from args
  -- TODO Remove the call to head below to support nonLinear args
  let argVaenv = H.unions $ map (fmap head . snd . splitVarArgEnv . exprVarArgs . oaObjExpr) maybeConOa
  argVaenv' <- forM argVaenv $ \(inExpr, outM) -> do
    inExpr' <- showExpr env InputMeta inExpr
    outM' <- showM env ArrMeta outM
    return (mapMeta clearMetaDat InputMeta inExpr', outM')
  let argObjMap = objectMapFromList $ concatMap (\(inExpr, outM) -> [ObjArr (Just inExpr) ArgObj Nothing [] (Just (Nothing, emptyMetaT (substituteTVExt (fmap snd vaenv) (getMetaType outM))))]) $ H.elems argVaenv'

  -- final ReachesEnv. Reuse the per-epoch cached reaches type graph ('feReachesTypeEnv');
  -- it is constraint-independent, so rebuilding it per call would be redundant.
  let argTypeEnv = mkTypeEnv $ Prgm argObjMap (classGraphFromObjs argObjMap) []
  let feTypeEnv' = feReachesTypeEnv env
  return $ ReachesEnv (argTypeEnv <> feTypeEnv') (fmap snd vaenv) S.empty

arrowConstrainUbs :: FEnv -> RConstraint -> SType -> SType -> TypeCheckResult (Type, Type, Type, Maybe ReachesTree)
-- Relates the source and destination of an arrow in both directions and returns
-- @(srcAct', srcReq', destAct', reaches)@. Forward: derive the destination actual from what the
-- source reaches (the arrows applicable to it), bounded by the destination's requirement. Reverse:
-- narrow the source requirement to the components that actually reach that destination.
-- When the src is fully unresolved (Top) we cannot yet pick an arrow, so skip
-- deriving the destination from the whole-universe reach. This avoids re-running
-- the (expensive) universe 'reachesPartials' every epoch while the src stays Top;
-- once the src resolves, the concrete-src derivation below supersedes it.
arrowConstrainUbs _ _ SType{stypeAct=PTopType, stypeReq=srcReq} SType{stypeAct=dest} = return (PTopType, srcReq, dest, Nothing)
arrowConstrainUbs _ _ SType{stypeAct=src@(UnionType (Just _) _ _), stypeReq=srcReq} SType{stypeAct=dest} = return (src, srcReq, dest, Nothing)
arrowConstrainUbs env con@Constraint{conVaenv} srcS@SType{stypeAct=TypeVar v _, stypeReq=srcReq} destS = do
  let srcv = maybe PTopType (stypeAct . snd) (H.lookup v conVaenv)
  case (srcv, stypeAct destS) of
    (PTopType, PTopType) -> return (stypeAct srcS, srcReq, stypeAct srcS, Nothing)
    _ -> do
      (_, _, cdest, destRT) <- arrowConstrainUbs env con srcS{stypeAct=srcv} destS
      return (stypeAct srcS, srcReq, cdest, destRT)
arrowConstrainUbs env@FEnv{feTypeEnv} con@Constraint{conVaenv} SType{stypeAct=src@(UnionType Nothing srcPartials consts), stypeReq=srcReq} SType{stypeAct=destAct, stypeReq=destReq} = do
  reachesEnv <- mkReachesEnv env con
  let allPartials = splitUnionType srcPartials ++ map constantPartialType consts
  let reaches0 = reachesPartials reachesEnv allPartials
  let vaenv' = fmap (stypeAct . snd) conVaenv

  -- Where and how the requirement acts depends on the kind of destination:
  --   * A consumption point (an argument slot, a where-clause bool) holds the settled value
  --     its consumer receives, so the whole reached union takes the full actual∧required meet.
  --   * An Arrow result ('TypeCheck.Encode.fromObjectMap' constraint 32) is the definition
  --     body's rewrite frontier, deliberately holding raw applied forms alongside what they
  --     settle to (eval's dispatch keying reads them). There the requirement acts by ARROW
  --     CHOICE, and only when it actually FORCES one: when the plain reach already satisfies
  --     the requirement up to settling, it must do nothing — even a semantically-no-op meet
  --     can rewrite representations (completing a bare class member @IntOrdering@ to
  --     @IntOrdering[/T=Integer]@), which eval's value matching cannot tolerate. When it does
  --     force a choice, the reach tree's choice nodes are cut to the arrows guaranteed to
  --     satisfy it (an Integer requirement picks the Integer-returning arrow out of an
  --     @Integer ∪ Float@ reach), guarded by source coverage ('cutReachesTree').
  let destIsArrowResult = case con of
        Constraint _ _ (ArrowTo 32 _ _) -> True
        _                               -> False
  let destByGraph0 = unionReachesTree feTypeEnv vaenv' reaches0
  -- A Top reach carries no information yet (some arm is still unresolved): there is nothing
  -- to choose among, so the requirement cannot engage — forcing a choice there would derive
  -- a destination from the few arms that happen to have resolved, which is not an upper
  -- bound of the point's values and gets locked in by monotonicity.
  let reqForcesChoice = destIsArrowResult && destReq /= PTopType && destByGraph0 /= PTopType && not (isSubtypeUpToSettlingOf feTypeEnv vaenv' destByGraph0 destReq)
  () <- return $ if "rangeStep" `isInfixOf` show src
    then trace (printf "[RTRACE] src=%s\n  destReq=%s\n  destAct=%s\n  forces=%s\n  destByGraph0=%s" (show src) (show destReq) (show destAct) (show reqForcesChoice) (show destByGraph0)) ()
    else ()
  let destBound = intersectTypesEnv feTypeEnv vaenv' destAct destReq
  let reaches' = if reqForcesChoice then cutReachesTree feTypeEnv vaenv' destBound reaches0 else reaches0
  let destByGraph = if reqForcesChoice then unionReachesTree feTypeEnv vaenv' reaches' else destByGraph0
  -- An arrow dest carrying a symbolic external var (an instance arm constructing
  -- @Some[/T=$_T]@) meets the accumulated dest's bound in the intersection's returned env,
  -- while the type keeps the bare var — discarding the env would WIDEN the stored dest (the
  -- free var reads as Top) and fail 'verifySchemeActLowers'. Resolve the newly recorded
  -- meets back into the result; still-unresolved vars stay symbolic ('substituteKeep', the
  -- 'mergeVarEnvVal' doctrine).
  let (venv', dest')
        | reqForcesChoice =
            -- A choice-forcing requirement splits the reached union: RAW leaves (ones the graph
            -- can still rewrite — the body's unrewritten calls, which eval's dispatch keying
            -- reads) are bounded only by the accumulated actual; the settled remainder is
            -- bounded by the full actual∧required meet, cutting reach over-approximation (e.g.
            -- a generic instance result impossible for this call) down to what is required. The
            -- raws are reattached STRUCTURALLY (a leaf-map union): an algebraic union with the
            -- settled part's predicate top would absorb them.
            let (rawLeafs, settledLeafs) = partition (not . isSettledLeaf feTypeEnv) $ case destByGraph of
                  UnionType _ leafs _ -> splitUnionType leafs
                  _                   -> []
                destByGraphSettled = case destByGraph of
                  UnionType dMTop _ dConsts -> UnionType dMTop (joinUnionType settledLeafs) dConsts
                  t                         -> t
                (venvRaw, destRaw') = intersectTypesWithVarEnv feTypeEnv vaenv' destAct (UnionType Nothing (joinUnionType rawLeafs) [])
                (venvSettled, destSettled') = intersectTypesWithVarEnv feTypeEnv vaenv' destBound destByGraphSettled
                merged = case (destRaw', destSettled') of
                  (UnionType Nothing rawLs [], UnionType sMt sLs sCs) -> UnionType sMt (H.unionWith S.union rawLs sLs) sCs
                  (r, s) | isBottomType r -> s
                         | isBottomType s -> r
                         | otherwise      -> unionTypesWithEnv feTypeEnv vaenv' r s
            in (mergeAllVarEnvs feTypeEnv [venvRaw, venvSettled], merged)
        -- An Arrow result whose reach already satisfies the requirement: the requirement is
        -- inert; only the accumulated actual bounds it (see the policy comment above).
        -- KNOWN GAP: without the meet's implicit ∅-gate an immature reach (an epoch where the
        -- source has not yet narrowed) publishes early and monotonicity locks it in — see the
        -- operator== deep-closure loss.
        | destIsArrowResult = intersectTypesWithVarEnv feTypeEnv vaenv' destAct destByGraph
        -- A consumption point: the committed full-meet behavior.
        | otherwise = intersectTypesWithVarEnv feTypeEnv vaenv' destBound destByGraph
  let newBindings = H.difference venv' vaenv'
  let resolveNew = if H.null newBindings then id else substituteKeep newBindings
  let destFinal = compactType feTypeEnv vaenv' (resolveNew dest')
  -- Return-type direction: a declaration output that is a bare declared variable is determined
  -- by the call site's destination — bind it onto the call partial (see 'outVarBoundSrc').
  let srcBound = outVarBoundSrc env destFinal src

  -- Reverse direction: the arrow imposes a requirement back on the source. Keep only the source
  -- components that actually reach the required destination — a component is viable if its own
  -- singleton is within 'destFinal', or (following intermediate arrows) its reach subtree can be
  -- cut to within 'destFinal'. This is the per-child test 'reachesHasCutSubtypeOf' already uses, so
  -- a source that reaches the destination only through a further arrow still counts as viable.
  let viableKeys = case reaches' of
        ReachesPartialTree children ->
          [ key
          | (key, subTree) <- H.toList children
          , isSubtypeOfWithEnv feTypeEnv vaenv' (singletonType key) destFinal
            || reachesHasCutSubtypeOf feTypeEnv vaenv' subTree destFinal ]
        _ -> []
  let viableComponents = UnionType Nothing (joinUnionType viableKeys) []
  let reqVaenv = fmap (stypeReq . snd) conVaenv
  let narrowedAct = intersectTypesEnv feTypeEnv vaenv'   srcBound viableComponents
  let narrowedReq = intersectTypesEnv feTypeEnv reqVaenv srcReq   viableComponents
  -- Narrow act and req in lockstep so the deferred act⊆req check still holds at Decode, and each
  -- side only shrinks. First guard: never publish an empty viability (a transiently-incomplete
  -- reach would otherwise drive the source to BottomType). Second guard: skip a narrowing that
  -- would drop a TypeVar the source carries (the output-directed dispatch 'outVarBoundSrc'
  -- handles), so 'verifyNoTypeVarLost' cannot fire.
  let (src', srcReq')
        | null viableKeys             = (srcBound, srcReq)
        | typeVarLost src narrowedAct = (srcBound, srcReq)
        | otherwise                   = (narrowedAct, narrowedReq)
  return (src', srcReq', destFinal, Just reaches0) -- TEMP BISECT: store uncut tree
