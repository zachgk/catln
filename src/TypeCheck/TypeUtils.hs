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
import           Data.List           (find, intercalate, nub, partition)
import           Data.Maybe          (fromMaybe, isNothing, mapMaybe)
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
      UnionType (Just (preds, _, _)) posLeafs _ -> predPartials preds ++ splitUnionType posLeafs
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
      -- Output-ONLY variables: with an argument occurrence of the same var (@mmap@'s
      -- @m -> $M@), the output position is just one more membership occurrence — the
      -- variable's witness is the occurrences' JOIN, and intersecting the destination alone
      -- onto it (@/M=Nothing@ from the result) would empty the argument evidence
      -- (@m=Some(..) ∩ Nothing = ∅@). Only a var with no argument occurrence (@pure@'s @$M@)
      -- is determined by the destination.
      , not (any (argRefsVar vn) (H.elems (ptArgs declObj)))
      -- Complete applications only: a partial application's ArrowTo destination is the
      -- further-applied call node, not the result the declaration's output variable stands for.
      , all (`H.member` ptArgs p) (H.keys (ptArgs declObj)) ]
    argRefsVar vn (TypeVar (TVVar vn') _)               = vn' == vn
    argRefsVar vn (TypeVar (TVApplied (TVVar vn') _) _) = vn' == vn
    argRefsVar _ _                                      = False
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
--
-- THE TWO INVARIANTS of type inference (Zach's ruling, 2026-08-01), and how a destination
-- union is read under them:
--
--   1. Every scheme is an UPPER BOUND on the possible values of its point.
--   2. Inference only ever NARROWS a scheme — each write reflects a more precise
--      understanding of the same value ('verifyScheme' enforces this).
--
-- A destination union enumerates the FORMS the term may take: @Nil ∪ fmap(⊤,⊤)@ means the
-- term is either the still-unevaluated call form @fmap(⊤,⊤)@ or one of the evaluated forms
-- listed beside it. The unevaluated member does NOT stand for "whatever fmap may produce" —
-- the evaluated members are the complete enumeration of what evaluation can yield, so
-- @Nil ∪ fmap(⊤,⊤)@ asserts the call evaluates only to @Nil@. A forward write must therefore
-- include EVERY applicable arrow's result (@Nil ∪ Cons(…) ∪ fmap(⊤,⊤)@) or not narrow at
-- all: an incomplete enumeration violates invariant (1), and invariant (2) makes the damage
-- permanent — later epochs intersect against the too-narrow bound, so an omitted
-- alternative can never re-enter, and every consumer downstream reasons soundly from the
-- wrong premise (a dropped call form, a missing dispatch arm, an eval failure).
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
  -- Every operation that consults the type graph's OBJECT META TYPES (the reach union's
  -- 'presentRawResult'/'typeGraphDeclaredResult', the requirement cut's re-query, settled-leaf
  -- classification) must use the per-epoch reaches env — the same graph 'reachesPartials'
  -- queries. The base 'feTypeEnv' was built from the PRE-typecheck program, whose object metas
  -- are still unresolved (⊤): a query against it fires for everything, so a bare call form
  -- would permanently absorb its declared result (e.g. @boolEq ∪ ∀Boolean@), which 'addArgToType'
  -- then cannot apply an argument to. The set-algebra meets below keep 'feTypeEnv' — they read
  -- only the class graph and structural arg keysets, which the two envs share.
  let reachesTypeEnv = rTypeEnv reachesEnv
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
  -- A stored point type may reference only variables its own constraint scope binds: read the
  -- graph presentation's out-of-scope external var spellings (source declarations' own
  -- parameters) as the ⊤ they denote ('dropOutOfScopeTVExt'), so denotationally-equal forms
  -- cannot flip between spellings across epochs.
  let dropFree = dropOutOfScopeTVExt vaenv'
  let destByGraph0 = dropFree $ unionReachesTree reachesTypeEnv vaenv' reaches0
  -- A Top reach carries no information yet (some arm is still unresolved): there is nothing
  -- to choose among, so the requirement cannot engage — forcing a choice there would derive
  -- a destination from the few arms that happen to have resolved, which is not an upper
  -- bound of the point's values and gets locked in by monotonicity.
  let reqForcesChoice = destIsArrowResult && destReq /= PTopType && destByGraph0 /= PTopType && not (isSubtypeUpToSettlingOf reachesTypeEnv vaenv' destByGraph0 destReq)
  -- Resolve each operand's recursive self-refs before it feeds any intersection below
  -- ('resolveTypeSelfRefs'): a concrete @Cons[/T=Ord]@ leaf carrying an unresolved recursive
  -- tail top @ConsList[/T=$_T]@ reads that tail as Top, so meeting it re-derives the outer
  -- element from the tail and collapses @/T@ to Top — WIDENING the stored Arrow result across
  -- epochs ('verifySchemeActLowers'). A leaf with no @/T@ param (raw applied forms the result
  -- keeps for eval dispatch) is untouched. The reqForcesChoice GATE above still uses the raw
  -- @destByGraph0@, so this cannot change which arrows a requirement forces.
  let resolveSelf = if destIsArrowResult then resolveTypeSelfRefs H.empty else id
  let destActR = resolveSelf destAct
  let destBound = intersectTypesEnv feTypeEnv vaenv' destActR destReq
  let reaches' = if reqForcesChoice then cutReachesTree reachesTypeEnv vaenv' destBound reaches0 else reaches0
  let destByGraph = resolveSelf $ if reqForcesChoice then unionReachesTree reachesTypeEnv vaenv' reaches' else destByGraph0
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
            let (rawLeafs, settledLeafs) = partition (not . isSettledLeaf reachesTypeEnv) $ case destByGraph of
                  UnionType _ leafs _ -> splitUnionType leafs
                  _                   -> []
                destByGraphSettled = case destByGraph of
                  UnionType dMTop _ dConsts -> UnionType dMTop (joinUnionType settledLeafs) dConsts
                  t                         -> t
                (venvRaw, destRaw') = intersectTypesWithVarEnv feTypeEnv vaenv' destActR (UnionType Nothing (joinUnionType rawLeafs) [])
                (venvSettled, destSettled') = intersectTypesWithVarEnv feTypeEnv vaenv' destBound destByGraphSettled
                merged = case (destRaw', destSettled') of
                  (UnionType Nothing rawLs [], UnionType sMt sLs sCs) -> UnionType sMt (H.unionWith S.union rawLs sLs) sCs
                  (r, s) | isBottomType r -> s
                         | isBottomType s -> r
                         | otherwise      -> unionTypesWithEnv feTypeEnv vaenv' r s
            in (mergeAllVarEnvs feTypeEnv [venvRaw, venvSettled], merged)
        -- An Arrow result whose reach already satisfies the requirement: the requirement is
        -- inert; only the accumulated actual bounds it (see the policy comment above). Both
        -- operands are already self-ref-resolved ('destActR'/'destByGraph').
        | destIsArrowResult = intersectTypesWithVarEnv feTypeEnv vaenv' destActR destByGraph
        -- A consumption point: the committed full-meet behavior.
        | otherwise = intersectTypesWithVarEnv feTypeEnv vaenv' destBound destByGraph
  -- Settle-forcing requirement: a leaf the requirement does NOT admit as it stands
  -- ('reqAdmitsLeaf' — the requirement is the boundary answering "must this settle now, or
  -- is unsettled valid here"), with a viable compilation path to forms within the bound
  -- ('reachesTo'), is REQUIRED to follow its arrows — the destination holds only what the
  -- consumer receives, so the unadmitted form is dropped and the actual keeps the settled
  -- remainder. The requirement never constrains such a leaf by plain membership (the
  -- 'isSubtypeUpToSettlingOf' doctrine): the algebra answers class membership of an
  -- unrewritten call SAT-conservatively ("could settle to a member") and would keep it. A
  -- leaf with no viable path (or no engaged requirement) is kept: mid-solve its reach may
  -- still be resolving, and dropping it would not be an upper bound of the point's values.
  -- The drop is also skipped when it would empty the stored type (settled alternatives not
  -- yet accumulated), which would poison the point to 'BottomType'.
  -- Arrow-applicability first: settling is rewriting, so a leaf whose NAME no arrow can
  -- accept never settles elsewhere — and 'reachesTo' alone cannot gate this, because its
  -- required-node check counts a leaf already inside the bound as reaching by STAYING PUT
  -- (a deep subtype per data literal per epoch). The probe is the bare name with open args
  -- (no deep-argument intersects — a full-leaf query pays 'accepts' meets on every deep
  -- operand); a name-level over-approximation only admits more leaves to the later gates.
  -- TODO(admission): this gate should become the requirement-admission form used by
  -- 'settlePropSlot' (drop iff an arrow applies, the requirement does not admit the form
  -- as-is per 'reqAdmitsLeaf', and the reach is viable) — one criterion, no settled-name
  -- class. The direct swap made a requirement's met call component LICENSE the dest's raw
  -- leaf that this gate previously dropped, and the keep/re-drop disagreement with the
  -- other writers oscillated past the 'runConstraints' limit (toString/++ family) — the
  -- other writers must learn the admission rule in the same change.
  let settleForced l = not (isSettledLeaf reachesTypeEnv l)
                     && reachesTo reachesTypeEnv vaenv' (singletonType l) destBound
  let dropSettleForcedRaws t@(UnionType dMTop dLeafs dConsts)
        | destReq /= PTopType && not (isBottomType destBound) =
            case partition settleForced (splitUnionType dLeafs) of
              ([], _) -> t
              (_, keptLs) | null keptLs && isNothing dMTop && null dConsts -> t
                          | otherwise -> UnionType dMTop (joinUnionType keptLs) dConsts
      dropSettleForcedRaws t = t
  let newBindings = H.difference venv' vaenv'
  let resolveNew = if H.null newBindings then id else substituteKeep newBindings
  let destFinal = compactType feTypeEnv vaenv' (dropFree (resolveSelf (resolveNew (dropSettleForcedRaws dest'))))
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
  -- The ACTUAL keeps the BARE viable keys: a stored actual shows what the call site itself
  -- passed, and the bindings a meet with a matched object's contract discovers are per-use
  -- derivations (TreeBuild re-derives them ephemerally against the pointer-linked objects).
  -- The REQUIREMENT instead folds each viable component to its MET form (the union of
  -- @potentialSrc@ = src ∩ arrow-object over the arrows it triggers): the meet with the
  -- object's contract is what imposes the declaration's variable structure as a requirement
  -- (e.g. @==(l=Integer, r=Integer)@ meeting @(l::$T == r::$T)[$T -> Eq]@ discovers
  -- @/T=Integer@). A component no arrow accepts — possible mid-epoch — keeps its bare form
  -- rather than emptying.
  let viableBare = unionAllTypesWithEnv feTypeEnv vaenv' (map singletonType viableKeys)
  -- The met union ranges over ALL accepting arrows ('typeGraphQueryAllWithReason'): the
  -- most-specific-promise pruning is a destination-side reaches optimization, and pruning
  -- against a sibling arm whose object presentation is still transiently ⊤ mid-solve would
  -- collapse the requirement to the one settled arm (monotonically stuck thereafter).
  let viableMet key = case [ metSrc | (_, metSrc, _) <- typeGraphQueryAllWithReason reachesTypeEnv vaenv' key ] of
        []   -> singletonType key
        mets -> unionAllTypesWithEnv feTypeEnv vaenv' mets
  let viableComponents = unionAllTypesWithEnv feTypeEnv vaenv' (map viableMet viableKeys)
  let reqVaenv = fmap (stypeReq . snd) conVaenv
  let narrowedAct = intersectTypesEnv feTypeEnv vaenv'   srcBound viableBare
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
  -- The stored tree is always the UNCUT reach: a requirement-forced cut is a per-epoch choice
  -- (valid only against that epoch's requirement), while the stored tree is read downstream as
  -- the record of where the source can rewrite to.
  return (src', srcReq', destFinal, Just reaches0)

-- | Whether a requirement admits a leaf AS IT STANDS — leaving it unsettled. The requirement
-- is the boundary answering "must this settle now, or is unsettled valid here": an unsettled
-- call form appearing IN the requirement licenses the matching unsettled form (leaving
-- operations unsettled is what multi-level definitions and metaprogramming build on), while a
-- class component is read by its DEFINITION — @∀ConsList@ is defined in terms of the settled
-- @Cons@\/@Nil@ objects, so it admits those settled members and never an unrewritten call.
-- Concretely the leaf is checked against the requirement's expansion restricted to the leaf's
-- own name ('expandTypeWithNames'): a class component expands to its declared members (none
-- of the leaf's name — not admitted), while a call-form component survives as itself. A
-- candidate still carrying class predicates admits the leaf only when the predicates provably
-- HOLD of it, which the engine answers conservatively-to-SAT (escape possible ⇒ not proven) —
-- exactly the polarity admission needs. Deliberately NOT plain 'isSubtypeOfWithEnv' against
-- the whole requirement: its class coverage reasons about what a term could settle TO, the
-- direction 'reachesTo' owns.
reqAdmitsLeaf :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> PartialType -> Bool
reqAdmitsLeaf _ _ PTopType _ = True
-- A var requirement resolves through the env; a self-referential entry (which an
-- unconstrained var may acquire during intersection) carries no information and licenses
-- like ⊤ — following it verbatim would recurse forever ('isSubtypeOfWithEnv' parity).
reqAdmitsLeaf typeEnv vaenv (TypeVar v _) l = case vaenvLookup vaenv v of
  TypeVar v' _ | v' == v -> True
  looked                 -> reqAdmitsLeaf typeEnv vaenv looked l
reqAdmitsLeaf typeEnv vaenv req l =
  isSubtypeOfWithEnv typeEnv vaenv (singletonType l)
    (UnionType Nothing (expandTypeWithNames typeEnv vaenv req (S.singleton (ptName l))) [])

-- | Settle a construction slot at a 'PropEq' write — the slot-write mirror of
-- 'arrowConstrainUbs''s settle-forcing. A 'PropEq' meet can never remove an unsettled call
-- form once written into a slot (the algebra answers its class membership
-- SAT-conservatively), so a definition body's early slot write (@tail = declared-top ∪
-- rawRecursiveCall@) keeps that spelling forever even after the slot's requirement engages,
-- and the dispatch presentation reads the stale form. Whether the form may stay unsettled is
-- answered by the requirement boundary ('reqAdmitsLeaf'); here the requirement is the slot's
-- own requirement MET WITH the field's declared bound (@Cons@'s @tail -> ConsList[$T -> $_T]@
-- instantiated through the enclosing partial — the @ConsList@ definition is in terms of the
-- settled @Cons@ object, so the declaration dictates a settled field). The bound is
-- instantiated AT THIS SITE from 'typeGraphData' rather than pre-materialized into every
-- stored requirement: an unconditional declared-bound meet on every requirement read re-runs
-- a deep meet per epoch (its respelling never commits under 'eqScheme') and was measured
-- intractable. Admission into the meet distributes (@l ⊆ A∩B ⟺ l ⊆ A ∧ l ⊆ B@), so the
-- common path needs no deep meet at all. An unadmitted leaf settles only with a viable
-- compilation path ('reachesTo') into the slot's actual∧required∧declared bound — the actual
-- in the bound demands the leaf's settled alternatives are ALREADY accumulated in the slot
-- (the alternatives-accumulated guard: @fmap@'s @Nil ∪ fmap(⊤,⊤)@ keeps its call form until
-- the sub point delivers the settled top), and the drop never empties the stored slot. When
-- the settle fires, the slot's stored requirement is tightened to its declared-bound meet
-- (the settled spelling consumers read). Only 'TVArg' slots participate; pointer slots
-- (partial-internal vars) are left to the var machinery.
settlePropSlot :: FEnv -> TypeVarArgEnv -> TypeVarAux -> Type -> Type -> (Type, Type)
settlePropSlot env@FEnv{feTypeEnv} vaenv propName@(TVArg k) superAct@(UnionType supMTop supLeafs supConsts) superReq@(UnionType reqMTop reqLeafs reqConsts) =
  (superAct', superReq')
  where
    reachesTypeEnv = feReachesTypeEnv env
    reqByName = splitUnionTypeByName reqLeafs

    settlePartial :: PartialType -> (PartialType, Maybe PartialType, Bool)
    settlePartial p = fromMaybe (p, Nothing, False) $ do
      -- Fireable shape only: the stale family spelling is @declared-top ∪ rawCall@ — a slot
      -- with no predicate-top component cannot fire (a call form's dests are never inside
      -- its sibling leaves alone, and the bound includes the slot actual), so gate before
      -- any graph or engine work.
      UnionType aMTop@(Just _) aLeafs aConsts <- H.lookup k (ptArgs p)
      let aLeafList = splitUnionType aLeafs
      guard (not (null aLeafList))
      [rp] <- H.lookup (ptName p) reqByName
      slotReq <- typeGetAux propName rp
      -- Engaged requirement only; a pointer-managed (var) slot is left to the var machinery.
      guard (slotReq /= PTopType)
      case slotReq of
        TypeVar{} -> Nothing
        _         -> return ()
      let declBounds = [freeTVIntToTopDeep vaenv p d | PartialType{ptArgs=dArgs} <- typeGraphData feTypeEnv (ptName p), Just d <- [H.lookup k dArgs]]
      -- DECLARATION-BOUNDED construction slots only (a data-graph declaration exists for
      -- this field): the declared bound is what dictates settling (@Cons@'s tail is a
      -- settled @ConsList@ by definition). An ordinary call-argument slot has no such
      -- declaration and keeps its unsettled forms — they may be exactly what the
      -- presentation and dispatch keying read. ANNOTATION argument slots (the parser's @#@
      -- marker) are also excluded for now: their declarations reach 'typeGraphData' like
      -- data, but settling e.g. @#print@'s argument drops many-leaf unions whose commit-time
      -- verification ('verifyScheme' subtype + compact on the changed spelling) is measured
      -- intractable — deferred until the print-presentation family needs it.
      guard (not (null declBounds))
      guard ('#' `notElem` nameStr (ptName p))
      let declBoundU = unionAllTypesWithEnv feTypeEnv vaenv declBounds
      let admitted l = reqAdmitsLeaf reachesTypeEnv vaenv slotReq l
                     && reqAdmitsLeaf reachesTypeEnv vaenv declBoundU l
      -- The settle bound (lazy — forced only when some leaf is not admitted).
      let slotAct = UnionType aMTop aLeafs aConsts
      let bound = intersectTypesEnv feTypeEnv vaenv (intersectTypesEnv feTypeEnv vaenv slotAct slotReq) declBoundU
      -- Arrow-applicability first: settling is rewriting, so a leaf whose NAME no arrow can
      -- accept never settles elsewhere — and 'reachesTo' alone cannot gate this, because
      -- its required-node check counts a leaf already inside the bound as reaching by
      -- STAYING PUT (a deep subtype against the forced bound per data literal per epoch).
      -- The probe is the bare name with open args (no deep-argument intersects).
      let settleForced l = not (null (typeGraphQueryAllWithReason reachesTypeEnv vaenv ((partialVal (ptName l)){ptArgMode=PtArgAny})))
                         && not (admitted l)
                         && not (isBottomType bound)
                         && reachesTo reachesTypeEnv vaenv (singletonType l) bound
      case partition settleForced aLeafList of
        ([], _) -> Nothing
        (_, keptLs)
          | null keptLs && isNothing aMTop && null aConsts -> Nothing
          | otherwise -> do
              let slotAct' = UnionType aMTop (joinUnionType keptLs) aConsts
              let p' = p{ptArgs=H.insert k slotAct' (ptArgs p)}
              let rp' = case H.lookup k (ptArgs rp) of
                    Just UnionType{} ->
                      Just rp{ptArgs=H.insert k (compactType feTypeEnv vaenv (intersectTypesEnv feTypeEnv vaenv slotReq declBoundU)) (ptArgs rp)}
                    _ -> Nothing
              return (p', rp', True)

    results = map settlePartial (splitUnionType supLeafs)
    changed = or [c | (_, _, c) <- results]
    superAct' | changed = compactType feTypeEnv vaenv (UnionType supMTop (joinUnionType [p | (p, _, _) <- results]) supConsts)
              | otherwise = superAct
    reqUpdates = H.fromList [(ptName rp, rp) | (_, Just rp, _) <- results]
    superReq' | H.null reqUpdates = superReq
              | otherwise = UnionType reqMTop (joinUnionType (map (\q -> fromMaybe q (H.lookup (ptName q) reqUpdates)) (splitUnionType reqLeafs))) reqConsts
settlePropSlot _ _ _ superAct superReq = (superAct, superReq)
