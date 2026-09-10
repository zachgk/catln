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
{-# LANGUAGE ViewPatterns     #-}

module TypeCheck.TypeUtils where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S

import           Control.Monad
import           Control.Monad.State
import           Data.Bifunctor      (Bifunctor (bimap))
import           Data.List           (find, intercalate, nub, partition)
import           Data.Maybe          (fromMaybe, isJust, isNothing, mapMaybe)
import           Data.UUID           (nil)
import           MapMeta
import           Semantics
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
  -- 'isRuntimeDecl', not an exact-name annotation test: the annotation path is still RELATIVE
  -- here, so an exact compare against @/Catln/#runtime@ answers False for every runtime
  -- declaration and hands it declaration precedence. It then survives 'filterBestPrecedence'
  -- into 'feUnionAllObjs' beside the real declaration, where 'BoundedByObjs' bounds it by
  -- ITSELF; the self-meet carries no type parameters and absorbs the declaration's
  -- instantiation, so the object decodes without them (@/operator++(/r=/Str, /l=/Str)@ rather
  -- than @/operator++[/T=/Str](…)@) and the declaration-coverage check then rejects it.
  | isRuntimeDecl oa                                                  = [2, 3] -- Runtime declarations operate at definition precedence
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

-- | Recursively enumerates an object map with its precedence map, in the form
-- 'addUnionObjToEnv' consumes for the typechecked dependency side. Associative inputs for an
-- accumulating solve loop: merge maps with '<>' and precedences with @H.unionWith min@.
recursiveObjsWithPrecedence :: (Show m, MetaDat m) => ObjectMap Expr m -> (ObjectMap Expr m, H.HashMap TypeName [Int])
recursiveObjsWithPrecedence objMap = (objMapRec, buildPrecedenceMap objMapRec)
  where objMapRec = mconcat $ map getRecursiveObjs $ flatObjectMap objMap


-- | This creates 'feUnionAllObjs' and adds it to the 'FEnv'. The typechecked dependency
-- objects arrive ALREADY recursively enumerated with their precedence map
-- ('recursiveObjsWithPrecedence') so a per-fragment solve loop can accumulate them
-- incrementally instead of re-walking the whole dependency set every fragment.
addUnionObjToEnv :: VObjectMap -> TObjectMap -> H.HashMap TypeName [Int] -> StateT FEnv TypeCheckResult ()
addUnionObjToEnv vobjMap tobjMapRec tPrecedenceMap = do
  FEnv{feTypeEnv} <- get
  let vobjMapRec = mconcat $ map getRecursiveObjs $ flatObjectMap vobjMap

  -- Finds the best precedence for all each object name
  let vePrecedenceMap = buildPrecedenceMap vobjMapRec
  let precedenceMap = H.unionWith min vePrecedenceMap tPrecedenceMap

  -- Filter the objects to only those with the best precedence
  let vobjs' = filterBestPrecedence precedenceMap vobjMapRec
  let tobjs' = filterBestPrecedence precedenceMap tobjMapRec

  let vobjMetas = map (getExprMeta . oaObjExpr) $ flatObjectMap vobjs'
  let tobjMetas = map (getMetaType . getExprMeta . oaObjExpr) $ flatObjectMap tobjs'

  -- Builds vars to use for union and union powerset
  unionAllObjs <- fresh $ TypeCheckResult [] $ SType PTopType PTopType PTopType Nothing "unionAllObjs"
  unionAllObjsPs <- fresh $ TypeCheckResult [] $ SType PTopType PTopType PTopType Nothing "unionAllObjsPs"

  let mkVarMeta p = Meta PTopType Nothing nil (VarMetaDat (Just p) Nothing)

  -- Build a variable to store union of tobjs. Single-compaction union: the pairwise fold
  -- rebuilds the accumulated union per element (quadratic allocation over hundreds of
  -- dependency objects, measured at a third of a fragmented solve).
  let typecheckedAllType = unionAllTypesWithEnvOnce feTypeEnv H.empty $ filter (not . isTypeVar) tobjMetas
  when (typecheckedAllType == PTopType) $ fail $ printf "Failed to deteremine dependency TopType. Found undetermined dependent objects: \n\t%s" (intercalate "\n\t" $ map (show . snd) $ filter ((==) PTopType . fst) $ zip tobjMetas $ flatObjectMap tobjs')
  typecheckedAllObjs <- fresh $ TypeCheckResult [] $ SType typecheckedAllType PTopType PTopType Nothing "typecheckedAll"
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
addInferArgToType :: FEnv -> TypeVarArgEnv -> Maybe ArgName -> Type -> Maybe Type
addInferArgToType _ _ _ PTopType = Nothing
addInferArgToType _ _ _ (UnionType (Just _) _ _) = Nothing
addInferArgToType env vaenv patKey (TypeVar t _ _) = case H.lookup t vaenv of
  Just t' -> addInferArgToType env vaenv patKey t'
  Nothing -> error $ printf "Failed to find %s in addInferArgToType" (show t)
addInferArgToType env@FEnv{feTypeEnv} vaenv patKey (UnionType Nothing partials []) = Just $ unionAllTypes feTypeEnv partials'
  where
    partials' = map (addInferArgToPartial env vaenv patKey) $ splitUnionType partials
addInferArgToType _ _ _ _ = Nothing

-- | Builds the candidate shapes for an 'AddInferArg' 'Constraint': for each arrow of the
-- base's name, each arg the base does not yet apply is offered as the filled slot, and the
-- destination keeps the union of candidates until inference selects one
-- ('TypeCheck.Decode.toExpr''s @learnedArgName@ then requires a single survivor).
-- Arrows are precedence-filtered like the object universe ('addUnionObjToEnv'): where a
-- declaration exists (@[2,2]@) definition arrows (@[2,3]@) offer no candidates — in
-- particular a bare destructuring definition (@unwrap(Box(v))= v@) is never a candidate
-- for its own filled arg, which it cannot name by pointing at itself.
-- Each candidate slot carries the arrow's spelled arg type — a phase-stable 'Meta'
-- spelling, not a solving point's scheme — so a declaration contributes its declared
-- content (@/b=/Box[/T=⊤](/v=⊤)@) rather than a @⊤@ placeholder (which nothing narrows on
-- the actual side under the req-only architecture), while an unannotated definition arg
-- still contributes @⊤@. Spellings referencing the arrow's own scope (external vars, a
-- bare own-var annotation) read as the @⊤@ they denote here ('dropOutOfScopeTVExt').
addInferArgToPartial :: FEnv -> TypeVarArgEnv -> Maybe ArgName -> PartialType -> Type
addInferArgToPartial FEnv{feVTypeGraph=ObjectMap vtypeGraph, feTTypeGraph=ObjectMap ttypeGraph, feTypeEnv} _ patKey partial@PartialType{ptName=name, ptArgs} = do
  let vtypeArrows = flatObjectMapItem $ H.lookupDefault mempty name vtypeGraph
  let ttypeArrows = flatObjectMapItem $ H.lookupDefault mempty name ttypeGraph
  case map objectPrecedence vtypeArrows ++ map objectPrecedence ttypeArrows of
    [] -> BottomType
    precs -> do
      let bestPrec = minimum precs
      let vTypes = tryArrows $ filter ((== bestPrec) . objectPrecedence) vtypeArrows
      let tTypes = tryArrows $ filter ((== bestPrec) . objectPrecedence) ttypeArrows
      unionTypes feTypeEnv vTypes tTypes
  where
    tryArrows :: (MetaDat m, Show m) => [ObjArr Expr m] -> Type
    tryArrows = unionAllTypes feTypeEnv . map tryArrow

    tryArrow :: (MetaDat m, Show m) => ObjArr Expr m -> Type
    tryArrow oa = if H.keysSet ptArgs `isSubsetOf` H.keysSet arrowArgs
      then UnionType Nothing (joinUnionType $ map addArg candidates) []
      else BottomType
      where
        arrowArgs = exprAppliedArgsMap $ oaObjExpr oa
        unfilled = H.toList $ H.difference arrowArgs ptArgs
        -- When the applied argument was written in a shape that NAMES one of the arrow's
        -- arguments — a higher-order parameter @fn(v)@ against a declared @fn(v -> $_A) -> $_B@ —
        -- there is nothing to infer: that is the slot. Offer only it.
        --
        -- Guessing anyway is not harmless. A slot the declaration spells with a bare type
        -- variable (@init -> $_B@) reads as ⊤ through 'spelledContent' below, so the guess
        -- "@fn(v)@ fills @init@" accepts anything and no later evidence can refute it. The
        -- candidate union then never collapses to one survivor, and the surplus alternative —
        -- which has dropped the function argument entirely — reaches 'Decode.verifyObjMapItem' as
        -- a definition outside its declaration. This is why a fold's signature (a function
        -- argument beside a plain one typed by the same variable) could not be written.
        --
        -- A genuine destructuring pattern (@unwrap(Box(v))= v@) names no argument — @/Box@ is the
        -- constructor, not the slot — so it matches nothing here and still infers by type.
        candidates = case patKey of
          Just k | matching@(_:_) <- filter (namesArg k) unfilled -> matching
          _                                                       -> unfilled
        namesArg k (arg, _) = pkName k == pkName arg

    addArg :: (MetaDat m, Show m) => (ArgName, Maybe (Meta m, Maybe (Expr m))) -> PartialType
    addArg (arg, argVal) = partial{ptArgs=H.insert arg (spelledContent argVal) ptArgs}

    -- The arrow's spelled type for the slot, with references out of the arrow's own scope
    -- read as the ⊤ they denote here (a bare top-level var names the arrow's scope too).
    spelledContent :: (MetaDat m, Show m) => Maybe (Meta m, Maybe (Expr m)) -> Type
    spelledContent (Just (argM, _)) = case dropOutOfScopeTVExt H.empty (getMetaType argM) of
      TypeVar{} -> PTopType
      t         -> t
    spelledContent Nothing = PTopType

-- | Also returns the live (V-side) part alone: the typechecked side only changes between
-- solve fragments, so the caller's unchanged-env check can compare just the small live part.
buildTypeEnv :: FEnv -> TypeCheckResult (ObjectMap Expr (), FEnvTypeEnv)
buildTypeEnv env@FEnv{feTypeEnv, feVTypeGraph, feTTypeGraphCleared} = do
  feVTypeGraph' <- mapMObjectMap buildVObjArr feVTypeGraph
  return (feVTypeGraph', feTypeEnv{teTypeGraph=ObjArrTypeGraph (feVTypeGraph' <> feTTypeGraphCleared)})

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
-- the call's arguments determine (@x=Integer ⇒ $_A=Integer@).
--
-- Three things bind a declaration variable here, in narrowing order: an argument whose whole type
-- IS the variable; a class PARAMETER that points at it (@lst -> List[$T -> $_A]@, recovered by
-- 'alignClassPtrs', including one level in through a variable's own declared bound); and failing
-- both, the variable's declared bound itself, which holds whatever the call passes.
--
-- The result must be RESOLVED but need not be fully concrete: a class-predicate top like @∀Eq@ is
-- a sound upper bound at every epoch, and refusing it is what left a lambda inside a generic
-- definition at Top. What stays out is a symbolic variable — writing one into a resolving point
-- oscillates against later epochs — and Top itself, which says nothing.
-- Declarations only ('omiDecls') — an unannotated definition carries no input contract.
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
      let argActuals = [ (getMetaType aM, srcV)
                       | argOa <- objArgs
                       , Just aObj <- [oaObj argOa]
                       , Just (_, aM) <- [oaArr argOa]
                       , Just srcV <- [typeGetArgLoose (inExprSingleton aObj) partial] ]
      let venv0 = mergeAllVarEnvs typeEnv [ fst $ intersectTypesWithVarEnv typeEnv H.empty aT srcV
                                          | (aT, srcV) <- argActuals ]
      -- An argument that SPELLS a class partial (@lst -> List[$T -> $_A]@) binds nothing on its
      -- own: the parameter points at the variable rather than being one, so 'alignClassPtrs'
      -- recovers it from the meet. An argument typed by a bare declaration variable
      -- (@lst -> $F@) binds only that variable, and the variable's declared bound
      -- (@$F -> Functor[$T -> $_A]@, held in the declaration object's own var slot) is what
      -- carries the pointers on to further variables — so run the same recovery a level in.
      let declVars = maybe H.empty ptVars (maybeGetSingleton (getMetaType (getExprMeta (oaObjExpr oa))))
      let argVenvs = [ v | (aT, srcV) <- argActuals, v <- alignClassPtrs aT srcV ]
      let boundVenvs = [ v
                       | (TVVar vn, binding) <- H.toList venv0
                       , Just bound <- [H.lookup vn declVars]
                       , v <- alignClassPtrs bound binding ]
      -- The variable's own declared bound, as a floor under all of the above: @$A -> Eq@ says a
      -- parameter typed by @$_A@ is an @Eq@ whatever the call passes, and that holds even when
      -- nothing binds the variable at all — an empty list pins no element type, but the bound is
      -- still written on the declaration. Merging is by intersection ('mergeVarEnvs'), so a real
      -- binding narrows past this rather than being widened by it.
      let declBoundVenvs = [ H.singleton (TVVar vn) b | (vn, b) <- H.toList declVars, b /= PTopType ]
      let inTy = substituteTVExt typeEnv (mergeAllVarEnvs typeEnv (venv0 : argVenvs ++ boundVenvs ++ declBoundVenvs)) (getMetaType inM)
      -- Resolved knowledge only, which is NOT the same as fully concrete. Inside a generic
      -- definition the element type is the declared bound — @$_A -> Eq@ read in that frame is
      -- @∀Eq@ — and that is a sound upper bound on the parameter at every epoch, since no value
      -- of @$_A@ can fall outside its own class. What must stay out is a SYMBOLIC variable, which
      -- may still resolve to anything and would oscillate against later epochs, and Top, which
      -- says nothing. Rejecting @∀Eq@ too is what left a lambda inside a generic definition at
      -- Top, so a @Some(value= v)@ built from it could not meet the @Maybe@ instance's @Eq@ bound.
      guard (inTy /= PTopType && not (isBottomType inTy) && not (typeHasVars inTy))
      return inTy
    -- Recover the bindings a class parameter's var POINTER stands for, by meeting the declared
    -- spelling against what the call passes. Meeting @List[$T -> $_A]@ with a @Cons(head= 1, ..)@
    -- binds nothing in 'intersectTypesWithVarEnv' — the value's own 'ptVars' are empty until the
    -- meet fills them — but the meet's RESULT holds @Cons[/T=Integer]@, materialized through the
    -- data definition's @head -> $T@ field tie, and the spelling says which external var that
    -- parameter names, so matching the two by parameter name recovers @$_A=Integer@.
    alignClassPtrs spelled actual =
      [ H.singleton (TVVar extV) elemT
      | spelled /= PTopType
      , UnionType Nothing rLeafs [] <- [intersectTypes typeEnv spelled actual]
      , (cvn, extV) <- boundClassVarPtrs spelled
      , let elemT = unionAllTypes typeEnv (mapMaybe (H.lookup cvn . ptVars) (splitUnionType rLeafs))
      , not (isBottomType elemT) ]
    -- The (class parameter name, external var name) pointers of a bound's class partials:
    -- @Functor[$T -> $_A]@ ⇒ [(T, A)], whether the bound is a predicate top or a positive
    -- class-named leaf.
    boundClassVarPtrs bound = concatMap fromPartial (case bound of
      UnionType (Just (preds, _, _)) posLeafs _ -> predPartials preds ++ splitUnionType posLeafs
      UnionType Nothing posLeafs _           -> splitUnionType posLeafs
      _                                      -> [])
      where
        fromPartial p = [ (vn, an) | (vn, TypeVar (TVVar an) TVOuter _) <- H.toList (ptVars p) ]
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
      , TypeVar (TVVar vn) _ _ <- [getMetaType outM]
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
    argRefsVar vn (TypeVar (TVVar vn') _ _)               = vn' == vn
    argRefsVar vn (TypeVar (TVApplied (TVVar vn') _) _ _) = vn' == vn
    argRefsVar _ _                                        = False
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
  let argObjMap = objectMapFromList $ concatMap (\(inExpr, outM) -> [ObjArr (Just inExpr) ArgObj Nothing [] (Just (Nothing, emptyMetaT (substituteTVExt (feTypeEnv env) (fmap snd vaenv) (getMetaType outM))))]) $ H.elems argVaenv'

  -- final ReachesEnv. Reuse the per-epoch cached reaches type graph ('feReachesTypeEnv');
  -- it is constraint-independent, so rebuilding it per call would be redundant.
  let argTypeEnv = mkTypeEnv $ Prgm argObjMap (classGraphFromObjs argObjMap) []
  let feTypeEnv' = feReachesTypeEnv env
  -- A constraint with NO arg objects uses the per-epoch env value ITSELF. The empty merge
  -- is not free — '<>' on 'TypeEnv' re-merges the class graph ('graphFromEdges') and
  -- re-derives 'mkSatCache' from the merged representation — and it ran once per
  -- 'ArrowTo'\/'PropEq' constraint EXECUTION, every epoch (profiled at 72% of typecheck
  -- inside the reach machinery it feeds). Sharing the one per-epoch value drops that
  -- rebuild for the (majority) no-arg constraints.
  -- A merged env never carries a query-cache token ('<>' clears it: a merged env is a
  -- different value), so every memo — reach-query, satGoal, class-expansion — silently
  -- bypassed for ARG-CARRYING constraints: their whole reaches/SAT compute re-derived
  -- shared subgoals as a tree (set.ct's functor-law fragment: one goal re-decided 3,553
  -- times under a byte-identical key, OOM before epoch 5). The merged env lives for this
  -- one constraint execution and is immutable, so mint it its own token — the same
  -- token-per-immutable-env-value rule as the per-epoch mint in 'runConstraintsIdx'.
  let mergedTypeEnv
        | H.null argVaenv' = feTypeEnv'
        | otherwise        = let m = argTypeEnv <> feTypeEnv'
                             in m{teQueryCacheKey = Just (unsafeFreshQueryToken (teTypeGraph m))}
  return $ ReachesEnv mergedTypeEnv (fmap snd vaenv) S.empty

-- | The resolved view of exactly the object-side arg entries 'mkReachesEnv' reads from a
-- constraint's 'conOs' (each arg's in-expression and out meta, resolved through the current
-- point schemes). These are the only per-execution inputs of an 'ArrowTo' beyond its own
-- resolved constraint view, so they complete the skip-memo key ('feConMemo').
conOaResolveKey :: FEnv -> VConstraint -> [(Maybe SExpr, Maybe ShowMeta)]
conOaResolveKey env con =
  [ (tcreToMaybe (showExpr env InputMeta inExpr), tcreToMaybe (showM env ArrMeta outM))
  | oa <- conOs con
  , (inExpr, outM) <- map head $ H.elems $ snd $ splitVarArgEnv $ exprVarArgs $ oaObjExpr oa ]

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
arrowConstrainUbs env con@Constraint{conVaenv} srcS@SType{stypeAct=TypeVar v _ _, stypeReq=srcReq} destS = do
  let srcv = maybe PTopType (stypeAct . snd) (H.lookup v conVaenv)
  case (srcv, stypeAct destS) of
    (PTopType, PTopType) -> return (stypeAct srcS, srcReq, stypeAct srcS, Nothing)
    _ -> do
      (_, _, cdest, destRT) <- arrowConstrainUbs env con srcS{stypeAct=srcv} destS
      return (stypeAct srcS, srcReq, cdest, destRT)
arrowConstrainUbs env@FEnv{feTypeEnv} con@Constraint{conVaenv} SType{stypeAct=src@(UnionType Nothing srcPartials consts), stypeReq=srcReq} SType{stypeAct=destAct, stypeReq=destReq, stypeReqDecl=destReqDecl} = do
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
  -- Settle-forcing requirement: a FORM the DECL-ORIGIN requirement REFUSES ('reqAdmitsForm'
  -- against 'stypeReqDecl' — the unified admission rule; the declaration boundary answers
  -- "must this settle now, or may it settle later"), with a viable compilation path to forms
  -- within the bound ('reachesTo'), is REQUIRED to follow its arrows — the destination holds
  -- only what the consumer receives, so the refused form is dropped and the actual keeps the
  -- settled remainder. The accumulated 'stypeReq' is NOT consulted here: its reverse-direction
  -- content includes the source's own met forms (the contract-discovery echo), and a form's
  -- own echo would license the form itself (the toString/@++@ self-licensing divergence; a
  -- nullary form like @mempty@ shows no structural test can separate echo from license). A
  -- form with no viable path (or no engaged decl requirement) is kept: mid-solve its reach may
  -- still be resolving, and dropping it would not be an upper bound of the point's values.
  -- The drop is also skipped when it would empty the stored type (settled alternatives not
  -- yet accumulated), which would poison the point to 'BottomType'.
  -- Arrow-applicability first (as 'settlePropSlot'): settling is rewriting, so a form whose
  -- NAME no arrow can accept can never settle elsewhere and is never dropped — its refusal
  -- surfaces as an error at Decode instead. The probe is the bare name with open args (no
  -- deep-argument intersects); a name-level over-approximation only admits more leaves to
  -- the later gates. This retires the TODO(admission) that lived here: all the
  -- settle-forcing writers ('dropSettleForcedRaws', the stay-put viability/met forms below,
  -- and 'settlePropSlot') consult the ONE criterion in the same change.
  -- DEFERRAL NEEDS A CONSUMER (Zach's ruling, 2026-08-16): deferring a rewritable form is
  -- meaningful only while some arrow can still act on it — its own arrows at its rewrite
  -- frontier, or an arrow whose declared INPUT spells the unsettled form
  -- ('feInputArrowNames'). The frontier spans the whole application PLUMBING: an Arrow
  -- result (dispatch keying reads those raws deliberately) and the base/builder links
  -- (ArrowTo 7 — the raw form there IS the call being assembled; forcing it empties the
  -- base to ⊥). Only an ARGUMENT-ARM destination (ArrowTo 10/15 — the value passed INTO
  -- another call) is past the frontier: a form there that no arrow input accepts is forced
  -- to settle through its direct arrows NOW even where no declaration engages — otherwise
  -- it rides (through var-linked dims) into values no arrow can ever act on ("no arrows
  -- from Some[/T=raw ∪ …]" at TreeBuild).
  let destIsArgArm = case con of
        Constraint _ _ (ArrowTo 10 _ _) -> True
        Constraint _ _ (ArrowTo 15 _ _) -> True
        _                               -> False
  let settleForced l = not (null (typeGraphQueryAllWithReason reachesTypeEnv vaenv' ((partialVal (ptName l)){ptArgMode=PtArgAny})))
                     && (not (reqAdmitsForm reachesTypeEnv vaenv' destReqDecl l)
                          || (destIsArgArm && not (S.member (ptName l) (feInputArrowNames env))))
                     && reachesTo reachesTypeEnv vaenv' (singletonType l) destBound
  let dropSettleForcedRaws t@(UnionType dMTop dLeafs dConsts)
        | (destReqDecl /= PTopType || destIsArgArm) && not (isBottomType destBound) =
            case partition settleForced (splitUnionType dLeafs) of
              ([], _) -> t
              (_, keptLs) | null keptLs && isNothing dMTop && null dConsts -> t
                          | otherwise -> UnionType dMTop (joinUnionType keptLs) dConsts
      dropSettleForcedRaws t = t
  let newBindings = H.difference venv' vaenv'
  let resolveNew = if H.null newBindings then id else substituteKeep feTypeEnv newBindings
  let destFinal = compactType feTypeEnv vaenv' (dropFree (resolveSelf (resolveNew (dropSettleForcedRaws dest'))))
  -- Return-type direction: a declaration output that is a bare declared variable is determined
  -- by the call site's destination — bind it onto the call partial (see 'outVarBoundSrc').
  let srcBound = outVarBoundSrc env destFinal src

  -- Reverse direction: the arrow imposes a requirement back on the source. Keep only the source
  -- components that actually reach the required destination — a component is viable if its own
  -- singleton is within 'destFinal', or (following intermediate arrows) its reach subtree can be
  -- cut to within 'destFinal'. This is the per-child test 'reachesHasCutSubtypeOf' already uses, so
  -- a source that reaches the destination only through a further arrow still counts as viable.
  -- A component also reaches the destination by STAYING PUT — a power of the term-rewriting
  -- language: settling may happen later through multi-level function rules, and the
  -- destination's DECL-ORIGIN requirement is what decides whether it must happen NOW (the
  -- 'reqAdmitsForm' rule). The requirement decides at the level of the FORM: when its own
  -- expansion restricted to the component's name still contains that form (a data member, or
  -- a matching unsettled form the declaration licenses), the component may stay, and its
  -- stay-put contribution is the PORTION of its values the destination admits (the meet — a
  -- module-generic @Node(value=⊤, ..)@ against a refined slot keeps the refined portion). A
  -- form the declaration refuses outright must settle through its arrows, exactly the
  -- settle-forcing doctrine. No settled/raw distinction is made: an unsettled call form is a
  -- valid data object like a constructed one. Without stay-put, a component that neither
  -- subtypes the destination nor follows arrows (the rebuilt @Node(value=⊤, ..)@ of an
  -- instance arm's body — data has no arrows) was silently neither narrowed nor errored, and
  -- TreeBuild later failed the same conversion. Consulting the DECL channel (never the
  -- accumulated requirement) is what makes this safe: 'narrowedReq' below writes the
  -- source's own met forms into 'stypeReq' (contract discovery), and admission reading that
  -- channel would license the very forms the machinery wrote — the self-licensing loop.
  -- Stay-put requires an ENGAGED declaration: a ⊤ decl requirement means no declaration
  -- decides at this destination — nothing licenses rewriting the source requirement toward
  -- the destination's accumulated content, so the reverse direction keeps its master
  -- behavior (arrow mets or bare forms). Without this gate, every undeclared consumption
  -- point contributes @key ∩ destFinal@ where 'destFinal' includes the accumulated
  -- requirement — re-injecting the previous epoch's met forms one nesting level deeper
  -- (measured: the toString/@++@ family diverges to the 'runConstraints' limit).
  let declEngaged = case destReqDecl of
        PTopType -> False
        TypeVar v _ _ -> case vaenvLookup vaenv' v of
          PTopType                 -> False
          TypeVar v' _ _ | v' == v -> False
          _                        -> True
        _ -> True
  let stayPutMet key
        | declEngaged
        , reqAdmitsForm reachesTypeEnv vaenv' destReqDecl key =
            -- A key already WITHIN the destination stays with its own spelling: the meet
            -- would only respell it with the destination frame's materializations
            -- (@head=$/T@ resolved per call becoming a module-frame @head=∀Ord@), which
            -- downstream per-call reads can no longer resolve. Only a genuinely wide key
            -- contributes its met (narrowed) portion.
            if isSubtypeOfWithEnv feTypeEnv vaenv' (singletonType key) destFinal
              then Just (singletonType key)
              else case intersectTypesEnv feTypeEnv vaenv' (singletonType key) destFinal of
                met | isBottomType met -> Nothing
                    -- A TypeVar meet is a symbolic LINK (the destination is a bare variable
                    -- slot, e.g. an argument reference's @TVArg@ point), not a value-set
                    -- portion — storing it in the requirement would replace a concrete
                    -- spelling with a var that reads ⊤.
                    | isTypeVar met    -> Nothing
                    | otherwise        -> Just met
        | otherwise = Nothing
  let viableKeys = case reaches' of
        ReachesPartialTree children ->
          [ key
          | (key, subTree) <- H.toList children
          , isSubtypeOfWithEnv feTypeEnv vaenv' (singletonType key) destFinal
            || reachesHasCutSubtypeOf feTypeEnv vaenv' subTree destFinal
            || isJust (stayPutMet key) ]
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
  -- A component's met form unions its STAY-PUT portion (its meet with the destination — see
  -- 'stayPutMet' above) with the met forms of the arrows it triggers: the value either
  -- already lies in the destination or rewrites toward it. A component with neither (no
  -- admitted portion, no accepting arrow — possible mid-epoch) keeps its bare form.
  -- The two sides combine STRUCTURALLY (a leaf-map merge, the same mechanism the
  -- requirement-forced choice above uses to reattach raw leaves): an algebraic union would
  -- let normalization absorb the arrow mets' member templates
  -- (@Cons[/T=⊤](head=$/T, tail=∀ConsList)@ — whose pointer-occurrence reads narrow the
  -- element per level from the content) into a stay-put class-pred, erasing the pointers so
  -- the element sticks at its class bound (@/T=∀Ord@ where the content is @Integer@).
  let structUnion a b = case (a, b) of
        (UnionType Nothing aLs [], UnionType mt bLs cs) -> UnionType mt (H.unionWith S.union aLs bLs) cs
        (UnionType mt aLs cs, UnionType Nothing bLs []) -> UnionType mt (H.unionWith S.union aLs bLs) cs
        _ -> unionTypesWithEnv feTypeEnv vaenv' a b
  -- A constant reaching an ARGUMENT SLOT is held there at its TYPE, not as itself: an argument
  -- in expression position gets its slot through 'ArrowTo', whose forward reach reads a constant
  -- source through 'constantPartialType' (@CStr "x"@ arrives as @/Data/String@). A DECLARATION's
  -- fields keep their constants — its arguments are tied to their values by 'EqPoints', not by a
  -- reach — so a met against a declared value imports a constant the actual it constrains can
  -- never carry. Monotonicity then locks the requirement below the actual for good, and the two
  -- disagree at TreeBuild ("no arrows from @Cons(/head=/Data/String)@" toward
  -- @Cons(/head=CStr "x")@ — the shape @maybeField.ct@\/@listField.ct@\/@valueDeclFields.ct@ fail).
  -- So read the met's nested constants in the same currency as the source: at their type. Only
  -- NESTED ones — a constant the source itself is stays whatever it is, and the type parameters
  -- the met discovers (@/T=Integer@ out of @(l::$T == r::$T)@) are untouched.
  let widenSlotConsts t = case t of
        UnionType mTop leafs tConsts -> UnionType mTop (joinUnionType (map onLeaf (splitUnionType leafs))) tConsts
        _ -> t
        where
          onLeaf l@PartialType{ptArgs} = l{ptArgs = fmap atSlot ptArgs}
          atSlot (UnionType Nothing (H.null -> True) cs@(_:_)) =
            unionAllTypes feTypeEnv (map (singletonType . constantPartialType) cs)
          atSlot v = widenSlotConsts v
  let viableMet key =
        let arrowMets = [ widenSlotConsts metSrc | (_, metSrc, _) <- typeGraphQueryAllWithReason reachesTypeEnv vaenv' key ]
            arrowMet = case arrowMets of
              []   -> Nothing
              mets -> Just (unionAllTypesWithEnv feTypeEnv vaenv' mets)
        in case (stayPutMet key, arrowMet) of
             (Nothing, Nothing) -> singletonType key
             (Just sp, Nothing) -> sp
             (Nothing, Just am) -> am
             (Just sp, Just am) -> structUnion am sp
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

-- | THE UNIFIED ADMISSION RULE (Zach's ruling, 2026-08-14): the requirement is the boundary
-- answering "must this form settle NOW, or may it settle LATER" — settling later through
-- multi-level function rules is a core power of the term-rewriting language, and an
-- unsettled call form is a valid data object like any other (no settled/raw category test
-- appears anywhere in this rule). Admission is decided at the level of the FORM: the
-- requirement's expansion RESTRICTED TO THE FORM'S NAME must be non-empty. A class
-- component expands to its declared members and never an unrewritten call (@∀ConsList@
-- admits @Cons@/@Nil@), so a raw call against a settled requirement is refused — the
-- settle-forcing doctrine, expressed by the expansion. The VALUE portion that survives an
-- admission is each consumer's own meet against its own bound (the stay-put portion in
-- 'arrowConstrainUbs', the slot bound in 'settlePropSlot'); the earlier as-is subtype test
-- (@reqAdmitsLeaf@) wrongly refused a wide data form (@Node(value=⊤, ..)@ against its
-- Eq-refined member expansion) whose narrowed portion is exactly the point.
-- The requirement consulted is always the DECL-ORIGIN channel ('stypeReqDecl' or a
-- site-derived declared bound), never the accumulated 'stypeReq': the accumulated channel
-- carries the reverse direction's echo of a point's own met forms, and a form's own echo
-- licensing the form is the self-licensing loop (one nesting level per epoch in the
-- toString/@++@ family; nullary forms like @mempty@ prove no structural cap can fix it).
-- Every settle-forcing writer consults THIS function — one criterion (see the retired
-- TODO(admission)): 'arrowConstrainUbs' (stay-put viability/met forms and
-- 'dropSettleForcedRaws') and 'settlePropSlot'.
reqAdmitsForm :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> PartialType -> Bool
reqAdmitsForm _ _ PTopType _ = True
-- A var requirement resolves through the env; a self-referential entry (which an
-- unconstrained var may acquire during intersection) carries no information and licenses
-- like ⊤ — following it verbatim would recurse forever ('isSubtypeOfWithEnv' parity).
reqAdmitsForm typeEnv vaenv (TypeVar v _ _) l = case vaenvLookup vaenv v of
  TypeVar v' _ _ | v' == v -> True
  looked                   -> reqAdmitsForm typeEnv vaenv looked l
reqAdmitsForm typeEnv vaenv req l =
  not $ null $ splitUnionType $ expandTypeWithNames typeEnv vaenv req (S.singleton (ptName l))

-- | Settle a construction slot at a 'PropEq' write — the slot-write mirror of
-- 'arrowConstrainUbs''s settle-forcing. A 'PropEq' meet can never remove an unsettled call
-- form once written into a slot (the algebra answers its class membership
-- SAT-conservatively), so a definition body's early slot write (@tail = declared-top ∪
-- rawRecursiveCall@) keeps that spelling forever even after the slot's requirement engages,
-- and the dispatch presentation reads the stale form. Whether the form may stay unsettled is
-- answered by the requirement boundary ('reqAdmitsForm'); here the requirement is the slot's
-- own DECL-ORIGIN requirement MET WITH the field's declared bound (@Cons@'s @tail -> ConsList[$T -> $_T]@
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
settlePropSlot :: FEnv -> TypeVarArgEnv -> TypeVarAux -> Type -> Type -> Type -> (Type, Type, Type)
settlePropSlot env@FEnv{feTypeEnv} vaenv propName@(TVArg k) superAct@(UnionType supMTop supLeafs supConsts) superReq@(UnionType reqMTop reqLeafs reqConsts) superReqDecl =
  (superAct', superReq', superReqDecl')
  where
    reachesTypeEnv = feReachesTypeEnv env
    reqByName = splitUnionTypeByName reqLeafs
    -- The decl-origin requirement channel ('stypeReqDecl') receives the same declared-bound
    -- tighten when the settle fires: 'declBoundU' is instantiated from 'typeGraphData' at this
    -- site, so it is decl-origin by construction and the write preserves the channel's purity.
    declReqByName = case superReqDecl of
      UnionType _ dLeafs _ -> splitUnionTypeByName dLeafs
      _                    -> H.empty

    settlePartial :: PartialType -> (PartialType, Maybe PartialType, Maybe PartialType, Bool)
    settlePartial p = fromMaybe (p, Nothing, Nothing, False) $ do
      -- Fireable shapes: the stale family spelling is @declared-top ∪ rawCall@, and — now
      -- that arm-level forcing follows the DECL channel and no longer prunes every
      -- accumulated raw at undeclared arms — also a PLAIN positive union whose settled
      -- alternatives sit among the sibling leaves (@rawCall ∪ String ∪ Integer@): there the
      -- bound (slot actual ∩ requirement ∩ declared) contains the settled siblings, so the
      -- drop is exactly the alternatives-accumulated settle. The never-empty guard below
      -- refuses a drop that would empty a top-less slot.
      UnionType aMTop aLeafs aConsts <- H.lookup k (ptArgs p)
      let aLeafList = splitUnionType aLeafs
      guard (not (null aLeafList))
      -- Only a REWRITABLE leaf (an unsettled name — one the graph may still rewrite) can be
      -- settle-forced, so a slot holding only settled data content skips ALL of the work
      -- below (declared-bound meets, arrow probes, reach queries, commit-time
      -- verification) before any graph or engine call. This is the cheap membership check
      -- ('isSettledLeaf'), not the per-leaf arrow probe 'settleForced' still applies.
      guard (any (not . isSettledLeaf reachesTypeEnv) aLeafList)
      [rp] <- H.lookup (ptName p) reqByName
      slotReq <- typeGetAux propName rp
      -- Engaged requirement only; a pointer-managed (var) slot is left to the var machinery.
      guard (slotReq /= PTopType)
      case slotReq of
        TypeVar{} -> Nothing
        _         -> return ()
      -- Declared bounds come from BOTH declaration families: data-definition objects
      -- ('typeGraphData' — construction slots) AND arrow-object input templates
      -- ('typeGraphArrowTemplates' — call-argument slots, e.g. @operator++@'s declared
      -- operand @$T@ under @[$T -> Semigroup]@). The arrow templates are the decl-side
      -- analogue of the reverse direction's contract discovery: the same declared contract,
      -- taken from the declaration's SPELLING rather than from the source's met forms, so
      -- admission stays origin-pure. Without them, call slots have no declared boundary and
      -- the raw operand forms of a nested call chain are never pruned — the slot's leaf args
      -- and its operand's union embed each other one level deeper per epoch (the
      -- toString/@++@ family diverges to the 'runConstraints' limit).
      -- A ⊤ TEMPLATE slot contributes nothing rather than widening the union to ⊤: a
      -- declaration's var-typed slot (@l -> $T@ under @[$T -> Semigroup]@) currently
      -- PRESENTS as ⊤ in the base-env object map (the declared-var erasure family — see the
      -- bare-arg gap), and a ⊤ contribution would swallow the concrete instance templates
      -- (@Integer@/@String@) and neutralize the bound. The union of the remaining templates
      -- is the realizable declared bound (a slot value must fit SOME arrow's declared
      -- input). Data-side contributions keep their current semantics unchanged.
      -- A VAR-TYPED template slot (@equal[$T -> Eq](this -> $T, other -> $T)@) resolves
      -- through the template's OWN ptVars to the var's declared bound (@∀Eq@) — the spelled
      -- declared linkage, exactly what 'varStampSupported' reads from these templates. A
      -- plain 'freeTVIntToTopDeep' would read the free @$T@ as ⊤ and lose the bound.
      -- Arrow templates are read from the PER-EPOCH reaches env, not the base 'feTypeEnv':
      -- the base env's object metas for the CURRENT program are still unresolved (⊤ — see
      -- the env-choice doctrine in 'arrowConstrainUbs'), so same-program declarations would
      -- present no templates and their call slots would never force. Data spellings are
      -- parse-stable and keep the base env.
      let dataBounds = [freeTVIntToTopDeep feTypeEnv vaenv p d | PartialType{ptArgs=dArgs} <- typeGraphData feTypeEnv (ptName p), Just d <- [H.lookup k dArgs]]
      let declBounds = dataBounds
                    ++ [b | PartialType{ptArgs=dArgs, ptVars=dVars} <- typeGraphArrowTemplates reachesTypeEnv (ptName p)
                          , Just d0 <- [H.lookup k dArgs]
                          , let d = case d0 of
                                  TypeVar (TVVar vn) _ _ | Just vb <- H.lookup vn dVars -> vb
                                  _                                                  -> d0
                          , let b = freeTVIntToTopDeep feTypeEnv vaenv p d
                          , b /= PTopType]
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
      -- The unified admission rule ('reqAdmitsForm'), distributed over the DECL-ORIGIN slot
      -- requirement and the declared bound (form present in the meet ⟸ present in both — the
      -- keep-safe direction). The accumulated 'slotReq' still gates ENGAGEMENT above (the
      -- settle machinery only runs where a requirement engaged), but admission itself never
      -- consults it: its reverse-direction met forms would license the very forms they echo.
      let slotDeclReq = fromMaybe PTopType $ do
            [rdp] <- H.lookup (ptName p) declReqByName
            typeGetAux propName rdp
      -- DEFERRAL NEEDS A CONSUMER (Zach's ruling, 2026-08-16): deferring a rewritable form
      -- is meaningful only while some arrow can still ACT on the deferred form — its own
      -- arrows at its rewrite frontier, or an arrow whose declared INPUT spells the
      -- unsettled form. A DATA-declared field is where deferral dies: data has no arrows,
      -- and once a raw form is embedded in a constructed value's field no arrow path leads
      -- back to it (TreeBuild later finds "no arrows from Some[/T=raw ∪ …]"). So at a
      -- data-declared slot a ⊤-resolved field bound (@Some@'s @value -> $T@ with free @$T@)
      -- means "settled content, element unconstrained" — it does NOT license unsettled
      -- forms; only a field whose declared bound literally SPELLS the form admits it (the
      -- metaprogramming license). Call-argument slots keep ⊤-licenses: their values remain
      -- at positions arrows can consume.
      let admittedAtDataField l = case declBoundU of
            PTopType -> False
            _        -> reqAdmitsForm reachesTypeEnv vaenv declBoundU l
      let admitted l | not (null dataBounds) = reqAdmitsForm reachesTypeEnv vaenv slotDeclReq l
                                             && admittedAtDataField l
                     | otherwise = reqAdmitsForm reachesTypeEnv vaenv slotDeclReq l
                                 && reqAdmitsForm reachesTypeEnv vaenv declBoundU l
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
              -- The tighten never commits a respell that LOSES a type-var spelling the slot
              -- carried (@∀Map[/K=$/MK, /V=$/MV]@ meeting a declared bound whose frame reads
              -- the pointers as ⊤ would store @∀Map[/K=⊤, /V=⊤]@) — the same 'typeVarLost'
              -- discipline the reverse narrowing applies; the pointer spelling is what
              -- downstream per-call reads resolve.
              let tightenSlot cur = do
                    let t = compactType feTypeEnv vaenv (intersectTypesEnv feTypeEnv vaenv cur declBoundU)
                    guard (not (typeVarLost cur t))
                    return t
              let rp' = case H.lookup k (ptArgs rp) of
                    Just UnionType{} -> do
                      t <- tightenSlot slotReq
                      Just rp{ptArgs=H.insert k t (ptArgs rp)}
                    _ -> Nothing
              let rdp' = case H.lookup (ptName p) declReqByName of
                    Just [rdp] -> case H.lookup k (ptArgs rdp) of
                      Just slotDeclCur@UnionType{} -> do
                        t <- tightenSlot slotDeclCur
                        Just rdp{ptArgs=H.insert k t (ptArgs rdp)}
                      _ -> Nothing
                    _ -> Nothing
              return (p', rp', rdp', True)

    results = map settlePartial (splitUnionType supLeafs)
    changed = or [c | (_, _, _, c) <- results]
    superAct' | changed = compactType feTypeEnv vaenv (UnionType supMTop (joinUnionType [p | (p, _, _, _) <- results]) supConsts)
              | otherwise = superAct
    reqUpdates = H.fromList [(ptName rp, rp) | (_, Just rp, _, _) <- results]
    superReq' | H.null reqUpdates = superReq
              | otherwise = UnionType reqMTop (joinUnionType (map (\q -> fromMaybe q (H.lookup (ptName q) reqUpdates)) (splitUnionType reqLeafs))) reqConsts
    declUpdates = H.fromList [(ptName rdp, rdp) | (_, _, Just rdp, _) <- results]
    superReqDecl' = case superReqDecl of
      UnionType dMTop dLeafs dConsts | not (H.null declUpdates) ->
        UnionType dMTop (joinUnionType (map (\q -> fromMaybe q (H.lookup (ptName q) declUpdates)) (splitUnionType dLeafs))) dConsts
      _ -> superReqDecl
settlePropSlot _ _ _ superAct superReq superReqDecl = (superAct, superReq, superReqDecl)
