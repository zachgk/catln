--------------------------------------------------------------------
-- |
-- Module    :  Eval
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is used to run the interpreter (or compiler macro)
-- on a program. It can build, run, and compute the annotations
-- for the program.
--------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Eval where

import           CompileExpr             (compileStatementsIn, parseExprSource,
                                          prgmFromGraphData)
import           CRes
import qualified Data.HashMap.Strict     as H
import           Semantics.Prgm
import           Semantics.Types

import           Control.Applicative     (asum, (<|>))
import           Control.Monad
import           Data.Graph
import           Data.Maybe
-- import           Emit                (codegenExInit)
import           Control.Monad.State
import           CtConstants
import           Data.Hashable           (hash)
import qualified Data.HashSet            as S
import           Data.UUID               (UUID)
import           Eval.Common
import           Eval.Env
import           Eval.ExprBuilder
import           Eval.Runtime
import           Eval.Runtime.System     (realIOHandle)
import           Semantics
import           Semantics.Annots        (isAnnot)
import           Semantics.CtType        (fromCt)
import           Semantics.TypeGraph     (ReachesEnv (ReachesEnv),
                                          reachesHasCutSubtypeOf,
                                          reachesPartials, reachesTo)
import           Syntax.Ct.Builder       (rawVal)
import           Syntax.Ct.Formatter     (formatRootPrgm)
import           Syntax.Ct.Prgm          (RawObjArr (..), RawPrgm (..),
                                          RawStatement (..),
                                          RawStatementTree (..))
import           Syntax.Ct.Results       (AnnotShown (..), notePage,
                                          showAnnotResults, statementStart)
import           Syntax.HsBuiltins.Types (oneFileResult, valResultFiles)
import           Text.Megaparsec         (sourceName)
import           Text.Printf
import           TreeBuild
import           Utils

evalRunnable :: EvalMode -> Bool
evalRunnable EvalRunWithContext{} = True
evalRunnable EvalRun{}            = True
evalRunnable _                    = False

evalBuildable :: EvalMode -> Bool
evalBuildable EvalRunWithContext{}   = True
evalBuildable EvalBuildWithContext{} = True
evalBuildable EvalBuild{}            = True
evalBuildable _                      = False

-- | Checks if a function is defined (not declared) and finds the 'EvalMode' of the function.
-- It will look through all of the objects to see if they match the function name or are Context(value=function name).
-- Then, those found functions can be checked for run/build based on whether the return type is a CatlnResult.
-- It also will pass the function name back through EvalMode in order to convert 'PRelativeName' into the matching 'PTypeName'
-- TODO The use of listToMaybe will secretly discard if multiple evalTargetModes or function names are found. Instead, an error should be thrown
evalPrgmTargetMode :: String -> EPrgm -> EvalMode
evalPrgmTargetMode function prgm =
  case maybeGetSingleton $ expandRelPartial typeEnv H.empty (partialVal (mkName function)) of
    Nothing -> NoEval (reachesPartials reachEnv [partialVal (mkName function)]) (reachesPartials reachEnv [partialVal (mkName function)])
    Just f ->
      let function' = ptName f
          funCtxTp = (partialVal (mkName ContextInStr)){ptArgs=H.fromList [(partialKey (mkName contextValStr), typeVal function'), (partialKey (mkName ioStr), ioType)], ptArgMode=PtArgAny}
          funTp = partialVal function'
          funCtxReaches = reachesPartials reachEnv [funCtxTp]
          funReaches = reachesPartials reachEnv [funTp]
      in case (funCtxReaches, funReaches) of
           (rt, _) | reachesHasCutSubtypeOf typeEnv H.empty rt (ioContextType resultType) -> EvalBuildWithContext (nameStr function')
           (rt, _) | reachesHasCutSubtypeOf typeEnv H.empty rt (ioContextType PTopType) -> EvalRunWithContext (nameStr function')
           (_, rt) | reachesHasCutSubtypeOf typeEnv H.empty rt resultType -> EvalBuild (nameStr function')
           (_, rt) | reachesHasCutSubtypeOf typeEnv H.empty rt ioType -> EvalRun (nameStr function') -- Should require isShowable for run result
           _ -> NoEval funCtxReaches funReaches
  where
    typeEnv = mkTypeEnv prgm
    reachEnv = ReachesEnv typeEnv H.empty S.empty

-- | What a function run in an io answers with: a 'ContextStr' holding its value, of the type given,
-- in the io it ends in. A run's is any value (@(){IO}@) and a build's a result (@CatlnResult{IO}@).
ioContextType :: Type -> Type
ioContextType valT = singletonType $ (partialVal (mkName ContextStr)){ptArgs=H.fromList [(partialKey (mkName contextValStr), valT), (partialKey (mkName ioStr), ioType)], ptArgMode=PtArgAny}

-- | The input a run or build of a function in an io starts from: the function in an io.
ioContextInput :: String -> EExpr
ioContextInput function' = eApplyM (eVal ContextInStr `eApply` (contextValStr, eVal function')) ioStr ioM

evalTargetMode :: String -> FileImport -> EPrgmGraphData -> CRes EvalMode
evalTargetMode function prgmName prgmGraphData = do
  prgm <- prgmFromGraphData prgmName prgmGraphData
  return $ evalPrgmTargetMode function prgm

-- | Gets the target modes for all objects in the file
evalAllTargetModes :: EPrgm -> H.HashMap UUID EvalMode
evalAllTargetModes prgm@Prgm{prgmObjMap} = H.fromList $ mapMaybe targetMode $ flatObjectMap prgmObjMap
  where
    targetMode oa = case nameStr (oaObjPath oa) of
      ContextInStr -> case H.lookupDefault Nothing (partialKey (mkName contextValStr)) $ exprAppliedArgsMap $ oaObjExpr oa of
        Just (_, Just valE) -> case maybeExprPath valE of
          Just valName -> Just (getMetaId $ getExprMeta $ oaObjExpr oa, evalPrgmTargetMode (nameStr valName) prgm)
          _ -> Nothing
        _ -> Nothing
      _ -> Just (getMetaId $ getExprMeta $ oaObjExpr oa, evalPrgmTargetMode (nameStr (oaObjPath oa)) prgm)

-- | evaluate annotations such as assertions that require compiler verification
--
-- The annotation is taken as well as the value it evaluated to, because an annotation reporting on
-- where it is written cannot be told that from its value: an evaluated annotation is a tuple of what
-- its arguments came to, with nothing left of the expression it was.
evalCompAnnot :: TExpr EvalMetaDat -> Val -> EvalM ()

evalCompAnnot _ (TupleVal n args) | n == assertStr = case (H.lookup assertTestStr args, H.lookup assertMsgStr args) of
  (Just b, Just (StrVal _)) | b == true -> return ()
  (Just b, Just (StrVal msg)) | b == false -> liftCRes $ CErr [MkCNote $ AssertCErr msg]
  (Just b, Nothing) | b == true -> return ()
  (Just b, Nothing) | b == false -> liftCRes $ CErr [MkCNote $ AssertCErr "Failed assertion"]
  _ -> evalError $ printf "Invalid assertion with unexpected args %s" (show args)
-- A @#debug@ records its argument against the call it was reached in. The position is the
-- annotation's own rather than its argument's, since an operator apply carries none (see
-- 'EvalDebug'), and it is read here rather than at the tree built from it because that tree is what
-- the argument evaluates through and says nothing about where it was written.
evalCompAnnot annot (TupleVal n args) | n == debugAnnot = case H.lookup debugAnnotArg args of
  Just v -> modify $ evalRecordDebug (getMetaPos $ getExprMeta annot) v
  Nothing -> evalError $ printf "Invalid debug with unexpected args %s" (show args)
evalCompAnnot _ TupleVal{} = return ()
evalCompAnnot _ _ = evalError "Eval: Invalid compiler annotation type"

evalObjArr :: Val -> AnyObjArr -> EvalM Val
evalObjArr = evalObjArrCaptured H.empty

-- | 'evalObjArr' for a closure ('ObjArrVal'): the captured creation-site args underlay the
-- closure's own parameters, so body references to enclosing args resolve lexically.
evalObjArrCaptured :: Args -> Val -> AnyObjArr -> EvalM Val
evalObjArrCaptured captured input oa = do
  let newArrArgs = H.union (buildArrArgs oa input) captured
  -- The arm is built for the input as its arguments are: a closure argument's slot holds the
  -- closure's input object (@/fn(/v=..)@), as a typed arm's higher-order slot does, and the arm's
  -- body reaches what it returns through the closure's arrow, which the build is given.
  let closures = valClosureArrows input
  -- Read as deep as the arm's own object looks ('getValDispatchPartial'), as dispatch reads it:
  -- past that a value is its constructors' declared forms with their parameters bound. The arm is
  -- built for what this input is to it, and what the build reads of the input reaches every call
  -- in the arm ('TreeBuild.toTExprDest' reads arguments through it), so the whole value — a
  -- string read as one cell per character — would make every one of them as deep as the value.
  Env{evTypeEnv} <- get
  let armT = either (getExprType . oaObjExpr) (getExprType . oaObjExpr) oa
  let srcP = getValDispatchPartial evTypeEnv [armT] (1 + typePatternDepth armT) input
  (resArrowTree, compAnnots, oldArgs) <- evalStartEArrow srcP closures oa newArrArgs
  forM_ compAnnots $ \compAnnot -> do
            mark <- gets evalTreebugMark
            compAnnot' <- withEvalPush (printf "annot %s" (show compAnnot)) $ evalExpr compAnnot
            modify $ evalRecordAnnot mark (getExprMeta compAnnot) compAnnot'
            evalCompAnnot compAnnot compAnnot'
  res <- withEvalPush (printf "ResEArrow %s" (show oa)) $ evalExpr resArrowTree
  modify $ evalEndEArrow input res oldArgs
  return res

-- | Reduce each partial's argument keys to their base name, dropping the function-parameter
-- signature carried in a higher-order argument's key (e.g. @/fn(/v=..)@ ↦ @/fn@). A runtime
-- value keys a function argument by its bare name (@/fn@) while the typed arrow keys it by
-- the function signature (@/fn(/v=..)@); without this they never match during dispatch even
-- though the function argument is non-discriminating (it is identical across the arrows).
simplifyArgKeys :: Type -> Type
simplifyArgKeys (UnionType m pos cs) = UnionType m (joinUnionType $ map reKey $ splitUnionType pos) cs
  where reKey p = p{ptArgs = H.mapKeys (partialKey . pkName) (ptArgs p)}
simplifyArgKeys t = t

-- | The output type a closure value carries, read from what its body actually forwards.
--
-- A closure is built from the typed tree as it stands, so its output is spelled as the enclosing
-- DEFINITION wrote it. Where the definition is generic that spelling is the declaration variable's
-- BOUND, not what the call bound it to: a wrapper generic over its monad forwards its own function
-- argument onward, and the forwarded closure's output reads as the whole class however the wrapper
-- was called. A bound is not a fact about the value, and everything downstream reads it as one --
-- dispatch asks whether the closure meets the selected arm's requirement, and TreeBuild expands the
-- bound into every member and asks each to reach that arm's destination. A Maybe arm asks its
-- function for a Maybe, and the ConsList member of the bound cannot reach it, so the wrapper is
-- rejected for being imprecise rather than for being wrong.
--
-- The variable itself is gone by here (the typed meta holds the bound, and the arrow's source
-- partial reads the variable as the top), so there is nothing to substitute. What is in hand is
-- the forwarded argument as a VALUE, carrying the output its own creation site gave it, which is
-- the instantiation the bound stands in for. A closure body that forwards nothing keeps its
-- spelling.
closureOutInstantiated :: ObjArr TExpr EvalMetaDat -> EvalM (ObjArr TExpr EvalMetaDat)
closureOutInstantiated oa = do
  Env{evArgs} <- get
  return $ case oaArr oa of
    Just (Just body, arrM) | Just out <- forwardedOut evArgs body ->
      oa{oaArr = Just (Just body, mWithType out arrM)}
    _ -> oa
  where
    -- The closure's body is an application of a function-typed ARGUMENT of the enclosing arrow
    -- (the mandatory re-wrap, written @fn(v)= fn(v= v)@ — a bare @fn@ does not typecheck), so what
    -- this closure returns is what that argument returns. The argument is in hand as a value, and
    -- its output is the one its own creation site gave it, so reading it here replaces the bound
    -- with the instantiation without needing the erased variable back.
    forwardedOut args body = do
      basePath <- maybeExprPath body
      case H.lookup (nameStr basePath) args of
        Just (ObjArrVal inner _) -> do
          (_, innerArrM) <- oaArr inner
          return $ getMetaType innerArrM
        _ -> Nothing

evalCallTree :: Val -> TCallTree -> EvalM Val
-- evalCallTree v ct | trace (printf "evalCallTree %s using %s" (show v) (show ct)) False = undefined
evalCallTree v TCTId = return v
evalCallTree  m (TCMatch destType opts) = do
  Env{evTypeEnv} <- get
  -- Normalize higher-order argument keys (bare @/fn@ on the runtime value vs the
  -- function-signature key @/fn(/v=..)@ on the typed arrow) before matching, mirroring
  -- the 'simplifyArgKeys' handling in the 'TCCond' branch below.
  let reKeyP p = p{ptArgs = H.mapKeys (partialKey . pkName) (ptArgs p)}
  -- A closure argument's slot holds the closure's input object, as a typed arm's higher-order slot
  -- does. The value is read only as deep as the arms look ('getValDispatchPartial').
  let depth = 1 + maximum (0 : map (typePatternDepth . singletonType) (H.keys opts))
  let pats = map singletonType (H.keys opts)
  let valP = reKeyP $ getValDispatchPartial evTypeEnv pats depth m
  -- Membership with the arm's value predicates evaluated on the value ('valWithinTypeAs'): an arm
  -- refined by a predicate the type layer could not decide is taken when the predicate holds.
  let optMatches optType = valWithinTypeAs valP m (singletonType $ reKeyP optType)
  matching <- filterM (optMatches . fst) (H.toList opts)
  case matching of
    [(_, resArrowTree)] -> do
      withEvalPush ("match with val " ++ show m) $ evalCallTree m resArrowTree
    -- No statically-built arm matches the concrete runtime value. An arm can be dropped at build
    -- time by a COMPILE-TIME-ONLY cyclic dispatch (e.g. an 'Eq' instance whose @==@ arm dispatches
    -- back through the same function — the 'ConsList' 'equal' arm recursing through 'tail.equal').
    -- The runtime value is
    -- concrete, so rebuilding the dispatch from its type cannot cycle. Rebuild from the value and
    -- retry once; if the rebuild is not strictly more resolved, fall back to the original error.
    [] -> do
      Env{evTbEnv} <- get
      case buildPartialCallTree evTbEnv [] S.empty PTopType MayHold DestRequired (getValPartialType m) destType of
        CRes _ rebuilt | rebuilt /= TCMatch destType opts ->
          withEvalPush ("rebuild match with val " ++ show m) $ evalCallTree m rebuilt
        _ -> evalError $ printf "Failed match in eval callTree: \n\tVal: %s \n\tVal type: %s\n\tOptions: %s" (show m) (show $ getValPartialType m) (show opts)
    (_:_:_) -> evalError $ printf "Multiple matches in eval callTree: \n\tVal: %s \n\tOptions: %s " (show m) (show opts)
evalCallTree v (TCSeq a b) = do
  v' <- evalCallTree v a
  evalCallTree v' b
evalCallTree m (TCCond _ []) = evalError $ printf "Found no matching conditions in evalTree for val %s (val type %s)" (show m) (show $ getValPartialType m)
evalCallTree m (TCCond resType ((cond, ifThenTree):restIfTrees)) = do
  -- The value's partial is read as the 'TCMatch' branch above reads it: a closure argument's slot
  -- holds the closure's input object, as the arm's object's higher-order slot does.
  -- Membership with the arm's value predicates evaluated on the value ('valMatchesArm'); the
  -- arm's guard, which its object's type also carries as a predicate over the arm's arguments,
  -- is evaluated below as the condition it was built as.
  armMatches <- maybe (return True) (valMatchesArm m . getExprType . oaObjExpr . snd) cond
  cond' <- case cond of
    Just _ | not armMatches -> return False
    Nothing -> return True
    Just (ifCondTrees, ifObj) -> do
      Env{evArgs} <- get
      modify $ evalSetArgs $ buildArrArgs (Left ifObj) m
      conds' <- withEvalPush "cond" $ forM ifCondTrees $ \ifCondTree ->
        evalExpr ifCondTree
      modify $ evalSetArgs evArgs
      -- A non-linear pattern (@f(a@x, b@x)@) only applies when its shared positions are equal. That
      -- constraint is not a where-condition; it is checked here from the @ifObj@, comparing the
      -- relevant input fields through the argument type's 'Eq' (see 'evalNonLinearEq').
      nlOk <- withEvalPush "nonLinear" $ evalNonLinearEq m ifObj
      return $ all (true ==) conds' && nlOk
  if cond'
    then withEvalPush ("then for " ++ show cond) $ evalCallTree m ifThenTree
    else withEvalPush ("else for " ++ show cond) $ evalCallTree m (TCCond resType restIfTrees)
-- A value held to a type it is within by its shape but whose value predicates the type layer
-- could not decide: it passes when they hold of it, evaluated here ('valWithinType'), and is a
-- failed run otherwise. The static decision stands where it was made: a value decided outside the
-- type is an error at the build, not a check left to here.
evalCallTree v (TCRefine t) = do
  ok <- withEvalPush (printf "refinement %s" (show t)) $ valMatchesArm v t
  if ok
    then return v
    else evalError $ printf "The value %s is outside the refinement %s" (show v) (show t)
evalCallTree input (TCArg _ name) = do
  Env{evArgs} <- get
  case H.lookup name evArgs of
    Just (ObjArrVal oa captured) -> evalObjArrCaptured captured input (Right oa)
    Just arg' -> return arg'
    Nothing -> evalError $ printf "Unknown arg %s found during evaluation \n\t\t with arg env %s" name (show evArgs)
evalCallTree input (TCObjArr oa) = evalObjArr input (Left oa)
evalCallTree input (TCPrim oa (EPrim _ f)) = do
  case primInputArgs input of
    Just args -> case f args of
      Right val -> return val
      Left err  -> evalError err
    _                 -> evalError $ printf "Unexpected eval PrimArrow input %s to %s" (show input) (show oa)
evalCallTree input (TCPrim oa (EEnvPrim _ f)) = do
  env <- get
  case primInputArgs input of
    Just args -> case f env args of
      Right val -> return val
      Left err  -> evalError err
    _                 -> evalError $ printf "Unexpected eval PrimArrow input %s to %s" (show input) (show oa)
-- An effect, which only an IO value can ask for: the primitive is handed the handle of the IO value
-- in the context it was called in, and a call in anything else is refused rather than given some
-- other handle.
evalCallTree input (TCPrim oa (EIOPrim name f)) = do
  case primInputArgs input of
    Just args -> case H.lookup ioStr args of
      Just (IOVal _ handle) -> do
        res <- liftIO $ f handle args
        case res of
          Right val -> return val
          Left err  -> evalError err
      _ -> evalError $ printf "The IO primitive %s can only be called in an io context, but was called with %s" name (show args)
    _                 -> evalError $ printf "Unexpected eval PrimArrow input %s to %s" (show input) (show oa)
evalCallTree input (TCPrim oa (EEvalPrim _ f)) = do
  case primInputArgs input of
    Just args -> f args
    _                 -> evalError $ printf "Unexpected eval PrimArrow input %s to %s" (show input) (show oa)
evalCallTree _ TCMacro{} = evalError $ printf "Can't evaluate a macro - it should be removed during TreeBuild"

-- | Checks the implicit equality of a non-linear pattern's shared argument positions. For an
-- @ifObj@ such as @f(a@x, b@x)@, each non-linear group names input fields (here @a@ and @b@) that
-- must be equal for the clause to apply; this compares the corresponding values of the input tuple
-- @m@ pairwise. Equality goes through the argument type's 'Eq' (the @==@ operator over the two
-- values as constants), not Haskell structural equality, so a type with a custom equality behaves
-- as the user expects. If a value's type has no 'Eq', that surfaces as a build error here.
evalNonLinearEq :: Val -> EObjArr -> EvalM Bool
evalNonLinearEq (TupleVal _ tupleArgs) ifObj = allM id
  [ evalValEq va vb
  | grp <- exprNonLinearArgGroups (oaObjExpr ifObj)
  , ((ea, _), (eb, _)) <- zip grp (tail grp)
  , Just va <- [H.lookup (nameStr (exprPath ea)) tupleArgs]
  , Just vb <- [H.lookup (nameStr (exprPath eb)) tupleArgs]
  ]
evalNonLinearEq _ _ = return True

-- | Whether two runtime values are equal under their type's 'Eq', by building and evaluating
-- @l == r@ over the values as constants.
evalValEq :: Val -> Val -> EvalM Bool
evalValEq v1 v2 = do
  Env{evTbEnv} <- get
  let eqExpr = eVal operatorEq `eApply` (operatorArgL, valToEExpr v1) `eApply` (operatorArgR, valToEExpr v2)
  tree <- liftCRes $ buildRoot evTbEnv eqExpr (getExprPartialType eqExpr) boolType
  (== true) <$> evalExpr tree

-- | Whether a runtime value is within a type, its value predicates evaluated on the value.
--
-- The type layer answers first: a predicate it decided through the graph (a @Z@ within
-- @Nat_isZ@) or one the value's own type carries needs no evaluation. What it leaves undecided
-- is the residue a refinement has at runtime — a predicate bottoming out at a @#runtime@
-- primitive, @>=@ under @Integer_nonNeg@ — and that is evaluated on the value it is stated of
-- ('evalPredExprOn'): the type's own predicates and a leaf's on the value, a slot's on the value
-- in that slot. A value outside the type's shape is outside it whatever the predicates say, and a
-- predicate naming a scope's variable rather than @$this@ (an arm's guard, typed with the arm's
-- arguments) is not evaluated here but where it is written ('TCCond', 'TWhere').
valWithinType :: Val -> Type -> EvalM Bool
valWithinType = valMatchesArm

-- | 'valWithinType' with the value's partial read as the caller reads it for dispatch
-- ('getValDispatchPartial'): as deep as the arms look, in the slot convention they use, which is
-- how a constant in a pattern's slot (@head= '\\'@) is matched. The value in a slot is read the same
-- way against the slot's type ('valMatchesArm'), and only a slot whose type carries a predicate is
-- read at all: the shape was decided whole above.
valWithinTypeAs :: PartialType -> Val -> Type -> EvalM Bool
valWithinTypeAs vp v t = do
  Env{evTypeEnv} <- get
  let within = isSubtypePartialOf evTypeEnv vp
  if within t then return True
  else if not (typeHasPredExpr t) || not (within (typeWithoutPredExprs t)) then return False
  else case t of
    UnionType mTop leafs _ -> do
      topOk <- maybe (return True) (\(preds, _, _) -> atomsHold preds) mTop
      -- The leaves the value's shape is in; none when its shape is a constant of the type or the
      -- top alone admits it.
      let shapeLeafs = [l | l <- splitUnionType leafs, within (singletonType (partialWithoutPredExprs l))]
      leafOk <- if null shapeLeafs then return True else anyM leafHolds shapeLeafs
      return (topOk && leafOk)
    _ -> return False
  where
    atomsHold preds = allM (\e -> (== true) <$> evalPredExprOn v e) (predsValueExprAtoms preds)
    leafHolds l = do
      ok <- atomsHold (ptPreds l)
      if not ok then return False else case v of
        TupleVal _ args -> allM (\(k, slotT) -> maybe (return True) (`valMatchesArm` slotT) (H.lookup (nameStr (pkName k)) args)) [ (k, slotT) | (k, slotT) <- H.toList (ptArgs l), typeHasPredExpr slotT ]
        _               -> return True

-- | Whether a value is within an arm's object type (or a destination spelled like one), read as
-- dispatch reads it: as deep as the type's pattern ('getValDispatchPartial'), with higher-order
-- argument keys reduced to their names ('simplifyArgKeys') — and its value predicates evaluated
-- ('valWithinTypeAs').
valMatchesArm :: Val -> Type -> EvalM Bool
valMatchesArm v t0 = do
  Env{evTypeEnv} <- get
  let t = simplifyArgKeys t0
      depth = 1 + typePatternDepth t
      valP = getSingleton $ simplifyArgKeys $ singletonType $ getValDispatchPartial evTypeEnv [t] depth v
  valWithinTypeAs valP v t

-- | A value predicate evaluated on a value: the 'PredExpr' partial reduced to what it comes to with
-- @$this@ standing for the value — the desugaring of @Integer_nonNeg@'s @$this.nonNeg == True@ is
-- @operator==(r=True, l=nonNeg(this=$this))@. A nullary partial is that value; an application has
-- its operands reduced first, bottom-up, and is then dispatched through the tree builder and the
-- evaluator to its declared result, which runs the @#runtime@ primitives a refinement bottoms out
-- at. The same reduction the type graph performs over types ('typeGraphPredExprHolds'), on a value.
evalPredExprOn :: Val -> PartialType -> EvalM Val
evalPredExprOn thisVal p
  | nameStr (ptName p) == macroThisKey = return thisVal
  | H.null (ptArgs p) = do
      nm <- resolveName (ptName p)
      return $ TupleVal (nameStr nm) H.empty
  | otherwise = do
      argVals <- mapM (\(k, at) -> (nameStr (pkName k),) <$> operand at) (H.toList (ptArgs p))
      nm <- resolveName (ptName p)
      Env{evTbEnv} <- get
      dest <- funcRetDest nm
      let input = TupleVal (nameStr nm) (H.fromList argVals)
      let inputE = valToEExpr input
      tree <- liftCRes $ buildRoot evTbEnv inputE (getExprPartialType inputE) dest
      withEvalPush (printf "predicate %s" (show p)) $ evalExpr tree
  where
    -- An operand is a constant, the @$this@ placeholder, or a nested application carried as a
    -- leaf or as the relation/expression top the condition's spelling gives a method call.
    operand t = case t of
      UnionType Nothing _ (cst:_)                        -> return (constToVal cst)
      UnionType (Just (PredsOne (PredRel q), _, _)) _ _  -> evalPredExprOn thisVal q
      UnionType (Just (PredsOne (PredExpr q), _, _)) _ _ -> evalPredExprOn thisVal q
      _ -> case maybeGetSingleton t of
        Just q  -> evalPredExprOn thisVal q
        Nothing -> evalError $ printf "Unsupported refinement predicate operand %s" (show t)
    constToVal (CInt n)   = IntVal n
    constToVal (CFloat f) = FloatVal f
    constToVal (CStr s)   = StrVal s
    constToVal (CChar ch) = CharVal ch
    -- The predicate keeps the spelling it was written in (@nonNeg@); the object map is keyed by
    -- absolute name.
    resolveName n = do
      Env{evObjMap=ObjectMap om} <- get
      return $ case filter (relativeNameMatches n) (H.keys om) of
        (k:_) -> k
        []    -> n
    -- The destination one application is built toward: the function's NOMINAL declared result,
    -- expanded to its data leaves (@Boolean@ → @{True, False}@) — not the type stored on a
    -- body-carrying definition, which keeps the body's own unevaluated application as a leaf the
    -- input would already be within, leaving it unevaluated.
    funcRetDest nm = do
      Env{evObjMap=ObjectMap om, evTypeEnv} <- get
      let declared = case H.lookup nm om of
            Just o -> case [getMetaType m | d <- omiDecls o ++ omiRuntimeDecls o ++ omiDefs o, Just (_, m) <- [oaArr d]] of
              (t:_) -> t
              []    -> boolType
            Nothing -> boolType
      return $ case declared of
        UnionType (Just (preds, neg, ncs)) _ cs ->
          UnionType Nothing (expandTypeWithNames evTypeEnv H.empty (UnionType (Just (preds, neg, ncs)) H.empty cs) (teNames evTypeEnv)) []
        t -> clearUnionTypePreds t

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f = foldr (\x acc -> f x >>= \b -> if b then acc else return False) (return True)

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM f = foldr (\x acc -> f x >>= \b -> if b then return True else acc) (return False)

evalExpr :: TExpr EvalMetaDat -> EvalM Val
-- evalExpr e | trace (printf "eval %s" (show e)) False = undefined
evalExpr (TCExpr _ v) = return v
evalExpr (TValue m _) = do
  let tp = getMetaType m
  let PartialType{ptName} = case maybeGetSingleton tp of
        Just p -> p
        Nothing -> case tp of
          UnionType Nothing leafs [] -> head $ splitUnionType leafs
          _ -> error $ printf "TValue with unexpected type %s" (show tp)
  return $ TupleVal (nameStr ptName) H.empty
evalExpr (THoleExpr m h) = liftCRes $ CErr [MkCNote $ GenCErr (getMetaPos m) $ printf "Can't evaluate hole %s" (show h)]
evalExpr (TAliasExpr b _) = evalExpr b
-- A value narrowed by a condition is the value when the condition holds of it, and nothing
-- otherwise: the condition is built toward 'boolType' ('TreeBuild.toTExpr') and evaluated here,
-- in the scope the value is written in, which its own arguments are read from.
evalExpr (TWhere _ b c) = do
  v <- evalExpr b
  c' <- withEvalPush (printf "where %s" (show c)) $ evalExpr c
  if c' == true
    then return v
    else evalError $ printf "The value %s does not satisfy its condition %s" (show v) (show c)
-- An argument applied without a value is given by the argument of the evaluation of its name, as
-- the @io@ of a run is, and the page a file processor is given ('Process.processCall').
evalExpr (TTupleApply _ (_, b) (EAppArg oa@ObjArr{oaObj=Just (TValue _ argName), oaArr=Just (Nothing, _)})) = do
  Env{evArgs} <- get
  TupleVal n args <- evalExpr b
  case H.lookup (nameStr argName) evArgs of
    Just v  -> return $ TupleVal n (H.insert (nameStr (oaObjPath oa)) v args)
    Nothing -> error $ printf "evalExpr with no %s" (nameStr argName)
evalExpr (TTupleApply _ (_, b) (EAppVar{})) = evalExpr b
evalExpr (TTupleApply m (_, b) (EAppArg oa)) = do
  (TupleVal n args) <- evalExpr b
  v <- case oaArr oa of
    Just (Just oaExpr, _) -> case oaObj oa of
      Just TValue{} -> evalExpr oaExpr
      Just TTupleApply{} -> do
        Env{evArgs=creationArgs} <- get
        oa' <- closureOutInstantiated oa
        return $ ObjArrVal oa' creationArgs
      _ -> error $ printf "Unsupported eval argument of %s" (show oa)
    Just (Nothing, _) -> error $ printf "Missing arrExpr in evalExpr TupleApply with %s - %s" (show b) (show oa)
    Nothing -> error $ printf "Missing arrExpr in evalExpr TupleApply with %s - %s" (show b) (show oa)
  -- A bare relative value's meta can be a NAME-AMBIGUITY union (@insert@ matching both
  -- /Data/Map/insert and list.ct's nested /sorted./Data/List/insert — no args at the bare
  -- value, so nothing refutes a candidate there) and its eval picks an arbitrary leaf. The
  -- APPLICATION's meta is disambiguated (an applied arg the name's objects don't declare
  -- prunes the candidate, 'addArgToType'), so each application level re-keys the tuple by
  -- its own resolved name; a still-ambiguous or exotic meta keeps the base's name.
  let n' = case uniqueLeafName (getMetaType m) of
        Just nm -> nameStr nm
        Nothing -> n
  return $ TupleVal n' (H.insert (nameStr (oaObjPath oa)) v args)
-- A value spread into a call gives the call each of its arguments, over any the base already has
-- (it is written after them), as each argument written after it does over its own.
evalExpr (TTupleApply _ (_, b) (EAppSpread a)) = do
  TupleVal n args <- evalExpr b
  spread <- evalExpr a
  case spread of
    TupleVal _ spreadArgs -> return $ TupleVal n (H.union spreadArgs args)
    _ -> error $ printf "A spread must be a value with arguments, but %s is %s" (show a) (show spread)
evalExpr (TCalls m b callTree) = do
  b' <- evalExpr b
  -- Marked after the base, whose own expressions are recorded as themselves, so the steps are the
  -- calls this expression's own call tree made (see 'EvalExprRun').
  mark <- gets evalTreebugMark
  v <- evalCallTree b' callTree
  -- By what the expression was written as: the tree's own meta is the destination's.
  let ids = getMetaId (getExprMeta b) : maybe [] (pure . getMetaId . snd) (maybeExprPathM b)
  modify $ evalRecordExpr mark (getMetaPos m <|> getMetaPos (getExprMeta b)) ids v
  return v

-- | An 'Env' with nothing of the pipeline around it: no other stage of the program, and no service
-- to ask for one.
--
-- This is also what every evaluation the service starts on a primitive's behalf is built with, which
-- is what keeps such an evaluation from asking the service for another
-- ('Eval.Common.evService'). So it is not only the environment of a caller that has no service to
-- give, it is the environment that says an evaluation is already inside one.
evalBaseEnv :: EPrgm -> Env
evalBaseEnv = evalBaseEnvService Nothing

-- | An 'Env' that can also report on the program beyond what the evaluation itself holds: the
-- compilation service it was evaluated by, and the other pipeline stages of its program. Callers
-- that have them pass them here; 'evalBaseEnv' is the same thing for callers that do not, and
-- reports them as unavailable rather than substituting what the evaluation does hold.
evalBaseEnvService :: Maybe EvalService -> EPrgm -> Env
evalBaseEnvService = evalBaseEnvWith H.empty

-- | An 'Env' that can also answer for the pages its program was read from, as they were written
-- ('evPages'), which is what a call asking for the page it is written in is answered from.
evalBaseEnvWith :: EvalPages -> Maybe EvalService -> EPrgm -> Env
evalBaseEnvWith pages service prgm@Prgm{prgmObjMap} = Env {
        evObjMap = prgmObjMap,
        evTypeEnv = (mkTypeEnv prgm){teArgModeProjection=True},
        evArgs = H.empty,
        evExEnv = H.empty,
        evTbEnv = buildTBEnv (primEnv <> evalPrims) prgm,
        evClosureEnvs = H.empty,
        evService = service,
        evAnnotObj = Nothing,
        evPages = pages,
        evShowingPages = S.empty,
        evCallStack = [],
        evCoverage = H.empty,
        evTreebugOpen = [],
        evTreebugClaimed = S.empty,
        evTreebugClosed = [],
        evTreebugNextId = 0,
        evDebugs = [],
        evExprRuns = [],
        evAnnotRuns = []
                }

-- | The primitives an evaluation supplies itself, because answering them evaluates more of the
-- program ('EEvalPrim'): they are written over the evaluator, which the other primitives are beneath.
evalPrims :: ResBuildPrims
evalPrims = H.fromList [(mkName (ePrimName p), Left p) | p <- [llmShowPage, parseExprPrim]]

-- | Backs @\/Llm\/showPage@: a page as it was written, with each of its annotations a language model
-- can be shown given what it is shown as ('llmshowClass'), and what is wrong with the page written
-- where it is wrong, formatted as Catln source.
--
-- The page's annotations are found in the program being evaluated by their meta, which the page and
-- the program it was compiled into share. Each is shown in an evaluation of its own, as a page's
-- annotations are for its reader ('evalAnnots'): one that fails is written as it was, with what it
-- failed with under it, rather than failing the rest. A print with a golden is checked as its test
-- checks it ('checkPrintGolden'), and is written as it was, with the golden it no longer prints
-- noted if it fails. What the compiler said about the page is noted where it said it
-- ('Syntax.Ct.Results.notePage').
--
-- An annotation of the page that asks for the page to be shown again would be shown as part of
-- answering itself, so the page is refused while it is being shown ('evShowingPages'), and that
-- annotation is noted as refused.
llmShowPage :: EPrim
llmShowPage = EEvalPrim "llmShowPage" prim
  where
    prim args = case H.lookup "/page" args of
      Just pageVal -> case fromCt pageVal of
        Right page -> showPage page
        Left err -> evalError $ printf "llmShowPage was given %s, which is not a page: %s" (show pageVal) err
      Nothing -> evalError $ printf "llmShowPage has no /page argument, only %s" (show $ H.keys args)

    showPage :: RawPrgm () -> EvalM Val
    showPage page = do
      env@Env{evTbEnv=TBEnv{tbPrgm}, evShowingPages, evPages} <- get
      let pageHash = hash page
      when (S.member pageHash evShowingPages) $
        evalError "llmShowPage was asked to show a page while showing it, which only the page being shown can be answered by"
      let env' = env{evArgs = H.empty, evCallStack = [], evShowingPages = S.insert pageHash evShowingPages}
      let annotId = getMetaId . getExprMeta
      shown <- forM (pageAnnots page tbPrgm) $ \(os, a) -> fmap (annotId a,) <$> liftIO (showAnnot env' os a)
      let notes = case pageFile page >>= (`H.lookup` evPages) of
            Just EvalPage{epNotes} -> epNotes
            Nothing                -> []
      return $ StrVal $ formatRootPrgm $ notePage notes $ showAnnotResults (H.fromList $ catMaybes shown) page

    showAnnot :: Env -> [(PartialType, EObjArr)] -> EExpr -> IO (Maybe AnnotShown)
    showAnnot env os a
      | isAnnot printAnnot a, Just _ <- printResultArg a = do
          res <- runCResT $ evalDShowAnnot env os a
          return $ case res of
            CErr notes -> Just $ ShownFailed notes
            CRes _ (Just (StrVal actual)) -> case checkPrintGolden a actual of
              CErr notes -> Just $ ShownFailed notes
              CRes{}     -> Nothing
            CRes{} -> Nothing
      | otherwise = do
          res <- runCResT $ evalShowAnnot llmshowClass env os a
          return $ case res of
            CErr notes                  -> Just $ ShownFailed notes
            CRes _ (Just (StrVal text)) -> Just $ Shown text
            CRes{}                      -> Nothing

    -- The page a page was read from is the one its statements say they were written in.
    pageFile (RawPrgm _ trees) = listToMaybe [sourceName start | tree <- trees, Just start <- [treeStart tree]]
    treeStart (RawStatementTree st subs) = statementStart st <|> asum (map treeStart subs)

-- | Backs @\/Catln\/parseExprIn@: the value of a Catln expression written as text, required to be a
-- value of a type, with the names in it meaning what they mean on the page @source@.
--
-- The text is read by the parser a page's expressions are read with ('parseExprSource'), and
-- compiled as the one definition of a page importing @source@, declared to answer the type
-- ('parsedDecl'), so typechecking it is what says whether it is one. The value is then evaluated
-- in an evaluation of its own over that program, and checked against the type once it is a value
-- ('valWithinType'), which is what decides a refinement the typecheck left to evaluation.
--
-- Each way the text can fail to be a value of the type is answered as a @\/Catln\/ParseExprErr@: not
-- an expression, not typechecking as the type, failing to evaluate, or evaluating to something else.
-- What is wrong with the CALL rather than the text is an error of the evaluation: a page that is
-- not one the program was read from, or whose scope did not typecheck.
parseExprPrim :: EPrim
parseExprPrim = EEvalPrim "parseExpr" prim
  where
    parsedName = "parsedExpr"
    prim args = case (H.lookup "/input" args, H.lookup "/type" args, H.lookup "/source" args) of
      (Just (StrVal input), Just tpVal, Just (StrVal source)) -> case fromCt tpVal of
        Right tp -> parseAs input tp source
        Left err -> evalError $ printf "parseExpr was given %s, which is not a type: %s" (show tpVal) err
      _ -> evalError $ printf "parseExpr takes the text /input, the /type and the page /source, and was given %s" (show $ H.keys args)

    parseAs :: String -> Type -> String -> EvalM Val
    parseAs input tp source = do
      Env{evPages} <- get
      scope <- case H.lookup source evPages of
        Just EvalPage{epScope} -> asCResT' epScope
        Nothing -> evalError $ printf "parseExpr was asked to read text as written on %s, which is not a page the program was read from" source
      case parseExprSource "<parseExpr>" input of
        Left err -> return $ failed ctParseExprErrSyntax [(ctParseExprErrMessage, StrVal err)]
        Right parsed -> case compileStatementsIn "<parseExpr>" scope [parsedDecl parsed] (return . declaredAs tp) of
          CErr notes -> return $ failed ctParseExprErrType [(ctParseExprErrMessage, StrVal $ prettyCNotes notes)]
          CRes _ (_, prgm) -> do
            res <- liftIO $ runCResT $ evalParsed prgm tp
            case res of
              CErr notes -> return $ failed ctParseExprErrEval [(ctParseExprErrMessage, StrVal $ prettyCNotes notes)]
              CRes _ (val, True) -> return $ resultVal $ Right val
              CRes _ (val, False) -> return $ failed ctParseExprErrNotOf [(ctParseExprErrFound, val)]

    asCResT' (CRes _ r)  = return r
    asCResT' (CErr notes) = evalError $ printf "parseExpr reads text against the pages its page reaches, which did not all typecheck:\n%s" (prettyCNotes notes)

    failed name fields = resultVal $ Left $ TupleVal name (H.fromList [(runtimeArgKey k, v) | (k, v) <- fields])

    -- The one statement of the page: the parsed expression as the definition of 'parsedName'.
    parsedDecl parsed = RawStatementTree (RawDeclStatement (RawObjArr (Just $ rawVal parsedName) FunctionObj Nothing [] (Just (Just parsed, Nothing)) Nothing)) []

    -- The definition declared to answer the type, as though the page also wrote @parsedExpr -> T@.
    -- The declaration is what the typecheck holds the definition to: a definition answering more
    -- than its declaration is an error ('ResultNotGuaranteed'), and that is the text not being a
    -- value of the type.
    declaredAs tp prgm@Prgm{prgmObjMap=ObjectMap items} = prgm{prgmObjMap=ObjectMap $ H.mapWithKey declareItem items}
      where
        declareItem name item@ObjectMapItem{omiDefs}
          | nameStr name == '/' : parsedName = item{omiDecls=[declOf oa | oa@ObjArr{oaArr=Just _} <- omiDefs]}
          | otherwise = item
        declOf oa@ObjArr{oaArr=Just (_, m)} = oa{oaArr=Just (Nothing, mWithType tp m)}
        declOf oa = oa

    evalParsed prgm tp = do
      let env@Env{evTbEnv} = evalBaseEnvWith H.empty Nothing prgm
      tree <- asCResT $ toTExprDest evTbEnv [] (eVal ('/' : parsedName)) (emptyMetaT tp)
      evalStateT (do
        val <- evalExpr tree
        within <- valWithinType val tp
        return (val, within)) env

-- | The meta ID of each annotation a page writes, at whatever depth.
rawPrgmAnnotIds :: RawPrgm m -> [UUID]
rawPrgmAnnotIds (RawPrgm _ trees) = concatMap treeIds trees
  where
    treeIds (RawStatementTree st subs) = stIds st ++ concatMap treeIds subs
    stIds (RawAnnot a) = [getMetaId $ getExprMeta a]
    stIds _            = []

-- | The annotations of a program written on a page, each with the object it is written on ('os'),
-- found by the meta the page and the program it was compiled into share.
pageAnnots :: RawPrgm m -> EPrgm -> [([(PartialType, EObjArr)], EExpr)]
pageAnnots page (Prgm objMap _ globalAnnots) =
  [([], a) | a <- globalAnnots, S.member (annotId a) written]
  ++ [ (os, a)
     | oa <- flatObjectMap objMap
     , let os = [(getExprPartialType $ oaObjExpr oa, oa)]
     , a <- oaAnnots oa
     , S.member (annotId a) written ]
  where
    written = S.fromList $ rawPrgmAnnotIds page
    annotId = getMetaId . getExprMeta

-- | Tries to TreeBuild all ObjArrs, returning all built successfully
evalBuildAll :: EPrgmGraphData -> GraphData (CRes (Prgm TExpr EvalMetaDat)) FileImport
evalBuildAll = evalBuildAllOf (const True)

-- | 'evalBuildAll' restricted to the pages a caller is actually going to read back.
--
-- A page the selector rejects keeps its place in the graph — the selected pages' eval environments
-- are assembled from the whole reachable set ('prgmFromGraphData') — but its own object map is
-- returned EMPTY rather than built. Its arrow trees were built when its own fragment was solved and
-- are cached in that page's 'ssfTBPrgm'; building them again for every dependent fragment is work
-- whose result is dropped on the floor, since only the fragment's own names are ever looked up in
-- the result. That made a whole-build quadratic in the number of pages: each of the ~50 integration
-- files rebuilt all ~20 standard-library pages' trees before building its own.
--
-- Each page's build is a result of its own, so what one page could not build is reported on that
-- page rather than on every page built beside it.
evalBuildAllOf :: (FileImport -> Bool) -> EPrgmGraphData -> GraphData (CRes (Prgm TExpr EvalMetaDat)) FileImport
evalBuildAllOf selected prgmGraphData = graphFromEdges $ map buildPage $ graphToNodes prgmGraphData
  where
    buildPage (Prgm objMap cg annots, prgmName, deps) = (, prgmName, deps) $
      if not (selected prgmName)
        then return $ Prgm (objectMapFromList []) cg []
        else do
          evalPrgm <- prgmFromGraphData prgmName prgmGraphData
          let Env{evTbEnv} = evalBaseEnv evalPrgm
          objMap' <- buildReporting (printf "The definition %s" . show . oaObjPath) (getMetaPos . getExprMeta . oaObjExpr) (buildRootOA evTbEnv) (flatObjectMap objMap)
          annots' <- buildReporting (printf "The annotation %s" . maybe "" show . maybeExprPath) (getMetaPos . getExprMeta) (toTExpr evTbEnv []) annots
          return $ Prgm (objectMapFromList objMap') cg annots'

-- | Builds each of a page's items, leaving out the ones that cannot be built and saying so.
--
-- One item that fails to build does not take the page with it, because the rest of the page can
-- still run. But what is left out is reported as a warning naming it and the reason. Dropping it
-- without a word hid failures: the generic @++@ declaration of @core\/algebra.ct@ once stopped
-- building, and the only sign was that it went missing from a tree-build golden.
buildReporting :: (a -> String) -> (a -> CodeRange) -> (a -> CRes b) -> [a] -> CRes [b]
buildReporting name pos build = fmap catMaybes . mapM buildOne
  where
    buildOne x = case build x of
      CErr errs -> CRes [MkCNote $ GenCNote (pos x) $ printf "%s could not be built, so it is left out of the built program:%s" (name x) (prettyCNotes errs)] Nothing
      res -> Just <$> res

evalBuildPrgm :: EExpr -> PartialType -> Type -> EPrgm -> CRes (TExpr EvalMetaDat, Env)
evalBuildPrgm input srcType destType prgm = do
  let env@Env{evTbEnv} = evalBaseEnv prgm
  initTree <- buildRoot evTbEnv input srcType destType
  return (initTree, env)

-- | The expression of a @#print@'s @p@ argument, if present.
printPArg :: EExpr -> Maybe EExpr
printPArg annot = case H.lookup (partialKey (mkName printAnnotText)) (exprAppliedArgsMap annot) of
  Just (Just (_, Just e)) -> Just e
  _                       -> Nothing

-- | The stored golden string of a @#print@'s @result@ argument, if present.
printResultArg :: EExpr -> Maybe String
printResultArg annot = case H.lookup (partialKey (mkName resultAnnotText)) (exprAppliedArgsMap annot) of
  Just (Just (_, Just (CExpr _ (CStr s)))) -> Just s
  _                                        -> Nothing

-- | The source range of a @#print@ (taken from its @p@ argument, the only positioned part).
printRange :: EExpr -> CodeRange
printRange annot = maybe Nothing (getMetaPos . getExprMeta) (printPArg annot)

-- | Whether a @#print@ still prints what it was written to: its @result@ golden is what its value is
-- now shown as ('evalDShowAnnot'). One with no golden has nothing to differ from.
--
-- Shared by the test a print is checked by and by a page shown with what is wrong with it
-- ('llmShowPage'), so that the two cannot disagree on whether a print fails.
checkPrintGolden :: EExpr -> String -> CRes ()
checkPrintGolden annot actual = case printResultArg annot of
  Just golden | golden /= actual -> CErr [MkCNote $ GenCErr (printRange annot) $ printf "Print golden mismatch:\n  expected: %s\n  actual:   %s" golden actual]
  _ -> return ()

-- | A class values are shown to some reader through, and the function showing a value as one of it.
--
-- There is one per reader, because what a reader can take differs: the webdocs show a person any
-- value their page has a view of, and a language model reads text. A type may be shown each way
-- differently, and both readers share the text of a @ToString@.
data ShowClass = ShowClass {
  scClass :: String,
  scFun   :: String,
  scArg   :: String,
  -- | Whether what is shown is text and nothing else. A display that is not text is no display
  -- at all to such a reader, and a print is shown through 'scFun' like any value rather than as its
  -- value, since the print's own contract ('dshowClass') is not the reader's.
  scText  :: Bool
  }

-- | What the webdocs show a person: 'dshowClassName'.
dshowClass :: ShowClass
dshowClass = ShowClass dshowClassName dshowName dshowArg False

-- | What a language model is shown: 'llmshowClassName'.
llmshowClass :: ShowClass
llmshowClass = ShowClass llmshowClassName llmshowName llmshowArg True

-- | Build and evaluate a single annotation expression to the value it renders to through the
-- @DShow@ class (the same path used to display @#print@ values in webdocs). Returns @Nothing@
-- when the annotation cannot reach @DShow@ and so is not renderable.
evalDShowAnnot :: Env -> [(PartialType, EObjArr)] -> EExpr -> CResT IO (Maybe Val)
evalDShowAnnot = evalShowAnnot dshowClass

-- | 'evalDShowAnnot' for the reader of a 'ShowClass': the annotation shown through its class.
-- @Nothing@ when it cannot be, which for a reader of text ('scText') is also when what it is shown
-- as is not text.
evalShowAnnot :: ShowClass -> Env -> [(PartialType, EObjArr)] -> EExpr -> CResT IO (Maybe Val)
evalShowAnnot sc env0@Env{evTbEnv, evTypeEnv} os annot0
  | isPrint && scText sc = do
      pVal <- evalToValue annot
      (shown, _) <- lift $ showValAs sc env pVal
      return $ onlyText shown
  | isPrint = do
      -- A @#print@ displays a runtime VALUE. Evaluate @p@ to its concrete value (built toward
      -- @DShow@, which certifies displayability and yields the value itself), then render THAT
      -- value as text. Driving the render from the evaluated value — not @p@'s statically-inferred
      -- type — is what lets a computed or interface-typed result print. @sorted([3,1,2])@ has
      -- static type @/Data/List@ (the interface, which is not a @ToString@) and, being a function
      -- call, cannot dispatch @toString@ on its own name; but its value is a concrete @ConsList@ of
      -- @Integer@s, which renders as @[1, 2, 3]@. Building @toString(this= p)@ from @p@'s static
      -- type instead only ever worked for string/scalar/literal @p@ and silently skipped the rest.
      pVal <- evalToValue annot
      -- Render the value as a @String@ through @/Data/toString@ when it is @ToString@ (the only
      -- form a golden can check); a @DShow@ value that is not @ToString@ (a webdocs visualization)
      -- keeps its raw value, so it is displayed but not golden-checked.
      text <- lift $ renderValText env pVal
      return $ Just $ fromMaybe pVal text
  -- Whether THIS program can display the annotation: a question about the program as written,
  -- whose classes have exactly the members it declares — the closed-world view TreeBuild then
  -- builds the display in ('tePrgmEnv'). Asked open-world, every annotation could still become a
  -- @DShow@ member in some later file, so each one would be sent to a build with no arm to take.
  | reachesTo evTypeEnv{tePrgmEnv=True} H.empty (getExprType $ showAnnot annot) (showDestType sc) = do
      tree <- asCResT $ toTExprDest evTbEnv os (showAnnot annot) (emptyMetaT $ showDestType sc)
      val <- evalStateT (evalExpr tree) env
      if scText sc
        then onlyText <$> lift (renderValText env val)
        else return (Just val)
  | otherwise = return Nothing
  where
    -- The annotation is evaluated knowing what it is written on, which the expression built from it
    -- no longer says. A primitive reporting on the annotated definition — a @#test@ showing that
    -- test's run — has no other way to know which definition it is being asked about.
    env = env0{evAnnotObj = listToMaybe (map snd os)}
    isPrint = nameStr (exprPath annot0) == printAnnot
    -- A #print's rendered value is its `p` argument, so render that directly. This is robust to
    -- the print also carrying a `result` golden (the 2-arg #print), which the bare
    -- @dshow(s= #print(p))= p@ rule would not match.
    annot
      | isPrint
      , Just (Just (_, Just p)) <- H.lookup (partialKey (mkName printAnnotText)) (exprAppliedArgsMap annot0)
      = p
      | otherwise = annot0
    showAnnot a = eVal (scFun sc) `eApply ` ('/' : scArg sc, a)
    -- Evaluate @p@ to its concrete value by building it toward @DShow@ (which certifies
    -- displayability and yields the value itself). @p@ is declared @-> DShow@, so this resolves for
    -- every valid print, whichever reader it is being shown to.
    evalToValue a = do
      tree <- asCResT $ toTExprDest evTbEnv os a (emptyMetaT dshowDestType)
      evalStateT (evalExpr tree) env
    onlyText v@(Just StrVal{}) = v
    onlyText _                 = Nothing

-- | A value rendered as text through @/Data/toString@, when it is @ToString@: what a @#print@ of it
-- shows, and the only form a golden can check.
--
-- @toString@ is dispatched on the value itself ('valCallTree'), so its concrete type is what
-- dispatches: a value an interface-typed expression computed renders by what it is — a @ConsList@ of
-- @Integer@s — rather than by what the expression said it would be. 'Nothing' for a runtime-only
-- value ('displayableVal') and for one that is not @ToString@.
renderValText :: Env -> Val -> IO (Maybe Val)
renderValText env v = fst <$> renderValTextIn env v

-- | 'renderValText', also handing back the arrows the evaluation built ('evExEnv'), so that rendering
-- many values can share them ('showRunValues').
renderValTextIn :: Env -> Val -> IO (Maybe Val, ResExEnv)
renderValTextIn env@Env{evExEnv} v
  | displayableVal v = do
      called <- callOnVal env dshowDestType (TupleVal "/Data/toString" (H.singleton "/this" v))
      return $ case called of
        Just (str@StrVal{}, built) -> (Just str, built)
        _                          -> (Nothing, evExEnv)
  | otherwise = return (Nothing, evExEnv)

-- | A value's display: what @/Catln/Doc/dshow@ makes of it, and that rendered as text when it is
-- @ToString@ ('renderValText').
--
-- Through @dshow@ rather than straight to text so that a type displays as its library says. A
-- member of @DShow@ is handed back by the identity arm, and so is shown as itself or as its text; a
-- type with an arm of its own is shown as whatever that arm builds, which need not be text at all --
-- a note's @Page@ is a heading of markup. 'Nothing' for a value that reaches no arm of @dshow@, or
-- that is runtime-only ('displayableVal').
--
-- Checked to reach @DShow@ before anything is built, as 'evalDShowAnnot' checks an annotation: a tree
-- built for a value no arm takes is an @error@ in 'TreeBuild' rather than a note.
--
-- The arrows the evaluation built ('evExEnv') are handed back beside it. An arrow is built per type
-- it is entered at, and a recursion over a list enters its arrows at the type of every tail, so a
-- list's rendering builds an arrow per element. The values of one run are largely the same lists
-- over and over -- each tail a recursion was called with, and each list it built on the way back --
-- and the tails of a list are the tails of every list it is a tail of, so 'showRunValues' carries
-- these from one value to the next rather than building each tail's arrows again for every list it
-- ends.
dshowVal :: Env -> Val -> IO (Maybe Val, ResExEnv)
dshowVal = showValAs dshowClass

-- | 'dshowVal' for the reader of a 'ShowClass': what its function makes of a value, and that as text
-- when it is @ToString@.
showValAs :: ShowClass -> Env -> Val -> IO (Maybe Val, ResExEnv)
showValAs sc env@Env{evTypeEnv, evExEnv} v
  | displayableVal v && reachesTo evTypeEnv H.empty (singletonType $ displayPartial evTypeEnv input) (showDestType sc) = do
      called <- callOnVal env (showDestType sc) input
      case called of
        Just (shown, built) -> do
          (text, built') <- renderValTextIn env{evExEnv=built} shown
          return (Just $ fromMaybe shown text, built')
        Nothing -> return (Nothing, evExEnv)
  | otherwise = return (Nothing, evExEnv)
  where input = TupleVal (scFun sc) (H.singleton ('/' : scArg sc) v)

-- | A call of a function on runtime values, evaluated toward a display class ('showDestType'): the
-- call's tree built from the value's own type ('valCallTree') and run on it, with the arrows the run
-- built.
--
-- Built from the value rather than from an expression rebuilt out of it ('valToEExpr'). A rebuilt
-- expression is compiled whole, one level of the tree per level of the value, and a list of forty
-- elements is forty nested constructions compiled again for every list a debugger shows -- nearly
-- all of what rendering a run cost. The value's type says the same thing and is dispatched on once.
callOnVal :: Env -> Type -> Val -> IO (Maybe (Val, ResExEnv))
callOnVal env@Env{evTbEnv} destType input = case valCallTree evTbEnv destType input of
  Nothing -> return Nothing
  Just tree -> do
    res <- runCResT $ runStateT (evalCallTree input tree) env
    return $ do
      (val, Env{evExEnv}) <- cresToMaybe res
      return (val, evExEnv)

-- | The call tree a runtime value takes toward a display class, built where the closures the value
-- holds are arrows ('valClosureArrows'): a call display's @fmap@ reaches what its function returns
-- through the closure's arrow, as its higher-order slot holds only the closure's input object.
--
-- 'MustCall' rather than 'MayHold': the point is to reach the arm, and a source that already
-- satisfies the destination would otherwise be handed back as it stands.
valCallTree :: TBEnv -> Type -> Val -> Maybe TCallTree
valCallTree tbEnv@TBEnv{tbTypeEnv} destType input = cresToMaybe $ buildCallTree (withArgArrows tbEnv (valClosureArrows input)) [] S.empty PTopType MustCall DestRequired (singletonType (displayPartial tbTypeEnv input)) destType

-- | A value's type for building its display, read only as deep as the value's own shape: the call
-- the display is asked through (@toString@'s or @dshow@'s argument), the value, and the value's
-- fields. Deeper than that the value is its constructors' declared forms ('getValDispatchPartial').
--
-- Building a display from the whole value asks the tree builder about a type as deep as the value,
-- and a value holding strings is a @Cons@ per character: printing a build result holding a page of
-- HTML did not finish. What the display's arms need deeper than the value's fields is read off the
-- value when the tree runs, by the dispatch that chooses among them ('evalCallTree').
displayPartial :: TypeEnv (ObjArrTypeGraph Expr EvalMetaDat) -> Val -> PartialType
displayPartial typeEnv = getValDispatchPartial typeEnv [] displayDepth
  where displayDepth = 3

-- | Whether a value is data all the way down: no closure, IO handle, or other runtime-only value in
-- it, none of which is @ToString@ or has a display.
displayableVal :: Val -> Bool
displayableVal IntVal{}        = True
displayableVal FloatVal{}      = True
displayableVal CharVal{}       = True
displayableVal (TupleVal _ as) = all displayableVal (H.elems as)
displayableVal _               = False

-- | The class a display is built toward, which is what forces it to be evaluated: a tree built
-- toward an expression's own type comes back as the application itself, unevaluated.
dshowDestType :: Type
dshowDestType = showDestType dshowClass

-- | 'dshowDestType' for the reader of a 'ShowClass'.
showDestType :: ShowClass -> Type
showDestType ShowClass{scClass} = classPartial $ partialVal $ mkName scClass

-- | The functions that have a display of a call: those an arm of @\/Catln\/Doc\/dshowCall@ is
-- written over.
--
-- Read off the arms rather than from anything marking the function, because the arm IS the
-- declaration — a library says one of its functions has a display by writing how it is displayed,
-- the same way it says a type has one. A function named here is one 'attachCallShows' will try; every
-- other call in a run is left alone without anything being built for it, which is what keeps this off
-- the overwhelming majority of an evaluation tree.
callShowFuns :: EPrgm -> S.HashSet TypeName
callShowFuns Prgm{prgmObjMap=ObjectMap objMap} = case H.lookup (mkName dshowCallName) objMap of
  Nothing                     -> S.empty
  Just ObjectMapItem{omiDefs} -> S.fromList $ mapMaybe armFun omiDefs
  where
    -- The call an arm displays is the call written in its @/s@, so the function it is of is that
    -- call's own name.
    armFun oa = do
      obj <- oaObj oa
      case H.lookup (partialKey (mkName dshowCallArg)) (exprAppliedArgsMap obj) of
        Just (Just (_, Just argE)) -> Just $ makeAbsoluteName $ exprPath argE
        _                          -> Nothing

-- | Fill in each call of an evaluation tree that has a display of its own, leaving the rest as they
-- closed.
--
-- This is the one caller of @\/Catln\/Doc\/dshowCall@, and the reason that function exists rather
-- than more arms of @dshow@: the call is in hand here as the runtime tuple the arrow was entered
-- with, so an arm whose pattern is a call is handed a call. Written from a page it would be handed
-- the call's result instead — an argument is evaluated before the callee is entered — and would die
-- destructuring it. See 'CtConstants.dshowCallName'.
--
-- A chain of calls of the SAME function gets one display, on the outermost. A display of a call
-- speaks for the whole call including whatever that call did to reach its answer, and a recursive
-- definition reaches its answer by calling itself: @fmap@ over a three-element list is four nested
-- calls, and four tables each holding a shorter tail of the same one is not four displays but one,
-- shown four times. The inner calls keep their nodes and their children — nothing is dropped from the
-- tree — they simply have no display of their own, and whatever is showing the outermost decides what
-- to do with what is underneath it.
attachCallShows :: EPrgm -> [EvalTreebugClosed] -> CResT IO [EvalTreebugClosed]
attachCallShows prgm nodes
  | S.null funs = return nodes
  | otherwise = mapM (go Nothing) nodes
  where
    funs = callShowFuns prgm
    env@Env{evTbEnv} = evalBaseEnv prgm

    go parentFun (EvalTreebugClosed oa input val children nodeId _) = do
      let fun = either oaObjPath oaObjPath oa
      children' <- mapM (go (Just fun)) children
      showVal <- if S.member fun funs && parentFun /= Just fun
        then buildShow fun input
        else return Nothing
      return $ EvalTreebugClosed oa input val children' nodeId showVal

    -- The call, wrapped as the argument of a @dshowCall@, built toward @DShow@ and run.
    buildShow fun input = do
      let showInput = TupleVal dshowCallName (H.singleton dshowCallArg input)
      case valCallTree evTbEnv dshowDestType showInput of
        -- No arm of the function's display matches THIS call, which is a display saying nothing about
        -- this one rather than a display that broke: the list's @fmap@ display is written for elements
        -- that render text, and a mapping over elements that render none is a mapping it does not
        -- describe. So the call keeps the display every other call gets, and nothing is said about it.
        Nothing -> return Nothing
        Just tree -> do
          res <- lift $ runCResT $ evalStateT (evalCallTree showInput tree) env
          asCResT $ case res of
            -- Built and then failed to run, which is the opposite case and is worth saying: an arm
            -- was found for this call and could not produce what it promised.
            CErr notes -> CRes [MkCNote $ GenCErr Nothing $ printf "The display of a call of %s could not be evaluated: %s" (nameStr fun) (prettyCNotes notes)] Nothing
            CRes _ v -> return $ Just v

-- | Build and evaluate a call to a named function of the program, with arguments supplied as runtime
-- values, to the value it displays as through the @DShow@ class.
--
-- This is what lets the webdocs ask the program to compute something: a function is named and given
-- what to compute over, rather than an expression being written out as source for the compiler to
-- read. The function is one the program already holds, so nothing here parses, desugars or
-- typechecks — the arguments are reconstructed as typed constants ('valToEExpr'), applied, and the
-- call is built and run against the program as it already compiled. That is the same construction a
-- generated test case is run by ('Eval.Testing.runProp').
--
-- The result must reach @DShow@, which is both what makes it displayable and what keeps a call that
-- cannot be built from reaching 'TreeBuild': tree building a call it cannot resolve is an @error@
-- rather than a note, and an @error@ raised answering a request is a failed connection rather than
-- something the reader can be shown.
evalDShowCall :: Env -> String -> [(String, Val)] -> CResT IO Val
evalDShowCall env@Env{evTypeEnv} fun args = do
  fun' <- asCResT $ evalCallName env fun
  let input = foldl (\base (argName, argVal) -> eApply base ('/' : argName, valToEExpr argVal)) (eVal fun') args
  unless (reachesTo evTypeEnv H.empty (getExprType input) dshowDestType) $
    fail $ printf "Calling %s with %s does not reach %s, so it is not something the webdocs can show" fun' (show $ map fst args) dshowClassName
  evalCallTo env dshowDestType fun args

-- | The one function of the program written under this name, or why the name is not one function.
evalCallName :: Env -> String -> CRes String
evalCallName Env{evTypeEnv} fun = case maybeGetSingleton $ expandRelPartial evTypeEnv H.empty (partialVal (mkName fun)) of
  Just f -> return $ nameStr $ ptName f
  -- Both no match and several are reported as the name not being one function, since what the
  -- caller has to do about either is the same: name the one they meant, absolutely.
  Nothing -> fail $ printf "The program holds no single function named %s to call" fun

-- | Build and evaluate a call to a named function of the program, toward the destination given.
--
-- 'evalDShowCall' above is this with @DShow@ as the destination and the check that the call reaches
-- it. Split out because not every caller is a display: an importer written in Catln is called for
-- the program it answers with, which is a thing to be read rather than shown, and a caller that had
-- to make its result displayable in order to be called at all would be a display class deciding what
-- the compiler may run.
--
-- The destination is what makes the call evaluate, and choosing it is the caller's whole
-- responsibility — see 'dshowDestType': a tree built toward the expression's OWN type comes back as
-- the unevaluated application. A caller wanting the value of a function declared to return @T@ must
-- therefore ask for something @T@ resolves to rather than for @T@.
evalCallTo :: Env -> Type -> String -> [(String, Val)] -> CResT IO Val
evalCallTo env@Env{evTbEnv} dest fun args = do
  fun' <- asCResT $ evalCallName env fun
  let input = foldl (\base (argName, argVal) -> eApply base ('/' : argName, valToEExpr argVal)) (eVal fun') args
  tree <- asCResT $ toTExprDest evTbEnv [] input (emptyMetaT dest)
  evalStateT (evalExpr tree) env

-- | Evaluate the annotations a page WRITES, in the program that page COMPILES TO. The two scopes
-- differ: the annotations are the page's own, while the environment they are evaluated in is the
-- page joined with everything it imports, since that is the program they report on.
--
-- Only the page's own annotations, because an annotation belongs to the page it is written in. A
-- reader of one page is shown that page's annotations; core's are shown when core is the page being
-- read. Evaluating the imports' annotations too would compute a whole library's worth of values for
-- every page, and then discard them — @WebDocs.docApiBase@ keys the results back onto the page's own
-- source by meta ID, which no imported annotation can match.
evalAnnots :: EvalPages -> Maybe EvalService -> FileImport -> EPrgmGraphData -> CResT IO [(EExpr, Val)]
evalAnnots pages service prgmName prgmGraphData = do
  prgm <- asCResT $ prgmFromGraphData prgmName prgmGraphData
  Prgm objMap _ globalAnnots <- asCResT $ case graphLookup prgmName prgmGraphData of
    Just own -> return own
    Nothing  -> CErr [MkCNote $ GenCErr Nothing $ printf "Could not find the page %s to evaluate the annotations of" (show $ impRaw prgmName)]
  let env = evalBaseEnvWith pages service prgm
  globalAnnots' <- buildDisplays env [] "annotation" globalAnnots
  objMapAnnots' <- forM (flatObjectMap objMap) $ \oa -> do
    let os = [(getExprPartialType $ oaObjExpr oa, oa)]
    -- TODO Make this build annots within arguments
    annots' <- buildDisplays env os "annotation" (oaAnnots oa)
    -- A declaration of a value displays as the value it declares, which is what shows the task of
    -- an Obsidian note as a task rather than as the syntax the note was read into. Only a value:
    -- 'isValueObjArr' leaves out the declaration of a type, whose arguments are given types rather
    -- than expressions. That distinction is the only guard there is, because 'evalExpr' meets an
    -- argument with no expression with an @error@ rather than a 'CErr' — a failure this cannot
    -- catch and turn into a note.
    own <- if isValueObjArr oa then buildDisplays env os "declaration" [oaObjExpr oa] else return []
    return (annots' ++ own)
  return $ concat (globalAnnots' : objMapAnnots')

-- | Build and evaluate expressions to the values they display as.
--
-- Every expression is evaluated on its own, so a failing one costs only itself: it becomes an error
-- note naming it, and the others are still returned. This is what lets a reader of a page with one
-- broken annotation still be shown the rest of them. The failure is not swallowed — it is reported
-- as an error note, so a consumer that must reject any failure (the integration tests, via
-- 'failOnErrorNotes') still fails on it.
buildDisplays :: Env -> [(PartialType, EObjArr)] -> String -> [EExpr] -> CResT IO [(EExpr, Val)]
buildDisplays env os what exprs = do
    results <- forM exprs $ \e -> do
      res <- lift $ runCResT $ evalDShowAnnot env os e
      asCResT $ case fmap (e,) <$> res of
        CRes notes r -> CRes notes r
        CErr notes   -> CRes (displayFailedNote e : notes) Nothing
    return $ catMaybes results
  where
    displayFailedNote e = MkCNote $ GenCErr (getMetaPos $ getExprMeta e) $ printf "Failed to evaluate the %s %s" what (show e)

-- | Evaluate the values a page DECLARES of one type, in the program that page COMPILES TO. The
-- scopes differ in the same way 'evalAnnots' describes: the declarations are the page's own, while
-- the environment they are evaluated in is the page joined with everything it imports.
--
-- A value is evaluated exactly as it is when the page holding it is read — the value declaration
-- path of 'evalAnnots' — so a value gathered from a page and the same value shown on that page are
-- one value, displayed one way, and edited by replacing one expression.
--
-- Only the types asked for, and nothing a page merely annotates. Gathering a type of value across
-- every page there is asks this of pages that declare none, and evaluating each of them whole would
-- compute a doc server's worth of values to discard all but a few of them.
--
-- Several types at once rather than one at a time, so that what comes back is in the order the page
-- wrote it however many types were asked for. Two gatherings joined would be two runs of that order.
evalPageValues :: [Name] -> FileImport -> EPrgmGraphData -> CResT IO [(EExpr, Val)]
evalPageValues names prgmName prgmGraphData = do
  Prgm objMap _ _ <- asCResT $ case graphLookup prgmName prgmGraphData of
    Just own -> return own
    Nothing  -> CErr [MkCNote $ GenCErr Nothing $ printf "Could not find the page %s to evaluate the declarations of" (show $ impRaw prgmName)]
  case filter declaresOne (flatObjectMap objMap) of
    -- A page that declares none is answered before anything is built, so asking every page of a doc
    -- server about a type most of them never mention costs no evaluation environments.
    [] -> return []
    oas -> do
      prgm <- asCResT $ prgmFromGraphData prgmName prgmGraphData
      -- Neither the stages nor the service: a declared value is the declaration itself and reports
      -- on no other stage of the program. That is also what keeps an annotation which gathers such
      -- values from re-entering the gathering — this evaluation is one the service started, and an
      -- evaluation the service started has no service to ask.
      let env = evalBaseEnv prgm
      concat <$> forM oas (\oa -> buildDisplays env [(getExprPartialType $ oaObjExpr oa, oa)] "declaration" [oaObjExpr oa])
  where
    declaresOne oa = isValueObjArr oa && oaObjPath oa `elem` names

-- | Asks a method of a page's program about a value, and evaluates the answer.
--
-- The companion of 'evalPageValues': that gathers what pages declare, this asks each of the gathered
-- values a question the program knows how to answer. Together they are what lets a display gather a
-- class of value across pages and then read something off each one without knowing what any of them
-- is — the class declares the method, each member answers it, and this runs that answer.
--
-- The value is rebuilt as a typed constant ('valToEExpr') and the method applied to it, which is how
-- 'evalDShowAnnot' renders a computed value through @toString@ and works for the same reason: the
-- receiver is the concrete value, so it dispatches where the static type of whatever produced it
-- could not. It is limited the same way too — a value 'valToEExpr' cannot rebuild (a closure, an IO
-- handle) cannot be asked anything.
--
-- Built toward @DShow@, which is what forces the answer to be evaluated at all: a tree built toward
-- an expression's own type comes back as the unevaluated application. So a method asked through this
-- has to answer with something showable, which is a real constraint on the method and is why
-- @stack\/core\/calendar.ct@ has to say that a @\/Data\/Maybe@ of a showable thing is showable.
--
-- An asker is handed back rather than an answer, as 'CtServiceApi.csValues' hands back a gatherer and
-- for the same reason: the page's program is what answering costs, and it is one program however
-- many of the page's values are asked.
evalPageAsk :: Name -> FileImport -> EPrgmGraphData -> CRes (Val -> CResT IO Val)
evalPageAsk method prgmName prgmGraphData = do
  prgm <- prgmFromGraphData prgmName prgmGraphData
  -- No service, as 'evalPageValues' takes none and for its reason: this evaluation is one the
  -- service started, and an evaluation the service started has no service to ask.
  let env@Env{evTbEnv} = evalBaseEnv prgm
  return $ \val -> do
    let e = eVal (nameStr method) `eApply` ("/this", valToEExpr val)
    tree <- asCResT $ buildRoot evTbEnv e (getExprPartialType e) dshowDestType
    evalStateT (evalExpr tree) env

-- | Builds a function of a program as a run and evaluates it, threading @ioVal@ as its @/io@ handle.
-- That handle is the only difference between a real run and a mocked one: its 'IOHandle' says
-- whether the run's effects reach the process's stdin and stdout or a buffer.
--
-- The run's effects happen as it is evaluated, each when the IO method asking for it is reached, so
-- what comes back is only the exit code the run ended with.
evalRunPrgm :: EvalPages -> Val -> String -> EPrgm -> CResT IO (Integer, EvalResult)
evalRunPrgm pages ioVal function prgm = do
  input <- asCResT $ case evalPrgmTargetMode function prgm of
        EvalRunWithContext function' ->
          -- Case for eval ContextIn(value=main, io=IO)
          return (ioContextInput function', ioContextType PTopType)
        EvalRun function' ->
          -- Case for eval main
          return (eVal function', ioType)
        _ -> CErr [MkCNote $ GenCErr Nothing $ printf "Eval could not find a function %s to run" (show function)]
  let (inputE, dest) = input
  let src = getExprPartialType inputE
  (expr, env) <- asCResT $ evalBuildPrgm inputE src dest prgm
  let env2 = evalSetArgs (H.singleton ioStr ioVal) env{evPages = pages}
  (res, env') <- runStateT (evalExpr expr) env2
  case res of
    (IOVal r _) -> return (r, evalResult env')
    -- A run in an io answers with the io it ended in, in the context it returns.
    (TupleVal n args) | n == ContextStr, Just (IOVal r _) <- H.lookup ioStr args -> return (r, evalResult env')
    _ -> asCResT $ CErr [MkCNote $ GenCErr Nothing $ printf "Eval did not return an instance of IO \n\tInstead returned %s \n\t With expr %s" (show res) (show expr)]

evalRun :: EvalPages -> String -> FileImport -> EPrgmGraphData -> CResT IO (Integer, EvalResult)
evalRun pages function prgmName prgmGraphData = do
  prgm <- asCResT $ prgmFromGraphData prgmName prgmGraphData
  evalRunPrgm pages (IOVal 0 realIOHandle) function prgm

-- | Like 'evalRun' but captures the program's output into the given 'MockIO' buffer instead of
-- writing it to real stdout.
evalRunMock :: EvalPages -> MockIO -> String -> FileImport -> EPrgmGraphData -> CResT IO (Integer, EvalResult)
evalRunMock pages mockIO function prgmName prgmGraphData = do
  prgm <- asCResT $ prgmFromGraphData prgmName prgmGraphData
  evalRunPrgm pages (IOVal 0 (mockIOHandle mockIO)) function prgm

-- | Runs a function of a program for a debugger reporting on it: its exit code, its captured output,
-- and the tree of what it evaluated.
--
-- The run is given a mock handle ('mockIOHandle'), so its effects are captured rather than
-- performed: every write is to a buffer created in this call and dropped when it returns, and a
-- read finds no input rather than waiting on the enclosing process's stdin. A debugger reporting on
-- a run from inside the doc server must not write to the server's stdout or block it on a line
-- nobody will type.
evalDebugPrgm :: EvalPages -> String -> EPrgm -> CResT IO DebugRun
evalDebugPrgm pages function prgm = do
  mockIO <- lift newMockIO
  (code, res) <- evalRunPrgm pages (IOVal 0 (mockIOHandle mockIO)) function prgm
  out <- lift $ getMockIOOutput mockIO
  res' <- withCallShows prgm res
  lift $ showRunValues prgm $ DebugRun (Just code) out Nothing res' H.empty emptySyntaxRun

-- | Runs one expression of a program for a debugger reporting on it: the value it evaluated to, and
-- the tree of what it evaluated on the way.
--
-- Unlike 'evalDebugPrgm' there is no mock, because there is nothing here to capture. An expression is
-- not given an @\/io@ handle the way 'evalRunPrgm' gives one to a program, and an effect can only be
-- performed through one, so an expression cannot print or read: running a page's effects because a
-- reader opened a debugger on one of its expressions is not what was asked for.
--
-- Built exactly the way a @#print@'s @p@ is ('evalDShowAnnot'): toward @DShow@, and from no enclosing
-- object. The destination is what makes the expression evaluate at all — a tree built toward the
-- expression's own type has nothing to resolve and comes back as the unevaluated application it
-- started as — and @DShow@ is the destination that says "a value that can be shown", which is what a
-- debugger page has to have to show one.
--
-- The empty object context is not a simplification: an expression asked about over the API is written
-- inside no definition, so there are no arguments of one for it to read. It is the context a global
-- annotation is evaluated in, which is what this is. Building it this way is also why no source
-- partial is needed, and so why 'getExprPartialType' — which raises on a type that is not exactly one,
-- as a bare string literal's is not — is not on this path at all.
evalDebugExprPrgm :: EvalPages -> EExpr -> EPrgm -> CResT IO DebugRun
evalDebugExprPrgm pages input prgm = do
  let env@Env{evTbEnv} = evalBaseEnvWith pages Nothing prgm
  tree <- asCResT $ toTExprDest evTbEnv [] input (emptyMetaT dshowDestType)
  (val, env') <- runStateT (evalExpr tree) env
  res <- withCallShows prgm (evalResult env')
  lift $ showRunValues prgm $ DebugRun Nothing "" (Just val) res H.empty emptySyntaxRun

-- | An evaluation's result with the display filled in for each call that has one
-- ('attachCallShows'). Both debugger entry points go through this, since which of them ran a
-- program is not something a display of one of its calls depends on.
withCallShows :: EPrgm -> EvalResult -> CResT IO EvalResult
withCallShows prgm res@EvalResult{erTreebug} = do
  treebug' <- attachCallShows prgm erTreebug
  return res{erTreebug = treebug'}

-- | A run with every value in it shown as its display ('dshowVal'): a list as @[1, 2, 3]@ rather
-- than as the conses it is built from, and a value whose library displays it as something other than
-- text -- markup, a colour, an image -- as that.
--
-- Afterwards, as 'attachCallShows' is, and after it: rendering is evaluation, which recorded during
-- the run would be recorded as part of it, and a call's display is built from the call as it was
-- entered rather than from text standing for it. It is also why a display that asks the compilation
-- service for something reports rather than re-entering: the run's environment has none.
--
-- A value with no display is shown by its parts, each of which may have one, and so is a value
-- whose display is itself. A display that is something else is not looked into: it is what its
-- library said the value looks like. What a call was given is
-- always shown by its parts: it is the call rather than a value, and rendered whole it would be
-- evaluated, showing what the call returned where what it was given belongs. Scalars are left as
-- they are, since they already read as their text. Each distinct value is rendered once, as a run
-- passes the same lists down a recursion many times over, and the arrows one rendering builds are
-- kept for the next (see 'dshowVal').
showRunValues :: EPrgm -> DebugRun -> IO DebugRun
showRunValues prgm run@DebugRun{drValue, drResult=res@EvalResult{erTreebug, erDebugs, erExprs, erAnnots}} = evalStateT go (H.empty, evExEnv env)
  where
    env = evalBaseEnv prgm
    go = do
      treebug' <- mapM showNode erTreebug
      debugs' <- forM erDebugs $ \d@EvalDebug{edValue} -> (\v -> d{edValue = v}) <$> showVal edValue
      exprs' <- forM erExprs $ \e@EvalExprRun{exValue} -> (\v -> e{exValue = v}) <$> showVal exValue
      annots' <- forM erAnnots $ \a@EvalAnnotRun{eaValue} -> (\v -> a{eaValue = v}) <$> showVal eaValue
      value' <- mapM showVal drValue
      return run{drValue = value', drResult = res{erTreebug = treebug', erDebugs = debugs', erExprs = exprs', erAnnots = annots'}}
    showNode (EvalTreebugClosed oa input val children nodeId callShow) = do
      input' <- showParts input
      val' <- showVal val
      children' <- mapM showNode children
      return $ EvalTreebugClosed oa input' val' children' nodeId callShow
    showParts (TupleVal name args) = TupleVal name <$> mapM showVal args
    showParts v                    = return v
    -- A value holding an io (the one a context carries) has no equality to key a display by, and
    -- no display: it is shown by its parts.
    showVal v@TupleVal{} | holdsIO v = showParts v
    showVal v@TupleVal{} = do
      (cache, built) <- get
      case H.lookup v cache of
        Just shown -> return shown
        Nothing -> do
          (display, built') <- lift $ dshowVal env{evExEnv=built} v
          modify $ \(c, _) -> (c, built')
          shown <- case display of
            Just display' | display' /= v -> return display'
            -- Handed back as itself, which is how a member of @DShow@ that is not text is shown: by
            -- its parts, as a value with no display is. So its parts are given theirs.
            _                             -> showParts v
          modify $ \(c, b) -> (H.insert v shown c, b)
          return shown
    showVal v = return v
    holdsIO IOVal{}         = True
    holdsIO (TupleVal _ as) = any holdsIO (H.elems as)
    holdsIO _               = False

evalBuild :: String -> FileImport -> EPrgmGraphData -> CResT IO (Val, EvalResult)
evalBuild function prgmName prgmGraphData = do
  prgm <- asCResT $ prgmFromGraphData prgmName prgmGraphData
  targetMode <- asCResT $ evalTargetMode function prgmName prgmGraphData
  input <-  case targetMode of
        EvalRunWithContext function' ->
          -- Case for eval llvm(c=Context(value=main, io=IO))
          return $ eVal "/Catln/llvm" `eApply` ("/c", eVal function')
        EvalBuildWithContext function' ->
          -- Case for buildable ContextIn(value=main, io=IO)
          return $ ioContextInput function'
        EvalBuild function' ->
          -- Case for buildable main
          return $ eVal function'
        unexpectedEvalMode -> asCResT $ CErr [MkCNote $ GenCErr Nothing $ printf "Eval could not find a function %s to build with unexpected eval mode %s" (show function) (show unexpectedEvalMode)]
  let src = getExprPartialType input
  let dest = case targetMode of
        EvalBuildWithContext{} -> ioContextType resultType
        _                      -> resultType
  (expr, env) <- asCResT $ evalBuildPrgm input src dest prgm
  (res0, env') <- runStateT (evalExpr expr) env
  -- A build in an io answers with its result in the context it returns.
  let res = case res0 of
        TupleVal n args | n == ContextStr, Just v <- H.lookup contextValStr args -> v
        _ -> res0
  case res of
    (LLVMVal _) -> do
      -- llvmStr <- codegenExInit toCodegen
      let llvmStr = "LLVM Placeholder result"
      return (oneFileResult "out.ll" llvmStr, evalResult env')
    -- Checked here rather than merely returned, so that a value which is not a result is reported
    -- as the compilation failure it is instead of reaching whoever asked for the build.
    val -> case valResultFiles val of
      Right _ -> return (val, evalResult env')
      Left err -> asCResT $ CErr [MkCNote $ GenCErr Nothing $ printf "Eval %s did not build a %s: %s" function catlnResultPath err]
