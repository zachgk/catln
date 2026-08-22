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

import           CRes
import qualified Data.HashMap.Strict as H
import           Semantics.Prgm
import           Semantics.Types

import           Control.Monad
import           Data.Graph
import           Data.Maybe
-- import           Emit                (codegenExInit)
import           Control.Monad.State
import           CtConstants
import qualified Data.HashSet        as S
import           Data.UUID           (UUID)
import           Eval.Common
import           Eval.Env
import           Eval.ExprBuilder
import           Eval.Runtime
import           Semantics
import           Semantics.TypeGraph (ReachesEnv (ReachesEnv),
                                      reachesHasCutSubtypeOf, reachesPartials,
                                      reachesTo)
import           System.IO.Unsafe    (unsafePerformIO)
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
          funCtxTp = (partialVal (mkName ContextStr)){ptArgs=H.fromList [(partialKey (mkName contextValStr), typeVal function')], ptArgMode=PtArgAny}
          funTp = partialVal function'
          funCtxReaches = reachesPartials reachEnv [funCtxTp]
          funReaches = reachesPartials reachEnv [funTp]
      in case (funCtxReaches, funReaches) of
           (rt, _) | reachesHasCutSubtypeOf typeEnv H.empty rt resultType -> EvalBuildWithContext (nameStr function')
           (rt, _) | reachesHasCutSubtypeOf typeEnv H.empty rt ioType -> EvalRunWithContext (nameStr function')
           (_, rt) | reachesHasCutSubtypeOf typeEnv H.empty rt resultType -> EvalBuild (nameStr function')
           (_, rt) | reachesHasCutSubtypeOf typeEnv H.empty rt ioType -> EvalRun (nameStr function') -- Should require isShowable for run result
           _ -> NoEval funCtxReaches funReaches
  where
    typeEnv = mkTypeEnv prgm
    reachEnv = ReachesEnv typeEnv H.empty S.empty

evalTargetMode :: String -> FileImport -> EPrgmGraphData -> CRes EvalMode
evalTargetMode function prgmName prgmGraphData = do
  prgm <- prgmFromGraphData prgmName prgmGraphData
  return $ evalPrgmTargetMode function prgm

-- | Gets the target modes for all objects in the file
evalAllTargetModes :: EPrgm -> H.HashMap UUID EvalMode
evalAllTargetModes prgm@Prgm{prgmObjMap} = H.fromList $ mapMaybe targetMode $ flatObjectMap prgmObjMap
  where
    targetMode oa = case nameStr (oaObjPath oa) of
      ContextStr -> case H.lookupDefault Nothing (partialKey (mkName contextValStr)) $ exprAppliedArgsMap $ oaObjExpr oa of
        Just (_, Just valE) -> case maybeExprPath valE of
          Just valName -> Just (getMetaID $ getExprMeta $ oaObjExpr oa, evalPrgmTargetMode (nameStr valName) prgm)
          _ -> Nothing
        _ -> Nothing
      _ -> Just (getMetaID $ getExprMeta $ oaObjExpr oa, evalPrgmTargetMode (nameStr (oaObjPath oa)) prgm)

-- | evaluate annotations such as assertions that require compiler verification
--
-- The annotation is taken as well as the value it evaluated to, because an annotation reporting on
-- where it is written cannot be told that from its value: an evaluated annotation is a tuple of what
-- its arguments came to, with nothing left of the expression it was.
evalCompAnnot :: TExpr EvalMetaDat -> Val -> StateT Env CRes ()

evalCompAnnot _ (TupleVal n args) | n == assertStr = case (H.lookup assertTestStr args, H.lookup assertMsgStr args) of
  (Just b, Just (StrVal _)) | b == true -> return ()
  (Just b, Just (StrVal msg)) | b == false -> lift $ CErr [MkCNote $ AssertCErr msg]
  (Just b, Nothing) | b == true -> return ()
  (Just b, Nothing) | b == false -> lift $ CErr [MkCNote $ AssertCErr "Failed assertion"]
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

evalGuards :: Env -> Val -> EObjArr -> CRes (Bool, Env)
evalGuards = undefined
  -- case cond' of
  --   b | b == true -> evalPopVal <$> evalCallTree (evalPush env4 $ "then for " ++ show ifObj) m ifThenTree
  --   b | b == false -> evalPopVal <$> evalCallTree (evalPush env4 $ "else for " ++ show ifObj) m (TCCond resType restIfTrees elseTree)
  --   _ -> error "Non-Bool eval resArrowCond"

evalObjArr :: Val -> AnyObjArr -> StateT Env CRes Val
evalObjArr = evalObjArrCaptured H.empty

-- | 'evalObjArr' for a closure ('ObjArrVal'): the captured creation-site args underlay the
-- closure's own parameters, so body references to enclosing args resolve lexically.
evalObjArrCaptured :: Args -> Val -> AnyObjArr -> StateT Env CRes Val
evalObjArrCaptured captured input oa = do
  let newArrArgs = H.union (buildArrArgs oa input) captured
  -- The src partial seeds the arm-body rebuild's var/arg environment, where a function-typed
  -- slot's VALUE is its output type (the input signature lives in the key — the typechecker's
  -- pointer-slot convention). The input-signature form would bind an arm's output var to the
  -- signature (@$M := /fn(/v=Int)@) and leave the body's argument application unable to reach
  -- a concrete destination type.
  (resArrowTree, compAnnots, oldArgs) <- evalStartEArrow (getValPartialTypeOut input) oa newArrArgs
  forM_ compAnnots $ \compAnnot -> do
            compAnnot' <- withEvalPush (printf "annot %s" (show compAnnot)) $ evalExpr compAnnot
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

evalCallTree :: Val -> TCallTree -> StateT Env CRes Val
-- evalCallTree v ct | trace (printf "evalCallTree %s using %s" (show v) (show ct)) False = undefined
evalCallTree v TCTId = return v
evalCallTree  m (TCMatch destType opts) = do
  Env{evTypeEnv} <- get
  -- Normalize higher-order argument keys (bare @/fn@ on the runtime value vs the
  -- function-signature key @/fn(/v=..)@ on the typed arrow) before matching, mirroring
  -- the 'simplifyArgKeys' handling in the 'TCCond' branch below.
  let reKeyP p = p{ptArgs = H.mapKeys (partialKey . pkName) (ptArgs p)}
  -- A closure argument's slot in a typed arm holds either the closure's input signature or the
  -- output contract delivered from the declaration; the value-derived partial only carries the
  -- input signature, so also try the output form ('getValPartialTypeOut') before failing an arm.
  let valP = reKeyP $ getValPartialType m
  let valPOut = reKeyP $ getValPartialTypeOut m
  let optMatches optType = isSubtypePartialOf evTypeEnv valP (singletonType $ reKeyP optType)
        || (valPOut /= valP && isSubtypePartialOf evTypeEnv valPOut (singletonType $ reKeyP optType))
  case H.toList $ H.filterWithKey (\optType _ -> optMatches optType) opts of
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
      case buildPartialCallTree evTbEnv [] S.empty PTopType (getValPartialType m) destType of
        CRes _ rebuilt | rebuilt /= TCMatch destType opts ->
          withEvalPush ("rebuild match with val " ++ show m) $ evalCallTree m rebuilt
        _ -> evalError $ printf "Failed match in eval callTree: \n\tVal: %s \n\tVal type: %s\n\tOptions: %s" (show m) (show $ getValPartialType m) (show opts)
    (_:_:_) -> evalError $ printf "Multiple matches in eval callTree: \n\tVal: %s \n\tOptions: %s " (show m) (show opts)
evalCallTree v (TCSeq a b) = do
  v' <- evalCallTree v a
  evalCallTree v' b
evalCallTree m (TCCond _ []) = evalError $ printf "Found no matching conditions in evalTree for val %s (val type %s)" (show m) (show $ getValPartialType m)
evalCallTree m (TCCond resType ((cond, ifThenTree):restIfTrees)) = do
  Env{evTypeEnv} <- get
  -- The value's partial is tried in both slot conventions, like the 'TCMatch' branch above: a
  -- closure argument's slot in the arm's object holds either the closure's input signature or
  -- the output contract delivered from the declaration.
  let condMatches armOa =
        let armT = simplifyArgKeys $ clearUnionTypePreds $ getExprType $ oaObjExpr armOa
            valT = simplifyArgKeys $ singletonType $ getValPartialType m
            valTOut = simplifyArgKeys $ singletonType $ getValPartialTypeOut m
        in isSubtypeOf evTypeEnv valT armT || (valTOut /= valT && isSubtypeOf evTypeEnv valTOut armT)
  cond' <- case cond of
    -- TODO Remove the use of clearUnionTypePreds below. It is a workaround as type predicates are not supported by subtypeOf
    Just (_, oa) | not (condMatches oa) -> return False
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
evalCallTree input (TCArg _ name) = do
  Env{evArgs} <- get
  case H.lookup name evArgs of
    Just (ObjArrVal oa captured) -> evalObjArrCaptured captured input (Right oa)
    Just arg' -> return arg'
    Nothing -> evalError $ printf "Unknown arg %s found during evaluation \n\t\t with arg env %s" name (show evArgs)
evalCallTree input (TCObjArr oa) = evalObjArr input (Left oa)
evalCallTree input (TCPrim oa (EPrim _ f)) = do
  case input of
    (TupleVal _ args) -> case f args of
      Right val -> return val
      Left err  -> evalError err
    _                 -> evalError $ printf "Unexpected eval PrimArrow input %s to %s" (show input) (show oa)
evalCallTree input (TCPrim oa (EEnvPrim _ f)) = do
  env <- get
  case input of
    (TupleVal _ args) -> case f env args of
      Right val -> return val
      Left err  -> evalError err
    _                 -> evalError $ printf "Unexpected eval PrimArrow input %s to %s" (show input) (show oa)
evalCallTree _ TCMacro{} = evalError $ printf "Can't evaluate a macro - it should be removed during TreeBuild"

-- | Checks the implicit equality of a non-linear pattern's shared argument positions. For an
-- @ifObj@ such as @f(a@x, b@x)@, each non-linear group names input fields (here @a@ and @b@) that
-- must be equal for the clause to apply; this compares the corresponding values of the input tuple
-- @m@ pairwise. Equality goes through the argument type's 'Eq' (the @==@ operator over the two
-- values as constants), not Haskell structural equality, so a type with a custom equality behaves
-- as the user expects. If a value's type has no 'Eq', that surfaces as a build error here.
evalNonLinearEq :: Val -> EObjArr -> StateT Env CRes Bool
evalNonLinearEq (TupleVal _ tupleArgs) ifObj = allM
  [ evalValEq va vb
  | grp <- exprNonLinearArgGroups (oaObjExpr ifObj)
  , ((ea, _), (eb, _)) <- zip grp (tail grp)
  , Just va <- [H.lookup (nameStr (exprPath ea)) tupleArgs]
  , Just vb <- [H.lookup (nameStr (exprPath eb)) tupleArgs]
  ]
  where
    allM = foldr (\mb acc -> mb >>= \b -> if b then acc else return False) (return True)
evalNonLinearEq _ _ = return True

-- | Whether two runtime values are equal under their type's 'Eq', by building and evaluating
-- @l == r@ over the values as constants.
evalValEq :: Val -> Val -> StateT Env CRes Bool
evalValEq v1 v2 = do
  Env{evTbEnv} <- get
  let eqExpr = eVal operatorEq `eApply` (operatorArgL, valToEExpr v1) `eApply` (operatorArgR, valToEExpr v2)
  tree <- lift $ buildRoot evTbEnv eqExpr (getExprPartialType eqExpr) boolType
  (== true) <$> evalExpr tree

evalExpr :: TExpr EvalMetaDat -> StateT Env CRes Val
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
evalExpr (THoleExpr m h) = lift $ CErr [MkCNote $ GenCErr (getMetaPos m) $ printf "Can't evaluate hole %s" (show h)]
evalExpr (TAliasExpr b _) = evalExpr b
evalExpr (TWhere _ b _) = evalExpr b
evalExpr (TTupleApply _ (_, b) (EAppArg oa@ObjArr{oaObj=Just (TValue _ "/io"), oaArr=Just (Nothing, _)})) = do
  Env{evArgs} <- get
  TupleVal n args <- evalExpr b
  case H.lookup "/io" evArgs of
    Just io -> return $ TupleVal n (H.insert (nameStr (oaObjPath oa)) io args)
    Nothing -> error $ printf "evalExpr with no io"
evalExpr (TTupleApply _ (_, b) (EAppVar{})) = evalExpr b
evalExpr (TTupleApply m (_, b) (EAppArg oa)) = do
  (TupleVal n args) <- evalExpr b
  v <- case oaArr oa of
    Just (Just oaExpr, _) -> case oaObj oa of
      Just TValue{} -> evalExpr oaExpr
      Just TTupleApply{} -> do
        Env{evArgs=creationArgs} <- get
        return $ ObjArrVal oa creationArgs
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
evalExpr (TTupleApply _ (_, _) (EAppSpread a)) = error $ printf "Not yet implemented evalExpr %s" (show a)
evalExpr (TCalls _ b callTree) = do
  b' <- evalExpr b
  evalCallTree b' callTree

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
evalBaseEnvService service prgm@Prgm{prgmObjMap} = Env {
        evObjMap = prgmObjMap,
        evTypeEnv = (mkTypeEnv prgm){teArgModeProjection=True},
        evArgs = H.empty,
        evExEnv = H.empty,
        evTbEnv = buildTBEnv primEnv prgm,
        evService = service,
        evAnnotObj = Nothing,
        evCallStack = [],
        evCoverage = H.empty,
        evTreebugOpen = [],
        evTreebugClosed = [],
        evTreebugNextId = 0,
        evDebugs = []
                }

-- | Flattens the program reachable from a file into one. It is polymorphic in the program because
-- every pipeline stage holds one, and scoping a stage to a file has to mean the same thing at each:
-- that file plus everything it imports.
prgmFromGraphData :: (Monoid p) => FileImport -> GraphData p FileImport -> CRes p
prgmFromGraphData prgmName (prgmGraph, nodeFromVertex, vertexFromKey) = case vertexFromKey prgmName of
  Just v -> return $ mconcat $ map (fst3 . nodeFromVertex) $ reachable prgmGraph v
  Nothing -> fail $ printf "Could not find prgm %s" (show prgmName)

-- | Tries to TreeBuild all ObjArrs, returning all built successfully
evalBuildAll :: EPrgmGraphData -> CRes (GraphData (Prgm TExpr EvalMetaDat) FileImport)
evalBuildAll prgmGraphData = do
  let prgms = graphToNodes prgmGraphData
  prgms' <- forM prgms $ \(Prgm objMap cg annots, prgmName, deps) -> do
    evalPrgm <- prgmFromGraphData prgmName prgmGraphData
    let Env{evTbEnv} = evalBaseEnv evalPrgm
    objMap' <- catCRes (buildRootOA evTbEnv <$> flatObjectMap objMap)
    annots' <- catCRes $ fmap (toTExpr evTbEnv []) annots
    let objMap'' = objectMapFromList objMap'
    return (Prgm objMap'' cg annots', prgmName, deps)
  return $ graphFromEdges prgms'

evalBuildPrgm :: EExpr -> PartialType -> Type -> EPrgm -> CRes (TExpr EvalMetaDat, Env)
evalBuildPrgm input srcType destType prgm = do
  let env@Env{evTbEnv} = evalBaseEnv prgm
  initTree <- buildRoot evTbEnv input srcType destType
  return (initTree, env)

-- | Build and evaluate a single annotation expression to the value it renders to through the
-- @DShow@ class (the same path used to display @#print@ values in webdocs). Returns @Nothing@
-- when the annotation cannot reach @DShow@ and so is not renderable.
evalDShowAnnot :: Env -> [(PartialType, EObjArr)] -> EExpr -> CRes (Maybe Val)
evalDShowAnnot env0@Env{evTbEnv, evTypeEnv} os annot0
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
      return $ Just $ case renderValString pVal of
        CRes _ str@StrVal{} -> str
        _                   -> pVal
  | reachesTo evTypeEnv H.empty (getExprType $ showAnnot annot) destType = do
      tree <- toTExprDest evTbEnv os (showAnnot annot) destM
      val <- evalStateT (evalExpr tree) env
      return (Just val)
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
    showAnnot a = eVal "/Catln/Doc/dshow" `eApply ` ("/s", a)
    -- Render a value as text through @/Data/toString@. The value is reconstructed as a typed
    -- constant expression ('valToEExpr', whose meta carries the concrete runtime type — e.g. a
    -- @ConsList@ of @Integer@s), and @toString@ is built over that; the concrete receiver type
    -- dispatches where @p@'s abstract static type could not. Fails (leaving the caller with the raw
    -- value) for a value 'valToEExpr' cannot reconstruct or one that is not @ToString@.
    renderValString v
      | reconstructableVal v = do
          let e = eVal "/Data/toString" `eApply` ("/this", valToEExpr v)
          tree <- buildRoot evTbEnv e (getExprPartialType e) destType
          evalStateT (evalExpr tree) env
      | otherwise = CErr []
    -- Whether 'valToEExpr' can rebuild this value (it errors on closures, IO, and other runtime-only
    -- values, none of which are @ToString@).
    reconstructableVal IntVal{}        = True
    reconstructableVal FloatVal{}      = True
    reconstructableVal StrVal{}        = True
    reconstructableVal CharVal{}       = True
    reconstructableVal (TupleVal _ as) = all reconstructableVal (H.elems as)
    reconstructableVal _               = False
    -- Evaluate @p@ to its concrete value by building it toward @DShow@ (which certifies
    -- displayability and yields the value itself). @p@ is declared @-> DShow@, so this resolves for
    -- every valid print.
    evalToValue a = do
      tree <- toTExprDest evTbEnv os a destM
      evalStateT (evalExpr tree) env
    destType = dshowDestType
    destM = emptyMetaT destType

-- | The class a display is built toward, which is what forces it to be evaluated: a tree built
-- toward an expression's own type comes back as the application itself, unevaluated.
dshowDestType :: Type
dshowDestType = classPartial $ partialVal $ mkName dshowClassName

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
evalDShowCall :: Env -> String -> [(String, Val)] -> CRes Val
evalDShowCall env@Env{evTbEnv, evTypeEnv} fun args = do
  fun' <- case maybeGetSingleton $ expandRelPartial evTypeEnv H.empty (partialVal (mkName fun)) of
    Just f -> return $ nameStr $ ptName f
    -- Both no match and several are reported as the name not being one function, since what the
    -- caller has to do about either is the same: name the one they meant, absolutely.
    Nothing -> fail $ printf "The doc server holds no single function named %s to call" fun
  let input = foldl (\base (argName, argVal) -> eApply base ('/' : argName, valToEExpr argVal)) (eVal fun') args
  unless (reachesTo evTypeEnv H.empty (getExprType input) dshowDestType) $
    fail $ printf "Calling %s with %s does not reach %s, so it is not something the webdocs can show" fun' (show $ map fst args) dshowClassName
  tree <- toTExprDest evTbEnv [] input (emptyMetaT dshowDestType)
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
evalAnnots :: Maybe EvalService -> FileImport -> EPrgmGraphData -> CRes [(EExpr, Val)]
evalAnnots service prgmName prgmGraphData = do
  prgm <- prgmFromGraphData prgmName prgmGraphData
  Prgm objMap _ globalAnnots <- case graphLookup prgmName prgmGraphData of
    Just own -> return own
    Nothing  -> CErr [MkCNote $ GenCErr Nothing $ printf "Could not find the page %s to evaluate the annotations of" (show $ impRaw prgmName)]
  let env = evalBaseEnvService service prgm
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
buildDisplays :: Env -> [(PartialType, EObjArr)] -> String -> [EExpr] -> CRes [(EExpr, Val)]
buildDisplays env os what exprs = do
    results <- forM exprs $ \e -> case fmap (e,) <$> evalDShowAnnot env os e of
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
evalPageValues :: [Name] -> FileImport -> EPrgmGraphData -> CRes [(EExpr, Val)]
evalPageValues names prgmName prgmGraphData = do
  Prgm objMap _ _ <- case graphLookup prgmName prgmGraphData of
    Just own -> return own
    Nothing  -> CErr [MkCNote $ GenCErr Nothing $ printf "Could not find the page %s to evaluate the declarations of" (show $ impRaw prgmName)]
  case filter declaresOne (flatObjectMap objMap) of
    -- A page that declares none is answered before anything is built, so asking every page of a doc
    -- server about a type most of them never mention costs no evaluation environments.
    [] -> return []
    oas -> do
      prgm <- prgmFromGraphData prgmName prgmGraphData
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
evalPageAsk :: Name -> FileImport -> EPrgmGraphData -> CRes (Val -> CRes Val)
evalPageAsk method prgmName prgmGraphData = do
  prgm <- prgmFromGraphData prgmName prgmGraphData
  -- No service, as 'evalPageValues' takes none and for its reason: this evaluation is one the
  -- service started, and an evaluation the service started has no service to ask.
  let env@Env{evTbEnv} = evalBaseEnv prgm
  return $ \val -> do
    let e = eVal (nameStr method) `eApply` ("/this", valToEExpr val)
    tree <- buildRoot evTbEnv e (getExprPartialType e) dshowDestType
    evalStateT (evalExpr tree) env

-- | Builds a function of a program as a run and evaluates it, threading @ioVal@ as its @/io@ handle.
-- That handle is the only difference between a real run and a mocked one: an 'IOVal' accumulates
-- writes to stdout where a 'MockIOVal' accumulates writes to a buffer.
--
-- Everything here is pure. The program's accumulated IO action is returned unperformed alongside its
-- exit code, so it is the caller that decides whether the run's effects happen at all — which is what
-- lets a caller wanting only the evaluation tree take it without them.
evalRunPrgm :: Val -> String -> EPrgm -> CRes (Integer, IO (), EvalResult)
evalRunPrgm ioVal function prgm = do
  input <- case evalPrgmTargetMode function prgm of
        EvalRunWithContext function' ->
          -- Case for eval Context(value=main, io=IO)

          return $ eApplyM (eVal ContextStr `eApply` (contextValStr, eVal function')) "/io" ioM
        EvalRun function' ->
          -- Case for eval main
          return $ eVal function'
        _ -> CErr [MkCNote $ GenCErr Nothing $ printf "Eval could not find a function %s to run" (show function)]
  let src = getExprPartialType input
  let dest = ioType
  (expr, env) <- evalBuildPrgm input src dest prgm
  let env2 = evalSetArgs (H.singleton "/io" ioVal) env
  (res, env') <- runStateT (evalExpr expr) env2
  case res of
    (IOVal r io)       -> return (r, io, evalResult env')
    (MockIOVal r io _) -> return (r, io, evalResult env')
    _ -> CErr [MkCNote $ GenCErr Nothing $ printf "Eval did not return an instance of IO \n\tInstead returned %s \n\t With expr %s" (show res) (show expr)]

evalRun :: String -> FileImport -> EPrgmGraphData -> CResT IO (Integer, EvalResult)
evalRun function prgmName prgmGraphData = do
  prgm <- asCResT $ prgmFromGraphData prgmName prgmGraphData
  (r, io, res) <- asCResT $ evalRunPrgm (IOVal 0 $ pure ()) function prgm
  lift (io >> pure (r, res))

-- | Like 'evalRun' but captures the program's output into the given 'MockIO' buffer instead of
-- writing it to real stdout.
evalRunMock :: MockIO -> String -> FileImport -> EPrgmGraphData -> CResT IO (Integer, EvalResult)
evalRunMock mockIO function prgmName prgmGraphData = do
  prgm <- asCResT $ prgmFromGraphData prgmName prgmGraphData
  (r, io, res) <- asCResT $ evalRunPrgm (MockIOVal 0 (pure ()) (mockIORef mockIO)) function prgm
  lift (io >> pure (r, res))

-- | Runs a function of a program for a debugger reporting on it: its exit code, its captured output,
-- and the tree of what it evaluated.
--
-- The 'unsafePerformIO' is what lets a pure caller ask for this at all, and the mock is what makes
-- that defensible: every effect is a 'modifyIORef' on a handle created in this call and dropped when
-- it returns, so nothing outside can observe that the run happened. The same code over 'evalRun'
-- would write the debugged program's output to the enclosing process's stdout and could block it
-- forever in 'Eval.Runtime.ioInput' — effects a pure caller cannot be asked to tolerate.
-- 'Eval.Runtime.ioInput' makes the same trade for the same reason.
evalDebugPrgm :: String -> EPrgm -> CRes DebugRun
evalDebugPrgm function prgm = unsafePerformIO $ do
  mockIO <- newMockIO
  case evalRunPrgm (MockIOVal 0 (pure ()) (mockIORef mockIO)) function prgm of
    CErr notes -> return $ CErr notes
    CRes notes (code, io, res) -> do
      io
      out <- getMockIOOutput mockIO
      return $ CRes notes (DebugRun (Just code) out Nothing res)

-- | Runs one expression of a program for a debugger reporting on it: the value it evaluated to, and
-- the tree of what it evaluated on the way.
--
-- Unlike 'evalDebugPrgm' this is a plain 'CRes' with no 'unsafePerformIO' and no mock, because there
-- is nothing here to capture. An expression is not given an @\/io@ handle the way 'evalRunPrgm' gives
-- one to a program, so it cannot print, and an expression whose value /is/ an unperformed 'IOVal' is
-- reported as that value rather than performed: running a page's effects because a reader opened a
-- debugger on one of its expressions is not what was asked for.
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
evalDebugExprPrgm :: EExpr -> EPrgm -> CRes DebugRun
evalDebugExprPrgm input prgm = do
  let env@Env{evTbEnv} = evalBaseEnv prgm
  tree <- toTExprDest evTbEnv [] input (emptyMetaT dshowDestType)
  (val, env') <- runStateT (evalExpr tree) env
  return $ DebugRun Nothing "" (Just val) (evalResult env')

evalBuild :: String -> FileImport -> EPrgmGraphData -> CResT IO (Val, EvalResult)
evalBuild function prgmName prgmGraphData = do
  prgm <- asCResT $ prgmFromGraphData prgmName prgmGraphData
  targetMode <- asCResT $ evalTargetMode function prgmName prgmGraphData
  input <-  case targetMode of
        EvalRunWithContext function' ->
          -- Case for eval llvm(c=Context(value=main, io=IO))
          return $ eVal "/Catln/llvm" `eApply` ("/c", eVal function')
        EvalBuildWithContext function' ->
          -- Case for buildable Context(value=main, io=IO)
          return $ eApplyM (eVal ContextStr `eApply` (contextValStr, eVal function')) "/io" ioM
        EvalBuild function' ->
          -- Case for buildable main
          return $ eVal function'
        unexpectedEvalMode -> asCResT $ CErr [MkCNote $ GenCErr Nothing $ printf "Eval could not find a function %s to build with unexpected eval mode %s" (show function) (show unexpectedEvalMode)]
  let src = getExprPartialType input
  let dest = resultType
  (expr, env) <- asCResT $ evalBuildPrgm input src dest prgm
  (res, env') <- asCResT $ runStateT (evalExpr expr) env
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
