-- | Unit-level repro harness for the functor-law fragment divergence (task #36).
--
-- The full-build repro is @catln build stack/core/functor.ct@: the per-SCC solver's
-- @testFunctorComposition@ fragment (198 constraints) diverges at its 4th epoch inside a
-- transiently-built meet (per -hc cost centers: 'expandTypesWithNamesFull' under
-- @computeConstraint.narrowChannel.chanReq'.metTopConsts@), while its stored point types
-- stay tiny and byte-stable. This harness reproduces JUST that state and probes the
-- fragment's constraints one at a time:
--
--   1. Load and desugar functor.ct's closure (2 pages), typecheck page 1 normally.
--   2. Replay the functor page's fragments; solve every fragment normally EXCEPT
--      @testFunctorComposition@, which runs for exactly the 3 epochs that survive.
--   3. Rebuild the per-epoch env caches exactly as the fatal 4th epoch would.
--   4. Force each of the fragment's constraint computes in isolation under a timeout;
--      report the hanger(s), then force the culprit's channels separately.
--
-- Run (stack repl is broken on this box — use ghci against the built library):
--   stack exec ghci -- test/Typecheck/LawMeetRepro.hs   (module Typecheck.LawMeetRepro)
--   ghci> reproMain
module Typecheck.LawMeetRepro where

import           Control.Exception     (evaluate)
import           Control.Monad
import           Control.Monad.State   (StateT, modify, runStateT)
import           CRes
import           Data.Graph            (flattenSCC, reachable,
                                        stronglyConnCompR)
import qualified Data.HashMap.Strict   as H
import qualified Data.HashSet          as S
import qualified Data.IntMap.Strict    as IM
import           Data.List             (isInfixOf, partition)
import           Data.Maybe            (fromJust, mapMaybe)
import           Data.Time.Clock       (diffUTCTime, getCurrentTime)
import           System.Environment    (lookupEnv)
import           System.Timeout        (timeout)

import           CtService             (ctssBaseFiles, getRawPrgmFrom)
import           MapMeta               (clearMetaDat, mapMetaPrgm)
import           Semantics             (testCtssConfig, unsafeFreshQueryToken)
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Desugarf    (desFiles)
import           Syntax.Parsers        (mkDesCanonicalImportStr)
import           TypeCheck             (depPieceOf, typecheckPrgms)
import           TypeCheck.Common
import           TypeCheck.Constrain   (computeConstraint, executeConstraint,
                                        runConstraints, stypeConstraint)
import           TypeCheck.Decode      (toPrgms)
import           TypeCheck.Encode      (fromPrgmsFragment, makeBaseFEnv,
                                        prgmsLocalDefNames)
import           TypeCheck.Instruments (defaultInstruments)
import           TypeCheck.Partition
import           TypeCheck.Show        (showCon)
import           TypeCheck.TypeUtils   (buildTypeEnv)
import           Utils

-- | Seconds each probed constraint may run before being declared a culprit.
probeTimeoutSecs :: Int
probeTimeoutSecs = 20

lawKey :: String
lawKey = "testFunctorComposition"

reproMain :: IO ()
reproMain = do
  ctss <- ctssBaseFiles testCtssConfig ["stack/core/functor.ct"]
  fileName <- mkDesCanonicalImportStr testCtssConfig "stack/core/functor.ct"
  desRes <- runCResT $ do
    raw <- getRawPrgmFrom fileName ctss
    desFiles raw
  case desRes of
    CErr notes  -> putStrLn $ "desugar failed:\n" ++ unlines (map show (take 4 notes))
    CRes _ des -> runGraph des

-- | Walk the page graph in dependency order (the 'mapGraphWithDeps' fold, inlined so the
-- functor page can be intercepted). Every earlier SCC typechecks via the REAL
-- 'typecheckPrgms'; the SCC containing the law page goes to 'probeLawPage'.
runGraph :: GraphData PPrgm FileImport -> IO ()
runGraph g@(graph, fromNode, fromName) = go H.empty (stronglyConnCompR (graphToNodes g))
  where
    go _ [] = putStrLn "runGraph: law page never found"
    go finished (scc:rest) = do
      let newList = flattenSCC scc
      let newNames = S.fromList (map snd3 newList)
      let perNodeData = map (\(n, k, _) ->
            let sccPeerDeps = [n' | (n', k', _) <- newList, k' /= k]
                transDepNames = S.fromList $ map (snd3 . fromNode) $ reachable graph $
                  fromJust (fromName k)
                finishedDepNames = S.difference transDepNames newNames
                extDeps = map (\dn -> fst3 (H.lookupDefault (error "missing dep") dn finished))
                              (S.toList finishedDepNames)
            in (n, sccPeerDeps, extDeps)) newList
      let isLawScc = any (\(n, _, _) -> any ((lawKey `isInfixOf`) . nameStr) (pprgmKeys n)) perNodeData
      if isLawScc
        then probeLawPage (head perNodeData)
        else case typecheckPrgms perNodeData of
          TypeCheckResE notes -> putStrLn $ "page typecheck FAILED:\n" ++ unlines (map show (take 4 notes))
          TypeCheckResult _ results -> do
            putStrLn $ "page typechecked ok (" ++ show (length newList) ++ " node(s), "
                       ++ show (sum (map (length . pprgmKeys . fst3) perNodeData)) ++ " items)"
            let finished' = foldr (\((_, k, ds), r) acc -> H.insert k (r, k, ds) acc)
                                  finished (zip newList results)
            go finished' rest

pprgmKeys :: PPrgm -> [TypeName]
pprgmKeys p = case prgmObjMap p of ObjectMap m -> H.keys m

-- | The functor page: replicate 'typecheckPrgmsSt' with the law fragment stopped after 3
-- epochs, then probe its constraints from the exact state the fatal epoch would see.
probeLawPage :: (PPrgm, [PPrgm], [TPrgm]) -> IO ()
probeLawPage (page, peers, deps) = do
  putStrLn "== law page reached; replaying fragments =="
  loopFrags env0 depPieces0 fragments
  where
    pprgms = page : peers
    env0 = (makeBaseFEnv defaultInstruments (mconcat (pprgms ++ map (mapMetaPrgm clearMetaDat) deps)))
             {feLocalDefNames = prgmsLocalDefNames pprgms}
    pageObjMap = mconcat (map prgmObjMap pprgms)
    depObjMap = prgmObjMap (mconcat deps)
    sccFragments = fragmentPrgms (itemSccGroups pageObjMap) pprgms ++ annotFragmentPrgms pprgms
    fragments = if null sccFragments then [pprgms] else sccFragments
    unO (ObjectMap m) = m
    allKeys = H.keys (unO pageObjMap) ++ H.keys (unO depObjMap)
    edgeMap = resolveRefEdges allKeys (H.unionWith (++) (itemRawRefs pageObjMap) (itemRawRefsShallow depObjMap))
    depPieces0 = H.mapWithKey depPieceOf (unO depObjMap)
    allDataKeys = dataItemKeys pageObjMap ++ dataItemKeys depObjMap

    loopFrags _ _ [] = putStrLn "law fragment never reached?!"
    loopFrags env pieces (frag:rest) = do
      let fragKeys = concatMap (H.keys . unO . prgmObjMap) frag
      let isLaw = any ((lawKey `isInfixOf`) . nameStr) fragKeys
      let closureKeys
            | null fragKeys = H.keys pieces
            | otherwise = hashNub (allDataKeys ++ S.toList (closureFrom edgeMap fragKeys))
      let cPieces = mapMaybe (`H.lookup` pieces) closureKeys
      let tRec = mconcat [r | (r, _, _, _) <- cPieces]
      let tPrec = foldr (H.unionWith min) H.empty [p | (_, p, _, _) <- cPieces]
      let tGraph = mconcat [g' | (_, _, g', _) <- cPieces]
      let tCleared = mconcat [c | (_, _, _, c) <- cPieces]
      let envF = env{feVTypeGraph = mempty, feTTypeGraph = tGraph, feTTypeGraphCleared = tCleared}
      if isLaw
        then do
          putStrLn $ "== LAW fragment (" ++ show (length fragKeys) ++ " key(s)); replaying epochs =="
          case runStateTC (fromPrgmsFragment frag (tRec, tPrec)) envF of
            Left notes -> putStrLn $ "encode ABORTED:\n" ++ unlines (map show (take 4 notes))
            Right ((_, cons), envE) -> do
              putStrLn $ "encoded; " ++ show (length cons) ++ " constraints"
              mSel <- lookupEnv "REPRO_PROBE"
              case mSel of
                Just sel -> runColdProbe sel (feTypeEnv envE)
                Nothing  -> replayEpochs envE{feConMemo = mempty} (zip [0 ..] cons)
        else
          case runStateTC (do (vp, cons) <- fromPrgmsFragment frag (tRec, tPrec)
                              _ <- runConstraints 25 cons
                              modify nextConstrainEpoch
                              toPrgms vp) envF of
            Left notes -> putStrLn $ "fragment ABORTED:\n" ++ unlines (map show (take 4 notes))
            Right (tprgms', env') -> do
              let fragPieces = H.mapWithKey depPieceOf (unO (mconcat (map prgmObjMap tprgms')))
              loopFrags env' (H.union fragPieces pieces) rest

-- | Run a 'StateT FEnv TypeCheckResult' action, demoting failure to 'Left'.
runStateTC :: StateT FEnv TypeCheckResult a -> FEnv -> Either [TypeCheckError] (a, FEnv)
runStateTC action env = case runStateT action env of
  TypeCheckResult _ r -> Right r
  TypeCheckResE notes -> Left notes

-- | Rebuild the per-epoch env caches exactly as 'runConstraintsIdx' does at the top of an
-- epoch, so probed constraints see the same 'feReachesTypeEnv'/'feUniverseKey' the
-- fatal epoch would.
prepareEpochEnv :: FEnv -> Either [TypeCheckError] FEnv
prepareEpochEnv env = case buildTypeEnv env of
  TypeCheckResE notes -> Left notes
  TypeCheckResult _ (reachesVPart, reachesTypeEnv) ->
    let u = maybe BottomType stypeAct $ tcreToMaybe $ descriptor env $ feUnionAllObjs env
        uUnchanged = u == feUniverseKey env
        envUnchanged = uUnchanged && reachesVPart == feReachesVPart env
        inputArrowNames = if envUnchanged then feInputArrowNames env
                          else typeGraphInputEmbeddedNames reachesTypeEnv
        reachesTypeEnv' = if envUnchanged
          then feReachesTypeEnv env
          else reachesTypeEnv{teQueryCacheKey = Just (unsafeFreshQueryToken (teTypeGraph reachesTypeEnv))}
    in Right env{feReachesTypeEnv = reachesTypeEnv', feReachesVPart = reachesVPart,
                 feUniverseKey = u,
                 feInputArrowNames = inputArrowNames}

-- | Probe each constraint in isolation: resolve its view ('showCon'), run
-- 'computeConstraint', and force the full result under a timeout. The hanging one is the
-- diverging compute; force its channels separately to localize.
scanConstraints :: FEnv -> [VConstraint] -> IO ()
scanConstraints env cons = do
  putStrLn $ "== probing " ++ show (length cons) ++ " constraints, "
             ++ show probeTimeoutSecs ++ "s each =="
  forM_ (zip [(0 :: Int) ..] cons) $ \(i, con) -> do
    r <- timeout (probeTimeoutSecs * 1000000) $ evaluate (forceWholeCon env con)
    case r of
      Just n  -> when (n > 2000) $ putStrLn $ show i ++ ": ok but LARGE (" ++ show n ++ " chars)"
      Nothing -> do
        putStrLn $ show i ++ ": *** TIMEOUT *** view=" ++ take 250 (resolvedView env con)
        probeChannels env con
  putStrLn "== scan complete =="

resolvedView :: FEnv -> VConstraint -> String
resolvedView env con = case stypeConstraint (showCon env con) of
  TypeCheckResE _        -> "<unresolvable>"
  TypeCheckResult _ con' -> show (conDat con')

forceWholeCon :: FEnv -> VConstraint -> Int
forceWholeCon env con = case stypeConstraint (showCon env con) of
  TypeCheckResE _        -> 0
  TypeCheckResult _ con' -> let (_, conNew) = computeConstraint env con'
                            in length (show (conDat conNew)) + length (show (conVaenv conNew))

-- | Force the culprit's updated channels one at a time to localize the diverging one.
probeChannels :: FEnv -> VConstraint -> IO ()
probeChannels env con = case stypeConstraint (showCon env con) of
  TypeCheckResE _ -> putStrLn "  (unresolvable view)"
  TypeCheckResult _ con' -> do
    let (_, conNew) = computeConstraint env con'
    forM_ (zip [(0 :: Int) ..] (conSTypes (conDat conNew))) $ \(j, st) ->
      forM_ [("act", stypeAct st), ("req", stypeReq st), ("reqDecl", stypeReqDecl st)] $ \(nm, t) -> do
        r <- timeout (probeTimeoutSecs * 1000000) $ evaluate (length (show t))
        putStrLn $ "  sty" ++ show j ++ "." ++ nm ++ ": "
                   ++ maybe "*** HANGS ***" (\n -> "ok (" ++ show n ++ ")") r

-- | The 'SType's embedded in a constraint's payload ('ConstraintDat' has no Foldable).
conSTypes :: ConstraintDat SType -> [SType]
conSTypes d = case d of
  EqualsKnown _ p _    -> [p]
  EqPoints _ a b       -> [a, b]
  BoundedByKnown _ p _ -> [p]
  BoundedByObjs _ p _  -> [p]
  NoReturnArg _ p      -> [p]
  EqReqAct _ p         -> [p]
  ArrowTo _ a b        -> [a, b]
  PropEq _ (a, _) b    -> [a, b]
  AddArg _ (a, _) b    -> [a, b]
  AddInferArg _ a b    -> [a, b]
  SetArgMode _ _ a b   -> [a, b]
  ConWhere _ a b c     -> [a, b, c]
  UnionOf _ p ps       -> p : ps

-- | Replay the fragment's epochs one constraint at a time, mirroring
-- 'runConstraintsIdx': per-epoch env-cache rebuild, sequential 'executeConstraint' with
-- pruning, 'nextConstrainEpoch' + list reversal between epochs. Epochs 1-3 run without
-- timeouts (they survive in the real build); from epoch 4 on, each step runs under a
-- timeout — the step that hangs is the sequential detonation point. At the hang, scan
-- every point with a short timeout to find the poisoned (unforceable) schemes.
replayEpochs :: FEnv -> [(Int, VConstraint)] -> IO ()
replayEpochs = go (1 :: Int)
  where
    go epochN env cons = do
      putStrLn $ "-- epoch " ++ show epochN ++ " (" ++ show (length cons) ++ " constraints)"
      case prepareEpochEnv env of
        Left notes -> putStrLn $ "epoch env rebuild failed:\n" ++ unlines (map show (take 4 notes))
        Right envE -> do
          let withTimeouts = epochN >= 4
          let mCanary = lookup 109 cons >>= \c -> Just (109 :: Int, c)
          stepRes <- runSteps withTimeouts mCanary envE cons []
          case stepRes of
            Left (cid, con, envAt, k) -> do
              putStrLn $ "*** epoch " ++ show epochN ++ " HANGS at step " ++ show k
                         ++ " (cid=" ++ show cid ++ ") ***"
              putStrLn $ "  raw dat: " ++ take 300 (show (conDat con))
              rv <- timeout (probeTimeoutSecs * 1000000) $ evaluate
                      (length (resolvedView envAt con))
              putStrLn $ "  resolved view: " ++ maybe "*** view itself HANGS ***" show rv
            Right (env', consKept, updated) ->
              if not updated
                then putStrLn $ "converged after epoch " ++ show epochN
                else go (epochN + 1) (nextConstrainEpoch env') (reverse consKept)

    -- Run one epoch's steps; Left = hang (cid, con, env before the step, step index).
    -- With a canary (cid 109, the constraint whose view detonates in the real build):
    -- after every step, probe the canary's resolved view under a short timeout; the FIRST
    -- step after which it hangs is the poisoning write. Report that step's own written
    -- channels and stop.
    runSteps _ _ env [] kept = return (Right (env, reverse kept, feUpdatedDuringEpoch env))
    runSteps wt mCanary env ((cid, con):rest) kept = do
      let step = runStateTC (executeConstraint cid con) env
      r <- if wt
             then timeout (probeTimeoutSecs * 1000000) (evaluate step)
             else Just <$> evaluate step
      case r of
        Nothing -> return $ Left (cid, con, env, length kept)
        Just (Left notes) -> do
          putStrLn $ "  step cid=" ++ show cid ++ " ABORTED: "
                     ++ show (take 2 notes)
          runSteps wt mCanary env rest kept
        Just (Right (prune, env')) -> do
          poisoned <- case (wt, mCanary) of
            (True, Just (ccid, ccon)) | ccid /= cid -> do
              cv <- timeout 3000000 $ evaluate (length (resolvedView env' ccon))
              return (cv == Nothing)
            _ -> return False
          if poisoned
            then do
              putStrLn $ "*** POISONER: step cid=" ++ show cid ++ " (canary view hangs after it) ***"
              putStrLn $ "  poisoner raw dat: " ++ take 300 (show (conDat con))
              let (_, canaryCon) = maybe (error "no canary") id mCanary
              pv <- timeout (probeTimeoutSecs * 1000000) $ evaluate (length (resolvedView env canaryCon))
              putStrLn $ "  canary view BEFORE this step: "
                         ++ maybe "hangs (?!)" (\n -> "ok (" ++ show n ++ " chars)") pv
              probeNarrow env con
              return $ Left (cid, con, env, length kept)
            else runSteps wt mCanary env' rest (if prune then kept else (cid, con) : kept)

-- | Which points hold schemes that can no longer be forced? Those carry the poisoned
-- thunks written by an earlier step of this epoch.
pointScan :: FEnv -> IO ()
pointScan env = do
  putStrLn "  == point scan (3s each; only large or hanging points shown) =="
  forM_ (IM.toList (fePnts env)) $ \(p, sch) -> do
    r <- timeout 3000000 $ evaluate $ case sch of
      TypeCheckResE _    -> -1
      TypeCheckResult _ st -> length (show (stypeAct st)) + length (show (stypeReq st))
                              + length (show (stypeReqDecl st))
    case r of
      Nothing -> putStrLn $ "  pnt " ++ show p ++ ": *** POISONED (hangs) ***"
      Just n | n > 3000 -> putStrLn $ "  pnt " ++ show p ++ ": large (" ++ show n ++ ")"
      _ -> return ()
  putStrLn "  == point scan done =="

-- | Decompose the poisoner's 'narrowChannel' compute (the BoundedByObjs clause of
-- 'computeConstraint', replicated), forcing each sub-expression under a timeout to name
-- the diverging piece and print the (small) operand spellings for a standalone test.
probeNarrow :: FEnv -> VConstraint -> IO ()
probeNarrow env con0 = case stypeConstraint (showCon env con0) of
  TypeCheckResE _ -> putStrLn "  probeNarrow: unresolvable view"
  TypeCheckResult _ con'@(Constraint _ _ vaenv (BoundedByObjs _ SType{stypeReq=pReq, stypeReqDecl=pReqDecl} objMapBoundUb)) -> do
    let feTypeEnvL = feTypeEnv env
    let vaenv' = ambientEnv $ fmap (stypeAct . snd) vaenv
    let argsBoundUb = setArgMode vaenv' PtArgExact $ powersetType feTypeEnvL vaenv' $
          UnionType Nothing (joinUnionType $ map partialToType $ H.keys $ snd $
                             splitVarArgEnv $ constraintVarArgEnv con') []
    let objMapBoundUb' = objMapBoundUb
    forceStep "objMapBoundUb'" (show objMapBoundUb')
    forceStep "argsBoundUb" (show argsBoundUb)
    forM_ [("REQ", ambientEnv $ fmap (stypeReq . snd) vaenv, pReq),
           ("REQDECL", ambientEnv $ fmap (stypeReqDecl . snd) vaenv, pReqDecl)] $ \(chNm, chanVaenv, chanReq) -> do
      putStrLn $ "  -- channel " ++ chNm ++ ": chanReq = " ++ take 400 (show chanReq)
      let ocp = openClassPart feTypeEnvL chanReq
      forceStep (chNm ++ ".openClassPart") (show ocp)
      let boundUbReq = relaxRecursiveSelfArgs feTypeEnvL $
            unionTypes feTypeEnvL (unionTypes feTypeEnvL objMapBoundUb' ocp) argsBoundUb
      forceStep (chNm ++ ".boundUbReq") (show boundUbReq)
      let boundNames = case boundUbReq of
            UnionType _ bLeafs _ -> map ptName (splitUnionType bLeafs)
            _                    -> []
      let nameKnown l = any (relativeNameMatches (ptName l)) boundNames
      case chanReq of
        UnionType mTop leafs consts -> do
          let (knownLeafs, unknownLeafs) = partition nameKnown (splitUnionType leafs)
          putStrLn $ "  " ++ chNm ++ ": " ++ show (length knownLeafs) ++ " known / "
                     ++ show (length unknownLeafs) ++ " unknown leafs; mTop="
                     ++ (case mTop of Just _ -> "Just"; Nothing -> "Nothing")
          forM_ (zip [(0 :: Int) ..] knownLeafs) $ \(i, l) ->
            forceStep (chNm ++ ".metKnown[" ++ show i ++ "] " ++ take 60 (show (ptName l)))
              (show (snd (intersectTypesWithVarEnv feTypeEnvL chanVaenv (singletonType l) boundUbReq)))
          let metTopConsts = snd $ intersectTypesWithVarEnv feTypeEnvL chanVaenv
                                     (UnionType mTop H.empty consts) boundUbReq
          forceStep (chNm ++ ".metTopConsts") (show metTopConsts)
          mtcHangs <- hangs (show metTopConsts)
          when mtcHangs $ do
            mMin <- shrinkBound feTypeEnvL chanVaenv (UnionType mTop H.empty consts) boundUbReq
            case (mMin, mTop) of
              (Just [l], Just (topPreds, _, _)) -> stageProbes feTypeEnvL chanVaenv l topPreds (UnionType mTop H.empty consts)
              _ -> putStrLn "  (minimal bound not a single leaf; skipping stage probes)"
        _ ->
          forceStep (chNm ++ ".direct-meet")
            (show (snd (intersectTypesWithVarEnv feTypeEnvL chanVaenv chanReq boundUbReq)))
  TypeCheckResult _ _ -> putStrLn "  probeNarrow: not a BoundedByObjs"
  where
    forceStep nm str = do
      r <- timeout 8000000 $ evaluate (length str)
      putStrLn $ "  [" ++ nm ++ "] "
                 ++ maybe "*** HANGS ***" (\n -> "ok (" ++ show n ++ " chars)") r

-- | Does forcing this string hang (6s budget)?
hangs :: String -> IO Bool
hangs str = do
  r <- timeout 6000000 $ evaluate (length str)
  return (r == Nothing)

-- | Delta-debug the universe bound down to a minimal leaf subset whose meet against the
-- requirement top still diverges — small enough to hand-write as a real unit test.
shrinkBound :: FEnvTypeEnv -> TypeVarArgEnv -> Type -> Type -> IO (Maybe [PartialType])
shrinkBound te chanVaenv topT bound = case bound of
  UnionType bTop bLeafs bConsts -> do
    let meetOf ls consts' bt = show (snd (intersectTypesWithVarEnv te chanVaenv topT
                                 (UnionType bt (joinUnionType ls) consts')))
    let leaves0 = splitUnionType bLeafs
    putStrLn $ "  == shrinking bound (" ++ show (length leaves0) ++ " leaves) =="
    noConsts <- hangs (meetOf leaves0 [] bTop)
    let consts' = if noConsts then [] else bConsts
    putStrLn $ "  consts needed: " ++ show (not noConsts)
    noTop <- hangs (meetOf leaves0 consts' Nothing)
    let bTop' = if noTop then Nothing else bTop
    putStrLn $ "  bound top needed: " ++ show (not noTop)
    minimal <- ddmin (\ls -> hangs (meetOf ls consts' bTop')) leaves0
    putStrLn $ "  == MINIMAL diverging bound: " ++ show (length minimal) ++ " leaves =="
    forM_ minimal $ \l -> do
      putStrLn $ "    " ++ take 400 (show l)
      putStrLn $ "      name=" ++ show (ptName l) ++ " vars=" ++ show (H.toList (ptVars l))
                 ++ " args=" ++ show (H.toList (ptArgs l)) ++ " preds=" ++ show (ptPreds l)
                 ++ " mode=" ++ show (ptArgMode l)
    return (Just minimal)
  _ -> putStrLn "  shrinkBound: bound not a leaf union" >> return Nothing

-- | Greedy chunked minimization: keep removing chunks while the predicate (still
-- diverges) holds; halve the chunk size when stuck.
ddmin :: ([a] -> IO Bool) -> [a] -> IO [a]
ddmin p = go' Nothing
  where
    go' mk xs = do
      let k = maybe (max 1 (length xs `div` 2)) id mk
      xs' <- tryChunks k xs
      if length xs' < length xs
        then go' (Just (min k (max 1 (length xs' `div` 2)))) xs'
        else if k <= 1 then return xs else go' (Just (max 1 (k `div` 2))) xs
    tryChunks k xs = go2 0 xs
      where
        go2 i cur
          | i * k >= length cur = return cur
          | otherwise = do
              let cand = take (i * k) cur ++ drop ((i + 1) * k) cur
              stillHangs <- if null cand then return False else p cand
              if stillHangs then go2 i cand else go2 (i + 1) cur

-- | With the minimal single-leaf bound in hand: (a) does the meet still hang with an
-- EMPTY vaenv (two-literal repro)? (b) attach the requirement's pred formula to the leaf
-- ('partialAddPreds' — the meet clause's own move) and force each compaction stage
-- individually to name the diverging stage.
stageProbes :: FEnvTypeEnv -> TypeVarArgEnv -> PartialType -> TypePredicates -> Type -> IO ()
stageProbes te chanVaenv l topPreds topT = do
  putStrLn "  == stage probes on the minimal leaf =="
  step "meet with FULL vaenv (control)" $
    show (snd (intersectTypesWithVarEnv te chanVaenv topT (singletonType l)))
  step "meet with EMPTY vaenv" $
    show (snd (intersectTypesWithVarEnv te H.empty topT (singletonType l)))
  let l' = partialAddPreds l topPreds
  putStrLn $ "  pred-attached leaf: " ++ take 300 (show l')
  step "compactDisconnectedPreds" $ show (compactDisconnectedPreds te chanVaenv [l'])
  step "isEqTypeWithEnv l' vs bare" $
    show (isEqTypeWithEnv te chanVaenv (singletonType l') (singletonType l))
  step "isSubtypeOf l' <= bare" $
    show (isSubtypeOfWithEnv te chanVaenv (singletonType l') (singletonType l))
  step "isSubtypeOf bare <= l'" $
    show (isSubtypeOfWithEnv te chanVaenv (singletonType l) (singletonType l'))
  let renamed nm = l'{ptName = mkName nm}
  step "CONTROL undeclared plain name" $
    show (compactDisconnectedPreds te chanVaenv [renamed "/zzUndeclaredCtrl"])
  step "CONTROL declared name Integer" $
    show (compactDisconnectedPreds te chanVaenv [renamed "/Data/Primitive/Integer"])
  let exactMode = l'{ptArgMode = PtArgExact}
  step "CONTROL exact mode (same $B name)" $
    show (compactDisconnectedPreds te chanVaenv [exactMode])
  step "compactModedPredLeafs" $ show (compactModedPredLeafs te [l'])
  step "compactNegPredLeafs" $ show (compactNegPredLeafs te chanVaenv [l'])
  step "compactPartialsWithClassPred" $ show (compactPartialsWithClassPred te chanVaenv [l'])
  step "full compactType" $ show (compactType te chanVaenv (singletonType l'))
  where
    step nm str = do
      r <- timeout 8000000 $ evaluate (length str)
      putStrLn $ "  [" ++ nm ++ "] "
                 ++ maybe "*** HANGS ***" (\n -> "ok (" ++ show n ++ " chars): " ++ take 200 str) r

-- | ONE cold probe per process (select with REPRO_PROBE=<name>): decompose
-- 'compactDisconnectedPreds.aux' on the minimal leaf with genuinely cold memos —
-- in-process probe sequences lie (a hanging probe's partial evaluation warms the token
-- memos and later probes of the same operands then complete). Runs right after the law
-- fragment's ENCODE (no epoch replay): the operands are hardcoded (vaenv-independence and
-- name-independence were established by the in-process controls) and only the static page
-- env is needed.
runColdProbe :: String -> FEnvTypeEnv -> IO ()
runColdProbe sel te = do
  let topPreds = PredsOne (PredClass (partialVal (mkName "/Data/Algebra/Eq")))
  let leaf = (partialVal (mkName "/$B")){ptArgMode = PtArgAny}
  let bare = singletonType leaf
  let leafP = partialAddPreds leaf topPreds
  let vaenv = H.empty :: TypeVarArgEnv
  let meetDis = intersectTypesEnv te{teDisableCompact = True} vaenv bare (TopType H.empty topPreds)
  let probes =
        [ ("meetDis",  show meetDis)
        , ("eq",       show (isEqTypeWithEnv te vaenv meetDis bare))
        , ("sub1",     show (isSubtypeOfWithEnv te vaenv meetDis bare))
        , ("sub2",     show (isSubtypeOfWithEnv te vaenv bare meetDis))
        , ("meetReal", show (snd (intersectTypesWithVarEnv te vaenv (TopType H.empty topPreds) bare)))
        , ("whole",    show (compactDisconnectedPreds te vaenv [leafP]))
        , ("expandFull", show (expandTypesWithNamesFull te vaenv (TopType H.empty topPreds) (S.singleton (mkName "/$B"))))
        , ("eqMembers", showEqMembers te)
        , ("memSemigroup", showClassMembersLen te "/Data/Algebra/Semigroup")
        , ("memGroup", showClassMembersLen te "/Data/Algebra/Group")
        , ("memCM", showClassMembersLen te "/Data/Algebra/CommutativeMonoid")
        ]
  case lookup sel probes of
    Nothing  -> putStrLn ("[COLD] unknown probe: " ++ sel)
    Just str -> do
      t0 <- getCurrentTime
      r <- timeout 30000000 (evaluate (length str))
      t1 <- getCurrentTime
      putStrLn $ "[COLD " ++ sel ++ "] "
                 ++ maybe "*** HANGS (30s) ***" (\n -> "ok (" ++ show n ++ " chars)") r
                 ++ " elapsed=" ++ show (diffUTCTime t1 t0)
      case r of
        Just _  -> putStrLn $ "[COLD " ++ sel ++ " CONTENT] " ++ take 1500 str
        Nothing -> return ()

-- | Eq's raw class-graph entry: member shapes (finite, cheap) — the cycle's static side.
showEqMembers :: FEnvTypeEnv -> String
showEqMembers te = case teClassGraph te of
  ClassGraph cg -> case graphLookup (PClassName (mkName "/Data/Algebra/Eq")) cg of
    Just (CGClass sealed decl classTypes doc) ->
      "sealed=" ++ show sealed ++ " decl=" ++ show decl ++ " members=" ++ show classTypes ++ " doc=" ++ show doc
    r -> "lookup: " ++ show r

-- | Timed length of a class's member list: an infinite (cyclic) list from the ClassGraph
-- merge makes 'length' diverge while a bounded 'take' completes.
showClassMembersLen :: FEnvTypeEnv -> String -> String
showClassMembersLen te nm = case teClassGraph te of
  ClassGraph cg -> case graphLookup (PClassName (mkName nm)) cg of
    Just (CGClass _ _ classTypes _) ->
      "first50=" ++ take 400 (show (take 50 classTypes))
      ++ " | len=" ++ show (length classTypes)
    r -> "lookup: " ++ show r
