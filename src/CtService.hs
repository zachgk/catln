--------------------------------------------------------------------
-- |
-- Module    :  CtService
-- Copyright :  (c) Zach Kimberg 2024
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is a top-level manager for the compilation pipeline.
-- It supports partial and incremental compilation and rebuilding, for both the Main build and pieces like webdocs or IDE integration.
--------------------------------------------------------------------

module CtService where
import           Control.Concurrent      (MVar, modifyMVar_, newMVar, readMVar,
                                          swapMVar)
import           Control.Monad           (forM, forM_, unless)
import           Control.Monad.Trans     (lift)
import           CRes
import           Data.Graph
import qualified Data.HashMap.Strict     as H
import qualified Data.HashSet            as S
import           Data.List               (partition)
import           Data.Maybe
import           Eval                    (evalAnnots, evalBuild, evalBuildAll,
                                          evalRun, prgmFromGraphData)
import           Eval.Common             (EvalMetaDat, EvalResult,
                                          StagePrgms (..), TExpr,
                                          Val (StrVal, TupleVal))
import           Eval.Testing            (MockIO, evalPrints, evalRunMock,
                                          evalTest)
import           Semantics               (CTSSConfig)
import           Semantics.Prgm
import           Syntax.Ct.Desugarf      (desFinalPasses, desPrgm, validPrgm)
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Syntax.Parsers
import           Text.Printf
import           TypeCheck               (typecheckPrgms)
import           TypeCheck.Common        (TPrgm, TraceConstrain, VPrgm,
                                          typeCheckToRes)
import           Utils

type TBPrgm = Prgm TExpr EvalMetaDat

newtype CTSS = CTSS (MVar CTSSDat)

-- Main data type for CtService Status
data CTSSDat = CTSSDat {
  ctssConfig        :: CTSSConfig,
  ctssBaseFileNames :: [String],
  ctssData          :: GraphData SSF FileImport
                                       }

data SSF = SSF {
  ssfSource         :: Maybe String,
  ssfRaw            :: CRes (RawPrgm ParseMetaDat),
  ssfBuilt          :: Bool,
  ssfDes            :: Maybe (CRes (Prgm Expr ParseMetaDat)),
  ssfTPrgmWithTrace :: Maybe (CRes (TPrgm, VPrgm, TraceConstrain)),
  ssfTPrgm          :: Maybe (CRes TPrgm),
  ssfTBPrgm         :: Maybe (CRes TBPrgm),
  ssfAnnots         :: Maybe (CRes [(Expr EvalMetaDat, Val)])
               }

emptyCTSS :: CTSSConfig -> IO CTSS
emptyCTSS config = do
  v <- newMVar $ CTSSDat config [] (graphFromEdges [])
  return $ CTSS v

newSSF :: Maybe String -> CRes (RawPrgm ParseMetaDat) -> SSF
newSSF src rawPrgm = SSF src rawPrgm False Nothing Nothing Nothing Nothing Nothing

ctssSetFiles :: CTSS -> [String] -> IO ()
ctssSetFiles (CTSS ssmv) baseFileNames = do
  modifyMVar_ ssmv $ \ctss -> return ctss{ctssBaseFileNames=baseFileNames}
  ctssRead (CTSS ssmv)

ctssBaseFiles :: CTSSConfig -> [String] -> IO CTSS
ctssBaseFiles config baseFileNames = do
  v <- newMVar $ CTSSDat config baseFileNames (graphFromEdges [])
  let ctss = CTSS v
  ctssRead ctss
  return ctss

ctssRead :: CTSS -> IO ()
ctssRead (CTSS ssmv) = do
  ss@CTSSDat{ctssConfig, ctssBaseFileNames, ctssData} <- readMVar ssmv
  rawPrgms <- readFiles ctssConfig (map mkRawImportStr ctssBaseFileNames)
  let (data', invalidations) = unzip $ map (buildSSF ctssData) $ graphToNodes rawPrgms
  let ss' = ss{ctssData=graphFromEdges data'}
  _ <- swapMVar ssmv $ ctssClearInvalidations (catMaybes invalidations) ss'
  return ()
  where
    buildSSF ctssData ((rawPrgm, src), prgmName, imports) = case graphLookup prgmName ctssData of
      Just old@SSF{ssfRaw=CRes _ oldRaw} | oldRaw == rawPrgm -> ((old, prgmName, imports), Nothing)
      Just{} -> ((newSSF src $ pure rawPrgm, prgmName, imports), Just prgmName)
      Nothing -> ((newSSF src $ pure rawPrgm, prgmName, imports), Nothing)

ctssClearInvalidations :: [FileImport] -> CTSSDat -> CTSSDat
ctssClearInvalidations invalidations ss@CTSSDat{ctssData=(g, nodeFromVertex, vertexFromKey)} = ss{ctssData=fmapGraphWithKey maybeClear $ ctssData ss}
  where
    propagated = map (snd3 . nodeFromVertex) $ foldMap (foldMap (:[])) $ dfs (transposeG g) (mapMaybe vertexFromKey invalidations)
    maybeClear prgmName ssf = if prgmName `elem` propagated
      then newSSF (ssfSource ssf) (ssfRaw ssf)
      else ssf

-- | Build pages incrementally, one SCC at a time.
-- Each successfully built SCC is committed to the MVar independently,
-- so earlier SCCs (e.g. core library) remain cached even if a later SCC fails.
ctssBuildPagesExact :: CTSS -> GraphData SSF FileImport -> IO ()
ctssBuildPagesExact _ pages | all (ssfBuilt . fst3) (graphToNodes pages) = return ()
ctssBuildPagesExact (CTSS ssmv) pages = do
  -- stronglyConnCompR returns SCCs in reverse topological order (dependencies first)
  let sccs = stronglyConnCompR $ graphToNodes pages
  forM_ sccs $ \comp -> do
    let sccNodes = flattenSCC comp
    unless (all (ssfBuilt . fst3) sccNodes) $
      ctssBuildSCC ssmv sccNodes pages

-- | Build a single SCC and commit the result to the MVar atomically.
ctssBuildSCC :: MVar CTSSDat -> [(SSF, FileImport, [FileImport])] -> GraphData SSF FileImport -> IO ()
ctssBuildSCC ssmv sccNodes pages = do
  let sccFileNames = map snd3 sccNodes
  let sccNamesSet = S.fromList sccFileNames
  -- Get dep lists from the pages graph structure (stable across MVar updates)
  let pageDeps = H.fromList [(name, deps) | (_, name, deps) <- graphToNodes pages]
  modifyMVar_ ssmv $ \ctssdat@CTSSDat{ctssData} -> do
    -- Re-check with current state (another thread may have built this SCC)
    let currentSSFs = mapMaybe (`graphLookup` ctssData) sccFileNames
    if all ssfBuilt currentSSFs
      then return ctssdat
      else do
        -- Look up current SSFs and deps for SCC members
        let currentSCCNodes = mapMaybe (\name -> do
              ssf <- graphLookup name ctssData
              let deps = fromMaybe [] $ H.lookup name pageDeps
              return (ssf, name, deps)) sccFileNames
        p <- runCResT $ buildSCCPipeline ctssData sccNamesSet currentSCCNodes pages
        case p of
          CRes notes updates -> do
            unless (null notes) $ putStrLn $ prettyCNotes notes
            let updatedData = fmapGraphWithKey (\k ssf -> fromMaybe ssf (H.lookup k updates)) ctssData
            return ctssdat{ctssData = updatedData}
          CErr notes -> do
            unless (null notes) $ putStrLn $ prettyCNotes notes
            fail "Could not build catln service"

-- | Run the full pipeline (desugar → typecheck → build → annotate) for a single SCC.
-- Returns a map of updated SSFs for the SCC members.
buildSCCPipeline :: GraphData SSF FileImport -> S.HashSet FileImport
                 -> [(SSF, FileImport, [FileImport])] -> GraphData SSF FileImport
                 -> CResT IO (H.HashMap FileImport SSF)
buildSCCPipeline ctssData sccNamesSet sccNodes pages = do
  -- Get raw programs for all SCC members
  rawPrgms <- forM sccNodes $ \(ssf, name, deps) -> do
    raw <- asCResT $ ssfRaw ssf
    return (raw, ssf, name, deps)

  -- Separate valid programs from ctx-annotated ones (which skip the pipeline)
  let (validRaws, invalidRaws) = partition (\(raw, _, _, _) -> validPrgm raw) rawPrgms
  let invalidUpdates = [(name, ssf{ssfBuilt=True}) | (_, ssf, name, _) <- invalidRaws]

  if null validRaws
    then return $ H.fromList invalidUpdates
    else do
      let namesAndDeps = [(name, deps) | (_, _, name, deps) <- validRaws]

      -- Step 1: Desugar each raw program individually
      desResults <- forM validRaws $ \(raw, _, name, deps) ->
        asCResT $ desPrgm (raw, name, deps)

      -- Step 2: Collect data from already-built transitive dependencies
      -- All non-SCC nodes in `pages` are potential external deps
      let extDepNames = filter (not . (`S.member` sccNamesSet)) $
            map snd3 $ graphToNodes pages
      let extDepDes = mapMaybe (\name ->
            graphLookup name ctssData >>= ssfDes >>= cresToMaybe
            ) extDepNames

      -- Step 3: Run final desugar passes with SCC peers and external deps
      let desNodesForFinal = [(des, peerDes, extDepDes)
            | (des, name, _) <- desResults
            , let peerDes = [d | (d, n, _) <- desResults, n /= name]]
      finalDes <- desFinalPasses desNodesForFinal

      -- Step 4: Collect typechecked data from already-built dependencies
      let extDepTC = mapMaybe (\name ->
            graphLookup name ctssData >>= ssfTPrgmWithTrace >>= cresToMaybe
            ) extDepNames

      -- Step 5: Typecheck with SCC peers and external deps
      let tcNodesForTC = [(des, peerDes, extDepTC)
            | (des, (name, _)) <- zip finalDes namesAndDeps
            , let peerDes = [d | (d, (n, _)) <- zip finalDes namesAndDeps, n /= name]]
      let tcCRes = typeCheckToRes $ typecheckPrgms tcNodesForTC
      let tcNotes = getCNotes tcCRes
      tcResults <- asCResT tcCRes

      -- Step 6: Construct TPrgm graph for eval (SCC results + built dep TPrgms)
      let sccTPrgmNodes = [( fst3 tc, name, deps)
            | (tc, (name, deps)) <- zip tcResults namesAndDeps]
      let depTPrgmNodes = mapMaybe (\(_, name, deps) ->
            if name `S.member` sccNamesSet then Nothing
            else do
              ssf <- graphLookup name ctssData
              ssfTPrgm ssf >>= cresToMaybe >>= \t -> Just (t, name, deps)
            ) (graphToNodes pages)
      let tprgmGraph = graphFromEdges (sccTPrgmNodes ++ depTPrgmNodes)

      -- Step 6b: The same graph at the desugar stage. Eval cannot reach this stage on its own, so
      -- it has to be assembled here to be handed to it, alongside the typecheck graph above.
      let sccDesNodes = [(des, name, deps)
            | (des, (name, deps)) <- zip finalDes namesAndDeps]
      let depDesNodes = mapMaybe (\(_, name, deps) ->
            if name `S.member` sccNamesSet then Nothing
            else do
              ssf <- graphLookup name ctssData
              ssfDes ssf >>= cresToMaybe >>= \d -> Just (d, name, deps)
            ) (graphToNodes pages)
      let desGraph = graphFromEdges (sccDesNodes ++ depDesNodes)

      -- Step 7: Eval build all (processes SCC + deps, we extract SCC results).
      -- Tolerate failure: a type-erroneous program cannot build a tree, but we
      -- still want to cache the desugar/typecheck artifacts (including the trace)
      -- for tooling. Any eval-build error is deferred into ssfTBPrgm below.
      let tbprgmGraphRes = evalBuildAll tprgmGraph

      -- Step 8: Eval annotations per SCC file (kept lazy to match original behavior,
      -- since evalAnnots may reference classes not in the class graph for #noCore files).
      -- The other stages are scoped to the same files as the typecheck program the annotations are
      -- evaluated against, and stay unforced for the same reason the annotations do: a page that
      -- never reads a stage must not pay to build it.
      -- The type inference comes straight from step 5 rather than being reassembled: it is that
      -- page's own typecheck result, which is the only scope where its point numbers mean anything.
      let stages name typeInferred = StagePrgms (prgmFromGraphData name desGraph)
                                                (tbprgmGraphRes >>= prgmFromGraphData name)
                                                typeInferred
      let annotsMap = H.fromList [(name, evalAnnots (stages name (CRes tcNotes (v, t))) name tprgmGraph)
                                 | ((_, v, t), (name, _)) <- zip tcResults namesAndDeps]

      -- Step 9: Assemble updated SSFs
      let validUpdates = [(name, ssf{
              ssfBuilt = True,
              ssfDes = Just $ pure des,
              -- Carries the notes but is deliberately not failOnErrorNotes: the whole point of
              -- keeping the trace is to explain a program that failed to typecheck, and the notes
              -- are what say why, so a type inference view needs both together.
              ssfTPrgmWithTrace = Just $ CRes tcNotes tc,
              -- failOnErrorNotes so eval consumers (run/test/build) cleanly reject
              -- a type-erroneous program, while ssfTPrgmWithTrace still carries the trace.
              ssfTPrgm = Just $ failOnErrorNotes $ CRes tcNotes (fst3 tc),
              ssfTBPrgm = case tbprgmGraphRes of
                CRes _ g  -> fmap pure $ graphLookup name g
                CErr errs -> Just $ CErr errs,
              ssfAnnots = H.lookup name annotsMap
            })
            | ((_, ssf, name, _), des, tc) <-
                zip3 validRaws finalDes tcResults]

      return $ H.fromList (validUpdates ++ invalidUpdates)

ctssBuildFrom :: CTSS -> FileImport -> IO ()
ctssBuildFrom ctss@(CTSS ssmv) src = do
  CTSSDat{ctssData} <- readMVar ssmv
  ctssBuildPagesExact ctss (graphFilterReaches src ctssData)

ctssBuildAll :: CTSS -> IO ()
ctssBuildAll ctss@(CTSS ssmv) = do
  CTSSDat{ctssData} <- readMVar ssmv
  ctssBuildPagesExact ctss ctssData

ctssKeys :: CTSS -> IO [FileImport]
ctssKeys (CTSS ssmv) = do
  CTSSDat{ctssData} <- readMVar ssmv
  return $ map snd3 $ graphToNodes ctssData

ctssGet :: (SSF -> Maybe (CRes a)) -> CTSS -> CResT IO (GraphData a FileImport)
ctssGet f (CTSS ssmv) = do
  lift $ ctssBuildAll (CTSS ssmv)
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  let computed = graphFromEdges $ filter (isJust . f . fst3) $ graphToNodes ctssData
  asCResT $ mapMGraph (requireComputed . f) computed

ctssGetFrom :: (SSF -> Maybe (CRes a)) -> FileImport -> CTSS -> CResT IO (GraphData a FileImport)
ctssGetFrom f prgmName (CTSS ssmv) = do
  lift $ ctssBuildFrom (CTSS ssmv) prgmName
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  asCResT $ mapMGraph (requireComputed . f) (graphFilterReaches prgmName ctssData)

requireComputed :: Maybe (CRes r) -> CRes r
requireComputed = fromMaybe (CErr [MkCNote $ GenCErr Nothing "Value not computed in catln service"])

ctssGetJoined :: (Monoid a) => (SSF -> Maybe (CRes a)) -> CTSS -> CResT IO a
ctssGetJoined f ctss = do
  graph <- ctssGet f ctss
  return $ mconcat $ map fst3 $ graphToNodes graph

ctssLookup :: (SSF -> Maybe (CRes a)) -> FileImport -> CTSS -> CResT IO (Maybe a)
ctssLookup f k ctss = do
  graph <- ctssGetFrom f k ctss
  return $ graphLookup k graph

ctssLookupReq :: (SSF -> Maybe (CRes a)) -> FileImport -> CTSS -> CResT IO a
ctssLookupReq f k ctss = do
  mv <- ctssLookup f k ctss
  case mv of
    Just v  -> return v
    Nothing -> fail $ printf "Could not find %s" (show $ impRaw k)

-- This does not use ctssGet because it doesn't require building
getSource :: CTSS -> FileImport -> CResT IO String
getSource (CTSS ssmv) prgmName = do
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  case graphLookup prgmName ctssData of
    Just SSF{ssfSource=Just src} -> return src
    Just SSF{ssfSource=Nothing}  -> fail $ printf "No source text available for %s" (show prgmName)
    Nothing                      -> fail $ printf "Could not find %s" (show $ impRaw prgmName)

-- | Overwrite the source file backing a page, then re-read so the change is picked up.
-- Only pages already in the service graph with source text can be written, which confines
-- writes to the files served by the doc roots.
setSource :: CTSS -> FileImport -> String -> CResT IO ()
setSource ctss@(CTSS ssmv) prgmName src = do
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  case graphLookup prgmName ctssData of
    Nothing -> fail $ printf "Could not find %s" (show $ impRaw prgmName)
    Just SSF{ssfSource=Nothing} -> fail $ printf "No source text available for %s" (show prgmName)
    Just SSF{ssfSource=Just _} -> case impDisp prgmName of
      Nothing -> fail $ printf "No file path available for %s" (show prgmName)
      Just filePath -> lift $ do
        writeFile filePath src
        ctssRead ctss

-- This does not use ctssGet because it doesn't require building
getRawPrgm :: CTSS -> CResT IO PPrgmGraphData
getRawPrgm (CTSS ssmv) = do
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  asCResT $ mapMGraph ssfRaw ctssData

-- | Like getRawPrgm but only returns the subgraph reachable from a given file.
getRawPrgmFrom :: FileImport -> CTSS -> CResT IO PPrgmGraphData
getRawPrgmFrom prgmName (CTSS ssmv) = do
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  asCResT $ mapMGraph ssfRaw (graphFilterReaches prgmName ctssData)

getTreebug :: CTSS -> FileImport -> String -> CResT IO EvalResult
getTreebug ctss prgmName fun = do
  base <- ctssGetFrom ssfTPrgm prgmName ctss
  snd <$> evalRun fun prgmName base

getEvaluated :: CTSS -> FileImport -> String -> CResT IO Integer
getEvaluated ctss prgmName fun = do
  base <- ctssGetFrom ssfTPrgm prgmName ctss
  fst <$> evalRun fun prgmName base

getEvaluatedMock :: CTSS -> MockIO -> FileImport -> String -> CResT IO Integer
getEvaluatedMock ctss mockIO prgmName fun = do
  base <- ctssGetFrom ssfTPrgm prgmName ctss
  fst <$> evalRunMock mockIO fun prgmName base

getEvalBuild :: CTSS -> FileImport -> String -> CResT IO Val
getEvalBuild ctss prgmName fun = do
  base <- ctssGetFrom ssfTPrgm prgmName ctss
  fst <$> evalBuild fun prgmName base

getTestResults :: CTSS -> CResT IO [(String, CResT IO Val)]
getTestResults ctss = do
  base <- ctssGet ssfTPrgm ctss
  asCResT $ evalTest base

-- | The freshly-evaluated golden values for every string-serializable @#print@ reachable from the
-- loaded program, each keyed by the source range of its @p@ argument. Used by @catln test --update@.
getPrintGoldens :: CTSS -> CResT IO [(CodeRangeDat, String)]
getPrintGoldens ctss = do
  base <- ctssGet ssfTPrgm ctss
  asCResT $ evalPrints $ mconcat $ map fst3 $ graphToNodes base

getEvalAnnots :: CTSS -> FileImport -> CResT IO [(Expr EvalMetaDat, Val)]
getEvalAnnots ctss prgmName = do
  annotsGraph <- ctssGetFrom ssfAnnots prgmName ctss
  return $ fromMaybe [] $ graphLookup prgmName annotsGraph

getWeb :: CTSS -> FileImport -> String -> CResT IO String
getWeb ctss prgmName fun = do
  base <- ctssGetFrom ssfTPrgm prgmName ctss
  (TupleVal _ args, _) <- evalBuild fun prgmName base
  case H.lookup "contents" args of
    Just (StrVal s) -> return s
    _               -> return "";

-- Helpers

getPrgm :: CTSS -> CResT IO (GraphData DesPrgm FileImport)
getPrgm = ctssGet ssfDes

getTPrgmWithTrace :: CTSS -> CResT IO (GraphData (TPrgm, VPrgm, TraceConstrain) FileImport)
getTPrgmWithTrace = ctssGet ssfTPrgmWithTrace

getTPrgm :: CTSS -> CResT IO (GraphData TPrgm FileImport)
getTPrgm = ctssGet ssfTPrgm

getTPrgmJoined :: CTSS -> CResT IO TPrgm
getTPrgmJoined = ctssGetJoined ssfTPrgm

getTBPrgm :: CTSS -> CResT IO (GraphData TBPrgm FileImport)
getTBPrgm = ctssGet ssfTBPrgm

-- | The desugar and tree build programs of every loaded page, joined and left unforced. This is the
-- 'StagePrgms' for an evaluation that is not scoped to one page, and so reaches all of them.
--
-- It cannot go through 'ctssGetJoined': that runs in @CResT IO@ and so forces its result, and
-- merely inspecting 'ssfTBPrgm' forces 'evalBuildAll' over every page. Keeping the join a thunk is
-- what lets a caller that never reads a stage avoid building it.
--
-- There is no type inference to report here. Points are numbered per typecheck run, so the traces of
-- every page joined together would be a set of colliding numbers rather than an answer; an
-- evaluation that wants one has to name the page it is asking about ('ctssStagePrgmsFrom').
ctssStagePrgms :: CTSS -> IO StagePrgms
ctssStagePrgms ctss@(CTSS ssmv) = do
  ctssBuildAll ctss
  CTSSDat{ctssData} <- readMVar ssmv
  let ssfs = map fst3 $ graphToNodes ctssData
  return $ StagePrgms (joinedStage ssfDes ssfs) (joinedStage ssfTBPrgm ssfs) notScoped
  where
    notScoped = CErr [MkCNote $ GenCErr Nothing "This evaluation is not scoped to a single page, so it has no type inference to report on"]

-- | Like 'ctssStagePrgms' but for an evaluation scoped to one page: every stage covers that page and
-- what it imports, and the type inference is that page's own, which is the only scope its points are
-- numbered in.
ctssStagePrgmsFrom :: CTSS -> FileImport -> IO StagePrgms
ctssStagePrgmsFrom ctss@(CTSS ssmv) prgmName = do
  ctssBuildFrom ctss prgmName
  CTSSDat{ctssData} <- readMVar ssmv
  let reaches = graphFilterReaches prgmName ctssData
  let ssfs = map fst3 $ graphToNodes reaches
  let typeInferred = do
        (_, vprgm, trace) <- requireComputed $ ssfTPrgmWithTrace =<< graphLookup prgmName ctssData
        return (vprgm, trace)
  return $ StagePrgms (joinedStage ssfDes ssfs) (joinedStage ssfTBPrgm ssfs) typeInferred

-- | Joins one stage over a set of pages, left unforced. See 'ctssStagePrgms' for why it must stay a
-- thunk.
joinedStage :: (Monoid a) => (SSF -> Maybe (CRes a)) -> [SSF] -> CRes a
joinedStage f ssfs = fmap mconcat $ sequence $ mapMaybe f ssfs
