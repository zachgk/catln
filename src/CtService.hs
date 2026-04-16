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
                                          evalRun, evalTest)
import           Eval.Common             (EvalMetaDat, EvalResult, TExpr,
                                          Val (StrVal, TupleVal))
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
            graphLookup name ctssData >>= ssfDes >>= cresJust
            ) extDepNames

      -- Step 3: Run final desugar passes with SCC peers and external deps
      let desNodesForFinal = [(des, peerDes, extDepDes)
            | (des, name, _) <- desResults
            , let peerDes = [d | (d, n, _) <- desResults, n /= name]]
      finalDes <- desFinalPasses desNodesForFinal

      -- Step 4: Collect typechecked data from already-built dependencies
      let extDepTC = mapMaybe (\name ->
            graphLookup name ctssData >>= ssfTPrgmWithTrace >>= cresJust
            ) extDepNames

      -- Step 5: Typecheck with SCC peers and external deps
      let tcNodesForTC = [(des, peerDes, extDepTC)
            | (des, (name, _)) <- zip finalDes namesAndDeps
            , let peerDes = [d | (d, (n, _)) <- zip finalDes namesAndDeps, n /= name]]
      tcResults <- asCResT $ typeCheckToRes $ typecheckPrgms tcNodesForTC

      -- Step 6: Construct TPrgm graph for eval (SCC results + built dep TPrgms)
      let sccTPrgmNodes = [( fst3 tc, name, deps)
            | (tc, (name, deps)) <- zip tcResults namesAndDeps]
      let depTPrgmNodes = mapMaybe (\(_, name, deps) ->
            if name `S.member` sccNamesSet then Nothing
            else do
              ssf <- graphLookup name ctssData
              ssfTPrgm ssf >>= cresJust >>= \t -> Just (t, name, deps)
            ) (graphToNodes pages)
      let tprgmGraph = graphFromEdges (sccTPrgmNodes ++ depTPrgmNodes)

      -- Step 7: Eval build all (processes SCC + deps, we extract SCC results)
      tbprgmGraph <- asCResT $ evalBuildAll tprgmGraph

      -- Step 8: Eval annotations per SCC file (kept lazy to match original behavior,
      -- since evalAnnots may reference classes not in the class graph for #noCore files)
      let annotsMap = H.fromList [(name, evalAnnots name tprgmGraph) | (name, _) <- namesAndDeps]

      -- Step 9: Assemble updated SSFs
      let validUpdates = [(name, ssf{
              ssfBuilt = True,
              ssfDes = Just $ pure des,
              ssfTPrgmWithTrace = Just $ pure tc,
              ssfTPrgm = Just $ pure (fst3 tc),
              ssfTBPrgm = fmap pure $ graphLookup name tbprgmGraph,
              ssfAnnots = H.lookup name annotsMap
            })
            | ((_, ssf, name, _), des, tc) <-
                zip3 validRaws finalDes tcResults]

      return $ H.fromList (validUpdates ++ invalidUpdates)

-- | Extract a successful result from CRes, returning Nothing on error.
cresJust :: CRes a -> Maybe a
cresJust (CRes _ a) = Just a
cresJust (CErr _)   = Nothing

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

getEvalBuild :: CTSS -> FileImport -> String -> CResT IO Val
getEvalBuild ctss prgmName fun = do
  base <- ctssGetFrom ssfTPrgm prgmName ctss
  fst <$> evalBuild fun prgmName base

getTestResults :: CTSS -> CResT IO [(String, CRes Val)]
getTestResults ctss = do
  base <- ctssGet ssfTPrgm ctss
  asCResT $ evalTest base

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

getTBPrgmJoined :: CTSS -> CResT IO TBPrgm
getTBPrgmJoined = ctssGetJoined ssfTBPrgm
