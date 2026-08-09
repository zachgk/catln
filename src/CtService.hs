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
import           Control.Monad           (forM, forM_, unless, when)
import           Control.Monad.Trans     (lift)
import           CRes
import           CtConstants             (testAnnot)
import           Data.Graph
import qualified Data.HashMap.Strict     as H
import qualified Data.HashSet            as S
import           Data.List               (intercalate, partition)
import           Data.Maybe
import           Data.UUID               (UUID)
import           Eval                    (evalAnnots, evalBuild, evalBuildAll,
                                          evalDebugPrgm, evalRun, evalRunMock)
import           Eval.Common             (EvalMetaDat, MockIO, StagePrgms (..),
                                          TExpr, TestResult,
                                          Val (StrVal, TupleVal))
import           Eval.Testing            (TestDesc (..), evalPrints, evalTest,
                                          oaTestId, runTestDesc)
import           Semantics               (CTSSConfig)
import           Semantics.Annots        (hasAnnot)
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
  ssfAnnots         :: Maybe (CRes [(Expr EvalMetaDat, Val)]),
  -- | The tests this page wrote, run. Running a test is not part of building a page, so this is
  -- filled in when something asks for it (see 'ctssTestsFrom') rather than by the pipeline.
  ssfTests          :: Maybe (CRes [TestResult])
               }

emptyCTSS :: CTSSConfig -> IO CTSS
emptyCTSS config = do
  v <- newMVar $ CTSSDat config [] (graphFromEdges [])
  return $ CTSS v

newSSF :: Maybe String -> CRes (RawPrgm ParseMetaDat) -> SSF
newSSF src rawPrgm = SSF { ssfSource = src
                         , ssfRaw = rawPrgm
                         , ssfBuilt = False
                         , ssfDes = Nothing
                         , ssfTPrgmWithTrace = Nothing
                         , ssfTPrgm = Nothing
                         , ssfTBPrgm = Nothing
                         , ssfAnnots = Nothing
                         , ssfTests = Nothing
                         }

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
    maybeClear prgmName ssf
      | prgmName `elem` propagated = newSSF (ssfSource ssf) (ssfRaw ssf)
      | null invalidations = ssf
      -- Which tests a page has, and what they find, is decided over every loaded page joined
      -- together rather than over the page and its imports (see 'ctssTestsFrom'). Invalidation
      -- walks the pages that DEPEND on the changed one, so it never reaches a page the change is
      -- downstream of — whose tests the change can still alter. Every page's tests are therefore
      -- dropped by any change at all, and re-run for whichever page is asked about next.
      | otherwise = ssf{ssfTests = Nothing}

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

      -- Step 7: Eval build all (processes SCC + deps, we extract SCC results).
      -- Tolerate failure: a type-erroneous program cannot build a tree, but we
      -- still want to cache the desugar/typecheck artifacts (including the trace)
      -- for tooling. Any eval-build error is deferred into ssfTBPrgm below.
      let tbprgmGraphRes = evalBuildAll tprgmGraph

      -- Step 8: Assemble updated SSFs
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
                CErr errs -> Just $ CErr errs
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

-- | The page a service opens at: the first file it was started with. Which page that is, is the
-- service's own knowledge, and a reader of it would otherwise have to guess it out of a list of
-- every page. Resolved the same way a page asked for by name is, so it is a page that can be asked
-- for; a service started with no files at all has none.
ctssRootPage :: CTSS -> IO (Maybe String)
ctssRootPage (CTSS ssmv) = do
  CTSSDat{ctssConfig, ctssBaseFileNames} <- readMVar ssmv
  case ctssBaseFileNames of
    []       -> return Nothing
    (name:_) -> impDisp <$> mkDesCanonicalImportStr ctssConfig name

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

getTestResults :: CTSS -> CResT IO [TestDesc]
getTestResults ctss = do
  base <- ctssGet ssfTPrgm ctss
  asCResT $ evalTest base

-- | The tests a page wrote, run and kept.
--
-- Which tests exist at all is decided over the whole PROGRAM the page belongs to, NOT over the page
-- and what it imports. A class law is checked against the instances of that class, and those live in
-- the files that define them — which are downstream of the file stating the law. @/Data/Algebra@
-- states that @++@ is associative and imports nothing that is a @Semigroup@; deriving its tests from
-- what it can reach would leave the law quantifying over nothing and passing vacuously. Likewise an
-- agreement test pairs two definitions of one function, which two unrelated files can each hold half
-- of. See 'ctssTestPages' for what "the program the page belongs to" means.
--
-- Each derived test is then attributed to the page that wrote the definition it came from, and only
-- that page's are run here. So the answer is the same set of tests @catln test@ runs on that program,
-- sliced by who wrote them, and asking about one page costs that page's tests rather than every
-- page's.
ctssTestsFrom :: CTSS -> FileImport -> CResT IO [TestResult]
ctssTestsFrom ctss prgmName = do
  cached <- lift $ ctssCached ssfTests prgmName ctss
  case cached of
    Just tests -> asCResT tests
    Nothing -> do
      tprgms <- ctssTestPages ctss prgmName
      descs <- asCResT $ evalTest tprgms
      let owners = pageOwners tprgms
      -- Every test comes from a definition and every definition was written in a page, so a test
      -- that cannot be placed is reported rather than dropped — dropped, it would be a test that
      -- belongs to no page and so is run by nobody.
      case [tdName d | d <- descs, not (H.member (tdOwnerId d) owners)] of
        []      -> return ()
        orphans -> fail $ printf "Could not find the page that wrote %s" (intercalate ", " orphans)
      results <- lift $ mapM runTestDesc [d | d <- descs, H.lookup (tdOwnerId d) owners == Just prgmName]
      lift $ ctssStore prgmName (\ssf -> ssf{ssfTests = Just $ pure results}) ctss
      return results

-- | The pages a page's tests are decided over: every page reached by a file the service was asked to
-- serve that reaches this page.
--
-- Not every loaded page. A service can be asked to serve several files that are separate programs —
-- the integration tests load thirty of them — and a page belongs to the ones that reach it, not to
-- the ones beside it. Joining programs that were never one is also what the compiler cannot yet do:
-- @buildSCCPipeline@ offers every loaded page as an external dependency of every other, so an
-- unrelated file's @abs@ collides with a test file's own.
--
-- Not just what the page imports either, which is the point of 'ctssTestsFrom' above. Asked to serve
-- the standard library, the file it is asked for reaches every page of it, so a law stated in one
-- file is checked against the instances defined in another.
ctssTestPages :: CTSS -> FileImport -> CResT IO (GraphData TPrgm FileImport)
ctssTestPages ctss@(CTSS ssmv) prgmName = do
  CTSSDat{ctssConfig, ctssBaseFileNames, ctssData} <- lift $ readMVar ssmv
  roots <- lift $ mapM (mkDesCanonicalImportStr ctssConfig) ctssBaseFileNames
  let reaching = [root | root <- roots, isJust $ graphLookup prgmName $ graphFilterReaches root ctssData]
  when (null reaching) $
    fail $ printf "No file this service was asked to serve reaches %s, so it is in no program to take the tests of" (show $ impRaw prgmName)
  mapM_ (lift . ctssBuildFrom ctss) reaching
  CTSSDat{ctssData=built} <- lift $ readMVar ssmv
  let scope = S.fromList $ concat [map snd3 $ graphToNodes $ graphFilterReaches root built | root <- reaching]
  asCResT $ mapMGraph (requireComputed . ssfTPrgm) $ graphFromEdges
    [node | node@(_, name, _) <- graphToNodes built, name `S.member` scope]

-- | Which page each definition was written in, keyed the way a test names the definition it was
-- derived from. A page's own program holds what that page wrote; the joined program the tests are
-- derived over has lost which file anything came from.
pageOwners :: GraphData TPrgm FileImport -> H.HashMap UUID FileImport
pageOwners tprgms = H.fromList
  [ (i, name)
  | (Prgm objMap _ globalAnnots, name, _) <- graphToNodes tprgms
  , i <- map oaTestId (flatObjectMap objMap) ++ map (getMetaID . getExprMeta) globalAnnots
  ]

-- | The freshly-evaluated golden values for every string-serializable @#print@ reachable from the
-- loaded program, each keyed by the source range of its @p@ argument. Used by @catln test --update@.
getPrintGoldens :: CTSS -> CResT IO [(CodeRangeDat, String)]
getPrintGoldens ctss = do
  base <- ctssGet ssfTPrgm ctss
  asCResT $ evalPrints $ mconcat $ map fst3 $ graphToNodes base

-- | The annotations a page writes, evaluated to the values they display as.
--
-- They are evaluated when something asks to see them rather than when the page is built, so a build
-- that displays nothing — a run, a compile, an editor session — does not pay for them. The result is
-- kept on the page, so the second reader of a page gets the first reader's answer.
getEvalAnnots :: CTSS -> FileImport -> CResT IO [(Expr EvalMetaDat, Val)]
getEvalAnnots ctss prgmName = do
  cached <- lift $ ctssCached ssfAnnots prgmName ctss
  case cached of
    Just annots -> asCResT annots
    Nothing -> do
      tprgms <- ctssGetFrom ssfTPrgm prgmName ctss
      stages <- lift $ ctssStagePrgmsFrom ctss prgmName
      -- A @#test@ displays the run of the test it marks, which is the one thing an evaluation
      -- cannot work out for itself. The page's tests are run only when the page writes one, since
      -- running them is work rather than a lookup and no other annotation reads them.
      stages' <- if pageWritesATest prgmName tprgms
        then (\tests -> stages{spTests = pure tests}) <$> ctssTestsFrom ctss prgmName
        else return stages
      let annots = evalAnnots stages' prgmName tprgms
      lift $ ctssStore prgmName (\ssf -> ssf{ssfAnnots = Just annots}) ctss
      asCResT annots

-- | Whether a page marks any of its definitions as a test. A page that does not has no annotation
-- that reads a test run, so it is not made to wait for one.
pageWritesATest :: FileImport -> GraphData TPrgm FileImport -> Bool
pageWritesATest prgmName tprgms = case graphLookup prgmName tprgms of
  Just (Prgm objMap _ _) -> any (hasAnnot testAnnot) (flatObjectMap objMap)
  -- A page that is not in its own program graph has no annotations either; 'evalAnnots' is what
  -- reports that, rather than it being decided here.
  Nothing                -> False

-- | Read a page's computed item, without building the page or computing anything.
ctssCached :: (SSF -> Maybe (CRes a)) -> FileImport -> CTSS -> IO (Maybe (CRes a))
ctssCached f prgmName (CTSS ssmv) = do
  CTSSDat{ctssData} <- readMVar ssmv
  return $ f =<< graphLookup prgmName ctssData

-- | Keep a page's computed item on the page, for whoever asks for it next.
--
-- The page is read again under the lock rather than being written back from a copy captured before
-- the computation: a page that was invalidated while its item was being computed is unbuilt again,
-- and restoring it from a stale copy of itself would undo the invalidation. Two threads racing to
-- compute the same item both store it, which is harmless — they computed it from the same built
-- page, so they computed the same thing.
--
-- Whatever computes the item must do so OUTSIDE this lock. It is the same lock a build takes, and
-- computing an item needs the page built.
ctssStore :: FileImport -> (SSF -> SSF) -> CTSS -> IO ()
ctssStore prgmName store (CTSS ssmv) = modifyMVar_ ssmv $ \ctssdat ->
  return ctssdat{ctssData = fmapGraphWithKey keep (ctssData ctssdat)}
  where
    keep k ssf | k == prgmName && ssfBuilt ssf = store ssf
               | otherwise = ssf

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
-- Neither the type inference nor a debug run is available here. Points are numbered per typecheck
-- run, so the traces of every page joined together would be a set of colliding numbers rather than an
-- answer, and a run needs one program with one entry point where the join has as many as it has
-- pages. An evaluation that wants either has to name the page it is asking about
-- ('ctssStagePrgmsFrom').
ctssStagePrgms :: CTSS -> IO StagePrgms
ctssStagePrgms ctss@(CTSS ssmv) = do
  ctssBuildAll ctss
  CTSSDat{ctssData} <- readMVar ssmv
  let ssfs = map fst3 $ graphToNodes ctssData
  return $ StagePrgms (joinedStage ssfDes ssfs) (joinedStage ssfTBPrgm ssfs) (notScoped "type inference to report on") (const $ notScoped "program to run") (return $ map snd3 $ graphToNodes ctssData) (notScoped "tests to report on")
  where
    notScoped :: String -> CRes a
    notScoped what = CErr [MkCNote $ GenCErr Nothing $ "This evaluation is not scoped to a single page, so it has no " ++ what]

-- | Like 'ctssStagePrgms' but for an evaluation scoped to one page: every stage covers that page and
-- what it imports, the type inference is that page's own, which is the only scope its points are
-- numbered in, and a debug run runs that page.
ctssStagePrgmsFrom :: CTSS -> FileImport -> IO StagePrgms
ctssStagePrgmsFrom ctss@(CTSS ssmv) prgmName = do
  ctssBuildFrom ctss prgmName
  CTSSDat{ctssData} <- readMVar ssmv
  let reaches = graphFilterReaches prgmName ctssData
  let ssfs = map fst3 $ graphToNodes reaches
  let typeInferred = do
        (_, vprgm, trace) <- requireComputed $ ssfTPrgmWithTrace =<< graphLookup prgmName ctssData
        return (vprgm, trace)
  -- Runs against the page's own typecheck program rather than the evaluation's, which for an
  -- annotation is that program joined with the synthetic one holding the annotation itself.
  let debugRun fun = evalDebugPrgm fun =<< joinedStage ssfTPrgm ssfs
  -- No test run: unlike every stage here, one cannot be looked up — it has to be produced by
  -- running the page's tests, which is the work an evaluation with nothing to report on them must
  -- not be made to do. An evaluation that does have a test to report on is given one by whoever
  -- asked for it (see 'getEvalAnnots').
  return $ StagePrgms (joinedStage ssfDes ssfs) (joinedStage ssfTBPrgm ssfs) typeInferred debugRun (return $ map snd3 $ graphToNodes reaches) notRun
  where
    notRun = CErr [MkCNote $ GenCErr Nothing "This evaluation was not given a run of the page's tests"]

-- | Joins one stage over a set of pages, left unforced. See 'ctssStagePrgms' for why it must stay a
-- thunk.
joinedStage :: (Monoid a) => (SSF -> Maybe (CRes a)) -> [SSF] -> CRes a
joinedStage f ssfs = fmap mconcat $ sequence $ mapMaybe f ssfs
