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
import           Control.Monad           (unless)
import           CRes
import           Data.Graph
import qualified Data.HashMap.Strict     as H
import           Data.Maybe
import           Eval                    (evalAnnots, evalBuild, evalBuildAll,
                                          evalRun)
import           Eval.Common             (EvalMetaDat, EvalResult, TExpr,
                                          Val (StrVal, TupleVal))
import           Semantics.Prgm
import           Syntax.Ct.Desugarf      (desFiles)
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Syntax.Parsers
import           Text.Printf
import           TypeCheck               (typecheckPrgmWithTrace)
import           TypeCheck.Common        (TPrgm, TraceConstrain, VPrgm)
import           Utils

type TBPrgm = Prgm TExpr EvalMetaDat

-- Main data type for CtService Status
data CTSS = CTSS {
  ctssBaseFileNames :: [String],
  ctssData          :: GraphData SSF FileImport
                                       }

data SSF = SSF {
  ssfRaw            :: CRes (RawPrgm ParseMetaDat),
  ssfBuilt          :: Bool,
  ssfDes            :: Maybe (CRes (Prgm Expr ParseMetaDat)),
  ssfTPrgmWithTrace :: Maybe (CRes (TPrgm, VPrgm, TraceConstrain)),
  ssfTPrgm          :: Maybe (CRes TPrgm),
  ssfTBPrgm         :: Maybe (CRes TBPrgm),
  ssfAnnots         :: Maybe (CRes [(Expr EvalMetaDat, Val)])
               }

emptyCTSS :: CTSS
emptyCTSS = CTSS [] (graphFromEdges [])

newSSF :: CRes (RawPrgm ParseMetaDat) -> SSF
newSSF rawPrgm = SSF rawPrgm False Nothing Nothing Nothing Nothing Nothing

ctssBaseFiles :: [String] -> CTSS
ctssBaseFiles baseFileNames = CTSS baseFileNames (graphFromEdges [])

ctssRead :: CTSS -> IO CTSS
ctssRead ss@CTSS{ctssBaseFileNames, ctssData} = do
  rawPrgms <- readFiles (map mkRawImportStr ctssBaseFileNames)
  let (data', invalidations) = unzip $ map buildSSF $ graphToNodes rawPrgms
  let ss' = ss{ctssData=graphFromEdges data'}
  return $ ctssClearInvalidations (catMaybes invalidations) ss'
  where
    buildSSF (rawPrgm, prgmName, imports) = case graphLookup prgmName ctssData of
      Just old@SSF{ssfRaw=CRes _ oldRaw} | oldRaw == rawPrgm -> ((old, prgmName, imports), Nothing)
      Just{} -> ((newSSF $ pure rawPrgm, prgmName, imports), Just prgmName)
      Nothing -> ((newSSF $ pure rawPrgm, prgmName, imports), Nothing)

ctssClearInvalidations :: [FileImport] -> CTSS -> CTSS
ctssClearInvalidations invalidations ss@CTSS{ctssData=(g, nodeFromVertex, vertexFromKey)} = ss{ctssData=fmapGraphWithKey maybeClear $ ctssData ss}
  where
    propagated = map (snd3 . nodeFromVertex) $ foldMap (foldMap (:[])) $ dfs (transposeG g) (mapMaybe vertexFromKey invalidations)
    maybeClear prgmName ssf = if prgmName `elem` propagated
      then newSSF (ssfRaw ssf)
      else ssf

ctssBuildPagesExact :: CTSS -> GraphData SSF FileImport -> IO CTSS
ctssBuildPagesExact ctss pages | all (ssfBuilt . fst3) (graphToNodes pages) = return ctss
ctssBuildPagesExact ctss@CTSS{ctssData} pages = do
  -- TODO Re-implement using mapGraphWithDeps
  p <- runCResT $ do
    rawPrgm <- asCResT $ mapMGraph ssfRaw pages
    prgm <- desFiles rawPrgm
    let withTrace = typecheckPrgmWithTrace prgm
    let tprgm = fmap (fmapGraph fst3) withTrace
    let tbprgm = tprgm >>= evalBuildAll
    let annots' = tprgm >>= (\jtprgm -> fmap graphFromEdges $ traverse (\(_, n, deps) -> (, n, deps) <$> evalAnnots n jtprgm) $  graphToNodes jtprgm)
    let ctssData' = fmapGraphWithKey (\prgmName ssf -> ssf{
          ssfBuilt=True,
          ssfDes=pure <$> graphLookup prgmName prgm,
          ssfTPrgmWithTrace=mapM (graphLookup prgmName) withTrace,
          ssfTPrgm=mapM (graphLookup prgmName) tprgm,
          ssfTBPrgm=mapM (graphLookup prgmName) tbprgm,
          ssfAnnots=mapM (graphLookup prgmName) annots'
        }) ctssData
    return ctss{ctssData=ctssData'}
  case p of
    CRes notes r -> do
      unless (null notes) $ putStrLn $ prettyCNotes notes
      return r
    CErr notes -> do
      unless (null notes) $ putStrLn $ prettyCNotes notes
      fail "Could not build catln service"

ctssBuildFrom :: CTSS -> FileImport -> IO CTSS
ctssBuildFrom ctss@CTSS{ctssData} src = ctssBuildPagesExact ctss (graphFilterReaches src ctssData)

ctssBuildAll :: CTSS -> IO CTSS
ctssBuildAll ctss@CTSS{ctssData} = ctssBuildPagesExact ctss ctssData

ctssKeys :: CTSS -> [FileImport]
ctssKeys CTSS{ctssData} = map snd3 $ graphToNodes ctssData

ctssGet :: (SSF -> Maybe (CRes a)) -> CTSS -> CResT IO (GraphData a FileImport)
ctssGet f CTSS{ctssData} = asCResT $ mapMGraph (requireComputed . f) ctssData
  where
  -- TODO Remove usage and enable live build of components
  requireComputed :: Maybe (CRes r) -> CRes r
  requireComputed = fromMaybe (CErr [MkCNote $ GenCErr Nothing "Value not computed in catln service"])

ctssGetJoined :: (Monoid a) => (SSF -> Maybe (CRes a)) -> CTSS -> CResT IO a
ctssGetJoined f ctss = do
  graph <- ctssGet f ctss
  return $ mconcat $ map fst3 $ graphToNodes graph

ctssLookup :: (SSF -> Maybe (CRes a)) -> FileImport -> CTSS -> CResT IO (Maybe a)
ctssLookup f k ctss = do
  graph <- ctssGet f ctss
  return $ graphLookup k graph

ctssLookupReq :: (SSF -> Maybe (CRes a)) -> FileImport -> CTSS -> CResT IO a
ctssLookupReq f k ctss = do
  mv <- ctssLookup f k ctss
  case mv of
    Just v  -> return v
    Nothing -> fail $ printf "Could not find %s" (show $ impRaw k)

-- This does not use ctssGet because it doesn't require building
getRawPrgm :: CTSS -> CResT IO PPrgmGraphData
getRawPrgm CTSS{ctssData} = asCResT $ mapMGraph ssfRaw ctssData

getTreebug :: CTSS -> FileImport -> String -> CResT IO EvalResult
getTreebug provider prgmName fun = do
  base <- getTPrgm provider
  snd <$> evalRun fun prgmName base

getEvaluated :: CTSS -> FileImport -> String -> CResT IO Integer
getEvaluated provider prgmName fun = do
  base <- getTPrgm provider
  fst <$> evalRun fun prgmName base

getEvalBuild :: CTSS -> FileImport -> String -> CResT IO Val
getEvalBuild provider prgmName fun = do
  base <- getTPrgm provider
  fst <$> evalBuild fun prgmName base

getEvalAnnots :: CTSS -> FileImport -> CResT IO [(Expr EvalMetaDat, Val)]
getEvalAnnots ctss prgmName = do
  annotsGraph <- ctssGet ssfAnnots ctss
  return $ fromMaybe [] $ graphLookup prgmName annotsGraph

getWeb :: CTSS -> FileImport -> String -> CResT IO String
getWeb provider prgmName fun = do
  base <- getTPrgm provider
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
