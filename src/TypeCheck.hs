--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This is the main module for typechecking. It takes a program
-- with some typing information and computes the rest of the typing
-- information. It's general goal is to narrow down types as precise
-- as possible.
--
-- The typechecking works through a constraint system. It converts
-- the code into a number of type variables and rules describing
-- relationships between those variables. Then, those relationships
-- are applied until the typing has converged (no more changes).
--------------------------------------------------------------------

module TypeCheck where

import           Control.Monad.State
import           CRes
import           Data.Graph
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Maybe
import           MapMeta
import           Semantics.Prgm
import           TypeCheck.Common
import           TypeCheck.Constrain (runConstraints)
import           TypeCheck.Decode
import           TypeCheck.Encode
import           Utils

type TypecheckTuplePrgm = (TPrgm, VPrgm, TraceConstrain)
type FinalTypecheckTuplePrgm = (TPrgm, VPrgm, TraceConstrain)
type TypecheckFileResult = H.HashMap FileImport (GraphNodes TypecheckTuplePrgm FileImport)

runConstraintsLimit :: Integer
runConstraintsLimit = 15

typecheckPrgmsSt :: [PPrgm] -> [TPrgm] -> StateT FEnv TypeCheckResult ([TPrgm], [VPrgm], TraceConstrain)
typecheckPrgmsSt pprgms typechecked = do
  vprgms <- fromPrgms pprgms typechecked
  cons <- gets feCons
  runConstraints runConstraintsLimit cons
  trace <- gets feTrace
  tprgms <- toPrgms vprgms
  return (tprgms, vprgms, trace)

typecheckPrgms :: [PPrgm] -> [TPrgm] -> TypeCheckResult [TypecheckTuplePrgm]
typecheckPrgms pprgms typechecked = do
  let baseFEnv = makeBaseFEnv (mergePrgms (pprgms ++ map (mapMetaPrgm clearMetaDat) typechecked))
  (tprgms, vprgms, trace) <- evalStateT (typecheckPrgmsSt pprgms typechecked) baseFEnv
  return $ zip3 tprgms vprgms (repeat trace)

typecheckConnComps :: PPrgmGraphData -> TypecheckFileResult -> [SCC (GraphNodes PPrgm FileImport)] -> TypeCheckResult TypecheckFileResult
typecheckConnComps _ res [] = return res
typecheckConnComps graphData@(prgmGraph, prgmFromNode, prgmFromName) results (curPrgm:nextPrgms) = do
  let pprgms = flattenSCC curPrgm
  let pprgmsNames = S.fromList $ map snd3 pprgms
  let importNames = S.fromList $ map (snd3 . prgmFromNode) $ concatMap (reachable prgmGraph . fromJust . prgmFromName) pprgmsNames
  let strictDepImportNames = S.difference importNames pprgmsNames
  let importChecked = fromJust $ mapM (`H.lookup` results) (S.toList strictDepImportNames)
  newResults <- typecheckPrgms (map fst3 pprgms) (map (fst3 . fst3) importChecked)
  let results' = foldr (\((_, prgmName, prgmImports), prgm') accResults -> H.insert prgmName (prgm', prgmName, prgmImports) accResults) results (zip pprgms newResults)
  typecheckConnComps graphData results' nextPrgms

typecheckPrgmWithTrace :: PPrgmGraphData -> CRes (GraphData FinalTypecheckTuplePrgm FileImport)
typecheckPrgmWithTrace pprgms = typeCheckToRes $ do
  let pprgmSCC = stronglyConnCompR $ graphToNodes pprgms
  typechecked <- typecheckConnComps pprgms H.empty pprgmSCC
  return $ graphFromEdges $ H.elems typechecked

typecheckPrgm :: PPrgmGraphData -> CRes (GraphData TPrgm FileImport)
typecheckPrgm pprgms = do
  graph <- failOnErrorNotes $ typecheckPrgmWithTrace pprgms
  return $ fmapGraph fst3 graph
