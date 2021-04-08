--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module TypeCheck where

import           CRes
import           TypeCheck.Common
import           TypeCheck.Encode
import           TypeCheck.Constrain (runConstraints)
import           TypeCheck.Decode
import Syntax.Prgm
import Utils
import Data.Graph
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as S
import Data.Maybe

type TypecheckTuplePrgm = (TPrgm, VPrgm, TraceConstrain)
type TypecheckFileResult = H.HashMap String (GraphNodes TypecheckTuplePrgm String)

runConstraintsLimit :: Integer
runConstraintsLimit = 100

typecheckPrgms :: [PPrgm] -> [TPrgm] -> TypeCheckResult [TypecheckTuplePrgm]
typecheckPrgms pprgms typechecked = do
  -- determine total classmap
  let (_, pclassMap, _) = mergePrgms pprgms
  let (_, tclassMap, _) = mergePrgms typechecked
  let classMap = mergeClassMaps pclassMap tclassMap

  let baseFEnv = makeBaseFEnv classMap
  (vprgms, env@FEnv{feCons}) <- fromPrgms baseFEnv pprgms typechecked
  env'@FEnv{feTrace} <- runConstraints runConstraintsLimit env feCons
  tprgms <- toPrgms env' vprgms
  return $ zip3 tprgms vprgms (repeat feTrace)

typecheckConnComps :: PPrgmGraphData -> TypecheckFileResult -> [SCC (GraphNodes PPrgm String)] -> TypeCheckResult TypecheckFileResult
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

typecheckPrgmWithTrace :: PPrgmGraphData -> CRes (GraphData TypecheckTuplePrgm String)
typecheckPrgmWithTrace pprgms = typeCheckToRes $ do
  let pprgmSCC = stronglyConnCompR $ graphToNodes pprgms
  typechecked <- typecheckConnComps pprgms H.empty pprgmSCC
  return $ graphFromEdges $ H.elems typechecked

typecheckPrgm :: PPrgmGraphData -> CRes (GraphData TPrgm String)
typecheckPrgm pprgms = do
  graph <- failOnErrorNotes $ typecheckPrgmWithTrace pprgms
  return $ fmapGraph fst3 graph
