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
import qualified Data.HashMap.Strict as H
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

typecheckPrgms :: [PPrgm] -> [TypecheckTuplePrgm] -> TypeCheckResult [TypecheckTuplePrgm]
typecheckPrgms pprgms depsTrace = do
  let deps = map fst3 depsTrace
  let baseFEnv = makeBaseFEnv (mergePrgms (pprgms ++ map (mapMetaPrgm clearMetaDat) deps))
  (tprgms, vprgms, trace) <- evalStateT (typecheckPrgmsSt pprgms deps) baseFEnv
  return $ zip3 tprgms vprgms (repeat trace)

typecheckPrgmWithTrace :: PPrgmGraphData -> CRes (GraphData FinalTypecheckTuplePrgm FileImport)
typecheckPrgmWithTrace pprgms = typeCheckToRes $ mapGraphWithDeps typecheckPrgms pprgms

typecheckPrgm :: PPrgmGraphData -> CRes (GraphData TPrgm FileImport)
typecheckPrgm pprgms = do
  graph <- failOnErrorNotes $ typecheckPrgmWithTrace pprgms
  return $ fmapGraph fst3 graph
