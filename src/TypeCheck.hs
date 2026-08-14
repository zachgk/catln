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
import           Semantics.Types
import           TypeCheck.Common
import           TypeCheck.Constrain (runConstraints)
import           TypeCheck.Decode
import           TypeCheck.Encode
import           Utils

type TypecheckTuplePrgm = (TPrgm, VPrgm, TraceConstrain)
type FinalTypecheckTuplePrgm = (TPrgm, VPrgm, TraceConstrain)
type TypecheckFileResult = H.HashMap FileImport (GraphNodes TypecheckTuplePrgm FileImport)

runConstraintsLimit :: Integer
-- Raised from 20: a name-ambiguous call (map.ct's @insert@ also matching list.ct's nested
-- @/sorted./Data/List/insert@) is only disambiguated once the AddArg chain reaches the
-- refuting graft (~epoch 8-14 — application chains propagate one ArrowTo level per epoch),
-- so such programs legitimately converge in 21-25 epochs. The headroom is NOT for masking
-- non-convergence: unbounded growth is a bug (and was fixed, not budgeted around).
runConstraintsLimit = 25

typecheckPrgmsSt :: [PPrgm] -> [TPrgm] -> StateT FEnv TypeCheckResult ([TPrgm], [VPrgm], TraceConstrain)
typecheckPrgmsSt pprgms typechecked = do
  vprgms <- fromPrgms pprgms typechecked
  cons <- gets feCons
  runConstraints runConstraintsLimit cons
  -- Invariant: after convergence the union of all objects (the "TopTop") must be
  -- determined. If it remained 'TopType', some object failed to resolve (e.g. an
  -- untyped destructured pattern binding poisoned the union), which would otherwise
  -- silently disable 'BoundedByObjs' and surface only as a broken build/eval later.
  env <- get
  case stypeAct <$> descriptor env (feUnionAllObjs env) of
    TypeCheckResult _ PTopType -> lift $ TypeCheckResult [GenTypeCheckError Nothing "Type checking converged but unionAllObjs (the TopTop) remained TopType; an object failed to resolve"] ()
    _                          -> return ()
  trace <- gets feTrace
  tprgms <- toPrgms vprgms
  return (tprgms, vprgms, trace)

typecheckPrgms :: [(PPrgm, [PPrgm], [TypecheckTuplePrgm])] -> TypeCheckResult [TypecheckTuplePrgm]
typecheckPrgms nodesWithDeps = do
  let pprgms = map (\(p, _, _) -> p) nodesWithDeps
  let allDepsTrace = concatMap (\(_, _, d) -> d) nodesWithDeps
  let deps = map fst3 allDepsTrace
  let baseFEnv = makeBaseFEnv (mconcat (pprgms ++ map (mapMetaPrgm clearMetaDat) deps))
  (tprgms, vprgms, trace) <- evalStateT (typecheckPrgmsSt pprgms deps) baseFEnv
  return $ zip3 tprgms vprgms (repeat trace)

typecheckPrgmWithTrace :: PPrgmGraphData -> CRes (GraphData FinalTypecheckTuplePrgm FileImport)
typecheckPrgmWithTrace pprgms = typeCheckToRes $ mapGraphWithDeps typecheckPrgms pprgms

typecheckPrgm :: PPrgmGraphData -> CRes (GraphData TPrgm FileImport)
typecheckPrgm pprgms = do
  graph <- failOnErrorNotes $ typecheckPrgmWithTrace pprgms
  return $ fmapGraph fst3 graph
