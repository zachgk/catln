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
import qualified Data.HashSet        as S
import           Data.List           (transpose)
import           Data.Maybe          (isJust, mapMaybe)
import           MapMeta
import           Semantics.Prgm
import           Semantics.Types
import           System.Environment  (lookupEnv)
import           System.IO.Unsafe    (unsafePerformIO)
import           TypeCheck.Common
import           TypeCheck.Constrain (runConstraints)
import           TypeCheck.Decode
import           TypeCheck.Encode
import           TypeCheck.Partition
import           TypeCheck.TypeUtils (buildPrecedenceMap)
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

-- | Development A\/B switch: solve the page as ONE fragment (the pre-per-SCC behavior
-- expressed in the fragment loop) instead of per function SCC.
solverGlobal :: Bool
solverGlobal = unsafePerformIO (isJust <$> lookupEnv "CATLN_SOLVER_GLOBAL")
{-# NOINLINE solverGlobal #-}

-- | Development A\/B switch: per-SCC fragments but each solving against the FULL dependency
-- set instead of its transitive closure (exact global-universe semantics).
solverFullDeps :: Bool
solverFullDeps = unsafePerformIO (isJust <$> lookupEnv "CATLN_SOLVER_FULLDEPS")
{-# NOINLINE solverFullDeps #-}

-- | A typechecked item's pre-computed contribution to a fragment's dependency side:
-- its recursive objects, their precedences, the typed arrow-graph piece, and the
-- metadata-cleared form of that piece. Computed ONCE per item (at page start for
-- page-external dependencies, at promotion for solved fragments) so each fragment
-- assembles its dependency closure by merging pieces instead of re-walking and
-- re-clearing the whole dependency set.
type DepPiece = (TObjectMap, H.HashMap TypeName [Int], TTypeGraph, ObjectMap Expr ())

depPieceOf :: TypeName -> ObjectMapItem Expr TypedMetaDat -> DepPiece
depPieceOf k item = (recPiece, buildPrecedenceMap recPiece, graphPiece, mapMetaObjectMap clearMetaDat graphPiece)
  where
    graphPiece = ObjectMap (H.singleton k item)
    recPiece = mconcat $ map getRecursiveObjs $ flatObjectMapItem item

typecheckPrgmsSt :: [PPrgm] -> [TPrgm] -> StateT FEnv TypeCheckResult ([TPrgm], [VPrgm], TraceConstrain)
typecheckPrgmsSt pprgms typechecked = do
  modify $ \e -> e{feLocalDefNames = prgmsLocalDefNames pprgms}
  -- Solve per function SCC in dependency order, promoting each solved fragment to a
  -- typechecked dependency of the later ones: the live V-side holds only the fragment being
  -- solved, and everything already solved enters later solves the same way page-external
  -- dependencies do (the decoded objects through 'addUnionObjToEnv', the decoded arrows
  -- through the typechecked type graph). Each fragment sees only its TRANSITIVE dependency
  -- closure over the name-reference graph (spanning page and page-external items), so every
  -- per-fragment cost — the reaches graph, its query memo warmup, the object universe and its
  -- completion — scales with the closure rather than the whole accumulated dependency set.
  -- Statement annotations are consumer-only and solve last, against the full dependency set.
  let pageObjMap = mconcat (map prgmObjMap pprgms)
  let depObjMap = prgmObjMap (mconcat typechecked)
  let sccFragments = fragmentPrgms (itemSccGroups pageObjMap) pprgms ++ annotFragmentPrgms pprgms
  let fragments = if solverGlobal || null sccFragments then [pprgms] else sccFragments
  let allKeys = objMapKeys pageObjMap ++ objMapKeys depObjMap
  let edgeMap = resolveRefEdges allKeys (H.unionWith (++) (itemRawRefs pageObjMap) (itemRawRefsShallow depObjMap))
  let depPieces0 = H.mapWithKey depPieceOf (unObjectMap depObjMap)
  -- Data items (page and dependency side) belong to every fragment's closure: class
  -- expansions materialize their objects from the static class graph with no name edge.
  let allDataKeys = dataItemKeys pageObjMap ++ dataItemKeys depObjMap
  (tfragsRev, vfragsRev, _, _) <- foldM (solveFragment edgeMap allDataKeys) ([], [], depPieces0, True) fragments
  -- Invariant: after convergence the union of all objects (the "TopTop") must be
  -- determined. If it remained 'TopType', some object failed to resolve (e.g. an
  -- untyped destructured pattern binding poisoned the union), which would otherwise
  -- silently disable 'BoundedByObjs' and surface only as a broken build/eval later.
  env <- get
  case stypeAct <$> descriptor env (feUnionAllObjs env) of
    TypeCheckResult _ PTopType -> lift $ TypeCheckResult [GenTypeCheckError Nothing "Type checking converged but unionAllObjs (the TopTop) remained TopType; an object failed to resolve"] ()
    _                          -> return ()
  trace <- gets feTrace
  let tprgms = map mconcat $ transpose $ reverse tfragsRev
  let vprgms = map mconcat $ transpose $ reverse vfragsRev
  return (tprgms, vprgms, trace)
  where
    unObjectMap (ObjectMap m) = m
    objMapKeys = H.keys . unObjectMap
    -- A fragment that fails to converge stops the loop: its dependents would solve against a
    -- half-settled fragment ('addUnionObjToEnv' rejects undetermined dependency objects
    -- outright), and the failure's own notes already say why the page failed.
    solveFragment edgeMap allDataKeys acc@(taccs, vaccs, pieces, ok) frag
      | not ok = return acc
      | otherwise = do
          let fragKeys = concatMap (objMapKeys . prgmObjMap) frag
          -- An annotation fragment has no items of its own and its expressions are not part
          -- of the reference graph: give it the full dependency set (it runs once, last).
          let closureKeys
                | solverGlobal || null fragKeys || solverFullDeps = H.keys pieces
                | otherwise = hashNub (allDataKeys ++ S.toList (closureFrom edgeMap fragKeys))
          let cPieces = mapMaybe (`H.lookup` pieces) closureKeys
          let tRec = mconcat [r | (r, _, _, _) <- cPieces]
          let tPrec = foldr (H.unionWith min) H.empty [p | (_, p, _, _) <- cPieces]
          let tGraph = mconcat [g | (_, _, g, _) <- cPieces]
          let tCleared = mconcat [c | (_, _, _, c) <- cPieces]
          modify $ \e -> e{feVTypeGraph = mempty, feTTypeGraph = tGraph, feTTypeGraphCleared = tCleared}
          (vprgms', cons) <- fromPrgmsFragment frag (tRec, tPrec)
          converged <- runConstraints runConstraintsLimit cons
          modify nextConstrainEpoch
          tprgms' <- toPrgms vprgms'
          uni <- gets (\e -> stypeAct <$> descriptor e (feUnionAllObjs e))
          let uniOk = case uni of
                TypeCheckResult _ PTopType -> False
                TypeCheckResE _            -> False
                _                          -> True
          -- Promote the solved fragment: per-item pieces join the dependency side the same
          -- way page-external dependencies entered.
          let fragPieces = H.mapWithKey depPieceOf (unObjectMap (mconcat (map prgmObjMap tprgms')))
          return (tprgms':taccs, vprgms':vaccs, H.union fragPieces pieces, converged && uniOk)

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
