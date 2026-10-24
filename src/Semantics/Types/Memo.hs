-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.Memo
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- The shared infrastructure behind the type algebra's memo tables: the per-thread in-flight sets
-- that cut cycles ('InFlight') and the read frames that record which graph items an answer read
-- ('ReadDeps').
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Semantics.Types.Memo where

import           Control.Concurrent   (ThreadId, myThreadId)
import           Control.Exception    (bracket_, onException)
import           Data.Hashable
import qualified Data.HashMap.Strict  as H
import qualified Data.HashSet         as S
import           Data.IORef           (IORef, atomicModifyIORef', newIORef,
                                       readIORef)
import qualified Data.Map.Strict      as M
import           Semantics.Types.Data
import           Semantics.Types.Name
import           System.IO.Unsafe     (unsafePerformIO)


-- | The goals a derivation is in the middle of, kept apart for each thread deriving.
--
-- A goal in flight is one on the CURRENT derivation's own path: met again, it is a cycle, and
-- each user answers it the way its cycle rule says. That reading holds only of the path the
-- goal was met on. The doc server answers requests on threads of their own, so two derivations
-- can run at once, and a goal one of them is in the middle of is no cycle of the other's —
-- answering it as one (a 'False' for "no finite witness") is an answer about a different
-- derivation. Two requests for one page then disagreed about whether a value was a member of a
-- class, the planner showed a timed task as unscheduled or dropped it, and which one depended on
-- how the requests happened to overlap.
--
-- So each thread has a set of its own. A derivation stays on the thread forcing it: a thunk
-- another thread demands meanwhile blocks that thread until the first finishes it
-- ('unsafePerformIO' is not duplicated), so a cycle is always met on the thread it is a cycle
-- of. A thread's set is dropped when it empties, so the map holds only threads mid-derivation
-- rather than every thread a server has run.
type InFlight k = IORef (M.Map ThreadId (S.HashSet k))

newInFlight :: IO (InFlight k)
newInFlight = newIORef M.empty

-- | Runs a derivation of a goal with the goal marked in flight on this thread, or answers the
-- re-entry where this thread's derivation already has it in flight.
--
-- The mark is taken off however the derivation ends. One left behind by a derivation that threw
-- would refute its goal on every later derivation of this thread as though it were a cycle.
inFlightOn :: (Eq k, Hashable k) => InFlight k -> k -> IO a -> IO a -> IO a
inFlightOn ref k reentered derive = do
  thread <- myThreadId
  inflight <- M.findWithDefault S.empty thread <$> readIORef ref
  if S.member k inflight
    then reentered
    else bracket_
      (atomicModifyIORef' ref $ \m -> (M.insertWith S.union thread (S.singleton k) m, ()))
      (atomicModifyIORef' ref $ \m -> (M.update (\s -> let s' = S.delete k s in if S.null s' then Nothing else Just s') thread m, ()))
      derive

-- | The items of a graph an answer was read from that can change while the graph's other answers
-- are reused, each at the version it was read at ('ReuseScope'). An answer is the same answer
-- again in another env of its scope where each of these is still at the version it was read at.
type ReadDeps = H.HashMap TypeName Int

-- | Each thread's stack of answers being worked out, innermost first, with the 'ReadDeps' each has
-- made so far. An answer's reads are its own and those of every answer it asked for on the way,
-- so a frame's reads join its parent's when it closes.
{-# NOINLINE readFramesRef #-}
readFramesRef :: IORef (M.Map ThreadId [ReadDeps])
readFramesRef = unsafePerformIO (newIORef M.empty)

-- | Works out an answer in a frame of its own, and gives the reads it made beside it. The action
-- must force the answer through everything that can read the graph, or the reads still to come
-- would be made in whatever frame forces them later.
withReadFrame :: IO a -> IO (a, ReadDeps)
withReadFrame act = do
  thread <- myThreadId
  atomicModifyIORef' readFramesRef $ \m -> (M.insertWith (++) thread [H.empty] m, ())
  r <- act `onException` atomicModifyIORef' readFramesRef (\m -> (M.update popFrame thread m, ()))
  deps <- atomicModifyIORef' readFramesRef $ \m -> case M.lookup thread m of
    Just (d:parents) -> (M.update (const (joinParent d parents)) thread m, d)
    _                -> (m, H.empty)
  return (r, deps)
  where
    popFrame (_:parents@(_:_)) = Just parents
    popFrame _                 = Nothing
    joinParent d (p:ps) = Just (H.union p d : ps)
    joinParent _ []     = Nothing

-- | Adds reads to the answer this thread is working out, if it is working one out: those of a
-- remembered answer it is handed instead of working it out again.
noteReadDeps :: ReadDeps -> IO ()
noteReadDeps deps
  | H.null deps = return ()
  | otherwise = do
      thread <- myThreadId
      atomicModifyIORef' readFramesRef $ \m -> (M.adjust addTop thread m, ())
  where
    addTop (d:parents) = H.union d deps : parents
    addTop []          = []

-- | A value read out of an env's graph under a name, noted as a read of that name where the name's
-- item can change within the env's 'ReuseScope'. The read is noted when the value is demanded.
{-# NOINLINE noteGraphRead #-}
noteGraphRead :: TypeEnv tg -> TypeName -> a -> a
noteGraphRead TypeEnv{teReuse=Just ReuseScope{rsVersions}} name x
  | Just v <- H.lookup name rsVersions = unsafePerformIO (noteReadDeps (H.singleton name v) >> return x)
noteGraphRead _ _ x = x

-- | A value read out of the whole of an env's graph, noted as a read of every item that can change.
{-# NOINLINE noteWholeGraphRead #-}
noteWholeGraphRead :: TypeEnv tg -> a -> a
noteWholeGraphRead TypeEnv{teReuse=Just ReuseScope{rsVersions}} x = unsafePerformIO (noteReadDeps rsVersions >> return x)
noteWholeGraphRead _ x = x

-- | Works out a remembered answer, with the reads it made beside it where the env is one whose
-- answers can be reused (a 'ReuseScope'); elsewhere nothing reads that is ever checked.
memoReading :: TypeEnv tg -> IO a -> IO (a, ReadDeps)
memoReading TypeEnv{teReuse=Nothing} act = (, H.empty) <$> act
memoReading _ act                        = withReadFrame act

-- | How many in-flight re-entries have answered early so far: the cycle cuts of
-- 'subPartialInFlightRef', 'satInFlightRef', 'innerRejectsInFlightRef' and
-- 'negPredDecideInFlightRef'. A cut answers for the derivation path it happened on, so an
-- answer computed while one fired may depend on which goals were in flight above it; the
-- result memos compare this counter before and after a computation and store only answers
-- no cut touched.
--
-- One counter for every thread, where the in-flight sets are one per thread ('InFlight'). It
-- decides only whether an answer is stored, and counting other threads' cuts as well as this
-- one's can only withhold a store, never keep an answer one of this thread's cuts touched — so
-- sharing it costs some answers recomputed and nothing worse, where a counter per thread would
-- be a count kept for every thread a server ever ran, with nowhere to drop it.
{-# NOINLINE cycleCutCountRef #-}
cycleCutCountRef :: IORef Int
cycleCutCountRef = unsafePerformIO (newIORef 0)

-- | Record a cycle cut and answer with it.
noteCycleCut :: a -> IO a
noteCycleCut r = atomicModifyIORef' cycleCutCountRef (\n -> (n + 1, ())) >> return r
