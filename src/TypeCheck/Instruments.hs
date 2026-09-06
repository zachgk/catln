--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Instruments
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- The instruments a typecheck can be asked to run alongside itself.
--
-- An instrument watches a solve without taking part in it: it is handed each event the solver
-- reaches ('TypeCheck.Common.Instrument') and keeps whatever it makes of them, and what it kept is
-- read back off the environment once the solve is over. Since the solve behaves identically
-- whichever are attached, what it costs to watch is the caller's choice — which is what lets the
-- expensive watching be left off until something is going to read it.
--------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}

module TypeCheck.Instruments where

import qualified Data.HashMap.Strict as H
import           TypeCheck.Common
import           TypeCheck.Show      (showTraceConstrainEpoch)

-- | Every scheme write the solve committed, per constraint, per epoch, with every point's seed
-- scheme and the whole constraint list beside them.
--
-- This is the whole inference, and it is far larger than the program it describes: a point is
-- written many times over the epochs and every one of those writes is kept, as is a seed scheme
-- for every point ever allocated. It is what @catln typetrace@ and the webdocs point view read,
-- and it is attached only when one of them has asked.
newtype TraceInstrument = TraceInstrument TraceConstrain

mkTraceInstrument :: TraceInstrument
mkTraceInstrument = TraceInstrument mkTraceConstrain

traceInstrument :: AnyInstrument
traceInstrument = AnyInstrument mkTraceInstrument

instance Instrument TraceInstrument where
  instrumentName _ = "trace"

  onFreshPoint p scheme (TraceInstrument tc@TraceConstrain{tcInitial}) =
    TraceInstrument tc{tcInitial = H.insert p scheme tcInitial}

  onConstraintStart c (TraceInstrument tc@TraceConstrain{tcEpochs}) = case tcEpochs of
    curEpoch:prevEpochs -> TraceInstrument tc{tcEpochs = ((c, []):curEpoch):prevEpochs}
    []                  -> error "no epochs in TraceInstrument.onConstraintStart"

  onPointUpdate p scheme' (TraceInstrument tc) = TraceInstrument $ traceConstrainChange tc p scheme'

  onEpochEnd cons allObjs (TraceInstrument tc@TraceConstrain{tcEpochs}) =
    TraceInstrument tc{tcEpochs = [] : tcEpochs, tcCons = cons, tcAllObjs = allObjs}

  instrumentResult (TraceInstrument tc) = TraceResult tc

-- | The epoch being run and the one before it, and nothing else.
--
-- This is what a solve that did not converge is explained by: what was still changing when it ran
-- out of epochs ('TypeCheck.runConstraintsLimit'). Keeping that much is bounded — one epoch's
-- writes, dropped at each boundary — so this rides along with every typecheck, where the whole
-- 'TraceInstrument' cannot.
--
-- Two strict fields rather than a list kept short: @take 2@ in weak head normal form leaves a thunk
-- holding the tail, so a bound written that way would retain every epoch it was written to drop.
data LastEpochs = LastEpochs !TraceConstrainEpoch !TraceConstrainEpoch

mkLastEpochs :: LastEpochs
mkLastEpochs = LastEpochs [] []

lastEpochsInstrument :: AnyInstrument
lastEpochsInstrument = AnyInstrument mkLastEpochs

instance Instrument LastEpochs where
  instrumentName _ = "lastEpochs"

  onConstraintStart c (LastEpochs cur prev) = LastEpochs ((c, []) : cur) prev

  onPointUpdate p scheme' (LastEpochs cur prev) = case cur of
    (con, changed):rest -> LastEpochs ((con, (p, scheme') : changed) : rest) prev
    []                  -> error "no constraint in LastEpochs.onPointUpdate"

  onEpochEnd _ _ (LastEpochs cur _) = LastEpochs [] cur

  -- The PREVIOUS epoch, not the current one. The limit is reached at the head of the epoch which
  -- would have run next, before any constraint of it has executed, so the current epoch is empty
  -- and what was still changing is the one before it.
  instrumentDiagnostic env (LastEpochs _ prev) = Just $ show $ showTraceConstrainEpoch env prev

-- | How many epochs a solve ran, over all of its fragments, and how many fragments those were.
--
-- Counted at the head of each epoch rather than at the boundaries between them: a boundary is
-- crossed only when another epoch will follow, and a fragment crosses one of its own after
-- converging, so boundaries and epochs are not the same count.
--
-- The fragment count rides along because the epoch count means nothing without it. A fragment
-- cannot cost less than the pass that changes something and the pass that finds nothing left to
-- change, so twice the fragments is the floor, and what an ordering of the constraints can win back
-- is the distance above it rather than the total.
data EpochCount = EpochCount !Integer !Integer

mkEpochCount :: EpochCount
mkEpochCount = EpochCount 0 0

epochCountInstrument :: AnyInstrument
epochCountInstrument = AnyInstrument mkEpochCount

instance Instrument EpochCount where
  instrumentName _ = "epochCount"
  onEpochStart (EpochCount n f) = EpochCount (n + 1) f
  onSolveStart (EpochCount n f) = EpochCount n (f + 1)
  instrumentResult (EpochCount n f) = EpochCountResult n f

-- | What every typecheck runs with, which is only what a solve needs to explain its own failure.
defaultInstruments :: [AnyInstrument]
defaultInstruments = [lastEpochsInstrument]

-- | What a caller asking to read the inference runs with: the whole trace, and the epoch count
-- beside it, on top of what every typecheck already carries.
typeInfoInstruments :: [AnyInstrument]
typeInfoInstruments = defaultInstruments ++ [traceInstrument, epochCountInstrument]

-- | What a caller MEASURING a solve runs with: how long the inference took to settle, and nothing
-- of the inference itself.
--
-- 'EpochCount' keeps one 'Integer', so unlike the trace this is free to attach to every page of a
-- whole compilation — which is what a total over the run has to be. Read by @catln epochs@.
epochInstruments :: [AnyInstrument]
epochInstruments = defaultInstruments ++ [epochCountInstrument]
