--------------------------------------------------------------------
-- |
-- Module    :  Eval.Instruments
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- The instruments an evaluation can be asked to run alongside itself.
--
-- An instrument watches an evaluation without taking part in it: it is handed each event the
-- evaluator reaches ('Eval.Common.EvalInstrument') and keeps whatever it makes of them, and what it
-- kept is read back once the evaluation is over. Since the evaluation behaves identically whichever
-- are attached, what it costs to watch is the caller's choice — which is what lets the expensive
-- watching be left off until something is going to read it.
--------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}

module Eval.Instruments where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S

import           Data.List           (find)
import           Eval.Common
import           Text.Printf

-- | How many times each arrow of the program was entered.
--
-- Only the program's own arrows ('Left'): an arrow built during the evaluation is not one of the
-- program's to have been covered or not.
newtype Coverage = Coverage (H.HashMap EObjArr Int)

coverageInstrument :: AnyEvalInstrument
coverageInstrument = AnyEvalInstrument (Coverage H.empty)

instance EvalInstrument Coverage where
  evalInstrumentName _ = "coverage"
  onArrowEnter _ (Left oa) (Coverage c) = Coverage $ H.insertWith (+) oa 1 c
  onArrowEnter _ Right{} cov            = cov
  evalInstrumentResult (Coverage c) = CoverageResult c

-- | An arrow the evaluation is inside: the id of the node it will close as, the arrow itself, and
-- the calls the ENCLOSING arrow had already finished when this one was entered.
--
-- The id is the one the call was entered under ('evCallIds'), assigned when the arrow is ENTERED
-- rather than derived when it closes, because what wants to name a node is inside it at the time: a
-- @#debug@ recorded partway through a call has to say which call it came from, and the state a
-- closing node is identified from does not exist yet.
--
-- The third is what makes the result a tree of calls rather than a chain of them. 'tbsClosed'
-- holds the calls finished so far at ONE level, so entering an arrow has to put the enclosing level's
-- aside and start an empty one; closing takes it back out and adds the call that just finished to it.
-- Without it a closing arrow adopts whatever closed before it, whether or not that call happened
-- inside it — so an arrow calling two others in sequence reads as the second one calling the first,
-- and a run reads as a single spine however it branched.
type EvalTreebugOpen = (String, AnyObjArr, [EvalTreebugClosed], S.HashSet String)

-- | The tree of calls a run made, with the expressions and annotations it evaluated recorded against
-- them ('EvalTreebug'). What a debugger shows.
--
-- This holds every value the run passed, which is the size of the run rather than of the program,
-- and is attached only when a debugger has asked for it.
data Treebug = Treebug { tbsOpen    :: [EvalTreebugOpen]
                       , tbsClosed  :: [EvalTreebugClosed]
                       -- | The calls of 'tbsClosed' already recorded as the steps of an expression
                       -- ('EvalExprRun'). An expression is evaluated inside another -- the condition
                       -- of the arm a call is dispatched to is evaluated while the call is -- and the
                       -- calls the inner one made close at the same level as the outer one's, so
                       -- without this the outer one would count them as its own steps. Put aside and
                       -- restored with 'tbsClosed'.
                       , tbsClaimed :: S.HashSet String
                       -- | Where the evaluation was in the tree as each expression or annotation
                       -- being evaluated started, innermost first: the call it is inside, and how many
                       -- calls had finished in it so far. Each is handed back as its end is reached.
                       , tbsMarks   :: [(Maybe String, Int)]
                       -- | Every expression the run rewrote through calls, most recent first.
                       , tbsExprs   :: [EvalExprRun]
                       -- | Every annotation of a definition the run evaluated, most recent first.
                       , tbsAnnots  :: [EvalAnnotRun]
                       }

treebugInstrument :: AnyEvalInstrument
treebugInstrument = AnyEvalInstrument $ Treebug [] [] S.empty [] [] []

instance EvalInstrument Treebug where
  evalInstrumentName _ = "treebug"

  -- The enclosing call's finished calls go aside with the arrow being entered, and this one starts
  -- with none of its own. 'onArrowExit' puts them back.
  onArrowEnter callId oa tb@Treebug{tbsOpen, tbsClosed, tbsClaimed} = tb{
      tbsOpen = (callId, oa, tbsClosed, tbsClaimed) : tbsOpen
    , tbsClosed = []
    , tbsClaimed = S.empty
    }

  onArrowExit _ _ _ Treebug{tbsOpen=[]} = error "Tried to close a call with an empty treebug open"
  onArrowExit _ input val tb@Treebug{tbsOpen=(closedId, oa, siblings, claimed):open, tbsClosed} = tb{
      tbsOpen = open
    -- The call that just finished takes what finished INSIDE it as its children, and joins what the
    -- enclosing call had already finished. Appended rather than prepended, so the calls under a node
    -- are in the order the run made them.
    , tbsClosed = siblings ++ [EvalTreebugClosed oa input val tbsClosed closedId Nothing]
    , tbsClaimed = claimed
    }

  -- Marked after the base, whose own expressions are recorded as themselves, so the steps are the
  -- calls this expression's own call tree made (see 'EvalExprRun').
  onExprStart tb = tb{tbsMarks = treebugMark tb : tbsMarks tb}

  -- Its steps are the calls made since the mark that no expression evaluated inside it has already
  -- claimed.
  onExprEnd _ _ _ Treebug{tbsMarks=[]} = error "Tried to record an expression with no treebug mark"
  onExprEnd pos ids val tb@Treebug{tbsMarks=(node, before):marks, tbsExprs, tbsClaimed} = tb{
      tbsMarks = marks
    , tbsExprs = EvalExprRun node pos ids steps val : tbsExprs
    , tbsClaimed = foldr S.insert tbsClaimed steps
    }
    where steps = filter (not . (`S.member` tbsClaimed)) (treebugClosedSince before tb)

  onAnnotStart tb = tb{tbsMarks = treebugMark tb : tbsMarks tb}

  -- There is always a call open, as for a @#debug@: an annotation of a definition is evaluated inside
  -- a call of it.
  onAnnotEnd annotId pos val tb@Treebug{tbsMarks, tbsAnnots} = case tbsMarks of
    (Just node, before):marks -> tb{
        tbsMarks = marks
      , tbsAnnots = EvalAnnotRun node annotId pos val (treebugClosedSince before tb) : tbsAnnots
      }
    (Nothing, _):_ -> error "Tried to record an annotation with an empty treebug open"
    [] -> error "Tried to record an annotation with no treebug mark"

  evalInstrumentResult Treebug{tbsClosed, tbsExprs, tbsAnnots} = TreebugResult $ EvalTreebug tbsClosed (reverse tbsExprs) (reverse tbsAnnots)

-- | Where the evaluation is in the tree: the call it is inside, and how many calls have finished in
-- it so far. Taken before evaluating something whose calls are to be told apart from the ones before
-- it, and handed back to 'treebugClosedSince' afterwards.
treebugMark :: Treebug -> (Maybe String, Int)
treebugMark Treebug{tbsOpen, tbsClosed} = (case tbsOpen of
                                              (openId, _, _, _):_ -> Just openId
                                              []                  -> Nothing, length tbsClosed)

-- | The ids of the calls that finished in the current call since a mark was taken, in the order they
-- were entered. Every call entered in between has closed again by then, and closed into the current
-- call's level, so those are exactly the ones past the count the mark took.
treebugClosedSince :: Int -> Treebug -> [String]
treebugClosedSince before Treebug{tbsClosed} = map (\(EvalTreebugClosed _ _ _ _ closedId _) -> closedId) $ drop before tbsClosed

-- | Every @#debug@ the run recorded, against the call it was recorded in.
--
-- Unlike the tree of calls, which is regrouped by each call that closes over it, this only ever
-- grows: what a reader of a run wants is every value it recorded, in the order it recorded them,
-- however many calls of however many definitions they came from.
newtype Debugs = Debugs [EvalDebug]

debugInstrument :: AnyEvalInstrument
debugInstrument = AnyEvalInstrument (Debugs [])

instance EvalInstrument Debugs where
  evalInstrumentName _ = "debug"
  onDebug callId pos val (Debugs ds) = Debugs (EvalDebug pos val callId : ds)
  evalInstrumentResult (Debugs ds) = DebugResult (reverse ds)

-- | What every evaluation runs with unless its caller asks for more, which is nothing: an evaluation
-- needs nothing watching it to explain its own failure, as a typecheck does.
defaultEvalInstruments :: [AnyEvalInstrument]
defaultEvalInstruments = []

-- | What a debugger shows a run with: the tree of its calls, and its @#debug@ records.
debuggerInstruments :: [AnyEvalInstrument]
debuggerInstruments = [treebugInstrument, debugInstrument]

-- | Every instrument there is, which is what a caller outside Haskell can ask for by name.
allEvalInstruments :: [AnyEvalInstrument]
allEvalInstruments = [coverageInstrument, treebugInstrument, debugInstrument]

-- | The instrument of a name ('evalInstrumentName'), for a caller that names the instruments it
-- wants rather than holding them: the webdocs, asking for a run.
evalInstrumentByName :: String -> Either String AnyEvalInstrument
evalInstrumentByName name = case find (\(AnyEvalInstrument i) -> evalInstrumentName i == name) allEvalInstruments of
  Just i  -> Right i
  Nothing -> Left $ printf "There is no evaluation instrument %s, only %s" (show name) (show $ map show allEvalInstruments)
