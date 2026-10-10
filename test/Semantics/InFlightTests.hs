-- | Tests for the in-flight sets the type algebra cuts its cycles with ('Semantics.Types.InFlight').
--
-- A goal in flight is a cycle only of the derivation it is in flight on. These hold the three
-- things that makes true: a goal met again on its own derivation is answered as the cycle it is;
-- the same goal in flight on another thread's derivation is no cycle of this one and is derived;
-- and a derivation which throws leaves nothing behind to be taken for a cycle later.
--
-- The second is the one that went wrong. The doc server answers requests on threads of their own,
-- and the sets were shared by every thread, so a goal one request was in the middle of was cut as a
-- cycle by another — and two requests for one page disagreed about whether a value was a member of
-- a class.
module Semantics.InFlightTests where

import           Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception  (ErrorCall (..), throwIO, try)
import           Semantics.Types    (inFlightOn, newInFlight)
import           Test.Tasty
import           Test.Tasty.HUnit

-- | What a derivation answers with, and what a re-entry is answered with.
cut, derived :: IO String
cut = return "cut"
derived = return "derived"

-- | A derivation which meets its own goal again is answered with the re-entry.
testReentry :: TestTree
testReentry = testCase "a goal met again on its own derivation is a cycle" $ do
  inflight <- newInFlight
  answer <- inFlightOn inflight "goal" cut $
    inFlightOn inflight ("goal" :: String) cut derived
  assertEqual "" "cut" answer

-- | A goal another thread is in the middle of deriving is derived here, not cut: it is on that
-- thread's derivation and not on this one's.
testOtherThread :: TestTree
testOtherThread = testCase "a goal in flight on another thread is no cycle of this one" $ do
  inflight <- newInFlight
  entered <- newEmptyMVar
  release <- newEmptyMVar
  done <- newEmptyMVar
  -- The other thread takes the goal and holds it, in flight, until this one has asked.
  _ <- forkIO $ do
    r <- inFlightOn inflight ("goal" :: String) cut $ do
      putMVar entered ()
      takeMVar release
      derived
    putMVar done r
  takeMVar entered
  answer <- inFlightOn inflight "goal" cut derived
  putMVar release ()
  other <- takeMVar done
  assertEqual "this thread" "derived" answer
  assertEqual "the other thread" "derived" other

-- | A derivation which throws takes its goal out of flight, so a later derivation of the same goal
-- on the same thread derives it rather than reading the leftover mark as a cycle.
testThrown :: TestTree
testThrown = testCase "a derivation which throws leaves its goal out of flight" $ do
  inflight <- newInFlight
  thrown <- try $ inFlightOn inflight ("goal" :: String) cut (throwIO (ErrorCall "failed"))
  assertEqual "the derivation failed" (Left (ErrorCall "failed")) (thrown :: Either ErrorCall String)
  answer <- inFlightOn inflight "goal" cut derived
  assertEqual "" "derived" answer

inFlightTests :: TestTree
inFlightTests = testGroup "InFlight" [testReentry, testOtherThread, testThrown]
