-- | Tests for what an evaluated value is, and when two of them are the same value.
--
-- Over 'Eval.Common.Val' directly rather than through a @.ct@ program, because the values covered
-- here are ones no Catln source can write. There is no character literal in the language, so a
-- 'Semantics.Val.CharVal' arrives only from the compiler — a tree build making one out of a @CChar@
-- constant, or the test generator producing one — and an equality that was wrong about it could not
-- be caught by any program.
module Eval.CommonTests where

import qualified Data.HashMap.Strict as H
import           Eval.Common         (DebugRun (..), EvalDebug (..),
                                      EvalResult (..), EvalTreebugClosed (..),
                                      TExpr (..), Val, withRunSources)
import           Semantics.Prgm
import           Semantics.Val       hiding (Val)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec     (initialPos)

-- | Two values that must be the same value.
--
-- Typed at 'Eval.Common.Val' — the evaluation's own instantiation — because a value written here
-- names no runtime arm and so is otherwise polymorphic in which runtime it belongs to.
same :: String -> Val -> Val -> TestTree
same name a b = testCase name $ a @?= b

-- | Two values that must not be.
differs :: String -> Val -> Val -> TestTree
differs name a b = testCase name $ assertBool (show a ++ " should not equal " ++ show b) (a /= b)

-- | A meta written somewhere in a file, which is all a run's sources are read from.
metaIn :: String -> Meta m -> Meta m
metaIn file m = m{getMetaPos = Just (initialPos file, initialPos file)}

-- | A run that entered one arrow of the program, written in @prog.ct@, and within it one arrow built
-- for the call, written in the debugged expression; and that recorded one @#debug@ in @lib.ct@.
sampleRun :: DebugRun
sampleRun = DebugRun Nothing "" (Just NoVal) result H.empty
  where
    result = EvalResult H.empty [node (Left progArrow) [node (Right builtArrow) []]] [EvalDebug (getMetaPos (metaIn "lib.ct" emptyMetaN :: Meta ())) NoVal "1"]
    node oa children = EvalTreebugClosed oa NoVal NoVal children "0" Nothing
    progArrow = ObjArr (Just (Value (metaIn "prog.ct" emptyMetaN) "/f")) FunctionObj Nothing [] Nothing
    builtArrow = ObjArr (Just (TValue (metaIn "<debug>" emptyMetaN) "/g")) FunctionObj Nothing [] Nothing

evalCommonTests :: TestTree
evalCommonTests = testGroup "EvalCommonTests" [
  -- What a debugger run carries of the files it was written in: the text of each file its positions
  -- name -- the program's arrows, the arrows built for its calls, its #debug records -- and of no
  -- other, however many it was offered.
  testCase "a run carries the sources of the files it names and no others" $
      drSources (withRunSources [("prog.ct", "p"), ("lib.ct", "l"), ("<debug>", "d"), ("unused.ct", "u")] sampleRun)
        @?= H.fromList [("prog.ct", "p"), ("lib.ct", "l"), ("<debug>", "d")]
  , testCase "a named file with no source offered is left out rather than guessed" $
      drSources (withRunSources [("prog.ct", "p")] sampleRun) @?= H.fromList [("prog.ct", "p")]

  , testGroup "two values are the same value when what they carry is" [
      same "a whole number" (IntVal 3) (IntVal 3)
    , same "a fractional number" (FloatVal 1.5) (FloatVal 1.5)
    , same "a string" (StrVal "hi") (StrVal "hi")
      -- The arm this file exists for: a character had no clause of its own and fell to the last one,
      -- so it was unequal to itself.
    , same "a character" (CharVal 'a') (CharVal 'a')
    , same "a tuple, by its name and its arguments"
        (TupleVal "/T" (H.singleton "/x" (IntVal 1)))
        (TupleVal "/T" (H.singleton "/x" (IntVal 1)))
    , same "nothing at all" NoVal NoVal
                                                                   ]

  , testGroup "and are not when it differs" [
      differs "two characters" (CharVal 'a') (CharVal 'b')
    , differs "a character and the string of it" (CharVal 'a') (StrVal "a")
    , differs "two whole numbers" (IntVal 1) (IntVal 2)
    , differs "a tuple differing in an argument"
        (TupleVal "/T" (H.singleton "/x" (IntVal 1)))
        (TupleVal "/T" (H.singleton "/x" (IntVal 2)))
    , differs "a tuple differing in its name"
        (TupleVal "/T" H.empty)
        (TupleVal "/U" H.empty)
                                            ]
                                          ]
