-- | Tests for what an evaluated value is, and when two of them are the same value.
--
-- Over 'Eval.Common.Val' directly rather than through a @.ct@ program, because the values covered
-- here are ones no Catln source can write. There is no character literal in the language, so a
-- 'Semantics.Val.CharVal' arrives only from the compiler — a tree build making one out of a @CChar@
-- constant, or the test generator producing one — and an equality that was wrong about it could not
-- be caught by any program.
module Eval.CommonTests where

import qualified Data.HashMap.Strict as H
import           Eval.Common         (Val)
import           Semantics.Val       hiding (Val)
import           Test.Tasty
import           Test.Tasty.HUnit

-- | Two values that must be the same value.
--
-- Typed at 'Eval.Common.Val' — the evaluation's own instantiation — because a value written here
-- names no runtime arm and so is otherwise polymorphic in which runtime it belongs to.
same :: String -> Val -> Val -> TestTree
same name a b = testCase name $ a @?= b

-- | Two values that must not be.
differs :: String -> Val -> Val -> TestTree
differs name a b = testCase name $ assertBool (show a ++ " should not equal " ++ show b) (a /= b)

evalCommonTests :: TestTree
evalCommonTests = testGroup "EvalCommonTests" [
  testGroup "two values are the same value when what they carry is" [
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
