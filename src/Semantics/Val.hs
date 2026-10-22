--------------------------------------------------------------------
-- |
-- Module    :  Semantics.Val
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- What an evaluated value is, apart from what a runtime can do with one.
--
-- A value is either data — a number, a string, a character, a tuple of further values — or something
-- only a runtime knows the inside of: a closure, an action on the outside world, a queue of compiled
-- code. The first kind is what a program computes and what is read back out of one; the second is
-- machinery, and every one of its arms names a type from "Eval.Common".
--
-- So the runtime kinds are one parameter rather than a list of constructors, and this module holds
-- the data ones and nothing else. That is what lets it sit below "Semantics.Types" — it imports
-- "CtConstants" and the base libraries and nothing further — and so lets a type declare how it is
-- read as a Catln value beside the type itself, rather than in some module above the evaluator.
--
-- 'Eval.Common' supplies the runtime half: an @EvalRuntime@ for the parameter, pattern synonyms so
-- the arms are still written 'Eval.Common.ObjArrVal' and 'Eval.Common.IOVal', and the
-- 'RuntimeValue' instance below.
--------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Semantics.Val where

import           CtConstants
import           Data.Aeson
import           Data.Hashable
import qualified Data.HashMap.Strict as H
import           Data.List           (intercalate)
import qualified Data.Text           as T
import           Data.Time.Calendar  (Day, fromGregorianValid)
import           Text.Printf

-- | An evaluated value.
--
-- The runtime arms are the parameter @r@. A 'Val' built by conversion never uses it, so a converter
-- is parametric in @r@ and needs no coercion at the boundary; an evaluation instantiates it at its
-- own runtime type.
data Val r
  = IntVal Integer
  | FloatVal Double
  | CharVal Char
  -- | A @\/Catln\/CStr@: a string as the interpreter holds one, packed rather than a list of its
  -- characters. It is what a string constant and a runtime primitive's string are, and it converts
  -- to and from a @\/Data\/String\/Text@ by the runtime arrows of @stack\/core\/string.ct@.
  | CStrVal !T.Text
  | TupleVal String (H.HashMap String (Val r))
  -- | A value only the runtime knows the inside of — a closure, an action, compiled code.
  | RuntimeVal r
  | NoVal

-- | The arguments of an evaluated value, by the name each is declared under.
type Args r = H.HashMap String (Val r)

-- | A @\/Catln\/CStr@, the string the interpreter holds packed ('CStrVal'), as the Haskell 'String'
-- it is to the outside world.
--
-- A string is one of the members of the open @String@ class of @stack\/core\/string.ct@. This is the
-- one the interpreter makes — every string constant, and every string a runtime primitive returns —
-- and the one a runtime primitive is declared to take, so matching it is how a primitive reads its
-- argument. It does not match the other member, a @\/Data\/String\/Text@ holding its characters as a
-- list: a primitive given one was given what its declaration does not take, which the tree build is
-- to have converted. A string read out of data the program built may be either, and is read by
-- 'valString'.
pattern StrVal :: String -> Val r
pattern StrVal s <- CStrVal (T.unpack -> s)
  where StrVal s = CStrVal (T.pack s)
{-# COMPLETE IntVal, FloatVal, CharVal, StrVal, TupleVal, RuntimeVal, NoVal #-}

-- | A @\/Data\/String\/Text@: a string as Catln takes one apart, the list of the characters it is made
-- of. Built where a runtime arrow converts a 'CStrVal' to one.
textVal :: String -> Val r
textVal s = TupleVal ctStringText (H.singleton (runtimeArgKey ctStringChars) (valListOf (map CharVal s)))

-- | The characters of a string value, where it is one, in whichever representation it is held:
-- packed ('CStrVal') or as a @\/Data\/String\/Text@.
--
-- A tuple whose characters are not all 'CharVal's, or whose list does not end, does not match.
valString :: Val r -> Maybe String
valString (CStrVal t) = Just (T.unpack t)
valString (TupleVal name args) | name == ctStringText = charsOf [] =<< H.lookup (runtimeArgKey ctStringChars) args
  where
    -- A loop over the characters seen so far rather than a recursion, since whether the value is a
    -- string at all is only known at its end: a recursion would hold a frame per character until
    -- then, which for a long string is a stack as large as the string.
    charsOf seen (TupleVal n _) | n == ctListNil = Just (reverse seen)
    charsOf seen (TupleVal n as) | n == ctListCons =
      case (H.lookup (runtimeArgKey ctListConsHead) as, H.lookup (runtimeArgKey ctListConsTail) as) of
        (Just (CharVal c), Just t) -> charsOf (c : seen) t
        _                          -> Nothing
    charsOf _ _ = Nothing
valString _ = Nothing

-- | A string in either representation ('valString'), as a view.
pattern AnyStrVal :: String -> Val r
pattern AnyStrVal s <- (valString -> Just s)
-- Every packed string is one, so with the other arms it is all of them.
{-# COMPLETE IntVal, FloatVal, CharVal, AnyStrVal, TupleVal, RuntimeVal, NoVal #-}

-- | What a runtime must say about its own values for a 'Val' to be shown, compared, hashed or
-- serialized.
--
-- 'rvEqVal' takes the whole other value rather than another @r@ because a runtime arm's equality is
-- not always a comparison: an action on the outside world refuses to be compared with ANYTHING, and
-- a @r -> r -> Bool@ could only refuse when both sides were actions — answering False for the mixed
-- case, which is answering a question this is meant to refuse.
class RuntimeValue r where
  rvShow :: r -> String
  rvEqVal :: r -> Val r -> Bool
  rvHash :: Int -> r -> Int
  rvToJSON :: r -> Value

-- | Whether two values are the same value.
--
-- The data arms are equal when what they carry is; a runtime arm answers for itself, on either side.
-- Two strings are equal when their characters are, whichever representation each is held in: a
-- @\/Catln\/CStr@ and a @\/Data\/String\/Text@ are two ways of holding the one string.
instance (RuntimeValue r) => Eq (Val r) where
  (IntVal a) == (IntVal b)             = a == b
  (FloatVal a) == (FloatVal b)         = a == b
  (CStrVal a) == (CStrVal b)           = a == b
  (AnyStrVal a) == (AnyStrVal b)       = a == b
  (CharVal a) == (CharVal b)           = a == b
  (TupleVal an aa) == (TupleVal bn ba) = an == bn && aa == ba
  (RuntimeVal a) == b                  = rvEqVal a b
  a == (RuntimeVal b)                  = rvEqVal b a
  NoVal == NoVal                       = True
  _ == _                               = False

instance (RuntimeValue r) => Show (Val r) where
  show (IntVal i)   = show i
  show (FloatVal d) = show d
  show (CStrVal s)  = show s
  show (AnyStrVal s) = show s
  show (CharVal c)  = show c
  show (TupleVal name args) = "TupleVal " ++ name ++ showArgs args
    where
      showArgs as | H.null as = ""
      showArgs as = printf "(%s)" (intercalate ", " $ map showArg $ H.toList as)
      showArg (argName, val) = argName ++ " = " ++ show val
  show (RuntimeVal r) = rvShow r
  show NoVal          = "NoVal"

instance (RuntimeValue r) => Hashable (Val r) where
  hashWithSalt s (IntVal i)      = s `hashWithSalt` i
  hashWithSalt s (FloatVal i)    = s `hashWithSalt` i
  -- By its characters, as two strings holding them are equal ('Eq').
  hashWithSalt s (CStrVal i)     = s `hashWithSalt` T.unpack i
  hashWithSalt s (AnyStrVal i)   = s `hashWithSalt` i
  hashWithSalt s (CharVal i)     = s `hashWithSalt` i
  hashWithSalt s (TupleVal n as) = s `hashWithSalt` n `hashWithSalt` as
  hashWithSalt s (RuntimeVal r)  = rvHash s r
  hashWithSalt s NoVal           = s

instance (RuntimeValue r) => ToJSON (Val r) where
  toJSON (IntVal v) = object ["tag".=("IntVal" :: String), "contents".=toJSON v]
  toJSON (FloatVal v) = object ["tag".=("FloatVal" :: String), "contents".=toJSON v]
  -- The packed text as it is, with no walk over characters: a listing is one of these, and is
  -- written out of it whole.
  toJSON (CStrVal v) = object ["tag".=("StrVal" :: String), "contents".=v]
  toJSON (AnyStrVal v) = object ["tag".=("StrVal" :: String), "contents".=toJSON v]
  toJSON (CharVal v) = object ["tag".=("CharVal" :: String), "contents".=toJSON v]
  toJSON (TupleVal name args) = object ["tag".=("TupleVal" :: String), "name".=name, "args".=toJSON args]
  toJSON (RuntimeVal r) = rvToJSON r
  toJSON NoVal = object ["tag".=("NoVal" :: String)]

-- | An evaluated @\/Data\/List@ as the values in it. A list is a @\/Data\/Cons@ chain ending in a
-- @\/Data\/Nil@, which is what the @[ ]@ sugar desugars to in @Syntax.Ct.Desugarf.Expr@.
valList :: (RuntimeValue r) => Val r -> Either String [Val r]
valList (TupleVal name _) | name == ctListNil = Right []
valList (TupleVal name args) | name == ctListCons =
  (:) <$> valArg name ctListConsHead args <*> (valList =<< valArg name ctListConsTail args)
valList val = Left $ printf "%s is not a list" (show val)

-- | An argument of an evaluated value, by the name it is declared under.
valArg :: (RuntimeValue r) => String -> String -> Args r -> Either String (Val r)
valArg name argName args = case H.lookup (runtimeArgKey argName) args of
  Just v  -> Right v
  Nothing -> Left $ printf "A %s has no %s, only %s" name (runtimeArgKey argName) (show $ H.keys args)

-- | The same, where the argument must be a string — which every name and every file's writing is —
-- given to a runtime primitive, which is declared to take a @\/Catln\/CStr@ ('StrVal').
valStrArg :: (RuntimeValue r) => String -> String -> Args r -> Either String String
valStrArg name argName args = do
  v <- valArg name argName args
  case v of
    StrVal s -> Right s
    AnyStrVal _ -> Left $ printf "The %s of a %s is %s, a String held as its characters where a %s is declared: the tree build did not convert it" (runtimeArgKey argName) name (show v) ctCStr
    _        -> Left $ printf "The %s of a %s is %s, which is not a String" (runtimeArgKey argName) name (show v)

-- | The same, where the argument is a string read out of data the program built, which may be held
-- in either representation ('valString').
valAnyStrArg :: (RuntimeValue r) => String -> String -> Args r -> Either String String
valAnyStrArg name argName args = do
  v <- valArg name argName args
  case v of
    AnyStrVal s -> Right s
    _           -> Left $ printf "The %s of a %s is %s, which is not a String" (runtimeArgKey argName) name (show v)

-- | The same, where the argument is a string which may be absent — which is a @\/Data\/Maybe@ of one.
valMaybeStrArg :: (RuntimeValue r) => String -> String -> Args r -> Either String (Maybe String)
valMaybeStrArg name argName args = do
  v <- valArg name argName args
  case v of
    TupleVal m _ | m == ctMaybeNothing -> Right Nothing
    TupleVal m inner | m == ctMaybeSome -> Just <$> valAnyStrArg name ctMaybeSomeValue inner
    _ -> Left $ printf "The %s of a %s is %s, which is not a Maybe String" (runtimeArgKey argName) name (show v)

-- | The same, where the argument is a whole number.
valIntArg :: (RuntimeValue r) => String -> String -> Args r -> Either String Integer
valIntArg name argName args = do
  v <- valArg name argName args
  case v of
    IntVal i -> Right i
    _        -> Left $ printf "The %s of a %s is %s, which is not an Integer" (runtimeArgKey argName) name (show v)

-- | The same, where the argument may be absent, read by the given reader where it is there.
valMaybeArg :: (RuntimeValue r) => (Val r -> Either String a) -> String -> String -> Args r -> Either String (Maybe a)
valMaybeArg readInner name argName args = do
  v <- valArg name argName args
  valMaybe readInner v

-- | A @\/Data\/Maybe@ as the value it holds, read by the given reader, or nothing.
valMaybe :: (RuntimeValue r) => (Val r -> Either String a) -> Val r -> Either String (Maybe a)
valMaybe _ (TupleVal m _) | m == ctMaybeNothing = Right Nothing
valMaybe readInner (TupleVal m inner) | m == ctMaybeSome = Just <$> (readInner =<< valArg m ctMaybeSomeValue inner)
valMaybe _ v = Left $ printf "%s is not a Maybe" (show v)

-- | A @\/Data\/Time\/Date@ as the day it is. A date naming no day that exists is reported rather
-- than moved onto a day that does.
valDate :: (RuntimeValue r) => Val r -> Either String Day
valDate (TupleVal name args) | name == timeDatePath = do
  year <- valIntArg name timeDateYear args
  month <- valIntArg name timeDateMonth args
  day <- valIntArg name timeDateDay args
  maybe (Left $ printf "%d-%d-%d is no day that exists" year month day) Right $ fromGregorianValid year (fromInteger month) (fromInteger day)
valDate v = Left $ printf "%s is not a %s" (show v) timeDatePath

-- | A @\/Data\/Time\/TimeOfDay@ as its hour and minute.
valTimeOfDay :: (RuntimeValue r) => Val r -> Either String (Integer, Integer)
valTimeOfDay (TupleVal name args) | name == timeOfDayPath = (,) <$> valIntArg name timeOfDayHour args <*> valIntArg name timeOfDayMinute args
valTimeOfDay v = Left $ printf "%s is not a %s" (show v) timeOfDayPath

-- | A @\/Data\/Time\/TimeRange@ as its start and end.
valTimeRange :: (RuntimeValue r) => Val r -> Either String ((Integer, Integer), (Integer, Integer))
valTimeRange (TupleVal name args) | name == timeRangePath = (,) <$> (valTimeOfDay =<< valArg name timeRangeStart args) <*> (valTimeOfDay =<< valArg name timeRangeEnd args)
valTimeRange v = Left $ printf "%s is not a %s" (show v) timeRangePath

-- | A list of values as the value of a @\/Data\/List@ holding them, which is the inverse of
-- 'valList'. Used to build a result in Haskell where the program did not build one itself.
valListOf :: [Val r] -> Val r
valListOf = foldr cons (TupleVal ctListNil H.empty)
  where cons v rest = TupleVal ctListCons $ H.fromList [(runtimeArgKey ctListConsHead, v), (runtimeArgKey ctListConsTail, rest)]
