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
  | TupleVal String (H.HashMap String (Val r))
  -- | A value only the runtime knows the inside of — a closure, an action, compiled code.
  | RuntimeVal r
  | NoVal

-- | The arguments of an evaluated value, by the name each is declared under.
type Args r = H.HashMap String (Val r)

-- | A string: a @\/Data\/String\/Text@ holding the list of characters it is made of.
--
-- A string is not a primitive of its own. @stack\/core\/string.ct@ declares it as the one member of
-- the @String@ class, @String\/Text(chars -> ConsList[$T -> Char])@, so what an evaluation holds for
-- one is that tuple, with a
-- @Cons@ per character — which is what lets a string be taken apart and built up in Catln. This
-- pattern is how the compiler's own code reads and writes one as the Haskell 'String' it is to the
-- outside world: matching it walks the characters, and building it lays them out.
--
-- A tuple whose characters are not all 'CharVal's, or whose list does not end, does not match.
pattern StrVal :: String -> Val r
pattern StrVal s <- (valString -> Just s)
  where StrVal s = TupleVal ctStringText (H.singleton (runtimeArgKey ctStringChars) (valListOf (map CharVal s)))

-- | The characters of a string value, where it is one.
valString :: Val r -> Maybe String
valString (TupleVal name args) | name == ctStringText = charsOf =<< H.lookup (runtimeArgKey ctStringChars) args
  where
    charsOf (TupleVal n _) | n == ctListNil = Just []
    charsOf (TupleVal n as) | n == ctListCons = do
      h <- H.lookup (runtimeArgKey ctListConsHead) as
      t <- H.lookup (runtimeArgKey ctListConsTail) as
      case h of
        CharVal c -> (c:) <$> charsOf t
        _         -> Nothing
    charsOf _ = Nothing
valString _ = Nothing

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
instance (RuntimeValue r) => Eq (Val r) where
  (IntVal a) == (IntVal b)             = a == b
  (FloatVal a) == (FloatVal b)         = a == b
  (StrVal a) == (StrVal b)             = a == b
  (CharVal a) == (CharVal b)           = a == b
  (TupleVal an aa) == (TupleVal bn ba) = an == bn && aa == ba
  (RuntimeVal a) == b                  = rvEqVal a b
  a == (RuntimeVal b)                  = rvEqVal b a
  NoVal == NoVal                       = True
  _ == _                               = False

instance (RuntimeValue r) => Show (Val r) where
  show (IntVal i)   = show i
  show (FloatVal d) = show d
  show (StrVal s)   = show s
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
  hashWithSalt s (StrVal i)      = s `hashWithSalt` i
  hashWithSalt s (CharVal i)     = s `hashWithSalt` i
  hashWithSalt s (TupleVal n as) = s `hashWithSalt` n `hashWithSalt` as
  hashWithSalt s (RuntimeVal r)  = rvHash s r
  hashWithSalt s NoVal           = s

instance (RuntimeValue r) => ToJSON (Val r) where
  toJSON (IntVal v) = object ["tag".=("IntVal" :: String), "contents".=toJSON v]
  toJSON (FloatVal v) = object ["tag".=("FloatVal" :: String), "contents".=toJSON v]
  toJSON (StrVal v) = object ["tag".=("StrVal" :: String), "contents".=toJSON v]
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

-- | The same, where the argument must be a string — which every name and every file's writing is.
valStrArg :: (RuntimeValue r) => String -> String -> Args r -> Either String String
valStrArg name argName args = do
  v <- valArg name argName args
  case v of
    StrVal s -> Right s
    _        -> Left $ printf "The %s of a %s is %s, which is not a String" (runtimeArgKey argName) name (show v)

-- | The same, where the argument is a string which may be absent — which is a @\/Data\/Maybe@ of one.
valMaybeStrArg :: (RuntimeValue r) => String -> String -> Args r -> Either String (Maybe String)
valMaybeStrArg name argName args = do
  v <- valArg name argName args
  case v of
    TupleVal m _ | m == ctMaybeNothing -> Right Nothing
    TupleVal m inner | m == ctMaybeSome -> Just <$> valStrArg name ctMaybeSomeValue inner
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
