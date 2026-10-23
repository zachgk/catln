--------------------------------------------------------------------
-- |
-- Module    :  Eval.Runtime.String
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines the primitives backing the operations of a @\/Catln\/CStr@, the packed
-- string the interpreter holds, declared in @stack\/core\/string.ct@.
--
-- Each is the same operation as an arm written there over a @\/Data\/String\/Text@, done on the
-- packed text with "Data.Text" rather than character by character. The two are arms of one
-- function, and which runs is the tree build's choice of the arm the string's representation
-- reaches. They are held to agree by the agreement tests derived for each pair
-- ('Eval.Testing.deriveOverlapTests').
--------------------------------------------------------------------

module Eval.Runtime.String where

import           CtConstants         (ctMaybeNothing, ctMaybeSome,
                                      ctMaybeSomeValue, falsePrim,
                                      runtimeArgKey, truePrim)
import           Data.Char           (chr, ord)
import qualified Data.HashMap.Strict as H
import qualified Data.Text           as T
import           Eval.Common
import           Text.Printf

-- | The primitives backing the @\/Catln\/CStr@ arms.
stringPrims :: [EPrim]
stringPrims =
  [ cstrPrim "cstrLength" (IntVal . fromIntegral . T.length)
  , cstrPrim "cstrIsEmpty" (boolVal . T.null)
  , cstrBinPrim "cstr++" "/l" "/r" (\l r -> CStrVal (l <> r))
  , cstrBinPrim "cstrEqual" "/a" "/b" (\a b -> boolVal (a == b))
  , cstrBinPrim "cstrCompare" "/a" "/b" (\a b -> comparisonVal (compare a b))
  , cstrPrim "cstrFirst" (maybeVal . fmap (CharVal . fst) . T.uncons)
  , cstrIntPrim "cstrAt" "/index" (\s i -> maybeVal (CharVal . fst <$> T.uncons (dropChars i s)))
  , cstrIntPrim "cstrTake" "/n" (\s n -> CStrVal (takeChars n s))
  , cstrIntPrim "cstrDrop" "/n" (\s n -> CStrVal (dropChars n s))
  , slicePrim
  , cstrPrim "cstrReversed" (CStrVal . T.reverse)
  , cstrBinPrim "cstrStartsWith" "/this" "/prefix" (\s p -> boolVal (p `T.isPrefixOf` s))
  , cstrBinPrim "cstrEndsWith" "/this" "/suffix" (\s x -> boolVal (x `T.isSuffixOf` s))
  , cstrBinPrim "cstrIndexOf" "/this" "/sub" (\s sub -> maybeVal (IntVal . fromIntegral <$> indexOf sub s))
  , cstrBinPrim "cstrContains" "/this" "/sub" (\s sub -> boolVal (sub `T.isInfixOf` s))
  , cstrPrim "cstrToUpper" (CStrVal . T.map toUpper)
  , cstrPrim "cstrToLower" (CStrVal . T.map toLower)
  , cstrPrim "cstrTrimStart" (CStrVal . T.dropWhile isSpace)
  , cstrPrim "cstrTrimEnd" (CStrVal . T.dropWhileEnd isSpace)
  , cstrPrim "cstrTrim" (CStrVal . T.dropWhileEnd isSpace . T.dropWhile isSpace)
  , replacePrim
  , cstrBinPrim "cstrSplit" "/this" "/sep" (\s sep -> strList (if T.null sep then [s] else T.splitOn sep s))
  , cstrPrim "cstrLines" (strList . T.lines)
  , cstrPrim "cstrWords" (strList . filter (not . T.null) . T.split isSpace)
  ]

-- What a character is, as @stack\/core\/char.ct@ says: the letters are the ASCII @A@ to @Z@ and
-- @a@ to @z@, and white space is a space or one of the codes 9 to 13. A string's operation reads
-- its characters as its characters' own operations do, so these are those and not "Data.Char"'s,
-- which read all of Unicode.
isUpper, isLower, isSpace :: Char -> Bool
isUpper c = c >= 'A' && c <= 'Z'
isLower c = c >= 'a' && c <= 'z'
isSpace c = c == ' ' || (ord c >= 9 && ord c <= 13)

toUpper, toLower :: Char -> Char
toUpper c | isLower c = chr (ord c - 32)
          | otherwise = c
toLower c | isUpper c = chr (ord c + 32)
          | otherwise = c

-- | The first @n@ characters, none for an @n@ of zero or less, as @takeChars@.
takeChars :: Integer -> T.Text -> T.Text
takeChars n s | n <= 0 = T.empty
              | otherwise = T.take (fromIntegral n) s

-- | Without the first @n@ characters, all of them for an @n@ of zero or less, as @dropChars@.
dropChars :: Integer -> T.Text -> T.Text
dropChars n s | n <= 0 = s
              | otherwise = T.drop (fromIntegral n) s

-- | Where @sub@ first occurs in @s@, the empty string at 0, as @indexOfChars@.
indexOf :: T.Text -> T.Text -> Maybe Int
indexOf sub s
  | T.null sub = Just 0
  | otherwise = case T.breakOn sub s of
      (before, after) | T.null after -> Nothing
                      | otherwise -> Just (T.length before)

-- | @slice(start, end)@: the characters from @start@ up to @end@, taken after dropping, as the
-- @String\/Text@ arm writes it.
slicePrim :: EPrim
slicePrim = EPrim "cstrSlice" prim
  where
    prim args = do
      s <- cstrArg "cstrSlice" "/this" args
      start <- intArg "cstrSlice" "/start" args
      end <- intArg "cstrSlice" "/end" args
      return $ CStrVal $ takeChars (end - start) (dropChars start s)

-- | @replace(old, new)@: each occurrence of @old@ replaced, each found after the one before it,
-- and an empty @old@ occurring nowhere.
replacePrim :: EPrim
replacePrim = EPrim "cstrReplace" prim
  where
    prim args = do
      s <- cstrArg "cstrReplace" "/this" args
      old <- cstrArg "cstrReplace" "/old" args
      new <- cstrArg "cstrReplace" "/new" args
      return $ CStrVal $ if T.null old then s else T.replace old new s

-- | A primitive of the string it is called on (@this@) alone.
cstrPrim :: String -> (T.Text -> Val) -> EPrim
cstrPrim name f = EPrim name prim
  where
    prim args = f <$> cstrArg name "/this" args

-- | A primitive of two strings, the arguments @a@ and @b@ of it.
cstrBinPrim :: String -> String -> String -> (T.Text -> T.Text -> Val) -> EPrim
cstrBinPrim name a b f = EPrim name prim
  where
    prim args = f <$> cstrArg name a args <*> cstrArg name b args

-- | A primitive of the string it is called on (@this@) and an integer argument.
cstrIntPrim :: String -> String -> (T.Text -> Integer -> Val) -> EPrim
cstrIntPrim name argName f = EPrim name prim
  where
    prim args = f <$> cstrArg name "/this" args <*> intArg name argName args

-- | The packed text of a @\/Catln\/CStr@ argument. A string held any other way is not one the
-- primitive is declared to take, so the tree build was to have converted it.
cstrArg :: String -> String -> Args -> Either String T.Text
cstrArg name argName args = case H.lookup argName args of
  Just (CStrVal t) -> Right t
  Just v           -> Left $ printf "%s was given %s for %s, which is not a /Catln/CStr" name (show v) argName
  Nothing          -> Left $ printf "%s has no %s argument, only %s" name argName (show $ H.keys args)

boolVal :: Bool -> Val
boolVal True  = TupleVal truePrim H.empty
boolVal False = TupleVal falsePrim H.empty

intArg :: String -> String -> Args -> Either String Integer
intArg name argName args = case H.lookup argName args of
  Just (IntVal i) -> Right i
  v               -> Left $ printf "%s was given %s for %s, which is not an Integer" name (show v) argName

-- | A @\/Data\/Maybe@ of a value.
maybeVal :: Maybe Val -> Val
maybeVal (Just v) = TupleVal ctMaybeSome (H.singleton (runtimeArgKey ctMaybeSomeValue) v)
maybeVal Nothing  = TupleVal ctMaybeNothing H.empty

-- | A @\/Data\/Algebra\/Comparison@, as @compareChars@ answers.
comparisonVal :: Ordering -> Val
comparisonVal LT = TupleVal "/Data/Algebra/LessThan" H.empty
comparisonVal EQ = TupleVal "/Data/Algebra/Equal" H.empty
comparisonVal GT = TupleVal "/Data/Algebra/GreaterThan" H.empty

-- | A list of packed strings.
strList :: [T.Text] -> Val
strList = valListOf . map CStrVal
