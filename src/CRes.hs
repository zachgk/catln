--------------------------------------------------------------------
-- |
-- Module    :  CRes
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module CRes where

import           Data.List                      ( intercalate )
import           Text.Megaparsec.Error (errorBundlePretty)
import qualified Data.Text.Lazy as T
import Text.Pretty.Simple
import           Text.Printf

import Syntax

-- compile errors
type EStacktrace = [String]
data CNote
  = GenCNote String
  | GenCErr String
  | ParseCErr ParseErrorRes
  | TypeCheckCErr
  | BuildTreeCErr String
  | AssertCErr String
  | EvalCErr EStacktrace String
  | WrapCN [CNote] String
  deriving (Eq, Show)

prettyCNote :: CNote -> String
prettyCNote (ParseCErr p) = errorBundlePretty p
prettyCNote (WrapCN n s) = s ++ "\n\t\t" ++ intercalate "\n\t\t" (map prettyCNote n)
prettyCNote (EvalCErr st err) = printf "%s\n\tStack trace:\n\t\t%s" err (intercalate "\n\t\t" st)
prettyCNote n = T.unpack $ pShow n

wrapCErr :: [CNote] -> String -> CRes r
wrapCErr notes s = CErr [WrapCN notes s]

data CRes r
  = CRes [CNote] r
  | CErr [CNote]
  deriving (Eq, Show)

getCNotes :: CRes r -> [CNote]
getCNotes (CRes notes _) = notes
getCNotes (CErr notes) = notes

partitionCRes :: [CRes r] -> ([CNote], [CRes r])
partitionCRes = aux ([], [])
  where
    aux x [] = x
    aux (errRes, resRes) (r@CRes{}:xs) = aux (errRes, r:resRes) xs
    aux (errRes, resRes) ((CErr newErrNotes):xs) = aux (newErrNotes ++ errRes, resRes) xs

instance Functor CRes where
  fmap f (CRes notes r) = CRes notes (f r)
  fmap _ (CErr notes) = CErr notes

instance Applicative CRes where
  pure = CRes []
  (CRes notesA f) <*> (CRes notesB b) = CRes (notesA ++ notesB) (f b)
  resA <*> resB = CErr (getCNotes resA ++ getCNotes resB)

instance Monad CRes where
  return = pure
  (CRes notesA a) >>= f = case f a of
    (CRes notesB b) -> CRes (notesA ++ notesB) b
    (CErr notesB) -> CErr (notesA ++ notesB)
  (CErr notes) >>= _ = CErr notes


