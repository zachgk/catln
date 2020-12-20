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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module CRes where

import           GHC.Generics          (Generic)
import           Data.List                      ( intercalate )
-- import qualified Data.Text.Lazy as T
-- import Text.Pretty.Simple
import           Text.Printf

import Syntax
import Text.Megaparsec (pstateSourcePos, SourcePos)
import Text.Megaparsec.Error
import Data.Aeson (ToJSON)

data CNoteType
  = CNoteError
  | CNoteWarn
  deriving (Eq, Ord, Generic, ToJSON)

class CNoteTC n where
  posCNote :: n -> Maybe SourcePos
  typeCNote :: n -> CNoteType

data CNote
  where
  MkCNote :: (CNoteTC a, Show a) => a -> CNote

instance CNoteTC CNote where
  posCNote (MkCNote a) = posCNote a
  typeCNote (MkCNote a) = typeCNote a

instance Show CNote
  where
  showsPrec p (MkCNote a) = showsPrec p a

data CNoteI
  = GenCNote (Maybe SourcePos) String
  | GenCErr (Maybe SourcePos) String
  | ParseCErr ParseErrorRes
  | BuildTreeCErr (Maybe SourcePos) String
  | AssertCErr String
  | EvalCErr [String] String
  | WrapCN [CNote] String

instance Show CNoteI where
  show (GenCNote _ s) = printf "Note: %s" s
  show (GenCErr _ s) = printf "Error: %s" s
  show (ParseCErr p) = errorBundlePretty p
  show (BuildTreeCErr _ s) = printf "Failed to Build Tree: %s" s
  show (AssertCErr s) = printf "Failed assertion: %s" s
  show (EvalCErr st err) = printf "%s\n\tStack trace:\n\t\t%s" err (intercalate "\n\t\t" st)
  show (WrapCN n s) = s ++ "\n\t\t" ++ intercalate "\n\t\t" (map show n)

instance CNoteTC CNoteI where
  posCNote (ParseCErr bundle) = Just $ pstateSourcePos $ bundlePosState bundle
  posCNote _ = Nothing

  typeCNote GenCNote{} = CNoteWarn
  typeCNote _ = CNoteError

wrapCErr :: [CNote] -> String -> CRes r
wrapCErr notes s = CErr [MkCNote $ WrapCN notes s]

data CRes r
  = CRes [CNote] r
  | CErr [CNote]
  deriving (Show)

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

failOnErrorNotes :: CRes n -> CRes n
failOnErrorNotes (CRes [] r) = CRes [] r
failOnErrorNotes (CRes notes r) = if any (\n -> typeCNote n == CNoteError) notes
  then CErr notes
  else CRes notes r
failOnErrorNotes (CErr notes) = CErr notes
