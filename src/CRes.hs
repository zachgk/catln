--------------------------------------------------------------------
-- |
-- Module    :  CRes
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module focuses on defining 'CRes', a monad to store the
-- results of compiler steps.
--------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GADTs          #-}

module CRes where

import           Data.List             (intercalate)
import           GHC.Generics          (Generic)
-- import qualified Data.Text.Lazy as T
-- import Text.Pretty.Simple
import           Text.Printf

import           Control.Monad         (forM_)
import           Data.Aeson
import           Data.String.Builder   (Builder, build, literal)
import qualified Data.Text.Lazy        as T
import           Semantics.Prgm
import           Text.Megaparsec       (pstateSourcePos)
import           Text.Megaparsec.Error
import           Text.Pretty.Simple

data CNoteType
  = CNoteError
  | CNoteWarn
  deriving (Eq, Ord, Generic, ToJSON)

class CNoteTC n where
  metaCNote :: n -> Maybe (Meta ())
  posCNote :: n -> CodeRange
  posCNote n = metaCNote n >>= getMetaPos
  typeCNote :: n -> CNoteType
  showRecursiveCNote :: Int -> n -> Builder

data CNote
  where
  MkCNote :: (CNoteTC a, Show a) => a -> CNote

instance CNoteTC CNote where
  metaCNote (MkCNote a) = metaCNote a
  posCNote (MkCNote a) = posCNote a
  typeCNote (MkCNote a) = typeCNote a
  showRecursiveCNote i (MkCNote a) = showRecursiveCNote i a

instance Show CNote
  where
  showsPrec p (MkCNote a) = showsPrec p a

instance ToJSON CNote where
  toJSON (MkCNote n) = object ["msg".=show n, "pos".= posCNote n, "tp" .= typeCNote n, "id".=(getMetaID <$> metaCNote n)]

data CNoteI
  = GenCNote CodeRange String
  | GenCErr CodeRange String
  | GenMapCErr CodeRange String [(String, CRes String)]
  | ParseCErr ParseErrorRes
  | BuildTreeCErr CodeRange String
  | AssertCErr String
  | EvalCErr [String] String
  | WrapCN [CNote] String

withIndent :: Int -> String -> Builder
withIndent indent s = literal (replicate (4*indent) ' ' ++ s ++ "\n")

showRecursiveCNoteI :: Int -> CNoteI -> Builder
showRecursiveCNoteI indent (GenMapCErr _ s subs) = do
  withIndent indent s
  forM_ subs $ \sub -> do
    case sub of
      (subKey, CRes _ r) -> withIndent (indent+1) (subKey ++ " - " ++ r)
      (subKey, CErr n) -> do
        withIndent (indent+1) ("Failed: " ++ subKey)
        mapM_ (showRecursiveCNote (indent + 2)) n
showRecursiveCNoteI _ n = literal (show n)

instance Show CNoteI where
  show (GenCNote _ s) = s
  show (GenCErr _ s) = s
  show n@GenMapCErr{} = build $ showRecursiveCNoteI 0 n
  show (ParseCErr p) = errorBundlePretty p
  show (BuildTreeCErr _ s) = printf "Failed to Build Tree: %s" (T.unpack $ pShow s)
  show (AssertCErr s) = printf "Failed assertion: %s" s
  show (EvalCErr st err) = printf "%s\n\tStack trace:\n\t\t%s" err (intercalate "\n\t\t" st)
  show (WrapCN n s) = s ++ "\n\t\t" ++ intercalate "\n\t\t" (map show n)

instance CNoteTC CNoteI where
  metaCNote _ = Nothing
  posCNote (ParseCErr bundle) = Just (pos, pos, "")
    where pos = pstateSourcePos $ bundlePosState bundle
  posCNote _ = Nothing

  typeCNote GenCNote{} = CNoteWarn
  typeCNote _          = CNoteError

  showRecursiveCNote i n = showRecursiveCNoteI i n

wrapCErr :: [CNote] -> String -> CRes r
wrapCErr notes s = CErr [MkCNote $ WrapCN notes s]

data CRes r
  = CRes [CNote] r
  | CErr [CNote]
  deriving (Show, Generic, ToJSON)

getCNotes :: CRes r -> [CNote]
getCNotes (CRes notes _) = notes
getCNotes (CErr notes)   = notes

partitionCRes :: [CRes r] -> ([CNote], [CRes r])
partitionCRes = aux ([], [])
  where
    aux x [] = x
    aux (errRes, resRes) (r@CRes{}:xs) = aux (errRes, r:resRes) xs
    aux (errRes, resRes) ((CErr newErrNotes):xs) = aux (newErrNotes ++ errRes, resRes) xs

instance Functor CRes where
  fmap f (CRes notes r) = CRes notes (f r)
  fmap _ (CErr notes)   = CErr notes

instance Applicative CRes where
  pure = CRes []
  (CRes notesA f) <*> (CRes notesB b) = CRes (notesA ++ notesB) (f b)
  resA <*> resB                       = CErr (getCNotes resA ++ getCNotes resB)

instance Monad CRes where
  return = pure
  (CRes notesA a) >>= f = case f a of
    (CRes notesB b) -> CRes (notesA ++ notesB) b
    (CErr notesB)   -> CErr (notesA ++ notesB)
  (CErr notes) >>= _ = CErr notes

instance MonadFail CRes where
  fail s = CErr [MkCNote $ GenCErr Nothing s]

prettyCNotes :: [CNote] -> String
prettyCNotes notes = "\n\n\t\t" ++ intercalate "\n\n\t\t" (map prettyNote notes)
  where
    prettyNote note = printf "%s%s:\n\t\t%s" (showMsg $ typeCNote note) (showPos $ posCNote note) (show note)

    showMsg :: CNoteType -> String
    showMsg CNoteError = "Error"
    showMsg CNoteWarn  = "Warning"

    showPos :: CodeRange -> String
    showPos (Just jcr) = " at " ++ showCodeRange jcr
    showPos Nothing    = ""

failOnErrorNotes :: CRes n -> CRes n
failOnErrorNotes (CRes [] r) = CRes [] r
failOnErrorNotes (CRes notes r) = if any (\n -> typeCNote n == CNoteError) notes
  then CErr notes
  else CRes notes r
failOnErrorNotes (CErr notes) = CErr notes

cresToMaybe :: CRes r -> Maybe r
cresToMaybe (CRes _ r) = Just r
cresToMaybe CErr{}     = Nothing

fromCRes :: CRes r -> r
fromCRes (CRes _ r) = r
fromCRes (CErr e)   = error $ printf "Failed fromCRes with error: %s" (show e)

catCRes :: [CRes r] -> CRes [r]
catCRes [] = return []
catCRes (CRes notes r:rs) = do
  rs' <- catCRes rs
  CRes notes (r:rs')
catCRes (CErr _:rs) = catCRes rs
