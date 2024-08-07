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

import           Control.Monad
import           Control.Monad.Trans   (MonadTrans (lift))
import           Data.Aeson
import           Data.String.Builder   (Builder, build, literal)
import           Semantics.Prgm
import           Text.Megaparsec       (pstateSourcePos)
import           Text.Megaparsec.Error
import           Utils                 (withIndent)

data CNoteType
  = CNoteError
  | CNoteWarn
  deriving (Eq, Ord, Generic, ToJSON)

instance Semigroup CNoteType where
  CNoteWarn <> CNoteWarn = CNoteWarn
  _ <> _                 = CNoteError
instance Monoid CNoteType where
  mempty = CNoteWarn

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

showRecursiveCNoteI :: Int -> CNoteI -> Builder
showRecursiveCNoteI indent (GenMapCErr _ s subs) = do
  withIndent indent s
  forM_ subs $ \sub -> do
    case sub of
      (subKey, CRes _ r) -> withIndent (indent+1) (subKey ++ " - " ++ r)
      (subKey, CErr n) -> do
        withIndent (indent+1) ("Failed: " ++ subKey)
        mapM_ (showRecursiveCNote (indent + 2)) n
showRecursiveCNoteI indent (WrapCN subs s) = do
  withIndent indent s
  forM_ subs $ \sub -> do
    showRecursiveCNote (indent + 2) sub
showRecursiveCNoteI _ n = literal (show n)

instance Show CNoteI where
  show (GenCNote _ s) = s
  show (GenCErr _ s) = s
  show n@GenMapCErr{} = build $ showRecursiveCNoteI 0 n
  show n@WrapCN{} = build $ showRecursiveCNoteI 0 n
  show (ParseCErr p) = errorBundlePretty p
  show (BuildTreeCErr _ s) = printf "Failed to Build Tree: %s" s
  show (AssertCErr s) = printf "Failed assertion: %s" s
  show (EvalCErr st err) = printf "%s\n\tStack trace:\n\t\t%s" err (intercalate "\n\t\t" st)

instance CNoteTC CNoteI where
  metaCNote _ = Nothing
  posCNote (ParseCErr bundle) = Just (pos, pos)
    where pos = pstateSourcePos $ bundlePosState bundle
  posCNote _ = Nothing

  typeCNote GenCNote{}       = CNoteWarn
  typeCNote (WrapCN notes _) = mconcat $ map typeCNote notes
  typeCNote _                = CNoteError

  showRecursiveCNote i n = showRecursiveCNoteI i n

wrapCRes :: String -> CRes r -> CRes r
wrapCRes _ (CRes [] r)    = CRes [] r
wrapCRes s (CRes notes r) = CRes [MkCNote $ WrapCN notes s] r
wrapCRes s (CErr notes)   = CErr [MkCNote $ WrapCN notes s]

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

newtype CResT m r = CResT { runCResT :: m (CRes r) }

instance Monad m => Functor (CResT m) where
  fmap = liftM

instance Monad m => Applicative (CResT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (CResT m) where
  return = CResT . return . CRes []
  x >>= f = CResT $ do maybe_value <- runCResT x
                       case maybe_value of
                         CErr notes    -> return $ CErr notes
                         CRes notes value -> do
                           value' <- runCResT (f value)
                           case value' of
                             CErr notes2 -> return $ CErr (notes ++ notes2)
                             CRes notes2 value'' -> return $ CRes (notes ++ notes2) value''

instance Monad m => MonadFail (CResT m) where
  fail = CResT . return . fail

instance MonadTrans CResT where
  lift = CResT . fmap (CRes [])

asCResT :: (Monad m) => CRes r -> CResT m r
asCResT = CResT . return

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
