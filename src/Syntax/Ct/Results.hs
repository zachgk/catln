--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Results
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module writes what a program's annotations evaluated to back into the program as written,
-- as the @>>>@ lines of a print, for the program to be formatted with them
-- ('Syntax.Ct.Formatter.formatRootPrgm'). What went wrong with the program is written into it the
-- same way, as the @!!!@ lines of a note under the statement it is about.
--------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}

module Syntax.Ct.Results where

import           Control.Monad              (forM)
import           Control.Monad.Trans.Writer (execWriter, tell)
import qualified Data.HashMap.Strict        as H
import           Data.List                  (mapAccumL, maximumBy, sortOn)
import           Data.Maybe                 (fromMaybe, isNothing, mapMaybe)
import           Data.Ord                   (comparing)
import           Data.UUID                  (UUID)
import           Text.Megaparsec            (SourcePos (..))

import           CRes
import           CtConstants
import           Semantics.Prgm
import           Semantics.Types
import           Semantics.Val              (Val (..), pattern StrVal)
import           Syntax.Ct.Builder
import           Syntax.Ct.MapRawMeta       (mapMetaRawStatementM)
import           Syntax.Ct.Prgm

-- | Each annotation statement of a program, at whatever depth, rewritten as a statement.
mapRawAnnots :: (RawExpr m -> RawStatement RawExpr m) -> RawPrgm m -> RawPrgm m
mapRawAnnots f (RawPrgm imports stmts) = RawPrgm imports (map fixTree stmts)
  where
    fixTree (RawStatementTree st subs) = RawStatementTree (fixStmt st) (map fixTree subs)
    fixStmt (RawAnnot annot) = f annot
    fixStmt st               = st

-- | What a print annotation prints, or 'Nothing' for any other annotation.
printAnnotArg :: (MetaDat m, Show m) => RawExpr m -> Maybe (RawExpr m)
printAnnotArg annot
  | nameStr (exprPath annot) == printAnnot = case H.lookup (partialKey (mkName printAnnotText)) (rawExprAppliedArgsMap annot) of
      Just (Just (_, Just e)) -> Just e
      _                       -> Nothing
  | otherwise = Nothing

-- | A print of an expression, with what it printed as its result.
--
-- Keeps the meta of the annotation it replaces, so it is still the same annotation to anything that
-- finds annotations by their meta.
printWithResult :: (MetaDat m, Show m) => Meta m -> RawExpr m -> String -> RawExpr m
printWithResult m p result = (rawVal printAnnot `applyRawArgs` [(Just $ partialKey (mkName printAnnotText), p), (Just $ partialKey (mkName resultAnnotText), rawStr result)]) `setExprMeta` m

-- | Each print's result, set to the golden of the print whose printed expression is at the range.
setPrintResults :: (MetaDat m, Show m) => [(CodeRangeDat, String)] -> RawPrgm m -> RawPrgm m
setPrintResults goldens = mapRawAnnots fixAnnot
  where
    fixAnnot annot
      | Just pExpr <- printAnnotArg annot
      , Just crange <- getMetaPos (getExprMeta pExpr)
      , Just golden <- lookup crange goldens
      = RawAnnot $ printWithResult (getExprMeta annot) pExpr golden
    fixAnnot annot = RawAnnot annot

-- | Each annotation rewritten as the one given for its meta ID, which keeps that meta and so is still
-- the same annotation to anything that finds annotations by their meta.
setAnnotsById :: (MetaDat m, Show m) => H.HashMap UUID (RawExpr m) -> RawPrgm m -> RawPrgm m
setAnnotsById updated = mapRawAnnots $ \annot -> RawAnnot $ case H.lookup (getMetaId $ getExprMeta annot) updated of
  Just annot' -> annot' `setExprMeta` getExprMeta annot
  Nothing     -> annot

-- | A value written as the expression constructing it: a literal, or a tuple of its name applied to
-- each of its arguments written the same way. A value only the runtime can hold has no such
-- expression.
valToRawExpr :: (MetaDat m, Show m) => Val r -> Either String (RawExpr m)
valToRawExpr (StrVal s) = Right $ rawStr s
valToRawExpr (IntVal i) = Right $ rawInt i
valToRawExpr (FloatVal d) = Right $ RawCExpr emptyMetaN (CFloat d)
valToRawExpr (CharVal c) = Right $ RawCExpr emptyMetaN (CChar c)
valToRawExpr (TupleVal name args) = do
  args' <- forM (sortOn fst $ H.toList args) $ \(argName, argVal) ->
    (Just $ partialKey $ mkName $ dropWhile (== '/') argName,) <$> valToRawExpr argVal
  return $ rawVal name `applyRawArgs` args'
valToRawExpr RuntimeVal{} = Left "a value only the runtime holds the inside of has no expression to write it as"
valToRawExpr NoVal = Left "no value has no expression to write it as"

-- | What became of an annotation that was asked to be shown: the text it was shown as, or what
-- went wrong in showing it.
data AnnotShown
  = Shown String
  | ShownFailed [CNote]

-- | Each annotation shown as what became of it, given that by the meta ID of the annotation.
--
-- A print is given its value as its result. Any other annotation with a value becomes a print of
-- itself, so that every annotation shown with a value is written the one way: as an expression and
-- the @>>>@ lines under it. An annotation that failed is left as it was written, a print's result
-- included, with what it failed with noted under it. One with neither is left as it was written.
showAnnotResults :: (MetaDat m, Show m) => H.HashMap UUID AnnotShown -> RawPrgm m -> RawPrgm m
showAnnotResults vals (RawPrgm imports stmts) = RawPrgm imports (map fixTree stmts)
  where
    fixTree (RawStatementTree st subs) = case st of
      RawAnnot annot -> case H.lookup (getMetaId $ getExprMeta annot) vals of
        Just (Shown val) -> RawStatementTree (RawAnnot $ printWithResult (getExprMeta annot) (fromMaybe annot $ printAnnotArg annot) val) subs'
        Just (ShownFailed notes) -> RawStatementTree st (map noteTree notes ++ subs')
        Nothing -> RawStatementTree st subs'
      _ -> RawStatementTree st subs'
      where subs' = map fixTree subs

-- | A note, as the statement writing it.
--
-- Written without where it is: it is written under the statement it is about, which says so.
noteTree :: (MetaDat m, Show m) => CNote -> RawStatementTree RawExpr m
noteTree note = RawStatementTree (RawAnnot annot) []
  where
    annot = rawVal noteAnnot `applyRawArgs` [(Just $ partialKey (mkName noteAnnotKind), rawStr kind), (Just $ partialKey (mkName noteAnnotText), rawStr (show note))]
    kind = case typeCNote note of
      CNoteError -> "error"
      CNoteWarn  -> "warning"

-- | Each note written under the statement it is about, which is the statement written last before
-- where the note is: the innermost one, where a statement and one under it start at the same place.
-- The notes of a statement are written first under it, so they follow the line they are about.
--
-- A note that says it is nowhere in the page — placed nowhere, in another page, or before its first
-- statement — is about the page as a whole, and is written at its top.
notePage :: (MetaDat m, Show m) => [CNote] -> RawPrgm m -> RawPrgm m
notePage [] prgm = prgm
notePage notes (RawPrgm imports stmts) = RawPrgm imports (map noteTree unplaced ++ snd (mapAccumL place 0 stmts))
  where
    starts = concatMap treeStarts stmts
    treeStarts (RawStatementTree st subs) = statementStart st : concatMap treeStarts subs

    placeOf note = do
      (at, _) <- posCNote note
      let before = [(start, i) | (i, Just start) <- zip [0 :: Int ..] starts, sourceName start == sourceName at, posKey start <= posKey at]
      if null before
        then Nothing
        else Just $ snd $ maximumBy (comparing (\(start, i) -> (posKey start, i))) before
    posKey pos = (sourceLine pos, sourceColumn pos)

    placed = H.fromListWith (flip (++)) [(i, [note]) | note <- notes, Just i <- [placeOf note]]
    unplaced = filter (isNothing . placeOf) notes

    -- Numbered as 'starts' is, before the statements under it.
    place i (RawStatementTree st subs) = (i', RawStatementTree st (map noteTree (H.lookupDefault [] i placed) ++ subs'))
      where (i', subs') = mapAccumL place (i + 1) subs

-- | Where a statement starts: the earliest place any part of it was written.
statementStart :: (MetaDat m, Show m) => RawStatement RawExpr m -> Maybe SourcePos
statementStart st = case mapMaybe (fmap fst) $ execWriter $ mapMetaRawStatementM (\_ m -> tell [getMetaPos m] >> return m) st of
  []        -> Nothing
  positions -> Just $ minimum positions
