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
-- ('Syntax.Ct.Formatter.formatRootPrgm').
--------------------------------------------------------------------

module Syntax.Ct.Results where

import qualified Data.HashMap.Strict as H
import           Data.Maybe          (fromMaybe)
import           Data.UUID           (UUID)

import           CtConstants
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Builder
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

-- | Each annotation shown with its value, given the values by the meta ID of their annotation.
--
-- A print is given its value as its result. Any other annotation with a value becomes a print of
-- itself, so that every annotation shown with a value is written the one way: as an expression and
-- the @>>>@ lines under it. An annotation with no value is left as it was written, a print's result
-- included.
showAnnotResults :: (MetaDat m, Show m) => H.HashMap UUID String -> RawPrgm m -> RawPrgm m
showAnnotResults vals = mapRawAnnots fixAnnot
  where
    fixAnnot annot = case H.lookup (getMetaId $ getExprMeta annot) vals of
      Just val -> RawAnnot $ printWithResult (getExprMeta annot) (fromMaybe annot $ printAnnotArg annot) val
      Nothing  -> RawAnnot annot
