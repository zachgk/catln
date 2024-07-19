--------------------------------------------------------------------
-- |
-- Module    :  Semantics.Annots
-- Copyright :  (c) Zach Kimberg 2023
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module contains semantics around parsing and dealing with annotations.
--------------------------------------------------------------------

module Semantics.Annots where
import           CtConstants
import qualified Data.HashMap.Strict as H
import           Data.Maybe
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf

isElseAnnot :: (ExprClass e) => CompAnnot (e m) -> Bool
isElseAnnot e = exprPath e == elseAnnot

isCtxAnnot :: (ExprClass e) => CompAnnot (e m) -> Bool
isCtxAnnot e = exprPath e == ctxAnnot

hasElseAnnot :: (ObjArrClass oa, ExprClass e) => oa e m -> Bool
hasElseAnnot oa = any isElseAnnot $ getOaAnnots oa

getRuntimeAnnot :: (MetaDat m, Show m) => CompAnnot [Expr m] -> Maybe String
getRuntimeAnnot es = case mapMaybe aux es of
  [e] -> Just e
  _   -> Nothing
  where
    aux e | exprPath e /= runtimeAnnot = Nothing
    aux e = case H.lookup (partialKey runtimeAnnotK) (exprAppliedArgsMap e) of
      Just (Just (_, Just (CExpr _ (CStr s)))) -> Just s
      _ -> error $ printf "Unknown or missing key in #runtime annot: %s" (show e)
