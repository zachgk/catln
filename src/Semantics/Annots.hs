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
import           Constants
import           Semantics      (exprPath)
import           Semantics.Prgm

isElseAnnot :: (ExprClass e) => CompAnnot (e m) -> Bool
isElseAnnot e = exprPath e == elseAnnot

hasElseAnnot :: (ExprClass e) => ObjArr e m -> Bool
hasElseAnnot ObjArr{roaAnnots} = any isElseAnnot roaAnnots
