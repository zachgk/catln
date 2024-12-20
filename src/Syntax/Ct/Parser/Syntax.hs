--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Parser.Syntax
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines common types for parsing.
--------------------------------------------------------------------

module Syntax.Ct.Parser.Syntax where

import qualified Data.HashMap.Strict as H

import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Prgm
import           Text.Printf
import           Utils

type ParseMetaDat = ()
type ParseMeta = Meta ParseMetaDat
type PExpr = RawExpr ParseMetaDat
type PVarArgMap = VarArgMap RawExpr ParseMetaDat
type PRawApply = RawApply RawExpr ParseMetaDat
type PObjExpr = PExpr
type PCompAnnot = CompAnnot PExpr
type PObjArr = RawObjArr RawExpr ParseMetaDat
type PStatement = RawStatement RawExpr ParseMetaDat
type PStatementTree = RawStatementTree RawExpr ParseMetaDat
type PArgMetaMap = H.HashMap ArgName [ParseMeta]
type PPrgm = RawPrgm ParseMetaDat
type PPrgmGraphData = GraphData PPrgm FileImport
type PReplRes = ReplRes ParseMetaDat

type PDeclTree = (PObjArr, [PStatementTree])

type PSExpr = Expr ParseMetaDat
type PSCompAnnot = CompAnnot PSExpr
type PSObjArr = ObjArr Expr ParseMetaDat





type DesExpr = Expr ParseMetaDat
type DesCompAnnot = CompAnnot DesExpr
type DesObjArr = ObjArr Expr ParseMetaDat
type DesObjectMapItem = ObjArr Expr ParseMetaDat
type DesObjectMap = ObjectMap Expr ParseMetaDat
type DesPrgm = Prgm Expr ParseMetaDat
type DesPrgmGraphNodes = GraphNodes DesPrgm FileImport
type DesPrgmGraphData = GraphData DesPrgm FileImport

parseTVVar :: String -> Maybe Type
parseTVVar ('$':'_':n) = Just $ TypeVar (TVVar $ partialKey n) TVExt
parseTVVar ('$':n)     = Just $ TypeVar (TVVar $ partialKey n) TVInt
parseTVVar _           = Nothing

rawExprWithType :: Type -> RawExpr ParseMetaDat -> RawExpr ParseMetaDat
rawExprWithType t (RawCExpr m c) = RawCExpr (mWithType t m) c
rawExprWithType t (RawValue m n) = RawValue (mWithType t m) n
rawExprWithType t (RawHoleExpr m h) = RawHoleExpr (mWithType t m) h
rawExprWithType t (RawList m l) = RawList (mWithType t m) l
rawExprWithType _ e = error $ printf "rawExprWithType for unexpected type: %s" (show e)

mapOAObjPath :: (Show m, MetaDat m) => ((Meta m, TypeName) -> Expr m) -> ObjArr Expr m -> ObjArr Expr m
mapOAObjPath f = mapOAObjExpr (mapExprPath f)

mapExprAppliedArg :: (DesExpr -> DesExpr) -> ArgName -> DesExpr -> DesExpr
mapExprAppliedArg _ _ e@CExpr{} = e
mapExprAppliedArg _ _ e@Value{} = e
mapExprAppliedArg _ _ e@HoleExpr{} = e
mapExprAppliedArg f argName (AliasExpr base alias) = AliasExpr (mapExprAppliedArg f argName base) alias
mapExprAppliedArg f argName (EWhere m base cond) = EWhere m (mapExprAppliedArg f argName base) cond
mapExprAppliedArg f argName (TupleApply m (bm, be) (EAppArg oa@ObjArr{oaObj=Just an, oaArr=Just (Just aExpr, oaM)})) | argName == inExprSingleton an = TupleApply m (bm, be) (EAppArg oa{oaArr=Just (Just (f aExpr), oaM)})
mapExprAppliedArg f argName (TupleApply m (bm, be) a) = TupleApply m (bm, mapExprAppliedArg f argName be) a
