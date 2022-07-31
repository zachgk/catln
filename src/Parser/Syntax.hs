--------------------------------------------------------------------
-- |
-- Module    :  Parser.Syntax
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines common types for parsing.
--------------------------------------------------------------------

module Parser.Syntax where

import qualified Data.HashMap.Strict as H

import           Syntax
import           Syntax.Prgm
import           Syntax.Types
import           Text.Megaparsec     (SourcePos)
import           Utils

type ParseMeta = PreTyped
type PTupleArg = RawTupleArg ParseMeta
type PExpr = RawExpr ParseMeta
type PPattern = Pattern PExpr ParseMeta
type PCompAnnot = CompAnnot PExpr
type PGuard = Guard PExpr
type PDeclSubStatement = RawDeclSubStatement ParseMeta
type PDeclLHS = DeclLHS PExpr ParseMeta
type PDecl = RawDecl ParseMeta
type PObjectMap = ObjectMap PExpr ParseMeta
type PMultiTypeDef = MultiTypeDef ParseMeta
type PTypeDef = TypeDef ParseMeta
type PStatement = RawStatement ParseMeta
type PArgMetaMap = H.HashMap ArgName ParseMeta
type PObjArg = ObjArg ParseMeta
type PObject = Object ParseMeta
type PArrow = Arrow PExpr ParseMeta
type PPrgm = RawPrgm ParseMeta
type PPrgmGraphData = GraphData PPrgm String
type PReplRes = ReplRes ParseMeta



type PSExpr = Expr ParseMeta
type PSCompAnnot = CompAnnot PSExpr
type PSGuard = Guard PSExpr
type PSDeclLHS = DeclLHS PSExpr ParseMeta

data PSemiDecl = PSemiDecl PSDeclLHS [PSCompAnnot] (Maybe PSExpr)
  deriving (Show)




type DesExpr = Expr ParseMeta
type DesCompAnnot = CompAnnot DesExpr
type DesGuard = Guard DesExpr
type DesObjectMap = ObjectMap DesExpr ParseMeta
type DesObject = Object ParseMeta
type DesArrow = Arrow DesExpr ParseMeta
type DesObjectMapItem = ObjectMapItem DesExpr ParseMeta
type DesPrgm = Prgm DesExpr ParseMeta
type DesPrgmGraphData = GraphData DesPrgm String

emptyMeta :: SourcePos -> SourcePos -> ParseMeta
emptyMeta p1 p2 = PreTyped TopType (Just (p1, p2, ""))

emptyMetaN :: ParseMeta
emptyMetaN = PreTyped TopType Nothing

emptyMetaM :: (Meta m) => String -> m -> m
emptyMetaM = labelPosM

emptyMetaE :: (Meta m, ExprClass e) => String -> e m -> m
emptyMetaE s e = labelPosM s $ getExprMeta e


rawVal :: String -> PExpr
rawVal name = RawValue m name
  where m = PreTyped (singletonType $ PartialType (PTypeName name) H.empty H.empty H.empty PtArgExact) Nothing

applyRawArgs :: (Meta m) => RawExpr m -> [(Maybe ArgName, RawExpr m)] -> RawExpr m
applyRawArgs base args = RawTupleApply (emptyMetaE "app" base) (emptyMetaE "base" base, base) (map mapArg args)
  where
    mapArg (Just argName, argVal) = RawTupleArgNamed (emptyMetaE argName base) argName argVal
    mapArg (Nothing, argVal) = RawTupleArgInfer (emptyMetaE "noArg" base) argVal
