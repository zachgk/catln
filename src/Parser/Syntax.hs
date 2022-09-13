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

import           Data.List           (isPrefixOf)
import           Syntax
import           Syntax.Prgm
import           Syntax.Types
import           Text.Megaparsec     (SourcePos)
import           Utils

type ParseMeta = PreTyped
type PTupleArg = TupleArg RawExpr ParseMeta
type PExpr = RawExpr ParseMeta
type PPattern = Pattern RawExpr ParseMeta
type PCompAnnot = CompAnnot PExpr
type PGuard = Guard PExpr
type PDeclLHS = DeclLHS RawExpr ParseMeta
type PDecl = RawDecl ParseMeta
type PObjectMap = ObjectMap RawExpr ParseMeta
type PMultiTypeDef = MultiTypeDef ParseMeta
type PTypeDef = TypeDef ParseMeta
type PStatement = RawStatement ParseMeta
type PStatementTree = RawStatementTree ParseMeta
type PArgMetaMap = H.HashMap ArgName ParseMeta
type PObjArg = ObjArg ParseMeta
type PObject = Object ParseMeta
type PArrow = Arrow RawExpr ParseMeta
type PPrgm = RawPrgm ParseMeta
type PPrgmGraphData = GraphData PPrgm String
type PReplRes = ReplRes ParseMeta



type PSExpr = Expr ParseMeta
type PSCompAnnot = CompAnnot PSExpr
type PSGuard = Guard PSExpr
type PSDeclLHS = DeclLHS Expr ParseMeta

data PSemiDecl = PSemiDecl PSDeclLHS [PSCompAnnot] (Maybe PSExpr)
  deriving (Show)




type DesExpr = Expr ParseMeta
type DesCompAnnot = CompAnnot DesExpr
type DesGuard = Guard DesExpr
type DesObjectMap = ObjectMap Expr ParseMeta
type DesObject = Object ParseMeta
type DesArrow = Arrow Expr ParseMeta
type DesObjectMapItem = ObjectMapItem Expr ParseMeta
type DesPrgm = Prgm Expr ParseMeta
type DesPrgmGraphData = GraphData DesPrgm String

fromMaybeTypeName :: Maybe TypeName -> Type
fromMaybeTypeName = maybe TopType fromName
  where
    fromName n | "$" `isPrefixOf` n = TypeVar $ TVVar n
    fromName n = singletonType (PartialType (PRelativeName n) H.empty H.empty H.empty PtArgExact)

emptyMeta :: SourcePos -> SourcePos -> ParseMeta
emptyMeta p1 p2 = PreTyped TopType (Just (p1, p2, ""))

isAbsolutePath :: String -> Bool
isAbsolutePath name = "/" `isPrefixOf` name

getPath :: String -> Path
getPath name = if isAbsolutePath name then
  Absolute name
  else Relative name

rawVal :: String -> PExpr
rawVal name = RawValue m name
  where m = PreTyped (singletonType $ PartialType (PTypeName name) H.empty H.empty H.empty PtArgExact) Nothing

applyRawArgs :: (Meta m) => RawExpr m -> [(Maybe ArgName, RawExpr m)] -> RawExpr m
applyRawArgs base args = RawTupleApply (emptyMetaE "app" base) (emptyMetaE "base" base, base) (map mapArg args)
  where
    mapArg (Just argName, argVal) = TupleArgIO (emptyMetaE argName base) argName argVal
    mapArg (Nothing, argVal) = TupleArgO (emptyMetaE "noArg" base) argVal
