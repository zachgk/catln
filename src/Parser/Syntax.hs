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

type ParseMetaDat = ()
type ParseMeta = Meta ParseMetaDat
type PTupleArg = TupleArg RawExpr ParseMetaDat
type PExpr = RawExpr ParseMetaDat
type PPattern = Pattern RawExpr ParseMetaDat
type PCompAnnot = CompAnnot PExpr
type PGuard = Guard PExpr
type PDeclLHS = DeclLHS RawExpr ParseMetaDat
type PDecl = RawDecl ParseMetaDat
type PObjectMap = ObjectMap RawExpr ParseMetaDat
type PMultiTypeDef = MultiTypeDef ParseMetaDat
type PTypeDef = TypeDef ParseMetaDat
type PStatement = RawStatement ParseMetaDat
type PStatementTree = RawStatementTree ParseMetaDat
type PArgMetaMap = H.HashMap ArgName ParseMeta
type PObjArg = ObjArg ParseMetaDat
type PObject = Object ParseMetaDat
type PArrow = Arrow RawExpr ParseMetaDat
type PPrgm = RawPrgm ParseMetaDat
type PPrgmGraphData = GraphData PPrgm String
type PReplRes = ReplRes ParseMetaDat



type PSExpr = Expr ParseMetaDat
type PSCompAnnot = CompAnnot PSExpr
type PSGuard = Guard PSExpr
type PSDeclLHS = DeclLHS Expr ParseMetaDat

data PSemiDecl = PSemiDecl PSDeclLHS [PSCompAnnot] (Maybe PSExpr)
  deriving (Show)




type DesExpr = Expr ParseMetaDat
type DesCompAnnot = CompAnnot DesExpr
type DesGuard = Guard DesExpr
type DesObjectMap = ObjectMap Expr ParseMetaDat
type DesObject = Object ParseMetaDat
type DesArrow = Arrow Expr ParseMetaDat
type DesObjectMapItem = ObjectMapItem Expr ParseMetaDat
type DesPrgm = Prgm Expr ParseMetaDat
type DesPrgmGraphData = GraphData DesPrgm String

fromMaybeTypeName :: Maybe TypeName -> Type
fromMaybeTypeName = maybe TopType fromName
  where
    fromName n | "$" `isPrefixOf` n = TypeVar $ TVVar n
    fromName n = singletonType (partialVal (PRelativeName n))

emptyMeta :: SourcePos -> SourcePos -> ParseMeta
emptyMeta p1 p2 = Meta TopType (Just (p1, p2, "")) emptyMetaDat

isAbsolutePath :: String -> Bool
isAbsolutePath name = "/" `isPrefixOf` name

getPath :: String -> Path
getPath name = if isAbsolutePath name then
  Absolute name
  else Relative name

rawVal :: String -> PExpr
rawVal name = RawValue m name
  where m = Meta (singletonType $ partialVal (PTypeName name)) Nothing emptyMetaDat

applyRawArgs :: (MetaDat m) => RawExpr m -> [(Maybe ArgName, RawExpr m)] -> RawExpr m
applyRawArgs base args = RawTupleApply (emptyMetaE "app" base) (emptyMetaE "base" base, base) (map mapArg args)
  where
    mapArg (Just argName, argVal) = TupleArgIO (emptyMetaE argName base) argName argVal
    mapArg (Nothing, argVal) = TupleArgO (emptyMetaE "noArg" base) argVal
