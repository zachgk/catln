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

import           Data.List           (isPrefixOf)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Prgm
import           Text.Megaparsec     (SourcePos)
import           Text.Printf
import           Utils

type ParseMetaDat = ()
type ParseMeta = Meta ParseMetaDat
type PTupleArg = TupleArg RawExpr ParseMetaDat
type PExpr = RawExpr ParseMetaDat
type PPattern = Pattern RawExpr ParseMetaDat
type PCompAnnot = CompAnnot PExpr
type PGuard = Guard PExpr
type PDeclLHS = DeclLHS RawExpr ParseMetaDat
type PDecl = RawDecl RawExpr ParseMetaDat
type PObjectMap = ObjectMap RawExpr ParseMetaDat
type PMultiTypeDef = MultiTypeDef ParseMetaDat
type PTypeDef = TypeDef ParseMetaDat
type PStatement = RawStatement RawExpr ParseMetaDat
type PStatementTree = RawStatementTree RawExpr ParseMetaDat
type PArgMetaMap = H.HashMap ArgName ParseMeta
type PObjArg = ObjArg ParseMetaDat
type PObject = ExprObject RawExpr ParseMetaDat
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
type DesObjectMap = ExprObjectMap Expr ParseMetaDat
type DesObject = ExprObject Expr ParseMetaDat
type DesArrow = Arrow Expr ParseMetaDat
type DesObjectMapItem = ExprObjectMapItem Expr ParseMetaDat
type DesPrgm = ExprPrgm Expr ParseMetaDat
type DesPrgmGraphData = GraphData DesPrgm String

type FinalDesPrgm = Prgm Expr ParseMetaDat
type FinalDesPrgmGraphData = GraphData (Prgm Expr ParseMetaDat) String

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

mWithType :: Type -> ParseMeta -> ParseMeta
mWithType t (Meta _ p d) = Meta t p d

rawVal :: String -> PExpr
rawVal name = RawValue m name
  where m = Meta (singletonType $ partialVal (PTypeName name)) Nothing emptyMetaDat

applyRawArgs :: (MetaDat m) => RawExpr m -> [(Maybe ArgName, RawExpr m)] -> RawExpr m
applyRawArgs base args = RawTupleApply (emptyMetaE "app" base) (emptyMetaE "base" base, base) (map mapArg args)
  where
    mapArg (Just argName, argVal) = TupleArgIO (emptyMetaE argName base) argName argVal
    mapArg (Nothing, argVal) = TupleArgO (emptyMetaE "noArg" base) argVal


data IArg e = IArgNothing | IArgE (e ()) | IArgM (Meta ())
applyRawIArgs :: PExpr -> [(ArgName, IArg RawExpr)] -> PExpr
applyRawIArgs base args = RawTupleApply (emptyMetaE "app" base) (emptyMetaE "base" base, base) (map mapArg args)
  where
    mapArg :: (ArgName, IArg RawExpr) -> TupleArg RawExpr ()
    mapArg (argName, IArgE argVal) = TupleArgIO (emptyMetaE argName base) argName argVal
    mapArg (argName, IArgM argM) = TupleArgI argM argName
    mapArg (argName, IArgNothing) = TupleArgI (emptyMetaE "noArg" base) argName

exprVal :: (MetaDat m) => String -> Expr m
exprVal = Value m
  where
    m = emptyMetaN
    -- m = Meta (singletonType $ partialVal (PTypeName name)) Nothing emptyMetaDat

applyExprOArgs :: (MetaDat m) => Expr m -> [(Maybe ArgName, Expr m)] -> Expr m
applyExprOArgs = foldl addArg
  where
    addArg b a = TupleApply (emptyMetaE "app" b) (emptyMetaE "base" b, b) (mapArg a)
      where
        mapArg (Just argName, argVal) = TupleArgIO (emptyMetaE argName b) argName argVal
        mapArg (Nothing, argVal) = TupleArgO (emptyMetaE "noArg" b) argVal

applyExprIArgs :: Expr () -> [(ArgName, IArg Expr)] -> Expr ()
applyExprIArgs = foldl addArg
  where
    addArg b a = TupleApply (emptyMetaE "app" b) (emptyMetaE "base" b, b) (mapArg a)
      where
        mapArg :: (ArgName, IArg Expr) -> TupleArg Expr ()
        mapArg (argName, IArgE argVal) = TupleArgIO (emptyMetaE argName b) argName argVal
        mapArg (argName, IArgM argM) = TupleArgI argM argName
        mapArg (argName, IArgNothing) = TupleArgI (emptyMetaE "noArg" b) argName

applyExprVars :: (MetaDat m) => Expr m -> [(TypeVarName, Meta m)] -> Expr m
applyExprVars = foldl addVar
  where
    addVar b (varName, varVal) = VarApply (emptyMetaE "varBase" b) b varName varVal

mapExprPath :: (Show m) => ((Meta m, TypeName) -> Expr m) -> Expr m -> Expr m
mapExprPath f (Value m n) = f (m, n)
mapExprPath f (TupleApply m (bm, be) a) = TupleApply m (bm, mapExprPath f be) a
mapExprPath f (VarApply m be an av) = VarApply m (mapExprPath f be) an av
mapExprPath _ e = error $ printf "Unexpected expr to mapExprPath: %s" (show e)

mapExprObjPath :: (Show m) => ((Meta m, TypeName) -> Expr m) -> ExprObject Expr m -> ExprObject Expr m
mapExprObjPath f (ExprObject b d e) = ExprObject b d (mapExprPath f e)

mapExprAppliedArg :: (DesExpr -> DesExpr) -> ArgName -> DesExpr -> DesExpr
mapExprAppliedArg _ _ e@CExpr{} = e
mapExprAppliedArg _ _ e@Value{} = e
mapExprAppliedArg _ _ e@Arg{} = e
mapExprAppliedArg _ _ e@HoleExpr{} = e
mapExprAppliedArg f argName (TupleApply m (bm, be) (TupleArgIO am an av)) | argName == an = TupleApply m (bm, be) (TupleArgIO am an (f av))
mapExprAppliedArg f argName (TupleApply m (bm, be) a) = TupleApply m (bm, mapExprAppliedArg f argName be) a
mapExprAppliedArg f argName (VarApply m be an av) = VarApply m (mapExprAppliedArg f argName be) an av


mkExprObj :: (MetaDat m) => ObjectBasis -> [(TypeVarName, Meta m)] -> [(Maybe ArgName, Expr m)] -> Maybe DocComment -> String -> ExprObject Expr m
mkExprObj basis vars args comment path = ExprObject basis comment (exprVal path `applyExprVars` vars `applyExprOArgs` args)
