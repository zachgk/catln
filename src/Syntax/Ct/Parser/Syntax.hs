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

import           Data.Bifunctor
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
type PExpr = RawExpr ParseMetaDat
type PObjExpr = PExpr
type PCompAnnot = CompAnnot PExpr
type PGuardExpr = GuardExpr RawExpr ParseMetaDat
type PObjectMap = ObjectMap RawExpr ParseMetaDat
type PMultiTypeDef = MultiTypeDef ParseMetaDat
type PObjArr = RawObjArr RawExpr ParseMetaDat
type PStatement = RawStatement RawExpr ParseMetaDat
type PStatementTree = RawStatementTree RawExpr ParseMetaDat
type PArgMetaMap = H.HashMap ArgName [ParseMeta]
type PObjArg = ObjArg RawExpr ParseMetaDat
type PArrow = Arrow RawExpr ParseMetaDat
type PPrgm = RawPrgm ParseMetaDat
type PPrgmGraphData = GraphData PPrgm String
type PReplRes = ReplRes ParseMetaDat

type PDeclTree = (PObjArr, [PStatementTree])

type PSExpr = Expr ParseMetaDat
type PSCompAnnot = CompAnnot PSExpr
type PSGuardExpr = GuardExpr Expr ParseMetaDat
type PSObjArr = ObjArr Expr ParseMetaDat

newtype PSemiDecl = PSemiDecl PSObjArr
  deriving (Show)




type DesExpr = Expr ParseMetaDat
type DesGuardExpr = GuardExpr Expr ParseMetaDat
type DesCompAnnot = CompAnnot DesExpr
type DesObjArr = ObjArr Expr ParseMetaDat
type DesObjectMapItem = ExprObjectMapItem Expr ParseMetaDat
type DesObjectMap = ExprObjectMap Expr ParseMetaDat
type DesPrgm = ExprPrgm Expr ParseMetaDat
type DesPrgmGraphData = GraphData DesPrgm String

parseTVVar :: String -> Maybe Type
parseTVVar ('$':'_':n) = Just $ TypeVar (TVVar n) TVExt
parseTVVar ('$':n)     = Just $ TypeVar (TVVar n) TVInt
parseTVVar _           = Nothing

fromMaybeTypeName :: Maybe TypeName -> Type
fromMaybeTypeName = maybe topType fromName
  where
    fromName n = case parseTVVar n of
      Just t  -> t
      Nothing -> typeVal (PRelativeName n)

emptyMeta :: SourcePos -> SourcePos -> ParseMeta
emptyMeta p1 p2 = Meta topType (Just (p1, p2, "")) emptyMetaDat

isAbsolutePath :: String -> Bool
isAbsolutePath name = "/" `isPrefixOf` name

getPath :: String -> Path
getPath name = if isAbsolutePath name then
  Absolute name
  else Relative name

rawExprWithType :: Type -> RawExpr ParseMetaDat -> RawExpr ParseMetaDat
rawExprWithType t (RawCExpr m c) = RawCExpr (mWithType t m) c
rawExprWithType t (RawValue m n) = RawValue (mWithType t m) n
rawExprWithType t (RawHoleExpr m h) = RawHoleExpr (mWithType t m) h
rawExprWithType t (RawList m l) = RawList (mWithType t m) l
rawExprWithType _ e = error $ printf "rawExprWithType for unexpected type: %s" (show e)

rawVal :: (MetaDat m) => String -> RawExpr m
rawVal = RawValue m
  where
    m = emptyMetaN
    -- m = Meta (typeVal (PTypeName name)) Nothing emptyMetaDat

applyRawArgs :: (MetaDat m) => RawExpr m -> [(Maybe ArgName, RawExpr m)] -> RawExpr m
applyRawArgs base [] = base
applyRawArgs base args = RawTupleApply (emptyMetaE "app" base) (emptyMetaE "base" base, base) (map mapArg args)
  where
    mapArg :: (Maybe ArgName, RawExpr m) -> RawObjArr RawExpr m
    mapArg (Just argName, argVal) = RawObjArr (Just (GuardExpr (RawValue (emptyMetaE ("in-" ++ argName) argVal) argName) Nothing)) ArgObj Nothing [] (Just (Just (GuardExpr argVal Nothing), emptyMetaE argName argVal)) Nothing
    mapArg (Nothing, argVal) = RawObjArr Nothing ArgObj Nothing [] (Just (Just (GuardExpr argVal Nothing), emptyMetaE "noArg" argVal)) Nothing


data IArg e = IArgNothing | IArgE (e ()) | IArgM (Meta ())
applyRawIArgs :: PExpr -> [(ArgName, IArg RawExpr)] -> PExpr
applyRawIArgs base [] = base
applyRawIArgs base args = RawTupleApply (emptyMetaE "app" base) (emptyMetaE "base" base, base) (map mapArg args)
  where
    mapArg :: (ArgName, IArg RawExpr) -> RawObjArr RawExpr ()
    mapArg (argName, IArgE argVal) = RawObjArr (Just (GuardExpr (RawValue (emptyMetaE ("in-" ++ argName) argVal) argName) Nothing)) ArgObj Nothing [] (Just (Just (GuardExpr argVal Nothing), emptyMetaE argName argVal)) Nothing
    mapArg (argName, IArgM argM) = RawObjArr (Just (GuardExpr (RawValue (emptyMetaM ("in-" ++ argName) argM) argName) Nothing)) ArgObj Nothing [] (Just (Nothing, argM)) Nothing
    mapArg (argName, IArgNothing) = RawObjArr (Just (GuardExpr (RawValue (emptyMetaE ("in-" ++ argName) base) argName) Nothing)) ArgObj Nothing [] (Just (Nothing, emptyMetaE ("m-" ++ argName) base)) Nothing

applyRawExprVars :: (MetaDat m) => RawExpr m -> [(TypeVarName, Meta m)] -> RawExpr m
applyRawExprVars base []   = base
applyRawExprVars base vars = RawVarsApply (emptyMetaE "app" base) base (map (first (\n -> RawValue (emptyMetaE ("var" ++ n) base) n)) vars)

exprVal :: (MetaDat m) => String -> Expr m
exprVal = Value m
  where
    m = emptyMetaN
    -- m = Meta (typeVal (PTypeName name)) Nothing emptyMetaDat

applyExprOArgs :: (MetaDat m, Show m) => Expr m -> [(Maybe ArgName, Expr m)] -> Expr m
applyExprOArgs = foldl addArg
  where
    addArg b a = TupleApply (emptyMetaE "app" b) (emptyMetaE "base" b, b) (mapArg a)
      where
        mapArg (Just argName, argVal) = ObjArr (Just (GuardExpr (Arg (emptyMetaE argName b) argName) Nothing)) ArgObj Nothing [] (Just (Just (GuardExpr argVal Nothing), emptyMetaE "argRes" argVal))
        mapArg (Nothing, argVal) = ObjArr Nothing ArgObj Nothing [] (Just (Just (GuardExpr argVal Nothing), emptyMetaE "argRes" argVal))

applyExprIArgs :: Expr () -> [(ArgName, IArg Expr)] -> Expr ()
applyExprIArgs = foldl addArg
  where
    addArg b a = TupleApply (emptyMetaE "app" b) (emptyMetaE "base" b, b) (mapArg a)
      where
        mapArg :: (ArgName, IArg Expr) -> ObjArr Expr ()
        mapArg (argName, IArgE argVal) = ObjArr (Just (GuardExpr (Arg (emptyMetaE argName b) argName) Nothing)) ArgObj Nothing [] (Just (Just (GuardExpr argVal Nothing), emptyMetaE "argRes" argVal))
        mapArg (argName, IArgM argM) = ObjArr (Just (GuardExpr (Arg argM argName) Nothing)) ArgObj Nothing [] (Just (Nothing, emptyMetaE ("argRes" ++ argName) b))
        mapArg (argName, IArgNothing) = ObjArr (Just (GuardExpr (Arg (emptyMetaE argName b) argName) Nothing)) ArgObj Nothing [] (Just (Nothing, emptyMetaE ("argRes" ++ argName) b))

applyExprVars :: (MetaDat m) => Expr m -> [(TypeVarName, Meta m)] -> Expr m
applyExprVars = foldl addVar
  where
    addVar b (varName, varVal) = VarApply (emptyMetaE "varBase" b) b varName varVal

mapExprPath :: (Show m) => ((Meta m, TypeName) -> Expr m) -> Expr m -> Expr m
mapExprPath f (Value m n) = f (m, n)
mapExprPath f (TupleApply m (bm, be) a) = TupleApply m (bm, mapExprPath f be) a
mapExprPath f (VarApply m be an av) = VarApply m (mapExprPath f be) an av
mapExprPath _ e = error $ printf "Unexpected expr to mapExprPath: %s" (show e)

mapOAObjPath :: (Show m, MetaDat m) => ((Meta m, TypeName) -> Expr m) -> ObjArr Expr m -> ObjArr Expr m
mapOAObjPath f = mapOAObjExpr (mapExprPath f)

mapExprAppliedArg :: (DesExpr -> DesExpr) -> ArgName -> DesExpr -> DesExpr
mapExprAppliedArg _ _ e@CExpr{} = e
mapExprAppliedArg _ _ e@Value{} = e
mapExprAppliedArg _ _ e@Arg{} = e
mapExprAppliedArg _ _ e@HoleExpr{} = e
mapExprAppliedArg f argName (AliasExpr base alias) = AliasExpr (mapExprAppliedArg f argName base) alias
mapExprAppliedArg f argName (TupleApply m (bm, be) oa@ObjArr{oaObj=Just (GuardExpr (Value _ an) Nothing), oaArr=Just (Just (GuardExpr aExpr aGuard), oaM)}) | argName == an = TupleApply m (bm, be) oa{oaArr=Just (Just (GuardExpr (f aExpr) aGuard), oaM)}
mapExprAppliedArg f argName (TupleApply m (bm, be) a) = TupleApply m (bm, mapExprAppliedArg f argName be) a
mapExprAppliedArg f argName (VarApply m be an av) = VarApply m (mapExprAppliedArg f argName be) an av
