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
type PMultiTypeDef = MultiTypeDef ParseMetaDat
type PObjArr = RawObjArr RawExpr ParseMetaDat
type PStatement = RawStatement RawExpr ParseMetaDat
type PStatementTree = RawStatementTree RawExpr ParseMetaDat
type PArgMetaMap = H.HashMap ArgName [ParseMeta]
type PPrgm = RawPrgm ParseMetaDat
type PPrgmGraphData = GraphData PPrgm RawFileImport
type PReplRes = ReplRes ParseMetaDat

type PDeclTree = (PObjArr, [PStatementTree])

type PSExpr = Expr ParseMetaDat
type PSCompAnnot = CompAnnot PSExpr
type PSObjArr = ObjArr Expr ParseMetaDat

newtype PSemiDecl = PSemiDecl PSObjArr
  deriving (Show)




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

fromMaybeTypeName :: Maybe TypeName -> Type
fromMaybeTypeName = maybe topType fromName
  where
    fromName n = case parseTVVar n of
      Just t  -> t
      Nothing -> relTypeVal n

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

applyRawArgs :: (MetaDat m, Show m) => RawExpr m -> [(Maybe ArgName, RawExpr m)] -> RawExpr m
applyRawArgs base args = applyRawEArgs base (map mapArg args)
  where
    mapArg :: (MetaDat m) => (Maybe ArgName, RawExpr m) -> (Maybe (RawExpr m), RawExpr m)
    mapArg (Just argName, argVal) = (Just $ RawValue (emptyMetaE ("in-" ++ show argName) argVal) (pkName argName), argVal)
    mapArg (Nothing, argVal) = (Nothing, argVal)

applyRawEArgs :: (MetaDat m, Show m) => RawExpr m -> [(Maybe (RawExpr m), RawExpr m)] -> RawExpr m
applyRawEArgs base [] = base
applyRawEArgs base args = case base of
  (RawTupleApply m base' baseArgs) -> RawTupleApply m base' (baseArgs ++ args')
  _ -> RawTupleApply (emptyMetaE "app" base) (emptyMetaE "base" base, base) args'
  where
    args' = map mapArg args
    mapArg :: (MetaDat m, Show m) => (Maybe (RawExpr m), RawExpr m) -> RawObjArr RawExpr m
    mapArg (Just argName, argVal) = RawObjArr (Just argName) ArgObj Nothing [] (Just (Just argVal, emptyMetaE (show argName) argVal)) Nothing
    mapArg (Nothing, argVal) = RawObjArr Nothing ArgObj Nothing [] (Just (Just argVal, emptyMetaE "noArg" argVal)) Nothing

data IArg e = IArgNothing | IArgE (e ()) | IArgM (Meta ())
applyRawIArgs :: PExpr -> [(ArgName, IArg RawExpr)] -> PExpr
applyRawIArgs base args = applyRawEIArgs base (map mapArg args)
  where
    mapArg :: (ArgName, IArg RawExpr) -> (PExpr, IArg RawExpr)
    mapArg (argName, IArgE argVal) = (RawValue (emptyMetaE ("in-" ++ show argName) argVal) (pkName argName), IArgE argVal)
    mapArg (argName, IArgM argM) = (RawValue (emptyMetaM ("in-" ++ show argName) argM) (pkName argName), IArgM argM)
    mapArg (argName, IArgNothing) = (RawValue (emptyMetaE ("in-" ++ show argName) base) (pkName argName), IArgNothing)

applyRawEIArgs :: PExpr -> [(PExpr, IArg RawExpr)] -> PExpr
applyRawEIArgs base [] = base
applyRawEIArgs base args = case base of
  (RawTupleApply m base' baseArgs) -> RawTupleApply m base' (baseArgs ++ args')
  _ -> RawTupleApply (emptyMetaE "app" base) (emptyMetaE "base" base, base) args'
  where
    args' = map mapArg args
    mapArg :: (PExpr, IArg RawExpr) -> RawObjArr RawExpr ()
    mapArg (argName, IArgE argVal) = RawObjArr (Just argName) ArgObj Nothing [] (Just (Just argVal, emptyMetaE (show argName) argVal)) Nothing
    mapArg (argName, IArgM argM) = RawObjArr (Just argName) ArgObj Nothing [] (Just (Nothing, argM)) Nothing
    mapArg (argName, IArgNothing) = RawObjArr (Just argName) ArgObj Nothing [] (Just (Nothing, emptyMetaE ("m-" ++ show argName) base)) Nothing

applyRawExprVars :: (MetaDat m) => RawExpr m -> [(TypeVarName, Meta m)] -> RawExpr m
applyRawExprVars base vars = applyRawExprEVars base vars'
  where
    vars' = map (first (\n -> RawValue (emptyMetaE ("var" ++ show n) base) (pkName n))) vars

applyRawExprEVars :: (MetaDat m) => RawExpr m -> [(RawExpr m, Meta m)] -> RawExpr m
applyRawExprEVars base []   = base
applyRawExprEVars base vars = case base of
  (RawVarsApply m base' baseArgs) -> RawVarsApply m base' (baseArgs ++ vars)
  _ -> RawVarsApply (emptyMetaE "app" base) base vars

rawStr :: (MetaDat m) => String -> RawExpr m
rawStr = RawCExpr emptyMetaN . CStr

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
        mapArg (Just argName, argVal) = ObjArr (Just (Value (emptyMetaE (show argName) b) (pkName argName))) ArgObj Nothing [] (Just argVal, emptyMetaE "argRes" argVal)
        mapArg (Nothing, argVal) = ObjArr Nothing ArgObj Nothing [] (Just argVal, emptyMetaE "argRes" argVal)

applyExprIArgs :: Expr () -> [(ArgName, IArg Expr)] -> Expr ()
applyExprIArgs = foldl addArg
  where
    addArg b a = TupleApply (emptyMetaE "app" b) (emptyMetaE "base" b, b) (mapArg a)
      where
        mapArg :: (ArgName, IArg Expr) -> ObjArr Expr ()
        mapArg (argName, IArgE argVal) = ObjArr (Just (Value (emptyMetaE (show argName) b) (pkName argName))) ArgObj Nothing [] (Just argVal, emptyMetaE "argRes" argVal)
        mapArg (argName, IArgM argM) = ObjArr (Just (Value argM (pkName argName))) ArgObj Nothing [] (Nothing, emptyMetaE ("argRes" ++ show argName) b)
        mapArg (argName, IArgNothing) = ObjArr (Just (Value (emptyMetaE (show argName) b) (pkName argName))) ArgObj Nothing [] (Nothing, emptyMetaE ("argRes" ++ show argName) b)

applyExprVars :: (MetaDat m) => Expr m -> [(TypeVarName, Meta m)] -> Expr m
applyExprVars = foldl addVar
  where
    addVar b (varName, varVal) = VarApply (emptyMetaE "varBase" b) b varName varVal

mapExprPath :: (Show m) => ((Meta m, TypeName) -> Expr m) -> Expr m -> Expr m
mapExprPath f (Value m n) = f (m, n)
mapExprPath f (EWhere b c) = EWhere (mapExprPath f b) c
mapExprPath f (TupleApply m (bm, be) a) = TupleApply m (bm, mapExprPath f be) a
mapExprPath f (VarApply m be an av) = VarApply m (mapExprPath f be) an av
mapExprPath _ e = error $ printf "Unexpected expr to mapExprPath: %s" (show e)

mapOAObjPath :: (Show m, MetaDat m) => ((Meta m, TypeName) -> Expr m) -> ObjArr Expr m -> ObjArr Expr m
mapOAObjPath f = mapOAObjExpr (mapExprPath f)

mapExprAppliedArg :: (DesExpr -> DesExpr) -> ArgName -> DesExpr -> DesExpr
mapExprAppliedArg _ _ e@CExpr{} = e
mapExprAppliedArg _ _ e@Value{} = e
mapExprAppliedArg _ _ e@HoleExpr{} = e
mapExprAppliedArg f argName (AliasExpr base alias) = AliasExpr (mapExprAppliedArg f argName base) alias
mapExprAppliedArg f argName (EWhere base cond) = EWhere (mapExprAppliedArg f argName base) cond
mapExprAppliedArg f argName (TupleApply m (bm, be) oa@ObjArr{oaObj=Just an, oaArr=(Just aExpr, oaM)}) | argName == inExprSingleton an = TupleApply m (bm, be) oa{oaArr=(Just (f aExpr), oaM)}
mapExprAppliedArg f argName (TupleApply m (bm, be) a) = TupleApply m (bm, mapExprAppliedArg f argName be) a
mapExprAppliedArg f argName (VarApply m be an av) = VarApply m (mapExprAppliedArg f argName be) an av
