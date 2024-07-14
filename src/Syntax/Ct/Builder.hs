--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Builder
-- Copyright :  (c) Zach Kimberg 2024
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines utilities for building syntax elements
--------------------------------------------------------------------

module Syntax.Ct.Builder where
import           CtConstants
import           Data.Bifunctor          (first)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Text.Megaparsec         (SourcePos)

emptyMeta :: SourcePos -> SourcePos -> ParseMeta
emptyMeta p1 p2 = Meta PTopType (Just (p1, p2, "")) emptyMetaDat

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
    args' = map ((False,) . mapArg) args
    mapArg :: (MetaDat m, Show m) => (Maybe (RawExpr m), RawExpr m) -> RawObjArr RawExpr m
    mapArg (Just argName, argVal) = RawObjArr (Just argName) ArgObj Nothing [] (Just (Just argVal, Nothing, emptyMetaE (show argName) argVal)) Nothing
    mapArg (Nothing, argVal) = RawObjArr Nothing ArgObj Nothing [] (Just (Just argVal, Nothing, emptyMetaE "noArg" argVal)) Nothing

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
    args' = map ((False,) . mapArg) args
    mapArg :: (PExpr, IArg RawExpr) -> RawObjArr RawExpr ()
    mapArg (argName, IArgE argVal) = RawObjArr (Just argName) ArgObj Nothing [] (Just (Just argVal, Nothing, emptyMetaE (show argName) argVal)) Nothing
    mapArg (argName, IArgM argM) = RawObjArr (Just argName) ArgObj Nothing [] (Just (Nothing, Nothing, argM)) Nothing
    mapArg (argName, IArgNothing) = RawObjArr (Just argName) ArgObj Nothing [] (Just (Nothing, Nothing, emptyMetaE ("m-" ++ show argName) base)) Nothing

applyRawExprVars :: (MetaDat m) => RawExpr m -> [(TypeVarName, Meta m)] -> RawExpr m
applyRawExprVars base vars = applyRawExprEVars base vars'
  where
    vars' = map (first (\n -> RawValue (emptyMetaE ("var" ++ show n) base) (pkName n))) vars

applyRawExprEVars :: (MetaDat m) => RawExpr m -> [(RawExpr m, Meta m)] -> RawExpr m
applyRawExprEVars base []   = base
applyRawExprEVars base vars = case base of
  (RawVarsApply m base' baseArgs) -> RawVarsApply m base' (baseArgs ++ vars')
  _ -> RawVarsApply (emptyMetaE "app" base) base vars'
  where
    vars' = map aux vars
    aux (n, m) = RawObjArr (Just n) ArgObj Nothing [] (Just (Nothing, Nothing, m)) Nothing

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
        mapArg (Just argName, argVal) = ObjArr (Just (Value (emptyMetaE (show argName) b) (pkName argName))) ArgObj Nothing [] (Just (Just argVal, emptyMetaE "argRes" argVal))
        mapArg (Nothing, argVal) = ObjArr Nothing ArgObj Nothing [] (Just (Just argVal, emptyMetaE "argRes" argVal))

applyExprIArgs :: Expr () -> [(ArgName, IArg Expr)] -> Expr ()
applyExprIArgs = foldl addArg
  where
    addArg b a = TupleApply (emptyMetaE "app" b) (emptyMetaE "base" b, b) (mapArg a)
      where
        mapArg :: (ArgName, IArg Expr) -> ObjArr Expr ()
        mapArg (argName, IArgE argVal) = ObjArr (Just (Value (emptyMetaE (show argName) b) (pkName argName))) ArgObj Nothing [] (Just(Just argVal, emptyMetaE "argRes" argVal))
        mapArg (argName, IArgM argM) = ObjArr (Just (Value argM (pkName argName))) ArgObj Nothing [] (Just (Nothing, emptyMetaE ("argRes" ++ show argName) b))
        mapArg (argName, IArgNothing) = ObjArr (Just (Value (emptyMetaE (show argName) b) (pkName argName))) ArgObj Nothing [] (Just (Nothing, emptyMetaE ("argRes" ++ show argName) b))

applyExprVars :: (MetaDat m) => Expr m -> [(TypeVarName, Meta m)] -> Expr m
applyExprVars = foldl addVar
  where
    addVar b (varName, varVal) = VarApply (emptyMetaE "varBase" b) b varName varVal

rawAnon :: PExpr
rawAnon = rawVal anonStr

rawInObjArr :: Bool -> PExpr -> PObjArr
rawInObjArr withArr e = RawObjArr (Just e) FunctionObj Nothing [] (if withArr then Just (Nothing, Nothing, emptyMetaN) else Nothing) Nothing

rawOutObjArr :: PExpr -> PObjArr
rawOutObjArr e = RawObjArr Nothing FunctionObj Nothing [] (Just (Just e, Nothing, emptyMetaN)) Nothing

rawModule :: String -> PStatement
rawModule n = RawDeclStatement $ rawOutObjArr (rawVal modStr `applyRawExprVars` [(partialKey n, emptyMetaN)])

classInstSt :: (PExpr, [PExpr]) -> PStatement
classInstSt (cls, extends) = RawDeclStatement $ rawOutObjArr (rawVal everyStr `applyRawEIArgs` [(cls, IArgNothing), (rawVal isaStr, IArgE $ RawList emptyMetaN extends)])

classDeclSt :: PExpr -> [PExpr] -> PStatement
classDeclSt cls extends = RawDeclStatement $ rawOutObjArr (rawVal classStr `applyRawEIArgs` [(cls, IArgNothing)] `applyRawArgs` [(Just $ partialKey isaStr, RawList emptyMetaN extends) | not (null extends)])

classWithObjs :: PExpr -> [PExpr] -> [PExpr] -> PStatement
classWithObjs cls objs extends = RawDeclStatement $ rawOutObjArr (rawVal classStr `applyRawEIArgs` [(cls, IArgNothing), (RawList emptyMetaN objs, IArgNothing)] `applyRawArgs` [(Just $ partialKey isaStr, RawList emptyMetaN extends) | not (null extends)])