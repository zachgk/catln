--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Prgm
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines most of the types that make up the Catln Syntax.
--------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}

module Syntax.Ct.Prgm where

import qualified Data.HashMap.Strict as H
import           Data.Hashable
import           Data.Void           (Void)
import           GHC.Generics        (Generic)

import           Data.Aeson          hiding (Object)
import           Semantics.Prgm
import           Semantics.Types
import           Text.Megaparsec
import           Text.Printf
import           Utils

-- Expr before desugar
data RawExpr m
  = RawCExpr (Meta m) Constant
  | RawValue (Meta m) TypeName
  | RawHoleExpr (Meta m) Hole
  | RawTheExpr (RawExpr m) -- ^ Written :TypeName and read as The TypeName
  | RawAliasExpr (RawExpr m) (RawExpr m) -- ^ base aliasExpr
  | RawTupleApply (Meta m) (Meta m, RawExpr m) [ObjArr RawExpr m]
  | RawVarsApply (Meta m) (RawExpr m) [(RawExpr m, Meta m)]
  | RawContextApply (Meta m) (Meta m, RawExpr m) [(ArgName, Meta m)]
  | RawParen (RawExpr m)
  | RawMethod (RawExpr m) (RawExpr m) -- ^ base methodValue
  | RawList (Meta m) [RawExpr m]
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

data MultiTypeDef m = MultiTypeDef PartialType [GuardExpr RawExpr m]
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

type RawClassDef m = (RawExpr m, ClassName)

data Path = Relative String | Absolute String
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

data RawStatement e m
  = RawDeclStatement (ObjArr e m)
  | MultiTypeDefStatement (MultiTypeDef m) Path
  | TypeDefStatement (RawExpr m)
  | RawClassDefStatement (RawClassDef m) Path
  | RawClassDeclStatement PartialType Path
  | RawExprStatement (RawExpr m)
  | RawAnnot (CompAnnot (RawExpr m))
  | RawModule String Path
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

data RawStatementTree e m = RawStatementTree (RawStatement e m) [RawStatementTree e m]
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

type FileImport = String
type RawPrgm m = ([FileImport], [RawStatementTree RawExpr m]) -- TODO: Include [Export]

type ParseErrorRes = ParseErrorBundle String Void
data ReplRes m
  = ReplStatement (RawStatementTree RawExpr m)
  | ReplExpr (RawExpr m)
  | ReplErr ParseErrorRes
  deriving (Eq, Show)

instance ExprClass RawExpr where
  getExprMeta expr = case expr of
    RawCExpr m _          -> m
    RawValue m _          -> m
    RawHoleExpr m _       -> m
    RawTheExpr e          -> getExprMeta e
    RawAliasExpr b _      -> getExprMeta b
    RawTupleApply m _ _   -> m
    RawVarsApply m _ _    -> m
    RawContextApply m _ _ -> m
    RawParen e            -> getExprMeta e
    RawMethod e _         -> getExprMeta e
    RawList m _           -> m

  getExprArg _ = Nothing

  maybeExprPathM (RawValue m n)               = Just (n, m)
  maybeExprPathM (RawTupleApply _ (_, e) _)   = maybeExprPathM e
  maybeExprPathM (RawVarsApply _ e _)         = maybeExprPathM e
  maybeExprPathM (RawContextApply _ (_, e) _) = maybeExprPathM e
  maybeExprPathM (RawParen e)                 = maybeExprPathM e
  maybeExprPathM (RawMethod _ e)              = maybeExprPathM e
  maybeExprPathM _                            = Nothing

  exprAppliedArgs (RawValue _ _) = []
  exprAppliedArgs (RawTupleApply _ (_, be) args) = exprAppliedArgs be ++ map mapArgs args
    where
      mapArgs ObjArr{oaObj= Just (GuardExpr (RawValue _ argName) _), oaM, oaArr=(Just (GuardExpr argVal Nothing))} = TupleArgIO oaM argName argVal
      mapArgs ObjArr{oaObj=(Just (GuardExpr e Nothing)), oaArr=Nothing} = TupleArgO e -- Both TupleArgI and TupleArgO will show as TupleArgO because input/output has not yet been disambiguated
      mapArgs oa@ObjArr{oaObj=Nothing, oaArr=Just{}} = error $ printf "Unexpected exprAppliedArgs because single expressions should always be treated as inputs after parsing: %s" (show oa)
      mapArgs oa = error $ printf "exprAppliedArgs not defined for arg %s" (show oa)
  exprAppliedArgs (RawVarsApply _ e _) = exprAppliedArgs e
  exprAppliedArgs (RawContextApply _ (_, e) _) = exprAppliedArgs e
  exprAppliedArgs (RawParen e) = exprAppliedArgs e
  exprAppliedArgs (RawMethod _ e) = exprAppliedArgs e
  exprAppliedArgs _ = error "Unsupported RawExpr exprAppliedArgs"


  exprAppliedVars (RawValue _ _) = H.empty
  exprAppliedVars (RawTupleApply _ (_, be) _) = exprAppliedVars be
  -- exprAppliedVars (RawVarsApply _ e vars) = H.union (exprAppliedVars e) (H.fromList vars)
  exprAppliedVars RawVarsApply{} = error "Not implemented"
  exprAppliedVars (RawContextApply _ (_, e) _) = exprAppliedVars e
  exprAppliedVars (RawParen e) = exprAppliedVars e
  exprAppliedVars (RawMethod _ e) = exprAppliedVars e
  exprAppliedVars _ = error "Unsupported RawExpr exprAppliedVars"

  exprArgs :: (Show m) => RawExpr m -> H.HashMap ArgName [Meta m]
  exprArgs RawCExpr{} = H.empty
  exprArgs RawHoleExpr{} = H.empty
  exprArgs RawValue{} = H.empty
  exprArgs RawTheExpr{} = H.empty
  exprArgs (RawAliasExpr base alias) = H.unionWith (++) (exprArgs base) (exprArgs alias)
  exprArgs (RawTupleApply _ (_, be) args) = H.unionWith (++) (exprArgs be) (unionsWith (++) $ map exprArg args)
    where
      exprArg ObjArr{oaObj=(Just (GuardExpr (RawValue m argName) Nothing)), oaArr= Nothing} = H.singleton argName [m]
      exprArg ObjArr{oaArr=(Just (GuardExpr argVal Nothing))} = exprArgs argVal
      exprArg oa = error $ printf "exprArgs not defined for arg %s" (show oa)
  exprArgs (RawVarsApply _ e _) = exprArgs e
  exprArgs (RawContextApply _ (_, e) _) = exprArgs e
  exprArgs (RawParen e) = exprArgs e
  exprArgs (RawMethod be me) = H.unionWith (++) (exprArgs be) (exprArgs me)
  exprArgs e = error $ printf "Unsupported RawExpr exprArgs for %s" (show e)

