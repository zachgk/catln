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

data Pattern e m = Pattern (ExprObject e m) (Guard (e m))
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, ToJSONKey)

-- Expr before desugar
data RawExpr m
  = RawCExpr (Meta m) Constant
  | RawValue (Meta m) TypeName
  | RawHoleExpr (Meta m) Hole
  | RawTheExpr (RawExpr m) -- ^ Written :TypeName and read as The TypeName
  | RawAliasExpr (RawExpr m) (RawExpr m) -- ^ base aliasExpr
  | RawTupleApply (Meta m) (Meta m, RawExpr m) [TupleArg RawExpr m]
  | RawVarsApply (Meta m) (RawExpr m) [(TypeVarName, Meta m)]
  | RawContextApply (Meta m) (Meta m, RawExpr m) [(ArgName, Meta m)]
  | RawParen (RawExpr m)
  | RawMethod (RawExpr m) (RawExpr m) -- ^ base methodValue
  | RawList (Meta m) [RawExpr m]
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

data DeclLHS e m = DeclLHS (Meta m) (Pattern e m)
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

data RawDecl e m = RawDecl (DeclLHS e m) (Maybe (e m))
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

newtype TypeDef m = TypeDef (RawExpr m)
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

data MultiTypeDef m = MultiTypeDef ClassName (H.HashMap TypeVarName Type) [RawExpr m]
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

type RawClassDef m = (RawExpr m, ClassName)

type RawClassDecl = (ClassName, H.HashMap TypeVarName Type)

data Path = Relative String | Absolute String
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

data RawStatement e m
  = RawDeclStatement (RawDecl e m)
  | MultiTypeDefStatement (MultiTypeDef m) Path
  | TypeDefStatement (TypeDef m)
  | RawClassDefStatement (RawClassDef m) Path
  | RawClassDeclStatement RawClassDecl Path
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

  maybeExprPath (RawValue _ n)               = Just n
  maybeExprPath (RawTupleApply _ (_, e) _)   = maybeExprPath e
  maybeExprPath (RawVarsApply _ e _)         = maybeExprPath e
  maybeExprPath (RawContextApply _ (_, e) _) = maybeExprPath e
  maybeExprPath (RawParen e)                 = maybeExprPath e
  maybeExprPath (RawMethod _ e)              = maybeExprPath e
  maybeExprPath _                            = Nothing

  exprAppliedArgs (RawValue _ _) = []
  exprAppliedArgs (RawTupleApply _ (_, be) args) = exprAppliedArgs be ++ args
  exprAppliedArgs (RawVarsApply _ e _) = exprAppliedArgs e
  exprAppliedArgs (RawContextApply _ (_, e) _) = exprAppliedArgs e
  exprAppliedArgs (RawParen e) = exprAppliedArgs e
  exprAppliedArgs (RawMethod _ e) = exprAppliedArgs e
  exprAppliedArgs _ = error "Unsupported RawExpr exprAppliedArgs"


  exprAppliedVars (RawValue _ _) = H.empty
  exprAppliedVars (RawTupleApply _ (_, be) _) = exprAppliedVars be
  exprAppliedVars (RawVarsApply _ e vars) = H.union (exprAppliedVars e) (H.fromList vars)
  exprAppliedVars (RawContextApply _ (_, e) _) = exprAppliedVars e
  exprAppliedVars (RawParen e) = exprAppliedVars e
  exprAppliedVars (RawMethod _ e) = exprAppliedVars e
  exprAppliedVars _ = error "Unsupported RawExpr exprAppliedVars"

  exprArgs RawCExpr{} = H.empty
  exprArgs RawHoleExpr{} = H.empty
  exprArgs RawValue{} = H.empty
  exprArgs RawTheExpr{} = H.empty
  exprArgs (RawAliasExpr base alias) = H.unionWith (++) (exprArgs base) (exprArgs alias)
  exprArgs (RawTupleApply _ (_, be) args) = H.unionWith (++) (exprArgs be) (unionsWith (++) $ map exprArg args)
    where
      exprArg (TupleArgIO _ _ e) = exprArgs e
      exprArg (TupleArgO _ e)    = exprArgs e
      exprArg (TupleArgI m n)    = H.singleton n [m]
  exprArgs (RawVarsApply _ e _) = exprArgs e
  exprArgs (RawContextApply _ (_, e) _) = exprArgs e
  exprArgs (RawParen e) = exprArgs e
  exprArgs (RawMethod be me) = H.unionWith (++) (exprArgs be) (exprArgs me)
  exprArgs e = error $ printf "Unsupported RawExpr exprArgs for %s" (show e)

