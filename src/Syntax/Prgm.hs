--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Prgm
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

module Syntax.Prgm where

import           Data.Hashable
import qualified Data.HashMap.Strict as H
import           GHC.Generics          (Generic)

import Syntax.Types

newtype Import = Import String
  deriving (Eq, Ord, Show)

newtype Export = Export String
  deriving (Eq, Ord, Show)

data Constant
  = CInt Integer
  | CFloat Double
  | CStr String
  deriving (Eq, Ord, Show, Generic)
instance Hashable Constant

type RawExpr = Expr

data Expr m
  = CExpr m Constant
  | Value m Name
  | TupleApply m (m, Expr m) (H.HashMap Name (Expr m))
  deriving (Eq, Ord, Show, Generic)
instance Hashable m => Hashable (Expr m)

-- Compiler Annotation
data CompAnnot m = CompAnnot Name (H.HashMap Name (Expr m))
  deriving (Eq, Ord, Show, Generic)
instance Hashable m => Hashable (CompAnnot m)

data Guard m
  = IfGuard (Expr m)
  | ElseGuard
  | NoGuard
  deriving (Eq, Ord, Show, Generic)
instance Hashable m => Hashable (Guard m)

data RawDeclSubStatement m
  = RawDeclSubStatementDecl (RawDecl m)
  | RawDeclSubStatementAnnot (CompAnnot m)
  deriving (Eq, Ord, Show)

data DeclLHS m = DeclLHS m m Name (H.HashMap Name m) (Guard m) -- objM, arrM
  deriving (Eq, Ord, Show)

data RawDecl m = RawDecl (DeclLHS m) [RawDeclSubStatement m] (Maybe (Expr m))
  deriving (Eq, Ord, Show)

data RawTypeDef m = RawTypeDef Name RawLeafSet
  deriving (Eq, Ord, Show)

type RawClassDef = (TypeName, ClassName)

data RawStatement m
  = RawDeclStatement (RawDecl m)
  | RawTypeDefStatement (RawTypeDef m)
  | RawClassDefStatement RawClassDef
  deriving (Eq, Ord, Show)

type FileImport = String
type RawPrgm m = ([FileImport], [RawStatement m]) -- TODO: Include [Export]

data Object m = Object m Name (H.HashMap Name m)
  deriving (Eq, Ord, Show, Generic)
instance Hashable m => Hashable (Object m)

data Arrow m = Arrow m [CompAnnot m] (Guard m) (Maybe (Expr m)) -- m is result metadata
  deriving (Eq, Ord, Show, Generic)
instance Hashable m => Hashable (Arrow m)

type ObjectMap m = (H.HashMap (Object m) [Arrow m])
type Prgm m = (ObjectMap m, ClassMap) -- TODO: Include [Export]

getExprMeta :: Expr m -> m
getExprMeta expr = case expr of
  CExpr m _   -> m
  Value m _   -> m
  TupleApply m _ _ -> m
