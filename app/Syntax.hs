--------------------------------------------------------------------
-- |
-- Module    :  Syntax
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Syntax where

import           Data.Void             (Void)
import           Text.Megaparsec.Error (ParseErrorBundle)

type Name = String

data Type = Type String
  deriving (Eq, Ord, Show)

intType, floatType, boolType, strType :: Type
intType = Type "Integer"
floatType = Type "Float"
boolType = Type "Boolean"
strType = Type "String"

data Import = Import String
  deriving (Eq, Ord, Show)

data Export = Export String
  deriving (Eq, Ord, Show)

data Constant
  = CInt Integer
  | CFloat Double
  | CStr String
  deriving (Eq, Ord, Show)

data Expr m
  = CExpr m Constant
  | Var m String
  | Call m Name [(Expr m)]
  deriving (Eq, Ord, Show)

data DeclLHS m
  = DeclVal Name
  | DeclFun Name [(Name, m)]
  deriving (Eq, Ord, Show)

data Decl m = Decl (DeclLHS m) [(Decl m)] (Expr m)
  deriving (Eq, Ord, Show)

type Prgm m = ([Import], [Export], [(Decl m)])

type ParseErrorRes = ParseErrorBundle String Void

data ReplRes m
  = ReplDecl (Decl m)
  | ReplExpr (Expr m)
  | ReplErr ParseErrorRes
  deriving (Eq, Show)




-- Metadata for the Programs
data PreTyped = PreTyped (Maybe Type)
  deriving (Eq, Ord, Show)

data Typed = Typed Type
  deriving (Eq, Ord, Show)


getExprMeta :: Expr m -> m
getExprMeta expr = case expr of
  CExpr m _        -> m
  Var m _          -> m
  Call m _ _       -> m
