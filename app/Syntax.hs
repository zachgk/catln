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

import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle)

type Name = String

data Import = Import String
  deriving (Eq, Ord, Show)

data Export = Export String
  deriving (Eq, Ord, Show)

data Constant
  = CInt Integer
  | CFloat Double
  deriving (Eq, Ord, Show)

data Expr
  = CExpr Constant
  | Var String
  | UnaryOp Name Expr
  | BinaryOp Name Expr Expr
  | Call Name [Expr]
  deriving (Eq, Ord, Show)

data DeclLHS
  = DeclVal Name
  | DeclFun Name [Name]
  deriving (Eq, Ord, Show)

data Decl = Decl DeclLHS [Decl] Expr
  deriving (Eq, Ord, Show)

type Prgm = ([Import], [Export], [Decl])

type ParseErrorRes = ParseErrorBundle String Void

data ReplRes
  = ReplDecl Decl
  | ReplExpr Expr
  | ReplErr ParseErrorRes
  deriving (Eq, Show)
