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

newtype Type = Type String
  deriving (Eq, Ord, Show)

intType, floatType, boolType, strType :: Type
intType = Type "Integer"
floatType = Type "Float"
boolType = Type "Boolean"
strType = Type "String"

newtype Import = Import String
  deriving (Eq, Ord, Show)

newtype Export = Export String
  deriving (Eq, Ord, Show)

data Constant
  = CInt Integer
  | CFloat Double
  | CStr String
  deriving (Eq, Ord, Show)

type RawExpr = Expr

data Expr m
  = CExpr m Constant
  | Var m Name
  | Call m Name [Expr m]
  deriving (Eq, Ord, Show)

data DeclLHS m
  = DeclVal Name
  | DeclFun Name [(Name, m)]
  deriving (Eq, Ord, Show)

data RawDecl m = RawDecl (DeclLHS m) [RawDecl m] (Expr m)
  deriving (Eq, Ord, Show)

data Decl m = Decl (DeclLHS m) (Expr m)
  deriving (Eq, Ord, Show)

type RawPrgm m = [RawDecl m] -- TODO: Include [Import], [Export]

type Prgm m = [Decl m] -- TODO: Include [Import], [Export]

type ParseErrorRes = ParseErrorBundle String Void

data ReplRes m
  = ReplDecl (RawDecl m)
  | ReplExpr (Expr m)
  | ReplErr ParseErrorRes
  deriving (Eq, Show)




-- Metadata for the Programs
newtype PreTyped = PreTyped (Maybe Type)
  deriving (Eq, Ord, Show)

newtype Typed = Typed Type
  deriving (Eq, Ord, Show)


getExprMeta :: Expr m -> m
getExprMeta expr = case expr of
  CExpr m _  -> m
  Var m _    -> m
  Call m _ _ -> m

getDeclLHSName :: DeclLHS m -> Name
getDeclLHSName (DeclVal n) = n
getDeclLHSName (DeclFun n _ ) = n
