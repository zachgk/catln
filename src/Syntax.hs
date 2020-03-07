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

{-# LANGUAGE DeriveGeneric #-}

module Syntax where

import           Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet          as S
import           Data.Void             (Void)

import           GHC.Generics          (Generic)
import           Text.Megaparsec.Error (ParseErrorBundle)

type Name = String

data RawLeafType
  = RawLeafType String
  | RawProdType String (H.HashMap String RawLeafType)
  deriving (Eq, Ord, Show, Generic)
instance Hashable RawLeafType

data RawType
  = RawSumType (S.HashSet RawLeafType)
  | RawTopType
  | RawBottomType
  deriving (Eq, Ord, Show)

-- TODO: Consider unifying LeafType and ProdType with no args
data LeafType
  = LeafType String
  | ProdType String (H.HashMap String LeafType)
  deriving (Eq, Ord, Show, Generic)
instance Hashable LeafType

newtype Type = SumType (S.HashSet LeafType)
  deriving (Eq, Ord, Show)

rintType, rfloatType, rboolType, rstrType :: RawType
rintType = RawSumType $ S.singleton $ RawLeafType "Integer"
rfloatType = RawSumType $ S.singleton $ RawLeafType "Float"
rboolType = RawSumType $ S.singleton $ RawLeafType "Boolean"
rstrType = RawSumType $ S.singleton $ RawLeafType "String"

intLeaf, floatLeaf, boolLeaf, strLeaf :: LeafType
intLeaf = LeafType "Integer"
floatLeaf = LeafType "Float"
boolLeaf = LeafType "Boolean"
strLeaf = LeafType "String"

intType, floatType, boolType, strType :: Type
intType = SumType $ S.singleton $ LeafType "Integer"
floatType = SumType $ S.singleton $ LeafType "Float"
boolType = SumType $ S.singleton $ LeafType "Boolean"
strType = SumType $ S.singleton $ LeafType "String"

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
  | Tuple m Name (H.HashMap Name (Expr m))
  deriving (Eq, Ord, Show)

data DeclLHS m = DeclLHS m Name (H.HashMap Name m)
  deriving (Eq, Ord, Show)

data RawDecl m = RawDecl (DeclLHS m) [RawDecl m] (Expr m)
  deriving (Eq, Ord, Show)

type RawPrgm m = [RawDecl m] -- TODO: Include [Import], [Export]

data Object m = Object m Name (H.HashMap Name m)
  deriving (Eq, Ord, Show)

data Arrow m = Arrow m (Object m) (Expr m) -- m is result metadata
  deriving (Eq, Ord, Show)

type Prgm m = ([Object m], [Arrow m]) -- TODO: Include [Import], [Export]

type ParseErrorRes = ParseErrorBundle String Void

data ReplRes m
  = ReplDecl (RawDecl m)
  | ReplExpr (Expr m)
  | ReplErr ParseErrorRes
  deriving (Eq, Show)



-- Metadata for the Programs
newtype PreTyped = PreTyped RawType
  deriving (Eq, Ord, Show)

newtype Typed = Typed Type
  deriving (Eq, Ord, Show)

-- Maybe rename to subtypeOf
hasRawType :: RawType -> RawType -> Bool
hasRawType _ RawTopType = True
hasRawType RawTopType t = t == RawTopType
hasRawType RawBottomType _ = True
hasRawType t RawBottomType = t == RawBottomType
hasRawType (RawSumType subLeafs) (RawSumType superLeafs) = all (`elem` superLeafs) subLeafs

unionRawTypes :: RawType -> RawType -> RawType
unionRawTypes RawBottomType t = t
unionRawTypes t RawBottomType = t
unionRawTypes RawTopType _ = RawTopType
unionRawTypes _ RawTopType = RawTopType
unionRawTypes (RawSumType subLeafs) (RawSumType superLeafs) = RawSumType $ S.union subLeafs superLeafs

intersectRawTypes :: RawType -> RawType -> RawType
intersectRawTypes RawTopType t = t
intersectRawTypes t RawTopType = t
intersectRawTypes RawBottomType _ = RawBottomType
intersectRawTypes _ RawBottomType = RawBottomType
intersectRawTypes (RawSumType subLeafs) (RawSumType superLeafs) = RawSumType $ S.intersection subLeafs superLeafs

typedIs :: Typed -> Type -> Bool
typedIs (Typed t1) t2 = t1 == t2

getExprMeta :: Expr m -> m
getExprMeta expr = case expr of
  CExpr m _   -> m
  Tuple m _ _ -> m
