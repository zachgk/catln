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

data RawLeafType = RawLeafType String (H.HashMap String RawLeafType)
  deriving (Eq, Ord, Show, Generic)
instance Hashable RawLeafType

data RawType
  = RawSumType (S.HashSet RawLeafType)
  | RawTopType
  | RawBottomType
  deriving (Eq, Ord, Show, Generic)
instance Hashable RawType

data LeafType = LeafType String (H.HashMap String LeafType)
  deriving (Eq, Ord, Show, Generic)
instance Hashable LeafType

newtype Type = SumType (S.HashSet LeafType)
  deriving (Eq, Ord, Show, Generic)
instance Hashable Type

rintLeaf, rfloatLeaf, rboolLeaf, rstrLeaf :: RawLeafType
rintLeaf = RawLeafType "Integer" H.empty
rfloatLeaf = RawLeafType "Float" H.empty
rboolLeaf = RawLeafType "Boolean" H.empty
rstrLeaf = RawLeafType "String" H.empty

rintType, rfloatType, rboolType, rstrType :: RawType
rintType = RawSumType $ S.singleton $ RawLeafType "Integer" H.empty
rfloatType = RawSumType $ S.singleton $ RawLeafType "Float" H.empty
rboolType = RawSumType $ S.singleton $ RawLeafType "Boolean" H.empty
rstrType = RawSumType $ S.singleton $ RawLeafType "String" H.empty

intLeaf, floatLeaf, boolLeaf, strLeaf :: LeafType
intLeaf = LeafType "Integer" H.empty
floatLeaf = LeafType "Float" H.empty
boolLeaf = LeafType "Boolean" H.empty
strLeaf = LeafType "String" H.empty

intType, floatType, boolType, strType :: Type
intType = SumType $ S.singleton $ LeafType "Integer" H.empty
floatType = SumType $ S.singleton $ LeafType "Float" H.empty
boolType = SumType $ S.singleton $ LeafType "Boolean" H.empty
strType = SumType $ S.singleton $ LeafType "String" H.empty

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
  deriving (Eq, Ord, Show, Generic)
instance Hashable m => Hashable (Object m)

data Arrow m = Arrow m (Expr m) -- m is result metadata
  deriving (Eq, Ord, Show)

type Prgm m = (H.HashMap (Object m) [Arrow m]) -- TODO: Include [Import], [Export]

type ParseErrorRes = ParseErrorBundle String Void

data ReplRes m
  = ReplDecl (RawDecl m)
  | ReplExpr (Expr m)
  | ReplErr ParseErrorRes
  deriving (Eq, Show)



-- Metadata for the Programs
newtype PreTyped = PreTyped RawType
  deriving (Eq, Ord, Show, Generic)
instance Hashable PreTyped

newtype Typed = Typed Type
  deriving (Eq, Ord, Show, Generic)
instance Hashable Typed

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
