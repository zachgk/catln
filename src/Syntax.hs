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

data RawType
  = RawLeafType String
  | RawSumType [RawType]
  | RawProdType [RawType]
  | RawTopType
  | RawBottomType
  deriving (Eq, Ord, Show)

data Type
  = LeafType String
  | SumType [Type]
  | ProdType [Type]
  deriving (Eq, Ord, Show)

rintType, rfloatType, rboolType, rstrType :: RawType
rintType = RawLeafType "Integer"
rfloatType = RawLeafType "Float"
rboolType = RawLeafType "Boolean"
rstrType = RawLeafType "String"

intType, floatType, boolType, strType :: Type
intType = LeafType "Integer"
floatType = LeafType "Float"
boolType = LeafType "Boolean"
strType = LeafType "String"

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
  | Tuple m Name [Expr m]
  deriving (Eq, Ord, Show)

data DeclLHS m = DeclLHS m Name [(Name,m)]
  deriving (Eq, Ord, Show)

data RawDecl m = RawDecl (DeclLHS m) [RawDecl m] (Expr m)
  deriving (Eq, Ord, Show)

-- data Decl m = Decl (DeclLHS m) (Expr m)
--   deriving (Eq, Ord, Show)

type RawPrgm m = [RawDecl m] -- TODO: Include [Import], [Export]

data Global m = Global m Name (Expr m)
  deriving (Eq, Ord, Show)

data Object m = Object m Name [(Name, m)]
  deriving (Eq, Ord, Show)

data Arrow m = Arrow m Name (Expr m)
  deriving (Eq, Ord, Show)

type Prgm m = ([Global m], [Object m], [Arrow m]) -- TODO: Include [Import], [Export]

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

hasRawType :: RawType -> RawType -> Bool
hasRawType _ RawTopType = True
hasRawType RawTopType t = t == RawTopType
hasRawType RawBottomType _ = True
hasRawType t RawBottomType = t == RawBottomType
hasRawType subType@RawLeafType{} parentType@RawLeafType{} = subType == parentType
hasRawType subType@RawLeafType{} (RawSumType tps) = any (hasRawType subType) tps
hasRawType RawLeafType{} RawProdType{} = False
hasRawType (RawSumType subs) parents@(RawSumType _) = all (`hasRawType` parents) subs
hasRawType RawSumType{} _ = False
hasRawType (RawProdType subs) (RawProdType parents) | length subs /= length parents = False
hasRawType (RawProdType subs) (RawProdType parents) = all (uncurry hasRawType) $ zip subs parents
hasRawType RawProdType{} _ = False

unionRawTypes :: RawType -> RawType -> RawType
unionRawTypes RawBottomType t = t
unionRawTypes t RawBottomType = t
unionRawTypes RawTopType _ = RawTopType
unionRawTypes _ RawTopType = RawTopType
unionRawTypes t1@RawLeafType{} t2 = unionRawTypes (RawSumType [t1]) t2
unionRawTypes t1 t2@RawLeafType{} = unionRawTypes t1 (RawSumType [t2])
unionRawTypes t1@RawProdType{} t2 = unionRawTypes (RawSumType [t1]) t2
unionRawTypes t1 t2@RawProdType{} = unionRawTypes t1 (RawSumType [t2])
unionRawTypes (RawSumType []) s2s@RawSumType{} = s2s
unionRawTypes (RawSumType (t1:t1s)) s2s@(RawSumType t2s) = if hasRawType t1 s2s then unionRawTypes (RawSumType t1s) s2s else unionRawTypes (RawSumType t1s) (RawSumType (t1:t2s))

intersectRawTypes :: RawType -> RawType -> RawType
intersectRawTypes RawTopType t = t
intersectRawTypes t RawTopType = t
intersectRawTypes RawBottomType _ = RawBottomType
intersectRawTypes _ RawBottomType = RawBottomType
intersectRawTypes t1@RawLeafType{} t2 = intersectRawTypes (RawSumType [t1]) t2
intersectRawTypes t1 t2@RawLeafType{} = intersectRawTypes t1 (RawSumType [t2])
intersectRawTypes t1@RawProdType{} t2 = intersectRawTypes (RawSumType [t1]) t2
intersectRawTypes t1 t2@RawProdType{} = intersectRawTypes t1 (RawSumType [t2])
intersectRawTypes (RawSumType []) (RawSumType _) = RawSumType []
intersectRawTypes (RawSumType (t1:t1s)) s2s@(RawSumType t2s) = RawSumType $ first ++ rst
  where first = [t1 | any (hasRawType t1) t2s]
        (RawSumType rst) = intersectRawTypes (RawSumType t1s) s2s

typedIs :: Typed -> Type -> Bool
typedIs (Typed t1) t2 = t1 == t2

getExprMeta :: Expr m -> m
getExprMeta expr = case expr of
  CExpr m _  -> m
  Var m _    -> m
  Tuple m _ _ -> m

-- getDeclName :: Decl m -> Name
-- getDeclName (Decl (DeclLHS _ name _) _) = name

