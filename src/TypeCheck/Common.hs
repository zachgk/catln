--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Common
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

module TypeCheck.Common where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet          as S
import           Data.UnionFind.ST
import           Data.Hashable
import           GHC.Generics          (Generic)
import Control.Applicative
import Control.Monad ( when )
import Control.Monad.ST
import Data.STRef

import           Syntax

data TypeCheckError
  = GenTypeCheckError String
  | AbandonCon SConstraint
  | FailInfer String Scheme [SConstraint]
  | TupleMismatch String Typed (TypeCheckResult (H.HashMap String TExpr)) [SConstraint]
  deriving (Eq, Ord, Show)

data Scheme
  = SType RawType RawType String -- SType upper lower description
  | SCheckError String
  deriving (Eq, Ord, Show, Generic)
instance Hashable Scheme

type Pnt s = Point s Scheme

type ObjMap s = (H.HashMap String (VObject s))
data FEnv s = FEnv [Constraint s] (ObjMap s) [TypeCheckError]

data Constraint s
  = EqualsKnown (Pnt s) RawType
  | EqPoints (Pnt s) (Pnt s)
  | BoundedBy (Pnt s) (Pnt s)
  | IsTupleOf (Pnt s) (H.HashMap String (Pnt s))
  | ArrowTo (Pnt s) (Pnt s) -- ArrowTo src dest
  deriving (Eq)

data SConstraint
  = SEqualsKnown Scheme RawType
  | SEqPoints Scheme Scheme
  | SBoundedBy Scheme Scheme
  | SIsTupleOf Scheme (H.HashMap String Scheme)
  | SArrowTo Scheme Scheme
  deriving (Eq, Ord, Show)

type TypeCheckResult r = Either [TypeCheckError] r

type PreMeta = PreTyped
type PExpr = Expr PreMeta
type PCompAnnot = CompAnnot PreMeta
type PArrow = Arrow PreMeta
type PObject = Object PreMeta
type PPrgm = Prgm PreMeta
type PReplRes = ReplRes PreMeta

type ShowMeta = Scheme
type SExpr = Expr ShowMeta
type SCompAnnot = CompAnnot ShowMeta
type SArrow = Arrow ShowMeta
type SObject = Object ShowMeta
type SPrgm = Prgm ShowMeta
type SReplRes = ReplRes ShowMeta

type VarMeta s = Pnt s
type VExpr s = Expr (VarMeta s)
type VCompAnnot s = CompAnnot (VarMeta s)
type VArrow s = Arrow (VarMeta s)
type VObject s = Object (VarMeta s)
type VObjectMap s = [(VObject s, [VArrow s])]
type VPrgm s = (VObjectMap s, ClassMap)
type VReplRes s = ReplRes (VarMeta s)

type TypedMeta = Typed
type TExpr = Expr TypedMeta
type TCompAnnot = CompAnnot TypedMeta
type TArrow = Arrow TypedMeta
type TObject = Object TypedMeta
type TPrgm = Prgm TypedMeta
type TReplRes = ReplRes TypedMeta

-- implicit graph
type TypeGraph s = H.HashMap RawLeafType [Pnt s]

getPnt :: VarMeta s -> Pnt s
getPnt x = x

getPntExpr :: VExpr s -> Pnt s
getPntExpr = getPnt . getExprMeta

addErr :: FEnv s -> TypeCheckError -> FEnv s
addErr (FEnv cons pmap errs) newErr = FEnv cons pmap (newErr:errs)

fLookup :: FEnv s -> String -> (Maybe (VObject s), FEnv s)
fLookup env@(FEnv _ pmap _) k = case H.lookup k pmap of
  Just v  -> (Just v, env)
  Nothing -> (Nothing, addErr env (GenTypeCheckError $ "Failed to lookup " ++ k))

schemeError :: Scheme -> Bool
schemeError SCheckError{} = True
schemeError SType{} = False
