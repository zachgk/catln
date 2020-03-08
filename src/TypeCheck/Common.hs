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
import           Data.UnionFind.ST
import           Data.Hashable
import           GHC.Generics          (Generic)
import Control.Applicative
import Control.Monad ( when )
import Control.Monad.ST
import Data.STRef

import           Syntax

type TypeCheckError = String

data Scheme
  = SType RawType RawType -- SType upper lower
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
  | ArrowTo (Pnt s) (Pnt s)
  deriving (Eq)

type TypeCheckResult r = Either [TypeCheckError] r

type PreMeta = PreTyped
type PExpr = Expr PreMeta
type PArrow = Arrow PreMeta
type PObject = Object PreMeta
type PPrgm = Prgm PreMeta
type PReplRes = ReplRes PreMeta

type ShowMeta = Scheme
type SExpr = Expr ShowMeta
type SArrow = Arrow ShowMeta
type SObject = Object ShowMeta
type SPrgm = Prgm ShowMeta
type SReplRes = ReplRes ShowMeta

type VarMeta s = Pnt s
type VExpr s = Expr (VarMeta s)
type VArrow s = Arrow (VarMeta s)
type VObject s = Object (VarMeta s)
type VObjectMap s = [(VObject s, [VArrow s])]
type VPrgm s = (VObjectMap s, ClassMap)
type VReplRes s = ReplRes (VarMeta s)

type TypedMeta = Typed
type TExpr = Expr TypedMeta
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

addErr :: FEnv s -> String -> FEnv s
addErr (FEnv cons pmap errs) newErr = FEnv cons pmap (newErr:errs)

fLookup :: FEnv s -> String -> (Maybe (VObject s), FEnv s)
fLookup env@(FEnv _ pmap _) k = case H.lookup k pmap of
  Just v  -> (Just v, env)
  Nothing -> (Nothing, addErr env ("Failed to lookup " ++ k))

schemeError :: Scheme -> Bool
schemeError SCheckError{} = True
schemeError SType{} = False
