--------------------------------------------------------------------
-- |
-- Module    :  Eval.Common
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Eval.Common where

import qualified Data.HashMap.Strict as H

import           Syntax.Types
import           Syntax

type EvalMeta = Typed
type ECompAnnot = CompAnnot EvalMeta
type EExpr = Expr EvalMeta
type EObject = Object EvalMeta
type EArrow = Arrow EvalMeta
type EObjectMap = ObjectMap EvalMeta
type EPrgm = Prgm EvalMeta
type EReplRes = ReplRes EvalMeta

type EPrim = H.HashMap String Val -> Val

type Env = ResExEnv EPrim

data Val
  = IntVal Integer
  | FloatVal Double
  | BoolVal Bool
  | StrVal String
  | TupleVal String (H.HashMap String Val)
  | NoVal

instance Show Val where
  show (IntVal i)   = show i
  show (FloatVal d) = show d
  show (BoolVal b)  = show b
  show (StrVal s)   = show s
  show (TupleVal n a)   = show n ++ show a
  show NoVal   = "NoVal"

getValType :: Val -> LeafType
getValType IntVal{} = intLeaf
getValType FloatVal{} = floatLeaf
getValType BoolVal{} = boolLeaf
getValType StrVal{} = strLeaf
getValType (TupleVal name args) = LeafType name (fmap getValType args)
getValType NoVal = error "getValType of NoVal"
