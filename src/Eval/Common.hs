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
import           Data.List                      ( intercalate )

import           Syntax.Types
import           Syntax.Prgm
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

type Args = H.HashMap String Val

data Val
  = IntVal Integer
  | FloatVal Double
  | BoolVal Bool
  | StrVal String
  | TupleVal String Args
  | NoVal

instance Show Val where
  show (IntVal i)   = show i
  show (FloatVal d) = show d
  show (BoolVal b)  = show b
  show (StrVal s)   = show s
  show (TupleVal name args) = if H.null args
    then name
    else name ++ "(" ++ args' ++ ")"
    where
      showArg (argName, val) = argName ++ " = " ++ show val
      args' = intercalate ", " $ map showArg $ H.toList args
  show NoVal   = "NoVal"

getValType :: Val -> LeafType
getValType IntVal{} = intLeaf
getValType FloatVal{} = floatLeaf
getValType BoolVal{} = boolLeaf
getValType StrVal{} = strLeaf
getValType (TupleVal name args) = LeafType name (fmap getValType args)
getValType NoVal = error "getValType of NoVal"
