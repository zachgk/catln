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

import           Syntax

type EvalMeta = Typed
type ECompAnnot = CompAnnot EvalMeta
type EExpr = Expr EvalMeta
type EObject = Object EvalMeta
type EArrow = Arrow EvalMeta
type EObjectMap = ObjectMap EvalMeta
type EPrgm = Prgm EvalMeta
type EReplRes = ReplRes EvalMeta

data Val
  = IntVal Integer
  | FloatVal Double
  | BoolVal Bool
  | StrVal String
  | TupleVal String (H.HashMap String Val)

data EvalError
  = GenEvalError String
  | AssertError String
  deriving (Eq, Show)

data ResArrow
  = ResEArrow EArrow
  | PrimArrow Type (H.HashMap String Val -> Val) -- runtime function

type ResEnv = H.HashMap LeafType [ResArrow]
type Env = (ResEnv, H.HashMap LeafType Val)

data ResArrowTree
  = ResArrowTree ResArrow (H.HashMap LeafType ResArrowTree)
  | ResArrowID
  deriving (Show)

instance Show Val where
  show (IntVal i)   = show i
  show (FloatVal d) = show d
  show (BoolVal b)  = show b
  show (StrVal s)   = show s
  show (TupleVal n a)   = show n ++ show a

instance Show ResArrow where
  show (ResEArrow arrow) = "(ResEArrow " ++ show arrow ++ ")"
  show (PrimArrow tp _) = "(PrimArrow " ++ show tp ++ ")"

getValType :: Val -> LeafType
getValType IntVal{} = intLeaf
getValType FloatVal{} = floatLeaf
getValType BoolVal{} = boolLeaf
getValType StrVal{} = strLeaf
getValType (TupleVal name args) = LeafType name (fmap getValType args)

resArrowDestType :: ResArrow -> Type
resArrowDestType (ResEArrow (Arrow (Typed tp) _ _)) = tp
resArrowDestType (PrimArrow tp _) = tp
