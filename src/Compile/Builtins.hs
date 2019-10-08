--------------------------------------------------------------------
-- |
-- Module    :  Builtins
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Compile.Builtins(runtimeOps) where

import qualified Data.HashMap.Strict as H

import           LLVM.AST
import qualified LLVM.AST                        as AST

import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP

import Codegen
import qualified Syntax as S

-- Arithmetic and Constants
arith :: (Operand -> Operand -> Instruction) -> [(S.Typed, Operand)] -> Maybe (Codegen Operand)
arith _ [(S.Typed at, _), (S.Typed bt, _)] | at /= bt = Nothing
arith f [(at, ao), (_, bo)] | S.typedIs at S.floatType = Just $ instr $ f ao bo
arith f [(at, ao), (_, bo)] | S.typedIs at S.intType = Just $ instr $ f ao bo
arith _ _ = Nothing

fadd, fsub, fmul, fdiv, fand, for :: [(S.Typed, Operand)] -> Maybe (Codegen Operand)
fadd = arith (\a b -> FAdd noFastMathFlags a b [])
fsub = arith (\a b -> FSub noFastMathFlags a b [])
fmul = arith (\a b -> FMul noFastMathFlags a b [])
fdiv = arith (\a b -> FDiv noFastMathFlags a b [])
fand = arith (\a b -> And a b [])
for = arith (\a b -> Or a b [])

cmp :: FP.FloatingPointPredicate -> IP.IntegerPredicate -> [(S.Typed, Operand)] -> Maybe (Codegen Operand)
cmp _ _ [(S.Typed at, _), (S.Typed bt, _)] | at /= bt = Nothing
cmp cond _ [(at, ao), (_, bo)] | S.typedIs at S.floatType = Just $ instr $ FCmp cond ao bo []
cmp _ cond [(at, ao), (_, bo)] | S.typedIs at S.intType = Just $ instr $ ICmp cond ao bo []
cmp _ _ _ = Nothing

lt, gt, gte, lte, eq, neq :: [(S.Typed, Operand)] -> Maybe (Codegen Operand)
gt = cmp FP.UGT IP.UGT
lt = cmp FP.ULT IP.ULT
gte = cmp FP.UGE IP.UGE
lte = cmp FP.ULE IP.ULE
eq = cmp FP.UEQ IP.EQ
neq = cmp FP.UNE IP.NE

runtimeOps :: H.HashMap String ([(S.Typed, AST.Operand)] -> Maybe (Codegen AST.Operand))
runtimeOps = H.fromList [
    ("+", fadd)
  , ("-", fsub)
  , ("*", fmul)
  , ("/", fdiv)
  , (">", gt)
  , ("<", lt)
  , (">=", gte)
  , ("<=", lte)
  , ("==", eq)
  , ("!=", neq)
  , ("&", fand)
  , ("|", for)
  -- , ("~", fnot)
                          ]
