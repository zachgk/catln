--------------------------------------------------------------------
-- |
-- Module    :  Emit.Runtime
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Emit.Runtime where

import qualified Data.HashMap.Strict as H
import           Syntax.Types
import           Syntax.Prgm
import           Syntax

import Eval.Common
import Emit.Common
import Emit.Codegen
import qualified LLVM.AST as AST
import LLVM.AST.Operand (Operand)
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Constant as C

type Op = (TypeName, [(PartialType, Guard (Expr Typed), ResArrowTree LLVMPrim -> MacroData LLVMPrim -> ResArrowTree LLVMPrim)])

true, false :: LVal
true = LTupleVal "True" H.empty
false = LTupleVal "False" H.empty

bool :: Bool -> LVal
bool True = true
bool False = false

liftBinOp :: Type -> Type -> Type -> TypeName -> (Operand -> Operand -> AST.Instruction) -> Op
liftBinOp lType rType resType name f = (name', [(srcType, NoGuard, \input _ -> PrimArrow input resType prim)])
  where
    name' = "operator" ++ name
    srcType = (PTypeName name', H.empty, H.empty, H.fromList [("l", lType), ("r", rType)])
    prim = LLVMPrim srcType NoGuard (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (LOVal _ l), Just (LOVal _ r)) -> LOVal resType $ do
                             l' <- l
                             r' <- r
                             instr (f l' r')
                           _ -> LIOVal (pure ()) -- TODO: Delete, should be an error
                           -- _ -> error "Invalid binary op signature"
                           )

liftIntOp :: TypeName -> (Operand -> Operand -> AST.Instruction) -> Op
liftIntOp = liftBinOp intType intType intType

liftCmpOp :: TypeName -> IP.IntegerPredicate -> Op
liftCmpOp name predicate = liftBinOp intType intType boolType name (\l r -> AST.ICmp predicate l r [])

rneg :: TypeName -> Op
rneg name = (name', [(srcType, NoGuard, \input _ -> PrimArrow input resType prim)])
  where
    name' = "operator" ++ name
    srcType = (PTypeName name', H.empty, H.empty, H.singleton "a" intType)
    resType = intType
    prim = LLVMPrim srcType NoGuard (\args -> case H.lookup "a" args of
                           Just (LOVal _ a') -> LOVal intType $ do
                             a'' <- a'
                             instr (AST.Mul False False a'' (cons $ C.Int 32 (-1)) [])
                           _ -> error "Invalid strEq signature"
                           )

-- Floating point operations for reference
-- Arithmetic and Constants
-- arith :: (Operand -> Operand -> Instruction) -> [(S.Typed, Operand)] -> Maybe (Codegen Operand)
-- arith _ [(S.Typed at, _), (S.Typed bt, _)] | at /= bt = Nothing
-- arith f [(at, ao), (_, bo)] | S.typedIs at S.floatType = Just $ instr $ f ao bo
-- arith f [(at, ao), (_, bo)] | S.typedIs at S.intType = Just $ instr $ f ao bo
-- arith _ _ = Nothing

-- fadd, fsub, fmul, fdiv, fand, for :: [(S.Typed, Operand)] -> Maybe (Codegen Operand)
-- fadd = arith (\a b -> FAdd noFastMathFlags a b [])
-- fsub = arith (\a b -> FSub noFastMathFlags a b [])
-- fmul = arith (\a b -> FMul noFastMathFlags a b [])
-- fdiv = arith (\a b -> FDiv noFastMathFlags a b [])
-- fand = arith (\a b -> And a b [])
-- for = arith (\a b -> Or a b [])

-- cmp :: FP.FloatingPointPredicate -> IP.IntegerPredicate -> [(S.Typed, Operand)] -> Maybe (Codegen Operand)
-- cmp _ _ [(S.Typed at, _), (S.Typed bt, _)] | at /= bt = Nothing
-- cmp cond _ [(at, ao), (_, bo)] | S.typedIs at S.floatType = Just $ instr $ FCmp cond ao bo []
-- cmp _ cond [(at, ao), (_, bo)] | S.typedIs at S.intType = Just $ instr $ ICmp cond ao bo []
-- cmp _ _ _ = Nothing

-- lt, gt, gte, lte, eq, neq :: [(S.Typed, Operand)] -> Maybe (Codegen Operand)
-- gt = cmp FP.UGT IP.UGT
-- lt = cmp FP.ULT IP.ULT
-- gte = cmp FP.UGE IP.UGE
-- lte = cmp FP.ULE IP.ULE
-- eq = cmp FP.UEQ IP.EQ
-- neq = cmp FP.UNE IP.NE

strEq :: Op
strEq = (name', [(srcType, NoGuard, \input _ -> PrimArrow input resType prim)])
  where
    name' = "operator=="
    srcType = (PTypeName name', H.empty, H.empty, H.fromList [("l", strType), ("r", strType)])
    resType = boolType
    prim = LLVMPrim srcType NoGuard (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (LOVal _ l), Just (LOVal _ r)) -> LOVal boolType $ do
                             l' <- l
                             r' <- r
                             call (externf "strcmp") [l', r'] -- TODO: really returns int type as result of comparison
                           _ -> error "Invalid strEq signature"
                           )

intToString :: Op
intToString = (name', [(srcType, NoGuard, \input _ -> PrimArrow input resType prim)])
  where
    name' = "toString"
    srcType = (PTypeName name', H.empty, H.empty, H.singleton "this" intType)
    resType = strType
    prim = LLVMPrim srcType NoGuard (\args -> case H.lookup "this" args of
                           Just (LOVal _ _) -> LOVal strType (pure $ cons $ C.Int 64 0) --TODO: Should do actual conversion
                           _ -> error "Invalid strEq signature"
                           )

ioExit :: Op
ioExit = (name', [(srcType, NoGuard, \input _ -> PrimArrow input resType prim)])
  where
    name' = "exit"
    srcType = (PTypeName name', H.empty, H.empty, H.fromList [("this", ioType), ("val", intType)])
    resType = ioType
    prim = LLVMPrim srcType NoGuard (\args -> case (H.lookup "this" args, H.lookup "val" args) of
                           (Just (LIOVal _), Just (LOVal _ r)) -> LIOVal $ do
                             r' <- r
                             _ <- call (externf "exit") [r']
                             return ()
                           _ -> LIOVal (pure ())-- TODO: Delete, should be an error
                           -- _ -> error "Invalid ioExit signature"
                           )

println :: Op
println = (name', [(srcType, NoGuard, \input _ -> PrimArrow input resType prim)])
  where
    name' = "println"
    srcType = (PTypeName name', H.empty, H.empty, H.fromList [("this", ioType), ("msg", strType)])
    resType = ioType
    prim = LLVMPrim srcType NoGuard (\args -> case (H.lookup "this" args, H.lookup "msg" args) of
                           (Just (LIOVal _), Just (LOVal _ s)) -> LIOVal $ do
                             s' <- s
                             _ <- call (externf "puts") [s']
                             return ()
                           _ -> error "Invalid strEq signature"
                           )


primEnv :: ResBuildEnv LLVMPrim
primEnv = H.fromListWith (++) [liftIntOp "+" (\l r -> AST.Add False False l r [])
                              , liftIntOp "-" (\l r -> AST.Sub False False l r [])
                              , liftIntOp "*" (\l r -> AST.Mul False False l r [])
                              , liftCmpOp ">" IP.SGT
                              , liftCmpOp "<" IP.SLT
                              , liftCmpOp ">=" IP.SGE
                              , liftCmpOp "<=" IP.SLE
                              , liftCmpOp "==" IP.EQ
                              , liftCmpOp "!=" IP.NE
                              , rneg "-"
                              , strEq
                              , intToString
                              , ioExit
                              , println
                              ]
