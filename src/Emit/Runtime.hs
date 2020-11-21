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

import Emit.Common
import Codegen
import qualified LLVM.AST as AST
import LLVM.AST.Operand (Operand)
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Constant as C

type Op = (TypeName, [(PartialType, Guard (Expr Typed), ResArrow LLVMPrim)])

true, false :: Val
true = TupleVal "True" H.empty
false = TupleVal "False" H.empty

bool :: Bool -> Val
bool True = true
bool False = false

liftBinOp :: Type -> Type -> Type -> TypeName -> (Operand -> Operand -> AST.Instruction) -> Op
liftBinOp lType rType resType name f = (name', [(srcType, NoGuard, PrimArrow resType prim)])
  where
    name' = "operator" ++ name
    srcType = (PTypeName name', H.empty, H.empty, H.fromList [("l", lType), ("r", rType)])
    prim = LLVMPrim srcType NoGuard (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (OVal _ l), Just (OVal _ r)) -> OVal resType <$> instr (f l r)
                           _ -> pure IOVal -- TODO: Delete, should be an error
                           -- _ -> error "Invalid binary op signature"
                           )

liftIntOp :: TypeName -> (Operand -> Operand -> AST.Instruction) -> Op
liftIntOp = liftBinOp intType intType intType

liftCmpOp :: TypeName -> IP.IntegerPredicate -> Op
liftCmpOp name predicate = liftBinOp intType intType boolType name (\l r -> AST.ICmp predicate l r [])

rneg :: TypeName -> Op
rneg name = (name', [(srcType, NoGuard, PrimArrow resType prim)])
  where
    name' = "operator" ++ name
    srcType = (PTypeName name', H.empty, H.empty, H.singleton "a" intType)
    resType = intType
    prim = LLVMPrim srcType NoGuard (\args -> case H.lookup "a" args of
                           Just (OVal _ a') -> OVal intType <$> instr (AST.Mul False False a' (cons $ C.Int 32 (-1)) [])
                           _ -> error "Invalid strEq signature"
                           )

strEq :: Op
strEq = (name', [(srcType, NoGuard, PrimArrow resType prim)])
  where
    name' = "operator=="
    srcType = (PTypeName name', H.empty, H.empty, H.fromList [("l", strType), ("r", strType)])
    resType = boolType
    prim = LLVMPrim srcType NoGuard (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (OVal _ l), Just (OVal _ r)) -> OVal boolType <$> call (externf "strcmp") [l, r] -- TODO: really returns int type as result of comparison
                           _ -> error "Invalid strEq signature"
                           )

intToString :: Op
intToString = (name', [(srcType, NoGuard, PrimArrow resType prim)])
  where
    name' = "toString"
    srcType = (PTypeName name', H.empty, H.empty, H.singleton "this" intType)
    resType = strType
    prim = LLVMPrim srcType NoGuard (\args -> case H.lookup "this" args of
                           Just (OVal _ _) -> OVal strType <$> (pure $ cons $ C.Int 64 0) --TODO: Should do actual conversion
                           _ -> error "Invalid strEq signature"
                           )

ioExit :: Op
ioExit = (name', [(srcType, NoGuard, PrimArrow resType prim)])
  where
    name' = "exit"
    srcType = (PTypeName name', H.empty, H.empty, H.fromList [("this", ioType), ("val", intType)])
    resType = ioType
    prim = LLVMPrim srcType NoGuard (\args -> case (H.lookup "this" args, H.lookup "val" args) of
                           (Just IOVal, Just (OVal _ r)) -> call (externf "exit") [r] >> (pure IOVal)
                           _ -> pure IOVal -- TODO: Delete, should be an error
                           -- _ -> error "Invalid ioExit signature"
                           )

println :: Op
println = (name', [(srcType, NoGuard, PrimArrow resType prim)])
  where
    name' = "println"
    srcType = (PTypeName name', H.empty, H.empty, H.fromList [("this", ioType), ("msg", strType)])
    resType = ioType
    prim = LLVMPrim srcType NoGuard (\args -> case (H.lookup "this" args, H.lookup "msg" args) of
                           (Just IOVal, Just (OVal _ s)) -> call (externf "puts") [s] >> (pure IOVal)
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
