--------------------------------------------------------------------
-- |
-- Module    :  Eval.Runtime
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines additional functions that are added to the
-- program as the primitives. The declarations of these functions
-- are in the Catln core. The function are executed in the interpreter
-- by executing the backing Haskell functions.
--------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module Eval.Runtime where

import qualified Data.HashMap.Strict as H
import           Semantics.Prgm
import           Semantics.Types

-- import           Emit                (codegenPrgm)
import           Eval.Common
import           Eval.ExprBuilder
import           Text.Printf

type Op = (TypeName, [(PartialType, Maybe EExpr, Bool, ResBuildEnvFunction)])

true, false :: Val
true = TupleVal "/Data/Primitive/True" H.empty
false = TupleVal "/Data/Primitive/False" H.empty

bool :: Bool -> Val
bool True  = true
bool False = false

liftIntOp :: TypeName -> (Integer -> Integer -> Integer) -> Op
liftIntOp name f = (name', [(srcType, Nothing, False, TCPrim resType prim)])
  where
    name' = "/operator" ++ name
    srcType = PartialType name' H.empty (H.fromList [(partialKey "/l", intType), (partialKey "/r", intType)]) [] PtArgExact
    resType = intType
    prim = EPrim srcType Nothing (\args -> case (H.lookup "/l" args, H.lookup "/r" args) of
                           (Just (IntVal l), Just (IntVal r)) -> Right $ IntVal $ f l r
                           _ -> Left "Invalid intOp signature"
                           )

liftCmpOp :: TypeName -> (Integer -> Integer -> Bool) -> Op
liftCmpOp name f = (name', [(srcType, Nothing, False, TCPrim resType prim)])
  where
    name' = "/operator" ++ name
    srcType = PartialType name' H.empty (H.fromList [(partialKey "/l", intType), (partialKey "/r", intType)]) [] PtArgExact
    resType = boolType
    prim = EPrim srcType Nothing (\args -> case (H.lookup "/l" args, H.lookup "/r" args) of
                           (Just (IntVal l), Just (IntVal r)) -> Right $ bool $ f l r
                           _ -> Left "Invalid compOp signature"
                           )

rneg :: TypeName -> Op
rneg name = (name', [(srcType, Nothing, False, TCPrim resType prim)])
  where
    name' = "/operator" ++ name
    srcType = PartialType name' H.empty (H.singleton (partialKey "/a") intType) [] PtArgExact
    resType = intType
    prim = EPrim srcType Nothing (\args -> case H.lookup "/a" args of
                                  Just (IntVal i) -> Right $ IntVal $ -i
                                  _ -> Left "Invalid rneg signature"
                              )

strEq :: Op
strEq = (name', [(srcType, Nothing, False, TCPrim resType prim)])
  where
    name' = "/operator=="
    srcType = PartialType name' H.empty (H.fromList [(partialKey "/l", strType), (partialKey "/r", strType)]) [] PtArgExact
    resType = boolType
    prim = EPrim srcType Nothing (\args -> case (H.lookup "/l" args, H.lookup "/r" args) of
                                  (Just (StrVal l), Just (StrVal r)) -> Right $ bool $ l == r
                                  _ -> Left "Invalid intToString signature"
                              )

intToString :: Op
intToString = (name', [(srcType, Nothing, False, TCPrim resType prim)])
  where
    name' = "/Data/toString"
    srcType = PartialType name' H.empty (H.singleton (partialKey "/this") intType) [] PtArgExact
    resType = strType
    prim = EPrim srcType Nothing (\args -> case H.lookup "/this" args of
                                  (Just (IntVal val)) -> Right $ StrVal $ show val
                                  _ -> Left "Invalid intToString signature"
                              )


ioExit :: Op
ioExit = (name', [(srcType, Nothing, False, TCPrim resType prim)])
  where
    name' = "/Catln/exit"
    srcType = PartialType name' H.empty (H.fromList [(partialKey "/this", ioType), (partialKey "/val", intType)]) [] PtArgExact
    resType = ioType
    prim = EPrim srcType Nothing (\args -> case (H.lookup "/this" args, H.lookup "/val" args) of
                                  (Just (IOVal _ io), Just (IntVal val)) -> Right $ IOVal val io
                                  _ -> Left $ printf "Invalid exit signature with args: %s" (show args)
                              )

println :: Op
println = (name', [(srcType, Nothing, False, TCPrim resType prim)])
  where
    name' = "/Catln/println"
    srcType = PartialType name' H.empty (H.fromList [(partialKey "/this", ioType), (partialKey "/msg", strType)]) [] PtArgExact
    resType = ioType
    prim = EPrim srcType Nothing (\args -> case (H.lookup "/this" args, H.lookup "/msg" args) of
                                  (Just (IOVal r io), Just (StrVal msg)) -> Right $ IOVal r (io >> putStrLn msg)
                                  _ -> Left "Invalid println signature"
                              )

llvm :: Op
llvm = (name', [(srcType, Nothing, False, TCMacro (singletonType resultLeaf) (MacroFunction macroBuild))])
  where
    name' = "/Catln/llvm"
    srcType = PartialType name' H.empty (H.fromList [(partialKey "/c", topType)]) [] PtArgExact
    macroBuild input MacroData{mdTbEnv} = do
      case input of
        (TTupleApply _ (_, TValue _ "/Catln/llvm") ObjArr{oaObj=Just (GuardExpr (TValue _ "/c") Nothing), oaArr=(Just (GuardExpr (TValue _ functionToCodegen) Nothing), _)}) -> buildName functionToCodegen
        _ -> error $ printf "Unknown expr to llvm macro: %s" (show input)
      where
        buildName functionToCodegen = do
          let TBEnv{tbPrgm} = mdTbEnv
          let codegenSrcTypeInner = singletonType $ PartialType functionToCodegen H.empty H.empty [] PtArgExact
          let codegenSrcType = PartialType "/Catln/Context" H.empty (H.fromList [(partialKey "/value", codegenSrcTypeInner), (partialKey "/oaObjExprio", ioType)]) [] PtArgExact
          let val = LLVMVal $ codegenPrgm (eVal functionToCodegen) codegenSrcType ioType tbPrgm
          return $ TCExpr (emptyMetaT $ singletonType $ getValType val) val
        codegenPrgm _ _ _ _ = return ()

primEnv :: ResBuildEnv
primEnv = H.fromListWith (++) [ liftIntOp "+" (+)
                              , liftIntOp "-" (-)
                              , liftIntOp "*" (*)
                              , liftCmpOp ">" (>)
                              , liftCmpOp "<" (<)
                              , liftCmpOp ">=" (>=)
                              , liftCmpOp "<=" (<=)
                              , liftCmpOp "==" (==)
                              , liftCmpOp "!=" (/=)
                              , rneg "-"
                              , strEq
                              , intToString
                              , ioExit
                              , println
                              , llvm
                              ]
