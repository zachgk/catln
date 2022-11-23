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

import           Emit                (codegenPrgm)
import           Eval.Common
import           Eval.ExprBuilder
import           Text.Printf
import           TreeBuild

type Op = (TypeName, [(PartialType, Guard EExpr, ResBuildEnvFunction)])

true, false :: Val
true = TupleVal "/Data/Primitive/True" H.empty
false = TupleVal "/Data/Primitive/False" H.empty

bool :: Bool -> Val
bool True  = true
bool False = false

liftIntOp :: TypeName -> (Integer -> Integer -> Integer) -> Op
liftIntOp name f = (name', [(srcType, NoGuard, \input -> PrimArrow input resType prim)])
  where
    name' = "/operator" ++ name
    srcType = PartialType (PTypeName name') H.empty (H.fromList [("l", intType), ("r", intType)]) [] PtArgExact
    resType = intType
    prim = EPrim srcType NoGuard (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (IntVal l), Just (IntVal r)) -> IntVal $ f l r
                           _ -> error "Invalid intOp signature"
                           )

liftCmpOp :: TypeName -> (Integer -> Integer -> Bool) -> Op
liftCmpOp name f = (name', [(srcType, NoGuard, \input -> PrimArrow input resType prim)])
  where
    name' = "/operator" ++ name
    srcType = PartialType (PTypeName name') H.empty (H.fromList [("l", intType), ("r", intType)]) [] PtArgExact
    resType = boolType
    prim = EPrim srcType NoGuard (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (IntVal l), Just (IntVal r)) -> bool $ f l r
                           _ -> error "Invalid compOp signature"
                           )

rneg :: TypeName -> Op
rneg name = (name', [(srcType, NoGuard, \input -> PrimArrow input resType prim)])
  where
    name' = "/operator" ++ name
    srcType = PartialType (PTypeName name') H.empty (H.singleton "a" intType) [] PtArgExact
    resType = intType
    prim = EPrim srcType NoGuard (\args -> case H.lookup "a" args of
                                  Just (IntVal i) -> IntVal $ -i
                                  _ -> error "Invalid rneg signature"
                              )

strEq :: Op
strEq = (name', [(srcType, NoGuard, \input -> PrimArrow input resType prim)])
  where
    name' = "/operator=="
    srcType = PartialType (PTypeName name') H.empty (H.fromList [("l", strType), ("r", strType)]) [] PtArgExact
    resType = boolType
    prim = EPrim srcType NoGuard (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                                  (Just (StrVal l), Just (StrVal r)) -> bool $ l == r
                                  _ -> error "Invalid intToString signature"
                              )

intToString :: Op
intToString = (name', [(srcType, NoGuard, \input -> PrimArrow input resType prim)])
  where
    name' = "/Data/toString"
    srcType = PartialType (PTypeName name') H.empty (H.singleton "this" intType) [] PtArgExact
    resType = strType
    prim = EPrim srcType NoGuard (\args -> case H.lookup "this" args of
                                  (Just (IntVal val)) -> StrVal $ show val
                                  _ -> error "Invalid intToString signature"
                              )


ioExit :: Op
ioExit = (name', [(srcType, NoGuard, \input -> PrimArrow input resType prim)])
  where
    name' = "/Catln/exit"
    srcType = PartialType (PTypeName name') H.empty (H.fromList [("this", ioType), ("val", intType)]) [] PtArgExact
    resType = ioType
    prim = EPrim srcType NoGuard (\args -> case (H.lookup "this" args, H.lookup "val" args) of
                                  (Just (IOVal _ io), Just (IntVal val)) -> IOVal val io
                                  _ -> error $ printf "Invalid exit signature with args: %s" (show args)
                              )

println :: Op
println = (name', [(srcType, NoGuard, \input -> PrimArrow input resType prim)])
  where
    name' = "/Catln/println"
    srcType = PartialType (PTypeName name') H.empty (H.fromList [("this", ioType), ("msg", strType)]) [] PtArgExact
    resType = ioType
    prim = EPrim srcType NoGuard (\args -> case (H.lookup "this" args, H.lookup "msg" args) of
                                  (Just (IOVal r io), Just (StrVal msg)) -> IOVal r (io >> putStrLn msg)
                                  _ -> error "Invalid println signature"
                              )

llvm :: Op
llvm = (name', [(srcType, NoGuard, aux)])
  where
    name' = "/Catln/llvm"
    srcType = PartialType (PTypeName name') H.empty (H.fromList [("c", TopType)]) [] PtArgExact
    aux a = MacroArrow a (singletonType resultLeaf) (MacroFunction macroBuild)
    macroBuild input MacroData{mdTbEnv, mdObj, mdObjSrcType} = do
      input' <- resolveTree mdTbEnv (mdObjSrcType, mdObj) input
      case input' of
        (ResEArrow _ _ _ (Arrow _ _ (Just expr))) -> case expr of
          (TupleApply _ (_, Value _ "/Catln/llvm") (TupleArgIO _ "c" (Value _ functionToCodegen))) -> buildName functionToCodegen
          _ -> error $ printf "Unknown expr to llvm macro: %s" (show expr)
        (ResArrowTupleApply _ "c" (ResArrowTuple functionToCodegen _)) -> buildName functionToCodegen
        _ -> error $ printf "Unknown input to llvm macro: %s" (show input')
      where
        buildName functionToCodegen = do
          let TBEnv{tbPrgm} = mdTbEnv
          let codegenSrcTypeInner = singletonType $ PartialType (PTypeName functionToCodegen) H.empty H.empty [] PtArgExact
          let codegenSrcType = PartialType (PTypeName "/Catln/Context") H.empty (H.fromList [("value", codegenSrcTypeInner), ("io", ioType)]) [] PtArgExact
          return $ ConstantArrow $ LLVMVal $ codegenPrgm (eVal functionToCodegen) codegenSrcType ioType tbPrgm

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
