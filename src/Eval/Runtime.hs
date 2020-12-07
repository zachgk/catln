--------------------------------------------------------------------
-- |
-- Module    :  Eval.Runtime
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module Eval.Runtime where

import qualified Data.HashMap.Strict as H
import           Syntax.Types
import           Syntax.Prgm
import           Syntax

import Eval.Common
import Text.Printf
import Emit (codegenPrgm)

type Op = (TypeName, [(PartialType, Guard (Expr Typed), ResBuildEnvFunction EPrim)])

true, false :: Val
true = TupleVal "True" H.empty
false = TupleVal "False" H.empty

bool :: Bool -> Val
bool True = true
bool False = false

liftIntOp :: TypeName -> (Integer -> Integer -> Integer) -> Op
liftIntOp name f = (name', [(srcType, NoGuard, \input -> PrimArrow input resType prim)])
  where
    name' = "operator" ++ name
    srcType = (PTypeName name', H.empty, H.empty, H.fromList [("l", intType), ("r", intType)])
    resType = intType
    prim = EPrim srcType NoGuard (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (IntVal l), Just (IntVal r)) -> IntVal $ f l r
                           _ -> error "Invalid intOp signature"
                           )

liftCmpOp :: TypeName -> (Integer -> Integer -> Bool) -> Op
liftCmpOp name f = (name', [(srcType, NoGuard, \input -> PrimArrow input resType prim)])
  where
    name' = "operator" ++ name
    srcType = (PTypeName name', H.empty, H.empty, H.fromList [("l", intType), ("r", intType)])
    resType = boolType
    prim = EPrim srcType NoGuard (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (IntVal l), Just (IntVal r)) -> bool $ f l r
                           _ -> error "Invalid compOp signature"
                           )

rneg :: TypeName -> Op
rneg name = (name', [(srcType, NoGuard, \input -> PrimArrow input resType prim)])
  where
    name' = "operator" ++ name
    srcType = (PTypeName name', H.empty, H.empty, H.singleton "a" intType)
    resType = intType
    prim = EPrim srcType NoGuard (\args -> case H.lookup "a" args of
                                  Just (IntVal i) -> IntVal $ -i
                                  _ -> error "Invalid rneg signature"
                              )

strEq :: Op
strEq = (name', [(srcType, NoGuard, \input -> PrimArrow input resType prim)])
  where
    name' = "operator=="
    srcType = (PTypeName name', H.empty, H.empty, H.fromList [("l", strType), ("r", strType)])
    resType = boolType
    prim = EPrim srcType NoGuard (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                                  (Just (StrVal l), Just (StrVal r)) -> bool $ l == r
                                  _ -> error "Invalid intToString signature"
                              )

intToString :: Op
intToString = (name', [(srcType, NoGuard, \input -> PrimArrow input resType prim)])
  where
    name' = "toString"
    srcType = (PTypeName name', H.empty, H.empty, H.singleton "this" intType)
    resType = strType
    prim = EPrim srcType NoGuard (\args -> case H.lookup "this" args of
                                  (Just (IntVal val)) -> StrVal $ show val
                                  _ -> error "Invalid intToString signature"
                              )


ioExit :: Op
ioExit = (name', [(srcType, NoGuard, \input -> PrimArrow input resType prim)])
  where
    name' = "exit"
    srcType = (PTypeName name', H.empty, H.empty, H.fromList [("this", ioType), ("val", intType)])
    resType = ioType
    prim = EPrim srcType NoGuard (\args -> case (H.lookup "this" args, H.lookup "val" args) of
                                  (Just (IOVal _ io), Just (IntVal val)) -> IOVal val io
                                  _ -> error "Invalid exit signature"
                              )

println :: Op
println = (name', [(srcType, NoGuard, \input -> PrimArrow input resType prim)])
  where
    name' = "println"
    srcType = (PTypeName name', H.empty, H.empty, H.fromList [("this", ioType), ("msg", strType)])
    resType = ioType
    prim = EPrim srcType NoGuard (\args -> case (H.lookup "this" args, H.lookup "msg" args) of
                                  (Just (IOVal r io), Just (StrVal msg)) -> IOVal r (io >> putStrLn msg)
                                  _ -> error "Invalid println signature"
                              )

llvm :: Op
llvm = (name', [(srcType, NoGuard, aux)])
  where
    name' = "llvm"
    srcType = (PTypeName name', H.empty, H.empty, H.fromList [("c", TopType)])
    aux (ResEArrow input obj arr) = MacroArrow (ConstantArrow $ LLVMQueue [(input, obj, arr)]) (singletonType resultLeaf) (MacroFunction macroBuild)
    aux input = error $ printf "Unknown input to llvm macro: %s" (show input)
    macroBuild (ConstantArrow (LLVMQueue [(_, _, Arrow _ _ _ (Just expr))])) MacroData{mdPrgm} = case expr of
      (TupleApply _ (_, Value _ "llvm") "c" f@(Value _ functionToCodegen)) -> ConstantArrow $ LLVMVal $ codegenPrgm f (PTypeName functionToCodegen, H.empty, H.empty, H.singleton "io" ioType) ioType mdPrgm
      _ -> error $ printf "Unknown expr to llvm macro: %s" (show expr)
    macroBuild input _ = error $ printf "Unknown input to llvm build macro: %s" (show input)

primEnv :: ResBuildEnv EPrim
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
