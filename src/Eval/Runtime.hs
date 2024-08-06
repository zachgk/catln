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

type Op = (String, Either EPrim MacroFunction)

true, false :: Val
true = TupleVal "/Data/Primitive/True" H.empty
false = TupleVal "/Data/Primitive/False" H.empty

bool :: Bool -> Val
bool True  = true
bool False = false

liftIntOp :: TypeName -> (Integer -> Integer -> Integer) -> EPrim
liftIntOp name f = EPrim ("int" ++ name) prim
  where
    prim args = case (H.lookup "/l" args, H.lookup "/r" args) of
      (Just (IntVal l), Just (IntVal r)) -> Right $ IntVal $ f l r
      _                                  -> Left "Invalid intOp signature"

liftCmpOp :: TypeName -> (Integer -> Integer -> Bool) -> EPrim
liftCmpOp name f = EPrim ("int" ++ name) prim
  where
    prim args = case (H.lookup "/l" args, H.lookup "/r" args) of
      (Just (IntVal l), Just (IntVal r)) -> Right $ bool $ f l r
      _                                  -> Left "Invalid compOp signature"

rneg :: EPrim
rneg = EPrim "intNeg" prim
  where
    prim args = case H.lookup "/a" args of
      Just (IntVal i) -> Right $ IntVal $ -i
      _               -> Left "Invalid rneg signature"

strEq :: EPrim
strEq = EPrim "strEq" prim
  where
    prim args = case (H.lookup "/l" args, H.lookup "/r" args) of
      (Just (StrVal l), Just (StrVal r)) -> Right $ bool $ l == r
      _                                  -> Left "Invalid intToString signature"

intToString :: EPrim
intToString = EPrim "intToString" prim
  where
    prim args = case H.lookup "/this" args of
      (Just (IntVal val)) -> Right $ StrVal $ show val
      _                   -> Left "Invalid intToString signature"


ioExit :: EPrim
ioExit = EPrim "ioExit" prim
  where
    prim args = case (H.lookup "/this" args, H.lookup "/val" args) of
      (Just (IOVal _ io), Just (IntVal val)) -> Right $ IOVal val io
      _ -> Left $ printf "Invalid exit signature with args: %s" (show args)

println :: EPrim
println = EPrim "println" prim
  where
    prim args = case (H.lookup "/this" args, H.lookup "/msg" args) of
      (Just (IOVal r io), Just (StrVal msg)) -> Right $ IOVal r (io >> putStrLn msg)
      _ -> Left "Invalid println signature"

llvm :: Op
llvm = ("llvm", Right (MacroFunction macroBuild))
  where
    macroBuild input MacroData{mdTbEnv} =
      case input of
        (TupleApply _ (_, Value _ "/Catln/llvm") (EAppArg ObjArr{oaObj=Just (Value _ "/c"), oaArr=Just (Just (Value _ functionToCodegen), _)})) -> buildName functionToCodegen
        _ -> error $ printf "Unknown expr to llvm macro: %s" (show input)
      where
        buildName functionToCodegen = do
          let TBEnv{tbPrgm} = mdTbEnv
          let codegenSrcTypeInner = singletonType $ PartialType functionToCodegen H.empty H.empty PredsNone PtArgExact
          let codegenSrcType = PartialType "/Catln/Context" H.empty (H.fromList [(partialKey "/value", codegenSrcTypeInner), (partialKey "/oaObjExprio", ioType)]) PredsNone PtArgExact
          let val = LLVMVal $ codegenPrgm (eVal functionToCodegen) codegenSrcType ioType tbPrgm
          return $ Left val
        codegenPrgm _ _ _ _ = return ()

primEnv :: ResBuildPrims
primEnv = H.fromList (map mapPrim prims ++ macros)
  where
    mapPrim p@(EPrim n _) = (n, Left p)
    prims = [ liftIntOp "+" (+)
            , liftIntOp "-" (-)
            , liftIntOp "*" (*)
            , liftCmpOp ">" (>)
            , liftCmpOp "<" (<)
            , liftCmpOp ">=" (>=)
            , liftCmpOp "<=" (<=)
            , liftCmpOp "==" (==)
            , liftCmpOp "!=" (/=)
            , rneg
            , strEq
            , intToString
            , ioExit
            , println
            ]
    macros = [llvm]
