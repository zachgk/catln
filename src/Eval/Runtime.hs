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
import           CtConstants
import           Data.Maybe          (fromMaybe)
import           Eval.Common
import           Eval.ExprBuilder
import           Text.Printf

type Op = (String, Either EPrim MacroFunction)

true, false :: Val
true = TupleVal truePrim H.empty
false = TupleVal falsePrim H.empty

trueE, falseE :: Expr EvalMetaDat
trueE = Value (emptyMetaT trueType) truePrim
falseE = Value (emptyMetaT falseType) falsePrim

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

arrExists :: Op
arrExists = ("arrExists", Right (MacroFunction macroBuild))
  where
    macroBuild input MacroData{mdTbEnv=TBEnv{tbTypeEnv}} = do
      let args = exprAppliedArgsMap input
      case (H.lookup (partialKey operatorArgL) args, H.lookup (partialKey operatorArgR) args) of
        (Just (Just (l, _)), Just (Just (_r, _))) -> case typeGraphQuery tbTypeEnv H.empty (fromMaybe (error "Non singleton in arrExists") $ maybeGetSingleton $ getMetaType l) of
          [] -> return $ Right falseE
          _  -> return $ Right trueE
        _ -> fail "Invalid arrExists input"

contextMacro :: Op
contextMacro = ("context", Right (MacroFunction macroBuild))
  where
    macroBuild input MacroData{} = do
      let args = exprAppliedArgsMap input
      case H.lookup (partialKey contextValStr) args of
        Just (Just (_, Just val)) -> do
          let contextArgs = filter ((/=) contextValStr . oaObjPath) $ exprAppliedArgs input
          return $ Right $ eVal ContextInStr `eApply` (contextValStr, val) `eApplyOAs` contextArgs
        _ -> fail "Invalid context macro input"

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
    macros = [arrExists, contextMacro, llvm]
