--------------------------------------------------------------------
-- |
-- Module    :  Emit.Common
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Emit.Common where

import Data.Hashable
import Syntax
import Syntax.Prgm
import GHC.Generics
import Syntax.Types
import qualified Data.HashMap.Strict as H
import qualified LLVM.AST as AST
import Codegen

type TypedMeta = Typed
type TExpr = Expr TypedMeta
type TGuard = Guard TExpr
type TObject = Object TypedMeta
type TArrow = Arrow TExpr TypedMeta
type TPrgm = Prgm TExpr TypedMeta
type TReplRes = ReplRes TypedMeta

data LLVMPrim = LLVMPrim PartialType TGuard (H.HashMap String Val -> Codegen Val)
  deriving (Generic)

instance Eq LLVMPrim where
  (LLVMPrim at ag _) == (LLVMPrim bt bg _) = at == bt && ag == bg

instance Hashable LLVMPrim where
  hashWithSalt s (LLVMPrim at ag _) = s `hashWithSalt` at `hashWithSalt` ag

data Val
  = OVal Type AST.Operand
  | TupleVal String Args
  | IOVal
  | NoVal
  deriving (Show)

type Args = H.HashMap String Val
type Env = ClassMap
