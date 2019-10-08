--------------------------------------------------------------------
-- |
-- Module    :  Emit
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.Module
import LLVM.Context
import LLVM.Analysis
import LLVM.PassManager

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import LLVM.ExecutionEngine ( withMCJIT, withModuleInEngine, getFunction )

import Data.Maybe
import Data.Word
import Data.Int
import qualified Data.ByteString as BBS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Short as SBS
import Control.Monad.Except
import Control.Applicative
import qualified Data.HashMap.Strict as H

import Codegen
import Syntax
import Compile.Builtins

type TypedMeta = Typed
type TExpr = Expr TypedMeta
type TDecl = Decl TypedMeta
type TDeclLHS = DeclLHS TypedMeta
type TPrgm = Prgm TypedMeta
type TReplRes = ReplRes TypedMeta

toSig :: [SBS.ShortByteString] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

initModule :: AST.Module
initModule = emptyModule "repl"

codegenExpr :: TExpr -> Codegen AST.Operand
codegenExpr (CExpr _ (CInt n)) = return $ cons $ C.Int 64 n
codegenExpr (CExpr _ (CFloat n)) = return $ cons $ C.Float (F.Double n)
codegenExpr (CExpr _ (CStr _)) = error "no string implemented"
codegenExpr (Var _ name) = getvar name >>= load
codegenExpr (Call _ name exprs) = case H.lookup name runtimeOps of
    Just f -> do
      exprs' <- mapM codegenExpr exprs
      let exprTypes = map getExprMeta exprs
      fromMaybe callOther (f $ zip exprTypes exprs')
    Nothing -> callOther
  where callOther = do
          largs <- mapM codegenExpr exprs
          call (externf (AST.Name $ SBS.toShort $ BSU.fromString name)) largs

codegenDecl :: TDecl -> LLVM ()
codegenDecl (Decl (DeclLHS m name args) expr) = define (getType m) (SBS.toShort $ BSU.fromString name) largs bls
  where
    largs = map (\(an, am) -> (getType am, AST.Name $ SBS.toShort $ BSU.fromString an)) args
    bls = createBlocks $ execCodegen [] $ do
      ent <- addBlock (BSU.toString $ SBS.fromShort entryBlockName)
      _ <- setBlock ent
      forM_ args $ \(an, am) -> do
        var <- alloca (getType am)
        _ <- store var (local (AST.Name $ SBS.toShort $ BSU.fromString an))
        assign an var
      codegenExpr expr >>= ret

codegenPrgm :: TPrgm -> LLVM ()
codegenPrgm = mapM_ codegenDecl

codegen :: AST.Module -> TPrgm -> IO BSU.ByteString
codegen astMod prgm = withContext $ \context ->
  withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    -- putStrLn $ BSU.toString llstr
    return llstr
  where
    modn = codegenPrgm prgm
    newast = runLLVM astMod modn
