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

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Emit where

import           LLVM.Context
import           LLVM.Module

import qualified LLVM.AST                        as AST
import qualified LLVM.AST.Constant               as C
import qualified Data.ByteString.UTF8       as BSU
-- import qualified LLVM.AST.Float                  as F

import           Control.Monad
import qualified Data.HashMap.Strict             as H

import           Syntax
import           Syntax.Types
import           Syntax.Prgm
import Eval.Common
import TreeBuild
import CRes
import Text.Printf
-- import Data.Bifunctor
import Data.Hashable
import           Emit.Codegen
import Emit.Runtime (primEnv)
import LLVM.AST.Type (i8, ptr, double, i1, i32)
import qualified LLVM.AST.Float as F
import Data.Char (ord)
import qualified Data.HashSet as S
import Control.Monad.State

data LEnv = LEnv { lvArgs :: H.HashMap ArgName Val
                 , lvTbEnv :: TBEnv EPrim
                 , lvClassMap :: ClassMap
                 }

initModule :: AST.Module
initModule = emptyModule "repl"

asOperand :: Val -> Codegen AST.Operand
asOperand (LLVMOperand _ o) = o
asOperand (IntVal i) = return $ cons $ C.Int 64 i
asOperand (FloatVal f) = return $ cons $ C.Float (F.Double f)
asOperand (StrVal s) = return $ cons $ C.Array i8 $ map (C.Int 8 . toInteger . ord) s
asOperand val = error $ printf "Invalid val to operand: %s" (show val)

-- TODO: Add genType with varEnv
-- TODO: Add genType that is a union of multiple types (with tag)
genType :: H.HashMap TypeVarName Type -> Type -> AST.Type
genType _ t | t == intType = i32
genType _ t | t == boolType = i1
genType _ t | t == floatType = double
genType _ t | t == strType = ptr i8
genType varEnv (SumType leafs) = case splitPartialLeafs leafs of
  [PartialType{ptVars, ptArgs}] -> structType $ H.elems $ fmap (genType (H.union ptVars varEnv)) ptArgs
  _ -> error "genType does not have a single partial"
genType varEnv (TypeVar (TVVar v)) = case H.lookup v varEnv of
  Just t -> genType varEnv t
  Nothing -> error $ printf "Unknown type var in emit genType: %s" (show v)
genType _ TopType = i32 -- TODO: Should compute the top type
genType _ t = error $ printf "Unsupported emit genType: %s" (show t)

genTypeMeta :: EvalMeta -> AST.Type
genTypeMeta (Typed t _) = genType H.empty t

arrowName :: EObject -> EArrow -> String
arrowName (Object _ _ name _ _) arrow = printf "fun:%s-%s" name arrHash
  where arrHash = take 6 (printf "%08x" (hash arrow)) :: String

typeName :: Type -> String
typeName tp = take 6 (printf "%08x" (hash tp))

codegenTree :: LEnv -> ResArrowTree EPrim -> Val
codegenTree env (ResEArrow input object arrow) = do
  let val = codegenTree env input
  -- let outType = resArrowDestType lvClassMap (getValType val) resArrow TODO
  let outType = intType
  case val of
    TupleVal{} ->
      LLVMOperand outType $ do
        let args' = buildArrArgs object val
        mapM_ asOperand $ H.elems args'
        addTaskArrow (object, arrow, True)
        callf (genType H.empty outType) (arrowName object arrow) [cons $ C.Int 32 0]
    _ -> do
      LLVMOperand outType $ do
        _ <- asOperand val
        addTaskArrow (object, arrow, False)
        callf (genType H.empty outType) (arrowName object arrow) [cons $ C.Int 32 0]
codegenTree _ MacroArrow{} = error $ printf "Can't evaluate a macro - it should be removed during TreeBuild"
codegenTree _ ExprArrow{} = error $ printf "Can't evaluate an expr - it should be removed during TreeBuild"
codegenTree env (PrimArrow input outType (EPrim _ _ f)) = do
  let input' = codegenTree env input
  case input' of
    TupleVal _ args -> f args
    LLVMOperand _ o ->
      LLVMOperand outType $ do
          _ <- o
          return $ cons $ C.Int 32 0
    _ -> error $ printf "Unknown input to PrimArrow"
codegenTree _ (ConstantArrow val) = LLVMOperand (singletonType $ getValType val) (asOperand val)
-- codegenTree env@LEnv{lvArgs} (ArgArrow _ name) = case H.lookup name lvArgs of
--   Just arg' -> (arg', env)
  -- Nothing -> error $ printf "Failed to find emit ArgArrow %s" (show name)
codegenTree env@LEnv{lvClassMap} match@(ResArrowMatch m opts) = do
  let matchType = unionTypes lvClassMap $ map singletonType $ H.keys opts
  let matchHashName = "match:" ++ take 6 (printf "%08x" (hash match))
  LLVMOperand matchType $ do
    -- matchResult <- alloca $ genType H.empty matchType TODO
    matchResult <- alloca i32

    -- Create blocks
    exitBlock <- addBlock $ matchHashName ++ "-exit"
    noMatchBlock <- addBlock $ matchHashName ++ "-noMatch"
    opts' <- forM (H.toList opts) $ \opt@(optType, _) -> do
      optBlock <- addBlock $ printf "%s-opt:%s" matchHashName (typeName $ singletonType optType)
      return (optBlock, opt)

    -- End block with switch of input
    m' <- asOperand $ codegenTree env m
    _ <- switch m' noMatchBlock (map ((C.Int 64 0,) . fst) opts')

    -- Build switch bblocks
    forM_ opts' $ \(optBlock, (_, optTree)) -> do
      _ <- setBlock optBlock
      tree' <- asOperand $ codegenTree env optTree
      _ <- store matchResult tree'
      br exitBlock

    -- Build error for no match block
    _ <- setBlock noMatchBlock
    _ <- panic $ printf "Could not complete match %s" matchHashName
    _ <- br exitBlock

    -- Start building exit block with result
    _ <- setBlock exitBlock
    load matchResult
-- codegenTree env val (ResArrowCond [] elseTree) = codegenTree env val elseTree
-- codegenTree env val (ResArrowCond ((ifCondTree, ifThenTree):restIfTrees) elseTree) = do
--   cond' <- codegenTree env val ifCondTree
--   codegenTree env cond' (ResArrowMatch $ H.fromList [
--                           (trueLeaf, ifThenTree),
--                           (falseLeaf, (ResArrowCond restIfTrees elseTree))
--                                                   ])
codegenTree _ (ResArrowTuple name args) | H.null args = TupleVal name H.empty
codegenTree env (ResArrowTuple name args) = do
  let args' = fmap (codegenTree env) args
  TupleVal name args'
codegenTree env (ResArrowTupleApply base argName argRATree) = do
  let base' = codegenTree env base
  let argRATree' = codegenTree env argRATree
  case base' of
    TupleVal name baseArgs -> do
      let args' = H.insert argName argRATree' baseArgs
      TupleVal name args'
    _ -> error "Invalid input to tuple application"
codegenTree _ _ = LLVMOperand intType (return $ cons $ C.Int 64 0)

codegenDecl :: LEnv -> String -> PartialType -> ResArrowTree EPrim -> LLVM ()
codegenDecl env name PartialType{} tree = do
  blks <- createBlocks codegenState
  define retType name args' blks
  where
    -- args' = map(\(argName, argTp) -> (genType H.empty argTp, astName argName)) $ H.toList ptArgs -- TODO
    -- retType = genType H.empty $ singletonType tp
    args' = [(i32, astName "i")]
    retType = i32
    codegenState = execCodegen [] buildBlock
    replaceArgs = [] -- TODO: Use actual args
    env' = env{lvArgs = H.fromList replaceArgs}
    tree' = codegenTree env' tree
    buildBlock = do
      ent <- addBlock entryBlockName
      _ <- setBlock ent
      -- replaceArgs <- forM (H.toList args) $ \(argName, (argM, _)) -> do
      --   var <- alloca (genTypeMeta argM)
      --   _ <- store var (local (AST.Name $ SBS.toShort $ BSU.fromString argName))
      --   assign argName var
      --   return (argName, OVal (getMetaType argM) var)
      case tree' of
        (LLVMOperand _ o) -> do
          _ <- o
          -- ret o'
          ret $ cons $ C.Int 32 0
        TupleVal _ _ -> do
      --     -- TODO: This should build a struct and return it
      --     -- let tp = structType as
          ret $ cons $ C.Int 32 0
      --   IOVal -> ret $ cons $ C.Int 64 0 -- TODO: Delete, this should be an error
        err -> error $ printf "Unexpected return type in codegenDecl: %s" (show err)

codegenDecls :: LEnv -> String -> Type -> ResArrowTree EPrim -> LLVM ()
codegenDecls env name (SumType partialLeafs) tree = case splitPartialLeafs partialLeafs of
  [leaf] -> codegenDecl env name leaf tree
  _ -> error $ printf "CodegenDecls only supports a singleton partial right now"
codegenDecls _ _ _ _ = error $ printf "Invalid input to codegenDecls"

codegenStruct :: EObject -> LLVM ()
codegenStruct (Object objM _ _ _ args) = struct name (map (\(argM, _) -> genTypeMeta argM) $ H.elems args)
  where
    name = astName $ typeName $ getMetaType objM

codegenTasks :: LEnv -> LLVM ()
codegenTasks env@LEnv{lvTbEnv} = do
  taskArrows <- gets lTaskArrows
  completed <- gets lTasksCompleted
  case taskArrows of
    (obj@(Object objM _ _ _ _), arr, _):tas -> do
      modify $ \s -> s {lTaskArrows = tas}
      let nm = arrowName obj arr
      if S.member nm completed
        then codegenTasks env
        else case buildArrow lvTbEnv obj arr of
          CRes _ (Just (_, (tree, _))) -> do
            modify $ \s -> s {lTasksCompleted = S.insert nm completed}
            codegenDecls env nm (getMetaType objM) tree
            codegenTasks env
          _ -> error $ printf "Failed to buildtree to emit arrow"
    [] -> return ()

applyIO :: EExpr -> EExpr
applyIO input@(Value m name) = TupleApply applyMeta (m, input) "io" (Arg (Typed ioType Nothing) "io")
  where applyMeta = Typed (singletonType (PartialType (PTypeName name) H.empty H.empty (H.singleton "io" ioType) PtArgExact)) Nothing
applyIO _ = error "Bad applyIO"

codegenPrgm :: EExpr -> PartialType -> Type -> EPrgm -> LLVM ()
codegenPrgm input srcType destType tprgm@(_, classMap, _) = do
  let tbEnv = buildTBEnv primEnv tprgm
  case buildRoot tbEnv (applyIO input) srcType destType of
    CRes _ initTree -> do
      let env = LEnv H.empty tbEnv classMap
      -- forM_ (H.keys objMap) $ \obj -> do
      --   codegenStruct obj
      -- forM_ (H.toList exEnv) $ \(arrow, (obj, tree, _)) -> do
      --   codegenDecl env (arrowName obj arrow) obj tree
      codegenDecl env "main" srcType initTree
      codegenTasks env
    CErr err -> error $ printf "Build to buildPrgm in codegen: \n\t%s" (show err)

codegenEx :: AST.Module -> LLVM () -> IO String
codegenEx astMod modn = withContext $ \context ->
  withModuleFromAST context newast (fmap BSU.toString . moduleLLVMAssembly)
  where
    newast = runLLVM astMod modn

codegenExInit :: LLVM () -> IO String
codegenExInit = codegenEx initModule
