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
asOperand val@(TupleVal _ args) = do
  let tp = singletonType $ getValType val
  tp' <- genType H.empty tp
  res <- alloca tp'
  -- let tpDef = global tp' (typeName tp)
  forM_ (zip [0..] $ H.toList args) \(argIndx, (_, argVal)) -> do
    argPntr <- getelementptr res [res, cons $ C.Int 32 argIndx]
    argVal' <- asOperand argVal
    store argPntr argVal'
  return res
asOperand val = error $ printf "Invalid val to operand: %s" (show val)

-- TODO: Add genType with varEnv
-- TODO: Add genType that is a union of multiple types (with tag)
genType :: (Monad m, TaskState m) => H.HashMap TypeVarName Type -> Type -> m AST.Type
genType _ t | t == intType = return i32
genType _ t | t == boolType = return i1
genType _ t | t == floatType = return double
genType _ t | t == strType = return $ ptr i8
genType varEnv tp@(SumType leafs) = case splitPartialLeafs leafs of
  [PartialType{ptVars, ptArgs}] -> do
    addTaskStruct tp
    let varEnv' = H.union ptVars varEnv
    args' <- mapM (genType varEnv') ptArgs
    return $ structType $ H.elems args'
  _ -> error "genType does not have a single partial"
genType varEnv (TypeVar (TVVar v)) = case H.lookup v varEnv of
  Just t -> genType varEnv t
  Nothing -> error $ printf "Unknown type var in emit genType: %s" (show v)
genType _ TopType = return i32 -- TODO: Should compute the top type
genType _ t = error $ printf "Unsupported emit genType: %s" (show t)

genTypeMeta :: (Monad m, TaskState m) => EvalMeta -> m AST.Type
genTypeMeta (Typed t _) = genType H.empty t

arrowName :: EObject -> EArrow -> String
arrowName (Object _ _ name _ _) arrow = printf "fun:%s-%s" name arrHash
  where arrHash = take 6 (printf "%08x" (hash arrow)) :: String

typeName :: Type -> String
typeName tp = printf "tp_%s" tpHash
  where tpHash = take 6 (printf "%08x" (hash tp)) :: String

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
        addTaskArrow (object, arrow, TupleInput)
        outType' <- genType H.empty outType
        callf outType' (arrowName object arrow) [cons $ C.Int 32 0]
    _ -> do
      LLVMOperand outType $ do
        _ <- asOperand val
        addTaskArrow (object, arrow, StructInput)
        outType' <- genType H.empty outType
        callf outType' (arrowName object arrow) [cons $ C.Int 32 0]
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
codegenTree env match@(ResArrowMatch m matchType opts) = do
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
codegenTree env (ResArrowCond _ [] elseTree) = codegenTree env elseTree
codegenTree env cond@(ResArrowCond resType (((ifCondTree, ifCondInput, ifObj), ifThenTree):restIfTrees) elseTree) = do
  let condHashName = "cond:" ++ take 6 (printf "%08x" (hash cond))
  let ifCondInput' = codegenTree env ifCondInput
  let envCond = env{lvArgs=buildArrArgs ifObj ifCondInput'}
  let cond' = codegenTree envCond ifCondTree
  let ifThenTree' = codegenTree env ifThenTree
  let rest' = codegenTree env (ResArrowCond resType restIfTrees elseTree)
  LLVMOperand resType $ do
      -- result <- alloca $ genType H.empty matchType TODO
      result <- alloca i32

      thenBlock <- addBlock $ condHashName ++ "-then"
      elseBlock <- addBlock $ condHashName ++ "-else"
      exitBlock <- addBlock $ condHashName ++ "-exit"

      -- Branch on condition
      cond'' <- asOperand cond'
      _ <- cbr cond'' thenBlock elseBlock

      -- True condition
      _ <- setBlock thenBlock
      thenRes <- asOperand ifThenTree'
      store result thenRes
      _ <- br exitBlock

      -- False / rest of tree
      _ <- setBlock elseBlock
      elseRes <- asOperand rest'
      store result elseRes
      _ <- br exitBlock

      -- Exit after branch
      _ <- setBlock exitBlock
      load result

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

codegenDecl :: LEnv -> String -> PartialType -> ResArrowTree EPrim -> DeclInput -> LLVM ()
codegenDecl env name tp@PartialType{ptArgs, ptVars} tree declInput = do
  _ <- case declInput of -- TODO Use as args'
    TupleInput -> do
      forM (H.toList ptArgs) $ \(argName, argTp) -> do
        argTp' <- genType ptVars argTp
        return (argTp', astName argName)
    StructInput -> do
      tp' <- genType H.empty (singletonType tp)
      return [(tp', astName "_i")]
  _ <- genType H.empty $ singletonType tp -- TODO Use as retType
  let args' = [(i32, astName "i")]
  let retType = i32
  let codegenState = execCodegen [] buildBlock
  blks <- createBlocks codegenState
  define retType name args' blks
  where
    buildBlock = do
      ent <- addBlock entryBlockName
      _ <- setBlock ent
      let replaceArgs = [] -- TODO: Use actual replaceArgs below
      -- replaceArgs <- forM (H.toList args) $ \(argName, (argM, _)) -> do
      --   var <- alloca (genTypeMeta argM)
      --   _ <- store var (local (AST.Name $ SBS.toShort $ BSU.fromString argName))
      --   assign argName var
      --   return (argName, OVal (getMetaType argM) var)
      let env' = env{lvArgs = H.fromList replaceArgs}
      _ <- asOperand $ codegenTree env' tree -- TODO use as return value
      ret $ cons $ C.Int 32 0

codegenDecls :: LEnv -> String -> Type -> ResArrowTree EPrim -> DeclInput -> LLVM ()
codegenDecls env name (SumType partialLeafs) tree declInput = case splitPartialLeafs partialLeafs of
  [leaf] -> codegenDecl env name leaf tree declInput
  _ -> error $ printf "CodegenDecls only supports a singleton partial right now"
codegenDecls _ _ _ _ _ = error $ printf "Invalid input to codegenDecls"

codegenStruct :: Type -> LLVM ()
codegenStruct tp@(SumType partialLeafs) = do
  case splitPartialLeafs partialLeafs of
    [PartialType{ptArgs, ptVars}] -> do
      args' <- mapM (genType ptVars) $ H.elems ptArgs
      struct structName args'
    _ -> error $ printf "Invalid type count to codegenStruct: %s" (show tp)
  where
    structName = astName $ typeName tp
codegenStruct tp = error $ printf "Invalid type to codegenStruct: %s" (show tp)

codegenTasks :: LEnv -> LLVM ()
codegenTasks env@LEnv{lvTbEnv} = do
  taskArrows <- gets lTaskArrows
  completed <- gets lTasksCompleted
  case taskArrows of
    (obj@(Object objM _ _ _ _), arr, declInput):tas -> do
      modify $ \s -> s {lTaskArrows = tas}
      let nm = arrowName obj arr
      if S.member nm completed
        then codegenTasks env
        else case buildArrow lvTbEnv obj arr of
          CRes _ (Just (_, (tree, _))) -> do
            modify $ \s -> s {lTasksCompleted = S.insert nm completed}
            codegenDecls env nm (getMetaType objM) tree declInput
            codegenTasks env
          _ -> error $ printf "Failed to buildtree to emit arrow"
    [] -> do
      taskStructs <- gets lTaskStructs
      case taskStructs of
        str:strs -> do
          modify $ \s -> s {lTaskStructs = strs}
          let nm = typeName str
          if S.member nm completed
            then codegenTasks env
            else do
              modify $ \s -> s {lTasksCompleted = S.insert nm completed}
              codegenStruct str
              codegenTasks env
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
      codegenDecl env "main" srcType initTree TupleInput
      codegenTasks env
    CErr err -> error $ printf "Build to buildPrgm in codegen: \n\t%s" (show err)

codegenEx :: AST.Module -> LLVM () -> IO String
codegenEx astMod modn = withContext $ \context ->
  withModuleFromAST context newast (fmap BSU.toString . moduleLLVMAssembly)
  where
    newast = runLLVM astMod modn

codegenExInit :: LLVM () -> IO String
codegenExInit = codegenEx initModule
