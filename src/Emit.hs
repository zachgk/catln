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
import qualified Data.Text.Lazy as T
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
import Emit.Runtime (primEnv, genType)
import LLVM.AST.Type (i32, i8)
import qualified LLVM.AST.Float as F
import Data.Char (ord)
import qualified Data.HashSet as S
import Control.Monad.State
import qualified LLVM.AST.Typed as ASTT
import LLVM.Pretty (ppllvm)

data LEnv = LEnv { lvArgs :: H.HashMap ArgName Val
                 , lvTbEnv :: TBEnv EPrim
                 , lvClassMap :: ClassMap
                 }

initModule :: AST.Module
initModule = emptyModule "repl"

asOperand :: Val -> Codegen AST.Operand
asOperand (LLVMOperand _ o) = o
asOperand (IntVal i) = return $ cons $ C.Int 32 i
asOperand (FloatVal f) = return $ cons $ C.Float (F.Double f)
asOperand (StrVal s) = return $ cons $ C.Array i8 $ map (C.Int 8 . toInteger . ord) s
asOperand val@(TupleVal _ args) = do
  let tp = singletonType $ getValType val
  tp' <- genType H.empty tp
  res <- alloca tp'
  -- let tpDef = global tp' (typeName tp)
  forM_ (zip [0..] $ H.toList args) \(argIndex, (_, argVal)) -> do
    argVal' <- asOperand argVal
    argPntr <- getelementptr (ASTT.typeOf argVal') res [res, cons $ C.Int 32 argIndex]
    store argPntr argVal'
  return res
asOperand val = error $ printf "Invalid val to operand: %s" (show val)

genTypeMeta :: (Monad m, TaskState m) => EvalMeta -> m AST.Type
genTypeMeta (Typed t _) = genType H.empty t

arrowName :: PartialType -> EObject -> EArrow -> String
arrowName srcType (Object _ _ name _ _) arrow = printf "fun:%s-%s" name arrHash
  where arrHash = take 6 (printf "%08x" (hash (srcType, arrow))) :: String

typeName :: Type -> String
typeName tp = printf "tp_%s" tpHash
  where tpHash = take 6 (printf "%08x" (hash tp)) :: String

codegenTree :: LEnv -> ResArrowTree EPrim -> Val
codegenTree env@LEnv{lvClassMap} resArrow@(ResEArrow input object arrow) = do
  let val = codegenTree env input
  let arrowSrcType = getValType val
  let outType = resArrowDestType lvClassMap arrowSrcType resArrow
  case val of
    TupleVal{} ->
      LLVMOperand outType $ do
        let args' = buildArrArgs object val
        args'' <- mapM asOperand $ H.elems args'
        addTaskArrow (arrowSrcType, object, arrow, TupleInput)
        outType' <- genType H.empty outType
        callf outType' (arrowName arrowSrcType object arrow) args''
    _ -> do
      LLVMOperand outType $ do
        val' <- asOperand val
        addTaskArrow (arrowSrcType, object, arrow, StructInput)
        outType' <- genType H.empty outType
        callf outType' (arrowName arrowSrcType object arrow) [val']
codegenTree _ MacroArrow{} = error $ printf "Can't evaluate a macro - it should be removed during TreeBuild"
codegenTree _ ExprArrow{} = error $ printf "Can't evaluate an expr - it should be removed during TreeBuild"
codegenTree env (PrimArrow input outType (EPrim _ _ f)) = do
  let input' = codegenTree env input
  case input' of
    TupleVal _ args -> f args
    LLVMOperand _ o ->
      LLVMOperand outType o
    _ -> error $ printf "Unknown input to PrimArrow"
codegenTree _ (ConstantArrow val) = LLVMOperand (singletonType $ getValType val) (asOperand val)
codegenTree LEnv{lvArgs} (ArgArrow _ name) = case H.lookup name lvArgs of
  Just arg' -> arg'
  Nothing -> error $ printf "Failed to find emit ArgArrow %s" (show name)
codegenTree env match@(ResArrowMatch m matchType opts) = do
  let matchHashName = "match:" ++ take 6 (printf "%08x" (hash match))
  LLVMOperand matchType $ do
    matchType' <- genType H.empty matchType
    matchResult <- alloca matchType'

    -- Create blocks
    exitBlock <- addBlock $ matchHashName ++ "-exit"
    noMatchBlock <- addBlock $ matchHashName ++ "-noMatch"
    opts' <- forM (H.toList opts) $ \opt@(optType, _) -> do
      optBlock <- addBlock $ printf "%s-opt:%s" matchHashName (typeName $ singletonType optType)
      return (optBlock, opt)

    -- End block with switch of input
    m' <- asOperand $ codegenTree env m
    _ <- switch m' noMatchBlock (map ((C.Int 32 0,) . fst) opts')

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
      resType' <- genType H.empty resType
      result <- alloca resType'

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

getValArgs :: Val -> Codegen (H.HashMap ArgName Val)
getValArgs (TupleVal _ args) = return args
getValArgs (LLVMOperand tp o) = do
  o' <- o
  case tp of
    SumType partialLeafs -> case splitPartialLeafs partialLeafs of
      [PartialType{ptArgs, ptVars}] -> do
        args' <- forM (zip [0..] $ H.toList ptArgs) $ \(argIndex, (argName, argType)) -> do
          argType' <- genType ptVars argType
          argPntr <- getelementptr argType' o' [o', cons $ C.Int 32 argIndex]
          argVal <- load argPntr
          return (argName, LLVMOperand argType (return argVal))
        return $ H.fromList args'
      _ -> error $ printf "Invalid leaf number" (show tp)
    t -> error $ printf "Invalid operand type: %s" (show t)
getValArgs v = error $ printf "Val does not have args: %s" (show v)

formArgValMap :: EObject -> Val -> Codegen (H.HashMap ArgName (Typed, Val))
formArgValMap (Object m _ name _ args) val | H.null args = return $ H.singleton name (m, val)
formArgValMap (Object _ _ _ _ args) val = do
  valArgs <- getValArgs val
  args' <- mapM (fromArg valArgs) $ H.toList args
  return $ unionsWith (error "Duplicate var matched") args'
  where
    fromArg valArgs (argName, (m, Nothing)) = case H.lookup argName valArgs of
      Just valArg -> return $ H.singleton argName (m, valArg)
      Nothing -> return H.empty
    fromArg valArgs (argName, (_, Just arg)) = case H.lookup argName valArgs of
      Just valArg -> formArgValMap arg valArg
      Nothing -> return H.empty

codegenDecl :: LEnv -> String -> EObject -> PartialType -> Type -> ResArrowTree EPrim -> DeclInput -> LLVM ()
codegenDecl env name obj srcType@PartialType{ptArgs, ptVars} destType tree declInput = do
  args' <- case declInput of
    TupleInput -> do
      forM (H.toList ptArgs) $ \(argName, argTp) -> do
        argTp' <- genType ptVars argTp
        return (argTp', astName argName)
    StructInput -> do
      tp' <- genType H.empty (singletonType srcType)
      return [(tp', astName "_i")]
  destType' <- genType H.empty destType
  let codegenState = execCodegen [] buildBlock
  blks <- createBlocks codegenState
  define destType' name args' blks
  where
    buildBlock = do
      ent <- addBlock entryBlockName
      _ <- setBlock ent
      argVals <- case declInput of
            TupleInput -> do
              args' <- forM (H.toList ptArgs) $ \(argName, argTp) -> do
                argTp' <- genType ptVars argTp
                let argOp = local argTp' (astName argName)
                let argVal = LLVMOperand argTp (return argOp)
                return (argName, argVal)
              return $ TupleVal name (H.fromList args')
            StructInput -> return $ LLVMOperand (singletonType srcType) $ do
              srcType' <- genType H.empty (singletonType srcType)
              return $ local srcType' (astName "_i")
      replaceArgs <- fmap snd <$> formArgValMap obj argVals
      let env' = env{lvArgs = replaceArgs}
      res <- asOperand $ codegenTree env' tree
      ret res

codegenMain :: LEnv -> ResArrowTree EPrim -> LLVM ()
codegenMain env tree = do
  let codegenState = execCodegen [] buildBlock
  blks <- createBlocks codegenState
  define i32 "main" [] blks
  where
    buildBlock = do
      ent <- addBlock entryBlockName
      _ <- setBlock ent

      ioType' <- genType H.empty ioType
      initIO <- alloca ioType'
      let replaceArgs = [("io", LLVMOperand ioType (return initIO))]

      let env' = env{lvArgs = H.fromList replaceArgs}
      res <- asOperand $ codegenTree env' tree
      ret res

codegenDecls :: LEnv -> String -> EObject -> Type -> Type -> ResArrowTree EPrim -> DeclInput -> LLVM ()
codegenDecls env name obj (SumType partialLeafs) destType tree declInput = case splitPartialLeafs partialLeafs of
  [leaf] -> codegenDecl env name obj leaf destType tree declInput
  _ -> error $ printf "CodegenDecls only supports a singleton partial right now"
codegenDecls _ _ _ _ _ _ _ = error $ printf "Invalid input to codegenDecls"

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
codegenTasks env@LEnv{lvTbEnv, lvClassMap} = do
  taskArrows <- gets lTaskArrows
  completed <- gets lTasksCompleted
  case taskArrows of
    (arrowSrcType, obj, arr, declInput):tas -> do
      modify $ \s -> s {lTaskArrows = tas}
      let nm = arrowName arrowSrcType obj arr
      if S.member nm completed
        then codegenTasks env
        else case buildArrow lvTbEnv arrowSrcType obj arr of
          CRes _ (Just (_, (tree, _))) -> do
            modify $ \s -> s {lTasksCompleted = S.insert nm completed}
            let destType = arrowDestType False lvClassMap arrowSrcType obj arr
            codegenDecls env nm obj (singletonType arrowSrcType) destType tree declInput
            codegenTasks env
          err -> error $ printf "Failed to buildtree to emit arrow: %s" (show err)
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
      codegenMain env initTree
      codegenTasks env
    CErr err -> error $ printf "Build to buildPrgm in codegen: \n\t%s" (show err)

codegenExPrint :: AST.Module -> LLVM () -> String
codegenExPrint astMod modn = T.unpack $ ppllvm newast
  where
    newast = runLLVM astMod modn

codegenExAPI :: AST.Module -> LLVM () -> IO String
codegenExAPI astMod modn = withContext $ \context ->
  withModuleFromAST context newast (fmap BSU.toString . moduleLLVMAssembly)
  where
    newast = runLLVM astMod modn

codegenEx :: AST.Module -> LLVM () -> IO String
-- codegenEx astMod modn = return $ codegenExPrint astMod modn
codegenEx = codegenExAPI

codegenExInit :: LLVM () -> IO String
codegenExInit = codegenEx initModule
