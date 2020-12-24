{-# LANGUAGE BlockArguments #-}
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

data LEnv = LEnv { lvArgs :: H.HashMap ArgName Val
                 , lvTbEnv :: TBEnv EPrim
                 , lvClassMap :: ClassMap
                 , lvTaskArrow :: [(Object Typed, Arrow (Expr Typed) Typed, Bool)] -- Bool is true for tuple, false for codegenOperand
                 , lvTasksCompleted :: S.HashSet String
                 }

mergeLEnvs :: Foldable f => f LEnv -> LEnv
mergeLEnvs = foldr1 aux
  where aux (LEnv args tb classMap arrows1 comp1) (LEnv _ _ _ arrows2 comp2) = LEnv args tb classMap (arrows1 ++ arrows2) (S.union comp1 comp2)

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

codegenTree :: LEnv -> ResArrowTree EPrim -> (Val, LEnv)
codegenTree env@LEnv{lvTaskArrow} (ResEArrow input object arrow) = do
  let (val, env2) = codegenTree env input
  -- let outType = resArrowDestType lvClassMap (getValType val) resArrow TODO
  let outType = intType
  case val of
    TupleVal{} ->
      (LLVMOperand outType $ do
        let args' = buildArrArgs object val
        mapM_ asOperand $ H.elems args'
        callf (genType H.empty outType) (arrowName object arrow) [cons $ C.Int 32 0]
       , env2{lvTaskArrow = (object, arrow, True):lvTaskArrow})
    _ -> do
      (LLVMOperand outType $ do
        _ <- asOperand val
        callf (genType H.empty outType) (arrowName object arrow) [cons $ C.Int 32 0]
       , env2{lvTaskArrow = (object, arrow, False):lvTaskArrow})
-- codegenTree _ (TupleVal _ args) (PrimArrow _ _ (EPrim _ _ f)) = f args
-- codegenTree _ (LLVMOperand _ _) (PrimArrow _ _ (EPrim _ _ f)) = f H.empty -- TODO: Extract values from OVal which should be a struct
-- codegenTree _ NoVal (PrimArrow _ _ (EPrim _ _ f)) = f H.empty
codegenTree env (ConstantArrow val) = (LLVMOperand (singletonType $ getValType val) (asOperand val), env)
-- codegenTree _ _ (ArgArrow _ name) = error $ printf "Unexpected arg arrow %s not removed during evaluation" name
-- codegenTree _ val arr = error $ printf "Unknown codegenTree with arrow %s and val %s" (show arr) (show val)
codegenTree env@LEnv{lvClassMap} match@(ResArrowMatch m opts) = do
  let matchType = unionTypes lvClassMap $ map singletonType $ H.keys opts
  let matchHashName = "match:" ++ take 6 (printf "%08x" (hash match))
  let (m', env2) = codegenTree env m
  let (opts', env3s) = unzip $ (\(optType, optTree) -> do
                                        let (optTree', e) = codegenTree env2 optTree
                                        ((optType, optTree'), e)
                                    ) <$> H.toList opts
  let env3 = mergeLEnvs env3s
  (LLVMOperand matchType $ do
    -- matchResult <- alloca $ genType H.empty matchType TODO
    matchResult <- alloca i32

    -- Create blocks
    exitBlock <- addBlock $ matchHashName ++ "-exit"
    noMatchBlock <- addBlock $ matchHashName ++ "-noMatch"
    opts'' <- forM opts' $ \opt@(optType, _) -> do
      optBlock <- addBlock $ printf "%s-opt:%s" matchHashName (typeName $ singletonType optType)
      return (optBlock, opt)

    -- End block with switch of input
    m'' <- asOperand m'
    _ <- switch m'' noMatchBlock (map ((C.Int 64 0,) . fst) opts'')

    -- Build switch bblocks
    forM_ opts'' $ \(optBlock, (_, optTree)) -> do
      _ <- setBlock optBlock
      tree' <- asOperand optTree
      _ <- store matchResult tree'
      br exitBlock

    -- Build error for no match block
    _ <- setBlock noMatchBlock
    _ <- panic $ printf "Could not complete match %s" matchHashName
    _ <- br exitBlock

    -- Start building exit block with result
    _ <- setBlock exitBlock
    load matchResult
    , env3)
-- codegenTree env val (ResArrowCond [] elseTree) = codegenTree env val elseTree
-- codegenTree env val (ResArrowCond ((ifCondTree, ifThenTree):restIfTrees) elseTree) = do
--   cond' <- codegenTree env val ifCondTree
--   codegenTree env cond' (ResArrowMatch $ H.fromList [
--                           (trueLeaf, ifThenTree),
--                           (falseLeaf, (ResArrowCond restIfTrees elseTree))
--                                                   ])
-- codegenTree env val (ResArrowTuple name args) = do
--   args' <- traverse (codegenTree env val) args
--   return $ TupleVal name args'
-- codegenTree env val (ResArrowTupleApply base argName argRATree) = do
--   base' <- codegenTree env val base
--   case base' of
--     TupleVal name baseArgs -> do
--       argVal <- codegenTree env val argRATree
--       let args' = H.insert argName argVal baseArgs
--       return $ TupleVal name args'
--     _ -> error "Invalid input to tuple application"
-- codegenTree _ t | trace (printf "Uncompleted gen: %s" (show t)) False = undefined
codegenTree env _ = (LLVMOperand intType (return $ cons $ C.Int 64 0), env)

codegenDecl :: LEnv -> String -> PartialType -> ResArrowTree EPrim -> LLVM LEnv
codegenDecl env name PartialType{} tree = define retType name args' blks >> return env''
  where
    -- args' = map(\(argName, argTp) -> (genType H.empty argTp, astName argName)) $ H.toList ptArgs -- TODO
    -- retType = genType H.empty $ singletonType tp
    args' = [(i32, astName "i")]
    retType = i32
    blks = createBlocks $ execCodegen [] buildBlock
    replaceArgs = [] -- TODO: Use actual args
    env' = env{lvArgs = H.fromList replaceArgs}
    (tree', env'') = codegenTree env' tree
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
      --   TupleVal _ _ -> do
      --     -- TODO: This should build a struct and return it
      --     -- let tp = structType as
      --     ret $ cons $ C.Int 64 0
      --   IOVal -> ret $ cons $ C.Int 64 0 -- TODO: Delete, this should be an error
        err -> error $ printf "Unexpected return type in codegenDecl: %s" (show err)

codegenDecls :: LEnv -> String -> Type -> ResArrowTree EPrim -> LLVM LEnv
codegenDecls env name (SumType partialLeafs) tree = case splitPartialLeafs partialLeafs of
  [leaf] -> codegenDecl env name leaf tree
  _ -> error $ printf "CodegenDecls only supports a singleton partial right now"
codegenDecls _ _ _ _ = error $ printf "Invalid input to codegenDecls"

codegenStruct :: EObject -> LLVM ()
codegenStruct (Object objM _ _ _ args) = struct name (map (\(argM, _) -> genTypeMeta argM) $ H.elems args)
  where
    name = astName $ typeName $ getMetaType objM

codegenTasks :: LEnv -> LLVM LEnv
codegenTasks env@LEnv{lvTbEnv, lvTaskArrow, lvTasksCompleted} = case lvTaskArrow of
  (obj@(Object objM _ _ _ _), arr, _):tas -> do
    let env' = env{lvTaskArrow=tas}
    let nm = arrowName obj arr
    if S.member nm lvTasksCompleted
      then codegenTasks env'
      else case buildArrow lvTbEnv obj arr of
        CRes _ (Just (_, (tree, _))) -> do
          env'' <- codegenDecls env' nm (getMetaType objM) tree
          codegenTasks env''{lvTasksCompleted = S.insert nm lvTasksCompleted}
        _ -> error $ printf "Failed to buildtree to emit arrow"
  [] -> return env

applyIO :: EExpr -> EExpr
applyIO input@(Value m name) = TupleApply applyMeta (m, input) "io" (Arg (Typed ioType Nothing) "io")
  where applyMeta = Typed (singletonType (PartialType (PTypeName name) H.empty H.empty (H.singleton "io" ioType) PtArgExact)) Nothing
applyIO _ = error "Bad applyIO"

codegenPrgm :: EExpr -> PartialType -> Type -> EPrgm -> LLVM ()
codegenPrgm input srcType destType tprgm@(_, classMap, _) = do
  let tbEnv = buildTBEnv primEnv tprgm
  case buildRoot tbEnv (applyIO input) srcType destType of
    CRes _ initTree -> do
      let env = LEnv H.empty tbEnv classMap [] S.empty
      -- forM_ (H.keys objMap) $ \obj -> do
      --   codegenStruct obj
      -- forM_ (H.toList exEnv) $ \(arrow, (obj, tree, _)) -> do
      --   codegenDecl env (arrowName obj arrow) obj tree
      env' <- codegenDecl env "main" srcType initTree
      _ <- codegenTasks env'
      return ()
    CErr err -> error $ printf "Build to buildPrgm in codegen: \n\t%s" (show err)

codegenEx :: AST.Module -> LLVM () -> IO String
codegenEx astMod modn = withContext $ \context ->
  withModuleFromAST context newast (fmap BSU.toString . moduleLLVMAssembly)
  where
    newast = runLLVM astMod modn

codegenExInit :: LLVM () -> IO String
codegenExInit = codegenEx initModule
