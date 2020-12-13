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
-- import qualified LLVM.AST.Float                  as F

-- import           Control.Monad.Except
import qualified Data.ByteString.Short           as SBS
import qualified Data.ByteString.UTF8            as BSU
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

type LEnv = ClassMap

toSig :: [SBS.ShortByteString] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

initModule :: AST.Module
initModule = emptyModule "repl"

asOperand :: Val -> Codegen AST.Operand
asOperand (LLVMOperand _ o) = o
asOperand _ = error "Does not have type operand"

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

arrowName :: EArrow -> String
arrowName arrow = take 6 (printf "%08x" (hash arrow))

typeName :: Type -> String
typeName tp = take 6 (printf "%08x" (hash tp))

codegenTree :: LEnv -> Val -> ResArrowTree EPrim -> Codegen Val
codegenTree = undefined
-- codegenTree classMap val resArrow@(ResEArrow _ object arrow) = do
--   let args' = buildArrArgs object val
--   let outType = resArrowDestType classMap (getValType val) resArrow
--   OVal outType <$> call (externf (AST.Name $ SBS.toShort $ BSU.fromString  $ arrowName arrow)) (map asOperand $ H.elems args')
-- codegenTree _ (TupleVal _ args) (PrimArrow _ _ (EPrim _ _ f)) = f args
-- codegenTree _ (LLVMOperand _ _) (PrimArrow _ _ (EPrim _ _ f)) = f H.empty -- TODO: Extract values from OVal which should be a struct
-- codegenTree _ NoVal (PrimArrow _ _ (EPrim _ _ f)) = f H.empty
-- codegenTree _ _ (ConstantArrow (CInt i)) = return $ OVal intType $ cons $ C.Int 64 i
-- codegenTree _ _ (ConstantArrow (CFloat f)) = return $ OVal floatType $ cons $ C.Float (F.Double f)
-- codegenTree _ _ (ConstantArrow (CStr _)) = error "no string implemented"
-- codegenTree _ _ (ArgArrow _ name) = error $ printf "Unexpected arg arrow %s not removed during evaluation" name
-- codegenTree _ val arr = error $ printf "Unknown codegenTree with arrow %s and val %s" (show arr) (show val)
-- codegenTree classMap val (ResArrowMatch opts) = case H.toList $ H.filterWithKey (\optType _ -> hasPartial classMap (getValType val) (singletonType optType)) opts of
--   [(_, resArrowTree)] -> case val of
--     (TupleVal _ arrArgs) ->
--       codegenTree classMap val $ replaceTreeArgs arrArgs resArrowTree
--     _ -> codegenTree classMap val resArrowTree
--   [] -> error $ "Failed match in eval resArrowTree: \n\tOptions: " ++ show opts
--   (_:_:_) -> error $ "Multiple matches in eval resArrowTree: \n\tOptions: " ++ show opts
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

codegenDecl :: LEnv -> String -> PartialType -> ResArrowTree EPrim -> LLVM ()
codegenDecl _ name tp@PartialType{ptArgs} _ = define (genType H.empty $ singletonType tp) (SBS.toShort $ BSU.fromString name) args' blks
  where
    args' = map(\(argName, argTp) -> (genType H.empty argTp, AST.Name $ SBS.toShort $ BSU.fromString argName)) $ H.toList ptArgs
    blks = createBlocks $ execCodegen [] $ do
      ent <- addBlock (BSU.toString $ SBS.fromShort entryBlockName)
      _ <- setBlock ent
      ret $ cons $ C.Int 64 0
      -- replaceArgs <- forM (H.toList args) $ \(argName, (argM, _)) -> do
      --   var <- alloca (genTypeMeta argM)
      --   _ <- store var (local (AST.Name $ SBS.toShort $ BSU.fromString argName))
      --   assign argName var
      --   return (argName, OVal (getMetaType argM) var)
      -- let treeWithoutArgs = replaceTreeArgs (H.fromList replaceArgs) tree
      -- res <- codegenTree env (TupleVal name (H.fromList replaceArgs)) treeWithoutArgs
      -- case res of
      --   OVal _ o -> ret o
      --   TupleVal _ _ -> do
      --     -- TODO: This should build a struct and return it
      --     -- let tp = structType as
      --     ret $ cons $ C.Int 64 0
      --   IOVal -> ret $ cons $ C.Int 64 0 -- TODO: Delete, this should be an error
      --   err -> error $ printf "Bad result in codegenDecl: %s" (show err)

codegenStruct :: EObject -> LLVM ()
codegenStruct (Object objM _ _ _ args) = struct name (map (\(argM, _) -> genTypeMeta argM) $ H.elems args)
  where
    name = AST.Name $ SBS.toShort $ BSU.fromString $ typeName $ getMetaType objM

-- mainObject :: TObject
-- mainObject = Object (Typed $ singletonType mainPartial) FunctionObj "main" H.empty (H.singleton "io" (Typed ioType, Nothing))

applyIO :: EExpr -> EExpr
applyIO input@(Value m name) = TupleApply applyMeta (m, input) "io" (Arg (Typed ioType Nothing) "io")
  where applyMeta = Typed (singletonType (PartialType (PTypeName name) H.empty H.empty (H.singleton "io" ioType) PtArgExact)) Nothing
applyIO _ = error "Bad applyIO"

codegenPrgm :: EExpr -> PartialType -> Type -> EPrgm -> LLVM ()
codegenPrgm input srcType destType tprgm@(_, classMap, _) = do
  let tbEnv = buildTBEnv primEnv tprgm
  case buildRoot tbEnv (applyIO input) srcType destType of
    CRes _ initTree -> do
      let env = classMap
      -- forM_ (H.keys objMap) $ \obj -> do
      --   codegenStruct obj
      -- forM_ (H.toList exEnv) $ \(arrow, (obj, tree, _)) -> do
      --   codegenDecl env (arrowName arrow) obj tree
      codegenDecl env "main" srcType initTree
    CErr err -> error $ printf "Build to buildPrgm in codegen: \n\t%s" (show err)

codegenEx :: AST.Module -> LLVM () -> IO String
codegenEx astMod modn = withContext $ \context ->
  withModuleFromAST context newast (fmap BSU.toString . moduleLLVMAssembly)
  where
    newast = runLLVM astMod modn

codegenExInit :: LLVM () -> IO String
codegenExInit = codegenEx initModule
