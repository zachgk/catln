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

import           LLVM.Context
import           LLVM.Module

import qualified LLVM.AST                        as AST
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.Float                  as F

import           Control.Monad.Except
import qualified Data.ByteString.Short           as SBS
import qualified Data.ByteString.UTF8            as BSU
import qualified Data.HashMap.Strict             as H

import           Codegen
import           Syntax
import           Syntax.Types
import           Syntax.Prgm
import TreeBuild
import CRes
import Text.Printf
import Data.Bifunctor
import Emit.Common
import Data.Hashable
import Emit.Runtime (primEnv)
import LLVM.AST.Type (i8, ptr, double, i1, i32)

getValType :: Val -> PartialType
getValType (OVal t _) = case t of
  SumType leafs -> case splitPartialLeafs leafs of
    [partial] -> partial
    _ -> error "could not getValType without a single partial"
  _ -> error "could not get non sum getValType"
getValType (TupleVal name args) = (PTypeName name, H.empty, H.empty, fmap fromArg args)
  where fromArg arg = singletonType $ getValType arg
getValType IOVal{} = ioLeaf
getValType NoVal = error "getValType of NoVal"

toSig :: [SBS.ShortByteString] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

initModule :: AST.Module
initModule = emptyModule "repl"

buildArrArgs :: TObject -> Val -> Args
buildArrArgs = aux H.empty
  where
    aux acc (Object _ _ objName _ objArgs) val | H.null objArgs = H.insert objName val acc
    aux _ (Object _ _ objName _ _) (TupleVal tupleName _) | objName /= tupleName = error $ printf "Found name mismatch in buildArrArgs: object %s and tuple %s" objName tupleName
    aux acc (Object _ _ _ _ objArgs) (TupleVal _ tupleArgs) = H.foldrWithKey addArgs acc $ H.intersectionWith (,) objArgs tupleArgs
    aux _ _ _ = error $ "Invalid buildArrArgs value"
    addArgs argName ((_, Nothing), argVal) acc = H.insert argName argVal acc
    addArgs _ ((_, Just subObj), argVal) acc = aux acc subObj argVal

replaceTreeArgs :: Args -> ResArrowTree LLVMPrim -> ResArrowTree LLVMPrim
replaceTreeArgs args (ResArrowCompose a b) = ResArrowCompose (replaceTreeArgs args a) (replaceTreeArgs args b)
replaceTreeArgs _ (ResArrowMatch m) = ResArrowMatch m -- Match does not propagate reachesTreeArgs to subtree as the match introduces it's own args later
replaceTreeArgs args (ResArrowCond ifThens els) = ResArrowCond (map (bimap (replaceTreeArgs args) (replaceTreeArgs args)) ifThens) (replaceTreeArgs args els)
replaceTreeArgs args (ResArrowTuple n vs) = ResArrowTuple n (fmap (replaceTreeArgs args) vs)
replaceTreeArgs args (ResArrowTupleApply b an av) = ResArrowTupleApply (replaceTreeArgs args b) an (replaceTreeArgs args av)
replaceTreeArgs args a@(ResArrowSingle (ArgArrow tp name)) = case H.lookup name args of
  Just arg -> ResArrowSingle $ PrimArrow tp $ LLVMPrim (getValType arg) NoGuard (const $ pure arg)
  Nothing -> a
replaceTreeArgs _ (ResArrowSingle a) = ResArrowSingle a
replaceTreeArgs _ ResArrowID = ResArrowID

asOperand :: Val -> AST.Operand
asOperand (OVal _ o) = o
asOperand _ = error "Does not have type operand"

-- TODO: Add genType with varEnv
-- TODO: Add genType that is a union of multiple types (with tag)
genType :: H.HashMap TypeVarName Type -> Type -> AST.Type
genType _ t | t == intType = i32
genType _ t | t == boolType = i1
genType _ t | t == floatType = double
genType _ t | t == strType = ptr i8
genType varEnv (SumType leafs) = case splitPartialLeafs leafs of
  [(_, partialVars, _, partialArgs)] -> structType $ H.elems $ fmap (genType (H.union partialVars varEnv)) partialArgs
  _ -> error "genType does not have a single partial"
genType varEnv (TypeVar (TVVar v)) = case H.lookup v varEnv of
  Just t -> genType varEnv t
  Nothing -> error $ printf "Unknown type var in emit genType: %s" (show v)
genType _ t = error $ printf "Unsupported emit genType: %s" (show t)

genTypeMeta :: TypedMeta -> AST.Type
genTypeMeta (Typed t) = genType H.empty t

arrowName :: TArrow -> String
arrowName arrow = take 6 (printf "%08x" (hash arrow))

typeName :: Type -> String
typeName tp = take 6 (printf "%08x" (hash tp))

codegenArrow :: Env -> Val -> ResArrow LLVMPrim -> Codegen Val
codegenArrow classMap val resArrow@(ResEArrow object arrow) = do
  let args' = buildArrArgs object val
  let outType = resArrowDestType classMap (getValType val) resArrow
  OVal outType <$> call (externf (AST.Name $ SBS.toShort $ BSU.fromString  $ arrowName arrow)) (map asOperand $ H.elems args')
codegenArrow _ (TupleVal _ args) (PrimArrow _ (LLVMPrim _ _ f)) = f args
codegenArrow _ (OVal _ _) (PrimArrow _ (LLVMPrim _ _ f)) = f H.empty -- TODO: Extract values from OVal which should be a struct
codegenArrow _ NoVal (PrimArrow _ (LLVMPrim _ _ f)) = f H.empty
codegenArrow _ _ (ConstantArrow (CInt i)) = return $ OVal intType $ cons $ C.Int 64 i
codegenArrow _ _ (ConstantArrow (CFloat f)) = return $ OVal floatType $ cons $ C.Float (F.Double f)
codegenArrow _ _ (ConstantArrow (CStr _)) = error "no string implemented"
codegenArrow _ _ (ArgArrow _ name) = error $ printf "Unexpected arg arrow %s not removed during evaluation" name
codegenArrow _ val arr = error $ printf "Unknown codegenArrow with arrow %s and val %s" (show arr) (show val)

codegenTree :: Env -> Val -> ResArrowTree LLVMPrim -> Codegen Val
codegenTree env val (ResArrowCompose t1 t2) = do
  val' <- codegenTree env val t1
  codegenTree env val' t2
codegenTree classMap val (ResArrowMatch opts) = case H.toList $ H.filterWithKey (\optType _ -> hasPartial classMap (getValType val) (singletonType optType)) opts of
  [(_, resArrowTree)] -> case val of
    (TupleVal _ arrArgs) ->
      codegenTree classMap val $ replaceTreeArgs arrArgs resArrowTree
    _ -> codegenTree classMap val resArrowTree
  [] -> error $ "Failed match in eval resArrowTree: \n\tOptions: " ++ show opts
  (_:_:_) -> error $ "Multiple matches in eval resArrowTree: \n\tOptions: " ++ show opts
codegenTree env val (ResArrowCond [] elseTree) = codegenTree env val elseTree
codegenTree env val (ResArrowCond ((ifCondTree, ifThenTree):restIfTrees) elseTree) = do
  cond' <- codegenTree env val ifCondTree
  codegenTree env cond' (ResArrowMatch $ H.fromList [
                          (trueLeaf, ifThenTree),
                          (falseLeaf, (ResArrowCond restIfTrees elseTree))
                                                  ])
codegenTree env val (ResArrowTuple name args) = do
  args' <- traverse (codegenTree env val) args
  return $ TupleVal name args'
codegenTree env val (ResArrowTupleApply base argName argRATree) = do
  base' <- codegenTree env val base
  case base' of
    TupleVal name baseArgs -> do
      argVal <- codegenTree env val argRATree
      let args' = H.insert argName argVal baseArgs
      return $ TupleVal name args'
    _ -> error "Invalid input to tuple application"
codegenTree env val (ResArrowSingle r) = codegenArrow env val r
codegenTree _ val ResArrowID = return val

codegenDecl :: Env -> String -> TObject -> ResArrowTree LLVMPrim -> LLVM ()
codegenDecl env name (Object objM _ _ _ args) tree = define (genTypeMeta objM) (SBS.toShort $ BSU.fromString name) args' blks
  where
    args' = map(\(argName, (argM, _)) -> (genTypeMeta argM, AST.Name $ SBS.toShort $ BSU.fromString argName)) $ H.toList args
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

codegenStruct :: TObject -> LLVM ()
codegenStruct (Object objM _ _ _ args) = struct name (map (\(argM, _) -> genTypeMeta argM) $ H.elems args)
  where
    name = (AST.Name $ SBS.toShort $ BSU.fromString $ typeName $ getMetaType objM)

mainPartial :: PartialType
mainPartial = (PTypeName "main", H.empty, H.empty, H.singleton "io" ioType)

mainObject :: TObject
mainObject = Object (Typed $ singletonType mainPartial) FunctionObj "main" H.empty (H.singleton "io" (Typed ioType, Nothing))

codegenPrgm :: TPrgm -> LLVM ()
codegenPrgm tprgm@(objMap, classMap) = case buildPrgm primEnv mainPartial ioType tprgm of
  CRes _ (rootTree, exEnv) -> do
    let env = classMap
    forM_ (H.keys objMap) $ \obj -> do
      codegenStruct obj
    forM_ (H.toList exEnv) $ \(arrow, (obj, tree, _)) -> do
      codegenDecl env (arrowName arrow) obj tree
    codegenDecl env "main" mainObject rootTree
  CErr err -> error $ printf "Build to buildPrgm in codegen: \n\t%s" (show err)

codegen :: AST.Module -> TPrgm -> IO BSU.ByteString
codegen astMod prgm = withContext $ \context ->
  withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    -- putStrLn $ BSU.toString llstr
    return llstr
  where
    modn = codegenPrgm prgm
    newast = runLLVM astMod modn
