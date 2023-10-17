--------------------------------------------------------------------
-- |
-- Module    :  Testing.Generation
-- Copyright :  (c) Zach Kimberg 2023
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines arbitrary code generation for property testing.
-- It helps test both the compiler itself and code written in it.
--------------------------------------------------------------------

module Testing.Generation where

import           Control.Monad
import           Data.Graph          (graphFromEdges)
import qualified Data.HashMap.Strict as H
import           Data.Maybe
import           Hedgehog
import qualified Hedgehog.Gen        as HG
import           Hedgehog.Range      (linear)
import           Semantics           (getExprType, oaObjPath)
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           Utils

genTypeFromExpr :: ExprPrgm Expr () -> Expr () -> Gen PartialType
genTypeFromExpr _ (CExpr _ c) = return $ constantPartialType c
genTypeFromExpr _ (Value _ n) = return $ partialVal $ PTypeName n
genTypeFromExpr prgm (TupleApply _ (_, baseExpr) oa) = do
  base@PartialType{ptArgs=baseArgs} <- genTypeFromExpr prgm baseExpr
  shouldAddArg <- HG.bool
  if shouldAddArg
    then case oaArr oa of
           Just (GuardExpr arrExpr _) -> do
             arrExpr' <- genTypeFromExpr prgm arrExpr
             return base{ptArgs = H.insert (oaObjPath oa) (singletonType arrExpr') baseArgs}
           Nothing -> return base{ptArgs = H.insert (oaObjPath oa) (getMetaType $ oaM oa) baseArgs}
    else return base
genTypeFromExpr prgm (VarApply _ baseExpr varName m) = do
  base@PartialType{ptVars=baseVars} <- genTypeFromExpr prgm baseExpr
  shouldAddVar <- HG.bool
  return $ if shouldAddVar
    then base{ptVars = H.insert varName (getMetaType m) baseVars}
    else base
genTypeFromExpr _ e = error $ printf "Unimplemented genTypeFromExpr for %s" (show e)

genType :: ExprPrgm Expr () -> Gen Type
genType prgm@(objMap, ClassGraph cg, _) = HG.choice gens
  where

    gens = if graphEmpty cg
      then [genBasic]
      else [genBasic, genCGOld, genCG, genCGRel, genObjM, genObj]

    genBasic :: Gen Type
    genBasic = HG.element [topType, bottomType]

    genCGOld :: Gen Type
    genCGOld = do
      classNode <- HG.element $ graphToNodes cg
      return $ typeVal $ snd3 classNode

    genCG :: Gen Type
    genCG = do
      classNode <- HG.element $ graphToNodes cg
      return $ case snd3 classNode of
        cls@PClassName{} -> TopType [PredClass $ partialVal cls]
        t                -> typeVal t

    genCGRel :: Gen Type
    genCGRel = do
      classNode <- HG.element $ graphToNodes cg
      return $ TopType [PredRel $ partialVal $ PRelativeName $ fromPartialName $ snd3 classNode]

    genObjM :: Gen Type
    genObjM = do
      oa <- HG.element objMap
      let (GuardExpr objExpr Nothing) = fromJust $ oaObj oa
      return $ getExprType objExpr

    genObj :: Gen Type
    genObj = do
      oa <- HG.element objMap
      let (GuardExpr objExpr Nothing) = fromJust $ oaObj oa
      singletonType <$> genTypeFromExpr prgm objExpr

genPartialType :: ExprPrgm Expr () -> Gen PartialType
genPartialType prgm@(objMap, ClassGraph cg, _) = do
  gen <- if graphEmpty cg
    then if null objMap
      then HG.discard
      else return [genObj]
    else return [genCG, genObj]
  HG.choice gen
  where
    genCG :: Gen PartialType
    genCG = do
      classNode <- HG.element $ graphToNodes cg
      return $ partialVal $ snd3 classNode

    genObj :: Gen PartialType
    genObj = do
      oa <- HG.element objMap
      let (GuardExpr objExpr Nothing) = fromJust $ oaObj oa
      genTypeFromExpr prgm objExpr

genInputExpr :: Gen (Expr ())
genInputExpr = HG.recursive HG.choice [genValue] [genApply]
  where
    genValue = do
      name <- HG.string (linear 5 10) HG.lower
      return $ Value emptyMetaN name
    genApply = do
      argName <- HG.string (linear 5 10) HG.lower
      base <- genInputExpr
      return $ TupleApply emptyMetaN (emptyMetaN, base) (ObjArr (Just (GuardExpr (Arg emptyMetaN argName) Nothing)) ArgObj Nothing [] emptyMetaN Nothing)

genOutputExpr :: [Expr ()] -> Expr () -> Gen (Expr ())
genOutputExpr vals _input = do
   val <- HG.element vals
   return $ Value emptyMetaN (exprPath val)


genPrgm :: Gen (ExprPrgm Expr ())
genPrgm = do
  dataTypes <- HG.list (linear 1 20) genInputExpr
  let dataObjs = map (\obj -> ObjArr (Just (GuardExpr obj Nothing)) TypeObj Nothing [] emptyMetaN Nothing) dataTypes

  funs <- HG.list (linear 1 20) genInputExpr
  let allInputs = dataTypes ++ funs
  funObjs <- forM funs $ \obj -> do
                                arr <- genOutputExpr allInputs obj
                                return $ ObjArr (Just (GuardExpr obj Nothing)) FunctionObj Nothing [] emptyMetaN (Just (GuardExpr arr Nothing))

  let objMap = dataObjs ++ funObjs

  return (objMap, ClassGraph $ graphFromEdges [], [])

genPrgms :: Gen [GraphNodes (ExprPrgm Expr ()) String]
genPrgms = do
  prgm <- genPrgm
  prgmName <- HG.string (linear 5 10) HG.lower
  return [(prgm, prgmName, [])]
