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
import           Data.Bifunctor          (bimap)
import           Data.Either             (partitionEithers)
import qualified Data.HashMap.Strict     as H
import           Data.List               (partition)
import           Data.Maybe
import qualified Data.Set                as S
import           Hedgehog
import qualified Hedgehog.Gen            as HG
import           Hedgehog.Range          (linear, singleton)
import           Semantics               (classGraphFromObjs)
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Desugarf.Expr (desFileImport)
import           Syntax.Ct.Prgm          (RawExpr (RawCExpr), mkRawFileImport)
import           Text.Printf
import           Utils

genTypeFromExpr :: Prgm Expr () -> Expr () -> Gen PartialType
genTypeFromExpr _ (CExpr _ c) = return $ constantPartialType c
genTypeFromExpr _ (Value _ n) = return $ partialVal n
genTypeFromExpr prgm (TupleApply _ (_, baseExpr) (EAppArg oa)) = do
  base@PartialType{ptArgs=baseArgs} <- genTypeFromExpr prgm baseExpr
  shouldAddArg <- HG.bool
  if shouldAddArg
    then case oaArr oa of
           (Just (Just arrExpr, _)) -> do
             arrExpr' <- genTypeFromExpr prgm arrExpr
             return base{ptArgs = H.insert (inExprSingleton $ oaObjExpr oa) (singletonType arrExpr') baseArgs}
           (Just (Nothing, oaM)) -> return base{ptArgs = H.insert (inExprSingleton $ oaObjExpr oa) (getMetaType oaM) baseArgs}
           Nothing -> return base
    else return base
genTypeFromExpr prgm (TupleApply _ (_, baseExpr) (EAppVar varName m)) = do
  base@PartialType{ptVars=baseVars} <- genTypeFromExpr prgm baseExpr
  shouldAddVar <- HG.bool
  return $ if shouldAddVar
    then base{ptVars = H.insert varName (getMetaType m) baseVars}
    else base
genTypeFromExpr _ e = error $ printf "Unimplemented genTypeFromExpr for %s" (show e)

genType :: Prgm Expr () -> Gen Type
genType prgm@(Prgm objMap (ClassGraph cg) _) = HG.choice gens
  where

    gens = if graphEmpty cg
      then [genBasic]
      else [genBasic, genCGOld, genCG, genCGRel, genObjM, genObj]

    genBasic :: Gen Type
    genBasic = HG.element [PTopType, BottomType]

    genCGOld :: Gen Type
    genCGOld = do
      classNode <- HG.element $ graphToNodes cg
      return $ typeVal $ fromPartialName $ snd3 classNode

    genCG :: Gen Type
    genCG = do
      classNode <- HG.element $ graphToNodes cg
      return $ case snd3 classNode of
        cls@PClassName{} -> TopType H.empty (PredsOne $ PredClass $ partialVal $ fromPartialName cls)
        t                -> typeVal $ fromPartialName t

    genCGRel :: Gen Type
    genCGRel = do
      classNode <- HG.element $ graphToNodes cg
      return $ relTypeVal $ fromPartialName $ snd3 classNode

    genObjM :: Gen Type
    genObjM = do
      oa <- HG.element objMap
      let objExpr = fromJust $ oaObj oa
      return $ getExprType objExpr

    genObj :: Gen Type
    genObj = do
      oa <- HG.element objMap
      let objExpr = fromJust $ oaObj oa
      singletonType <$> genTypeFromExpr prgm objExpr

genPartialType :: Prgm Expr () -> Gen PartialType
genPartialType prgm@(Prgm objMap (ClassGraph cg) _) = do
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
      return $ partialVal $ fromPartialName $ snd3 classNode

    genObj :: Gen PartialType
    genObj = do
      oa <- HG.element objMap
      let objExpr = fromJust $ oaObj oa
      genTypeFromExpr prgm objExpr

genInputExpr :: Gen (Expr ())
genInputExpr = do
  n <- makeAbsoluteName <$> genName
  varArgNames <- HG.set (linear 0 5) genName
  varOrArg <- HG.list (singleton $ S.size varArgNames) HG.bool
  let (varNames, argNames) = bimap (map fst) (map fst) $ partition snd $ zip (S.toList varArgNames) varOrArg
  let val = Value (emptyMetaT $ typeVal n) n
  withVars <- foldM genVar val (map partialKey varNames)
  foldM genArg withVars argNames
  where
    genName = HG.string (linear 5 10) HG.lower
    genVar base varName = do
      return $ TupleApply emptyMetaN (emptyMetaN, base) (EAppVar varName emptyMetaN)
    genArg base argName = do
      return $ TupleApply emptyMetaN (emptyMetaN, base) (EAppArg (ObjArr (Just (Value emptyMetaN argName)) ArgObj Nothing [] (Just (Nothing, emptyMetaN))))

-- | Generates an output expression equivalent to an input expression
genOutputExpr :: Expr () -> Expr () -> Gen (Expr ())
genOutputExpr (Value _ n) _ = return $ Value emptyMetaN n
genOutputExpr (TupleApply _ (_, b) _) input = genOutputExpr b input
-- genOutputExpr (TupleApply _ (_, b) arg@ObjArr{oaArr}) input = do
--   b' <- genOutputExpr b input
--   oaArr' <- case oaArr of
--     (Just e, _) -> do
--       e' <- genOutputExpr e input
--       return (Just e', emptyMetaN)
--     (Nothing, _) -> return (Just (HoleExpr emptyMetaN (HoleActive Nothing)), emptyMetaN)
--   return $ TupleApply emptyMetaN (emptyMetaN, b') arg{oaArr=oaArr'}
-- genOutputExpr (VarApply _ b _ _) input = genOutputExpr b input
-- genOutputExpr (VarApply _ b varName _) input = do
--   b' <- genOutputExpr b input
--   return $ VarApply emptyMetaN b' varName emptyMetaN
genOutputExpr e _ = error $ printf "Not yet implemented getOutputExpr for %s" (show e)


genPrgm :: Gen (Prgm Expr ())
genPrgm = do
  inputExprs <- HG.list (linear 1 5) (HG.either_ genInputExpr genInputExpr)
  let (dataTypes, funs) = partitionEithers inputExprs
  let dataObjs = map (\obj -> ObjArr (Just obj) TypeObj Nothing [] Nothing) dataTypes

  let allInputs = dataTypes ++ funs
  funObjs <- forM funs $ \obj -> do
                                let otherInputs = filter (/= obj) allInputs
                                if null otherInputs
                                  then return $ ObjArr (Just obj) FunctionObj Nothing [] (Just (Nothing, emptyMetaN))
                                  else do
                                    arrGoal <- HG.element otherInputs
                                    arr <- genOutputExpr arrGoal obj
                                    return $ ObjArr (Just obj) FunctionObj Nothing [] (Just (Just arr, emptyMetaN))

  let objMap = dataObjs ++ funObjs

  return $ Prgm objMap (classGraphFromObjs objMap) []

genPrgms :: Gen [GraphNodes (Prgm Expr ()) FileImport]
genPrgms = do
  prgm <- genPrgm
  prgmName <- HG.string (linear 5 10) HG.lower
  return [(prgm, desFileImport $ mkRawFileImport $ RawCExpr emptyMetaN $ CStr prgmName, [])]
