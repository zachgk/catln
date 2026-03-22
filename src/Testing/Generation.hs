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
import           CtConstants
import           Data.Bifunctor          (bimap)
import           Data.Either             (partitionEithers)
import qualified Data.HashMap.Strict     as H
import           Data.List               (partition)
import           Data.Maybe
import qualified Data.Set                as S
import           Eval.Common             (Val (..))
import           Hedgehog
import qualified Hedgehog.Gen            as HG
import           Hedgehog.Range          (linear, linearFrac, singleton)
import           Semantics               (classGraphFromObjs, mkTypeEnv)
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
genTypeFromExpr prgm (EWhere _ base cond) = do
  baseType <- genTypeFromExpr prgm base
  condType <- genTypeFromExpr prgm cond
  return $ partialAddPreds baseType (PredsOne $ PredExpr condType)
genTypeFromExpr _ e = error $ printf "Unimplemented genTypeFromExpr for %s" (show e)

genType :: Prgm Expr () -> Gen Type
genType prgm@(Prgm objMap _ _) = HG.choice gens
  where
    TypeEnv{teClassGraph=ClassGraph cg} = mkTypeEnv prgm

    gens = if graphEmpty cg
      then [genBasic, genConst]
      else [genBasic, genCGOld, genCG, genCGRel, genObjM, genObj, genConst]

    -- Find all functions with single argument having thisKey
    typePropFunctions = mapMaybe checkFunction $ flatObjectMap objMap
      where
        checkFunction ObjArr{oaBasis=FunctionObj, oaObj=Just objExpr} =
          case exprAppliedArgs objExpr of
            [arg] -> case oaObj arg of
              Just (Value _ n) | n == thisKey -> Just objExpr
              _                               -> Nothing
            _ -> Nothing
        checkFunction _ = Nothing

    genBasic :: Gen Type
    genBasic = HG.element [PTopType, BottomType]

    genConst :: Gen Type
    genConst = do
      c <- HG.choice [ CInt <$> HG.integral (linear (-100) 100)
                     , CFloat <$> HG.double (linearFrac (-100) 100)
                     , CStr <$> HG.string (linear 0 5) HG.alpha
                     , CChar <$> HG.unicode ]
      return $ UnionType Nothing H.empty [c]

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
      oa <- HG.element $ flatObjectMap objMap
      let objExpr = fromJust $ oaObj oa
      return $ getExprType objExpr

    genObj :: Gen Type
    genObj = do
      oa <- HG.element $ flatObjectMap objMap
      let objExpr = fromJust $ oaObj oa
      basePartial <- genTypeFromExpr prgm objExpr

      -- Optionally add type properties as refinement predicates
      if null typePropFunctions
        then return $ singletonType basePartial
        else do
          -- TODO: Fix support for shouldAddProps
          -- The current implementation of it doesn't actually make sense or produce valid PredExpr
          let shouldAddProps = False -- HG.bool
          if shouldAddProps
            then do
              -- Select a random subset of type properties
              numProps <- HG.int (linear 0 (min 3 $ length typePropFunctions))
              selectedProps <- take numProps <$> HG.shuffle typePropFunctions
              -- Add each as a predicate
              finalPartial <- foldM addPred basePartial selectedProps
              return $ singletonType finalPartial
            else return $ singletonType basePartial
      where
        addPred partial prop = do
          propType <- genTypeFromExpr prgm prop
          return $ partialAddPreds partial (PredsOne $ PredExpr propType)

genPartialType :: Prgm Expr () -> Gen PartialType
genPartialType prgm@(Prgm objMap _ _) = do
  gen <- if graphEmpty cg
    then if nullObjectMap objMap
      then HG.discard
      else return [genObj]
    else return [genCG, genObj]
  HG.choice gen
  where
    TypeEnv{teClassGraph=ClassGraph cg} = mkTypeEnv prgm
    genCG :: Gen PartialType
    genCG = do
      classNode <- HG.element $ graphToNodes cg
      return $ partialVal $ fromPartialName $ snd3 classNode

    genObj :: Gen PartialType
    genObj = do
      oa <- HG.element $ flatObjectMap objMap
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

  let objMap = objectMapFromList (dataObjs ++ funObjs)

  return $ Prgm objMap (classGraphFromObjs objMap) []

genPrgms :: Gen [GraphNodes (Prgm Expr ()) FileImport]
genPrgms = do
  prgm <- genPrgm
  prgmName <- HG.string (linear 5 10) HG.lower
  return [(prgm, desFileImport $ mkRawFileImport $ RawCExpr emptyMetaN $ CStr prgmName, [])]

-- | Generate a random 'Val' for a concrete 'PartialType'.
-- Returns 'HG.discard' for unknown types.
genVal :: PartialType -> Gen Val
genVal pt
  | pt == intLeaf   = IntVal <$> HG.integral (linear (-1000) 1000)
  | pt == floatLeaf = FloatVal <$> HG.double (linearFrac (-1000) 1000)
  | pt == strLeaf   = StrVal <$> HG.string (linear 0 20) HG.unicode
  | pt == charLeaf  = CharVal <$> HG.unicode
  | ptName pt == truePrim  = pure $ TupleVal truePrim H.empty
  | ptName pt == falsePrim = pure $ TupleVal falsePrim H.empty
  | otherwise = HG.discard

-- | Generate a random 'Val' for a 'Type'.
-- For union types, picks a random variant and delegates to 'genVal'.
genValForType :: Type -> Gen Val
genValForType (UnionType Nothing leafs []) = case splitUnionType leafs of
  []  -> HG.discard
  [p] -> genVal p
  ps  -> HG.element ps >>= genVal
genValForType _ = HG.discard
