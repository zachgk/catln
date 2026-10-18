--------------------------------------------------------------------
-- |
-- Module    :  Eval.Runtime
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines additional functions that are added to the
-- program as the primitives. The declarations of these functions
-- are in the Catln core. The function are executed in the interpreter
-- by executing the backing Haskell functions.
--------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module Eval.Runtime where

import qualified Data.HashMap.Strict   as H
import           Semantics.Prgm
import           Semantics.Types

-- import           Emit                (codegenPrgm)
import           Control.Applicative   ((<|>))
import           CtConstants
import           Data.Char             (chr, ord)
import           Data.Maybe            (fromMaybe)
import           Eval.Common
import           Eval.ExprBuilder
import           Eval.Runtime.Calendar (calendarPrims)
import           Eval.Runtime.Import   (importPrims)
import           Eval.Runtime.Prgm     (prgmPrims)
import           Eval.Runtime.System   (systemPrims)
import           Eval.Runtime.Tasks    (tasksPrims)
import           Eval.Runtime.WebDocs  (webDocsPrims)
import           Text.Megaparsec       (sourceName)
import           Text.Printf

type Op = (String, Either EPrim MacroFunction)

true, false :: Val
true = TupleVal truePrim H.empty
false = TupleVal falsePrim H.empty

trueE, falseE :: Expr EvalMetaDat
trueE = Value (emptyMetaT trueType) (mkName truePrim)
falseE = Value (emptyMetaT falseType) (mkName falsePrim)

bool :: Bool -> Val
bool True  = true
bool False = false

liftIntOp :: TypeName -> (Integer -> Integer -> Integer) -> EPrim
liftIntOp name f = EPrim ("int" ++ nameStr name) prim
  where
    prim args = case (H.lookup "/l" args, H.lookup "/r" args) of
      (Just (IntVal l), Just (IntVal r)) -> Right $ IntVal $ f l r
      _                                  -> Left "Invalid intOp signature"

liftCmpOp :: TypeName -> (Integer -> Integer -> Bool) -> EPrim
liftCmpOp name f = EPrim ("int" ++ nameStr name) prim
  where
    prim args = case (H.lookup "/l" args, H.lookup "/r" args) of
      (Just (IntVal l), Just (IntVal r)) -> Right $ bool $ f l r
      _                                  -> Left "Invalid compOp signature"

rneg :: EPrim
rneg = EPrim "intNeg" prim
  where
    prim args = case H.lookup "/a" args of
      Just (IntVal i) -> Right $ IntVal $ -i
      _               -> Left "Invalid rneg signature"

liftFloatOp :: TypeName -> (Double -> Double -> Double) -> EPrim
liftFloatOp name f = EPrim ("float" ++ nameStr name) prim
  where
    prim args = case (H.lookup "/l" args, H.lookup "/r" args) of
      (Just (FloatVal l), Just (FloatVal r)) -> Right $ FloatVal $ f l r
      _                                      -> Left "Invalid floatOp signature"

liftFloatCmpOp :: TypeName -> (Double -> Double -> Bool) -> EPrim
liftFloatCmpOp name f = EPrim ("float" ++ nameStr name) prim
  where
    prim args = case (H.lookup "/l" args, H.lookup "/r" args) of
      (Just (FloatVal l), Just (FloatVal r)) -> Right $ bool $ f l r
      _                                      -> Left "Invalid floatCmpOp signature"

rfneg :: EPrim
rfneg = EPrim "floatNeg" prim
  where
    prim args = case H.lookup "/a" args of
      Just (FloatVal f) -> Right $ FloatVal $ -f
      _                 -> Left "Invalid rfneg signature"

intAbs :: EPrim
intAbs = EPrim "intAbs" prim
  where
    prim args = case H.lookup "/this" args of
      Just (IntVal i) -> Right $ IntVal $ abs i
      _               -> Left "Invalid intAbs signature"

floatAbs :: EPrim
floatAbs = EPrim "floatAbs" prim
  where
    prim args = case H.lookup "/this" args of
      Just (FloatVal f) -> Right $ FloatVal $ abs f
      _                 -> Left "Invalid floatAbs signature"

intPow :: EPrim
intPow = EPrim "intPow" prim
  where
    prim args = case (H.lookup "/this" args, H.lookup "/exp" args) of
      (Just (IntVal b), Just (IntVal e)) -> Right $ IntVal $ b ^ e
      _                                  -> Left "Invalid intPow signature"

floatPow :: EPrim
floatPow = EPrim "floatPow" prim
  where
    prim args = case (H.lookup "/this" args, H.lookup "/exp" args) of
      (Just (FloatVal b), Just (FloatVal e)) -> Right $ FloatVal $ b ** e
      _ -> Left "Invalid floatPow signature"

floatToInt :: EPrim
floatToInt = EPrim "floatToInt" prim
  where
    prim args = case H.lookup "/this" args of
      Just (FloatVal f) -> Right $ IntVal $ truncate f
      _                 -> Left "Invalid floatToInt signature"

intToFloat :: EPrim
intToFloat = EPrim "intToFloat" prim
  where
    prim args = case H.lookup "/this" args of
      Just (IntVal i) -> Right $ FloatVal $ fromIntegral i
      _               -> Left "Invalid intToFloat signature"

-- | A float taken apart into what its text is written from: whether it is negative, and its
-- binary mantissa and exponent ('decodeFloat' of its magnitude), or that it is not a number or
-- an infinity. The text itself is written in Catln (@stack\/core\/string.ct@).
--
-- The sign is its own field rather than the mantissa's, so that @-0.0@ keeps it.
floatParts :: EPrim
floatParts = EPrim "floatParts" prim
  where
    prim args = case H.lookup "/this" args of
      Just (FloatVal f)
        | isNaN f -> Right $ TupleVal (partsName "NaN") H.empty
        | isInfinite f -> Right $ TupleVal (partsName "Infinite") (H.singleton (runtimeArgKey "negative") (bool (f < 0)))
        | otherwise -> let (m, e) = decodeFloat (abs f)
                       in Right $ TupleVal (partsName "Finite") $ H.fromList
                            [ (runtimeArgKey "negative", bool (f < 0 || isNegativeZero f))
                            , (runtimeArgKey "mantissa", IntVal m)
                            , (runtimeArgKey "exponent", IntVal (fromIntegral e)) ]
      _ -> Left "Invalid floatParts signature"
    partsName n = "/Data/Primitive/FloatParts/" ++ n

intToBool :: EPrim
intToBool = EPrim "intToBool" prim
  where
    prim args = case H.lookup "/this" args of
      Just (IntVal 0) -> Right false
      Just (IntVal _) -> Right true
      _               -> Left "Invalid intToBool signature"

floatToBool :: EPrim
floatToBool = EPrim "floatToBool" prim
  where
    prim args = case H.lookup "/this" args of
      Just (FloatVal 0.0) -> Right false
      Just (FloatVal _)   -> Right true
      _                   -> Left "Invalid floatToBool signature"

-- | A character's Unicode code point.
charCode :: EPrim
charCode = EPrim "charCode" prim
  where
    prim args = case H.lookup "/this" args of
      Just (CharVal c) -> Right $ IntVal $ fromIntegral $ ord c
      _                -> Left "Invalid charCode signature"

-- | The character at a Unicode code point, refusing a number that is not one.
charFromCode :: EPrim
charFromCode = EPrim "charFromCode" prim
  where
    prim args = case H.lookup "/code" args of
      Just (IntVal n) | n >= 0 && n <= fromIntegral (ord maxBound) -> Right $ CharVal $ chr $ fromIntegral n
      Just (IntVal n) -> Left $ printf "%d is not a Unicode code point" n
      _ -> Left $ printf "Invalid charFromCode signature with args %s" (show args)

intMax :: EPrim
intMax = liftIntOp "Max" max

floatMax :: EPrim
floatMax = liftFloatOp "Max" max

intMin :: EPrim
intMin = liftIntOp "Min" min

floatMin :: EPrim
floatMin = liftFloatOp "Min" min

floatRound :: EPrim
floatRound = EPrim "floatRound" prim
  where
    prim args = case H.lookup "/this" args of
      Just (FloatVal f) -> Right $ IntVal $ round f
      _                 -> Left "Invalid floatRound signature"

floatFloor :: EPrim
floatFloor = EPrim "floatFloor" prim
  where
    prim args = case H.lookup "/this" args of
      Just (FloatVal f) -> Right $ IntVal $ floor f
      _                 -> Left "Invalid floatFloor signature"

floatCeil :: EPrim
floatCeil = EPrim "floatCeil" prim
  where
    prim args = case H.lookup "/this" args of
      Just (FloatVal f) -> Right $ IntVal $ ceiling f
      _                 -> Left "Invalid floatCeil signature"

intDiv :: EPrim
intDiv = liftIntOp "//" div

-- | Sets the exit code of the io it is called in, answering with unit in the io carrying it.
ioExit :: EPrim
ioExit = EPrim "ioExit" prim
  where
    prim args = case (H.lookup ioStr args, H.lookup "/val" args) of
      (Just (IOVal _ io), Just (IntVal val)) -> Right $ ioContextVal unitVal (IOVal val io)
      _ -> Left $ printf "Invalid exit signature with args: %s" (show args)

-- | Writes a line through the handle of the io it is called in, as it is evaluated, and answers
-- with unit in that io so later effects are sequenced after this one.
println :: EPrim
println = EIOPrim "println" prim
  where
    prim handle args = case (H.lookup ioStr args, H.lookup "/msg" args) of
      (Just io@IOVal{}, Just (StrVal msg)) -> do
        iohPutLine handle msg
        return $ Right $ ioContextVal unitVal io
      _ -> return $ Left $ printf "Invalid println signature with args: %s" (show args)

-- | Reads a line through the handle of the io it is called in.
ioInput :: EPrim
ioInput = EIOPrim "ioInput" prim
  where
    prim handle args = case H.lookup ioStr args of
      Just io -> fmap (\l -> ioContextVal (StrVal l) io) <$> iohGetLine handle
      Nothing -> return $ Left "Invalid input signature"

arrExists :: Op
arrExists = ("arrExists", Right (MacroFunction macroBuild))
  where
    macroBuild input MacroData{mdTbEnv=TBEnv{tbTypeEnv}} = do
      let args = exprAppliedArgsMap input
      case (H.lookup (partialKey (mkName operatorArgL)) args, H.lookup (partialKey (mkName operatorArgR)) args) of
        (Just (Just (l, _)), Just (Just (_r, _))) -> case typeGraphQuery tbTypeEnv H.empty (fromMaybe (error "Non singleton in arrExists") $ maybeGetSingleton $ getMetaType l) of
          [] -> return $ Right falseE
          _  -> return $ Right trueE
        _ -> fail "Invalid arrExists input"

contextMacro :: Op
contextMacro = ("context", Right (MacroFunction macroBuild))
  where
    macroBuild input MacroData{} = do
      let args = exprAppliedArgsMap input
      case H.lookup (partialKey (mkName contextValStr)) args of
        Just (Just (_, Just val)) -> do
          let contextArgs = filter ((/=) contextValStr . nameStr . oaObjPath) $ exprAppliedArgs input
          return $ Right $ eVal ContextInStr `eApply` (contextValStr, val) `eApplyOAs` contextArgs
        _ -> fail "Invalid context macro input"

llvm :: Op
llvm = ("llvm", Right (MacroFunction macroBuild))
  where
    macroBuild input MacroData{mdTbEnv} =
      case input of
        (TupleApply _ (_, Value _ "/Catln/llvm") (EAppArg ObjArr{oaObj=Just (Value _ "/c"), oaArr=Just (Just (Value _ functionToCodegen), _)})) -> buildName functionToCodegen
        _ -> error $ printf "Unknown expr to llvm macro: %s" (show input)
      where
        buildName functionToCodegen = do
          let TBEnv{tbPrgm} = mdTbEnv
          let codegenSrcTypeInner = singletonType $ PartialType functionToCodegen H.empty H.empty PredsNone PtArgExact
          let codegenSrcType = PartialType "/Catln/Context" H.empty (H.fromList [(partialKey "/value", codegenSrcTypeInner), (partialKey "/oaObjExprio", ioType)]) PredsNone PtArgExact
          let val = LLVMVal $ codegenPrgm (eVal (nameStr functionToCodegen)) codegenSrcType ioType tbPrgm
          return $ Left val
        codegenPrgm _ _ _ _ = return ()

-- | Completes a call with the page it is written in, as a @page@ argument read by
-- @\/Catln\/pageAt@: a call of @lcm(input)@ written in a page is a call of @lcm(input, page)@.
--
-- A macro because only the call knows where it is written. The page is not what the call is
-- completed with, only the source naming it: the page itself is a value as large as everything
-- written on it, and a macro's value is built as a constant typed to exactly itself. Read at
-- evaluation instead, it is one call, typed as @pageAt@ declares.
--
-- A call with no position is reported rather than completed with some other page. Neither is a call
-- whose type is not one tuple, which has no one shape to add the argument to.
callerPage :: Op
callerPage = ("callerPage", Right (MacroFunction macroBuild))
  where
    pageAtName = "/Catln/pageAt"
    macroBuild input MacroData{mdTbEnv=TBEnv{tbPrgm=Prgm{prgmObjMap=ObjectMap objMap}}} = do
      -- A call in a context (@ContextIn(value= lcm(input= ..), io)@, as a bind writes it) is the
      -- call it holds, given its states: that call is what is completed, and where it is written is
      -- where the call is.
      let called = fromMaybe input (contextValueOf input)
      source <- case getMetaPos (getExprMeta input) <|> getMetaPos (getExprMeta called) of
        Just (start, _) -> return $ sourceName start
        Nothing -> fail $ printf "%s asks for the page it is written in, but the call has no position to say which page that is" (show input)
      pageType <- case H.lookup (mkName pageAtName) objMap of
        Just ObjectMapItem{omiRuntimeDecls=(ObjArr{oaArr=Just (_, m)}:_)} -> return $ getMetaType m
        _ -> fail $ printf "%s asks for the page it is written in, which is answered by %s, and nothing in the program declares it (it is declared by import hsrawprgm())" (show input) pageAtName
      case maybeGetSingleton (getExprType input) of
        Nothing -> fail $ printf "%s asks for the page it is written in, but it is not a call of one shape to add the page to: %s" (show input) (show $ getExprType input)
        Just _ -> do
          let pageCall = eVal pageAtName `eApply` ("/source", valToEExpr (StrVal source))
          let withPage e = e `eApplyOAs` [mkIOObjArr (emptyMetaT pageType) (partialKey (mkName "/page")) pageCall]
          return $ Right $ fromMaybe (withPage input) (inContextValue withPage input)
    -- The call a call in a context holds, and the context given the call changed.
    contextValueOf e = case [v | ObjArr{oaObj=Just k, oaArr=Just (Just v, _)} <- exprAppliedArgs e, isContextValue k] of
      (v:_) | nameStr (exprPath e) == ContextInStr -> Just v
      _                                            -> Nothing
    -- Built again rather than edited in place, so the context's type is the completed call's: the
    -- states are given to the call as it now is.
    inContextValue f e = do
      v <- contextValueOf e
      let states = [oa | oa@ObjArr{oaObj=Just k} <- exprAppliedArgs e, not (isContextValue k)]
      return $ positionedAs e $ (eVal ContextInStr `eApply` (contextValStr, f v)) `eApplyOAs` states
    positionedAs e e' = setExprMeta e' (getExprMeta e'){getMetaPos = getMetaPos (getExprMeta e)}
    isContextValue k = nameStr (makeAbsoluteName (exprPath k)) == contextValStr

primEnv :: ResBuildPrims
primEnv = H.fromList [(mkName k, v) | (k, v) <- map mapPrim prims ++ macros]
  where
    mapPrim p = (ePrimName p, Left p)
    prims = webDocsPrims ++ tasksPrims ++ calendarPrims ++ prgmPrims ++ importPrims ++ systemPrims ++
            [ liftIntOp "+" (+)
            , liftIntOp "-" (-)
            , liftIntOp "*" (*)
            , liftIntOp "%" mod
            , liftCmpOp ">" (>)
            , liftCmpOp "<" (<)
            , liftCmpOp ">=" (>=)
            , liftCmpOp "<=" (<=)
            , liftCmpOp "==" (==)
            , liftCmpOp "!=" (/=)
            , rneg
            , liftFloatOp "+" (+)
            , liftFloatOp "-" (-)
            , liftFloatOp "*" (*)
            , liftFloatOp "//" (/)
            , liftFloatCmpOp ">" (>)
            , liftFloatCmpOp "<" (<)
            , liftFloatCmpOp ">=" (>=)
            , liftFloatCmpOp "<=" (<=)
            , liftFloatCmpOp "==" (==)
            , liftFloatCmpOp "!=" (/=)
            , rfneg
            , ioExit
            , println
            , ioInput
            , intAbs
            , floatAbs
            , intPow
            , floatPow
            , floatToInt
            , intToFloat
            , floatParts
            , intToBool
            , floatToBool
            , charCode
            , charFromCode
            , intMax
            , floatMax
            , intMin
            , floatMin
            , floatRound
            , floatFloor
            , floatCeil
            , intDiv
            ]
    macros = [arrExists, contextMacro, llvm, callerPage]
