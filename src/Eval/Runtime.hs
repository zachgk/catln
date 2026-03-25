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

import qualified Data.HashMap.Strict as H
import           Semantics.Prgm
import           Semantics.Types

-- import           Emit                (codegenPrgm)
import           CtConstants
import           Data.Char           (chr, ord)
import           Data.List           (sort)
import           Data.Maybe          (fromMaybe)
import           Eval.Common
import           Eval.ExprBuilder
import           Numeric             (showHex, showOct)
import           Text.Printf

type Op = (String, Either EPrim MacroFunction)

true, false :: Val
true = TupleVal truePrim H.empty
false = TupleVal falsePrim H.empty

trueE, falseE :: Expr EvalMetaDat
trueE = Value (emptyMetaT trueType) truePrim
falseE = Value (emptyMetaT falseType) falsePrim

bool :: Bool -> Val
bool True  = true
bool False = false

liftIntOp :: TypeName -> (Integer -> Integer -> Integer) -> EPrim
liftIntOp name f = EPrim ("int" ++ name) prim
  where
    prim args = case (H.lookup "/l" args, H.lookup "/r" args) of
      (Just (IntVal l), Just (IntVal r)) -> Right $ IntVal $ f l r
      _                                  -> Left "Invalid intOp signature"

liftCmpOp :: TypeName -> (Integer -> Integer -> Bool) -> EPrim
liftCmpOp name f = EPrim ("int" ++ name) prim
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
liftFloatOp name f = EPrim ("float" ++ name) prim
  where
    prim args = case (H.lookup "/l" args, H.lookup "/r" args) of
      (Just (FloatVal l), Just (FloatVal r)) -> Right $ FloatVal $ f l r
      _                                      -> Left "Invalid floatOp signature"

liftFloatCmpOp :: TypeName -> (Double -> Double -> Bool) -> EPrim
liftFloatCmpOp name f = EPrim ("float" ++ name) prim
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

strEq :: EPrim
strEq = EPrim "strEq" prim
  where
    prim args = case (H.lookup "/l" args, H.lookup "/r" args) of
      (Just (StrVal l), Just (StrVal r)) -> Right $ bool $ l == r
      _                                  -> Left "Invalid intToString signature"

intToString :: EPrim
intToString = EPrim "intToString" prim
  where
    prim args = case H.lookup "/this" args of
      (Just (IntVal val)) -> Right $ StrVal $ show val
      _                   -> Left "Invalid intToString signature"


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

floatToString :: EPrim
floatToString = EPrim "floatToString" prim
  where
    prim args = case H.lookup "/this" args of
      Just (FloatVal f) -> Right $ StrVal $ show f
      _                 -> Left "Invalid floatToString signature"

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

intToChar :: EPrim
intToChar = EPrim "intToChar" prim
  where
    prim args = case H.lookup "/n" args of
      Just (IntVal n) -> Right $ StrVal [chr (fromIntegral n)]
      _               -> Left "Invalid intToChar signature"

strToOrd :: EPrim
strToOrd = EPrim "strToOrd" prim
  where
    prim args = case H.lookup "/this" args of
      Just (StrVal (c:_)) -> Right $ IntVal $ fromIntegral (ord c)
      _                   -> Left "Invalid strToOrd signature"

intToHex :: EPrim
intToHex = EPrim "intToHex" prim
  where
    prim args = case H.lookup "/this" args of
      Just (IntVal n)
        | n < 0     -> Right $ StrVal $ "-0x" ++ showHex (-n) ""
        | otherwise -> Right $ StrVal $ "0x" ++ showHex n ""
      _ -> Left "Invalid intToHex signature"

intToOct :: EPrim
intToOct = EPrim "intToOct" prim
  where
    prim args = case H.lookup "/this" args of
      Just (IntVal n)
        | n < 0     -> Right $ StrVal $ "-0o" ++ showOct (-n) ""
        | otherwise -> Right $ StrVal $ "0o" ++ showOct n ""
      _ -> Left "Invalid intToOct signature"

intToBin :: EPrim
intToBin = EPrim "intToBin" prim
  where
    prim args = case H.lookup "/this" args of
      Just (IntVal n)
        | n < 0     -> Right $ StrVal $ "-0b" ++ toBin (-n)
        | otherwise -> Right $ StrVal $ "0b" ++ toBin n
      _ -> Left "Invalid intToBin signature"
    toBin 0 = "0"
    toBin n = reverse $ go n
    go 0 = ""
    go n = (if n `mod` 2 == 0 then '0' else '1') : go (n `div` 2)

strConcat :: EPrim
strConcat = EPrim "strConcat" prim
  where
    prim args = case (H.lookup "/l" args, H.lookup "/r" args) of
      (Just (StrVal l), Just (StrVal r)) -> Right $ StrVal $ l ++ r
      _                                  -> Left "Invalid strConcat signature"

strLength :: EPrim
strLength = EPrim "strLength" prim
  where
    prim args = case H.lookup "/this" args of
      Just (StrVal s) -> Right $ IntVal $ fromIntegral (length s)
      _               -> Left "Invalid strLength signature"

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

--------------------------------------------------------------------
-- ConsList runtime support
--------------------------------------------------------------------

nilPrim, consPrim :: String
nilPrim  = "/Data/Nil"
consPrim = "/Data/Cons"

-- | Traverse a TupleVal-encoded ConsList to a Haskell list.
fromConsList :: Val -> Either String [Val]
fromConsList (TupleVal n _)       | n == nilPrim  = Right []
fromConsList (TupleVal n fields)  | n == consPrim =
  case (H.lookup "/head" fields, H.lookup "/tail" fields) of
    (Just h, Just t) -> (h :) <$> fromConsList t
    _                -> Left "Invalid Cons: missing head or tail"
fromConsList v = Left $ printf "Expected ConsList, got: %s" (show v)

-- | Build a TupleVal-encoded ConsList from a Haskell list.
toConsList :: [Val] -> Val
toConsList []     = TupleVal nilPrim H.empty
toConsList (x:xs) = TupleVal consPrim $ H.fromList [("/head", x), ("/tail", toConsList xs)]

listSum :: EPrim
listSum = EPrim "listSum" prim
  where
    prim args = case H.lookup "/lst" args of
      Just lst -> case fromConsList lst of
        Left err  -> Left err
        Right []  -> Right (IntVal 0)
        Right vs  -> case head vs of
          IntVal _   -> Right $ IntVal   $ sum [i | IntVal   i <- vs]
          FloatVal _ -> Right $ FloatVal $ sum [f | FloatVal f <- vs]
          _          -> Left "listSum: expected numeric elements"
      _ -> Left "Invalid listSum signature"

listAll :: EPrim
listAll = EPrim "listAll" prim
  where
    prim args = case H.lookup "/lst" args of
      Just lst -> case fromConsList lst of
        Right vs -> Right $ bool $ all isTruthy vs
        Left err -> Left err
      _ -> Left "Invalid listAll signature"
    isTruthy (TupleVal n _) = n == truePrim
    isTruthy _              = False

listAny :: EPrim
listAny = EPrim "listAny" prim
  where
    prim args = case H.lookup "/lst" args of
      Just lst -> case fromConsList lst of
        Right vs -> Right $ bool $ any isTruthy vs
        Left err -> Left err
      _ -> Left "Invalid listAny signature"
    isTruthy (TupleVal n _) = n == truePrim
    isTruthy _              = False

listReversed :: EPrim
listReversed = EPrim "listReversed" prim
  where
    prim args = case H.lookup "/lst" args of
      Just lst -> case fromConsList lst of
        Right vs -> Right $ toConsList (reverse vs)
        Left err -> Left err
      _ -> Left "Invalid listReversed signature"

listSorted :: EPrim
listSorted = EPrim "listSorted" prim
  where
    prim args = case H.lookup "/lst" args of
      Just lst -> case fromConsList lst of
        Left err -> Left err
        Right [] -> Right (toConsList [])
        Right vs -> case head vs of
          IntVal _   -> Right $ toConsList $ map IntVal   $ sort [i | IntVal   i <- vs]
          FloatVal _ -> Right $ toConsList $ map FloatVal $ sort [f | FloatVal f <- vs]
          _          -> Left "listSorted: expected numeric elements"
      _ -> Left "Invalid listSorted signature"

listRange :: EPrim
listRange = EPrim "listRange" prim
  where
    prim args = case H.lookup "/stop" args of
      Just (IntVal n) -> Right $ toConsList [IntVal i | i <- [0 .. n - 1]]
      _               -> Left "Invalid listRange signature"

listLength :: EPrim
listLength = EPrim "listLength" prim
  where
    prim args = case H.lookup "/lst" args of
      Just lst -> case fromConsList lst of
        Right vs -> Right $ IntVal $ fromIntegral (length vs)
        Left err -> Left err
      _ -> Left "Invalid listLength signature"

ioExit :: EPrim
ioExit = EPrim "ioExit" prim
  where
    prim args = case (H.lookup "/this" args, H.lookup "/val" args) of
      (Just (IOVal _ io), Just (IntVal val)) -> Right $ IOVal val io
      _ -> Left $ printf "Invalid exit signature with args: %s" (show args)

println :: EPrim
println = EPrim "println" prim
  where
    prim args = case (H.lookup "/this" args, H.lookup "/msg" args) of
      (Just (IOVal r io), Just (StrVal msg)) -> Right $ IOVal r (io >> putStrLn msg)
      _ -> Left "Invalid println signature"

arrExists :: Op
arrExists = ("arrExists", Right (MacroFunction macroBuild))
  where
    macroBuild input MacroData{mdTbEnv=TBEnv{tbTypeEnv}} = do
      let args = exprAppliedArgsMap input
      case (H.lookup (partialKey operatorArgL) args, H.lookup (partialKey operatorArgR) args) of
        (Just (Just (l, _)), Just (Just (_r, _))) -> case typeGraphQuery tbTypeEnv H.empty (fromMaybe (error "Non singleton in arrExists") $ maybeGetSingleton $ getMetaType l) of
          [] -> return $ Right falseE
          _  -> return $ Right trueE
        _ -> fail "Invalid arrExists input"

contextMacro :: Op
contextMacro = ("context", Right (MacroFunction macroBuild))
  where
    macroBuild input MacroData{} = do
      let args = exprAppliedArgsMap input
      case H.lookup (partialKey contextValStr) args of
        Just (Just (_, Just val)) -> do
          let contextArgs = filter ((/=) contextValStr . oaObjPath) $ exprAppliedArgs input
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
          let val = LLVMVal $ codegenPrgm (eVal functionToCodegen) codegenSrcType ioType tbPrgm
          return $ Left val
        codegenPrgm _ _ _ _ = return ()

primEnv :: ResBuildPrims
primEnv = H.fromList (map mapPrim prims ++ macros)
  where
    mapPrim p@(EPrim n _) = (n, Left p)
    prims = [ liftIntOp "+" (+)
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
            , strEq
            , intToString
            , ioExit
            , println
            , intAbs
            , floatAbs
            , intPow
            , floatPow
            , floatToInt
            , intToFloat
            , floatToString
            , intToBool
            , floatToBool
            , intToChar
            , strToOrd
            , intToHex
            , intToOct
            , intToBin
            , strConcat
            , strLength
            , intMax
            , floatMax
            , intMin
            , floatMin
            , floatRound
            , floatFloor
            , floatCeil
            , intDiv
            , listSum
            , listAll
            , listAny
            , listReversed
            , listSorted
            , listRange
            , listLength
            ]
    macros = [arrExists, contextMacro, llvm]
