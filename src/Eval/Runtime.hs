--------------------------------------------------------------------
-- |
-- Module    :  Eval.Runtime
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Eval.Runtime where

import qualified Data.HashMap.Strict as H
import           Syntax.Types
import           Syntax.Prgm
import           Syntax

import Eval.Common

type Op = (TypeName, [(PartialType, Guard (Expr Typed), ResArrow EPrim)])

true, false :: Val
true = TupleVal "True" H.empty
false = TupleVal "False" H.empty

bool :: Bool -> Val
bool True = true
bool False = false

liftIntOp :: TypeName -> (Integer -> Integer -> Integer) -> Op
liftIntOp name f = (name', [(srcType, NoGuard, PrimArrow resType prim)])
  where
    name' = "operator" ++ name
    srcType = (PTypeName name', H.empty, H.fromList [("l", intType), ("r", intType)])
    resType = intType
    prim = EPrim srcType NoGuard (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (IntVal l), Just (IntVal r)) -> IntVal $ f l r
                           _ -> error "Invalid intOp signature"
                           )

liftCmpOp :: TypeName -> (Integer -> Integer -> Bool) -> Op
liftCmpOp name f = (name', [(srcType, NoGuard, PrimArrow resType prim)])
  where
    name' = "operator" ++ name
    srcType = (PTypeName name', H.empty, H.fromList [("l", intType), ("r", intType)])
    resType = boolType
    prim = EPrim srcType NoGuard (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (IntVal l), Just (IntVal r)) -> bool $ f l r
                           _ -> error "Invalid compOp signature"
                           )

rneg :: TypeName -> Op
rneg name = (name', [(srcType, NoGuard, PrimArrow resType prim)])
  where
    name' = "operator" ++ name
    srcType = (PTypeName name', H.empty, H.singleton "a" intType)
    resType = intType
    prim = EPrim srcType NoGuard (\args -> case H.lookup "a" args of
                                  Just (IntVal i) -> IntVal $ -i
                                  _ -> error "Invalid rneg signature"
                              )

strEq :: Op
strEq = (name', [(srcType, NoGuard, PrimArrow resType prim)])
  where
    name' = "operator=="
    srcType = (PTypeName name', H.empty, H.fromList [("l", strType), ("r", strType)])
    resType = boolType
    prim = EPrim srcType NoGuard (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                                  (Just (StrVal l), Just (StrVal r)) -> bool $ l == r
                                  _ -> error "Invalid intToString signature"
                              )

intToString :: Op
intToString = (name', [(srcType, NoGuard, PrimArrow resType prim)])
  where
    name' = "toString"
    srcType = (PTypeName name', H.empty, H.singleton "this" intType)
    resType = strType
    prim = EPrim srcType NoGuard (\args -> case H.lookup "this" args of
                                  (Just (IntVal val)) -> StrVal $ show val
                                  _ -> error "Invalid intToString signature"
                              )


primEnv :: ResBuildEnv EPrim
primEnv = H.fromListWith (++) [ liftIntOp "+" (+)
                              , liftIntOp "-" (-)
                              , liftIntOp "*" (*)
                              , liftCmpOp ">" (>)
                              , liftCmpOp "<" (<)
                              , liftCmpOp ">=" (>=)
                              , liftCmpOp "<=" (<=)
                              , liftCmpOp "==" (==)
                              , liftCmpOp "!=" (/=)
                              , rneg "-"
                              , strEq
                              , intToString
                              ]
