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

type Op = (PartialType, [(Guard (Expr Typed), ResArrow EPrim)])

true, false :: Val
true = TupleVal "True" H.empty
false = TupleVal "False" H.empty

bool :: Bool -> Val
bool True = true
bool False = false

liftIntOp :: Name -> (Integer -> Integer -> Integer) -> Op
liftIntOp name f = (srcType, [(NoGuard, arrow)])
  where
    srcType = ("operator" ++ name, H.fromList [("l", intType), ("r", intType)])
    arrow = PrimArrow intType (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (IntVal l), Just (IntVal r)) -> IntVal $ f l r
                           _ -> error "Invalid intOp signature"
                           )

liftCmpOp :: Name -> (Integer -> Integer -> Bool) -> Op
liftCmpOp name f = (srcType, [(NoGuard, arrow)])
  where
    srcType = ("operator" ++ name, H.fromList [("l", intType), ("r", intType)])
    arrow = PrimArrow boolType (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (IntVal l), Just (IntVal r)) -> bool $ f l r
                           _ -> error "Invalid compOp signature"
                           )

rneg :: Name -> Op
rneg name = (srcType, [(NoGuard, arrow)])
  where
    srcType = ("operator" ++ name, H.singleton "a" intType)
    arrow = PrimArrow intType (\args -> case H.lookup "a" args of
                                  Just (IntVal i) -> IntVal $ -i
                                  _ -> error "Invalid rneg signature"
                              )

primEnv :: ResBuildEnv EPrim
primEnv = H.fromList [ liftIntOp "+" (+)
                     , liftIntOp "-" (-)
                     , liftIntOp "*" (*)
                     , liftCmpOp ">" (>)
                     , liftCmpOp "<" (<)
                     , liftCmpOp ">=" (>=)
                     , liftCmpOp "<=" (<=)
                     , liftCmpOp "==" (==)
                     , liftCmpOp "!=" (/=)
                     , rneg "-"
                     ]
