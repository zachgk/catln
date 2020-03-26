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
import           Syntax

import Eval.Common

liftIntOp :: Name -> (Integer -> Integer -> Integer) -> (LeafType, [ResArrow])
liftIntOp name f = (srcType, [arrow])
  where
    srcType = LeafType ("operator" ++ name) (H.fromList [("l", intLeaf), ("r", intLeaf)])
    arrow = PrimArrow intType (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (IntVal l), Just (IntVal r)) -> IntVal $ f l r
                           _ -> error "Invalid intOp signature"
                           )

liftCmpOp :: Name -> (Integer -> Integer -> Bool) -> (LeafType, [ResArrow])
liftCmpOp name f = (srcType, [arrow])
  where
    srcType = LeafType ("operator" ++ name) (H.fromList [("l", intLeaf), ("r", intLeaf)])
    arrow = PrimArrow boolType (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (IntVal l), Just (IntVal r)) -> BoolVal $ f l r
                           _ -> error "Invalid compOp signature"
                           )

liftBoolOp :: Name -> (Bool -> Bool -> Bool) -> (LeafType, [ResArrow])
liftBoolOp name f = (srcType, [arrow])
  where
    srcType = LeafType ("operator" ++ name) (H.fromList [("l", boolLeaf), ("r", boolLeaf)])
    arrow = PrimArrow boolType (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (BoolVal l), Just (BoolVal r)) -> BoolVal $ f l r
                           _ -> error "Invalid boolOp signature"
                           )

rnot :: Name -> (LeafType, [ResArrow])
rnot name = (srcType, [arrow])
  where
    srcType = LeafType ("operator" ++ name) (H.singleton "a" boolLeaf)
    arrow = PrimArrow boolType (\args -> case H.lookup "a" args of
          Just (BoolVal b) -> BoolVal $ not b
          _ -> error "Invalid rnot signature"
          )

primEnv :: ResEnv
primEnv = H.fromList [ liftIntOp "+" (+)
                     , liftIntOp "-" (-)
                     , liftIntOp "*" (*)
                     , liftCmpOp ">" (>)
                     , liftCmpOp "<" (<)
                     , liftCmpOp ">=" (>=)
                     , liftCmpOp "<=" (<=)
                     , liftCmpOp "==" (==)
                     , liftCmpOp "!=" (/=)
                     , liftBoolOp "&" (&&)
                     , liftBoolOp "|" (||)
                     , rnot "~"
                     ]