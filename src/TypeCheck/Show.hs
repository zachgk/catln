--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Show
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module TypeCheck.Show where

import           Control.Monad.ST
import           Data.UnionFind.ST

import           Syntax
import           TypeCheck.Common

showM :: VarMeta s -> ST s ShowMeta
showM = descriptor

showExpr :: VExpr s -> ST s SExpr
showExpr (CExpr m c) = do
  m' <- showM m
  return (CExpr m' c)
showExpr (Tuple m name args) = do
  m' <- showM m
  args' <- mapM showExpr args
  return (Tuple m' name args')

showObj :: VObject s -> ST s SObject
showObj (Object m name args) = do
  m' <- showM m
  args' <- mapM showM args
  return (Object m' name args')

showArrow :: VArrow s -> ST s SArrow
showArrow (Arrow m obj expr) = do
  m' <- showM m
  expr' <- showExpr expr
  obj' <- showObj obj
  return (Arrow m' obj' expr')

showPrgm :: VPrgm s -> ST s SPrgm
showPrgm (objects, arrows) = do
  objs' <- mapM showObj objects
  arrows' <- mapM showArrow arrows
  return (objs', arrows')
