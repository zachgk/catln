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
import qualified Data.HashMap.Strict as H

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

showArrow :: VArrow s -> ST s SArrow
showArrow (Arrow m expr) = do
  m' <- showM m
  expr' <- showExpr expr
  return (Arrow m' expr')

showObj :: (VObject s, [VArrow s]) -> ST s (SObject, [SArrow])
showObj (Object m name args, arrows) = do
  m' <- showM m
  args' <- mapM showM args
  arrows' <- mapM showArrow arrows
  return (Object m' name args', arrows')

showPrgm :: VPrgm s -> ST s SPrgm
showPrgm prgm = do
  objs' <- mapM showObj prgm
  return $ H.fromList objs'
