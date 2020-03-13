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

showCompAnnot :: VCompAnnot s -> ST s SCompAnnot
showCompAnnot (CompAnnot name args) = do
  args' <- mapM showExpr args
  return $ CompAnnot name args'

showArrow :: VArrow s -> ST s SArrow
showArrow (Arrow m annots maybeExpr) = do
  m' <- showM m
  annots' <- mapM showCompAnnot annots
  case maybeExpr of
    Just expr -> do
      expr' <- showExpr expr
      return (Arrow m' annots' (Just expr'))
    Nothing -> return (Arrow m' annots' Nothing)

showObj :: (VObject s, [VArrow s]) -> ST s (SObject, [SArrow])
showObj (Object m name args, arrows) = do
  m' <- showM m
  args' <- mapM showM args
  arrows' <- mapM showArrow arrows
  return (Object m' name args', arrows')

showCon :: Constraint s -> SConstraint
showCon (EqualsKnown _ t) = SEqualsKnown t
showCon (EqPoints _ _) = SEqPoints
showCon (BoundedBy _ _) = SBoundedBy
showCon (IsTupleOf _ args) = SIsTupleOf (H.keysSet args)
showCon (ArrowTo _ _) = SArrowTo

showPrgm :: VPrgm s -> ST s SPrgm
showPrgm (objMap, classMap) = do
  objs' <- mapM showObj objMap
  return (H.fromList objs', classMap)

showConstraints :: [Constraint s] -> [SConstraint]
showConstraints = map showCon
