--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Constrain
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module TypeCheck.Constrain where

import           Control.Monad.ST

import           Syntax
import           TypeCheck.Common
import           Data.UnionFind.ST

isSolved :: Scheme -> Bool
isSolved (SType a b) = a == b
isSolved _ = False

equalizeSchemes :: (Scheme, Scheme) -> Scheme
equalizeSchemes (_, SCheckError s) = SCheckError s
equalizeSchemes (SCheckError s, _) = SCheckError s
equalizeSchemes (SType lb1 ub1, SType lb2 ub2) = let lbBoth = unionRawTypes lb1 lb2
                                                     ubBoth = intersectRawTypes ub1 ub2
                                                  in if hasRawType lbBoth ubBoth
                                                        then SType lbBoth ubBoth
                                                        else SCheckError $ concat ["Type Mismatched: ", show lbBoth, " is not a subtype of ", show ubBoth]

executeConstraint :: Constraint s -> ST s [Constraint s]
executeConstraint (EqualsKnown pnt tp) = modifyDescriptor pnt (\oldScheme -> equalizeSchemes (oldScheme, SType tp tp)) >> return []
executeConstraint (EqPoints p1 p2) = union' p1 p2 (\s1 s2 -> return (equalizeSchemes (s1, s2))) >> return []
executeConstraint cons@(BoundedBy subPnt parentPnt) = do
  subScheme <- descriptor subPnt
  parentScheme <- descriptor parentPnt
  case (subScheme, parentScheme) of
    (_, SCheckError _) -> return []
    (SCheckError _, _) -> return []
    (SType lb1 ub1, SType _ ub2) -> do
      let subScheme' = SType lb1 (intersectRawTypes ub1 ub2)
      setDescriptor subPnt subScheme'
      return [cons | isSolved subScheme']
executeConstraint (IsTupleOf wholePnt partPnts) = do
  wholeScheme <- descriptor wholePnt
  partSchemes <- mapM descriptor partPnts
  return []


abandonConstraints :: Constraint s -> ST s ()
abandonConstraints = undefined
-- abandonConstraints EqualsKnown{} = error "Bad Type equality"
-- abandonConstraints EqPoints{} = error "Bad point equality"
-- abandonConstraints (HasType subPnt parentPnt) = do
--   subScheme <- descriptor subPnt
--   parentScheme <- descriptor parentPnt
--   case (subScheme, parentScheme) of
--     -- (SKnown _, SUnknown) -> setDescriptor parentPnt $ SCheckError "Failed to unify hasType"
--     (_, _) -> error "Uknown abandon constraint failure"

runConstraints :: [Constraint s] -> ST s ()
runConstraints [] = return ()
runConstraints cons = do
  res <- mapM executeConstraint cons
  let cons' = concat res
  if cons == cons'
    then mapM_ abandonConstraints cons
    else runConstraints cons'
