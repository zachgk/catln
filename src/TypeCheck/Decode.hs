--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Decode
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module TypeCheck.Decode where

import           Control.Monad
import           Control.Monad.ST
import           Data.Either
import           Data.Functor
import qualified Data.HashMap.Strict as H
import           Data.UnionFind.ST

import           Syntax
import           TypeCheck.Common

merge2TypeCheckResults :: TypeCheckResult a -> TypeCheckResult b -> TypeCheckResult (a, b)
merge2TypeCheckResults a b = case (a, b) of
  (Right a', Right b') -> Right (a', b')
  (a', b')             -> Left $ fromLeft [] a' ++ fromLeft [] b'

mergeTypeCheckResults :: [TypeCheckResult r] -> TypeCheckResult [r]
mergeTypeCheckResults res = case partitionEithers res of
  ([], rs) -> Right rs
  (ls, _)  -> Left $ concat ls

fromRawType :: RawType -> Maybe Type
fromRawType RawTopType = Nothing
fromRawType RawBottomType = Nothing
fromRawType (RawLeafType s) = Just $ LeafType s
fromRawType (RawSumType [t]) = fromRawType t
fromRawType (RawSumType ts) = do
  ts' <- mapM fromRawType ts
  Just $ SumType ts'
fromRawType (RawProdType ts) = do
  ts' <- mapM fromRawType ts
  Just $ ProdType ts'


toMeta :: VarMeta s -> String -> ST s (TypeCheckResult Typed)
toMeta p name = do
  scheme <- descriptor p
  return $ case scheme of
    SCheckError s -> Left ["CheckError on " ++ name ++ ": " ++ s]
    SType lb ub -> case fromRawType ub of
      Nothing -> Left ["CheckError on " ++ show name ++ ": \ntLower Bound: " ++ show lb ++ "\n\tUpper Bound: " ++ show ub]
      Just t -> Right $ Typed t

toExpr :: VExpr s -> ST s (TypeCheckResult TExpr)
toExpr (CExpr m c) = do
  res <- toMeta m $ "Constant " ++ show c
  return $ res <&> (`CExpr` c)
toExpr (Var m name) = do
  res <- toMeta m $ "variable " ++ name
  return $ res >>= (\m' -> Right $ Var m' name)
-- toExpr (Call m name exprs) = do
--   res1 <- toMeta m $ "Function call " ++ name
--   res2 <- mapM toExpr exprs
--   return $ case (res1, mergeTypeCheckResults res2) of
--     (Right m', Right exprs') -> Right $ Call m' name exprs'
--     (a, b)                   -> Left $ fromLeft [] a ++ fromLeft [] b

toDeclLHS :: VDeclLHS s -> ST s (TypeCheckResult TDeclLHS)
toDeclLHS (DeclLHS m name [] ) = do
  res1 <- toMeta m $ "Declaration Return Type " ++ name
  return $ res1 >>= (\m' -> Right $ DeclLHS m' name [])
toDeclLHS (DeclLHS m name ((an, am):args) ) = do
  res1 <- toMeta am $ "Declaration Argument: " ++ name ++ "." ++ an
  res2 <- toDeclLHS (DeclLHS m name args)
  return $ merge2TypeCheckResults res1 res2 >>= (\(am', DeclLHS m' _ args') -> Right $ DeclLHS m' name ((an, am'):args'))


-- toDecl :: VDecl s -> ST s (TypeCheckResult TDecl)
-- toDecl (Decl lhs expr) = do
--   res1 <- toDeclLHS lhs
--   res2 <- toExpr expr
--   return $ merge2TypeCheckResults res1 res2 >>= (\(lhs', expr') -> Right $ Decl lhs' expr')

toPrgm :: VPrgm s -> ST s (TypeCheckResult TPrgm)
toPrgm = undefined
-- toPrgm decls = do
--   res <- mapM toDecl decls
--   return $ mergeTypeCheckResults res
