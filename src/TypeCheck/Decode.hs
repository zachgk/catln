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
import           Control.Applicative
import           Data.Either
import           Data.Functor
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet          as S
import           Data.UnionFind.ST

import           Syntax
import           TypeCheck.Common

mergeTypeCheckResultsList :: [TypeCheckResult r] -> TypeCheckResult [r]
mergeTypeCheckResultsList res = case partitionEithers res of
  ([], rs) -> Right rs
  (ls, _)  -> Left $ concat ls

mergeTypeCheckResultsMap :: (Eq a, Hashable a) => H.HashMap a (TypeCheckResult b) -> TypeCheckResult (H.HashMap a b)
mergeTypeCheckResultsMap res = fmap H.fromList $ mergeTypeCheckResultsList $ map (\(a, typeCheckResultB) -> fmap (a,) typeCheckResultB) $ H.toList res

mergeTypeCheckResultsPair :: (TypeCheckResult a, TypeCheckResult b) -> TypeCheckResult (a, b)
mergeTypeCheckResultsPair (a, b) = case (a,b) of
  (Right a', Right b') -> Right (a', b')
  (a', b')             -> Left $ fromLeft [] a' ++ fromLeft [] b'

mergeTypeCheckResultsTriple :: (TypeCheckResult a, TypeCheckResult b, TypeCheckResult c) -> TypeCheckResult (a, b, c)
mergeTypeCheckResultsTriple (a, b, c) = case (a, b, c) of
  (Right a', Right b', Right c') -> Right (a', b', c')
  (a', b', c')             -> Left $ fromLeft [] a' ++ fromLeft [] b' ++ fromLeft [] c'

fromRawLeafType :: RawLeafType -> LeafType
fromRawLeafType (RawLeafType name ts) = LeafType name (fmap fromRawLeafType ts)

fromRawType :: RawType -> Maybe Type
fromRawType RawTopType = Nothing
fromRawType RawBottomType = Nothing
fromRawType (RawSumType ts) = Just $ SumType $ S.map fromRawLeafType ts

toMeta :: VarMeta s -> String -> ST s (TypeCheckResult Typed)
toMeta p name = do
  scheme <- descriptor p
  return $ case scheme of
    SCheckError s -> Left ["CheckError on " ++ name ++ ": " ++ s]
    SType ub lb -> case fromRawType ub of
      Nothing -> Left ["CheckError on " ++ show name ++ ": \ntLower Bound: " ++ show lb ++ "\n\tUpper Bound: " ++ show ub]
      Just t -> Right $ Typed t

toExpr :: VExpr s -> ST s (TypeCheckResult TExpr)
toExpr (CExpr m c) = do
  res <- toMeta m $ "Constant " ++ show c
  return $ res <&> (`CExpr` c)
toExpr (Tuple m name args) = do
  m' <- toMeta m $ "Tuple_" ++ name
  args' <- mapM toExpr args
  return $ (\(m'', args'') -> Tuple m'' name args'') <$> mergeTypeCheckResultsPair (m', mergeTypeCheckResultsMap args')

toObjectArg :: Name -> (Name, VarMeta s) -> ST s (TypeCheckResult (Name, Typed))
toObjectArg objName (name, m) = do
  m' <- toMeta m $ "Arg_" ++ objName ++ "." ++ name
  return $ case m' of
    Left m'' -> Left m''
    Right m'' -> Right (name, m'')

toObject :: VObject s -> ST s (TypeCheckResult TObject)
toObject (Object m name args) = do
  m' <- toMeta m $ "Object_" ++ name
  args' <- mapM (toObjectArg name) $ H.toList args
  return $ (\(m'', args'') -> Object m'' name args'') <$> mergeTypeCheckResultsPair (m', H.fromList <$> mergeTypeCheckResultsList args')

toArrow :: VArrow s -> ST s (TypeCheckResult TArrow)
toArrow (Arrow m obj expr) = do
  m' <- toMeta m "Arrow"
  obj' <- toObject obj
  expr' <- toExpr expr
  return $ (\(m'', obj'', expr'') -> Arrow m'' obj'' expr'') <$> mergeTypeCheckResultsTriple (m', obj', expr')

toPrgm :: VPrgm s -> ST s (TypeCheckResult TPrgm)
toPrgm (objects, arrows) = do
  objects' <- mapM toObject objects
  let objects'' = mergeTypeCheckResultsList objects'
  arrows' <- mapM toArrow arrows
  let arrows'' = mergeTypeCheckResultsList arrows'
  return $ mergeTypeCheckResultsPair (objects'', arrows'')
