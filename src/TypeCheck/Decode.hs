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
import           Debug.Trace                    ( trace )

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
  case m' of -- check for errors
    Right (Typed (SumType sumType)) | all (\(LeafType _ leafArgs) -> H.keysSet args' /= H.keysSet leafArgs) (S.toList sumType) -> return $ Left ["toExpr has mismatch in type and args for " ++ name ++ " ---meta--- " ++ show m' ++ " ---args--- " ++ show args']
    _ -> return $ (\(m'', args'') -> Tuple m'' name args'') <$> mergeTypeCheckResultsPair (m', mergeTypeCheckResultsMap args')

toCompAnnot :: VCompAnnot s -> ST s (TypeCheckResult TCompAnnot)
toCompAnnot (CompAnnot name args) = do
  args' <- mapM toExpr args
  return $ fmap (CompAnnot name) (mergeTypeCheckResultsMap args')

toArrow :: VArrow s -> ST s (TypeCheckResult TArrow)
toArrow (Arrow m annots maybeExpr) = do
  m' <- toMeta m "Arrow"
  annotsT <- mapM toCompAnnot annots
  let annots' = mergeTypeCheckResultsList annotsT
  case maybeExpr of
    Just expr -> do
      expr' <- toExpr expr
      return $ (\(m'', annots'', expr'') -> Arrow m'' annots'' (Just expr'')) <$> mergeTypeCheckResultsTriple (m', annots', expr')
    Nothing -> return $ (\(annots'', m'') -> Arrow m'' annots'' Nothing) <$> mergeTypeCheckResultsPair(annots', m')

toObjectArg :: Name -> (Name, VarMeta s) -> ST s (TypeCheckResult (Name, Typed))
toObjectArg objName (name, m) = do
  m' <- toMeta m $ "Arg_" ++ objName ++ "." ++ name
  return $ case m' of
    Left m'' -> Left m''
    Right m'' -> Right (name, m'')

toObject :: (VObject s, [VArrow s]) -> ST s (TypeCheckResult (TObject, [TArrow]))
toObject (Object m name args, arrows) = do
  m' <- toMeta m $ "Object_" ++ name
  args' <- mapM (toObjectArg name) $ H.toList args
  let object' = (\(m'', args'') -> Object m'' name args'') <$> mergeTypeCheckResultsPair (m', H.fromList <$> mergeTypeCheckResultsList args')
  arrows' <- mapM toArrow arrows
  let arrows'' = mergeTypeCheckResultsList arrows'
  return $ mergeTypeCheckResultsPair (object', arrows'')

toPrgm :: VPrgm s -> ST s (TypeCheckResult TPrgm)
toPrgm (objMap, classMap) = do
  objects' <- mapM toObject objMap
  let objMap = H.fromList <$> mergeTypeCheckResultsList objects'
  return $ mergeTypeCheckResultsPair (objMap, return classMap)
