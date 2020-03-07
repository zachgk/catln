--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module TypeCheck where

import           Control.Monad
import           Control.Monad.ST
import           Data.Either
import           Data.Functor
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet          as S
import           Data.UnionFind.ST

import           Syntax
import           TypeCheck.Common
import           TypeCheck.Encode
import           TypeCheck.Show
import           TypeCheck.Constrain (runConstraints)
import           TypeCheck.Decode
import Text.Pretty.Simple (pString)

typecheckPrgm :: PPrgm -> TypeCheckResult TPrgm
typecheckPrgm pprgm = runST $ do
  baseFEnv <- makeBaseFEnv
  (vprgm, typeGraph, FEnv cons _ errs) <- fromPrgm baseFEnv pprgm
  case errs of
    [] -> do
      runConstraints typeGraph cons
      toPrgm vprgm
    _ -> return $ Left errs

traceTestPrgm :: PPrgm -> Either [TypeCheckError] [SPrgm]
traceTestPrgm pprgm = runST $ do
  baseFEnv <- makeBaseFEnv
  (vprgm, typeGraph, FEnv cons _ errs) <- fromPrgm baseFEnv pprgm
  case errs of
    [] -> do
      sprgm1 <- showPrgm vprgm
      runConstraints typeGraph cons
      sprgm2 <- showPrgm vprgm
      return $ Right [sprgm1, sprgm2]
    _ -> return $ Left errs

showTestPrgm :: PPrgm -> Either [TypeCheckError] SPrgm
showTestPrgm pprgm = runST $ do
  baseFEnv <- makeBaseFEnv
  (vprgm, typeGraph, FEnv _ _ errs) <- fromPrgm baseFEnv pprgm
  case errs of
    [] -> do
      sprgm <- showPrgm vprgm
      return $ Right sprgm
    _ -> return $ Left errs


testPrgm :: PPrgm
testPrgm = H.fromList [
  (Object (PreTyped (RawSumType $ S.singleton $ RawLeafType "one" H.empty)) "one" H.empty, [
              Arrow (PreTyped RawTopType) (CExpr (PreTyped rintType) (CInt 1))
                                                                                           ])
                      ]
