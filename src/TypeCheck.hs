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
import           Data.UnionFind.ST

import           Syntax
import           TypeCheck.Common
import           TypeCheck.Encode
import           TypeCheck.Show
-- import           TypeCheck.Constrain
-- import           TypeCheck.Decode

-- typecheckPrgm :: PPrgm -> TypeCheckResult TPrgm
-- typecheckPrgm pprgm = runST $ do
--   baseFEnv <- makeBaseFEnv
--   (vprgm, FEnv cons _ errs) <- fromPrgm baseFEnv pprgm
--   case errs of
--     [] -> do
--       runConstraints cons
--       toPrgm vprgm
--     _ -> return $ Left errs

showTestPrgm :: PPrgm -> Either [TypeCheckError] SPrgm
showTestPrgm pprgm = runST $ do
  baseFEnv <- makeBaseFEnv
  (vprgm, FEnv cons _ errs) <- fromPrgm baseFEnv pprgm
  case errs of
    [] -> do
      sprgm <- showPrgm vprgm
      return $ Right sprgm
    _ -> return $ Left errs


testPrgm :: PPrgm
testPrgm = ([
               Object (PreTyped RawTopType) "one" []
            ],
            [
              Arrow (PreTyped RawTopType) "one" (CExpr (PreTyped rintType) (CInt 1))
            ])
