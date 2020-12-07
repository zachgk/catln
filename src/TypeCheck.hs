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
{-# LANGUAGE NamedFieldPuns #-}

module TypeCheck where

import           CRes
import           TypeCheck.Common
import           TypeCheck.Encode
import           TypeCheck.Show
import           TypeCheck.Constrain (runConstraints)
import           TypeCheck.Decode

runConstraintsLimit :: Integer
runConstraintsLimit = 100

typecheckPrgm :: PPrgm -> CRes TPrgm
typecheckPrgm pprgm = (\(a, _, _) -> a) <$> typecheckPrgmWithTrace pprgm

typecheckPrgmWithTrace :: PPrgm -> CRes (TPrgm, VPrgm, TraceConstrain)
typecheckPrgmWithTrace pprgm@(_, classMap, _) = case aux of
  TypeCheckResult notes res -> CRes (map MkCNote notes) res
  TypeCheckResE notes -> CErr (map MkCNote notes)
  where
    aux = do
      let baseFEnv = makeBaseFEnv classMap
      (vprgm, env@FEnv{feCons}) <- fromPrgm baseFEnv pprgm
      env'@FEnv{feTrace} <- runConstraints runConstraintsLimit env feCons
      tprgm <- toPrgm env' vprgm
      return (tprgm, vprgm, feTrace)

traceTestPrgm :: PPrgm -> ([TypeCheckError], [(SPrgm, [SConstraint])])
traceTestPrgm pprgm@(_, classMap, _) = case aux of
    TypeCheckResult notes res -> (notes, res)
    TypeCheckResE notes -> (notes, [])
  where
    aux = do
      let baseFEnv = makeBaseFEnv classMap
      (vprgm, env@FEnv{feCons}) <- fromPrgm baseFEnv pprgm
      sprgm1 <- showPrgm env vprgm
      let scons1 = showConstraints env feCons
      _ <- runConstraints runConstraintsLimit env feCons
      sprgm2 <- showPrgm env vprgm
      return [(sprgm1, scons1), (sprgm2, [])]

showTestPrgm :: PPrgm -> TypeCheckResult SPrgm
showTestPrgm pprgm@(_, classMap, _) = do
  let baseFEnv = makeBaseFEnv classMap
  (vprgm, env) <- fromPrgm baseFEnv pprgm
  showPrgm env vprgm
