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
import Syntax.Prgm
import Utils

runConstraintsLimit :: Integer
runConstraintsLimit = 100

typecheckPrgm :: PPrgmGraphData -> CRes TPrgm
typecheckPrgm pprgms = do
  (a, _, _) <- failOnErrorNotes $ typecheckPrgmWithTrace pprgms
  return a

typecheckPrgmWithTrace :: PPrgmGraphData -> CRes (TPrgm, VPrgm, TraceConstrain)
typecheckPrgmWithTrace pprgms = typeCheckToRes $ do
  let pprgm@(_, classMap, _) = mergePrgms $ map fst3 $ graphToNodes pprgms
  let baseFEnv = makeBaseFEnv classMap
  (vprgm, env@FEnv{feCons}) <- fromPrgm baseFEnv pprgm
  env'@FEnv{feTrace} <- runConstraints runConstraintsLimit env feCons
  tprgm <- toPrgm env' vprgm
  return (tprgm, vprgm, feTrace)

traceTestPrgm :: PPrgmGraphData -> ([TypeCheckError], [(SPrgm, [SConstraint])])
traceTestPrgm pprgms = case aux of
    TypeCheckResult notes res -> (notes, res)
    TypeCheckResE notes -> (notes, [])
  where
    aux = do
      let pprgm@(_, classMap, _) = mergePrgms $ map fst3 $ graphToNodes pprgms
      let baseFEnv = makeBaseFEnv classMap
      (vprgm, env@FEnv{feCons}) <- fromPrgm baseFEnv pprgm
      sprgm1 <- showPrgm env vprgm
      let scons1 = showConstraints env feCons
      _ <- runConstraints runConstraintsLimit env feCons
      sprgm2 <- showPrgm env vprgm
      return [(sprgm1, scons1), (sprgm2, [])]

showTestPrgm :: PPrgmGraphData -> TypeCheckResult SPrgm
showTestPrgm pprgms = do
  let pprgm@(_, classMap, _) = mergePrgms $ map fst3 $ graphToNodes pprgms
  let baseFEnv = makeBaseFEnv classMap
  (vprgm, env) <- fromPrgm baseFEnv pprgm
  showPrgm env vprgm
