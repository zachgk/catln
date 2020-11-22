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
typecheckPrgm pprgm = fst <$> typecheckPrgmWithTrace pprgm

typecheckPrgmWithTrace :: PPrgm -> CRes (TPrgm, TraceConstrain)
typecheckPrgmWithTrace pprgm@(_, classMap) = case aux of
  TypeCheckResult notes res -> CRes (map MkCNote notes) res
  TypeCheckResE notes -> CErr (map MkCNote notes)
  where
    aux = do
      let baseFEnv = makeBaseFEnv classMap
      (vprgm, env@FEnv{feCons}) <- fromPrgm baseFEnv pprgm
      env'@FEnv{feTrace} <- runConstraints runConstraintsLimit env feCons
      tprgm <- toPrgm env' vprgm
      return (tprgm, feTrace)

traceTestPrgm :: PPrgm -> ([TypeCheckError], [(SPrgm, [SConstraint])])
traceTestPrgm pprgm@(_, classMap) = do
  let baseFEnv = makeBaseFEnv classMap
  case fromPrgm baseFEnv pprgm of
    TypeCheckResult notes (vprgm, env@FEnv{feCons}) -> do
      let sprgm1 = showPrgm env vprgm
      let scons1 = showConstraints env feCons
      case runConstraints runConstraintsLimit env feCons of
        TypeCheckResult notes2 env' -> do
          let sprgm2 = showPrgm env' vprgm
          let res = [(sprgm1, scons1), (sprgm2, [])]
          (notes ++ notes2, res)
        TypeCheckResE errs -> (notes ++ errs, [(sprgm1, scons1)])
    TypeCheckResE errs -> (errs, [])

showTestPrgm :: PPrgm -> TypeCheckResult SPrgm
showTestPrgm pprgm@(_, classMap) = do
  let baseFEnv = makeBaseFEnv classMap
  (vprgm, env) <- fromPrgm baseFEnv pprgm
  return $ showPrgm env vprgm
