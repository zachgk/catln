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

import           TypeCheck.Common
import           TypeCheck.Encode
import           TypeCheck.Show
import           TypeCheck.Constrain (runConstraints)
import           TypeCheck.Decode

runConstraintsLimit :: Integer
runConstraintsLimit = 100

typecheckPrgm :: PPrgm -> TypeCheckResult TPrgm
typecheckPrgm pprgm@(_, classMap) = do
  let baseFEnv = makeBaseFEnv classMap
  (vprgm, env@(FEnv _ cons _ _)) <- fromPrgm baseFEnv pprgm
  env' <- runConstraints runConstraintsLimit env cons
  toPrgm env' vprgm

traceTestPrgm :: PPrgm -> ([TypeCheckError], [(SPrgm, [SConstraint])])
traceTestPrgm pprgm@(_, classMap) = do
  let baseFEnv = makeBaseFEnv classMap
  case fromPrgm baseFEnv pprgm of
    TypeCheckResult notes (vprgm, env@(FEnv _ cons _ _)) -> do
      let sprgm1 = showPrgm env vprgm
      let scons1 = showConstraints env cons
      case runConstraints runConstraintsLimit env cons of
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
