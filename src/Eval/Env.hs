--------------------------------------------------------------------
-- |
-- Module    :  Eval.Common
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines functions for working with the 'Env' as part
-- of "Eval".
--------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}

module Eval.Env where

import qualified Data.HashMap.Strict as H

import           CRes
import           Data.Hashable
import           Eval.Common
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           TreeBuild           (buildArrow)

evalStartEArrow :: Env -> PartialType -> AnyObjArr -> Args -> CRes (TExpr (), [TExpr ()], Args, Env)
evalStartEArrow env@Env{evExEnv, evTbEnv, evArgs, evCoverage, evTreebugOpen} srcType oa newArgs = do
  let env' = env{
                evArgs=newArgs
                , evTreebugOpen = oa : evTreebugOpen
                }
  case oa of
    Right oa' -> return (getOaArrExpr oa', oaAnnots oa', evArgs, env')
    Left oa' -> do
      let env'' = env'{evCoverage = H.insertWith (+) oa' 1 evCoverage}
      case H.lookup (srcType, oa') evExEnv of
        Just (tree, annots') -> return (tree, annots', evArgs, env'')
        Nothing -> do
          maybeArrow' <- buildArrow evTbEnv srcType oa'
          case maybeArrow' of
            Just (_, arrow'@(tree, annots')) -> do
              let env''' = env'' {evExEnv = H.insert (srcType, oa') arrow' evExEnv}
              return (tree, annots', evArgs, env''')
            Nothing -> evalError env $ printf "Failed to find arrow in eval resArrow: %s" (show oa')

evalEndEArrow :: Env -> Val -> Args -> Env
evalEndEArrow Env{evTreebugOpen} _ _ | null evTreebugOpen = error $ printf "Tried to evalEndEArrow with an empty treebug open"
evalEndEArrow env@Env{evTreebugOpen, evTreebugClosed, evCallStack} val newArgs = env {
  evTreebugOpen = tail evTreebugOpen,
  evTreebugClosed = pure $ (\oa -> EvalTreebugClosed oa val evTreebugClosed closedId) (head evTreebugOpen),
  evArgs = newArgs
                                                            }
  where
    closedId = take 10 (printf "%08x" (hash (evTreebugOpen, evTreebugClosed, evCallStack)))

evalEnvJoin :: Env -> Env -> Env
evalEnvJoin (Env objMap classGraph args exEnv1 tbEnv callStack cov1 treebugOpen treebugClosed1) (Env _ _ _ exEnv2 _ _ cov2 _ treebugClosed2) = Env objMap classGraph args (H.union exEnv1 exEnv2) tbEnv callStack (H.unionWith (+) cov1 cov2) treebugOpen (treebugClosed1 ++ treebugClosed2)

evalEnvJoinAll :: Foldable f => f Env -> Env
evalEnvJoinAll = foldr1 evalEnvJoin

evalError :: Env -> String -> CRes a
evalError Env{evCallStack} msg = CErr [MkCNote $ EvalCErr evCallStack msg]

evalSetArgs :: Env -> Args -> Env
evalSetArgs env args' = env{evArgs=args'}

evalPush :: Env -> String -> Env
evalPush env@Env{evCallStack} c = env{evCallStack = c:evCallStack}

evalPop :: Env -> Env
evalPop env@Env{evCallStack} = case evCallStack of
  (_:stack') -> env{evCallStack=stack'}
  _          -> error "Popped empty evCallStack"

evalPopVal :: (a, Env) -> (a, Env)
evalPopVal (a, env) = (a, evalPop env)

evalResult :: Env -> EvalResult
evalResult Env{evCoverage, evTreebugClosed} = EvalResult evCoverage evTreebugClosed
