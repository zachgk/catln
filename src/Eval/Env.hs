--------------------------------------------------------------------
-- |
-- Module    :  Eval.Common
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}

module Eval.Env where

import qualified Data.HashMap.Strict as H

import Eval.Common
import TreeBuild (buildArrow)
import           CRes
import Syntax.Types
import Text.Printf

evalStartEArrow :: Env -> EObject -> EArrow -> Args -> CRes (ResArrowTree EPrim, [ResArrowTree EPrim], Args, Env)
evalStartEArrow env@Env{evExEnv, evTbEnv, evArgs, evCoverage, evTreebugOpen} obj arr newArgs = do
  let env' = env{
                evArgs=newArgs
                , evCoverage = H.insertWith (+) arr 1 evCoverage
                , evTreebugOpen = EvalTreebugOpen obj arr : evTreebugOpen
                }
  case H.lookup arr evExEnv of
    Just (tree, annots) -> return (tree, annots, evArgs, env')
    Nothing -> do
      maybeArrow' <- buildArrow evTbEnv obj arr
      case maybeArrow' of
        Just (_, arrow'@(tree, annots)) -> do
          let env'' = env' {evExEnv = H.insert arr arrow' evExEnv}
          return (tree, annots, evArgs, env'')
        Nothing -> evalError env $ printf "Failed to find arrow in eval resArrow: %s" (show arr)

evalEndEArrow :: Env -> Val -> Args -> Env
evalEndEArrow Env{evTreebugOpen} _ _ | null evTreebugOpen = error $ printf "Tried to evalEndEArrow with an empty treebug open"
evalEndEArrow env@Env{evTreebugOpen, evTreebugClosed} val newArgs = env {
  evTreebugOpen = tail evTreebugOpen,
  evTreebugClosed = pure $ (\(EvalTreebugOpen obj arr) -> EvalTreebugClosed obj arr val evTreebugClosed) (head evTreebugOpen),
  evArgs = newArgs
                                                            }

evalEnvJoin :: Env -> Env -> Env
evalEnvJoin (Env objMap classMap args exEnv1 tbEnv callStack cov1 treebugOpen treebugClosed1) (Env _ _ _ exEnv2 _ _ cov2 _ treebugClosed2) = Env objMap classMap args (H.union exEnv1 exEnv2) tbEnv callStack (H.unionWith (+) cov1 cov2) treebugOpen (treebugClosed1 ++ treebugClosed2)

evalEnvJoinAll :: Foldable f => f Env -> Env
evalEnvJoinAll = foldr1 evalEnvJoin

evalError :: Env -> String -> CRes a
evalError Env{evCallStack} msg = CErr [MkCNote $ EvalCErr evCallStack msg]

evalSetArgs :: Env -> H.HashMap ArgName Val -> Env
evalSetArgs env args' = env{evArgs=args'}

evalPush :: Env -> String -> Env
evalPush env@Env{evCallStack} c = env{evCallStack = c:evCallStack}

evalPop :: Env -> Env
evalPop env@Env{evCallStack} = case evCallStack of
  (_:stack') -> env{evCallStack=stack'}
  _ -> error "Popped empty evCallStack"

evalPopVal :: (a, Env) -> (a, Env)
evalPopVal (a, env) = (a, evalPop env)

evalResult :: Env -> EvalResult
evalResult Env{evCoverage, evTreebugClosed} = EvalResult evCoverage evTreebugClosed
