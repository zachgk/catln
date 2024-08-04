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

import           Control.Monad.State
import           CRes
import           Data.Hashable
import           Eval.Common
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           TreeBuild           (buildArrow)

evalStartEArrow :: PartialType -> AnyObjArr -> Args -> StateT Env CRes (TExpr EvalMetaDat, [TExpr EvalMetaDat], Args)
evalStartEArrow srcType oa newArgs = do
  env@Env{evExEnv, evTbEnv, evArgs, evCoverage, evTreebugOpen} <- get
  let env' = env{
                evArgs=newArgs
                , evTreebugOpen = oa : evTreebugOpen
                }
  case oa of
    Right oa' -> do
      put env'
      return (getOaArrExpr oa', oaAnnots oa', evArgs)
    Left oa' -> do
      let env'' = env'{evCoverage = H.insertWith (+) oa' 1 evCoverage}
      case H.lookup (srcType, oa') evExEnv of
        Just (tree, annots') -> do
          put env''
          return (tree, annots', evArgs)
        Nothing -> do
          maybeArrow' <- lift $ buildArrow evTbEnv srcType oa'
          case maybeArrow' of
            Just (_, arrow'@(tree, annots')) -> do
              let env''' = env'' {evExEnv = H.insert (srcType, oa') arrow' evExEnv}
              put env'''
              return (tree, annots', evArgs)
            Nothing -> evalError $ printf "Failed to find arrow in eval resArrow: %s" (show oa')

evalEndEArrow :: Val -> Val -> Args -> Env -> Env
evalEndEArrow _ _ _ Env{evTreebugOpen} | null evTreebugOpen = error $ printf "Tried to evalEndEArrow with an empty treebug open"
evalEndEArrow input val newArgs env@Env{evTreebugOpen, evTreebugClosed, evCallStack} = env {
  evTreebugOpen = tail evTreebugOpen,
  evTreebugClosed = pure $ (\oa -> EvalTreebugClosed oa input val evTreebugClosed closedId) (head evTreebugOpen),
  evArgs = newArgs
                                                            }
  where
    closedId = take 10 (printf "%08x" (hash (evTreebugOpen, evTreebugClosed, evCallStack)))

evalError :: String -> StateT Env CRes v
evalError msg = do
  stack <- gets evCallStack
  lift $ CErr [MkCNote $ EvalCErr stack msg]

evalSetArgs :: Args -> Env -> Env
evalSetArgs args' env = env{evArgs=args'}

evalPush :: String -> Env -> Env
evalPush c env@Env{evCallStack} = env{evCallStack = c:evCallStack}

evalPop :: Env -> Env
evalPop env@Env{evCallStack} = case evCallStack of
  (_:stack') -> env{evCallStack=stack'}
  _          -> error "Popped empty evCallStack"

withEvalPush :: String -> StateT Env CRes v -> StateT Env CRes v
withEvalPush c f = do
  modify $ evalPush c
  res <- f
  modify evalPop
  return res

evalResult :: Env -> EvalResult
evalResult Env{evCoverage, evTreebugClosed} = EvalResult evCoverage evTreebugClosed
