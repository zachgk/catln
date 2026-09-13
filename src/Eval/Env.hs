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
import           Eval.Common
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           TreeBuild           (buildArrow)

evalStartEArrow :: PartialType -> AnyObjArr -> Args -> StateT Env CRes (TExpr EvalMetaDat, [TExpr EvalMetaDat], Args)
evalStartEArrow srcType oa newArgs = do
  env@Env{evExEnv, evTbEnv, evArgs, evCoverage, evTreebugOpen, evTreebugClosed, evTreebugNextId} <- get
  let env' = env{
                evArgs=newArgs
                -- The enclosing call's finished calls go aside with the arrow being entered, and this
                -- one starts with none of its own. 'evalEndEArrow' puts them back.
                , evTreebugOpen = (show evTreebugNextId, oa, evTreebugClosed) : evTreebugOpen
                , evTreebugClosed = []
                , evTreebugNextId = evTreebugNextId + 1
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
evalEndEArrow input val newArgs env@Env{evTreebugOpen, evTreebugClosed} = env {
  evTreebugOpen = tail evTreebugOpen,
  -- The call that just finished takes what finished INSIDE it as its children, and joins what the
  -- enclosing call had already finished. Appended rather than prepended, so the calls under a node
  -- are in the order the run made them.
  evTreebugClosed = (\(closedId, oa, siblings) -> siblings ++ [EvalTreebugClosed oa input val evTreebugClosed closedId Nothing]) (head evTreebugOpen),
  evArgs = newArgs
                                                            }

-- | Records a @#debug@ against the arrow the evaluation is currently inside.
--
-- The innermost open arrow, because that is the definition the annotation is written on: an
-- annotation is evaluated by 'Eval.evalObjArrCaptured' between the arrow being entered and its body
-- being run, so there is always one open. That is stated rather than worked around, as
-- 'evalEndEArrow' states it: a record with no call to belong to would be a broken evaluation rather
-- than a record to place somewhere else.
evalRecordDebug :: CodeRange -> Val -> Env -> Env
evalRecordDebug _ _ Env{evTreebugOpen} | null evTreebugOpen = error $ printf "Tried to record a #debug with an empty treebug open"
evalRecordDebug pos val env@Env{evTreebugOpen, evDebugs} = env {
  evDebugs = EvalDebug pos val ((\(openId, _, _) -> openId) $ head evTreebugOpen) : evDebugs
                                                               }

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
evalResult Env{evCoverage, evTreebugClosed, evDebugs} = EvalResult evCoverage evTreebugClosed (reverse evDebugs)
