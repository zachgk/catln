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
import qualified Data.HashSet        as S

import           Control.Monad.State
import           CRes
import           Data.Maybe          (listToMaybe)
import           Data.UUID           (UUID)
import           Eval.Common
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           TreeBuild           (buildArrow)

evalStartEArrow :: PartialType -> AnyObjArr -> Args -> EvalM (TExpr EvalMetaDat, [TExpr EvalMetaDat], Args)
evalStartEArrow srcType oa newArgs = do
  env@Env{evExEnv, evTbEnv, evArgs, evCoverage, evTreebugOpen, evTreebugClosed, evTreebugClaimed, evTreebugNextId} <- get
  let env' = env{
                evArgs=newArgs
                -- The enclosing call's finished calls go aside with the arrow being entered, and this
                -- one starts with none of its own. 'evalEndEArrow' puts them back.
                , evTreebugOpen = (show evTreebugNextId, oa, evTreebugClosed, evTreebugClaimed) : evTreebugOpen
                , evTreebugClosed = []
                , evTreebugClaimed = S.empty
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
          maybeArrow' <- liftCRes $ buildArrow evTbEnv srcType oa'
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
  evTreebugClosed = (\(closedId, oa, siblings, _) -> siblings ++ [EvalTreebugClosed oa input val evTreebugClosed closedId Nothing]) (head evTreebugOpen),
  evTreebugClaimed = (\(_, _, _, claimed) -> claimed) (head evTreebugOpen),
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
  evDebugs = EvalDebug pos val ((\(openId, _, _, _) -> openId) $ head evTreebugOpen) : evDebugs
                                                               }

-- | Where the evaluation is in the tree: the call it is inside, and how many calls have finished in
-- it so far. Taken before evaluating something whose calls are to be told apart from the ones before
-- it, and handed back to 'evalClosedSince' afterwards.
evalTreebugMark :: Env -> (Maybe String, Int)
evalTreebugMark Env{evTreebugOpen, evTreebugClosed} = ((\(openId, _, _, _) -> openId) <$> listToMaybe evTreebugOpen, length evTreebugClosed)

-- | The ids of the calls that finished in the current call since a mark was taken, in the order they
-- were entered. Every call entered in between has closed again by then, and closed into the current
-- call's level, so those are exactly the ones past the count the mark took.
evalClosedSince :: Int -> Env -> [String]
evalClosedSince before Env{evTreebugClosed} = map (\(EvalTreebugClosed _ _ _ _ closedId _) -> closedId) $ drop before evTreebugClosed

-- | Records an expression rewritten through calls ('EvalExprRun'), from the mark taken before its
-- call tree was run. Its steps are the calls made since then that no expression evaluated inside it
-- has already claimed ('evTreebugClaimed').
evalRecordExpr :: (Maybe String, Int) -> CodeRange -> [UUID] -> Val -> Env -> Env
evalRecordExpr (node, before) pos ids val env@Env{evExprRuns, evTreebugClaimed} = env{
    evExprRuns = EvalExprRun node pos ids steps val : evExprRuns
  , evTreebugClaimed = foldr S.insert evTreebugClaimed steps
  }
  where steps = filter (not . (`S.member` evTreebugClaimed)) (evalClosedSince before env)

-- | Records an annotation of the call being evaluated ('EvalAnnotRun'), from the mark taken before
-- it was evaluated. There is always a call open, as for 'evalRecordDebug': an annotation of a
-- definition is evaluated inside a call of it.
evalRecordAnnot :: (Maybe String, Int) -> Meta EvalMetaDat -> Val -> Env -> Env
evalRecordAnnot (Nothing, _) _ _ _ = error "Tried to record an annotation with an empty treebug open"
evalRecordAnnot (Just node, before) m val env@Env{evAnnotRuns} = env{
  evAnnotRuns = EvalAnnotRun node (getMetaId m) (getMetaPos m) val (evalClosedSince before env) : evAnnotRuns
                                                                    }

evalError :: String -> EvalM v
evalError msg = do
  stack <- gets evCallStack
  liftCRes $ CErr [MkCNote $ EvalCErr stack msg]

evalSetArgs :: Args -> Env -> Env
evalSetArgs args' env = env{evArgs=args'}

evalPush :: String -> Env -> Env
evalPush c env@Env{evCallStack} = env{evCallStack = c:evCallStack}

evalPop :: Env -> Env
evalPop env@Env{evCallStack} = case evCallStack of
  (_:stack') -> env{evCallStack=stack'}
  _          -> error "Popped empty evCallStack"

withEvalPush :: String -> EvalM v -> EvalM v
withEvalPush c f = do
  modify $ evalPush c
  res <- f
  modify evalPop
  return res

evalResult :: Env -> EvalResult
evalResult Env{evCoverage, evTreebugClosed, evDebugs, evExprRuns, evAnnotRuns} = EvalResult evCoverage evTreebugClosed (reverse evDebugs) (reverse evExprRuns) (reverse evAnnotRuns)
