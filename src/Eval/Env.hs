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
import           Data.UUID           (UUID)
import           Eval.Common
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           TreeBuild           (buildArrow, withArgArrows)

-- | Enters an arrow for a source. An arm is built for the source in the environment holding the
-- arrows of the closures the source's arguments are ('withArgArrows'): a higher-order slot holds
-- the closure's input object, and the arm's body reaches what the closure returns through its arrow.
evalStartEArrow :: PartialType -> [ObjArr Expr EvalMetaDat] -> AnyObjArr -> Args -> EvalM (TExpr EvalMetaDat, [TExpr EvalMetaDat], Args)
evalStartEArrow srcType closures oa newArgs = do
  env@Env{evExEnv, evTbEnv, evClosureEnvs, evArgs, evCallIds, evNextCallId, evInstruments} <- get
  let callId = show evNextCallId
  let env' = env{
                evArgs=newArgs
                , evCallIds = callId : evCallIds
                , evNextCallId = evNextCallId + 1
                , evInstruments = evalInstArrowEnter callId oa evInstruments
                }
  let cacheKey' oa' = (srcType, [ getMetaType arrM | ObjArr{oaArr=Just (_, arrM)} <- closures ], oa')
  case oa of
    Right oa' -> do
      put env'
      return (getOaArrExpr oa', oaAnnots oa', evArgs)
    Left oa' -> do
      let cacheKey = cacheKey' oa'
      case H.lookup cacheKey evExEnv of
        Just (tree, annots') -> do
          put env'
          return (tree, annots', evArgs)
        Nothing -> do
          let closuresKey = [ (getExprType (oaObjExpr c), getMetaType arrM) | c@ObjArr{oaArr=Just (_, arrM)} <- closures ]
          let (buildEnv, closureEnvs') = case closures of
                [] -> (evTbEnv, evClosureEnvs)
                _  -> case H.lookup closuresKey evClosureEnvs of
                  Just e  -> (e, evClosureEnvs)
                  Nothing -> let e = withArgArrows evTbEnv closures in (e, H.insert closuresKey e evClosureEnvs)
          maybeArrow' <- liftCRes $ buildArrow buildEnv srcType oa'
          case maybeArrow' of
            Just (_, arrow'@(tree, annots')) -> do
              let env'' = env' {evExEnv = H.insert cacheKey arrow' evExEnv, evClosureEnvs = closureEnvs'}
              put env''
              return (tree, annots', evArgs)
            Nothing -> evalError $ printf "Failed to find arrow in eval resArrow: %s" (show oa')

evalEndEArrow :: Val -> Val -> Args -> Env -> Env
evalEndEArrow _ _ _ Env{evCallIds=[]} = error "Tried to evalEndEArrow with no call open"
evalEndEArrow input val newArgs env@Env{evCallIds=callId:callIds, evInstruments} = env {
  evCallIds = callIds,
  evInstruments = evalInstArrowExit callId input val evInstruments,
  evArgs = newArgs
                                                                                       }

-- | Records a @#debug@ against the call the evaluation is currently inside.
--
-- The innermost open call, because that is the definition the annotation is written on: an
-- annotation is evaluated by 'Eval.evalObjArrCaptured' between the arrow being entered and its body
-- being run, so there is always one open. That is stated rather than worked around, as
-- 'evalEndEArrow' states it: a record with no call to belong to would be a broken evaluation rather
-- than a record to place somewhere else.
evalRecordDebug :: CodeRange -> Val -> Env -> Env
evalRecordDebug _ _ Env{evCallIds=[]} = error "Tried to record a #debug with no call open"
evalRecordDebug pos val env@Env{evCallIds=callId:_, evInstruments} = env {
  evInstruments = evalInstDebug callId pos val evInstruments
                                                                     }

-- | An expression is about to run its call tree ('onExprStart').
evalStartExpr :: Env -> Env
evalStartExpr env@Env{evInstruments} = env{evInstruments = evalInstExprStart evInstruments}

-- | The expression of the innermost 'evalStartExpr' has run ('onExprEnd').
evalEndExpr :: CodeRange -> [UUID] -> Val -> Env -> Env
evalEndExpr pos ids val env@Env{evInstruments} = env{evInstruments = evalInstExprEnd pos ids val evInstruments}

-- | An annotation of the call being evaluated is about to be evaluated ('onAnnotStart').
evalStartAnnot :: Env -> Env
evalStartAnnot env@Env{evInstruments} = env{evInstruments = evalInstAnnotStart evInstruments}

-- | The annotation of the innermost 'evalStartAnnot' has been evaluated ('onAnnotEnd').
evalEndAnnot :: Meta EvalMetaDat -> Val -> Env -> Env
evalEndAnnot m val env@Env{evInstruments} = env{evInstruments = evalInstAnnotEnd (getMetaId m) (getMetaPos m) val evInstruments}

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

-- | What the instruments watching an evaluation made of it, read once it is over.
evalEnvResults :: Env -> EvalInstrumentResults
evalEnvResults Env{evInstruments} = evalInstrumentResults evInstruments
