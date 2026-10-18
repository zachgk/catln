--------------------------------------------------------------------
-- |
-- Module    :  Process
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module runs the processors of the annotations written on a page, which is what
-- @catln process@ does. A line processor updates its annotation; a file processor updates the text
-- of the page the annotation is written on.
--
-- It is above the evaluator, as each processor is an evaluation of the program the page was
-- compiled into, and beneath the compilation service, which writes and compiles the page again
-- each time a file processor changes it ('CtService.getProcessedWith').
--------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module Process where

import           Control.Monad.State
import           CRes
import           CtConstants
import qualified Data.HashMap.Strict as H
import           Data.List           (sortOn)
import           Data.Maybe          (listToMaybe)
import           Data.UUID           (UUID)
import           Eval                (evalBaseEnvWith, evalExpr, ioContextType,
                                      pageAnnots, rawPrgmAnnotIds)
import           Eval.Common
import           Eval.Env            (evalSetArgs)
import           Eval.ExprBuilder
import           Semantics           (exprWithMeta)
import           Semantics.Prgm
import           Semantics.TypeGraph (reachesTo)
import           Semantics.Types
import           Syntax.Ct.Formatter (formatRootPrgm)
import           Syntax.Ct.Prgm      (RawExpr)
import           Syntax.Ct.Results   (removeAnnotById, setAnnotsById,
                                      valToRawExpr)
import           Text.Printf
import           TreeBuild           (toTExprDest)

-- | The kinds of processor @catln process@ runs an annotation with.
--
-- A line processor (@\/Catln\/process{io}(annot)@, 'processFun') updates the annotation itself: the
-- annotation is written back as what it gives, and nothing else on the page changes. A file processor
-- (@\/Catln\/processFile{io}(annot, source, withoutAnnot)@, 'processFileFun') updates the page the
-- annotation is written in: it is given the text of the page and gives the text it is to become.
data ProcessKind = LineProcess | FileProcess
  deriving (Eq, Show)

-- | The call of a processor of an annotation, with the arguments beyond the annotation, in the io:
-- processing runs in an io context, so it is the call held in a 'ContextInStr' with the io. It is
-- positioned where the annotation is written.
--
-- It is the call a page makes of its annotation, and an arm asking for the page it is written in
-- (@callerPage@) is asking for that one, rather than for the library declaring the arm.
processCall :: String -> EExpr -> [(String, EExpr)] -> EExpr
processCall fun annot args = positioned $ (eVal ContextInStr `eApply` (contextValStr, positioned (foldl eApply (eVal fun `eApply` (processFunAnnot, annot)) args))) `eApplyM` processFunIO $ ioM
  where
    positioned e = exprWithMeta (getExprMeta e){getMetaPos = getMetaPos (getExprMeta annot)} e

-- | The destination the processor of a kind is to reach for an annotation, in the io it ends in: the
-- annotation updated, for a line processor, and the text of the page, for a file processor.
processDest :: ProcessKind -> EExpr -> Type
processDest LineProcess annot = ioContextType $ singletonType (partialVal $ exprPath annot){ptArgMode = PtArgAny}
processDest FileProcess _ = ioContextType strType

-- | Which kind of processor an annotation is processed by, if any: the kind with an arm in this
-- program taking the annotation to its destination ('processDest').
--
-- Asked closed-world, as 'evalShowAnnot' asks whether an annotation can be shown: whether this
-- program, as written, has an arm for it. An annotation with an arm of each kind is reported, as
-- there is no saying which it is to be processed by.
processKindOf :: Env -> EExpr -> CRes (Maybe ProcessKind)
processKindOf Env{evTypeEnv} annot = case filter reaches [LineProcess, FileProcess] of
  []     -> return Nothing
  [kind] -> return $ Just kind
  _      -> CErr [MkCNote $ GenCErr (getMetaPos $ getExprMeta annot) $ printf "%s has both a line processor (%s) and a file processor (%s), so there is no saying which processes it" (show $ exprPath annot) processFun processFileFun]
  where
    reaches kind = reachesTo evTypeEnv{tePrgmEnv=True} H.empty (getExprType $ processCall (kindFun kind) annot (kindArgs kind)) (processDest kind annot)
    kindFun LineProcess = processFun
    kindFun FileProcess = processFileFun
    kindArgs LineProcess = []
    kindArgs FileProcess = [(processFileSource, valToEExpr (StrVal "")), (processFileWithoutAnnot, valToEExpr (StrVal ""))]

-- | Evaluates the call of a processor of an annotation to its destination, run in the io @ioVal@, and
-- reads what it gives out of the context it answers with.
evalProcessCall :: Env -> Val -> [(PartialType, EObjArr)] -> ProcessKind -> EExpr -> [(String, EExpr)] -> String -> CResT IO Val
evalProcessCall env0@Env{evTbEnv} ioVal os kind annot args fun = do
  tree <- asCResT $ toTExprDest evTbEnv os (processCall fun annot args) (emptyMetaT $ processDest kind annot)
  res <- evalStateT (evalExpr tree) env
  -- What processing gives is its value, in the io processing left.
  case res of
    TupleVal n resArgs | n == ContextStr, Just v <- H.lookup contextValStr resArgs -> return v
    _ -> asCResT $ CErr [MkCNote $ GenCErr (getMetaPos (getExprMeta annot)) $ printf "Processing %s did not answer with it in an io, but with %s" (show annot) (show res)]
  where
    env = evalSetArgs (H.singleton processFunIO ioVal) env0{evAnnotObj = listToMaybe (map snd os)}

-- | What a line processor updates an annotation to: @\/Catln\/process{io}(annot)@ ('processFun').
evalProcessLine :: Env -> Val -> [(PartialType, EObjArr)] -> EExpr -> CResT IO Val
evalProcessLine env ioVal os annot = evalProcessCall env ioVal os LineProcess annot [] processFun

-- | What a file processor updates the text of the page an annotation is written in to:
-- @\/Catln\/processFile{io}(annot, source, withoutAnnot)@ ('processFileFun'), given the text of the
-- page as @source@ and the text of the page without the annotation as @withoutAnnot@.
evalProcessFile :: Env -> Val -> [(PartialType, EObjArr)] -> EExpr -> String -> String -> CResT IO String
evalProcessFile env ioVal os annot source withoutAnnot = do
  val <- evalProcessCall env ioVal os FileProcess annot args processFileFun
  case val of
    StrVal text -> return text
    _ -> asCResT $ CErr [MkCNote $ GenCErr (getMetaPos $ getExprMeta annot) $ printf "%s was processed by %s into %s, which is not the text of a page" (show $ exprPath annot) processFileFun (show val)]
  where
    args = [(processFileSource, valToEExpr (StrVal source)), (processFileWithoutAnnot, valToEExpr (StrVal withoutAnnot))]

-- | Where a pass of processing over a page ended ('evalProcessPage').
data ProcessStep
  -- | Every processor ran, and none changed the page. What is held is each annotation the line
  -- processors updated, by its meta ID.
  = ProcessDone (H.HashMap UUID (RawExpr ()))
  -- | A file processor changed the page into this text. The processors after it were not run, as
  -- they are to run on the page as it now is.
  | ProcessFileChanged String

-- | One pass of processing over the page at @file@: each annotation written on it which can be
-- processed ('processKindOf'), in the order it is written, top to bottom.
--
-- An annotation updated by a line processor is held to be written back. An annotation holding a
-- result has been processed by its line processor, and is left as it is: processing it again would
-- replace what was written for it with something else, and a result is removed to ask for it again.
--
-- A file processor is given the text of the page with the line processors before it applied, so
-- it sees and keeps what they wrote. Where it gives other text the pass ends there, as the page it
-- was compiled from is no longer the page; the page is to be written, compiled and processed again.
--
-- Each annotation is processed in an evaluation of its own, so one that fails is noted and the
-- rest are still processed. One processed by a line processor into an annotation holding a
-- 'processFailureText' was processed without failing but could not be answered, such as a question
-- asked of a model that is not configured: it is noted with why, and left as it was rather than
-- written back holding it.
evalProcessPage :: EvalPages -> Val -> FilePath -> EPrgm -> CResT IO ProcessStep
evalProcessPage pages ioVal file prgm = do
  EvalPage{epRaw=page} <- case H.lookup file pages of
    Just p  -> return p
    Nothing -> asCResT $ CErr [MkCNote $ GenCErr Nothing $ printf "There is no page %s to process" file]
  let env = evalBaseEnvWith pages Nothing prgm
  let order = H.fromList $ zip (rawPrgmAnnotIds page) [(0 :: Int)..]
  let annots = sortOn (\(_, a) -> H.lookup (annotId a) order) $ pageAnnots page prgm
  go env page H.empty annots
  where
    annotId = getMetaId . getExprMeta
    go _ _ updated [] = return $ ProcessDone updated
    go env page updated ((os, a):rest) = do
      kind <- noted $ asCResT $ processKindOf env a
      case kind of
        Just (Just LineProcess) | not (H.member (partialKey $ mkName resultAnnotText) (exprAppliedArgsMap a)) -> do
          val <- noted $ evalProcessLine env ioVal os a
          val' <- maybe (return Nothing) (answered a) val
          case val' of
            Just val'' -> do
              e <- case valToRawExpr val'' of
                Right e  -> return e
                Left err -> asCResT $ CErr [MkCNote $ GenCErr (getMetaPos $ getExprMeta a) $ printf "An annotation was processed to %s, which can not be written: %s" (show val'') err]
              go env page (H.insert (annotId a) e updated) rest
            Nothing -> go env page updated rest
        Just (Just FileProcess) -> do
          let page' = setAnnotsById updated page
          let source = formatRootPrgm page'
          text <- noted $ evalProcessFile env ioVal os a source (formatRootPrgm $ removeAnnotById (annotId a) page')
          case text of
            Just text' | text' /= source -> return $ ProcessFileChanged text'
            _                            -> go env page updated rest
        _ -> go env page updated rest

    -- What a processor gave, or 'Nothing' with why noted where it failed.
    noted :: CResT IO a -> CResT IO (Maybe a)
    noted act = do
      res <- lift $ runCResT act
      case res of
        CRes notes v -> asCResT (CRes notes ()) >> return (Just v)
        CErr notes   -> asCResT (CRes notes ()) >> return Nothing

    answered a val = case processFailure val of
      Just failure -> do
        asCResT $ CRes [MkCNote $ GenCNote (getMetaPos $ getExprMeta a) $ printf "The annotation was not processed: %s" failure] ()
        return Nothing
      Nothing -> return $ Just val
    processFailure (TupleVal _ args) = case H.lookup (runtimeArgKey processFailureText) args of
      Just (StrVal failure) -> Just failure
      _                     -> Nothing
    processFailure _ = Nothing
