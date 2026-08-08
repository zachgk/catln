--------------------------------------------------------------------
-- |
-- Module    :  Eval.Runtime.WebDocs
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines the primitives backing the webdocs display
-- annotations declared in @stack/core/webdoc.ct@. They let a page be
-- defined in Catln (as a @Show/*@ value carrying its own data) rather
-- than by the webdocs web application fetching from a dedicated API
-- route.
--------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module Eval.Runtime.WebDocs where

import           CRes
import           Data.Aeson          (ToJSON)
import           Data.Aeson.Text     (encodeToLazyText)
import           Data.Graph
import qualified Data.HashMap.Strict as H
import           Data.List           (sort)
import qualified Data.Text.Lazy      as TL
import           Eval.Common
import           Semantics.Prgm
import           Semantics.Types     (ClassGraph (..), fromPartialName, mkName,
                                      relativeNameMatches)
import           Text.Printf
import           TypeCheck.Common    (filterTraceConstrain, flipTraceConstrain)
import           Utils

-- | The primitives backing the @Catln/Doc@ display annotations.
webDocsPrims :: [EPrim]
webDocsPrims = [listProgram, filterProgram, typeInfer, typeInferPnt, debugger]

-- | The argument of @/Catln/Doc/listProgram@ naming which stage to list, and the members of the
-- @ListProgramMode@ enum it takes. @stack\/core\/webdoc.ct@ is the source of truth for these; a
-- name that drifts from it is reported rather than defaulted, so the drift is visible instead of
-- being served as the wrong stage.
listProgramModeArg :: String
listProgramModeArg = "/mode"

desugarMode, typecheckMode, treeBuildMode :: String
desugarMode = "/Catln/Doc/Desugar"
typecheckMode = "/Catln/Doc/Typecheck"
treeBuildMode = "/Catln/Doc/TreeBuild"

-- | Backs @/Catln/Doc/listProgram@, the data carried by a @Show/ListProgram@ page. It renders the
-- program being evaluated as JSON for the webdocs listing to display, at the pipeline stage the
-- annotation's @mode@ names. The program comes from the evaluation 'Env', so the listing is scoped
-- to whatever program the annotation was evaluated against — the page holding the @#listProgram@
-- annotation and everything it reaches — at every stage alike.
--
-- The typecheck stage is the program the evaluation itself runs on, so it is read straight off the
-- 'TBEnv'. The other two are not reachable from an evaluation and are supplied by whoever ran those
-- stages, as 'evStagePrgms'.
listProgram :: EPrim
listProgram = EEnvPrim "listProgram" prim
  where
    prim Env{evTbEnv=TBEnv{tbPrgm}, evStagePrgms=StagePrgms{spDesPrgm, spTBPrgm}} args = case H.lookup listProgramModeArg args of
      Just (TupleVal mode _)
        | mode == desugarMode -> fromStage spDesPrgm
        | mode == typecheckMode -> Right $ encodeJson tbPrgm
        | mode == treeBuildMode -> fromStage spTBPrgm
      Just v -> Left $ printf "listProgram was given %s, which is not a ListProgramMode" (show v)
      Nothing -> Left $ printf "listProgram has no %s argument, only %s" listProgramModeArg (show $ sort $ H.keys args)
    fromStage (CRes _ prgm) = Right $ encodeJson prgm
    fromStage (CErr notes)  = Left $ prettyCNotes notes

-- | The argument of @/Catln/Doc/filterProgram@ naming which type or class to keep.
filterProgramNameArg :: String
filterProgramNameArg = "/name"

-- | Backs @/Catln/Doc/filterProgram@, the data carried by a @Show/TypePage@ page. It is
-- 'listProgram' narrowed to one name: the same program, filtered to the declarations and definitions
-- of a single type or class, rendered as JSON.
--
-- Only the typecheck stage is offered, because that is the stage the evaluation itself runs on and so
-- the one readable straight off the 'TBEnv'. A type page has no stage selector to need the others.
filterProgram :: EPrim
filterProgram = EEnvPrim "filterProgram" prim
  where
    prim Env{evTbEnv=TBEnv{tbPrgm}} args = case H.lookup filterProgramNameArg args of
      Just (StrVal name) -> Right $ encodeJson $ filterByType name tbPrgm
      Just v -> Left $ printf "filterProgram was given %s, which is not a String type name" (show v)
      Nothing -> Left $ printf "filterProgram has no %s argument, only %s" filterProgramNameArg (show $ sort $ H.keys args)

-- | Filters a program's object map and class map to only the declarations and definitions of a
-- particular type or class name. It is polymorphic in the program because it reads only names, so it
-- applies at whichever pipeline stage the caller holds.
filterByType :: (ExprClass e, MetaDat m, Show m, Show (e m)) => String -> Prgm e m -> Prgm e m
filterByType name (Prgm objMap (ClassGraph classGraph) _) = Prgm objMap' classGraph' []
  where
    objMap' = filterObjectMap (relativeNameMatches (mkName name) . oaObjPath) objMap
    classGraph' = ClassGraph $ graphFromEdges $ filter (\(_, n, subTypes) -> relativeNameMatches (mkName name) (fromPartialName n) || n `elem` subTypes) $ graphToNodes classGraph

-- | The argument of @/Catln/Doc/typeInferPnt@ naming which point of the trace to report on.
typeInferPntArg :: String
typeInferPntArg = "/pnt"

-- | Backs @/Catln/Doc/typeInfer@, the data carried by a @Show/TypeInfer@ page. It renders the type
-- inference of the program being evaluated as JSON: a type variable on every meta, paired with the
-- notes its typecheck produced.
--
-- The trace itself is deliberately not included. It records every scheme update of every point in
-- every epoch, so serializing it whole would dwarf the program it describes; a page reads one point
-- at a time through 'typeInferPnt'.
typeInfer :: EPrim
typeInfer = EEnvPrim "typeInfer" prim
  where
    prim Env{evStagePrgms=StagePrgms{spTypeInfer}} _ = case spTypeInfer of
      CRes notes (vprgm, _) -> Right $ encodeJson (vprgm, notes)
      CErr notes            -> Left $ prettyCNotes notes

-- | Backs @/Catln/Doc/typeInferPnt@, the data carried by a @Show/TypeInferPnt@ page. It renders the
-- part of the same trace that touches a single point: the constraints mentioning it and, per epoch,
-- the updates it received, flipped into the chronological order the page reads them in.
typeInferPnt :: EPrim
typeInferPnt = EEnvPrim "typeInferPnt" prim
  where
    prim Env{evStagePrgms=StagePrgms{spTypeInfer}} args = case (spTypeInfer, H.lookup typeInferPntArg args) of
      (CErr notes, _) -> Left $ prettyCNotes notes
      (CRes _ (_, trace), Just (IntVal pnt)) -> Right $ encodeJson $ flipTraceConstrain $ filterTraceConstrain trace (fromInteger pnt)
      (_, Just v) -> Left $ printf "typeInferPnt was given %s, which is not an Integer point" (show v)
      (_, Nothing) -> Left $ printf "typeInferPnt has no %s argument, only %s" typeInferPntArg (show $ sort $ H.keys args)

-- | The argument of @/Catln/Doc/debugger@ naming which function to run.
debuggerFunArg :: String
debuggerFunArg = "/fun"

-- | Backs @/Catln/Doc/debugger@, the data carried by a @Show/Debugger@ page. It runs a function of
-- the program being evaluated and renders that run as JSON: what it exited with, what it printed,
-- and the tree of what it evaluated.
--
-- Unlike the prims above this one does not read an artifact, it asks for one to be produced, so it
-- cannot compute its own answer: the evaluator is downstream of this module (@Eval@ imports
-- @Eval.Runtime@ imports this), and reaching back up to it would be a module cycle. The run therefore
-- arrives as a function on 'StagePrgms', supplied by whoever is above both.
debugger :: EPrim
debugger = EEnvPrim "debugger" prim
  where
    prim Env{evStagePrgms=StagePrgms{spDebugRun}} args = case H.lookup debuggerFunArg args of
      Just (StrVal fun) -> case spDebugRun fun of
        CRes _ run -> Right $ encodeJson run
        CErr notes -> Left $ prettyCNotes notes
      Just v -> Left $ printf "debugger was given %s, which is not a String function name" (show v)
      Nothing -> Left $ printf "debugger has no %s argument, only %s" debuggerFunArg (show $ sort $ H.keys args)

encodeJson :: (ToJSON p) => p -> Val
encodeJson = StrVal . TL.unpack . encodeToLazyText
