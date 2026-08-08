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
import qualified Data.HashMap.Strict as H
import           Data.List           (sort)
import qualified Data.Text.Lazy      as TL
import           Eval.Common
import           Text.Printf

-- | The primitives backing the @Catln/Doc@ display annotations.
webDocsPrims :: [EPrim]
webDocsPrims = [listProgram]

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
        | mode == typecheckMode -> Right $ encodePrgm tbPrgm
        | mode == treeBuildMode -> fromStage spTBPrgm
      Just v -> Left $ printf "listProgram was given %s, which is not a ListProgramMode" (show v)
      Nothing -> Left $ printf "listProgram has no %s argument, only %s" listProgramModeArg (show $ sort $ H.keys args)
    fromStage (CRes _ prgm) = Right $ encodePrgm prgm
    fromStage (CErr notes)  = Left $ prettyCNotes notes
    encodePrgm :: (ToJSON p) => p -> Val
    encodePrgm = StrVal . TL.unpack . encodeToLazyText
