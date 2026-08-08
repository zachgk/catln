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

import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy  as TL
import           Eval.Common

-- | The primitives backing the @Catln/Doc@ display annotations.
webDocsPrims :: [EPrim]
webDocsPrims = [listProgram]

-- | Backs @/Catln/Doc/listProgram@, the data carried by a @Show/ListProgram@ page. It renders the
-- program being evaluated as JSON for the webdocs listing to display. The program comes from the
-- evaluation 'Env', so the listing is scoped to whatever program the annotation was evaluated
-- against — the page holding the @#listProgram@ annotation and everything it reaches.
listProgram :: EPrim
listProgram = EEnvPrim "listProgram" prim
  where
    prim Env{evTbEnv=TBEnv{tbPrgm}} _ = Right $ StrVal $ TL.unpack $ encodeToLazyText tbPrgm
