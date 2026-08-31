--------------------------------------------------------------------
-- |
-- Module    :  Eval.Runtime.Prgm
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Handing a program its own program.
--
-- Everything else in this project declared what a program IS. This is what gives one to a program:
-- the compiler's own 'Semantics.Prgm.Prgm', converted by the same reading that declared it, so what
-- a Catln program is handed and what the compiler holds cannot come apart.
--
-- Narrowed to a name rather than handed over whole. 'Eval.Common.getValType' builds a
-- 'Semantics.Types.PartialType' by walking every argument of a value, and it is asked for one on
-- every dispatch that touches the value — so a whole program, standard library and all, would be
-- walked structurally over and over. A program asks for the name it is interested in, which is the
-- same shape @\/Catln\/Doc\/filterProgram@ already answers questions in and is bounded by what was
-- asked for rather than by how big the library is.
--------------------------------------------------------------------

module Eval.Runtime.Prgm where

import qualified Data.HashMap.Strict  as H
import           Eval.Common
import           Eval.Runtime.WebDocs (filterByType)
import           MapMeta              (clearMetaDat, mapMetaPrgm)
import           Semantics.CtType     (toCt)
import           Text.Printf

-- | The primitives backing the compiler's own program types.
prgmPrims :: [EPrim]
prgmPrims = [prgmFor]

-- | The argument of @\/Catln\/prgmFor@ naming what to be given.
prgmForNameArg :: String
prgmForNameArg = "/name"

-- | Backs @\/Catln\/prgmFor@: the program being evaluated, narrowed to one name, as a
-- @\/Catln\/Prgm@ a Catln program can read.
--
-- The program comes from the evaluation's own 'Eval.Common.TBEnv', so what is answered is the
-- program the asking page was compiled as part of — the page and everything it reaches.
--
-- 'MapMeta.clearMetaDat' narrows it to the metadata every stage shares, which is what
-- @instance CtType (Prgm Expr ())@ is written at. What that drops is the stage's own supplement,
-- so a program reads the same shape whichever stage it asked about.
prgmFor :: EPrim
prgmFor = EEnvPrim "prgmFor" prim
  where
    prim Env{evTbEnv=TBEnv{tbPrgm}} args = case H.lookup prgmForNameArg args of
      Just (StrVal name) -> Right $ toCt $ mapMetaPrgm clearMetaDat $ filterByType name tbPrgm
      Just v -> Left $ printf "prgmFor was given %s, which is not a name" (show v)
      Nothing -> Left $ printf "prgmFor has no %s argument, only %s" prgmForNameArg (show $ H.keys args)
