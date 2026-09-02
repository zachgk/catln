--------------------------------------------------------------------
-- |
-- Module    :  Eval.Runtime.Import
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Reading a file, as a primitive an importer declared in Catln is backed by.
--
-- An importer is a declaration in a @.ct@ file, and what it answers with is a
-- @\/Catln\/ImportResult@. Nothing says the body of one has to be written in Catln: a declaration
-- carrying @#runtime@ is answered by a primitive here, which is how the compiler's own readers are
-- reached from a declaration that a program can name.
--
-- That is what the Python importer is. Its declaration moves into @stack\/python\/importer.ct@ and
-- its reading stays where it was, in "Syntax.Python.Parser" — so what changes is which of the
-- compiler's tables the importer is found in, and not one line of how a Python file is read.
--------------------------------------------------------------------

module Eval.Runtime.Import where

import           Control.Exception    (SomeException, evaluate, try)
import           CtConstants          (runtimeArgKey)
import           Data.Hashable        (hash)
import qualified Data.HashMap.Strict  as H
import           Eval.Common
import           Semantics.CtType     (toCt)
import           Syntax.Ct.Prgm       (ParsedImport (..))
import           Syntax.Python.Parser (pyParseFile)
import           System.IO.Unsafe     (unsafePerformIO)
import           Text.Printf

-- | The primitives an importer declared in Catln can be backed by.
importPrims :: [EPrim]
importPrims = [pyImport]

-- | The argument every importer takes: the path of the file to read.
--
-- Named here because two things have to agree on it — the declaration written in the @.ct@ file, and
-- the call 'CtService.ctssImporterEnv' builds — and a third would be a third place to keep in step.
importPathArg :: String
importPathArg = "path"

-- | Backs the Python importer: the file at @path@, read as the page it holds.
--
-- The 'unsafePerformIO' is what lets a primitive read a file at all, and is the same trade
-- 'Eval.Runtime.WebDocs.askService' and 'Eval.evalDebugPrgm' already make and document. What makes
-- it defensible here is narrower than either: this reads one file and writes nothing, so the only
-- thing an evaluation can observe is the contents of a path it named itself.
--
-- Forced before it is returned. A lazy value carrying an unperformed read would be forced by
-- whoever reads it, and that is 'CtService.buildSCCPipeline' inside @modifyMVar_@ — the rule
-- 'Eval.Common.CtServiceApi' states, and the reason 'CtService.ctssImporterEnv' forces again on its
-- own side.
--
-- A file that cannot be read is reported rather than raised, so that a program importing something
-- that is not there fails as a compile error naming the path.
pyImport :: EPrim
pyImport = EPrim "pyImport" prim
  where
    prim args = case H.lookup (runtimeArgKey importPathArg) args of
      Just (StrVal path) -> unsafePerformIO $ do
        read' <- try $ do
          parsed <- pyParseFile path
          _ <- evaluate (hash $ piPrgm parsed)
          return parsed
        return $ case read' of
          Left e -> Left $ printf "Could not read the Python file %s: %s" path (show (e :: SomeException))
          Right parsed -> Right $ toCt parsed
      Just v -> Left $ printf "The Python importer was given %s as a path, which is not a name" (show v)
      Nothing -> Left $ printf "The Python importer has no %s argument, only %s" (runtimeArgKey importPathArg) (show $ H.keys args)
