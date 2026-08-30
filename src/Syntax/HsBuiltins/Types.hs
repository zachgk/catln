--------------------------------------------------------------------
-- |
-- Module    :  Syntax.HsBuiltins.Types
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- The types the compiler declares for itself, and the list the builtins page is built from.
--
-- A type here is a Haskell type whose Catln declaration, and whose conversions to and from an
-- evaluated value, are all read off its own shape by "Syntax.HsBuiltins.Derive". Adding one is
-- deriving 'GHC.Generics.Generic', writing @instance CtType T@, and naming it in 'hsBuiltinTypes' —
-- which is what puts its declaration, and the pages that declaration needs, onto the page.
--
-- The types live here rather than in "Eval.Common" beside 'Eval.Common.Val', which is where
-- 'BuiltFile' was. The instance has to see both the value and the syntax, and this is the module
-- that is below neither — "Eval.Common" cannot import the derivation, because the derivation is
-- written in terms of 'Eval.Common.Val'. Reading a value back is what the instance IS, so a reader
-- written next to the type would be a second reading of the same declaration, which is the thing
-- this exists to stop.
--------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Syntax.HsBuiltins.Types where

import           CtConstants              (buildFilePath, catlnResultFiles,
                                           catlnResultPath, runtimeArgKey)
import qualified Data.HashMap.Strict      as H
import           Data.Proxy               (Proxy (..))
import           Eval.Common              (Val)
import           GHC.Generics             (Generic)
import           Semantics.Val            hiding (Val)
import           Syntax.HsBuiltins.Derive
import           Text.Printf

-- | One file a build produces.
--
-- The Haskell name is not the Catln one: a @BuiltFile@ here is a @\/Catln\/BuildFile@ there, which
-- is what 'ctPath' is overridable for. Renaming either to match the other would be renaming a type
-- for the sake of a derivation.
data BuiltFile = BuiltFile {
    bfPath     :: FilePath,
    bfContents :: String
  } deriving (Eq, Show, Generic)

instance CtType BuiltFile where
  ctPath _ = buildFilePath
  ctDoc _ = [
    "One file a build produces, at the [path] it is produced at.",
    "A path rather than a name and a place in a tree, so that a result is a flat list of files and every entry of that list is the same kind of thing. The nested form — a file or a directory of further entries, which is [/Data/FileOrDir] — is the shape this wants, and is not yet a shape the compiler can build: a list takes its element type from its first entry, so a directory holding both a file and a subdirectory is a list whose second entry does not fit its first. See the comment on [/Data/FileOrDir].",
    "[path] is written with @/@ between its parts, and names a place inside the build and nowhere else. A path leading out of it is refused when the build is written rather than here, since a result may be built long before anything writes it.",
    "It is declared from Haskell rather than in Catln because the compiler reads a build result back out of an evaluation itself, in [valResultFiles]. Declared in a `.ct` file, what it is called there and what it is read as here would be two things to keep in step."
            ]

-- | Every type the builtins page declares, each as what it adds to a page.
--
-- Functions rather than the types themselves, since a list of types is a list of different types
-- and would need a wrapper whose only job is to be unwrapped again.
hsBuiltinTypes :: [CtPage -> CtPage]
hsBuiltinTypes = [ctPageInto (Proxy :: Proxy BuiltFile)]

-- | The page every builtin type together makes: what it must import, and what it declares.
hsBuiltinPage :: CtPage
hsBuiltinPage = foldl (flip ($)) emptyCtPage hsBuiltinTypes

-- | The files of an evaluated @\/Catln\/CatlnResult@, or why the value is not one.
--
-- This is the one place the shape of a build result is known, and every caller that has to look
-- inside one goes through it. Written once because it was written three times before, each copy
-- knowing the argument keys for itself, and one of them ('Main.xBuild') had them wrong — it looked
-- up @name@ where an evaluated value keys @\/name@, so every build crashed on a 'Data.Maybe.fromJust'
-- and nothing noticed, since the tests call the evaluation and never the command.
--
-- What is left written by hand is the @CatlnResult@ around them, which is still declared in
-- @stack\/core\/result.ct@. Each file inside is read by 'fromCt', off the same declaration the
-- builtins page carries — so the keys that were wrong before are no longer written down at all.
valResultFiles :: Val -> Either String [BuiltFile]
valResultFiles (TupleVal name args) | name == catlnResultPath = case H.lookup (runtimeArgKey catlnResultFiles) args of
  Just files -> traverse fromCt =<< valList files
  Nothing -> Left $ printf "A %s has no %s, only %s" catlnResultPath (runtimeArgKey catlnResultFiles) (show $ H.keys args)
valResultFiles val = Left $ printf "%s is not a %s" (show val) catlnResultPath

-- | A build result of a single file, as the value one would have been built as. The inverse of
-- 'valResultFiles' over one file, and its file is built by 'toCt' for that function's reason.
oneFileResult :: FilePath -> String -> Val
oneFileResult path contents = TupleVal catlnResultPath $ H.singleton (runtimeArgKey catlnResultFiles) $ valListOf [toCt $ BuiltFile path contents]
