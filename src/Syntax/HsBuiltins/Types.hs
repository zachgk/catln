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
-- evaluated value, are all read off its own shape by "Semantics.CtType". Adding one is
-- deriving 'GHC.Generics.Generic', writing @instance CtType T@, and naming it in 'hsBuiltinTypes' —
-- which is what puts its declaration, and the pages that declaration needs, onto the page.
--
-- A type WHOSE HOME IS HERE is one the compiler supplies and nothing else owns — a build result is
-- the compiler's own idea, and there is no other module it belongs beside. That is the only reason
-- left for this module. It used to be a constraint rather than a choice: the derivation was written
-- in terms of 'Eval.Common.Val' and produced syntax, so an instance had to sit above both the
-- evaluator and the parser, and this was the module below neither. It no longer is — the derivation
-- reads a value polymorphically ("Semantics.Val") and describes a declaration rather than building
-- one ("Semantics.CtType"), so a type can carry its own instance wherever the type itself lives.
-- Which is what the compiler's own AST will do, rather than being mirrored here.
--------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Syntax.HsBuiltins.Types where

import           CtConstants         (buildFilePath, catlnResultFiles,
                                      catlnResultPath, runtimeArgKey)
import qualified Data.HashMap.Strict as H
import           Data.Proxy          (Proxy (..))
import           Eval.Common         (Val)
import           GHC.Generics        (Generic)
import           Semantics.CtType
import           Semantics.Prgm      (Hole, ObjectBasis)
import           Semantics.Types     (Constant, PartialKey, PartialName,
                                      PtArgMode, TypeVarLoc)
import           Semantics.Val       hiding (Val)
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
    "A path rather than a name and a place in a tree, so that a result is a flat list of files and every entry of that list is the same kind of thing. The nested form — a file or a directory of further entries, which is [/Data/FileOrDir] — is the shape this wants, and is a shape the compiler can build: a directory holding both a file and a subdirectory writes and walks, read by arms matching its two constructors. What a result is has stayed flat for its own reason rather than for want of the tree.",
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

-- | The compiler's own program, as types a Catln program can be handed.
--
-- A page of its own rather than more of 'hsBuiltinTypes', because of what the two cost. Everything
-- on the builtins page is compiled by every program — @stack\/core\/main.ct@ imports
-- @hsbuiltins()@ — and the AST is dozens of mutually recursive declarations where a build result is
-- two fields. @stack\/core\/result.ct@ records what a badly placed edge costs here: one import from
-- @core\/compile.ct@ to @core\/list.ct@ took @test\/Integration\/code\/io.ct@ from 203 seconds to
-- over 900. So a program pays for these by asking for them, with @import hsprgm()@, and a program
-- which does not ask is no slower than it was.
--
-- No page of the standard library may write that import. This page reaches @core\/list.ct@ and
-- whatever else its fields need, and the core reaching back into it is the cycle
-- @core\/result.ct@'s comment is about.
hsPrgmTypes :: [CtPage -> CtPage]
hsPrgmTypes = [
  ctPageInto (Proxy :: Proxy ObjectBasis),
  ctPageInto (Proxy :: Proxy Hole),
  ctPageInto (Proxy :: Proxy Constant),
  ctPageInto (Proxy :: Proxy PartialName),
  ctPageInto (Proxy :: Proxy PtArgMode),
  ctPageInto (Proxy :: Proxy TypeVarLoc),
  ctPageInto (Proxy :: Proxy PartialKey)
              ]

hsPrgmPage :: CtPage
hsPrgmPage = foldl (flip ($)) emptyCtPage hsPrgmTypes

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
