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
--------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Syntax.HsBuiltins.Types where

import           Data.Proxy               (Proxy (..))
import           GHC.Generics             (Generic)
import           Syntax.HsBuiltins.Derive

-- | A stand-in builtin, until there is a real one.
--
-- It is here to be a type the page declares and an evaluation converts, and so to be the one thing
-- a test can check a derived mapping against from end to end: the declaration is typechecked
-- because the standard library imports the page it is on, and the conversions are checked against
-- the same declaration rather than against a second reading of it.
--
-- Its fields are one of each kind of thing the core declares a type for, so that the walk over a
-- record's fields is visible in what the page says rather than merely exercised. The last two are
-- there for what they cost the page as well as for what they declare: a parameterized field is
-- written @\/Data\/List[$T -> \/Data\/String]@, and its element type is what puts @core\/list.ct@ in
-- 'Syntax.HsBuiltins.Parser.hsBuiltinsImports' without anyone naming it there.
data Sample = Sample {
    sampleName    :: String,
    sampleCount   :: Integer,
    sampleEnabled :: Bool,
    sampleNote    :: Maybe String,
    sampleTags    :: [String]
  } deriving (Eq, Show, Generic)

instance CtType Sample where
  ctDoc _ = Just "A stand-in builtin, until there is a real one. It is declared from Haskell rather than written in Catln, and is here so that there is something on this page for the compiler to supply."

-- | Every type the builtins page declares, each as what it adds to a page.
--
-- Functions rather than the types themselves, since a list of types is a list of different types
-- and would need a wrapper whose only job is to be unwrapped again.
hsBuiltinTypes :: [CtPage -> CtPage]
hsBuiltinTypes = [ctPageInto (Proxy :: Proxy Sample)]

-- | The page every builtin type together makes: what it must import, and what it declares.
hsBuiltinPage :: CtPage
hsBuiltinPage = foldl (flip ($)) emptyCtPage hsBuiltinTypes
