--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Haskell.Prgm
-- Copyright :  (c) Zach Kimberg 2023
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines a Haskell program syntax
--------------------------------------------------------------------

module Syntax.Haskell.Prgm where

type HsMComment = String
type HsPragma = String

data HsStatement
  = HsFunctionDeclaration
  deriving (Eq, Ord, Show)

data HsModule = HsModule ![HsMComment] ![HsPragma] !String ![HsStatement]
  deriving (Eq, Ord, Show)
