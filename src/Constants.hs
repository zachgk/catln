--------------------------------------------------------------------
-- |
-- Module    :  Constants
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines simple constants
--------------------------------------------------------------------

module Constants where

operatorPrefix, operatorArgUnary, operatorArgL, operatorArgR :: String
operatorPrefix = "/operator"
operatorArgUnary = "a"
operatorArgL = "l"
operatorArgR = "r"

mdAnnot, mdAnnotText :: String
mdAnnot = "/Catln/#md"
mdAnnotText = "text"

nestedDeclaration :: String
nestedDeclaration = "/Catln/nestedDeclaration"
