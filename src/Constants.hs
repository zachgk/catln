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

operatorType :: String
operatorType = operatorPrefix ++ ":"

intPrim, floatPrim, truePrim, falsePrim, strPrim, ioPrim :: String
intPrim = "/Data/Primitive/Integer"
floatPrim = "/Data/Primitive/Float"
truePrim = "/Data/Primitive/True"
falsePrim = "/Data/Primitive/False"
strPrim = "/Data/String"
ioPrim = "/Catln/IO"

mdAnnot, mdAnnotText :: String
mdAnnot = "/Catln/#md"
mdAnnotText = "text"

ctxAnnot :: String
ctxAnnot = "/Catln/#ctx"

elseAnnot :: String
elseAnnot = "/Catln/#else"

argStartAnnot, argEndAnnot :: String
argStartAnnot = "/Catln/#argStart"
argEndAnnot = "/Catln/#argEnd"

nestedDeclaration :: String
nestedDeclaration = "/Catln/nestedDeclaration"
