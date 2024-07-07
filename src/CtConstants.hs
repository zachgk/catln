--------------------------------------------------------------------
-- |
-- Module    :  CtConstants
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines simple constants
--------------------------------------------------------------------

module CtConstants where

operatorPrefix, operatorArgUnary, operatorArgL, operatorArgR :: String
operatorPrefix = "/operator"
operatorArgUnary = "a"
operatorArgL = "l"
operatorArgR = "r"

operatorName :: String -> String
operatorName n = operatorPrefix ++ n

operatorType :: String
operatorType = operatorName ":"

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

printAnnot, printAnnotText :: String
printAnnot = "/Catln/#print"
printAnnotText = "p"

ctxAnnot :: String
ctxAnnot = "/Catln/#ctx"

elseAnnot :: String
elseAnnot = "/Catln/#else"

argStartAnnot, argEndAnnot :: String
argStartAnnot = "/Catln/#argStart"
argEndAnnot = "/Catln/#argEnd"

nestedDeclaration, modStr :: String
nestedDeclaration = "/Catln/nestedDeclaration"
modStr = "module"

ctIf, ctThen, ctElse :: String
ctIf = "if"
ctThen = "then"
ctElse = "else"

ctMatch, ctCase :: String
ctMatch = "match"
ctCase = "case"

ctListType, ctListCons, ctListConsHead, ctListConsTail :: String
ctListType = "List"
ctListCons = "/Data/Cons"
ctListConsHead = "head"
ctListConsTail = "tail"
