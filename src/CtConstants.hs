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

{-# LANGUAGE PatternSynonyms #-}

module CtConstants where

operatorPrefix, operatorArgUnary, operatorArgL, operatorArgR :: String
operatorPrefix = "/operator"
operatorArgUnary = "a"
operatorArgL = "/l"
operatorArgR = "/r"

operatorName :: String -> String
operatorName n = operatorPrefix ++ n

operatorType, operatorHasArrow, operatorEq :: String
operatorType = operatorName ":"
operatorHasArrow = operatorName "?->"
operatorEq = operatorName "=="

intPrim, floatPrim, truePrim, falsePrim, strPrim, ioPrim :: String
intPrim = "/Data/Primitive/Integer"
floatPrim = "/Data/Primitive/Float"
truePrim = "/Data/Primitive/True"
falsePrim = "/Data/Primitive/False"
strPrim = "/Data/String"
ioPrim = "/Catln/IO"

pattern ContextStr :: String
pattern ContextStr = "/Context"
pattern ContextInStr :: String
pattern ContextInStr = "/ContextIn"
contextOutStr, contextValStr, ioStr :: String
contextOutStr = "/ContextOut"
contextValStr = "/value"
ioStr = "/io"

assertStr, assertTestStr, assertMsgStr :: String
assertStr = "/Catln/#assert"
assertTestStr = "/test"
assertMsgStr = "/msg"

testAnnot :: String
testAnnot = "/Catln/#test"

exampleAnnot :: String
exampleAnnot = "/Catln/#example"

mdAnnot, mdAnnotText :: String
mdAnnot = "/Catln/#md"
mdAnnotText = "text"

printAnnot, printAnnotText :: String
printAnnot = "/Catln/#print"
printAnnotText = "p"

ctxAnnot :: String
ctxAnnot = "/Catln/#ctx"

noCoreAnnot :: String
noCoreAnnot = "#noCore"

elseAnnot :: String
elseAnnot = "/Catln/#else"

runtimeAnnot, runtimeAnnotK :: String
runtimeAnnot = "/Catln/#runtime"
runtimeAnnotK = "/k"

argStartAnnot, argEndAnnot :: String
argStartAnnot = "/Catln/#argStart"
argEndAnnot = "/Catln/#argEnd"

anonStr :: String
anonStr = "/"

inKey :: String
inKey = "/$in"

classStr, classObj, classK :: String
classStr = "class"
classObj = "/Class"
classK = "/k"

isaObj, isaClassK :: String
isaObj = "/Isa"
isaClassK = "/class"

nestedDeclaration, modStr, dataStr, annotStr, everyStr, isaStr, applyStr :: String
nestedDeclaration = "/Catln/nestedDeclaration"
modStr = "module"
dataStr = "data"
annotStr = "annot"
everyStr = "every"
isaStr = "isa"
applyStr = "apply"

ctIf, ctThen, ctElse :: String
ctIf = "if"
ctThen = "then"
ctElse = "else"

ctMatch, ctCase :: String
ctMatch = "match"
ctCase = "case"

thisKey, macroThisKey :: String
thisKey = "/this"
macroThisKey = "/$this"

classPlaceholderStr :: String
classPlaceholderStr = "/Catln/classPlaceholder"

ctListType, ctListCons, ctListConsHead, ctListConsTail :: String
ctListType = "List"
ctListCons = "/Data/Cons"
ctListConsHead = "head"
ctListConsTail = "tail"
