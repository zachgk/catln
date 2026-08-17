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

-- | The class of what the webdocs can show, declared in @stack/core/webdoc.ct@. A display is built
-- toward it, which is what makes it a display rather than the expression it was written as: see
-- 'Eval.dshowDestType'.
dshowClassName :: String
dshowClassName = "/Catln/Doc/DShow"

-- | The page a page is written over, declared in @stack/core/webdoc.ct@. A page carrying one is
-- shown with the page at 'overlayAnnotUrl' beneath its own writing.
overlayAnnot, overlayAnnotUrl :: String
overlayAnnot = "/Catln/Doc/#overlay"
overlayAnnotUrl = "url"

-- | The types one note is read as, declared in @stack/core/note.ct@. A note is a 'notePagePath'
-- holding the writing of it, opened by the 'notePropertiesAnnot' its frontmatter is read as and
-- broken at each checkbox by a member of 'noteTaskPath'.
--
-- 'noteTaskPath' is the class the readings of a checkbox are members of, and is named by nothing
-- that gathers or displays one: a value is declared as the member it is, so what looks for a task
-- looks for the members by name. Which reading a checkbox gets is
-- 'Syntax.Obsidian.Parser.partStatement', out of what the vault is set to.
notePagePath, notePageTitle, noteTaskPath, noteSimpleTaskPath, noteObsidianTaskPath, noteTaskContents, noteTaskState, notePropertiesAnnot, notePropertiesAnnotText :: String
notePagePath = "/Data/Note/Page"
notePageTitle = "title"
noteTaskPath = "/Data/Note/Task"
noteSimpleTaskPath = "/Data/Note/Task/Simple"
noteObsidianTaskPath = "/Data/Note/Task/Obsidian"
noteTaskContents = "contents"
noteTaskState = "state"

-- | The fields of a 'noteObsidianTaskPath', beside the 'noteTaskContents' it shares with the plain
-- reading. Only the few worth naming are fields; the rest travel together in 'noteTaskRest' as
-- JSON, for the reason the type's own comment gives.
noteTaskSymbol, noteTaskStatusType, noteTaskDescription, noteTaskPriority, noteTaskScheduled, noteTaskDue, noteTaskRecurrence, noteTaskRest :: String
noteTaskSymbol = "symbol"
noteTaskStatusType = "statusType"
noteTaskDescription = "description"
noteTaskPriority = "priority"
noteTaskScheduled = "scheduled"
noteTaskDue = "due"
noteTaskRecurrence = "recurrence"
noteTaskRest = "rest"

-- | The members of the two classes those fields are typed by, which are the plugin's own seven
-- kinds of status and six priorities.
noteStatusTypePath, notePriorityPath :: String
noteStatusTypePath = "/Data/Note/StatusType"
notePriorityPath = "/Data/Note/Priority"
notePropertiesAnnot = "/Data/Note/#properties"
notePropertiesAnnotText = "text"

printAnnot, printAnnotText, resultAnnotText :: String
printAnnot = "/Catln/#print"
printAnnotText = "p"
resultAnnotText = "result"

-- | The value a run records as it reaches it, declared in @stack/core/compile.ct@. Unlike the
-- annotations above, 'debugAnnotArg' names an argument of the annotation once it has been
-- EVALUATED, so it is the runtime argument key (leading @\/@) rather than the name a partial key is
-- built from — as 'assertTestStr' is.
debugAnnot, debugAnnotArg :: String
debugAnnot = "/Catln/#debug"
debugAnnotArg = "/d"

ctxAnnot :: String
ctxAnnot = "/Catln/#ctx"

noCoreAnnot :: String
noCoreAnnot = "#noCore"

elseAnnot :: String
elseAnnot = "/Catln/#else"

refineAnnot :: String
refineAnnot = "/Catln/#refine"

runtimeAnnot, runtimeAnnotK :: String
runtimeAnnot = "/Catln/#runtime"
runtimeAnnotK = "/k"

argStartAnnot, argEndAnnot :: String
argStartAnnot = "/Catln/#argStart"
argEndAnnot = "/Catln/#argEnd"

-- | Every annotation name the compiler itself emits or recognizes: doc comments desugar to
-- @#md@, @>@ statements to @#print@\/@#example@\/@#assert@, parenthesized arguments to
-- @#argStart@\/@#argEnd@, and so on. These names are part of the compiler's vocabulary and
-- exist in every program regardless of imports, so the type-name universe must always
-- contain them; the declarations in core's compile.ct refine their argument types when in
-- scope. Without this, a file whose import closure lacks compile.ct (a core module, or a
-- @#noCore@ test file) would read its own desugared doc comment as an undeclared name.
builtinAnnotNames :: [String]
builtinAnnotNames =
  [ assertStr, testAnnot, exampleAnnot, mdAnnot, printAnnot, ctxAnnot, elseAnnot
  , refineAnnot, runtimeAnnot, argStartAnnot, argEndAnnot, "/Catln/#noCore", "/Catln/#name"
  -- The annotations' argument keywords are emitted alongside them (@#md(text= ...)@), so they
  -- are part of the same vocabulary.
  , assertTestStr, assertMsgStr, mdAnnotText, printAnnotText, resultAnnotText, runtimeAnnotK, "name"
  ]

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

-- | The bind (@<-@) desugar routes @x <- m; rest@ into @bindap(m= m, fn(v)= continuation(v))@.
-- The arg names match 'fmap'/'mmap' so the stdlib arms forward trivially.
bindApName, bindApM, bindApFn, bindApV :: String
bindApName = "/Data/bindap"
bindApM = "m"
bindApFn = "fn"
bindApV = "v"

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

ctListType, ctListCons, ctListNil, ctListConsHead, ctListConsTail :: String
ctListType = "List"
ctListCons = "/Data/List/Cons"
ctListNil = "/Data/List/Nil"
ctListConsHead = "head"
ctListConsTail = "tail"

-- | The two arms of a @\/Data\/Maybe@, for building one where a value may be absent — as a date a
-- task has or has not got is.
ctMaybeSome, ctMaybeSomeValue, ctMaybeNothing :: String
ctMaybeSome = "/Data/Some"
ctMaybeSomeValue = "value"
ctMaybeNothing = "/Data/Nothing"
