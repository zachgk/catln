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

import           Data.Char   (isPrint, isSpace, ord)
import           Text.Printf (printf)

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

intPrim, floatPrim, truePrim, falsePrim, strPrim, charPrim, ioPrim :: String
intPrim = "/Data/Primitive/Integer"
floatPrim = "/Data/Primitive/Float"
truePrim = "/Data/Primitive/True"
falsePrim = "/Data/Primitive/False"
strPrim = "/Data/String"
-- | Declared in @stack\/core\/char.ct@, and made into strings by @stack\/core\/string.ct@. Named here rather than written where
-- it is used, as every primitive beside it is: 'Semantics.Types.charLeaf' spelled it out for itself
-- while nothing declared the type at all, which is two ways of the same name going unchecked.
charPrim = "/Data/Char"
ioPrim = "/Catln/IO"

-- | What a string value is: the one member of the @\/Data\/String@ class, holding in its one field
-- the list of @\/Data\/Char@ the string is made of. A class with one member rather than a data type,
-- so that @String@ written as a type means every string and not only the one with no characters —
-- a data type's bare name is its argument-less form, which a string with characters is not.
ctStringText, ctStringChars :: String
ctStringText = "/Data/String/Text"
ctStringChars = "chars"

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

-- | The function resolving a value to the display it is shown as, and the argument it takes it in,
-- both declared in @stack\/core\/webdoc.ct@.
--
-- Named here because a syntax generating a page may have to write the call itself. A type displayed
-- by an arm of this is by that fact not a member of 'dshowClassName' — the two together are two arms
-- matching one value — so a @#print@ of such a value, whose argument is declared to /be/ a
-- 'dshowClassName', has to be handed the display rather than the value. See
-- 'Syntax.Json.Parser.jsonStatements', which writes one.
dshowName, dshowArg :: String
dshowName = "/Catln/Doc/dshow"
dshowArg = "s"

-- | The class of what a language model can be shown, the function resolving a value to what it is
-- shown as, and the argument it takes it in, all declared in @stack\/llm\/show.ct@.
--
-- The counterpart of 'dshowClassName' and 'dshowName' for a reader that is a model rather than a
-- person: a type may be displayed one way in the webdocs and written another way for a model. The
-- two share the text of a @ToString@, which either reader can read.
llmshowClassName, llmshowName, llmshowArg :: String
llmshowClassName = "/Llm/LlmShow"
llmshowName = "/Llm/llmshow"
llmshowArg = "s"

-- | The class of calls that have a display of their own, and the function resolving a call to that
-- display, both declared in @stack\/core\/webdoc.ct@.
--
-- A second pair rather than more arms of 'dshowName' because a call is not a value and cannot be
-- passed as one: an argument is evaluated before the callee is entered, so an arm of 'dshowName'
-- whose pattern is a call is selected for the call and handed the call's result, and dies
-- destructuring it. Nothing calls this from a page. Its one caller is 'Eval.attachCallShows', which
-- has a call in hand as a runtime value — the tuple an arrow was entered with — and so can hand an
-- arm exactly what its pattern is written against.
dshowCallClassName, dshowCallName, dshowCallArg :: String
dshowCallClassName = "/Catln/Doc/DShowCall"
dshowCallName = "/Catln/Doc/dshowCall"
dshowCallArg = "/s"

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
noteTaskSymbol, noteTaskStatusType, noteTaskDescription, noteTaskPriority, noteTaskStart, noteTaskScheduled, noteTaskDue, noteTaskRecurrence, noteTaskRest :: String
noteTaskSymbol = "symbol"
noteTaskStatusType = "statusType"
noteTaskDescription = "description"
noteTaskPriority = "priority"
-- | The three days a task may be done on. They are fields together because
-- @\/Data\/Calendar\/calendarDay@ reads all three to say which day a task sits on, and that arm is
-- written in Catln — which can read a field and cannot read a day out of 'noteTaskRest'.
noteTaskStart = "start"
noteTaskScheduled = "scheduled"
noteTaskDue = "due"
noteTaskRecurrence = "recurrence"
noteTaskRest = "rest"

-- | What the Obsidian Day Planner plugin reads a checkbox as, which both readings of one carry: the
-- block of the day its line writes at its start, and the day the plugin plans it for. They are
-- fields of both because the plugin is a reading of the vault apart from the Tasks plugin, and a
-- vault may run either without the other.
noteTaskTime, noteTaskPlannedDay :: String
noteTaskTime = "time"
noteTaskPlannedDay = "plannedDay"

-- | The members of the two classes those fields are typed by, which are the plugin's own seven
-- kinds of status and six priorities.
noteStatusTypePath, notePriorityPath :: String
noteStatusTypePath = "/Data/Note/StatusType"
notePriorityPath = "/Data/Note/Priority"
notePropertiesAnnot = "/Data/Note/#properties"
notePropertiesAnnotText = "text"

-- | Days and times of day, declared in @stack\/core\/time.ct@. A day a note writes is read into a
-- 'timeDatePath' where it is read, and a block of a planned day into a 'timeRangePath' of two
-- 'timeOfDayPath's, so that what Catln is handed is a value with an order rather than text.
timeDatePath, timeDateYear, timeDateMonth, timeDateDay, timeOfDayPath, timeOfDayHour, timeOfDayMinute, timeRangePath, timeRangeStart, timeRangeEnd :: String
timeDatePath = "/Data/Time/Date"
timeDateYear = "year"
timeDateMonth = "month"
timeDateDay = "day"
timeOfDayPath = "/Data/Time/TimeOfDay"
timeOfDayHour = "hour"
timeOfDayMinute = "minute"
timeRangePath = "/Data/Time/TimeRange"
timeRangeStart = "start"
timeRangeEnd = "end"

-- | A calendar, declared in @stack\/core\/calendar.ct@. 'calendarEntryClass' is the class of things
-- a calendar can show and 'calendarDayMethod' the one question asked of a member of it, which is
-- which day it is assigned to. They are named here rather than where a task is because a calendar
-- knows nothing about tasks: what joins the class and answers the method is written in @.ct@, so
-- adding a kind of thing a calendar shows changes no Haskell.
--
-- 'calendarTimeMethod' is the second question, which is the span of its day an entry takes up where
-- it takes up only part of it.
calendarAnnot, calendarEntryClass, calendarDayMethod, calendarTimeMethod :: String
calendarAnnot = "/Data/Calendar/#calendar"
calendarEntryClass = "/Data/Calendar/CalendarEntry"
calendarDayMethod = "/Data/Calendar/calendarDay"
calendarTimeMethod = "/Data/Calendar/calendarTime"

-- | What a day planner lays out along a day, declared in @stack\/core\/calendar.ct@. 'eventClass' is
-- the class of things taking up a block of a day, and 'eventSpanMethod' and 'eventDayMethod' the
-- two questions asked of a member: the block it takes up, and the day it takes it up on.
--
-- 'noteEventPath' is the one member a note declares that is no task: a line the Day Planner reads a
-- block off which has no checkbox.
eventClass, eventSpanMethod, eventDayMethod, noteEventPath :: String
eventClass = "/Data/Calendar/Event"
eventSpanMethod = "/Data/Calendar/eventSpan"
eventDayMethod = "/Data/Calendar/eventDay"
noteEventPath = "/Data/Note/Event"

-- | The query a note writes in a block of code the Obsidian Tasks plugin reads, which is a fenced
-- block whose info string is @tasks@. The query is carried as the text it was written as, for the
-- reason 'notePropertiesAnnot' carries a block of properties that way: the block is written back
-- from it, and it is what the reader typed.
noteTasksAnnot, noteTasksAnnotQuery, noteTasksInfoString :: String
noteTasksAnnot = "/Data/Note/#tasks"
noteTasksAnnotQuery = "query"
noteTasksInfoString = "tasks"

-- | One entry of an RSS feed, declared in @stack\/rss\/main.ct@. A feed is read as a page holding
-- one of these per entry, in the order 'Syntax.Rss.Cache' keeps them.
--
-- Declared outside the standard library rather than in it, so a program which imports no feed does
-- not compile the type. That is why 'rssStackModule' is here: the page a feed is read as writes
-- that import itself, where a note relies on the core every page is given.
--
-- 'rssItemGuid' is what the feed calls the entry — its id, or failing that its address, or failing
-- that its title. It is the name the record merges on, so an entry is one entry across the runs
-- which saw it rather than a new one each time it is fetched.
rssItemPath, rssItemTitle, rssItemLink, rssItemGuid, rssItemPublished, rssItemDescription, rssStackModule :: String
rssItemPath = "/Data/RSS/Item"
rssItemTitle = "title"
rssItemLink = "link"
rssItemGuid = "guid"
rssItemPublished = "published"
rssItemDescription = "description"
rssStackModule = "rss/main.ct"

printAnnot, printAnnotText, resultAnnotText :: String
printAnnot = "/Catln/#print"
printAnnotText = "p"
resultAnnotText = "result"

-- | A question asked of a model about the page it is written on, declared in @stack/llm/q.ct@
-- and written @q> text@. @catln process@ answers it into its 'resultAnnotText', the @>>>@ lines
-- under it.
queryAnnot, queryAnnotText, queryAnnotShort :: String
queryAnnot = "/Llm/#q"
queryAnnotText = "input"
queryAnnotShort = "#q"

-- | What @catln process@ asks of each annotation on a page with a line processor:
-- @\/Catln\/process{io}(annot)@, its updated form. An annotation is processable where the program
-- has an arm of it that takes the annotation; a library declares one beside each annotation it can
-- process.
processFun, processFunIO, processFunAnnot :: String
processFun = "/Catln/process"
processFunIO = "/io"
processFunAnnot = "/annot"

-- | What @catln process@ asks of each annotation on a page with a file processor:
-- @\/Catln\/processFile{io}(annot, page, annotId)@, the page it is written in as it is to become,
-- as a @\/Data\/Result@ of a @\/Catln\/RawPrgm@. It is given the page as @page@, and the meta ID of
-- the annotation as @annotId@, for a processor to find itself in the page by.
processFileFun, processFilePage, processFileAnnotId, rawPrgmPath :: String
processFileFun = "/Catln/processFile"
processFilePage = "/page"
processFileAnnotId = "/annotId"
rawPrgmPath = "/Catln/RawPrgm"

-- | Why text is not a page, declared in @stack/process/page.ct@: what the parser said of it.
parsePageErr, parsePageErrMessage :: String
parsePageErr = "/Process/ParsePageErr/Syntax"
parsePageErrMessage = "message"

-- | What went wrong, written for a person to read: @\/Data\/Err@'s @msg@, declared in
-- @stack/core/error.ct@ as a method of the error, its @this@.
errMsgFun, errMsgThis :: String
errMsgFun = "/Data/msg"
errMsgThis = "/this"

-- | The field an annotation is processed into where processing it failed, holding why. Such an
-- annotation is reported with that reason and written back as it was, so it is processed again
-- the next time rather than being answered with its failure.
processFailureText :: String
processFailureText = "failure"

-- | What the compiler or an evaluation said about a statement, written under it for a reader of the
-- page as @!!! kind: text@ lines ('Syntax.Ct.Results.notePage'). Only ever written: nothing parses
-- @!!!@, so a page shown with its notes is shown them rather than given them.
noteAnnot, noteAnnotKind, noteAnnotText :: String
noteAnnot = "/Catln/#note"
noteAnnotKind = "kind"
noteAnnotText = "text"

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

-- | Marks a refinement declaration as one to PROVE with the type checker rather than to test by
-- generating inputs (see "TypeCheck.Proof").
proofAnnot :: String
proofAnnot = "/Catln/#proof"

-- | Marks a declaration's arrows as EXISTENTIALLY quantified: its overlapping arms are alternative
-- rewrites, any one of which is a valid result, rather than arms which must agree (see
-- 'Semantics.Prgm.omiQuant').
existAnnot :: String
existAnnot = "/Catln/#exist"

-- | The annotation marking a declaration whose promise holds only on part of its domain, declared
-- in @stack/core/compile.ct@. Such a declaration is not checked for fulfillment
-- ('TypeCheck.Decode.verifyObjMapItem') and derives no test of its promise
-- ('Eval.Testing.deriveRefinementTests').
invalidAnnot :: String
invalidAnnot = "/Catln/#invalid"

runtimeAnnot, runtimeAnnotK :: String
runtimeAnnot = "/Catln/#runtime"
runtimeAnnotK = "/k"

argStartAnnot, argEndAnnot :: String
argStartAnnot = "/Catln/#argStart"
argEndAnnot = "/Catln/#argEnd"

-- | The annotation marking a declaration as a way of reading files written in another syntax.
--
-- Given relative as well as absolute, which is unlike the annotations above and like 'noCoreAnnot'.
-- Both are read off a program that has been PARSED and not desugared — this one so that which
-- importer reads a file can be decided while the imports are being resolved, long before anything
-- has been desugared — and a parsed annotation carries the name as it was written.
importerAnnot, importerAnnotAbs, importerAnnotName, importerAnnotExts :: String
importerAnnot = "#importer"
importerAnnotAbs = "/Catln/#importer"
importerAnnotName = "name"
importerAnnotExts = "extensions"

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
  , refineAnnot, proofAnnot, existAnnot, invalidAnnot, runtimeAnnot, argStartAnnot, argEndAnnot, "/Catln/#noCore", "/Catln/#name"
  , importerAnnotAbs
  -- The annotations' argument keywords are emitted alongside them (@#md(text= ...)@), so they
  -- are part of the same vocabulary.
  , assertTestStr, assertMsgStr, mdAnnotText, printAnnotText, resultAnnotText, runtimeAnnotK, "name"
  , importerAnnotName, importerAnnotExts
  ]

-- | How a page written to fail is named: a test compiles it, requires that it fails, and does not
-- print the errors it fails with ('Semantics.ctssQuietPage').
errPageSuffix :: String
errPageSuffix = ".err.ct"

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

-- | The value a condition refining a type is written over, as it is written: @$this@ is the value
-- of the type the condition refines, and @$out@ is that same value named as what an arrow
-- produces, the spelling for the condition on an arrow's result type
-- (@-> (Integer | even(x= $out))@), where @this@ is a method's receiver. Both desugar to
-- 'macroThisKey', the one name the value is bound under wherever a value predicate is decided.
thisMacro, outMacro :: String
thisMacro = "$this"
outMacro = "$out"

classPlaceholderStr :: String
classPlaceholderStr = "/Catln/classPlaceholder"

ctListType, ctListCons, ctListNil, ctListConsHead, ctListConsTail :: String
ctListType = "List"
ctListCons = "/Data/List/Cons"
ctListNil = "/Data/List/Nil"
ctListConsHead = "head"
ctListConsTail = "tail"

-- | What a build produces, declared in @stack\/core\/result.ct@: the files of the directory it is
-- built into, each at the path it is built at.
--
-- The files rather than a directory of its own, because the directory a result is written into
-- already has a name and did not get it from the program.
--
-- Flat, with the tree said by the @\/@ in each path, rather than the nested @\/Data\/FileOrDir@ it
-- wants to be. A list takes its element type from its first entry, so a directory holding both a
-- file and a subdirectory is a list whose second entry does not fit its first — see the comment on
-- @\/Data\/FileOrDir@ in @stack\/core\/file.ct@, which is declared and not yet usable for that
-- reason and so is named nowhere here.
catlnResultPath, catlnResultFiles, buildFilePath, buildFileFilePath, buildFileContents :: String
catlnResultPath = "/Catln/CatlnResult"
catlnResultFiles = "files"
buildFilePath = "/Catln/BuildFile"
buildFileFilePath = "path"
buildFileContents = "contents"

-- | The files a directory is entered through, in the order one takes precedence over another.
--
-- A directory is a page of its own, and the program that page holds is the one in the first of
-- these it has ('Syntax.InferImport.dirParse'). 'dirCtcFileName' comes first because it says what
-- the directory /is/ — an overlay directory's @dir.ctc@ builds the tree the directory stands for —
-- where a @main.ct@ is a program that happens to be kept there.
--
-- Named here rather than where they are looked for, because two things must agree on them: the read
-- which makes a directory a page, and the navigation which leads a directory's entry with the page
-- it is entered through ('Eval.Runtime.WebDocs.sidebarLevel'). A directory entered through one file
-- and listed under another is a navigation whose first entry is not the page it opens.
dirCtcFileName, mainCtFileName :: String
dirCtcFileName = "dir.ctc"
mainCtFileName = "main.ct"

dirEntryFileNames :: [String]
dirEntryFileNames = [dirCtcFileName, mainCtFileName]

-- | An argument name as it is keyed once the value carrying it has been EVALUATED, which is the
-- name it is declared under with a leading separator (see 'debugAnnotArg'). The declared and the
-- evaluated form are written once each here rather than as two constants per argument, so a name
-- cannot drift between the shape a value is built with and the shape it is read back out of.
runtimeArgKey :: String -> String
runtimeArgKey = ('/' :)

-- | The characters a name cannot hold, which is what @Syntax.Ct.Parser.Lexer.identifier@ refuses.
--
-- Here rather than only there because two things must agree on it: the parser, which decides what a
-- written name is, and 'fileDirName', which builds a name that has to be written back. A character
-- this missed would not be a name rejected but a name silently read as something else — @.@ is the
-- method operator and @-@ is subtraction, so @\/FileDir\/my-notes@ parses as a subtraction of two
-- names rather than failing.
nameInvalidChars :: String
nameInvalidChars = "-~:*%+<>()[]{}=!&|.@_,?\""

-- | The name a directory read by @fileDirReference@ is declared under: @\/FileDir@ followed by the
-- absolute path of the directory.
--
-- The path is what makes the name, so two reads of one directory are one name and two directories
-- are never one name, whoever imported them and whatever they called it.
--
-- A path is not a name, though. Most real paths hold a @-@, a @.@ or a @_@, none of which a name
-- may hold ('nameInvalidChars'), and each of which parses as something else rather than failing. So
-- each such character becomes @q@ and its two hex digits, and a @q@ becomes @qq@ — which is
-- reversible, uses only characters a name may hold, and leaves paths without any of them written as
-- themselves. @\/home\/zach\/my-notes\/site_v2@ is @\/FileDir\/home\/zach\/myq2Dnotes\/siteq5Fv2@.
--
-- The separator is left alone, since a path's @\/@ and a name's @\/@ are the same character and the
-- one is meant to read as the other.
fileDirName :: FilePath -> String
fileDirName = escapedPathName fileDirNamePrefix

-- | A name built from a path, under the prefix a syntax declares its pages' values beneath.
--
-- The escaping is 'fileDirName''s and is described there; what is shared is the rule and not the
-- prefix, so two syntaxes naming a page after the file it was read from cannot collide with one
-- another. A JSON file read by @json\/importer.ct@ is named this way too, for the reason a directory
-- is: a program may import two of them, and two files must never be one name.
escapedPathName :: String -> FilePath -> String
escapedPathName prefix path = prefix ++ concatMap esc path
  where
    esc '/' = "/"
    esc c
      | c == fileDirEscapeChar = [fileDirEscapeChar, fileDirEscapeChar]
      | isPrint c && not (isSpace c) && c `notElem` nameInvalidChars = [c]
      | otherwise = fileDirEscapeChar : printf "%02X" (ord c)

fileDirNamePrefix :: String
fileDirNamePrefix = "/FileDir"

fileDirEscapeChar :: Char
fileDirEscapeChar = 'q'

-- | The six arms of a @\/Data\/Json@ and the fields each carries, as the reader in
-- "Syntax.Json.Parser" builds them.
--
-- Named here rather than written into that module because two things must agree on them: the reader,
-- which builds the value, and @stack\/json\/json.ct@, which declares the type it is a value of. A
-- name that drifted apart would be a page that parses and does not typecheck.
jsonPath, jsonNullPath, jsonBoolPath, jsonNumberPath, jsonStrPath, jsonArrayPath, jsonObjectPath, jsonMemberPath :: String
jsonPath = "/Data/Json"
jsonNullPath = "/Data/Json/Null"
jsonBoolPath = "/Data/Json/Bool"
jsonNumberPath = "/Data/Json/Number"
jsonStrPath = "/Data/Json/Str"
jsonArrayPath = "/Data/Json/Array"
jsonObjectPath = "/Data/Json/Object"
jsonMemberPath = "/Data/Json/Member"

-- | The field a scalar arm carries its value in, the field a container carries its children in, and
-- the two a member is.
jsonValueField, jsonEntriesField, jsonMemberKeyField, jsonMemberValueField :: String
jsonValueField = "value"
jsonEntriesField = "entries"
jsonMemberKeyField = "key"
jsonMemberValueField = "value"

-- | The page declaring @\/Data\/Json@, which every page a JSON file is read as imports.
jsonTypesPage :: FilePath
jsonTypesPage = "json/json.ct"

-- | The name the value of a JSON file is declared under: @\/JsonFile@ followed by the file's path,
-- escaped as 'escapedPathName' escapes one.
--
-- Named after the path for the reason 'fileDirName' is, and it is a sharper reason here: a program
-- importing two JSON files is ordinary, and a fixed name would make the second silently the first.
--
-- @\/JsonFile@ and not @\/Json@, because @stack\/json\/importer.ct@ declares @parseJson@ inside
-- @module(Json)@ and 'Syntax.Native.Parser' puts the importer's page among the read page's imports —
-- so the two are always in one program, and a value named under @\/Json@ would sit in the namespace
-- the importer's own declaration is in.
jsonFileName :: FilePath -> String
jsonFileName = escapedPathName jsonFileNamePrefix

jsonFileNamePrefix :: String
jsonFileNamePrefix = "/JsonFile"

-- | The two arms of a @\/Data\/Maybe@, for building one where a value may be absent — as a date a
-- task has or has not got is.
ctMaybeSome, ctMaybeSomeValue, ctMaybeNothing :: String
ctMaybeSome = "/Data/Maybe/Some"
ctMaybeSomeValue = "value"
ctMaybeNothing = "/Data/Maybe/Nothing"

-- | The two arms of a @\/Data\/Result@ (@stack\/core\/error.ct@), for answering with the value an
-- operation computed or the error it failed with.
ctResultOk, ctResultOkValue, ctResultFail, ctResultFailErr :: String
ctResultOk = "/Data/Result/Ok"
ctResultOkValue = "value"
ctResultFail = "/Data/Result/Fail"
ctResultFailErr = "err"

-- | Why a request has no response, as @stack\/http\/main.ct@ declares it: the @\/Http\/HttpErr@
-- arms and their fields.
ctHttpErrInvalidUrl, ctHttpErrUnreachable, ctHttpErrStatus, ctHttpErrNotSent :: String
ctHttpErrInvalidUrl = "/Http/HttpErr/InvalidUrl"
ctHttpErrUnreachable = "/Http/HttpErr/Unreachable"
ctHttpErrStatus = "/Http/HttpErr/Status"
ctHttpErrNotSent = "/Http/HttpErr/NotSent"

ctHttpErrUrl, ctHttpErrDetail, ctHttpErrCode, ctHttpErrBody :: String
ctHttpErrUrl = "url"
ctHttpErrDetail = "detail"
ctHttpErrCode = "code"
ctHttpErrBody = "body"

-- | Why there is no line of input, as @stack\/core\/compile.ct@ declares it.
ctInputErrEndOfInput, ctInputErrNoInput :: String
ctInputErrEndOfInput = "/Catln/InputErr/EndOfInput"
ctInputErrNoInput = "/Catln/InputErr/NoInput"

-- | Why source is not a value of a type, as @stack\/reflect\/main.ct@ declares it: the
-- @\/Catln\/ParseExprErr@ arms and their fields.
ctParseExprErrSyntax, ctParseExprErrType, ctParseExprErrNotOf, ctParseExprErrEval :: String
ctParseExprErrSyntax = "/Catln/ParseExprErr/Syntax"
ctParseExprErrType = "/Catln/ParseExprErr/Type"
ctParseExprErrNotOf = "/Catln/ParseExprErr/NotOf"
ctParseExprErrEval = "/Catln/ParseExprErr/Eval"

ctParseExprErrMessage, ctParseExprErrFound :: String
ctParseExprErrMessage = "message"
ctParseExprErrFound = "found"

-- | The type variable a parameterized standard-library type takes its element type as: the @$T@ of
-- @\/Data\/List[$T -> String]@ in @stack\/core\/result.ct@.
--
-- Named here because two things have to agree on it — the @.ct@ files which declare those types, and
-- 'Semantics.CtType', which writes a Haskell @[a]@ or @Maybe a@ back out as one of them.
ctElemVar :: String
ctElemVar = "$T"

-- | The paths of the standard library's types a derived builtin's field can be, where the path is
-- not already named by the constructors above.
--
-- 'ctBooleanPath' beside 'truePrim' and 'falsePrim', which are its two members;
-- @stack\/core\/bool.ct@ writes them as one @class(Boolean, [True, False])@.
ctBooleanPath, ctMaybePath, ctListPath, ctConsListPath :: String
ctBooleanPath = "/Data/Primitive/Boolean"
ctMaybePath = "/Data/Maybe"
ctListPath = "/Data/List"
-- | The list a string's characters are held in (@stack\/core\/cons.ct@).
ctConsListPath = "/Data/List/ConsList"

-- | The files a library may ship beside its pages for the webdocs to load, and the content type each
-- is served as.
--
-- A display a library writes is a @\/Catln\/Doc\/Show\/Rendered@ naming these by path
-- (@stack\/core\/render.ct@), and the doc server serves them from its @\/api\/asset@ route. The list
-- is what that route will answer for at all: a doc server already serves a @.ct@ as the page it
-- compiles to, and this must not become a second way to read one as bytes.
--
-- The content type is written out rather than looked up, because a module script served without one
-- is refused by the browser and a table of every type there is would be answering for files this
-- does not serve.
libraryAssetTypes :: [(String, String)]
libraryAssetTypes =
  [ (".js", "text/javascript; charset=utf-8")
  , (".css", "text/css; charset=utf-8")
  ]
