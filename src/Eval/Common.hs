--------------------------------------------------------------------
-- |
-- Module    :  Eval.Common
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines some common types and functions used by
-- "Eval".
--------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE PatternSynonyms            #-}

-- The data half of a value lives in "Semantics.Val" and is re-exported here, so a module that reads
-- @import Eval.Common@ still sees 'IntVal', 'TupleVal' and the rest under the names they have always
-- had. Only 'Val' and 'Args' themselves are hidden and re-bound below, as this evaluation's
-- instantiation of the parameterized ones.
module Eval.Common (module Eval.Common, module Semantics.Val) where

import           Data.Hashable
import qualified Data.HashMap.Strict        as H
import qualified Data.HashSet               as S
import qualified Data.Map                   as Map
import           Data.Maybe                 (mapMaybe)
import           GHC.Generics               (Generic)

import           Control.Monad.State
import           Control.Monad.Trans.Writer (execWriter, tell)
import           CRes
import           CtConstants
import           Data.Aeson                 hiding (Object)
import           Data.IORef
import           Data.Time.Calendar         (Day)
import           Data.UUID                  (UUID)
-- import qualified LLVM.AST            as AST
import           MapMeta
import           Semantics
import           Semantics.Prgm
import           Semantics.TypeGraph
import           Semantics.Types
import qualified Semantics.Val              as SV
import           Semantics.Val              hiding (Args, Val)
import           Syntax.Ct.Parser.Syntax    (ParseMetaDat)
import           Syntax.Ct.Prgm             (RawExpr, RawPrgm, RawStatementTree,
                                             ReadPage)
import           Text.Megaparsec            (sourceName)
import           Text.Printf
import           TypeCheck.Common           (InstrumentResults, TPrgm, VPrgm)
import           Utils

type EvalMetaDat = Maybe ReachesTree
type EvalMeta = Meta EvalMetaDat
type ECompAnnot = CompAnnot (Expr EvalMetaDat)
type EExpr = Expr EvalMetaDat
type EGuard = Maybe EExpr
type EObjArr = ObjArr Expr EvalMetaDat
type EObjectMap = ObjectMap Expr EvalMetaDat
type EPrgm = Prgm Expr EvalMetaDat
type EPrgmGraphData = GraphData EPrgm FileImport

-- | One page of the compilation service: what a read of it left, and everything the pipeline has
-- computed from it so far.
--
-- It lives here, below the service that keeps it, because an evaluation is given the service to ask
-- its own questions of and so has to be able to name a page's answer. Every stage below mentions a
-- type of this module — a tree build is a 'TExpr' program, a test run is a 'TestResult', an
-- evaluated annotation is a 'Val' — and 'Env' is in one recursive knot with all of them, so there
-- is nowhere lower to put it.
--
-- 'Nothing' means the same in every stage: not yet, ask and it will be. What asks is
-- 'CtService.ctssLoad' for the program and 'CtService.ctssBuildFrom' for the stages below it, which
-- every reader of this goes through. A stage is also left unforced once computed, so holding a page
-- costs nothing until something reads the stage it wants — merely inspecting 'ssfTBPrgm' runs an
-- 'Eval.evalBuildAll' over the page.
data SSF = SSF {
  ssfSource    :: Maybe String,
  -- | The page's program, or 'Nothing' for a page whose imports are known and whose program has not
  -- been read.
  ssfRaw       :: Maybe (CRes (RawPrgm ParseMetaDat)),
  -- | What the read which produced this page left: the imports it walked on from, and when the
  -- files it read were last written. Handed back to the next read, which gives this page back
  -- untouched while those files are unchanged, so a page is parsed again only when something it was
  -- read from has been written to.
  ssfRead      :: ReadPage,
  ssfBuilt     :: Bool,
  ssfDes       :: Maybe (CRes (Prgm Expr ParseMetaDat)),
  ssfTPrgm     :: Maybe (CRes TPrgm),
  -- | The pages this page's typecheck was solved against: the peers it was solved WITH (its
  -- strongly connected component, whose programs are their own 'ssfDes') and the pages it was
  -- solved AGAINST (its transitive imports, whose programs are their own 'ssfTPrgm').
  --
  -- Names rather than programs, so that this retains nothing: a page invalidated takes its
  -- importers with it, so a page still built is one whose peers and dependencies are still built,
  -- and looking them up is what keeps the two from drifting apart. Kept because a typecheck is not
  -- reproducible from the page alone — which set it was solved with decides what it means — and
  -- reading the inference means running it again ('CtService.ctssTypeInfo').
  ssfTcInputs  :: Maybe ([FileImport], [FileImport]),
  -- | The page's inference, for whoever asked to read it: the encoded program the solve was over,
  -- and what the instruments watching it made of it.
  --
  -- Absent on a page built the ordinary way, which is watched by nothing worth keeping: the trace
  -- of a solve is far larger than the program it explains, and holding one per page is holding a
  -- multiple of the whole program for the sake of the one page someone may look at. It is filled
  -- in when something asks — by the build itself where the asking came first, and otherwise by
  -- typechecking the page again with the instruments attached.
  ssfTypeInfo  :: Maybe (CRes (VPrgm, InstrumentResults)),
  ssfTBPrgm    :: Maybe (CRes (Prgm TExpr EvalMetaDat)),
  ssfAnnots    :: Maybe (CRes [(Expr EvalMetaDat, Val)]),
  -- | What evaluating 'ssfAnnots' asked the compilation service about, which is what decides when
  -- the answer kept there stops being an answer. Written with the annotations it came from and read
  -- only beside them, so an empty record on a page holding no annotations says nothing.
  ssfAnnotAsks :: AnnotAsks,
  -- | The tests this page wrote, run. Running a test is not part of building a page, so this is
  -- filled in when something asks for it (see 'CtService.ctssTestsFrom') rather than by the
  -- pipeline.
  ssfTests     :: Maybe (CRes [TestResult])
               }

-- | Joins one stage over a set of pages, left unforced.
--
-- It must stay a thunk: a caller that never reads the stage must not pay for it, and merely
-- inspecting 'ssfTBPrgm' runs an 'Eval.evalBuildAll' over every page joined.
--
-- A page which was never built has nothing to contribute and is passed over rather than failing the
-- join — a @ctx@ page is marked built while deliberately skipping the pipeline. A page which failed
-- to build fails whoever asked, since a stage joined out of the pages that happened to compile is
-- not the stage of the program.
joinedStage :: (Monoid a) => (SSF -> Maybe (CRes a)) -> [SSF] -> CRes a
joinedStage f ssfs = fmap mconcat $ sequence $ mapMaybe f ssfs

-- | The compilation service, as an evaluation of one of its programs may ask things of it.
--
-- A class rather than the service itself because the service is the top of the pipeline and this is
-- the bottom of it: @CtService@ imports @Eval@, so nothing under @Eval@ can name a @CTSS@. The class
-- is declared here and instantiated there, as 'CRes.CNoteTC' is for the typechecker's errors.
--
-- These are the service's own operations, not a list of the questions anything currently asks. That
-- is the point of handing an evaluation the service at all: a display that wants something new asks
-- 'csBuilt' for another stage of the pages it already has, rather than the pipeline growing a field
-- to carry that one answer to it. Only a new stage of the pipeline, or a new way of re-entering the
-- compiler, is a new method here.
--
-- Every method is in 'IO', which an evaluation does not have. A primitive reaches them through
-- 'System.IO.Unsafe.unsafePerformIO', as @Eval.Runtime.ioInput@ and 'Eval.evalDebugPrgm' already do.
-- What that costs is discipline about /when/ the effect happens: the service is behind an MVar which
-- a build holds for a whole SCC (@CtService.ctssBuildSCC@) and a read holds for a whole read
-- (@CtService.ctssReadOf@), and an MVar is not re-entrant. So a primitive must force what it asks
-- for before returning it. A lazy 'Val' carrying an unforced ask would be forced by whoever reads
-- the display, which may be inside one of those locks, and would hang there rather than fail.
class CtServiceApi s where
  -- | Every page the service holds, whatever any of them has been built to. Nothing is read or
  -- built to answer it: which pages there are is known as soon as they were read.
  csPages :: s -> IO [FileImport]

  -- | Every page the service holds, with the pages of a scope built — that page and everything it
  -- imports, or all of them for a scope of 'Nothing'.
  --
  -- Every page rather than the scope's, because what is worth building and what is worth asking
  -- about are different questions. A navigation is about the pages a reader has NOT reached, and
  -- costs nothing to answer; scoping is how a caller keeps from compiling every page it can name.
  -- A caller wanting the scope alone filters what comes back by the same scope.
  --
  -- The stages on the pages that come back are unforced, so this costs the build and no more.
  -- Reading 'ssfTBPrgm' off one of them is what runs an 'Eval.evalBuildAll' over it.
  csBuilt :: s -> Maybe FileImport -> IO (GraphData SSF FileImport)

  -- | The tests a page wrote, run.
  --
  -- Unlike every stage 'csBuilt' carries, a test run is not an artifact of compiling a page but of
  -- running part of it, so asking for one is asking for work rather than for a lookup. It is the
  -- service's to do because a test is decided over the whole program a page belongs to rather than
  -- over the page and its imports (@CtService.ctssTestPages@).
  csTests :: s -> FileImport -> IO (CRes [TestResult])

  -- | A page's type inference: the encoded program its typecheck was over, and what the
  -- instruments watching that typecheck recorded.
  --
  -- Asking for work rather than for a lookup, as 'csTests' is. A page is compiled watched by
  -- nothing worth keeping — the inference of a solve is much larger than the program it produces —
  -- so unless whoever built the page had already asked for this, answering it means typechecking
  -- the page again with the instruments attached. The answer is then kept, since a reader of an
  -- inference reads it a point at a time and would otherwise pay for the solve at every point.
  csTypeInfo :: s -> FileImport -> IO (CRes (VPrgm, InstrumentResults))

  -- | Runs a named function of a page's program, for a debugger reporting on what it evaluated.
  --
  -- A page rather than a scope: a run needs one program with one entry point, and the join of every
  -- page has as many @main@s as it has pages.
  csRunFun :: s -> FileImport -> String -> IO (CRes DebugRun)

  -- | Compiles one expression, written as Catln source, as though it were a statement of a page, and
  -- runs it.
  --
  -- Source rather than a name because what is worth debugging is often something no definition
  -- names: the computation a @#print@ shows, or the one generated input a test case ran on. A
  -- generated input in particular exists nowhere in the program, so the text of the call that ran is
  -- the only handle on that case there is.
  csRunExpr :: s -> FileImport -> String -> IO (CRes DebugRun)

  -- | Gathers the values pages declare of some types, evaluated to the values they display as, in
  -- the order each page writes them.
  --
  -- It hands back a gatherer rather than an answer because what is asked about is one page at a time
  -- while the work of answering — the scope's typechecked programs — is shared across every page
  -- asked about. A call per page would pay for the scope once per page, which over a vault of notes
  -- is quadratic in the notes.
  --
  -- Which types are worth gathering is the caller's business and not the service's: what is offered
  -- is values by the path they were declared at, so that the pipeline stays a pipeline for programs
  -- rather than one that knows what a note is. Several types are asked for at once because what
  -- comes back is in the order the page wrote it, and two gatherings joined would be two runs of
  -- that order rather than one.
  csValues :: s -> Maybe FileImport -> IO ([Name] -> FileImport -> CResT IO [(EExpr, Val)])

  -- | Asks a method of a page's program about a value, and gives back what the answer evaluates to.
  --
  -- The companion of 'csValues'. That gathers a class of value across pages without knowing what any
  -- of them is; this reads something off one of them the same way — by naming a method the class
  -- declares and letting the member answer it. So a display can gather and then ask, and adding a
  -- new kind of thing for it to show is a @.ct@ file joining the class and answering the method,
  -- with nothing here changed.
  --
  -- Which method is worth asking is the caller's business and not the service's, for the reason
  -- 'csValues' gives about which types are worth gathering.
  --
  -- An asker per page rather than one for everything, because the page's program is what answering
  -- costs and a value is answered about in the program of the page that declared it. That is the
  -- same program 'csValues' built to gather it; asking is a second build of it, which is a cost
  -- worth naming and is why the asker is taken once per page rather than once per value.
  csAsk :: s -> Maybe FileImport -> IO (Name -> FileImport -> CRes (Val -> CResT IO Val))

-- | What a display asked the compilation service about, as what would have to change for its answer
-- to stop being its answer.
--
-- A display's value is kept on the page it was evaluated for, and a kept value is what the next
-- reader is answered with before anything is asked again. So whatever the display reached has to be
-- written down, or the answer outlives what it was an answer about. It is recorded rather than
-- enumerated per display because the enumeration was already wrong before a display could ask for
-- anything (see @CtService.ctssClearInvalidations@), and there is now no list of what a display may
-- reach to keep up to date.
--
-- Scopes rather than pages, because that is the grain the service is asked in: a display asks about a
-- page and gets that page and everything it imports. It is also the grain that makes the check
-- cheap — a change to page @C@ can only alter an ask about scope @S@ if @S@ imports @C@, and the
-- pages which import a changed one are exactly what the invalidation walk already computes.
data AnnotAsks = AnnotAsks {
    aaScopes :: S.HashSet FileImport
  -- | Whether anything was asked that any page at all can bear on, which nothing narrower covers. A
  -- test run is the case: which tests a page has is decided over the whole program that page belongs
  -- to, so a change to a page which imports it can alter them.
  , aaEvery  :: Bool
  }

instance Semigroup AnnotAsks where
  a <> b = AnnotAsks (aaScopes a <> aaScopes b) (aaEvery a || aaEvery b)

instance Monoid AnnotAsks where
  mempty = AnnotAsks S.empty False

-- | The compilation service an evaluation was given, and which of its pages the evaluation is of.
--
-- Existential because the service cannot be named here (see 'CtServiceApi'); this is the same shape
-- 'CRes.MkCNote' uses to carry a note whose type is declared above the module that holds it.
--
-- The scope is carried beside the service rather than baked into what the service was asked, so that
-- which pages an evaluation covers is a question asked where it is answered. A display reporting on
-- the program it is written in reads the scope; one reporting on the pages there are ignores it; and
-- the two can be written in the same page without the service having been asked twice.
--
-- The asks are recorded as they are made, which is what lets a kept answer be dropped when it stops
-- being one. Every ask goes through the @es@ functions below rather than through the class directly,
-- so a display cannot ask about a page without that being written down.
data EvalService where
  EvalService :: CtServiceApi s => s -> Maybe FileImport -> Day -> IORef AnnotAsks -> EvalService

-- | A handle on the service for one evaluation, with nothing asked yet.
--
-- The day is given rather than read here so that one evaluation reads one day. A display asking
-- what is due today may ask it of many tasks, and reading the clock for each of them would let an
-- evaluation straddling midnight answer one question two ways.
newEvalService :: (CtServiceApi s) => s -> Maybe FileImport -> Day -> IO EvalService
newEvalService s scope today = EvalService s scope today <$> newIORef mempty

-- | What the evaluation asked about, read back once it has finished asking.
--
-- Read by a caller which keeps the evaluation's value, so that it can be dropped when it stops being
-- an answer. A caller which keeps nothing — a call answered and forgotten — never reads this, and its
-- record is discarded with the handle.
esAsks :: EvalService -> IO AnnotAsks
esAsks (EvalService _ _ _ asks) = readIORef asks

-- | Which of the service's pages the evaluation is of, or none where it is of no one page.
esScope :: EvalService -> Maybe FileImport
esScope (EvalService _ scope _ _) = scope

-- | The day the evaluation reads @today@ as, which is one day for the whole of it.
--
-- In 'IO' as every other ask is, though it reaches nothing: what a display may ask about is one
-- list, and a day answered some other way would be an ask that went unwritten.
esToday :: EvalService -> IO Day
esToday (EvalService _ _ today _) = return today

-- | 'CtServiceApi.csPages', recording nothing: which pages there are is not what is in any of them,
-- so no change to a page's contents can alter this. A page appearing or going can, and that is
-- answered by the page set having changed rather than by anything recorded here.
esPages :: EvalService -> IO [FileImport]
esPages (EvalService s _ _ _) = csPages s

-- | 'CtServiceApi.csBuilt' of a scope, recorded as asked about.
esBuilt :: EvalService -> Maybe FileImport -> IO (GraphData SSF FileImport)
esBuilt svc@(EvalService s _ _ _) scope = recordAsk svc scope >> csBuilt s scope

-- | 'CtServiceApi.csTests' of a page, recorded as an ask about every page. A test is decided over the
-- whole program its page belongs to, which reaches past what that page imports, so nothing narrower
-- than every page covers what can alter one.
esTests :: EvalService -> FileImport -> IO (CRes [TestResult])
esTests svc@(EvalService s _ _ _) prgmName = recordEvery svc >> csTests s prgmName

-- | 'CtServiceApi.csTypeInfo' of a page, recorded as an ask about that page's scope: an inference
-- is of that page's program, which is the page and what it imports.
esTypeInfo :: EvalService -> FileImport -> IO (CRes (VPrgm, InstrumentResults))
esTypeInfo svc@(EvalService s _ _ _) prgmName = recordAsk svc (Just prgmName) >> csTypeInfo s prgmName

-- | 'CtServiceApi.csRunFun' of a page, recorded as an ask about that page's scope: a run is of that
-- page's program, which is the page and what it imports.
esRunFun :: EvalService -> FileImport -> String -> IO (CRes DebugRun)
esRunFun svc@(EvalService s _ _ _) prgmName fun = recordAsk svc (Just prgmName) >> csRunFun s prgmName fun

-- | 'CtServiceApi.csRunExpr' of a page, recorded as 'esRunFun' is: the expression is compiled against
-- that page and means what that page makes it mean.
esRunExpr :: EvalService -> FileImport -> String -> IO (CRes DebugRun)
esRunExpr svc@(EvalService s _ _ _) prgmName src = recordAsk svc (Just prgmName) >> csRunExpr s prgmName src

-- | 'CtServiceApi.csValues' over a scope, recorded as asked about. The gatherer that comes back can
-- only be asked about pages of that scope, so recording the scope covers every page it will gather
-- from — which the gatherer could not record for itself.
esValues :: EvalService -> Maybe FileImport -> IO ([Name] -> FileImport -> CResT IO [(EExpr, Val)])
esValues svc@(EvalService s _ _ _) scope = recordAsk svc scope >> csValues s scope

-- | 'CtServiceApi.csAsk' over a scope, recorded as 'esValues' is and for its reason: the asker that
-- comes back can only be asked about pages of that scope, and cannot record for itself.
esAsk :: EvalService -> Maybe FileImport -> IO (Name -> FileImport -> CRes (Val -> CResT IO Val))
esAsk svc@(EvalService s _ _ _) scope = recordAsk svc scope >> csAsk s scope

-- | Write down a scope a display asked about. An unscoped ask is about every page there is, which is
-- what asking without naming a page means.
recordAsk :: EvalService -> Maybe FileImport -> IO ()
recordAsk svc Nothing = recordEvery svc
recordAsk (EvalService _ _ _ asks) (Just prgmName) =
  modifyIORef' asks $ \a -> a{aaScopes = S.insert prgmName (aaScopes a)}

recordEvery :: EvalService -> IO ()
recordEvery (EvalService _ _ _ asks) = modifyIORef' asks $ \a -> a{aaEvery = True}

data EPrim
  = EPrim String (H.HashMap String Val -> Either String Val)
  -- | A primitive that also reads the evaluation 'Env'. This is what a primitive reporting on the
  -- program itself needs (the webdocs pages), since the program is only reachable through
  -- @evTbEnv@. It must be a primitive rather than a 'MacroFunction' because a macro runs during
  -- TreeBuild, which would bake the program into every built arm ('evalBuildAll') instead of
  -- computing it per evaluation.
  | EEnvPrim String (Env -> H.HashMap String Val -> Either String Val)
  -- | A primitive that performs an effect: a method of the Catln @IO@ object. It is handed the
  -- 'IOHandle' of the IO value it was called on, and is the only kind of primitive that runs in
  -- 'IO' — so an effect can only happen where a program holds an IO value and calls a method of it.
  -- 'Eval.evalCallTree' refuses one called on anything else.
  | EIOPrim String (IOHandle -> H.HashMap String Val -> IO (Either String Val))
  -- | A primitive that evaluates more of the program: one showing values through a class of the
  -- program has to build and run the arms of that class, which is an evaluation of its own. It runs
  -- as a step of the evaluation that called it, so what it asks for is asked the way any other
  -- step's is. It is handed no 'IOHandle', so it can perform no effect: evaluating Catln reaches one
  -- only through an IO value, and none is passed to it.
  | EEvalPrim String (H.HashMap String Val -> EvalM Val)
  deriving (Generic)

ePrimName :: EPrim -> String
ePrimName (EPrim n _)     = n
ePrimName (EEnvPrim n _)  = n
ePrimName (EIOPrim n _)   = n
ePrimName (EEvalPrim n _) = n

instance Eq EPrim where
  a == b = ePrimName a == ePrimName b

instance Hashable EPrim where
  hashWithSalt s p = s `hashWithSalt` ePrimName p

instance ToJSON EPrim where
  toJSON EPrim{}     = object ["type".=("EPrim" :: String)]
  toJSON EEnvPrim{}  = object ["type".=("EEnvPrim" :: String)]
  toJSON EIOPrim{}   = object ["type".=("EIOPrim" :: String)]
  toJSON EEvalPrim{} = object ["type".=("EEvalPrim" :: String)]


-- | A page a program was read from: as it was written, and what the compiler said about it.
--
-- The notes are lazy and stay so. They include what building the page's trees reported, and reading
-- that builds them ('CtService.ctssPagesOf'), so only an evaluation that shows the page to a reader
-- ('Eval.llmShowPage') pays for it.
--
-- The scope is lazy for the same reason: the typechecked pages the page reaches, itself among them,
-- under the import each was read as and with the imports each reaches. It is what source compiled as
-- though it were written on the page is typechecked against ('Eval.parseExprPrim'), and only an
-- evaluation that compiles some pays for gathering it.
data EvalPage = EvalPage {
  epRaw   :: RawPrgm (),
  epNotes :: [CNote],
  epScope :: CRes [(TPrgm, FileImport, [FileImport])]
  }

-- | The pages a program was read from, under the path each was parsed as.
type EvalPages = H.HashMap String EvalPage

-- | An arrow the evaluation is inside: the id of the node it will close as, the arrow itself, and
-- the calls the ENCLOSING arrow had already finished when this one was entered.
--
-- The id is assigned when the arrow is ENTERED rather than derived when it closes, because what wants
-- to name a node is inside it at the time: a @#debug@ recorded partway through a call has to say
-- which call it came from, and the state a closing node is identified from does not exist yet.
--
-- The third is what makes the result a tree of calls rather than a chain of them. 'evTreebugClosed'
-- holds the calls finished so far at ONE level, so entering an arrow has to put the enclosing level's
-- aside and start an empty one; closing takes it back out and adds the call that just finished to it.
-- Without it a closing arrow adopts whatever closed before it, whether or not that call happened
-- inside it — so an arrow calling two others in sequence reads as the second one calling the first,
-- and a run reads as a single spine however it branched.
type EvalTreebugOpen = (String, AnyObjArr, [EvalTreebugClosed], S.HashSet String)

-- | One call the evaluation made: the arrow it entered, what it was given, what it returned, the
-- calls made inside it, the id it was entered under, and the display the call has of its own.
--
-- The last is 'Nothing' for every call as it closes, and is filled afterwards by
-- 'Eval.attachCallShows' for the few whose function declares an arm of
-- 'CtConstants.dshowCallName'. Afterwards rather than here because a display is built by evaluating,
-- and evaluating while an evaluation is being recorded would record the display's own calls as part
-- of the run they describe.
data EvalTreebugClosed = EvalTreebugClosed AnyObjArr Val Val [EvalTreebugClosed] String (Maybe Val)
  deriving (Eq, Generic, Hashable, ToJSON)

data Env = Env { evObjMap         :: EObjectMap
               , evTypeEnv        :: TypeEnv (ObjArrTypeGraph Expr EvalMetaDat)
               , evArgs           :: Args
               , evExEnv          :: ResExEnv
               , evTbEnv          :: TBEnv
               -- | 'evTbEnv' extended with the arrows of a set of closures ('valClosureArrows'),
               -- keyed by those arrows' objects and results. An arm entered with closures is built
               -- where they are arrows, and each extension is a type environment with a cache of its
               -- own; made afresh for every arm built it would start every build with that cache
               -- cold.
               , evClosureEnvs    :: H.HashMap [(Type, Type)] TBEnv
               -- | The compilation service this evaluation may ask about the pages there are, and
               -- about the stages of them it cannot reach on its own.
               --
               -- 'Nothing' for an evaluation whose caller gave it none, which a primitive reports
               -- rather than answering around. Notably that is every evaluation the service itself
               -- starts on a primitive's behalf ('evalBaseEnv'): a gathering of what pages declare,
               -- a debugger's run, a page's tests. That is what bounds re-entry to one level — an
               -- annotation may ask the service to evaluate something, and what it evaluates cannot
               -- ask again — and it falls out of those paths building their environments with no
               -- service rather than out of anything tracking how deep a call is.
               , evService        :: Maybe EvalService
               -- | The definition an annotation being evaluated is written on, for a primitive
               -- reporting on that definition. An annotation is local to the program it is written
               -- in and to whatever it is written on, and only the second is otherwise lost: the
               -- annotation is built as an expression of its own, with nothing left of what it
               -- annotates. Nothing for a program's own annotation, which is written on the program
               -- rather than on any definition of it.
               , evAnnotObj       :: Maybe EObjArr
               -- | The pages the program was read from, as written, under the names the positions
               -- of what was parsed from them carry ('CtService.rawPagesByPath'). What a call asking
               -- for the page it is written in is answered from ('Eval.Runtime.Prgm.pageAt'). Empty
               -- for an evaluation whose caller holds no pages, where such a call is reported.
               , evPages          :: EvalPages
               -- | The pages being shown, by hash, while their annotations are evaluated
               -- ('Eval.llmShowPage'). An annotation of a page being shown that asks for the page to
               -- be shown again could only ever be answered by itself, so it is refused instead.
               , evShowingPages   :: S.HashSet Int
               , evCallStack      :: [String]
               , evCoverage       :: H.HashMap EObjArr Int
               , evTreebugOpen    :: [EvalTreebugOpen]
               , evTreebugClosed  :: [EvalTreebugClosed]
               -- | The calls of 'evTreebugClosed' already recorded as the steps of an expression
               -- ('EvalExprRun'). An expression is evaluated inside another -- the condition of the
               -- arm a call is dispatched to is evaluated while the call is -- and the calls the inner
               -- one made close at the same level as the outer one's, so without this the outer one
               -- would count them as its own steps. Put aside and restored with 'evTreebugClosed'.
               , evTreebugClaimed :: S.HashSet String
               -- | The next id to give an arrow being entered. A counter rather than something
               -- derived from the arrow, since the same definition is entered as many times as it is
               -- called and each of those is a different node of the tree.
               , evTreebugNextId  :: Int
               -- | Every @#debug@ the run has recorded so far, most recent first. Unlike
               -- 'evTreebugClosed', which is replaced by each arrow that closes over it, this only
               -- ever grows: what a reader of a run wants is every value it recorded, in the order
               -- it recorded them, however many calls of however many definitions they came from.
               , evDebugs         :: [EvalDebug]
               -- | Every expression the run rewrote through calls, most recent first ('EvalExprRun').
               , evExprRuns       :: [EvalExprRun]
               -- | Every annotation of a definition the run evaluated, most recent first
               -- ('EvalAnnotRun').
               , evAnnotRuns      :: [EvalAnnotRun]
               }

-- | One expression of a run that was rewritten through calls: where it is written, the call it was
-- evaluated inside, the calls that rewrote it, and the value it came to.
--
-- Recorded because a call's output is not what the expression making it came to. An arrow's result
-- is the term its body rewrites to, and the rest of the rewriting belongs to whoever called it: the
-- body of @total@ is @scale(…) + sum * sign(…)@, so the call of @total@ closes with the output
-- @operator+(l= 60, r= 3)@, and the @63@ it comes to is reached by the caller applying @operator+@
-- afterwards, as a primitive, which enters no arrow and has no node. What the expression came to is
-- only in hand where the whole of its call tree has run, which is here.
--
-- 'exSteps' are the calls entered running the expression's call tree, in the order they were
-- entered, and not those of its arguments, which are expressions of their own and are recorded as
-- such. So the first step is the call the expression was written as, and the rest are the calls that
-- rewrote what it returned. An expression rewritten by no call -- a primitive applied to values,
-- such as @sum * sign(x= a)@ once its arguments are in hand -- has none, and is recorded for its value.
--
-- 'exNode' is the call it was evaluated inside, as 'edNode' is: 'Nothing' for an expression run
-- inside no definition, which is what an expression a debugger was asked about is.
--
-- 'exIds' are the ids of the expression as written: its own meta's, and that of the name it applies,
-- which is all that is left of a written reference to a line once desugar has applied the enclosing
-- function's arguments to it (@sum@ becomes @total.sum(a= a, b= b)@). They are how a debugger finds
-- each part of a line it shows among what the line evaluated.
data EvalExprRun = EvalExprRun { exNode  :: Maybe String
                               , exPos   :: CodeRange
                               , exIds   :: [UUID]
                               , exSteps :: [String]
                               , exValue :: Val
                               } deriving (Eq, Generic, ToJSON)

-- | One annotation of a definition evaluated by a call of it: the call, the annotation by the id and
-- position it is written with, what it evaluated to, and the calls made evaluating it.
--
-- An annotation is evaluated inside the call of the definition it is written on, before the body,
-- so without this the calls it makes read as calls of the body: an @#assert@ comparing a line's value
-- would look like the line being used once more than the body uses it.
data EvalAnnotRun = EvalAnnotRun { eaNode  :: String
                                 , eaAnnot :: UUID
                                 , eaPos   :: CodeRange
                                 , eaValue :: Val
                                 , eaCalls :: [String]
                                 } deriving (Eq, Generic, ToJSON)

-- | The monad an evaluation runs in.
--
-- Over 'IO' because an evaluation performs the effects a program asks for as it reaches them: a
-- method of the Catln IO object ('EIOPrim') runs its effect when it is evaluated, and what it
-- answers with is a value the rest of the evaluation goes on to use. Only those methods reach the
-- 'IO'; every other part of an evaluation is as pure as it was.
type EvalM = StateT Env (CResT IO)

-- | A result of the compiler's own, as a step of an evaluation.
liftCRes :: CRes a -> EvalM a
liftCRes = lift . asCResT

-- | One @#debug@ of a run: where it is written, what it evaluated to there, and which node of the
-- evaluation tree it was recorded in.
--
-- Where it is written rather than what was written: an expression is rendered back out by
-- 'Syntax.Ct.Formatter.formatExpr', which takes the parsed form, and by the time an annotation is
-- being evaluated the parsed form is several stages upstream. The position is on the annotation
-- itself rather than on its argument, which matters for an argument that is an operator apply:
-- @Syntax.Ct.Parser.Expr.mkOp2@ builds one with an empty meta, so @#debug(d= x + 1)@ has a position
-- for the annotation and none for the @x + 1@ inside it.
--
-- 'edNode' is the id the enclosing arrow was given when it was entered, so a reader following a
-- record arrives at the call it came from rather than at the definition, which every call of it
-- shares.
data EvalDebug = EvalDebug { edPos   :: CodeRange
                           , edValue :: Val
                           , edNode  :: String
                           } deriving (Eq, Generic, ToJSON)

data EvalResult = EvalResult { erCoverage :: H.HashMap EObjArr Int
                             , erTreebug  :: [EvalTreebugClosed]
                             -- | The run's @#debug@ records, in the order they were recorded.
                             , erDebugs   :: [EvalDebug]
                             -- | The expressions it rewrote through calls, in the order they finished.
                             , erExprs    :: [EvalExprRun]
                             -- | The annotations of definitions it evaluated, in the order evaluated.
                             , erAnnots   :: [EvalAnnotRun]
                             } deriving (Eq, Generic, ToJSON)

-- | One debugger run, and the tree of what it evaluated. The output is captured through a 'MockIO'
-- rather than written, so running a program to report on it does not print that program's output to
-- the reporting process's own stdout.
--
-- What a run /ends with/ depends on what was run, so exactly one of 'drExit' and 'drValue' is set. A
-- program is run for its effects and ends by exiting, having printed; an expression is run for its
-- value and is given no @\/io@ handle to print with, so it has neither output nor an exit code. They
-- are two fields rather than one because the two are not the same answer rendered differently: a
-- reader of a program run wants to know it exited 0, and a reader of an expression run wants the
-- value, and neither reads as the other.
--
-- 'drSources' is the text of every file the run's positions name, by the name they name it by, so a
-- reader shown where a call is written can be shown what is written there without asking for each
-- page it reached. A run evaluates whatever its page imports, and the pages a reader might open are
-- the few it actually entered rather than every page in scope. It is filled by whoever holds the
-- pages ('withRunSources'); a run is made empty of them.
data DebugRun = DebugRun { drExit    :: Maybe Integer
                         , drOutput  :: String
                         , drValue   :: Maybe Val
                         , drResult  :: EvalResult
                         , drSources :: H.HashMap String String
                         -- | The run read by the page that was written rather than by the arrows it
                         -- was compiled to ('Eval.SyntaxTree.withRunSyntax'). Filled by whoever holds
                         -- the pages, as 'drSources' is; a run is made with none.
                         , drSyntax  :: SyntaxRun
                         } deriving (Eq, Generic, ToJSON)

-- | A run read by the functions its pages write: a tree of the calls of each function as written,
-- each holding what each of that function's lines came to in it.
--
-- The evaluation tree ('erTreebug') is of arrows, and desugar writes a function as several: each
-- intermediate line of a definition is an arrow of its own, called wherever the line is used, and a
-- bind or a branch is continued in an arrow of its own. This regroups those back into the function
-- they were written as. The arrows' nodes are still what a line is broken down by, so a line points
-- at them by id rather than holding a copy.
--
-- 'srStatements' holds the written statement of each function 'srCalls' calls, so a reader has the
-- functions it is shown without every page they came from.
data SyntaxRun = SyntaxRun { srCalls      :: [SyntaxCall]
                           , srStatements :: [SyntaxStatement]
                           } deriving (Eq, Generic, ToJSON)

-- | A statement of a page that a run called, by the id its object's meta has
-- ('Syntax.Ct.MapRawMeta.rawStatementExprIds'), with the lines written under it.
--
-- 'ssLines' are the statements under it that are its own lines, which a call of it has a run of
-- whenever it evaluated them. The rest of what is written under it is a function nested in it, which
-- takes parameters of its own and whose lines are run in calls of that function instead.
data SyntaxStatement = SyntaxStatement { ssId    :: UUID
                                       , ssFile  :: String
                                       , ssTree  :: RawStatementTree RawExpr ()
                                       , ssLines :: [UUID]
                                       } deriving (Eq, Generic, ToJSON)

-- | One call of a function as written: its statement, the node of the arrow that was entered for
-- it, what each of its lines came to in it, and the calls of functions it made, in the order made.
--
-- A line with no run was not evaluated in this call. Desugar evaluates a line wherever it is used
-- rather than once, so a line used twice has two runs, and one never used has none.
data SyntaxCall = SyntaxCall { scStmt  :: UUID
                             , scNode  :: String
                             , scLines :: [SyntaxLine]
                             , scCalls :: [SyntaxCall]
                             } deriving (Eq, Generic, ToJSON)

-- | The runs of one statement written in a function: a line of it, an annotation of it, its result,
-- or the function's own statement, whose run is what the call came to.
data SyntaxLine = SyntaxLine { slStmt :: UUID
                             , slRuns :: [LineRun]
                             } deriving (Eq, Generic, ToJSON)

-- | One evaluation of a line: what it came to, the nodes of the evaluation tree it was evaluated
-- through, which break it down, and the node that is the line itself where it has one.
--
-- The value is 'Nothing' where the run recorded nothing the line came to, rather than being filled
-- with something else: a node's output is the term its arrow rewrote to, which is not the value.
--
-- 'lrNode' is the arrow desugar made of the line, entered wherever the line was used: its node
-- appears inside the breakdown of whatever used the line, which is where a reader is led back to the
-- line from. A result or an annotation is evaluated in the arrow of the function it is written in,
-- and has none of its own.
--
-- 'lrIn' is the node the line's expression was evaluated in -- the line's own arrow, or the arrow of
-- the function a result or an annotation is written in -- which is the 'exNode' of each part of the
-- line that was evaluated ('EvalExprRun'). A bind has none: what it binds is evaluated as part of
-- the function's result, and the run of the bind is the value it bound.
data LineRun = LineRun { lrValue :: Maybe Val
                       , lrInner :: [String]
                       , lrNode  :: Maybe String
                       , lrIn    :: Maybe String
                       } deriving (Eq, Generic, ToJSON)

emptySyntaxRun :: SyntaxRun
emptySyntaxRun = SyntaxRun [] []

-- | The files a run's positions name: those of every arrow it entered and of every @#debug@ it
-- recorded.
runSourceNames :: DebugRun -> S.HashSet String
runSourceNames DebugRun{drResult=EvalResult{erTreebug, erDebugs}} =
  S.fromList $ concatMap nodeNames erTreebug ++ concatMap (rangeNames . edPos) erDebugs
  where
    nodeNames (EvalTreebugClosed oa _ _ children _ _) = arrowNames oa ++ concatMap nodeNames children
    arrowNames (Left oa)  = execWriter $ mapMetaObjArrM tellNames Nothing oa
    arrowNames (Right oa) = execWriter $ mapMetaObjArrM tellNames Nothing oa
    tellNames _ m = tell (rangeNames $ getMetaPos m) >> return m
    rangeNames (Just (start, end)) = [sourceName start, sourceName end]
    rangeNames Nothing             = []

-- | A run with the text of the files it names, out of the files it could name: those of the pages it
-- was run in, by the name their positions carry. A named file with no text offered is left out, and
-- a reader of the run is told there is no source for it rather than shown a wrong one.
withRunSources :: [(String, String)] -> DebugRun -> DebugRun
withRunSources offered run = run{drSources = H.fromList [(name, src) | (name, src) <- offered, name `S.member` named]}
  where named = runSourceNames run

-- | Which family of test a 'TestResult' came from. A @#test@ or @#example@ definition produces a
-- 'TestSimple' when it takes no arguments and a 'TestProperty' when the test engine has to generate
-- them; the others are derived from the program rather than written in it.
data TestKind
  = TestSimple
  | TestProperty
  | TestOverlapAgreement
  | TestRefinementAgreement
  | TestRefinementProof
  | TestPrintGolden
  deriving (Eq, Show, Generic, ToJSON)

-- | One test of a program, after it has been run: what it was derived from, whether it passed, and
-- what it reported. This is the result rather than the check, so it can be cached, served, and
-- displayed; the unrun form a test runner wants is @Eval.Testing.TestDesc@.
--
-- It lives here beside 'DebugRun' for the same reason that does: 'SSF' carries it, and
-- @Eval.Testing@ is downstream of this module.
data TestResult = TestResult
  { trName    :: String
    -- | The definition the test was derived from, and the identity of the arm it was derived from.
    -- A name alone does not identify an arm — a function's definitions all share its 'oaObjPath' —
    -- so a display asking for one definition's tests matches on 'trOwnerId', which is that arm's own
    -- meta. It is also what attributes a test to the page that wrote it.
  , trOwner   :: String
  , trOwnerId :: UUID
  , trKind    :: TestKind
  , trPassed  :: Bool
    -- | How many generated cases were run, for a test the engine generated inputs for. A test
    -- written without arguments is run once and has none.
  , trCases   :: Maybe Int
    -- | The input each of those cases ran on, in the order they ran, as many entries as 'trCases'.
    -- A count alone does not say what a passing test covered — a law checked against a hundred empty
    -- strings and one checked against a hundred varied ones both report a hundred cases — so the
    -- inputs are carried rather than counted. A test with nothing generated for it has none.
    --
    -- When the test failed, the last entry is the input it first failed on. That is not the same
    -- value as the counterexample in 'trNotes', which is what the engine shrank that input down to.
  , trInputs  :: [String]
    -- | What a failure reported: the notes it failed with, and for a generated test the rendered
    -- counterexample. Empty for a test that passed.
  , trNotes   :: Maybe String
  } deriving (Eq, Show, Generic, ToJSON)

-- | The identity of the definition a test was derived from: the meta of the arm's object, which is
-- what distinguishes one arm from its siblings (they all share the function's 'oaObjPath') and what
-- attributes the test to the page that wrote it.
oaTestId :: EObjArr -> UUID
oaTestId = getMetaId . getExprMeta . oaObjExpr

-- | What an IO value performs its effects through.
--
-- An evaluation performs an effect at the moment it evaluates the IO method asking for it, so a
-- program's effects happen in the order its IO values are threaded through its calls. The handle is
-- what decides where those effects go: 'Eval.Runtime.System.realIOHandle' is the process's own
-- stdin, stdout, environment and network, and 'mockIOHandle' is whatever a caller reading the run
-- back gave it (a test, or a debugger reporting on a run from inside the doc server).
data IOHandle = IOHandle
  { iohPutLine   :: String -> IO ()
  -- | The next line of input, or why there is none.
  , iohGetLine   :: IO (Either InputFailure String)
  -- | The value of an environment variable, where it is set.
  , iohLookupEnv :: String -> IO (Maybe String)
  , iohHttpPost  :: HttpPost
  }

-- | Why there is no line of input to read. A program is answered with one of these as the
-- @\/Catln\/InputErr@ of the same name (@stack\/core\/compile.ct@).
data InputFailure
  -- | The input has ended.
  = EndOfInput
  -- | The run has no input to read at all, as a mocked one has not.
  | NoInput
  deriving (Eq, Show)

-- | Sends a POST request: the URL, the headers as name and value, and the body. Answers with the
-- response body, or why there is none.
type HttpPost = String -> [(String, String)] -> String -> IO (Either HttpFailure String)

-- | Why a request has no response body to answer with. A program is answered with one of these as
-- the @\/Http\/HttpErr@ of the same name (@stack\/http\/main.ct@), so each says only what a program
-- could tell apart, with the details as text.
data HttpFailure
  -- | The URL is not one a request can be sent to, and why.
  = HttpInvalidUrl String
  -- | The request could not be made, as where the host does not answer, and why.
  | HttpUnreachable String
  -- | The response's status is not a success: the status, and the body it came with, which is where
  -- an API says what was wrong.
  | HttpStatus Int String
  -- | The run reaches no network, so the request was not sent.
  | HttpNotSent
  deriving (Eq, Show)

-- | A @\/Data\/Result@ of the value given, or of the error, as an evaluation holds one.
resultVal :: Either Val Val -> Val
resultVal (Right v) = TupleVal ctResultOk (H.singleton (runtimeArgKey ctResultOkValue) v)
resultVal (Left e)  = TupleVal ctResultFail (H.singleton (runtimeArgKey ctResultFailErr) e)

-- | A mock IO handle that captures printed lines in an internal buffer instead of writing to real
-- stdout. Create with 'newMockIO' or 'newMockIOWith', pass to 'Eval.evalRunMock', then read
-- captured output with 'getMockIOOutput'.
data MockIO = MockIO
  { mockOut  :: IORef [String]
  -- | The whole environment the run can see.
  , mockEnv  :: H.HashMap String String
  , mockHttp :: HttpPost
  }

-- | A mock with an empty environment and no network: every request is refused rather than made.
newMockIO :: IO MockIO
newMockIO = newMockIOWith H.empty (\_ _ _ -> return $ Left HttpNotSent)

-- | A mock that sees the environment given, and sends requests through the function given.
newMockIOWith :: H.HashMap String String -> HttpPost -> IO MockIO
newMockIOWith env http = do
  out <- newIORef []
  return $ MockIO out env http

-- | Returns all lines printed to the MockIO, joined with newlines
-- (equivalent to what would have appeared on stdout).
getMockIOOutput :: MockIO -> IO String
getMockIOOutput MockIO{mockOut} = unlines . reverse <$> readIORef mockOut

-- | The handle a 'MockIO' captures through, for seeding an evaluation's 'IOVal'.
--
-- It has no input. A mocked run is one whose caller reads its output back, and a caller doing that
-- is not a person at a terminal: reading the process's real stdin from inside one would block a doc
-- server on a line nobody is going to type.
mockIOHandle :: MockIO -> IOHandle
mockIOHandle MockIO{mockOut, mockEnv, mockHttp} = IOHandle
  { iohPutLine = \msg -> modifyIORef mockOut (msg:)
  , iohGetLine = return $ Left NoInput
  , iohLookupEnv = \name -> return $ H.lookup name mockEnv
  , iohHttpPost = mockHttp
  }

-- | The arms of a 'Semantics.Val.Val' only an evaluation knows the inside of.
--
-- These are what keeps a value from being data: each names a type of this module, and this module
-- is in one recursive knot with the whole runtime — a closure holds a 'TExpr', a 'TExpr' holds a
-- value, a call tree holds an 'EPrim' which holds an 'Env' which holds the compilation service. The
-- data arms are in "Semantics.Val" instead, below all of it, which is what lets a type say how it
-- is read as a Catln value beside the type itself.
data EvalRuntime
  -- | A function-typed argument's value: the lambda plus the arg env captured at its
  -- creation site, so body references to enclosing args (@fn(v)= fn(v= v)@'s inner @fn@)
  -- resolve lexically at application.
  = ObjArrRV (ObjArr TExpr EvalMetaDat) Args
  -- | An IO value: the exit code the run will end with, and the handle its effects go through.
  | IORV Integer IOHandle
  | LLVMRV (LLVM ())
  | LLVMQueueRV [(TExpr EvalMetaDat, EObjArr)]
  -- | LLVMOperandRV Type (Codegen AST.Operand)
  | LLVMIORV (Codegen ())

-- | An evaluated value, at this evaluation's runtime arms.
type Val = SV.Val EvalRuntime

-- | The arguments of an evaluated value, by the name each is declared under.
type Args = SV.Args EvalRuntime

-- | The runtime arms as they have always been written. Bidirectional, so these are constructors as
-- well as patterns and every existing site reads and builds a value unchanged.
pattern ObjArrVal :: ObjArr TExpr EvalMetaDat -> Args -> Val
pattern ObjArrVal oa as = RuntimeVal (ObjArrRV oa as)

pattern IOVal :: Integer -> IOHandle -> Val
pattern IOVal i io = RuntimeVal (IORV i io)

pattern LLVMVal :: LLVM () -> Val
pattern LLVMVal l = RuntimeVal (LLVMRV l)

pattern LLVMQueue :: [(TExpr EvalMetaDat, EObjArr)] -> Val
pattern LLVMQueue q = RuntimeVal (LLVMQueueRV q)

pattern LLVMIO :: Codegen () -> Val
pattern LLVMIO c = RuntimeVal (LLVMIORV c)

-- | The arms above are every 'Semantics.Val.RuntimeVal' there is, so a match listing them together
-- with the data arms is total. Without this every existing @case@ over a value would be read as
-- missing the 'Semantics.Val.RuntimeVal' case and @-Wall@ would report it.
{-# COMPLETE IntVal, FloatVal, CharVal, TupleVal, ObjArrVal, IOVal, LLVMVal, LLVMQueue, LLVMIO, NoVal #-}

-- | What an evaluation says about its own values.
--
-- An 'IOVal' is not compared but refused, against ANY value rather than against another 'IOVal':
-- what a comparison of one would mean is the question, and answering it False would be answering
-- it. That is why 'SV.rvEqVal' is given the whole other value.
instance RuntimeValue EvalRuntime where
  rvEqVal IORV{} _ = error "Can't determine equality with IOVal"
  rvEqVal _ _      = False

  rvShow (ObjArrRV oa _) = printf "(ObjArrVal %s)" (show oa)
  rvShow IORV{}          = "IOVal"
  rvShow LLVMRV{}        = "LLVMVal"
  rvShow LLVMQueueRV{}   = "LLVMQueue"
  rvShow LLVMIORV{}      = "LLVMIO"

  rvHash s (ObjArrRV oa _) = s `hashWithSalt` oa
  rvHash s (IORV i _)      = s `hashWithSalt` i
  rvHash s LLVMRV{}        = s
  rvHash s LLVMQueueRV{}   = s
  rvHash s LLVMIORV{}      = s

  rvToJSON ObjArrRV{}    = object ["tag" .= ("ObjArrVal" :: String)]
  rvToJSON IORV{}        = object ["tag" .= ("IOVal" :: String)]
  rvToJSON LLVMRV{}      = object ["tag" .= ("LLVMVal" :: String)]
  rvToJSON LLVMQueueRV{} = object ["tag" .= ("LLVMQueue" :: String)]
  rvToJSON LLVMIORV{}    = object ["tag" .= ("LLVMIO" :: String)]

-- | A build result, as the type a build is built toward.
--
-- With its arguments left unspecified ('PtArgAny'), which is what @CatlnResult..@ says where the
-- type is written in Catln. The entries are a list of a recursive class, and writing that out here
-- would be this module's copy of a shape @stack\/core\/compile.ct@ already declares — a copy which
-- would then have to be kept in step with it, and which says nothing a build needs: what a build
-- has to reach is a result, and what is in it is read back out of the value afterwards by
-- 'valResultTree'.
resultLeaf, queueLeaf :: PartialType
resultLeaf = (partialVal (mkName catlnResultPath)){ptArgMode=PtArgAny}
queueLeaf = partialVal "llvmQueue"

-- | What a function run in an io answers with: its value in the io it leaves ('ContextStr').
ioContextVal :: Val -> Val -> Val
ioContextVal v io = TupleVal ContextStr (H.fromList [(contextValStr, v), (ioStr, io)])

-- | The unit value @()@.
unitVal :: Val
unitVal = TupleVal anonStr H.empty

-- | The arguments a primitive is given from the value it is called on. A primitive declared in a
-- context (@println{io -> IO}(msg -> String)@) is called on the call in its states
-- (@ContextIn(value= println(msg= ..), io= ..)@), and is given the call's arguments beside the
-- states.
primInputArgs :: Val -> Maybe (H.HashMap String Val)
primInputArgs (TupleVal n args)
  | n == ContextInStr, Just (TupleVal _ callArgs) <- H.lookup contextValStr args = Just $ H.union callArgs (H.delete contextValStr args)
  | otherwise = Just args
primInputArgs _ = Nothing

resultType :: Type
resultType = singletonType resultLeaf

-- | The singleton typeset of a value: a 'Type' that carries a primitive's concrete value as a
-- 'Constant' (e.g. @IntVal 5@ becomes the type @{5}@, a subtype of @Integer@). This is what a
-- refinement membership check needs — @isSubtypeOf (getValType v) refinedType@ — which a bare
-- 'PartialType' cannot express, as it has nowhere to hold the constant. For arrow building and
-- dispatch matching, where the concrete constant is irrelevant, use 'getValPartialType'.
getValType :: Val -> Type
getValType (IntVal n)   = constantType (CInt n)
getValType (FloatVal f) = constantType (CFloat f)
getValType (CharVal c)  = constantType (CChar c)
getValType (TupleVal name args) = singletonType (partialVal (mkName name)){ptArgs=H.fromList $ map fromArg $ H.toList args}
  where fromArg (argName, argVal) = (partialKey (mkName argName), getValType argVal)
getValType (ObjArrVal oa _) = getExprType $ oaObjExpr oa
getValType IOVal{}     = singletonType ioLeaf
getValType LLVMVal{}   = singletonType resultLeaf
getValType LLVMQueue{} = singletonType queueLeaf
getValType LLVMIO{}    = singletonType ioLeaf
getValType NoVal = error "getValType of NoVal"

-- | The structural 'PartialType' of a value — its shape, ignoring any concrete constant. Used for
-- arrow building and dispatch matching (which key on a 'PartialType'); the runtime value's exact
-- constant does not discriminate between arrows there.
getValPartialType :: Val -> PartialType
getValPartialType IntVal{} = intLeaf
getValPartialType FloatVal{} = floatLeaf
getValPartialType CharVal{} = charLeaf
getValPartialType (TupleVal name args) = (partialVal (mkName name)){ptArgs=H.fromList $ map (\(argName, argVal) -> (valArgKey argName argVal, singletonType $ getValPartialType argVal)) $ H.toList args}
getValPartialType (ObjArrVal oa _) = getSingleton $ getExprType $ oaObjExpr oa
getValPartialType IOVal{}     = ioLeaf
getValPartialType LLVMVal{} = resultLeaf
getValPartialType LLVMQueue{} = queueLeaf
-- getValPartialType (LLVMOperand t _) = case t of
  -- UnionType leafs -> case splitUnionType leafs of
    -- [partial] -> partial
    -- _         -> error "could not getValPartialType without a single partial"
  -- _ -> error $ printf "could not get non sum getValPartialType %s" (show t)
getValPartialType LLVMIO{} = ioLeaf
getValPartialType NoVal = error "getValPartialType of NoVal"

-- | The arrows of the closures a value holds, at any depth: each closure ('ObjArrVal') adds the
-- arrow from its object (the pattern's type, under the pattern's name) to its result, as
-- 'TreeBuild.withCallArrows' gives a call's lambda. A value's higher-order slots hold the closures'
-- input objects, so a tree built for the value reaches what each returns through these.
valClosureArrows :: Val -> [ObjArr Expr EvalMetaDat]
valClosureArrows (TupleVal _ args) = concatMap arrowsOf (H.elems args)
  where
    arrowsOf (ObjArrVal ObjArr{oaObj=Just o, oaArr=Just (_, arrM)} _) = [ObjArr (Just (Value (getExprMeta o) (exprPath o))) ArgObj Nothing [] (Just (Nothing, arrM))]
    arrowsOf v = valClosureArrows v
valClosureArrows _ = []

-- | The key a value's argument is filled under. A closure argument ('ObjArrVal') is keyed with
-- its parameters (@/fn(/v)@), as a typed higher-order slot is: the slot holds the closure's input
-- object, and the key names the arguments that object takes. Only its arguments: a typed slot is
-- keyed by what is written on it ('Semantics.Prgm.inExprSingleton'), and the variables of the
-- closure's object (its hop parameters, @fn[/A=..]@) are not written there. Keyed by the bare name it would not be
-- the slot an arm's object spells, and a definition's argument read from the value would find
-- nothing there. Every other argument is keyed by its name.
valArgKey :: String -> Val -> PartialKey
valArgKey argName argVal@ObjArrVal{} = (partialKey (mkName argName)){pkArgs = H.keysSet (ptArgs (getValPartialType argVal))}
valArgKey argName _                 = partialKey (mkName argName)

-- | How deep a type's constructor patterns look: one for each constructor, nested through the
-- arguments a constructor spells, and nothing for a class, a variable or ⊤, which name no
-- constructor to tell apart. @lenChars(cs=Cons(head=Char, tail=∀ConsList[Char]))@ looks three deep:
-- the call, the @Cons@, and the @Char@ it holds.
typePatternDepth :: Type -> Int
typePatternDepth (UnionType _ leafs _) = maximum (0 : map partialDepth (splitUnionType leafs))
  where partialDepth p = 1 + maximum (0 : map typePatternDepth (H.elems (ptArgs p)))
typePatternDepth _ = 0

-- | A value's type for choosing among arms, read only as deep as those arms' patterns look.
--
-- 'getValPartialType' is the whole value, a @Cons@ per element of a list. Choosing an arm by asking
-- whether that is within the arm's type costs work as deep as the value on every call, and a
-- recursion down a list makes one call per element: a walk over @n@ items came to @n@ cubed, and a
-- string of two hundred characters took minutes.
--
-- Past the given depth a tuple is its constructor's DECLARED form, with each type parameter bound
-- to the value of the field declared as it: the rest of a list of characters is
-- @Cons[T=Char](head=$T, tail=ConsList[T=$T])@. What an arm asks of the value there — that the
-- rest is a @ConsList[Char]@ — is what the typechecker already established of it, and the declared
-- form says it without walking the list. The parameters are read from the fields, recursing only
-- through them, so an element is typed as deeply as it is nested and never as long as the list.
--
-- A tuple with no data declaration (a call, a context) is read whole, as there is no declared form
-- to stand for it.
getValDispatchPartial :: (TypeGraph tg) => TypeEnv tg -> [Type] -> Int -> Val -> PartialType
getValDispatchPartial typeEnv pats d v@(TupleVal name args)
  | d > 0 = (partialVal (mkName name)){ptArgs=H.fromList $ map fromArg $ H.toList args}
  | otherwise = case typeGraphData typeEnv (mkName name) of
      (dp:_) -> dp{ptVars = H.union (boundVars dp) (ptVars dp)}
      []     -> getValPartialType v
  where
    -- A primitive argument is typed as the constant it is (@{0}@) where an arm names a constant in
    -- its slot (@isZero(x= 0)@): that arm asks whether the value is that constant, and the
    -- primitive is not within it. Elsewhere it stays its primitive, as reading a value no more
    -- precisely than the arms ask is what the depth above does too — and a constant inside a record
    -- is not read as a member of a class bounding that record ('Semantics.Types' does not place
    -- @Node(key={1}, ..)@ in @Map[K=Ord]@), so a constant nobody asked for loses arms that apply.
    fromArg (argName, argVal) | isConstantVal argVal, any hasConsts (slotPats argName) = (partialKey (mkName argName), getValType argVal)
    fromArg (argName, argVal) = (valArgKey argName argVal, singletonType $ getValDispatchPartial typeEnv (slotPats argName) (d - 1) argVal)
    -- The arms' types for one argument of this tuple: its slot in each arm leaf of this name.
    slotPats argName = [ t | UnionType _ leafs _ <- pats, p <- splitUnionType leafs
                           , relativeNameMatches (ptName p) (mkName name)
                           , (k, t) <- H.toList (ptArgs p), pkName k == mkName argName ]
    hasConsts (UnionType _ _ cs) = not (null cs)
    hasConsts _                  = False
    isConstantVal IntVal{}   = True
    isConstantVal FloatVal{} = True
    isConstantVal CharVal{}  = True
    isConstantVal _          = False
    boundVars dp = H.fromList
      [ (k, singletonType $ getValDispatchPartial typeEnv [] 0 argVal)
      | (argName, argVal) <- H.toList args
      , Just (TypeVar (TVVar k) TVInt _) <- [H.lookup (partialKey (mkName argName)) (ptArgs dp)] ]
getValDispatchPartial _ _ _ v = getValPartialType v

--- ResArrowTree
data MacroData = MacroData {
                               mdTbEnv  :: TBEnv
                             , mdObjSrc :: [ObjSrc]
                             }
newtype MacroFunction = MacroFunction (Expr EvalMetaDat -> MacroData -> CRes (Either Val (Expr EvalMetaDat)))
type ResBuildEnvFunction = TCallTree
type ResBuildPrims = H.HashMap TypeName (Either EPrim MacroFunction)
type ResBuildEnvItem = (PartialType, Maybe (ObjArr Expr EvalMetaDat), ResBuildEnvFunction)
type ResBuildEnv = H.HashMap TypeName [ResBuildEnvItem]
-- | Built arm trees, keyed by the source they were built for, the results of the closures that
-- source's arguments hold (the arrows the arm reaches its higher-order arguments through, which
-- the source's input objects do not spell), and the arm.
type ResExEnv = H.HashMap (PartialType, [Type], ObjArr Expr EvalMetaDat) (TExpr EvalMetaDat, [TExpr EvalMetaDat]) -- (result, [compAnnot trees])

data TBEnv = TBEnv {
    tbName    :: String
  , tbResEnv  :: ResBuildEnv
  , tbPrgm    :: Prgm Expr EvalMetaDat
  , tbTypeEnv :: TypeEnv (ObjArrTypeGraph Expr EvalMetaDat)
  -- | The definitions left out of 'tbResEnv' because they have no type to be called at, which is
  -- what a definition that failed to typecheck leaves behind. Kept rather than forgotten so that a
  -- call which cannot find one is told why, instead of being told only that nothing matched.
  , tbUntyped :: S.HashSet TypeName
  }

instance Eq MacroFunction where
  _ == _ = False

instance Hashable MacroFunction where
  s `hashWithSalt` _ = s

instance ToJSON MacroFunction where
  toJSON MacroFunction{} = object ["type".=("MacroFunction" :: String)]

instance Show TCallTree where
  show TCTId            = "TCTId"
  show (TCMatch _ opts) = printf "TCMatch (%s)" (show opts)
  show (TCSeq a b)      = printf "TCSeq (%s) (%s)" (show a) (show b)
  show (TCCond t ifs)   = printf "TCCond %s (%s)" (show t) (show ifs)
  show (TCRefine t)     = printf "TCRefine %s" (show t)
  show (TCArg t _)      = printf "TCArg %s" (show t)
  show (TCObjArr oa)    = printf "TCObjArr (%s)" (show oa)
  show (TCPrim oa _)    = printf "TCPrim (%s)" (show oa)
  show (TCMacro oa _)   = printf "TCMacro (%s)" (show oa)

data TCallTree
  = TCTId
  | TCMatch Type (H.HashMap PartialType TCallTree)
  | TCSeq TCallTree TCallTree
  | TCCond Type [(Maybe ([TExpr EvalMetaDat], EObjArr), TCallTree)] -- [((if, ifObj), then)]
  | TCRefine Type -- ^ The value as it is, when its value predicates hold of it ('Eval.valWithinType')
  | TCArg Type String
  | TCObjArr EObjArr
  | TCPrim EObjArr EPrim
  | TCMacro EObjArr MacroFunction
  deriving (Eq, Generic, Hashable, ToJSON)

data TExpr m
  = TCExpr (Meta m) Val
  | TValue (Meta m) TypeName
  | THoleExpr (Meta m) Hole
  | TAliasExpr (TExpr m) (TExpr m) -- ^ AliasTExpr baseExpr aliasExpr
  | TWhere (Meta m) (TExpr m) (TExpr m) -- ^ TWhere res baseExpr cond
  | TTupleApply (Meta m) (Meta m, TExpr m) (EApp TExpr m)
  | TCalls (Meta m) (TExpr m) TCallTree
  deriving (Eq, Generic, Hashable, ToJSON)

type AnyObjArr = Either EObjArr (ObjArr TExpr EvalMetaDat)

-- | Walks a built expression's metas as 'Expr' has them walked, so that whatever reads every meta of
-- a program -- where it is written, most of all -- can read a built one too.
--
-- A call tree is left as it is. It holds arrows of the program rather than of this expression, whose
-- metas are the program's and are not what this expression is made of.
instance MapMeta TExpr where
  mapMetaM f loc (TCExpr m v) = do
    m' <- f (ExprMeta loc ExprMetaConstant) m
    return $ TCExpr m' v
  mapMetaM f loc (TValue m n) = do
    m' <- f (ExprMeta loc ExprMetaVal) m
    return $ TValue m' n
  mapMetaM f loc (THoleExpr m h) = do
    m' <- f (ExprMeta loc ExprMetaHole) m
    return $ THoleExpr m' h
  mapMetaM f loc (TAliasExpr b a) = TAliasExpr <$> mapMetaM f loc b <*> mapMetaM f loc a
  mapMetaM f loc (TWhere m b c) = do
    m' <- f (ExprMeta loc ExprMetaWhere) m
    TWhere m' <$> mapMetaM f loc b <*> mapMetaM f loc c
  mapMetaM f loc (TTupleApply m (bm, be) arg) = do
    m' <- f (ExprMeta loc ExprMetaApplyArg) m
    bm' <- f (ExprMeta loc ExprMetaApplyArgBase) bm
    be' <- mapMetaM f loc be
    arg' <- case arg of
      EAppArg a     -> EAppArg <$> mapMetaObjArrM f (Just loc) a
      EAppVar vn vm -> EAppVar vn <$> f (ExprMeta loc ExprMetaApplyVarVal) vm
      EAppSpread a  -> EAppSpread <$> mapMetaM f loc a
    return $ TTupleApply m' (bm', be') arg'
  mapMetaM f loc (TCalls m e t) = do
    m' <- f (ExprMeta loc ExprMetaApplyArg) m
    e' <- mapMetaM f loc e
    return $ TCalls m' e' t

instance ExprClass TExpr where
  exprSpreadsArgs (TTupleApply _ (_, be) arg) = case arg of
    EAppSpread{} -> True
    _            -> exprSpreadsArgs be
  exprSpreadsArgs (TAliasExpr b _) = exprSpreadsArgs b
  exprSpreadsArgs (TWhere _ b _) = exprSpreadsArgs b
  exprSpreadsArgs (TCalls _ b _) = exprSpreadsArgs b
  exprSpreadsArgs _ = False
  getExprMeta :: TExpr m -> Meta m
  getExprMeta expr = case expr of
    TCExpr m _        -> m
    TValue m _        -> m
    THoleExpr m _     -> m
    TAliasExpr b _    -> getExprMeta b
    TWhere m _ _      -> m
    TTupleApply m _ _ -> m
    TCalls m _ _      -> m

  setExprMeta = undefined

  maybeExprPathM (TValue m n)             = Just (n, m)
  maybeExprPathM (TTupleApply _ (_, e) _) = maybeExprPathM e
  maybeExprPathM (TAliasExpr b _)         = maybeExprPathM b
  maybeExprPathM (TWhere _ b _)           = maybeExprPathM b
  maybeExprPathM (TCalls _ b _)           = maybeExprPathM b
  maybeExprPathM _                        = Nothing

  exprAppliedArgs (TValue _ _) = []
  exprAppliedArgs (TTupleApply _ (_, be) (EAppArg ae)) = ae : exprAppliedArgs be
  exprAppliedArgs (TTupleApply _ (_, be) EAppVar{}) = exprAppliedArgs be
  exprAppliedArgs (TTupleApply _ (_, be) (EAppSpread ae)) = exprAppliedArgs ae ++ exprAppliedArgs be
  exprAppliedArgs (TAliasExpr b _) = exprAppliedArgs b
  exprAppliedArgs (TCalls _ b _) = exprAppliedArgs b
  exprAppliedArgs e = error $ printf "Unsupported Expr exprAppliedArgs for %s" (show e)

  exprAppliedOrdVars (TValue _ _) = []
  exprAppliedOrdVars (TTupleApply _ (_, be) (EAppVar n m)) = (n, m) : exprAppliedOrdVars be
  exprAppliedOrdVars (TTupleApply _ (_, be) _) = exprAppliedOrdVars be
  exprAppliedOrdVars (TAliasExpr b _) = exprAppliedOrdVars b
  exprAppliedOrdVars (TCalls _ b _) = exprAppliedOrdVars b
  exprAppliedOrdVars _ = error "Unsupported Expr exprAppliedOrdVars"

  exprVarArgs TCExpr{} = H.empty
  exprVarArgs TValue{} = H.empty
  exprVarArgs THoleExpr{} = H.empty
  exprVarArgs (TAliasExpr base n) = H.insertWith (++) (TVArg $ inExprSingleton n) [(n, getExprMeta base)] (exprVarArgs base)
  exprVarArgs (TWhere _ base _) = exprVarArgs base
  exprVarArgs (TTupleApply _ (_, be) (EAppArg ObjArr{oaObj=Just n, oaArr=Just (Nothing, arrM)})) = H.insertWith (++) (TVArg $ inExprSingleton n) [(n, arrM)] (exprVarArgs be)
  exprVarArgs (TTupleApply _ _ (EAppArg ObjArr{oaObj, oaArr=Just (Nothing, _)})) = error $ printf "Unexpected unhandled obj type in exprVarArgs: %s" (show oaObj)
  exprVarArgs (TTupleApply _ (_, be) (EAppArg ObjArr{oaArr=Just (Just e, _)})) = H.unionWith (++) (exprVarArgs be) (exprVarArgs e)
  exprVarArgs (TTupleApply _ _ (EAppArg ObjArr{oaObj, oaArr=Nothing})) = error $ printf "Not yet implemented: %s" (show oaObj)
  exprVarArgs (TTupleApply _ (_, be) (EAppVar n m)) = H.insertWith (++) (TVVar n) [(TValue (emptyMetaT $ partialToTypeSingleton n) (pkName n), m)] (exprVarArgs be)
  exprVarArgs (TTupleApply _ (_, be) (EAppSpread arg)) = H.unionWith (++) (exprVarArgs arg) (exprVarArgs be)
  exprVarArgs (TCalls _ b _) = exprVarArgs b

  exprVarArgsWithSrc = undefined
  mkValue = undefined

-- | 'EvalMode' contains how to evaluate the function and the 'PTypeName' to eval
data EvalMode
  = EvalRunWithContext String -- ^ Run f{IO io} -> IO
  | EvalRun String -- ^ Run f -> Show
  | EvalBuildWithContext String -- ^ Build f{IO io} -> CatlnResult
  | EvalBuild String -- ^ Build f -> CatlnResult
  | NoEval ReachesTree ReachesTree -- ^ Can't run or build
  deriving (Eq, Show, Generic, ToJSON)


-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

data LLVMState
  = LLVMState {
    -- lsMod           :: AST.Module
  lTaskArrows       :: [TaskArrow]
  , lTaskStructs    :: [TaskStruct]
  , lTasksCompleted :: S.HashSet String
                           }

newtype LLVM a = LLVM (State LLVMState a)
  deriving newtype (Functor, Applicative, Monad, MonadState LLVMState )
data DeclInput
  = TupleInput
  | StructInput
  deriving (Eq, Ord, Show, Generic, Hashable)

type TaskArrow = (PartialType, EObjArr, DeclInput)
type TaskStruct = Type

type Names = Map.Map String Int

data CodegenState
  = CodegenState {
    -- currentBlock :: AST.Name                     -- Name of the active block to append to
  -- , blocks       :: Map.Map AST.Name BlockState  -- Blocks for function
  -- , cgArgs       :: H.HashMap ArgName AST.Operand    -- Function scope symbol table
  blockCount    :: Int                      -- Count of basic blocks
  , count       :: Word                     -- Count of unnamed instructions
  , names       :: Names                    -- Name Supply
  , taskArrows  :: [TaskArrow]
  , taskStructs :: [TaskStruct]
  } deriving Show

newtype BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  -- , stack :: [AST.Named AST.Instruction]            -- Stack of instructions
  -- , term  :: Maybe (AST.Named AST.Terminator)       -- Block terminator
  } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving newtype (Functor, Applicative, Monad, MonadState CodegenState )

instance Show (Codegen a) where
  show Codegen{} = "Codegen"

instance Show m => Show (TExpr m) where
  show (TCExpr _ c) = show c
  show (TValue _ name) = printf "Value %s" (nameStr name)
  show (THoleExpr m hole) = printf "Hole %s %s" (show m) (show hole)
  show (TAliasExpr base alias) = printf "%s@%s" (show base) (show alias)
  show (TWhere _ base cond) = printf "%s | %s" (show base) (show cond)
  show (TTupleApply _ (_, baseExpr) arg) = printf "%s(%s)" baseExpr' (show arg)
    where
      baseExpr' = case baseExpr of
        TValue _ funName -> nameStr funName
        TTupleApply{}    -> show baseExpr
        _                -> printf "(%s)" (show baseExpr)
  show (TCalls m e _) = printf "(%s ↦ %s)" (show e) (show $ getMetaType m)


type ObjSrc = (PartialType, EObjArr)

resArrowDestType :: (TypeGraph tg) => TypeEnv tg -> PartialType -> TCallTree -> [Type]
resArrowDestType typeEnv src (TCObjArr oa) = arrowDestTypeDispatch typeEnv H.empty src oa
resArrowDestType _ _ (TCPrim oa _) = [getMetaType $ getOaArrM oa]
resArrowDestType typeEnv src (TCMacro oa _) = arrowDestTypeDispatch typeEnv H.empty src oa
-- resArrowDestType _ _ (ConstantArrow v) = singletonType $ getValType v
resArrowDestType _ _ (TCArg tp _) = [tp]
resArrowDestType _ _ t = error $ printf "Not yet implemented resArrowDestType for %s" (show t)

-- | Extracts the arguments from a value using a matching expression
-- | As there are no vars stored in the value, it only gets arguments
exprArgsWithVal :: (MetaDat m, Show m) => Expr m -> Val -> Args
exprArgsWithVal CExpr{} _ = H.empty
exprArgsWithVal Value{} _ = H.empty
exprArgsWithVal HoleExpr{} _ = H.empty
exprArgsWithVal (EWhere _ base _) val = exprArgsWithVal base val
exprArgsWithVal (AliasExpr base n) val = H.insert (nameStr (exprPath n)) val (exprArgsWithVal base val)
exprArgsWithVal (TupleApply _ (_, be) arg) val = H.union (exprArgsWithVal be val) (fromArg arg val)
  where
    fromArg :: (MetaDat m, Show m) => EApp Expr m -> Val -> Args
    fromArg (EAppArg ObjArr{oaObj=Just obj, oaArr=Just (Just e, _)}) (TupleVal _ tupleArgs) = case H.lookup (nameStr (exprPath obj)) tupleArgs of
      Just val' -> exprArgsWithVal e val'
      Nothing -> error $ printf "Failed to find expected arg %s in tuple %s" (show $ exprPath obj) (show val)
    fromArg (EAppArg ObjArr{oaObj=Nothing, oaArr=Just (Just e, _)}) _ = exprArgsWithVal e val
    fromArg (EAppArg ObjArr{oaObj=Just obj, oaArr=Just (Nothing, _)}) (TupleVal _ tupleArgs) = case H.lookup (nameStr (exprPath obj)) tupleArgs of
      -- An alias destructure (@field@name@) is matched by the field (@exprPath obj@ follows the
      -- base) and bound ONLY under the alias name (what the body references) — renaming to the
      -- alias is precisely how a nested destructure avoids colliding with a same-named parameter
      -- of the enclosing arm (e.g. @insert(m= Node(key@nk, ..), key, ..)@ where the node's own
      -- @key@ must not shadow the arm's @key@ argument; the 'H.union' above is left-biased toward
      -- the deeper destructure). The non-linear equality guard (@f(a@x, b@x)@) does not rely on a
      -- bare-field binding here — 'evalNonLinearEq' reads the original positions straight from the
      -- value's own tuple args.
      Just val' -> case obj of
        AliasExpr _ aliasE -> H.singleton (nameStr (exprPath aliasE)) val'
        _                  -> H.singleton (nameStr (exprPath obj)) val'
      Nothing -> error $ printf "Failed to find expected arg %s in tuple %s" (show $ exprPath obj) (show val)
    fromArg EAppVar{} _ = H.empty
    -- A spread in a pattern (@Box(value, .._)@) stands for the rest of the matched value, so it is
    -- matched against the value itself: the hole @.._@ binds nothing, and the fields it stands for
    -- are left to whatever else the value is matched by.
    fromArg (EAppSpread a) _ = exprArgsWithVal a val
    fromArg oa _ = error $ printf "Invalid exprArgsWithVal fromArg of %s and %s" (show oa) (show val)

-- | Extracts the arguments from a value using a matching expression
-- | As there are no vars stored in the value, it only gets arguments
texprArgsWithVal :: (MetaDat m, Show m) => TExpr m -> Val -> Args
texprArgsWithVal TCExpr{} _ = H.empty
texprArgsWithVal TValue{} _ = H.empty
texprArgsWithVal THoleExpr{} _ = H.empty
texprArgsWithVal (TWhere _ base _) val = texprArgsWithVal base val
texprArgsWithVal (TAliasExpr base n) val = H.insert (nameStr (exprPath n)) val (texprArgsWithVal base val)
texprArgsWithVal (TTupleApply _ (_, be) arg) val = H.union (texprArgsWithVal be val) (fromArg arg val)
  where
    fromArg :: (MetaDat m, Show m) => EApp TExpr m -> Val -> Args
    fromArg (EAppArg ObjArr{oaObj=Just obj, oaArr=Just (Just e, _)}) (TupleVal _ tupleArgs) = case H.lookup (nameStr (exprPath obj)) tupleArgs of
      Just val' -> texprArgsWithVal e val'
      Nothing -> error $ printf "Failed to find expected arg %s in tuple %s" (show $ exprPath obj) (show val)
    fromArg (EAppArg ObjArr{oaObj=Nothing, oaArr=Just (Just e, _)}) _ = texprArgsWithVal e val
    fromArg (EAppArg ObjArr{oaObj=Just obj, oaArr=Just (Nothing, _)}) (TupleVal _ tupleArgs) = case H.lookup (nameStr (exprPath obj)) tupleArgs of
      -- Bound ONLY under the alias name, mirroring 'exprArgsWithVal' above (which carries the
      -- full rationale) — the two paths are selected by 'buildArrArgs' on the same patterns, so
      -- they must agree.
      Just val' -> case obj of
        TAliasExpr _ aliasE -> H.singleton (nameStr (exprPath aliasE)) val'
        _                   -> H.singleton (nameStr (exprPath obj)) val'
      Nothing -> error $ printf "Failed to find expected arg %s in tuple %s" (show $ exprPath obj) (show val)
    fromArg EAppVar{} _ = H.empty
    -- A pattern spread, as in 'exprArgsWithVal' above.
    fromArg (EAppSpread a) _ = texprArgsWithVal a val
    fromArg oa _ = error $ printf "Invalid exprArgsWithVal fromArg of %s and %s" (show oa) (show val)
texprArgsWithVal (TCalls _ e _) val = texprArgsWithVal e val

buildArrArgs :: AnyObjArr -> Val -> Args
buildArrArgs (Left oa)  = exprArgsWithVal (oaObjExpr oa)
buildArrArgs (Right oa) = texprArgsWithVal (oaObjExpr oa)
