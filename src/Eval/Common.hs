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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module Eval.Common where

import           Data.Hashable
import qualified Data.HashMap.Strict     as H
import qualified Data.HashSet            as S
import           Data.List               (intercalate)
import qualified Data.Map                as Map
import           GHC.Generics            (Generic)

import           Control.Monad.State
import           CRes
import           Data.Aeson              hiding (Object)
import           Data.IORef
import           Data.UUID               (UUID)
-- import qualified LLVM.AST            as AST
import           Semantics
import           Semantics.Prgm
import           Semantics.TypeGraph
import           Semantics.Types
import           Syntax.Ct.Parser.Syntax (DesPrgm)
import           Text.Printf
import           TypeCheck.Common        (TraceConstrain, VPrgm)
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

-- | The program being evaluated, at the pipeline stages the evaluation cannot reach on its own.
-- Desugar is strictly upstream of eval and tree build is a separate pass, so an 'Env' holds neither;
-- a caller that has run those stages passes them here. This is what a primitive reporting on the
-- program itself reads, in the same way 'evTbEnv' is read for the stage the evaluation runs on.
--
-- All are unforced: constructing an 'Env' must not compute a stage, since a caller pays only for
-- the ones something actually asks for. Forcing the tree build runs a whole 'Eval.evalBuildAll'.
data StagePrgms = StagePrgms {
    spDesPrgm    :: CRes DesPrgm
  , spTBPrgm     :: CRes (Prgm TExpr EvalMetaDat)
  -- | How the program's types were inferred: a type variable on every meta, and the trace of what
  -- each epoch updated it to. Unlike the two stages above this is never joined across pages — a
  -- typecheck numbers its points within the run that assigned them, so two pages' traces have
  -- colliding point numbers and mean nothing merged together. An evaluation that is not scoped to
  -- one page therefore has no type inference to report, rather than an amalgamated one.
  , spTypeInfer  :: CRes (VPrgm, TraceConstrain)
  -- | Runs one of the program's functions, for a debugger reporting on what it evaluated. It is a
  -- function rather than a value because which function to run is the annotation's argument, and it
  -- is supplied from outside for the same reason the stages above are: an evaluation cannot run the
  -- program it is itself part of, since the evaluator is downstream of the primitives that would
  -- have to call it. Like 'spTypeInfer' it is never joined across pages — a run needs one program
  -- with one entry point, and the union of every loaded page has as many @main@s as it has pages.
  , spDebugRun   :: String -> CRes DebugRun
  -- | Compiles one expression, written as Catln source, as though it were a statement of the page
  -- being evaluated, and runs it for a debugger reporting on what it evaluated.
  --
  -- A source string rather than a name because what is worth debugging is often something no
  -- definition names: the computation a @#print@ shows the result of, or the one generated input a
  -- test case ran on. A generated input in particular exists nowhere in the program — the test
  -- engine made it up, under a seed that is not kept — so the text of the call it ran is the only
  -- handle on that case there is.
  --
  -- Supplied from outside for the reasons 'spDebugRun' is, and one more: compiling an expression
  -- against the page is the whole of the work here, and the stages that would be compiled against
  -- are the ones above.
  , spDebugExpr  :: String -> CRes DebugRun
  -- | The pages the evaluation's program was assembled from. An 'Env' holds one flattened 'Prgm'
  -- with no file identity left in it, so which files it came from is reachable no more than the
  -- stages above are. It is the pages of whatever the evaluation was scoped to, so it goes with the
  -- stages above: these are the files those programs were joined out of.
  , spPages      :: CRes [FileImport]
  -- | Every page there is, whatever the evaluation was scoped to.
  --
  -- For an annotation which reports on what there is rather than on the program it is written in. A
  -- listing of the pages, or a search over them, is about the ones the reader has NOT reached as
  -- much as the ones they have: read as a record of an exploration, what a page is worth offering
  -- next to is exactly what that page does not import. Such an annotation is answered the same way
  -- wherever it is written, and so can be written in a small program while still answering about
  -- every page — which is what keeps a navigation from costing a compilation of everything it
  -- names.
  --
  -- Unlike 'spPages' this is not a stage of a program at all, and nothing need be compiled to
  -- answer it. It is which pages were read, which is known as soon as they were.
  , spAllPages   :: CRes [FileImport]
  -- | The tests of the page the evaluation is scoped to, already run. Unlike every stage above,
  -- these are not an artifact of compiling a program but of running part of it, which needs IO — so
  -- they cannot be produced from within an evaluation at all and arrive already run. They are
  -- supplied only by an evaluation that has a test to report on, since running a page's tests is
  -- work rather than a lookup, and are never joined across pages: a test belongs to the page that
  -- wrote the definition it checks.
  , spTests      :: CRes [TestResult]
  -- | The values one of the evaluation's pages declares of one type, evaluated to the values they
  -- display as, in the order that page writes them. Like 'spPages' this is about the pages there are
  -- rather than about the one program the evaluation runs on, and it takes the page as an argument
  -- for the same reason 'spDebugRun' takes a function name: which page is being asked about is the
  -- caller's question, not a property of the evaluation. An 'Env' holds one flattened 'Prgm' with no
  -- file identity left in it, so a page other than the one being evaluated is reachable no more than
  -- the stages above are.
  --
  -- Supplied from outside for the same reason 'spDebugRun' is: evaluating a page's declarations is
  -- work for the evaluator, which is downstream of the primitives that would have to ask for it.
  , spPageValues :: Name -> FileImport -> CRes [(EExpr, Val)]
  }

-- | The stage programs for an evaluation whose caller did not supply any. Asking for one is then an
-- error rather than a silent substitution of the stage the evaluation does hold: an evaluation with
-- no upstream artifacts to report on cannot answer, and reporting the wrong stage would be worse
-- than failing.
stagePrgmsUnavailable :: StagePrgms
stagePrgmsUnavailable = StagePrgms unavailable unavailable unavailable (const unavailable) (const unavailable) unavailable unavailable unavailable (\_ _ -> unavailable)
  where
    unavailable :: CRes a
    unavailable = CErr [MkCNote $ GenCErr Nothing "This evaluation was not given the other pipeline stages of its program"]

data EPrim
  = EPrim String (H.HashMap String Val -> Either String Val)
  -- | A primitive that also reads the evaluation 'Env'. This is what a primitive reporting on the
  -- program itself needs (the webdocs pages), since the program is only reachable through
  -- @evTbEnv@. It must be a primitive rather than a 'MacroFunction' because a macro runs during
  -- TreeBuild, which would bake the program into every built arm ('evalBuildAll') instead of
  -- computing it per evaluation.
  | EEnvPrim String (Env -> H.HashMap String Val -> Either String Val)
  deriving (Generic)

ePrimName :: EPrim -> String
ePrimName (EPrim n _)    = n
ePrimName (EEnvPrim n _) = n

instance Eq EPrim where
  a == b = ePrimName a == ePrimName b

instance Hashable EPrim where
  hashWithSalt s p = s `hashWithSalt` ePrimName p

instance ToJSON EPrim where
  toJSON EPrim{}    = object ["type".=("EPrim" :: String)]
  toJSON EEnvPrim{} = object ["type".=("EEnvPrim" :: String)]


type EvalTreebugOpen = AnyObjArr
data EvalTreebugClosed = EvalTreebugClosed AnyObjArr Val Val [EvalTreebugClosed] String
  deriving (Eq, Generic, Hashable, ToJSON)

type Args = H.HashMap String Val
data Env = Env { evObjMap        :: EObjectMap
               , evTypeEnv       :: TypeEnv (ObjArrTypeGraph Expr EvalMetaDat)
               , evArgs          :: Args
               , evExEnv         :: ResExEnv
               , evTbEnv         :: TBEnv
               , evStagePrgms    :: StagePrgms
               -- | The definition an annotation being evaluated is written on, for a primitive
               -- reporting on that definition. An annotation is local to the program it is written
               -- in and to whatever it is written on, and only the second is otherwise lost: the
               -- annotation is built as an expression of its own, with nothing left of what it
               -- annotates. Nothing for a program's own annotation, which is written on the program
               -- rather than on any definition of it.
               , evAnnotObj      :: Maybe EObjArr
               , evCallStack     :: [String]
               , evCoverage      :: H.HashMap EObjArr Int
               , evTreebugOpen   :: [EvalTreebugOpen]
               , evTreebugClosed :: [EvalTreebugClosed]
               }

data EvalResult = EvalResult { erCoverage :: H.HashMap EObjArr Int
                             , erTreebug  :: [EvalTreebugClosed]
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
data DebugRun = DebugRun { drExit   :: Maybe Integer
                         , drOutput :: String
                         , drValue  :: Maybe Val
                         , drResult :: EvalResult
                         } deriving (Eq, Generic, ToJSON)

-- | Which family of test a 'TestResult' came from. A @#test@ or @#example@ definition produces a
-- 'TestSimple' when it takes no arguments and a 'TestProperty' when the test engine has to generate
-- them; the other three are derived from the program rather than written in it.
data TestKind
  = TestSimple
  | TestProperty
  | TestOverlapAgreement
  | TestRefinementAgreement
  | TestPrintGolden
  deriving (Eq, Show, Generic, ToJSON)

-- | One test of a program, after it has been run: what it was derived from, whether it passed, and
-- what it reported. This is the result rather than the check, so it can be cached, served, and
-- displayed; the unrun form a test runner wants is @Eval.Testing.TestDesc@.
--
-- It lives here beside 'DebugRun' for the same reason that does: 'StagePrgms' carries it, and
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
oaTestId = getMetaID . getExprMeta . oaObjExpr

-- | A mock IO handle that captures printed lines in an internal buffer instead of writing to real
-- stdout. It lives beside 'MockIOVal', the value that carries the same buffer through an evaluation.
-- Create with 'newMockIO', pass to 'Eval.evalRunMock', then read captured output with
-- 'getMockIOOutput'.
newtype MockIO = MockIO (IORef [String])

newMockIO :: IO MockIO
newMockIO = MockIO <$> newIORef []

-- | Returns all lines printed to the MockIO, joined with newlines
-- (equivalent to what would have appeared on stdout).
getMockIOOutput :: MockIO -> IO String
getMockIOOutput (MockIO ref) = unlines . reverse <$> readIORef ref

-- | The buffer a 'MockIO' captures into, for seeding an evaluation's 'MockIOVal'.
mockIORef :: MockIO -> IORef [String]
mockIORef (MockIO ref) = ref

data Val
  = IntVal Integer
  | FloatVal Double
  | StrVal String
  | CharVal Char
  | TupleVal String (H.HashMap String Val)
  -- | A function-typed argument's value: the lambda plus the arg env captured at its
  -- creation site, so body references to enclosing args (@fn(v)= fn(v= v)@'s inner @fn@)
  -- resolve lexically at application.
  | ObjArrVal (ObjArr TExpr EvalMetaDat) Args
  | IOVal Integer (IO ())
  | MockIOVal Integer (IO ()) (IORef [String])
  | LLVMVal (LLVM ())
  | LLVMQueue [(TExpr EvalMetaDat, EObjArr)]
  -- | LLVMOperand Type (Codegen AST.Operand)
  | LLVMIO (Codegen ())
  | NoVal

instance Eq Val where
  (IntVal a) == (IntVal b) = a == b
  (FloatVal a) == (FloatVal b) = a == b
  (StrVal a) == (StrVal b) = a == b
  (TupleVal an aa) == (TupleVal bn ba) = an == bn && aa == ba
  IOVal{} == _ = error "Can't determine equality with IOVal"
  _ == IOVal{} = error "Can't determine equality with IOVal"
  MockIOVal{} == _ = error "Can't determine equality with MockIOVal"
  _ == MockIOVal{} = error "Can't determine equality with MockIOVal"
  NoVal == NoVal = True
  _ == _ = False

instance Show Val where
  show (IntVal i)   = show i
  show (FloatVal d) = show d
  show (StrVal s)   = show s
  show (CharVal c)   = show c
  show (TupleVal name args) = "TupleVal " ++ name ++ showArgs args
    where
      showArgs as | H.null as = ""
      showArgs as = printf "(%s)" (intercalate ", " $ map showArg $ H.toList as)
      showArg (argName, val) = argName ++ " = " ++ show val
  show (ObjArrVal oa _) = printf "(ObjArrVal %s)" (show oa)
  show IOVal{}      = "IOVal"
  show MockIOVal{}  = "MockIOVal"
  show LLVMVal{}   = "LLVMVal"
  show LLVMQueue{}   = "LLVMQueue"
  -- show (LLVMOperand tp _)   = printf "LLVMOperand<$T=%s>" (show tp)
  show LLVMIO{}   = "LLVMIO"
  show NoVal   = "NoVal"

instance Hashable Val where
  hashWithSalt s (IntVal i)        = s `hashWithSalt` i
  hashWithSalt s (FloatVal i)      = s `hashWithSalt` i
  hashWithSalt s (StrVal i)        = s `hashWithSalt` i
  hashWithSalt s (CharVal i)       = s `hashWithSalt` i
  hashWithSalt s (TupleVal n as)   = s `hashWithSalt` n `hashWithSalt` as
  hashWithSalt s (ObjArrVal oa _)  = s `hashWithSalt` oa
  hashWithSalt s (IOVal i _)       = s `hashWithSalt` i
  hashWithSalt s (MockIOVal i _ _) = s `hashWithSalt` i
  hashWithSalt s (LLVMVal _)       = s
  hashWithSalt s (LLVMQueue _)     = s
  -- hashWithSalt s (LLVMOperand tp _) = s `hashWithSalt` tp
  hashWithSalt s (LLVMIO _)        = s
  hashWithSalt s NoVal             = s

instance ToJSON Val where
  toJSON (IntVal v) = object ["tag".=("IntVal" :: String), "contents".=toJSON v]
  toJSON (FloatVal v) = object ["tag".=("FloatVal" :: String), "contents".=toJSON v]
  toJSON (StrVal v) = object ["tag".=("StrVal" :: String), "contents".=toJSON v]
  toJSON (CharVal v) = object ["tag".=("CharVal" :: String), "contents".=toJSON v]
  toJSON (TupleVal name args) = object ["tag".=("TupleVal" :: String), "name".=name, "args".=toJSON args]
  toJSON (ObjArrVal _ _) = object ["tag".=("ObjArrVal" :: String)]
  toJSON IOVal{}     = object ["tag".=("IOVal" :: String)]
  toJSON MockIOVal{} = object ["tag".=("MockIOVal" :: String)]
  toJSON LLVMVal{} = object ["tag".=("LLVMVal" :: String)]
  toJSON LLVMQueue{} = object ["tag".=("LLVMQueue" :: String)]
  -- toJSON LLVMOperand{} = object ["tag".=("LLVMOperand" :: String)]
  toJSON LLVMIO{} = object ["tag".=("LLVMIO" :: String)]
  toJSON NoVal = object ["tag".=("NoVal" :: String)]

resultLeaf, queueLeaf :: PartialType
resultLeaf = (partialVal "/Catln/CatlnResult"){ptArgs=H.fromList [(partialKey "name", strType), (partialKey "contents", strType)]}
queueLeaf = partialVal "llvmQueue"

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
getValType (StrVal s)   = constantType (CStr s)
getValType (CharVal c)  = constantType (CChar c)
getValType (TupleVal name args) = singletonType (partialVal (mkName name)){ptArgs=H.fromList $ map fromArg $ H.toList args}
  where fromArg (argName, argVal) = (partialKey (mkName argName), getValType argVal)
getValType (ObjArrVal oa _) = getExprType $ oaObjExpr oa
getValType IOVal{}     = singletonType ioLeaf
getValType MockIOVal{} = singletonType ioLeaf
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
getValPartialType StrVal{} = strLeaf
getValPartialType CharVal{} = charLeaf
getValPartialType (TupleVal name args) = (partialVal (mkName name)){ptArgs=H.fromList $ map fromArg $ H.toList args}
  where fromArg (argName, argVal) = (partialKey (mkName argName), singletonType $ getValPartialType argVal)
getValPartialType (ObjArrVal oa _) = getSingleton $ getExprType $ oaObjExpr oa
getValPartialType IOVal{}     = ioLeaf
getValPartialType MockIOVal{} = ioLeaf
getValPartialType LLVMVal{} = resultLeaf
getValPartialType LLVMQueue{} = queueLeaf
-- getValPartialType (LLVMOperand t _) = case t of
  -- UnionType leafs -> case splitUnionType leafs of
    -- [partial] -> partial
    -- _         -> error "could not getValPartialType without a single partial"
  -- _ -> error $ printf "could not get non sum getValPartialType %s" (show t)
getValPartialType LLVMIO{} = ioLeaf
getValPartialType NoVal = error "getValPartialType of NoVal"

-- | Like 'getValPartialType', but a closure argument ('ObjArrVal') contributes its declared
-- OUTPUT type as the argument slot instead of its input signature. A typed arm's higher-order
-- slot holds whichever form typechecking delivered: the input signature (@/fn(/v=..)@) or the
-- output contract propagated from the declaration's pointer slot (@/fn=∀Monad[..]@). Dispatch
-- matching tries 'getValPartialType' first and falls back to this form.
getValPartialTypeOut :: Val -> PartialType
getValPartialTypeOut (TupleVal name args) = (partialVal (mkName name)){ptArgs=H.fromList $ map fromArg $ H.toList args}
  where
    -- The closure slot is keyed with the closure's parameter names ('pkArgs'), matching the
    -- typed structural key (@/fn(/v)@) so 'typeGetArgLoose' finds it; a bare key would miss
    -- and fall back to the arm's unresolved contract meta.
    fromArg (argName, argVal@(ObjArrVal oa _)) | Just (_, arrM) <- oaArr oa =
      ((partialToKey (getValPartialType argVal)){pkName=mkName argName}, getMetaType arrM)
    fromArg (argName, argVal) = (partialKey (mkName argName), singletonType $ getValPartialTypeOut argVal)
getValPartialTypeOut v = getValPartialType v


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
type ResExEnv = H.HashMap (PartialType, ObjArr Expr EvalMetaDat) (TExpr EvalMetaDat, [TExpr EvalMetaDat]) -- (result, [compAnnot trees])

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
  show (TCArg t _)      = printf "TCArg %s" (show t)
  show (TCObjArr oa)    = printf "TCObjArr (%s)" (show oa)
  show (TCPrim oa _)    = printf "TCPrim (%s)" (show oa)
  show (TCMacro oa _)   = printf "TCMacro (%s)" (show oa)

data TCallTree
  = TCTId
  | TCMatch Type (H.HashMap PartialType TCallTree)
  | TCSeq TCallTree TCallTree
  | TCCond Type [(Maybe ([TExpr EvalMetaDat], EObjArr), TCallTree)] -- [((if, ifObj), then)]
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

instance ExprClass TExpr where
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
resArrowDestType typeEnv src (TCObjArr oa) = arrowDestTypeDispatch typeEnv src oa
resArrowDestType _ _ (TCPrim oa _) = [getMetaType $ getOaArrM oa]
resArrowDestType typeEnv src (TCMacro oa _) = arrowDestTypeDispatch typeEnv src oa
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
    fromArg (EAppSpread a) _ = error $ printf "Not yet implemented exprArgsWithVal fromArg of %s and %s" (show a) (show val)
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
    fromArg (EAppSpread a) _ = error $ printf "Not yet defined spread exprArgsWithVal fromArg of %s and %s" (show a) (show val)
    fromArg oa _ = error $ printf "Invalid exprArgsWithVal fromArg of %s and %s" (show oa) (show val)
texprArgsWithVal (TCalls _ e _) val = texprArgsWithVal e val

buildArrArgs :: AnyObjArr -> Val -> Args
buildArrArgs (Left oa)  = exprArgsWithVal (oaObjExpr oa)
buildArrArgs (Right oa) = texprArgsWithVal (oaObjExpr oa)
