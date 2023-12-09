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

module Eval.Common where

import           Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.List           (intercalate)
import qualified Data.Map            as Map
import           GHC.Generics        (Generic)

import           Control.Monad.State
import           CRes
import           Data.Aeson          hiding (Object)
-- import qualified LLVM.AST            as AST
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           Utils

type EvalMetaDat = ()
type EvalMeta = Meta EvalMetaDat
type ECompAnnot = CompAnnot (Expr EvalMetaDat)
type EExpr = Expr EvalMetaDat
type EGuard = Maybe EExpr
type EObjArr = ObjArr Expr EvalMetaDat
type EObjectMap = ObjectMap Expr EvalMetaDat
type EPrgm = Prgm Expr EvalMetaDat
type EPrgmGraphData = GraphData EPrgm String

data EPrim = EPrim PartialType EGuard (H.HashMap String Val -> Val)
  deriving (Generic)

-- TODO: Maybe should include result type?
instance Eq EPrim where
  (EPrim at ag _) == (EPrim bt bg _) = at == bt && ag == bg

instance Hashable EPrim where
  hashWithSalt s (EPrim at ag _) = s `hashWithSalt` at `hashWithSalt` ag

type EvalTreebugOpen = EObjArr
data EvalTreebugClosed = EvalTreebugClosed EObjArr Val [EvalTreebugClosed] String
  deriving (Eq, Generic, Hashable, ToJSON)

type Args = H.HashMap String Val
data Env = Env { evObjMap        :: EObjectMap
               , evTypeEnv       :: TypeEnv
               , evArgs          :: Args
               , evExEnv         :: ResExEnv
               , evTbEnv         :: TBEnv
               , evCallStack     :: [String]
               , evCoverage      :: H.HashMap EObjArr Int
               , evTreebugOpen   :: [EvalTreebugOpen]
               , evTreebugClosed :: [EvalTreebugClosed]
               }

data EvalResult = EvalResult { erCoverage :: H.HashMap EObjArr Int
                             , erTreebug  :: [EvalTreebugClosed]
                             } deriving (Eq, Generic, ToJSON)

data Val
  = IntVal Integer
  | FloatVal Double
  | StrVal String
  | TupleVal String (H.HashMap String Val)
  | IOVal Integer (IO ())
  | LLVMVal (LLVM ())
  | LLVMQueue [(TExpr (), EObjArr)]
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
  NoVal == NoVal = True
  _ == _ = False

instance Show Val where
  show (IntVal i)   = show i
  show (FloatVal d) = show d
  show (StrVal s)   = show s
  show (TupleVal name args) = "TupleVal " ++ name ++ showArgs args
    where
      showArgs as | H.null as = ""
      showArgs as = printf "(%s)" (intercalate ", " $ map showArg $ H.toList as)
      showArg (argName, val) = argName ++ " = " ++ show val
  show IOVal{}   = "IOVal"
  show LLVMVal{}   = "LLVMVal"
  show LLVMQueue{}   = "LLVMQueue"
  -- show (LLVMOperand tp _)   = printf "LLVMOperand<$T=%s>" (show tp)
  show LLVMIO{}   = "LLVMIO"
  show NoVal   = "NoVal"

instance Hashable Val where
  hashWithSalt s (IntVal i)      = s `hashWithSalt` i
  hashWithSalt s (FloatVal i)    = s `hashWithSalt` i
  hashWithSalt s (StrVal i)      = s `hashWithSalt` i
  hashWithSalt s (TupleVal n as) = s `hashWithSalt` n `hashWithSalt` as
  hashWithSalt s (IOVal i _)     = s `hashWithSalt` i
  hashWithSalt s (LLVMVal _)     = s
  hashWithSalt s (LLVMQueue _)   = s
  -- hashWithSalt s (LLVMOperand tp _) = s `hashWithSalt` tp
  hashWithSalt s (LLVMIO _)      = s
  hashWithSalt s NoVal           = s

instance ToJSON Val where
  toJSON (IntVal v) = object ["tag".=("IntVal" :: String), "contents".=toJSON v]
  toJSON (FloatVal v) = object ["tag".=("FloatVal" :: String), "contents".=toJSON v]
  toJSON (StrVal v) = object ["tag".=("StrVal" :: String), "contents".=toJSON v]
  toJSON (TupleVal name args) = object ["tag".=("TupleVal" :: String), "name".=name, "args".=toJSON args]
  toJSON IOVal{} = object ["tag".=("IOVal" :: String)]
  toJSON LLVMVal{} = object ["tag".=("LLVMVal" :: String)]
  toJSON LLVMQueue{} = object ["tag".=("LLVMQueue" :: String)]
  -- toJSON LLVMOperand{} = object ["tag".=("LLVMOperand" :: String)]
  toJSON LLVMIO{} = object ["tag".=("LLVMIO" :: String)]
  toJSON NoVal = object ["tag".=("NoVal" :: String)]

resultLeaf, queueLeaf :: PartialType
resultLeaf = (partialVal (PTypeName "/Catln/CatlnResult")){ptArgs=H.fromList [(partialKey "name", strType), (partialKey "contents", strType)]}
queueLeaf = partialVal (PTypeName "llvmQueue")

resultType :: Type
resultType = singletonType resultLeaf

getValType :: Val -> PartialType
getValType IntVal{} = intLeaf
getValType FloatVal{} = floatLeaf
getValType StrVal{} = strLeaf
getValType (TupleVal name args) = (partialVal (PTypeName name)){ptArgs=H.fromList $ map fromArg $ H.toList args}
  where fromArg (argName, argVal) = (partialKey argName, singletonType $ getValType argVal)
getValType IOVal{} = ioLeaf
getValType LLVMVal{} = resultLeaf
getValType LLVMQueue{} = queueLeaf
-- getValType (LLVMOperand t _) = case t of
  -- UnionType leafs -> case splitUnionType leafs of
    -- [partial] -> partial
    -- _         -> error "could not getValType without a single partial"
  -- _ -> error $ printf "could not get non sum getValType %s" (show t)
getValType LLVMIO{} = ioLeaf
getValType NoVal = error "getValType of NoVal"


--- ResArrowTree
data MacroData = MacroData {
                               mdTbEnv  :: TBEnv
                             , mdObjSrc :: [ObjSrc]
                             }
newtype MacroFunction = MacroFunction (TExpr () -> MacroData -> CRes (TExpr ()))
type ResBuildEnvFunction = TCallTree
type ResBuildEnvItem = (PartialType, Maybe (Expr EvalMetaDat), Bool, ResBuildEnvFunction)
type ResBuildEnv = H.HashMap TypeName [ResBuildEnvItem]
type ResExEnv = H.HashMap (PartialType, ObjArr Expr EvalMetaDat) (TExpr (), [TExpr ()]) -- (result, [compAnnot trees])

data TBEnv = TBEnv {
    tbName    :: String
  , tbResEnv  :: ResBuildEnv
  , tbPrgm    :: Prgm Expr EvalMetaDat
  , tbTypeEnv :: TypeEnv
  }

instance Eq MacroFunction where
  _ == _ = False

instance Hashable MacroFunction where
  s `hashWithSalt` _ = s

instance Show TCallTree where
  show TCTId = "TCTId"
  show (TCMatch opts) = printf "TCMatch (%s)" (show opts)
  show (TCSeq a b) = printf "TCSeq (%s) (%s)" (show a) (show b)
  show (TCCond t ifs els) = printf "TCCond %s (%s) (%s)" (show t) (show ifs) (show els)
  show (TCArg t _) = printf "TCArg %s" (show t)
  show (TCObjArr oa) = printf "TCObjArr %s" (show oa)
  show (TCPrim t _) = printf "TCPrim %s" (show t)
  show (TCMacro t _) = printf "TCMacro %s" (show t)

data TCallTree
  = TCTId
  | TCMatch (H.HashMap PartialType TCallTree)
  | TCSeq TCallTree TCallTree
  | TCCond Type [((TExpr (), EObjArr), TCallTree)] TCallTree -- [((if, ifObj), then)] else
  | TCArg Type String
  | TCObjArr EObjArr
  | TCPrim Type EPrim
  | TCMacro Type MacroFunction
  deriving (Eq, Generic, Hashable)

data TExpr m
  = TCExpr (Meta m) Val
  | TValue (Meta m) TypeName
  | THoleExpr (Meta m) Hole
  | TAliasExpr (TExpr m) (TExpr m) -- ^ AliasTExpr baseExpr aliasExpr
  | TTupleApply (Meta m) (TExpr m) (ObjArr TExpr m)
  | TVarApply (Meta m) (TExpr m) TypeVarName (Meta m)
  | TCalls (Meta m) (TExpr m) TCallTree
  deriving (Eq, Show, Generic, Hashable)

instance ExprClass TExpr where
  getExprMeta expr = case expr of
    TCExpr m _        -> m
    TValue m _        -> m
    THoleExpr m _     -> m
    TAliasExpr b _    -> getExprMeta b
    TTupleApply m _ _ -> m
    TVarApply m _ _ _ -> m
    TCalls m _ _      -> m

  maybeExprPathM (TValue m n)        = Just (n, m)
  maybeExprPathM (TTupleApply _ e _) = maybeExprPathM e
  maybeExprPathM (TVarApply _ e _ _) = maybeExprPathM e
  maybeExprPathM (TAliasExpr b _)    = maybeExprPathM b
  maybeExprPathM (TCalls _ b _)      = maybeExprPathM b
  maybeExprPathM _                   = Nothing

  exprAppliedArgs (TValue _ _) = []
  exprAppliedArgs (TTupleApply _ be ae) = ae : exprAppliedArgs be
  exprAppliedArgs (TVarApply _ e _ _) = exprAppliedArgs e
  exprAppliedArgs (TAliasExpr b _) = exprAppliedArgs b
  exprAppliedArgs (TCalls _ b _) = exprAppliedArgs b
  exprAppliedArgs e = error $ printf "Unsupported Expr exprAppliedArgs for %s" (show e)

  exprAppliedOrdVars (TValue _ _) = []
  exprAppliedOrdVars (TTupleApply _ be _) = exprAppliedOrdVars be
  exprAppliedOrdVars (TVarApply _ e n m) = (n, m) : exprAppliedOrdVars e
  exprAppliedOrdVars (TAliasExpr b _) = exprAppliedOrdVars b
  exprAppliedOrdVars (TCalls _ b _) = exprAppliedOrdVars b
  exprAppliedOrdVars _ = error "Unsupported Expr exprAppliedOrdVars"

  exprVarArgs TCExpr{} = H.empty
  exprVarArgs TValue{} = H.empty
  exprVarArgs THoleExpr{} = H.empty
  exprVarArgs (TAliasExpr base n) = H.insertWith (++) (TVArg $ inExprSingleton n) [(n, getExprMeta base)] (exprVarArgs base)
  exprVarArgs (TTupleApply _ be ObjArr{oaObj=Just (GuardExpr n _), oaArr=(Nothing, arrM)}) = H.insertWith (++) (TVArg $ inExprSingleton n) [(n, arrM)] (exprVarArgs be)
  exprVarArgs (TTupleApply _ _ ObjArr{oaObj, oaArr=(Nothing, _)}) = error $ printf "Unexpected unhandled obj type in exprVarArgs: %s" (show oaObj)
  exprVarArgs (TTupleApply _ be ObjArr{oaArr=(Just (GuardExpr e _), _)}) = H.unionWith (++) (exprVarArgs be) (exprVarArgs e)
  exprVarArgs (TVarApply _ e n m) = H.unionWith (++) (exprVarArgs e) (H.singleton (TVVar n) [(TValue (emptyMetaT $ partialToTypeSingleton n) (pkName n), m)])
  exprVarArgs (TCalls _ b _) = exprVarArgs b


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


type ObjSrc = (PartialType, EObjArr)

resArrowDestType :: TypeEnv -> PartialType -> TCallTree -> Type
resArrowDestType typeEnv src (TCObjArr oa) = arrowDestType False typeEnv src oa
resArrowDestType _ _ (TCPrim tp _) = tp
resArrowDestType _ _ (TCMacro tp _) = tp
-- resArrowDestType _ _ (ConstantArrow v) = singletonType $ getValType v
resArrowDestType _ _ (TCArg tp _) = tp
resArrowDestType _ _ t = error $ printf "Not yet implemented resArrowDestType for %s" (show t)


buildArrArgs :: EObjArr -> Val -> Args
buildArrArgs obj = aux H.empty (oaObjExpr obj)
  where
    aux acc oExpr val | null (exprAppliedArgs oExpr) = H.insert (exprPath oExpr) val acc
    aux _ oExpr (TupleVal tupleName _) | exprPath oExpr /= tupleName = error $ printf "Found name mismatch in buildArrArgs: object %s and tuple %s" (exprPath oExpr) tupleName
    aux acc oExpr (TupleVal _ tupleArgs) = H.foldrWithKey addArgs acc $ H.intersectionWith (,) (H.mapKeys pkName $ exprAppliedArgsMap oExpr) tupleArgs
    aux _ oExpr val = error $ printf "Invalid buildArrArgs with oExpr %s and value %s" (show oExpr) (show val)

    addArgs argName ((_, Nothing), argVal) acc   = H.insert argName argVal acc
    addArgs _ ((_, Just subObjExpr), argVal) acc = aux acc subObjExpr argVal
