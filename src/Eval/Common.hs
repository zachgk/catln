--------------------------------------------------------------------
-- |
-- Module    :  Eval.Common
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Eval.Common where

import           GHC.Generics          (Generic)
import           Data.Hashable
import qualified Data.HashMap.Strict as H
import           Data.List                      ( intercalate )
import qualified Data.Map                   as Map
import qualified Data.HashSet as S

import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           Text.Printf
import Data.Aeson hiding (Object)
import qualified LLVM.AST as AST
import CRes
import Control.Monad.State
import Utils

type EvalMeta = Typed
type ECompAnnot = CompAnnot EvalMeta
type EExpr = Expr EvalMeta
type EGuard = Guard EExpr
type EObject = Object EvalMeta
type EArrow = Arrow EExpr EvalMeta
type EObjectMap = ObjectMap EExpr EvalMeta
type EPrgm = Prgm EExpr EvalMeta
type EPrgmGraphData = GraphData EPrgm String
type EReplRes = ReplRes EvalMeta

data EPrim = EPrim PartialType EGuard (H.HashMap String Val -> Val)
  deriving (Generic)

-- TODO: Maybe should include result type?
instance Eq EPrim where
  (EPrim at ag _) == (EPrim bt bg _) = at == bt && ag == bg

instance Hashable EPrim where
  hashWithSalt s (EPrim at ag _) = s `hashWithSalt` at `hashWithSalt` ag

data EvalTreebugOpen = EvalTreebugOpen EObject EArrow
  deriving (Eq, Generic, Hashable)
data EvalTreebugClosed = EvalTreebugClosed EObject EArrow Val [EvalTreebugClosed] String
  deriving (Eq, Generic, Hashable, ToJSON)

data Env = Env { evObjMap :: EObjectMap
               , evClassMap :: ClassMap
               , evArgs :: H.HashMap ArgName Val
               , evExEnv :: ResExEnv
               , evTbEnv :: TBEnv
               , evCallStack :: [String]
               , evCoverage :: H.HashMap EArrow Int
               , evTreebugOpen :: [EvalTreebugOpen]
               , evTreebugClosed :: [EvalTreebugClosed]
               }

data EvalResult = EvalResult { erCoverage :: H.HashMap EArrow Int
                             , erTreebug :: [EvalTreebugClosed]
                             } deriving (Eq, Generic, ToJSON)

type Args = H.HashMap String Val

data Val
  = IntVal Integer
  | FloatVal Double
  | StrVal String
  | TupleVal String (H.HashMap String Val)
  | IOVal Integer (IO ())
  | LLVMVal (LLVM ())
  | LLVMQueue [(ResArrowTree, Object Typed, Arrow (Expr Typed) Typed)]
  | LLVMOperand Type (Codegen AST.Operand)
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
  show (TupleVal name args) = name ++ showArgs args
    where
      showArgs as | H.null as = ""
      showArgs as = printf "(%s)" (intercalate ", " $ map showArg $ H.toList as)
      showArg (argName, val) = argName ++ " = " ++ show val
  show IOVal{}   = "IOVal"
  show LLVMVal{}   = "LLVMVal"
  show LLVMQueue{}   = "LLVMQueue"
  show (LLVMOperand tp _)   = printf "LLVMOperand<$T=%s>" (show tp)
  show LLVMIO{}   = "LLVMIO"
  show NoVal   = "NoVal"

instance Hashable Val where
  hashWithSalt s (IntVal i) = s `hashWithSalt` i
  hashWithSalt s (FloatVal i) = s `hashWithSalt` i
  hashWithSalt s (StrVal i) = s `hashWithSalt` i
  hashWithSalt s (TupleVal n as) = s `hashWithSalt` n `hashWithSalt` as
  hashWithSalt s (IOVal i _) = s `hashWithSalt` i
  hashWithSalt s (LLVMVal _) = s
  hashWithSalt s (LLVMQueue _) = s
  hashWithSalt s (LLVMOperand tp _) = s `hashWithSalt` tp
  hashWithSalt s (LLVMIO _) = s
  hashWithSalt s NoVal = s

instance ToJSON Val where
  toJSON (IntVal v) = object ["tag".=("IntVal" :: String), "contents".=toJSON v]
  toJSON (FloatVal v) = object ["tag".=("FloatVal" :: String), "contents".=toJSON v]
  toJSON (StrVal v) = object ["tag".=("StrVal" :: String), "contents".=toJSON v]
  toJSON (TupleVal name args) = object ["tag".=("TupleVal" :: String), "name".=name, "args".=toJSON args]
  toJSON IOVal{} = object ["tag".=("IOVal" :: String)]
  toJSON LLVMVal{} = object ["tag".=("LLVMVal" :: String)]
  toJSON LLVMQueue{} = object ["tag".=("LLVMQueue" :: String)]
  toJSON LLVMOperand{} = object ["tag".=("LLVMOperand" :: String)]
  toJSON LLVMIO{} = object ["tag".=("LLVMIO" :: String)]
  toJSON NoVal = object ["tag".=("NoVal" :: String)]

resultLeaf, queueLeaf :: PartialType
resultLeaf = PartialType (PTypeName "CatlnResult") H.empty H.empty (H.fromList [("name", strType), ("contents", strType)]) PtArgExact
queueLeaf = PartialType (PTypeName "llvmQueue") H.empty H.empty H.empty PtArgExact

getValType :: Val -> PartialType
getValType IntVal{} = intLeaf
getValType FloatVal{} = floatLeaf
getValType StrVal{} = strLeaf
getValType (TupleVal name args) = PartialType (PTypeName name) H.empty H.empty (fmap fromArg args) PtArgExact
  where fromArg arg = singletonType $ getValType arg
getValType IOVal{} = ioLeaf
getValType LLVMVal{} = resultLeaf
getValType LLVMQueue{} = queueLeaf
getValType (LLVMOperand t _) = case t of
  SumType leafs -> case splitPartialLeafs leafs of
    [partial] -> partial
    _ -> error "could not getValType without a single partial"
  _ -> error $ printf "could not get non sum getValType %s" (show t)
getValType LLVMIO{} = ioLeaf
getValType NoVal = error "getValType of NoVal"


--- ResArrowTree
data MacroData = MacroData {
                               mdTbEnv :: TBEnv
                             , mdObj :: Object Typed
                             , mdObjSrcType :: PartialType
                             }
newtype MacroFunction = MacroFunction (ResArrowTree -> MacroData -> CRes ResArrowTree)
type ResBuildEnvFunction = ResArrowTree -> ResArrowTree
type ResBuildEnvItem = (PartialType, Guard (Expr Typed), ResBuildEnvFunction)
type ResBuildEnv = H.HashMap TypeName [ResBuildEnvItem]
type ResExEnv = H.HashMap (PartialType, Arrow (Expr Typed) Typed) (ResArrowTree, [ResArrowTree]) -- (result, [compAnnot trees])

data TBEnv = TBEnv {
    tbName :: String
  , tbResEnv :: ResBuildEnv
  , tbVals :: H.HashMap PartialType ResArrowTree
  , tbPrgm :: Prgm (Expr Typed) Typed
  , tbClassMap :: ClassMap
  }

instance Eq MacroFunction where
  _ == _ = False

instance Hashable MacroFunction where
  s `hashWithSalt` _ = s

data ResArrowTree
  = ResEArrow ResArrowTree (Object Typed) (Arrow (Expr Typed) Typed)
  | PrimArrow ResArrowTree Type EPrim
  | MacroArrow ResArrowTree Type MacroFunction
  | ExprArrow EExpr Type Type
  | ConstantArrow Val
  | ArgArrow Type String
  | ResArrowMatch ResArrowTree Type (H.HashMap PartialType ResArrowTree)
  | ResArrowCond Type [((ResArrowTree, ResArrowTree, Object Typed), ResArrowTree)] ResArrowTree -- [((if, ifInput, ifObj), then)] else
  | ResArrowTuple String (H.HashMap String ResArrowTree)
  | ResArrowTupleApply ResArrowTree String ResArrowTree
  deriving (Eq, Generic, Hashable)

instance Show ResArrowTree where
  show (ResEArrow _ obj arrow) = printf "(ResEArrow: %s -> %s)" (show obj) (show arrow)
  show (PrimArrow _ tp _) = "(PrimArrow " ++ show tp ++ ")"
  show (MacroArrow _ tp _) = "(MacroArrow " ++ show tp ++ ")"
  show (ExprArrow _ exprType destType) = "(ExprArrow " ++ show exprType ++ " to " ++ show destType ++ ")"
  show (ConstantArrow c) = "(ConstantArrow " ++ show c ++ ")"
  show (ArgArrow tp n) = "(ArgArrow " ++ show tp ++ " " ++ n ++ ")"
  show (ResArrowMatch m _ args) = printf "match (%s) {%s}" (show m) args'
    where
      showArg (leaf, tree) = show leaf ++ " -> " ++ show tree
      args' = intercalate ", " $ map showArg $ H.toList args
  show (ResArrowCond _ ifTrees elseTree) = "( [" ++ ifTrees' ++ "] ( else " ++ show elseTree ++ ") )"
    where
      showIfTree (condTree, thenTree) = "if " ++ show condTree ++ " then " ++ show thenTree
      ifTrees' = intercalate ", " $ map showIfTree ifTrees
  show (ResArrowTuple name args) = if H.null args
    then name
    else name ++ "(" ++ args' ++ ")"
    where
      showArg (argName, val) = argName ++ " = " ++ show val
      args' = intercalate ", " $ map showArg $ H.toList args
  show (ResArrowTupleApply base argName argVal) = printf "(%s)(%s = %s)" (show base) argName (show argVal)


-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

data LLVMState
  = LLVMState {
    lsMod :: AST.Module
  , lTaskArrows :: [TaskArrow]
  , lTaskStructs :: [TaskStruct]
  , lTasksCompleted :: S.HashSet String
                           }

newtype LLVM a = LLVM (State LLVMState a)
  deriving newtype (Functor, Applicative, Monad, MonadState LLVMState )
data DeclInput
  = TupleInput
  | StructInput
  deriving (Eq, Ord, Show, Generic, Hashable)

type TaskArrow = (PartialType, Object Typed, Arrow (Expr Typed) Typed, DeclInput)
type TaskStruct = Type

type Names = Map.Map String Int

data CodegenState
  = CodegenState {
    currentBlock :: AST.Name                     -- Name of the active block to append to
  , blocks       :: Map.Map AST.Name BlockState  -- Blocks for function
  , cgArgs         :: H.HashMap ArgName AST.Operand    -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  , taskArrows :: [TaskArrow]
  , taskStructs :: [TaskStruct]
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [AST.Named AST.Instruction]            -- Stack of instructions
  , term  :: Maybe (AST.Named AST.Terminator)       -- Block terminator
  } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving newtype (Functor, Applicative, Monad, MonadState CodegenState )

instance Show (Codegen a) where
  show Codegen{} = "Codegen"


type ObjSrc = (PartialType, Object Typed)
macroData :: TBEnv -> ObjSrc -> MacroData
macroData tbEnv (objSrcType, obj) = MacroData tbEnv obj objSrcType

resArrowDestType :: ClassMap -> PartialType -> ResArrowTree -> Type
resArrowDestType classMap src (ResEArrow _ obj arr) = arrowDestType False classMap src obj arr
resArrowDestType _ _ (PrimArrow _ tp _) = tp
resArrowDestType _ _ (MacroArrow _ tp _) = tp
resArrowDestType _ _ (ConstantArrow v) = singletonType $ getValType v
resArrowDestType _ _ (ArgArrow tp _) = tp
resArrowDestType _ _ t = error $ printf "Not yet implemented resArrowDestType for %s" (show t)


buildArrArgs :: EObject -> Val -> Args
buildArrArgs = aux H.empty
  where
    aux acc Object{objName, objArgs} val | H.null objArgs = H.insert objName val acc
    aux _ Object{objName} (TupleVal tupleName _) | objName /= tupleName = error $ printf "Found name mismatch in buildArrArgs: object %s and tuple %s" objName tupleName
    aux acc Object{objArgs} (TupleVal _ tupleArgs) = H.foldrWithKey addArgs acc $ H.intersectionWith (,) objArgs tupleArgs
    aux _ obj val = error $ printf "Invalid buildArrArgs with obj %s and value %s" (show obj) (show val)
    addArgs argName ((_, Nothing), argVal) acc = H.insert argName argVal acc
    addArgs _ ((_, Just subObj), argVal) acc = aux acc subObj argVal
