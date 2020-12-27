--------------------------------------------------------------------
-- |
-- Module    :  Emit.Codegen
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

-- Originally from http://www.stephendiehl.com/llvm/#haskell-llvm-bindings

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

module Emit.Codegen where

import qualified Data.ByteString.Short      as SBS
import qualified Data.ByteString.UTF8       as BSU
import           Data.Function
import           Data.List
import qualified Data.Map                   as Map

import           Control.Monad.State

import           LLVM.AST
import qualified LLVM.AST                   as AST
import           LLVM.AST.Global

import qualified LLVM.AST.Attribute         as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant          as C
import qualified LLVM.AST.Linkage           as L
import LLVM.AST.Type
import LLVM.AST.Typed
import Syntax.Prgm
import qualified Syntax as SYN
import qualified Syntax.Types as SYNT
import qualified Data.HashSet as S
import Text.Printf
import GHC.Generics (Generic)
import Data.Hashable

-------------------------------------------------------------------------------
-- Module Level
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

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM astMod (LLVM m) = lsMod $ execState m initState
  where initState = LLVMState astMod [] [] S.empty

emptyModule :: SBS.ShortByteString -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  m <- gets lsMod
  let defs = moduleDefinitions m
  modify $ \s -> s { lsMod = m {moduleDefinitions = defs ++ [d] }}

define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name $ fromString label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

external ::  Type -> SBS.ShortByteString -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , linkage     = L.External
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

struct :: Name -> [Type] -> LLVM ()
struct n as = addDefn $ TypeDefinition n (Just $ StructureType False as)

structType :: [Type] -> Type
structType = StructureType False

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map String Int

uniqueName :: String -> Names -> (SBS.ShortByteString, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (fromString nm,  Map.insert nm 1 ns)
    Just ix -> (fromString $ nm ++ show ix, Map.insert nm (ix+1) ns)


-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(String, Operand)]

data DeclInput
  = TupleInput
  | StructInput
  deriving (Eq, Ord, Show, Generic, Hashable)
type TaskArrow = (SYNT.PartialType, Object SYN.Typed, Arrow (Expr SYN.Typed) SYN.Typed, DeclInput)
type TaskStruct = SYNT.Type

data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  , taskArrows :: [TaskArrow]
  , taskStructs :: [TaskStruct]
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving newtype (Functor, Applicative, Monad, MonadState CodegenState )

instance Show (Codegen a) where
  show Codegen{} = "Codegen"

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> LLVM [BasicBlock]
createBlocks m = do
  curTaskArrows <- gets lTaskArrows
  modify $ \s -> s {lTaskArrows = taskArrows m ++ curTaskArrows}

  curTaskStructs <- gets lTaskStructs
  modify $ \s -> s {lTaskStructs = taskStructs m ++ curTaskStructs}

  return $ map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing  = error $ "Block has no terminator: " ++ show l

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name $ fromString entryBlockName) Map.empty [] 1 0 Map.empty [] []

execCodegen :: [(String, Operand)] -> Codegen a -> CodegenState
execCodegen vars m = execState (runCodegen m) emptyCodegen { symtab = vars }

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x  -> return x
    Nothing -> error $ "No such block: " ++ show c

instr :: Type -> Instruction -> Codegen Operand
instr tp ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref := ins) : i } )
  return $ local tp ref

voidInstr :: Instruction -> Codegen ()
voidInstr ins = do
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = Do ins : i } )
  return ()

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

-- named :: SBS.ShortByteString -> Codegen a -> Codegen Operand
-- named iname m = m >> do
--   blk <- current
--   let b = Name iname
--       (_ := x) = last (stack blk)
--   modifyBlock $ blk { stack = init (stack blk) ++ [b := x] }
--   return $ local double b

-------------------------------------------------------------------------------
-- Tasks
-------------------------------------------------------------------------------

class TaskState m where
  addTaskArrow :: TaskArrow -> m ()
  addTaskStruct :: TaskStruct -> m ()

instance TaskState Codegen where
  addTaskArrow task = do
    curTasks <- gets taskArrows
    modify $ \s -> s {taskArrows = task:curTasks}
  addTaskStruct task = do
    curTasks <- gets taskStructs
    modify $ \s -> s {taskStructs = task:curTasks}

instance TaskState LLVM where
  addTaskArrow task = do
    curTasks <- gets lTaskArrows
    modify $ \s -> s {lTaskArrows = task:curTasks}
  addTaskStruct task = do
    curTasks <- gets lTaskStructs
    modify $ \s -> s {lTaskStructs = task:curTasks}

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = (var, x):lcls }

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

-------------------------------------------------------------------------------

-- References
local ::  Type -> Name -> Operand
local = LocalReference

global ::  Type -> String -> Operand
global tp = ConstantOperand . C.GlobalReference tp . astName

externf :: Type -> Name -> Operand
externf t = ConstantOperand . C.GlobalReference (ptr t)

cons :: C.Constant -> Operand
cons = ConstantOperand

getelementptr :: Type -> Operand -> [Operand] -> Codegen Operand
getelementptr elementType tp ops = case typeOf tp of
  PointerType{} -> instr (ptr elementType) $ GetElementPtr True tp ops []
  _ -> error $ printf "Invalid getElementPtr address: %s" (show tp)

typeOperand :: Type -> Operand
typeOperand = ConstantOperand . C.Undef

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr undefined $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (, [])

-- Effects
call :: Type -> Operand -> [Operand] -> Codegen Operand
call retType fn ags = instr retType $ Call Nothing CC.C [] (Right fn) (toArgs ags) [] []

callf :: Type -> String -> [Operand] -> Codegen Operand
callf retType fnName args = do
  let fnType = FunctionType retType (map typeOf args) False
  let fn = externf fnType (astName fnName)
  call retType fn args

alloca :: Type -> Codegen Operand
alloca ty = instr (ptr ty) $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen ()
store pointer val = voidInstr $ Store False pointer val Nothing 0 []

load :: Operand -> Codegen Operand
load pointer = instr (getElementType $ typeOf pointer) $ Load False pointer Nothing 0 []

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

switch :: Operand -> Name -> [(C.Constant, Name)] -> Codegen (Named Terminator)
switch cond def dests = terminator $ Do $ Switch cond def dests []

phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi ty incoming = instr ty $ Phi ty incoming []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

panic :: String -> Codegen Operand
-- panic _ = call (externf "exit") [cons $ C.Int 32 3] -- TODO
panic _ = return $ cons $ C.Int 32 0

astName :: String -> AST.Name
astName = AST.Name . fromString

toString :: SBS.ShortByteString -> String
toString = BSU.toString . SBS.fromShort

fromString :: String -> SBS.ShortByteString
fromString = SBS.toShort . BSU.fromString
