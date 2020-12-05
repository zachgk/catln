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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Eval.Common where

import           GHC.Generics          (Generic)
import           Data.Hashable
import qualified Data.HashMap.Strict as H
import           Data.List                      ( intercalate )

import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           Text.Printf
import CRes

type EvalMeta = Typed
type ECompAnnot = CompAnnot EvalMeta
type EExpr = Expr EvalMeta
type EGuard = Guard EExpr
type EObject = Object EvalMeta
type EArrow = Arrow EExpr EvalMeta
type EObjectMap = ObjectMap EExpr EvalMeta
type EPrgm = Prgm EExpr EvalMeta
type EReplRes = ReplRes EvalMeta

data EPrim = EPrim PartialType EGuard (H.HashMap String Val -> Val)
  deriving (Generic)

-- TODO: Maybe should include result type?
instance Eq EPrim where
  (EPrim at ag _) == (EPrim bt bg _) = at == bt && ag == bg

instance Hashable EPrim where
  hashWithSalt s (EPrim at ag _) = s `hashWithSalt` at `hashWithSalt` ag

data Env = Env { evObjMap :: EObjectMap
               , evClassMap :: ClassMap
               , evExEnv :: ResExEnv EPrim
               , evCallStack :: [String]
               , evCoverage :: H.HashMap EArrow Int
               } deriving (Eq, Show)

type Args = H.HashMap String Val

data Val
  = IntVal Integer
  | FloatVal Double
  | StrVal String
  | TupleVal String Args
  | IOVal Integer (IO ())
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
  show NoVal   = "NoVal"

getValType :: Val -> PartialType
getValType IntVal{} = intLeaf
getValType FloatVal{} = floatLeaf
getValType StrVal{} = strLeaf
getValType (TupleVal name args) = (PTypeName name, H.empty, H.empty, fmap fromArg args)
  where fromArg arg = singletonType $ getValType arg
getValType IOVal{} = ioLeaf
getValType NoVal = error "getValType of NoVal"

evalStartEArrow :: Env -> EObject -> EArrow -> CRes (ResArrowTree EPrim, [ResArrowTree EPrim], Env)
evalStartEArrow env@Env{evExEnv, evCallStack, evCoverage} _ arr = case H.lookup arr evExEnv of
  Just (tree, annots) -> return (tree, annots, env{evCoverage = H.insertWith (+) arr 1 evCoverage})
  Nothing -> CErr [MkCNote $ EvalCErr evCallStack $ printf "Failed to find arrow in eval resArrow: %s" (show arr)]

evalEndEArrow :: Env -> Env
evalEndEArrow env = env

evalEnvJoin :: Env -> Env -> Env
evalEnvJoin (Env objMap classMap exEnv1 callStack cov1) (Env _ _ exEnv2 _ cov2) = Env objMap classMap (H.union exEnv1 exEnv2) callStack (H.unionWith (+) cov1 cov2)

evalEnvJoinAll :: Foldable f => f Env -> Env
evalEnvJoinAll = foldr1 evalEnvJoin

evalError :: Env -> String -> CRes a
evalError Env{evCallStack} msg = CErr [MkCNote $ EvalCErr evCallStack msg]

evalPush :: Env -> String -> Env
evalPush env@Env{evCallStack} c = env{evCallStack = c:evCallStack}

evalPop :: Env -> Env
evalPop env@Env{evCallStack} = case evCallStack of
  (_:stack') -> env{evCallStack=stack'}
  _ -> error "Popped empty evCallStack"

evalPopVal :: (a, Env) -> (a, Env)
evalPopVal (a, env) = (a, evalPop env)
