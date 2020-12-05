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

getArrowTree :: Env -> EStacktrace -> EObject -> EArrow -> CRes (ResArrowTree EPrim, [ResArrowTree EPrim], Env)
getArrowTree env@Env{evExEnv} st _ arr = case H.lookup arr evExEnv of
  Just (_, tree, annots) -> return (tree, annots, env)
  Nothing -> CErr [MkCNote $ EvalCErr st $ printf "Failed to find arrow in eval resArrow: %s" (show arr)]

