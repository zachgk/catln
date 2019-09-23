--------------------------------------------------------------------
-- |
-- Module    :  Eval
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Eval where

import qualified Data.HashMap.Strict as H
import           Syntax

import           Control.Monad

type EvalMeta = PreTyped
type EExpr = Expr EvalMeta
type EDecl = Decl EvalMeta
type EDeclLHS = DeclLHS EvalMeta
type EPrgm = Prgm EvalMeta
type EReplRes = ReplRes EvalMeta

data Val
  = IntVal Integer
  | FloatVal Double
  | BoolVal Bool
  | StrVal String
  | PrimVal ([Val] -> Val) -- runtime function
  | CloVal [Name] Env [EDecl] EExpr -- Function definition

data EvalError
  = GenEvalError String
  | AssertError String
  deriving (Eq, Show)
type Env = H.HashMap String Val

instance Show Val where
  show (IntVal i)   = show i
  show (FloatVal d) = show d
  show (BoolVal b)  = show b
  show (PrimVal _)  = "*primitive"
  show CloVal{}     = "*closure"

liftInt :: Integer -> Val
liftInt i = IntVal i

liftFloat :: Double -> Val
liftFloat d = FloatVal d

lowerInt ::  Val -> Integer
lowerInt (IntVal i) = i

liftIntOp :: (Integer -> Integer -> Integer) -> Val
liftIntOp f = PrimVal f'
  where f' [a, b] = liftInt $ f (lowerInt a) (lowerInt b)

liftCmpOp :: (Integer -> Integer -> Bool) -> Val
liftCmpOp f = PrimVal f'
  where f' [a, b] = BoolVal $ f (lowerInt a) (lowerInt b)

liftBoolOp :: (Bool -> Bool -> Bool) -> Val
liftBoolOp f = PrimVal f'
  where f' [BoolVal a, BoolVal b] = BoolVal $ f a b

rnot = PrimVal f'
  where f' [BoolVal b] = BoolVal $ not b

baseEnv :: Env
baseEnv = H.fromList [ ("+", liftIntOp (+))
                     , ("-", liftIntOp (-))
                     , ("*", liftIntOp (*))
                     , (">", liftCmpOp (>))
                     , ("<", liftCmpOp (<))
                     , (">=", liftCmpOp (>=))
                     , ("<=", liftCmpOp (<=))
                     , ("==", liftCmpOp (==))
                     , ("!=", liftCmpOp (/=))
                     , ("&", liftBoolOp (&&))
                     , ("|", liftBoolOp (||))
                     , ("~", rnot)
                     ]

evalExpr :: Env -> EExpr -> Either EvalError Val
evalExpr env (CExpr _ (CInt i)) = Right $ IntVal i
evalExpr env (CExpr _ (CFloat f)) = Right $ FloatVal f
evalExpr env (CExpr _ (CStr s)) = Right $ StrVal s
evalExpr env (Var _ id) = case H.lookup id env of
  Just v  -> Right v
  Nothing -> Left $ GenEvalError $ "Could not find value " ++ id
evalExpr env (Call _ "assert" [test, (CExpr _ (CStr msg))]) = evalExpr env test >>= (\(BoolVal b) -> if b == True then Right (BoolVal b) else Left (AssertError msg))
evalExpr env (Call _ name exprs) = do
  vals <- mapM (evalExpr env) exprs
  case H.lookup name env of
    Just (PrimVal f) -> Right $ f vals
    Just (CloVal names cenv subDecls cexpr) -> evalDecl (H.union (H.fromList $ zip names vals) cenv) cexpr subDecls
    Just _ -> Left $ GenEvalError $ "Could not call " ++ name
    Nothing -> Left $ GenEvalError $ "Could not find function " ++ name

evalDecl :: Env -> EExpr -> [EDecl] -> Either EvalError Val
evalDecl env expr subDecls = do
  subEnv <- addDecls env subDecls
  evalExpr subEnv expr

addDecl :: Env -> EDecl -> Either EvalError Env
addDecl env (Decl (DeclVal id) subDecls expr) = evalDecl env expr subDecls >>= (\val -> Right $ H.insert id val env)
addDecl env (Decl (DeclFun id args) subDecls expr) = return env'
                                                     where cl = CloVal (map fst args) env' subDecls expr
                                                           env' = H.insert id cl env

addDecls :: Env -> [EDecl] -> Either EvalError Env
addDecls env decls = foldM (\e d -> addDecl e d) env decls

evalPrgm :: EPrgm -> Either EvalError Val
evalPrgm decls = do
  env <- addDecls baseEnv decls
  main <- case H.lookup "main" env of
                                   Just m@CloVal{} -> Right m
                                   Just _ -> Left $ GenEvalError "Wrong type of main function"
                                   Nothing -> Left $ GenEvalError "No main function defined"
  let CloVal _ env subDecls expr = main in
    evalDecl env expr subDecls
