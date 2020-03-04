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
type EObject = Object EvalMeta
type EArrow = Arrow EvalMeta
type EPrgm = Prgm EvalMeta
type EReplRes = ReplRes EvalMeta

data Val
  = IntVal Integer
  | FloatVal Double
  | BoolVal Bool
  | StrVal String
  | PrimVal (H.HashMap String Val -> Val) -- runtime function
  | CloVal [Name] Env EExpr -- Function definition

data EvalError
  = GenEvalError String
  | AssertError String
  deriving (Eq, Show)
type Env = H.HashMap String Val

instance Show Val where
  show (IntVal i)   = show i
  show (FloatVal d) = show d
  show (BoolVal b)  = show b
  show (StrVal s)   = show s
  show (PrimVal _)  = "*primitive"
  show CloVal{}     = "*closure"

liftInt :: Integer -> Val
liftInt = IntVal

liftFloat :: Double -> Val
liftFloat = FloatVal

lowerInt ::  Val -> Integer
lowerInt (IntVal i) = i
lowerInt _          = error "can't lift non-int"

liftIntOp :: (Integer -> Integer -> Integer) -> Val
liftIntOp f = PrimVal (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (IntVal l), Just (IntVal r)) -> IntVal $ f l r
                           _ -> error "Invalid intOp signature"
                           )

liftCmpOp :: (Integer -> Integer -> Bool) -> Val
liftCmpOp f = PrimVal (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (IntVal l), Just (IntVal r)) -> BoolVal $ f l r
                           _ -> error "Invalid compOp signature"
                           )

liftBoolOp :: (Bool -> Bool -> Bool) -> Val
liftBoolOp f = PrimVal (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (BoolVal l), Just (BoolVal r)) -> BoolVal $ f l r
                           _ -> error "Invalid boolOp signature"
                           )

rnot :: Val
rnot = PrimVal (\args -> case H.lookup "a" args of
          Just (BoolVal b) -> BoolVal $ not b
          _ -> error "Invalid rnot signature"
                )

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
evalExpr _ (CExpr _ (CInt i)) = Right $ IntVal i
evalExpr _ (CExpr _ (CFloat f)) = Right $ FloatVal f
evalExpr _ (CExpr _ (CStr s)) = Right $ StrVal s
evalExpr env (Tuple _ "assert" args) =
  case (H.lookup "test" args, H.lookup "msg" args) of
    (Just test, Just (CExpr _ (CStr msg))) -> evalExpr env test >>= (\(BoolVal b) -> if b then Right (BoolVal b) else Left (AssertError msg))
    _ -> error "Invalid assertion"
evalExpr env (Tuple _ name exprs) = do
  vals <- mapM (evalExpr env) exprs
  case H.lookup name env of
    Just (PrimVal f) -> Right $ f vals
    Just (CloVal names cenv cexpr) -> evalExpr (H.union vals cenv) cexpr
    Just _ -> Left $ GenEvalError $ "Could not call " ++ name
    Nothing -> Left $ GenEvalError $ "Could not find function " ++ name

-- addDecl :: Env -> EDecl -> Either EvalError Env
-- addDecl env (Decl (DeclLHS _ name []) expr) = do
--   val <- evalExpr env expr
--   return $ H.insert name val env
-- addDecl env (Decl (DeclLHS _ name args) expr) = return env'
--                                                      where cl = CloVal (map fst args) env' expr
--                                                            env' = H.insert name cl env

-- addDecls :: Env -> [EDecl] -> Either EvalError Env
-- addDecls = foldM addDecl

-- evalDecl :: EDecl -> Either EvalError Val
-- evalDecl decl = do
--   env <- addDecl baseEnv decl
--   case H.lookup (getDeclName decl) env of
--     Just (CloVal _ env' expr) -> evalExpr env' expr
--     Just val -> Right val
--     Nothing -> Left $ GenEvalError "No decl function defined"

evalObject :: [EArrow] -> EObject -> Either EvalError Val
evalObject = undefined

addObjects :: Env -> [EObject] -> Either EvalError Env
addObjects = undefined

evalPrgm :: EPrgm -> Either EvalError Val
evalPrgm (objects, arrows) = do
  env <- addObjects baseEnv objects
  case H.lookup "main" env of
    Just (CloVal _ env' expr) -> evalExpr env' expr
    Just _ -> Left $ GenEvalError "Wrong type of main function"
    Nothing -> Left $ GenEvalError "No main function defined"
