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
import qualified Data.HashSet          as S
import Data.Bifunctor (first)
import           Syntax

import           Control.Monad

type EvalMeta = Typed
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
  | TupleVal String (H.HashMap String Val)

data EvalError
  = GenEvalError String
  | AssertError String
  deriving (Eq, Show)

data ResArrow
  = ResEArrow EArrow
  | IDArrow
  | PrimArrow (H.HashMap String Val -> Val) -- runtime function
  | ValArrow Val

-- TODO: Convert destType to Type and then handle the sum as necessary
-- (consider case of Optional which is not known until runtime)
type ResEnv = H.HashMap (LeafType, LeafType) ResArrow
type Env = (ResEnv, H.HashMap LeafType Val)

instance Show Val where
  show (IntVal i)   = show i
  show (FloatVal d) = show d
  show (BoolVal b)  = show b
  show (StrVal s)   = show s
  show (TupleVal n a)   = show n ++ show a

liftInt :: Integer -> Val
liftInt = IntVal

liftFloat :: Double -> Val
liftFloat = FloatVal

lowerInt ::  Val -> Integer
lowerInt (IntVal i) = i
lowerInt _          = error "can't lift non-int"

liftIntOp :: Name -> (Integer -> Integer -> Integer) -> ((LeafType, LeafType), ResArrow)
liftIntOp name f = ((srcType, destType), arrow)
  where
    srcType = LeafType name (H.fromList [("l", intLeaf), ("r", intLeaf)])
    destType = intLeaf
    arrow = PrimArrow (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (IntVal l), Just (IntVal r)) -> IntVal $ f l r
                           _ -> error "Invalid intOp signature"
                           )

liftCmpOp :: Name -> (Integer -> Integer -> Bool) -> ((LeafType, LeafType), ResArrow)
liftCmpOp name f = ((srcType, destType), arrow)
  where
    srcType = LeafType name (H.fromList [("l", intLeaf), ("r", intLeaf)])
    destType = boolLeaf
    arrow = PrimArrow (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (IntVal l), Just (IntVal r)) -> BoolVal $ f l r
                           _ -> error "Invalid compOp signature"
                           )

liftBoolOp :: Name -> (Bool -> Bool -> Bool) -> ((LeafType, LeafType), ResArrow)
liftBoolOp name f = ((srcType, destType), arrow)
  where
    srcType = LeafType name (H.fromList [("l", boolLeaf), ("r", boolLeaf)])
    destType = boolLeaf
    arrow = PrimArrow (\args -> case (H.lookup "l" args, H.lookup "r" args) of
                           (Just (BoolVal l), Just (BoolVal r)) -> BoolVal $ f l r
                           _ -> error "Invalid boolOp signature"
                           )

rnot :: Name -> ((LeafType, LeafType), ResArrow)
rnot name = ((srcType, destType), arrow)
  where
    srcType = LeafType name (H.singleton "a" boolLeaf)
    destType = boolLeaf
    arrow = PrimArrow (\args -> case H.lookup "a" args of
          Just (BoolVal b) -> BoolVal $ not b
          _ -> error "Invalid rnot signature"
          )

primEnv :: ResEnv
primEnv = H.fromList [ liftIntOp "+" (+)
                     , liftIntOp "-" (-)
                     , liftIntOp "*" (*)
                     , liftCmpOp ">" (>)
                     , liftCmpOp "<" (<)
                     , liftCmpOp ">=" (>=)
                     , liftCmpOp "<=" (<=)
                     , liftCmpOp "==" (==)
                     , liftCmpOp "!=" (/=)
                     , liftBoolOp "&" (&&)
                     , liftBoolOp "|" (||)
                     , rnot "~"
                     ]

leafFromMeta :: EvalMeta -> LeafType
leafFromMeta (Typed (SumType prodTypes)) = case S.toList prodTypes of
  [leafType] -> leafType
  _ -> error "Arrow has multiple leaves"

makeBaseEnv :: [EArrow] -> Env
makeBaseEnv arrows = (H.union primEnv resEnv, H.empty)
  where
    resEnv = H.fromList $ map resFromArrow arrows
    resFromArrow arrow@(Arrow am (Object om _ _) _) = ((leafFromMeta om, leafFromMeta am), ResEArrow arrow)

evalExpr :: Env -> EExpr -> LeafType -> Either EvalError Val
evalExpr _ (CExpr _ (CInt i)) intType = Right $ IntVal i
evalExpr _ (CExpr _ (CFloat f)) floatType = Right $ FloatVal f
evalExpr _ (CExpr _ (CStr s)) strType = Right $ StrVal s
evalExpr env (Tuple _ "assert" args) _ =
  case (H.lookup "test" args, H.lookup "msg" args) of
    (Just test, Just (CExpr _ (CStr msg))) -> evalExpr env test boolLeaf >>= (\(BoolVal b) -> if b then Right (BoolVal b) else Left (AssertError msg))
    _ -> Left $ GenEvalError "Invalid assertion"
evalExpr env (Tuple typed@(Typed (SumType prodTypes)) name exprs) destType = case S.toList prodTypes of
    (_:_:_) -> Left $ GenEvalError $ "Found multiple types for " ++ name
    [] -> Left $ GenEvalError $ "Found no types for " ++ name
    [prodType@(LeafType name leafType)] | H.keysSet exprs == H.keysSet leafType -> do
                           vals <- mapM (\(destType, expr) -> evalExpr env (Tuple typed name exprs) destType) $ H.intersectionWith (,) leafType exprs
                           case envLookup env prodType destType of
                             Right (ResEArrow (Arrow m _ resExpr)) -> do
                               let env' = envWithVals env (H.fromList $ map (first (\valName -> LeafType valName H.empty)) $ H.toList vals)
                               let destType' = leafFromMeta m
                               evalExpr env' resExpr destType'
                             Right IDArrow -> return $ TupleVal name vals
                             Right (ValArrow val) -> return val
                             Right (PrimArrow f) -> Right $ f vals
                             Left err -> Left err
    _ -> Left $ GenEvalError $ "Found bad types for " ++ name

envWithVals :: Env -> H.HashMap LeafType Val -> Env
envWithVals (resEnv, _) vals = (resEnv, vals)

envLookup :: Env -> LeafType -> LeafType -> Either EvalError ResArrow
envLookup _ srcType destType | srcType == destType = Right IDArrow
envLookup (resEnv, valEnv) srcType destType = case H.lookup srcType valEnv of
  Just val -> Right $ ValArrow val
  Nothing -> case H.lookup (srcType, destType) resEnv of
    Just resArrow -> Right resArrow
    Nothing -> Left $ GenEvalError $ "Failed to lookup arrow for " ++ show (srcType, destType)

evalPrgm :: EPrgm -> Either EvalError Val
evalPrgm (objects, arrows) = evalExpr (makeBaseEnv arrows) main intLeaf
  where main = Tuple (Typed $ SumType $ S.singleton $ LeafType "main" H.empty) "main" H.empty
