--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Prgm
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Syntax.Prgm where

import           Data.Hashable
import qualified Data.HashMap.Strict as H
import           Data.List                      ( intercalate )
import           GHC.Generics          (Generic)

import Syntax.Types
import           Text.Printf

newtype Import = Import String
  deriving (Eq, Ord, Show)

newtype Export = Export String
  deriving (Eq, Ord, Show)

data Constant
  = CInt Integer
  | CFloat Double
  | CStr String
  deriving (Eq, Ord, Show, Generic, Hashable)

data Pattern m e = Pattern (Object m) (Guard e)
  deriving (Eq, Ord, Show, Generic, Hashable)

data RawExpr m
  = RawCExpr m Constant
  | RawValue m Name
  | RawTupleApply m (m, RawExpr m) (H.HashMap Name (RawExpr m))
  | RawIfThenElse m (RawExpr m) (RawExpr m) (RawExpr m)
  | RawMatch m (RawExpr m) (H.HashMap (Pattern m (RawExpr m)) (RawExpr m))
  | RawCase m (RawExpr m) [(Pattern m (RawExpr m), RawExpr m)]
  deriving (Eq, Ord, Show, Generic, Hashable)

data Expr m
  = CExpr m Constant
  | Value m Name
  | Arg m Name
  | TupleApply m (m, Expr m) (H.HashMap Name (Expr m))
  deriving (Eq, Ord, Generic, Hashable)

-- Compiler Annotation
data CompAnnot e = CompAnnot Name (H.HashMap Name e)
  deriving (Eq, Ord, Generic, Hashable)

data Guard e
  = IfGuard e
  | ElseGuard
  | NoGuard
  deriving (Eq, Ord, Generic, Hashable)

instance Functor Guard where
  fmap f (IfGuard e) = IfGuard (f e)
  fmap _ ElseGuard = ElseGuard
  fmap _ NoGuard = NoGuard

data RawDeclSubStatement m
  = RawDeclSubStatementDecl (RawDecl m)
  | RawDeclSubStatementAnnot (CompAnnot (RawExpr m))
  deriving (Eq, Ord, Show)

data DeclLHS m e = DeclLHS m (Pattern m e)
  deriving (Eq, Ord, Show)

data RawDecl m = RawDecl (DeclLHS m (RawExpr m)) [RawDeclSubStatement m] (Maybe (RawExpr m))
  deriving (Eq, Ord, Show)

data TypeDef m = TypeDef Name Type
  deriving (Eq, Ord, Show)

type RawClassDef = (TypeName, ClassName)

data RawStatement m
  = RawDeclStatement (RawDecl m)
  | TypeDefStatement (TypeDef m)
  | RawClassDefStatement RawClassDef
  deriving (Eq, Ord, Show)

type FileImport = String
type RawPrgm m = ([FileImport], [RawStatement m]) -- TODO: Include [Export]

type ObjArg m = (m, Maybe (Object m))
data ObjectBasis = FunctionObj | TypeObj | PatternObj
  deriving (Eq, Ord, Show, Generic, Hashable)
data Object m = Object m ObjectBasis Name (H.HashMap Name (ObjArg m))
  deriving (Eq, Ord, Generic, Hashable)

data Arrow m = Arrow m [CompAnnot (Expr m)] (Guard (Expr m)) (Maybe (Expr m)) -- m is result metadata
  deriving (Eq, Ord, Generic, Hashable)

type ObjectMap m = (H.HashMap (Object m) [Arrow m])
type Prgm m = (ObjectMap m, ClassMap) -- TODO: Include [Export]

instance Show m => Show (Expr m) where
  show (CExpr _ c) = show c
  show (Value _ name) = printf "Value %s" name
  show (Arg m name) = printf "Arg %s %s" (show m) name
  show (TupleApply _ (_, baseExpr) args) = printf "(%s)(%s)" (show baseExpr) args'
    where
      showArg (name, expr) = name ++ " = " ++ show expr
      args' = intercalate ", " $ map showArg $ H.toList args

instance Show e => Show (CompAnnot e) where
  show (CompAnnot name args) = if H.null args
    then name
    else name ++ "(" ++ args' ++ ")"
    where
      showArg (argName, expr) = argName ++ " = " ++ show expr
      args' = intercalate ", " $ map showArg $ H.toList args

instance Show e => Show (Guard e) where
  show (IfGuard expr) = "if (" ++ show expr ++ ")"
  show ElseGuard = "else"
  show NoGuard = ""

instance Show m => Show (Object m) where
  show (Object _ basis name args) = printf "%s %s %s" (show basis) name maybeArgsString
    where
      showArg (argName, (_, Just argVal)) = printf "%s = %s" argName (show argVal)
      showArg (argName, (argM, Nothing)) = printf "%s %s" (show argM) argName
      maybeArgsString = if H.size args == 0
        then ""
        else "(" ++ intercalate ", " (map showArg $ H.toList args) ++ ")"

instance Show m => Show (Arrow m) where
  show (Arrow m annots guard maybeExpr) = concat $ [show guard, " -> ", show m, " "] ++ showExpr maybeExpr ++ showAnnots annots
    where
      showExpr (Just expr) = [" = ", show expr]
      showExpr Nothing = []
      showAnnots [] = []
      showAnnots _ = [" ", show annots]

getExprMeta :: Expr m -> m
getExprMeta expr = case expr of
  CExpr m _   -> m
  Value m _   -> m
  Arg m _   -> m
  TupleApply m _ _ -> m

type ArgMetaMap m = H.HashMap Name m
formArgMetaMap :: Object m -> ArgMetaMap m
formArgMetaMap (Object m _ name args) | H.null args = H.singleton name m
formArgMetaMap (Object _ _ _ args) = H.foldr (H.unionWith unionCombine) H.empty $ H.mapWithKey fromArg args
  where
    unionCombine _ _ = error "Duplicate var matched"
    fromArg k (m, Nothing) = H.singleton k m
    fromArg _ (_, Just arg) = formArgMetaMap arg
