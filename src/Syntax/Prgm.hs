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
import qualified Data.HashSet as S
import           Data.List                      ( intercalate )
import           GHC.Generics          (Generic)

import Syntax.Types
import           Text.Printf
import Data.Aeson.Types (ToJSON)
import Data.Aeson (ToJSONKey)

newtype Import = Import String
  deriving (Eq, Ord, Show)

newtype Export = Export String
  deriving (Eq, Ord, Show)

data Constant
  = CInt Integer
  | CFloat Double
  | CStr String
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

data Pattern e m = Pattern (Object m) (Guard e)
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, ToJSONKey)

data RawTupleArg m
  = RawTupleArgNamed ArgName (RawExpr m)
  | RawTupleArgInfer (RawExpr m)
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- Expr before desugar
data RawExpr m
  = RawCExpr m Constant
  | RawValue m TypeName
  | RawTupleApply m (m, RawExpr m) [RawTupleArg m]
  | RawParen (RawExpr m)
  | RawMethods (RawExpr m) [RawExpr m]
  | RawIfThenElse m (RawExpr m) (RawExpr m) (RawExpr m)
  | RawMatch m (RawExpr m) [(Pattern (RawExpr m) m, RawExpr m)]
  | RawCase m (RawExpr m) [(Pattern (RawExpr m) m, RawExpr m)]
  | RawList m [RawExpr m]
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- Expr (to infer) from desugar to typecheck
data IExpr m
  = ICExpr m Constant
  | IValue m TypeName
  | IArg m ArgName
  | ITupleApply m (m, IExpr m) (Maybe ArgName) (IExpr m) -- the ArgName is optional. Must be inferred if Nothing
  deriving (Eq, Ord, Generic, Hashable, ToJSON)

-- Expr after typechecking
data Expr m
  = CExpr m Constant
  | Value m TypeName
  | Arg m ArgName
  | TupleApply m (m, Expr m) ArgName (Expr m)
  deriving (Eq, Ord, Generic, Hashable, ToJSON)

-- Compiler Annotation
type CompAnnot e = e

data Guard e
  = IfGuard e
  | ElseGuard
  | NoGuard
  deriving (Eq, Ord, Generic, Hashable, ToJSON)

instance Functor Guard where
  fmap f (IfGuard e) = IfGuard (f e)
  fmap _ ElseGuard = ElseGuard
  fmap _ NoGuard = NoGuard

data RawDeclSubStatement m
  = RawDeclSubStatementDecl (RawDecl m)
  | RawDeclSubStatementAnnot (CompAnnot (RawExpr m))
  | RawDeclSubStatementComment String
  deriving (Eq, Ord, Show, Generic, ToJSON)

data DeclLHS e m = DeclLHS m (Pattern e m)
  deriving (Eq, Ord, Show, Generic, ToJSON)

data RawDecl m = RawDecl (DeclLHS (RawExpr m) m) [RawDeclSubStatement m] (Maybe (RawExpr m))
  deriving (Eq, Ord, Show, Generic, ToJSON)

newtype TypeDef m = TypeDef m
  deriving (Eq, Ord, Show, Generic, ToJSON)

data MultiTypeDef m = MultiTypeDef ClassName (H.HashMap TypeVarName Type) [m]
  deriving (Eq, Ord, Show, Generic, ToJSON)

type RawClassDef = ((TypeName, H.HashMap TypeVarName Type), ClassName)

type RawClassDecl = (ClassName, H.HashMap TypeVarName Type)

data RawStatement m
  = RawDeclStatement (RawDecl m)
  | MultiTypeDefStatement (MultiTypeDef m)
  | TypeDefStatement (TypeDef m)
  | RawClassDefStatement RawClassDef
  | RawClassDeclStatement RawClassDecl
  | RawComment String
  | RawGlobalAnnot (CompAnnot (RawExpr m)) [RawStatement m]
  deriving (Eq, Ord, Show, Generic, ToJSON)

type FileImport = String
type RawPrgm m = ([FileImport], [RawStatement m]) -- TODO: Include [Export]

type ObjArg m = (m, Maybe (Object m))
data ObjectBasis = FunctionObj | TypeObj | PatternObj | MatchObj
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)
data Object m = Object {
  objM :: m,
  objBasis :: ObjectBasis,
  objName :: TypeName,
  objVars :: H.HashMap TypeVarName m,
  objArgs :: H.HashMap ArgName (ObjArg m)
                       }
  deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)

data Arrow e m = Arrow m [CompAnnot e] (Guard e) (Maybe e) -- m is result metadata
  deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)

type ObjectMap e m = [(Object m, [Arrow e m])]
type Prgm e m = (ObjectMap e m, ClassMap, [CompAnnot e]) -- TODO: Include [Export]

instance Show m => Show (IExpr m) where
  show (ICExpr _ c) = show c
  show (IValue _ name) = printf "Value %s" name
  show (IArg m name) = printf "Arg %s %s" (show m) name
  show (ITupleApply _ (_, baseExpr) argName argVal) = printf "%s(%s%s)" baseExpr' argName' (show argVal)
    where
      baseExpr' = case baseExpr of
        IValue _ funName -> funName
        ITupleApply{} -> show baseExpr
        _ -> printf "(%s)" (show baseExpr)
      argName' = maybe "" (++ " = ") argName

instance Show m => Show (Expr m) where
  show (CExpr _ c) = show c
  show (Value _ name) = printf "Value %s" name
  show (Arg m name) = printf "Arg %s %s" (show m) name
  show (TupleApply _ (_, Value _ funName) argName argVal) = printf "%s(%s = %s)" funName argName (show argVal)
  show (TupleApply _ (_, baseExpr@TupleApply{}) argName argVal) = printf "%s(%s = %s)" (show baseExpr) argName (show argVal)
  show (TupleApply _ (_, baseExpr) argName argVal) = printf "(%s)(%s = %s)" (show baseExpr) argName (show argVal)

instance Show e => Show (Guard e) where
  show (IfGuard expr) = "if (" ++ show expr ++ ")"
  show ElseGuard = "else"
  show NoGuard = ""

instance Show m => Show (Object m) where
  -- show (Object m basis name vars args) = printf "%s %s (%s) %s %s" (show basis) name (show m) maybeVarsString maybeArgsString
  show (Object _ basis name vars args) = printf "%s %s %s %s" (show basis) name maybeVarsString maybeArgsString
    where
      showVar (varName, varVal) = printf "%s = %s" varName (show varVal)
      maybeVarsString :: String
      maybeVarsString = if H.size vars == 0
        then ""
        else printf "<%s>" (intercalate ", " $ map showVar $ H.toList vars)
      showArg (argName, (_, Just argVal)) = printf "%s = %s" argName (show argVal)
      showArg (argName, (argM, Nothing)) = printf "%s %s" (show argM) argName
      maybeArgsString = if H.size args == 0
        then ""
        else "(" ++ intercalate ", " (map showArg $ H.toList args) ++ ")"

instance (Show m, Show e) => Show (Arrow e m) where
  show (Arrow m annots guard maybeExpr) = concat $ [show guard, " -> ", show m, " "] ++ showExpr maybeExpr ++ showAnnots annots
    where
      showExpr (Just expr) = [" = ", show expr]
      showExpr Nothing = []
      showAnnots [] = []
      showAnnots _ = [" ", show annots]

class ExprClass e where
  getExprMeta ::  e m -> m
  getExprArg :: e m -> Maybe ArgName

instance ExprClass RawExpr where
  getExprMeta expr = case expr of
    RawCExpr m _   -> m
    RawValue m _   -> m
    RawTupleApply m _ _ -> m
    RawParen e -> getExprMeta e
    RawMethods e _ -> getExprMeta e
    RawIfThenElse m _ _ _ -> m
    RawMatch m _ _ -> m
    RawCase m _ _ -> m
    RawList m _ -> m

  getExprArg _ = Nothing

instance ExprClass Expr where
  getExprMeta expr = case expr of
    CExpr m _   -> m
    Value m _   -> m
    Arg m _   -> m
    TupleApply m _ _ _ -> m

  getExprArg (Arg _ n) = Just n
  getExprArg _ = Nothing

instance ExprClass IExpr where
  getExprMeta expr = case expr of
    ICExpr m _   -> m
    IValue m _   -> m
    IArg m _   -> m
    ITupleApply m _ _ _ -> m

  getExprArg (IArg _ n) = Just n
  getExprArg _ = Nothing

type ArgMetaMap m = H.HashMap ArgName m
formArgMetaMap :: Object m -> ArgMetaMap m
formArgMetaMap (Object m _ name _ args) | H.null args = H.singleton name m
formArgMetaMap Object{objArgs} = H.foldr (H.unionWith unionCombine) H.empty $ H.mapWithKey fromArg objArgs
  where
    unionCombine _ _ = error "Duplicate var matched"
    fromArg k (m, Nothing) = H.singleton k m
    fromArg _ (_, Just arg) = formArgMetaMap arg

mergeClassMaps :: ClassMap -> ClassMap -> ClassMap
mergeClassMaps classMap@(toClassA, toTypeA) (toClassB, toTypeB) = (H.unionWith S.union toClassA toClassB, H.unionWith mergeClasses toTypeA toTypeB)
  where mergeClasses (sealedA, classVarsA, setA) (sealedB, classVarsB, setB) = if sealedA == sealedB
          then (sealedA, H.unionWith (unionType classMap) classVarsA classVarsB, setA ++ setB)
          else error "Added to sealed class definition"

mergePrgm :: Prgm e m -> Prgm e m -> Prgm e m
mergePrgm (objMap1, classMap1, annots1) (objMap2, classMap2, annots2) = (
  objMap1 ++ objMap2,
  mergeClassMaps classMap1 classMap2,
  annots1 ++ annots2
                                                                           )

mergePrgms :: Foldable f => f (Prgm e m) -> Prgm e m
mergePrgms = foldr mergePrgm emptyPrgm
  where emptyPrgm = ([], (H.empty, H.empty), [])
