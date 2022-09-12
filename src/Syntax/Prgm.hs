--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Prgm
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines most of the types that make up a Catln
-- program.
--------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Syntax.Prgm where

import qualified Data.HashMap.Strict as H
import           Data.Hashable
import           Data.List           (intercalate)
import           GHC.Generics        (Generic)

import           Data.Aeson          (ToJSONKey)
import           Data.Aeson.Types    (ToJSON)
import           Data.Graph
import           Syntax.Types
import           Text.Printf

newtype Import = Import String
  deriving (Eq, Ord, Show)

newtype Export = Export String
  deriving (Eq, Ord, Show)

data Constant
  = CInt Integer
  | CFloat Double
  | CStr String
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

data Pattern e m = Pattern (Object m) (Guard (e m))
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, ToJSONKey)

-- |
-- An argument applied in an expression.
-- TODO Consider replacing the TupleArgI ArgName with an Expr as a generalization. In that case, this ArgName would be equivalent to a Value. It could also include lenses.
data TupleArg e m
  = TupleArgI m ArgName -- ^ An input arg. Can be thought of as a key without a value. Used only in input expressions.
  | TupleArgO m (e m) -- ^ An output arg. Can be thought of as a value with an unknown key. Used only in output expressions before typechecking, as typechecking will determine the matching key.
  | TupleArgIO m ArgName (e m) -- ^ An arg containing both the name and value.
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- | The type of hole (a gap where an expression should be). Used in inputs to ignore the expression and outputs.
data Hole
  = HoleActive (Maybe Name) -- ^ A hole such as _ or _name, where the name is optional and treated as an error
  | HoleUndefined -- ^ A hole with the keyword undefined that is an error only in runtime
  | HoleTodefine -- ^ A hole with the keyword todefine that is an error during runtime and commit time
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- Expr before desugar
data RawExpr m
  = RawCExpr m Constant
  | RawValue m TypeName
  | RawHoleExpr m Hole
  | RawTupleApply m (m, RawExpr m) [TupleArg RawExpr m]
  | RawVarsApply m (RawExpr m) [(TypeVarName, m)]
  | RawContextApply m (m, RawExpr m) [(ArgName, m)]
  | RawParen (RawExpr m)
  | RawMethod (RawExpr m) (RawExpr m) -- base methodValue
  | RawIfThenElse m (RawExpr m) (RawExpr m) (RawExpr m)
  | RawMatch m (RawExpr m) [(Pattern RawExpr m, RawExpr m)]
  | RawCase m (RawExpr m) [(Pattern RawExpr m, RawExpr m)]
  | RawList m [RawExpr m]
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- Expr after desugar
data Expr m
  = CExpr m Constant
  | Value m TypeName
  | Arg m ArgName
  | HoleExpr m Hole
  | TupleApply m (m, Expr m) (TupleArg Expr m)
  | VarApply m (Expr m) TypeVarName m
  deriving (Eq, Ord, Generic, Hashable, ToJSON)

-- Compiler Annotation
type CompAnnot em = em

data Guard em
  = IfGuard em
  | ElseGuard
  | NoGuard
  deriving (Eq, Ord, Generic, Hashable, ToJSON)

instance Functor Guard where
  fmap f (IfGuard e) = IfGuard (f e)
  fmap _ ElseGuard   = ElseGuard
  fmap _ NoGuard     = NoGuard

data DeclLHS e m = DeclLHS m (Pattern e m)
  deriving (Eq, Ord, Show, Generic, ToJSON)

data RawDecl m = RawDecl (DeclLHS RawExpr m) [RawStatement m] (Maybe (RawExpr m))
  deriving (Eq, Ord, Show, Generic, ToJSON)

newtype TypeDef m = TypeDef m
  deriving (Eq, Ord, Show, Generic, ToJSON)

data MultiTypeDef m = MultiTypeDef ClassName (H.HashMap TypeVarName Type) [m]
  deriving (Eq, Ord, Show, Generic, ToJSON)

type RawClassDef = ((TypeName, H.HashMap TypeVarName Type), ClassName)

type RawClassDecl = (ClassName, H.HashMap TypeVarName Type)

data Path = Relative String | Absolute String
  deriving (Eq, Ord, Show, Generic, ToJSON)

data RawStatement m
  = RawDeclStatement (RawDecl m)
  | MultiTypeDefStatement (MultiTypeDef m) [RawStatement m] Path
  | TypeDefStatement (TypeDef m) [RawStatement m]
  | RawClassDefStatement RawClassDef [RawStatement m] Path
  | RawClassDeclStatement RawClassDecl [RawStatement m] Path
  | RawAnnot (CompAnnot (RawExpr m)) [RawStatement m]
  | RawModule String [RawStatement m] Path
  deriving (Eq, Ord, Show, Generic, ToJSON)

type FileImport = String
type RawPrgm m = ([FileImport], [RawStatement m]) -- TODO: Include [Export]

type ObjArg m = (m, Maybe (Object m))
data ObjectBasis = FunctionObj | TypeObj | PatternObj | MatchObj
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)
-- |
-- Represents an input.
-- The current plan is to deprecate objM, objVars, objArgs, and objPath and replace them with objExpr. TODO Deprecate the rest.
data Object m = Object {
  deprecatedObjM    :: m,
  objBasis          :: ObjectBasis,
  deprecatedObjVars :: H.HashMap TypeVarName m,
  deprecatedObjArgs :: H.HashMap ArgName (ObjArg m),
  objDoc            :: Maybe String,
  deprecatedObjPath :: String
                       }
  deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)

data Arrow e m = Arrow m (Guard (e m)) (Maybe (e m)) -- m is result metadata
  deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)

type ObjectMapItem e m = (Object m, [CompAnnot (e m)], Maybe (Arrow e m))
type ObjectMap e m = [ObjectMapItem e m]
type Prgm e m = (ObjectMap e m, ClassGraph, [CompAnnot (e m)]) -- TODO: Include [Export]

instance Show m => Show (Expr m) where
  show (CExpr _ c) = show c
  show (Value _ name) = printf "Value %s" name
  show (Arg m name) = printf "Arg %s %s" (show m) name
  show (HoleExpr m hole) = printf "Hole %s %s" (show m) (show hole)
  show (TupleApply _ (_, baseExpr) arg) = printf "%s(%s)" baseExpr' arg'
    where
      baseExpr' = case baseExpr of
        Value _ funName -> funName
        TupleApply{}    -> show baseExpr
        _               -> printf "(%s)" (show baseExpr)
      arg' = case arg of
        TupleArgIO _ argName argVal -> argName ++ " = " ++ show argVal
        TupleArgI _ argName         -> argName
        TupleArgO _ argVal          -> show argVal
  show (VarApply _ baseExpr varName varVal) = printf "%s<%s%s>" baseExpr' varName (show varVal)
    where
      baseExpr' = case baseExpr of
        Value _ funName -> funName
        TupleApply{}    -> show baseExpr
        _               -> printf "<%s>" (show baseExpr)

instance Show e => Show (Guard e) where
  show (IfGuard expr) = "if (" ++ show expr ++ ")"
  show ElseGuard      = "else"
  show NoGuard        = ""

instance Show m => Show (Object m) where
  -- show (Object m basis vars args _ p) = printf "%s %s (%s) %s %s" (show basis) p (show m) maybeVarsString maybeArgsString
  show (Object _ basis vars args _ p) = printf "%s %s %s %s" (show basis) p maybeVarsString maybeArgsString
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

instance (Show m, Show (e m)) => Show (Arrow e m) where
  show (Arrow m guard maybeExpr) = concat $ [show guard, " -> ", show m, " "] ++ showExpr maybeExpr
    where
      showExpr (Just expr) = [" = ", show expr]
      showExpr Nothing     = []

class ExprClass e where
  -- | Returns the metadata for the top level of an expression
  getExprMeta ::  e m -> m

  -- | Returns the argname if the expression is an arg, else Nothing
  getExprArg :: e m -> Maybe ArgName

  -- | Returns the value at the base of an expression, if it exists
  maybeExprPath :: e m -> Maybe TypeName

  -- | Returns all arguments applied to a value
  exprAppliedArgs :: e m -> [TupleArg e m]

  -- | Returns all vars applied to a value
  exprAppliedVars :: e m -> H.HashMap TypeVarName m

instance ExprClass RawExpr where
  getExprMeta expr = case expr of
    RawCExpr m _          -> m
    RawValue m _          -> m
    RawHoleExpr m _       -> m
    RawTupleApply m _ _   -> m
    RawVarsApply m _ _    -> m
    RawContextApply m _ _ -> m
    RawParen e            -> getExprMeta e
    RawMethod e _         -> getExprMeta e
    RawIfThenElse m _ _ _ -> m
    RawMatch m _ _        -> m
    RawCase m _ _         -> m
    RawList m _           -> m

  getExprArg _ = Nothing

  maybeExprPath (RawValue _ n)               = Just n
  maybeExprPath (RawTupleApply _ (_, e) _)   = maybeExprPath e
  maybeExprPath (RawVarsApply _ e _)         = maybeExprPath e
  maybeExprPath (RawContextApply _ (_, e) _) = maybeExprPath e
  maybeExprPath (RawParen e)                 = maybeExprPath e
  maybeExprPath (RawMethod _ e)              = maybeExprPath e
  maybeExprPath _                            = Nothing

  exprAppliedArgs (RawValue _ _) = []
  exprAppliedArgs (RawTupleApply _ (_, be) args) = exprAppliedArgs be ++ args
  exprAppliedArgs (RawVarsApply _ e _) = exprAppliedArgs e
  exprAppliedArgs (RawContextApply _ (_, e) _) = exprAppliedArgs e
  exprAppliedArgs (RawParen e) = exprAppliedArgs e
  exprAppliedArgs (RawMethod _ e) = exprAppliedArgs e
  exprAppliedArgs _ = error "Unsupported RawExpr exprAppliedArgs"


  exprAppliedVars (RawValue _ _) = H.empty
  exprAppliedVars (RawTupleApply _ (_, be) _) = exprAppliedVars be
  exprAppliedVars (RawVarsApply _ e vars) = H.union (exprAppliedVars e) (H.fromList vars)
  exprAppliedVars (RawContextApply _ (_, e) _) = exprAppliedVars e
  exprAppliedVars (RawParen e) = exprAppliedVars e
  exprAppliedVars (RawMethod _ e) = exprAppliedVars e
  exprAppliedVars _ = error "Unsupported RawExpr exprAppliedVars"


instance ExprClass Expr where
  getExprMeta expr = case expr of
    CExpr m _        -> m
    Value m _        -> m
    Arg m _          -> m
    HoleExpr m _     -> m
    TupleApply m _ _ -> m
    VarApply m _ _ _ -> m

  getExprArg (Arg _ n) = Just n
  getExprArg _         = Nothing

  maybeExprPath (Value _ n)             = Just n
  maybeExprPath (TupleApply _ (_, e) _) = maybeExprPath e
  maybeExprPath (VarApply _ e _ _)      = maybeExprPath e
  maybeExprPath _                       = Nothing

  exprAppliedArgs (Value _ _) = []
  exprAppliedArgs (TupleApply _ (_, be) ae) = ae : exprAppliedArgs be
  exprAppliedArgs (VarApply _ e _ _) = exprAppliedArgs e
  exprAppliedArgs _ = error "Unsupported Expr exprAppliedArgs"

  exprAppliedVars (Value _ _) = H.empty
  exprAppliedVars (TupleApply _ (_, be) _) = exprAppliedVars be
  exprAppliedVars (VarApply _ e n m) = H.insert n m (exprAppliedVars e)
  exprAppliedVars _ = error "Unsupported Expr exprAppliedVars"


mapTupleArgValue :: (e1 m -> e2 m) -> TupleArg e1 m -> TupleArg e2 m
mapTupleArgValue _ (TupleArgI m n)    = TupleArgI m n
mapTupleArgValue f (TupleArgO m v)    = TupleArgO m (f v)
mapTupleArgValue f (TupleArgIO m n v) = TupleArgIO m n (f v)

mergeDoc :: Maybe String -> Maybe String -> Maybe String
mergeDoc (Just a) (Just b) = Just (a ++ " " ++ b)
mergeDoc (Just a) Nothing  = Just a
mergeDoc Nothing (Just b)  = Just b
mergeDoc _ _               = Nothing

mergeClassGraphs :: ClassGraph -> ClassGraph -> ClassGraph
mergeClassGraphs (ClassGraph classGraphA) (ClassGraph classGraphB) = ClassGraph $ mapToGraph $ H.unionWith mergeClasses (graphToMap classGraphA) (graphToMap classGraphB)
  where
    graphToMap (g, nodeFromVertex, _) = H.fromList $ map ((\classData@(_, className, _) -> (className, classData)) . nodeFromVertex) $ vertices g
    mapToGraph = graphFromEdges . H.elems
    mergeClasses (CGClass (sealedA, classVarsA, setA, docA, pathA), className, subClassNamesA) (CGClass (sealedB, classVarsB, setB, docB, _), _, subClassNamesB) = if sealedA == sealedB
          then (CGClass (sealedA, H.unionWith (unionTypes (ClassGraph classGraphA)) classVarsA classVarsB, setA ++ setB, mergeDoc docA docB, pathA), className, subClassNamesA ++ subClassNamesB)
          else error "Added to sealed class definition"
    mergeClasses node@(CGClass{}, _, _) (CGType{}, _, _) = node
    mergeClasses (CGType{}, _, _) node@(CGClass{}, _, _) = node
    mergeClasses (CGType, name, []) (CGType, _, []) = (CGType, name, [])
    mergeClasses cg1 cg2 = error $ printf "Unexpected input to mergeClassGraphs: \n\t%s \n\t%s" (show cg1) (show cg2)

mergePrgm :: Prgm e m -> Prgm e m -> Prgm e m
mergePrgm (objMap1, classGraph1, annots1) (objMap2, classGraph2, annots2) = (
  objMap1 ++ objMap2,
  mergeClassGraphs classGraph1 classGraph2,
  annots1 ++ annots2
                                                                           )

mergePrgms :: Foldable f => f (Prgm e m) -> Prgm e m
mergePrgms = foldr mergePrgm emptyPrgm
  where emptyPrgm = ([], ClassGraph $ graphFromEdges [], [])
