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

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}

module Syntax.Prgm where

import qualified Data.HashMap.Strict as H
import           Data.Hashable
import           Data.List           (intercalate)
import           GHC.Generics        (Generic)

import           Data.Aeson          (object)
import           Data.Aeson          hiding (Object)
import           Data.Aeson.Types    (ToJSON)
import           Data.Graph
import           Syntax.Types
import           Text.Megaparsec
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

data Pattern e m = Pattern (ExprObject e m) (Guard (e m))
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, ToJSONKey)

-- |
-- An argument applied in an expression.
-- TODO Consider replacing the TupleArgI ArgName with an Expr as a generalization. In that case, this ArgName would be equivalent to a Value. It could also include lenses.
data TupleArg e m
  = TupleArgI (Meta m) ArgName -- ^ An input arg. Can be thought of as a key without a value. Used only in input expressions.
  | TupleArgO (Meta m) (e m) -- ^ An output arg. Can be thought of as a value with an unknown key. Used only in output expressions before typechecking, as typechecking will determine the matching key.
  | TupleArgIO (Meta m) ArgName (e m) -- ^ An arg containing both the name and value.
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- | The type of hole (a gap where an expression should be). Used in inputs to ignore the expression and outputs.
data Hole
  = HoleActive (Maybe Name) -- ^ A hole such as _ or _name, where the name is optional and treated as an error
  | HoleUndefined -- ^ A hole with the keyword undefined that is an error only in runtime
  | HoleTodefine -- ^ A hole with the keyword todefine that is an error during runtime and commit time
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- Metadata for the Programs
type CodeRange = Maybe (SourcePos, SourcePos, String)
class (Eq m, Ord m) => MetaDat m where
  emptyMetaDat :: m
-- | Contains the type, position, and supplemental 'MetaDat' m
data Meta m = Meta Type CodeRange m
  deriving (Eq, Ord, Generic, Hashable, ToJSON)
-- | MetaDat contains supplemental metadata
instance MetaDat () where
  emptyMetaDat = ()

-- Expr before desugar
data RawExpr m
  = RawCExpr (Meta m) Constant
  | RawValue (Meta m) TypeName
  | RawHoleExpr (Meta m) Hole
  | RawTupleApply (Meta m) (Meta m, RawExpr m) [TupleArg RawExpr m]
  | RawVarsApply (Meta m) (RawExpr m) [(TypeVarName, Meta m)]
  | RawContextApply (Meta m) (Meta m, RawExpr m) [(ArgName, Meta m)]
  | RawParen (RawExpr m)
  | RawMethod (RawExpr m) (RawExpr m) -- base methodValue
  | RawIfThenElse (Meta m) (RawExpr m) (RawExpr m) (RawExpr m)
  | RawMatch (Meta m) (RawExpr m) [(Pattern RawExpr m, RawExpr m)]
  | RawCase (Meta m) (RawExpr m) [(Pattern RawExpr m, RawExpr m)]
  | RawList (Meta m) [RawExpr m]
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- Expr after desugar
data Expr m
  = CExpr (Meta m) Constant
  | Value (Meta m) TypeName
  | Arg (Meta m) ArgName
  | HoleExpr (Meta m) Hole
  | TupleApply (Meta m) (Meta m, Expr m) (TupleArg Expr m)
  | VarApply (Meta m) (Expr m) TypeVarName (Meta m)
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

data DeclLHS e m = DeclLHS (Meta m) (Pattern e m)
  deriving (Eq, Ord, Show, Generic, ToJSON)

data RawDecl e m = RawDecl (DeclLHS e m) (Maybe (e m))
  deriving (Eq, Ord, Show, Generic, ToJSON)

newtype TypeDef m = TypeDef (RawExpr m)
  deriving (Eq, Ord, Show, Generic, ToJSON)

data MultiTypeDef m = MultiTypeDef ClassName (H.HashMap TypeVarName Type) [RawExpr m]
  deriving (Eq, Ord, Show, Generic, ToJSON)

type RawClassDef m = (RawExpr m, ClassName)

type RawClassDecl = (ClassName, H.HashMap TypeVarName Type)

data Path = Relative String | Absolute String
  deriving (Eq, Ord, Show, Generic, ToJSON)

data RawStatement e m
  = RawDeclStatement (RawDecl e m)
  | MultiTypeDefStatement (MultiTypeDef m) Path
  | TypeDefStatement (TypeDef m)
  | RawClassDefStatement (RawClassDef m) Path
  | RawClassDeclStatement RawClassDecl Path
  | RawAnnot (CompAnnot (RawExpr m))
  | RawModule String Path
  deriving (Eq, Ord, Show, Generic, ToJSON)

data RawStatementTree e m = RawStatementTree (RawStatement e m) [RawStatementTree e m]
  deriving (Eq, Ord, Show, Generic, ToJSON)

type FileImport = String
type RawPrgm m = ([FileImport], [RawStatementTree RawExpr m]) -- TODO: Include [Export]

type ObjArg m = (Meta m, Maybe (Object m))
data ObjectBasis = FunctionObj | TypeObj | PatternObj | MatchObj
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)
-- |
-- Represents an input.
-- The current plan is to deprecate objM, objVars, objArgs, and objPath and replace them with objExpr. TODO Deprecate the rest (or replace with 'ExprObject').
data Object m = Object {
  deprecatedObjM    :: Meta m,
  objBasis          :: ObjectBasis,
  deprecatedObjVars :: H.HashMap TypeVarName (Meta m),
  deprecatedObjArgs :: H.HashMap ArgName (ObjArg m),
  objDoc            :: Maybe DocComment,
  deprecatedObjPath :: String
                       }
  deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)

-- |
-- Represents an input.
-- The current plan is to replace 'Object' with this, then rename this to standard Object
data ExprObject e m = ExprObject {
  eobjBasis :: ObjectBasis,
  eobjDoc   :: Maybe DocComment,
  eobjExpr  :: e m
                       }
  deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)

data Arrow e m = Arrow (Meta m) (Guard (e m)) (Maybe (e m)) -- m is result metadata
  deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)

type ObjectMapItem e m = (Object m, [CompAnnot (e m)], Maybe (Arrow e m))
type ObjectMap e m = [ObjectMapItem e m]
type Prgm e m = (ObjectMap e m, ClassGraph, [CompAnnot (e m)]) -- TODO: Include [Export]

type ExprObjectMapItem e m = (ExprObject e m, [CompAnnot (e m)], Maybe (Arrow e m))
type ExprObjectMap e m = [ExprObjectMapItem e m]
type ExprPrgm e m = (ExprObjectMap e m, ClassGraph, [CompAnnot (e m)]) -- TODO: Include [Export]


instance Show (Meta m) where
  show :: Meta m -> String
  show (Meta t _ _) = show t

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

instance (Show (e m)) => Show (ExprObject e m) where
  show (ExprObject basis _ e) = printf "%s %s" (show basis) (show e)

instance (Show m, Show (e m)) => Show (Arrow e m) where
  show (Arrow m guard maybeExpr) = concat $ [show guard, " -> ", show m, " "] ++ showExpr maybeExpr
    where
      showExpr (Just expr) = [" = ", show expr]
      showExpr Nothing     = []

instance Hashable SourcePos where
  hashWithSalt s (SourcePos name line col) = s `hashWithSalt` show name `hashWithSalt` unPos line `hashWithSalt` unPos col

instance ToJSON SourcePos where
  toJSON (SourcePos name line col) = object ["name".=name, "line".=unPos line, "col".=unPos col]


class ExprClass e where
  -- | Returns the metadata for the top level of an expression
  getExprMeta ::  e m -> Meta m

  -- | Returns the argname if the expression is an arg, else Nothing
  getExprArg :: e m -> Maybe ArgName

  -- | Returns the value at the base of an expression, if it exists
  maybeExprPath :: e m -> Maybe TypeName

  -- | Returns all arguments applied to a value
  exprAppliedArgs :: e m -> [TupleArg e m]

  -- | Returns all vars applied to a value
  exprAppliedVars :: e m -> H.HashMap TypeVarName (Meta m)

  -- | Returns all arguments located recursively in an expression
  exprArgs :: e m -> H.HashMap ArgName (Meta m)


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

  exprArgs RawCExpr{} = H.empty
  exprArgs RawHoleExpr{} = H.empty
  exprArgs RawValue{} = H.empty
  exprArgs (RawTupleApply _ (_, be) args) = H.union (exprArgs be) (H.unions $ map exprArg args)
    where
      exprArg (TupleArgIO _ _ e) = exprArgs e
      exprArg (TupleArgO _ e)    = exprArgs e
      exprArg (TupleArgI m n)    = H.singleton n m
  exprArgs (RawVarsApply _ e _) = exprArgs e
  exprArgs (RawContextApply _ (_, e) _) = exprArgs e
  exprArgs (RawParen e) = exprArgs e
  exprArgs (RawMethod be me) = H.union (exprArgs be) (exprArgs me)
  exprArgs e = error $ printf "Unsupported RawExpr exprArgs for %s" (show e)


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
  maybeExprPath (Arg _ n)               = Just n
  maybeExprPath (TupleApply _ (_, e) _) = maybeExprPath e
  maybeExprPath (VarApply _ e _ _)      = maybeExprPath e
  maybeExprPath _                       = Nothing

  exprAppliedArgs (Value _ _) = []
  exprAppliedArgs (Arg _ _) = []
  exprAppliedArgs (TupleApply _ (_, be) ae) = ae : exprAppliedArgs be
  exprAppliedArgs (VarApply _ e _ _) = exprAppliedArgs e
  exprAppliedArgs _ = error "Unsupported Expr exprAppliedArgs"

  exprAppliedVars (Value _ _) = H.empty
  exprAppliedVars (TupleApply _ (_, be) _) = exprAppliedVars be
  exprAppliedVars (VarApply _ e n m) = H.insert n m (exprAppliedVars e)
  exprAppliedVars _ = error "Unsupported Expr exprAppliedVars"

  exprArgs CExpr{} = H.empty
  exprArgs Value{} = H.empty
  exprArgs HoleExpr{} = H.empty
  exprArgs (Arg m n) = H.singleton n m
  exprArgs (TupleApply _ (_, be) arg) = H.union (exprArgs be) (exprArg arg)
    where
      exprArg (TupleArgIO _ _ e) = exprArgs e
      exprArg (TupleArgO _ e)    = exprArgs e
      exprArg (TupleArgI m n)    = H.singleton n m
  exprArgs (VarApply _ e _ _) = exprArgs e

mapMetaDat :: (m1 -> m2) -> Meta m1 -> Meta m2
mapMetaDat f (Meta t p md) = Meta t p (f md)

mapTupleArgValue :: (e1 m -> e2 m) -> TupleArg e1 m -> TupleArg e2 m
mapTupleArgValue _ (TupleArgI m n)    = TupleArgI m n
mapTupleArgValue f (TupleArgO m v)    = TupleArgO m (f v)
mapTupleArgValue f (TupleArgIO m n v) = TupleArgIO m n (f v)

constantPartialType :: Constant -> PartialType
constantPartialType CInt{}   = intLeaf
constantPartialType CFloat{} = floatLeaf
constantPartialType CStr{}   = strLeaf

constantType :: Constant -> Type
constantType = singletonType . constantPartialType

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

mergeExprPrgm :: ExprPrgm e m -> ExprPrgm e m -> ExprPrgm e m
mergeExprPrgm (objMap1, classGraph1, annots1) (objMap2, classGraph2, annots2) = (
  objMap1 ++ objMap2,
  mergeClassGraphs classGraph1 classGraph2,
  annots1 ++ annots2
                                                                           )

mergeExprPrgms :: Foldable f => f (ExprPrgm e m) -> ExprPrgm e m
mergeExprPrgms = foldr mergeExprPrgm emptyPrgm
  where emptyPrgm = ([], ClassGraph $ graphFromEdges [], [])
