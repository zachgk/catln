--------------------------------------------------------------------
-- |
-- Module    :  Semantics.Prgm
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

module Semantics.Prgm where

import qualified Data.HashMap.Strict as H
import           Data.Hashable
import           Data.List           (intercalate)
import           GHC.Generics        (Generic)

import           Data.Aeson          hiding (Object)
import           Data.Graph
import           Data.Maybe
import           Semantics.Types
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

-- |
-- An argument applied in an expression.
-- TODO Consider replacing the TupleArgI ArgName with an Expr as a generalization. In that case, this ArgName would be equivalent to a Value. It could also include lenses.
data TupleArg e m
  = TupleArgI (Meta m) ArgName -- ^ An input arg. Can be thought of as a key without a value. Used only in input expressions.
  | TupleArgO (e m) -- ^ An output arg. Can be thought of as a value with an unknown key. Used only in output expressions before typechecking, as typechecking will determine the matching key.
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

emptyMetaN :: (MetaDat m) => Meta m
emptyMetaN = Meta TopType Nothing emptyMetaDat

getMetaType :: Meta m -> Type
getMetaType (Meta t _ _) = t

getMetaPos :: Meta m -> CodeRange
getMetaPos (Meta _ pos _) = pos

showCodeRange :: (SourcePos, SourcePos, String) -> String
showCodeRange (start, end, _) = printf "%s:%d:%d-%d:%d" (sourceName start) (unPos $ sourceLine start) (unPos $ sourceColumn start) (unPos $ sourceLine end) (unPos $ sourceColumn end)

-- Expr after desugar
data Expr m
  = CExpr (Meta m) Constant
  | Value (Meta m) TypeName
  | Arg (Meta m) ArgName
  | HoleExpr (Meta m) Hole
  | AliasExpr (Expr m) (Expr m) -- ^ AliasExpr baseExpr aliasExpr
  | TupleApply (Meta m) (Meta m, Expr m) (ObjArr Expr m)
  | VarApply (Meta m) (Expr m) TypeVarName (Meta m)
  deriving (Eq, Ord, Generic, Hashable, ToJSON)

-- Compiler Annotation
type CompAnnot em = em

type ObjArg e m = (Meta m, Maybe (Object e m))
data ObjectBasis = FunctionObj | TypeObj | PatternObj | MatchObj | ArgObj
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)
-- |
-- Represents an input.
-- The current plan is to deprecate objM, objVars, objArgs, and objPath and replace them with objExpr. TODO Deprecate the rest (or replace with 'ExprObject').
data Object e m = Object {
  deprecatedObjM    :: Meta m,
  objBasis          :: ObjectBasis,
  deprecatedObjVars :: H.HashMap TypeVarName (Meta m),
  deprecatedObjArgs :: H.HashMap ArgName (ObjArg e m),
  objDoc            :: Maybe DocComment,
  objDupExpr        :: e m,
  deprecatedObjPath :: String
                       }
  deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)

type ExprCond e m = Maybe (e m)
data Arrow e m = Arrow (Meta m) (ExprCond e m) (Maybe (e m)) -- m is result metadata
  deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)

type ObjectMapItem e m = (Object e m, [CompAnnot (e m)], Maybe (Arrow e m))
type ObjectMap e m = [ObjectMapItem e m]
type Prgm e m = (ObjectMap e m, ClassGraph, [CompAnnot (e m)]) -- TODO: Include [Export]

data GuardExpr e m = GuardExpr {
   rgeExpr  :: !(e m),
   rgeGuard :: !(ExprCond e m)
                                     }
  deriving (Eq, Ord, Generic, Hashable, ToJSON)
data ObjArr e m = ObjArr {
  oaObj    :: !(Maybe (GuardExpr e m)),
  oaBasis  :: !ObjectBasis,
  oaDoc    :: !(Maybe DocComment),
  oaAnnots :: ![CompAnnot (e m)],
  oaM      :: !(Meta m),
  oaArr    :: !(Maybe (GuardExpr e m))
                               }
  deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)


type ExprObjectMapItem e m = ObjArr e m
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
  show (AliasExpr base alias) = printf "%s@%s" (show base) (show alias)
  show (TupleApply _ (_, baseExpr) arg) = printf "%s(%s)" baseExpr' (show arg)
    where
      baseExpr' = case baseExpr of
        Value _ funName -> funName
        TupleApply{}    -> show baseExpr
        _               -> printf "(%s)" (show baseExpr)
  show (VarApply _ baseExpr varName varVal) = printf "%s[%s : %s]" baseExpr' varName (show varVal)
    where
      baseExpr' = case baseExpr of
        Value _ funName -> funName
        TupleApply{}    -> show baseExpr
        _               -> printf "<%s>" (show baseExpr)

instance (Show m, Show (e m)) => Show (ObjArr e m) where
  show ObjArr{oaObj, oaM, oaArr} = printf "%s%s%s%s" (showNoMaybe oaObj) showM showEquals (showNoMaybe oaArr)
    where
      showNoMaybe :: (Show a) => Maybe a -> String
      showNoMaybe (Just a) = show a
      showNoMaybe Nothing  = ""

      showM :: String
      showM = if getMetaType oaM /= TopType
        then printf " -> %s " (show oaM)
        else ""

      showEquals :: String
      showEquals = case (oaObj, oaArr) of
        (Just _, Just _) -> "= "
        _                -> ""

instance (Show m, Show (e m)) => Show (GuardExpr e m) where
  show (GuardExpr e g) = show e ++ showGuard
    where
      showGuard = case g of
        Nothing -> ""
        Just g' -> printf " if %s" (show g')

instance Show m => Show (Object e m) where
  -- show (Object m basis vars args _ p) = printf "%s %s (%s) %s %s" (show basis) p (show m) maybeVarsString maybeArgsString
  show (Object _ basis vars args _ _ p) = printf "%s %s %s %s" (show basis) p maybeVarsString maybeArgsString
    where
      showVar (varName, varVal) = printf "%s = %s" varName (show varVal)
      maybeVarsString :: String
      maybeVarsString = if H.size vars == 0
        then ""
        else printf "[%s]" (intercalate ", " $ map showVar $ H.toList vars)
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
  exprAppliedArgs :: (Show m) => e m -> [TupleArg e m]

  -- | Returns all vars applied to a value
  exprAppliedVars :: e m -> H.HashMap TypeVarName (Meta m)

  -- | Returns all arguments located recursively in an expression
  exprArgs :: (Show m) => e m -> H.HashMap ArgName [Meta m]


instance ExprClass Expr where
  getExprMeta expr = case expr of
    CExpr m _        -> m
    Value m _        -> m
    Arg m _          -> m
    HoleExpr m _     -> m
    AliasExpr b _    -> getExprMeta b
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
  exprAppliedArgs (TupleApply _ (_, be) ae) = toTupleArg ae : exprAppliedArgs be
  exprAppliedArgs (VarApply _ e _ _) = exprAppliedArgs e
  exprAppliedArgs _ = error "Unsupported Expr exprAppliedArgs"

  exprAppliedVars (Value _ _) = H.empty
  exprAppliedVars (Arg _ _) = H.empty
  exprAppliedVars (TupleApply _ (_, be) _) = exprAppliedVars be
  exprAppliedVars (VarApply _ e n m) = H.insert n m (exprAppliedVars e)
  exprAppliedVars _ = error "Unsupported Expr exprAppliedVars"

  exprArgs CExpr{} = H.empty
  exprArgs Value{} = H.empty
  exprArgs (Arg m n) = H.singleton n [m]
  exprArgs HoleExpr{} = H.empty
  exprArgs (AliasExpr base alias) = H.unionWith (++) (exprArgs base) (exprArgs alias)
  exprArgs (TupleApply _ (_, be) arg) = H.unionWith (++) (exprArgs be) (exprArg (toTupleArg arg))
    where
      exprArg (TupleArgIO _ _ e) = exprArgs e
      exprArg (TupleArgO e)      = exprArgs e
      exprArg (TupleArgI m n)    = H.singleton n [m]
  exprArgs (VarApply _ e _ _) = exprArgs e

-- | To deprecate as part of moving to ObjArr
toTupleArg :: (Show m) => ObjArr Expr m -> TupleArg Expr m
toTupleArg ObjArr{oaObj=Just (GuardExpr (Arg _ n) Nothing), oaM, oaArr=Just (GuardExpr arr Nothing)} = TupleArgIO oaM n arr
toTupleArg ObjArr{oaObj=Just (GuardExpr (Arg m n) Nothing), oaArr=Nothing} = TupleArgI m n
toTupleArg ObjArr{oaObj=Just (GuardExpr (Value _ n) Nothing), oaM, oaArr=Just (GuardExpr arr Nothing)} = TupleArgIO oaM n arr
toTupleArg ObjArr{oaObj=Just (GuardExpr (Value m n) Nothing), oaArr=Nothing} = TupleArgI m n
toTupleArg ObjArr{oaObj=Nothing, oaArr=Just (GuardExpr arr Nothing)} = TupleArgO arr
toTupleArg oa = error $ printf "Invalid toTupleArg: %s" (show oa)

fromTupleArg :: (MetaDat m, Show m) => TupleArg Expr m -> ObjArr Expr m
fromTupleArg (TupleArgIO m argName argVal) = ObjArr (Just (GuardExpr (Arg m argName) Nothing)) ArgObj Nothing [] emptyMetaN (Just (GuardExpr argVal Nothing))
fromTupleArg (TupleArgI m argName) = ObjArr (Just (GuardExpr (Arg m argName) Nothing)) ArgObj Nothing [] emptyMetaN Nothing
fromTupleArg (TupleArgO argVal) = ObjArr Nothing ArgObj Nothing [] emptyMetaN (Just (GuardExpr argVal Nothing))

mapMetaDat :: (m1 -> m2) -> Meta m1 -> Meta m2
mapMetaDat f (Meta t p md) = Meta t p (f md)

-- | Maps the objArr.oaArr.expr
mapTupleArgValue :: (e m -> e m) -> ObjArr e m -> ObjArr e m
mapTupleArgValue f oa@ObjArr{oaArr} = oa{oaArr=fmap (\(GuardExpr e g) -> GuardExpr (f e) g) oaArr}

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

-- | Gets all recursive sub expression objects from an expression's arguments. Helper for 'getRecursiveExprObjs'
getRecursiveExprObjsExpr :: (ExprClass e, Show m) => e m -> [e m]
getRecursiveExprObjsExpr expr = subObjects ++ recursedSubObjects
  where
    subObjects = filter (isJust . maybeExprPath) $ mapMaybe exprFromTupleArg $ exprAppliedArgs expr
    exprFromTupleArg (TupleArgO e)      = Just e
    exprFromTupleArg (TupleArgIO _ _ e) = Just e
    exprFromTupleArg TupleArgI{}        = Nothing
    recursedSubObjects = concatMap getRecursiveExprObjsExpr subObjects

-- | Gets an object and all sub-objects (recursively) from it's arguments
getRecursiveExprObjs :: (ExprClass e, Show m, Show (e m), MetaDat m) => ExprObjectMapItem e m -> ExprObjectMap e m
getRecursiveExprObjs ObjArr{oaBasis} | oaBasis == MatchObj = []
getRecursiveExprObjs oa@ObjArr{oaBasis, oaObj=Just (GuardExpr objE _)} = oa : recursedSubObjects
  where
    recursedSubObjects = map toObjMapItem $ getRecursiveExprObjsExpr objE
    -- toObjMapItem e = (ExprObject oaBasis Nothing e, [], Nothing)
    toObjMapItem e = ObjArr (Just (GuardExpr e Nothing)) oaBasis Nothing [] emptyMetaN Nothing
getRecursiveExprObjs oa = error $ printf "getRecursiveExprObjs with no input expression: %s" (show oa)
