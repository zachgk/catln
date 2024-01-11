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

import           Data.Hashable
import qualified Data.HashMap.Strict as H
import           GHC.Generics        (Generic)

import           Data.Aeson          hiding (Object)
import           Data.Bifunctor      (first)
import           Data.Graph
import qualified Data.HashSet        as S
import           Data.Maybe
import           Data.Void           (Void)
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

-- | The type of hole (a gap where an expression should be). Used in inputs to ignore the expression and outputs.
data Hole
  = HoleActive (Maybe Name) -- ^ A hole such as _ or _name, where the name is optional and treated as an error
  | HoleUndefined -- ^ A hole with the keyword undefined that is an error only in runtime
  | HoleTodefine -- ^ A hole with the keyword todefine that is an error during runtime and commit time
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- Metadata for the Programs
type ParseErrorRes = ParseErrorBundle String Void
type CodeRangeDat = (SourcePos, SourcePos, String)
type CodeRange = Maybe CodeRangeDat
class (Eq m, Ord m) => MetaDat m where
  emptyMetaDat :: m
-- | Contains the type, position, and supplemental 'MetaDat' m
data Meta m = Meta Type CodeRange m
  deriving (Eq, Ord, Generic, Hashable, ToJSON)
-- | MetaDat contains supplemental metadata
instance MetaDat () where
  emptyMetaDat = ()

emptyMetaT :: (MetaDat m) => Type -> Meta m
emptyMetaT t = Meta t Nothing emptyMetaDat

emptyMetaN :: (MetaDat m) => Meta m
emptyMetaN = emptyMetaT topType

getMetaType :: Meta m -> Type
getMetaType (Meta t _ _) = t

getMetaPos :: Meta m -> CodeRange
getMetaPos (Meta _ pos _) = pos

showCodeRange :: CodeRangeDat -> String
showCodeRange (start, end, _) = printf "%s:%d:%d-%d:%d" (sourceName start) (unPos $ sourceLine start) (unPos $ sourceColumn start) (unPos $ sourceLine end) (unPos $ sourceColumn end)

-- Expr after desugar
data Expr m
  = CExpr (Meta m) Constant
  | Value (Meta m) TypeName
  | HoleExpr (Meta m) Hole
  | AliasExpr (Expr m) (Expr m) -- ^ AliasExpr baseExpr aliasExpr
  | TupleApply (Meta m) (Meta m, Expr m) (ObjArr Expr m)
  | VarApply (Meta m) (Expr m) TypeVarName (Meta m)
  deriving (Eq, Ord, Generic, Hashable, ToJSON)

-- Compiler Annotation
type CompAnnot em = em
type FileImport = Expr ()


type ExprCond e m = Maybe (e m)

data GuardExpr e m = GuardExpr {
   rgeExpr  :: !(e m),
   rgeGuard :: !(ExprCond e m)
                                     }
  deriving (Eq, Ord, Generic, Hashable, ToJSON)
data ObjectBasis = FunctionObj | TypeObj | PatternObj | MatchObj | ArgObj
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)
data ObjArr e m = ObjArr {
  oaObj    :: !(Maybe (GuardExpr e m)),
  oaBasis  :: !ObjectBasis,
  oaDoc    :: !(Maybe DocComment),
  oaAnnots :: ![CompAnnot (e m)],
  oaArr    :: !(Maybe (GuardExpr e m), Meta m)
                               }
  deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)


type ObjectMap e m = [ObjArr e m]
type Prgm e m = (ObjectMap e m, ClassGraph, [CompAnnot (e m)]) -- TODO: Include [Export]


instance (Show m) => Show (Meta m) where
  show :: Meta m -> String
  show (Meta t _ _) = show t
  -- show (Meta t p d) = printf "(Meta %s %s (%s))" (show t) (show p) (show d)

instance Show m => Show (Expr m) where
  show (CExpr _ c) = show c
  show (Value _ name) = printf "Value %s" name
  show (HoleExpr m hole) = printf "Hole %s %s" (show m) (show hole)
  show (AliasExpr base alias) = printf "%s@%s" (show base) (show alias)
  show (TupleApply _ (_, baseExpr) arg) = printf "%s(%s)" baseExpr' (show arg)
    where
      baseExpr' = case baseExpr of
        Value _ funName -> funName
        TupleApply{}    -> show baseExpr
        VarApply{}      -> show baseExpr
        _               -> printf "(%s)" (show baseExpr)
  show (VarApply _ baseExpr varName varVal) = printf "%s[%s : %s]" baseExpr' (show varName) (show varVal)
    where
      baseExpr' = case baseExpr of
        Value _ funName -> funName
        TupleApply{}    -> show baseExpr
        VarApply{}      -> show baseExpr
        _               -> printf "(%s)" (show baseExpr)

instance (Show m, Show (e m)) => Show (ObjArr e m) where
  show ObjArr{oaObj, oaArr} = printf "%s%s" (showNoMaybe oaObj) showArr
    where
      showNoMaybe :: (Show a) => Maybe a -> String
      showNoMaybe (Just a) = show a
      showNoMaybe Nothing  = ""

      showArr :: String
      showArr = case oaArr of
        (Just e, m) | getMetaType m /= topType -> printf " -> %s = %s" (show m) (show e)
        (Just e, _) | isJust oaObj -> printf "= %s" (show e)
        (Just e, _) -> show e
        (Nothing, m) | getMetaType m /= topType -> printf " -> %s" (show m)
        (Nothing, _) -> ""

instance (Show m, Show (e m)) => Show (GuardExpr e m) where
  show (GuardExpr e g) = show e ++ showGuard
    where
      showGuard = case g of
        Nothing -> ""
        Just g' -> printf " if %s" (show g')

instance Hashable SourcePos where
  hashWithSalt s (SourcePos name line col) = s `hashWithSalt` show name `hashWithSalt` unPos line `hashWithSalt` unPos col

instance ToJSON SourcePos where
  toJSON (SourcePos name line col) = object ["name".=name, "line".=unPos line, "col".=unPos col]


class ExprClass e where
  -- | Returns the metadata for the top level of an expression
  getExprMeta ::  e m -> Meta m

  -- | Returns the value at the base of an expression, if it exists
  maybeExprPathM :: e m -> Maybe (TypeName, Meta m)

  -- | Returns all arguments applied to a value
  exprAppliedArgs :: (Show m, MetaDat m) => e m -> [ObjArr e m]

  -- | Returns all vars applied to a value
  exprAppliedOrdVars :: e m -> [(TypeVarName, Meta m)]

  -- | Returns all arguments located recursively in an expression
  exprVarArgs :: (MetaDat m, Show m) => e m -> H.HashMap TypeVarAux [(e m, Meta m)]


instance ExprClass Expr where
  getExprMeta expr = case expr of
    CExpr m _        -> m
    Value m _        -> m
    HoleExpr m _     -> m
    AliasExpr b _    -> getExprMeta b
    TupleApply m _ _ -> m
    VarApply m _ _ _ -> m

  maybeExprPathM (Value m n)             = Just (n, m)
  maybeExprPathM (TupleApply _ (_, e) _) = maybeExprPathM e
  maybeExprPathM (VarApply _ e _ _)      = maybeExprPathM e
  maybeExprPathM (AliasExpr b _)         = maybeExprPathM b
  maybeExprPathM _                       = Nothing

  exprAppliedArgs (Value _ _)               = []
  exprAppliedArgs CExpr{}                   = []
  exprAppliedArgs HoleExpr{}                = []
  exprAppliedArgs (TupleApply _ (_, be) ae) = ae : exprAppliedArgs be
  exprAppliedArgs (VarApply _ e _ _)        = exprAppliedArgs e
  exprAppliedArgs (AliasExpr b _)           = exprAppliedArgs b

  exprAppliedOrdVars (Value _ _) = []
  exprAppliedOrdVars (TupleApply _ (_, be) _) = exprAppliedOrdVars be
  exprAppliedOrdVars (VarApply _ e n m) = (n, m) : exprAppliedOrdVars e
  exprAppliedOrdVars (AliasExpr b _) = exprAppliedOrdVars b
  exprAppliedOrdVars _ = error "Unsupported Expr exprAppliedOrdVars"

  exprVarArgs CExpr{} = H.empty
  exprVarArgs Value{} = H.empty
  exprVarArgs HoleExpr{} = H.empty
  exprVarArgs (AliasExpr base n) = H.insertWith (++) (TVArg $ inExprSingleton n) [(n, getExprMeta base)] (exprVarArgs base)
  exprVarArgs (TupleApply _ (_, be) ObjArr{oaObj=Just (GuardExpr n _), oaArr=(Nothing, arrM)}) = H.insertWith (++) (TVArg $ inExprSingleton n) [(n, arrM)] (exprVarArgs be)
  exprVarArgs (TupleApply _ _ ObjArr{oaObj, oaArr=(Nothing, _)}) = error $ printf "Unexpected unhandled obj type in exprVarArgs: %s" (show oaObj)
  exprVarArgs (TupleApply _ (_, be) ObjArr{oaArr=(Just (GuardExpr e _), _)}) = H.unionWith (++) (exprVarArgs be) (exprVarArgs e)
  exprVarArgs (VarApply _ e n m) = H.unionWith (++) (exprVarArgs e) (H.singleton (TVVar n) [(Value (emptyMetaT $ partialToTypeSingleton n) (pkName n), m)])

class ObjArrClass oa where
  -- | See exprArgs
  oaVarArgs :: (ExprClass e, MetaDat m, Show m, Show (e m)) => oa e m -> H.HashMap TypeVarAux [(e m, Meta m)]

  getOaAnnots :: oa e m -> [CompAnnot (e m)]

instance ObjArrClass ObjArr where

  oaVarArgs oa = exprArg oa
    where
      exprArg ObjArr{oaArr=(Just (GuardExpr e Nothing), _)} = exprVarArgs e
      exprArg ObjArr{oaObj=Just (GuardExpr obj Nothing), oaArr=(_, m)} = H.singleton (TVArg $ inExprSingleton obj) [(obj, m)]
      exprArg _ = error $ printf "Invalid oa %s" (show oa)

  getOaAnnots = oaAnnots


emptyPrgm :: Prgm e m
emptyPrgm = ([], emptyClassGraph, [])

mkIOObjArr :: (MetaDat m, Show m) => Meta m -> ArgName -> Expr m -> ObjArr Expr m
mkIOObjArr m argName argVal = ObjArr (Just (GuardExpr (Value (emptyMetaT $ partialToTypeSingleton argName) (pkName argName)) Nothing)) ArgObj Nothing [] (Just (GuardExpr argVal Nothing), m)

mkIObjArr :: (MetaDat m, Show m) => Meta m -> ArgName -> ObjArr Expr m
mkIObjArr m argName = ObjArr (Just (GuardExpr (Value (emptyMetaT $ partialToTypeSingleton argName) (pkName argName)) Nothing)) ArgObj Nothing [] (Nothing, m)

mkOObjArr :: (MetaDat m, Show m) => Expr m -> ObjArr Expr m
mkOObjArr argVal = ObjArr Nothing ArgObj Nothing [] (Just (GuardExpr argVal Nothing), emptyMetaN )


mapMetaDat :: (m1 -> m2) -> Meta m1 -> Meta m2
mapMetaDat f (Meta t p md) = Meta t p (f md)

-- | Maps the objArr.oaArr.expr
mapTupleArgValue :: (e m -> e m) -> ObjArr e m -> ObjArr e m
mapTupleArgValue f oa@ObjArr{oaArr} = oa{oaArr=first (fmap fge) oaArr}
  where fge (GuardExpr e g) = GuardExpr (f e) g

constantPartialType :: Constant -> PartialType
constantPartialType CInt{}   = intLeaf
constantPartialType CFloat{} = floatLeaf
constantPartialType CStr{}   = strLeaf

constantType :: Constant -> Type
constantType = singletonType . constantPartialType

exprAppliedVars :: (ExprClass e) => e m -> H.HashMap TypeVarName (Meta m)
exprAppliedVars = H.fromList . exprAppliedOrdVars

getExprType :: (ExprClass e) => e m -> Type
getExprType = getMetaType . getExprMeta

inExprSingleton :: (ExprClass e, MetaDat m, Show m, Show (e m)) => e m -> ArgName
inExprSingleton e = maybe
  (partialKey $ makeAbsoluteName $ exprPath e) {pkArgs = H.keysSet $ exprAppliedArgsMap e, pkVars = H.keysSet $ exprAppliedVars e} partialToKey (maybeGetSingleton (getExprType e))

maybeExprPath :: (ExprClass e) => e m -> Maybe TypeName
maybeExprPath = fmap fst . maybeExprPathM

exprPathM :: (ExprClass e) => e m -> (TypeName, Meta m)
exprPathM = fromMaybe (error "No exprPath found") . maybeExprPathM

exprPath :: (ExprClass e) => e m -> TypeName
exprPath = fst . exprPathM

exprArgs :: (ExprClass e, MetaDat m, Show m) => e m -> H.HashMap ArgName [(e m, Meta m)]
exprArgs e = H.fromList $ mapMaybe aux $ H.toList $ exprVarArgs e
  where
    aux (TVArg a, ms) = Just (a, ms)
    aux (TVVar _, _)  = Nothing

exprAppliedArgsMap :: (ExprClass e, MetaDat m, Show m, Show (e m)) => e m -> H.HashMap ArgName (Meta m, Maybe (e m))
exprAppliedArgsMap = H.fromList . mapMaybe mapArg . exprAppliedArgs
  where
    mapArg ObjArr{oaObj=Just (GuardExpr oe _), oaArr=(arrExpr, oaM)} = Just (inExprSingleton oe, (oaM, fmap rgeExpr arrExpr))
    mapArg _ = Nothing

oaObjExpr :: (MetaDat m, ExprClass e, Show m, Show (e m)) => ObjArr e m -> e m
oaObjExpr ObjArr{oaObj=Just (GuardExpr e _)} = e
oaObjExpr oa = error $ printf "oaObjExpr with no input expression: %s" (show oa)


oaObjPath :: (MetaDat m, ExprClass e, Show m, Show (e m)) => ObjArr e m -> TypeName
oaObjPath = exprPath . oaObjExpr

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
    mergeClasses (CGClass (sealedA, classA, setA, docA), className, subClassNamesA) (CGClass (sealedB, classB, setB, docB), _, subClassNamesB) = if sealedA == sealedB
          then (CGClass (sealedA, mergeClassPartials classA classB, setA ++ setB, mergeDoc docA docB), className, subClassNamesA ++ subClassNamesB)
          else error "Added to sealed class definition"
    mergeClasses node@(CGClass{}, _, _) (CGType{}, _, _) = node
    mergeClasses (CGType{}, _, _) node@(CGClass{}, _, _) = node
    mergeClasses (CGType, name, []) (CGType, _, []) = (CGType, name, [])
    mergeClasses cg1 cg2 = error $ printf "Unexpected input to mergeClassGraphs: \n\t%s \n\t%s" (show cg1) (show cg2)

    mergeClassPartials clss@PartialType{ptVars=varsA} PartialType{ptVars=varsB} = clss{ptVars = H.unionWith (unionTypes (TypeEnv (ClassGraph classGraphA) S.empty)) varsA varsB}

mergeTypeEnv :: TypeEnv -> TypeEnv -> TypeEnv
mergeTypeEnv (TypeEnv cg1 n1) (TypeEnv cg2 n2) = TypeEnv (mergeClassGraphs cg1 cg2) (S.union n1 n2)

mergePrgm :: Prgm e m -> Prgm e m -> Prgm e m
mergePrgm (objMap1, classGraph1, annots1) (objMap2, classGraph2, annots2) = (
  objMap1 ++ objMap2,
  mergeClassGraphs classGraph1 classGraph2,
  annots1 ++ annots2
                                                                           )

mergePrgms :: Foldable f => f (Prgm e m) -> Prgm e m
mergePrgms = foldr mergePrgm emptyPrgm

-- | Gets all recursive sub expression objects from an expression's arguments. Helper for 'getRecursiveObjs'
getRecursiveObjsExpr :: (ExprClass e, MetaDat m, Show m) => e m -> [e m]
getRecursiveObjsExpr expr | isNothing (maybeExprPath expr) = []
getRecursiveObjsExpr expr = subObjects ++ recursedSubObjects
  where
    subObjects = filter (isJust . maybeExprPath) $ concatMap exprFromTupleArg $ exprAppliedArgs expr
    exprFromTupleArg ObjArr{oaObj, oaArr=(maybeOaObjExpr, _)} = map rgeExpr (maybeToList oaObj ++ maybeToList maybeOaObjExpr)
    recursedSubObjects = concatMap getRecursiveObjsExpr subObjects

-- | Gets an object and all sub-objects (recursively) from it's arguments
getRecursiveObjs :: (ExprClass e, Show m, Show (e m), MetaDat m) => ObjArr e m -> ObjectMap e m
getRecursiveObjs ObjArr{oaBasis} | oaBasis == MatchObj = []
getRecursiveObjs oa@ObjArr{oaBasis, oaObj=Just (GuardExpr objE _)} = oa : recursedSubObjects
  where
    recursedSubObjects = map toObjMapItem $ getRecursiveObjsExpr objE
    toObjMapItem e = ObjArr (Just (GuardExpr e Nothing)) oaBasis Nothing [] (Nothing, emptyMetaN)
getRecursiveObjs oa = error $ printf "getRecursiveObjs with no input expression: %s" (show oa)

getAllObjArrNames :: (ExprClass e, MetaDat m, Show m) => ObjArr e m -> [Name]
getAllObjArrNames ObjArr{oaObj, oaArr=(oaArrExpr, _)} = maybe [] (getAllExprNames . rgeExpr) oaObj ++ maybe [] (getAllExprNames . rgeExpr) oaArrExpr

getAllExprNames :: (ExprClass e, MetaDat m, Show m) => e m -> [Name]
getAllExprNames e = maybeToList (maybeExprPath e) ++ concatMap getAllObjArrNames (exprAppliedArgs e)

mkTypeEnv :: (ExprClass e, Show m, Show (e m), MetaDat m) => Prgm e m -> TypeEnv
mkTypeEnv (objMap, classGraph, _) = TypeEnv classGraph typeNames
  where
    typeNames = S.fromList $ map makeAbsoluteName $ concatMap getAllObjArrNames objMap
