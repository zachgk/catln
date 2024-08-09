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
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}

module Semantics.Prgm where

import           Data.Hashable
import qualified Data.HashMap.Strict as H
import           GHC.Generics        (Generic)

import           CtConstants
import           Data.Aeson          hiding (Object)
import           Data.Bifunctor      (first)
import           Data.Graph
import qualified Data.HashSet        as S
import           Data.Maybe
import           Data.UUID
import           Data.Void           (Void)
import           Semantics.TypeGraph (ReachesTree)
import           Semantics.Types
import           Text.Megaparsec
import           Text.Printf
import           Utils               (unionsWith)

newtype Import = Import String
  deriving (Eq, Ord, Show)

newtype Export = Export String
  deriving (Eq, Ord, Show)

data Constant
  = CInt Integer
  | CFloat Double
  | CStr String
  | CChar Char
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- | The type of hole (a gap where an expression should be). Used in inputs to ignore the expression and outputs.
data Hole
  = HoleActive (Maybe Name) -- ^ A hole such as _ or _name, where the name is optional and treated as an error
  | HoleUndefined -- ^ A hole with the keyword undefined that is an error only in runtime
  | HoleTodefine -- ^ A hole with the keyword todefine that is an error during runtime and commit time
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- Metadata for the Programs
type ParseErrorRes = ParseErrorBundle String Void
type CodeRangeDat = (SourcePos, SourcePos)
type CodeRange = Maybe CodeRangeDat
class (Eq m, Ord m) => MetaDat m where
  emptyMetaDat :: m
-- | Contains the type, position, and supplemental 'MetaDat' m
data Meta m = Meta {
  getMetaType :: Type,
  getMetaPos  :: CodeRange,
  getMetaID   :: UUID,
  getMetaDat  :: m
                   }
  deriving (Eq, Ord, Generic, ToJSON)
-- | MetaDat contains supplemental metadata
instance MetaDat () where
  emptyMetaDat = ()

instance Hashable m => Hashable (Meta m) where
  hashWithSalt s (Meta t _ _ d)      = s `hashWithSalt` t `hashWithSalt` d

instance MetaDat (Maybe ReachesTree) where
  emptyMetaDat = Nothing

emptyMetaT :: (MetaDat m) => Type -> Meta m
emptyMetaT t = Meta t Nothing nil emptyMetaDat

emptyMetaN :: (MetaDat m) => Meta m
emptyMetaN = emptyMetaT PTopType

mWithType :: Type -> Meta m -> Meta m
mWithType t m = m{getMetaType=t}

showCodeRange :: CodeRangeDat -> String
showCodeRange (start, end) = printf "%s:%d:%d-%d:%d" (sourceName start) (unPos $ sourceLine start) (unPos $ sourceColumn start) (unPos $ sourceLine end) (unPos $ sourceColumn end)

data EApp e m
  = EAppArg (ObjArr e m)
  | EAppVar TypeVarName (Meta m)
  | EAppSpread (e m)
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- Expr after desugar
data Expr m
  = CExpr (Meta m) Constant
  | Value (Meta m) TypeName
  | HoleExpr (Meta m) Hole
  | AliasExpr (Expr m) (Expr m) -- ^ AliasExpr baseExpr aliasExpr
  | EWhere (Meta m) (Expr m) (Expr m) -- ^ base cond
  | TupleApply (Meta m) (Meta m, Expr m) (EApp Expr m)
  deriving (Eq, Ord, Generic, Hashable, ToJSON)

-- Compiler Annotation
type CompAnnot em = em
type FileImport = Expr ()


type ExprCond e m = Maybe (e m)

data ObjectBasis = FunctionObj | TypeObj | PatternObj | MatchObj | ArgObj
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)
data ObjArr e m = ObjArr {
  oaObj    :: !(Maybe (e m)),
  oaBasis  :: !ObjectBasis,
  oaDoc    :: !(Maybe DocComment),
  oaAnnots :: ![CompAnnot (e m)],
  oaArr    :: !(Maybe (Maybe (e m), Meta m))
                               }
  deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)


type ObjectMap e m = [ObjArr e m]
type Prgm e m = (ObjectMap e m, ClassGraph, [CompAnnot (e m)]) -- TODO: Include [Export]


instance (Show m) => Show (Meta m) where
  show :: Meta m -> String
  show (Meta t _ _ _) = show t
  -- show (Meta t p d) = printf "(Meta %s %s (%s))" (show t) (show p) (show d)

instance Show m => Show (Expr m) where
  show (CExpr _ c) = show c
  show (Value _ name) = printf "Value %s" name
  show (HoleExpr m hole) = printf "Hole %s %s" (show m) (show hole)
  show (AliasExpr base alias) = printf "%s@%s" (show base) (show alias)
  show (EWhere _ base cond) = printf "%s | %s" (show base) (show cond)
  show (TupleApply _ (_, baseExpr) arg) = printf "%s%s" baseExpr' showArg
    where
      baseExpr' = case baseExpr of
        Value _ funName -> funName
        TupleApply{}    -> show baseExpr
        _               -> printf "(%s)" (show baseExpr)
      showArg :: String
      showArg = case arg of
        EAppArg a    -> printf "(%s)" (show a)
        EAppVar a v  -> printf "[%s : %s]" (show a) (show v)
        EAppSpread a -> printf "(..%s)" (show a)

instance (Show m, Show (e m)) => Show (ObjArr e m) where
  show ObjArr{oaObj, oaArr} = printf "%s%s" (showNoMaybe oaObj) showArr
    where
      showNoMaybe :: (Show a) => Maybe a -> String
      showNoMaybe (Just a) = show a
      showNoMaybe Nothing  = ""

      showArr :: String
      showArr = case oaArr of
        Just (Just e, m) | getMetaType m /= PTopType -> printf " -> %s = %s" (show m) (show e)
        Just (Just e, _) | isJust oaObj -> printf "= %s" (show e)
        Just (Just e, _) -> show e
        Just (Nothing, m) | getMetaType m /= PTopType -> printf " -> %s" (show m)
        Just (Nothing, _) -> ""
        Nothing -> ""

instance Hashable SourcePos where
  hashWithSalt s (SourcePos name line col) = s `hashWithSalt` show name `hashWithSalt` unPos line `hashWithSalt` unPos col

instance ToJSON SourcePos where
  toJSON (SourcePos name line col) = object ["name".=name, "line".=unPos line, "col".=unPos col]

type VarArgMap e m = H.HashMap TypeVarAux [(e m, Meta m)]
type ArgMetaMapWithSrc m = H.HashMap TypeVarAux ([(Expr m, Meta m)], Type)
class ExprClass e where
  -- | Returns the metadata for the top level of an expression
  getExprMeta ::  e m -> Meta m

  -- | Returns the metadata for the top level of an expression
  setExprMeta :: e m -> Meta m -> e m

  -- | Returns the value at the base of an expression, if it exists
  maybeExprPathM :: e m -> Maybe (TypeName, Meta m)

  -- | Returns all arguments applied to a value
  exprAppliedArgs :: (Show m, MetaDat m) => e m -> [ObjArr e m]

  -- | Returns all vars applied to a value
  exprAppliedOrdVars :: (Show m, MetaDat m) => e m -> [(TypeVarName, Meta m)]

  -- | Returns all arguments located recursively in an expression
  exprVarArgs :: (MetaDat m, Show m) => e m -> VarArgMap e m

  -- | The 'exprVarArgsWithSrc' is similar to the 'exprArgs' function.
  -- | It differs in that it accepts an additional partial type that is equivalent to the expression and it pull the corresponding parts of the partial matching the args in the expression
  exprVarArgsWithSrc :: (TypeGraph tg, MetaDat m, Show m) => TypeEnv tg -> e m -> PartialType -> ArgMetaMapWithSrc m

  mkValue :: (MetaDat m) => Name -> e m


instance ExprClass Expr where
  getExprMeta expr = case expr of
    CExpr m _        -> m
    Value m _        -> m
    HoleExpr m _     -> m
    AliasExpr b _    -> getExprMeta b
    TupleApply m _ _ -> m
    EWhere m _ _     -> m

  setExprMeta expr m' = case expr of
    CExpr _ c         -> CExpr m' c
    Value _ v         -> Value m' v
    HoleExpr _ v      -> HoleExpr m' v
    AliasExpr b a     -> AliasExpr (setExprMeta b m') a
    TupleApply _ b as -> TupleApply m' b as
    EWhere _ b c      -> EWhere m' b c

  maybeExprPathM (Value m n)             = Just (n, m)
  maybeExprPathM (TupleApply _ (_, e) _) = maybeExprPathM e
  maybeExprPathM (AliasExpr b _)         = maybeExprPathM b
  maybeExprPathM (EWhere _ b _)          = maybeExprPathM b
  maybeExprPathM _                       = Nothing

  exprAppliedArgs (Value _ _)               = []
  exprAppliedArgs CExpr{}                   = []
  exprAppliedArgs HoleExpr{}                = []
  exprAppliedArgs (TupleApply _ (_, be) (EAppArg ae)) = ae : exprAppliedArgs be
  exprAppliedArgs (TupleApply _ (_, be) EAppVar{}) = exprAppliedArgs be
  exprAppliedArgs (TupleApply _ (_, be) (EAppSpread ae)) = exprAppliedArgs ae ++ exprAppliedArgs be
  exprAppliedArgs (AliasExpr b _)           = exprAppliedArgs b
  exprAppliedArgs (EWhere _ b _)              = exprAppliedArgs b

  exprAppliedOrdVars (Value _ _)              = []
  exprAppliedOrdVars CExpr{}                  = []
  exprAppliedOrdVars HoleExpr{}               = []
  exprAppliedOrdVars (TupleApply _ (_, be) (EAppVar n m)) = (n, m) : exprAppliedOrdVars be
  exprAppliedOrdVars (TupleApply _ (_, be) _) = exprAppliedOrdVars be
  exprAppliedOrdVars (AliasExpr b _)          = exprAppliedOrdVars b
  exprAppliedOrdVars (EWhere _ b _)           = exprAppliedOrdVars b

  exprVarArgs CExpr{} = H.empty
  exprVarArgs Value{} = H.empty
  exprVarArgs HoleExpr{} = H.empty
  exprVarArgs (AliasExpr base n) = H.insertWith (++) (TVArg $ inExprSingleton n) [(n, getExprMeta base)] (exprVarArgs base)
  exprVarArgs (EWhere _ base _) = exprVarArgs base
  exprVarArgs (TupleApply _ (_, be) (EAppArg ObjArr{oaObj=Just n, oaArr=Just (Nothing, arrM)})) = H.insertWith (++) (TVArg $ inExprSingleton n) [(n, arrM)] (exprVarArgs be)
  exprVarArgs (TupleApply _ _ (EAppArg ObjArr{oaObj, oaArr=Just (Nothing, _)})) = error $ printf "Unexpected unhandled obj type in exprVarArgs: %s" (show oaObj)
  exprVarArgs (TupleApply _ (_, be) (EAppArg ObjArr{oaArr=Just (Just e, _)})) = H.unionWith (++) (exprVarArgs be) (exprVarArgs e)
  exprVarArgs (TupleApply _ (_, be) (EAppVar n m)) = H.insertWith (++) (TVVar n) [(Value (emptyMetaT $ partialToTypeSingleton n) (pkName n), m)] (exprVarArgs be)
  exprVarArgs (TupleApply _ _ (EAppArg ObjArr{oaObj, oaArr=Nothing})) = error $ printf "Not yet implemented: %s" (show oaObj)
  exprVarArgs (TupleApply _ (_, be) (EAppSpread arg)) = H.unionWith (++) (exprVarArgs arg) (exprVarArgs be)

  exprVarArgsWithSrc typeEnv expr src = aux expr
    where
      aux CExpr{} = H.empty
      aux Value{} = H.empty
      aux HoleExpr{} = H.empty
      aux (EWhere _ base cond) = case maybeExprPath cond of
        Just n | n == operatorHasArrow -> case (H.lookup (partialKey operatorArgL) condArgs, H.lookup (partialKey operatorArgR) condArgs) of
                  (Just (Just (_, Just l)), Just (Just (_, Just r))) -> case (getExprType $ exprPropagateTypes l, getExprType r) of
                    (lt@UnionType{}, rt) -> case typeGraphQuery typeEnv (fmap snd base') $ fromJust $ maybeGetSingleton lt of
                      [] -> H.empty
                      qr -> let fromEachArrow = fmap (\qrt -> fst $ intersectTypesWithVarEnv typeEnv (fmap snd base') qrt rt) qr
                                fromAllArrows = ([],) <$> unionsWith (unionTypes typeEnv) fromEachArrow
                             in H.unionWith merge base' fromAllArrows
                    (lt, rt) -> error $ printf "Found invalid types of l=%s and r=%s in ?->" (show lt) (show rt)
                  _ -> error $ printf "Could not find /l and /r in %s in ?->" (show condArgs)
        _ -> base'
        where
          base' = exprVarArgsWithSrc typeEnv base src
          condArgs = exprAppliedArgsMap cond
      aux (AliasExpr base n) = H.insertWith merge (TVArg $ inExprSingleton n) ([(n, getExprMeta base)], singletonType src) (exprVarArgsWithSrc typeEnv base src)
      aux (TupleApply _ (_, be) arg) = H.unionWith merge (exprVarArgsWithSrc typeEnv be src) (fromArg arg)
        where
          fromArg (EAppArg ObjArr{oaObj=Just obj, oaArr=Just (Just e, _)}) = case typeGetArg (inExprSingleton obj) src of
            Just (UnionType srcArg) -> mergeMaps $ map (exprVarArgsWithSrc typeEnv e) $ splitUnionType srcArg
            Just t@TopType{} -> (,t) <$> exprVarArgs e
            _ -> H.empty
          fromArg (EAppArg ObjArr{oaArr=Just (Just e, _)}) = exprVarArgsWithSrc typeEnv e src
          fromArg (EAppArg ObjArr {oaObj=Just obj, oaArr=Just (Nothing, arrM)}) = case getMetaType arrM of
            (TypeVar v _) -> H.insertWith merge v ([(obj, arrM)], fromMaybe PTopType $ typeGetArg (inExprSingleton obj) src) mainArg
            _ -> mainArg
            where
              mainArg = H.singleton (TVArg $ inExprSingleton obj) ([(obj, arrM)], fromMaybe (getMetaType arrM) (typeGetArg (inExprSingleton obj) src))
          fromArg (EAppVar vn vm) = H.singleton (TVVar vn) ([(Value (emptyMetaT $ partialToTypeSingleton vn) (pkName vn), vm)], H.lookupDefault PTopType vn (ptVars src))
          fromArg (EAppSpread s) = exprVarArgsWithSrc typeEnv s src
          fromArg oa = error $ printf "Invalid oa %s" (show oa)

      merge (m1s, t1) (m2s, t2) = (m1s ++ m2s, intersectTypes typeEnv t1 t2)

      mergeMaps []     = H.empty
      mergeMaps (x:xs) = foldr (H.unionWith merge) x xs

  mkValue n = Value (emptyMetaT $ typeVal n) n

class ObjArrClass oa where
  -- | See exprArgs
  oaVarArgs :: (ExprClass e, MetaDat m, Show m, Show (e m)) => oa e m -> H.HashMap TypeVarAux [(e m, Meta m)]

  getOaAnnots :: oa e m -> [CompAnnot (e m)]

instance ObjArrClass ObjArr where

  oaVarArgs oa = exprArg oa
    where
      exprArg ObjArr{oaArr=Just (Just e, _)} = exprVarArgs e
      exprArg ObjArr{oaObj=Just obj, oaArr=Just (_, m)} = H.singleton (TVArg $ inExprSingleton obj) [(obj, m)]
      exprArg _ = error $ printf "Invalid oa %s" (show oa)

  getOaAnnots = oaAnnots


emptyPrgm :: Prgm e m
emptyPrgm = ([], emptyClassGraph, [])

mkIOObjArr :: (MetaDat m, Show m) => Meta m -> ArgName -> Expr m -> ObjArr Expr m
mkIOObjArr m argName argVal = ObjArr (Just (Value (emptyMetaT $ partialToTypeSingleton argName) (pkName argName))) ArgObj Nothing [] (Just (Just argVal, m))

mkIObjArr :: (MetaDat m, Show m) => Meta m -> ArgName -> ObjArr Expr m
mkIObjArr m argName = ObjArr (Just (Value (emptyMetaT $ partialToTypeSingleton argName) (pkName argName))) ArgObj Nothing [] (Just (Nothing, m))

mkOObjArr :: (MetaDat m, Show m) => Expr m -> ObjArr Expr m
mkOObjArr argVal = ObjArr Nothing ArgObj Nothing [] (Just (Just argVal, emptyMetaN ))


mapMetaDat :: (m1 -> m2) -> Meta m1 -> Meta m2
mapMetaDat f m@Meta{getMetaDat} = m{getMetaDat=f getMetaDat}

-- | Maps the objArr.oaArr.expr
mapTupleArgValue :: (e m -> e m) -> ObjArr e m -> ObjArr e m
mapTupleArgValue f oa@ObjArr{oaArr} = oa{oaArr=fmap (first (fmap f)) oaArr}

constantPartialType :: Constant -> PartialType
constantPartialType CInt{}   = intLeaf
constantPartialType CFloat{} = floatLeaf
constantPartialType CStr{}   = strLeaf
constantPartialType CChar{}  = charLeaf

constantType :: Constant -> Type
constantType = singletonType . constantPartialType

exprAppliedVars :: (ExprClass e, Show m, MetaDat m) => e m -> H.HashMap TypeVarName (Meta m)
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

exprAppliedArgsMap :: (ExprClass e, MetaDat m, Show m, Show (e m)) => e m -> H.HashMap ArgName (Maybe (Meta m, Maybe (e m)))
exprAppliedArgsMap = H.fromList . mapMaybe mapArg . exprAppliedArgs
  where
    mapArg ObjArr{oaObj=Just oe, oaArr=Just (arrExpr, oaM)} = Just (inExprSingleton oe, Just (oaM, arrExpr))
    mapArg ObjArr{oaObj=Just oe, oaArr=Nothing} = Just (inExprSingleton oe, Nothing)
    mapArg _ = Nothing

oaObjExpr :: (MetaDat m, ExprClass e, Show m, Show (e m)) => ObjArr e m -> e m
oaObjExpr ObjArr{oaObj=Just e} = e
oaObjExpr oa = error $ printf "oaObjExpr with no input expression: %s" (show oa)


oaObjPath :: (MetaDat m, ExprClass e, Show m, Show (e m)) => ObjArr e m -> TypeName
oaObjPath = exprPath . oaObjExpr

getOaArrExpr :: (MetaDat m, ExprClass e, Show m, Show (e m)) => ObjArr e m -> e m
getOaArrExpr ObjArr{oaArr=Just (Just e, _)} = e
getOaArrExpr oa = error $ printf "oaArrExpr with no output expression: %s" (show oa)

getOaArrM :: (MetaDat m, ExprClass e, Show m, Show (e m)) => ObjArr e m -> Meta m
getOaArrM ObjArr{oaArr=Just (_, m)} = m
getOaArrM oa = error $ printf "getOaArrM with no output meta: %s" (show oa)

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

    mergeClassPartials clss@PartialType{ptVars=varsA} PartialType{ptVars=varsB} = clss{ptVars = H.unionWith (unionTypes (TypeEnv (ClassGraph classGraphA) EmptyTypeGraph S.empty)) varsA varsB}

mergeTypeEnv :: (TypeGraph tg) => TypeEnv tg -> TypeEnv tg -> TypeEnv tg
mergeTypeEnv (TypeEnv cg1 tg1 n1) (TypeEnv cg2 tg2 n2) = TypeEnv (mergeClassGraphs cg1 cg2) (typeGraphMerge tg1 tg2) (S.union n1 n2)

mergePrgm :: Prgm e m -> Prgm e m -> Prgm e m
mergePrgm (objMap1, classGraph1, annots1) (objMap2, classGraph2, annots2) = (
  objMap1 ++ objMap2,
  mergeClassGraphs classGraph1 classGraph2,
  annots1 ++ annots2
                                                                           )

mergePrgms :: Foldable f => f (Prgm e m) -> Prgm e m
mergePrgms = foldr mergePrgm emptyPrgm

varNamesWithPrefix :: Name -> [Name]
varNamesWithPrefix n = [n, makeAbsoluteName ('$':tail n)]

-- | Gets all recursive sub expression objects from an expression's arguments. Helper for 'getRecursiveObjs'
getRecursiveObjsExpr :: (ExprClass e, MetaDat m, Show (e m), Show m) => e m -> [e m]
getRecursiveObjsExpr expr | isNothing (maybeExprPath expr) = []
getRecursiveObjsExpr expr = concat [varSubObjects, argSubObjects, recursedSubObjects]
  where
    varSubObjects = map mkValue $ concatMap (varNamesWithPrefix . pkName . fst) $ exprAppliedOrdVars expr
    argSubObjects = filter (isJust . maybeExprPath) $ concatMap exprFromTupleArg $ exprAppliedArgs expr
    exprFromTupleArg ObjArr{oaObj, oaArr=Just (maybeOaObjExpr, _)} = maybeToList oaObj ++ maybeToList maybeOaObjExpr
    exprFromTupleArg ObjArr{oaArr=Nothing} = []
    recursedSubObjects = concatMap getRecursiveObjsExpr argSubObjects

-- | Gets an object and all sub-objects (recursively) from it's arguments
getRecursiveObjs :: (ExprClass e, Show m, Show (e m), MetaDat m) => ObjArr e m -> ObjectMap e m
getRecursiveObjs ObjArr{oaBasis} | oaBasis == MatchObj = []
getRecursiveObjs oa@ObjArr{oaBasis, oaObj=Just objE} = oa : recursedSubObjects
  where
    recursedSubObjects = map toObjMapItem $ getRecursiveObjsExpr objE
    toObjMapItem e = ObjArr (Just e) oaBasis Nothing [] Nothing
getRecursiveObjs oa = error $ printf "getRecursiveObjs with no input expression: %s" (show oa)

getAllObjArrNames :: (ExprClass e, MetaDat m, Show m, Show (e m)) => ObjArr e m -> [Name]
getAllObjArrNames ObjArr{oaObj, oaArr=Just (oaArrExpr, _)} = maybe [] getAllExprNames oaObj ++ maybe [] getAllExprNames oaArrExpr
getAllObjArrNames ObjArr{oaObj, oaArr=Nothing} = maybe [] getAllExprNames oaObj

getAllExprNames :: (ExprClass e, MetaDat m, Show m, Show (e m)) => e m -> [Name]
getAllExprNames e = maybeToList (maybeExprPath e) ++ concatMap getAllObjArrNames (exprAppliedArgs e) ++ concatMap fromOrdVar (exprAppliedOrdVars e)
  where
    fromOrdVar (v, _) = varNamesWithPrefix $ pkName v

-- | Updates the types based on the format as they are fixed for inputs (due to arrows this does not work for output expressions)
exprPropagateTypes :: (MetaDat m, Show m) => Expr m -> Expr m
exprPropagateTypes (CExpr m c) = CExpr (mWithType (constantType c) m) c
exprPropagateTypes (Value m n) | getMetaType m == PTopType = Value (mWithType t m) n
  where
    t = relTypeVal n
exprPropagateTypes (Value m n) = Value m n
exprPropagateTypes (HoleExpr m h) = HoleExpr m h
exprPropagateTypes (AliasExpr base alias) = AliasExpr base' alias'
  where
    base' = exprPropagateTypes base
    alias' = exprPropagateTypes alias
exprPropagateTypes (EWhere m base cond) = EWhere m' base' cond'
  where
    m' = mWithType (typeAddPreds (getExprType base') (PredsOne $ PredExpr $ fromJust $ maybeGetSingleton $ getExprType cond')) m
    base' = exprPropagateTypes base
    cond' = exprPropagateTypes cond
exprPropagateTypes mainExpr@(TupleApply m (bm, be) tupleApplyArgs) = do
  let be' = exprPropagateTypes be
  let bm' = mWithType (getMetaType $ getExprMeta be') bm
  case tupleApplyArgs of
      (EAppArg arg@ObjArr{oaObj=Just argObj, oaArr=Just (Just argVal, argM)}) -> do
        let argName = inExprSingleton argObj
        let argVal' = exprPropagateTypes argVal
        let tp' = typeSetArg argName (getExprType argVal') (getExprType be')
        let m' = mWithType tp' m
        TupleApply m' (bm', be') (EAppArg arg{oaArr=Just (Just argVal', argM)})
      (EAppArg ObjArr{oaObj=Just argObj, oaArr=Just (Nothing, argM)}) -> do
        let argName = inExprSingleton argObj
        let tp' = typeSetArg argName (getMetaType argM) (getExprType be')
        let m' = mWithType tp' m
        TupleApply m' (bm', be') (EAppArg $ mkIObjArr argM argName)
      (EAppVar vn vm) -> do
        let tp' = typeSetVar vn (getMetaType vm) (getExprType be')
        let m' = mWithType tp' m
        TupleApply m' (bm', be') (EAppVar vn vm)
      (EAppSpread a) -> do
        let m' = mWithType (spreadType H.empty $ getMetaType bm') m
        TupleApply m' (bm', be') (EAppSpread $ exprPropagateTypes a)
      _ -> error $ printf "Unexpected ObjArr in exprPropagateTypes (probably because arrow only ObjArr): %s" (show mainExpr)
