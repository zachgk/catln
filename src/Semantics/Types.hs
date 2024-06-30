-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines the 'Type' which are used to represent
-- Catln types. It also includes functions for the various set
-- operations implemented for types.
--------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Semantics.Types where

import           Data.Aeson
import           Data.Bifunctor      (Bifunctor (bimap, first))
import           Data.Either
import           Data.Graph          (graphFromEdges)
import           Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.List           (intercalate, partition)
import qualified Data.List           as L
import           Data.List.Split     (splitOn)
import           Data.Maybe
import           Data.Zip
import           GHC.Generics        (Generic)
import           Prelude             hiding (unzip)
import           Text.Printf
import           Utils

-- |The name is the basic type used for various kinds of names
type Name = String

-- | A NPath (name path) is used for names with namespaces, either absolutely referenced to with only part of it
data NPath = NPath {
  npathIsAbs :: Bool,
  npathPath  :: [String]
                   }
  deriving (Eq, Ord, Hashable, Generic, ToJSON)

type ArgName = PartialKey
type TypeVarName = PartialKey
type TypeName = Name
type ClassName = Name
type TypePropName = Name
type RelativeName = Name
type DocComment = String

-- | The name or tuple-type of a 'PartialType'.
-- This would be the name of the function, data type, or class.
-- Names which are PTypeName, PClassName refer to a particular type or class respectively
-- The PRelativeName refers to a suffix of some number of types or classes (or possibly both).
data PartialName
  = PTypeName TypeName
  | PClassName ClassName
  | PRelativeName RelativeName
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, ToJSONKey)

data PtArgMode
  = PtArgExact -- ^ matches only the exact args
  | PtArgAny -- ^ matches with any additional args
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- | A predicate applied to a type
data TypePredicate
  = PredExpr PartialType
  | PredClass PartialType -- A hasClass predicate
  | PredRel PartialType -- a isa predicate. Represented by val : classOrModuleOrType
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)
type TypePredicates = [TypePredicate]

data PartialKey = PartialKey {
  pkName :: Name,
  pkVars :: S.HashSet PartialKey,
  pkArgs :: S.HashSet PartialKey
  } deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)

-- |
-- A particle type describes a simple set of types.
-- It corresponds to what could be matched by a single object or data declaration.
-- The arguments are treated independently except for type variables and properties.
data PartialType = PartialType {
  ptName    :: TypeName,
  ptVars    :: H.HashMap TypeVarName Type,
  ptArgs    :: H.HashMap ArgName Type,
  ptPreds   :: TypePredicates,
  ptArgMode :: PtArgMode
  } deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)

-- | The non-name properties of a 'PartialType'
type PartialArgsOption = (H.HashMap TypeVarName Type, H.HashMap ArgName Type, TypePredicates, PtArgMode)

-- | An alternative format for many 'PartialType's which combine those that share the same name
type PartialLeafs = H.HashMap TypeName (S.HashSet PartialArgsOption)

-- | The basic format of a 'Type' in Catln and the several formats it comes in.
data Type
  = UnionType PartialLeafs -- ^ The main format, 'UnionType', is a union of 'PartialType's
  | TypeVar TypeVarAux TypeVarLoc -- ^ A type which refers to some variable in the surrounding context
  | TopType TypePredicates -- ^ A type which refers to any possible value or the universal set of values (passing the predicates)
  deriving (Eq, Ord, Generic, Hashable, ToJSON)

-- | Indicates whether a type var or arg is located internally to the object or externally.
-- | Internal is if it is defined in the type as an argument or variable.
-- | External is if it is from a parent object or outer context.
data TypeVarLoc
  = TVInt
  | TVExt -- ^ TODO: Begin using TVExt. Right now, only TVInt is used.
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- | A type because two kinds of type variables are supported
data TypeVarAux
  = TVVar TypeVarName -- ^ A type variable stored from the 'PartialType' ptVars
  | TVArg ArgName -- ^ A type variable referring to an arguments type
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, ToJSONKey)

-- | Whether the typeclass can be extended or not
type Sealed = Bool

-- |
-- The classGraph contains the possible classes and how they relate to existing types.
-- Each edge goes from a class to a constituent class/type.
-- Note that the graph form is an approximation because it fails to account for type variables, so using graph operations can be a fast sanity test but not a substitute for the appropriate type functions.
-- The keys in the graph are PartialName because the originally inserted values (before desugaring) may use RelativeNames
-- TODO: ClassGraph should be more granular. Can have class to only a certain object or based on type variables.
newtype ClassGraph = ClassGraph (GraphData CGNode PartialName)

data TypeEnv = TypeEnv ClassGraph (S.HashSet Name)
  deriving (Show)

-- | A class or type node within the 'ClassGraph'
data CGNode
  = CGClass (Sealed, PartialType, [Type], Maybe DocComment)
  | CGType
  deriving (Eq, Ord, Show)

-- |
-- The old format for a 'ClassGraph'.
-- It is still used as the JSON representation.
type ClassMap = (
    H.HashMap TypeName (S.HashSet ClassName),
    H.HashMap ClassName (Sealed, PartialType, [Type], Maybe String)
  )

-- | The type variables in the surrounding context that could be referred to by a 'TypeVar' 'TVVar'
type TypeVarEnv = H.HashMap TypeVarName Type

-- | The arguments in the surrounding context that could be referred to by a 'TypeVar' 'TVArg'
type TypeArgEnv = H.HashMap ArgName Type

-- | The arguments in the surrounding context that could be referred to by a 'TypeVar' 'TVArg'
-- TODO: Use this as an intermediate step to combine vars and args completely (i.e. to(String) not to[String])
--         Will need to deal with the issue that TVArg currently behaves locally.
--         It is used mostly for id functions so that arrowDestType can keep the same id
--         Will need to replace that with an alternative mechanism
--         Perhaps, can make two types of TypeVar - local and foreign. Foreign are outside of partial and local inside.
--         Substitute would then only affect foreign and various get operations would move references from local to foreign.
--         Will also need support for non-linear pattern matching.
type TypeVarArgEnv = H.HashMap TypeVarAux Type
type TypeIOVarArgEnv = H.HashMap TypeVarAux (Type, Type)

instance Show PartialType where
  show (PartialType ptName ptVars ptArgs ptPreds ptArgMode) = concat [ptName, showTypeVars ptVars, showArgs ptArgs, showPreds ptPreds, showPtArgMode]
    where
      showArg (argName, argVal) = show argName ++ "=" ++ show argVal
      showTypeVars vars | H.null vars = ""
      showTypeVars vars = printf "[%s]" (intercalate ", " $ map showArg $ H.toList vars)
      showArgs args | H.null args = ""
      showArgs args = printf "(%s)" (intercalate ", " $ map showArg $ H.toList args)
      showPreds preds | null preds = ""
      showPreds preds = printf "| %s" (intercalate ", " $ map show preds)
      showPtArgMode = case ptArgMode of
        PtArgExact -> ""
        PtArgAny   -> ".."

instance Show PartialKey where
  show pk = show $ partialToType pk

instance Show Type where
  show (TopType []) = "TopType"
  show (TopType [PredClass c]) = "∀" ++ show c
  show (TopType [PredRel c]) = "~" ++ show c
  show (TopType preds) = printf "(TopType | %s)" (intercalate ", " $ map show preds)
  show (TypeVar v _) = show v
  show (UnionType partials) = join $ map show $ splitUnionType partials
    where
      join []  = "∅"
      join [p] = p
      join ps  = "(" ++ intercalate " | " ps ++ ")"

instance Show ClassGraph where
  show (ClassGraph graphData) = show $ map fst3 $ graphToNodes graphData

-- | Converts the 'ClassGraph' into the equivalent 'ClassMap'
asClassMap :: ClassGraph -> ClassMap
asClassMap (ClassGraph graphData) = (typesToClass, classToTypes)
  where
    typesToClass = fmap S.fromList $ H.fromListWith (++) $ mapMaybe typeToClassFromPartial $ concatMap getNodeTypeToClass $ graphToNodes graphData
    typeToClassFromPartial (PTypeName t, cs) = Just (t, map fromPartialName cs)
    typeToClassFromPartial (PClassName{}, _) = Nothing
    typeToClassFromPartial (PRelativeName{}, _) = error "unexpected PRelativeName in asClassMap." -- Maybe should return Nothing?
    getNodeTypeToClass (CGClass{}, c, ts) = map (,[c]) ts
    getNodeTypeToClass _                  = []

    classToTypes = H.fromList $ map classToTypeFromPartial $ mapMaybe getNodeClassToType $ graphToNodes graphData
    classToTypeFromPartial (c, d) = (fromPartialName c, d)
    getNodeClassToType (CGClass d, n, _) = Just (n, d)
    getNodeClassToType _                 = Nothing

instance ToJSON ClassGraph where
  toJSON classGraph = toJSON $ asClassMap classGraph

mapTypePred :: (PartialType -> PartialType) -> TypePredicate -> TypePredicate
mapTypePred f (PredExpr p)  = PredExpr (f p)
mapTypePred f (PredClass p) = PredClass (f p)
mapTypePred f (PredRel p)   = PredRel (f p)

typeEnvNamesMatching :: TypeEnv -> PartialType -> [Type]
typeEnvNamesMatching (TypeEnv classGraph names) relPartial = tpNames ++ clsNames
  where
    relName = ptName relPartial
    tpNames = map (\n -> singletonType relPartial{ptName=n}) $ relativeNameFilter relName $ S.toList names
    clsNames = map (setArgMode H.empty (ptArgMode relPartial) . classPartial . partialVal) $ relativeNameFilter relName $ listClassNames classGraph

listClassNames :: ClassGraph -> [ClassName]
listClassNames (ClassGraph graphData) = map (fromPartialName . snd3) $ filter isClass $ graphToNodes graphData
  where
    isClass (CGClass{}, _, _) = True
    isClass (CGType{}, _, _)  = False

-- | Defines some of the standard types used elsewhere in the compiler as 'PartialType'
intLeaf, floatLeaf, trueLeaf, falseLeaf, strLeaf, charLeaf, ioLeaf :: PartialType
intLeaf = partialVal "/Data/Primitive/Integer"
floatLeaf = partialVal "/Data/Primitive/Float"
trueLeaf = partialVal "/Data/Primitive/True"
falseLeaf = partialVal "/Data/Primitive/False"
strLeaf = partialVal "/Data/String"
charLeaf = partialVal "/Data/Char"
ioLeaf = partialVal "/Catln/IO"

-- | Defines some of the standard types used elsewhere in the compiler as 'Type'
intType, floatType, trueType, falseType, boolType, strType, ioType :: Type
intType = singletonType intLeaf
floatType = singletonType floatLeaf
trueType = singletonType trueLeaf
falseType = singletonType falseLeaf
boolType = UnionType $ joinUnionType [trueLeaf, falseLeaf]
strType = singletonType strLeaf
ioType = singletonType ioLeaf

-- | The 'Type' containing all possible values, equivalent to the universal.
topType :: Type
topType = TopType []

-- | The 'Type' containing no possible values, equivalent to the empty set ∅.
-- It often corresponds to errors in the type inference process and indicates the error type.
bottomType :: Type
bottomType = UnionType H.empty

-- | Used to check if a type is equivalent to 'bottomType'.
-- It can be necessary because it is possible for non-compacted types (see 'compactType') to be bottom types but not equal to 'bottomType'.
isBottomType :: Type -> Bool
-- isBottomType t = compactType t == bottomType
isBottomType t = t == bottomType

containsBottomType :: Type -> Bool
containsBottomType t | isBottomType t = True
containsBottomType TopType{} = False
containsBottomType TypeVar{} = False
containsBottomType (UnionType leafs) = any containsBottomPartialType $ splitUnionType leafs

containsBottomPartialType :: PartialType -> Bool
containsBottomPartialType PartialType{ptArgs, ptVars} = any containsBottomType ptArgs || any containsBottomType ptVars

emptyClassGraph :: ClassGraph
emptyClassGraph = ClassGraph $ graphFromEdges []

ptVarArg :: PartialType -> TypeVarArgEnv
ptVarArg PartialType{ptArgs, ptVars} = joinVarArgEnv ptVars ptArgs

joinVarArgEnv :: H.HashMap TypeVarName v -> H.HashMap ArgName v -> H.HashMap TypeVarAux v
joinVarArgEnv venv aenv = H.fromList (venv' ++ aenv')
  where
    venv' = map (first TVVar) $ H.toList venv
    aenv' = map (first TVArg) $ H.toList aenv

splitVarArgEnv :: H.HashMap TypeVarAux v -> (H.HashMap TypeVarName v, H.HashMap ArgName v)
splitVarArgEnv vaenv = bimap H.fromList H.fromList $ partitionEithers $ map eitherVarArg $ H.toList vaenv
  where
    eitherVarArg (TVVar v, t) = Left (v, t)
    eitherVarArg (TVArg v, t) = Right (v, t)

makeAbsoluteName :: Name -> Name
makeAbsoluteName n@('/':_) = n
makeAbsoluteName n         = '/':n

makeAbsolutePk :: PartialKey -> PartialKey
makeAbsolutePk (PartialKey name vars args)= PartialKey (makeAbsoluteName name) (S.map makeAbsolutePk vars) (S.map makeAbsolutePk args)

instance Show NPath where
  show (NPath a n) = (if a then "/" else "") ++ intercalate "/" n

getPath :: String -> NPath
getPath name = case splitOn "/" name of
  ("":n) -> NPath True n
  n      -> NPath False n

relPathAddPrefix :: String -> NPath -> NPath
relPathAddPrefix _ p@NPath{npathIsAbs=True}      = p
relPathAddPrefix prefix (NPath False n) = NPath False (splitOn "/" prefix ++ n)

matchesNPath :: NPath -> NPath -> Bool
matchesNPath (NPath True rn) (NPath True n) = rn == n
matchesNPath (NPath False rn) (NPath _ n)   = rn `L.isSuffixOf` n
matchesNPath (NPath True _) (NPath False _) = False

relativeNameMatches :: RelativeName -> Name -> Bool
relativeNameMatches rn n = split rn `L.isSuffixOf` split n
  where split = splitOn "/"

relativeNameFilter :: RelativeName -> [Name] -> [Name]
relativeNameFilter rn = filter (relativeNameMatches rn)

-- | Takes a list of relative and abs names, and prunes the relative names describing an absolute one
relAbsNamePrune :: [TypeName] -> [TypeName]
relAbsNamePrune names = map show (absNames ++ filter matchesNoAbsName relNames)
  where
    names' = map getPath names
    (absNames, relNames) = partition npathIsAbs names'
    matchesNoAbsName n = not $ any (matchesNPath n) absNames

-- | Gets the name String from a partial name
fromPartialName :: PartialName -> Name
fromPartialName (PTypeName n)     = n
fromPartialName (PClassName n)    = n
fromPartialName (PRelativeName n) = n

-- | Used to convert a 'UnionType' into its components
splitUnionType :: PartialLeafs -> [PartialType]
splitUnionType partials = concatMap (\(k, vs) -> map (aux k) vs) $ H.toList $ fmap S.toList partials
  where aux name (vars, args, preds, argMode) = PartialType name vars args preds argMode

-- | Used to combine the component 'PartialType' to form a 'UnionType'
joinUnionType :: [PartialType] -> PartialLeafs
joinUnionType = foldr (\(PartialType pName pVars pArgs pPreds pArgMode) partials -> H.insertWith S.union pName (S.singleton (pVars, pArgs, pPreds, pArgMode)) partials) H.empty

-- | Used to convert a 'UnionType' into its components while keeping types with the same name together
splitUnionTypeByName :: PartialLeafs -> H.HashMap TypeName [PartialType]
splitUnionTypeByName = H.mapWithKey (\k vs -> map (aux k) (S.toList vs))
  where aux name (vars, args, preds, argMode) = PartialType name vars args preds argMode

-- | Used to combine the component 'PartialType' to form a 'UnionType' while keeping types with the same name together
joinUnionTypeByName :: H.HashMap TypeName [PartialType] -> PartialLeafs
joinUnionTypeByName leafs = H.map (S.fromList . map typeToArgOption) $ H.filter (not . null) leafs
  where typeToArgOption (PartialType _ pVars pArgs pPreds pArgMode) = (pVars, pArgs, pPreds, pArgMode)

-- | Helper to create a 'UnionType' consisting of a single 'PartialType'
singletonType :: PartialType -> Type
singletonType partial = UnionType $ joinUnionType [partial]

-- | Helper to create a 'PartialKey' for a value (no args, no vars)
partialKey :: Name -> PartialKey
partialKey n = PartialKey (makeAbsoluteName n) S.empty S.empty

partialToKey :: PartialType -> PartialKey
partialToKey PartialType{ptName, ptVars, ptArgs} = makeAbsolutePk $ PartialKey ptName (H.keysSet ptVars) (H.keysSet ptArgs)

partialToType :: PartialKey -> PartialType
partialToType PartialKey{pkName, pkVars, pkArgs} = (partialVal pkName){ptVars = asArgs pkVars, ptArgs = asArgs pkArgs}
  where
    asArgs = fmap (const topType) . S.toMap

partialToTypeSingleton :: PartialKey -> Type
partialToTypeSingleton = singletonType . partialToType

-- | Helper to create a 'PartialType' for a value (no args, no vars)
partialVal :: TypeName -> PartialType
partialVal n = PartialType n H.empty H.empty [] PtArgExact

-- | Helper to create a 'Type' for a value (no args, no vars)
typeVal :: TypeName -> Type
typeVal = singletonType . partialVal

-- | Helper to create a relative 'Type' for a value (no args, no vars)
relTypeVal :: TypeName -> Type
relTypeVal n = TopType [PredRel $ partialVal n]

classPartial :: PartialType -> Type
classPartial p = TopType [PredClass p]

maybeGetSingleton :: Type -> Maybe PartialType
maybeGetSingleton (UnionType leafs) = case splitUnionType leafs of
  [p] -> Just p
  _   -> Nothing
maybeGetSingleton _ = Nothing

maybeGetClassSingleton :: Type -> Maybe PartialType
maybeGetClassSingleton (TopType [PredClass c]) = Just c
maybeGetClassSingleton _                       = Nothing

maybeGetTypeName :: Type -> Maybe TypeName
maybeGetTypeName t | isJust (maybeGetSingleton t) = Just $ ptName $ fromJust $ maybeGetSingleton t
maybeGetTypeName (TopType [PredRel p]) = Just $ ptName p
maybeGetTypeName _ = Nothing

typeSetArg :: ArgName -> Type -> Type -> Type
typeSetArg argName argVal (UnionType leafs) = UnionType $ joinUnionType $ map aux $ splitUnionType leafs
  where
    aux p@PartialType{ptArgs} = p{ptArgs=H.insert argName argVal ptArgs}
typeSetArg argName argVal (TopType [PredRel p@PartialType{ptArgs}]) = TopType [PredRel p{ptArgs=H.insert argName argVal ptArgs}]
typeSetArg _ _ tp = error $ printf "Unimplemented typeSetArg for %s" (show tp)

typeSetVar :: TypeVarName -> Type -> Type -> Type
typeSetVar varName varVal (UnionType leafs) = UnionType $ joinUnionType $ map aux $ splitUnionType leafs
  where
    aux p@PartialType{ptVars} = p{ptVars=H.insert varName varVal ptVars}
typeSetVar varName varVal (TopType [PredRel p@PartialType{ptVars}]) = TopType [PredRel p{ptVars=H.insert varName varVal ptVars}]
typeSetVar _ _ tp = error $ printf "Unimplemented typeSetVar for %s" (show tp)

suffixLookup :: String -> [String] -> Maybe String
suffixLookup s (x:xs)
  | s == x = Just s
  | otherwise = if relativeNameMatches s x then Just x else suffixLookup s xs
suffixLookup _ [] = Nothing

suffixLookupInDict :: String -> H.HashMap String b -> Maybe b
suffixLookupInDict s dict = case suffixLookup s (H.keys dict) of
  Just k  -> H.lookup k dict
  Nothing -> Nothing

expandType :: TypeEnv -> TypeVarArgEnv -> Type -> Type
expandType _ _ t@UnionType{} = t
expandType typeEnv vaenv (TypeVar v _) = expandType typeEnv vaenv $ H.lookupDefault topType v vaenv
expandType _ _ (TopType []) = TopType []
expandType typeEnv vaenv (TopType preds) = intersectAllTypes typeEnv $ map expandPred preds
  where
    expandPred (PredClass clss) = expandClassPartial typeEnv vaenv clss
    expandPred (PredRel rel)    = expandRelPartial typeEnv vaenv rel
    expandPred (PredExpr _)     = undefined

expandClassPartial :: TypeEnv -> TypeVarArgEnv -> PartialType -> Type
expandClassPartial typeEnv@(TypeEnv (ClassGraph cg) _) vaenv PartialType{ptName, ptVars=classVarsP} = expanded
  where
    className = PClassName ptName
    expanded = case graphLookup className cg of
      Just (CGClass (_, PartialType{ptVars=classVarsDecl}, classTypes, _)) -> unionAllTypes typeEnv $ map mapClassType classTypes
        where
          classVars = H.unionWith (intersectTypesEnv typeEnv vaenv) classVarsP classVarsDecl
          mapClassType (TopType ps) = expandType typeEnv vaenv $ TopType ps
          mapClassType (TypeVar (TVVar t) _) = case H.lookup t classVars of
            Just v -> expandType typeEnv vaenv $ intersectTypesEnv typeEnv vaenv v (H.lookupDefault topType t classVars)
            Nothing -> error $ printf "Unknown var %s in expandPartial" (show t)
          mapClassType (TypeVar (TVArg t) _) = error $ printf "Arg %s found in expandPartial" (show t)
          mapClassType (UnionType p) = UnionType $ joinUnionType $ map mapClassPartial $ splitUnionType p
          mapClassPartial tp@PartialType{ptVars} = tp{ptVars=fmap (substituteVarsWithVarEnv classVars) ptVars}
      r -> error $ printf "Unknown class %s in expandPartial. Found %s" (show className) (show r)

expandRelPartial :: TypeEnv -> TypeVarArgEnv -> PartialType -> Type
expandRelPartial typeEnv vaenv relPartial = unionAllTypesWithEnv typeEnv vaenv (UnionType (joinUnionType fromArgs) : fromTypeEnv)
  where
    name = ptName relPartial
    fromTypeEnv = typeEnvNamesMatching typeEnv relPartial
    fromArgs = map (\n -> relPartial{ptName=n}) $ relativeNameFilter name $ map pkName $ H.keys $ snd $ splitVarArgEnv vaenv

isSubPredicateOfWithEnv :: TypeEnv -> TypeVarArgEnv -> TypePredicate -> TypePredicate -> Bool
isSubPredicateOfWithEnv typeEnv vaenv (PredExpr sub) (PredExpr super) = isSubPartialOfWithEnv typeEnv vaenv sub super
isSubPredicateOfWithEnv _ _ _ _ = undefined

-- | A private helper for 'isSubPartialOfWithEnv' that checks while ignore class expansions
isSubPartialOfWithEnv :: TypeEnv -> TypeVarArgEnv -> PartialType -> PartialType -> Bool
isSubPartialOfWithEnv _ _ PartialType{ptName=subName} PartialType{ptName=superName} | subName /= superName = False
isSubPartialOfWithEnv _ _ PartialType{ptArgs=subArgs, ptArgMode=subArgMode} PartialType{ptArgs=superArgs, ptArgMode=superArgMode} | subArgMode == PtArgExact && superArgMode == PtArgExact && H.keysSet subArgs /= H.keysSet superArgs = False
isSubPartialOfWithEnv _ _ PartialType{ptArgs=subArgs} PartialType{ptArgs=superArgs, ptArgMode=superArgMode} | superArgMode == PtArgExact && not (H.keysSet subArgs `isSubsetOf` H.keysSet superArgs) = False
isSubPartialOfWithEnv typeEnv vaenv sub@PartialType{ptVars=subVars, ptArgs=subArgs, ptPreds=subPreds} super@PartialType{ptVars=superVars, ptArgs=superArgs, ptPreds=superPreds} = hasAll subArgs superArgs && hasAllPreds && hasAll subVars superVars
  where
    vaenv' = substituteWithVarArgEnv vaenv <$> H.unionWith (intersectTypes typeEnv) (ptVarArg super) (ptVarArg sub)
    hasAll sb sp = and $ H.elems $ H.intersectionWith (isSubtypeOfWithEnv typeEnv vaenv') sb sp
    hasAllPreds = all (\subPred -> any (isSubPredicateOfWithEnv typeEnv vaenv' subPred) superPreds) subPreds

-- | Checks if one type contains another type. In set terminology, it is equivalent to subset or equal to ⊆.
isSubtypeOfWithEnv :: TypeEnv -> TypeVarArgEnv -> Type -> Type -> Bool
isSubtypeOfWithEnv _ _ _ (TopType []) = True
isSubtypeOfWithEnv _ _ t1 t2 | t1 == t2 = True
isSubtypeOfWithEnv typeEnv vaenv (TypeVar v _) t2 = case H.lookup v vaenv of
  Just t1 -> isSubtypeOfWithEnv typeEnv vaenv t1 t2
  Nothing -> error $ printf "isSubtypeOfWithEnv with unknown type var or arg %s" (show v)
isSubtypeOfWithEnv typeEnv vaenv t1 (TypeVar v _) = case H.lookup v vaenv of
  Just t2 -> isSubtypeOfWithEnv typeEnv vaenv t1 t2
  Nothing -> error $ printf "isSubtypeOfWithEnv with unknown type var or arg %s" (show v)
isSubtypeOfWithEnv _ _ (TopType []) t = t == topType
isSubtypeOfWithEnv typeEnv vaenv t1 t2@(TopType (_:_)) = isSubtypeOfWithEnv typeEnv vaenv t1 t2'
  where
    t2' = expandType typeEnv vaenv t2
isSubtypeOfWithEnv typeEnv vaenv t1@(TopType (_:_)) t2 = isSubtypeOfWithEnv typeEnv vaenv t1' t2
  where
    t1' = expandType typeEnv vaenv t1
isSubtypeOfWithEnv typeEnv vaenv (UnionType subPartials) (UnionType superPartials) = all isSubPartial $ splitUnionType subPartials
  where
    isSubPartial sub = any (isSubPartialOfWithEnv typeEnv vaenv sub) $ splitUnionType superPartials

-- | Checks if one type contains another type. In set terminology, it is equivalent to subset or equal to ⊆.
isSubtypeOf :: TypeEnv -> Type -> Type -> Bool
isSubtypeOf typeEnv = isSubtypeOfWithEnv typeEnv H.empty

-- | Checks if one type contains another type. In set terminology, it is equivalent to subset or equal to ⊆.
isSubtypePartialOf :: TypeEnv -> PartialType -> Type -> Bool
isSubtypePartialOf typeEnv subPartial = isSubtypeOf typeEnv (singletonType subPartial)

isEqType :: TypeEnv -> Type -> Type -> Bool
isEqType _ a b | a == b = True
isEqType typeEnv a b = isSubtypeOf typeEnv a b && isSubtypeOf typeEnv b a

isEqTypeWithEnv :: TypeEnv -> TypeVarArgEnv -> Type -> Type -> Bool
isEqTypeWithEnv _ _ a b | a == b = True
isEqTypeWithEnv typeEnv vaenv a b = isSubtypeOfWithEnv typeEnv vaenv a b && isSubtypeOfWithEnv typeEnv vaenv b a

-- |
-- Join partials by checking if one is a subset of another (redundant) and removing it.
-- TODO: This currently joins only with matching names. More matches could improve the effectiveness of the compaction, but slows down the code significantly
compactOverlapping :: TypeEnv -> PartialLeafs -> PartialLeafs
compactOverlapping typeEnv = joinUnionTypeByName . fmap ((aux . reverse) . aux) . splitUnionTypeByName
  where
    -- Tests each partial against all following partials to check if it is already handled by it
    aux [] = []
    aux (partial:rest) = if any (partialAlreadyCovered partial) rest
      then aux rest
      else partial : aux rest

    -- checks if a partial is covered by the candidate from rest
    partialAlreadyCovered partial restPartial = isSubtypePartialOf typeEnv partial (singletonType restPartial)

-- |
-- Joins partials with only one difference between their args or vars. Then, it can join the two partials into one partial
-- TODO: Should check if preds are suitable for joining
compactJoinPartials :: TypeEnv -> PartialLeafs -> PartialLeafs
compactJoinPartials typeEnv partials = joinUnionType $ concat $ H.elems $ fmap joinMatchArgPartials $ H.fromListWith (++) $ map prepGroupJoinable $ splitUnionType partials
  where
    -- group partials by name, argSet, and varSet to check for joins
    prepGroupJoinable partial@PartialType{ptName, ptArgs, ptVars, ptArgMode} = ((ptName, H.keysSet ptArgs, H.keysSet ptVars, ptArgMode), [partial])

    -- Checks pairs of tuples for joins
    joinMatchArgPartials []     = []
    joinMatchArgPartials (p:ps) = joinMatchArgPartialsAux p ps []
    joinMatchArgPartialsAux curPartial [] tried = curPartial:joinMatchArgPartials tried
    joinMatchArgPartialsAux curPartial (toTry:toTrys) tried = case tryJoin curPartial toTry of
      Just joined -> joinMatchArgPartialsAux joined toTrys tried
      Nothing     -> joinMatchArgPartialsAux curPartial toTrys (toTry:tried)

    -- if two partials differ by only one arg or var, joins them else Nothing
    tryJoin (PartialType name1 vars1 args1 [] mode1) (PartialType _ vars2 args2 [] _) = if numDifferences args1 args2 + numDifferences vars1 vars2 == 1
      then Just $ PartialType name1 (joinMap vars1 vars2) (joinMap args1 args2) [] mode1
      else Nothing
    tryJoin _ _ = Nothing

    numDifferences m1 m2 = sum $ fromEnum <$> H.intersectionWith (/=) m1 m2
    joinMap = H.unionWith (unionTypes typeEnv)

-- | Processes partials that have a 'PredClass' predicate
compactPartialsWithClassPred :: TypeEnv -> TypeVarArgEnv -> PartialLeafs -> PartialLeafs
compactPartialsWithClassPred typeEnv vaenv partials = unionPartialLeafs $ map aux $ splitUnionType partials
  where
    aux partial@PartialType{ptPreds} = if null classPreds && null relPreds
      then joinUnionType [partial]
      else asLeafs $ intersectAllTypes typeEnv (singletonType partial' : map (expandRelPartial typeEnv vaenv) relPreds ++ map (expandClassPartial typeEnv vaenv) classPreds)
      where
        -- Take class preds out of partial
        (exprPreds, classPreds, relPreds) = splitPreds ptPreds
        partial' = partial{ptPreds = map PredExpr exprPreds}

        asLeafs (UnionType leafs) = leafs
        asLeafs t                 = asLeafs $ expandType typeEnv vaenv t

        splitPreds :: [TypePredicate] -> ([PartialType], [PartialType], [PartialType])
        splitPreds [] = ([], [], [])
        splitPreds (p:ps) = case p of
          PredExpr p'  -> (p':exprs', classes', rels')
          PredClass p' -> (exprs', p':classes', rels')
          PredRel p'   -> (exprs', classes', p':rels')
          where
            (exprs', classes', rels') = splitPreds ps

compactDuplicatePreds :: PartialLeafs -> PartialLeafs
compactDuplicatePreds partials = joinUnionType $ map aux $ splitUnionType partials
  where
    aux p@PartialType{ptPreds} = p{ptPreds = uniq ptPreds}

-- | Removes partials which contain a type variable that is the 'bottomType', because then the whole partial is a 'bottomType'.
compactBottomType :: PartialLeafs -> PartialLeafs
compactBottomType partials = joinUnionType $ mapMaybe aux $ splitUnionType partials
  where
    aux partial = if containsBottomPartialType partial
      then Nothing
      else Just partial

-- |
-- Used to simplify and reduce the size of a 'Type'.
-- It has several internal passes that apply various optimizations to a type.
-- TODO: This should merge type partials into class partials
compactType :: TypeEnv -> TypeVarArgEnv -> Type -> Type
compactType _ _ (TopType ps) = TopType (uniq ps)
compactType _ _ t@TypeVar{} = t
compactType typeEnv vaenv (UnionType partials) = UnionType $ (compactOverlapping typeEnv . compactJoinPartials typeEnv . compactPartialsWithClassPred typeEnv vaenv . compactBottomType) partials

unionPartialLeafs :: Foldable f => f PartialLeafs -> PartialLeafs
unionPartialLeafs = unionsWith S.union

-- | Takes the union of two types (∪)
unionTypesWithEnv :: TypeEnv -> TypeVarArgEnv -> Type -> Type -> Type
unionTypesWithEnv _ _ (TopType []) _ = topType
unionTypesWithEnv _ _ _ (TopType []) = topType
unionTypesWithEnv _ _ t1 t2 | isBottomType t2 = t1
unionTypesWithEnv _ _ t1 t2 | isBottomType t1 = t2
unionTypesWithEnv _ _ t1 t2 | t1 == t2 = t1
unionTypesWithEnv typeEnv vaenv t1@(TopType (_:_)) t2 = unionTypesWithEnv typeEnv vaenv (expandType typeEnv vaenv t1) t2
unionTypesWithEnv typeEnv vaenv t1 t2@(TopType (_:_)) = unionTypesWithEnv typeEnv vaenv t1 (expandType typeEnv vaenv t2)
unionTypesWithEnv typeEnv vaenv (TypeVar v _) t = case H.lookup v vaenv of
  Just v' -> unionTypesWithEnv typeEnv vaenv v' t
  Nothing -> error $ printf "Can't union unknown type vars %s with %s with env %s" (show t) (show v) (show $ H.keys vaenv)
unionTypesWithEnv typeEnv vaenv t v@TypeVar{} = unionTypesWithEnv typeEnv vaenv v t
unionTypesWithEnv _ _ (UnionType aPartials) (UnionType bPartials) = UnionType $ unionPartialLeafs [aPartials, bPartials]

unionTypes :: TypeEnv -> Type -> Type -> Type
unionTypes typeEnv = unionTypesWithEnv typeEnv H.empty

-- | Takes the 'unionTypes' of many types
unionAllTypesWithEnv :: Foldable f => TypeEnv -> TypeVarArgEnv -> f Type -> Type
unionAllTypesWithEnv typeEnv vaenv = foldr (unionTypesWithEnv typeEnv vaenv) bottomType

-- | Takes the 'unionTypes' of many types
unionAllTypes :: Foldable f => TypeEnv -> f Type -> Type
unionAllTypes typeEnv = foldr (unionTypes typeEnv) bottomType

-- | Takes the 'intersectTypes' of many types
intersectAllTypes :: Foldable f => TypeEnv -> f Type -> Type
intersectAllTypes _ types | null types = bottomType
intersectAllTypes typeEnv types = foldr1 (intersectTypes typeEnv) types

-- | A private helper for 'intersectPartialsBase' that intersects while ignore class expansions
intersectPartialsBase :: TypeEnv -> TypeVarArgEnv -> PartialType -> PartialType -> Maybe (TypeVarArgEnv, [PartialType])
intersectPartialsBase _ _ PartialType{ptName=aName} PartialType{ptName=bName} | aName /= bName = Nothing
intersectPartialsBase _ _ PartialType{ptArgs=aArgs, ptArgMode=aArgMode} PartialType{ptArgs=bArgs, ptArgMode=bArgMode} | aArgMode == PtArgExact && bArgMode == PtArgExact && H.keysSet aArgs /= H.keysSet bArgs = Nothing
intersectPartialsBase typeEnv vaenv (PartialType name' aVars aArgs aPreds aArgMode) (PartialType _ bVars bArgs bPreds bArgMode) = do
  (varsVaenvs, vars') <- unzip <$> intersectMap H.empty aVars bVars
  (argsVaenvs, args') <- unzip <$> intersectMap vaenv aArgs bArgs
  let venvs' = mergeAllVarEnvs typeEnv [fst $ splitVarArgEnv $ mergeAllVarEnvs typeEnv argsVaenvs, vars']
  let argMode' = case (aArgMode, bArgMode) of
        (PtArgAny, _)            -> PtArgAny
        (_, PtArgAny)            -> PtArgAny
        (PtArgExact, PtArgExact) -> PtArgExact
  let preds' = aPreds ++ bPreds
  return (mergeAllVarEnvs typeEnv varsVaenvs, [PartialType name' venvs' args' preds' argMode'])
  where
    -- intersectMap unions so that all typeVars from either a or b are kept
    intersectMap vev a b = traverse subValidate $ H.unionWith subUnion (fmap (vev,) a) (fmap (vev,) b)
    subUnion (aVaenv, a) (bVaenv, b) = intersectTypesWithVarEnv typeEnv (mergeAllVarEnvs typeEnv [aVaenv, bVaenv]) a b
    subValidate (vev, subTp) = if isBottomType subTp then Nothing else Just (vev, subTp)

intersectPartials :: TypeEnv -> TypeVarArgEnv -> PartialType -> PartialType -> (TypeVarArgEnv, [PartialType])
intersectPartials typeEnv vaenv a b = case intersectPartialsBase typeEnv vaenv a b of
  Just (vaenv', partials') -> (vaenv', partials')
  Nothing                  -> (vaenv, [])

-- |
-- Takes the intersection of two 'Type'.
-- It uses the 'TypeVarEnv' for type variable arguments and determines any possible changes to the surrounding 'TypeVarEnv'.
intersectTypesWithVarEnv :: TypeEnv -> TypeVarArgEnv -> Type -> Type -> (TypeVarArgEnv, Type)
intersectTypesWithVarEnv _ vaenv (TopType []) t = (vaenv, t)
intersectTypesWithVarEnv _ vaenv t (TopType []) = (vaenv, t)
intersectTypesWithVarEnv _ vaenv (TopType ps1) (TopType ps2) = (vaenv, TopType $ uniq (ps1 ++ ps2))
intersectTypesWithVarEnv _ vaenv t1 t2 | t1 == t2 = (vaenv, t1)
intersectTypesWithVarEnv typeEnv vaenv tv@(TypeVar v _) t = case (v, H.lookup v vaenv) of
  (TVArg{}, Nothing) -> error $ printf "Failed to intersect with unknown TVArg %s in vaenv %s" (show v) (show vaenv)
  (_, Just l) | isBottomType (intersectTypesEnv typeEnv vaenv l t) -> (vaenv, bottomType)
  _ -> (H.insertWith (intersectTypesEnv typeEnv vaenv) v t vaenv, tv)
intersectTypesWithVarEnv typeEnv vaenv t tv@TypeVar{} = intersectTypesWithVarEnv typeEnv vaenv tv t
intersectTypesWithVarEnv _ vaenv _ t | isBottomType t = (vaenv, t)
intersectTypesWithVarEnv _ vaenv t _ | isBottomType t = (vaenv, t)
intersectTypesWithVarEnv typeEnv vaenv t1@(UnionType partials) t2@TopType{} = case expandType typeEnv vaenv t2 of
  t2'@UnionType{} -> intersectTypesWithVarEnv typeEnv vaenv t1 t2'
  TypeVar{} -> undefined
  TopType topPreds -> (vaenv, compactType typeEnv vaenv $ UnionType $ joinUnionType $ map addPreds $ splitUnionType partials)
    where
      addPreds partial@PartialType{ptPreds} = partial{ptPreds = topPreds ++ ptPreds}
intersectTypesWithVarEnv typeEnv vaenv t1@TopType{} t2@UnionType{} = intersectTypesWithVarEnv typeEnv vaenv t2 t1
intersectTypesWithVarEnv typeEnv vaenv (UnionType aPartials) (UnionType bPartials) = (vaenv', type')
  where
    intersected = H.intersectionWith (\as bs -> [intersectPartials typeEnv vaenv a b | a <- as, b <- bs]) (splitUnionTypeByName aPartials) (splitUnionTypeByName bPartials)
    vaenv' = mergeAllVarEnvs typeEnv $ fmap fst $ concat $ H.elems intersected
    type' = UnionType $ joinUnionTypeByName $ fmap (concatMap snd) intersected

intersectTypesEnv :: TypeEnv -> TypeVarArgEnv -> Type -> Type -> Type
intersectTypesEnv typeEnv vaenv t1 t2 = snd $ intersectTypesWithVarEnv typeEnv vaenv t1 t2

-- | Takes the intersection of two 'Type' (∩).
intersectTypes :: TypeEnv -> Type -> Type -> Type
intersectTypes typeEnv = intersectTypesEnv typeEnv H.empty

-- | Takes the powerset of a 'Type' with the powerset of the arguments in the type.
powersetType :: TypeEnv -> TypeVarArgEnv -> Type -> Type
powersetType _ _ (TopType []) = topType
powersetType _ _ TopType{} = undefined
powersetType _ _ t@TypeVar{} = t
powersetType typeEnv vaenv (UnionType partials) = compactType typeEnv vaenv $ UnionType partials'
  where
    partials' = joinUnionType $ concatMap fromPartialType $ splitUnionType partials
    fromArgs args = powerset $ H.toList args
    fromPartialType (PartialType name vars args _ argMode) = [PartialType name vars (H.fromList a) [] argMode | a <- fromArgs args]

-- | Spreads a type by making it use PtArgAny mode
spreadType :: TypeVarArgEnv -> Type -> Type
spreadType vaenv = setArgMode vaenv PtArgAny

setArgMode :: TypeVarArgEnv -> PtArgMode -> Type -> Type
setArgMode _ mode (UnionType leafs) = UnionType $ joinUnionType $ map (\p -> p{ptArgMode=mode}) $ splitUnionType leafs
setArgMode vaenv mode (TypeVar v _) = setArgMode vaenv mode (fromJust $ H.lookup v vaenv)
setArgMode _ _ (TopType []) = TopType []
setArgMode _ mode (TopType [PredRel n]) = TopType [PredRel n{ptArgMode=mode}]
setArgMode _ _ t@(TopType [PredClass{}]) = t
setArgMode _ _ t = error $ printf "Unimplemented setArgMode for %s" (show t)

-- |
-- Combines two 'TypeVarEnv' to form the one applying the knowledge from both
-- It takes the union of all variables from either, and shared variables combine knowledge by intersection
mergeVarEnvs :: (Eq k, Hashable k) => TypeEnv -> H.HashMap k Type -> H.HashMap k Type -> H.HashMap k Type
mergeVarEnvs typeEnv = H.unionWith (intersectTypes typeEnv)


-- | Applies 'mergeVarEnvs' to many 'TypeVarEnv'
mergeAllVarEnvs :: (Foldable f, Eq k, Hashable k) => TypeEnv -> f (H.HashMap k Type) -> H.HashMap k Type
mergeAllVarEnvs typeEnv = foldr (mergeVarEnvs typeEnv) H.empty

-- | Replaces the type variables 'TVVar' in a 'Type' based on the variables in a provided 'TypeVarEnv'
substituteVarsWithVarEnv :: TypeVarEnv -> Type -> Type
substituteVarsWithVarEnv venv (UnionType partials) = UnionType $ joinUnionType $ map (substitutePartial venv) $ splitUnionType partials
  where substitutePartial pVenv partial@PartialType{ptVars, ptArgs, ptPreds} = partial{
          ptVars = fmap (substituteVarsWithVarEnv pVenv) ptVars,
          ptArgs = fmap (substituteVarsWithVarEnv ptVars') ptArgs,
          ptPreds = map (mapTypePred (substitutePartial ptVars')) ptPreds
                                                                        }
          where ptVars' = fmap (substituteVarsWithVarEnv venv) ptVars
substituteVarsWithVarEnv venv (TypeVar (TVVar v) TVInt) = fromMaybe topType (H.lookup v venv)
substituteVarsWithVarEnv _ t = t

-- | Replaces the type variables 'TVVar' in a 'Type'
substituteVars :: Type -> Type
substituteVars = substituteVarsWithVarEnv H.empty

-- | Replaces the argument type variables 'TVArg' in a 'Type' based on the variables in a provided 'TypeArgEnv'
substituteArgsWithArgEnv :: TypeArgEnv -> Type -> Type
substituteArgsWithArgEnv aenv (UnionType partials) = UnionType $ joinUnionType $ map substitutePartial $ splitUnionType partials
  where substitutePartial partial@PartialType{ptArgs} = partial{
          ptArgs = fmap (substituteArgsWithArgEnv aenv') ptArgs
                                                                 }
          where aenv' = H.union aenv ptArgs
substituteArgsWithArgEnv aenv (TypeVar (TVArg v) TVInt) = case H.lookup v aenv of
  Just v' -> v'
  Nothing -> error $ printf "Could not substitute unknown type arg %s. Only found args %s" (show v) (show aenv)
substituteArgsWithArgEnv _ t = t

substituteWithVarArgEnv :: TypeVarArgEnv -> Type -> Type
substituteWithVarArgEnv vaenv = substituteArgsWithArgEnv aenv . substituteVarsWithVarEnv venv
  where
    (venv, aenv) = splitVarArgEnv vaenv

typeGetAux :: TypeVarAux -> PartialType -> Maybe Type
typeGetAux (TVVar v) p = Just $ H.lookupDefault topType v $ ptVars p
typeGetAux (TVArg v) p = typeGetArg v p

-- | Gets an arg from a type while substituting the variables used in the types ptVars
typeGetArg :: ArgName -> PartialType -> Maybe Type
typeGetArg argName PartialType{ptArgs, ptVars, ptArgMode} = case H.lookup argName ptArgs of
  Nothing -> case ptArgMode of
    PtArgAny   -> Just topType
    PtArgExact -> Nothing
  Just arg -> Just $ case arg of
    t@TopType{} -> t
    TypeVar (TVVar v) TVInt -> H.lookupDefault topType v ptVars
    TypeVar (TVVar _) TVExt -> error $ printf "Not yet implemented"
    TypeVar (TVArg _) _ -> error $ printf "Not yet implemented"
    UnionType partialLeafs -> UnionType $ joinUnionType $ map substitutePartial $ splitUnionType partialLeafs
      where
        substitutePartial partial@PartialType{ptVars=vs} = partial{ptVars = fmap (substituteVarsWithVarEnv ptVars) vs}

-- | Gets an arg from a type while substituting the variables used in the types ptVars
typesGetArg :: TypeEnv -> ArgName -> Type -> Maybe Type
typesGetArg typeEnv argName (UnionType partialLeafs) = fmap (unionAllTypes typeEnv) $ mapM (typeGetArg argName) $ splitUnionType partialLeafs
typesGetArg _ _ _ = Nothing

typeSetAux :: TypeVarAux -> Type -> PartialType -> PartialType
typeSetAux (TVVar k) v p@PartialType{ptVars} = p{ptVars=H.insert k v ptVars}
typeSetAux (TVArg k) v p@PartialType{ptArgs} = p{ptArgs=H.insert k v ptArgs}

updateTypeProp :: TypeEnv -> TypeVarArgEnv -> Type -> TypeVarAux -> Type -> (TypeVarArgEnv, Type, Type)
updateTypeProp typeEnv vaenv superType propName subType = case (superType, subType) of
    (TopType [], _) -> (vaenv, topType, subType)
    (TypeVar v _, _) -> do
      let (vaenv', superType', subType') = updateTypeProp typeEnv vaenv (H.lookupDefault topType v vaenv) propName subType
      (H.insert v superType' vaenv', superType, subType')
    (TopType _, _) -> updateTypeProp typeEnv vaenv (expandType typeEnv vaenv superType) propName subType
    (UnionType supPartials, _) -> do
      let supPartialList = splitUnionType supPartials
      let intersectedPartials sup@PartialType{ptVars=supVars} sub = case typeGetAux propName sup of
            Just (TypeVar (TVVar v) TVInt) -> do
              let supVar = H.lookupDefault topType v supVars
              let newProp = intersectTypesEnv typeEnv vaenv supVar sub
              Just (sup{ptVars=H.insert v newProp supVars}, newProp)
            Just (TypeVar (TVVar _) TVExt) -> error $ printf "Not yet implemented"
            Just (TypeVar TVArg{} _) -> error $ printf "Not yet implemented"
            Just supProp -> do
              let newProp = intersectTypesEnv typeEnv vaenv supProp sub
              if isBottomType newProp
                then Nothing
                else Just (typeSetAux propName newProp sup, newProp)
            Nothing -> Nothing
      case subType of
        UnionType subPartials -> do
          let subPartialList = splitUnionType subPartials
          let (supPartialList', subPartialList') = unzip $ catMaybes $ [intersectedPartials sup (singletonType sub) | sup <- supPartialList, sub <- subPartialList]
          (vaenv, compactType typeEnv vaenv $ UnionType $ joinUnionType supPartialList', unionAllTypes typeEnv subPartialList')
        TypeVar v _ -> do
          -- Update vaenv.v with supPartials
          let tp' = intersectAllTypes typeEnv $ H.lookupDefault topType v vaenv : mapMaybe (typeGetAux propName) supPartialList
          let vaenv' = H.insert v tp' vaenv
          let tp'' = substituteWithVarArgEnv vaenv' tp'
          let superType' = compactType typeEnv vaenv $ UnionType $ joinUnionType $ map (\p -> typeSetAux propName (intersectTypes typeEnv (fromMaybe topType $ typeGetAux propName p) tp'') p) $ filter (isJust . typeGetAux propName) $ splitUnionType supPartials

          (vaenv', superType', subType)
        TopType [] -> do
          let sub' = case mapMaybe (typeGetAux propName) supPartialList of
                []       -> topType
                supProps -> compactType typeEnv vaenv $ unionAllTypes typeEnv supProps
          (vaenv, superType, sub')
        _ -> do
          let (supPartialList', subPartialList') = unzip $ catMaybes $ [intersectedPartials sup subType | sup <- supPartialList]
          (vaenv, compactType typeEnv vaenv $ UnionType $ joinUnionType supPartialList', compactType typeEnv vaenv $ unionAllTypes typeEnv subPartialList')
