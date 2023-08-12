--------------------------------------------------------------------
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
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Hashable
import           Data.List           (intercalate)
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

type ArgName = Name
type TypeVarName = Name
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
type TypePredicates = [PartialType]

-- |
-- A particle type describes a simple set of types.
-- It corresponds to what could be matched by a single object or data declaration.
-- The arguments are treated independently except for type variables and properties.
data PartialType = PartialType {
  ptName    :: PartialName,
  ptVars    :: H.HashMap TypeVarName Type,
  ptArgs    :: H.HashMap ArgName Type,
  ptPreds   :: TypePredicates,
  ptArgMode :: PtArgMode
  } deriving (Eq, Ord, Generic, Hashable, ToJSON)

-- | The non-name properties of a 'PartialType'
type PartialArgsOption = (H.HashMap TypeVarName Type, H.HashMap ArgName Type, [PartialType], PtArgMode)

-- | An alternative format for many 'PartialType's which combine those that share the same name
type PartialLeafs = (H.HashMap PartialName (S.HashSet PartialArgsOption))

-- | The basic format of a 'Type' in Catln and the several formats it comes in.
data Type
  = UnionType PartialLeafs -- ^ The main format, 'UnionType', is a union of 'PartialType's
  | TypeVar TypeVarAux -- ^ A type which refers to some variable in the surrounding context
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
  = TVVar TypeVarLoc TypeVarName -- ^ A type variable stored from the 'PartialType' ptVars
  | TVArg TypeVarLoc ArgName -- ^ A type variable referring to an arguments type
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- | Whether the typeclass can be extended or not
type Sealed = Bool

-- |
-- The classGraph contains the possible classes and how they relate to existing types.
-- Each edge goes from a class to a constituent class/type.
-- Note that the graph form is an approximation because it fails to account for type variables, so using graph operations can be a fast sanity test but not a substitute for the appropriate type functions.
-- The keys in the graph are PartialName because the originally inserted values (before desugaring) may use RelativeNames
-- TODO: ClassGraph should be more granular. Can have class to only a certain object or based on type variables.
newtype ClassGraph = ClassGraph (GraphData CGNode PartialName)

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

instance Show PartialType where
  show (PartialType ptName ptVars ptArgs ptPreds _) = concat [showName ptName, showTypeVars ptVars, showArgs ptArgs, showPreds ptPreds]
    where
      showName = fromPartialName
      showArg (argName, argVal) = argName ++ "=" ++ show argVal
      showTypeVars vars | H.null vars = ""
      showTypeVars vars = printf "[%s]" (intercalate ", " $ map showArg $ H.toList vars)
      showArgs args | H.null args = ""
      showArgs args = printf "(%s)" (intercalate ", " $ map showArg $ H.toList args)
      showPreds preds | null preds = ""
      showPreds preds = printf "| %s" (intercalate ", " $ map show preds)

instance Show Type where
  show (TopType []) = "TopType"
  show (TopType preds) = printf "(TopType | %s)" (intercalate ", " $ map show preds)
  show (TypeVar v) = show v
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

listClassGraphNames :: ClassGraph -> [PartialName]
listClassGraphNames (ClassGraph graphData) = map snd3 (graphToNodes graphData)

listClassNames :: ClassGraph -> [ClassName]
listClassNames (ClassGraph graphData) = map (fromPartialName . snd3) $ filter isClass $ graphToNodes graphData
  where
    isClass (CGClass{}, _, _) = True
    isClass (CGType{}, _, _)  = False

-- | Defines some of the standard types used elsewhere in the compiler as 'PartialType'
intLeaf, floatLeaf, trueLeaf, falseLeaf, strLeaf, ioLeaf :: PartialType
intLeaf = partialVal (PTypeName "/Data/Primitive/Integer")
floatLeaf = partialVal (PTypeName "/Data/Primitive/Float")
trueLeaf = partialVal (PTypeName "/Data/Primitive/True")
falseLeaf = partialVal (PTypeName "/Data/Primitive/False")
strLeaf = partialVal (PTypeName "/Data/String")
ioLeaf = partialVal (PTypeName "/Catln/IO")

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

ptVarArg :: PartialType -> TypeVarArgEnv
ptVarArg PartialType{ptArgs, ptVars} = joinVarArgEnv ptVars ptArgs

joinVarArgEnv :: TypeVarEnv -> TypeArgEnv -> TypeVarArgEnv
joinVarArgEnv venv aenv = H.fromList (venv' ++ aenv')
  where
    venv' = map (first (TVVar TVInt)) $ H.toList venv
    aenv' = map (first (TVArg TVInt)) $ H.toList aenv

splitVarArgEnv :: TypeVarArgEnv -> (TypeVarEnv, TypeArgEnv)
splitVarArgEnv vaenv = bimap H.fromList H.fromList $ partitionEithers $ map eitherVarArg $ H.toList vaenv
  where
    eitherVarArg (TVVar _ v, t) = Left (v, t)
    eitherVarArg (TVArg _ v, t) = Right (v, t)

relativeNameMatches :: RelativeName -> Name -> Bool
relativeNameMatches rn n = split rn `L.isSuffixOf` split n
  where split = splitOn "/"

relativeNameFilter :: RelativeName -> [Name] -> [Name]
relativeNameFilter rn = filter (relativeNameMatches rn)

relativePartialNameFilter :: RelativeName -> [PartialName] -> [PartialName]
relativePartialNameFilter rn = filter (relativeNameMatches rn . fromPartialName)

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
splitUnionTypeByName :: PartialLeafs -> H.HashMap PartialName [PartialType]
splitUnionTypeByName = H.mapWithKey (\k vs -> map (aux k) (S.toList vs))
  where aux name (vars, args, preds, argMode) = PartialType name vars args preds argMode

-- | Used to combine the component 'PartialType' to form a 'UnionType' while keeping types with the same name together
joinUnionTypeByName :: H.HashMap PartialName [PartialType] -> PartialLeafs
joinUnionTypeByName = H.map (S.fromList . map typeToArgOption)
  where typeToArgOption (PartialType _ pVars pArgs pPreds pArgMode) = (pVars, pArgs, pPreds, pArgMode)

-- | Helper to create a 'UnionType' consisting of a single 'PartialType'
singletonType :: PartialType -> Type
singletonType partial = UnionType $ joinUnionType [partial]

-- | Helper to create a 'PartialType' for a value (no args, no vars)
partialVal :: PartialName -> PartialType
partialVal n = PartialType n H.empty H.empty [] PtArgExact

-- | Helper to create a 'Type' for a value (no args, no vars)
typeVal :: PartialName -> Type
typeVal = singletonType . partialVal

suffixLookup :: String -> [String] -> Maybe String
suffixLookup s (x:xs)
  | s == x = Just s
  | otherwise = if relativeNameMatches s x then Just x else suffixLookup s xs
suffixLookup _ [] = Nothing

suffixLookupInDict :: String -> H.HashMap String b -> Maybe b
suffixLookupInDict s dict = case suffixLookup s (H.keys dict) of
  Just k  -> H.lookup k dict
  Nothing -> Nothing

-- |
-- Expands a class partial into a union of the types that make up that class (in the 'ClassGraph')
-- It also expands a relative into the matching types and classes
-- TODO: Should preserve type properties when expanding
expandPartial :: ClassGraph -> PartialType -> Type
expandPartial _ PartialType{ptName=PTypeName n} = error $ printf "Bad type name %s found in expandPartial" n
expandPartial _ p@PartialType{ptName=PClassName{}, ptArgs} | not (H.null ptArgs) = error $ printf "expandPartial class with args: %s" (show p)
expandPartial classGraph@(ClassGraph cg) PartialType{ptName=className@PClassName{}, ptVars=classVarsP} = expanded
  where
    expanded = case graphLookup className cg of
      Just (CGClass (_, PartialType{ptVars=classVarsDecl}, classTypes, _)) -> unionAllTypes classGraph $ map mapClassType classTypes
        where
          classVars = H.unionWith (intersectTypes classGraph) classVarsP classVarsDecl
          mapClassType (TopType ps) = TopType ps
          mapClassType (TypeVar (TVVar _ t)) = case H.lookup t classVars of
            Just v -> intersectTypes classGraph v (H.lookupDefault topType t classVars)
            Nothing -> error $ printf "Unknown var %s in expandPartial" t
          mapClassType (TypeVar (TVArg _ t)) = error $ printf "Arg %s found in expandPartial" t
          mapClassType (UnionType p) = UnionType $ joinUnionType $ map mapClassPartial $ splitUnionType p
          mapClassPartial tp@PartialType{ptVars} = tp{ptVars=fmap (substituteVarsWithVarEnv classVars) ptVars}
      r -> error $ printf "Unknown class %s in expandPartial. Found %s" (show className) (show r)
expandPartial classGraph relPartial@PartialType{ptName=PRelativeName relName} = UnionType $ joinUnionType $ map (\typeName -> relPartial{ptName=typeName}) $ relativePartialNameFilter relName (listClassGraphNames classGraph)

isSubPartialName :: PartialName -> PartialName -> Bool
isSubPartialName (PTypeName a) (PTypeName b) = a == b
isSubPartialName (PClassName a) (PClassName b) = a == b
isSubPartialName a (PRelativeName b) = relativeNameMatches b (fromPartialName a)
isSubPartialName _ _ = False

-- | A private helper for 'isSubPartialOfWithEnv' that checks while ignore class expansions
isSubPartialOfWithEnvBase :: ClassGraph -> TypeVarArgEnv -> PartialType -> PartialType -> Bool
isSubPartialOfWithEnvBase _ _ PartialType{ptName=subName} PartialType{ptName=superName} | not $ isSubPartialName subName superName = False
isSubPartialOfWithEnvBase _ _ PartialType{ptArgs=subArgs, ptArgMode=subArgMode} PartialType{ptArgs=superArgs} | subArgMode == PtArgExact && H.keysSet subArgs /= H.keysSet superArgs = False
isSubPartialOfWithEnvBase _ _ PartialType{ptArgs=subArgs} PartialType{ptArgs=superArgs, ptArgMode=superArgMode} | superArgMode == PtArgExact && not (H.keysSet subArgs `isSubsetOf` H.keysSet superArgs) = False
isSubPartialOfWithEnvBase classGraph vaenv sub@PartialType{ptVars=subVars, ptArgs=subArgs, ptPreds=subPreds} super@PartialType{ptVars=superVars, ptArgs=superArgs, ptPreds=superPreds} = hasAll subArgs superArgs && hasAllPreds && hasAll subVars superVars
  where
    vaenv' = substituteWithVarArgEnv vaenv <$> H.unionWith (intersectTypes classGraph) (ptVarArg super) (ptVarArg sub)
    hasAll sb sp = and $ H.elems $ H.intersectionWith (isSubtypeOfWithEnv classGraph vaenv') sb sp
    hasAllPreds = all (\subPred -> isSubtypeOfWithEnv classGraph vaenv' (singletonType subPred) (UnionType $ joinUnionType superPreds)) subPreds

-- | Checks if one type contains another type. In set terminology, it is equivalent to subset or equal to ⊆.
isSubPartialOfWithEnv :: ClassGraph -> TypeVarArgEnv -> PartialType -> PartialType -> Bool
isSubPartialOfWithEnv classGraph vaenv sub super | isSubPartialOfWithEnvBase classGraph vaenv sub super = True
isSubPartialOfWithEnv classGraph vaenv sub super@PartialType{ptName=PClassName{}} = isSubtypeOfWithEnv classGraph vaenv (singletonType sub) (expandPartial classGraph super)
isSubPartialOfWithEnv _ _ _ _ = False

-- | Checks if one type contains another type. In set terminology, it is equivalent to subset or equal to ⊆.
isSubtypeOfWithEnv :: ClassGraph -> TypeVarArgEnv -> Type -> Type -> Bool
isSubtypeOfWithEnv _ _ _ (TopType []) = True
isSubtypeOfWithEnv _ _ t1 t2 | t1 == t2 = True
isSubtypeOfWithEnv classGraph vaenv (TypeVar v) t2 = case H.lookup v vaenv of
  Just t1 -> isSubtypeOfWithEnv classGraph vaenv t1 t2
  Nothing -> error $ printf "isSubtypeOfWithEnv with unknown type var or arg %s" (show v)
isSubtypeOfWithEnv classGraph vaenv t1 (TypeVar v) = case H.lookup v vaenv of
  Just t2 -> isSubtypeOfWithEnv classGraph vaenv t1 t2
  Nothing -> error $ printf "isSubtypeOfWithEnv with unknown type var or arg %s" (show v)
isSubtypeOfWithEnv _ _ (TopType []) t = t == topType
isSubtypeOfWithEnv _ _ _ (TopType _) = undefined
isSubtypeOfWithEnv _ _ (TopType _) _ = undefined
isSubtypeOfWithEnv classGraph vaenv (UnionType subPartials) super@(UnionType superPartials) = all isSubPartial $ splitUnionType subPartials
  where
    isSubPartial sub | any (isSubPartialOfWithEnv classGraph vaenv sub) $ splitUnionType superPartials = True
    isSubPartial sub@PartialType{ptName=PClassName{}} | isSubtypeOfWithEnv classGraph vaenv (expandPartial classGraph sub) super = True
    isSubPartial _ = False

-- | Checks if one type contains another type. In set terminology, it is equivalent to subset or equal to ⊆.
isSubtypeOf :: ClassGraph -> Type -> Type -> Bool
isSubtypeOf classGraph = isSubtypeOfWithEnv classGraph H.empty

-- | Checks if one type contains another type. In set terminology, it is equivalent to subset or equal to ⊆.
isSubtypePartialOf :: ClassGraph -> PartialType -> Type -> Bool
isSubtypePartialOf classGraph subPartial = isSubtypeOf classGraph (singletonType subPartial)

isEqType :: ClassGraph -> Type -> Type -> Bool
isEqType _ a b | a == b = True
isEqType classGraph a b = isSubtypeOf classGraph a b && isSubtypeOf classGraph b a

isEqTypeWithEnv :: ClassGraph -> TypeVarArgEnv -> Type -> Type -> Bool
isEqTypeWithEnv _ _ a b | a == b = True
isEqTypeWithEnv classGraph vaenv a b = isSubtypeOfWithEnv classGraph vaenv a b && isSubtypeOfWithEnv classGraph vaenv b a

-- |
-- Join partials by checking if one is a subset of another (redundant) and removing it.
-- TODO: This currently joins only with matching names. More matches could improve the effectiveness of the compaction, but slows down the code significantly
compactOverlapping :: ClassGraph -> PartialLeafs -> PartialLeafs
compactOverlapping classGraph = joinUnionTypeByName . fmap (\ps -> aux ps ps) . splitUnionTypeByName
  where
    -- Tests each partial against all partials to check if it is already handled by it
    aux [] _ = []
    aux (partial:rest) allPartials = if any (partialAlreadyCovered partial) allPartials
      then aux rest allPartials
      else partial : aux rest allPartials

    -- checks if a partial is covered by the candidate allPartial
    -- it compares each partial to all others (including itself), so must ignore the self comparison case (partial == allPartial)
    partialAlreadyCovered partial allPartial = partial /= allPartial && isSubtypePartialOf classGraph partial (singletonType allPartial)

-- |
-- Joins partials with only one difference between their args or vars. Then, it can join the two partials into one partial
-- TODO: Should check if preds are suitable for joining
compactJoinPartials :: ClassGraph -> PartialLeafs -> PartialLeafs
compactJoinPartials classGraph partials = joinUnionType $ concat $ H.elems $ fmap joinMatchArgPartials $ H.fromListWith (++) $ map prepGroupJoinable $ splitUnionType partials
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
    joinMap = H.unionWith (unionTypes classGraph)

-- | Removes partials which contain a type variable that is the 'bottomType', because then the whole partial is a 'bottomType'.
compactBottomTypeVars :: PartialLeafs -> PartialLeafs
compactBottomTypeVars partials = joinUnionType $ mapMaybe aux $ splitUnionType partials
  where
    aux partial@PartialType{ptVars} = if any isBottomType ptVars
      then Nothing
      else Just partial

-- |
-- Used to simplify and reduce the size of a 'Type'.
-- It has several internal passes that apply various optimizations to a type.
-- TODO: This should merge type partials into class partials
compactType :: ClassGraph -> Type -> Type
compactType _ (TopType ps) = TopType ps
compactType _ t@TypeVar{} = t
compactType classGraph (UnionType partials) = UnionType $ (compactOverlapping classGraph . compactJoinPartials classGraph . compactBottomTypeVars) partials

-- | Takes the union of two types (∪)
unionTypes :: ClassGraph -> Type -> Type -> Type
unionTypes _ (TopType []) _ = topType
unionTypes _ _ (TopType []) = topType
unionTypes _ (TopType _) _ = undefined
unionTypes _ _ (TopType _) = undefined
unionTypes _ t1 t2 | isBottomType t2 = t1
unionTypes _ t1 t2 | isBottomType t1 = t2
unionTypes _ t1 t2 | t1 == t2 = t1
unionTypes _ (TypeVar v) t = error $ printf "Can't union type vars %s with %s " (show v) (show t)
unionTypes _ t (TypeVar v) = error $ printf "Can't union type vars %s with %s " (show t) (show v)
unionTypes classGraph (UnionType aPartials) (UnionType bPartials) = compactType classGraph $ UnionType partials'
  where
    partials' = H.unionWith S.union aPartials bPartials

-- | Takes the 'unionTypes' of many types
unionAllTypes :: Foldable f => ClassGraph -> f Type -> Type
unionAllTypes classGraph = foldr (unionTypes classGraph) bottomType

-- | Takes the 'intersectTypes' of many types
intersectAllTypes :: Foldable f => ClassGraph -> f Type -> Type
intersectAllTypes classGraph = foldr (intersectTypes classGraph) topType

intersectPartialName :: PartialName -> PartialName -> Maybe PartialName
intersectPartialName a b   | a == b = Just a
intersectPartialName (PRelativeName a) (PRelativeName b) = case () of
  _ | a `relativeNameMatches` b -> Just (PRelativeName b)
  _ | b `relativeNameMatches` a -> Just (PRelativeName a)
  _                             -> Nothing
intersectPartialName a b@PRelativeName{} = intersectPartialName b a
intersectPartialName (PRelativeName a) b = if a `relativeNameMatches` fromPartialName b
  then Just b
  else Nothing
intersectPartialName _ _                           = Nothing

-- | A private helper for 'intersectPartialsBase' that intersects while ignore class expansions
intersectPartialsBase :: ClassGraph -> TypeVarEnv -> PartialType -> PartialType -> Maybe (TypeVarEnv, [PartialType])
intersectPartialsBase _ _ PartialType{ptName=aName} PartialType{ptName=bName} | isNothing (intersectPartialName aName bName) = Nothing
intersectPartialsBase _ _ PartialType{ptArgs=aArgs, ptArgMode=aArgMode} PartialType{ptArgs=bArgs, ptArgMode=bArgMode} | aArgMode == PtArgExact && bArgMode == PtArgExact && H.keysSet aArgs /= H.keysSet bArgs = Nothing
intersectPartialsBase classGraph venv (PartialType aName aVars aArgs aPreds aArgMode) (PartialType bName bVars bArgs bPreds bArgMode) = do
  (varsVenvs, vars') <- unzip <$> intersectMap H.empty aVars bVars
  (argsVenvs, args') <- unzip <$> intersectMap venv aArgs bArgs
  let venvs' = mergeAllVarEnvs classGraph [mergeAllVarEnvs classGraph argsVenvs, vars']
  let argMode' = case (aArgMode, bArgMode) of
        (PtArgAny, _)            -> PtArgAny
        (_, PtArgAny)            -> PtArgAny
        (PtArgExact, PtArgExact) -> PtArgExact
  let name' = fromJust $ intersectPartialName aName bName
  let preds' = aPreds ++ bPreds
  return (mergeAllVarEnvs classGraph varsVenvs, [PartialType name' venvs' args' preds' argMode'])
  where
    -- intersectMap unions so that all typeVars from either a or b are kept
    intersectMap vev a b = traverse subValidate $ H.unionWith subUnion (fmap (vev,) a) (fmap (vev,) b)
    subUnion (aVenv, a) (bVenv, b) = intersectTypesWithVarEnv classGraph (mergeAllVarEnvs classGraph [aVenv, bVenv]) a b
    subValidate (vev, subTp) = if isBottomType subTp then Nothing else Just (vev, subTp)

-- |
-- Takes the intersection of two 'PartialType' or returns Nothing if their intersection is 'bottomType'
-- It uses the 'TypeVarEnv' for type variable arguments and determines any possible changes to the surrounding 'TypeVarEnv'.
intersectPartials :: ClassGraph -> TypeVarEnv -> PartialType -> PartialType -> Maybe (TypeVarEnv, [PartialType])
intersectPartials classGraph venv a b | ptName a /= ptName b && isSubPartialOfWithEnv classGraph (joinVarArgEnv venv H.empty) a b = Just (venv, [a])
intersectPartials classGraph venv a b | ptName a /= ptName b && isSubPartialOfWithEnv classGraph (joinVarArgEnv venv H.empty) b a = Just (venv, [b])
intersectPartials classGraph venv a b = case catMaybes [base, aExpandClass, bExpandClass] of
  [] -> Nothing
  combined -> Just $ foldr1 (\(venv1, a') (venv2, b') -> (mergeVarEnvs classGraph venv1 venv2, a' ++ b')) combined
  where
    base = intersectPartialsBase classGraph venv a b
    typeAsUnion (v, UnionType pl) = (v, splitUnionType pl)
    typeAsUnion _                 = error "Expected a union"
    aExpandClass = case a of
      PartialType{ptName=PClassName{}} -> Just $ typeAsUnion $ intersectTypesWithVarEnv classGraph venv (expandPartial classGraph a) (singletonType b)
      _ -> Nothing
    bExpandClass = case b of
      PartialType{ptName=PClassName{}} -> Just $ typeAsUnion $ intersectTypesWithVarEnv classGraph venv (singletonType a) (expandPartial classGraph b)
      _ -> Nothing

-- |
-- Takes the intersection of two 'Type'.
-- It uses the 'TypeVarEnv' for type variable arguments and determines any possible changes to the surrounding 'TypeVarEnv'.
intersectTypesWithVarEnv :: ClassGraph -> TypeVarEnv -> Type -> Type -> (TypeVarEnv, Type)
intersectTypesWithVarEnv _ venv (TopType []) t = (venv, t)
intersectTypesWithVarEnv _ venv t (TopType []) = (venv, t)
intersectTypesWithVarEnv _ venv (TopType ps1) (TopType ps2) = (venv, TopType (ps1 ++ ps2))
intersectTypesWithVarEnv _ venv t1 t2 | t1 == t2 = (venv, t1)
intersectTypesWithVarEnv _ _ (TypeVar v1) (TypeVar v2) = error $ printf "Can't intersect type vars %s with %s" (show v1) (show v2)
intersectTypesWithVarEnv classGraph venv tv@(TypeVar (TVVar TVInt v)) t = (H.insertWith (intersectTypes classGraph) v t venv, tv)
intersectTypesWithVarEnv classGraph venv t tv@(TypeVar (TVVar TVInt v)) = (H.insertWith (intersectTypes classGraph) v t venv, tv)
intersectTypesWithVarEnv _ _ (TypeVar v) t = error $ printf "Can't intersect type vars %s with %s" (show v) (show t)
intersectTypesWithVarEnv _ _ t (TypeVar v) = error $ printf "Can't intersect type vars %s with %s" (show t) (show v)
intersectTypesWithVarEnv _ venv _ t | isBottomType t = (venv, t)
intersectTypesWithVarEnv _ venv t _ | isBottomType t = (venv, t)
intersectTypesWithVarEnv _ venv (UnionType partials) (TopType topPreds) = (venv, UnionType $ joinUnionType $ map addPreds $ splitUnionType partials)
  where
    addPreds partial@PartialType{ptPreds} = partial{ptPreds = topPreds ++ ptPreds}
intersectTypesWithVarEnv classGraph venv t1@TopType{} t2@UnionType{} = intersectTypesWithVarEnv classGraph venv t2 t1
intersectTypesWithVarEnv classGraph venv (UnionType aPartials) (UnionType bPartials) = case catMaybes [intersectPartials classGraph venv aPartial bPartial | aPartial <- splitUnionType aPartials, bPartial <- splitUnionType bPartials] of
  [] -> (venv, bottomType)
  combined -> do
    let (venvs', partials') = unzip combined
    (mergeAllVarEnvs classGraph venvs', compactType classGraph $ UnionType $ joinUnionType $ concat partials')

-- | Takes the intersection of two 'Type' (∩).
intersectTypes :: ClassGraph -> Type -> Type -> Type
intersectTypes classGraph a b = snd $ intersectTypesWithVarEnv classGraph H.empty a b

-- | Takes the powerset of a 'Type' with the powerset of the arguments in the type.
powersetType :: ClassGraph -> Type -> Type
powersetType _ (TopType []) = topType
powersetType _ TopType{} = undefined
powersetType _ (TypeVar t) = TypeVar t
powersetType classGraph (UnionType partials) = compactType classGraph $ UnionType partials'
  where
    partials' = joinUnionType $ concatMap fromPartialType $ splitUnionType partials
    fromArgs args = powerset $ H.toList args
    fromPartialType (PartialType name vars args _ argMode) = [PartialType name vars (H.fromList a) [] argMode | a <- fromArgs args]

-- |
-- Combines two 'TypeVarEnv' to form the one applying the knowledge from both
-- It takes the union of all variables from either, and shared variables combine knowledge by intersection
mergeVarEnvs :: ClassGraph -> TypeVarEnv -> TypeVarEnv -> TypeVarEnv
mergeVarEnvs classGraph = H.unionWith (intersectTypes classGraph)


-- | Applies 'mergeVarEnvs' to many 'TypeVarEnv'
mergeAllVarEnvs :: Foldable f => ClassGraph -> f TypeVarEnv -> TypeVarEnv
mergeAllVarEnvs classGraph = foldr (mergeVarEnvs classGraph) H.empty

-- | Replaces the type variables 'TVVar' in a 'Type' based on the variables in a provided 'TypeVarEnv'
substituteVarsWithVarEnv :: TypeVarEnv -> Type -> Type
substituteVarsWithVarEnv venv (UnionType partials) = UnionType $ joinUnionType $ map (substitutePartial venv) $ splitUnionType partials
  where substitutePartial pVenv partial@PartialType{ptVars, ptArgs, ptPreds} = partial{
          ptVars = fmap (substituteVarsWithVarEnv pVenv) ptVars,
          ptArgs = fmap (substituteVarsWithVarEnv ptVars') ptArgs,
          ptPreds = map (substitutePartial ptVars') ptPreds
                                                                        }
          where ptVars' = fmap (substituteVarsWithVarEnv venv) ptVars
substituteVarsWithVarEnv venv (TypeVar (TVVar TVInt v)) = case H.lookup v venv of
  Just v' -> v'
  Nothing -> error $ printf "Could not substitute unknown type var %s with venv keys %s" v (show $ H.keys venv)
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
substituteArgsWithArgEnv aenv (TypeVar (TVArg TVInt v)) = case H.lookup v aenv of
  Just v' -> v'
  Nothing -> error $ printf "Could not substitute unknown type arg %s" v
substituteArgsWithArgEnv _ t = t

substituteWithVarArgEnv :: TypeVarArgEnv -> Type -> Type
substituteWithVarArgEnv vaenv = substituteArgsWithArgEnv aenv . substituteVarsWithVarEnv venv
  where
    (venv, aenv) = splitVarArgEnv vaenv

-- | Gets an arg from a type while substituting the variables used in the types ptVars
typeGetArg :: ArgName -> PartialType -> Maybe Type
typeGetArg argName PartialType{ptArgs, ptVars} = do
  arg <- H.lookup argName ptArgs
  return $ case arg of
    (TopType _) -> topType
    TypeVar (TVVar TVInt v) -> H.lookupDefault topType v ptVars
    TypeVar (TVVar TVExt _) -> error $ printf "Not yet implemented"
    TypeVar (TVArg _ _) -> error $ printf "Not yet implemented"
    UnionType partialLeafs -> UnionType $ joinUnionType $ map substitutePartial $ splitUnionType partialLeafs
      where
        substitutePartial partial@PartialType{ptVars=vs} = partial{ptVars = fmap (substituteVarsWithVarEnv ptVars) vs}

-- | Gets an arg from a type while substituting the variables used in the types ptVars
typesGetArg :: ClassGraph -> ArgName -> Type -> Maybe Type
typesGetArg classGraph argName (UnionType partialLeafs) = fmap (unionAllTypes classGraph) $ mapM (typeGetArg argName) $ splitUnionType partialLeafs
typesGetArg _ _ _ = Nothing
