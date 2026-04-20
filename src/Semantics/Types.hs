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

{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Semantics.Types where

import           CtConstants
import           Data.Aeson
import           Data.Bifunctor      (Bifunctor (bimap, first))
import           Data.Either
import           Data.Graph          (graphFromEdges, vertices)
import           Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.List           (intercalate, partition)
import qualified Data.List           as L
import           Data.List.Split     (splitOn)
import           Data.Maybe
import           Data.Zip
import           Debug.Trace         (trace)
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

data TypePredicates
  = PredsOne TypePredicate
  | PredsAnd [TypePredicates]
  | PredsNot TypePredicates
  deriving (Eq, Ord, Generic, Hashable, ToJSON)

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

data Constant
  = CInt Integer
  | CFloat Double
  | CStr String
  | CChar Char
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- | The non-name properties of a 'PartialType'
type PartialArgsOption = (H.HashMap TypeVarName Type, H.HashMap ArgName Type, TypePredicates, PtArgMode)

-- | An alternative format for many 'PartialType's which combine those that share the same name
type PartialLeafs = H.HashMap TypeName (S.HashSet PartialArgsOption)

-- |
-- The basic format of a 'Type' in Catln.
-- 'UnionType mTop posLeafs consts' semantics:
--   * Nothing             + posLeafs + []     = positive union of partials
--   * Just (preds, negLeafs) + posLeafs + []  = complement type (universal filtered by preds, minus negLeafs) ∪ posLeafs
--   * Any                 + _         + consts = union includes constants
data Type
  = UnionType (Maybe (TypePredicates, PartialLeafs)) PartialLeafs [Constant]
  | TypeVar TypeVarAux TypeVarLoc
  deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)

-- | Backward-compat pattern: matches a complement/top type (predicates, negative partials).
-- Corresponds to the old @TopType PartialLeafs TypePredicates@ constructor.
pattern TopType :: PartialLeafs -> TypePredicates -> Type
pattern TopType negLeafs preds <- UnionType (Just (preds, negLeafs)) (H.null -> True) []
  where TopType negLeafs preds = UnionType (Just (preds, negLeafs)) H.empty []

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

class TypeGraph tg where
  -- | Queries the type graph for possible conversions from a partial type to a type, returning the reason for each conversion as well.
  typeGraphQueryWithReason :: TypeEnv tg -> TypeVarArgEnv -> PartialType -> [(String, Type)]

  -- | Queries the type graph for possible conversions from a TopTypes PredExpr to the possible partialTypes that can contain the PredExpr
  typeGraphExpandPredExpr :: TypeEnv tg -> TypeVarArgEnv -> PartialType -> Maybe [PartialType]

data EmptyTypeGraph = EmptyTypeGraph
instance Semigroup EmptyTypeGraph where
  _ <> _ = EmptyTypeGraph
instance TypeGraph EmptyTypeGraph where
  typeGraphQueryWithReason _ _ _ = []
  typeGraphExpandPredExpr _ _ _ = Nothing

typeGraphQuery :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> [Type]
typeGraphQuery typeEnv vaenv src = map snd $ typeGraphQueryWithReason typeEnv vaenv src

data TypeEnv tg = TypeEnv {
  teClassGraph     :: ClassGraph,
  teTypeGraph      :: tg,
  teNames          :: S.HashSet Name,
  teDisableCompact :: Bool,
  teDebug          :: Bool,
  -- | When 'True', type operations are performed under the closed-world assumption:
  -- no types exist outside those currently defined, so all classes are effectively
  -- sealed and predicates can be safely expanded.  Used for declaration-coverage checks.
  tePrgmEnv        :: Bool
                          }
  deriving (Show)

defaultTypeEnvDebug :: Bool
defaultTypeEnvDebug = False

emptyTypeEnv' :: TypeEnv EmptyTypeGraph
emptyTypeEnv' = TypeEnv mempty EmptyTypeGraph S.empty False defaultTypeEnvDebug False

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

instance Show TypePredicates where
  show (PredsOne p)  = show p
  show (PredsAnd ps) = intercalate " && " $ map show ps
  show (PredsNot p)  = printf "!(%s)" (show p)

instance Show PartialType where
  show (PartialType ptName ptVars ptArgs ptPreds ptArgMode) = concat [showName, showTypeVars ptVars, showArgs ptArgs, showPreds ptPreds, showPtArgMode]
    where
      showName = if ptName == anonStr then "" else ptName
      showArg (argName, argVal) = show argName ++ "=" ++ show argVal
      showTypeVars vars | H.null vars = ""
      showTypeVars vars = printf "[%s]" (intercalate ", " $ map showArg $ H.toList vars)
      showArgs args | ptName == anonStr && H.null args = "()"
      showArgs args | H.null args = ""
      showArgs args = printf "(%s)" (intercalate ", " $ map showArg $ H.toList args)
      showPreds PredsNone    = ""
      showPreds (PredsOne p) = printf "| %s" (show p)
      showPreds preds        = printf "| (%s)" (show preds)
      showPtArgMode = case ptArgMode of
        PtArgExact -> ""
        PtArgAny   -> ".."

instance Show PartialKey where
  show pk = show $ partialToType pk

instance Show Type where
  show (UnionType Nothing leafs []) = joinParts $ map show $ splitUnionType leafs
    where
      joinParts []  = "∅"
      joinParts [p] = p
      joinParts ps  = "(" ++ intercalate " + " ps ++ ")"
  show (UnionType (Just (p, negPartials)) (H.null -> True) []) = p' ++ negPartials'
    where
      p' = case tryPredsToList p of
                Just [PredClass c] -> "∀" ++ show c
                Just [PredRel c]   -> "~" ++ show c
                Just []            -> "TopType"
                _                  -> printf "(TopType | %s)" (show p)
      negPartials' = if H.null negPartials
        then ""
        else " - " ++ (joinParts $ map show $ splitUnionType negPartials)
      joinParts []  = "∅"
      joinParts [s] = s
      joinParts ss  = "(" ++ intercalate " + " ss ++ ")"
  show (UnionType mTop leafs consts) = printf "UnionType(%s, %s, %s)" (show mTop) (joinParts $ map show $ splitUnionType leafs) (show consts)
    where
      joinParts []  = "∅"
      joinParts [p] = p
      joinParts ps  = "(" ++ intercalate " + " ps ++ ")"
  show (TypeVar v _) = show v

instance Show ClassGraph where
  show (ClassGraph graphData) = show $ map fst3 $ graphToNodes graphData

instance Semigroup ClassGraph where
  (ClassGraph classGraphA) <> (ClassGraph classGraphB) = ClassGraph $ mapToGraph $ H.unionWith mergeClasses (graphToMap classGraphA) (graphToMap classGraphB)
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

      mergeClassPartials clss@PartialType{ptVars=varsA} PartialType{ptVars=varsB} = clss{ptVars = H.unionWith (unionTypes emptyTypeEnv'{teClassGraph=ClassGraph classGraphA}) varsA varsB}
instance Monoid ClassGraph where
  mempty = ClassGraph $ graphFromEdges []

instance (Semigroup tg) => Semigroup (TypeEnv tg) where
  (TypeEnv cg1 tg1 n1 d1 dc1 pe1) <> (TypeEnv cg2 tg2 n2 d2 dc2 pe2) = TypeEnv (cg1 <> cg2) (tg1 <> tg2) (S.union n1 n2) (d1 || d2) (dc1 || dc2) (pe1 || pe2)


mergeDoc :: Maybe String -> Maybe String -> Maybe String
mergeDoc (Just a) (Just b) = Just (a ++ " " ++ b)
mergeDoc (Just a) Nothing  = Just a
mergeDoc Nothing (Just b)  = Just b
mergeDoc _ _               = Nothing

-- | Converts the 'ClassGraph' into the equivalent 'ClassMap'
asClassMap :: ClassGraph -> ClassMap
asClassMap (ClassGraph graphData) = (typesToClass, classToTypes)
  where
    typesToClass = fmap S.fromList $ H.fromListWith (++) $ concatMap (mapMaybe typeToClassFromPartial . getNodeTypeToClass) (graphToNodes graphData)
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

mapTypePreds :: (TypePredicate -> TypePredicate) -> TypePredicates -> TypePredicates
mapTypePreds f (PredsOne p)  = PredsOne (f p)
mapTypePreds f (PredsAnd ps) = PredsAnd (map (mapTypePreds f) ps)
mapTypePreds f (PredsNot p)  = PredsNot (mapTypePreds f p)

pattern PredsNone :: TypePredicates
pattern PredsNone = PredsAnd []

predsAnd :: TypePredicates -> TypePredicates -> TypePredicates
predsAnd (PredsAnd as) (PredsAnd bs) = mkAnd (as ++ bs)
predsAnd (PredsAnd ps) p             = mkAnd (p:ps)
predsAnd p (PredsAnd ps)             = mkAnd (p:ps)
predsAnd a b                         = mkAnd [a, b]

-- | Normalize a list of predicates into a 'TypePredicates', deduplicating and
-- collapsing singleton lists so 'PredsAnd [p] = p'.
mkAnd :: [TypePredicates] -> TypePredicates
mkAnd ps = case uniq ps of
  [p] -> p
  ps' -> PredsAnd ps'

predsNot :: TypePredicates -> TypePredicates
predsNot (PredsNot p) = p
predsNot PredsNone    = PredsNone
predsNot p            = PredsNot p

-- | Returns True if the predicates are syntactically contradictory, i.e. contain both p and ¬p.
-- Uses iterative unit propagation: ¬(A∧B) with A known → derives ¬B, allowing multi-step
-- contradiction detection. Limited to 3 rounds to bound complexity.
isPredsContradictory :: TypePredicates -> Bool
isPredsContradictory (PredsAnd ps) = go (3 :: Int) ps
  where
    go 0 _  = False
    go n ps'
      | any (\p -> PredsNot p `elem` ps') ps' = True
      | otherwise =
          let derived = L.nub $ concatMap (unitPropDerive ps') ps'
              ps'' = L.nub $ ps' ++ derived
          in if length ps'' > length ps'
             then go (n - 1) ps''
             else False
    -- From ¬(A∧B∧...) with all-but-one known, derive the negation of the remaining.
    -- Use predsNot (not PredsNot) so that ¬(¬A) normalizes to A, enabling contradiction detection.
    unitPropDerive :: [TypePredicates] -> TypePredicates -> [TypePredicates]
    unitPropDerive known (PredsNot (PredsAnd binds)) =
      [predsNot b | b <- binds, let others = filter (/= b) binds, all (`elem` known) others]
    unitPropDerive _ _ = []
isPredsContradictory _             = False

partialAddPreds :: PartialType -> TypePredicates -> PartialType
partialAddPreds partial@PartialType{ptPreds} newPreds = partial{ptPreds = predsAnd newPreds ptPreds}

partialLeafsAddPreds :: PartialLeafs -> TypePredicates -> PartialLeafs
partialLeafsAddPreds partials newPreds = joinUnionType $ map (`partialAddPreds` newPreds) $ splitUnionType partials

typeAddPreds :: Type -> TypePredicates -> Type
typeAddPreds (TopType negLeafs preds) ps = TopType negLeafs (predsAnd ps preds)
typeAddPreds v@TypeVar{} _ = error $ printf "Unimplemented typeAddPreds: %s" (show v)
typeAddPreds (UnionType Nothing leafs []) ps = UnionType Nothing (partialLeafsAddPreds leafs ps) []
typeAddPreds t _ = error $ printf "Unimplemented typeAddPreds: %s" (show t)

clearUnionTypePreds :: Type -> Type
clearUnionTypePreds (UnionType Nothing partials []) = UnionType Nothing (joinUnionType $ map (\p -> p{ptPreds=PredsNone}) $ splitUnionType partials) []
clearUnionTypePreds t = t

tryPredsToList :: TypePredicates -> Maybe [TypePredicate]
tryPredsToList (PredsOne p)  = Just [p]
tryPredsToList (PredsAnd ps) = concat <$> mapM tryPredsToList ps
tryPredsToList (PredsNot _)  = Nothing


-- | Helper function for conditional trace debugging
debugTrace :: (TypeGraph tg) => TypeEnv tg -> String -> a -> a
debugTrace _ "" x                      = x
debugTrace TypeEnv{teDebug=True} msg x = trace msg x
debugTrace _ _ x                       = x

listClassNames :: (TypeGraph tg) => TypeEnv tg -> [ClassName]
listClassNames TypeEnv{teClassGraph=ClassGraph graphData} = map (fromPartialName . snd3) $ filter isClass $ graphToNodes graphData
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
boolType = UnionType Nothing (joinUnionType [trueLeaf, falseLeaf]) []
strType = singletonType strLeaf
ioType = singletonType ioLeaf

constantPartialType :: Constant -> PartialType
constantPartialType CInt{}   = intLeaf
constantPartialType CFloat{} = floatLeaf
constantPartialType CStr{}   = strLeaf
constantPartialType CChar{}  = charLeaf

-- | The singleton type containing exactly one constant value.
-- For example, @constantType (CInt 5)@ represents the type @{5}@, a subtype of @Integer@.
constantType :: Constant -> Type
constantType c = UnionType Nothing H.empty [c]


-- | The 'Type' containing all possible values, equivalent to the universal.
pattern PTopType :: Type
pattern PTopType <- UnionType (Just (PredsNone, H.null -> True)) (H.null -> True) []
  where PTopType = UnionType (Just (PredsNone, H.empty)) H.empty []

-- | The 'Type' containing no possible values, equivalent to the empty set ∅.
-- It often corresponds to errors in the type inference process and indicates the error type.
pattern BottomType :: Type
pattern BottomType <- UnionType Nothing (H.null -> True) []
  where BottomType = UnionType Nothing H.empty []

-- | Used to check if a type is equivalent to 'bottomType'.
-- It can be necessary because it is possible for non-compacted types (see 'compactType') to be bottom types but not equal to 'bottomType'.
isBottomType :: Type -> Bool
-- isBottomType t = compactType t == bottomType
isBottomType t = t == BottomType

isTypeVar :: Type -> Bool
isTypeVar TypeVar{} = True
isTypeVar _         = False

containsBottomType :: Type -> Bool
containsBottomType BottomType = True
containsBottomType (UnionType (Just _) _ _) = False
containsBottomType (TypeVar _ _) = False
containsBottomType (UnionType Nothing leafs _) = any containsBottomPartialType $ splitUnionType leafs

containsBottomPartialType :: PartialType -> Bool
containsBottomPartialType PartialType{ptArgs, ptVars} = any containsBottomType ptArgs || any containsBottomType ptVars

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
makeAbsoluteName ""        = ""
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
relPathAddPrefix "" n = n
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
singletonType partial = UnionType Nothing (joinUnionType [partial]) []

-- | Splits a partial type by expanding any union-typed vars or args into separate partials.
-- For example, P[$T=(A + B)] splits into [P[$T=A], P[$T=B]].
-- Similarly, P(x=(A + B)) splits into [P(x=A), P(x=B)].
-- A partial with no union-typed vars or args returns [itself] unchanged.
splitPartialByUnionVarArgs :: PartialType -> [PartialType]
splitPartialByUnionVarArgs p@PartialType{ptVars, ptArgs} =
    case findUnion allVarArgs of
      Nothing -> [p]
      Just (keyName, components) -> concatMap splitPartialByUnionVarArgs
        [typeSetAux keyName (singletonType component) p | component <- components]
  where
    allVarArgs = map (first TVVar) (H.toList ptVars) ++ map (first TVArg) (H.toList ptArgs)

    findUnion :: [(TypeVarAux, Type)] -> Maybe (TypeVarAux, [PartialType])
    findUnion [] = Nothing
    findUnion ((keyName, UnionType Nothing leafs []):rest) = case splitUnionType leafs of
      (_:_:_) -> Just (keyName, splitUnionType leafs)
      _       -> findUnion rest
    findUnion (_:rest) = findUnion rest

-- | Helper to create a 'PartialKey' for a value (no args, no vars)
partialKey :: Name -> PartialKey
partialKey n = PartialKey (makeAbsoluteName n) S.empty S.empty

partialToKey :: PartialType -> PartialKey
partialToKey PartialType{ptName, ptVars, ptArgs} = makeAbsolutePk $ PartialKey ptName (H.keysSet ptVars) (H.keysSet ptArgs)

partialToType :: PartialKey -> PartialType
partialToType PartialKey{pkName, pkVars, pkArgs} = (partialVal pkName){ptVars = asArgs pkVars, ptArgs = asArgs pkArgs}
  where
    asArgs = fmap (const PTopType) . S.toMap

partialToTypeSingleton :: PartialKey -> Type
partialToTypeSingleton = singletonType . partialToType

-- | Helper to create a 'PartialType' for a value (no args, no vars)
partialVal :: TypeName -> PartialType
partialVal n = PartialType n H.empty H.empty PredsNone PtArgExact

-- | Helper to create a 'Type' for a value (no args, no vars)
typeVal :: TypeName -> Type
typeVal = singletonType . partialVal

-- | Helper to create a relative 'Type' for a value (no args, no vars)
-- | TODO Make this return a 'typeVal' when it is an absolute path
relTypeVal :: TypeName -> Type
relTypeVal n = TopType H.empty (PredsOne (PredRel $ partialVal n))

classPartial :: PartialType -> Type
classPartial p = TopType H.empty (PredsOne (PredClass p))

-- | Creates a classPlaceholder[$T] partial type for a given class name.
-- Used when expanding a class that has no constituent types to avoid returning the empty set.
classPlaceholderLeaf :: TypeName -> PartialType
classPlaceholderLeaf className = (partialVal classPlaceholderStr){ptVars = H.singleton (partialKey "$T") (singletonType $ partialVal className)}

getSingleton :: Type -> PartialType
getSingleton t = case maybeGetSingleton t of
  Just t' -> t'
  Nothing -> error $ printf "Failed to get singleton for %s" (show t)

maybeGetSingleton :: Type -> Maybe PartialType
maybeGetSingleton (UnionType Nothing leafs []) = case splitUnionType leafs of
  [p] -> Just p
  _   -> Nothing
maybeGetSingleton _ = Nothing

maybeGetClassSingleton :: Type -> Maybe PartialType
maybeGetClassSingleton (TopType _ (PredsOne (PredClass c))) = Just c
maybeGetClassSingleton _                                    = Nothing

maybeGetTypeName :: Type -> Maybe TypeName
maybeGetTypeName t | isJust (maybeGetSingleton t) = Just $ ptName $ getSingleton t
maybeGetTypeName (TopType _ (PredsOne (PredRel p))) = Just $ ptName p
maybeGetTypeName _ = Nothing

typeSetArg :: ArgName -> Type -> Type -> Type
typeSetArg argName argVal (UnionType Nothing leafs []) = UnionType Nothing (joinUnionType $ map aux $ splitUnionType leafs) []
  where
    aux p@PartialType{ptArgs} = p{ptArgs=H.insert argName argVal ptArgs}
typeSetArg argName argVal (TopType negPartials (PredsOne (PredRel p@PartialType{ptArgs}))) = TopType negPartials (PredsOne $ PredRel p{ptArgs=H.insert argName argVal ptArgs})
typeSetArg _ _ tp = error $ printf "Unimplemented typeSetArg for %s" (show tp)

typeSetVar :: TypeVarName -> Type -> Type -> Type
typeSetVar varName varVal (UnionType Nothing leafs []) = UnionType Nothing (joinUnionType $ map aux $ splitUnionType leafs) []
  where
    aux p@PartialType{ptVars} = p{ptVars=H.insert varName varVal ptVars}
typeSetVar varName varVal (TopType negPartials (PredsOne (PredRel p@PartialType{ptVars}))) = TopType negPartials (PredsOne $ PredRel p{ptVars=H.insert varName varVal ptVars})
typeSetVar varName varVal (TopType negPartials (PredsOne (PredClass p@PartialType{ptVars}))) = TopType negPartials (PredsOne $ PredClass p{ptVars=H.insert varName varVal ptVars})
typeSetVar varName varVal tp = error $ printf "Unimplemented typeSetVar for %s.%s = %s" (show tp) (show varName) (show varVal)

suffixLookup :: String -> [String] -> Maybe String
suffixLookup s (x:xs)
  | s == x = Just s
  | otherwise = if relativeNameMatches s x then Just x else suffixLookup s xs
suffixLookup _ [] = Nothing

suffixLookupInDict :: String -> H.HashMap String b -> Maybe b
suffixLookupInDict s dict = case suffixLookup s (H.keys dict) of
  Just k  -> H.lookup k dict
  Nothing -> Nothing

topTypeAsPartials :: TypeEnv tg ->[PartialType]
topTypeAsPartials TypeEnv{teNames} = map ((\p -> p{ptArgMode=PtArgAny}) . partialVal) $ S.toList teNames

-- | Expands a 'TypePredicates' value into a 'Type' by resolving class, relation, and expression predicates.
expandPredicates :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicates -> Type
expandPredicates _       _     PredsNone    = PTopType
expandPredicates typeEnv vaenv (PredsOne p) = expandPredicate typeEnv vaenv p
expandPredicates typeEnv vaenv (PredsAnd ps) = intersectAllTypes typeEnv $ map (expandPredicates typeEnv vaenv) ps
expandPredicates typeEnv vaenv (PredsNot p)  = complementTypeEnv typeEnv vaenv $ expandPredicates typeEnv vaenv p

expandPredicate :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicate -> Type
expandPredicate typeEnv vaenv (PredClass clss) = expandClassPartial typeEnv vaenv clss
expandPredicate typeEnv vaenv (PredRel rel)    = expandRelPartial typeEnv vaenv rel
expandPredicate typeEnv vaenv (PredExpr e)     = maybe (TopType H.empty (PredsOne $ PredExpr e)) (\ps -> UnionType Nothing (joinUnionType ps) []) (typeGraphExpandPredExpr typeEnv vaenv e)

-- | Returns 'True' if every type satisfying @p1@ also satisfies @p2@, i.e. U_p1 ⊆ U_p2.
-- Sound but potentially incomplete: 'False' does not guarantee the predicates are incomparable.
predImplies :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicates -> TypePredicates -> Bool
-- Structural base cases
predImplies _       _     _  PredsNone = True   -- anything ⊆ top
predImplies _       _     p1 p2 | p1 == p2 = True  -- reflexivity
predImplies _       _     PredsNone _ = False   -- top ⊄ anything specific
-- Logical connectives
predImplies typeEnv vaenv (PredsAnd ps) p2      = any (\p -> predImplies typeEnv vaenv p p2) ps || cancellationCheck
  -- if any conjunct alone implies p2, the intersection does too
  -- also: (X₁ ∨ X₂ ∨ ... ∨ q) ∧ ¬X₁ ∧ ¬X₂ ∧ ... ⊆ q  (disjunction cancellation)
  where
    cancellationCheck = or
      [ all (\x -> PredsNot x `elem` ps) xs
      | PredsNot (PredsAnd nots) <- ps
      , Just inners <- [mapM extractNot nots]
      , (q_inner, xs) <- choices inners
      , predImplies typeEnv vaenv q_inner p2
      ]
    extractNot (PredsNot x) = Just x
    extractNot _            = Nothing
    choices []     = []
    choices (y:ys) = (y, ys) : map (\(z, zs) -> (z, y:zs)) (choices ys)
predImplies typeEnv vaenv p1 (PredsAnd ps)      = all (predImplies typeEnv vaenv p1) ps
  -- p1 must imply every conjunct
predImplies typeEnv vaenv (PredsNot inner) (PredsNot outer) = predImplies typeEnv vaenv outer inner
  -- contravariance: ¬A ⊆ ¬B ↔ B ⊆ A
predImplies _       _     p1 (PredsNot p2)      = isPredsContradictory (predsAnd p1 p2)
  -- U_p1 ⊆ ¬U_p2 ↔ U_p1 ∩ U_p2 = ∅
-- !(¬a₁ ∧ ¬a₂ ∧ ...) = a₁ ∨ a₂ ∨ ...: a disjunction implies q iff every disjunct implies q
predImplies typeEnv vaenv (PredsNot (PredsAnd nots)) q2
  = case mapM extractNot nots of
      Just inners -> all (\inner -> predImplies typeEnv vaenv inner q2) inners
      Nothing     -> False
  where
    extractNot (PredsNot x) = Just x
    extractNot _            = Nothing
predImplies _       _     (PredsNot _) _        = False  -- conservative
-- Atom cases
predImplies typeEnv vaenv (PredsOne a1) (PredsOne a2) = predImpliesAtom typeEnv vaenv a1 a2

predImpliesAtom :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicate -> TypePredicate -> Bool
-- PredClass: c1 implies c2 only when c1 is sealed (all members known) and each member is in U_c2
predImpliesAtom typeEnv@TypeEnv{teClassGraph=ClassGraph cg, tePrgmEnv} vaenv (PredClass c1) (PredClass c2) =
  case graphLookup (PClassName (ptName c1)) cg of
    Just (CGClass (sealed, _, classTypes, _)) | sealed || tePrgmEnv ->
      let c2Type = expandClassPartial typeEnv vaenv c2
      in all (\t -> isSubtypeOfWithEnv typeEnv vaenv t c2Type) classTypes
    _ -> False  -- unsealed or unknown class: can't enumerate all members
-- PredRel: r1 implies r2 when r2's name path is a suffix of r1's (r2 is more general)
-- and r1's args are subtypes of r2's args.
predImpliesAtom typeEnv vaenv (PredRel r1) (PredRel r2) =
  splitOn "/" (ptName r2) `L.isSuffixOf` splitOn "/" (ptName r1)
  && all checkArg (H.keys (ptArgs r2))
  where checkArg k = isSubtypeOfWithEnv typeEnv vaenv
                       (H.lookupDefault PTopType k (ptArgs r1))
                       (ptArgs r2 H.! k)
-- PredExpr: use structural partial comparison (equality-based simplifying assumption)
predImpliesAtom typeEnv vaenv (PredExpr e1) (PredExpr e2) = isSubPartialOfWithEnv typeEnv vaenv e1 e2
-- PredClass c implies PredRel r when r's name is a suffix of c's name.
-- This holds because expandRelPartial r includes classPartial c, so U_PredClass c ⊆ U_PredRel r.
predImpliesAtom _ _ (PredClass c) (PredRel r) =
  splitOn "/" (ptName r) `L.isSuffixOf` splitOn "/" (ptName c)
-- Cross-kind: conservative False
predImpliesAtom _ _ _ _ = False

-- | Returns 'True' if every type satisfying @preds@ is a member of @partials@, i.e. U_preds ⊆ partials.
-- Expands @preds@ restricted to the names present in @partials@; if the expansion is exact
-- (no elements outside those names) and the result is a subset of @partials@, returns 'True'.
-- When the restricted expansion is inexact ('hasOutside = True'), re-expands with all known names:
-- if that full expansion is empty the predicate is contradictory (U_preds = ∅ ⊆ partials).
predImpliesPartials :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicates -> PartialLeafs -> Bool
predImpliesPartials typeEnv@TypeEnv{teNames} vaenv preds partials =
  case expandTypesWithNamesFull typeEnv vaenv (TopType H.empty preds) (H.keysSet partials) of
    (expanded, False) -> isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing expanded []) (UnionType Nothing partials [])
    (_,        True)  ->
      -- Conservative: types fell outside allowedNames.
      -- Last-resort: re-expand with all known names. If the result is empty (contradictory
      -- predicates), U_preds = ∅ ⊆ partials holds.
      let allNames = teNames `S.union` H.keysSet partials
          (fullLeafs, fullHasOutside) = expandTypesWithNamesFull typeEnv vaenv (TopType H.empty preds) allNames
      in not fullHasOutside && H.null fullLeafs

-- | Returns only the 'PartialLeafs' of a type whose names are in 'allowedNames'.
expandTypeWithNames :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> S.HashSet TypeName -> PartialLeafs
expandTypeWithNames typeEnv vaenv t allowedNames = fst $ expandTypesWithNamesFull typeEnv vaenv t allowedNames

-- | Like 'expandTypeWithNames', but also returns a 'Bool' indicating whether the expansion
-- is approximate or has components outside 'allowedNames'. 'True' means the expansion is
-- approximate and the result may be incomplete. 'False' means the expansion is exact and all
-- components are within 'allowedNames'.
expandTypesWithNamesFull :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> S.HashSet TypeName -> (PartialLeafs, Bool)
expandTypesWithNamesFull _ _ (UnionType Nothing partials []) allowedNames =
  (filtered, hasOutside)
  where
    filtered = H.filterWithKey (\k _ -> S.member k allowedNames) partials
    hasOutside = any (\k -> not $ S.member k allowedNames) (H.keys partials)
expandTypesWithNamesFull typeEnv vaenv (TypeVar v _) allowedNames =
  expandTypesWithNamesFull typeEnv vaenv (H.lookupDefault PTopType v vaenv) allowedNames
expandTypesWithNamesFull TypeEnv{teNames} _ PTopType allowedNames =
  (topFilteredLeafs, not $ S.isSubsetOf teNames allowedNames)
  where
    topFilteredLeafs = joinUnionType $ map ((\p -> p{ptArgMode=PtArgAny}) . partialVal) $ S.toList allowedNames
-- Constants carry no partial-name information; drop them before filtering
expandTypesWithNamesFull typeEnv vaenv (UnionType mTop leafs (_:_)) allowedNames =
  expandTypesWithNamesFull typeEnv vaenv (UnionType mTop leafs []) allowedNames
expandTypesWithNamesFull typeEnv@TypeEnv{teNames} vaenv (TopType negPartials preds) allowedNames =
  let (leafs, hasOutside) = expandPredsWithNamesFull preds
  in (subtractNeg leafs, hasOutside)
  where
    topFilteredLeafs = joinUnionType $ map ((\p -> p{ptArgMode=PtArgAny}) . partialVal) $ S.toList allowedNames
    hasUniverseOutside = not $ S.isSubsetOf teNames allowedNames

    expandPredsWithNamesFull :: TypePredicates -> (PartialLeafs, Bool)
    expandPredsWithNamesFull (PredsAnd []) = (topFilteredLeafs, hasUniverseOutside)
    expandPredsWithNamesFull (PredsAnd ps) =
      let mapped = map expandPredsWithNamesFull ps
          leafs = foldl1 (intersectPartialLeafsEnv typeEnv vaenv) (map fst mapped)
      in (leafs, all snd mapped)
    expandPredsWithNamesFull (PredsNot inner) =
      let (innerLeafs, _) = expandPredsWithNamesFull inner
      in case differencePartialLeafs typeEnv vaenv topFilteredLeafs innerLeafs of
           UnionType Nothing result [] -> (result, hasUniverseOutside)
           _                           -> (topFilteredLeafs, hasUniverseOutside)
    expandPredsWithNamesFull (PredsOne (PredClass clss)) =
      expandClassPartialWithNamesFull typeEnv vaenv clss allowedNames
    expandPredsWithNamesFull (PredsOne (PredRel rel)) =
      case expandRelPartial typeEnv vaenv rel of
        UnionType Nothing leafs [] ->
          (H.filterWithKey (\k _ -> S.member k allowedNames) leafs,
           any (\k -> not $ S.member k allowedNames) (H.keys leafs))
        t -> expandTypesWithNamesFull typeEnv vaenv t allowedNames
    expandPredsWithNamesFull (PredsOne (PredExpr e)) =
      case typeGraphExpandPredExpr typeEnv vaenv e of
        Nothing ->
          let filteredPartials = filter (\PartialType{ptName=n} -> S.member n allowedNames) $ topTypeAsPartials typeEnv
              leafsWithPred = joinUnionType $ map (`partialAddPreds` PredsOne (PredExpr e)) filteredPartials
          in (leafsWithPred, hasUniverseOutside)
        Just ps ->
          let fullLeafs = joinUnionType ps
          in (H.filterWithKey (\k _ -> S.member k allowedNames) fullLeafs,
              any (\k -> not $ S.member k allowedNames) (H.keys fullLeafs))

    subtractNeg :: PartialLeafs -> PartialLeafs
    subtractNeg leafs
      | H.null negPartials = leafs
      | otherwise =
          let negFiltered = H.filterWithKey (\k _ -> S.member k allowedNames) negPartials
          in case differencePartialLeafs typeEnv vaenv leafs negFiltered of
               UnionType Nothing result [] -> result
               _                           -> leafs
-- Mixed type (both top and pos non-empty): distribute expansion over union
expandTypesWithNamesFull typeEnv vaenv (UnionType (Just topData) pos []) allowedNames
  | not (H.null pos) =
      let (topLeafs, topHasOutside) = expandTypesWithNamesFull typeEnv vaenv (UnionType (Just topData) H.empty []) allowedNames
          (posLeafs, posHasOutside) = expandTypesWithNamesFull typeEnv vaenv (UnionType Nothing pos []) allowedNames
      in (H.unionWith S.union topLeafs posLeafs, topHasOutside || posHasOutside)
expandTypesWithNamesFull _ _ t _ = error $ printf "expandTypesWithNamesFull: unexpected type %s" (show t)

expandClassPartialWithNames :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> S.HashSet TypeName -> PartialLeafs
expandClassPartialWithNames typeEnv vaenv clss allowedNames = fst $ expandClassPartialWithNamesFull typeEnv vaenv clss allowedNames

expandClassPartialWithNamesFull :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> S.HashSet TypeName -> (PartialLeafs, Bool)
expandClassPartialWithNamesFull typeEnv@TypeEnv{teClassGraph=ClassGraph cg} vaenv PartialType{ptName, ptVars=classVarsP} allowedNames =
  case graphLookup (PClassName ptName) cg of
    Just (CGClass (_, PartialType{ptVars=classVarsDecl}, classTypes, _)) -> case classTypes of
      [] ->
        let leafs = joinUnionType [classPlaceholderLeaf ptName]
        in (H.filterWithKey (\k _ -> S.member k allowedNames) leafs,
            any (\k -> not $ S.member k allowedNames) (H.keys leafs))
      _  ->
        let mapped = map mapClassTypeWithNamesFull classTypes
            leafs = foldl (H.unionWith S.union) H.empty (map fst mapped)
        in (leafs, any snd mapped)
      where
        classVars = H.unionWith (intersectTypesEnv typeEnv vaenv) classVarsP classVarsDecl
        mapClassTypeWithNamesFull (TopType negPartials ps) =
          expandTypesWithNamesFull typeEnv vaenv (TopType negPartials ps) allowedNames
        mapClassTypeWithNamesFull (TypeVar (TVVar t) _) = case H.lookup t classVars of
          Just v  -> expandTypesWithNamesFull typeEnv vaenv (intersectTypesEnv typeEnv vaenv v (H.lookupDefault PTopType t classVars)) allowedNames
          Nothing -> error $ printf "Unknown var %s in expandPartial" (show t)
        mapClassTypeWithNamesFull (TypeVar (TVArg t) _) = error $ printf "Arg %s found in expandPartial" (show t)
        mapClassTypeWithNamesFull (UnionType Nothing p []) =
          let fullList = splitUnionType p
              filteredList = filter (\PartialType{ptName=n} -> S.member n allowedNames) fullList
              hasOut = any (\PartialType{ptName=n} -> not $ S.member n allowedNames) fullList
          in (joinUnionType $ map mapClassPartial filteredList, hasOut)
        mapClassTypeWithNamesFull t = expandTypesWithNamesFull typeEnv vaenv t allowedNames
        mapClassPartial tp@PartialType{ptVars} = tp{ptVars=fmap (substituteVarsWithVarEnv classVars) ptVars}
    r -> error $ printf "Unknown class %s in expandPartial. Found %s" (show (PClassName ptName)) (show r)


expandClassPartial :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> Type
expandClassPartial typeEnv@TypeEnv{teClassGraph=ClassGraph cg} vaenv PartialType{ptName, ptVars=classVarsP} = expanded
  where
    className = PClassName ptName
    expanded = case graphLookup className cg of
      Just (CGClass (_, PartialType{ptVars=classVarsDecl}, classTypes, _)) -> case classTypes of
        [] -> singletonType $ classPlaceholderLeaf ptName
        _  -> unionAllTypes typeEnv $ map mapClassType classTypes
        where
          classVars = H.unionWith (intersectTypesEnv typeEnv vaenv) classVarsP classVarsDecl
          mapClassType (TypeVar (TVVar t) _) = case H.lookup t classVars of
            Just v -> expandNeg $ intersectTypesEnv typeEnv vaenv v (H.lookupDefault PTopType t classVars)
            Nothing -> error $ printf "Unknown var %s in expandPartial" (show t)
          mapClassType (TypeVar (TVArg t) _) = error $ printf "Arg %s found in expandPartial" (show t)
          mapClassType (UnionType Nothing p []) = UnionType Nothing (joinUnionType $ map mapClassPartial $ splitUnionType p) []
          mapClassType t@(UnionType Nothing _ _) = t
          mapClassType t@(UnionType (Just _) _ _) = t
          expandNeg t@(UnionType (Just _) _ _) = t
          expandNeg (TypeVar tv _) = H.lookupDefault PTopType tv vaenv
          expandNeg t              = t
          mapClassPartial tp@PartialType{ptVars} = tp{ptVars=fmap (substituteVarsWithVarEnv classVars) ptVars}
      r -> error $ printf "Unknown class %s in expandPartial. Found %s" (show className) (show r)

expandRelPartial :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> Type
expandRelPartial typeEnv@TypeEnv{teNames} vaenv relPartial = unionAllTypesWithEnv typeEnv vaenv (UnionType Nothing (joinUnionType fromArgs) [] : fromTypeEnv)
  where
    name = ptName relPartial
    fromTypeEnv = typeEnvNamesMatching
    fromArgs = map (\n -> relPartial{ptName=n}) $ relativeNameFilter name $ map pkName $ H.keys $ snd $ splitVarArgEnv vaenv


    typeEnvNamesMatching = tpNames ++ clsNames
    relName = ptName relPartial
    tpNames = map (\n -> singletonType relPartial{ptName=n}) $ relativeNameFilter relName $ S.toList teNames
    clsNames = map (setArgMode H.empty (ptArgMode relPartial) . classPartial . partialVal) $ relativeNameFilter relName $ listClassNames typeEnv

isSubPredicateOfWithEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicate -> TypePredicate -> Bool
isSubPredicateOfWithEnv typeEnv vaenv p1 p2 = predImpliesAtom typeEnv vaenv p1 p2

-- | A private helper for 'isSubPartialOfWithEnv' that checks while ignore class expansions
isSubPartialOfWithEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> Bool
isSubPartialOfWithEnv TypeEnv{teDebug=True} _ l r | trace (printf "[SUBPARTIAL] %s ⊆ %s" (show l) (show r)) False = undefined
isSubPartialOfWithEnv _ _ PartialType{ptName=subName} PartialType{ptName=superName} | subName /= superName = False
isSubPartialOfWithEnv _ _ PartialType{ptArgs=subArgs, ptArgMode=PtArgExact} PartialType{ptArgs=superArgs, ptArgMode=PtArgExact} | H.keysSet subArgs /= H.keysSet superArgs = False
isSubPartialOfWithEnv _ _ PartialType{ptArgs=subArgs, ptArgMode=PtArgAny} PartialType{ptArgs=superArgs, ptArgMode=PtArgAny} | not (H.keysSet superArgs `isSubsetOf` H.keysSet subArgs) = False
isSubPartialOfWithEnv _ _ PartialType{ptArgs=subArgs} PartialType{ptArgs=superArgs, ptArgMode=superArgMode} | superArgMode == PtArgExact && not (H.keysSet subArgs `isSubsetOf` H.keysSet superArgs) = False
isSubPartialOfWithEnv typeEnv vaenv sub@PartialType{ptVars=subVars, ptArgs=subArgs, ptPreds=subPreds} super@PartialType{ptVars=superVars, ptArgs=superArgs, ptPreds=superPreds} = debugTrace typeEnv msg result
  where
    vaenv' = H.union vaenv (substituteWithVarArgEnv vaenv <$> H.unionWith (intersectTypes typeEnv) (ptVarArg super) (ptVarArg sub))
    hasAll sb sp = and $ H.elems $ H.intersectionWith (isSubtypeOfWithEnv typeEnv vaenv') sb sp
    result = hasAll subArgs superArgs && hasAllPreds superPreds subPreds && hasAll subVars superVars
    msg = printf "[SUBPARTIAL] %s ⊆ %s = %s" (show sub) (show super) (show result)

    hasAllPreds :: TypePredicates -> TypePredicates -> Bool
    hasAllPreds (PredsAnd subPs) supPs = all (`hasAllPreds` supPs) subPs
    hasAllPreds subPs@PredsOne{} (PredsAnd supPs) = any (hasAllPreds subPs) supPs
    hasAllPreds (PredsOne subPs) (PredsOne supPs) = isSubPredicateOfWithEnv typeEnv vaenv' subPs supPs
    hasAllPreds p PredsNone = p == PredsNone
    hasAllPreds subPs supPs = error $ printf "Unimplemented hasAllPreds for sub (%s) and sup (%s)" (show subPs) (show supPs)

-- | Checks if a constant value is a member of a type.
-- A constant is in a type if it appears in the constant list, or its parent partial type is in the type.
constantInType :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Constant -> Type -> Bool
constantInType _ _ _ PTopType = True
constantInType _ _ c (UnionType Nothing leafs cs) =
  c `elem` cs || isSubtypeOfWithEnv emptyTypeEnv' H.empty (singletonType $ constantPartialType c) (UnionType Nothing leafs [])
-- NegPartials: c is in the type iff not excluded by negConsts, and either covered by posLeafs or
-- (not covered by negLeafs and parent is in the predicated set)
constantInType typeEnv vaenv c (UnionType (Just (preds, negLeafs)) posLeafs negConsts) =
  c `notElem` negConsts
  && ( isSubtypeOfWithEnv emptyTypeEnv' H.empty (singletonType $ constantPartialType c) (UnionType Nothing posLeafs [])
     || ( not (isSubtypeOfWithEnv emptyTypeEnv' H.empty (singletonType $ constantPartialType c) (UnionType Nothing negLeafs []))
          && isSubtypeOfWithEnv typeEnv vaenv (singletonType $ constantPartialType c) (UnionType (Just (preds, H.empty)) H.empty [])
        )
     )
constantInType _ _ _ _ = False

-- | Checks if one type contains another type. In set terminology, it is equivalent to subset or equal to ⊆.
isSubtypeOfWithEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Bool
isSubtypeOfWithEnv TypeEnv{teDebug=True} _ l r | trace (printf "[SUBTYPE] %s ⊆ %s" (show l) (show r)) False = undefined
isSubtypeOfWithEnv _ _ _ PTopType = True
isSubtypeOfWithEnv _ _ t1 t2 | t1 == t2 = True
isSubtypeOfWithEnv typeEnv vaenv (TypeVar v _) t2 = isSubtypeOfWithEnv typeEnv vaenv (vaenvLookup vaenv v) t2
isSubtypeOfWithEnv typeEnv vaenv t1 (TypeVar v _) = isSubtypeOfWithEnv typeEnv vaenv t1 (vaenvLookup vaenv v)
isSubtypeOfWithEnv _ _ PTopType t = t == PTopType
-- Mixed type on left (top + non-empty positive leafs): (top ∪ posLeafs) ⊆ t2 iff top ⊆ t2 ∧ posLeafs ⊆ t2
isSubtypeOfWithEnv typeEnv vaenv (UnionType (Just topData) pos cs) t2
  | not (H.null pos) =
      isSubtypeOfWithEnv typeEnv vaenv (UnionType (Just topData) H.empty cs) t2
      && isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing pos []) t2
-- PosPartials ⊆ NegPartials: filter by name.
-- filteredLeafs is always exact for names in allowedNames; hasOutside only means the type
-- has additional elements outside those names, which cannot affect subtype checking here.
isSubtypeOfWithEnv typeEnv vaenv t1@(UnionType Nothing subPartials []) t2@(UnionType (Just _) _ _) =
  let (filteredLeafs, _) = expandTypesWithNamesFull typeEnv vaenv t2 (H.keysSet subPartials)
  in isSubtypeOfWithEnv typeEnv vaenv t1 (UnionType Nothing filteredLeafs [])
-- PosPartials-with-constants ⊆ t2: partials check + each constant must be in t2
-- (Only for PosPartials: in NegPartials, cs are excluded constants, not members)
isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing leafs cs@(_:_)) t2 =
  isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing leafs []) t2
  && all (\c -> constantInType typeEnv vaenv c t2) cs
-- NegPartials ⊆ NegPartials: (U_p1-n1) ⊆ (U_p2-n2).
-- Same predicates: sufficient iff n2 ⊆ n1.
-- Differing predicates: sufficient iff predImplies p1 p2 and n2 ⊆ n1; otherwise False.
isSubtypeOfWithEnv typeEnv vaenv (UnionType (Just (p1, n1)) _ _) t2@(UnionType (Just (p2, n2)) _ _)
  | p1 == p2             = isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing n2 []) (UnionType Nothing n1 [])
  | isPredsContradictory p1 = True  -- t1 = ∅ ⊆ anything
  | isPredsContradictory (predsAnd p1 (predsNot p2)) = True  -- p1 ≤ p2 syntactically (p1 ∧ ¬p2 = ∅)
  | predImplies typeEnv vaenv p1 p2
    && isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing n2 []) (UnionType Nothing n1 []) = True
    -- U_p1 ⊆ U_p2 and n2 ⊆ n1, so (U_p1 - n1) ⊆ (U_p2 - n2)
  | tePrgmEnv typeEnv = isSubtypeOfWithEnv typeEnv vaenv (snd $ differenceTypeWithEnv typeEnv vaenv (expandPredicates typeEnv vaenv p1) (UnionType Nothing n1 [])) t2
  | otherwise         = False
-- NegPartials ⊆ PosPartials: filter t1 by superPartials names.
-- hasOutside=True: t1 has elements outside superPartials names.
--   True if preds are syntactically contradictory (t1 = ∅).
--   True if predImpliesPartials establishes U_preds ⊆ superPartials (negLeafs removal can only shrink).
--   Otherwise False (conservative: can't prove subtype without knowing predicate members).
isSubtypeOfWithEnv typeEnv vaenv t1@(UnionType (Just (preds, negLeafs)) _ _) t2@(UnionType Nothing superPartials _) =
  case expandTypesWithNamesFull typeEnv vaenv t1 (H.keysSet superPartials) of
    (_, True)      | isPredsContradictory preds -> True
    (_, True)      | predImpliesPartials typeEnv vaenv preds superPartials -> True
                   -- U_preds ⊆ superPartials implies (U_preds - negLeafs) ⊆ superPartials
    (_, True)      | tePrgmEnv typeEnv -> isSubtypeOfWithEnv typeEnv vaenv (snd $ differenceTypeWithEnv typeEnv vaenv (expandPredicates typeEnv vaenv preds) (UnionType Nothing negLeafs [])) t2
    (_, True)      -> False
    (leafs, False) -> isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing leafs []) t2
-- PosPartials ⊆ PosPartials: check partials structurally
isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing subPartials []) (UnionType Nothing superPartials []) =
  debugTrace typeEnv msg result
  where
    supersByName = splitUnionTypeByName superPartials
    subsByName   = splitUnionTypeByName subPartials
    result = H.keysSet subsByName `S.isSubsetOf` H.keysSet supersByName
          && and (H.intersectionWith areSubPartials subsByName supersByName)
    areSubPartials subList superList = all (isSubPartial superList) subList
    isSubPartial sup sub = directCheck || splitCheck || expandSplitCheck
      where
        directCheck = any (isSubPartialOfWithEnv typeEnv vaenv sub) sup
        splitSubs = splitPartialByUnionVarArgs sub
        splitCheck = length splitSubs > 1
                     && all (\s -> any (isSubPartialOfWithEnv typeEnv vaenv s) sup) splitSubs
        expandedSub = sub{ptArgs = fmap expandArg (ptArgs sub), ptVars = fmap expandArg (ptVars sub)}
          where expandArg (UnionType (Just (preds, negLeafs)) _ _) =
                  snd $ differenceTypeWithEnv typeEnv vaenv (expandPredicates typeEnv vaenv preds) (UnionType Nothing negLeafs [])
                expandArg t = t
        expandedSplitSubs = splitPartialByUnionVarArgs expandedSub
        expandSplitCheck = length expandedSplitSubs > 1
                           && all (\s -> any (isSubPartialOfWithEnv typeEnv vaenv s) sup) expandedSplitSubs
    msg = printf "[SUBTYPE] %s ⊆ %s = %s" (show $ UnionType Nothing subPartials []) (show $ UnionType Nothing superPartials []) (show result)
-- partials ⊆ (superLeafs ∪ constants): constants in supertype don't help cover partials
isSubtypeOfWithEnv typeEnv vaenv t1@(UnionType Nothing _ []) (UnionType Nothing superLeafs (_:_)) =
  isSubtypeOfWithEnv typeEnv vaenv t1 (UnionType Nothing superLeafs [])

-- | Checks if one type contains another type. In set terminology, it is equivalent to subset or equal to ⊆.
isSubtypeOf :: TypeGraph tg => TypeEnv tg -> Type -> Type -> Bool
isSubtypeOf typeEnv = isSubtypeOfWithEnv typeEnv H.empty

-- | Checks if one type contains another type. In set terminology, it is equivalent to subset or equal to ⊆.
isSubtypePartialOf :: TypeGraph tg => TypeEnv tg -> PartialType -> Type -> Bool
isSubtypePartialOf typeEnv subPartial = isSubtypeOf typeEnv (singletonType subPartial)

isEqType :: TypeGraph tg => TypeEnv tg -> Type -> Type -> Bool
isEqType TypeEnv{teDebug=True} a b | trace (printf "[EQ] %s = %s" (show a) (show b)) False = undefined
isEqType _ a b | a == b = True
isEqType typeEnv a b = isSubtypeOf typeEnv a b && isSubtypeOf typeEnv b a

isEqTypeWithEnv :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Bool
isEqTypeWithEnv _ _ a b | a == b = True
isEqTypeWithEnv typeEnv vaenv a b = isSubtypeOfWithEnv typeEnv vaenv a b && isSubtypeOfWithEnv typeEnv vaenv b a

-- |
-- Join partials by checking if one is a subset of another (redundant) and removing it.
-- TODO: This currently joins only with matching names. More matches could improve the effectiveness of the compaction, but slows down the code significantly
compactOverlapping :: TypeGraph tg => TypeEnv tg -> PartialLeafs -> PartialLeafs
compactOverlapping typeEnv = joinUnionTypeByName . fmap ((aux . reverse) . aux) . splitUnionTypeByName
  where
    -- Tests each partial against all following partials to check if it is already handled by it
    aux [] = []
    aux (partial:rest) = if any (partialAlreadyCovered partial) rest
      then aux rest
      else partial : aux rest

    -- checks if a partial is covered by the candidate from rest
    partialAlreadyCovered PartialType{ptArgMode=PtArgAny} PartialType{ptArgMode=PtArgAny} = False -- Don't count any coverings because it loses type information
    partialAlreadyCovered partial restPartial = isSubtypePartialOf typeEnv partial (singletonType restPartial)

-- |
-- Joins partials with only one difference between their args or vars. Then, it can join the two partials into one partial
-- TODO: Should check if preds are suitable for joining
compactJoinPartials :: TypeGraph tg => TypeEnv tg -> PartialLeafs -> PartialLeafs
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
    tryJoin (PartialType name1 vars1 args1 PredsNone mode1) (PartialType _ vars2 args2 PredsNone _) = if numDifferences args1 args2 + numDifferences vars1 vars2 == 1
      then Just $ PartialType name1 (joinMap vars1 vars2) (joinMap args1 args2) PredsNone mode1
      else Nothing
    tryJoin _ _ = Nothing

    numDifferences m1 m2 = sum $ fromEnum <$> H.intersectionWith (/=) m1 m2
    joinMap = H.unionWith (unionTypes typeEnv)

-- | Processes partials that have a 'PredClass' predicate
compactPartialsWithClassPred :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialLeafs -> PartialLeafs
compactPartialsWithClassPred typeEnv vaenv partials = unionPartialLeafs $ map tryAux $ splitUnionType partials
  where
    tryAux :: PartialType -> PartialLeafs
    tryAux partial = case tryPredsToList (ptPreds partial) of
      Just []    -> joinUnionType [partial]
      Nothing    -> joinUnionType [partial]
      Just preds -> aux (partial, preds)

    aux :: (PartialType, [TypePredicate]) -> PartialLeafs
    aux (partial, preds) = if null classPreds && null relPreds
      then joinUnionType [partial]
      else asLeafs $ intersectAllTypes typeEnv (singletonType partial' : map (expandRelPartial typeEnv vaenv) relPreds ++ map (expandClassPartial typeEnv vaenv) classPreds)
      where
        -- Take class preds out of partial
        (exprPreds, classPreds, relPreds) = splitPreds preds
        partial' = partial{ptPreds = PredsAnd $ map (PredsOne . PredExpr) exprPreds}

        asLeafs (UnionType Nothing leafs []) = leafs
        asLeafs (UnionType (Just (_, negLeafs)) posLeafs []) = H.unionWith S.union negLeafs posLeafs  -- shouldn't normally occur
        asLeafs t = error $ printf "compactPartialsWithClassPred: unexpected type %s" (show t)

        splitPreds :: [TypePredicate] -> ([PartialType], [PartialType], [PartialType])
        splitPreds [] = ([], [], [])
        splitPreds (p:ps) = case p of
          PredExpr p'  -> (p':exprs', classes', rels')
          PredClass p' -> (exprs', p':classes', rels')
          PredRel p'   -> (exprs', classes', p':rels')
          where
            (exprs', classes', rels') = splitPreds ps

-- | Checks if a partial has predicates that turn the partial into a 'bottomType' (e.g. a partial with contradictory preds such as isClass A and not isClass A)
compactDisconnectedPreds :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialLeafs -> PartialLeafs
compactDisconnectedPreds typeEnv vaenv partials = joinUnionType $ mapMaybe aux $ splitUnionType partials
  where
    aux partial@PartialType{ptPreds=PredsNone} = Just partial
    aux partial@PartialType{ptPreds} = case intersectTypesEnv typeEnv{teDisableCompact=True} vaenv (singletonType partialWithoutPreds) (TopType H.empty ptPreds) of

                                        -- | If the intersection of the partial with its preds has no overlap then it is a bottom type
                                        intersection | isBottomType intersection -> Nothing

                                        -- | If the intersection of the partial with its preds fully overlaps, then the preds are redundant
                                        intersection | isEqType typeEnv intersection (singletonType partialWithoutPreds) -> Just partialWithoutPreds

                                        _ -> Just partial
      where
        partialWithoutPreds = partial{ptPreds = PredsNone}

-- | Removes partials which contain a type variable that is the 'bottomType', because then the whole partial is a 'bottomType'.
compactBottomType :: PartialLeafs -> PartialLeafs
compactBottomType partials = joinUnionType $ mapMaybe aux $ splitUnionType partials
  where
    aux partial = if containsBottomPartialType partial
      then Nothing
      else Just partial

compactPartialLeafs :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> PartialLeafs -> PartialLeafs
compactPartialLeafs typeEnv vaenv = compactOverlapping typeEnv . compactJoinPartials typeEnv . compactPartialsWithClassPred typeEnv vaenv . compactDisconnectedPreds typeEnv vaenv . compactBottomType

-- |
-- Used to simplify and reduce the size of a 'Type'.
-- TODO: This should merge type partials into class partials
compactType :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
compactType TypeEnv{teDisableCompact=True} _ t = t
-- Top component with empty neg and non-trivial preds
compactType typeEnv vaenv t@(UnionType (Just (preds, negPartials)) posLeafs [])
  | H.null negPartials && H.null posLeafs && preds /= PredsNone = case preds of
      (PredsNot (PredsOne (PredRel rp))) -> case expandRelPartial typeEnv vaenv rp of
        (UnionType Nothing rp' []) -> UnionType (Just (PredsNone, rp')) H.empty []
        _               -> t
      -- ¬(contradictory) = PredsNone = everything → PTopType
      (PredsNot p) | isPredsContradictory p -> PTopType
      _ -> t
-- Top component with non-empty neg and non-trivial preds
compactType typeEnv vaenv t@(UnionType (Just (preds, negPartials)) posLeafs [])
  | not (H.null negPartials) && H.null posLeafs && preds /= PredsNone =
      if isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing negPartials []) (TopType H.empty (predsNot preds))
        then UnionType (Just (preds, H.empty)) H.empty []
        else t
-- ¬(contradictory) = everything: top component is U, simplify to PTopType
compactType typeEnv vaenv (UnionType (Just (PredsNot p, negLeafs)) posLeafs cs)
  | isPredsContradictory p = compactType typeEnv vaenv (UnionType (Just (PredsNone, negLeafs)) posLeafs cs)
-- Contradictory predicates (e.g. PredsAnd[p, ¬p]): top component is empty, reduce to posLeafs only
compactType typeEnv vaenv (UnionType (Just (preds, _)) pos _)
  | isPredsContradictory preds = compactType typeEnv vaenv (UnionType Nothing pos [])
-- Cancel pos/neg overlap: (T - neg) ∪ pos = (T - (neg∖pos)) ∪ (pos∖neg)
-- e.g. (U - {X}) ∪ {X} = U
compactType typeEnv vaenv (UnionType (Just (preds, negLeafs)) posLeafs cs)
  | not (H.null posLeafs), not (H.null negLeafs)
  , let common = H.filter (not . S.null) $ H.intersectionWith S.intersection posLeafs negLeafs
  , not (H.null common) =
      let subtractCommon leafs = H.filter (not . S.null) $ H.differenceWith (\a b -> Just (S.difference a b)) leafs common
      in compactType typeEnv vaenv $ UnionType (Just (preds, subtractCommon negLeafs)) (subtractCommon posLeafs) cs
-- PredsAnd: remove PredRel atoms dominated by a PredClass atom in the same conjunction.
-- e.g. PredsAnd[PredClass /Boolean, PredRel Boolean] → PredsOne (PredClass /Boolean)
-- Soundness: expandRelPartial r includes classPartial c in its expansion, so U_PredClass c ⊆ U_PredRel r
-- when r's name path is a suffix of c's name path.
compactType typeEnv vaenv (UnionType (Just (PredsAnd ps, negPartials)) posLeafs cs)
  | not (null predClassAtoms)
  , let simplified = filter (not . isDominatedByPredClass) ps
  , length simplified < length ps =
      -- Recurse: might simplify further (e.g. single-element PredsAnd → PredsOne)
      compactType typeEnv vaenv $ UnionType (Just (mkAnd simplified, negPartials)) posLeafs cs
  where
    predClassAtoms = [c | PredsOne (PredClass c) <- ps]
    -- A PredRel r is dominated if some PredClass c in the conjunction has r's path as a suffix
    isDominatedByPredClass (PredsOne (PredRel r)) =
      any (\c -> splitOn "/" (ptName r) `L.isSuffixOf` splitOn "/" (ptName c)) predClassAtoms
    isDominatedByPredClass _                      = False
-- Top component catch-all (includes PTopType and TopType np PredsNone); preserve constants
compactType _ _ t@(UnionType (Just _) _ _) = t
-- TypeVar: unchanged
compactType _ _ t@TypeVar{} = t
-- PosPartials with constants: compact leafs, remove constants covered by leafs
compactType typeEnv vaenv (UnionType Nothing partials consts) =
  let compactedLeafs = compactPartialLeafs typeEnv vaenv partials
      -- Remove constants whose parent partial type is already in the compacted leafs
      filteredConsts = filter (\c -> not $ isSubtypeOfWithEnv emptyTypeEnv' H.empty (singletonType $ constantPartialType c) (UnionType Nothing compactedLeafs [])) consts
  in UnionType Nothing compactedLeafs filteredConsts

unionPartialLeafs :: Foldable f => f PartialLeafs -> PartialLeafs
unionPartialLeafs = unionsWith S.union

-- | Takes the union of two types (∪).
unionTypesWithEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Type
unionTypesWithEnv TypeEnv{teDebug=True} _ l r | trace (printf "[UNION] %s ∪ %s" (show l) (show r)) False = PTopType
unionTypesWithEnv _ _ PTopType _ = PTopType
unionTypesWithEnv _ _ _ PTopType = PTopType
unionTypesWithEnv _ _ t BottomType = t
unionTypesWithEnv _ _ BottomType t = t
unionTypesWithEnv _ _ t1 t2 | t1 == t2 = t1
-- TypeVar ∪ t → look up in vaenv
unionTypesWithEnv typeEnv vaenv (TypeVar v _) t = case H.lookup v vaenv of
  Just v' -> unionTypesWithEnv typeEnv vaenv v' t
  Nothing -> error $ printf "unionTypesWithEnv: unknown type var %s with %s in env %s" (show t) (show v) (show $ H.keys vaenv)
unionTypesWithEnv typeEnv vaenv t v@TypeVar{} = unionTypesWithEnv typeEnv vaenv v t
-- Structured union: separate handling for positive and top components.
-- Positive constants from Nothing side are checked against the top; if covered, dropped (already included).
-- Top components: same preds → neg leafs intersect; PredsNone ∪ anything → PredsNone with intersected negs;
-- different preds → De Morgan normalization: p1∨p2 = ¬(¬p1∧¬p2) via predsNot/predsAnd (flat, depth 1).
unionTypesWithEnv typeEnv vaenv (UnionType mTop1 pos1 cs1) (UnionType mTop2 pos2 cs2) =
  debugTrace typeEnv msg result
  where
    pos'  = unionPartialLeafs [pos1, pos2]
    result = case (mTop1, mTop2) of
      (Nothing, Nothing) ->
        compactType typeEnv vaenv $ UnionType Nothing pos' (L.nub (cs1 ++ cs2))
      (Nothing, Just (p2, n2)) ->
        -- cs1 are positive constants (from Nothing side); cs2 are negative constants (excluded from top)
        let notInTop = filter (\c -> not $ constantInType typeEnv vaenv c (UnionType (Just (p2, n2)) H.empty cs2)) cs1
        in if null notInTop
           then compactType typeEnv vaenv $ UnionType (Just (p2, n2)) pos' cs2
           else -- Positive constants not covered by top: expand top to concrete Nothing form
                let topExpanded = snd $ differenceTypeWithEnv typeEnv vaenv
                                          (expandPredicates typeEnv vaenv p2)
                                          (UnionType Nothing n2 [])
                in unionTypesWithEnv typeEnv vaenv
                     (UnionType Nothing pos1 cs1)
                     (unionTypesWithEnv typeEnv vaenv topExpanded (UnionType Nothing pos2 []))
      (Just (p1, n1), Nothing) ->
        -- cs1 are negative constants (excluded from top); cs2 are positive constants (from Nothing side)
        let notInTop = filter (\c -> not $ constantInType typeEnv vaenv c (UnionType (Just (p1, n1)) H.empty cs1)) cs2
        in if null notInTop
           then compactType typeEnv vaenv $ UnionType (Just (p1, n1)) pos' cs1
           else -- Positive constants not covered by top: expand top to concrete Nothing form
                let topExpanded = snd $ differenceTypeWithEnv typeEnv vaenv
                                          (expandPredicates typeEnv vaenv p1)
                                          (UnionType Nothing n1 [])
                in unionTypesWithEnv typeEnv vaenv
                     (unionTypesWithEnv typeEnv vaenv topExpanded (UnionType Nothing pos1 []))
                     (UnionType Nothing pos2 cs2)
      (Just (p1, n1), Just (p2, n2))
        | p1 == p2 ->
            compactType typeEnv vaenv $
              UnionType (Just (p1, snd $ intersectPartialLeafsWithVarEnv typeEnv vaenv n1 n2)) pos' (L.nub (cs1 ++ cs2))
        | p1 == PredsNone || p2 == PredsNone ->
            -- One side is universal top (PredsNone): U ∪ X = U minus the parts excluded from both
            compactType typeEnv vaenv $
              UnionType (Just (PredsNone, snd $ intersectPartialLeafsWithVarEnv typeEnv vaenv n1 n2)) pos' (L.nub (cs1 ++ cs2))
        | otherwise ->
            -- De Morgan with predsNot/predsAnd normalization: (U_p1) ∪ (U_p2) = U_{p1∨p2}
            -- where p1∨p2 = ¬(¬p1 ∧ ¬p2). Using predsNot/predsAnd keeps structure flat (depth 1).
            compactType typeEnv vaenv $
              UnionType (Just (predsNot $ predsAnd (predsNot p1) (predsNot p2), snd $ intersectPartialLeafsWithVarEnv typeEnv vaenv n1 n2)) pos' (L.nub (cs1 ++ cs2))
    msg = printf "[UNION] %s ∪ %s = %s" (show $ UnionType mTop1 pos1 cs1) (show $ UnionType mTop2 pos2 cs2) (show result)

unionTypes :: TypeGraph tg => TypeEnv tg -> Type -> Type -> Type
unionTypes typeEnv = unionTypesWithEnv typeEnv H.empty

-- | Takes the 'unionTypes' of many types
unionAllTypesWithEnv :: (TypeGraph tg, Foldable f) => TypeEnv tg -> TypeVarArgEnv -> f Type -> Type
unionAllTypesWithEnv typeEnv vaenv = foldr (unionTypesWithEnv typeEnv vaenv) BottomType

-- | Takes the 'unionTypes' of many types
unionAllTypes :: (TypeGraph tg, Foldable f) => TypeEnv tg -> f Type -> Type
unionAllTypes typeEnv = foldr (unionTypes typeEnv) BottomType

-- | Takes the 'intersectTypes' of many types
intersectAllTypesWithEnv :: (TypeGraph tg, Foldable f) => TypeEnv tg -> TypeVarArgEnv -> f Type -> Type
intersectAllTypesWithEnv _ _ types | null types = BottomType
intersectAllTypesWithEnv typeEnv vaenv types = foldr1 (intersectTypesEnv typeEnv vaenv) types

-- | Takes the 'intersectTypes' of many types
intersectAllTypes :: (TypeGraph tg, Foldable f) => TypeEnv tg -> f Type -> Type
intersectAllTypes _ types | null types = BottomType
intersectAllTypes typeEnv types = foldr1 (intersectTypes typeEnv) types

-- | A private helper for 'intersectPartialsBase' that intersects while ignore class expansions
intersectPartialsBase :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> Maybe (TypeVarArgEnv, [PartialType])
intersectPartialsBase _ _ PartialType{ptName=aName} PartialType{ptName=bName} | aName /= bName = Nothing
intersectPartialsBase _ _ PartialType{ptArgs=aArgs, ptArgMode=aArgMode} PartialType{ptArgs=bArgs, ptArgMode=bArgMode} | aArgMode == PtArgExact && bArgMode == PtArgExact && H.keysSet aArgs /= H.keysSet bArgs = Nothing
intersectPartialsBase _ _ PartialType{ptArgs=aArgs, ptArgMode=PtArgExact} PartialType{ptArgs=bArgs, ptArgMode=PtArgAny} | not (H.keysSet bArgs `S.isSubsetOf` H.keysSet aArgs) = Nothing
intersectPartialsBase _ _ PartialType{ptArgs=aArgs, ptArgMode=PtArgAny} PartialType{ptArgs=bArgs, ptArgMode=PtArgExact} | not (H.keysSet aArgs `S.isSubsetOf` H.keysSet bArgs) = Nothing
intersectPartialsBase typeEnv vaenv (PartialType name' aVars aArgs aPreds aArgMode) (PartialType _ bVars bArgs bPreds bArgMode) = do
  (varsVaenvs, vars') <- unzip <$> intersectMap H.empty aVars bVars
  (argsVaenvs, args') <- unzip <$> intersectMap vaenv aArgs bArgs
  let venvs' = mergeAllVarEnvs typeEnv [fst $ splitVarArgEnv $ mergeAllVarEnvs typeEnv argsVaenvs, vars']
  let argMode' = case (aArgMode, bArgMode) of
        (PtArgAny, _)            -> PtArgAny
        (_, PtArgAny)            -> PtArgAny
        (PtArgExact, PtArgExact) -> PtArgExact
  let preds' = predsAnd aPreds bPreds
  return (mergeAllVarEnvs typeEnv varsVaenvs, [PartialType name' venvs' args' preds' argMode'])
  where
    -- intersectMap unions so that all typeVars from either a or b are kept
    intersectMap vev a b = traverse subValidate $ H.unionWith subUnion (fmap (vev,) a) (fmap (vev,) b)
    subUnion (aVaenv, a) (bVaenv, b) = intersectTypesWithVarEnv typeEnv (mergeAllVarEnvs typeEnv [aVaenv, bVaenv]) a b
    subValidate (vev, subTp) = if isBottomType subTp then Nothing else Just (vev, subTp)

intersectPartials :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> (TypeVarArgEnv, [PartialType])
intersectPartials typeEnv vaenv a b = case intersectPartialsBase typeEnv vaenv a b of
  Just (vaenv', partials') -> (vaenv', partials')
  Nothing                  -> (vaenv, [])

intersectPartialLeafsWithVarEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialLeafs -> PartialLeafs -> (TypeVarArgEnv, PartialLeafs)
intersectPartialLeafsWithVarEnv typeEnv vaenv aPartials bPartials = (vaenv', type')
  where
    intersected = H.intersectionWith (\as bs -> [intersectPartials typeEnv vaenv a b | a <- as, b <- bs]) (splitUnionTypeByName aPartials) (splitUnionTypeByName bPartials)
    vaenv' = mergeAllVarEnvs typeEnv $ fmap fst $ concat $ H.elems intersected
    type' = joinUnionTypeByName $ fmap (concatMap snd) intersected

intersectPartialLeafsEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialLeafs -> PartialLeafs -> PartialLeafs
intersectPartialLeafsEnv typeEnv vaenv aPartials bPartials = debugTrace typeEnv msg result
  where
    result = snd $ intersectPartialLeafsWithVarEnv typeEnv vaenv aPartials bPartials
    msg = printf "[INTERSECT_LEAFS] %s ∩ %s = %s" (show $ UnionType Nothing aPartials []) (show $ UnionType Nothing bPartials []) (show $ UnionType Nothing result [])

-- |
-- Takes the intersection of two 'Type'.
-- It uses the 'TypeVarEnv' for type variable arguments and determines any possible changes to the surrounding 'TypeVarEnv'.
intersectTypesWithVarEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> (TypeVarArgEnv, Type)
intersectTypesWithVarEnv TypeEnv{teDebug=True} _ l r | trace (printf "[INTERSECT] %s ∩ %s" (show l) (show r)) False = undefined
intersectTypesWithVarEnv _ vaenv PTopType t = (vaenv, t)
intersectTypesWithVarEnv _ vaenv t PTopType = (vaenv, t)
-- Mixed type (both top and pos non-empty): distribute intersection over union
-- (top ∪ pos) ∩ t = (top ∩ t) ∪ (pos ∩ t)
intersectTypesWithVarEnv typeEnv vaenv (UnionType (Just topData) pos cs) t2
  | not (H.null pos) || not (null cs) =
      let (vaenv1, topResult) = intersectTypesWithVarEnv typeEnv vaenv (UnionType (Just topData) H.empty []) t2
          (vaenv2, posResult) = intersectTypesWithVarEnv typeEnv vaenv (UnionType Nothing pos cs) t2
      in (mergeAllVarEnvs typeEnv [vaenv1, vaenv2], unionTypesWithEnv typeEnv vaenv topResult posResult)
intersectTypesWithVarEnv typeEnv vaenv t1 t2@(UnionType (Just _) pos _)
  | not (H.null pos) = intersectTypesWithVarEnv typeEnv vaenv t2 t1
-- Top ∩ Top → union excluded sets, predicates, and constants
intersectTypesWithVarEnv typeEnv vaenv (UnionType (Just (ps1, np1)) (H.null -> True) cs1) (UnionType (Just (ps2, np2)) (H.null -> True) cs2) =
  (vaenv, compactType typeEnv vaenv $ UnionType (Just (predsAnd ps1 ps2, unionPartialLeafs [np1, np2])) H.empty (L.nub $ cs1 ++ cs2))
intersectTypesWithVarEnv _ vaenv t1 t2 | t1 == t2 = (vaenv, t1)
-- TypeVar: constrain in vaenv
intersectTypesWithVarEnv typeEnv vaenv tv@(TypeVar v _) t = case (v, H.lookup v vaenv) of
  (TVArg{}, Nothing) -> error $ printf "Failed to intersect unknown %s with %s in vaenv %s" (show v) (show t) (show vaenv)
  (_, Just l) | isBottomType (intersectTypesEnv typeEnv vaenv l t) -> (vaenv, BottomType)
  _ -> (H.insertWith (intersectTypesEnv typeEnv vaenv) v t vaenv, tv)
intersectTypesWithVarEnv typeEnv vaenv t tv@TypeVar{} = intersectTypesWithVarEnv typeEnv vaenv tv t
intersectTypesWithVarEnv _ vaenv _ BottomType = (vaenv, BottomType)
intersectTypesWithVarEnv _ vaenv BottomType _ = (vaenv, BottomType)
-- PosPartials with constants ∩ Top: handle constant exclusion before leafs logic
intersectTypesWithVarEnv typeEnv vaenv (UnionType Nothing posLeafs posConsts) t2@(UnionType (Just (preds, negLeafs)) (H.null -> True) negConsts)
  | not (null posConsts) || not (null negConsts) =
      let (vaenv', baseResult) = intersectTypesWithVarEnv typeEnv vaenv
                                   (UnionType Nothing posLeafs [])
                                   (UnionType (Just (preds, negLeafs)) H.empty [])
          survivingConsts = filter (\c -> constantInType typeEnv vaenv c t2) posConsts
      in case (baseResult, survivingConsts) of
           (_, []) -> (vaenv', baseResult)
           (BottomType, cs) -> (vaenv', UnionType Nothing H.empty cs)
           (UnionType Nothing ls [], cs) ->
             let finalCs = filter (\c -> not $ isSubtypeOfWithEnv emptyTypeEnv' H.empty (singletonType $ constantPartialType c) (UnionType Nothing ls [])) cs
             in (vaenv', UnionType Nothing ls finalCs)
           _ -> (vaenv', baseResult)
-- PosPartials ∩ Top(negLeafs, PredsNone) when negLeafs non-empty → subtract
intersectTypesWithVarEnv typeEnv vaenv (UnionType Nothing posPartials []) (UnionType (Just (PredsNone, negPartials)) (H.null -> True) [])
  | not (H.null negPartials) =
      (vaenv, compactType typeEnv vaenv $ differencePartialLeafs typeEnv vaenv posPartials negPartials)
-- PosPartials ∩ Top(_, preds) when negLeafs empty → expand/filter
intersectTypesWithVarEnv typeEnv vaenv t1@(UnionType Nothing partials []) t2@(UnionType (Just (preds, negPartials)) (H.null -> True) _)
  | H.null negPartials =
      case expandTypesWithNamesFull typeEnv vaenv t2 (H.keysSet partials) of
        (filteredLeafs, False) -> intersectTypesWithVarEnv typeEnv vaenv t1 (UnionType Nothing filteredLeafs [])
        -- Symbolic: add preds to each partial when compactPartialsWithClassPred can resolve them
        -- (requires no PredsNot, which tryPredsToList signals by returning Nothing).
        -- Otherwise expand preds symbolically within pp's names (no expandPredicates needed).
        (_, True) -> case tryPredsToList preds of
          Just _  -> (vaenv, compactType typeEnv vaenv $ UnionType Nothing (joinUnionType $ map (`partialAddPreds` preds) $ splitUnionType partials) [])
          Nothing -> case preds of
            -- pp ∩ Top(¬inner, ∅) = pp - expand(inner within pp)
            PredsNot inner ->
              let innerLeafs = expandTypeWithNames typeEnv vaenv (UnionType (Just (inner, H.empty)) H.empty []) (H.keysSet partials)
              in (vaenv, compactType typeEnv vaenv $ differencePartialLeafs typeEnv vaenv partials innerLeafs)
            -- General: pp ∩ Top(preds, ∅) = pp ∩ expand(preds within pp)
            _ ->
              let predsLeafs = expandTypeWithNames typeEnv vaenv (UnionType (Just (preds, H.empty)) H.empty []) (H.keysSet partials)
              in intersectTypesWithVarEnv typeEnv vaenv t1 (UnionType Nothing predsLeafs [])
-- Top ∩ PosPartials → flip
intersectTypesWithVarEnv typeEnv vaenv t1@(UnionType (Just _) (H.null -> True) _) t2@(UnionType Nothing _ _) =
  intersectTypesWithVarEnv typeEnv vaenv t2 t1
-- PosPartials ∩ PosPartials → intersect leafs and constants
intersectTypesWithVarEnv typeEnv vaenv (UnionType Nothing aPartials aConsts) (UnionType Nothing bPartials bConsts) =
  debugTrace typeEnv msg (vaenv', compactType typeEnv vaenv result)
  where
    (vaenv', partials') = intersectPartialLeafsWithVarEnv typeEnv vaenv aPartials bPartials
    -- Constants that appear in both sides, or whose parent partial is in the other side's leafs
    aConsts' = filter (\c -> c `elem` bConsts || isSubtypeOfWithEnv emptyTypeEnv' H.empty (singletonType $ constantPartialType c) (UnionType Nothing bPartials [])) aConsts
    bConsts' = filter (\c -> c `elem` aConsts || isSubtypeOfWithEnv emptyTypeEnv' H.empty (singletonType $ constantPartialType c) (UnionType Nothing aPartials [])) bConsts
    -- Also remove constants already covered by the intersected partial leafs
    mergedConsts = L.nub $ filter (\c -> not $ isSubtypeOfWithEnv emptyTypeEnv' H.empty (singletonType $ constantPartialType c) (UnionType Nothing partials' [])) (aConsts' ++ bConsts')
    result = UnionType Nothing partials' mergedConsts
    msg = printf "[INTERSECT] %s ∩ %s = %s" (show $ UnionType Nothing aPartials aConsts) (show $ UnionType Nothing bPartials bConsts) (show result)
intersectTypesWithVarEnv _ _ t1 t2 = error $ printf "intersectTypesWithVarEnv: unhandled case %s ∩ %s" (show t1) (show t2)

intersectTypesEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Type
intersectTypesEnv typeEnv vaenv t1 t2 = snd $ intersectTypesWithVarEnv typeEnv vaenv t1 t2

-- | Takes the intersection of two 'Type' (∩).
intersectTypes :: (TypeGraph tg) => TypeEnv tg -> Type -> Type -> Type
intersectTypes typeEnv = intersectTypesEnv typeEnv H.empty

differencePartialLeafs :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialLeafs -> PartialLeafs -> Type
differencePartialLeafs typeEnv vaenv posPartialLeafs negPartialLeafs = UnionType Nothing (joinUnionTypeByName $ H.differenceWith differencePartials (splitUnionTypeByName posPartialLeafs) (splitUnionTypeByName negPartialLeafs)) []
  where
    differencePartials :: [PartialType] -> [PartialType] -> Maybe [PartialType]
    differencePartials poss negs = case concatMap (`differencePartialNegs` negs) poss of
      []  -> Nothing
      res -> Just res

    differencePartialNegs :: PartialType -> [PartialType] -> [PartialType]
    differencePartialNegs pos = foldr (\n ps -> concatMap (`differencePartial` n) ps) [pos]

    differencePartial :: PartialType -> PartialType -> [PartialType]
    differencePartial p1 p2 | p1 == p2 = []
    differencePartial p1@PartialType{ptArgs=posArgs, ptArgMode=PtArgExact} PartialType{ptArgs=negArgs, ptArgMode=PtArgExact} | H.keysSet posArgs /= H.keysSet negArgs = [p1]
    differencePartial p1@PartialType{ptArgs=posArgs, ptVars=posVars, ptArgMode=posArgMode} PartialType{ptArgs=negArgs, ptVars=negVars, ptArgMode=negArgMode} | posArgMode == negArgMode || negArgMode == PtArgExact = mapMaybe subtractArg (H.toList negArgs) ++ mapMaybe subtractVar (H.toList negVars)
      where
        subtractArg (_, PTopType) = Nothing
        subtractArg (argName, negArgVal) = case H.lookup argName posArgs of
          Just posArgVal -> case snd $ differenceTypeWithEnv typeEnv vaenv posArgVal negArgVal of
            BottomType -> Nothing
            argVal'    -> Just $ p1{ptArgs=H.insert argName argVal' posArgs}
          Nothing -> Nothing  -- neg specifies an arg pos doesn't constrain (PtArgAny); skip
        subtractVar (_, PTopType) = Nothing
        subtractVar (varName, negVarVal) = case H.lookupDefault PTopType varName posVars of
          BottomType -> Nothing
          varVal' -> Just p1{ptVars=H.insert varName (snd $ differenceTypeWithEnv typeEnv vaenv varVal' negVarVal) posArgs}
    -- PtArgExact - PtArgAny: neg requires at least negArgs; pos (exact) is in neg iff neg's required
    -- keys are all present in pos with matching values. Subtract matching args just like the main case.
    differencePartial p1@PartialType{ptArgs=posArgs, ptVars=posVars} PartialType{ptArgs=negArgs, ptVars=negVars, ptArgMode=PtArgAny}
      | not (H.keysSet negArgs `S.isSubsetOf` H.keysSet posArgs) = [p1]
      | otherwise = mapMaybe subtractArg (H.toList negArgs) ++ mapMaybe subtractVar (H.toList negVars)
      where
        subtractArg (_, PTopType) = Nothing
        subtractArg (argName, negArgVal) = case H.lookup argName posArgs of
          Just posArgVal -> case snd $ differenceTypeWithEnv typeEnv vaenv posArgVal negArgVal of
            BottomType -> Nothing
            argVal'    -> Just $ p1{ptArgs=H.insert argName argVal' posArgs}
          Nothing -> Nothing
        subtractVar (_, PTopType) = Nothing
        subtractVar (varName, negVarVal) = case H.lookupDefault PTopType varName posVars of
          BottomType -> Nothing
          varVal' -> Just p1{ptVars=H.insert varName (snd $ differenceTypeWithEnv typeEnv vaenv varVal' negVarVal) posVars}
    differencePartial p1 p2 = error $ printf "Unimplemented differencePartial: %s - %s" (show p1) (show p2)

-- | Difference of two types (t1 − t2 = t1 ∩ ¬t2).
differenceTypeWithEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> (TypeVarArgEnv, Type)
differenceTypeWithEnv TypeEnv{teDebug=True} _ t1 t2 | trace (printf "[DIFF] %s - %s" (show t1) (show t2)) False = undefined
differenceTypeWithEnv typeEnv vaenv t1 t2 = intersectTypesWithVarEnv typeEnv vaenv t1 (complementTypeEnv typeEnv vaenv t2)

differenceTypeEnv :: (TypeGraph tg) => TypeEnv tg -> Type -> Type -> Type
differenceTypeEnv typeEnv t1 t2 = snd $ differenceTypeWithEnv typeEnv H.empty t1 t2

-- | Complement of a type (¬t).
complementTypeEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
-- ¬(PosPartials leafs consts): Top(leafs, PredsNone) with excluded constants
-- The [Constant] in Top represents constants excluded from the complement
complementTypeEnv _ _ (UnionType Nothing leafs consts) = UnionType (Just (PredsNone, leafs)) H.empty consts
-- ¬PTopType = BottomType
complementTypeEnv _ _ PTopType = BottomType
-- ¬((top - {negConsts}) ∪ pos) = (¬top ∪ {negConsts}) ∩ ¬pos
-- cs are NEGATIVE constants (excluded from top), so they become POSITIVE in the complement
complementTypeEnv typeEnv vaenv (UnionType (Just topData) pos cs)
  | not (H.null pos) || not (null cs) =
      intersectTypesEnv typeEnv vaenv
        (unionTypesWithEnv typeEnv vaenv
          (complementTypeEnv typeEnv vaenv (UnionType (Just topData) H.empty []))
          (UnionType Nothing H.empty cs))
        (complementTypeEnv typeEnv vaenv (UnionType Nothing pos []))
-- ¬(Top(neg, preds, consts)):
-- Top(neg, consts) = "universe minus neg minus consts"
-- so its complement = neg ∪ consts = PosPartials(neg, consts) (plus preds handling)
complementTypeEnv typeEnv vaenv (UnionType (Just (preds, negPartials)) (H.null -> True) negConsts) =
  case (H.null negPartials, preds, negConsts) of
    (True, _, [])          -> UnionType (Just (predsNot preds, H.empty)) H.empty []
    -- Top with only constants: ¬(¬{cs}) = {cs}
    (True, PredsNone, cs)  -> UnionType Nothing H.empty cs
    (True, _, cs)          -> unionTypesWithEnv typeEnv vaenv
                                (UnionType Nothing H.empty cs)
                                (UnionType (Just (predsNot preds, H.empty)) H.empty [])
    (False, PredsNone, []) -> UnionType Nothing negPartials []
    (False, PredsNone, cs) -> UnionType Nothing negPartials cs
    (False, _, [])         -> unionTypesWithEnv typeEnv vaenv
                                (UnionType Nothing negPartials [])
                                (UnionType (Just (predsNot preds, H.empty)) H.empty [])
    (False, _, cs)         -> unionTypesWithEnv typeEnv vaenv
                                (UnionType Nothing negPartials cs)
                                (UnionType (Just (predsNot preds, H.empty)) H.empty [])
complementTypeEnv typeEnv vaenv (TypeVar v _) = complementTypeEnv typeEnv vaenv (vaenvLookup vaenv v)
complementTypeEnv _ _ t = error $ printf "complementTypeEnv: unhandled case %s" (show t)

-- | Takes the powerset of a 'Type' with the powerset of the arguments in the type.
powersetType :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
powersetType _ _ PTopType = PTopType
powersetType _ _ (UnionType (Just _) _ _) = undefined
powersetType _ _ t@(TypeVar _ _) = t
powersetType typeEnv vaenv (UnionType Nothing partials []) =
  compactType typeEnv vaenv $ UnionType Nothing partials' []
  where
    partials' = joinUnionType $ concatMap fromPartialType $ splitUnionType partials
    fromArgs args = powerset $ H.toList args
    fromPartialType (PartialType name vars args _ argMode) = [PartialType name vars (H.fromList a) PredsNone argMode | a <- fromArgs args]
powersetType _ _ t = error $ printf "powersetType: unexpected case %s" (show t)

-- | Spreads a type by making it use PtArgAny mode
spreadType :: TypeVarArgEnv -> Type -> Type
spreadType vaenv = setArgMode vaenv PtArgAny

setArgMode :: TypeVarArgEnv -> PtArgMode -> Type -> Type
setArgMode _ mode (UnionType Nothing leafs []) =
  UnionType Nothing (joinUnionType $ map (\p -> p{ptArgMode=mode}) $ splitUnionType leafs) []
setArgMode vaenv mode (TypeVar v _) = setArgMode vaenv mode (fromJust $ H.lookup v vaenv)
setArgMode _ _ PTopType = PTopType
setArgMode _ mode (UnionType (Just (PredsOne (PredRel n), negPartials)) (H.null -> True) []) =
  UnionType (Just (PredsOne $ PredRel n{ptArgMode=mode}, negPartials)) H.empty []
setArgMode _ _ t@(UnionType (Just (PredsOne PredClass{}, _)) (H.null -> True) []) = t
setArgMode _ _ t = error $ printf "Unimplemented setArgMode for %s" (show t)

-- |
-- Combines two 'TypeVarEnv' to form the one applying the knowledge from both
-- It takes the union of all variables from either, and shared variables combine knowledge by intersection
mergeVarEnvs :: (TypeGraph tg, Eq k, Hashable k) => TypeEnv tg -> H.HashMap k Type -> H.HashMap k Type -> H.HashMap k Type
mergeVarEnvs typeEnv = H.unionWith (intersectTypes typeEnv)


-- | Applies 'mergeVarEnvs' to many 'TypeVarEnv'
mergeAllVarEnvs :: (TypeGraph tg, Foldable f, Eq k, Hashable k, Show (f (H.HashMap k Type))) => TypeEnv tg -> f (H.HashMap k Type) -> H.HashMap k Type
mergeAllVarEnvs typeEnv = foldr (mergeVarEnvs typeEnv) H.empty

vaenvLookup :: TypeVarArgEnv -> TypeVarAux -> Type
vaenvLookup vaenv v@TVVar{} = H.lookupDefault PTopType v vaenv
vaenvLookup vaenv v@TVArg{} = case H.lookup v vaenv of
  Just t -> t
  Nothing -> error $ printf "Failed vaenvLookup for %s in %s" (show v) (show vaenv)

-- | Replaces the type variables 'TVVar' in a 'Type' based on the variables in a provided 'TypeVarEnv'
substituteVarsWithVarEnv :: TypeVarEnv -> Type -> Type
substituteVarsWithVarEnv venv (UnionType Nothing partials []) =
  UnionType Nothing (joinUnionType $ map (substitutePartial venv) $ splitUnionType partials) []
  where
    substitutePartial pVenv partial@PartialType{ptVars, ptArgs, ptPreds} = partial{
        ptVars = fmap (substituteVarsWithVarEnv pVenv) ptVars,
        ptArgs = fmap (substituteVarsWithVarEnv ptVars') ptArgs,
        ptPreds = mapTypePreds (mapTypePred (substitutePartial ptVars')) ptPreds
                                                                      }
        where ptVars' = fmap (substituteVarsWithVarEnv venv) ptVars
substituteVarsWithVarEnv venv (TypeVar (TVVar v) TVInt) = fromMaybe PTopType (H.lookup v venv)
substituteVarsWithVarEnv _ t = t

-- | Replaces the type variables 'TVVar' in a 'Type'
substituteVars :: Type -> Type
substituteVars = substituteVarsWithVarEnv H.empty

-- | Replaces the argument type variables 'TVArg' in a 'Type' based on the variables in a provided 'TypeArgEnv'
substituteArgsWithArgEnv :: TypeArgEnv -> Type -> Type
substituteArgsWithArgEnv aenv (UnionType Nothing partials []) =
  UnionType Nothing (joinUnionType $ map substitutePartial $ splitUnionType partials) []
  where
    substitutePartial partial@PartialType{ptArgs} = partial{
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
typeGetAux (TVVar v) p = Just $ H.lookupDefault PTopType v $ ptVars p
typeGetAux (TVArg v) p = typeGetArg v p

-- | Gets an arg from a type while substituting the variables used in the types ptVars
typeGetArg :: ArgName -> PartialType -> Maybe Type
typeGetArg argName partial@PartialType{ptArgs, ptVars, ptArgMode} = case H.lookup argName ptArgs of
  Nothing -> case ptArgMode of
    PtArgAny   -> Just PTopType
    PtArgExact -> Nothing
  Just arg -> case arg of
    t@(UnionType (Just _) _ _) -> Just t
    TypeVar (TVArg a) _ | a == argName -> error $ printf "Found getArg cycle looking for %s in %s" (show argName) (show partial)
    t'@(TypeVar v _) -> case typeGetAux v partial of
      Just t'' -> Just t''
      Nothing  -> Just t'
    UnionType Nothing partialLeafs [] -> Just $ UnionType Nothing (joinUnionType $ map substitutePartial $ splitUnionType partialLeafs) []
      where
        substitutePartial p@PartialType{ptVars=vs} = p{ptVars = fmap (substituteVarsWithVarEnv ptVars) vs}
    t -> Just t

-- | Gets an arg from a type while substituting the variables used in the types ptVars
typesGetArg :: TypeGraph tg => TypeEnv tg -> ArgName -> Type -> Maybe Type
typesGetArg typeEnv argName (UnionType Nothing partialLeafs []) = fmap (unionAllTypes typeEnv) $ mapM (typeGetArg argName) $ splitUnionType partialLeafs
typesGetArg _ _ _ = Nothing

typeSetAux :: TypeVarAux -> Type -> PartialType -> PartialType
typeSetAux (TVVar k) v p@PartialType{ptVars} = p{ptVars=H.insert k v ptVars}
typeSetAux (TVArg k) v p@PartialType{ptArgs} = p{ptArgs=H.insert k v ptArgs}

updateTypeProp :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> TypeVarAux -> Type -> (TypeVarArgEnv, Type, Type)
updateTypeProp typeEnv vaenv superType propName subType = case superType of
    PTopType -> (vaenv, PTopType, subType)
    TypeVar v _ -> do
      let (vaenv', superType', subType') = updateTypeProp typeEnv vaenv (H.lookupDefault PTopType v vaenv) propName subType
      (H.insert v superType' vaenv', superType, subType')
    t@(UnionType (Just _) _ _) ->
      (vaenv, t, subType)
    UnionType Nothing supPartials [] -> do
      let supPartialList = splitUnionType supPartials
      let intersectedPartials sup@PartialType{ptVars=supVars} sub = case typeGetAux propName sup of
            Just (TypeVar (TVVar v) TVInt) -> do
              let supVar = H.lookupDefault PTopType v supVars
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
        UnionType Nothing subPartials [] -> do
          let subPartialList = splitUnionType subPartials
          let (supPartialList', subPartialList') = unzip $ catMaybes $ [intersectedPartials sup (singletonType sub) | sup <- supPartialList, sub <- subPartialList]
          (vaenv, compactType typeEnv vaenv $ UnionType Nothing (joinUnionType supPartialList') [], unionAllTypes typeEnv subPartialList')
        TypeVar v _ -> do
          -- Update vaenv.v with supPartials
          let tp' = intersectTypesEnv typeEnv vaenv (H.lookupDefault PTopType v vaenv) (unionAllTypesWithEnv typeEnv vaenv $ mapMaybe (typeGetAux propName) supPartialList)
          let vaenv' = H.insert v tp' vaenv
          let tp'' = substituteWithVarArgEnv vaenv' tp'
          let updateSuperPartial p = case typeGetAux propName p of
                Nothing -> Nothing
                Just supTp | supTp == subType -> Just $ typeSetAux propName subType p
                -- TODO Uncomment below to better propogate type variables during type inference
                -- Just PTopType | propName /= v -> Just $ typeSetAux propName subType p
                Just supTp -> Just $ typeSetAux propName (intersectTypes typeEnv supTp tp'') p
          let superType' = compactType typeEnv vaenv $ UnionType Nothing (joinUnionType $ mapMaybe updateSuperPartial $ splitUnionType supPartials) []

          (vaenv', superType', subType)
        PTopType -> do
          let sub' = case mapMaybe (typeGetAux propName) supPartialList of
                []       -> PTopType
                supProps -> compactType typeEnv vaenv $ unionAllTypes typeEnv supProps
          (vaenv, superType, sub')
        _ -> do
          let (supPartialList', subPartialList') = unzip $ catMaybes $ [intersectedPartials sup subType | sup <- supPartialList]
          (vaenv, compactType typeEnv vaenv $ UnionType Nothing (joinUnionType supPartialList') [], compactType typeEnv vaenv $ unionAllTypes typeEnv subPartialList')
    t -> (vaenv, t, subType)
