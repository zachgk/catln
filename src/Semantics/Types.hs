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
import           Data.Aeson.Types    (contramapToJSONKeyFunction)
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
import           Data.String         (IsString (..))
import           Data.Zip
import           Debug.Trace         (trace)
import           GHC.Generics        (Generic)
import           Prelude             hiding (unzip)
import           Text.Printf
import           Utils

-- |The name is the basic type used for various kinds of names.
-- It carries a precomputed hash so 'Hashable' is O(1).  Names are used as keys in the
-- 'PartialLeafs' maps that the type set-algebra rebuilds billions of times; hashing them
-- char-by-char (when @Name = String@) dominated compile time.  'nameHash' is exactly
-- @hash nameStr@, so the value 'Data.HashMap' uses to place a key is bit-identical to the
-- old @String@ key — preserving HashMap iteration order (hence golden output) for name keys.
data Name = Name { nameHash :: !Int, nameStr :: String }

-- | The canonical way to build a 'Name': computes and caches the hash once.
mkName :: String -> Name
mkName s = Name (hash s) s

instance IsString Name where
  fromString = mkName

instance Hashable Name where
  hash = nameHash
  hashWithSalt s n = hashWithSalt s (nameHash n)

instance Eq Name where
  a == b = nameHash a == nameHash b && nameStr a == nameStr b

instance Ord Name where
  compare a b = compare (nameStr a) (nameStr b)

instance Show Name where
  show = show . nameStr

instance ToJSON Name where
  toJSON = toJSON . nameStr
  toEncoding = toEncoding . nameStr

instance ToJSONKey Name where
  toJSONKey = contramapToJSONKeyFunction nameStr toJSONKey

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

-- |
-- Describes how the listed args of a 'PartialType' bound the args of a matching
-- instance, as two independent booleans (a bounded lower\/upper end of the
-- arg-name set, treated as an interval in the subset lattice):
--
--   * 'ptArgLb' (lower bounded): every listed arg is required (instance args ⊇ listed).
--   * 'ptArgUb' (upper bounded): no unlisted args are allowed (instance args ⊆ listed).
--
-- The four combinations: (T,T) exactly the listed args; (T,F) listed required,
-- extras allowed; (F,T) any subset of listed, no extras; (F,F) unconstrained.
data PtArgMode = PtArgMode {
  ptArgLb :: Bool,
  ptArgUb :: Bool
  } deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- | Matches only the exact args (lower and upper bounded).
pattern PtArgExact :: PtArgMode
pattern PtArgExact = PtArgMode True True

-- | Matches with any additional args (lower bounded, upper unbounded).
pattern PtArgAny :: PtArgMode
pattern PtArgAny = PtArgMode True False

-- | A predicate applied to a type
data TypePredicate
  = PredExpr PartialType
  | PredClass PartialType -- A hasClass predicate
  | PredRel PartialType -- a isa predicate. Represented by val : classOrModuleOrType
  deriving (Eq, Ord, Show, Generic, ToJSON)

data TypePredicates
  = PredsOne TypePredicate
  | PredsAnd [TypePredicates]
  | PredsNot TypePredicates
  deriving (Eq, Ord, Generic, ToJSON)

data PartialKey = PartialKey {
  pkName :: Name,
  pkVars :: S.HashSet PartialKey,
  pkArgs :: S.HashSet PartialKey
  } deriving (Eq, Ord, Generic, ToJSON, ToJSONKey)

-- | Fast hash: only name and set sizes.  Equality is still checked via 'Eq'.
instance Hashable PartialKey where
  hashWithSalt s (PartialKey name vars args) =
    s `hashWithSalt` name
      `hashWithSalt` S.size vars
      `hashWithSalt` S.size args

-- | Shallow predicate tag used inside 'PartialType' hash to avoid the
-- TypePredicates → TypePredicate → PartialType → TypePredicates cycle.
predsTag :: TypePredicates -> Int
predsTag (PredsOne _)  = 0
predsTag (PredsAnd ps) = 1 + length ps
predsTag (PredsNot _)  = -1

-- | Hash predicate: constructor + recursive children.
-- PartialType breaks the cycle by hashing only 'predsTag', not the full predicates.
instance Hashable TypePredicates where
  hashWithSalt s (PredsOne p)  = s `hashWithSalt` (0 :: Int) `hashWithSalt` p
  hashWithSalt s (PredsAnd ps) = foldl hashWithSalt (s `hashWithSalt` (1 :: Int)) ps
  hashWithSalt s (PredsNot p)  = s `hashWithSalt` (2 :: Int) `hashWithSalt` p

instance Hashable TypePredicate where
  hashWithSalt s (PredExpr  p) = s `hashWithSalt` (0 :: Int) `hashWithSalt` p
  hashWithSalt s (PredClass p) = s `hashWithSalt` (1 :: Int) `hashWithSalt` p
  hashWithSalt s (PredRel   p) = s `hashWithSalt` (2 :: Int) `hashWithSalt` p

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
  } deriving (Eq, Ord, Generic, ToJSON, ToJSONKey)

-- | Hash PartialType without recursing into map *values* (which are Types containing
-- more PartialLeafs).  Key names and sizes discriminate the common cases cheaply.
-- predsTag breaks the TypePredicates→TypePredicate→PartialType cycle.
instance Hashable PartialType where
  hashWithSalt s PartialType{ptName, ptVars, ptArgs, ptPreds, ptArgMode} =
    s `hashWithSalt` ptName
      `hashWithSalt` H.size ptVars
      `hashWithSalt` H.size ptArgs
      `hashWithSalt` predsTag ptPreds
      `hashWithSalt` ptArgMode

data Constant
  = CInt Integer
  | CFloat Double
  | CStr String
  | CChar Char
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- | The non-name properties of a 'PartialType'
newtype PartialArgsOption = PartialArgsOption
  (H.HashMap TypeVarName Type, H.HashMap ArgName Type, TypePredicates, PtArgMode)
  deriving (Eq, Ord)

instance Show PartialArgsOption where
  show (PartialArgsOption t) = show t

instance ToJSON PartialArgsOption where
  toJSON (PartialArgsOption t) = toJSON t

-- | Shallow hash: map sizes + predsTag + argMode, avoiding recursive descent into Type values.
-- Mirrors the pattern used for 'PartialType' and 'Type'.
instance Hashable PartialArgsOption where
  hashWithSalt s (PartialArgsOption (vars, args, preds, mode)) =
    s `hashWithSalt` H.size vars
      `hashWithSalt` H.size args
      `hashWithSalt` predsTag preds
      `hashWithSalt` mode

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
  deriving (Eq, Ord, Generic, ToJSON, ToJSONKey)

-- | Hash Type using only structural shape and key names, not the nested Type
-- values inside PartialLeafs.  This avoids the expensive recursive descent
-- through PartialLeafs → HashSet PartialArgsOption → HashMap ArgName Type → Type.
instance Hashable Type where
  hashWithSalt s (TypeVar aux loc) =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` aux `hashWithSalt` loc
  hashWithSalt s (UnionType mt leafs cs) =
    let h = s `hashWithSalt` (1 :: Int)
              `hashWithSalt` length cs
              `hashWithSalt` H.size leafs
        h' = case mt of
               Nothing           -> h `hashWithSalt` (0 :: Int)
               Just (preds, neg) -> h `hashWithSalt` (1 :: Int)
                                      `hashWithSalt` predsTag preds
                                      `hashWithSalt` H.size neg
    in H.foldlWithKey' (\acc k _ -> acc `hashWithSalt` k) h' leafs

-- | Backward-compat pattern: matches a complement/top type (predicates, negative partials).
-- Corresponds to the old @TopType PartialLeafs TypePredicates@ constructor.
pattern TopType :: PartialLeafs -> TypePredicates -> Type
pattern TopType negLeafs preds <- UnionType (Just (preds, negLeafs)) (H.null -> True) []
  where TopType negLeafs preds = UnionType (Just (preds, negLeafs)) H.empty []

-- | Indicates whether a type var or arg is located internally to the object or externally.
-- | 'TVInt' references a variable defined on the *same* 'PartialType' (resolved against its 'ptVars'/'ptArgs').
-- | 'TVExt' references a variable from a parent object or outer context (resolved against 'TypeVarArgEnv').
-- | In source syntax, 'TVInt' is written as @$T@ and 'TVExt' as @$_T@.
data TypeVarLoc
  = TVInt
  | TVExt
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

  -- | Returns the data-definition object partials for a type name, carrying the declared
  -- relationship between its 'ptVars' (type parameters) and 'ptArgs' (fields). Empty if the
  -- name has no data definition. Used to complete a param-form annotation (e.g. @Box[T]@)
  -- back to its full field form (@Box[T](v=$T)@).
  typeGraphData :: TypeEnv tg -> TypeName -> [PartialType]

data EmptyTypeGraph = EmptyTypeGraph
instance Semigroup EmptyTypeGraph where
  _ <> _ = EmptyTypeGraph
instance TypeGraph EmptyTypeGraph where
  typeGraphQueryWithReason _ _ _ = []
  typeGraphExpandPredExpr _ _ _ = Nothing
  typeGraphData _ _ = []

typeGraphQuery :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> [Type]
typeGraphQuery typeEnv vaenv src = map snd $ typeGraphQueryWithReason typeEnv vaenv src

data TypeEnv tg = TypeEnv {
  teClassGraph        :: ClassGraph,
  teTypeGraph         :: tg,
  teNames             :: S.HashSet Name,
  teDisableCompact    :: Bool,
  teDebug             :: Bool,
  -- | When 'True', type operations are performed under the closed-world assumption:
  -- no types exist outside those currently defined, so all classes are effectively
  -- sealed and predicates can be safely expanded.  Used for declaration-coverage checks.
  tePrgmEnv           :: Bool,
  -- | When 'True', the arg-keyset feasibility test in 'isSubPartialOfWithEnv' uses the
  -- loose /projection/ relation (an 'Any'\/upper-unbounded source may project onto an
  -- 'Exact' requirement by ignoring extra args) instead of the precise interval relation.
  -- Enabled by the matching\/dispatch consumers (TreeBuild, Eval, scheme verification)
  -- where this looseness is semantically intended; left 'False' for the set-algebra so the
  -- property tests stay sound. See 'looseArgModeRejects'.
  teArgModeProjection :: Bool,
  -- | Global views of 'teClassGraph' and 'teNames' for the symbolic satisfiability engine
  -- (see the section "Symbolic class-predicate satisfiability"), built strictly at
  -- construction via 'mkSatCache'. Any construction or update that changes 'teClassGraph'
  -- or 'teNames' must rebuild this with 'mkSatCache' so the cache matches them.
  teSatCache          :: !SatCache
                          }
  deriving (Show)

defaultTypeEnvDebug :: Bool
defaultTypeEnvDebug = False


emptyTypeEnv' :: TypeEnv EmptyTypeGraph
emptyTypeEnv' = TypeEnv mempty EmptyTypeGraph S.empty False defaultTypeEnvDebug False False (mkSatCache mempty S.empty)

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
  show = showPredsD maxShowDepth

showPredsD :: Int -> TypePredicates -> String
showPredsD d (PredsOne p)  = showPartialPredD d p
showPredsD d (PredsAnd ps) = intercalate " && " $ map (showPredsD d) ps
showPredsD d (PredsNot p)  = "!(" ++ showPredsD d p ++ ")"

-- | Render a single 'TypePredicate', preserving the derived-style output but
-- routing the nested 'PartialType' through the depth-bounded renderer.
showPartialPredD :: Int -> TypePredicate -> String
showPartialPredD d (PredExpr p)  = "PredExpr "  ++ showPartialD (d - 1) p
showPartialPredD d (PredClass p) = "PredClass " ++ showPartialD (d - 1) p
showPartialPredD d (PredRel p)   = "PredRel "   ++ showPartialD (d - 1) p

instance Show PartialType where
  show = boundedTypeStr . showPartialD maxShowDepth

showPartialD :: Int -> PartialType -> String
showPartialD d _ | d <= 0 = showTruncated
showPartialD d (PartialType ptName ptVars ptArgs ptPreds ptArgMode) = concat [showName, showTypeVars ptVars, showArgs ptArgs, showPreds ptPreds, showPtArgMode]
    where
      showName = if nameStr ptName == anonStr then "" else nameStr ptName
      showArg (argName, argVal) = show argName ++ "=" ++ showTypeD (d - 1) argVal
      showTypeVars vars | H.null vars = ""
      showTypeVars vars = "[" ++ intercalate ", " (map showArg $ H.toList vars) ++ "]"
      showArgs args | nameStr ptName == anonStr && H.null args = "()"
      showArgs args | H.null args = ""
      showArgs args = "(" ++ intercalate ", " (map showArg $ H.toList args) ++ ")"
      showPreds PredsNone    = ""
      showPreds (PredsOne p) = "| " ++ showPartialPredD d p
      showPreds preds        = "| (" ++ showPredsD d preds ++ ")"
      showPtArgMode = case ptArgMode of
        PtArgMode True True   -> ""
        PtArgMode True False  -> ".."
        PtArgMode False True  -> "??"
        PtArgMode False False -> "..??"

instance Show PartialKey where
  show pk = show $ partialToType pk

-- | A type's rendered form can be very large or effectively unbounded — the
-- constraint solver can build deeply-nested or even cyclic types whose full
-- rendering does not terminate in practical time. Two independent bounds keep any
-- consumer (error reporting, 'Eq' via 'show', JSON) prompt: a recursion-depth
-- limit ('maxShowDepth') that halts descent through nested types/partials, and a
-- length cap ('typeShowCap') on the rendered string. Both are far beyond any
-- well-formed type (the largest in practice nests only a few levels and is a few
-- hundred characters), so normal output is unaffected.
maxShowDepth :: Int
maxShowDepth = 100

typeShowCap :: Int
typeShowCap = 2000

showTruncated :: String
showTruncated = "…(truncated)"

boundedTypeStr :: String -> String
boundedTypeStr s
  | null (drop typeShowCap s) = s
  | otherwise                 = take typeShowCap s ++ showTruncated

instance Show Type where
  show = boundedTypeStr . showTypeD maxShowDepth

showTypeD :: Int -> Type -> String
showTypeD d _ | d <= 0 = showTruncated
showTypeD d (UnionType Nothing leafs []) = joinParts $ map (showPartialD (d - 1)) $ splitUnionType leafs
    where
      joinParts []  = "∅"
      joinParts [p] = p
      joinParts ps  = "(" ++ intercalate " + " ps ++ ")"
showTypeD d (UnionType (Just (p, negPartials)) (H.null -> True) []) = p' ++ negPartials'
    where
      p' = case tryPredsToList p of
                Just [PredClass c] -> "∀" ++ showPartialD (d - 1) c
                Just [PredRel c]   -> "~" ++ showPartialD (d - 1) c
                Just []            -> "TopType"
                _                  -> "(TopType | " ++ showPredsD (d - 1) p ++ ")"
      negPartials' = if H.null negPartials
        then ""
        else " - " ++ (joinParts $ map (showPartialD (d - 1)) $ splitUnionType negPartials)
      joinParts []  = "∅"
      joinParts [s] = s
      joinParts ss  = "(" ++ intercalate " + " ss ++ ")"
showTypeD d (UnionType mTop leafs consts) = "UnionType(" ++ show mTop ++ ", " ++ joinParts (map (showPartialD (d - 1)) $ splitUnionType leafs) ++ ", " ++ show consts ++ ")"
    where
      joinParts []  = "∅"
      joinParts [p] = p
      joinParts ps  = "(" ++ intercalate " + " ps ++ ")"
showTypeD _ (TypeVar (TVVar n) TVInt) = "$" ++ show n
showTypeD _ (TypeVar (TVVar n) TVExt) = "$_" ++ show n
showTypeD _ (TypeVar (TVArg n) TVInt) = "TVArg " ++ show n
showTypeD _ (TypeVar (TVArg n) TVExt) = "TVArg(ext) " ++ show n

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

      mergeClassPartials clss@PartialType{ptVars=varsA} PartialType{ptVars=varsB} = clss{ptVars = H.unionWith (unionTypes emptyTypeEnv'{teClassGraph=ClassGraph classGraphA, teSatCache=mkSatCache (ClassGraph classGraphA) S.empty}) varsA varsB}
instance Monoid ClassGraph where
  mempty = ClassGraph $ graphFromEdges []

instance (Semigroup tg) => Semigroup (TypeEnv tg) where
  (TypeEnv cg1 tg1 n1 d1 dc1 pe1 ap1 _) <> (TypeEnv cg2 tg2 n2 d2 dc2 pe2 ap2 _) = TypeEnv cg' (tg1 <> tg2) n' (d1 || d2) (dc1 || dc2) (pe1 || pe2) (ap1 || ap2) (mkSatCache cg' n')
    where
      cg' = cg1 <> cg2
      n' = S.union n1 n2


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
          let derived = L.nub $ classImplied ps' ++ concatMap (unitPropDerive ps') ps'
              ps'' = L.nub $ ps' ++ derived
          in if length ps'' > length ps'
             then go (n - 1) ps''
             else False
    -- @hasClass c ⟹ isa c@ (same name): a known @PredClass c@ also makes @PredRel c@ known. Lets
    -- contradiction detection see e.g. @PredClass Eq ∧ ¬(PredClass Eq ∧ PredRel Eq)@ as empty - the
    -- shape an element bound takes when a constructor is a member of two related classes (Eq, Ord).
    classImplied known = [PredsOne (PredRel c) | PredsOne (PredClass c) <- known]
    -- From ¬(A∧B∧...) with all-but-one known, derive the negation of the remaining.
    -- Use predsNot (not PredsNot) so that ¬(¬A) normalizes to A, enabling contradiction detection.
    unitPropDerive :: [TypePredicates] -> TypePredicates -> [TypePredicates]
    unitPropDerive known (PredsNot (PredsAnd binds)) =
      [predsNot b | b <- binds, let others = filter (/= b) binds, all (`elem` known) others]
    unitPropDerive _ _ = []
isPredsContradictory _             = False

-- | Class-graph-aware unsatisfiability: 'True' when no type satisfies @preds@, decided by expanding
-- against the class graph and checking for emptiness. Complements the purely syntactic
-- 'isPredsContradictory' (which only sees structural A∧¬A contradictions) with class-graph
-- disjointness - e.g. two distinct classes whose members\/placeholders do not overlap, so
-- @PredClass C ∧ PredClass D@ is empty. Sound by construction: it only reports 'True' when the
-- expansion is definitively bottom; predicates that do not fully expand (e.g. a 'PredRel' or
-- 'PredExpr') leave a symbolic, non-bottom expansion and so return 'False' (conservative).
predsEmpty :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicates -> Bool
-- Only conjunctions of >=2 atoms can be empty by class-graph disjointness (a single atom or a
-- negation is satisfiable on its own). Restricting the (relatively expensive) expansion to that
-- case keeps the common single-predicate subtype checks free.
predsEmpty typeEnv vaenv preds@(PredsAnd ps) | length ps >= 2 =
  satDecide (printf "predsEmpty %s" (show preds)) (predsEmptySymbolic typeEnv vaenv preds)
    -- Outside the symbolic fragment (a 'PredExpr', constants, an oversized formula): decide
    -- by materialized expansion.
    (isBottomType (expandPredicates typeEnv vaenv preds))
predsEmpty _       _     _                                    = False

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
listClassNames = scClassNames . teSatCache

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

-- | Whether a type references any type variable ('TypeVar') at any nesting depth, including
-- variable\/argument slot values and predicate partials.
typeContainsTypeVar :: Type -> Bool
typeContainsTypeVar TypeVar{} = True
typeContainsTypeVar BottomType = False
typeContainsTypeVar (UnionType mPreds leafs _) = predsPart || any partialHas (splitUnionType leafs)
  where
    predsPart = case mPreds of
      Just (preds, negLeafs) -> predsHas preds || any partialHas (splitUnionType negLeafs)
      Nothing                -> False
    predsHas (PredsAnd ps) = any predsHas ps
    predsHas (PredsOne p)  = predHas p
    predsHas (PredsNot p)  = predsHas p
    predHas (PredExpr p)  = partialHas p
    predHas (PredClass p) = partialHas p
    predHas (PredRel p)   = partialHas p
    partialHas PartialType{ptVars, ptArgs, ptPreds} =
      any typeContainsTypeVar ptVars || any typeContainsTypeVar ptArgs || predsHas ptPreds

containsBottomType :: Type -> Bool
containsBottomType BottomType = True
containsBottomType (UnionType (Just _) _ _) = False
containsBottomType (TypeVar _ _) = False
containsBottomType (UnionType Nothing leafs _) = any containsBottomPartialType $ splitUnionType leafs

containsBottomPartialType :: PartialType -> Bool
containsBottomPartialType PartialType{ptArgs, ptVars} = any containsBottomType ptArgs || any containsBottomType ptVars

ptVarArg :: PartialType -> TypeVarArgEnv
ptVarArg PartialType{ptArgs, ptVars} = joinVarArgEnv ptVars ptArgs

-- | Pushes a partial's own bindings ('ptVars' + 'ptArgs') onto an outer 'TypeVarArgEnv',
-- making them the resolution scope for TVExt references that appear inside the partial's
-- components.  The partial's binding values are first substituted using the outer env so
-- that any TVExt refs they contain are resolved before being installed as the new scope.
-- Bindings from the partial take precedence over outer bindings (inner scope shadows outer).
enterPartial :: TypeVarArgEnv -> PartialType -> TypeVarArgEnv
enterPartial vaenv partial = H.union (substituteTVExt vaenv <$> ptVarArg partial) vaenv

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
makeAbsoluteName n = case nameStr n of
  ""      -> n
  ('/':_) -> n
  s       -> mkName ('/' : s)

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
relativeNameMatches rn n = rn == n || ('/' : nameStr rn) `L.isSuffixOf` ('/' : nameStr n)

relativeNameFilter :: RelativeName -> [Name] -> [Name]
relativeNameFilter rn = filter (relativeNameMatches rn)

-- | Takes a list of relative and abs names, and prunes the relative names describing an absolute one
relAbsNamePrune :: [TypeName] -> [TypeName]
relAbsNamePrune names = map (mkName . show) (absNames ++ filter matchesNoAbsName relNames)
  where
    names' = map (getPath . nameStr) names
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
  where aux name (PartialArgsOption (vars, args, preds, argMode)) = PartialType name vars args preds argMode

-- | Used to combine the component 'PartialType' to form a 'UnionType'
joinUnionType :: [PartialType] -> PartialLeafs
joinUnionType = foldr (\(PartialType pName pVars pArgs pPreds pArgMode) partials -> H.insertWith S.union pName (S.singleton (PartialArgsOption (pVars, pArgs, pPreds, pArgMode))) partials) H.empty

-- | Used to convert a 'UnionType' into its components while keeping types with the same name together
splitUnionTypeByName :: PartialLeafs -> H.HashMap TypeName [PartialType]
splitUnionTypeByName = H.mapWithKey (\k vs -> map (aux k) (S.toList vs))
  where aux name (PartialArgsOption (vars, args, preds, argMode)) = PartialType name vars args preds argMode

-- | Used to combine the component 'PartialType' to form a 'UnionType' while keeping types with the same name together
joinUnionTypeByName :: H.HashMap TypeName [PartialType] -> PartialLeafs
joinUnionTypeByName leafs = H.map (S.fromList . map typeToArgOption) $ H.filter (not . null) leafs
  where typeToArgOption (PartialType _ pVars pArgs pPreds pArgMode) = PartialArgsOption (pVars, pArgs, pPreds, pArgMode)

-- | Helper to create a 'UnionType' consisting of a single 'PartialType'
singletonType :: PartialType -> Type
singletonType partial = UnionType Nothing (joinUnionType [partial]) []

-- | Recursively completes /nested/ parameterized-type partials against their data definition.
--
-- A nested partial in /param form/ — non-empty 'ptVars' but no 'ptArgs', e.g. the annotation
-- @Box[T]@ inside @unwrap(b=Box[T])@ for @data Box[T](v -> T)@ — has its declared fields added
-- (linked to its vars via the def), becoming @Box[T](v=$T)@. This lets it reconcile with the
-- /field form/ @Box(v=...)@ during intersection.
--
-- The /outermost/ partials are intentionally left alone: those may be deliberate
-- partial-application subsets produced by the arg-powerset (e.g. the no-arg subset @Box[T]@ of
-- @Box[T](v=$T)@), which must keep their reduced arg-set. Likewise the bare constructor (@Box@,
-- no vars) and any partial already carrying args are untouched. So partial application is
-- unaffected.
completeDataPartials :: (TypeGraph tg) => TypeEnv tg -> Type -> Type
completeDataPartials typeEnv = onArgs
  where
    -- Top level: recurse into args/vars only; do not complete the outermost partials.
    onArgs (TypeVar v l) = TypeVar v l
    onArgs (UnionType mPreds leafs consts) = UnionType (fmap (bimap id onArgsLeafs) mPreds) (onArgsLeafs leafs) consts
    onArgsLeafs = joinUnionType . map recurseArgs . splitUnionType
    recurseArgs p@PartialType{ptVars, ptArgs} = p{ptVars = fmap (completeDataPartialsTop typeEnv) ptVars, ptArgs = fmap (completeDataPartialsTop typeEnv) ptArgs}

-- | Like 'completeDataPartials' but also completes the /outermost/ partials, restoring their
-- declared data fields. Used when a partial is a supertype to match against rather than a value:
-- a partial may carry only its type parameters and omit the declared fields, in which case a
-- field-form value (one written with explicit args) would not reconcile with it. Restoring the
-- fields as 'PTopType' lets such a value match on its keys. (Fields and type parameters are
-- otherwise tracked independently and are not auto-related.)
completeDataPartialsTop :: (TypeGraph tg) => TypeEnv tg -> Type -> Type
completeDataPartialsTop typeEnv = go
  where
    -- Nested: complete each partial against its def, then recurse into its args/vars.
    go (TypeVar v l) = TypeVar v l
    go (UnionType mPreds leafs consts) = UnionType (fmap (bimap id goLeafs) mPreds) (goLeafs leafs) consts
    goLeafs = joinUnionType . map completePartial . splitUnionType
    -- Only an argument-less partial in /exact/ mode is an ambiguous bare reference needing its
    -- declared fields restored. A non-exact mode (an authored spread like @CatlnResult..@)
    -- already admits the field forms, and overwriting its mode with the data's would narrow it.
    completePartial p@PartialType{ptName, ptVars, ptArgs, ptArgMode}
      | H.null ptArgs && ptArgMode == PtArgExact =
          case [(dArgs, dMode) | PartialType{ptArgs=dArgs, ptArgMode=dMode} <- typeGraphData typeEnv ptName, not (H.null dArgs)] of
            -- Restore the declared fields, carrying the /bound/ of the type parameter each field
            -- is declared with (e.g. field @x : $A@ in @Box[$A -> Boxed]@ is restored as @Boxed@):
            -- so a value/definition that narrows the field to that bound still reconciles with this
            -- partial. Only a concrete parameter bound is carried; a field whose parameter is itself
            -- a variable (or whose declared type is not a bare parameter) is restored as 'PTopType'
            -- — its type is supplied by the value being matched, and carrying a variable here would
            -- reintroduce a free parameter that can collide with a like-named one in an enclosing scope.
            ((dArgs, dMode):_) -> p1{ptArgs = fmap restoreField dArgs, ptArgMode = dMode}
            []                 -> p1
      | otherwise = p1
      where
        p1 = p{ptVars = fmap go ptVars, ptArgs = fmap go ptArgs}
        restoreField fieldDefType = case fieldDefType of
          TypeVar (TVVar v) _ | Just b <- H.lookup v ptVars, not (isTypeVar b) -> b
          _                                                                    -> PTopType

-- | Relax each data partial's self-referential field — a field whose type recurses back to a
-- type the partial itself participates in (the same type, or a class the partial is a constituent
-- of) — to 'PTopType'. Used on a bound that constrains a destructured value: such a field would
-- otherwise require the value's corresponding field to already be recognized as a member of the
-- recursive type, but a freshly destructured field variable carries no such constraint yet, so
-- the match would spuriously fail (a rigid pattern-variable field cannot satisfy the recursive
-- predicate and collapses to bottom). The recursive structure is still enforced wherever the
-- field is genuinely used; a concrete field value still meets the relaxed bound unchanged, so no
-- type information is lost. Only the exact self-reference is relaxed (a bare @~C@ predicate-rel or
-- a lone @C@ partial), leaving every other field untouched.
relaxRecursiveSelfArgs :: (TypeGraph tg) => TypeEnv tg -> Type -> Type
relaxRecursiveSelfArgs typeEnv = onUnion
  where
    onUnion (UnionType mPreds leafs consts) = UnionType (fmap (bimap id onLeafs) mPreds) (onLeafs leafs) consts
    onUnion t = t
    onLeafs = joinUnionType . map onPartial . splitUnionType
    onPartial p@PartialType{ptName, ptArgs} = p{ptArgs = fmap (relaxArg ptName) ptArgs}
    relaxArg container t | referencesRecursively container t = PTopType
                         | otherwise                         = t

    -- The field's type names a type @c@ for which @container@ is recursive: either @c@ is the
    -- container itself, or @container@ is one of @c@'s declared class constituents.
    referencesRecursively container t = any recursiveWith (fieldRefNames t)
      where recursiveWith c = sameName container c || S.member (makeAbsoluteName container) (classConstituentNames typeEnv c)

    -- Top-level type names referenced by a field that is a bare self-reference: a @~C@
    -- predicate-rel, or a lone positive @C@ partial. Anything more complex is left untouched.
    fieldRefNames (UnionType (Just (preds, negLeafs)) ls _)
      | H.null ls && H.null negLeafs = case tryPredsToList preds of
          Just ps -> [ptName r | PredRel r <- ps]
          Nothing -> []
    fieldRefNames (UnionType Nothing ls []) = case splitUnionType ls of
      [PartialType{ptName=n}] -> [n]
      _                       -> []
    fieldRefNames _ = []

    sameName a b = makeAbsoluteName a == makeAbsoluteName b

-- | The absolute names of a sealed class's declared constituents (empty for a non-class name).
classConstituentNames :: (TypeGraph tg) => TypeEnv tg -> TypeName -> S.HashSet TypeName
classConstituentNames TypeEnv{teClassGraph=ClassGraph cg} cName = case graphLookup (PClassName cName) cg of
  Just (CGClass (_, _, classTypes, _)) -> S.map makeAbsoluteName $ S.unions $ map leafNameSet classTypes
  _                                    -> S.empty

-- | Splits a partial type by expanding any union-typed vars or args into separate partials.
-- For example, P[$T=(A + B)] splits into [P[$T=A], P[$T=B]].
-- Similarly, P(x=(A + B)) splits into [P(x=A), P(x=B)].
-- A partial with no union-typed vars or args returns [itself] unchanged.
splitPartialByUnionVarArgs :: PartialType -> [PartialType]
splitPartialByUnionVarArgs p@PartialType{ptVars, ptArgs} =
    case findUnion allVarArgs of
      Nothing -> [p]
      Just (keyName, components) -> concatMap splitPartialByUnionVarArgs
        [typeSetAux keyName component p | component <- components]
  where
    allVarArgs = map (first TVVar) (H.toList ptVars) ++ map (first TVArg) (H.toList ptArgs)

    findUnion :: [(TypeVarAux, Type)] -> Maybe (TypeVarAux, [Type])
    findUnion [] = Nothing
    findUnion ((keyName, t):rest) = case unionComponents t of
      Just comps@(_:_:_) -> Just (keyName, comps)
      _                  -> findUnion rest

    -- Splits a union-valued arg\/var into its members as separate single-member types, so an arg
    -- whose value is a union (e.g. from joining two same-key partials during a union) can be matched
    -- component-by-component against the supertypes. Handles the plain positive form and the mixed
    -- 'Just'-pred form (a predicated top plus positive leafs).
    unionComponents :: Type -> Maybe [Type]
    unionComponents (UnionType Nothing leafs []) = Just $ map singletonType $ splitUnionType leafs
    unionComponents (UnionType (Just topData) posLeafs cs)
      | not (H.null posLeafs) = Just $ UnionType (Just topData) H.empty cs : map singletonType (splitUnionType posLeafs)
    unionComponents _ = Nothing

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

-- | Relax an upper bound so an argument-less partial constrains only its name, but only
-- where the bounded value already carries that name /with arguments/.
--
-- A bound built from a bare value name (see 'relTypeVal') lists the name with no arguments
-- under 'PtArgExact', which means "exactly zero arguments". Intersecting that against an
-- argumented value — e.g. the head of a function-typed data field @rel(a, b)@, which
-- accretes its parameters onto its own point — yields bottom, since the exact arg-keysets
-- @{}@ and @{a, b}@ are disjoint. A name-only bound is not meant to constrain arguments, so
-- such a partial is widened to 'PtArgAny' (name required, any arguments allowed), leaving
-- the value's arguments intact.
--
-- The widening is gated on the actual type @act@: a name is relaxed only when @act@ already
-- lists it with arguments. This keeps the bound exact for an argument-less value (e.g. an
-- operator's value head, which stays 0-arg with its operands on the apply node), so the
-- bound does not turn a 'TopType' actual into a 'PtArgAny' one.
relaxNameBoundArgs :: Type -> Type -> Type
relaxNameBoundArgs act (UnionType mPreds leafs consts) = UnionType mPreds (relaxLeafs leafs) consts
  where
    actArgedNames = case act of
      UnionType _ actLeafs _ -> S.fromList [ptName p | p <- splitUnionType actLeafs, not (H.null (ptArgs p))]
      _                      -> S.empty
    relaxLeafs = joinUnionType . map relaxPartial . splitUnionType
    relaxPartial p@PartialType{ptName, ptArgs}
      | H.null ptArgs && ptName `S.member` actArgedNames = p{ptArgMode = PtArgAny}
      | otherwise                                        = p
relaxNameBoundArgs _ t = t

classPartial :: PartialType -> Type
classPartial p = TopType H.empty (PredsOne (PredClass p))

-- | Creates a classPlaceholder[$T] partial type for a given class name.
-- Used when expanding a class that has no constituent types to avoid returning the empty set.
classPlaceholderLeaf :: TypeName -> PartialType
classPlaceholderLeaf className = (partialVal (mkName classPlaceholderStr)){ptVars = H.singleton (partialKey "T") (singletonType $ partialVal className)}

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

-- | Sets a 'TypeVarAux'-keyed binding inside every partial of a 'Type'.
-- Handles 'UnionType Nothing' (set in each leaf partial) and 'TopType' wrapping
-- a 'PredRel'/'PredClass' (set in the singleton predicate partial).  Var/arg are
-- handled uniformly via 'typeSetAux' (defined later) — the binding location is
-- decided purely by the 'TypeVarAux' tag.
typeSetAuxType :: TypeVarAux -> Type -> Type -> Type
typeSetAuxType aux val (UnionType Nothing leafs []) =
  UnionType Nothing (joinUnionType $ map (typeSetAux aux val) $ splitUnionType leafs) []
typeSetAuxType aux val (TopType negPartials (PredsOne (PredRel p))) =
  TopType negPartials (PredsOne $ PredRel (typeSetAux aux val p))
typeSetAuxType (TVVar k) val (TopType negPartials (PredsOne (PredClass p@PartialType{ptVars}))) =
  TopType negPartials (PredsOne $ PredClass p{ptVars=H.insert k val ptVars})
typeSetAuxType aux val tp = error $ printf "Unimplemented typeSetAuxType for %s.%s = %s" (show tp) (show aux) (show val)

-- | Sets an arg-typed binding inside every partial of a 'Type'.
typeSetArg :: ArgName -> Type -> Type -> Type
typeSetArg argName = typeSetAuxType (TVArg argName)

-- | Sets a var-typed binding inside every partial of a 'Type'.
typeSetVar :: TypeVarName -> Type -> Type -> Type
typeSetVar varName = typeSetAuxType (TVVar varName)

suffixLookup :: Name -> [Name] -> Maybe Name
suffixLookup s (x:xs)
  | s == x = Just s
  | otherwise = if relativeNameMatches s x then Just x else suffixLookup s xs
suffixLookup _ [] = Nothing

suffixLookupInDict :: Name -> H.HashMap Name b -> Maybe b
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
predImplies typeEnv vaenv p1@(PredsAnd ps) p2      = any (\p -> predImplies typeEnv vaenv p p2) ps || allCheck || cancellationCheck || directContradictCheck
  -- if any conjunct alone implies p2, the intersection does too
  -- if p2 is a conjunction, p1 implies p2 iff p1 implies each conjunct of p2
  -- also: (X₁ ∨ X₂ ∨ ... ∨ q) ∧ ¬X₁ ∧ ¬X₂ ∧ ... ⊆ q  (disjunction cancellation)
  -- direct contradiction: p1 → ¬q iff p1 ∧ q = ∅
  where
    allCheck = case p2 of
      PredsAnd qs -> all (predImplies typeEnv vaenv p1) qs
      _           -> False
    -- A leftover disjunct @x@ is cancellable when @¬x@ is literally present, or when the
    -- conjunction is incompatible with @x@ by the class graph (@p1 ∧ x = ∅@, so @x@ can never
    -- co-occur) - e.g. @Boolean ∧ (Animal ∨ ~r)@ implies @~r@ because @Boolean ∧ Animal = ∅@.
    cancellationCheck = or
      [ all (\x -> PredsNot x `elem` ps || predsEmpty typeEnv vaenv (predsAnd p1 x)) xs
      | PredsNot (PredsAnd nots) <- ps
      , Just inners <- [mapM extractNot nots]
      , (q_inner, xs) <- choices inners
      , predImplies typeEnv vaenv q_inner p2
      ]
    directContradictCheck = case p2 of
      PredsNot inner -> isPredsContradictory (predsAnd p1 inner) || predsEmpty typeEnv vaenv (predsAnd p1 inner)
      _              -> False
    extractNot (PredsNot x) = Just x
    extractNot _            = Nothing
    choices []     = []
    choices (y:ys) = (y, ys) : map (\(z, zs) -> (z, y:zs)) (choices ys)
predImplies typeEnv vaenv p1 (PredsAnd ps)      = all (predImplies typeEnv vaenv p1) ps
  -- p1 must imply every conjunct
predImplies typeEnv vaenv (PredsNot inner) (PredsNot outer) = predImplies typeEnv vaenv outer inner
  -- contravariance: ¬A ⊆ ¬B ↔ B ⊆ A
predImplies typeEnv vaenv p1 (PredsNot p2)      = isPredsContradictory (predsAnd p1 p2) || predsEmpty typeEnv vaenv (predsAnd p1 p2)
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

-- | True when every class referenced by the predicates is sealed, so the predicates may be
-- soundly expanded to their declared constituents irrespective of the open/closed-world
-- ('tePrgmEnv') setting. For a sealed class @C@, @U_C@ is exactly the union of @C@'s declared
-- members, so @~C@ and its expansion are equal regardless of any types defined elsewhere.
predsSealed :: (TypeGraph tg) => TypeEnv tg -> TypePredicates -> Bool
predsSealed typeEnv@TypeEnv{teClassGraph=ClassGraph cg} = go
  where
    go (PredsOne p)  = atomSealed p
    go (PredsAnd ps) = not (null ps) && all go ps -- PredsNone = PredsAnd [] ↦ False
    go (PredsNot p)  = go p
    atomSealed (PredClass c) = classSealed (ptName c)
    atomSealed (PredRel c)   = classSealed (ptName c)
    atomSealed (PredExpr _)  = False
    -- In the program-as-written ('tePrgmEnv') environment every class is closed to its declared
    -- members, so any class predicate may be soundly expanded to its declared constituents.
    classSealed _ | tePrgmEnv typeEnv = True
    classSealed name = case graphLookup (PClassName name) cg of
      Just (CGClass (sealed, _, _, _)) -> sealed
      _ -> case [ s | (CGClass (s, _, _, _), cKey, _) <- graphToNodes cg, relativeNameMatches name (fromPartialName cKey) ] of
             (s:_) -> s
             []    -> False

-- | True when class @c1@ is a transitively-declared subclass of @c2@, established by the
-- @every(c1 isa c2)@ edges that record @c1@ as a class-predicate member of @c2@'s class node.
-- This is the "upward" is-a direction. It follows only /class/ members (those whose member type is
-- a bare 'PredClass'), never data members, so it cannot unroll a recursive parameterized member
-- such as @Maybe[$T -> Ord] ∈ Ord@ - which the "downward" member enumeration in 'predImpliesAtom'
-- does, making @Ord ⊆ Eq@ recurse back through @Maybe[Ord] ⊆ Maybe[Eq] ⊆ Eq@ into @Ord ⊆ Eq@.
-- Sound by definition: @every(c1 isa c2)@ asserts @U_c1 ⊆ U_c2@. Restricted to var-free class
-- members so a parameterized edge (@Ordering[$T] isa Eqv[$T]@) is left to the precise path.
classDeclaredSubclassOf :: (TypeGraph tg) => TypeEnv tg -> TypeName -> TypeName -> Bool
classDeclaredSubclassOf TypeEnv{teClassGraph=ClassGraph cg} c1 c2 = go S.empty c2
  where
    go visited cName
      | S.member cName visited = False
      | otherwise = case graphLookup (PClassName cName) cg of
          Just (CGClass (_, _, classTypes, _)) ->
            let subs = [ ptName sc | t <- classTypes, Just sc <- [maybeGetClassSingleton t], H.null (ptVars sc) ]
            in any (\s -> relativeNameMatches c1 s || go (S.insert cName visited) s) subs
          _ -> False

predImpliesAtom :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicate -> TypePredicate -> Bool
-- A declared subclass edge @every(c1 isa c2)@ proves @c1 ⊆ c2@ directly, without enumerating
-- @c1@'s members (which can recurse through a recursive parameterized member - see
-- 'classDeclaredSubclassOf').
predImpliesAtom typeEnv _ (PredClass c1) (PredClass c2)
  | classDeclaredSubclassOf typeEnv (ptName c1) (ptName c2) = True
-- Same class, covariant element bound: @hasClass C[/T=a] ⟹ hasClass C[/T=b]@ when @a ⊆ b@. A value
-- whose class-C element is within @a@ is also within the wider @b@. This is the parameterized-class
-- subtype rule for a shared class name with a type-var vs. a wider element bound (e.g. @Eqv[$T] ⟹
-- Eqv[⊤]@, @Functor[Integer] ⟹ Functor[$A]@), which the name-only 'classDeclaredSubclassOf' above
-- and the member-enumeration below never compare. Checked covariantly over each bound @c2@ declares,
-- with an absent @c1@ var/arg defaulting to ⊤: this is DIRECTIONAL (unlike the shared-keys-only
-- 'isSubPartialOfWithEnv', which would vacuously make @C ⟹ C[/T=∀Ord]@ True when @c1@ omits @/T@ and
-- so drop the tighter bound from an intersection like @C[/T=∀Ord] ∧ C@).
predImpliesAtom typeEnv vaenv (PredClass c1) (PredClass c2)
  | ptName c1 == ptName c2
  , all (\(k, v) -> isSubtypeOfWithEnv typeEnv vaenv (H.lookupDefault PTopType k (ptVars c1)) v) (H.toList (ptVars c2))
  , all (\(k, v) -> isSubtypeOfWithEnv typeEnv vaenv (H.lookupDefault PTopType k (ptArgs c1)) v) (H.toList (ptArgs c2)) = True
-- PredClass: c1 implies c2 only when c1 is sealed (all members known) and each member is in U_c2.
-- Decided symbolically (emptiness of @c1 ∧ ¬c2@ against the class graph); the engine's positive
-- gate reproduces the sealed/'tePrgmEnv' enumeration gate.
predImpliesAtom typeEnv@TypeEnv{teClassGraph=ClassGraph cg, tePrgmEnv} vaenv (PredClass c1) (PredClass c2) =
  satDecide (printf "impliesCC %s => %s" (show c1) (show c2))
    (predAtomImpliesSymbolic typeEnv vaenv True (PredClass c1) (PredClass c2))
    old
  where
    old = case graphLookup (PClassName (ptName c1)) cg of
      Just (CGClass (sealed, _, classTypes, _)) | sealed || tePrgmEnv ->
        let c2Type = expandClassPartial typeEnv vaenv c2
        in all (\t -> isSubtypeOfWithEnv typeEnv vaenv t c2Type) classTypes
      _ -> False  -- unsealed or unknown class: can't enumerate all members
-- PredRel: r1 implies r2 when r2's name path is a suffix of r1's (r2 is more general)
-- and r1's args are subtypes of r2's args. Otherwise fall back to expansion, which also
-- captures class-graph relationships between the names (e.g. ConsList isa List).
predImpliesAtom typeEnv vaenv (PredRel r1) (PredRel r2)
  | relativeNameMatches (ptName r2) (ptName r1) && all checkArg (H.keys (ptArgs r2)) = True
  -- A declared subclass edge @every(r1 isa r2)@ proves @isa r1 ⟹ isa r2@ directly, avoiding the
  -- 'expandRelPartial' below (which unrolls r1's members and recurses through a recursive
  -- parameterized member). Restricted to plain (arg/var-free) relations, matching the nullary
  -- class edges (@Ord isa Eq@) it targets - see 'classDeclaredSubclassOf'.
  | plainPartial r1 && plainPartial r2 && classDeclaredSubclassOf typeEnv (ptName r1) (ptName r2) = True
  -- Symbolic emptiness of @r1 ∧ ¬r2@ replaces expanding both relations (which for a recursive
  -- parameterized class universe re-expands the element class combinatorially).
  | otherwise = satDecide (printf "impliesRR %s => %s" (show r1) (show r2))
      (predAtomImpliesSymbolic typeEnv vaenv False (PredRel r1) (PredRel r2))
      (isSubtypeOfWithEnv typeEnv vaenv (expandRelPartial typeEnv vaenv r1) (expandRelPartial typeEnv vaenv r2))
  where checkArg k = isSubtypeOfWithEnv typeEnv vaenv
                       (H.lookupDefault PTopType k (ptArgs r1))
                       (ptArgs r2 H.! k)
        plainPartial p = H.null (ptArgs p) && H.null (ptVars p)
-- PredExpr: use structural partial comparison (equality-based simplifying assumption)
predImpliesAtom typeEnv vaenv (PredExpr e1) (PredExpr e2) = isSubPartialOfWithEnv typeEnv vaenv e1 e2
-- PredClass c implies PredRel r when r's name is a suffix of c's name (then
-- expandRelPartial r includes classPartial c, so U_PredClass c ⊆ U_PredRel r), or - like
-- the PredClass\/PredClass case - when c is sealed and each of its members is in U_PredRel r
-- (e.g. c is a subclass of the class that r names).
predImpliesAtom typeEnv@TypeEnv{teClassGraph=ClassGraph cg, tePrgmEnv} vaenv (PredClass c) (PredRel r)
  | relativeNameMatches (ptName r) (ptName c) = True
  | otherwise = satDecide (printf "impliesCR %s => %s" (show c) (show r))
      (predAtomImpliesSymbolic typeEnv vaenv True (PredClass c) (PredRel r))
      old
  where
    old = case graphLookup (PClassName (ptName c)) cg of
      Just (CGClass (sealed, _, classTypes, _)) | sealed || tePrgmEnv ->
        let rType = expandRelPartial typeEnv vaenv r
        in all (\t -> isSubtypeOfWithEnv typeEnv vaenv t rType) classTypes
      _ -> False
-- PredRel r implies PredClass c when every type matching r is a member of c.
-- Fast path (mirrors the @PredClass c ⟹ PredRel r@ same-name case above): for the SAME plain class
-- name, @isa c ⟹ hasClass c@ holds directly — a value whose type is a member of class @c@ has class
-- @c@. This avoids expanding BOTH sides via 'expandRelPartial'/'expandClassPartial', which for a
-- recursive parameterized class (e.g. @Eq@ with @ConsList[Eq]@/@Maybe[Eq]@ members) re-expands the
-- element class unboundedly. Restricted to arg/var-free partials naming a real class.
predImpliesAtom typeEnv vaenv (PredRel r) (PredClass c)
  | plainPartial r, plainPartial c
  , sameClass || classDeclaredSubclassOf typeEnv (ptName r) (ptName c) = True
  | otherwise = satDecide (printf "impliesRC %s => %s" (show r) (show c))
      (predAtomImpliesSymbolic typeEnv vaenv False (PredRel r) (PredClass c))
      (isSubtypeOfWithEnv typeEnv vaenv (expandRelPartial typeEnv vaenv r) (expandClassPartial typeEnv vaenv c))
  where
    plainPartial p = H.null (ptArgs p) && H.null (ptVars p)
    -- Same named entity (either absoluteness direction): @isa c ⟹ hasClass c@.
    sameClass = relativeNameMatches (ptName r) (ptName c) || relativeNameMatches (ptName c) (ptName r)
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
          (venv, leafs) = foldl1 combine (map ((vaenv,) . fst) mapped)
          combine (ve, aLeafs) (_, bLeafs) = intersectPartialLeafsWithVarEnv typeEnv ve aLeafs bLeafs
          -- Intersecting one predicate's members against another's meets an external var on one
          -- side (e.g. @Some[/T=$_B]@ from a @PredClass Maybe[/T=$_B]@) with the other side's
          -- bound by recording the meet in the returned env while the leaf keeps the var pointer.
          -- That env goes no further than this expansion, so resolve the newly recorded meets
          -- back into the leaves - otherwise the pointer later resolves to Top and the expansion
          -- is wider than the predicate it came from. Still-unresolved vars stay symbolic
          -- ('substituteKeep'); the same resolve-the-meet-back rule as 'mergeVarEnvVal'.
          newBindings = H.difference venv vaenv
          leafs' = case substituteKeep newBindings (UnionType Nothing leafs []) of
            UnionType Nothing l [] -> l
            _                      -> leafs
      in (leafs', all snd mapped)
    -- De Morgan: ¬(p1 ∧ ... ∧ pn) = ¬p1 ∨ ... ∨ ¬pn.
    -- Compute union of complements directly instead of topFiltered - intersect(expand(pi)),
    -- which avoids approximation errors from PtArgAny/PtArgExact subtraction.
    expandPredsWithNamesFull (PredsNot (PredsAnd ps)) =
      let negPs = map predsNot ps
          mapped = map expandPredsWithNamesFull negPs
          leafs = foldl (H.unionWith S.union) H.empty (map fst mapped)
      in (leafs, any snd mapped)
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
            -- Restore each member's declared data fields so a field-form value (written with
            -- explicit args) is recognized as a member of this class.
            leafs' = case completeDataPartialsTop typeEnv (UnionType Nothing leafs []) of
              UnionType Nothing l [] -> l
              _                      -> leafs
        in (leafs', any snd mapped)
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
        -- In the closed-world program-coverage view ('tePrgmEnv'), relax each member's
        -- explicitly-written fields to 'PTopType' (keys only). A class member is a match /target/
        -- whose field types are supplied by the value being matched; preserving the written field
        -- here reintroduces the member's type parameter (e.g. @head -> $T@ resolving to the class's
        -- instantiated element bound) and spuriously holds a destructured definition's binding
        -- field (which merely binds and ignores the value) outside its declaration. During
        -- inference ('tePrgmEnv' off) the precise field tie is kept, so recursive-type scheme
        -- verification ('verifySchemeActLowers') is unaffected. The true field types always remain
        -- in the data graph.
        mapClassPartial tp@PartialType{ptVars, ptArgs}
          | tePrgmEnv typeEnv = base{ptArgs=fmap (const PTopType) ptArgs}
          | otherwise         = base
          where base = tp{ptVars=fmap (substituteVarsWithVarEnv classVars) ptVars}
    r -> error $ printf "Unknown class %s in expandPartial. Found %s" (show (PClassName ptName)) (show r)


expandClassPartial :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> Type
expandClassPartial typeEnv@TypeEnv{teClassGraph=ClassGraph cg} vaenv PartialType{ptName, ptVars=classVarsP} = expanded
  where
    className = PClassName ptName
    expanded = case graphLookup className cg of
      Just (CGClass (_, PartialType{ptVars=classVarsDecl}, classTypes, _)) -> case classTypes of
        [] -> singletonType $ classPlaceholderLeaf ptName
        -- Restore each member's declared data fields so a field-form value (written with
        -- explicit args) is recognized as a member of this class.
        _  -> completeDataPartialsTop typeEnv $ unionAllTypes typeEnv $ map mapClassType classTypes
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
expandRelPartial typeEnv@TypeEnv{teNames} vaenv relPartial = result
  where
    result = unionAllTypesWithEnv typeEnv vaenv (UnionType Nothing (joinUnionType fromArgs) [] : fromTypeEnv)
    name = ptName relPartial
    fromTypeEnv = typeEnvNamesMatching
    fromArgs = map (\n -> relPartial{ptName=n}) $ relativeNameFilter name $ map pkName $ H.keys $ snd $ splitVarArgEnv vaenv

    typeEnvNamesMatching = tpNames ++ clsNames
    relName = ptName relPartial
    tpNames = map (\n -> singletonType relPartial{ptName=n}) $ relativeNameFilter relName $ S.toList teNames
    clsNames = map (setArgMode H.empty (ptArgMode relPartial) . classPartial . partialVal) $ relativeNameFilter relName $ listClassNames typeEnv

-- ============================================================================
-- * Symbolic class-predicate satisfiability
-- ============================================================================
-- Decides inhabitation (satisfiability) of a conjunction of class/relation/
-- concrete-shape constraints on a single value directly against the class
-- graph, without materializing member unions ('expandPredicates' /
-- 'expandClassPartial') and re-expanding recursive classes.
--
-- Recursion through the class graph (e.g. a class @Eq@ whose parameterized
-- members @ConsList[$T -> Eq]@ / @Maybe[$T -> Eq]@ re-reference the class in
-- their element) is handled by a goal stack (least fixpoint): a goal that
-- recurs on the current derivation path can produce no /finite/ witness - and
-- Catln values are finite - so that path is reported uninhabited instead of
-- being re-expanded. Every query therefore terminates in time polynomial in
-- the class-graph size, where the materializing algebra re-expands the
-- recursive universe combinatorially.
--
-- The engine is CONSERVATIVE TOWARDS SATISFIABILITY: whenever a constraint
-- falls outside the decidable fragment (a 'PredExpr', constants, an unhandled
-- type shape, an oversized DNF) it answers 'True' ("may be inhabited").
-- Callers may therefore only rely on a 'False' answer ("provably empty"),
-- which is exactly the polarity the emptiness/implication/coverage decisions
-- need.

-- | One conjunct constraining a single value in a satisfiability goal.
data SatAtom
  = SatClass PartialType   -- ^ the value has class C (a 'PredClass')
  | SatRel PartialType     -- ^ the value isa R (a 'PredRel')
  | SatLeafs [PartialType] -- ^ the value matches one of these concrete partials
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | A satisfiability goal: a single value satisfying all of 'sgPos' and none of 'sgNeg'.
data SatGoal = SatGoal { sgPos :: [SatAtom], sgNeg :: [SatAtom] }
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | The hypothesis stack of goals on the current derivation path (see 'satGoal').
type SatHyps = S.HashSet SatGoal

-- | A member shape produced by enumerating a class or relation.
data SatCand
  = CandLeaf PartialType -- ^ a concrete member shape
  | CandGoal SatGoal     -- ^ a member described by further predicates (e.g. a subclass edge)

satGoalNorm :: SatGoal -> SatGoal
satGoalNorm (SatGoal pos neg) = SatGoal (L.sort (L.nub pos)) (L.sort (L.nub neg))

mergeSatGoals :: SatGoal -> SatGoal -> SatGoal
mergeSatGoals (SatGoal p1 n1) (SatGoal p2 n2) = SatGoal (p1 ++ p2) (n1 ++ n2)

-- | Bound on DNF size during conversion; a larger formula is out of fragment.
satDNFCap :: Int
satDNFCap = 64

-- | Cartesian conjunction of per-conjunct DNFs, 'Nothing' when it exceeds 'satDNFCap'.
satDNFConj :: [[SatGoal]] -> Maybe [SatGoal]
satDNFConj = foldl step (Just [SatGoal [] []])
  where
    step macc gs = do
      acc <- macc
      let combined = [mergeSatGoals g1 g2 | g1 <- acc, g2 <- gs]
      if length combined > satDNFCap then Nothing else Just combined

-- | DNF of a predicate tree over 'SatAtom' literals under a polarity;
-- 'Nothing' outside the fragment ('PredExpr' needs the type graph).
predsSatDNF :: Bool -> TypePredicates -> Maybe [SatGoal]
predsSatDNF pol (PredsOne p) = do
  a <- case p of
    PredClass c -> Just (SatClass c)
    PredRel r   -> Just (SatRel r)
    PredExpr _  -> Nothing
  Just [if pol then SatGoal [a] [] else SatGoal [] [a]]
predsSatDNF pol (PredsNot p) = predsSatDNF (not pol) p
predsSatDNF True (PredsAnd ps) = mapM (predsSatDNF True) ps >>= satDNFConj
predsSatDNF False (PredsAnd ps) = concat <$> mapM (predsSatDNF False) ps

-- | DNF of "a value is a member of this type"; 'Nothing' outside the fragment.
-- A 'TVInt' variable is local to its enclosing partial (e.g. a class member's own
-- parameter): it is instantiable per value and never refers to the ambient 'vaenv', so it
-- carries no constraint here. Only a 'TVExt' variable resolves against the outer scope.
typeSatDNF :: TypeVarArgEnv -> Type -> Maybe [SatGoal]
typeSatDNF _ PTopType = Just [SatGoal [] []]
typeSatDNF _ (TypeVar _ TVInt) = Just [SatGoal [] []] -- partial-local var: no ambient constraint
typeSatDNF vaenv tv@(TypeVar v TVExt) = case vaenvLookup vaenv v of
  looked | looked == tv -> Just [SatGoal [] []] -- unbound/self-bound var: no constraint
  looked                -> typeSatDNF (H.delete v vaenv) looked -- delete to break var reference cycles
typeSatDNF _ (UnionType _ _ (_:_)) = Nothing -- constants: out of fragment
typeSatDNF _ (UnionType Nothing leafs []) = case splitUnionType leafs of
  [] -> Just [] -- bottom: unsatisfiable
  ls -> Just [SatGoal [SatLeafs ls] []]
typeSatDNF vaenv (UnionType (Just (preds, negLeafs)) pos []) = do
  topGoals <- predsSatDNF True preds
  let negAtoms = [SatLeafs (splitUnionType negLeafs) | not (H.null negLeafs)]
  let topGoals' = map (\g -> g{sgNeg = sgNeg g ++ negAtoms}) topGoals
  posGoals <- typeSatDNF vaenv (UnionType Nothing pos [])
  Just (topGoals' ++ posGoals)

-- | The independent ways to falsify a goal: one falsified positive or one satisfied negative.
satGoalNegChoices :: SatGoal -> [SatGoal]
satGoalNegChoices (SatGoal pos neg) = [SatGoal [] [p] | p <- pos] ++ [SatGoal [n] [] | n <- neg]

-- | SAT of a conjunction of member-of-type constraints; conservative towards 'True'.
typesConjSat :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Bool -> SatHyps -> [Type] -> Bool
typesConjSat typeEnv vaenv gatePos hyps ts = case mapM (typeSatDNF vaenv) ts >>= satDNFConj of
  Nothing    -> True
  Just goals -> any (satGoal typeEnv vaenv gatePos hyps) goals

-- | SAT of @t1 ∧ ¬t2@ (a value of @t1@ escaping @t2@); conservative towards 'True'.
typesSatDiff :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Bool -> SatHyps -> Type -> Type -> Bool
typesSatDiff typeEnv vaenv gatePos hyps t1 t2 = case res of
  Nothing    -> True
  Just goals -> any (satGoal typeEnv vaenv gatePos hyps) goals
  where
    res = do
      d1 <- typeSatDNF vaenv t1
      d2 <- typeSatDNF vaenv t2
      -- ¬(∨ d2) = ∧ ¬g2, and each ¬g2 is a disjunction of single-literal flips
      negConj <- satDNFConj (map satGoalNegChoices d2)
      satDNFConj [d1, negConj]

-- | Enumerate a class's member shapes from the class graph. 'Nothing' when the class is
-- unknown, or when 'gatePos' is set and the class is neither sealed nor in the closed
-- world ('tePrgmEnv') - the open-world gate of 'predImpliesAtom' (an unenumerable class
-- must be treated as possibly containing anything).
classSatCandidates :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Bool -> PartialType -> Maybe [SatCand]
classSatCandidates typeEnv vaenv gatePos PartialType{ptName, ptVars=classVarsP} =
  case H.lookup ptName (scClasses (teSatCache typeEnv)) of
    Just ClassSatInfo{csiSealed=sealed, csiVarsDecl=classVarsDecl, csiTypes=classTypes}
      | gatePos && not (sealed || tePrgmEnv typeEnv) -> Nothing
      | null classTypes -> Just [CandLeaf (classPlaceholderLeaf ptName)]
      | otherwise -> concat <$> mapM candsOfMember classTypes
      where
        classVars = H.unionWith (intersectTypesEnv typeEnv vaenv) classVarsP classVarsDecl
        candsOfMember (TypeVar (TVVar t) _) = case H.lookup t classVars of
          Just v  -> candsOfType v
          Nothing -> Nothing
        candsOfMember (TypeVar (TVArg _) _) = Nothing
        candsOfMember (UnionType Nothing p []) =
          Just (map (CandLeaf . completeLeaf . mapClassPartial) (splitUnionType p))
        candsOfMember t = candsOfType t
        candsOfType t = map CandGoal <$> typeSatDNF vaenv t
        -- Substitute only informative bindings: an uninstantiated parameter (bound to
        -- 'PTopType', the declaration default) leaves the member's own type variable in
        -- place, keeping it FLEXIBLE for the cover checks - parity with the enumeration
        -- paths, which check raw member declarations whose variables bind on demand.
        informativeVars = H.filter (/= PTopType) classVars
        substMemberVar (TypeVar (TVVar v) _) = case H.lookup v informativeVars of
          Just b  -> b
          -- Keep the member's own parameter as a PARTIAL-LOCAL ('TVInt') variable: an
          -- uninstantiated class parameter is flexible per value, and must not be captured
          -- by a like-named binding in the ambient scope (declarations write it as @$_T@,
          -- which is 'TVExt' and would otherwise resolve against the caller's environment).
          Nothing -> TypeVar (TVVar v) TVInt
        substMemberVar t = substituteVarsWithVarEnv informativeVars t
        mapClassPartial tp = tp{ptVars = fmap substMemberVar (ptVars tp)}
        -- Restore declared data fields (as 'expandClassPartial' does via 'completeDataPartialsTop')
        -- so a field-form shape is recognized against this member.
        completeLeaf tp = case completeDataPartialsTop typeEnv (singletonType tp) of
          UnionType Nothing leafs [] -> case splitUnionType leafs of
            [tp'] -> tp'
            _     -> tp
          _ -> tp
    _ -> Nothing

-- | Global views of a 'TypeEnv''s class graph and name universe used by the engine's hot
-- query paths, built strictly at 'TypeEnv' construction by 'mkSatCache': the class-name
-- list ('listClassNames' formerly re-listed 'graphToNodes' per call), the type-name list,
-- and each class's graph-node data ('classSatCandidates' formerly did a 'graphLookup' per
-- call). Pure reorganization of 'teClassGraph' / 'teNames' — nothing here depends on any
-- particular query or operation.
data SatCache = SatCache {
  scClassNames :: ![ClassName],
  scTypeNames  :: ![TypeName],
  scClasses    :: !(H.HashMap TypeName ClassSatInfo)
  }

instance Show SatCache where
  show _ = "SatCache"

-- | The class-graph node data for one class: the 'CGClass' payload minus its doc comment.
data ClassSatInfo = ClassSatInfo {
  csiSealed   :: Sealed,
  csiVarsDecl :: H.HashMap TypeVarName Type,
  csiTypes    :: [Type]
  }

-- | Builds the 'teSatCache' for a 'TypeEnv' with the given class graph and name universe.
mkSatCache :: ClassGraph -> S.HashSet TypeName -> SatCache
mkSatCache (ClassGraph cg) names = SatCache{
    scClassNames = [fromPartialName k | (CGClass{}, k, _) <- nodes],
    scTypeNames = S.toList names,
    scClasses = H.fromList [(n, classInfo d) | (CGClass d, PClassName n, _) <- nodes]
  }
  where
    nodes = graphToNodes cg
    classInfo (sealed, PartialType{ptVars=varsDecl}, classTypes, _) = ClassSatInfo{
        csiSealed = sealed,
        csiVarsDecl = varsDecl,
        csiTypes = classTypes
      }

-- | Enumerate the shapes matching a relation (mirrors 'expandRelPartial'): every known
-- type or vaenv-arg name the relative name matches (carrying the relation's args), plus
-- every matching class (as a class constraint).
relSatCandidates :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> PartialType -> [SatCand]
relSatCandidates typeEnv vaenv relPartial = argCands ++ tpCands ++ clsCands
  where
    relName = ptName relPartial
    argCands = [CandLeaf relPartial{ptName=n} | n <- relativeNameFilter relName (map pkName (H.keys (snd (splitVarArgEnv vaenv))))]
    tpCands = [CandLeaf relPartial{ptName=n} | n <- relativeNameFilter relName (scTypeNames (teSatCache typeEnv))]
    clsCands = [CandGoal (SatGoal [SatClass (partialVal n)] []) | n <- relativeNameFilter relName (listClassNames typeEnv)]

-- | The classes a relative name matches (the class routes of a 'PredRel').
relMatchingClasses :: TypeGraph tg => TypeEnv tg -> PartialType -> [PartialType]
relMatchingClasses typeEnv relPartial = map partialVal (relativeNameFilter (ptName relPartial) (listClassNames typeEnv))

-- | Whether a relation's /name route/ applies to a shape: the relative name matches AND the
-- shape's name is a known type name. Parity with 'expandRelPartial', whose type candidates
-- come only from 'teNames' - a name outside the universe is not matched by a relation.
relNameAdmits :: TypeEnv tg -> PartialType -> PartialType -> Bool
relNameAdmits TypeEnv{teNames} r m =
  relativeNameMatches (ptName r) (ptName m) && S.member (ptName m) teNames

-- | Keyset feasibility of two arg-set intervals intersecting (see 'PtArgMode' and
-- 'intersectPartialsBase'): the intersected interval @[req, cap]@ must be non-empty.
argModesOverlap :: PartialType -> PartialType -> Bool
argModesOverlap PartialType{ptArgs=aArgs, ptArgMode=PtArgMode aLb aUb} PartialType{ptArgs=bArgs, ptArgMode=PtArgMode bLb bUb} =
  not ub' || reqKeys `S.isSubsetOf` capKeys
  where
    allKeys = H.keysSet aArgs `S.union` H.keysSet bArgs
    reqKeys = (if aLb then H.keysSet aArgs else S.empty) `S.union` (if bLb then H.keysSet bArgs else S.empty)
    ub' = aUb || bUb
    capKeys = (if aUb then H.keysSet aArgs else allKeys) `S.intersection` (if bUb then H.keysSet bArgs else allKeys)

-- | Keyset coverage: every arg-set allowed by @sub@ is allowed by @sup@
-- (the @negCoversAllPos@ reasoning of 'differencePartialLeafs').
argModeCovers :: PartialType -> PartialType -> Bool
argModeCovers PartialType{ptArgs=supArgs, ptArgMode=PtArgMode supLb supUb} PartialType{ptArgs=subArgs, ptArgMode=PtArgMode subLb subUb} =
  lbOk && ubOk
  where
    supKeys = H.keysSet supArgs
    subKeys = H.keysSet subArgs
    lbOk = not supLb || (if subLb then supKeys `S.isSubsetOf` subKeys else S.null supKeys)
    ubOk = not supUb || (subUb && subKeys `S.isSubsetOf` supKeys)

-- | SAT(a ∧ b) for two concrete shapes; conservative towards 'True'.
partialsOverlapSat :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Bool -> SatHyps -> PartialType -> PartialType -> Bool
partialsOverlapSat typeEnv vaenv gatePos hyps a b =
  ptName a == ptName b
  && argModesOverlap a b
  && all elemSat (H.elems (H.intersectionWith (,) (ptVars a) (ptVars b)))
  && all elemSat (H.elems (H.intersectionWith (,) (ptArgs a) (ptArgs b)))
  && predsOk
  where
    elemSat (ta, tb) = typesConjSat typeEnv vaenv gatePos hyps [ta, tb]
    predsOk = case predsAnd (ptPreds a) (ptPreds b) of
      PredsNone -> True
      preds -> case predsSatDNF True preds of
        Nothing  -> True
        Just dnf -> any (\g -> satGoal typeEnv vaenv gatePos hyps
                          (mergeSatGoals g (SatGoal [SatLeafs [a{ptPreds=PredsNone}], SatLeafs [b{ptPreds=PredsNone}]] []))) dnf

-- | Proven cover of a shape by another: every value matching @sub@ matches @sup@.
-- Conservative towards 'False' (unproven cover), the safe direction for escape checks.
-- Mirrors 'isSubPartialOfWithEnv': only vars/args present on BOTH sides are compared (a
-- super-side binding absent from sub is instantiation information, not a constraint), and a
-- sub-side element that is an unresolved type variable is flexible - it can be bound to the
-- super's element (as 'intersectTypesWithVarEnv' would) - so it does not defeat the cover.
partialCoversSat :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Bool -> SatHyps -> PartialType -> PartialType -> Bool
partialCoversSat typeEnv vaenv gatePos hyps sup sub =
  ptName sup == ptName sub
  && argModeCovers sup sub
  && all varCovered (H.elems (H.intersectionWith (,) (ptVars sub) (ptVars sup)))
  && all elemCovered (H.elems (H.intersectionWith (,) (ptArgs sub) (ptArgs sup)))
  && supPredsCover
  where
    -- Under projection (matching/dispatch consumers, 'teArgModeProjection'), a Top-valued type
    -- parameter on the value side carries no information — it is equivalent to the parameter
    -- being absent, which the shared-keys discipline already skips. A definition whose element
    -- parameter could not be narrowed (e.g. a mapped element fed by a higher-order argument)
    -- completes to an explicit @[T=Top]@, and must dispatch exactly like the key-free form.
    -- The precise set algebra (projection off) keeps Top ⊄ concrete.
    varCovered (PTopType, _) | teArgModeProjection typeEnv = True
    varCovered p             = elemCovered p
    elemCovered (tSub, tSup)
      | isFlexibleVar tSub = True
      | otherwise = not (typesSatDiff typeEnv vaenv gatePos hyps tSub tSup)
    isFlexibleVar (TypeVar _ TVInt) = True -- partial-local var: bindable per value
    isFlexibleVar (TypeVar v TVExt) = case H.lookup v vaenv of
      Nothing             -> True
      Just (TypeVar v' _) -> v' == v
      _                   -> False
    isFlexibleVar _ = False
    -- sup's own predicates must also hold of every sub value: sub ∧ ¬preds must be unsatisfiable
    supPredsCover = case ptPreds sup of
      PredsNone -> True
      preds -> case predsSatDNF False preds of
        Nothing  -> False
        Just dnf -> not (any (\g -> satGoal typeEnv vaenv gatePos hyps
                               (mergeSatGoals g (SatGoal [SatLeafs [sub{ptPreds=PredsNone}]] []))) dnf)

-- | SAT(m ∧ atom): can a value of concrete shape @m@ satisfy the atom? Conservative towards 'True'.
satAdmits :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Bool -> SatHyps -> PartialType -> SatAtom -> Bool
satAdmits typeEnv vaenv gatePos hyps m atom = case atom of
  SatLeafs ls -> any (partialsOverlapSat typeEnv vaenv gatePos hyps m) ls
  SatClass c -> case classSatCandidates typeEnv vaenv gatePos c of
    Nothing    -> True
    Just cands -> any candAdmits cands
  SatRel r -> nameRoute || classRoute
    where
      nameRoute = relNameAdmits typeEnv r m
                  && partialsOverlapSat typeEnv vaenv gatePos hyps m r{ptName = ptName m}
      classRoute = any (\c -> satAdmits typeEnv vaenv gatePos hyps m (SatClass c)) (relMatchingClasses typeEnv r)
  where
    candAdmits (CandLeaf m') = partialsOverlapSat typeEnv vaenv gatePos hyps m m'
    candAdmits (CandGoal g) = satGoal typeEnv vaenv gatePos hyps (mergeSatGoals g (SatGoal [SatLeafs [m]] []))

-- | SAT(m ∧ ¬atom): can a value of concrete shape @m@ escape the atom? Conservative towards 'True'
-- (an escape is only denied when cover is proven). The shape is first split into its atomic
-- pieces (per union-valued var/arg component - 'splitPartialByUnionVarArgs'), since each piece
-- may be covered by a /different/ member of the atom; a cover need not be single-member.
satEscapes :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Bool -> SatHyps -> PartialType -> SatAtom -> Bool
satEscapes typeEnv vaenv gatePos hyps m0 atom = case splitPartialByUnionVarArgs m0 of
  pieces | length pieces > satDNFCap -> True -- oversized split: conservative
  pieces -> any (\m -> satEscapesAtomic typeEnv vaenv gatePos hyps m atom) pieces

-- | 'satEscapes' for a shape with no union-valued vars/args.
satEscapesAtomic :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Bool -> SatHyps -> PartialType -> SatAtom -> Bool
satEscapesAtomic typeEnv vaenv gatePos hyps m atom = case atom of
  SatLeafs ls -> not (any (\l -> partialCoversSat typeEnv vaenv gatePos hyps l m) ls)
  -- Negative atoms are enumerated regardless of the positive gate (parity with
  -- 'predImpliesAtom', which expands the implied class unconditionally).
  SatClass c -> case classSatCandidates typeEnv vaenv False c of
    Nothing    -> True
    Just cands -> not (any candCovers cands)
  SatRel r -> not (nameCovered || classCovered)
    where
      nameCovered = relNameAdmits typeEnv r m
                    && partialCoversSat typeEnv vaenv gatePos hyps r{ptName = ptName m} m
      classCovered = any (\c -> not (satEscapes typeEnv vaenv gatePos hyps m (SatClass c))) (relMatchingClasses typeEnv r)
  where
    candCovers (CandLeaf m') = partialCoversSat typeEnv vaenv gatePos hyps m' m
    -- m is covered by a predicate-described member iff m ∧ ¬member-goal is unsatisfiable
    candCovers (CandGoal g) =
      not (any (\ng -> satGoal typeEnv vaenv gatePos hyps (mergeSatGoals ng (SatGoal [SatLeafs [m]] [])))
             (satGoalNegChoices g))

-- | Is some (finite) value able to satisfy the goal? Conservative towards 'True'.
-- 'gatePos': when 'True', THIS goal's positive class generator may only be enumerated when
-- the class is sealed or 'tePrgmEnv' is set (the open-world gate); an unenumerable positive
-- class makes the goal conservatively satisfiable. The gate applies only to the outermost
-- enumeration - every nested derivation is closed-world over current members, mirroring the
-- materialized algebra (whose gate sits at the outermost implication/re-expression decision
-- while all downstream expansion is ungated).
satGoal :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Bool -> SatHyps -> SatGoal -> Bool
satGoal typeEnv vaenv gatePos hyps goal0
  | satDebugTrace && trace (printf "[SATG %d gate=%s] %s" (S.size hyps) (show gatePos) (show (satGoalNorm goal0))) False = undefined
  | goal `S.member` hyps = (satDebugTrace && trace (printf "[SATG %d] CYCLE -> False" (S.size hyps)) False) || False -- recursive goal: no finite witness along this path
  -- @X ∧ ¬X@: an atom required positively and negatively at once is contradictory, whatever
  -- the atom means. The enumeration below cannot be trusted to detect this (a candidate on
  -- which BOTH the admit and the escape check are conservative reads as a witness - e.g.
  -- @Ord ∧ Eq ∧ ¬Eq@ found "satisfiable" through a parameterized class member), so decide it
  -- syntactically first.
  | any (`elem` sgNeg goal) (sgPos goal) = False
  | otherwise = case sgPos goal of
      [] -> True -- no positive constraint: the open universe escapes any finite negatives
      _  -> case genCandidates generator of
        Nothing    -> True -- cannot enumerate: conservative
        Just cands -> any checkCand cands
  where
    goal = satGoalNorm goal0
    hyps' = S.insert goal hyps

    -- Prefer the most concrete generator: concrete leafs, then a class, then a relation.
    (generator, restPos) = pickGenerator (sgPos goal)
    pickGenerator atoms = case L.partition isLeafs atoms of
      (l:ls, others) -> (l, ls ++ others)
      ([], _) -> case L.partition isClass atoms of
        (c:cs, others) -> (c, cs ++ others)
        ([], r:rs)     -> (r, rs)
        ([], [])       -> error "satGoal: pickGenerator on empty positives"
    isLeafs SatLeafs{} = True
    isLeafs _          = False
    isClass SatClass{} = True
    isClass _          = False

    genCandidates (SatLeafs ls) = Just (map CandLeaf ls)
    genCandidates (SatClass c)  = classSatCandidates typeEnv vaenv gatePos c
    -- A relation's type-route candidates come from the name universe ('scTypeNames'); an env
    -- without one (e.g. the reduced env of the class-graph 'Semigroup' merge) makes the
    -- enumeration silently incomplete - a goal with no candidates would read as refuted when
    -- the engine merely knows nothing. Treat it as unenumerable (conservatively satisfiable).
    genCandidates (SatRel r)
      | null (scTypeNames (teSatCache typeEnv)) = Nothing
      | otherwise = Just (relSatCandidates typeEnv vaenv r)

    rest = SatGoal restPos (sgNeg goal)

    checkCand (CandGoal g) = satGoal typeEnv vaenv False hyps' (mergeSatGoals g rest)
    checkCand (CandLeaf m)
      | ptPreds m /= PredsNone = case predsSatDNF True (ptPreds m) of
          Nothing  -> True -- member predicates outside the fragment: conservative
          Just dnf -> any (\g -> checkCandLeaf (mergeSatGoals g rest) m{ptPreds=PredsNone}) dnf
      | otherwise = checkCandLeaf rest m
    checkCandLeaf r m = all (satAdmits typeEnv vaenv False hyps' m) (sgPos r)
                     && all (satEscapes typeEnv vaenv False hyps' m) (sgNeg r)

-- | TEMP (development only): when enabled, every engine-wired decision point computes BOTH
-- the symbolic and the materialized answer, returns the materialized one (baseline behavior),
-- and traces any disagreement with its operands - the empirical parity harness.
satCrossCheckEnabled :: Bool
satCrossCheckEnabled = False

-- | TEMP (development only): trace every 'satGoal' entry.
satDebugTrace :: Bool
satDebugTrace = False

-- | Decide via the engine when in-fragment, falling back to the materialized answer;
-- in cross-check mode always return the materialized answer and trace disagreements.
satDecide :: String -> Maybe Bool -> Bool -> Bool
satDecide site engineR oldR
  | not satCrossCheckEnabled = fromMaybe oldR engineR
  | otherwise = case engineR of
      Just r | r /= oldR -> trace (printf "[SATX %s] engine=%s old=%s" site (show r) (show oldR)) oldR
      _                  -> oldR

-- | Engine-backed emptiness of a predicate conjunction ('predsEmpty' fast path):
-- @Just isEmpty@ when the predicates are within the decidable fragment.
predsEmptySymbolic :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> TypePredicates -> Maybe Bool
predsEmptySymbolic typeEnv vaenv preds = do
  goals <- predsSatDNF True preds
  Just (not (any (satGoal typeEnv vaenv False S.empty) goals))

-- | Engine-backed subtype: @Just (t1 ⊆ t2)@, decided as emptiness of @t1 ∧ ¬t2@, when both
-- types are within the decidable fragment. 'gatePos' gates enumeration of t1's classes
-- (the open-world coverage gate).
typeCoveredSymbolic :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Bool -> Type -> Type -> Maybe Bool
typeCoveredSymbolic typeEnv vaenv gatePos t1 t2 = do
  d1 <- typeSatDNF vaenv t1
  d2 <- typeSatDNF vaenv t2
  negConj <- satDNFConj (map satGoalNegChoices d2)
  goals <- satDNFConj [d1, negConj]
  Just (not (any (satGoal typeEnv vaenv gatePos S.empty) goals))

-- | Engine-backed implication between two predicate atoms: @a1 ⟹ a2@ decided as emptiness
-- of @a1 ∧ ¬a2@. 'gatePos' gates enumeration of a1's class (the open-world gate).
-- A memberless (but enumerable) class on the left implies anything vacuously - parity with
-- the enumerate-members check, which is vacuously true on an empty member list (the class
-- placeholder inhabitant is a satisfiability device, not a counterexample to implication).
predAtomImpliesSymbolic :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Bool -> TypePredicate -> TypePredicate -> Maybe Bool
predAtomImpliesSymbolic typeEnv vaenv gatePos a1 a2
  | PredClass c1 <- a1
  , Just ClassSatInfo{csiSealed=sealed, csiTypes=[]} <- H.lookup (ptName c1) (scClasses (teSatCache typeEnv))
  , sealed || tePrgmEnv typeEnv = Just True
  | otherwise = do
      atom1 <- satAtomOf a1
      atom2 <- satAtomOf a2
      Just (not (satGoal typeEnv vaenv gatePos S.empty (SatGoal [atom1] [atom2])))
  where
    satAtomOf (PredClass c) = Just (SatClass c)
    satAtomOf (PredRel r)   = Just (SatRel r)
    satAtomOf (PredExpr _)  = Nothing

-- | Whether a partial is within the fragment of the dispatch-coverage queries below
-- ('satLeafEscapesAll' and its wrappers): no 'PredExpr' predicates and no constant
-- components anywhere in its var\/arg elements. An out-of-fragment dispatch falls back to
-- the materialized accumulation in 'completeTreeSet'.
dispatchInFragment :: PartialType -> Bool
dispatchInFragment PartialType{ptVars, ptArgs, ptPreds} =
  predsInFrag ptPreds && all elemInFrag (H.elems ptVars) && all elemInFrag (H.elems ptArgs)
  where
    predsInFrag = isJust . predsSatDNF True
    elemInFrag PTopType = True
    elemInFrag TypeVar{} = True
    elemInFrag (UnionType _ _ (_:_)) = False -- constants: out of fragment
    elemInFrag (UnionType mtop leafs []) =
      maybe True (predsInFrag . fst) mtop
      && all dispatchInFragment (splitUnionType leafs)

-- | SAT(m ∧ ¬n₁ ∧ … ∧ ¬nₖ): can a single value of shape @m@ escape ALL of @negs@ jointly?
-- The joint-cover decision needed by dispatch coverage ('completeTreeSet' in "TreeBuild"):
-- options that each match only part of a class-\/union-typed position can still jointly
-- cover it (e.g. @Nil@ and @Cons@ arms covering a @~ConsList@ argument). 'satEscapes'
-- checks each negative independently and cannot see such joint covers; this instead
-- refines @m@ by enumerating one discriminating top-level var\/arg position at a time
-- (union components, class members, relation candidates) and requiring every case to be
-- covered. Recursion through the class graph terminates via the goal stack exactly as in
-- 'satGoal': a repeated goal has no finite witness, so it cannot escape. Conservative
-- towards 'True' (escape unproven ⇒ possibly escapes), the safe polarity for coverage
-- (unproven cover ⇒ not covered).
satLeafEscapesAll :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> SatHyps -> PartialType -> [PartialType] -> Bool
satLeafEscapesAll typeEnv vaenv hyps m negs0
  | goal `S.member` hyps = False -- recursive goal: no finite witness along this path
  | any (\n -> partialCoversSat typeEnv vaenv False hyps' n m) negs = False
  | null negs = satGoal typeEnv vaenv False hyps (SatGoal [SatLeafs [m]] []) -- inhabited ⇒ escapes
  | otherwise = case refinements of
      []        -> True -- no discriminating position is refinable: conservative escape
      (cases:_) -> any (\m' -> satLeafEscapesAll typeEnv vaenv hyps' m' negs) cases
  where
    negs = filter (partialsOverlapSat typeEnv vaenv False hyps m) negs0
    goal = satGoalNorm (SatGoal [SatLeafs [m]] [SatLeafs [n] | n <- negs])
    hyps' = S.insert goal hyps

    -- Refinement at the first discriminating position: a var\/arg of m that some negative
    -- genuinely constrains (m's element there can escape the negative's), enumerated into
    -- exhaustive cases. An empty case list means the position (hence m) is uninhabited.
    refinements = mapMaybe refineAt (map (first TVVar) (H.toList (ptVars m)) ++ map (first TVArg) (H.toList (ptArgs m)))
    refineAt (k, tk)
      | any constrains negs = do
          cases <- enumTypeCases tk
          Just [typeSetAux k c m | c <- cases]
      | otherwise = Nothing
      where
        constrains n = case elemAt k n of
          Just nk -> typesSatDiff typeEnv vaenv False hyps' tk nk
          Nothing -> False
    elemAt (TVVar k) PartialType{ptVars} = H.lookup k ptVars
    elemAt (TVArg k) PartialType{ptArgs} = H.lookup k ptArgs

    -- Exhaustive top-level cases of a position's type: union components, class members
    -- (via 'classSatCandidates', gated closed-world like the outer coverage decisions), or
    -- relation candidates. 'Nothing' when not enumerable (then another position may be).
    -- Subtracted 'negLeafs' and non-generator conjuncts are dropped, which only ENLARGES
    -- the enumerated set - sound for cover proofs, conservative for escapes.
    enumTypeCases (UnionType Nothing leafs []) = case splitUnionType leafs of
      ls@(_:_:_) -> Just (map singletonType ls)
      _          -> Nothing -- single leaf: no top-level refinement available
    enumTypeCases (UnionType (Just (preds, _)) pos []) = do
      dnf <- predsSatDNF True preds
      satPos <- case dnf of
        [SatGoal ps _] -> Just ps
        _              -> Nothing
      topCases <- enumAtomCases satPos
      Just (topCases ++ map singletonType (splitUnionType pos))
    enumTypeCases _ = Nothing

    -- Enumerate the first enumerable atom of a positive conjunction, preferring a class
    -- (the most precise generator); remaining conjuncts are dropped (enlarging).
    enumAtomCases atoms = listToMaybe (mapMaybe enumAtom (L.sortOn atomPref atoms))
    atomPref SatClass{} = 0 :: Int
    atomPref SatRel{}   = 1
    atomPref SatLeafs{} = 2
    enumAtom (SatClass c) = classSatCandidates typeEnv vaenv True c >>= mapM candCase
    enumAtom (SatRel r)   = mapM candCase (relSatCandidates typeEnv vaenv r)
    enumAtom (SatLeafs _) = Nothing -- cannot appear in a preds DNF
    candCase (CandLeaf tp) = Just (singletonType tp)
    candCase (CandGoal (SatGoal [a] [])) = case a of
      SatClass c  -> Just (classPartial c)
      SatRel r    -> Just (TopType H.empty (PredsOne (PredRel r)))
      SatLeafs ls -> Just (UnionType Nothing (joinUnionType ls) [])
    candCase CandGoal{} = Nothing -- a compound member goal: not expressible as a case type

-- | Engine-backed completion check of 'completeTreeSet': every value of @full@ is matched
-- by some accumulated option. covered ⟺ UNSAT(full ∧ ¬opt₁ ∧ … ∧ ¬optₖ).
dispatchCoveredSymbolic :: TypeGraph tg => TypeEnv tg -> PartialType -> [PartialType] -> Bool
dispatchCoveredSymbolic typeEnv full opts = not (satLeafEscapesAll typeEnv H.empty S.empty full opts)

-- | Engine-backed accumulation check of 'completeTreeSet': @opt@ matches some value of
-- @full@ that no accumulated option matches yet. SAT(full ∧ opt ∧ ¬acc₁ ∧ … ∧ ¬accₖ).
dispatchAddsSymbolic :: TypeGraph tg => TypeEnv tg -> PartialType -> PartialType -> [PartialType] -> Bool
dispatchAddsSymbolic typeEnv full opt accOpts =
  case intersectTypes typeEnv (singletonType full) (singletonType opt) of
    UnionType Nothing leafs [] -> any (\l -> satLeafEscapesAll typeEnv H.empty S.empty l accOpts) (splitUnionType leafs)
    _ -> True -- out-of-fragment intersection: conservatively adds

isSubPredicateOfWithEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicate -> TypePredicate -> Bool
isSubPredicateOfWithEnv typeEnv vaenv p1 p2 = predImpliesAtom typeEnv vaenv p1 p2

-- | The loose /projection/ arg-keyset relation used by matching\/dispatch consumers (see
-- 'teArgModeProjection'). Returns 'True' when @sub@ definitely cannot project onto @super@
-- on the arg-set dimension. This reproduces the historical (pre-precise) behavior verbatim:
-- the legacy 'PtArgExact'\/'PtArgAny' combinations are handled by the first four cases, and
-- any combination involving a lower-unbounded mode falls back to the interval formula.
-- Crucially, an 'Any'\/upper-unbounded source projecting onto an 'Exact' requirement is
-- feasible iff its listed args ⊆ the requirement's args (extra args are ignored).
looseArgModeRejects :: PtArgMode -> S.HashSet ArgName -> PtArgMode -> S.HashSet ArgName -> Bool
looseArgModeRejects subMode subKeys superMode superKeys =
     (subMode == PtArgExact && superMode == PtArgExact && subKeys /= superKeys)
  || (subMode == PtArgAny && superMode == PtArgAny && not (superKeys `isSubsetOf` subKeys))
  || (superMode == PtArgExact && not (subKeys `isSubsetOf` superKeys))
  || (subMode == PtArgExact && superMode == PtArgAny && S.null subKeys && not (S.null superKeys))
  || (involvesLowerUnbounded && not (lowerOk && upperOk))
  where
    involvesLowerUnbounded = not (ptArgLb subMode) || not (ptArgLb superMode)
    lowerOk = not (ptArgLb superMode) || (if ptArgLb subMode then superKeys `isSubsetOf` subKeys else S.null superKeys)
    upperOk = not (ptArgUb superMode) || (ptArgUb subMode && subKeys `isSubsetOf` superKeys)

-- | A private helper for 'isSubPartialOfWithEnv' that checks while ignore class expansions
isSubPartialOfWithEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> Bool
isSubPartialOfWithEnv TypeEnv{teDebug=True} _ l r | trace (printf "[SUBPARTIAL] %s ⊆ %s" (show l) (show r)) False = undefined
isSubPartialOfWithEnv _ _ PartialType{ptName=subName} PartialType{ptName=superName} | subName /= superName = False
-- Projection (loose) keyset feasibility, used by matching\/dispatch consumers via
-- 'teArgModeProjection' (TreeBuild, Eval, scheme verification). Reproduces the historical
-- relation in which an 'Any'\/upper-unbounded source may project onto an 'Exact' requirement
-- by ignoring extra args. This looseness is genuine projection (not an interval relation)
-- and is intentionally NOT applied to the precise set algebra below.
isSubPartialOfWithEnv typeEnv _ PartialType{ptArgs=subArgs, ptArgMode=subMode} PartialType{ptArgs=superArgs, ptArgMode=superMode}
  | teArgModeProjection typeEnv
  , looseArgModeRejects subMode (H.keysSet subArgs) superMode (H.keysSet superArgs) = False
-- Keyset feasibility on the arg-set interval (see 'PtArgMode'): sub ⊆ super requires
-- super's lower\/upper bound to hold for every arg-set allowed by sub. The minimal
-- arg-set of sub is its required keys (or ∅ when sub is lower-unbounded); the maximal
-- is its keys (or unbounded when sub is upper-unbounded).
isSubPartialOfWithEnv typeEnv _ PartialType{ptArgs=subArgs, ptArgMode=PtArgMode subLb subUb} PartialType{ptArgs=superArgs, ptArgMode=PtArgMode superLb superUb}
  | not (teArgModeProjection typeEnv)
  , not lowerOk || not upperOk = False
  where
    subKeys = H.keysSet subArgs
    superKeys = H.keysSet superArgs
    lowerOk = not superLb || (if subLb then superKeys `isSubsetOf` subKeys else S.null superKeys)
    upperOk = not superUb || (subUb && subKeys `isSubsetOf` superKeys)
isSubPartialOfWithEnv typeEnv vaenv sub@PartialType{ptVars=subVars, ptArgs=subArgs, ptPreds=subPreds} super@PartialType{ptVars=superVars, ptArgs=superArgs, ptPreds=superPreds} = debugTrace typeEnv msg result
  where
    vaenv' = H.union vaenv (substituteWithVarArgEnv vaenv <$> H.unionWith (intersectTypesEnv typeEnv vaenv) (ptVarArg super) (ptVarArg sub))
    hasAll sb sp = and $ H.elems $ H.intersectionWith (isSubtypeOfWithEnv typeEnv vaenv') sb sp
    -- Type parameters: mirrors 'partialCoversSat' — under projection a Top-valued parameter on
    -- the sub side is no constraint (≡ absent, which the shared-keys intersection skips).
    hasAllVars sb sp = and $ H.elems $ H.intersectionWith varOk sb sp
    varOk PTopType _ | teArgModeProjection typeEnv = True
    varOk a b        = isSubtypeOfWithEnv typeEnv vaenv' a b
    result = hasAll subArgs superArgs && hasAllPreds superPreds subPreds && hasAllVars subVars superVars
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
-- A type var resolves to its 'vaenv' binding. A var bound to itself (a self-referential
-- entry, which an unconstrained var may acquire during intersection) carries no information,
-- so treat it as 'PTopType' - exactly as an unbound var already resolves via 'vaenvLookup'.
-- Following it verbatim would re-enter this clause with identical arguments forever.
isSubtypeOfWithEnv typeEnv vaenv (TypeVar v _) t2 = case vaenvLookup vaenv v of
  TypeVar v' _ | v' == v -> isSubtypeOfWithEnv typeEnv vaenv PTopType t2
  looked                 -> isSubtypeOfWithEnv typeEnv vaenv looked t2
isSubtypeOfWithEnv typeEnv vaenv t1 (TypeVar v _) = case vaenvLookup vaenv v of
  TypeVar v' _ | v' == v -> isSubtypeOfWithEnv typeEnv vaenv t1 PTopType
  looked                 -> isSubtypeOfWithEnv typeEnv vaenv t1 looked
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
  -- Symbolic decision (emptiness of @t1 ∧ ¬t2@ against the class graph) avoids materializing
  -- the predicate expansion; fall back to expansion outside the fragment.
  satDecide (printf "posSubPreds %s <= %s" (show t1) (show t2))
    (typeCoveredSymbolic typeEnv vaenv False t1 t2)
    (let (filteredLeafs, _) = expandTypesWithNamesFull typeEnv vaenv t2 (H.keysSet subPartials)
     in isSubtypeOfWithEnv typeEnv vaenv t1 (UnionType Nothing filteredLeafs []))
-- PosPartials-with-constants ⊆ t2: partials check + each constant must be in t2
-- (Only for PosPartials: in NegPartials, cs are excluded constants, not members)
isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing leafs cs@(_:_)) t2 =
  isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing leafs []) t2
  && all (\c -> constantInType typeEnv vaenv c t2) cs
-- NegPartials ⊆ NegPartials: (U_p1-n1) ⊆ (U_p2-n2).
-- Same predicates: sufficient iff n2 ⊆ n1.
-- Differing predicates: sufficient iff predImplies p1 p2 and n2 ⊆ n1; otherwise False.
isSubtypeOfWithEnv typeEnv vaenv t1@(UnionType (Just (p1, n1)) _ _) t2@(UnionType (Just (p2, n2)) _ _)
  | p1 == p2             = isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing n2 []) (UnionType Nothing n1 [])
  | isPredsContradictory p1 = True  -- t1 = ∅ ⊆ anything
  | isPredsContradictory (predsAnd p1 (predsNot p2)) = True  -- p1 ≤ p2 syntactically (p1 ∧ ¬p2 = ∅)
  | predImplies typeEnv vaenv p1 p2
    && isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing n2 []) (UnionType Nothing n1 []) = True
    -- U_p1 ⊆ U_p2 and n2 ⊆ n1, so (U_p1 - n1) ⊆ (U_p2 - n2)
  | predsEmpty typeEnv vaenv p1 = True  -- t1 = ∅ by class-graph disjointness ⊆ anything (fallback: expansion is costly)
  -- Symbolic decision (emptiness of @t1 ∧ ¬t2@); the engine's positive gate reproduces the
  -- 'tePrgmEnv'/'predsSealed' enumeration gate of the re-expression fallback.
  | otherwise = satDecide (printf "topSubTop %s <= %s" (show t1) (show t2))
      (typeCoveredSymbolic typeEnv vaenv True t1 t2)
      oldReexpress
  where
    -- Re-express U_p1 - n1 as a positive type and recurse. Guarded on progress: when the predicate
    -- expands to the universe (e.g. p1 = PredsNone) the difference reproduces the same complement,
    -- which would recurse forever, so require the rewrite to differ from t1 (else conservative False).
    oldReexpress
      | tePrgmEnv typeEnv || predsSealed typeEnv p1
      , let t1' = snd $ differenceTypeWithEnv typeEnv vaenv (expandPredicates typeEnv vaenv p1) (UnionType Nothing n1 [])
      , t1' /= t1 = isSubtypeOfWithEnv typeEnv vaenv t1' t2
      | otherwise = False
-- NegPartials ⊆ PosPartials: filter t1 by superPartials names.
-- hasOutside=True: t1 has elements outside superPartials names.
--   True if preds are syntactically contradictory (t1 = ∅).
--   True if predImpliesPartials establishes U_preds ⊆ superPartials (negLeafs removal can only shrink).
--   Otherwise False (conservative: can't prove subtype without knowing predicate members).
isSubtypeOfWithEnv typeEnv vaenv t1@(UnionType (Just (preds, negLeafs)) _ t1Consts) t2@(UnionType Nothing superPartials _) =
  -- The predicate region (U_preds - negLeafs) and any constants carried alongside it must both be
  -- covered by t2. (Positive leaves of t1 are split off by the "mixed type on left" branch above,
  -- so only the constants remain to add here; previously they were dropped.)
  predsCovered && all (\c -> constantInType typeEnv vaenv c t2) t1Consts
  where
   -- Symbolic decision first (emptiness of @(U_preds − negLeafs) ∧ ¬t2@ against the class
   -- graph, ungated - subsuming the ungated 'predImpliesPartials'/'predsEmpty' routes below);
   -- the materialized chain remains as the out-of-fragment fallback.
   predsCovered = satDecide (printf "predsCovered %s <= %s" (show t1) (show t2))
     (typeCoveredSymbolic typeEnv vaenv False (UnionType (Just (preds, negLeafs)) H.empty []) t2)
     predsCoveredExpand
   predsCoveredExpand = case expandTypesWithNamesFull typeEnv vaenv t1 (H.keysSet superPartials) of
    (_, True)      | isPredsContradictory preds -> True
    (_, True)      | predImpliesPartials typeEnv vaenv preds superPartials -> True
                   -- U_preds ⊆ superPartials implies (U_preds - negLeafs) ⊆ superPartials
    (_, True)      | predsEmpty typeEnv vaenv preds -> True  -- U_preds = ∅ by class-graph disjointness (fallback: costly)
    -- (U_preds - negLeafs) ⊆ t2  ⟺  U_preds ⊆ (t2 ∪ negLeafs)  [set identity]. Recursing isSubtypeOf
    -- on the expanded member union re-expresses the predicate as a positive type WITHOUT
    -- 'differenceTypeWithEnv', and decomposes it per-member through the positive/mixed branches: a
    -- member whose recursive element matches t2's reflexively short-circuits rather than re-expanding.
    -- Guarded on progress: when the expansion reproduces t1 (e.g. preds = PredsNone giving
    -- 'PTopType') recursing would not progress, so give a conservative False.
    (_, True)      | tePrgmEnv typeEnv || predsSealed typeEnv preds
                   , let expanded = expandPredicates typeEnv vaenv preds
                   , expanded /= t1
                   -> isSubtypeOfWithEnv typeEnv vaenv expanded
                        (unionTypesWithEnv typeEnv vaenv t2 (UnionType Nothing negLeafs []))
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
          -- The global (predicate) component and the positive partials of a 'UnionType' are
          -- joined by union; the predicate does not constrain the partials. So expanding a
          -- @Just(preds)@ arg to /just/ @expand(preds) - negLeafs@ would silently drop the
          -- union-joined positive leaves (and constants), under-approximating the arg and
          -- producing a false-positive subtype (e.g. @m(⊤)@, a leaf outside @preds@, would
          -- vanish). Union the partial component back in so the expansion is faithful.
          where expandArg (UnionType (Just (preds, negLeafs)) pos cs) =
                  unionTypesWithEnv typeEnv vaenv
                    (snd $ differenceTypeWithEnv typeEnv vaenv (expandPredicates typeEnv vaenv preds) (UnionType Nothing negLeafs []))
                    (UnionType Nothing pos cs)
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
    partialAlreadyCovered PartialType{ptArgMode=PtArgMode _ False} PartialType{ptArgMode=PtArgMode _ False} = False -- Don't count coverings between upper-unbounded partials because it loses type information
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

-- | Processes partials that have a 'PredClass' predicate. Takes an already-split list of
-- partials (the fused 'compactPartialLeafs' pipeline splits once up front) and produces the
-- merged 'PartialLeafs'. 'unionPartialLeafs' is an order-independent merge, so the result is
-- the same set regardless of the input list's order.
compactPartialsWithClassPred :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> [PartialType] -> PartialLeafs
compactPartialsWithClassPred typeEnv vaenv partials = unionPartialLeafs $ map tryAux partials
  where
    tryAux :: PartialType -> PartialLeafs
    tryAux partial = case tryPredsToList (ptPreds partial) of
      Just []    -> joinUnionType [partial]
      Nothing    -> joinUnionType [partial]
      Just preds -> aux (partial, preds)

    aux :: (PartialType, [TypePredicate]) -> PartialLeafs
    aux (partial, preds) = if null classPreds && null relPreds
      then joinUnionType [partial]
      -- Expand the partial's own class/rel preds and intersect them back in.
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

-- | Checks if a partial has predicates that turn the partial into a 'bottomType' (e.g. a partial with contradictory preds such as isClass A and not isClass A).
-- Operates on a list of partials in the fused pipeline (no intermediate join); each partial is mapped/dropped independently.
compactDisconnectedPreds :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> [PartialType] -> [PartialType]
compactDisconnectedPreds typeEnv vaenv = mapMaybe aux
  where
    -- NOTE: deliberately NOT decided by the symbolic engine. These predicates constrain the
    -- eventual term-rewritten value of a call-shaped partial (e.g. @isLess | PredClass Boolean@),
    -- not the partial's own name, so name-membership emptiness does not apply here.
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
-- Operates on a list of partials in the fused pipeline (a pure filter; no join).
compactBottomType :: [PartialType] -> [PartialType]
compactBottomType = filter (not . containsBottomPartialType)

-- | 'True' when a type is fully concrete: positive leafs only, with no type variables at any
-- nesting depth and no predicates. Such a type cannot act as a live link to any other position.
typeFullyConcrete :: Type -> Bool
typeFullyConcrete (UnionType Nothing leafs _) = all partialOk (splitUnionType leafs)
  where partialOk PartialType{ptVars, ptArgs, ptPreds} = ptPreds == PredsNone && all typeFullyConcrete ptVars && all typeFullyConcrete ptArgs
typeFullyConcrete _ = False

-- | Materializes a partial-local variable reference in an argument (@/v=$T@, 'TVInt') when the
-- partial's own 'ptVars' resolves that variable to a fully concrete type: @Box[T=Integer](v=$T)@
-- → @Box[T=Integer](v=Integer)@. Two orders of resolving the same value otherwise produce
-- incomparable syntactic forms — field-resolved @Box[T=$_T](v=Integer)@ vs param-resolved
-- @Box[T=Integer](v=$T)@ — which 'verifySchemeActLowers' cannot order, so an equivalent re-write
-- is rejected as a grow. Only a fully concrete binding is materialized: a variable-form binding
-- is a live link that must stay symbolic (e.g. recursive element linkage), and a Top or
-- predicate-form binding is a bound, not a resolution.
compactOwnVarRefs :: PartialType -> PartialType
compactOwnVarRefs p@PartialType{ptVars, ptArgs} = p{ptArgs = fmap subst ptArgs}
  where
    subst t@(TypeVar (TVVar v) TVInt) = case H.lookup v ptVars of
      Just b | typeFullyConcrete b -> b
      _                            -> t
    subst t = t

-- The first three stages ('compactBottomType', 'compactDisconnectedPreds',
-- 'compactPartialsWithClassPred') are fused over a single 'splitUnionType': stages 1-2 are pure
-- per-element list filters/maps and stage 3 ends in the order-independent 'unionPartialLeafs'
-- merge, so the 'PartialLeafs' produced after stage 3 is identical regardless of intermediate
-- list order. Fusing them removes two intermediate join+split round-trips (each of which
-- re-hashes every partial) while keeping the order-sensitive 'compactJoinPartials' /
-- 'compactOverlapping' stages operating on exactly the same 'PartialLeafs' as before.
compactPartialLeafs :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> PartialLeafs -> PartialLeafs
compactPartialLeafs typeEnv vaenv =
  compactOverlapping typeEnv
  . compactJoinPartials typeEnv
  . compactPartialsWithClassPred typeEnv vaenv
  . compactDisconnectedPreds typeEnv vaenv
  . map compactOwnVarRefs
  . compactBottomType
  . splitUnionType

-- |
-- Used to simplify and reduce the size of a 'Type'.
-- TODO: This should merge type partials into class partials
compactType :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
compactType TypeEnv{teDisableCompact=True} _ t = t
-- Idempotence fast paths for already-canonical trivial types. 'compactType' is provably the
-- identity on each of these, yet the set algebra re-compacts them constantly (~94% of calls are
-- on <=1-leaf types, ~63% on the empty 'BottomType'). Short-circuiting here avoids rebuilding the
-- same structure through the whole pipeline.
--   * 'BottomType' (empty Nothing-top): 'compactPartialLeafs' on empty is empty, 'foldRecursiveType' leaves it.
--   * a single predicate-free partial with no args/vars: nothing to filter, join, overlap, or recurse into.
compactType _ _ t@BottomType = t
compactType _ _ t@(UnionType Nothing pos [])
  | [PartialArgsOption (vars, args, PredsNone, _)] <- concatMap S.toList (H.elems pos)
  , H.null vars
  , H.null args = t
-- Class-graph-contradictory top: a conjunction of >=2 PredClass atoms can be empty by class-graph
-- disjointness (e.g. two distinct unsealed empty classes expand to distinct placeholders, or two
-- classes with disjoint members) which the syntactic 'isPredsContradictory' below cannot see.
-- Consult 'expandPredicates' - the same oracle the placeholder/expansion paths use - and treat a
-- bottom expansion as contradictory so the symbolic conjunction agrees with its expansion (without
-- this, 'isSubtypeOf X BottomType' is false for an empty class conjunction, breaking distributivity).
-- Must precede the empty-neg/empty-pos clause below, which would otherwise return such a top
-- unchanged. Gated to pure positive PredClass conjunctions to keep the expansion finite, cheap, and
-- non-recursive (the expansion yields positive leafs, which never re-enter this clause).
compactType typeEnv vaenv (UnionType (Just (preds, _)) pos _)
  | predClassConjunctionEmpty preds = compactType typeEnv vaenv (UnionType Nothing pos [])
  where
    predClassConjunctionEmpty (PredsAnd ps) = length ps >= 2 && all isPredClassAtom ps && predsEmpty typeEnv vaenv (PredsAnd ps)
    predClassConjunctionEmpty _              = False
    isPredClassAtom (PredsOne (PredClass _)) = True
    isPredClassAtom _                        = False
-- Syntactically contradictory top (e.g. PredsAnd[p, ¬p]): the universe filtered by an
-- unsatisfiable predicate is empty, so (∅ - neg) ∪ pos = pos. Placed here (like the class-graph
-- clause above) so it precedes the specialized empty-neg / non-empty-neg top clauses below, whose
-- catch-alls would otherwise return such a type un-reduced. This arises during difference/subtype
-- (@t ∩ ¬t@ on a predicated top yields @p ∧ ¬p@); leaving it un-reduced falsely reports monotonicity
-- growth in 'verifySchemeActLowers'.
compactType typeEnv vaenv (UnionType (Just (preds, _)) pos _)
  | isPredsContradictory preds = compactType typeEnv vaenv (UnionType Nothing pos [])
-- Class-graph unsatisfiable top: a negated conjunct implied by the rest (@p ∧ ¬q = ∅@ when @p ⟹ q@),
-- e.g. @PredClass C[/T=a] ∧ ¬PredClass C[/T=b]@ with @a ⊆ b@. The syntactic 'isPredsContradictory'
-- above only sees identical-atom @X ∧ ¬X@; this uses 'predImplies' to also reduce a covariant
-- parameterized-class pair — the difference @C[a] − C[b]@ when @a ⊆ b@ (which otherwise leaves an
-- unreduced @C[a] ∧ ¬C[b]@ residue). Gated on a present negation so the common all-positive
-- conjunctions never pay the 'predImplies' pass.
compactType typeEnv vaenv (UnionType (Just (preds@(PredsAnd ps), _)) pos _)
  | any isNeg ps
  , or [ predImplies typeEnv vaenv (mkAnd rest) q | (PredsNot q, rest) <- picks ps ] =
      compactType typeEnv vaenv (UnionType Nothing pos [])
  where
    isNeg (PredsNot _) = True
    isNeg _            = False
    -- each element paired with the list of all the others
    picks = go []
      where go _   []     = []
            go pre (x:post) = (x, pre ++ post) : go (pre ++ [x]) post
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
-- If the negated partials don't intersect with the predicated domain, the exclusion is vacuous.
-- Use intersection check (A ∩ B = ∅) rather than subtype check (A ⊆ ¬B) since the latter
-- can fail to prove disjointness when the ¬B expansion is approximate (hasOutside=True).
compactType typeEnv vaenv t@(UnionType (Just (preds, negPartials)) posLeafs [])
  | not (H.null negPartials) && H.null posLeafs && preds /= PredsNone =
      if isBottomType $ intersectTypesEnv typeEnv vaenv (UnionType Nothing negPartials []) (TopType H.empty preds)
        then UnionType (Just (preds, H.empty)) H.empty []
        else t
-- ¬(contradictory) = everything: top component is U, simplify to PTopType
compactType typeEnv vaenv (UnionType (Just (PredsNot p, negLeafs)) posLeafs cs)
  | isPredsContradictory p = compactType typeEnv vaenv (UnionType (Just (PredsNone, negLeafs)) posLeafs cs)
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
      any (\c -> relativeNameMatches (ptName r) (ptName c)) predClassAtoms
    isDominatedByPredClass _                      = False
-- PredsAnd: drop a conjunct implied by a sibling (@p ∧ q = p@ when @p ⟹ q@), so a disjunction
-- conjunct absorbed by a stronger sibling collapses - e.g. with @Ord isa Eq@,
-- @PredClass Eq ∧ (PredRel Eq ∨ PredRel Ord) → PredClass Eq@ (the element bound a parameterized
-- type accumulates when its constructor is a member of two related classes). Gated to conjunctions
-- containing a 'PredsNot' (the disjunction/complement form) so the common atom-only conjunctions
-- never pay the pairwise 'predImplies'.
compactType typeEnv vaenv (UnionType (Just (PredsAnd ps, negPartials)) posLeafs cs)
  | any isPredsNotConjunct ps
  , let simplified = dropImpliedConjuncts ps
  , length simplified < length ps =
      compactType typeEnv vaenv $ UnionType (Just (mkAnd simplified, negPartials)) posLeafs cs
  where
    isPredsNotConjunct (PredsNot _) = True
    isPredsNotConjunct _            = False
    -- Drop @q@ when a distinct sibling @p@ implies it; keep one of any mutually-implying pair. Uses
    -- only the cheap /syntactic/ implication (@p ∧ ¬q@ contradictory) - never the expansion-based
    -- 'predsEmpty' - so this hot-path clause stays cheap. The enhanced 'isPredsContradictory'
    -- (which knows @hasClass c ⟹ isa c@) is enough for the disjunction-absorption this targets.
    cheapImplies p q = isPredsContradictory (predsAnd p (predsNot q))
    dropImpliedConjuncts = go []
      where
        go kept [] = reverse kept
        go kept (q:rest)
          | any (\p -> p /= q && cheapImplies p q) (kept ++ rest) = go kept rest
          | otherwise = go (q:kept) rest
-- Top component catch-all (includes PTopType and TopType np PredsNone); preserve constants.
-- Also compact the pos component for structural canonicalization (ensures union commutativity
-- holds structurally: different union orderings that mix Just and Nothing forms produce
-- equivalent pos components after compaction).
compactType typeEnv vaenv (UnionType (Just topData) pos cs)
  | not (H.null pos) =
      let pos' = compactPartialLeafs typeEnv vaenv pos
      in UnionType (Just topData) pos' cs
compactType _ _ t@(UnionType (Just _) _ _) = t
-- TypeVar: unchanged
compactType _ _ t@TypeVar{} = t
-- PosPartials with constants: compact leafs, remove constants covered by leafs
compactType typeEnv vaenv (UnionType Nothing partials consts) =
  let -- Compact the nested argument/variable types (bottom-up) so a recursive arg already
      -- in canonical predicate form is recognised one level up.
      argCompacted = joinUnionType $ map (compactPartialArgs typeEnv vaenv) $ splitUnionType partials
      compactedLeafs = compactPartialLeafs typeEnv vaenv argCompacted
      -- Remove constants whose parent partial type is already in the compacted leafs
      filteredConsts = filter (\c -> not $ isSubtypeOfWithEnv emptyTypeEnv' H.empty (singletonType $ constantPartialType c) (UnionType Nothing compactedLeafs [])) consts
  in foldRecursiveType typeEnv vaenv $ UnionType Nothing compactedLeafs filteredConsts

-- | Recurse 'compactType' into a partial's argument and variable types. Compaction
-- otherwise only operates on the top level of a union, so without this a recursive type
-- nested inside an argument (e.g. the @tail@ of a @Cons@) is never canonicalised.
compactPartialArgs :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType
compactPartialArgs typeEnv vaenv p = p
  { ptArgs = fmap (compactType typeEnv vaenv) (ptArgs p)
  , ptVars = fmap (compactType typeEnv vaenv) (ptVars p)
  }

-- | Fold an explicit union of partials back into a recursive class predicate when the
-- union is semantically equal to that class's expansion (the inverse of
-- 'expandRelPartial'). Recursive types are otherwise unrolled one level deeper each
-- constraint epoch (@ConsList ≡ Nil + Cons(tail=ConsList)@), producing syntactically
-- distinct but semantically equal representations that never reach a fixpoint. Folding
-- restores the finite @PredRel@ form so the solver converges.
foldRecursiveType :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
foldRecursiveType TypeEnv{teClassGraph=ClassGraph cg} _vaenv t@(UnionType Nothing leafs [])
  | length leafList > 1 -- only fold a genuine union; a single partial is already minimal
  , hasExplicitRecursion leafList -- only fold an over-unrolled form, never the canonical depth-1 expansion
  , (relP:_) <- foldCandidates
  = TopType H.empty (PredsOne (PredRel relP))
  where
    leafList = splitUnionType leafs
    tNames = leafNameSet t
    -- @t@ is an (over-)unrolling of a sealed class @C@ when its top-level constructor names
    -- are exactly @C@'s declared constituents (from @class(C,[..])@ in the class graph).
    -- Sealed ⇒ @U_C@ is exactly that union, so folding is sound and independent of the
    -- open/closed-world setting. A structural name match sidesteps a deep 'isEqType', which
    -- cannot prove @Nil+Cons(tail=Nil+Cons(..)) ≡ Nil+Cons(tail=~C)@.
    matchingClassNames = [ fromPartialName cKey
                         | (CGClass (sealed, _, classTypes, _), cKey, _) <- graphToNodes cg
                         , sealed
                         , S.unions (map leafNameSet classTypes) == tNames ]
    -- Prefer a 'PredRel' partial nested within @t@ (it carries the right type-var bindings,
    -- e.g. @ConsList[T=$_T]@); otherwise fall back to the bare class partial. This also
    -- catches recursions that bottom out in 'TopType' rather than an explicit @~C@.
    nestedRels = L.nub $ concatMap relPartialsIn leafList
    foldCandidates = [ rp
                     | cName <- matchingClassNames
                     , rp <- case filter (\r -> relativeNameMatches (ptName r) cName) nestedRels of
                               (r:_) -> [r]
                               []    -> [partialVal cName] ]
foldRecursiveType _ _ t = t

-- | The set of top-level partial names in a union type (empty for non-unions).
leafNameSet :: Type -> S.HashSet TypeName
leafNameSet (UnionType _ pos _) = S.fromList $ map ptName $ splitUnionType pos
leafNameSet _                   = S.empty

-- | True when the union is unrolled at least one extra level: some partial has, within its
-- arguments, an /explicit/ (non-predicate) occurrence of a partial whose name is one of the
-- union's own names. The canonical depth-1 expansion (e.g. @Nil + Cons(tail=~ConsList)@,
-- whose recursive tail is the @PredRel@ form) does NOT qualify, so it is preserved for
-- dispatch; only genuinely over-unrolled chains are folded back to the predicate.
hasExplicitRecursion :: [PartialType] -> Bool
hasExplicitRecursion leafList = any partialRecurses leafList
  where
    names = S.fromList (map ptName leafList)
    partialRecurses PartialType{ptArgs, ptVars} = any argHasName (H.elems ptArgs ++ H.elems ptVars)
    argHasName (UnionType Nothing pos _) = any (\p -> S.member (ptName p) names || partialRecurses p) (splitUnionType pos)
    argHasName _ = False -- predicate/top forms are the canonical recursive reference, not an unrolling

-- | Collect the 'PredRel' partials nested anywhere within a partial's args/vars.
relPartialsIn :: PartialType -> [PartialType]
relPartialsIn PartialType{ptArgs, ptVars} = concatMap relPartialsInType (H.elems ptArgs ++ H.elems ptVars)

relPartialsInType :: Type -> [PartialType]
relPartialsInType (UnionType mtop pos _) = topRels ++ concatMap relPartialsIn (splitUnionType pos)
  where topRels = case mtop of
          Just (preds, _) -> predRelPartials preds
          Nothing         -> []
relPartialsInType _ = []

predRelPartials :: TypePredicates -> [PartialType]
predRelPartials (PredsOne (PredRel p)) = [p]
predRelPartials (PredsAnd ps)          = concatMap predRelPartials ps
predRelPartials (PredsNot p)           = predRelPartials p
predRelPartials _                      = []

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
-- TypeVar ∪ t → resolve the var in vaenv. An unconstrained var is unknown, i.e. 'PTopType'
-- (matching 'vaenvLookup' and the TypeVar handling in 'isSubtypeOfWithEnv'/'complementTypeEnv').
unionTypesWithEnv typeEnv vaenv (TypeVar v _) t = unionTypesWithEnv typeEnv vaenv (vaenvLookup vaenv v) t
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
        -- Absorption: when one predicate region implies the other, p1∨p2 is just the wider side
        -- (same intersected-negs treatment as the equal-preds case). Semantically equal to the
        -- De Morgan fallback below, but keeps a re-derived equivalent predicate syntactically
        -- stable across solver epochs instead of nesting another !(!p1 && !p2) layer per union -
        -- growth 'verifySchemeActLowers' cannot order.
        | predImplies typeEnv vaenv p1 p2 ->
            compactType typeEnv vaenv $
              UnionType (Just (p2, snd $ intersectPartialLeafsWithVarEnv typeEnv vaenv n1 n2)) pos' (L.nub (cs1 ++ cs2))
        | predImplies typeEnv vaenv p2 p1 ->
            compactType typeEnv vaenv $
              UnionType (Just (p1, snd $ intersectPartialLeafsWithVarEnv typeEnv vaenv n1 n2)) pos' (L.nub (cs1 ++ cs2))
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
intersectPartialsBase typeEnv vaenv (PartialType name' aVars aArgs aPreds (PtArgMode aLb aUb)) (PartialType _ bVars bArgs bPreds (PtArgMode bLb bUb)) = do
  (varsVaenvs, vars') <- unzip <$> intersectMap H.empty aVars bVars
  -- Args (unlike vars) are intersected without killing the partial when a value intersects to
  -- bottom: such an arg simply cannot be present. For a *required* arg (a lower-bounded side
  -- lists it) that makes the partial uninhabited; for an *optional* arg (a lower-unbounded mode)
  -- it just drops the arg, leaving the smaller arg-sets of the interval valid.
  let argsMap = H.unionWith subUnion (fmap (vaenv,) aArgs) (fmap (vaenv,) bArgs)
  let args' = fmap snd argsMap
  let bottomKeys = H.keysSet $ H.filter (isBottomType . snd) argsMap
  let argsVaenvs = map fst $ H.elems $ H.filter (not . isBottomType . snd) argsMap
  -- The args' envs are seeded with the whole incoming vaenv, so they carry ambient bindings
  -- (a caller's or an outer partial's vars) alongside the meets discovered here. Folding all
  -- of that into the result's ptVars stamps foreign var slots onto every intersected partial
  -- (e.g. an operator's /B appearing on a Cons leaf). An intersection may narrow the var
  -- slots a partial declares; it must not invent new ones - so keep only the declared keys.
  -- The args' envs are seeded with the whole incoming vaenv, so they carry the caller's ambient
  -- bindings alongside the meets discovered here. The discovered bindings must be kept on the
  -- partial's ptVars - they are returned no other way, and they carry a callee's type-var
  -- resolution (@unwrap[/T=Integer]@ from meeting @/b@'s @Box[/T=$_/T]@ with @Box[/T=Integer]@,
  -- which later resolves the passthrough output @$_T@). An ambient binding that merely passed
  -- through unchanged is dropped: it lives in the caller's env, and stamping it onto every
  -- intersected partial invents foreign var slots (an operator's /B appearing on a Cons leaf).
  let ambientVars = fst $ splitVarArgEnv vaenv
  let argsVarBindings = H.filterWithKey (\k v -> H.lookup k ambientVars /= Just v) $ fst $ splitVarArgEnv $ mergeAllVarEnvs typeEnv argsVaenvs
  let venvs' = mergeAllVarEnvs typeEnv [argsVarBindings, vars']
  let preds' = predsAnd aPreds bPreds
  -- The two arg-sets each constrain the instance args to an interval over the
  -- arg-name subset lattice (see 'PtArgMode'). Their intersection is the interval
  -- [reqKeys, capKeys]: the required (lower) keys are the union of each side's
  -- required keys; the cap (upper) keys are the intersection of each side's cap.
  -- Bottom-valued keys can never be present, so they are removed from the cap (and make the
  -- partial uninhabited if required - see the 'partials'' guard below).
  let okKeys = H.keysSet args' `S.difference` bottomKeys
  let allKeys = okKeys
  let reqKeys = (if aLb then H.keysSet aArgs else S.empty) `S.union` (if bLb then H.keysSet bArgs else S.empty)
  let ub' = aUb || bUb
  let capKeys = ((if aUb then H.keysSet aArgs else okKeys) `S.intersection` (if bUb then H.keysSet bArgs else okKeys)) `S.difference` bottomKeys
  let restrict ks = H.filterWithKey (\k _ -> k `S.member` ks) args'
  let mkPartial lb' ub'' ks = PartialType name' venvs' (restrict ks) preds' (PtArgMode lb' ub'')
  let partials'
        | not (S.null (reqKeys `S.intersection` bottomKeys)) = [] -- a required arg is uninhabited
        | ub' = if reqKeys `S.isSubsetOf` capKeys -- bounded interval [reqKeys, capKeys]
            then if reqKeys == capKeys
              then [mkPartial True True capKeys]         -- single arg-set: exact
              else if S.null reqKeys
                then [mkPartial False True capKeys]      -- [∅, capKeys]: powerset
                -- strict bounded interval: enumerate each arg-set as an exact partial
                else [mkPartial True True (reqKeys `S.union` S.fromList extra) | extra <- powerset $ S.toList (capKeys `S.difference` reqKeys)]
            else []                                       -- infeasible: required exceeds cap
        -- upper-unbounded. Optional keys (listed but not required) that are Top-typed
        -- add no constraint and can be dropped, usually leaving an exact single partial
        -- (required keys with a lower bound, extras allowed). A non-Top optional key
        -- cannot be expressed alongside required keys with a single lower bit, so fall
        -- back to a conservative single partial in that residual case.
        | otherwise =
            let nonTopOptional = S.filter (\k -> H.lookupDefault PTopType k args' /= PTopType) (allKeys `S.difference` reqKeys)
            in if S.null nonTopOptional
                 then [mkPartial (not $ S.null reqKeys) False reqKeys]
                 else [mkPartial (reqKeys == allKeys) False allKeys]
  if null partials' then Nothing else Just (mergeAllVarEnvs typeEnv varsVaenvs, partials')
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
    -- The result env must EXTEND the given one, not replace it: a caller folding several
    -- intersections (e.g. 'expandPredsWithNamesFull' over a predicate conjunction) threads the
    -- returned env into the next step, and a binding recorded by an earlier meet (@/B ↦ Integer@)
    -- must still constrain a later meet of the same var (@$_/B ∩ ∀Eq@) - shared keys merge by
    -- intersection in 'mergeVarEnvs', so knowledge only narrows.
    -- The pairwise intersections, however, are the ALTERNATIVES of distributing ∩ over the
    -- unions (d ∩ (l1 + l2) = d∩l1 + d∩l2), so a var bound by several inhabited alternatives
    -- holds their JOIN across pairs - meeting them would fabricate an empty binding whenever
    -- the alternatives disagree, e.g. a raw applied term kept alongside its settled sibling
    -- binding an output-contract var ($_A ∩ /operator+(..) in one leaf, $_A ∩ Integer in the
    -- other). Positions within one alternative still meet (inside 'intersectPartialsBase'),
    -- and an uninhabited alternative binds nothing.
    liveEnvs = [e | (e, ps) <- concat (H.elems intersected), not (null ps)]
    joinedAlternatives = case liveEnvs of
      []  -> H.empty
      [e] -> e
      es  -> foldr1 (H.unionWith (unionTypes typeEnv)) es
    vaenv' = mergeAllVarEnvs typeEnv [vaenv, joinedAlternatives]
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
          vaenv' = mergeAllVarEnvs typeEnv [vaenv1, vaenv2]
          -- A component intersection can meet an external var with the other component's bound,
          -- recording the meet in its returned env while its leaf keeps the var pointer (e.g.
          -- @Some[/T=$_B]@ with @/B ↦ (Top|Eq)@ recorded). The recombining union resolves var
          -- pointers through the env it is GIVEN, which does not hold those fresh meets - the
          -- pointer would widen to Top. Resolve the newly recorded meets back into the components
          -- first; still-unresolved vars stay symbolic ('substituteKeep', as 'mergeVarEnvVal').
          newBindings = H.difference vaenv' vaenv
          resolveNew | H.null newBindings = id
                     | otherwise = substituteKeep newBindings
      in (vaenv', unionTypesWithEnv typeEnv vaenv (resolveNew topResult) (resolveNew posResult))
intersectTypesWithVarEnv typeEnv vaenv t1 t2@(UnionType (Just _) pos _)
  | not (H.null pos) = intersectTypesWithVarEnv typeEnv vaenv t2 t1
-- Top ∩ Top → union excluded sets, predicates, and constants
intersectTypesWithVarEnv typeEnv vaenv (UnionType (Just (ps1, np1)) (H.null -> True) cs1) (UnionType (Just (ps2, np2)) (H.null -> True) cs2) =
  (vaenv, compactType typeEnv vaenv $ UnionType (Just (predsAnd ps1 ps2, unionPartialLeafs [np1, np2])) H.empty (L.nub $ cs1 ++ cs2))
intersectTypesWithVarEnv _ vaenv t1 t2 | t1 == t2 = (vaenv, t1)
-- TypeVar: constrain in vaenv.  Treats 'TVVar' and 'TVArg' uniformly: both look up in vaenv,
-- and an unknown key is recorded as a fresh constraint.  TVInt/TVExt is informational here:
-- a TVInt that reaches this top level was introduced as a fresh sentinel by an upstream
-- caller (e.g. a constituent's prop ref); the old code's behavior is preserved.
-- A binding equal to the var itself (a self-referential entry an unconstrained var may
-- acquire) carries no constraint: treat it as absent and bind to @t@, rather than
-- intersecting the binding with @t@ - which would re-enter this clause unchanged forever.
intersectTypesWithVarEnv typeEnv vaenv tv@(TypeVar v _) t
  | Just l <- H.lookup v vaenv, not (isSelfVar v l) =
      let i = intersectTypesEnv typeEnv vaenv l t
      in if isBottomType i then (vaenv, BottomType) else (H.insert v i vaenv, tv)
  | otherwise = (H.insert v t vaenv, tv)
  where isSelfVar var (TypeVar var' _) = var' == var
        isSelfVar _   _                = False
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
        -- When filteredLeafs is non-empty, use it directly regardless of hasOutside.
        -- Elements of t2 outside pos's names cannot affect the intersection result,
        -- so filteredLeafs (elements of t2 within pos's names) is sufficient.
        (filteredLeafs, _) | not (H.null filteredLeafs) ->
          intersectTypesWithVarEnv typeEnv vaenv t1 (UnionType Nothing filteredLeafs [])
        (filteredLeafs, False) -> intersectTypesWithVarEnv typeEnv vaenv t1 (UnionType Nothing filteredLeafs [])
        -- filteredLeafs is empty but hasOutside=True: t2 might still constrain t1 symbolically.
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
-- PosPartials ∩ Top(preds, negLeafs) with both preds non-trivial and negLeafs non-empty.
-- Split the top as Top(preds, ∅) ∩ Top(PredsNone, negLeafs) and intersect in sequence:
-- (pos ∩ U_preds) - negLeafs. The first step routes to the negLeafs-empty clause above and the
-- second to the PredsNone-subtract clause, both of which yield positive-partial forms, so this
-- terminates. (Without this clause the pair falls through to the unhandled-case error below.)
intersectTypesWithVarEnv typeEnv vaenv t1@(UnionType Nothing _ []) (UnionType (Just (preds, negPartials)) (H.null -> True) cs)
  | not (H.null negPartials) && preds /= PredsNone =
      let (vaenv', inner) = intersectTypesWithVarEnv typeEnv vaenv t1 (UnionType (Just (preds, H.empty)) H.empty cs)
      in intersectTypesWithVarEnv typeEnv vaenv' inner (UnionType (Just (PredsNone, negPartials)) H.empty [])
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
    -- PtArgAny pos minus PtArgExact neg: pos = {S ⊇ posArgs}, neg = {S = negArgs}. The only
    -- arg-set of pos that neg can cover is exactly negArgs, which requires posArgs ⊆ negArgs.
    -- When posArgs ⊄ negArgs the two are disjoint (every pos instance has a required arg that the
    -- exact neg arg-set omits), so pos survives unchanged → [p1]. (This subsumes the old "neg has
    -- no args but pos does" case.) When posArgs ⊆ negArgs they overlap on the negArgs arg-set;
    -- fall through to the legacy subtractArg handler below.
    differencePartial p1@PartialType{ptArgs=posArgs, ptArgMode=PtArgAny} PartialType{ptArgs=negArgs, ptArgMode=PtArgExact}
      | not (H.keysSet posArgs `S.isSubsetOf` H.keysSet negArgs) = [p1]
    -- The legacy clauses below are only valid when both sides are lower-bounded (the old
    -- modes); any lower-unbounded mode falls through to the general handler at the end.
    differencePartial p1@PartialType{ptArgs=posArgs, ptVars=posVars, ptArgMode=posArgMode} PartialType{ptArgs=negArgs, ptVars=negVars, ptArgMode=negArgMode} | ptArgLb posArgMode && ptArgLb negArgMode && (posArgMode == negArgMode || negArgMode == PtArgExact) = mapMaybe subtractArg (H.toList negArgs) ++ mapMaybe subtractVar (H.toList negVars)
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
          varVal' -> Just p1{ptVars=H.insert varName (snd $ differenceTypeWithEnv typeEnv vaenv varVal' negVarVal) posVars}
    -- PtArgExact - PtArgAny: neg requires at least negArgs; pos (exact) is in neg iff neg's required
    -- keys are all present in pos with matching values. Subtract matching args just like the main case.
    differencePartial p1@PartialType{ptArgs=posArgs, ptVars=posVars, ptArgMode=posArgMode} PartialType{ptArgs=negArgs, ptVars=negVars, ptArgMode=PtArgAny}
      | ptArgLb posArgMode && not (H.keysSet negArgs `S.isSubsetOf` H.keysSet posArgs) = [p1]
      | ptArgLb posArgMode = mapMaybe subtractArg (H.toList negArgs) ++ mapMaybe subtractVar (H.toList negVars)
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
    -- General handler for arg-mode combinations involving the lower-unbounded modes
    -- (the cases above cover the legacy modes verbatim). Reasoning on the arg-set
    -- intervals (see 'PtArgMode'): when neg's interval covers every arg-set allowed by
    -- pos (negL ⊆ posL and posU ⊆ negU), the only pos instances escaping neg are those
    -- failing neg on some per-arg\/var type; subtract those. Otherwise pos has arg-sets
    -- entirely outside neg, so conservatively keep pos.
    differencePartial p1@(PartialType _ posVars posArgs _ (PtArgMode posLb posUb)) (PartialType _ negVars negArgs _ (PtArgMode negLb negUb))
      | negCoversAllPos = mapMaybe subtractArg (H.toList negArgs) ++ mapMaybe subtractVar (H.toList negVars)
      | otherwise = [p1]
      where
        posKeys = H.keysSet posArgs
        negKeys = H.keysSet negArgs
        negLsubPosL = not negLb || (if posLb then negKeys `S.isSubsetOf` posKeys else S.null negKeys)
        posUsubNegU = not negUb || (posUb && posKeys `S.isSubsetOf` negKeys)
        negCoversAllPos = negLsubPosL && posUsubNegU

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
    -- Every subset of the args may be present: that is exactly the arg-mode interval
    -- [∅, args] — lower-unbounded (no arg required), with the source's upper bound kept
    -- (an upper-unbounded source still permits extra args; an upper-bounded one caps at
    -- @args@). Representing the powerset as this single interval partial avoids enumerating
    -- the 2^|args| subsets, which otherwise bloats the union and every compaction over it.
    partials' = joinUnionType $ map asInterval $ splitUnionType partials
    asInterval (PartialType name vars args _ (PtArgMode _ ub)) = PartialType name vars args PredsNone (PtArgMode False ub)
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
mergeVarEnvs typeEnv = H.unionWith mergeVarEnvVal
  where
    -- Intersecting @a ∩ b@ where one side is a concrete type and the other an unbound
    -- external var returns the var (recording the binding in the returned env), so plain
    -- 'intersectTypes' - which discards that env - would keep the wider var-form and lose the
    -- concrete. Apply the intersection's own bindings back to the meet so the resolved
    -- (narrower) type is what the merged env stores.
    mergeVarEnvVal a b = let (venv, t) = intersectTypesWithVarEnv typeEnv H.empty a b
                         in substituteWithVarArgEnv venv t


-- | Applies 'mergeVarEnvs' to many 'TypeVarEnv'
mergeAllVarEnvs :: (TypeGraph tg, Foldable f, Eq k, Hashable k, Show (f (H.HashMap k Type))) => TypeEnv tg -> f (H.HashMap k Type) -> H.HashMap k Type
mergeAllVarEnvs typeEnv = foldr (mergeVarEnvs typeEnv) H.empty

-- | Looks up a 'TypeVarAux' in the 'TypeVarArgEnv', returning 'PTopType' on miss.
-- Treats 'TVVar' and 'TVArg' uniformly: both kinds default to 'PTopType' when absent,
-- matching the TVInt/TVExt semantics where an unconstrained TVExt is unknown (i.e. PTopType).
vaenvLookup :: TypeVarArgEnv -> TypeVarAux -> Type
vaenvLookup vaenv v = H.lookupDefault PTopType v vaenv

-- | Replaces type variables in a 'Type' using the provided 'TypeVarArgEnv'.
-- 'TVVar' and 'TVArg' are handled uniformly via 'vaenvLookup'.  The 'TVInt'/'TVExt' axis
-- is reflected in the descent logic, not the leaf rule: descending into a partial pushes
-- the partial's own bindings onto the env, so a 'TVInt' inside the partial's components
-- resolves to a sibling's bound value, and a 'TVExt' resolves to whatever the caller
-- supplied (and that the partial's bindings did not shadow).
substituteTVExt :: TypeVarArgEnv -> Type -> Type
substituteTVExt vaenv (UnionType Nothing partials []) =
  UnionType Nothing (joinUnionType $ map (substitutePartial vaenv) $ splitUnionType partials) []
  where
    substitutePartial pVaenv partial@PartialType{ptVars, ptArgs, ptPreds} = partial{
        ptVars  = fmap (substituteTVExt pVaenv) ptVars,
        ptArgs  = fmap (substituteTVExt innerVaenv) ptArgs,
        ptPreds = mapTypePreds (mapTypePred (substitutePartial innerVaenv)) ptPreds
        }
      where
        ptVars' = fmap (substituteTVExt vaenv) ptVars
        innerVaenv = joinVarArgEnv ptVars' H.empty
-- Also substitute inside the predicate ('Just') component: a class/rel predicate such as
-- @PredClass ConsList[/T=$_B]@ carries external vars that must resolve the same way as those in
-- the positive leaves (e.g. an arrow whose result type is an abstract-class type like
-- @ConsList[$T -> $_B]@ needs @$_B@ bound from the matched argument). Leaving them unsubstituted
-- kept the result abstract, so downstream completion widened the parameter to Top. Only vars that
-- are actually bound in @vaenv@ are replaced; an external var absent from the env is *kept* (not
-- dropped to Top), so a recursive predicate like @PredClass ConsList[/T=$_T]@ stays linked.
substituteTVExt vaenv (UnionType (Just (preds, negLeafs)) posLeafs consts) =
  UnionType (Just (mapTypePreds (mapTypePred (substitutePartialKeep vaenv)) preds, negLeafs)) posLeafs consts
substituteTVExt vaenv (TypeVar v _) = vaenvLookup vaenv v
substituteTVExt _     t             = t

-- | Substitute vars within a predicate partial, but *only* when the env resolves an external
-- ('TVExt') var to a concrete (non-Top) type. An external var absent from the env — or bound only
-- to Top — is kept, so a recursive predicate like @PredClass ConsList[/T=$_T]@ stays linked rather
-- than widening to Top. Internal ('TVInt') vars are always kept: they name the partial's own
-- parameters and resolve through its @ptVars@, not the outer scope.
substitutePartialKeep :: TypeVarArgEnv -> PartialType -> PartialType
substitutePartialKeep vaenv partial@PartialType{ptVars, ptArgs, ptPreds} = partial{
    ptVars  = fmap (substituteKeep vaenv) ptVars,
    ptArgs  = fmap (substituteKeep innerVaenv) ptArgs,
    ptPreds = mapTypePreds (mapTypePred (substitutePartialKeep innerVaenv)) ptPreds
    }
  where
    ptVars' = fmap (substituteKeep vaenv) ptVars
    innerVaenv = joinVarArgEnv ptVars' H.empty

-- | See 'substitutePartialKeep': substitutes only concrete external bindings, keeping every other
-- var unchanged.
substituteKeep :: TypeVarArgEnv -> Type -> Type
substituteKeep vaenv t@(TypeVar v TVExt) = case H.lookup v vaenv of
  Just b | not (isTop b) -> b
  _                      -> t
  where
    isTop PTopType = True
    isTop _        = False
substituteKeep _     t@(TypeVar _ TVInt) = t
substituteKeep vaenv (UnionType Nothing partials []) =
  UnionType Nothing (joinUnionType $ map (substitutePartialKeep vaenv) $ splitUnionType partials) []
substituteKeep vaenv (UnionType (Just (preds, negLeafs)) posLeafs consts) =
  UnionType (Just (mapTypePreds (mapTypePred (substitutePartialKeep vaenv)) preds, negLeafs)) posLeafs consts
substituteKeep _     t                   = t

-- | Replaces 'TVExt' 'TVVar' references in a 'Type' using the provided 'TypeVarEnv'.
-- Thin wrapper over 'substituteTVExt'.
substituteVarsWithVarEnv :: TypeVarEnv -> Type -> Type
substituteVarsWithVarEnv venv = substituteTVExt (joinVarArgEnv venv H.empty)

-- | Replaces 'TVExt' 'TVVar' references in a 'Type' with an empty env.
substituteVars :: Type -> Type
substituteVars = substituteTVExt H.empty

-- | Replaces 'TVExt' 'TVArg' references in a 'Type' using the provided 'TypeArgEnv'.
-- Thin wrapper over 'substituteTVExt'.
substituteArgsWithArgEnv :: TypeArgEnv -> Type -> Type
substituteArgsWithArgEnv aenv = substituteTVExt (joinVarArgEnv H.empty aenv)

substituteWithVarArgEnv :: TypeVarArgEnv -> Type -> Type
substituteWithVarArgEnv = substituteTVExt

typeGetAux :: TypeVarAux -> PartialType -> Maybe Type
typeGetAux (TVVar v) p = Just $ H.lookupDefault PTopType v $ ptVars p
typeGetAux (TVArg v) p = typeGetArg v p

-- | Resolves TVInt references in @t@ against @partial@'s own bindings, "lifting"
-- @t@ out of the partial's scope.  TVExt references are also substituted via
-- @partial@'s bindings (since the partial is their referenced outer scope from
-- @t@'s point of view); the result lives at @partial@'s outer scope and contains
-- no TVInt refs to @partial@.
typeResolveInner :: PartialType -> Type -> Type
typeResolveInner partial = substituteTVExt vaenv . promote
  where
    vaenv = ptVarArg partial
    -- Only the outermost layer of @t@ is in @partial@'s scope; nested partials have
    -- their own scopes, so their TVInt refs are not promoted.
    promote (TypeVar v TVInt) = TypeVar v TVExt
    promote other             = other

-- | Gets an arg from a partial type, fully resolved against the partial's own bindings.
-- When the arg name is absent, falls back to 'PTopType' for upper-unbounded partials
-- (where the arg may still be present) and 'Nothing' for upper-bounded ones.
typeGetArg :: ArgName -> PartialType -> Maybe Type
typeGetArg argName partial@PartialType{ptArgs, ptVars, ptArgMode} = case H.lookup argName ptArgs of
  Nothing -> if ptArgUb ptArgMode then Nothing else Just PTopType
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

-- | 'typeGetArg' tolerating 'PartialKey' phase drift. An argument key computed from a resolved
-- type ('partialToKey', e.g. via 'inExprSingleton') carries the type's accreted variable names in
-- 'pkVars', while the same logical argument may be stored under an encode-time structural key
-- without them (a call site's higher-order argument @/fn(/v)@ vs the declaration's
-- @/fn[/F,..](/v)@). When the exact key is absent, fall back to the unique stored argument that
-- matches by name and applied-argument names, ignoring 'pkVars'.
typeGetArgLoose :: ArgName -> PartialType -> Maybe Type
typeGetArgLoose argName partial@PartialType{ptArgs}
  | H.member argName ptArgs = typeGetArg argName partial
  | otherwise = case filter matches (H.keys ptArgs) of
      [k] -> typeGetArg k partial
      _   -> typeGetArg argName partial
  where matches k = pkName k == pkName argName && pkArgs k == pkArgs argName

-- | Gets an arg from a type while resolving the variables used in the types ptVars
typesGetArg :: TypeGraph tg => TypeEnv tg -> ArgName -> Type -> Maybe Type
typesGetArg typeEnv argName (UnionType Nothing partialLeafs []) = fmap (unionAllTypes typeEnv) $ mapM (typeGetArg argName) $ splitUnionType partialLeafs
typesGetArg _ _ _ = Nothing

typeSetAux :: TypeVarAux -> Type -> PartialType -> PartialType
typeSetAux (TVVar k) v p@PartialType{ptVars} = p{ptVars=H.insert k v ptVars}
typeSetAux (TVArg k) v p@PartialType{ptArgs} = p{ptArgs=H.insert k v ptArgs}

-- | Undo a widening of an EXTERNAL type variable ('TVExt') back to Top. Wherever @old@ carried such
-- a variable at a nested argument or type-parameter position and @new@ replaced it with Top, keep
-- @old@'s variable. A resolution to a concrete (narrower) type in @new@ is left untouched — only a
-- loss to Top is reverted. This lets 'updateTypeProp' preserve the cross-scope link an explicit
-- external variable carries (e.g. a higher-order argument's @fn(/v=$_A)@ tied to a container element)
-- rather than dropping it to Top during intersection. Internal variables ('TVInt') are ignored:
-- they resolve through their own partial's @ptVars@, so showing as Top is equivalent, not a loss.
restoreLostTVExtVars :: Type -> Type -> Type
restoreLostTVExtVars old new = case (old, new) of
  (TypeVar _ TVExt, PTopType) -> old
  (UnionType _ oldLeafs _, UnionType ntop newLeafs ncs) ->
    let oldByName = splitUnionTypeByName oldLeafs
        restoreKey oldMap k nv = maybe nv (`restoreLostTVExtVars` nv) (H.lookup k oldMap)
        fixPartial np = case H.lookup (ptName np) oldByName of
          Just [op] -> np { ptArgs = H.mapWithKey (restoreKey (ptArgs op)) (ptArgs np)
                          , ptVars = H.mapWithKey (restoreKey (ptVars op)) (ptVars np) }
          _         -> np
    in UnionType ntop (joinUnionType (map fixPartial (splitUnionType newLeafs))) ncs
  _ -> new

updateTypeProp :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> TypeVarAux -> Type -> (TypeVarArgEnv, Type, Type)
updateTypeProp typeEnv vaenv superType propName subType =
  let (vaenv', superType', subType') = updateTypePropInner typeEnv vaenv superType propName subType
  in (vaenv', restoreLostTVExtVars superType superType', subType')

updateTypePropInner :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> TypeVarAux -> Type -> (TypeVarArgEnv, Type, Type)
updateTypePropInner typeEnv vaenv superType propName subType = case superType of
    PTopType -> (vaenv, PTopType, subType)
    TypeVar v _ -> do
      let (vaenv', superType', subType') = updateTypeProp typeEnv vaenv (H.lookupDefault PTopType v vaenv) propName subType
      (H.insert v superType' vaenv', superType, subType')
    t@(UnionType (Just _) _ _) ->
      (vaenv, t, subType)
    UnionType Nothing supPartials [] -> do
      let supPartialList = splitUnionType supPartials
      -- The slot AS STORED, without pointer resolution: a slot holding a partial-internal var
      -- pointer (@/fn(..)=$B@, like a data definition's @/head=$T@) must be recognized as one
      -- and updated through the partial's OWN var; the resolving read ('typeGetAux' ->
      -- 'typeGetArg') follows the pointer to the var's current (often Top) binding, and
      -- writing an update against that would clobber the pointer.
      let rawSlot (TVVar k) p = H.lookup k (ptVars p)
          rawSlot (TVArg k) p = H.lookup k (ptArgs p)
      let intersectedPartials sup@PartialType{ptVars=supVars} sub = case (rawSlot propName sup, typeGetAux propName sup) of
            (Just (TypeVar (TVVar v) TVInt), _) -> do
              let supVar = H.lookupDefault PTopType v supVars
              let newProp = intersectTypesEnv typeEnv vaenv supVar sub
              Just (sup{ptVars=H.insert v newProp supVars}, newProp)
            (_, Just (TypeVar (TVVar v) TVInt)) -> do
              let supVar = H.lookupDefault PTopType v supVars
              let newProp = intersectTypesEnv typeEnv vaenv supVar sub
              Just (sup{ptVars=H.insert v newProp supVars}, newProp)
            (_, Just supProp) -> do
              let newProp = intersectTypesEnv typeEnv vaenv supProp sub
              if isBottomType newProp
                then Nothing
                else Just (typeSetAux propName newProp sup, newProp)
            (_, Nothing) -> Nothing
      case subType of
        UnionType Nothing subPartials [] -> do
          let subPartialList = splitUnionType subPartials
          let (supPartialList', subPartialList') = unzip $ catMaybes $ [intersectedPartials sup (singletonType sub) | sup <- supPartialList, sub <- subPartialList]
          (vaenv, compactType typeEnv vaenv $ UnionType Nothing (joinUnionType supPartialList') [], unionAllTypes typeEnv subPartialList')
        TypeVar v _ -> do
          let tp' = intersectTypesEnv typeEnv vaenv (H.lookupDefault PTopType v vaenv) (unionAllTypesWithEnv typeEnv vaenv $ mapMaybe (typeGetAux propName) supPartialList)
          let vaenv' = H.insert v tp' vaenv
          let tp'' = substituteWithVarArgEnv vaenv' tp'
          let updateSuperPartial p@PartialType{ptVars=pVars} = case (rawSlot propName p, typeGetAux propName p) of
                -- Slot is an internal pointer: maintain the partial's own var, keep the pointer.
                (Just (TypeVar (TVVar vn) TVInt), _) ->
                  Just p{ptVars=H.insert vn (intersectTypes typeEnv (H.lookupDefault PTopType vn pVars) tp'') pVars}
                (_, Nothing) -> Nothing
                (_, Just supTp) | supTp == subType -> Just $ typeSetAux propName subType p
                -- An unconstrained ARG slot whose sub is an EXTERNAL var naming one of this
                -- partial's OWN vars (a declaration's @fn(v -> $_A) -> $_B@ arg under an
                -- object declaring @$B@): initialize the slot as the partial-internal
                -- pointer - 'typeResolveInner's scope promotion in reverse - so the declared
                -- relationship lives in the partial itself (as a data definition's
                -- @/head=$T@ does) and reaches the objects universe for 'BoundedByObjs' to
                -- deliver. Resolving the generic (unbound) var through the constraint env
                -- would store Top and sever the link. Arg slots ONLY: a VAR slot named like
                -- the external var (builtin @ContextIn[$V]@) would become a self-pointer
                -- (@ptVars[V]=$V@) that diverges on resolution.
                (_, Just PTopType) | TVArg _ <- propName, TypeVar (TVVar vn) TVExt <- subType, vn `H.member` pVars ->
                  Just $ typeSetAux propName (TypeVar (TVVar vn) TVInt) p
                (_, Just supTp) -> Just $ typeSetAux propName (intersectTypes typeEnv supTp tp'') p
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
