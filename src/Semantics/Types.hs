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
  teArgModeProjection :: Bool
                          }
  deriving (Show)

defaultTypeEnvDebug :: Bool
defaultTypeEnvDebug = False

emptyTypeEnv' :: TypeEnv EmptyTypeGraph
emptyTypeEnv' = TypeEnv mempty EmptyTypeGraph S.empty False defaultTypeEnvDebug False False

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
      showName = if ptName == anonStr then "" else ptName
      showArg (argName, argVal) = show argName ++ "=" ++ showTypeD (d - 1) argVal
      showTypeVars vars | H.null vars = ""
      showTypeVars vars = "[" ++ intercalate ", " (map showArg $ H.toList vars) ++ "]"
      showArgs args | ptName == anonStr && H.null args = "()"
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

      mergeClassPartials clss@PartialType{ptVars=varsA} PartialType{ptVars=varsB} = clss{ptVars = H.unionWith (unionTypes emptyTypeEnv'{teClassGraph=ClassGraph classGraphA}) varsA varsB}
instance Monoid ClassGraph where
  mempty = ClassGraph $ graphFromEdges []

instance (Semigroup tg) => Semigroup (TypeEnv tg) where
  (TypeEnv cg1 tg1 n1 d1 dc1 pe1 ap1) <> (TypeEnv cg2 tg2 n2 d2 dc2 pe2 ap2) = TypeEnv (cg1 <> cg2) (tg1 <> tg2) (S.union n1 n2) (d1 || d2) (dc1 || dc2) (pe1 || pe2) (ap1 || ap2)


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
predsEmpty typeEnv vaenv preds@(PredsAnd ps) | length ps >= 2 = isBottomType (expandPredicates typeEnv vaenv preds)
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
relativeNameMatches rn n = rn == n || ('/' : rn) `L.isSuffixOf` ('/' : n)

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
    completePartial p@PartialType{ptName, ptVars, ptArgs}
      | H.null ptArgs =
          case [(dArgs, dMode) | PartialType{ptArgs=dArgs, ptArgMode=dMode} <- typeGraphData typeEnv ptName, not (H.null dArgs)] of
            -- Restore the declared fields as 'PTopType' (their presence only, with the def's arg
            -- mode) rather than the def's field types. Matching only needs the field keys to line
            -- up; the field types are supplied by the value being matched. Reusing the def's field
            -- type here would reintroduce the def's type parameter into the partial, which can
            -- collide with a like-named parameter in an enclosing scope.
            ((dArgs, dMode):_) -> p1{ptArgs = fmap (const PTopType) dArgs, ptArgMode = dMode}
            []                 -> p1
      | otherwise = p1
      where p1 = p{ptVars = fmap go ptVars, ptArgs = fmap go ptArgs}

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

classPartial :: PartialType -> Type
classPartial p = TopType H.empty (PredsOne (PredClass p))

-- | Creates a classPlaceholder[$T] partial type for a given class name.
-- Used when expanding a class that has no constituent types to avoid returning the empty set.
classPlaceholderLeaf :: TypeName -> PartialType
classPlaceholderLeaf className = (partialVal classPlaceholderStr){ptVars = H.singleton (partialKey "T") (singletonType $ partialVal className)}

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

predImpliesAtom :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicate -> TypePredicate -> Bool
-- PredClass: c1 implies c2 only when c1 is sealed (all members known) and each member is in U_c2
predImpliesAtom typeEnv@TypeEnv{teClassGraph=ClassGraph cg, tePrgmEnv} vaenv (PredClass c1) (PredClass c2) =
  case graphLookup (PClassName (ptName c1)) cg of
    Just (CGClass (sealed, _, classTypes, _)) | sealed || tePrgmEnv ->
      let c2Type = expandClassPartial typeEnv vaenv c2
      in all (\t -> isSubtypeOfWithEnv typeEnv vaenv t c2Type) classTypes
    _ -> False  -- unsealed or unknown class: can't enumerate all members
-- PredRel: r1 implies r2 when r2's name path is a suffix of r1's (r2 is more general)
-- and r1's args are subtypes of r2's args. Otherwise fall back to expansion, which also
-- captures class-graph relationships between the names (e.g. ConsList isa List).
predImpliesAtom typeEnv vaenv (PredRel r1) (PredRel r2)
  | relativeNameMatches (ptName r2) (ptName r1) && all checkArg (H.keys (ptArgs r2)) = True
  | otherwise = isSubtypeOfWithEnv typeEnv vaenv (expandRelPartial typeEnv vaenv r1) (expandRelPartial typeEnv vaenv r2)
  where checkArg k = isSubtypeOfWithEnv typeEnv vaenv
                       (H.lookupDefault PTopType k (ptArgs r1))
                       (ptArgs r2 H.! k)
-- PredExpr: use structural partial comparison (equality-based simplifying assumption)
predImpliesAtom typeEnv vaenv (PredExpr e1) (PredExpr e2) = isSubPartialOfWithEnv typeEnv vaenv e1 e2
-- PredClass c implies PredRel r when r's name is a suffix of c's name (then
-- expandRelPartial r includes classPartial c, so U_PredClass c ⊆ U_PredRel r), or - like
-- the PredClass\/PredClass case - when c is sealed and each of its members is in U_PredRel r
-- (e.g. c is a subclass of the class that r names).
predImpliesAtom typeEnv@TypeEnv{teClassGraph=ClassGraph cg, tePrgmEnv} vaenv (PredClass c) (PredRel r)
  | relativeNameMatches (ptName r) (ptName c) = True
  | otherwise = case graphLookup (PClassName (ptName c)) cg of
      Just (CGClass (sealed, _, classTypes, _)) | sealed || tePrgmEnv ->
        let rType = expandRelPartial typeEnv vaenv r
        in all (\t -> isSubtypeOfWithEnv typeEnv vaenv t rType) classTypes
      _ -> False
-- PredRel r implies PredClass c when every type matching r is a member of c, checked by expansion.
predImpliesAtom typeEnv vaenv (PredRel r) (PredClass c) =
  isSubtypeOfWithEnv typeEnv vaenv (expandRelPartial typeEnv vaenv r) (expandClassPartial typeEnv vaenv c)
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
        mapClassPartial tp@PartialType{ptVars} = tp{ptVars=fmap (substituteVarsWithVarEnv classVars) ptVars}
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
isSubtypeOfWithEnv typeEnv vaenv t1@(UnionType (Just (p1, n1)) _ _) t2@(UnionType (Just (p2, n2)) _ _)
  | p1 == p2             = isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing n2 []) (UnionType Nothing n1 [])
  | isPredsContradictory p1 = True  -- t1 = ∅ ⊆ anything
  | isPredsContradictory (predsAnd p1 (predsNot p2)) = True  -- p1 ≤ p2 syntactically (p1 ∧ ¬p2 = ∅)
  | predImplies typeEnv vaenv p1 p2
    && isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing n2 []) (UnionType Nothing n1 []) = True
    -- U_p1 ⊆ U_p2 and n2 ⊆ n1, so (U_p1 - n1) ⊆ (U_p2 - n2)
  | predsEmpty typeEnv vaenv p1 = True  -- t1 = ∅ by class-graph disjointness ⊆ anything (fallback: expansion is costly)
  -- Re-express U_p1 - n1 as a positive type and recurse. Guarded on progress: when the predicate
  -- expands to the universe (e.g. p1 = PredsNone) the difference reproduces the same complement,
  -- which would recurse forever, so require the rewrite to differ from t1 (else conservative False).
  | tePrgmEnv typeEnv || predsSealed typeEnv p1
  , let t1' = snd $ differenceTypeWithEnv typeEnv vaenv (expandPredicates typeEnv vaenv p1) (UnionType Nothing n1 [])
  , t1' /= t1 = isSubtypeOfWithEnv typeEnv vaenv t1' t2
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
    (_, True)      | predsEmpty typeEnv vaenv preds -> True  -- U_preds = ∅ by class-graph disjointness (fallback: costly)
    -- Re-express U_preds - negLeafs as a positive type and recurse. Guarded on progress: when the
    -- predicate expands to the universe (e.g. preds = PredsNone) the difference reproduces the same
    -- complement, which would recurse forever, so require the rewrite to differ from t1 (else False).
    (_, True)      | tePrgmEnv typeEnv || predsSealed typeEnv preds
                   , let t1' = snd $ differenceTypeWithEnv typeEnv vaenv (expandPredicates typeEnv vaenv preds) (UnionType Nothing negLeafs [])
                   , t1' /= t1 -> isSubtypeOfWithEnv typeEnv vaenv t1' t2
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
    predClassConjunctionEmpty (PredsAnd ps) = length ps >= 2 && all isPredClassAtom ps && isBottomType (expandPredicates typeEnv vaenv (PredsAnd ps))
    predClassConjunctionEmpty _              = False
    isPredClassAtom (PredsOne (PredClass _)) = True
    isPredClassAtom _                        = False
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
      any (\c -> relativeNameMatches (ptName r) (ptName c)) predClassAtoms
    isDominatedByPredClass _                      = False
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
  let venvs' = mergeAllVarEnvs typeEnv [fst $ splitVarArgEnv $ mergeAllVarEnvs typeEnv argsVaenvs, vars']
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
          varVal' -> Just p1{ptVars=H.insert varName (snd $ differenceTypeWithEnv typeEnv vaenv varVal' negVarVal) posArgs}
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
mergeVarEnvs typeEnv = H.unionWith (intersectTypes typeEnv)


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
substituteTVExt vaenv (TypeVar v _) = vaenvLookup vaenv v
substituteTVExt _     t             = t

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

-- | Gets an arg from a type while resolving the variables used in the types ptVars
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
          let tp' = intersectTypesEnv typeEnv vaenv (H.lookupDefault PTopType v vaenv) (unionAllTypesWithEnv typeEnv vaenv $ mapMaybe (typeGetAux propName) supPartialList)
          let vaenv' = H.insert v tp' vaenv
          let tp'' = substituteWithVarArgEnv vaenv' tp'
          let updateSuperPartial p = case typeGetAux propName p of
                Nothing -> Nothing
                Just supTp | supTp == subType -> Just $ typeSetAux propName subType p
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
