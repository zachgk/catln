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
-- 'isSubtypeOfWithEnv' pairs many view-pattern clauses; the default coverage-checker budget
-- (30 models) is too small for it and degrades the check with a warning.
{-# OPTIONS_GHC -fmax-pmcheck-models=100 #-}

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
-- 'UnionType mTop posLeafs posConsts' semantics:
--   * Nothing:                          posLeafs ∪ posConsts (positive union of partials and constants)
--   * Just (preds, negLeafs, negConsts): (universe filtered by preds − negLeafs − negConsts) ∪ posLeafs ∪ posConsts
-- The 'Just' tuple is the whole subtractive component; the outer 'PartialLeafs' and '[Constant]'
-- are ALWAYS positive members of the union. (Previously a 'Just'-top reused the outer constant
-- list for its exclusions, so "class-top ∪ positive constant" was unrepresentable and forced a
-- lossy expansion of the class-top to its materialized members.)
data Type
  = UnionType (Maybe (TypePredicates, PartialLeafs, [Constant])) PartialLeafs [Constant]
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
               Nothing                -> h `hashWithSalt` (0 :: Int)
               Just (preds, neg, ncs) -> h `hashWithSalt` (1 :: Int)
                                           `hashWithSalt` predsTag preds
                                           `hashWithSalt` H.size neg
                                           `hashWithSalt` length ncs
    in H.foldlWithKey' (\acc k _ -> acc `hashWithSalt` k) h' leafs

-- | Backward-compat pattern: matches a complement/top type (predicates, negative partials).
-- Corresponds to the old @TopType PartialLeafs TypePredicates@ constructor.
pattern TopType :: PartialLeafs -> TypePredicates -> Type
pattern TopType negLeafs preds <- UnionType (Just (preds, negLeafs, [])) (H.null -> True) []
  where TopType negLeafs preds = UnionType (Just (preds, negLeafs, [])) H.empty []

-- | Maps a function over the negative leafs of a 'Just'-top tuple, keeping predicates and
-- excluded constants unchanged.
mapTopNegLeafs :: (PartialLeafs -> PartialLeafs) -> (TypePredicates, PartialLeafs, [Constant]) -> (TypePredicates, PartialLeafs, [Constant])
mapTopNegLeafs f (preds, negLeafs, ncs) = (preds, f negLeafs, ncs)

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
  | TVApplied TypeVarAux !(H.HashMap TypeVarName Type) -- ^ A type variable re-applied with new parameter bindings (@$F[$T -> $_B]@): whatever the base variable resolves to, with the named parameter slots rebound ('applyTypeVarOverrides')
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
  -- | Queries the type graph for possible conversions from a partial type to a type. Each result
  -- carries the reason for the conversion, the portion of the source partial the arrow accepts
  -- (its object intersected with the source — the whole source when the arrow covers it), and
  -- the destination type.
  typeGraphQueryWithReason :: TypeEnv tg -> TypeVarArgEnv -> PartialType -> [(String, Type, Type)]

  -- | Queries the type graph for possible conversions from a TopTypes PredExpr to the possible partialTypes that can contain the PredExpr
  typeGraphExpandPredExpr :: TypeEnv tg -> TypeVarArgEnv -> PartialType -> Maybe [PartialType]

  -- | Returns the data-definition object partials for a type name, carrying the declared
  -- relationship between its 'ptVars' (type parameters) and 'ptArgs' (fields). Empty if the
  -- name has no data definition. Used to complete a param-form annotation (e.g. @Box[T]@)
  -- back to its full field form (@Box[T](v=$T)@).
  typeGraphData :: TypeEnv tg -> TypeName -> [PartialType]

  -- | The declared RESULT type of an applied form — the destination of its DECLARATION
  -- arrow(s) resolved against the source partial. A raw/recursive applied form
  -- (@sorted(raw)@, @fmap(raw)@) uses this as the type it will settle to, so the reach can
  -- follow any arrow the settled value could reach even before it settles (see
  -- 'Semantics.TypeGraph.presentRawArgResults'). An undeclared function has no declaration
  -- arrow and so starts at 'PTopType', refining as inference narrows its arms.
  typeGraphDeclaredResult :: TypeEnv tg -> TypeVarArgEnv -> PartialType -> Type

  -- | The declared arg-name sets for a type name, one keyset per object declaration (data
  -- AND function objects, unlike 'typeGraphData'). An instance's args are always a subset
  -- of some declared arg-set (partial application never adds args), so these bound the
  -- realizable arg-sets of a moded partial (see 'normalizePtArgMode'). 'Nothing' when the
  -- name has no declaration in the graph: its realizable arg-sets are then unknown (open
  -- world) and no arg-mode reasoning may assume a bound.
  typeGraphArgKeys :: TypeEnv tg -> TypeName -> Maybe [S.HashSet ArgName]
  typeGraphArgKeys _ _ = Nothing

data EmptyTypeGraph = EmptyTypeGraph
instance Semigroup EmptyTypeGraph where
  _ <> _ = EmptyTypeGraph
instance TypeGraph EmptyTypeGraph where
  typeGraphQueryWithReason _ _ _ = []
  typeGraphExpandPredExpr _ _ _ = Nothing
  typeGraphData _ _ = []
  typeGraphDeclaredResult _ _ _ = PTopType

typeGraphQuery :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> [Type]
typeGraphQuery typeEnv vaenv src = map (\(_, _, dest) -> dest) $ typeGraphQueryWithReason typeEnv vaenv src

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

-- | Renders a list of already-shown union parts: "∅", a lone part, or a "+"-joined group.
showJoinParts :: [String] -> String
showJoinParts []  = "∅"
showJoinParts [p] = p
showJoinParts ps  = "(" ++ intercalate " + " ps ++ ")"

showTypeD :: Int -> Type -> String
showTypeD d _ | d <= 0 = showTruncated
showTypeD d (UnionType Nothing leafs []) = showJoinParts $ map (showPartialD (d - 1)) $ splitUnionType leafs
showTypeD d (UnionType (Just (p, negPartials, negConsts)) (H.null -> True) []) = p' ++ negPartials' ++ negConsts'
    where
      p' = case tryPredsToList p of
                Just [PredClass c] -> "∀" ++ showPartialD (d - 1) c
                Just [PredRel c]   -> "~" ++ showPartialD (d - 1) c
                Just []            -> "TopType"
                _                  -> "(TopType | " ++ showPredsD (d - 1) p ++ ")"
      negPartials' = if H.null negPartials
        then ""
        else " - " ++ (showJoinParts $ map (showPartialD (d - 1)) $ splitUnionType negPartials)
      negConsts' = if null negConsts
        then ""
        else " - " ++ show negConsts
showTypeD d (UnionType mTop leafs consts) = "UnionType(" ++ show mTop ++ ", " ++ showJoinParts (map (showPartialD (d - 1)) $ splitUnionType leafs) ++ ", " ++ show consts ++ ")"
showTypeD _ (TypeVar (TVVar n) TVInt) = "$" ++ show n
showTypeD _ (TypeVar (TVVar n) TVExt) = "$_" ++ show n
showTypeD _ (TypeVar (TVArg n) TVInt) = "TVArg " ++ show n
showTypeD _ (TypeVar (TVArg n) TVExt) = "TVArg(ext) " ++ show n
showTypeD d (TypeVar (TVApplied b ovr) loc) = showTypeD d (TypeVar b loc) ++ "[" ++ intercalate ", " (map showOvr (H.toList ovr)) ++ "]"
    where showOvr (k, v) = show k ++ "=" ++ showTypeD (d - 1) v

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

      mergeClassPartials clss@PartialType{ptVars=varsA} PartialType{ptVars=varsB} = clss{ptVars = H.unionWith (unionTypes mergeTypeEnv) varsA varsB}
      -- Shared across all class merges of this (<>): building the sat cache is O(class graph).
      mergeTypeEnv = emptyTypeEnv'{teClassGraph=ClassGraph classGraphA, teSatCache=mkSatCache (ClassGraph classGraphA) S.empty}
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

-- | Conjoin two top-component predicate formulae, UNIFYING same-class atoms instead of
-- accumulating them. When both sides assert @PredClass C[..]@ (or @PredRel C[..]@) for the same
-- class @C@, the two class 'PartialType's are intersected (threading the var env) into one atom:
-- e.g. @PredClass C[/T=∀Ord] ∧ PredClass C[/T=$_/T]@ becomes @PredClass C[/T=$_/T]@ with @$_/T ↦
-- ∀Ord@ recorded in the returned env. Unlike a post-hoc 'compactType' (which is 'Type -> Type' and
-- cannot record the binding, so it would have to DROP a bound), this keeps every bound: the tighter
-- one survives as a monotonic var binding rather than a lost conjunct. Without it a recursive
-- parameterized type grows a redundant same-class conjunct each epoch and never converges. A pair
-- that does not intersect to a single partial is left as separate conjuncts.
mergePredsAnd :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicates -> TypePredicates -> (TypeVarArgEnv, TypePredicates)
mergePredsAnd typeEnv vaenv0 ps1 ps2 = (vaenvF, rebuild keptF)
  where
    (vaenvS, keptS) = foldl step (vaenv0, []) (conjOf ps1 ++ conjOf ps2)
    -- After merging same-name atoms, propagate an element bound carried by a superCLASS atom
    -- into a subclass atom's free element via the declared (element-preserving) @isa@ edge:
    -- @ConsList[$_T] ∧ List[Integer]@ becomes @ConsList[Integer] ∧ List[Integer]@. The two
    -- conjuncts then agree, so a recursive element does not oscillate free/bound across epochs
    -- (a 'verifySchemeActLowers' monotonicity failure). Sound because @every(ConsList[$T] isa
    -- List[$T])@ makes ConsList's element the same as List's - the superclass's enumerated member
    -- of the subclass's name carries exactly that mapping, so meeting against it only refines.
    (vaenvF, keptF) = pinIsa vaenvS keptS
    pinIsa v atoms
      | length classPs < 2 = (v, atoms)
      | otherwise          = L.mapAccumL pinAtom v atoms
      where
        classPs = [c | PredsOne (PredClass c) <- atoms]
        pinAtom v' (PredsOne (PredClass ca)) =
          let (v'', ca') = foldl pinBy (v', ca) [cb | cb <- classPs, ptName cb /= ptName ca]
          in (v'', PredsOne (PredClass ca'))
        pinAtom v' other = (v', other)
        pinBy (v', ca) cb = case classSatCandidates typeEnv v' False cb of
          Just cands -> case [m | Just m <- map candClassPartial cands, ptName m == ptName ca] of
            (m:_) -> case intersectPartials typeEnv v' ca m of
              (vd, [r]) -> (mergeAllVarEnvs typeEnv [v', vd], r)
              _         -> (v', ca)
            []    -> (v', ca)
          Nothing -> (v', ca)
        candClassPartial (CandGoal (SatGoal [SatClass m] [])) = Just m
        candClassPartial (CandLeaf m)                         = Just m
        candClassPartial _                                    = Nothing
    conjOf (PredsAnd xs) = concatMap conjOf xs
    conjOf PredsNone     = []
    conjOf p             = [p]
    atomKC (PredsOne (PredClass c)) = Just (0 :: Int, c)
    atomKC (PredsOne (PredRel   r)) = Just (1 :: Int, r)
    atomKC _                        = Nothing
    mkAtom 0 p = PredsOne (PredClass p)
    mkAtom _ p = PredsOne (PredRel   p)
    -- Each unification EXTENDS the accumulated env with the meet's discoveries
    -- ('intersectPartials' returns the discovered bindings only): a later step that
    -- discovers nothing must not wipe an earlier step's binding (@ConsList[$_T] ∧
    -- ConsList[Int]@ records @$_T ↦ Int@, which a following @List[Int] ∧ List[Int]@
    -- unification would otherwise erase, leaving the kept flexible form unresolvable).
    step (v, kept) atom = case atomKC atom of
      Nothing     -> (v, kept ++ [atom])
      Just (k, p) -> case break (sameKN k p) kept of
        (pre, ex:post) | Just (_, ep) <- atomKC ex ->
          case intersectPartials typeEnv v ep p of
            (v', [m]) -> (mergeAllVarEnvs typeEnv [v, v'], pre ++ mkAtom k m : post)
            _         -> (v, kept ++ [atom])
        _ -> (v, kept ++ [atom])
    sameKN k p a = case atomKC a of
      Just (k', p') -> k == k' && ptName p' == ptName p
      Nothing       -> False
    rebuild []  = PredsNone
    rebuild [p] = p
    rebuild xs  = mkAnd xs

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
  -- Outside the symbolic fragment (a 'PredExpr', constants, an oversized formula): decide
  -- by materialized expansion.
  fromMaybe (isBottomType (expandPredicates typeEnv vaenv preds))
    (predsEmptySymbolic typeEnv vaenv preds)
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
pattern PTopType <- UnionType (Just (PredsNone, H.null -> True, [])) (H.null -> True) []
  where PTopType = UnionType (Just (PredsNone, H.empty, [])) H.empty []

-- | The 'Type' containing no possible values, equivalent to the empty set ∅.
-- It often corresponds to errors in the type inference process and indicates the error type.
pattern BottomType :: Type
pattern BottomType <- UnionType Nothing (H.null -> True) []
  where BottomType = UnionType Nothing H.empty []

-- | Used to check if a type is equivalent to 'bottomType'.
-- It can be necessary because it is possible for non-compacted types (see 'compactType') to be bottom types but not equal to 'bottomType'.
isBottomType :: Type -> Bool
isBottomType t = t == BottomType

isTypeVar :: Type -> Bool
isTypeVar TypeVar{} = True
isTypeVar _         = False

-- | Whether a type references any type variable ('TypeVar') at any nesting depth, including
-- variable\/argument slot values and predicate partials.
typeContainsTypeVar :: Type -> Bool
typeContainsTypeVar TypeVar{} = True
typeContainsTypeVar (UnionType mPreds leafs _) = predsPart || any partialHas (splitUnionType leafs)
  where
    predsPart = case mPreds of
      Just (preds, negLeafs, _) -> predsHas preds || any partialHas (splitUnionType negLeafs)
      Nothing                   -> False
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
    eitherVarArg (TVApplied v _, _) = error $ printf "Unexpected TVApplied %s used as an env key in splitVarArgEnv" (show v)

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
    onArgs (UnionType mPreds leafs consts) = UnionType (fmap (mapTopNegLeafs onArgsLeafs) mPreds) (onArgsLeafs leafs) consts
    onArgsLeafs = joinUnionType . map recurseArgs . splitUnionType
    recurseArgs p@PartialType{ptVars, ptArgs} = p{ptVars = fmap (completeDataPartialsTop typeEnv) ptVars, ptArgs = fmap (completeDataPartialsTop typeEnv) ptArgs}

-- | Like 'completeDataPartials' but also completes the /outermost/ partials, restoring their
-- declared data fields. Used when a partial is a supertype to match against rather than a value:
-- a partial may carry only its type parameters and omit the declared fields, in which case a
-- field-form value (one written with explicit args) would not reconcile with it. Restoring the
-- fields as 'PTopType' lets such a value match on its keys. (Fields and type parameters are
-- otherwise tracked independently and are not auto-related.)
--
-- This is NOT a semantic no-op: a bare argument-less exact partial like @/Cons@ is itself a
-- value — the partial-application form a @/Cons@ has before its args are applied (arg-set @{}@,
-- disjoint from the field forms) — and completing it maps it to a DIFFERENT type. Apply this
-- only where the bare form is known to stand for the field forms (a supertype/bound to match
-- against), never as a general canonicalization of value types.
completeDataPartialsTop :: (TypeGraph tg) => TypeEnv tg -> Type -> Type
completeDataPartialsTop typeEnv = go
  where
    -- Nested: complete each partial against its def, then recurse into its args/vars.
    go (TypeVar v l) = TypeVar v l
    go (UnionType mPreds leafs consts) = UnionType (fmap (mapTopNegLeafs goLeafs) mPreds) (goLeafs leafs) consts
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

-- | Narrow each partial's present fields to their /concrete/ declared type. A destructure pattern
-- (@#md(text)@) leaves its field's actual at Top; the declaration @#md(text -> String)@ says the
-- field IS a @String@, so an LHS input actually is one. Unlike 'completeDataPartialsTop' (which
-- adds missing fields as Top for keying), this tightens an /already-present/ field to its declared
-- type. Only a 'typeFullyConcrete' declared field type is carried — a parameter-typed field
-- (@v -> $A@, or a still-parameterized @tail -> ConsList[$T]@) is left as inference has it, so no
-- free type parameter is reintroduced. Recurses into nested args/vars.
completeConcreteFields :: (TypeGraph tg) => TypeEnv tg -> Type -> Type
completeConcreteFields typeEnv = onUnion
  where
    onUnion (UnionType mPreds leafs consts) = UnionType (fmap (mapTopNegLeafs onLeafs) mPreds) (onLeafs leafs) consts
    onUnion t = t
    onLeafs = joinUnionType . map onPartial . splitUnionType
    onPartial p@PartialType{ptName, ptArgs, ptVars} =
      let recArgs = fmap onUnion ptArgs
          p1 = p{ptArgs = recArgs, ptVars = fmap onUnion ptVars}
      in case [dArgs | PartialType{ptArgs=dArgs} <- typeGraphData typeEnv ptName, not (H.null dArgs)] of
           (dArgs:_) -> p1{ptArgs = H.mapWithKey (narrowField dArgs) recArgs}
           []        -> p1
    narrowField dArgs k v = case H.lookup k dArgs of
      Just dt | typeFullyConcrete dt -> intersectTypesEnv typeEnv H.empty v dt
      _                              -> v

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
    onUnion (UnionType mPreds leafs consts) = UnionType (fmap (mapTopNegLeafs onLeafs) mPreds) (onLeafs leafs) consts
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
    fieldRefNames (UnionType (Just (preds, negLeafs, _)) ls _)
      | H.null ls && H.null negLeafs = case tryPredsToList preds of
          Just ps -> [ptName r | PredRel r <- ps]
          Nothing -> []
    fieldRefNames (UnionType Nothing ls []) = case splitUnionType ls of
      [PartialType{ptName=n}] -> [n]
      _                       -> []
    fieldRefNames _ = []

    sameName a b = makeAbsoluteName a == makeAbsoluteName b

-- | The absolute names of a class's constituents, transitively. A class's constituents are its
-- directly declared members plus the constituents of any member that is itself a class, following
-- the @isa@ chain. For example @class(List[$T])@ declares no members; @every(ConsList isa List)@
-- makes @ConsList@ its only direct constituent, and @class(ConsList[$T],[Cons,Nil])@ makes
-- @Nil@\/@Cons@ ConsList's — so @classConstituentNames List@ yields all three. Without the descent
-- a @List@-topped union would never recognise its @Nil@\/@Cons@ leaves as constituents (they belong
-- to ConsList, not List). The visited set guards against cycles. Empty for a non-class name.
classConstituentNames :: (TypeGraph tg) => TypeEnv tg -> TypeName -> S.HashSet TypeName
classConstituentNames TypeEnv{teClassGraph=ClassGraph cg} = go S.empty
  where
    go visited cName
      | S.member absName visited = S.empty
      | otherwise = case graphLookup (PClassName cName) cg of
          Just (CGClass (_, _, classTypes, _)) ->
            let direct = S.map makeAbsoluteName $ S.unions $ map memberNames classTypes
            in S.union direct (S.unions [go (S.insert absName visited) d | d <- S.toList direct])
          _ -> S.empty
      where absName = makeAbsoluteName cName
    -- A class member is stored either as a positive leaf (@Cons@, @Nil@ under @ConsList@) or, for a
    -- subclass reached via @every … isa …@, as a predicate-top form (@List@'s member @ConsList@ is
    -- @∀ConsList@, a @Just (PredClass ConsList)@ with no positive leaf). 'leafNameSet' sees only the
    -- former, so also read the class\/rel atom names out of the predicate top.
    memberNames t = leafNameSet t `S.union` topAtomNames t
    topAtomNames (UnionType (Just (preds, _, _)) _ _) =
      S.fromList $ mapMaybe atomName $ fromMaybe [] (tryPredsToList preds)
    topAtomNames _ = S.empty
    atomName (PredClass p) = Just (ptName p)
    atomName (PredRel p)   = Just (ptName p)
    atomName (PredExpr _)  = Nothing

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

-- | The open-class predicate component of a type, for use as an additional 'BoundedByObjs'
-- bound. The objects universe is closed-world (only currently-declared data objects), but an
-- /open/ (unsealed) class may gain instances in a future file, so a value typed by one is
-- genuinely inhabitable and must survive the bound rather than empty (e.g. a @Functor[Eq]@
-- argument in a law with no in-scope instance). Returns the value's own predicate-top (with its
-- real element bounds) when its predicates reference an unsealed class, so intersecting it back
-- preserves exactly that component; 'BottomType' otherwise (union-neutral, leaving the bound
-- unchanged). Using the value's own component - rather than a synthetic @any C@ with an
-- unconstrained element - avoids widening the value's element to 'TopType'.
openClassPart :: (TypeGraph tg) => TypeEnv tg -> Type -> Type
openClassPart typeEnv (UnionType (Just (preds, negLeafs, _)) _ _)
  | predsReferenceUnsealed preds = TopType negLeafs preds
  where
    predsReferenceUnsealed = go
      where
        go (PredsAnd ps) = any go ps
        go (PredsNot p)  = go p
        go PredsNone     = False
        go (PredsOne a)  = case a of
          PredClass c -> unsealed (ptName c)
          PredRel   c -> unsealed (ptName c)
          _           -> False
        unsealed n = maybe False (not . csiSealed) $ H.lookup n (scClasses (teSatCache typeEnv))
openClassPart _ _ = BottomType

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
maybeGetTypeName t | Just p <- maybeGetSingleton t = Just $ ptName p
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
suffixLookup s = L.find (relativeNameMatches s)

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
predImplies typeEnv vaenv p1@(PredsAnd ps) p2      = any (\p -> predImplies typeEnv vaenv p p2) ps || allCheck || cancellationCheck || directContradictCheck || jointEmptyCheck
  -- if any conjunct alone implies p2, the intersection does too
  -- if p2 is a conjunction, p1 implies p2 iff p1 implies each conjunct of p2
  -- also: (X₁ ∨ X₂ ∨ ... ∨ q) ∧ ¬X₁ ∧ ¬X₂ ∧ ... ⊆ q  (disjunction cancellation)
  -- direct contradiction: p1 → ¬q iff p1 ∧ q = ∅
  where
    allCheck = case p2 of
      PredsAnd qs -> all (predImplies typeEnv vaenv p1) qs
      _           -> False
    -- Definitional fallback @p1 ⊆ p2 ⟺ p1 ∧ ¬p2 = ∅@, deciding the WHOLE conjunction jointly (via
    -- 'predsEmpty'\/the SAT engine) rather than conjunct-by-conjunct. A joint constraint the atoms do
    -- not carry alone is then seen: @Monad[Integer] ∧ ConsList ⊆ DShow@ holds because the two atoms
    -- together pin the list element to Integer (a @ToString@), which neither @Monad[Integer]@ nor a
    -- bare @ConsList@ implies by itself. Last in the chain, so the cheap syntactic routes win first.
    jointEmptyCheck = case p2 of
      PredsOne{} -> predsEmpty typeEnv vaenv (predsAnd p1 (predsNot p2))
      _          -> False
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

-- | An arg- and var-free partial (a bare name).
plainPartial :: PartialType -> Bool
plainPartial p = H.null (ptArgs p) && H.null (ptVars p)

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
predImpliesAtom typeEnv vaenv (PredClass c1) (PredClass c2) =
  satAtomImplies typeEnv vaenv True (SatClass c1) (SatClass c2)
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
  -- parameterized class universe re-expands the element class combinatorially). Gated open-world
  -- ('gatePos'), like every other kind of implication here: @isa r1 ⟹ isa r2@ must NOT be proved by
  -- enumerating an UNSEALED @r1@'s CURRENT members (@~List ⟹ ~Bag@ because List's only member so far
  -- is a Bag member is broken by any future @isa List@ declaration). Deciding it True closed-world
  -- while the gated 'predsEmpty' oracle refuses the same enumeration is an inconsistency that breaks
  -- distributivity — @(~List ∩ ¬Bag)@ collapses to ∅ while the compound universe does not. A declared
  -- @isa@ edge still implies (the 'classDeclaredSubclassOf' fast path above), and a SEALED @r1@ is
  -- still enumerated (the gate only refuses an unsealed, non-'tePrgmEnv' class).
  | otherwise = satAtomImplies typeEnv vaenv True (SatRel r1) (SatRel r2)
  where checkArg k = isSubtypeOfWithEnv typeEnv vaenv
                       (H.lookupDefault PTopType k (ptArgs r1))
                       (ptArgs r2 H.! k)
-- PredExpr: use structural partial comparison (equality-based simplifying assumption)
predImpliesAtom typeEnv vaenv (PredExpr e1) (PredExpr e2) = isSubPartialOfWithEnv typeEnv vaenv e1 e2
-- PredClass c implies PredRel r when r's name is a suffix of c's name (then
-- expandRelPartial r includes classPartial c, so U_PredClass c ⊆ U_PredRel r), or - like
-- the PredClass\/PredClass case - when c is sealed and each of its members is in U_PredRel r
-- (e.g. c is a subclass of the class that r names).
predImpliesAtom typeEnv vaenv (PredClass c) (PredRel r)
  | relativeNameMatches (ptName r) (ptName c) = True
  -- A declared subclass edge @every(c isa r)@ proves @hasClass c ⟹ isa r@: membership in class @c@
  -- gives @isa c@ (the same-name route above), and the edge gives @isa c ⟹ isa r@. Sound in the open
  -- world (a declared edge holds for every future member), so an UNSEALED empty class - an isa-only
  -- @Lizard@ - still implies its declared superclass relation @~Animal@ WITHOUT enumeration, which the
  -- gated 'satAtomImplies' below (rightly) refuses for an unenumerable open class. Mirrors the
  -- PredRel/PredRel and PredRel/PredClass 'classDeclaredSubclassOf' fast paths; only @r@ need be plain
  -- (a narrower element bound on @c@ only shrinks the left).
  | plainPartial r && classDeclaredSubclassOf typeEnv (ptName c) (ptName r) = True
  | otherwise = satAtomImplies typeEnv vaenv True (SatClass c) (SatRel r)
-- PredRel r implies PredClass c when every type matching r is a member of c.
-- Fast path (mirrors the @PredClass c ⟹ PredRel r@ same-name case above): for the SAME plain class
-- name, @isa c ⟹ hasClass c@ holds directly — a value whose type is a member of class @c@ has class
-- @c@. This avoids expanding BOTH sides via 'expandRelPartial'/'expandClassPartial', which for a
-- recursive parameterized class (e.g. @Eq@ with @ConsList[Eq]@/@Maybe[Eq]@ members) re-expands the
-- element class unboundedly. Restricted to arg/var-free partials naming a real class.
predImpliesAtom typeEnv vaenv (PredRel r) (PredClass c)
  | plainPartial r, plainPartial c
  , sameClass || classDeclaredSubclassOf typeEnv (ptName r) (ptName c) = True
  -- Gated ('gatePos') like the other outermost implication decisions: a relation naming an
  -- UNSEALED class must not be enumerated closed-world here — @~List ⟹ ∀Bag@ via the current
  -- members is broken by any future @isa List@ declaration, and deciding it True while the
  -- gated 'predsEmpty' oracle refuses the same enumeration made the union drop the relation
  -- in one associativity order but not the other.
  | otherwise = satAtomImplies typeEnv vaenv True (SatRel r) (SatClass c)
  where
    -- Same named entity (either absoluteness direction): @isa c ⟹ hasClass c@.
    sameClass = relativeNameMatches (ptName r) (ptName c) || relativeNameMatches (ptName c) (ptName r)
-- Cross-kind: conservative False
predImpliesAtom _ _ _ _ = False

-- | Returns 'True' if every type satisfying @preds@ is a member of @partials@, i.e. U_preds ⊆ partials.
-- Expands @preds@ restricted to the names present in @partials@; if the expansion is exact
-- (no elements outside those names) and the result is a subset of @partials@, returns 'True'.
-- When the restricted expansion is inexact ('hasOutside = True'), conservative 'False': the
-- name-materialized expansion does not exhaust U (constants and open-world values carry no
-- partial name), so an empty full-name expansion must NOT be read as @U_preds = ∅@ — e.g.
-- @¬(PredRel n)@ subtracts every known name yet still contains every constant. A genuinely
-- contradictory conjunction is decided by 'isPredsContradictory'/'predsEmpty' at the call site.
predImpliesPartials :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypePredicates -> PartialLeafs -> Bool
predImpliesPartials typeEnv vaenv preds partials
  | not (predsPositiveAtoms preds) = False
  | otherwise =
      case expandTypesWithNamesFull typeEnv vaenv (TopType H.empty preds) (H.keysSet partials) of
        (expanded, False) -> isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing expanded []) (UnionType Nothing partials [])
        (_,        True)  -> False

-- | Predicates made only of positive atoms (no negation, at least one atom): exactly the forms
-- whose value set is confined to the atoms' name-keyed members, so a name-materialized
-- expansion can exhaust them. 'PredsNone' and negation-containing forms are open-ended - they
-- contain constants of unlisted kinds and (open-world) values of names not yet declared - so
-- no finite leaf expansion is ever exhaustive of them.
predsPositiveAtoms :: TypePredicates -> Bool
predsPositiveAtoms preds = case tryPredsToList preds of
  Just (_:_) -> True
  _          -> False

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
-- Constants carry no partial-name information; drop them before filtering (both the outer
-- positive constants and a Just-top's excluded constants — this computes leaf expansions only,
-- and a point exclusion never removes a leaf)
expandTypesWithNamesFull typeEnv vaenv (UnionType mTop leafs (_:_)) allowedNames =
  expandTypesWithNamesFull typeEnv vaenv (UnionType mTop leafs []) allowedNames
expandTypesWithNamesFull typeEnv vaenv (UnionType (Just (preds, negLeafs, _:_)) leafs []) allowedNames =
  expandTypesWithNamesFull typeEnv vaenv (UnionType (Just (preds, negLeafs, [])) leafs []) allowedNames
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
        -- A class member that is itself a (parameterized) class - e.g. @List@'s sole member
        -- @ConsList[/T=$_T]@ - must receive this class's instantiated arguments before being
        -- re-expanded, or the nested member's element parameter stays the raw member variable
        -- (@$_T@) instead of the class argument (e.g. @Integer@). The sibling 'TypeVar' case below
        -- already looks the argument up; this mirrors it for the nested-class form.
        mapClassTypeWithNamesFull (TopType negPartials ps) =
          expandTypesWithNamesFull typeEnv vaenv (substituteVarsWithVarEnv classVars (TopType negPartials ps)) allowedNames
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
        -- Substitute this class's instantiated arguments through the member ('substitutePartialKeep'
        -- resolves each external 'TVExt' reference in 'ptVars' and 'ptArgs' while keeping every
        -- 'TVInt' field tied to the member's own now-substituted 'ptVars'), so a field like
        -- @tail -> ConsList[/T=$_T]@ receives the class argument rather than a free variable that
        -- later widens to Top. In the closed-world program-coverage view ('tePrgmEnv') the member is
        -- a match /target/ whose field types are supplied by the value being matched, so its
        -- explicitly-written fields are then relaxed to 'PTopType' (keys only) — preserving them
        -- would spuriously hold a destructured definition's binding field (which merely binds and
        -- ignores the value) outside its declaration. The true field types always remain in the data
        -- graph.
        mapClassPartial tp@PartialType{ptArgs}
          | tePrgmEnv typeEnv = base{ptArgs=fmap (const PTopType) ptArgs}
          | otherwise         = base
          where base = substitutePartialKeep (joinVarArgEnv classVars H.empty) tp
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
          mapClassType (TypeVar (TVApplied t _) _) = error $ printf "Applied var %s found in expandPartial" (show t)
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
    result = unionAllTypesWithEnv typeEnv vaenv (UnionType Nothing (joinUnionType fromArgs) [] : tpNames ++ clsNames)
    relName = ptName relPartial
    fromArgs = map (\n -> relPartial{ptName=n}) $ relativeNameFilter relName $ map pkName $ H.keys $ snd $ splitVarArgEnv vaenv
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

-- | Resolve same-class (and same-relation) POSITIVE atoms in a goal against each other: a
-- goal's positive atoms all constrain the ONE witness value, so two atoms over the same
-- class meet into a single atom, with the meet's own discovered bindings resolved back into
-- it (the standalone reading, as 'mergePredsAnd'; @Maybe[$_A] ∧ Maybe[Int] → Maybe[Int]@).
-- Such a pair cannot be unified at the type level (the predicate merge only sees DISTINCT
-- classes there, e.g. @Functor[$_A] ∧ Maybe[Int]@) — it arises here, when class-member
-- expansion ('classSatCandidates') rewrites the outer class into a member atom. Without the
-- meet, candidates generated from the flexible atom keep an unbound slot and manufacture
-- escape witnesses no value satisfying BOTH atoms has (@Some[$_A]@ "escaping" @∀Maybe[Int]@
-- — a false SAT that surfaced as a 'verifySchemeActLowers' monotonicity failure once the
-- flexible-slot cover was made sound). The discoveries are NOT propagated to the goal's
-- other atoms: an unbound flexible slot constrains nothing beyond class membership, so the
-- collapsed pair is exact, while pinning the variable elsewhere would strengthen those
-- atoms towards a false UNSAT — the polarity ('satGoal' is conservative-to-SAT; UNSAT
-- claims prove subtypes) the engine must never err towards.
satGoalUnifyPos :: TypeVarArgEnv -> SatGoal -> SatGoal
satGoalUnifyPos vaenv goal@(SatGoal pos neg)
  | length (L.nub keys) == length keys = goal
  | otherwise = satGoalNorm (SatGoal (foldl step [] pos) neg)
  where
    keys = [(k, ptName p) | Just (k, p) <- map atomKP pos]
    atomKP (SatClass p) = Just (0 :: Int, p)
    atomKP (SatRel p)   = Just (1, p)
    atomKP SatLeafs{}   = Nothing
    mkAtom 0 p = SatClass p
    mkAtom _ p = SatRel p
    step kept atom = case atomKP atom of
      Nothing -> kept ++ [atom]
      Just (k, p) -> case break (sameKN k p) kept of
        (pre, ex:post) | Just (_, ep) <- atomKP ex ->
          case unifyAtomVars ep p of
            Just m  -> pre ++ mkAtom k m : post
            Nothing -> kept ++ [atom]
        _ -> kept ++ [atom]
    sameKN k p a = case atomKP a of
      Just (k', p') -> k' == k && ptName p' == ptName p
      Nothing       -> False
    -- Slot-wise meet of two same-name atoms, resolvable only when each shared slot pair has
    -- a side that constrains nothing (⊤ or an unbound\/partial-local variable): take the
    -- other side. Anything else (two distinct concrete bounds, predicates, args) bails to
    -- keeping both atoms — the enumeration handles them as before. Deliberately NOT the full
    -- 'intersectPartials': the type-algebra meet re-enters engine decisions from inside
    -- 'satGoal', which is both a recursion hazard (no goal-stack guard protects this entry
    -- normalization) and a hot-path cost.
    unifyAtomVars a b
      | ptPreds a /= PredsNone || ptPreds b /= PredsNone = Nothing
      | not (H.null (ptArgs a) && H.null (ptArgs b)) = Nothing
      | otherwise = do
          vars <- sequenceA (H.unionWith meetSlot (fmap Just (ptVars a)) (fmap Just (ptVars b)))
          Just a{ptVars=vars}
    meetSlot mx my = do
      x <- mx
      y <- my
      meet2 x y
    meet2 x y
      | x == y = Just x
      | slotFree x = Just y
      | slotFree y = Just x
      | otherwise = Nothing
    slotFree PTopType = True
    slotFree (TypeVar _ TVInt) = True
    slotFree (TypeVar v TVExt) = case H.lookup v vaenv of
      Nothing             -> True
      Just (TypeVar v' _) -> v' == v
      _                   -> False
    slotFree _ = False

mergeSatGoals :: SatGoal -> SatGoal -> SatGoal
mergeSatGoals (SatGoal p1 n1) (SatGoal p2 n2) = SatGoal (p1 ++ p2) (n1 ++ n2)

-- | Bound on DNF size during conversion; a larger formula is out of fragment.
satDNFCap :: Int
satDNFCap = 64

-- | Cartesian conjunction of per-conjunct DNFs, 'Nothing' when it exceeds 'satDNFCap'.
-- Each step normalizes the combined disjunction — all three rewrites are exact, and without
-- them a tautology-heavy product (e.g. negating the DNF of @¬(A∧B) ∧ ¬(A∧C)@, whose raw
-- product is choicesᵍᵒᵃˡˢ) overflows the cap that a collapsed equivalent stays far under:
--   * dedup atoms within a goal ('satGoalNorm'; conjunction is idempotent),
--   * drop goals requiring and forbidding the same atom (unsatisfiable, removable from a
--     disjunction),
--   * drop subsumed goals (a goal whose literal set contains another goal's has only a
--     subset of its models, so the disjunction is unchanged without it).
satDNFConj :: [[SatGoal]] -> Maybe [SatGoal]
satDNFConj = foldl step (Just [SatGoal [] []])
  where
    step macc gs = do
      acc <- macc
      let combined = simplify [mergeSatGoals g1 g2 | g1 <- acc, g2 <- gs]
      if length combined > satDNFCap then Nothing else Just combined
    simplify = subsume . L.nub . filter (not . refuted) . map satGoalNorm
    refuted (SatGoal pos neg) = any (`elem` neg) pos
    subsume gs = [g | g <- gs, not (any (\g' -> g' /= g && g' `weakerThan` g) gs)]
    -- g' is weaker than g when its literals are a subset of g's (after 'satGoalNorm' dedup)
    weakerThan (SatGoal p1 n1) (SatGoal p2 n2) = p1 `subsetOf` p2 && n1 `subsetOf` n2
    subsetOf xs ys = all (`elem` ys) xs

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
typeSatDNF _ (UnionType (Just (_, _, _:_)) _ _) = Nothing -- excluded constants: out of fragment
typeSatDNF _ (UnionType Nothing leafs []) = case splitUnionType leafs of
  [] -> Just [] -- bottom: unsatisfiable
  ls -> Just [SatGoal [SatLeafs ls] []]
typeSatDNF vaenv (UnionType (Just (preds, negLeafs, [])) pos []) = do
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
        -- A member partial whose name is itself a CLASS (e.g. @ConsList[/T=Eq]@ as a member of @Eq@)
        -- is emitted as a class-predicate goal, not a bare leaf: 'partialCoversSat' requires an exact
        -- name match, so a leaf named @ConsList@ never covers a value shaped as one of its
        -- CONSTRUCTORS (@Nil@/@Cons@). Routing it through 'satGoal' expands the class to its
        -- constructors (with the member's element bound applied), so e.g. @Nil@ is recognised as an
        -- @Eq@ (an empty list vacuously satisfies the @[/T=Eq]@ element bound). A non-class member
        -- (a concrete data type) stays a completed leaf.
        candsOfMember (UnionType Nothing p []) = Just (map mkMemberCand (splitUnionType p))
        candsOfMember t = candsOfType t
        mkMemberCand tp@PartialType{ptName=tpNm}
          | H.member tpNm (scClasses (teSatCache typeEnv)) = CandGoal (SatGoal [SatClass (mapClassPartial tp)] [])
          | otherwise = CandLeaf (completeLeaf (mapClassPartial tp))
        -- Apply the class's instantiated arguments to a class-typed member (e.g. @List@'s
        -- @ConsList[/T=$_T]@ member) before enumerating it, mirroring 'mapClassPartial'/'substMemberVar'
        -- on the positive-partial members: otherwise the member's element parameter stays free and the
        -- enumeration widens it (@List[/T=Integer]@ would enumerate as @ConsList[/T=$_T]@).
        candsOfType t = map CandGoal <$> typeSatDNF vaenv (substituteVarsWithVarEnv informativeVars t)
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

-- | Interprets a concrete witness shape under its data declaration. Every actual value of a
-- data type satisfies the declaration's field types (a @Cons@ value's @tail@ IS a @ConsList@
-- by construction), so a ⊤-valued declared field carries no constraint of its own but the
-- value universe does. Substitute the declared field type (with the shape's own informative
-- param bindings applied) before the engine reasons about what the shape can escape; taking a
-- ⊤ field at face value manufactures escape witnesses no real value has (e.g.
-- @Cons(head=⊤, tail=⊤)@ "escaping" @∀ConsList@ through a non-list tail, wrongly refuting
-- @Cons(head=⊤, tail=⊤) ⊆ ∀ConsList@). Only /present/ ⊤-valued fields are tightened — the
-- value-set is unchanged, so this is a sound normalization for witness candidates (the
-- covering sup side must keep its authored ⊤ fields: narrowing a sup could flip a proven
-- cover to unproven). A bare argless partial is left alone: it may denote the unapplied
-- constructor (an arg-powerset partial application), whose value set has no fields at all.
satValueShape :: TypeGraph tg => TypeEnv tg -> PartialType -> PartialType
satValueShape typeEnv p@PartialType{ptName, ptVars, ptArgs}
  | H.null ptArgs = p
  | otherwise = case [dArgs | PartialType{ptArgs=dArgs} <- typeGraphData typeEnv ptName, not (H.null dArgs)] of
      (dArgs:_) -> p{ptArgs = H.mapWithKey (tightenField dArgs) ptArgs}
        where
          tightenField dArgs' k PTopType | Just ft <- H.lookup k dArgs' = interpretField ft
          tightenField _ _ t = t
          interpretField = substituteVarsWithVarEnv informativeVars
          informativeVars = H.filter (/= PTopType) ptVars
      _ -> p

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
-- every matching class (as a class constraint). The NAME universe is declaration-bounded
-- (closed-world, the settled relation-type semantics), but a class route inherits the
-- class's own open/closed-world standing: under 'gatePos', a relation matching an UNSEALED
-- class is unenumerable ('Nothing' → conservatively satisfiable), exactly as
-- 'classSatCandidates' gates the class itself — a future member of the open class matches
-- the relation, so a closed-world enumeration would prove implications
-- (e.g. @~List ⟹ ∀Bag@) that new declarations can break.
relSatCandidates :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Bool -> PartialType -> Maybe [SatCand]
relSatCandidates typeEnv vaenv gatePos relPartial
  | gatePos && not (tePrgmEnv typeEnv) && any unsealedClass matchingClassNames = Nothing
  | otherwise = Just (argCands ++ tpCands ++ clsCands)
  where
    relName = ptName relPartial
    matchingClassNames = relativeNameFilter relName (listClassNames typeEnv)
    unsealedClass n = case H.lookup n (scClasses (teSatCache typeEnv)) of
      Just ClassSatInfo{csiSealed} -> not csiSealed
      Nothing                      -> True
    argCands = [CandLeaf relPartial{ptName=n} | n <- relativeNameFilter relName (map pkName (H.keys (snd (splitVarArgEnv vaenv))))]
    tpCands = [CandLeaf relPartial{ptName=n} | n <- relativeNameFilter relName (scTypeNames (teSatCache typeEnv))]
    -- Carry the relation's own type arguments onto the class candidate (@isa ConsList[/T=∀ToString]@
    -- ⇒ @ConsList[/T=∀ToString]@, not a bare @ConsList@ whose Top element would spuriously escape a
    -- @DShow@ bound), mirroring the arg/type candidates above which already reuse @relPartial@.
    clsCands = [CandGoal (SatGoal [SatClass relPartial{ptName=n}] []) | n <- matchingClassNames]

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

-- | The declared arg-keysets bounding a name's realizable arg-sets, when known.
-- Object declarations come from 'typeGraphArgKeys'. A CLASS name has no object, but its
-- positive-partial form (the class placeholder) is exactly the bare argless reference —
-- a parameterized class carries type parameters in 'ptVars', never 'ptArgs' — so its only
-- realizable arg-set is ∅; likewise the synthetic 'classPlaceholderLeaf' marker name,
-- which is only ever built argless. An unknown name (neither declared nor a class) is
-- open world.
declaredArgKeys :: TypeGraph tg => TypeEnv tg -> TypeName -> Maybe [S.HashSet ArgName]
declaredArgKeys typeEnv name = case typeGraphArgKeys typeEnv name of
  Just kss -> Just kss
  Nothing | H.member name (scClasses (teSatCache typeEnv)) -> Just [S.empty]
          | nameStr name == classPlaceholderStr -> Just [S.empty]
          | otherwise -> Nothing

-- | Drops vacuous arg-mode interval bounds (see 'PtArgMode'), canonicalizing the mode
-- without changing the partial's value set:
--
--   * an unset lower bound is vacuous with no listed args (the interval floor is ∅ either way);
--   * an unset upper bound is vacuous when no declared arg-set strictly contains the listed
--     args ('declaredArgKeys'): every realizable arg-set is a subset of some declared one, so
--     no realizable arg-set lies strictly above the listed args and the interval collapses to
--     its floor. This is what makes e.g. @/length(this=..)..@ ('relaxNameBoundArgs' widening)
--     one set with @/length(this=..)@ — @/length@ declares no larger arg-set to relax into.
--
-- An unknown name (open world) keeps its bounds. Exact partials return instantly.
normalizePtArgMode :: TypeGraph tg => TypeEnv tg -> PartialType -> PartialType
normalizePtArgMode typeEnv p@PartialType{ptName, ptArgs, ptArgMode=PtArgMode lb ub}
  | lb' && ub' = if ptArgMode p == PtArgExact then p else p{ptArgMode=PtArgExact}
  | otherwise = p{ptArgMode=PtArgMode lb' ub'}
  where
    lb' = lb || H.null ptArgs
    ub' = ub || case declaredArgKeys typeEnv ptName of
      Just kss -> not $ any (H.keysSet ptArgs `strictSubsetOf`) kss
      Nothing  -> False
    strictSubsetOf a b = S.size a < S.size b && a `S.isSubsetOf` b

-- | The finite list of arg-keysets a partial's arg-mode interval admits, when enumerable:
-- an upper-bounded interval enumerates the subsets of its listed args directly; an
-- upper-unbounded one is bounded by the name's declared arg-sets ('declaredArgKeys' —
-- every realizable arg-set is a subset of a declared one). 'Nothing' only for an
-- upper-unbounded interval on an unknown name (open world).
ptIntervalArgKeys :: TypeGraph tg => TypeEnv tg -> PartialType -> Maybe [S.HashSet ArgName]
ptIntervalArgKeys typeEnv PartialType{ptName, ptArgs, ptArgMode=PtArgMode lb ub}
  | ub = Just $ if lb then [aKeys] else subsetsBetween S.empty aKeys
  | otherwise = do
      kss <- declaredArgKeys typeEnv ptName
      let lo = if lb then aKeys else S.empty
      return $ L.nub $ concat [subsetsBetween lo ks | ks <- kss, lo `S.isSubsetOf` ks]
  where
    aKeys = H.keysSet ptArgs
    subsetsBetween lo hi = [lo `S.union` S.fromList extra | extra <- powerset $ S.toList $ hi `S.difference` lo]

-- | Restricts a moded partial to one arg-set of its interval: the exact-mode partial with
-- precisely those args, keeping the listed values and leaving unlisted args unconstrained (⊤).
ptAtArgKeys :: PartialType -> S.HashSet ArgName -> PartialType
ptAtArgKeys p@PartialType{ptArgs} ks = p{ptArgs=args', ptArgMode=PtArgExact}
  where args' = H.mapWithKey (\k _ -> H.lookupDefault PTopType k ptArgs) (S.toMap ks)

-- | Reads a partial's FREE var references as ⊤: a bare partial-local ('TVInt') reference
-- in one of the partial's values names the partial's own var\/arg scope (or the ambient
-- env); one bound in neither is free — it constrains nothing, so its value set is ⊤
-- (the reading 'isSubtypeOfWithEnv' gives an UNBOUND var). Only a direct top-level
-- reference is rewritten: a nested one names the nested partial's own scope.
freeTVIntToTop :: TypeVarArgEnv -> PartialType -> Type -> Type
freeTVIntToTop vaenv p t@(TypeVar v TVInt) | not (H.member v (ptVarArg p)), not (H.member v vaenv) = PTopType
                                           | otherwise = t
freeTVIntToTop _ _ t = t

-- | All partial-local ('TVInt') references appearing in a type at any depth. Over-collects
-- through nested scopes (a name a nested partial itself binds is still counted), the safe
-- direction for its consumer ('subtractElems'\'s identical-form skip only becomes rarer).
typeTVIntRefs :: Type -> S.HashSet TypeVarAux
typeTVIntRefs (TypeVar v TVInt) = S.singleton v
typeTVIntRefs (UnionType mtop pos _) = S.unions $ topRefs ++ map leafRefs (splitUnionType pos)
  where
    topRefs = case mtop of
      Just (preds, negLeafs, _) -> predsRefs preds : map leafRefs (splitUnionType negLeafs)
      Nothing                -> S.empty : []
    leafRefs PartialType{ptVars, ptArgs, ptPreds} =
      S.unions $ predsRefs ptPreds : map typeTVIntRefs (H.elems ptVars ++ H.elems ptArgs)
    predsRefs (PredsOne p)  = leafRefs (predPartial p)
    predsRefs (PredsAnd ps) = S.unions $ map predsRefs ps
    predsRefs (PredsNot p)  = predsRefs p
    predPartial (PredExpr p)  = p
    predPartial (PredClass p) = p
    predPartial (PredRel p)   = p
typeTVIntRefs _ = S.empty

-- | The value SET a partial's value denotes standalone: read via 'substituteTVExt' under
-- the partial's OWN var\/arg scope over the ambient env — the codebase's canonical reading
-- (the same one 'mergeVarEnvVal' applies to every stored env binding), so forms produced
-- here are structurally comparable with the intersection's. A top-level reference reads
-- through the partial's slot (@head=$/T@ with @/T=X@ denotes @X@), a nested partial's
-- references read through ITS own slots, and an unbound reference reads as ⊤.
resolveOwnScopeRefs :: TypeVarArgEnv -> PartialType -> Type -> Type
resolveOwnScopeRefs vaenv p = substituteTVExt (ptVarArg p `H.union` vaenv)

-- | 'freeTVIntToTop' applied DEEP: every partial-local reference the given partial does not bind
-- reads as ⊤ at ANY depth, including class-predicate parameters (@∀ConsList[/T=$/T]@ under a
-- partial with no @/T@ slot reads @∀ConsList[/T=⊤]@ — the union over all instantiations). Used to
-- instantiate a DECLARED field type under a specific partial: the template's free parameters must
-- NOT stay symbolic, lest they resolve through a like-named SUPER binding in a shared env during
-- a subtype check (the same leak 'freeTVIntToTop' guards against, at depth). A reference a NESTED
-- partial itself links is also widened — a sound over-approximation of the field's value set,
-- which for a clamp read only makes the check more conservative.
freeTVIntToTopDeep :: TypeVarArgEnv -> PartialType -> Type -> Type
freeTVIntToTopDeep vaenv p = go
  where
    go t@(TypeVar v TVInt) | not (H.member v (ptVarArg p)), not (H.member v vaenv) = PTopType
                           | otherwise = t
    go (UnionType mtop pos consts) = UnionType (fmap goTop mtop) (goLeafs pos) consts
    go t = t
    goTop (preds, negLeafs, ncs) = (mapTypePreds (mapTypePred goPartial) preds, goLeafs negLeafs, ncs)
    goLeafs = joinUnionType . map goPartial . splitUnionType
    goPartial pt = pt{ ptVars  = fmap go (ptVars pt)
                     , ptArgs  = fmap go (ptArgs pt)
                     , ptPreds = mapTypePreds (mapTypePred goPartial) (ptPreds pt) }

-- | Whether a partial is uninhabited under DECLARATION-BOUNDING: a field value disjoint from the
-- field's declared type admits no constructible value (@Cons(tail=#noCore)@ denotes ∅ — every
-- @Cons@ tail is declared @∀ConsList@), making the partial set-equal to the empty set. Only a
-- concrete non-⊤ value differing from its declared bound pays the disjointness meet; ⊤ and
-- symbolic (variable) values, a ⊤/variable declared bound, and a value equal to its bound are
-- trivially inside, so member-form and field-completed partials decide without any meet.
partialUninhabitedByDecl :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> Bool
partialUninhabitedByDecl typeEnv vaenv p@PartialType{ptName, ptArgs}
  | H.null ptArgs = False
  | null decls = False
  | otherwise = any argEmptyInAllDecls (H.toList ptArgs)
  where
    decls = [dArgs | PartialType{ptArgs=dArgs} <- typeGraphData typeEnv ptName, not (H.null dArgs)]
    argEmptyInAllDecls (k, v) =
      let bounds = mapMaybe (H.lookup k) decls
      in not (null bounds) && all (clampEmpty v) bounds
    clampEmpty PTopType _  = False
    clampEmpty TypeVar{} _ = False
    clampEmpty _ PTopType  = False
    clampEmpty _ TypeVar{} = False
    clampEmpty v d | v == d = False
                   | otherwise = isBottomType (intersectTypesEnv typeEnv vaenv v (freeTVIntToTopDeep vaenv p d))

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
partialCoversSat typeEnv vaenv gatePos hyps sup0 sub0 =
  ptName sup == ptName sub
  -- Mode-normalized ('normalizePtArgMode') for parity with 'isSubPartialOfWithEnv': a
  -- vacuous interval bound must not defeat a cover (e.g. @/x..@ with no larger declared
  -- arg-set is one set with @/x@).
  && argModeCovers (normalizePtArgMode typeEnv sup) (normalizePtArgMode typeEnv sub)
  && all varCovered (H.elems (H.intersectionWith (,) (ptVars sub) (ptVars sup)))
  && all elemCovered (H.elems (H.intersectionWith (,) (ptArgs sub) (ptArgs sup)))
  && supPredsCover
  where
    sup = sup0
    sub = sub0
    -- A sub-side slot holding an internal POINTER (@/head=$T@, an arg or var naming one of
    -- the partial's own parameters, 'TVInt') denotes that parameter's binding: read it
    -- through before deciding (single step, as 'compactOwnVarRefs' materializes). SUB side
    -- only — the sub is the witness VALUE, whose pointers denote its actual content (an
    -- exact reading; engine-made member shapes never pass through 'compactType', and a
    -- var-free but predicate-bounded binding is never materialized at all, leaving
    -- field-resolved and param-resolved forms of the SAME value otherwise incomparable —
    -- the 'verifySchemeActLowers' trap 'compactOwnVarRefs' documents). The sup is the cover
    -- PATTERN: its pointer slots read as the flexible bounds its author wrote, and
    -- resolving them strengthens escape decisions that the predicate absorption of REQ
    -- bounds depends on (core @list.ct@ stops compiling).
    subSlot t@(TypeVar (TVVar n) TVInt) = case H.lookup n (ptVars sub) of
      Just b | b /= PTopType, b /= t -> b
      _                              -> t
    subSlot t = t
    -- Under projection (matching/dispatch consumers, 'teArgModeProjection'), a Top-valued type
    -- parameter on the value side carries no information — it is equivalent to the parameter
    -- being absent, which the shared-keys discipline already skips. A definition whose element
    -- parameter could not be narrowed (e.g. a mapped element fed by a higher-order argument)
    -- completes to an explicit @[T=Top]@, and must dispatch exactly like the key-free form.
    -- The precise set algebra (projection off) keeps Top ⊄ concrete.
    varCovered (PTopType, _) | teArgModeProjection typeEnv = True
    varCovered p             = elemCovered p
    -- This is the ∀-cover primitive (every sub value within sup — the escape/coverage
    -- direction), so under the precise algebra a flexible slot is covered only by a cover
    -- slot that itself admits every binding (⊤ or another flexible variable): its values
    -- range over every binding (@Maybe[$T]@ contains @Some(Some(5))@), and treating it as
    -- covered by a CONSTRAINED slot denies escapes the engine's conservative-to-SAT polarity
    -- requires (it made @Maybe[⊤] ⟹ Maybe[Integer]@ hold). Binding-on-demand belongs to the
    -- ∃-overlap direction ('partialsOverlapSat', the admit checks). Under projection
    -- (matching/dispatch consumers) a flexible slot on the value side behaves like the
    -- Top-valued parameter above: it binds per value and carries no dispatch information.
    elemCovered (tSub0, tSup0)
      | isFlexibleVar tSub = tSup == PTopType || isFlexibleVar tSup || teArgModeProjection typeEnv
      | otherwise = not (typesSatDiff typeEnv vaenv gatePos hyps tSub tSup)
      where tSub = resolveSelfRefs sub (subSlot tSub0)
            tSup = resolveSelfRefs sup tSup0
    -- Resolve a slot's SELF-referential recursion variable through its own partial's @ptVars@. A
    -- recursive field like @tail -> ConsList[/T=$_T]@ names the partial's own parameter @/T@ but is
    -- typed EXTERNAL ('TVExt', @$_T@) rather than the internal pointer ('TVInt', @$T@) that
    -- @head -> $T@ uses, so it does not resolve through @ptVars@ automatically. When one side's
    -- element was concretized (e.g. an enumerated class member with @tail=ConsList[/T=Integer]@) and
    -- the other kept the free form, the comparison spuriously fails; resolving both through their own
    -- @ptVars@ (@substituteKeep@ leaves genuinely-free vars untouched) restores the symmetry.
    resolveSelfRefs p t = substituteKeep (joinVarArgEnv (ptVars p) H.empty) t
    isFlexibleVar (TypeVar _ TVInt) = True -- partial-local var: bindable per value
    isFlexibleVar (TypeVar v TVExt) = case H.lookup v vaenv of
      Nothing             -> True
      Just (TypeVar v' _) -> v' == v
      Just PTopType       -> True -- bound to Top: unconstrained, so still bindable per value
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
-- A whole-shape cover ('satEscapesAtomic' on the unsplit shape) is checked first: cover of
-- the whole denies escape for every piece, and stays precise where the piece split would
-- exceed 'satDNFCap' (e.g. an applied operator whose arg slots each carry a member union) —
-- an oversized split otherwise reads as a conservative escape, which the coverage decisions
-- ('typeCoveredSymbolic') would surface as a definitive not-covered.
satEscapes :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Bool -> SatHyps -> PartialType -> SatAtom -> Bool
satEscapes typeEnv vaenv gatePos hyps m0 atom
  | not (satEscapesAtomic typeEnv vaenv gatePos hyps m0 atom) = False
  | otherwise = case splitPartialByUnionVarArgs m0 of
      [_] -> True -- no union-valued components: the whole-shape answer is final
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

-- | Declaration-level implication between two atoms: the non-recursive fast paths of
-- 'predImpliesAtom' (same-name and declared @isa@ edges), which are sound in the OPEN world -
-- a declared edge holds for every future member, so @a1 ∧ ¬a2@ is unsatisfiable whatever the
-- enumeration gate says. Used to refute a 'satGoal' whose positive atom implies a negative one
-- (e.g. @Fish ∧ ¬Animal@ with @every(Fish isa Animal)@) without enumerating the class - the
-- gated enumeration otherwise reads such a goal as conservatively satisfiable. Never consults
-- the engine, so it is recursion-safe inside 'satGoal'.
satAtomImpliesDeclared :: TypeGraph tg => TypeEnv tg -> SatAtom -> SatAtom -> Bool
satAtomImpliesDeclared typeEnv a1 a2 = case (a1, a2) of
  -- hasClass C1[..] ⟹ hasClass C2 for the same name or a declared subclass edge; the implied
  -- class must be PLAIN (an element bound would need a covariance check).
  (SatClass c1, SatClass c2) -> plainPartial c2
    && (ptName c1 == ptName c2 || classDeclaredSubclassOf typeEnv (ptName c1) (ptName c2))
  -- hasClass C ⟹ isa R when R's name path is a suffix of C's, or C is a declared subclass of R
  -- (@hasClass C ⟹ isa C ⟹ isa R@) - the same declared-edge route 'predImpliesAtom' PredClass/PredRel
  -- carries. Sound in the open world independent of the enumeration gate.
  (SatClass c, SatRel r) -> plainPartial r
    && (relativeNameMatches (ptName r) (ptName c) || classDeclaredSubclassOf typeEnv (ptName c) (ptName r))
  -- isa R ⟹ hasClass C for the same plain name (either absoluteness direction) or a declared
  -- edge ('predImpliesAtom' PredRel/PredClass fast path).
  (SatRel r, SatClass c) -> plainPartial r && plainPartial c
    && (relativeNameMatches (ptName r) (ptName c) || relativeNameMatches (ptName c) (ptName r)
        || classDeclaredSubclassOf typeEnv (ptName r) (ptName c))
  -- isa R1 ⟹ isa R2 when R2's name path is a suffix of R1's, or by a declared edge.
  (SatRel r1, SatRel r2) -> plainPartial r2
    && (relativeNameMatches (ptName r2) (ptName r1)
        || (plainPartial r1 && classDeclaredSubclassOf typeEnv (ptName r1) (ptName r2)))
  _ -> False

-- | Is some (finite) value able to satisfy the goal? Conservative towards 'True'.
-- 'gatePos': when 'True', THIS goal's positive class generator may only be enumerated when
-- the class is sealed or 'tePrgmEnv' is set (the open-world gate); an unenumerable positive
-- class makes the goal conservatively satisfiable. The gate applies only to the outermost
-- enumeration - every nested derivation is closed-world over current members, mirroring the
-- materialized algebra (whose gate sits at the outermost implication/re-expression decision
-- while all downstream expansion is ungated).
satGoal :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Bool -> SatHyps -> SatGoal -> Bool
satGoal typeEnv vaenv gatePos hyps goal0
  | goal `S.member` hyps = False -- recursive goal: no finite witness along this path
  -- @X ∧ ¬X@: an atom required positively and negatively at once is contradictory, whatever
  -- the atom means. The enumeration below cannot be trusted to detect this (a candidate on
  -- which BOTH the admit and the escape check are conservative reads as a witness - e.g.
  -- @Ord ∧ Eq ∧ ¬Eq@ found "satisfiable" through a parameterized class member), so decide it
  -- syntactically first.
  | any (`elem` sgNeg goal) (sgPos goal) = False
  -- A positive atom implying a negative one by declaration (@Fish ∧ ¬Animal@ under
  -- @every(Fish isa Animal)@) is contradictory in the open world, independent of the gate.
  | or [satAtomImpliesDeclared typeEnv p n | p <- sgPos goal, n <- sgNeg goal] = False
  | otherwise = case sgPos goal of
      [] -> True -- no positive constraint: the open universe escapes any finite negatives
      _  -> case chosenGen of
        Nothing         -> True -- no enumerable generator: conservative
        Just (_, cands) -> any checkCand cands
  where
    goal = satGoalUnifyPos vaenv (satGoalNorm goal0)
    hyps' = S.insert goal hyps

    -- Prefer the most concrete generator: concrete leafs, then a class, then a relation. Among
    -- those, take the first that can actually ENUMERATE: a gated-open positive class yields no
    -- candidates, and skipping it for a co-present enumerable atom (e.g. the SEALED @ConsList@
    -- deciding @Monad[Integer] ∧ ConsList ∧ ¬DShow@ that the OPEN @Monad@ generator only reports
    -- conservatively satisfiable) is sound - the skipped atom is still enforced as a POSITIVE
    -- membership on each enumerated witness ('satAdmits' in 'checkCandLeaf').
    orderedAtoms = let (leafs, nonLeafs) = L.partition isLeafs (sgPos goal)
                       (classes, rels)   = L.partition isClass nonLeafs
                   in leafs ++ classes ++ rels
    chosenGen = listToMaybe [(a, cands) | a <- orderedAtoms, Just cands <- [genCandidates a]]
    restPos = case chosenGen of
      Just (g, _) -> L.delete g (sgPos goal)
      Nothing     -> sgPos goal
    isLeafs SatLeafs{} = True
    isLeafs _          = False
    isClass SatClass{} = True
    isClass _          = False

    -- Witness candidates are interpreted under their data declarations ('satValueShape'): the
    -- escape checks below must reason over real values, not the leaf's face-value ⊤ fields.
    -- An arg-set INTERVAL leaf (e.g. @/Cons..@) is the union of its exact arg-set pieces
    -- ('ptIntervalArgKeys', declaration-bounded), and each piece may be covered by a DIFFERENT
    -- negative (@/Cons(head,tail)@ by a class, the partial-application pieces by leafs) - a
    -- joint cover the atomic leaf hides. Decompose into the pieces, mirroring the
    -- 'coveredByArgSets' decomposition of the positive-partial subtype.
    genCandidates (SatLeafs ls) = Just (map (CandLeaf . satValueShape typeEnv) (concatMap intervalPieces ls))
      where
        intervalPieces l = case ptIntervalArgKeys typeEnv lN of
          Just kss@(_:_:_) -> map (ptAtArgKeys lN) kss
          _                -> [lN]
          where lN = normalizePtArgMode typeEnv l
    genCandidates (SatClass c)  = classSatCandidates typeEnv vaenv gatePos c
    -- A relation's type-route candidates come from the name universe ('scTypeNames'); an env
    -- without one (e.g. the reduced env of the class-graph 'Semigroup' merge) makes the
    -- enumeration silently incomplete - a goal with no candidates would read as refuted when
    -- the engine merely knows nothing. Treat it as unenumerable (conservatively satisfiable).
    genCandidates (SatRel r)
      | null (scTypeNames (teSatCache typeEnv)) = Nothing
      | otherwise = relSatCandidates typeEnv vaenv gatePos r

    rest = SatGoal restPos (sgNeg goal)

    checkCand (CandGoal g) = satGoal typeEnv vaenv False hyps' (mergeSatGoals g rest)
    checkCand (CandLeaf m)
      | ptPreds m /= PredsNone = case predsSatDNF True (ptPreds m) of
          Nothing  -> True -- member predicates outside the fragment: conservative
          Just dnf -> any (\g -> checkCandLeaf (mergeSatGoals g rest) m{ptPreds=PredsNone}) dnf
      | otherwise = checkCandLeaf rest m
    checkCandLeaf r m = all (satAdmits typeEnv vaenv False hyps' mR) (sgPos r)
                     && all (satEscapes typeEnv vaenv False hyps' mR) (sgNeg r)
      where
        -- Refine the witness by each co-occurring positive atom it must ALSO satisfy: meeting a
        -- @Cons@ witness with @Monad[Integer]@'s same-named member @Cons[Integer]@ pins its otherwise
        -- free element before the escape checks. Without this a @Cons[$_T]@ generated from @ConsList@
        -- spuriously escapes @DShow@ though every value satisfying @Monad[Integer] ∧ ConsList@ has
        -- element Integer (a @ToString@). Only refine when the meet is a SINGLE leaf of the witness's
        -- own name — an element-preserving pinning, never an open-world emptiness conclusion (an
        -- unrelated class\/leaf yields ∅ or a multi-leaf union, which is left as the witness for
        -- 'satAdmits' to reject). Co-occurring positive LEAF atoms need the same meet: the generator
        -- enumerates a witness from ONE leaf atom while 'satAdmits' only checks OVERLAP against the
        -- others, so a witness broader than the true joint value (e.g. a @fluffy(this=Animal)@ chosen
        -- for @fluffy(this=Fish) ∧ fluffy(this=Animal)@) would escape a negative the refined
        -- (@this=Fish@) value cannot — a false SAT that breaks @A ⊆ A∪B@ (covariance).
        mR = foldl refineByPos m (sgPos r)
        refineByPos acc (SatClass c) = refineMeet acc (expandClassPartial typeEnv vaenv c)
        refineByPos acc (SatLeafs ls) = refineMeet acc (UnionType Nothing (joinUnionType ls) [])
        refineByPos acc _ = acc
        refineMeet acc other =
          case snd $ intersectTypesWithVarEnv typeEnv vaenv (singletonType acc) other of
            UnionType Nothing leafs [] | [acc'] <- splitUnionType leafs, ptName acc' == ptName acc -> acc'
            _ -> acc

-- | Engine-backed emptiness of a predicate conjunction ('predsEmpty' fast path):
-- @Just isEmpty@ when the predicates are within the decidable fragment.
--
-- The satisfiability check runs under the open-world gate ('gatePos' = 'True'), so a
-- conjunction of distinct /open/ classes (e.g. @Functor[Eq] ∧ Eq@) is treated as satisfiable
-- rather than empty: a future file may declare a type inhabiting both, so the conjunction is
-- not provably disjoint. The gate itself folds in 'tePrgmEnv' ('classSatCandidates'), so in the
-- closed-world coverage environment ('tePrgmEnv' set) enumeration is still exhaustive and a
-- genuinely uninhabited conjunction is reported empty. Only classes provably disjoint by the
-- class graph remain empty in the open world.
predsEmptySymbolic :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> TypePredicates -> Maybe Bool
predsEmptySymbolic typeEnv vaenv preds = do
  goals <- predsSatDNF True preds
  Just (not (any (satGoal typeEnv vaenv True S.empty) goals))

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

-- | Engine-backed implication between two class/relation atoms: @a1 ⟹ a2@ decided as emptiness
-- of @a1 ∧ ¬a2@. 'gatePos' gates enumeration of a1's class (the open-world gate).
-- A memberless (but enumerable) class on the left implies anything vacuously - parity with
-- the enumerate-members check, which is vacuously true on an empty member list (the class
-- placeholder inhabitant is a satisfiability device, not a counterexample to implication).
satAtomImplies :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Bool -> SatAtom -> SatAtom -> Bool
satAtomImplies typeEnv vaenv gatePos a1 a2
  | SatClass c1 <- a1
  , Just ClassSatInfo{csiSealed=sealed, csiTypes=[]} <- H.lookup (ptName c1) (scClasses (teSatCache typeEnv))
  , sealed || tePrgmEnv typeEnv = True
  | otherwise = not (satGoal typeEnv vaenv gatePos S.empty (SatGoal [a1] [a2]))

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
    elemInFrag (UnionType (Just (_, _, _:_)) _ _) = False -- excluded constants: out of fragment
    elemInFrag (UnionType mtop leafs []) =
      maybe True (\(preds, _, _) -> predsInFrag preds) mtop
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
    elemAt (TVApplied k _) _ = error $ printf "Unexpected TVApplied %s used as a position in elemAt" (show k)

    -- Exhaustive top-level cases of a position's type: union components, class members
    -- (via 'classSatCandidates', gated closed-world like the outer coverage decisions), or
    -- relation candidates. 'Nothing' when not enumerable (then another position may be).
    -- Subtracted 'negLeafs' and non-generator conjuncts are dropped, which only ENLARGES
    -- the enumerated set - sound for cover proofs, conservative for escapes.
    enumTypeCases (UnionType Nothing leafs []) = case splitUnionType leafs of
      ls@(_:_:_) -> Just (map singletonType ls)
      _          -> Nothing -- single leaf: no top-level refinement available
    enumTypeCases (UnionType (Just (preds, _, [])) pos []) = do
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
    enumAtom (SatRel r)   = relSatCandidates typeEnv vaenv True r >>= mapM candCase
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

-- | The loose /projection/ arg-keyset relation used by matching\/dispatch consumers (see
-- 'teArgModeProjection'). Returns 'True' when @sub@ definitely cannot project onto @super@
-- on the arg-set dimension. This reproduces the historical (pre-precise) behavior verbatim:
-- the legacy 'PtArgExact'\/'PtArgAny' combinations are handled by the first four cases, and
-- any combination involving a lower-unbounded mode falls back to the interval relation
-- ('argModeCovers'). Crucially, an 'Any'\/upper-unbounded source projecting onto an 'Exact'
-- requirement is feasible iff its listed args ⊆ the requirement's args (extra args are ignored).
looseArgModeRejects :: PartialType -> PartialType -> Bool
looseArgModeRejects sub@PartialType{ptArgs=subArgs, ptArgMode=subMode} super@PartialType{ptArgs=superArgs, ptArgMode=superMode} =
     (subMode == PtArgExact && superMode == PtArgExact && subKeys /= superKeys)
  || (subMode == PtArgAny && superMode == PtArgAny && not (superKeys `isSubsetOf` subKeys))
  || (superMode == PtArgExact && not (subKeys `isSubsetOf` superKeys))
  || (subMode == PtArgExact && superMode == PtArgAny && S.null subKeys && not (S.null superKeys))
  || (involvesLowerUnbounded && not (argModeCovers super sub))
  where
    subKeys = H.keysSet subArgs
    superKeys = H.keysSet superArgs
    involvesLowerUnbounded = not (ptArgLb subMode) || not (ptArgLb superMode)

-- | A private helper for 'isSubPartialOfWithEnv' that checks while ignore class expansions
isSubPartialOfWithEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> Bool
isSubPartialOfWithEnv TypeEnv{teDebug=True} _ l r | trace (printf "[SUBPARTIAL] %s ⊆ %s" (show l) (show r)) False = undefined
isSubPartialOfWithEnv _ _ PartialType{ptName=subName} PartialType{ptName=superName} | subName /= superName = False
-- Projection (loose) keyset feasibility, used by matching\/dispatch consumers via
-- 'teArgModeProjection' (TreeBuild, Eval, scheme verification). Reproduces the historical
-- relation in which an 'Any'\/upper-unbounded source may project onto an 'Exact' requirement
-- by ignoring extra args. This looseness is genuine projection (not an interval relation)
-- and is intentionally NOT applied to the precise set algebra below.
isSubPartialOfWithEnv typeEnv _ sub super
  | teArgModeProjection typeEnv
  , looseArgModeRejects sub super = False
-- Keyset feasibility on the arg-set interval (see 'PtArgMode'): sub ⊆ super requires
-- super's lower\/upper bound to hold for every arg-set allowed by sub ('argModeCovers'),
-- on mode-normalized partials so a vacuous bound does not read as extra arg-sets
-- (e.g. @/length(this=..)..@ IS @/length(this=..)@ — see 'normalizePtArgMode').
isSubPartialOfWithEnv typeEnv _ sub super
  | not (teArgModeProjection typeEnv)
  , not (argModeCovers (normalizePtArgMode typeEnv super) (normalizePtArgMode typeEnv sub)) = False
isSubPartialOfWithEnv typeEnv vaenv sub@PartialType{ptVars=subVars, ptArgs=subArgs, ptPreds=subPreds} super@PartialType{ptVars=superVars, ptArgs=superArgs, ptPreds=superPreds} = debugTrace typeEnv msg result
  where
    vaenv' = H.union vaenv (substituteTVExt vaenv <$> H.unionWith (intersectTypesEnv typeEnv vaenv) (ptVarArg super) (ptVarArg sub))
    -- A sub-side FREE reference reads as ⊤ ('freeTVIntToTop'): it must not resolve through
    -- the shared env's like-named SUPER binding, which would narrow the sub's value set to
    -- the super's and wrongly prove e.g. @Cons(head=$/T) ⊆ Cons[/T=#noCore](head=#noCore)@.
    -- A field's value set is DECLARATION-BOUNDED: no constructible value holds a field outside
    -- its declared type (@Cons(tail=⊤)@ admits no tail outside @∀ConsList@), so when the literal
    -- comparison fails the sub value is re-read clamped by the declared field type (the clamp
    -- only shrinks the sub side, so checking it as a fallback equals always clamping). This is
    -- what makes the field-completed @Cons(head=⊤, tail=⊤)@ a subtype of the class member form
    -- @Cons[/T=$/T](head=$/T, tail=∀ConsList[/T=$/T])@ — the same set under the declaration.
    -- The declared bound is instantiated with 'freeTVIntToTopDeep' under the SUB's scope: its
    -- free template parameters read ⊤ rather than staying symbolic and resolving through the
    -- shared env's SUPER binding (which would wrongly prove
    -- @Cons(head=$/T) ⊆ Cons[/T=#noCore](head=#noCore)@ again through the clamp).
    hasAll sb sp = and $ H.elems $ H.intersectionWithKey argOk (fmap (freeTVIntToTop vaenv sub) sb) sp
      where
        argOk k sbVal spVal = isSubtypeOfWithEnv typeEnv vaenv' sbVal spVal
          || any (\d -> isSubtypeOfWithEnv typeEnv vaenv' (intersectTypesEnv typeEnv vaenv' sbVal d) spVal) (declArgBounds k)
        declArgBounds k = [freeTVIntToTopDeep vaenv sub d | PartialType{ptArgs=dArgs} <- typeGraphData typeEnv (ptName sub), Just d <- [H.lookup k dArgs]]
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
    hasAllPreds (PredsOne subPs) (PredsOne supPs) = predImpliesAtom typeEnv vaenv' subPs supPs
    hasAllPreds p PredsNone = p == PredsNone
    hasAllPreds subPs supPs = error $ printf "Unimplemented hasAllPreds for sub (%s) and sup (%s)" (show subPs) (show supPs)

-- | Whether a constant's parent partial type is covered by the given positive leafs.
constantCoveredByLeafs :: Constant -> PartialLeafs -> Bool
constantCoveredByLeafs c leafs = isSubtypeOfWithEnv emptyTypeEnv' H.empty (singletonType $ constantPartialType c) (UnionType Nothing leafs [])

-- | Checks if a constant value is a member of a type.
-- A constant is in a type if it appears in the constant list, or its parent partial type is in the type.
constantInType :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Constant -> Type -> Bool
constantInType _ _ _ PTopType = True
constantInType _ _ c (UnionType Nothing leafs cs) = c `elem` cs || constantCoveredByLeafs c leafs
-- Just-top: c is in the type iff it is a positive member (posConsts/posLeafs), or it is in the
-- predicated universe and not excluded (by negConsts or negLeafs)
constantInType typeEnv vaenv c (UnionType (Just (preds, negLeafs, negConsts)) posLeafs posConsts) =
  c `elem` posConsts
  || constantCoveredByLeafs c posLeafs
  || ( c `notElem` negConsts
       && not (constantCoveredByLeafs c negLeafs)
       && isSubtypeOfWithEnv typeEnv vaenv (singletonType $ constantPartialType c) (TopType H.empty preds)
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
-- ⊤ ⊆ a positive union: never (open world - a fresh type name or constant kind escapes any
-- finite positive listing). A 'Just'-top rhs is NOT decided here: its subtractive component may
-- be vacuous (e.g. tautological preds ¬(A∧B) with A∧B unsatisfiable, making it ⊤), which the
-- predicate clauses below decide exactly (⊤ is itself the pure top with 'PredsNone').
isSubtypeOfWithEnv _ _ PTopType (UnionType Nothing _ _) = False
-- Mixed type on left (top + positive leafs/constants): (top ∪ pos ∪ cs) ⊆ t2 iff top ⊆ t2 ∧ (pos ∪ cs) ⊆ t2
isSubtypeOfWithEnv typeEnv vaenv (UnionType (Just topData) pos cs) t2
  | not (H.null pos) || not (null cs) =
      isSubtypeOfWithEnv typeEnv vaenv (UnionType (Just topData) H.empty []) t2
      && isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing pos cs) t2
-- PosPartials ⊆ NegPartials: filter by name.
-- filteredLeafs is always exact for names in allowedNames; hasOutside only means the type
-- has additional elements outside those names, which cannot affect subtype checking here.
isSubtypeOfWithEnv typeEnv vaenv t1@(UnionType Nothing subPartials []) t2@(UnionType (Just _) _ _) =
  -- Symbolic decision (emptiness of @t1 ∧ ¬t2@ against the class graph) avoids materializing
  -- the predicate expansion; fall back to expansion outside the fragment.
  fromMaybe
    (let (filteredLeafs, _) = expandTypesWithNamesFull typeEnv vaenv t2 (H.keysSet subPartials)
     in isSubtypeOfWithEnv typeEnv vaenv t1 (UnionType Nothing filteredLeafs []))
    (typeCoveredSymbolic typeEnv vaenv False t1 t2)
-- PosPartials-with-constants ⊆ t2: partials check + each constant must be in t2
-- (Only for PosPartials: in NegPartials, cs are excluded constants, not members)
isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing leafs cs@(_:_)) t2 =
  isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing leafs []) t2
  && all (\c -> constantInType typeEnv vaenv c t2) cs
-- Pure top ⊆ MIXED top: fold t2's positive components into t1's subtraction and recurse with a
-- pure rhs (the set identity @A ⊆ B ∪ C ⟺ A − C ⊆ B@, exact for any C): the pos2 leaf regions
-- join t1's excluded leafs and the cs2 points its excluded constants — both exactly
-- representable on a 'Just'-top. (t1 is always a pure top when both sides are 'Just'-tops:
-- a mixed left was split by the clause above.)
isSubtypeOfWithEnv typeEnv vaenv (UnionType (Just (p1, n1, nc1)) (H.null -> True) []) (UnionType (Just top2) pos2 cs2)
  | not (H.null pos2) || not (null cs2) =
      isSubtypeOfWithEnv typeEnv vaenv
        (UnionType (Just (p1, H.unionWith S.union n1 pos2, L.nub (nc1 ++ cs2))) H.empty [])
        (UnionType (Just top2) H.empty [])
-- Pure top ⊆ pure top with EXCLUDED constants on the rhs: @A ⊆ B − cs ⟺ A ⊆ B ∧ ∀c∈cs. c ∉ A@
-- (exact). Stripping nc2 up front keeps the rhs inside the symbolic engine's constant-free
-- fragment for the clauses below.
isSubtypeOfWithEnv typeEnv vaenv t1@(UnionType (Just _) (H.null -> True) []) (UnionType (Just (p2, n2, nc2@(_:_))) (H.null -> True) []) =
  all (\c -> not $ constantInType typeEnv vaenv c t1) nc2
  && isSubtypeOfWithEnv typeEnv vaenv t1 (TopType n2 p2)
-- Pure NegPartials-top ⊆ pure NegPartials-top, SAME predicates: (U_p − n1 − nc1) ⊆ (U_p − n2)
-- holds EXACTLY iff (U_p ∩ n2) ⊆ n1. Only the part of n2 inside the shared predicate universe
-- matters — an excluded leaf outside U_p removes nothing from either side, and one that differs
-- from an n1 leaf only in arg-mode/field-completion still excludes the same U_p members. This is
-- what makes @∀Bag − /Cons..@ and @∀Bag − /Cons[full]@ compare equal (both exclude
-- @∀Bag ∩ /Cons.. = /Cons[full]@). A point exclusion (nc1) can never cover the leaf region
-- (U_p ∩ n2) − n1 (leaf regions are not finite constant sets), so the region check ignores nc1.
-- Matched only for the fully-reduced form — no positive leafs/constants on either side (mixed
-- lefts split, mixed rights folded above) and no rhs excluded constants (stripped above) —
-- where the identity is exact.
isSubtypeOfWithEnv typeEnv vaenv (UnionType (Just (p1, n1, _)) (H.null -> True) []) (UnionType (Just (p2, n2, [])) (H.null -> True) [])
  | p1 == p2 = isSubtypeOfWithEnv typeEnv vaenv (intersectTypesEnv typeEnv vaenv (TopType H.empty p1) (UnionType Nothing n2 [])) (UnionType Nothing n1 [])
-- Pure NegPartials-top ⊆ pure NegPartials-top, DIFFERING predicates: (U_p1-n1-nc1) ⊆ (U_p2-n2).
-- Both sides are fully reduced here (mixed lefts split, mixed rights folded, rhs excluded
-- constants stripped, same-preds pairs decided, by the clauses above). The excluded constants
-- of t1 are captured by the @t1@ binding and consulted by the fallbacks below (not ignored);
-- the cheap guards decide the common cases: sufficient iff predImplies p1 p2 and n2 ⊆ n1;
-- otherwise the symbolic decision.
isSubtypeOfWithEnv typeEnv vaenv t1@(UnionType (Just (p1, n1, _)) (H.null -> True) []) t2@(UnionType (Just (p2, n2, [])) (H.null -> True) [])
  | isPredsContradictory p1 = True  -- t1 = ∅ ⊆ anything
  | (isPredsContradictory (predsAnd p1 (predsNot p2)) || predImplies typeEnv vaenv p1 p2)
    && isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing n2 []) (UnionType Nothing n1 []) = True
    -- U_p1 ⊆ U_p2 (syntactically: p1 ∧ ¬p2 = ∅, or by 'predImplies') and n2 ⊆ n1, so
    -- (U_p1 - n1 - nc1) ⊆ (U_p2 - n2). The universe implication alone is NOT sufficient:
    -- an n2 leaf inside U_p1 but not in n1 stays in t1 while t2 excludes it.
  | predsEmpty typeEnv vaenv p1 = True  -- t1 = ∅ by class-graph disjointness ⊆ anything (fallback: expansion is costly)
  -- Symbolic decision (emptiness of @t1 ∧ ¬t2@); the engine's positive gate reproduces the
  -- 'tePrgmEnv'/'predsSealed' enumeration gate of the re-expression fallback. Decided on t1
  -- WITHOUT its excluded constants (which are outside the DNF fragment): dropping them only
  -- widens t1, so a proven cover is sound — and it is exact, because with differing predicates
  -- t2's missing set inside U_p1 is a union of name-keyed leaf regions and nc2 points (both
  -- name-level), never exactly a finite set of excluded constants, so re-adding nc1's points
  -- cannot flip an uncovered t1 to covered.
  | otherwise = fromMaybe oldReexpress
      (typeCoveredSymbolic typeEnv vaenv True (UnionType (Just (p1, n1, [])) H.empty []) t2)
  where
    -- Re-express U_p1 - n1 as a positive type and recurse. Guarded on progress: when the predicate
    -- expands to the universe (e.g. p1 = PredsNone) the difference reproduces the same complement,
    -- which would recurse forever, so require the rewrite to differ from t1 (else conservative False).
    oldReexpress
      | tePrgmEnv typeEnv || predsSealed typeEnv p1
      , let t1' = snd $ differenceTypeWithEnv typeEnv vaenv (expandPredicates typeEnv vaenv p1) (UnionType Nothing n1 [])
      , t1' /= t1 = isSubtypeOfWithEnv typeEnv vaenv t1' t2
      | otherwise = False
-- Pure top ⊆ (PosPartials ∪ constants): the rhs constant points cannot help cover the leaf
-- regions of the predicate universe (leaf regions are not finite constant sets — the same
-- postulate as the nc1 treatment above), so decide against the positive leafs alone. Keeps
-- the rhs inside the symbolic engine's constant-free fragment for the clause below.
isSubtypeOfWithEnv typeEnv vaenv t1@(UnionType (Just _) (H.null -> True) []) (UnionType Nothing superPartials (_:_)) =
  isSubtypeOfWithEnv typeEnv vaenv t1 (UnionType Nothing superPartials [])
-- NegPartials ⊆ PosPartials: filter t1 by superPartials names.
-- hasOutside=True: t1 has elements outside superPartials names.
--   True if preds are syntactically contradictory (t1 = ∅).
--   True if predImpliesPartials establishes U_preds ⊆ superPartials (negLeafs removal can only shrink).
--   Otherwise False (conservative: can't prove subtype without knowing predicate members).
isSubtypeOfWithEnv typeEnv vaenv t1@(UnionType (Just (preds, negLeafs, _)) (H.null -> True) []) t2@(UnionType Nothing superPartials []) =
  -- The predicate region (U_preds - negLeafs) must be covered by t2; the excluded constants only
  -- shrink t1 further, and a positive union cannot cover all-but-points of a leaf region, so the
  -- region check without them is exact. (Positive leaves and constants of t1 are split off by the
  -- "mixed type on left" branch above.)
  predsCovered
  where
   -- Symbolic decision first (emptiness of @(U_preds − negLeafs) ∧ ¬t2@ against the class
   -- graph); the materialized chain remains as the out-of-fragment fallback. Gated open-world
   -- ('gatePos' = 'True', matching 'predsEmptySymbolic' and the sealed-only 'predsCoveredExpand'
   -- fallback below): an UNSEALED class in @preds@ must NOT be enumerated closed-world here, or the
   -- cover proves @Top(Bag ∧ ¬Nil) ⊆ Cons@ (Bag's only non-Nil member so far IS a Cons) — broken by
   -- any future @isa Bag@ declaration, and inconsistent with the open-world emptiness oracle, which
   -- collapses @∀Bag ∩ (¬Nil − Cons)@ to ∅ while the union side keeps it (a difference-subset law
   -- failure). In the closed-world coverage environment ('tePrgmEnv') the gate still enumerates.
   predsCovered = fromMaybe predsCoveredExpand
     (typeCoveredSymbolic typeEnv vaenv True (UnionType (Just (preds, negLeafs, [])) H.empty []) t2)
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
    -- An exact-flagged expansion exhausts the top only for positive-atom predicates
    -- ('predsPositiveAtoms'): a 'PredsNone'/negated top also contains constants of unlisted
    -- kinds and open-world values outside every declared name, so it is never inside a finite
    -- positive union (the same open-world rule as @⊤ ⊆ positive union = never@).
    (leafs, False) | predsPositiveAtoms preds -> isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing leafs []) t2
                   | otherwise -> False
-- PosPartials ⊆ PosPartials: check partials structurally
isSubtypeOfWithEnv typeEnv vaenv (UnionType Nothing subPartials []) (UnionType Nothing superPartials []) =
  debugTrace typeEnv msg result
  where
    supersByName = splitUnionTypeByName superPartials
    subsByName   = splitUnionTypeByName subPartials
    -- A sub group with no same-named super can still be covered when every one of its leaves is
    -- uninhabited by declaration-bounding ('partialUninhabitedByDecl' — the empty set is a subset
    -- of anything); an inhabited leaf requires a same-named super as before.
    result = and (H.elems (H.mapWithKey subGroupCovered subsByName))
    subGroupCovered n subs = case H.lookup n supersByName of
      Just sups -> all (isSubPartial sups) subs
      Nothing   -> all (partialUninhabitedByDecl typeEnv vaenv) subs
    isSubPartial sup sub = directCheck || coveredByArgSets || coveredBySlotUnion sub sup || coveredBySplit (splitPartialByUnionVarArgs sub) || coveredBySplit (splitPartialByUnionVarArgs expandedSub) || partialUninhabitedByDecl typeEnv vaenv sub
      where
        directCheck = any (isSubPartialOfWithEnv typeEnv vaenv sub) sup
        -- A moded sub leaf IS the union of its arg-set interval's exact pieces
        -- ('ptIntervalArgKeys'); it is covered when every piece is — each piece may be
        -- covered by a DIFFERENT super leaf (e.g. @/x(a=A)??@ ⊆ @/x ∪ /x(a=A)@), a joint
        -- cover no single-leaf check sees. Only a genuinely multi-piece interval recurses
        -- (an exact leaf is its own single piece, already decided by 'directCheck').
        coveredByArgSets = case ptIntervalArgKeys typeEnv subN of
          Just kss@(_:_:_) -> all (isSubPartial sup . ptAtArgKeys subN) kss
          _                -> False
          where subN = normalizePtArgMode typeEnv sub
        -- A genuinely split sub is covered when every split component is covered by some super.
        coveredBySplit splits = length splits > 1
                                && all (\s -> any (isSubPartialOfWithEnv typeEnv vaenv s) sup) splits
        expandedSub = sub{ptArgs = fmap expandArg (ptArgs sub), ptVars = fmap expandArg (ptVars sub)}
          -- The global (predicate) component and the positive partials of a 'UnionType' are
          -- joined by union; the predicate does not constrain the partials. So expanding a
          -- @Just(preds)@ arg to /just/ @expand(preds) - negLeafs@ would silently drop the
          -- union-joined positive leaves (and constants), under-approximating the arg and
          -- producing a false-positive subtype (e.g. @m(⊤)@, a leaf outside @preds@, would
          -- vanish). Union the partial component back in so the expansion is faithful.
          where expandArg (UnionType (Just (preds, negLeafs, _)) pos cs) =
                  unionTypesWithEnv typeEnv vaenv
                    (snd $ differenceTypeWithEnv typeEnv vaenv (expandPredicates typeEnv vaenv preds) (UnionType Nothing negLeafs []))
                    (UnionType Nothing pos cs)
                expandArg t = t
    -- A family of supers identical to the sub at every var/arg slot but ONE jointly covers the
    -- sub when the union of their values at that slot covers the sub's value there — e.g.
    -- @fluffy(∀Animal) ⊆ fluffy(Animal∧¬Fish) ∪ fluffy(∀Fish)@, the pair the exact union's
    -- negLeaf reduction produces alongside an untouched original. Erasing the slot to ⊤ on both
    -- sides and requiring structural equality delimits the family exactly; the slot cover is
    -- decided through the type-level union, so it is whatever the union algebra proves (sound).
    coveredBySlotUnion sub sup =
      any (coverSlot ptArgs (\p m -> p{ptArgs=m})) (H.keys (ptArgs sub))
      || any (coverSlot ptVars (\p m -> p{ptVars=m})) (H.keys (ptVars sub))
      where
        coverSlot get set k = case H.lookup k (get sub) of
          Nothing -> False
          Just sv ->
            let erase p = set p (H.insert k PTopType (get p))
                family = [v | supP <- sup, Just v <- [H.lookup k (get supP)], erase supP == erase sub]
            in length family > 1 && isSubtypeOfWithEnv typeEnv vaenv sv (unionAllTypesWithEnv typeEnv vaenv family)
    msg = printf "[SUBTYPE] %s ⊆ %s = %s" (show $ UnionType Nothing subPartials []) (show $ UnionType Nothing superPartials []) (show result)
-- partials ⊆ (superLeafs ∪ constants): constants in supertype don't help cover partials
isSubtypeOfWithEnv typeEnv vaenv t1@(UnionType Nothing _ []) (UnionType Nothing superLeafs (_:_)) =
  isSubtypeOfWithEnv typeEnv vaenv t1 (UnionType Nothing superLeafs [])
-- Unreachable by clause flow: every 'Just'-top pairing is decomposed above (mixed left split →
-- mixed right folded → rhs excluded constants stripped → pure pairs decided), which the
-- coverage checker cannot see through the view patterns and guards. A genuine flow hole must
-- fail loudly here rather than silently mis-decide a subtype.
isSubtypeOfWithEnv _ _ t1 t2 = error $ printf "Unreachable isSubtypeOfWithEnv arguments %s ⊆ %s" (show t1) (show t2)

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
        asLeafs (UnionType (Just (_, negLeafs, _)) posLeafs []) = H.unionWith S.union negLeafs posLeafs  -- shouldn't normally occur
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
-- Contradictory top: the universe filtered by an unsatisfiable predicate is empty, so
-- (∅ - neg) ∪ pos = pos. Two detectors: the syntactic 'isPredsContradictory' (e.g.
-- PredsAnd[p, ¬p], which arises during difference/subtype - @t ∩ ¬t@ on a predicated top yields
-- @p ∧ ¬p@; leaving it un-reduced falsely reports monotonicity growth in
-- 'verifySchemeActLowers'), and class-graph disjointness for a conjunction of >=2 PredClass
-- atoms (e.g. two distinct unsealed empty classes expand to distinct placeholders) which the
-- syntactic check cannot see - consulting 'predsEmpty', the same oracle the
-- placeholder/expansion paths use, so the symbolic conjunction agrees with its expansion
-- (without it, 'isSubtypeOf X BottomType' is false for an empty class conjunction, breaking
-- distributivity). Must precede the empty-neg/empty-pos clauses below, whose catch-alls would
-- otherwise return such a top unchanged. 'predsEmpty' itself gates to >=2-atom conjunctions and
-- decides symbolically first, so this stays cheap and non-recursive for any atom mix (e.g. two
-- distinct PredRel atoms - a value has ONE name, so the conjunction is empty - not just the
-- all-PredClass conjunctions it was once restricted to).
compactType typeEnv vaenv (UnionType (Just (preds, _, _)) pos cs)
  | isPredsContradictory preds || predsEmpty typeEnv vaenv preds = compactType typeEnv vaenv (UnionType Nothing pos cs)
-- Class-graph unsatisfiable top: a negated conjunct implied by the rest (@p ∧ ¬q = ∅@ when @p ⟹ q@),
-- e.g. @PredClass C[/T=a] ∧ ¬PredClass C[/T=b]@ with @a ⊆ b@. The syntactic 'isPredsContradictory'
-- above only sees identical-atom @X ∧ ¬X@; this uses 'predImplies' to also reduce a covariant
-- parameterized-class pair — the difference @C[a] − C[b]@ when @a ⊆ b@ (which otherwise leaves an
-- unreduced @C[a] ∧ ¬C[b]@ residue). Gated on a present negation so the common all-positive
-- conjunctions never pay the 'predImplies' pass.
compactType typeEnv vaenv (UnionType (Just (PredsAnd ps, _, _)) pos cs)
  | any isNeg ps
  , or [ predImplies typeEnv vaenv (mkAnd rest) q | (PredsNot q, rest) <- picks ps ] =
      compactType typeEnv vaenv (UnionType Nothing pos cs)
  where
    isNeg (PredsNot _) = True
    isNeg _            = False
    -- each element paired with the list of all the others
    picks = go []
      where go _   []       = []
            go pre (x:post) = (x, pre ++ post) : go (pre ++ [x]) post
-- Pure ¬(single relation) top: re-express as the universe minus the relation's expansion.
-- Matched on exactly this shape (no catch-all) so other pure-top predicates flow on to the
-- ¬(unsatisfiable) and PredsAnd-simplification clauses below.
compactType typeEnv vaenv t@(UnionType (Just (PredsNot (PredsOne (PredRel rp)), negPartials, [])) posLeafs [])
  | H.null negPartials && H.null posLeafs = case expandRelPartial typeEnv vaenv rp of
      (UnionType Nothing rp' []) -> UnionType (Just (PredsNone, rp', [])) H.empty []
      _               -> t
-- Top component with non-trivial preds and some exclusion (negated partials and/or excluded
-- constants), no positive leafs. This is the residual @U_preds − negPartials − negConsts@ that a
-- relation/class-top difference (@t1 ∩ ¬t2@) produces. Canonicalise it:
--  * Drop any exclusion disjoint from the predicated domain — excluding something not in the set is a
--    no-op (a negLeaf whose intersection with U_preds is ∅, or a negConst not a member of U_preds).
--    This is what lets @~/Cons − /Nil@ collapse back to @~/Cons@. Use the intersection check
--    (A ∩ B = ∅) rather than a subtype check for the leafs since the latter can fail to prove
--    disjointness when the ¬B expansion is approximate (hasOutside=True).
--  * If the SURVIVING negated partials cover the ENTIRE predicated domain (@U_preds ⊆ negPartials@)
--    the whole thing is empty. This is what makes @~/Cons − /Cons..@ reduce: it is
--    @Top(PredRel /Cons, {/Cons..})@ and @~/Cons ⊆ /Cons..@, so ∅ (without this the difference stays
--    an unreduced symbolic subtraction). 'isSubtypeOfWithEnv' is sound (no false positives), so this
--    never empties a genuinely inhabited residual.
compactType typeEnv vaenv t@(UnionType (Just (preds, negPartials, negConsts)) posLeafs [])
  | H.null posLeafs && preds /= PredsNone
  , not (H.null negPartials) || not (null negConsts) =
      if not (H.null keptNegLeafs) && isSubtypeOfWithEnv typeEnv vaenv predUniverse (UnionType Nothing keptNegLeafs [])
        then BottomType
      else if keptNegLeafs == negPartials && keptNegConsts == negConsts
        then t
      else compactType typeEnv vaenv $ UnionType (Just (preds, keptNegLeafs, keptNegConsts)) H.empty []
  where
    predUniverse  = TopType H.empty preds
    keptNegLeafs  = joinUnionType $ filter negLeafIntersects $ splitUnionType negPartials
    keptNegConsts = filter (\c -> constantInType typeEnv vaenv c predUniverse) negConsts
    negLeafIntersects p = not $ isBottomType $ intersectTypesEnv typeEnv vaenv (singletonType p) predUniverse
-- ¬(unsatisfiable) = everything: top component is U, simplify to PredsNone (same oracle pair
-- as the pure-top clause above).
compactType typeEnv vaenv (UnionType (Just (PredsNot p, negLeafs, ncs)) posLeafs cs)
  | isPredsContradictory p || predsEmpty typeEnv vaenv p = compactType typeEnv vaenv (UnionType (Just (PredsNone, negLeafs, ncs)) posLeafs cs)
-- Cancel pos/neg overlap: an excluded region the positive side re-adds is a vacuous exclusion
-- ((T − neg' − C) ∪ pos' ∪ C = (T − neg') ∪ pos' ∪ C), so the common part drops from the NEG
-- side unconditionally. Its positive listing drops only when the reduced top already contains
-- it (e.g. (U − {X}) ∪ {X} = U for X inside U); a piece outside the predicate universe must
-- survive positively (dropping both sides would LOSE it: (U¬A − A) ∪ A = U¬A ∪ A, not U¬A).
compactType typeEnv vaenv (UnionType (Just (preds, negLeafs, ncs)) posLeafs cs)
  | not (H.null posLeafs), not (H.null negLeafs)
  , let common = H.filter (not . S.null) $ H.intersectionWith S.intersection posLeafs negLeafs
  , not (H.null common) =
      let subtractLeafs leafs sub = H.filter (not . S.null) $ H.differenceWith (\a b -> Just (S.difference a b)) leafs sub
          negLeafs' = subtractLeafs negLeafs common
          topOnly = UnionType (Just (preds, negLeafs', ncs)) H.empty []
          redundantPos = joinUnionType $ filter (\p -> isSubtypeOfWithEnv typeEnv vaenv (singletonType p) topOnly) $ splitUnionType common
      in compactType typeEnv vaenv $ UnionType (Just (preds, negLeafs', ncs)) (subtractLeafs posLeafs redundantPos) cs
-- Cancel constants across the positive and subtractive components:
--  * a positive constant also excluded by the top (c ∈ negConsts) whose point lies in the
--    predicated region (U − negLeafs) cancels out of BOTH lists ((U − … − {c}) ∪ {c} restores the
--    point); if the point lies outside that region the exclusion is vacuous and drops while the
--    positive listing survives;
--  * a positive constant the top or the positive leafs already contain is redundant and drops.
compactType typeEnv vaenv (UnionType (Just (preds, negLeafs, negConsts)) posLeafs posConsts)
  | not (null posConsts)
  , let (negConsts', posConsts') = foldr cancelConst (negConsts, []) posConsts
  , (negConsts', posConsts') /= (negConsts, posConsts) =
      compactType typeEnv vaenv $ UnionType (Just (preds, negLeafs, negConsts')) posLeafs posConsts'
  where
    predRegion = UnionType (Just (preds, negLeafs, [])) H.empty []
    cancelConst c (ncs, pcs)
      | c `elem` ncs = if constantInType typeEnv vaenv c predRegion
                         then (L.delete c ncs, pcs)
                         else (L.delete c ncs, c : pcs)
      | constantInType typeEnv vaenv c (UnionType (Just (preds, negLeafs, ncs)) H.empty [])
        || constantCoveredByLeafs c posLeafs = (ncs, pcs)
      | otherwise = (ncs, c : pcs)
-- PredsAnd: remove PredRel atoms dominated by a PredClass atom in the same conjunction.
-- e.g. PredsAnd[PredClass /Boolean, PredRel Boolean] → PredsOne (PredClass /Boolean)
-- Soundness: expandRelPartial r includes classPartial c in its expansion, so U_PredClass c ⊆ U_PredRel r
-- when r's name path is a suffix of c's name path.
compactType typeEnv vaenv (UnionType (Just (PredsAnd ps, negPartials, ncs)) posLeafs cs)
  | not (null predClassAtoms)
  , let simplified = filter (not . isDominatedByPredClass) ps
  , length simplified < length ps =
      -- Recurse: might simplify further (e.g. single-element PredsAnd → PredsOne)
      compactType typeEnv vaenv $ UnionType (Just (mkAnd simplified, negPartials, ncs)) posLeafs cs
  where
    predClassAtoms = [c | PredsOne (PredClass c) <- ps]
    -- A PredRel r is dominated if some PredClass c in the conjunction has r's path as a suffix
    isDominatedByPredClass (PredsOne (PredRel r)) =
      any (\c -> relativeNameMatches (ptName r) (ptName c)) predClassAtoms
    isDominatedByPredClass _                      = False
-- PredsAnd: drop a bare PredClass conjunct that is a declared SUPERCLASS of a bare sibling
-- (@hasClass Sub ⟹ hasClass Super@, so @Sub ∧ Super = Sub@) - e.g. @PredClass Ord ∧ PredClass Eq →
-- PredClass Ord@ since @Ord isa Eq@. Uses the fast class-graph 'classDeclaredSubclassOf' (not
-- the sat engine). Restricted to arg/var-free (bare) class atoms so no element bound is lost (a
-- parameterized @Ord[X] ∧ Eq[Y]@ would need @X ⊆ Y@, which the name-only edge does not check).
-- Without this an element bound like @Cons[/T=(Ord∧Eq)]@ stays un-canonical and re-derives each
-- epoch, blocking convergence. Positive-only complement of the negation-gated clause below.
compactType typeEnv vaenv (UnionType (Just (PredsAnd ps, negPartials, ncs)) posLeafs cs)
  | length bareClasses >= 2
  , let simplified = filter (not . isSuperclassOfSibling) ps
  , length simplified < length ps =
      compactType typeEnv vaenv $ UnionType (Just (mkAnd simplified, negPartials, ncs)) posLeafs cs
  where
    bareClasses = [c | PredsOne (PredClass c) <- ps, plainPartial c]
    isSuperclassOfSibling (PredsOne (PredClass q))
      | plainPartial q =
          any (\p -> ptName p /= ptName q && classDeclaredSubclassOf typeEnv (ptName p) (ptName q)) bareClasses
    isSuperclassOfSibling _ = False
-- PredsAnd: drop a conjunct implied by a sibling (@p ∧ q = p@ when @p ⟹ q@), so a disjunction
-- conjunct absorbed by a stronger sibling collapses - e.g. with @Ord isa Eq@,
-- @PredClass Eq ∧ (PredRel Eq ∨ PredRel Ord) → PredClass Eq@ (the element bound a parameterized
-- type accumulates when its constructor is a member of two related classes). Gated to conjunctions
-- containing a 'PredsNot' (the disjunction/complement form) so the common atom-only conjunctions
-- never pay the pairwise 'predImplies'.
compactType typeEnv vaenv (UnionType (Just (PredsAnd ps, negPartials, ncs)) posLeafs cs)
  | any isPredsNotConjunct ps
  , let simplified = dropImpliedConjuncts ps
  , length simplified < length ps =
      compactType typeEnv vaenv $ UnionType (Just (mkAnd simplified, negPartials, ncs)) posLeafs cs
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
-- Mixed Just-top + pos: absorb a positive leaf that the predicate region already covers
-- (@L ⊆ ∀preds − negs@ makes @L@ redundant in @(∀preds − negs) ∪ pos@). A recursive class
-- re-expanded during solving accretes exactly this form one level deeper each epoch
-- (@∀ConsList ∪ Nil ∪ Cons(tail=∀ConsList)@); absorbing restores the finite predicate form so
-- the solver converges — the mixed-form complement of 'foldRecursiveType', which folds pure
-- positive unions. Candidates are name-gated to the declared constituents of the predicate's
-- class atoms so unrelated leaves never pay the subtype check.
compactType typeEnv vaenv (UnionType (Just topData@(preds, _, _)) pos cs)
  | not (H.null pos)
  , not (S.null constituentNames)
  , ((_:_), kept) <- L.partition absorbable (splitUnionType pos) =
      compactType typeEnv vaenv $ UnionType (Just topData) (joinUnionType kept) cs
  where
    constituentNames = case tryPredsToList preds of
      Just ps -> S.unions [classConstituentNames typeEnv (ptName c) | PredClass c <- ps]
      Nothing -> S.empty
    -- A leaf's RECURSIVE self-referential field can spuriously fail the strict subtype: the
    -- over-unrolled tail is @List@-topped while the class expansion of @Cons@ declares its tail
    -- @ConsList@ (and @List ⊋ ConsList@), so @Cons(tail=∀List) ⊄ Cons(tail=∀ConsList)@. Widen ONLY
    -- the recursive self-referential fields to ⊤ for the check ('relaxDeclaredRecursiveArgs', which
    -- keys off the DECLARATION so it applies even when the field's value is a complex union) — a
    -- sound over-approximation (@leaf ⊆ relaxed leaf@, so @relaxed ⊆ top ⟹ leaf ⊆ top@) that ignores
    -- the recursive field. NON-recursive fields are kept, so a concrete member (@IntOrdering(x=5)@)
    -- is absorbed into @∀Ord@ only when it genuinely is one — widening every field would fold
    -- concrete members into their class top, erasing structure eval dispatch keys on.
    -- Only fold a leaf whose type parameters are fully CONCRETE (e.g. @Cons[/T=Integer]@ from
    -- @rangeFrom@'s over-unrolling recursion): its structure is redundant once the class top holds
    -- the same element. A leaf with an ABSTRACT element (@Cons[/T=∀Ord]@, a test value or generic
    -- list) is left alone — folding it into its class top erases the constructor structure eval
    -- dispatch keys on. Widen the arg values to ⊤ for the membership check (soundness:
    -- @leaf ⊆ leaf{args=⊤}@) since membership is by class + element, not by recursive-field value.
    absorbable l = S.member (makeAbsoluteName (ptName l)) constituentNames
                   && all typeFullyConcrete (H.elems (ptVars l))
                   && isSubtypeOfWithEnv typeEnv vaenv (singletonType l{ptArgs = fmap (const PTopType) (ptArgs l)}) (UnionType (Just topData) H.empty [])
-- Top component catch-all (includes PTopType and TopType np PredsNone); preserve constants.
-- Also compact the pos component for structural canonicalization (ensures union commutativity
-- holds structurally: different union orderings that mix Just and Nothing forms produce
-- equivalent pos components after compaction).
compactType typeEnv vaenv (UnionType (Just topData) pos cs)
  | not (H.null pos) =
      -- Recurse into each leaf's nested arg/var types (as the Nothing-pos branch does), so a nested
      -- bound like @Cons[/T=(Ord∧Eq)]@ canonicalises (→ @Cons[/T=∀Ord]@) instead of re-deriving an
      -- un-reduced form each epoch and blocking convergence.
      let argCompacted = joinUnionType $ map (compactPartialArgs typeEnv vaenv) $ splitUnionType pos
          pos' = compactPartialLeafs typeEnv vaenv argCompacted
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
      filteredConsts = filter (\c -> not $ constantCoveredByLeafs c compactedLeafs) consts
  in foldRecursiveType typeEnv vaenv $ UnionType Nothing compactedLeafs filteredConsts

-- | Recurse 'compactType' into a partial's argument and variable types. Compaction
-- otherwise only operates on the top level of a union, so without this a recursive type
-- nested inside an argument (e.g. the @tail@ of a @Cons@) is never canonicalised.
compactPartialArgs :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType
compactPartialArgs typeEnv vaenv p = p
  -- Self-ref capping is applied ONLY to type-VARIABLE slots ('ptVars'), never argument slots
  -- ('ptArgs'). A recursive value's structure lives in its ARGUMENTS — a @ConsList@'s @tail=ConsList@,
  -- a @reverseOnto@'s accumulator @acc@ — and is load-bearing. The spurious over-unrolling to cap is a
  -- non-class arrow's result re-nesting into its own type PARAMETER (@operator++@'s @/T@), which no
  -- value structure depends on.
  { ptArgs = fmap (compactType typeEnv vaenv) (ptArgs p)
  , ptVars = fmap (compactType typeEnv vaenv . settleSelfRefRedundant typeEnv vaenv pName) (ptVars p)
  }
  where pName = makeAbsoluteName (ptName p)

-- | Drop an over-unrolled DIRECT self-referential leaf from a partial's own nested argument\/variable
-- union when its settled form is already covered by a non-self sibling. An arrow's raw applied form is
-- kept in the reaches for eval dispatch, but when it is tied back into one of its OWN parameters — a
-- non-class arrow (@operator++ -> $T@) whose raw result feeds its own @/T@ — it re-nests one level per
-- constraint epoch and never reaches a fixpoint ("Reached runConstraints limit"). Two signals gate the
-- drop, and BOTH are required to avoid removing anything real:
--
--   * SELF-REFERENCE — the leaf's name equals @pName@, the enclosing partial's name (the immediate
--     cycle @operator++[/T=(operator++ + …)]@). A NON-self recursive tail (@Cons(tail=rangeFrom(..))@,
--     a different name) is the genuine structure of a recursive list and is left alone.
--   * COVERAGE — the leaf REDUCES (through its arrows) to a type already present among the non-self
--     sibling leaves, so removing it loses nothing. A self leaf that does not reduce, or whose reduced
--     form is not covered, is a real value and is kept.
--
-- Applied only at NESTED positions (via 'compactPartialArgs'), so the top-level raw forms eval's
-- dispatch keying still needs are untouched.
settleSelfRefRedundant :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> TypeName -> Type -> Type
settleSelfRefRedundant typeEnv vaenv pName (UnionType Nothing pos consts)
  | not (null selfRefs)
  = UnionType Nothing (joinUnionType (others ++ filter keep selfRefs)) consts
  where
    leaves = splitUnionType pos
    (selfRefs, others) = L.partition (\l -> makeAbsoluteName (ptName l) == pName) leaves
    coveredType = UnionType Nothing (joinUnionType others) consts
    keep l = let sl = singletonType l
                 reduced = case typeGraphQueryWithReason typeEnv vaenv l of
                             []    -> sl
                             dests -> unionAllTypesWithEnv typeEnv vaenv [d | (_, _, d) <- dests]
             in reduced == sl || not (isSubtypeOfWithEnv typeEnv vaenv reduced coveredType)
settleSelfRefRedundant _ _ _ t = t

-- | Fold an explicit union of partials back into a recursive class predicate when the
-- union is semantically equal to that class's expansion (the inverse of
-- 'expandRelPartial'). Recursive types are otherwise unrolled one level deeper each
-- constraint epoch (@ConsList ≡ Nil + Cons(tail=ConsList)@), producing syntactically
-- distinct but semantically equal representations that never reach a fixpoint. Folding
-- restores the finite @PredRel@ form so the solver converges.
foldRecursiveType :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
foldRecursiveType typeEnv@TypeEnv{teClassGraph=ClassGraph cg} _vaenv t@(UnionType Nothing leafs [])
  | length leafList > 1 -- only fold a genuine union; a single partial is already minimal
  , hasExplicitRecursion leafList -- only fold an over-unrolled form, never the canonical depth-1 expansion
  , (relP:_) <- foldCandidates
  = TopType H.empty (PredsOne (PredRel (withElem relP)))
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
                         , S.unions (map leafNameSet classTypes) == tNames
                         , leafsWithinMembers classTypes ]
    -- Fold gate against WIDENING: a union leaf that carries an argument key NOT declared by its
    -- class member, with a narrowing (non-⊤) value, is strictly smaller than the bare member — so
    -- the union does not cover the class expansion @U_C@ and folding to @~C@ would widen it. Such
    -- extraneous keys arise when an application's argument keys (e.g. @/l@\/@/r@ from @operator-@)
    -- leak onto a primitive result leaf (@Integer(/r=Integer)@ from @5 - 2@), whose bare member
    -- @Integer@ declares no fields. A genuine over-unrolling only carries the member's DECLARED
    -- fields (a @Cons@'s @head@\/@tail@), so this never rejects a real recursive fold. ⊤-valued
    -- extra keys do not narrow, so they are permitted.
    leafsWithinMembers classTypes = all leafOk leafList
      where
        memberArgKeys = H.fromListWith S.union
          [ (ptName m, H.keysSet (ptArgs m)) | ct <- classTypes, m <- memberPartials ct ]
        memberPartials (UnionType _ pos _) = splitUnionType pos
        memberPartials _                   = []
        leafOk l =
          let declKeys = H.lookupDefault S.empty (ptName l) memberArgKeys
          in null [() | (k, v) <- H.toList (ptArgs l), not (S.member k declKeys), v /= PTopType]
    -- Prefer a 'PredRel' partial nested within @t@ (it carries the right type-var bindings,
    -- e.g. @ConsList[T=$_T]@); otherwise fall back to the bare class partial. This also
    -- catches recursions that bottom out in 'TopType' rather than an explicit @~C@.
    nestedRels = L.nub $ concatMap relPartialsIn leafList
    foldCandidates = [ rp
                     | cName <- matchingClassNames
                     , rp <- case filter (\r -> relativeNameMatches (ptName r) cName) nestedRels of
                               (r:_) -> [r]
                               []    -> [partialVal cName] ]
    -- Keep the element on the folded relation: join each type parameter across the union's leaves
    -- (@Nil + Cons[/T=Integer] + Cons[/T=∀ToString]@ folds to @~ConsList[/T=∀ToString]@, not a bare
    -- @~ConsList@ whose Top element would spuriously escape a @DShow@ bound). The join across the
    -- constructors is exactly the class parameter's value; 'leafsWithinMembers' has already ruled
    -- out any leaf var not declared by a member, so this invents no foreign key.
    joinedVars = H.fromListWith (unionTypes typeEnv)
      [ (k, v) | l <- leafList, (k, v) <- H.toList (ptVars l), v /= PTopType ]
    withElem rp = rp{ptVars = H.union joinedVars (ptVars rp)}
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
          Just (preds, _, _) -> predRelPartials preds
          Nothing            -> []
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
-- The outer posLeafs/posConsts of both sides always merge positively; a 'Just' top is carried
-- SYMBOLICALLY (never expanded to its materialized members — for an unsealed class the expansion
-- is a strictly smaller set, so expanding changes the union). 'compactType' canonicalises the
-- result (dropping positive members the top already covers and cancelling re-added exclusions).
-- Top components: same preds → neg leafs intersect; PredsNone ∪ anything → PredsNone with intersected negs;
-- different preds → De Morgan normalization: p1∨p2 = ¬(¬p1∧¬p2) via predsNot/predsAnd (flat, depth 1).
unionTypesWithEnv typeEnv vaenv t1@(UnionType mTop1 pos1 cs1) t2@(UnionType mTop2 pos2 cs2) =
  debugTrace typeEnv msg result
  where
    pos'  = unionPartialLeafs [pos1, pos2]
    cs'   = L.nub (cs1 ++ cs2)
    result = case (mTop1, mTop2) of
      (Nothing, Nothing) ->
        compactType typeEnv vaenv $ UnionType Nothing pos' cs'
      (Nothing, Just top2) ->
        compactType typeEnv vaenv $ UnionType (Just top2) pos' cs'
      (Just top1, Nothing) ->
        compactType typeEnv vaenv $ UnionType (Just top1) pos' cs'
      (Just (p1, n1, nc1), Just (p2, n2, nc2)) ->
        -- The union's predicate p1∨p2, kept syntactically stable across solver epochs: equal or
        -- one-sided-universal preds collapse directly, and when one predicate region implies the
        -- other the union is just the wider side (semantically equal to the De Morgan fallback,
        -- but avoids nesting another !(!p1 && !p2) layer per union - growth
        -- 'verifySchemeActLowers' cannot order). Otherwise De Morgan with predsNot/predsAnd
        -- normalization: p1∨p2 = ¬(¬p1 ∧ ¬p2), kept flat (depth 1).
        let preds' | p1 == p2 = p1
                   | p1 == PredsNone || p2 == PredsNone = PredsNone
                   | predImplies typeEnv vaenv p1 p2 = p2
                   | predImplies typeEnv vaenv p2 p1 = p1
                   | otherwise = predsNot $ predsAnd (predsNot p1) (predsNot p2)
            -- A constant stays excluded from the union only if NEITHER side contains it
            -- ((U−nc1) ∪ (U−nc2) excludes only nc1 ∩ nc2; membership is checked against the
            -- full side, so a constant one side re-adds positively is not excluded either)
            nc' = filter (\c -> not (constantInType typeEnv vaenv c t1) && not (constantInType typeEnv vaenv c t2)) (L.nub (nc1 ++ nc2))
            -- Excluded leaf regions: with the SAME predicate universe, n1 ∩ n2 is exact. With
            -- differing predicates it is not - an n1 leaf lying OUTSIDE U_p2 is in neither side
            -- (e.g. (U − /aaaaa) ∪ (U_{isa aaaab} − ∅): the /aaaaa leaf meets no n2 leaf yet must
            -- stay excluded, else the union widens to ⊤). The exact exclusion mirrors the constant
            -- rule: the part of each side's excluded leafs the OTHER side does not contain,
            -- (n1 − t2) ∪ (n2 − t1). A leaf−type difference yields positive leafs (plus re-added
            -- points of the other side's excluded constants, which the nc' rule already accounts
            -- for exactly: such a point survives there iff it is in neither side).
            negLeafs'
              | p1 == p2 = snd $ intersectPartialLeafsWithVarEnv typeEnv vaenv n1 n2
              | otherwise = unionPartialLeafs [exclOutside n1 t2, exclOutside n2 t1]
            exclOutside negs other = case snd $ differenceTypeWithEnv typeEnv vaenv (UnionType Nothing negs []) other of
              UnionType Nothing ls _ -> ls
              d -> error $ printf "unionTypesWithEnv: non-positive neg-leaf difference %s" (show d)
        in compactType typeEnv vaenv $
             UnionType (Just (preds', negLeafs', nc')) pos' cs'
    msg = printf "[UNION] %s ∪ %s = %s" (show $ UnionType mTop1 pos1 cs1) (show $ UnionType mTop2 pos2 cs2) (show result)

unionTypes :: TypeGraph tg => TypeEnv tg -> Type -> Type -> Type
unionTypes typeEnv = unionTypesWithEnv typeEnv H.empty

-- | Takes the 'unionTypes' of many types
unionAllTypesWithEnv :: (TypeGraph tg, Foldable f) => TypeEnv tg -> TypeVarArgEnv -> f Type -> Type
unionAllTypesWithEnv typeEnv vaenv = foldr (unionTypesWithEnv typeEnv vaenv) BottomType

-- | Takes the 'unionTypes' of many types
unionAllTypes :: (TypeGraph tg, Foldable f) => TypeEnv tg -> f Type -> Type
unionAllTypes typeEnv = unionAllTypesWithEnv typeEnv H.empty

-- | Takes the 'intersectTypes' of many types
intersectAllTypes :: (TypeGraph tg, Foldable f) => TypeEnv tg -> f Type -> Type
intersectAllTypes _ types | null types = BottomType
intersectAllTypes typeEnv types = foldr1 (intersectTypes typeEnv) types

-- | A private helper for 'intersectPartialsBase' that intersects while ignore class expansions
intersectPartialsBase :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> Maybe (TypeVarArgEnv, [PartialType])
intersectPartialsBase _ _ PartialType{ptName=aName} PartialType{ptName=bName} | aName /= bName = Nothing
intersectPartialsBase typeEnv vaenv (PartialType name' aVars aArgs aPreds (PtArgMode aLb aUb)) (PartialType _ bVars bArgs bPreds (PtArgMode bLb bUb)) = do
  (varsVaenvs, vars') <- unzip <$> intersectMapVars H.empty aVars bVars
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
  -- Only a binding some arg value REFERENCES externally (@$_T@ — a declaration's passthrough,
  -- e.g. @unwrap@'s @/b@ valued @Box[/T=$_T]@ meeting @Box[/T=Integer]@) may be kept: the
  -- reference marks the name as belonging to THIS partial's scope. The arg meets also record
  -- under names with no such reference — a nested partial's demoted local ('demoteTVExtVars')
  -- resolving per value, or a class predicate's own var KEY (@∀Monad[/T=..]@ meeting a member
  -- binds the class's @/T@) — and stamping those onto this partial captures a like-named var
  -- of an unrelated scope (@operator==[$T]@ gaining @/T=Integer@ from its operand's element).
  -- A partial-local ('TVInt') reference that is the arg's DIRECT value (a declaration's own
  -- field pointer — @Id(/value=$T)@, @testFunctorIdentity(/x=$F)@) also names this partial's
  -- scope; one nested inside an embedded partial (@/l=Cons[$T]@) names the inner partial's.
  let ownRefNames t = typeTVExtVarRefs t `S.union` case t of
        TypeVar (TVVar n) TVInt -> S.singleton n
        _                       -> S.empty
  let hoistNames = S.unions $ map ownRefNames (H.elems aArgs ++ H.elems bArgs)
  -- A hoisted binding is stored in its canonical 'substituteTVExt' reading (the same one
  -- 'mergeVarEnvVal' gives every merged env binding), so a binding that only passed through
  -- ONE env is structurally comparable with one that met across several — and with the
  -- difference side's 'resolveOwnScopeRefs' reading. It is read under the ambient @vaenv@ (as
  -- 'resolveOwnScopeRefs' also is), so an external ('TVExt') reference NESTED in the binding
  -- resolves to its ambient value rather than widening to ⊤ (@Cons[/T = #noCore[/a=$_a]]@ with
  -- @$_a@ bound keeps @/a@ at its value); with no ambient binding this is identical to reading
  -- under the empty env. A binding that IS a bare var is a genuine link and stays symbolic (the
  -- reading would default an unbound one to ⊤).
  let canonBinding t@TypeVar{} = t
      canonBinding t           = substituteTVExt vaenv t
  let hoistFilter env = fmap canonBinding $ H.filterWithKey (\k v -> S.member k hoistNames && H.lookup k ambientVars /= Just v) $ fst $ splitVarArgEnv env
  let argsVarBindings = hoistFilter (mergeAllVarEnvs typeEnv argsVaenvs)
  let venvs' = mergeAllVarEnvs typeEnv [argsVarBindings, vars']
  let preds' = predsAnd aPreds bPreds
  -- The two arg-sets each constrain the instance args to an interval over the
  -- arg-name subset lattice (see 'PtArgMode'). Their intersection is the interval
  -- [reqKeys, capKeys]: the required (lower) keys are the union of each side's
  -- required keys; the cap (upper) keys are the intersection of each side's cap.
  -- Bottom-valued keys can never be present, so they are removed from the cap (and make the
  -- partial uninhabited if required - see the 'partials'' guard below).
  let okKeys = H.keysSet args' `S.difference` bottomKeys
  let reqKeys = (if aLb then H.keysSet aArgs else S.empty) `S.union` (if bLb then H.keysSet bArgs else S.empty)
  let ub' = aUb || bUb
  let capKeys = ((if aUb then H.keysSet aArgs else okKeys) `S.intersection` (if bUb then H.keysSet bArgs else okKeys)) `S.difference` bottomKeys
  let restrict ks = H.filterWithKey (\k _ -> k `S.member` ks) args'
  let mkPartial lb' ub'' ks = PartialType name' venvs' (restrict ks) preds' (PtArgMode lb' ub'')
  -- A var binding hoisted from an OPTIONAL arg's meet (the arg is outside reqKeys) holds
  -- only for the arg-sets CONTAINING that arg — stamping it on the whole interval would
  -- narrow the arg-sets missing the arg (e.g. @/T=#noCore@ from a @head@ meet constraining
  -- the argless piece). No single ptVars fits, so such an interval is enumerated with each
  -- arg-set carrying exactly the bindings ITS OWN args' meets discovered.
  let liveArgEnvs = fmap fst $ H.filter (not . isBottomType . snd) argsMap
  let bindingOptArgs = S.filter (\k -> not (H.null (hoistFilter (H.lookupDefault H.empty k liveArgEnvs)))) (capKeys `S.difference` reqKeys)
  let venvsFor ks = mergeAllVarEnvs typeEnv [hoistFilter (mergeAllVarEnvs typeEnv [H.lookupDefault H.empty k liveArgEnvs | k <- S.toList ks]), vars']
  let mkPartialAt ks = PartialType name' (venvsFor ks) (restrict ks) preds' PtArgExact
  let partials'
        | not (S.null (reqKeys `S.intersection` bottomKeys)) = [] -- a required arg is uninhabited
        | ub' = if reqKeys `S.isSubsetOf` capKeys -- bounded interval [reqKeys, capKeys]
            then if reqKeys == capKeys
              then [mkPartial True True capKeys]         -- single arg-set: exact
              else if S.null reqKeys && S.null bindingOptArgs
                then [mkPartial False True capKeys]      -- [∅, capKeys]: powerset
                -- strict bounded interval (or optional-arg bindings): enumerate each
                -- arg-set as an exact partial with its own arg-derived bindings
                else [mkPartialAt (reqKeys `S.union` S.fromList extra) | extra <- powerset $ S.toList (capKeys `S.difference` reqKeys)]
            else []                                       -- infeasible: required exceeds cap
        -- upper-unbounded. Optional keys (listed but not required) that are Top-typed
        -- add no constraint and can be dropped, usually leaving an exact single partial
        -- (required keys with a lower bound, extras allowed). A non-Top optional key
        -- cannot be expressed alongside required keys with a single lower bit, so fall
        -- back to a conservative single partial in that residual case.
        | otherwise =
            let nonTopOptional = S.filter (\k -> H.lookupDefault PTopType k args' /= PTopType) (okKeys `S.difference` reqKeys)
            in if S.null nonTopOptional
                 then [mkPartial (not $ S.null reqKeys) False reqKeys]
                 else [mkPartial (reqKeys == okKeys) False okKeys]
  if null partials' then Nothing else Just (mergeAllVarEnvs typeEnv varsVaenvs, partials')
  where
    -- intersectMap unions so that all typeVars from either a or b are kept
    intersectMapVars vev a b = traverse subValidate $ H.unionWith subUnionVar (fmap (vev,) a) (fmap (vev,) b)
    -- A bare partial-local ('TVInt') reference stored in a VAR slot is a pointer within the
    -- partial itself (e.g. a demoted foreign arm's element var — see 'demoteTVExtVars'): it is
    -- flexible per value and must neither read nor record ambient bindings under its name — a
    -- like-named var of the enclosing scope is a DIFFERENT var. The meet is the other side.
    subUnionVar (aVaenv, TypeVar _ TVInt) (bVaenv, b) = (mergeAllVarEnvs typeEnv [aVaenv, bVaenv], b)
    subUnionVar (aVaenv, a) (bVaenv, TypeVar _ TVInt) = (mergeAllVarEnvs typeEnv [aVaenv, bVaenv], a)
    subUnionVar a b = subUnion a b
    -- Resolve the field meet's freshly-discovered CONCRETE bindings back into its TYPE (the same
    -- resolve-meets-back rule 'intersectTypesEnv' applies at the top level, hoisted into the
    -- recursive field meet). A recursive-tail field @tail -> ∀ConsList[/T=$_T]@ met with a concrete
    -- @Cons[/T=Integer]@ discovers @/T ↦ Integer@ but 'intersectTypesWithVarEnv' returns the bare var;
    -- taking @snd@ alone leaves the deep tail free, so it falls to the requirement's declared bound
    -- (@Ord@) — abstracting a concrete list's element at depth. Concrete-only, so a genuine var link
    -- (a polymorphic parameter tying positions together) is left as a var.
    subUnion (aVaenv, a) (bVaenv, b) =
      let inEnv = mergeAllVarEnvs typeEnv [aVaenv, bVaenv]
          (outEnv, t) = intersectTypesWithVarEnv typeEnv inEnv a b
          newBindings = H.filter (not . isTypeVar) $ H.difference outEnv inEnv
      in (outEnv, if H.null newBindings then t else substituteKeep newBindings t)
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
    -- A pairwise leaf meet can bind an external var while its leaf keeps the var pointer
    -- (e.g. @Some[/T=operator+..] ∩ Some[/T=$_/T]@ records @$_/T ↦ operator+..@ yet leaves the leaf
    -- as @Some[/T=$_/T]@, WIDER than the operand). Resolve each pair's freshly recorded bindings
    -- back into its OWN leafs so the result is the true meet — per pair (not the joined @vaenv'@),
    -- since each pair is a distinct distributed alternative with its own binding. @vaenv'@ itself is
    -- returned unchanged for the caller's downstream threading.
    resolvePairLeafs (e, ps)
      | H.null nb = ps
      | otherwise = case substituteKeep nb (UnionType Nothing (joinUnionType ps) []) of
                      UnionType Nothing leafs' _ -> splitUnionType leafs'
                      _                          -> ps
      where nb = H.difference e vaenv
    type' = joinUnionTypeByName $ fmap (concatMap resolvePairLeafs) intersected

-- |
-- Takes the intersection of two 'Type'.
-- It uses the 'TypeVarEnv' for type variable arguments and determines any possible changes to the surrounding 'TypeVarEnv'.
intersectTypesWithVarEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> (TypeVarArgEnv, Type)
intersectTypesWithVarEnv TypeEnv{teDebug=True} _ l r | trace (printf "[INTERSECT] %s ∩ %s" (show l) (show r)) False = undefined
intersectTypesWithVarEnv _ vaenv PTopType t = (vaenv, t)
intersectTypesWithVarEnv _ vaenv t PTopType = (vaenv, t)
-- ⊥ absorbs: @X ∩ ⊥ = ⊥@ for any @X@. Must precede the 'TypeVar' clause below, which would
-- otherwise intercept @$a ∩ ⊥@ and record @$a ↦ ⊥@ while returning the bare @$a@ — reading as ⊤
-- once the env is dropped (a self-difference @$a − $a = $a ∩ ¬$a@ then wrongly keeps the var).
intersectTypesWithVarEnv _ vaenv _ BottomType = (vaenv, BottomType)
intersectTypesWithVarEnv _ vaenv BottomType _ = (vaenv, BottomType)
intersectTypesWithVarEnv _ vaenv t1 t2 | t1 == t2 = (vaenv, t1)
-- TypeVar: constrain in vaenv.  Treats 'TVVar' and 'TVArg' uniformly: both look up in vaenv,
-- and an unknown key is recorded as a fresh constraint.  TVInt/TVExt is informational here:
-- a TVInt that reaches this top level was introduced as a fresh sentinel by an upstream
-- caller (e.g. a constituent's prop ref); the old code's behavior is preserved.
-- A binding equal to the var itself (a self-referential entry an unconstrained var may
-- acquire) carries no constraint: treat it as absent and bind to @t@, rather than
-- intersecting the binding with @t@ - which would re-enter this clause unchanged forever.
-- Must precede the mixed-distribute clauses: the var's constraint is the WHOLE other
-- operand; distributing @$a ∩ (top ∪ pos)@ would record @$a@ against each union ALTERNATIVE
-- separately and the env merge would MEET them ('mergeVarEnvs'), narrowing the binding to
-- @top ∩ pos@.
intersectTypesWithVarEnv typeEnv vaenv tv@(TypeVar v _) t
  | Just l <- H.lookup v vaenv, not (isSelfVar v l) =
      let i = intersectTypesEnv typeEnv vaenv l t
      in if isBottomType i then (vaenv, BottomType) else (H.insert v i vaenv, tv)
  | otherwise = (H.insert v t vaenv, tv)
  where isSelfVar var (TypeVar var' _) = var' == var
        isSelfVar _   _                = False
intersectTypesWithVarEnv typeEnv vaenv t tv@TypeVar{} = intersectTypesWithVarEnv typeEnv vaenv tv t
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
intersectTypesWithVarEnv typeEnv vaenv t1 t2@(UnionType (Just _) pos cs)
  | not (H.null pos) || not (null cs) = intersectTypesWithVarEnv typeEnv vaenv t2 t1
-- Top ∩ Top → union excluded sets (leafs and constants) and merge predicates
intersectTypesWithVarEnv typeEnv vaenv (UnionType (Just (ps1, np1, nc1)) (H.null -> True) []) (UnionType (Just (ps2, np2, nc2)) (H.null -> True) []) =
  let (vaenv', preds') = mergePredsAnd typeEnv vaenv ps1 ps2
  in (vaenv', compactType typeEnv vaenv' $ UnionType (Just (preds', unionPartialLeafs [np1, np2], L.nub $ nc1 ++ nc2)) H.empty [])
-- PosPartials with constants ∩ Top: handle constant exclusion before leafs logic
intersectTypesWithVarEnv typeEnv vaenv (UnionType Nothing posLeafs posConsts) t2@(UnionType (Just (preds, negLeafs, negConsts)) (H.null -> True) [])
  | not (null posConsts) || not (null negConsts) =
      let (vaenv', baseResult) = intersectTypesWithVarEnv typeEnv vaenv
                                   (UnionType Nothing posLeafs [])
                                   (UnionType (Just (preds, negLeafs, [])) H.empty [])
          survivingConsts = filter (\c -> constantInType typeEnv vaenv c t2) posConsts
      in case (baseResult, survivingConsts) of
           (_, []) -> (vaenv', baseResult)
           (BottomType, cs) -> (vaenv', UnionType Nothing H.empty cs)
           (UnionType Nothing ls [], cs) ->
             let finalCs = filter (\c -> not $ constantCoveredByLeafs c ls) cs
             in (vaenv', UnionType Nothing ls finalCs)
           _ -> (vaenv', baseResult)
-- PosPartials ∩ Top(negLeafs, PredsNone) when negLeafs non-empty → subtract
intersectTypesWithVarEnv typeEnv vaenv (UnionType Nothing posPartials []) (UnionType (Just (PredsNone, negPartials, [])) (H.null -> True) [])
  | not (H.null negPartials) =
      (vaenv, compactType typeEnv vaenv $ differencePartialLeafs typeEnv vaenv posPartials negPartials)
-- PosPartials ∩ Top(_, preds) when negLeafs empty → expand/filter
intersectTypesWithVarEnv typeEnv vaenv t1@(UnionType Nothing partials []) t2@(UnionType (Just (preds, negPartials, [])) (H.null -> True) _)
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
              let innerLeafs = expandTypeWithNames typeEnv vaenv (UnionType (Just (inner, H.empty, [])) H.empty []) (H.keysSet partials)
              in (vaenv, compactType typeEnv vaenv $ differencePartialLeafs typeEnv vaenv partials innerLeafs)
            -- General: pp ∩ Top(preds, ∅) = pp ∩ expand(preds within pp)
            _ ->
              let predsLeafs = expandTypeWithNames typeEnv vaenv (UnionType (Just (preds, H.empty, [])) H.empty []) (H.keysSet partials)
              in intersectTypesWithVarEnv typeEnv vaenv t1 (UnionType Nothing predsLeafs [])
-- PosPartials ∩ Top(preds, negLeafs) with both preds non-trivial and negLeafs non-empty.
-- Split the top as Top(preds, ∅) ∩ Top(PredsNone, negLeafs) and intersect in sequence:
-- (pos ∩ U_preds) - negLeafs. The first step routes to the negLeafs-empty clause above and the
-- second to the PredsNone-subtract clause, both of which yield positive-partial forms, so this
-- terminates. (Without this clause the pair falls through to the unhandled-case error below.)
intersectTypesWithVarEnv typeEnv vaenv t1@(UnionType Nothing _ []) (UnionType (Just (preds, negPartials, ncs)) (H.null -> True) [])
  | not (H.null negPartials) && preds /= PredsNone =
      let (vaenv', inner) = intersectTypesWithVarEnv typeEnv vaenv t1 (UnionType (Just (preds, H.empty, ncs)) H.empty [])
      in intersectTypesWithVarEnv typeEnv vaenv' inner (UnionType (Just (PredsNone, negPartials, [])) H.empty [])
-- Top ∩ PosPartials → flip
intersectTypesWithVarEnv typeEnv vaenv t1@(UnionType (Just _) (H.null -> True) _) t2@(UnionType Nothing _ _) =
  intersectTypesWithVarEnv typeEnv vaenv t2 t1
-- PosPartials ∩ PosPartials → intersect leafs and constants
intersectTypesWithVarEnv typeEnv vaenv (UnionType Nothing aPartials aConsts) (UnionType Nothing bPartials bConsts) =
  debugTrace typeEnv msg (vaenv', compactType typeEnv vaenv result)
  where
    (vaenv', partials') = intersectPartialLeafsWithVarEnv typeEnv vaenv aPartials bPartials
    -- Constants that appear in both sides, or whose parent partial is in the other side's leafs
    aConsts' = filter (\c -> c `elem` bConsts || constantCoveredByLeafs c bPartials) aConsts
    bConsts' = filter (\c -> c `elem` aConsts || constantCoveredByLeafs c aPartials) bConsts
    -- Also remove constants already covered by the intersected partial leafs
    mergedConsts = L.nub $ filter (not . (`constantCoveredByLeafs` partials')) (aConsts' ++ bConsts')
    result = UnionType Nothing partials' mergedConsts
    msg = printf "[INTERSECT] %s ∩ %s = %s" (show $ UnionType Nothing aPartials aConsts) (show $ UnionType Nothing bPartials bConsts) (show result)
intersectTypesWithVarEnv _ _ t1 t2 = error $ printf "intersectTypesWithVarEnv: unhandled case %s ∩ %s" (show t1) (show t2)

-- | 'intersectTypesWithVarEnv' with the returned env DISCARDED: a meet against a bare
-- external variable keeps the variable in the type while its binding lives only in that env
-- (the mergeVarEnvVal doctrine), so the result here can read WIDER than the true meet
-- (@∀C[$_v] ∩ ∀C[Integer]@ reads as @∀C[$_v]@). That is deliberate: many stored types keep
-- vars symbolic so cross-alternative joins and later refinement still work (eagerly
-- substituting here widened a call-site lambda's param, 'FnInputBound'). A caller that needs
-- the standalone (resolved) reading must fold the returned bindings back itself, as
-- 'TypeCheck.TypeUtils.arrowConstrainUbs' and 'updateTypeProp' do.
-- | The env-dropping meet. 'intersectTypesWithVarEnv' returns a var-meet as the bare VARIABLE while
-- recording the tightened binding in its returned env (@Integer ∩ $_T@ → @$_T@ with @$_T ↦ Integer@),
-- so the meet's occurrences stay linked. Dropping that env verbatim leaves the WIDER free var — a
-- recursive-tail @ConsList[/T=$_T]@ reading as Top, so a @/T=Integer@ list meets a @/T=Eq@ requirement
-- to the abstract @Eq@ (a 'verifySchemeReqLowers' widening). Resolve the meet's freshly discovered
-- bindings back into the result before discarding the env (the 'mergeVarEnvVal' resolve-meets-back
-- rule the manual call sites already apply) — but ONLY the CONCRETE ones. A binding to another
-- variable is genuine var LINKING (an @operator+[$T]@ tying its operands and result together); keeping
-- that link a var is the whole point of returning the var, and substituting it would collapse the
-- polymorphic operator. A binding to a concrete type is a true narrowing that the free var would
-- otherwise lose.
intersectTypesEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Type
intersectTypesEnv typeEnv vaenv t1 t2 =
  let (vaenv', t) = intersectTypesWithVarEnv typeEnv vaenv t1 t2
      newBindings = H.filter (not . isTypeVar) $ H.difference vaenv' vaenv
  in case t of
      _ | H.null newBindings -> t
      -- A meet returned as a bare top-level var IS that var's discovered binding — whatever
      -- the var's loc. 'substituteKeep' would keep a 'TVInt' (inside a partial that names a
      -- partial-scoped parameter), but at the RESULT's top level the var is this operation's
      -- own operand and its standalone reading would wrongly widen to ⊤.
      TypeVar v _ | Just b <- H.lookup v newBindings -> b
      _ -> substituteKeep newBindings t

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

    -- Exact difference on the arg-set intervals (see 'PtArgMode'), on mode-normalized
    -- partials ('normalizePtArgMode' — so a vacuous bound never reads as extra arg-sets):
    --   * identical or contained pos ⟹ ∅;
    --   * disjoint intervals ('argModesOverlap' fails) ⟹ pos unchanged;
    --   * neg interval covering the whole pos interval ('argModeCovers'), pos
    --     lower-bounded ⟹ only the per-arg\/var value escapes remain ('subtractElems').
    --     Lower-bounded pos only: escaping via an arg's value requires the arg PRESENT,
    --     which a lower bound guarantees; a lower-unbounded pos admits arg-sets missing
    --     the arg (vacuously inside neg), so it enumerates instead;
    --   * PARTIAL interval overlap: enumerate the pos interval's arg-sets
    --     ('ptIntervalArgKeys' — finite via the declared arg-sets) as exact partials and
    --     recurse; each exact piece is decided by the clauses above (an exact pos interval
    --     is a single point: it is covered by neg or disjoint from it, never partial).
    --   * Only an upper-unbounded pos of an UNKNOWN name (open world: arg-sets not
    --     enumerable) falls back to keeping pos unchanged.
    differencePartial :: PartialType -> PartialType -> [PartialType]
    differencePartial posRaw negRaw = go (normalizePtArgMode typeEnv posRaw) (normalizePtArgMode typeEnv negRaw)
      where
        go p1 p2 | p1 == p2 = []
        -- @p1 ⊆ p2 ⟹ p1 − p2 = ∅@. The syntactic @p1 == p2@ above only catches identical partials;
        -- this also cancels a partial contained in the neg up to variable renaming or field widening —
        -- e.g. a @Cons[/T=$_a](head=$x, tail=…)@ minus the class member @Cons[/T=$_b](head=$y, tail=…)@,
        -- whose free element\/field variables all read as ⊤, so the pos IS the member. Without this the
        -- per-field 'subtractElems' produces spurious variable-vs-variable escape leafs (@$x − $y@ never
        -- proves ∅) and the difference wrongly keeps a partial its own subtype relation calls covered.
        -- Uses the precise partial subtype (this env has 'teArgModeProjection' False).
        go p1 p2 | isSubPartialOfWithEnv typeEnv vaenv p1 p2 = []
        go p1 p2 | not (argModesOverlap p1 p2) = [p1]
        go p1 p2 | argModeCovers p2 p1, ptArgLb (ptArgMode p1) = subtractElems p1 p2
        go p1 p2 | Just kss <- ptIntervalArgKeys typeEnv p1 = concatMap ((`go` p2) . ptAtArgKeys p1) kss
        go p1 _ = [p1]

    -- The pos instances escaping neg on some per-arg\/var type: for each neg arg\/var, the
    -- pos partial with that element narrowed to its difference from neg's. A ⊤-valued neg
    -- element excludes nothing; a neg arg that pos does not constrain is skipped.
    subtractElems :: PartialType -> PartialType -> [PartialType]
    subtractElems p1 p2@PartialType{ptArgs=negArgs, ptVars=negVars} =
      mapMaybe subtractArg (H.toList negArgs) ++ mapMaybe subtractVar (H.toList negVars)
      where
        -- A neg value subtracts as the value SET it denotes: its own-scope references read
        -- through the NEG partial's slots ('resolveOwnScopeRefs' — @head=$/T@ under the
        -- neg's @/T=X@ excludes @X@, not the ambient reading of a bare @$/T@).
        negValSet = resolveOwnScopeRefs vaenv p2
        subtractArg (argName, negArgValRaw) = case H.lookup argName (ptArgs p1) of
          Nothing -> Nothing
          -- IDENTICAL linked forms on both sides (@tail=∀ConsList[/T=$/T]@) whose every
          -- reference the POS partial itself binds differ only through the two partials'
          -- own var scopes; those dimensions are exactly what 'subtractVar' subtracts, so
          -- the arg contributes no separate escape (emitting one duplicates the var escape
          -- as an unrecognizable symbolic residue). A reference the pos does NOT bind
          -- reads as ⊤ on the pos side only — a real per-arg escape — so it must subtract.
          Just posArgVal | posArgVal == negArgValRaw
                         , all (`H.member` ptVarArg p1) (S.toList (typeTVIntRefs posArgVal)) -> Nothing
          -- The pos value subtracts as the value SET it denotes: a free reference reads as
          -- ⊤ ('freeTVIntToTop' — @Cons(head=$/T)@ with no @/T@ slot escapes on @⊤ − neg@),
          -- while a DECLARED-var reference subtracts on the var slot via 'narrowPiece'.
          Just posArgVal -> case negValSet negArgValRaw of
            PTopType  -> Nothing -- a ⊤-valued neg element excludes nothing
            negArgVal -> case differenceTypeWithEnv typeEnv vaenv (freeTVIntToTop vaenv p1 posArgVal) negArgVal of
              (_, BottomType)    -> Nothing
              (outEnv, argVal')  -> narrowPiece p1{ptArgs=H.insert argName argVal' (ptArgs p1)} outEnv
        -- An arg valued by a var reference (@head=$/T@) subtracts on the partial's own var
        -- slot: the escape's narrowing comes back only as an env binding (the env-dropping
        -- wrappers return the bare var — the same resolve-meets-back rule as
        -- 'intersectTypesEnv'), so fold the discovered CONCRETE bindings onto the piece's
        -- DECLARED 'ptVars' (a name the partial does not declare is another scope's var and
        -- is dropped). A var slot narrowed to ∅ makes the escape piece uninhabited.
        narrowPiece piece outEnv
          | H.null varBindings = Just piece
          | any isBottomType (H.elems vars') = Nothing
          | otherwise = Just piece{ptVars=vars'}
          where
            newBindings = H.filter (not . isTypeVar) $ H.difference outEnv vaenv
            varBindings = H.filterWithKey (\k _ -> k `H.member` ptVars piece) $ fst $ splitVarArgEnv newBindings
            vars' = H.unionWith (intersectTypesEnv typeEnv vaenv) (ptVars piece) varBindings
        -- Mirror 'subtractArg': skip a neg type-parameter the pos partial does not itself
        -- constrain. Defaulting a missing pos var to ⊤ and subtracting (⊤ − negVar) manufactured
        -- a spurious narrowed leaf — e.g. the universe @Cons{}@ minus the class member
        -- @Cons(…, /T=$T)@ produced @Cons[/T = ⊤−$T = ∅]@, a bottom-element partial that then
        -- emptied any intersection against it (a partial-application subset @Cons(head)@ read as
        -- ⊆ its class by difference while intersect/subtype correctly read it as disjoint).
        subtractVar (_, PTopType) = Nothing
        subtractVar (varName, negVarVal) = case H.lookup varName (ptVars p1) of
          Just posVarVal -> case snd $ differenceTypeWithEnv typeEnv vaenv posVarVal negVarVal of
            BottomType -> Nothing
            varVal'    -> Just p1{ptVars=H.insert varName varVal' (ptVars p1)}
          Nothing -> Nothing

-- | Difference of two types (t1 − t2 = t1 ∩ ¬t2).
differenceTypeWithEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> (TypeVarArgEnv, Type)
differenceTypeWithEnv TypeEnv{teDebug=True} _ t1 t2 | trace (printf "[DIFF] %s - %s" (show t1) (show t2)) False = undefined
differenceTypeWithEnv typeEnv vaenv t1 t2 = intersectTypesWithVarEnv typeEnv vaenv t1 (complementTypeEnv typeEnv vaenv t2)

-- | The env-dropping difference: the same resolve-the-meet-back reading as 'intersectTypesEnv'
-- (@$a − X@ discovers @$a ↦ ¬X@, which the standalone result must carry rather than the bare var).
differenceTypeEnv :: (TypeGraph tg) => TypeEnv tg -> Type -> Type -> Type
differenceTypeEnv typeEnv t1 t2 = intersectTypesEnv typeEnv H.empty t1 (complementTypeEnv typeEnv H.empty t2)

-- | Complement of a type (¬t).
complementTypeEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
-- ¬(PosPartials leafs consts): the universe minus those leafs and constants
complementTypeEnv _ _ (UnionType Nothing leafs consts) = UnionType (Just (PredsNone, leafs, consts)) H.empty []
-- ¬PTopType = BottomType
complementTypeEnv _ _ PTopType = BottomType
-- ¬(top ∪ pos ∪ cs) = ¬top ∩ ¬(pos ∪ cs)
complementTypeEnv typeEnv vaenv (UnionType (Just topData) pos cs)
  | not (H.null pos) || not (null cs) =
      intersectTypesEnv typeEnv vaenv
        (complementTypeEnv typeEnv vaenv (UnionType (Just topData) H.empty []))
        (complementTypeEnv typeEnv vaenv (UnionType Nothing pos cs))
-- ¬(U_preds − negPartials − negConsts) = ¬U_preds ∪ negPartials ∪ negConsts
complementTypeEnv typeEnv vaenv (UnionType (Just (preds, negPartials, negConsts)) (H.null -> True) [])
  | H.null negPartials && null negConsts = notPreds
  | preds == PredsNone = UnionType Nothing negPartials negConsts
  | otherwise = unionTypesWithEnv typeEnv vaenv (UnionType Nothing negPartials negConsts) notPreds
  where
    notPreds = UnionType (Just (predsNot preds, H.empty, [])) H.empty []
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
setArgMode _ mode (UnionType (Just (PredsOne (PredRel n), negPartials, [])) (H.null -> True) []) =
  UnionType (Just (PredsOne $ PredRel n{ptArgMode=mode}, negPartials, [])) H.empty []
setArgMode _ _ t@(UnionType (Just (PredsOne PredClass{}, _, [])) (H.null -> True) []) = t
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
                         in substituteTVExt venv t


-- | Applies 'mergeVarEnvs' to many 'TypeVarEnv'
mergeAllVarEnvs :: (TypeGraph tg, Foldable f, Eq k, Hashable k) => TypeEnv tg -> f (H.HashMap k Type) -> H.HashMap k Type
mergeAllVarEnvs typeEnv = foldr (mergeVarEnvs typeEnv) H.empty

-- | Applies a 'TVApplied' reference's parameter overrides to the type its base variable
-- resolved to: each var slot named by an override is rebound on every positive leaf and on
-- every predicate partial (a class application such as @PredClass Maybe[/T=…]@). Slots a
-- leaf does not carry (e.g. the parameterless 'Nothing' member of 'Maybe') are left
-- untouched, matching class-application expansion. A still-unresolved base variable stays
-- symbolic: the overrides are kept on (or merged into) the 'TVApplied' reference.
applyTypeVarOverrides :: H.HashMap TypeVarName Type -> Type -> Type
applyTypeVarOverrides overrides t | H.null overrides = t
applyTypeVarOverrides overrides (UnionType mNeg posLeafs consts) = UnionType mNeg' posLeafs' consts
  where
    mNeg' = fmap (\(preds, nl, ncs) -> (mapTypePreds (mapTypePred overridePartial) preds, nl, ncs)) mNeg
    posLeafs' = joinUnionType $ map overridePartial $ splitUnionType posLeafs
    overridePartial p@PartialType{ptVars} = p{ptVars=H.mapWithKey (\k old -> H.lookupDefault old k overrides) ptVars}
applyTypeVarOverrides overrides (TypeVar (TVApplied b ovr) loc) = TypeVar (TVApplied b (H.union overrides ovr)) loc
applyTypeVarOverrides overrides (TypeVar b loc) = TypeVar (TVApplied b overrides) loc

-- | Looks up a 'TypeVarAux' in the 'TypeVarArgEnv', returning 'PTopType' on miss.
-- Treats 'TVVar' and 'TVArg' uniformly: both kinds default to 'PTopType' when absent,
-- matching the TVInt/TVExt semantics where an unconstrained TVExt is unknown (i.e. PTopType).
-- A 'TVApplied' reference resolves its base variable, then applies its overrides to the result.
vaenvLookup :: TypeVarArgEnv -> TypeVarAux -> Type
vaenvLookup vaenv (TVApplied b overrides) = applyTypeVarOverrides overrides (vaenvLookup vaenv b)
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
substituteTVExt vaenv (UnionType (Just (preds, negLeafs, negConsts)) posLeafs consts) =
  UnionType (Just (mapTypePreds (mapTypePred (substitutePartialKeep vaenv)) preds, negLeafs, negConsts)) posLeafs consts
substituteTVExt vaenv (TypeVar (TVApplied b overrides) loc) =
  applyTypeVarOverrides (fmap (substituteTVExt vaenv) overrides) (substituteTVExt vaenv (TypeVar b loc))
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

-- | Demotes every external ('TVExt') type-VARIABLE reference in a type to partial-local
-- ('TVInt'). Used when a type crosses a scope boundary INWARD — a point's type being written
-- into another partial's arg\/var slot ('updateTypeProp'). A @$_T@ in the written type referred
-- to the SOURCE point's scope (e.g. an instance arm's own element var, kept raw in the point's
-- union); inside the receiving partial that scope does not exist, and keeping the reference
-- external lets a like-named var of the RECEIVING scope capture it (a @pure@ arm's
-- @Cons[/T=$_T]@ stamping @/T=Integer@ onto an enclosing @operator==[$T]@ through the arg-meet
-- hoist of 'intersectPartialsBase'). Partial-local is exactly the remaining meaning: an element
-- instantiable per value, resolved only against its own partial. 'TVArg' references are left
-- untouched (pointer slots have their own machinery), as is everything under a 'Just' top's
-- predicates (a recursive @PredClass ConsList[/T=$_T]@ stays linked — see 'substitutePartialKeep').
demoteTVExtVars :: Type -> Type
demoteTVExtVars (TypeVar (TVVar n) TVExt) = TypeVar (TVVar n) TVInt
demoteTVExtVars (UnionType Nothing partials consts) =
  UnionType Nothing (joinUnionType $ map demotePartialTVExtVars $ splitUnionType partials) consts
demoteTVExtVars t = t

demotePartialTVExtVars :: PartialType -> PartialType
demotePartialTVExtVars partial@PartialType{ptVars, ptArgs} = partial{
    ptVars = fmap demoteTVExtVars ptVars,
    ptArgs = fmap demoteTVExtVars ptArgs
    }

-- | The names of all external ('TVExt') type-VARIABLE references occurring anywhere in a type —
-- var\/arg slot values of every positive and negative leaf, and predicate partials. Used by
-- 'intersectPartialsBase' to decide which arg-meet discoveries genuinely belong to the
-- enclosing partial's scope.
typeTVExtVarRefs :: Type -> S.HashSet TypeVarName
typeTVExtVarRefs (TypeVar (TVVar n) TVExt) = S.singleton n
typeTVExtVarRefs (TypeVar _ _) = S.empty
typeTVExtVarRefs PTopType = S.empty
typeTVExtVarRefs (UnionType mtop leafs _) = S.unions $ topRefs ++ map partialRefs (splitUnionType leafs)
  where
    topRefs = case mtop of
      Just (preds, negLeafs, _) -> predsRefs preds : map partialRefs (splitUnionType negLeafs)
      Nothing                -> []
    predsRefs (PredsOne p')  = predRefs p'
    predsRefs (PredsAnd ps') = S.unions (map predsRefs ps')
    predsRefs (PredsNot p')  = predsRefs p'
    predRefs (PredClass c) = partialRefs c
    predRefs (PredRel r)   = partialRefs r
    predRefs (PredExpr e)  = partialRefs e
    partialRefs PartialType{ptVars=vs, ptArgs=as, ptPreds=ps} =
      S.unions $ predsRefs ps : map typeTVExtVarRefs (H.elems vs ++ H.elems as)

-- | See 'substitutePartialKeep': substitutes only concrete external bindings, keeping every other
-- var unchanged.
substituteKeep :: TypeVarArgEnv -> Type -> Type
substituteKeep vaenv (TypeVar (TVApplied b overrides) loc) =
  applyTypeVarOverrides (fmap (substituteKeep vaenv) overrides) (substituteKeep vaenv (TypeVar b loc))
substituteKeep vaenv t@(TypeVar v TVExt) = case H.lookup v vaenv of
  Just b | not (isTop b) -> b
  _                      -> t
  where
    isTop PTopType = True
    isTop _        = False
substituteKeep _     t@(TypeVar _ TVInt) = t
substituteKeep vaenv (UnionType Nothing partials []) =
  UnionType Nothing (joinUnionType $ map (substitutePartialKeep vaenv) $ splitUnionType partials) []
substituteKeep vaenv (UnionType (Just (preds, negLeafs, negConsts)) posLeafs consts) =
  UnionType (Just (mapTypePreds (mapTypePred (substitutePartialKeep vaenv)) preds, negLeafs, negConsts)) posLeafs consts
substituteKeep _     t                   = t

-- | Resolve a recursive value's free element vars (@$_T@) throughout a type, both:
--
--   * SELF-references — a recursive field's @$_T@ naming the partial's OWN @/T@ param (the
--     'resolveSelfRefs' case: @tail -> ConsList[/T=$_T]@), resolved through that partial's
--     @ptVars@; and
--   * AMBIENT references — a top-level raw @Cons[/T=$_T]@ the body constructs, whose @$_T@ names
--     the enclosing DEFINITION's own @/T@ (bound concretely in @ambient@ — @sorted@'s @/T=Ord@).
--
-- Unlike 'substituteKeep' / 'substituteTVExt' this ALSO descends into the positive leaves of a
-- mixed 'Just'-top union — a partially-unrolled recursive tail
-- @UnionType(Just(ConsList[/T=$_T]), Cons[/T=$_T](..))@ carries the free element in BOTH the
-- predicate top and the enumerated leaf, and leaving the leaf unresolved keeps it reading as Top.
-- A var absent from both the partial's @ptVars@ and @ambient@ stays symbolic (a genuinely generic
-- element), so a generic scope is untouched.
--
-- Needed on an Arrow result before intersecting/checking it: an unresolved recursive tail reads
-- as Top, so the meet of two otherwise-concrete @Cons[/T=Ord]@ leaves re-derives the outer element
-- from it and collapses @/T@ to Top (a 'verifySchemeActLowers' widening), and the result fails the
-- declaration's requirement check. Leaves with no @/T@ param (raw applied forms kept for eval
-- dispatch) resolve nothing.
resolveTypeSelfRefs :: TypeVarArgEnv -> Type -> Type
resolveTypeSelfRefs = go
  where
    go env (UnionType mtop pos consts) =
      UnionType (fmap (goTop env) mtop) (joinUnionType $ map (goLeaf env) $ splitUnionType pos) consts
    go env (TypeVar (TVApplied b overrides) loc) =
      applyTypeVarOverrides (fmap (go env) overrides) (go env (TypeVar b loc))
    go env t@(TypeVar v TVExt) = case H.lookup v env of
      Just b | not (isTopT b) -> b
      _                       -> t
    go _   t                   = t
    goTop env (preds, negLeafs, ncs) =
      (mapTypePreds (mapTypePred (goLeaf env)) preds, joinUnionType $ map (goLeaf env) $ splitUnionType negLeafs, ncs)
    goLeaf env p@PartialType{ptVars, ptArgs, ptPreds} = p{
        ptVars  = fmap (go env) ptVars,
        ptArgs  = fmap (go innerEnv) ptArgs,
        ptPreds = mapTypePreds (mapTypePred (goLeaf innerEnv)) ptPreds
      }
      where
        -- Field vars resolve against the ambient env EXTENDED with this partial's own (resolved)
        -- @ptVars@ — a concrete own slot wins over ambient, a still-symbolic own slot defers to it.
        ownVars  = joinVarArgEnv (fmap (go env) ptVars) H.empty
        innerEnv = H.unionWith (\o a -> if isResolvable o then o else a) ownVars env
    isTopT PTopType = True
    isTopT _        = False
    isResolvable (TypeVar _ _) = False
    isResolvable PTopType      = False
    isResolvable _             = True

-- | Relate an arrow's declared result to the type variables the source bound. A method whose
-- result is a class parameterized by the RECEIVER's own type — @:$T.eq[$T -> Eq] -> Eqv[$T]@ — loses
-- that link at desugar: a bare @Eqv[$T]@ becomes @Eqv[/T=Top]@ (the var reads as the parameter NAME,
-- not a value bound to it), so 'arrowDestType's 'substituteTVExt' finds no @$/T@ occurrence to fill
-- and the element stays Top. Re-link each Top class-parameter slot whose NAME is a type variable the
-- source bound ('vaenv') to that binding — @Eqv[/T=Top]@ under @/T ↦ Integer@ becomes
-- @Eqv[/T=Integer]@, carrying the element the receiver determines. A slot with no matching bound var
-- stays Top (a genuinely open parameter), and a non-Top slot is left to 'substituteTVExt' (it already
-- holds its value or a resolvable var).
relateArrowVars :: TypeVarArgEnv -> Type -> Type
relateArrowVars vaenv = go
  where
    go (UnionType mtop pos consts) =
      UnionType (fmap goTop mtop) (joinUnionType $ map goLeaf $ splitUnionType pos) consts
    go t = t
    goTop (preds, negLeafs, ncs) =
      (mapTypePreds (mapTypePred goLeaf) preds, joinUnionType $ map goLeaf $ splitUnionType negLeafs, ncs)
    goLeaf p@PartialType{ptVars, ptArgs, ptPreds} = p{
        ptVars  = H.mapWithKey relateVar ptVars,
        ptArgs  = fmap go ptArgs,
        ptPreds = mapTypePreds (mapTypePred goLeaf) ptPreds
      }
    relateVar k PTopType = case H.lookup (TVVar k) vaenv of
      Just b | b /= PTopType -> b
      _                      -> PTopType
    relateVar _ v = go v

-- | Replaces 'TVExt' 'TVVar' references in a 'Type' using the provided 'TypeVarEnv'.
-- Thin wrapper over 'substituteTVExt'.
substituteVarsWithVarEnv :: TypeVarEnv -> Type -> Type
substituteVarsWithVarEnv venv = substituteTVExt (joinVarArgEnv venv H.empty)

typeGetAux :: TypeVarAux -> PartialType -> Maybe Type
typeGetAux (TVVar v) p = Just $ H.lookupDefault PTopType v $ ptVars p
typeGetAux (TVArg v) p = typeGetArg v p
typeGetAux (TVApplied b overrides) p = applyTypeVarOverrides overrides <$> typeGetAux b p

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

typeSetAux :: TypeVarAux -> Type -> PartialType -> PartialType
typeSetAux (TVVar k) v p@PartialType{ptVars} = p{ptVars=H.insert k v ptVars}
typeSetAux (TVArg k) v p@PartialType{ptArgs} = p{ptArgs=H.insert k v ptArgs}
typeSetAux (TVApplied k _) _ _ = error $ printf "Unexpected TVApplied %s used as a position in typeSetAux" (show k)

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
      let rawSlot (TVVar k) p   = H.lookup k (ptVars p)
          rawSlot (TVArg k) p   = H.lookup k (ptArgs p)
          rawSlot TVApplied{} _ = Nothing
      -- The value STORED into the slot is demoted ('demoteTVExtVars'): the sub's type crosses
      -- into the sup partial's scope, where its external var references would be captured by
      -- like-named sup vars. The value RETURNED to the sub point keeps its own scope's refs.
      -- The slot's partial-internal var, whether stored directly or read through the pointer.
      let slotOwnVar sup = case (rawSlot propName sup, typeGetAux propName sup) of
            (Just (TypeVar (TVVar v) TVInt), _) -> Just v
            (_, Just (TypeVar (TVVar v) TVInt)) -> Just v
            _                                   -> Nothing
      let intersectedPartials sup@PartialType{ptVars=supVars} sub = case (slotOwnVar sup, typeGetAux propName sup) of
            (Just v, _) -> do
              let supVar = H.lookupDefault PTopType v supVars
              let newProp = intersectTypesEnv typeEnv vaenv supVar sub
              Just (sup{ptVars=H.insert v (demoteTVExtVars newProp) supVars}, newProp)
            (_, Just supProp) -> do
              -- Same-class predicate intersection ('mergePredsAnd') unifies e.g.
              -- @Functor[/T=$_A] ∧ Functor[/T=Integer]@ into @Functor[/T=$_A]@ while recording the
              -- tighter binding @$_A ↦ Integer@ in the returned env. 'intersectTypesEnv' drops that
              -- env, leaving the wider @Functor[/T=$_A]@ — a result NOT ⊆ either operand, which
              -- re-widens the sub point a later epoch and breaks monotonicity ('verifySchemeActLowers').
              -- Resolve the freshly recorded bindings back into the result so it is the true (concrete)
              -- meet, mirroring 'intersectTypesWithVarEnv's own mixed-union recombination.
              -- The meet can drop a var the SUB carries but the sup does not (@sub@'s fmap element
              -- param @/A=$_T@ against a @sup@ slot that only lists @/T@) to Top, especially at a
              -- deeper recursion level. That widens the sub point's requirement a later epoch and
              -- breaks monotonicity ('verifySchemeReqLowers'). Restore those lost external
              -- ('TVExt') vars from the sub, as 'updateTypeProp' already does for the super side.
              let (ivaenv, newProp0) = restoreLostTVExtVars sub <$> intersectTypesWithVarEnv typeEnv vaenv supProp sub
                  newBindings = H.difference ivaenv vaenv
                  newProp | H.null newBindings = newProp0
                          | otherwise          = substituteKeep newBindings newProp0
              if isBottomType newProp
                then Nothing
                else Just (typeSetAux propName (demoteTVExtVars newProp) sup, newProp)
            (_, Nothing) -> Nothing
      case subType of
        UnionType Nothing subPartials [] -> do
          let subPartialList = splitUnionType subPartials
          let (supPartialList', subPartialList') = unzip $ catMaybes $ [intersectedPartials sup (singletonType sub) | sup <- supPartialList, sub <- subPartialList]
          (vaenv, compactType typeEnv vaenv $ UnionType Nothing (joinUnionType supPartialList') [], unionAllTypes typeEnv subPartialList')
        TypeVar v _ -> do
          -- The meet stored as the var's refined binding must be its STANDALONE reading: the
          -- intersection records a met external var in its returned env while the type keeps
          -- the var (@∀ConsList[Ord] ∩ ∀ConsList[$_T]@ reads as @∀ConsList[$_T]@ alone — WIDER
          -- than the existing binding, a monotonicity violation when this constraint's scope
          -- does not bind @$_T@). Resolve the meet's fresh discoveries back in, mirroring the
          -- prop intersection above ('intersectedPartials').
          let (ivaenv, tp0) = intersectTypesWithVarEnv typeEnv vaenv (H.lookupDefault PTopType v vaenv) (unionAllTypesWithEnv typeEnv vaenv $ mapMaybe (typeGetAux propName) supPartialList)
          let tpBindings = H.difference ivaenv vaenv
          let tp' = if H.null tpBindings then tp0 else substituteKeep tpBindings tp0
          let vaenv' = H.insert v tp' vaenv
          let tp'' = substituteTVExt vaenv' tp'
          let updateSuperPartial p@PartialType{ptVars=pVars} = case (rawSlot propName p, typeGetAux propName p) of
                -- Slot is an internal pointer: maintain the partial's own var, keep the pointer.
                (Just (TypeVar (TVVar vn) TVInt), _) ->
                  Just p{ptVars=H.insert vn (intersectTypes typeEnv (H.lookupDefault PTopType vn pVars) tp'') pVars}
                (_, Nothing) -> Nothing
                (_, Just supTp) | supTp == subType -> Just $ typeSetAux propName subType p
                -- An unconstrained ARG slot whose sub is an EXTERNAL var naming one of this
                -- partial's OWN vars (a declaration's @fn(v -> $_A) -> $_B@ arg under an
                -- object declaring @$B@): initialize the slot as the partial-internal
                -- pointer - demoting the external reference into the partial's own scope - so the declared
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
