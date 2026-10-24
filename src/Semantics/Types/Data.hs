-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.Data
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- The data model of Catln types: 'PartialType', 'Type', 'TypePredicates', type variables and
-- their locations, the 'TypeGraph' class and the 'TypeEnv' a type operation runs in, together with
-- their hashing, forcing and printing.
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns    #-}

module Semantics.Types.Data where

import           CtConstants
import           Data.Aeson
import           Data.Graph           (graphFromEdges)
import           Data.Hashable
import qualified Data.HashMap.Strict  as H
import qualified Data.HashSet         as S
import           Data.List            (foldl', intercalate)
import qualified Data.List            as L
import           Data.List.Split      (splitOn)
import           Data.Maybe
import           Data.Proxy           (Proxy (..))
import           GHC.Generics
import           Semantics.CtType
import           Semantics.Types.Name
import           Utils


type ArgName = PartialKey

type TypeVarName = PartialKey

-- | The name or tuple-type of a 'PartialType'.
-- This would be the name of the function, data type, or class.
-- Names which are PTypeName, PClassName refer to a particular type or class respectively
-- The PRelativeName refers to a suffix of some number of types or classes (or possibly both).
data PartialName
  = PTypeName { ptnName :: TypeName }
  | PClassName { pcnName :: ClassName }
  | PRelativeName { prnName :: RelativeName }
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, ToJSONKey)

-- | Which kind of name a partial is named by, read as a Catln type.
--
-- The arms are written in record syntax because a Catln argument is named and a positional
-- constructor's are not; the field names are all the reading needs.
instance CtType PartialName where
  ctDoc _ = [
    "What a partial type is named by: a type, a class, or a suffix that could be either.",
    "A [PTypeName] names one declared type and a [PClassName] one declared class. A [PRelativeName] is a name written without its full path — the compiler resolves it to one of the other two, so a program reading a compiled one will not meet it."
            ]

-- |
-- Describes how the listed args of a 'PartialType' bound the args of a matching
-- instance, as two independent booleans (a bounded lower\/upper end of the
-- arg-name set, treated as an interval in the subset lattice):
--
--   * 'ptArgLb' (lower bounded): every listed arg is required (instance args ⊇ listed), except
--     the optional ones ('ptArgOpt').
--   * 'ptArgUb' (upper bounded): no unlisted args are allowed (instance args ⊆ listed).
--
-- The four combinations: (T,T) exactly the listed args; (T,F) listed required,
-- extras allowed; (F,T) any subset of listed, no extras; (F,F) unconstrained.
--
-- Every listed arg is typed where it is present. 'ptArgOpt' names listed args the lower bound
-- does not require: an instance may lack one, and where it has one its value is the listed type.
-- Without it a lower-bounded, upper-unbounded partial could not say "@io@ is required, and
-- @counter@, where present, is a @Counter@" — listing @counter@ required it, and leaving it
-- unlisted admitted it with any value, though that is what a meet of @F(io=IO)..@ with a partial
-- typing an optional @counter@ is, and what @F(io=IO)..@ less @F(io=IO, counter=¬Counter)..@ is.
-- An optional arg typed ∅ is one an instance does not have.
--
-- It is kept canonical: only listed args, and empty unless the lower bound is set (without a
-- lower bound every listed arg is optional already).
data PtArgMode = PtArgMode {
  ptArgLb  :: Bool,
  ptArgUb  :: Bool,
  ptArgOpt :: S.HashSet ArgName
  } deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- | How a partial's listed arguments bound a matching instance's, read as a Catln type. Already a
-- record, so its whole instance is what it says.
instance CtType PtArgMode where
  ctDoc _ = [
    "How the arguments a type lists bound the arguments of what it matches.",
    "[argLb] says every listed argument is required, and [argUb] says no unlisted one is allowed. Both together is exactly the listed arguments; neither is no bound at all. They are two independent answers rather than four named cases, because that is what they are: an interval in the lattice of argument sets.",
    "[argOpt] lists the arguments [argLb] does not require: a value may lack one, and where it has one it is of the type listed."
            ]

-- | Matches only the exact args (lower and upper bounded).
pattern PtArgExact :: PtArgMode
pattern PtArgExact <- PtArgMode True True (S.null -> True) where
  PtArgExact = PtArgMode True True S.empty

-- | Matches with any additional args (lower bounded, upper unbounded): the written spread
-- @X..@. The spread lifts only the UPPER bound. For a DATA type the lower bound is what its
-- definition declares, read in by 'normalizePtArgMode': @Plain..@ over @data(Plain(name, other))@
-- requires @name@ and @other@ and admits more.
pattern PtArgAny :: PtArgMode
pattern PtArgAny <- PtArgMode True False (S.null -> True) where
  PtArgAny = PtArgMode True False S.empty

-- | Every arg-set of the name (lower unbounded, upper unbounded): the argument-less value,
-- every partial application and every full one. The algebra's OWN "any arguments" — a name's
-- relation ('relTypeAppliedVal'), a name-only bound relaxed for an argumented value
-- ('relaxNameBoundArgs'), ⊤ materialized as every name ('topTypeAsPartials'). Not a spelling
-- the language writes, so it takes no declared floor.
pattern PtArgUnbounded :: PtArgMode
pattern PtArgUnbounded <- PtArgMode False False _ where
  PtArgUnbounded = PtArgMode False False S.empty

-- | A predicate applied to a type
data TypePredicate
  = PredExpr { peType :: PartialType }
  | PredClass { pcType :: PartialType } -- A hasClass predicate
  | PredRel { prType :: PartialType } -- a isa predicate. Represented by val : classOrModuleOrType
  -- | The value IS the constant. Its use is the negation: a leaf minus the points of a constant
  -- exclusion, @/Integer − {c}@ = @/Integer|¬PredConstEq(c)@. Neither a positive union (a constant
  -- cannot be carved out of a leaf) nor a 'Just'-top (it re-adds its positive leafs whole) can
  -- otherwise spell it, so without this atom the meet @/Integer ∩ (⊤ − {c})@ could only drop the
  -- exclusion and widen to @/Integer@.
  --
  -- A stand-in. What it states is a refinement of the value, and the long-term home for it is a
  -- 'PredExpr' refinement type — @Integer__neq(-51)@, or @(Integer | $this != -51)@. A 'PredExpr'
  -- is decided on a value shape by evaluating it through the graph ('typeGraphPredExprHolds'),
  -- which cannot yet tell one constant from another (a constant is one leaf). Until it can, a
  -- predicate atom of its own is the closer substitute: its meaning is exactly "the value equals
  -- this constant", decided against constants and nothing else, and it should not grow into a
  -- refinement language.
  | PredConstEq { pceConst :: Constant }
  deriving (Eq, Ord, Show, Generic, ToJSON)

-- | One condition a type is filtered by, read as a Catln type.
instance CtType TypePredicate where
  ctDoc _ = [
    "One condition a type is filtered by, each written over the [type] it is about.",
    "A [PredExpr] says the value is that expression, a [PredClass] that it has that class, and a [PredRel] that it is one — the `isa` written `val : classOrModuleOrType`.",
    "A [PredConstEq] says the value is that constant. It is how a type says \"these values except that one\" — a leaf with the constant excluded — and stands in for a refinement written with [PredExpr] until those can be decided."
            ]

data TypePredicates
  = PredsOne { poPred :: TypePredicate }
  | PredsAnd { paPreds :: [TypePredicates] }
  | PredsNot { pnPred :: TypePredicates }
  deriving (Eq, Ord, Generic, ToJSON)

-- | The conditions a type is filtered by, read as a Catln type: one, all of several, or the
-- negation of one. There is no arm for none, which is 'PredsNone' — an empty [PredsAnd].
instance CtType TypePredicates where
  ctDoc _ = [
    "The conditions a type is filtered by: one of them, all of a list of them, or the negation of one.",
    "No condition at all is written as an empty [PredsAnd], so a type with nothing said about it and a type with everything said about it are the same shape read two ways rather than two shapes."
            ]

data PartialKey = PartialKey {
  pkName :: Name,
  pkVars :: S.HashSet PartialKey,
  pkArgs :: S.HashSet PartialKey
  } deriving (Eq, Ord, Generic, ToJSON, ToJSONKey)

-- | A key read as a Catln type, off its own shape.
--
-- A record of a name and two sets of itself: a set is read as the list of what is in it, and a
-- type reaching itself through one terminates.
--
-- Derived rather than written, unlike 'Name': there is no cached field here, and all three fields
-- are content. Its 'Hashable' hashes only the name and the two sizes, so what tells two keys apart
-- is 'Eq' over the whole record, which a value read back satisfies because nothing is dropped.
instance CtType PartialKey where
  ctDoc _ = [
    "The name of an argument or a type variable, with whatever it is itself applied to.",
    "A key is a [name] and two sets: the [vars] it is written with and the [args] it is written over. Both hold keys again, so a key is a small tree rather than a string — which is what lets `f(x)` and `f(x, y)` be told apart as names before anything is known about what they are.",
    "The sets are handed over as lists, in order, so that two keys holding the same members are handed over the same way."
            ]

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
  hashWithSalt s (PredExpr  p)   = s `hashWithSalt` (0 :: Int) `hashWithSalt` p
  hashWithSalt s (PredConstEq c) = s `hashWithSalt` (3 :: Int) `hashWithSalt` c
  hashWithSalt s (PredClass p)   = s `hashWithSalt` (1 :: Int) `hashWithSalt` p
  hashWithSalt s (PredRel   p)   = s `hashWithSalt` (2 :: Int) `hashWithSalt` p

-- |
-- A particle type describes a simple set of types.
-- It corresponds to what could be matched by a single object or data declaration.
-- The arguments are treated independently except for type variables and properties.
--
-- A type denotes a set of values, so every dimension a value has is accounted for. A partial's
-- 'ptVars' keys are its declaration's type parameters and every value of the name carries all of
-- them, so a parameter the partial does not spell is @PTopType@ there: the partial allows every
-- value along that dimension. @/Some@ and @/Some[/T=⊤]@ denote the same set, and @/Some[/T=/Nil]@
-- is a proper subset of both.
--
-- 'ptArgs' is different: an unspelled ARG is governed by 'ptArgMode', because a partial
-- application genuinely lacks the field rather than holding every value of it. The arg-set
-- interval algebra ('argModeCovers', 'ptIntervalArgKeys') decides those, and comparing arg VALUES
-- over shared keys is right once the modes agree.
data PartialType = PartialType {
  ptName    :: TypeName,
  -- | Declared type parameters. A parameter that is not spelled is ⊤ — see above.
  ptVars    :: H.HashMap TypeVarName Type,
  -- | Arguments. An unspelled arg is governed by 'ptArgMode', not read as ⊤.
  ptArgs    :: H.HashMap ArgName Type,
  ptPreds   :: TypePredicates,
  ptArgMode :: PtArgMode
  } deriving (Eq, Ord, Generic, ToJSON, ToJSONKey)

-- | One simple set of types, read as a Catln type. Already a record, so nothing had to be named.
instance CtType PartialType where
  ctDoc _ = [
    "A simple set of types: what one object or one data declaration could match.",
    "Its [name] is what it is called, its [vars] are the type parameters it is written with, its [args] are the fields it is written over, its [preds] are what it is filtered by, and its [argMode] says how those args bound what it matches.",
    "[vars] and [args] are tracked apart and matched apart. A field is not related to a type parameter by the declaration, so a value's field says nothing about a parameter of the type unless something else says it does."
            ]

-- | Over what the vars and args hold, as 'PartialArgsOption' is. predsTag breaks the
-- TypePredicates→TypePredicate→PartialType cycle.
instance Hashable PartialType where
  hashWithSalt s PartialType{ptName, ptVars, ptArgs, ptPreds, ptArgMode} =
    s `hashWithSalt` ptName
      `hashWithSalt` typeMapHash ptVars
      `hashWithSalt` typeMapHash ptArgs
      `hashWithSalt` predsTag ptPreds
      `hashWithSalt` ptArgMode

data Constant
  = CInt { ciValue :: Integer }
  | CFloat { cfValue :: Double }
  | CStr { csValue :: String }
  | CChar { chValue :: Char }
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- | A constant written in a program, read as a Catln type.
--
-- Its character arm is why @\/Data\/Char@ had to be declared: a constant is one of four things and
-- one of them is a character, so a program reading a program is handed one whether or not it could
-- have written one.
instance CtType Constant where
  ctDoc _ = [
    "A value written directly in a program: a whole number, a fractional number, a string, or a character.",
    "A character constant cannot be written in Catln — the language has no character literal — but the compiler makes one where it needs to, so a program reading a program can be handed one."
            ]

-- | The non-name properties of a 'PartialType': its own fields minus its name, named after them
-- so that the two spellings of one thing read as one thing.
data PartialArgsOption = PartialArgsOption {
  paoVars    :: H.HashMap TypeVarName Type,
  paoArgs    :: H.HashMap ArgName Type,
  paoPreds   :: TypePredicates,
  paoArgMode :: PtArgMode
  } deriving (Eq, Ord, Generic)

instance Show PartialArgsOption where
  show (PartialArgsOption vars args preds mode) = show (vars, args, preds, mode)

instance ToJSON PartialArgsOption where
  toJSON (PartialArgsOption vars args preds mode) = toJSON (vars, args, preds, mode)

-- | Over what the vars and args hold, through the hashes those types already carry ('typeHash'),
-- so it costs a step per var and arg rather than a walk of everything under them.
instance Hashable PartialArgsOption where
  hashWithSalt s (PartialArgsOption vars args preds mode) =
    s `hashWithSalt` typeMapHash vars
      `hashWithSalt` typeMapHash args
      `hashWithSalt` predsTag preds
      `hashWithSalt` mode

-- | The non-name half of a partial type, read as a Catln one.
instance CtType PartialArgsOption where
  ctDoc _ = [
    "Everything a partial type is apart from its name: the [vars] it is written with, the [args] it is written over, the [preds] it is filtered by, and how its arguments [argMode] bound what it matches.",
    "It is what a set of partials sharing one name holds, so that the name is written once and each of the ways it can be filled in is written beside it."
            ]

-- | An alternative format for many 'PartialType's which combine those that share the same name
type PartialLeafs = H.HashMap TypeName (S.HashSet PartialArgsOption)

-- |
-- The basic format of a 'Type' in Catln.
-- 'UnionType mTop posLeafs posConsts' semantics:
--   * Nothing:                          posLeafs ∪ posConsts (positive union of partials and constants)
--   * Just (preds, negLeafs, negConsts): (universe filtered by preds − negLeafs − negConsts) ∪ posLeafs ∪ posConsts
-- The 'Just' tuple is the whole subtractive component; the outer 'PartialLeafs' and '[Constant]'
-- are ALWAYS positive members of the union, so a mixed type such as "class-top ∪ positive
-- constant" (@∀Ord ∪ {c}@) is represented directly, without expanding the class-top to its
-- current members.
--
-- Each constructor also carries its hash ('typeHash'), which is how a type is written and matched
-- everywhere else without it: 'UnionType' and 'TypeVar' are the pattern synonyms below, and
-- building one through them is what computes it.
--
-- A union also carries whether a variable occurs anywhere in it ('typeMentionsVars'), worked out
-- the same way: lazily, from what its nested types already hold. A type that mentions no
-- variable reads the same under every environment, and every traversal that resolves, renames or
-- erases variables is the identity on it, so they can stop there rather than walk it to its leaves
-- and build it again.
data Type
  = UnionTypeH Int Bool (Maybe (TypePredicates, PartialLeafs, [Constant])) PartialLeafs [Constant]
  -- | A type-variable OCCURRENCE: the variable reference plus occurrence-local knowledge.
  -- The occurrence denotes @⟦var⟧ ∩ ⟦inner⟧@ — the third ('tvInner') component is an upper bound
  -- this particular occurrence is known to satisfy, carried WITH the reference so traversals
  -- never have to choose between keeping the pointer (linkage) and materializing its bound
  -- (value): out-of-scope reads erase to the inner rather than ⊤, and a frame exit can fold
  -- the binding into the inner rather than substituting the pointer away. @PTopType@ (no
  -- knowledge) reproduces the plain-variable semantics exactly. PRODUCERS of a non-⊤ inner
  -- should install only CLOSED values ('typeHasTypeVars' — the 'substituteFreeDeep'
  -- discipline); consumers do not assume it.
  | TypeVarH Int TypeVarAux TypeVarLoc Type

-- | A union: its subtractive top, its positive leafs and its positive constants (see 'Type').
pattern UnionType :: Maybe (TypePredicates, PartialLeafs, [Constant]) -> PartialLeafs -> [Constant] -> Type
pattern UnionType top leafs consts <- UnionTypeH _ _ top leafs consts
  where UnionType top leafs consts = UnionTypeH (unionTypeHash top leafs consts) (unionMentionsVars top leafs) top leafs consts

-- | A type-variable occurrence: which variable, where it is defined, and the inner it is known to
-- be within (see 'Type').
pattern TypeVar :: TypeVarAux -> TypeVarLoc -> Type -> Type
pattern TypeVar aux loc inner <- TypeVarH _ aux loc inner
  where TypeVar aux loc inner = TypeVarH (typeVarHash aux loc inner) aux loc inner

{-# COMPLETE UnionType, TypeVar #-}

-- | A type's hash, over its whole structure.
--
-- Held on the type rather than computed when asked, and lazily: it is worked out the first time
-- something hashes or compares the type, from the hashes its nested types already hold, so a type
-- costs as many steps as it has direct parts however deep it goes, and nothing is paid for a type
-- that is never hashed. The hash used to be SHALLOW instead — names and sizes, never what an arg
-- holds — which is the same cheapness bought by giving up discrimination: every list type hashed
-- alike whatever its length, so every table keyed by one (the subtype, sat, expansion and
-- reach-query memos, the evaluator's arrow cache, the option sets inside every 'PartialLeafs') kept
-- them in one bucket and told them apart with a deep '=='. A @toString@ over a sixty-element list
-- spent sixty percent of its arrow builds doing that.
typeHash :: Type -> Int
typeHash (UnionTypeH h _ _ _ _) = h
typeHash (TypeVarH h _ _ _)     = h

-- | Order-independent, as the maps and sets it reads are equal whatever order they hold their
-- entries in: each entry is hashed on its own and the entries summed.
unionTypeHash :: Maybe (TypePredicates, PartialLeafs, [Constant]) -> PartialLeafs -> [Constant] -> Int
unionTypeHash top leafs consts = (1 :: Int) `hashWithSalt` topHash top `hashWithSalt` partialLeafsHash leafs `hashWithSalt` consts
  where
    topHash Nothing                  = 0 :: Int
    topHash (Just (preds, neg, ncs)) = hash (predsTag preds, partialLeafsHash neg, ncs)

typeVarHash :: TypeVarAux -> TypeVarLoc -> Type -> Int
typeVarHash aux loc inner = (0 :: Int) `hashWithSalt` aux `hashWithSalt` loc `hashWithSalt` typeHash inner

-- | Whether a 'TypeVar' occurs anywhere in a type: in a slot value, a predicate's partial, or an
-- excluded leaf. Held on the union ('Type') rather than worked out when asked.
typeMentionsVars :: Type -> Bool
typeMentionsVars (UnionTypeH _ v _ _ _) = v
typeMentionsVars TypeVarH{}             = True

-- | 'typeMentionsVars' of a union built from these parts, from the flags its nested types hold.
unionMentionsVars :: Maybe (TypePredicates, PartialLeafs, [Constant]) -> PartialLeafs -> Bool
unionMentionsVars top leafs = leafsMentionVars leafs || topHas
  where
    topHas = case top of
      Just (preds, negLeafs, _) -> predsMentionVars preds || leafsMentionVars negLeafs
      Nothing                   -> False

leafsMentionVars :: PartialLeafs -> Bool
leafsMentionVars = any (any optMentions)
  where optMentions (PartialArgsOption vars args preds _) = any typeMentionsVars vars || any typeMentionsVars args || predsMentionVars preds

-- | Whether a 'TypeVar' occurs anywhere in a partial: its slots or its predicates.
partialMentionsVars :: PartialType -> Bool
partialMentionsVars PartialType{ptVars, ptArgs, ptPreds} = any typeMentionsVars ptVars || any typeMentionsVars ptArgs || predsMentionVars ptPreds

predsMentionVars :: TypePredicates -> Bool
predsMentionVars (PredsOne p)  = predMentionsVars p
predsMentionVars (PredsAnd ps) = any predsMentionVars ps
predsMentionVars (PredsNot p)  = predsMentionVars p

predMentionsVars :: TypePredicate -> Bool
predMentionsVars (PredClass p)   = partialMentionsVars p
predMentionsVars (PredRel p)     = partialMentionsVars p
predMentionsVars (PredExpr p)    = partialMentionsVars p
predMentionsVars (PredConstEq _) = False

-- | The hash of a set of partials sharing names, independent of the order they are held in.
partialLeafsHash :: PartialLeafs -> Int
partialLeafsHash = H.foldlWithKey' (\acc name opts -> acc + (hash name `hashWithSalt` S.foldl' (\a o -> a + hash o) (S.size opts) opts)) 0

-- | The hash of a map of types, independent of the order it holds them in.
typeMapHash :: Hashable k => H.HashMap k Type -> Int
typeMapHash = H.foldlWithKey' (\acc k t -> acc + (hash k `hashWithSalt` typeHash t)) 0

-- | Structural: the hash only decides a comparison when it differs, which is the common case and
-- the cheap one. Equal hashes still compare in full.
instance Eq Type where
  UnionTypeH h1 _ t1 l1 c1 == UnionTypeH h2 _ t2 l2 c2 = h1 == h2 && l1 == l2 && c1 == c2 && t1 == t2
  TypeVarH h1 a1 o1 i1 == TypeVarH h2 a2 o2 i2     = h1 == h2 && a1 == a2 && o1 == o2 && i1 == i2
  _ == _                                           = False

-- | The order of the fields as written, not the hash: a hash says nothing about which type comes
-- first, and an order read from it would change whenever the hash does.
instance Ord Type where
  compare (UnionType t1 l1 c1) (UnionType t2 l2 c2) = compare t1 t2 <> compare l1 l2 <> compare c1 c2
  compare UnionType{} TypeVar{}                     = LT
  compare TypeVar{} UnionType{}                     = GT
  compare (TypeVar a1 o1 i1) (TypeVar a2 o2 i2)     = compare a1 a2 <> compare o1 o2 <> compare i1 i2

-- | The representation 'Type' had before it held its hash: the two constructors under their names
-- with their three named fields each. It is what a derived instance reads — the JSON a type is
-- sent as, and the declaration a program is handed ('CtType') — so the hash, which is a cache and
-- not part of what a type is, stays out of both.
instance Generic Type where
  type Rep Type =
    D1 ('MetaData "Type" "Semantics.Types" "catln" 'False)
      (C1 ('MetaCons "UnionType" 'PrefixI 'True)
         (S1 ('MetaSel ('Just "utTop") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (Rec0 (Maybe (TypePredicates, PartialLeafs, [Constant])))
          :*: (S1 ('MetaSel ('Just "utLeafs") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (Rec0 PartialLeafs)
          :*: S1 ('MetaSel ('Just "utConsts") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (Rec0 [Constant])))
       :+: C1 ('MetaCons "TypeVar" 'PrefixI 'True)
         (S1 ('MetaSel ('Just "tvAux") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (Rec0 TypeVarAux)
          :*: (S1 ('MetaSel ('Just "tvLoc") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (Rec0 TypeVarLoc)
          :*: S1 ('MetaSel ('Just "tvInner") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (Rec0 Type))))
  from (UnionType top leafs consts) = M1 (L1 (M1 (M1 (K1 top) :*: (M1 (K1 leafs) :*: M1 (K1 consts)))))
  from (TypeVar aux loc inner)      = M1 (R1 (M1 (M1 (K1 aux) :*: (M1 (K1 loc) :*: M1 (K1 inner)))))
  to (M1 (L1 (M1 (M1 (K1 top) :*: (M1 (K1 leafs) :*: M1 (K1 consts)))))) = UnionType top leafs consts
  to (M1 (R1 (M1 (M1 (K1 aux) :*: (M1 (K1 loc) :*: M1 (K1 inner))))))   = TypeVar aux loc inner

instance ToJSON Type

instance ToJSONKey Type

-- | A set of types, read as a Catln type. The one the whole program is written in terms of.
--
-- Its subtractive half is a three-tuple, handed over as a [\/Catln\/Triple]: a way of handing over
-- what Haskell leaves anonymous without renaming it where it is used.
instance CtType Type where
  ctDoc _ = [
    "A set of types: what a value may be.",
    "A [UnionType] is the [leafs] and [consts] it holds, and — if it has a [top] — everything EXCEPT what that says: the [Triple] there is the conditions the universe is filtered by, the leafs taken out of it, and the constants taken out of it. So a type with no [top] is written by what it contains, and a type with one is written by what it does not.",
    "A [TypeVar] is a variable rather than a set: an [aux] saying which variable, a [loc] saying whether it was defined on the type it is written in or outside it, and an [inner] the occurrence is known to be within. The occurrence is the variable and the inner together, so it means everything the variable can be that is also within the inner."
            ]

-- | The hash the type holds ('typeHash').
instance Hashable Type where
  hash = typeHash
  hashWithSalt s t = hashWithSalt s (typeHash t)

-- | Force a type's STRUCTURE to normal form.
--
-- The type-graph query memo ('Semantics.objArrGraphQuery') has to publish a value rather than a
-- thunk: a thunk shared through the memo can be handed back to a query that re-enters it while it
-- is still being evaluated, and re-entering a thunk under evaluation is what GHC reports as
-- @<<loop>>@. Written out because the compiler carries no 'NFData' and the 'Hashable' instances are
-- deliberately shallow (sizes and keys only), so neither can serve.
--
-- Structure only: names and constants are left at WHNF. What has to be forced is everything that
-- could still CALL BACK into the type graph, and only the nested 'Type's do that — a name or a
-- literal computes nothing.
forceType :: Type -> ()
forceType (TypeVar aux loc inner)      = aux `seq` loc `seq` forceType inner
forceType (UnionType mTop leafs consts) =
  forceTop mTop `seq` forcePartialLeafs leafs `seq` forceSpine consts
  where
    forceTop Nothing                   = ()
    forceTop (Just (preds, negs, ncs)) =
      forceTypePredicates preds `seq` forcePartialLeafs negs `seq` forceSpine ncs

forcePartialLeafs :: PartialLeafs -> ()
forcePartialLeafs = H.foldl' (\acc opts -> acc `seq` S.foldl' forceOpt () opts) ()
  where
    forceOpt acc (PartialArgsOption vars args preds mode) =
      acc `seq` forceTypeMap vars `seq` forceTypeMap args `seq` forceTypePredicates preds `seq` mode `seq` ()

forcePartialType :: PartialType -> ()
forcePartialType (PartialType n vars args preds mode) =
  n `seq` forceTypeMap vars `seq` forceTypeMap args `seq` forceTypePredicates preds `seq` mode `seq` ()

forceTypeMap :: H.HashMap k Type -> ()
forceTypeMap = H.foldl' (\acc t -> acc `seq` forceType t) ()

forceTypePredicates :: TypePredicates -> ()
forceTypePredicates (PredsOne p)  = forceTypePredicate p
forceTypePredicates (PredsAnd ps) = foldl' (\acc x -> acc `seq` forceTypePredicates x) () ps
forceTypePredicates (PredsNot p)  = forceTypePredicates p

forceTypePredicate :: TypePredicate -> ()
forceTypePredicate (PredExpr p)    = forcePartialType p
forceTypePredicate (PredConstEq c) = c `seq` ()
forceTypePredicate (PredClass p)   = forcePartialType p
forceTypePredicate (PredRel p)     = forcePartialType p

-- | Force a list's spine, leaving its elements at WHNF.
forceSpine :: [a] -> ()
forceSpine = foldl' (\acc x -> acc `seq` x `seq` ()) ()

-- | A subtractive type with no positive members and no excluded constants: the universe filtered
-- by the predicates, minus the negative leafs.
pattern TopType :: PartialLeafs -> TypePredicates -> Type
pattern TopType negLeafs preds <- UnionType (Just (preds, negLeafs, [])) (H.null -> True) []
  where TopType negLeafs preds = UnionType (Just (preds, negLeafs, [])) H.empty []

-- | Maps a function over the negative leafs of a 'Just'-top tuple, keeping predicates and
-- excluded constants unchanged.
mapTopNegLeafs :: (PartialLeafs -> PartialLeafs) -> (TypePredicates, PartialLeafs, [Constant]) -> (TypePredicates, PartialLeafs, [Constant])
mapTopNegLeafs f (preds, negLeafs, ncs) = (preds, f negLeafs, ncs)

-- | Where a type var or arg reference finds the variable it names.
--
--   * 'TVInt' references a variable defined on the *same* 'PartialType' (resolved against its
--     'ptVars'\/'ptArgs'). Written @$T@ in source syntax.
--   * 'TVParent' references a variable of the nearest ENCLOSING partial — @$_/V@ in
--     @peq[$V -> Eqq](x -> Box[$T -> $_V])@ names @peq@'s own @/V@. It is meaningful exactly as
--     long as it sits inside that partial: reading the slot standalone must resolve it through
--     the parent's 'ptVars' ('resolveOwnScopeRefs'), and it is read as ⊤ only when genuinely
--     detached from its parent. It NEVER consults the ambient 'TypeVarArgEnv', so the ambient
--     scope rules ('dropOutOfScopeTVExt' and friends) must leave it alone.
--   * 'TVExt' references a variable of the enclosing OBJECT's scope — one introduced in the Obj
--     and used in the Arr (@unwrap(b -> Box[$T -> $_T]) -> $_T@). It resolves against the
--     ambient 'TypeVarArgEnv' and denotes ⊤ once that scope no longer binds it.
--
-- Both external forms are spelled @$_T@ in source syntax; which one a given reference IS
-- depends on the enclosing partial chain, so the distinction is assigned where that chain is
-- known and preserved by every operation afterwards — never re-derived deep in the algebra,
-- where a like-named ambient variable would be captured.
--
-- A 'TVParent' does not record WHICH parent it names: resolution is the nearest enclosing
-- partial that binds the name, which is well defined only because traversals descend from the
-- root of a type.
data TypeVarLoc
  = TVInt
  | TVParent
  | TVExt
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- | Whether a reference looks OUTSIDE the partial it sits on — at the nearest enclosing partial
-- ('TVParent') or at the ambient object scope ('TVExt'). Used by the sites that treat every
-- non-local reference alike; a site whose rule genuinely differs between the two matches the
-- constructors directly instead.
isOuterLoc :: TypeVarLoc -> Bool
isOuterLoc TVInt    = False
isOuterLoc TVParent = True
isOuterLoc TVExt    = True

-- | Matches either outward-looking location ('isOuterLoc'). A site that must distinguish the
-- parent from the ambient scope matches 'TVParent'\/'TVExt' directly; this is for the ones whose
-- rule is the same for both.
--
-- The substitution WALKERS match the constructors, because they carry a scope layer beside the
-- ambient env ('substituteTVExtScoped', 'substituteKeepScoped', 'substituteFreeDeepScoped',
-- 'resolveTypeSelfRefs'). What matches here are LEAF READERS and NAME MATCHES, and for them the
-- rule is genuinely one:
--
--   * a leaf reader ('slotFree', 'isFlexibleVar', 'flexibleVar', 'resolveFlex',
--     'flexibleSpelling', 'typeSatDNF') looks the name up in the env its CALLER hands it, and
--     the caller has already put the parent's bindings there — the compared partials'
--     ('comparedPartialsEnv') or, for a value read on its own, the object's, which is what the
--     ambient env registers (the fragment rule of 'substituteKeep'). The two locs therefore
--     resolve through the same lookup at these sites;
--   * a name match ('typeOuterVarRefs', 'cleanDeclSelfRefs', 'tightenDeclBoundedDims',
--     'restoreLostTVExtVars', 'typeVarLost', 'boundClassVarPtrs') asks "is this a reference to
--     a variable outside this position", which both outer locs are.
pattern TVOuter :: TypeVarLoc
pattern TVOuter <- (isOuterLoc -> True)

-- Matching the local case beside the outward one covers every location, so a function that
-- splits on 'TVInt' vs 'TVOuter' is total.
{-# COMPLETE TVInt, TVOuter #-}

-- | An outer reference ('TVOuter'), whatever it refers to (a variable, an argument link, a
-- re-applied variable): the reference itself. The name matches that ask "does this name
-- something outside its position" go through here.
outerVarRef :: Type -> Maybe TypeVarAux
outerVarRef (TypeVar v TVOuter _) = Just v
outerVarRef _                     = Nothing

-- | An outer reference to a named type VARIABLE: its name. 'Nothing' for a local reference, an
-- argument link, a re-applied variable, or a non-variable.
outerVarName :: Type -> Maybe TypeVarName
outerVarName t = case outerVarRef t of
  Just (TVVar n) -> Just n
  _              -> Nothing

-- | What an env says about an outer variable reference. The leaf readers ('slotFree',
-- 'isFlexibleVar', 'flexibleVar', 'resolveFlex', 'flexibleSpelling') classify the lookup the
-- same way and differ only in what they make of each case; each states its rule as a case list
-- over this type, so the rules can be compared side by side.
data OuterBinding
  = Unbound       -- ^ the env does not bind the name
  | SelfLink Type -- ^ bound to its own spelling (a self-entry), with that occurrence's inner
  | BoundTop      -- ^ bound to ⊤
  | BoundVar Type -- ^ bound to another reference (a different variable, an argument link, a re-application)
  | Bound Type    -- ^ bound to a concrete type

outerBinding :: TypeVarArgEnv -> TypeVarAux -> OuterBinding
outerBinding vaenv v = case H.lookup (extKey v) vaenv of
  Nothing                          -> Unbound
  Just PTopType                    -> BoundTop
  Just b@(TypeVar v' _ i) | v' == v   -> SelfLink i
                          | otherwise -> BoundVar b
  Just b                           -> Bound b

-- | Where a type variable is defined, read as a Catln type. Three constructors carrying nothing, so
-- the instance is what it says about itself.
instance CtType TypeVarLoc where
  ctDoc _ = [
    "Where a type variable was defined: on the type it is written in ([TVInt], written `$T`), on the nearest enclosing type ([TVParent]), or on the ambient object scope ([TVExt]). The latter two are both written `$_T`.",
    "The difference is what the variable is resolved against, so it is part of what the variable IS rather than a note about it."
            ]

-- | A type because two kinds of type variables are supported
data TypeVarAux
  = TVVar { tvvName :: TypeVarName } -- ^ A type variable stored from the 'PartialType' ptVars
  | TVArg { tvaName :: ArgName } -- ^ A type variable referring to an arguments type
  | TVApplied { tvpBase :: TypeVarAux, tvpVars :: !(H.HashMap TypeVarName Type) } -- ^ A type variable re-applied with new parameter bindings (@$F[$T -> $_B]@): whatever the base variable resolves to, with the named parameter slots rebound ('applyTypeVarOverrides')
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, ToJSONKey)

-- | Which type variable a 'TypeVar' is, read as a Catln type.
instance CtType TypeVarAux where
  ctDoc _ = [
    "Which type variable a variable type is: one written on the type ([TVVar], by [name]), one standing for an argument's type ([TVArg], by the argument's [name]), or one re-applied with new parameters ([TVApplied], the [base] it was and the [vars] rebound on it).",
    "A [TVApplied] holds a variable rather than a type, so it is the one arm that reaches this type again."
            ]

-- | Whether the typeclass can be extended or not
type Sealed = Bool

-- |
-- The classGraph contains the possible classes and how they relate to existing types.
-- Each edge goes from a class to a constituent class/type.
-- Note that the graph form is an approximation because it fails to account for type variables, so using graph operations can be a fast sanity test but not a substitute for the appropriate type functions.
-- The keys in the graph are PartialName because the originally inserted values (before desugaring) may use RelativeNames
-- TODO: ClassGraph should be more granular. Can have class to only a certain object or based on type variables.
newtype ClassGraph = ClassGraph (GraphData CGNode PartialName)

-- | How the classes and types of a program relate, read as a Catln type.
--
-- Written by hand and not read off the shape, because there is no shape to read. A 'GraphData' is a
-- tuple of a graph and two FUNCTIONS — one from a vertex to what is there, one from a name to a
-- vertex — so what it holds cannot be handed over as it is stored. What it IS, though, is a list of
-- what each name holds and which names it points at, which is what 'Utils.graphToNodes' answers and
-- what 'Utils.graphFromEdges' builds one back from. So that list is what a program is given.
instance CtType ClassGraph where
  ctPath _ = ctListPath
  ctTypeRef _ = ctElemTypeRef ctListPath (Proxy :: Proxy (CGNode, PartialName, [PartialName]))
  ctImports _ = [coreListPage]
  ctPageInto _ = ctPageInto (Proxy :: Proxy (CGNode, PartialName, [PartialName]))
               . ctPageImport (Proxy :: Proxy ClassGraph)
  toCt (ClassGraph g) = toCt (L.sortOn snd3 $ graphToNodes g)
  fromCt v = ClassGraph . graphFromEdges <$> fromCt v

class TypeGraph tg where
  -- | Queries the type graph for possible conversions from a partial type to a type. Each result
  -- carries the reason for the conversion, the portion of the source partial the arrow accepts
  -- (its object intersected with the source — the whole source when the arrow covers it), and
  -- the destination type.
  typeGraphQueryWithReason :: TypeEnv tg -> TypeVarArgEnv -> PartialType -> [(String, Type, Type)]

  -- | Like 'typeGraphQueryWithReason' but WITHOUT the most-specific-promise pruning: every
  -- arrow that accepts the source contributes. The pruning is a destination-side reaches
  -- optimization (drop an abstract declaration's class-top result when a concrete definition
  -- also triggers); the REQUIREMENT side instead unions @potentialSrc@ over ALL accepting
  -- arrows — a dominated (wider-input) arrow still bounds what the source may be, and pruning
  -- against a sibling whose object presentation is still transiently ⊤ mid-solve would
  -- collapse the requirement to the one settled arm (e.g. @length@'s @Nil@ arm dominating a
  -- not-yet-presented @Cons@ arm).
  typeGraphQueryAllWithReason :: TypeEnv tg -> TypeVarArgEnv -> PartialType -> [(String, Type, Type)]
  typeGraphQueryAllWithReason = typeGraphQueryWithReason

  -- | Whether the value predicate @pred@ (a 'PredExpr' partial, @$this@ standing for the value)
  -- holds of every value of the shape @val@: @Just True@ when, evaluated on the shape, it reaches
  -- only @True@; @Just False@ when it reaches only something else; 'Nothing' when the graph cannot
  -- follow it to the end (a primitive it bottoms out at, a shape some arm leaves open). The
  -- predicate is read as the reach reads a call: the arrows accepting it, followed to the end
  -- ('Semantics.TypeGraph.completePartials'), so the answer is what evaluation would give for
  -- every value of the shape. Undecided is the default: a graph with no arrows decides nothing.
  typeGraphPredExprHolds :: TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> Maybe Bool
  typeGraphPredExprHolds _ _ _ _ = Nothing

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

  -- | The declared RESULT of each higher-order argument of a name's declarations
  -- (@fn(v -> $_A) -> $_B@ gives @(fn(v), $_B)@). A higher-order argument's slot holds its input
  -- object; its result is the arrow it adds to the environment, which a definition reads as its
  -- declaration gives it — the declaration takes precedence, as it does over the object.
  typeGraphDeclaredArgResults :: TypeEnv tg -> TypeName -> [(ArgName, Type)]

  -- | What the one declaration applying to a call binds its type variables to from the call
  -- (@mmap[$M](m -> $M, ..)@ binds @/M@ to the call's @m@), or nothing when no declaration or
  -- several apply. A definition reads its declaration's variables as the declaration gives them:
  -- @mmap(m= Some(value), fn(v))= fn(v= value)@ returns its declaration's @$M@, which only the
  -- declaration ties to @m@.
  typeGraphDeclaredVarBindings :: TypeEnv tg -> TypeVarArgEnv -> PartialType -> H.HashMap TypeVarAux Type
  typeGraphDeclaredVarBindings _ _ _ = H.empty

  -- | The declared arg-name sets for a type name, one keyset per object declaration (data
  -- AND function objects, unlike 'typeGraphData'). An instance's args are always a subset
  -- of some declared arg-set (partial application never adds args), so these bound the
  -- realizable arg-sets of a moded partial (see 'normalizePtArgMode'). 'Nothing' when the
  -- name has no declaration in the graph: its realizable arg-sets are then unknown (open
  -- world) and no arg-mode reasoning may assume a bound.
  typeGraphArgKeys :: TypeEnv tg -> TypeName -> Maybe [S.HashSet ArgName]
  typeGraphArgKeys _ _ = Nothing

  -- | Whether a name's declaration admits args beyond the ones it lists (it ends in a spread, as
  -- @data((.._))@ and @data(/Context[$V](value -> $V, .._))@ do). 'typeGraphArgKeys' answers
  -- 'Nothing' for such a name as for an unknown one, and only this tells the two apart: an open
  -- name is bounded by no arg-sets at all, while an unknown one may still be read by the arg-sets
  -- its patterns bind ('declaredArgKeys').
  typeGraphNameOpen :: TypeEnv tg -> TypeName -> Bool
  typeGraphNameOpen _ _ = False

  -- | The input-type templates of a name's ARROW objects — the function-side complement of
  -- 'typeGraphData'. A declaration's template carries the declared linkage between its args
  -- and its type variables (@compareTo[$T -> Ord](this -> $T, other -> $T)@ ties both @this@
  -- and @other@ to @$T@), which 'varStampSupported' consults to withhold a var-binding stamp
  -- while a declared occurrence of the var has not been applied yet. Empty when the name has
  -- no arrow objects (or their input presentations are still unresolved) — no withholding is
  -- then possible from this source, which is the monotone-safe direction.
  typeGraphArrowTemplates :: TypeEnv tg -> TypeName -> [PartialType]
  typeGraphArrowTemplates _ _ = []

  -- | Every type name that appears EMBEDDED (as a positive leaf) inside some arrow object's
  -- input argument spelling — the set of forms some arrow's input can accept UNSETTLED.
  -- Deferring a rewritable form is meaningful only while some arrow can still act on the
  -- deferred form; a name absent from this set has no input arrow anywhere, so once its
  -- value leaves its own rewrite frontier nothing can ever consume the unsettled form and
  -- it might as well settle through its direct arrows now.
  typeGraphInputEmbeddedNames :: TypeEnv tg -> S.HashSet TypeName
  typeGraphInputEmbeddedNames _ = S.empty

  -- | The environment with arrows added for the lambdas of one call: each is given as its input
  -- object (@fn(/v=..)@, the pattern a higher-order slot holds) and the result its body gives,
  -- and is in scope for that call alone, as 'TreeBuild.withArgArrows' adds a call's lambdas. A
  -- graph that holds no arrows of its own has none to add them beside.
  typeGraphWithArgArrows :: TypeEnv tg -> [(Type, Type)] -> TypeEnv tg
  typeGraphWithArgArrows env _ = env

data EmptyTypeGraph = EmptyTypeGraph

instance Semigroup EmptyTypeGraph where
  _ <> _ = EmptyTypeGraph

instance TypeGraph EmptyTypeGraph where
  typeGraphQueryWithReason _ _ _ = []
  typeGraphData _ _ = []
  typeGraphDeclaredResult _ _ _ = PTopType
  typeGraphDeclaredArgResults _ _ = []

typeGraphQuery :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> [Type]
typeGraphQuery typeEnv vaenv src = map (\(_, _, dest) -> dest) $ typeGraphQueryWithReason typeEnv vaenv src

data TypeEnv tg = TypeEnv {
  teClassGraph        :: ClassGraph,
  teTypeGraph         :: tg,
  teNames             :: S.HashSet Name,
  -- | The arg keysets each name BOUND in an argument pattern declares, for the names 'teNames'
  -- holds only because they are bound there: a record field (@Rec(foo -> Integer)@'s @foo@,
  -- declaring @∅@) or a function-typed parameter (@fn(acc, v)@, declaring @{acc, v}@). Such a
  -- name has no object of its own in 'teTypeGraph', and without this it would be read as open
  -- world by 'declaredArgKeys' — admitting any args at all, so a relative reference that also
  -- matches it could never have that candidate refuted. A name bound in several patterns
  -- declares every keyset any of them does.
  teBoundArgKeys      :: H.HashMap Name [S.HashSet ArgName],
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
  -- | When 'True', a symbolic satisfiability decision ('satGoal' \/ 'satLeafEscapesAll') is
  -- in flight on this env: the decision roots set it for their entire bodies. It changes NO
  -- answer. A question posed inside a decision — a parameterized class's covariance is decided
  -- inside the engine and asks an element subtype — starts a nested decision and computes it in
  -- full; a nested decision that re-poses an ancestor's goal is stopped by 'satGoal''s in-flight
  -- marker, which reads a goal re-posed while it is being decided as having no finite witness
  -- along that path. The flag is read only by the two memo bypasses ('objArrGraphQuery',
  -- 'expandClassPartialAux'), which skip sharing inside a decision and affect no answer.
  teInSatDecision     :: Bool,
  -- | Identity token enabling the reach-query memo ('objArrGraphQuery'): @Just token@
  -- marks this env as a per-epoch reaches env, with @token@ a process-globally-unique
  -- number minted ONCE for this exact immutable env value ('unsafeFreshQueryToken').
  -- Uniqueness per VALUE is the entire soundness argument: every cache hit under a token
  -- was computed against the literally identical env, so the memoized answer is the pure
  -- function's answer. Any DERIVED env (the 'mkReachesEnv' arg merge, or any other
  -- modification beyond the behavior flags the memo keys on) must carry 'Nothing'.
  teQueryCacheKey     :: Maybe Int,
  -- | The identity a value-predicate decision is remembered under ('predExprHoldsOn'): the token
  -- of the env whose GRAPH this one reads, carried through the merge that adds a scope's local
  -- arrows ('TypeCheck.TypeUtils.withLocalArrows'). A decision depends on the graph alone, so
  -- every constraint of an epoch shares one memo rather than each minting its own, which left
  -- the same predicate re-evaluated under every constraint's token. A predicate naming one of
  -- the local arrows ('teLocalNames') is not remembered this way, since those differ per scope.
  teDecisionKey       :: Maybe Int,
  -- | The names of the arrows local to the scope this env was merged for (a definition's
  -- arguments, a call's lambdas), which the graph holds beside the program's own.
  teLocalNames        :: S.HashSet TypeName,
  -- | How many levels deeper a definition's body may still be read again for a particular call
  -- ('Semantics.arrowDestTypeBase'): each level is a call given lambdas inside the body read
  -- before, whose lambdas' bodies are read again in turn. A recursive definition given a lambda
  -- reads itself again at every level, so the reading stops where the fuel does and the
  -- definition's own result is taken there, as it always was before any reading again.
  teRederiveFuel      :: Int,
  -- | Where this env's graph is one of a series whose other members differ from it only in some
  -- items, which and at what version ('ReuseScope'): the reach query then hands back an answer
  -- worked out in another env of the series, when every item the answer read is still at the
  -- version it was read at ('Semantics.objArrGraphQuery').
  teReuse             :: Maybe ReuseScope,
  -- | What the local arrows this env was merged with are ('Semantics.withLocalArrowsOf'), as an id
  -- standing for exactly them within its 'ReuseScope' ('Semantics.internLocalArrows'): they are
  -- no item of the scope, so an answer read in the env is handed only to an env merged with the
  -- same. 'Nothing' for an env merged with arrows nothing named.
  teLocalKey          :: Maybe Int,
  -- | Global views of 'teClassGraph' and 'teNames' for the symbolic satisfiability engine
  -- (see the section "Symbolic class-predicate satisfiability"), built strictly at
  -- construction via 'mkSatCache'. Any construction or update that changes 'teClassGraph'
  -- or 'teNames' must rebuild this with 'mkSatCache' so the cache matches them.
  teSatCache          :: !SatCache
                          }
  deriving (Show)

-- | A series of envs whose graphs agree except in some items: the envs a type check reads its
-- program through, one per epoch, as the program's own items narrow and every other item stays as
-- it is. 'rsScope' names the series, and stands for everything about its envs that does not
-- change along it (the items it does not version, the graph's names, the class graph), and
-- 'rsVersions' holds the version each item that can change is at in this env. Two envs of one
-- series holding an item at one version hold the same item.
data ReuseScope = ReuseScope {
  rsScope    :: !Int,
  rsVersions :: !(H.HashMap TypeName Int)
  } deriving (Eq, Show)

defaultTypeEnvDebug :: Bool
defaultTypeEnvDebug = False

-- | The class graph with no classes or types: the 'mempty' of the 'Monoid' instance, which is
-- declared beside the 'Semigroup' that merges class graphs with the type algebra.
emptyClassGraph :: ClassGraph
emptyClassGraph = ClassGraph $ graphFromEdges []

emptyTypeEnv' :: TypeEnv EmptyTypeGraph
emptyTypeEnv' = TypeEnv emptyClassGraph EmptyTypeGraph S.empty H.empty False defaultTypeEnvDebug False False False Nothing Nothing S.empty defaultRederiveFuel Nothing Nothing (mkSatCache emptyClassGraph S.empty)

-- | The levels a definition's body may be read again for a call ('teRederiveFuel').
defaultRederiveFuel :: Int
defaultRederiveFuel = 4

-- | A class or type node within the 'ClassGraph'. The class arm's parts are named so that a
-- program handed one can tell them apart.
data CGNode
  = CGClass { cgcSealed :: Sealed, cgcPartial :: PartialType, cgcExtends :: [Type], cgcDoc :: Maybe DocComment }
  | CGType
  deriving (Eq, Ord, Show, Generic)

-- | What a class graph holds at a name, read as a Catln type.
instance CtType CGNode where
  ctDoc _ = [
    "What a class graph holds at a name: a class, or a type.",
    "A [CGClass] carries whether it is [sealed] — whether a later file may add members to it — the partial [class] itself, the [extends] it is a member of, and its [doc]. A [CGType] carries nothing: a type is in the graph so that classes can point at it, and has nothing of its own to say."
            ]

-- | A 'ClassGraph' as two maps — each type to its classes, and each class to its definition. Used
-- as the class graph's JSON representation.
type ClassMap = (
    H.HashMap TypeName (S.HashSet ClassName),
    H.HashMap ClassName (Sealed, PartialType, [Type], Maybe String)
  )

-- | The type variables in the surrounding context that could be referred to by a 'TypeVar' 'TVVar'
type TypeVarEnv = H.HashMap TypeVarName Type

-- | The arguments in the surrounding context that could be referred to by a 'TypeVar' 'TVArg'
type TypeArgEnv = H.HashMap ArgName Type

-- | The variables and arguments in the surrounding context that a 'TypeVar' can refer to, keyed
-- by 'TVVar', 'TVArg' or 'TVApplied'.
--
-- TODO: combine vars and args completely (@to(String)@ rather than @to[String]@). 'TVArg' is used
-- mostly so that 'arrowDestType' of an identity function keeps the same value, and that needs an
-- alternative mechanism first; non-linear pattern matching is also needed.
--
-- An entry is keyed by the variable AND the location a reference reads it at ('VarKey'): the
-- ambient scope ('TVExt'), the partial a position sits inside ('TVInt'), or the partial one level
-- further out ('TVParent'). The same name at two locations is two variables — a law body's
-- @$_T@ inside @operator++(..)@ is not @operator++@'s own @/T@ — so one environment holds every
-- layer a reference can read, and a reference reads only its own.
type TypeVarArgEnv = H.HashMap VarKey Type

type TypeIOVarArgEnv = H.HashMap TypeVarAux (Type, Type)

-- | A 'TypeVarArgEnv' key: the variable and the location a reference to it reads.
data VarKey = VarKey { vkLoc :: !TypeVarLoc, vkVar :: !TypeVarAux }
  deriving (Eq, Ord, Generic, Hashable)

instance Show VarKey where
  show (VarKey TVExt v)    = show v
  show (VarKey TVInt v)    = "int " ++ show v
  show (VarKey TVParent v) = "parent " ++ show v

-- | The ambient-layer key of a variable.
extKey :: TypeVarAux -> VarKey
extKey = VarKey TVExt

-- | The key a reference at @loc@ reads (and records its meets under): its own layer when the
-- environment holds the name there — inside a partial's arguments, that partial's parameters are
-- the 'TVInt' layer ('intersectPartialsBaseN') — and the ambient layer otherwise. A fragment's top
-- has no own layer, so every location reads the ambient one there (the fragment rule).
varKeyIn :: TypeVarArgEnv -> TypeVarLoc -> TypeVarAux -> VarKey
varKeyIn _ TVExt v = extKey v
varKeyIn env loc v
  | H.member (VarKey loc v) env = VarKey loc v
  -- A parent reference one level in names the partial whose parameters the 'TVInt' layer holds.
  | loc == TVParent, H.member (VarKey TVInt v) env = VarKey TVInt v
  | otherwise = extKey v

-- | The bindings a meet DISCOVERED, comparing the environment it returned with the one it was
-- given: a key the given one did not hold, and a SEEDED one — a partial's own parameter at its
-- 'TVInt' layer, or the enclosing partial's at 'TVParent', entered as ⊤ for the meet to record
-- under ('intersectPartialsBaseN') — that the meet narrowed. Read as "fresh keys only", a seeded
-- parameter's narrowing would never be resolved back into the type the meet returns. An AMBIENT
-- ('TVExt') entry the meet narrowed is not a discovery but a narrowing of an existing binding,
-- which the callers fold differently ('foldDiscovered').
discoveredBindings :: TypeVarArgEnv -> TypeVarArgEnv -> TypeVarArgEnv
discoveredBindings new old = H.union (H.difference new old) (H.differenceWith narrowedTop seeded old)
  where
    seeded = H.filterWithKey (\k _ -> vkLoc k /= TVExt) new
    narrowedTop n o = if o == PTopType && n /= PTopType then Just n else Nothing

-- | The entry a reference at @loc@ reads ('varKeyIn').
envLookupAt :: TypeVarArgEnv -> TypeVarLoc -> TypeVarAux -> Maybe Type
envLookupAt env loc v = H.lookup (varKeyIn env loc v) env

-- | 'vaenvLookup' for a reference at @loc@: its own layer when the environment holds the name
-- there, the ambient one otherwise.
vaenvLookupAt :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> TypeVarLoc -> TypeVarAux -> Type
vaenvLookupAt typeEnv env loc (TVApplied b overrides) = applyTypeVarOverrides typeEnv overrides (vaenvLookupAt typeEnv env loc b)
vaenvLookupAt _ env loc v = fromMaybe PTopType (envLookupAt env loc v)

-- | An environment read by name, its own ('TVInt') layer over the ambient one: what a site that
-- matches discoveries to a partial's parameters by NAME reads.
envByName :: TypeVarArgEnv -> H.HashMap TypeVarAux Type
envByName env = H.union (H.fromList [ (v, t) | (VarKey TVInt v, t) <- H.toList env ]) (envAmbient env)

-- | A name-keyed environment (a constraint's var-arg env, a match's bindings) as the AMBIENT layer.
ambientEnv :: H.HashMap TypeVarAux Type -> TypeVarArgEnv
ambientEnv = H.mapKeys extKey

-- | The ambient layer of an environment, keyed by name.
envAmbient :: TypeVarArgEnv -> H.HashMap TypeVarAux Type
envAmbient vaenv = H.fromList [ (v, t) | (VarKey TVExt v, t) <- H.toList vaenv ]

instance Show TypePredicates where
  show = showPredsD maxShowDepth

showPredsD :: Int -> TypePredicates -> String
showPredsD d (PredsOne p)  = showPartialPredD d p
showPredsD d (PredsAnd ps) = intercalate " && " $ map (showPredsD d) ps
showPredsD d (PredsNot p)  = "!(" ++ showPredsD d p ++ ")"

-- | Render a single 'TypePredicate', preserving the derived-style output but
-- routing the nested 'PartialType' through the depth-bounded renderer.
showPartialPredD :: Int -> TypePredicate -> String
showPartialPredD d (PredExpr p)    = "PredExpr "  ++ showPartialD (d - 1) p
showPartialPredD _ (PredConstEq c) = "PredConstEq " ++ show c
showPartialPredD d (PredClass p)   = "PredClass " ++ showPartialD (d - 1) p
showPartialPredD d (PredRel p)     = "PredRel "   ++ showPartialD (d - 1) p

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
      showArgs args = "(" ++ intercalate ", " (map showListedArg $ H.toList args) ++ ")"
      -- An optional arg ('ptArgOpt') is written with a @?@ after its name: @/counter?=/Counter@.
      showListedArg (argName, argVal)
        | S.member argName (ptArgOpt ptArgMode) = show argName ++ "?=" ++ showTypeD (d - 1) argVal
        | otherwise = showArg (argName, argVal)
      showPreds PredsNone    = ""
      showPreds (PredsOne p) = "| " ++ showPartialPredD d p
      showPreds preds        = "| (" ++ showPredsD d preds ++ ")"
      showPtArgMode = case ptArgMode of
        PtArgMode True True _   -> ""
        PtArgMode True False _  -> ".."
        PtArgMode False True _  -> "??"
        PtArgMode False False _ -> "..??"

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
-- The inner (occurrence-local knowledge) renders as a @∩{…}@ suffix ONLY when non-⊤, so a
-- knowledge-free occurrence prints as the bare variable.
showTypeD d (TypeVar aux loc inner) = base aux ++ innerSfx
    where
      -- Both outer locs print as the ONE source spelling, @$_n@: the parent/ambient distinction is
      -- decided at desugar ('classifyParentVarRefs') from the lexical nesting and is never written
      -- by the user, so a printed type round-trips through the @$_@ lexer rule. Comments in this
      -- module write @$^n@ as shorthand for a 'TVParent'-located reference.
      base (TVVar n) = case loc of
        TVInt -> "$" ++ show n
        _     -> "$_" ++ show n
      base (TVArg n) = case loc of
        TVInt    -> "TVArg " ++ show n
        TVParent -> "TVArg(parent) " ++ show n
        TVExt    -> "TVArg(ext) " ++ show n
      base (TVApplied b ovr) = showTypeD d (TypeVar b loc PTopType) ++ "[" ++ intercalate ", " (map showOvr (H.toList ovr)) ++ "]"
      showOvr (k, v) = show k ++ "=" ++ showTypeD (d - 1) v
      innerSfx = case inner of
        PTopType -> ""
        _        -> "∩{" ++ showTypeD (d - 1) inner ++ "}"

instance Show ClassGraph where
  show (ClassGraph graphData) = show $ map fst3 $ graphToNodes graphData

-- | The separator between the distinct docs a merged class node holds.
docSep :: String
docSep = "\n\n"

-- | Merges the docs of two class graph nodes for the same class.
--
-- This must be idempotent. The same base class re-merges once per importing page across an import
-- closure (see 'mergeClasses'), and every one of those merges offers the /same/ doc again. A doc
-- already present says nothing new, so the merged doc is the distinct docs of both sides, in
-- order, joined by 'docSep'. Empty docs are dropped rather than joined, because
-- 'desObjDocComment' gives an undocumented declaration @Just ""@ rather than @Nothing@.
mergeDoc :: Maybe DocComment -> Maybe DocComment -> Maybe DocComment
mergeDoc (Just a) (Just b) = Just $ intercalate docSep $ L.nub $ filter (not . null) $ splitOn docSep a ++ splitOn docSep b
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
    getNodeClassToType (CGClass sealed cls ext doc, n, _) = Just (n, (sealed, cls, ext, doc))
    getNodeClassToType _                                  = Nothing

instance ToJSON ClassGraph where
  toJSON classGraph = toJSON $ asClassMap classGraph

mapTypePred :: (PartialType -> PartialType) -> TypePredicate -> TypePredicate
mapTypePred f (PredExpr p)    = PredExpr (f p)
mapTypePred _ p@PredConstEq{} = p
mapTypePred f (PredClass p)   = PredClass (f p)
mapTypePred f (PredRel p)     = PredRel (f p)

mapTypePreds :: (TypePredicate -> TypePredicate) -> TypePredicates -> TypePredicates
mapTypePreds f (PredsOne p)  = PredsOne (f p)
mapTypePreds f (PredsAnd ps) = PredsAnd (map (mapTypePreds f) ps)
mapTypePreds f (PredsNot p)  = PredsNot (mapTypePreds f p)

pattern PredsNone :: TypePredicates
pattern PredsNone = PredsAnd []

tryPredsToList :: TypePredicates -> Maybe [TypePredicate]
tryPredsToList (PredsOne p)  = Just [p]
tryPredsToList (PredsAnd ps) = concat <$> mapM tryPredsToList ps
tryPredsToList (PredsNot _)  = Nothing

-- | Defines some of the standard types used elsewhere in the compiler as 'PartialType'
intLeaf, floatLeaf, trueLeaf, falseLeaf, strLeaf, cstrLeaf, charLeaf, ioLeaf :: PartialType
intLeaf = partialVal "/Data/Primitive/Integer"
floatLeaf = partialVal "/Data/Primitive/Float"
trueLeaf = partialVal "/Data/Primitive/True"
falseLeaf = partialVal "/Data/Primitive/False"
-- | A string holding any characters as Catln takes one apart: the @\/Data\/String\/Text@ member of the
-- @\/Data\/String@ class ('ctStringText'), as @stack\/core\/string.ct@ declares it — its one field the
-- list of characters it is made of, @chars -> ConsList[$T -> Char]@. It is the member's own declared
-- form so that it is a member.
strLeaf = (partialVal (mkName ctStringText)){ptArgs = H.singleton (partialKey (mkName ctStringChars)) charList}
  where charList = classPartial (partialVal (mkName ctConsListPath)){ptVars = H.singleton (partialKey "T") (singletonType charLeaf)}
-- | A string as the interpreter holds it, packed: the @\/Catln\/CStr@ member of the @\/Data\/String@
-- class ('ctCStr'). Every string constant is one.
cstrLeaf = partialVal (mkName ctCStr)
charLeaf = partialVal (mkName charPrim)
ioLeaf = partialVal "/Catln/IO"

constantPartialType :: Constant -> PartialType
constantPartialType CInt{}   = intLeaf
constantPartialType CFloat{} = floatLeaf
constantPartialType CStr{}   = cstrLeaf
constantPartialType CChar{}  = charLeaf

-- | The 'Type' containing all possible values, equivalent to the universal.
pattern PTopType :: Type
pattern PTopType <- UnionType (Just (PredsNone, H.null -> True, [])) (H.null -> True) []
  where PTopType = UnionType (Just (PredsNone, H.empty, [])) H.empty []

-- | Gets the name String from a partial name
fromPartialName :: PartialName -> Name
fromPartialName (PTypeName n)     = n
fromPartialName (PClassName n)    = n
fromPartialName (PRelativeName n) = n

-- | Used to convert a 'UnionType' into its components
splitUnionType :: PartialLeafs -> [PartialType]
splitUnionType partials = concatMap (\(k, vs) -> map (aux k) vs) $ H.toList $ fmap S.toList partials
  where aux name (PartialArgsOption vars args preds argMode) = PartialType name vars args preds argMode

-- | Used to combine the component 'PartialType' to form a 'UnionType'
joinUnionType :: [PartialType] -> PartialLeafs
joinUnionType = foldr (\(PartialType pName pVars pArgs pPreds pArgMode) partials -> H.insertWith S.union pName (S.singleton (PartialArgsOption pVars pArgs pPreds pArgMode)) partials) H.empty

-- | Helper to create a 'UnionType' consisting of a single 'PartialType'
singletonType :: PartialType -> Type
singletonType partial = UnionType Nothing (joinUnionType [partial]) []

-- | Helper to create a 'PartialKey' for a value (no args, no vars)
partialKey :: Name -> PartialKey
partialKey n = PartialKey (makeAbsoluteName n) S.empty S.empty

partialToType :: PartialKey -> PartialType
partialToType PartialKey{pkName, pkVars, pkArgs} = (partialVal pkName){ptVars = asArgs pkVars, ptArgs = asArgs pkArgs}
  where
    asArgs = fmap (const PTopType) . S.toMap

-- | Helper to create a 'PartialType' for a value (no args, no vars)
partialVal :: TypeName -> PartialType
partialVal n = PartialType n H.empty H.empty PredsNone PtArgExact

classPartial :: PartialType -> Type
classPartial p = TopType H.empty (PredsOne (PredClass p))

-- | Global views of a 'TypeEnv''s class graph and name universe used by the engine's hot
-- query paths, built strictly at 'TypeEnv' construction by 'mkSatCache': the class-name
-- list, the type-name list, and each class's graph-node data, so a query neither re-lists
-- 'graphToNodes' nor does a 'graphLookup' per call. Pure reorganization of 'teClassGraph' /
-- 'teNames' — nothing here depends on any particular query or operation.
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

-- | The sat cache of an env whose class graph is the one the cache was built from, with the
-- name universe it has now: only 'scTypeNames' reads the names.
satCacheWithNames :: SatCache -> S.HashSet TypeName -> SatCache
satCacheWithNames sc names = sc{scTypeNames = S.toList names}

-- | Builds the 'teSatCache' for a 'TypeEnv' with the given class graph and name universe.
mkSatCache :: ClassGraph -> S.HashSet TypeName -> SatCache
mkSatCache (ClassGraph cg) names = SatCache{
    scClassNames = [fromPartialName k | (CGClass{}, k, _) <- nodes],
    scTypeNames = S.toList names,
    scClasses = H.fromList [(n, classInfo (sealed, cls, ext, doc)) | (CGClass sealed cls ext doc, PClassName n, _) <- nodes]
  }
  where
    nodes = graphToNodes cg
    classInfo (sealed, PartialType{ptVars=varsDecl}, classTypes, _) = ClassSatInfo{
        csiSealed = sealed,
        csiVarsDecl = varsDecl,
        csiTypes = classTypes
      }

-- | The FREE partial-local references of a type: a 'TVInt' at a position where no partial in
-- view declares that name. Those are exactly the ones the leaf readers resolve through the
-- AMBIENT env ('partialCoversSat'.'ambientRef', 'isSubPartialOfWithEnv'.'readOwnScope' — a var
-- slot's value sits at the level of the scope CONTAINING the partial, so an internal spelling
-- the partial does not declare names that scope's parameter). A name a partial does declare is
-- its own and is bound by descent, so it is not collected — unlike 'typeTVIntRefs', which
-- over-collects on purpose for a different consumer.
typeFreeTVIntRefs :: Type -> S.HashSet TypeVarName
typeFreeTVIntRefs = go S.empty
  where
    go _ t | not (typeMentionsVars t) = S.empty
    go bound (TypeVar (TVVar n) TVInt i) = (if S.member n bound then S.empty else S.singleton n) `S.union` go bound i
    go bound (TypeVar _ _ i) = go bound i
    go bound (UnionType mtop pos _) = S.unions $ topRefs ++ map (leafRefs bound) (splitUnionType pos)
      where topRefs = case mtop of
              Just (preds, negLeafs, _) -> predsRefs bound preds : map (leafRefs bound) (splitUnionType negLeafs)
              Nothing                   -> [S.empty]
    -- A partial's own parameters bind its args and predicates; its 'ptVars' VALUES sit at the
    -- partial's own level, so they keep the enclosing bound set (the register rule).
    leafRefs bound PartialType{ptVars, ptArgs, ptPreds} = S.unions $
      predsRefs bound' ptPreds : map (go bound) (H.elems ptVars) ++ map (go bound') (H.elems ptArgs)
      where bound' = S.union bound (S.fromList (H.keys ptVars))
    predsRefs bound (PredsOne p)  = leafRefs bound (predPartial p)
    predsRefs bound (PredsAnd ps) = S.unions $ map (predsRefs bound) ps
    predsRefs bound (PredsNot p)  = predsRefs bound p
    predPartial (PredExpr p)    = p
    predPartial (PredClass p)   = p
    predPartial (PredRel p)     = p
    predPartial (PredConstEq c) = constantPartialType c

-- | Applies a 'TVApplied' reference's parameter overrides to the type its base variable
-- resolved to: each var slot named by an override is rebound on every positive leaf and on
-- every predicate partial (a class application such as @PredClass Maybe[/T=…]@). Slots a
-- leaf does not carry (e.g. the parameterless 'Nothing' member of 'Maybe') are left
-- untouched, matching class-application expansion. A still-unresolved base variable stays
-- symbolic: the overrides are kept on (or merged into) the 'TVApplied' reference.
--
-- A data leaf's fields follow its parameters: a field DECLARED in terms of a rebound parameter
-- ('typeGraphData' — @Some[$T](value -> $T)@) takes its declared spelling again, whose own-scope
-- reference reads the new binding. Its old value was the old binding's, so keeping it would
-- spell a leaf no declaration admits — @$F[$T -> $_B]@ over @Some[/T=Integer](/value=Integer)@
-- giving @Some[/T=B](/value=Integer)@, uninhabited by declaration. A field the rebound
-- parameters do not govern keeps its value.
applyTypeVarOverrides :: (TypeGraph tg) => TypeEnv tg -> H.HashMap TypeVarName Type -> Type -> Type
applyTypeVarOverrides _ overrides t | H.null overrides = t
applyTypeVarOverrides typeEnv overrides (UnionType mNeg posLeafs consts) = UnionType mNeg' posLeafs' consts
  where
    mNeg' = fmap (\(preds, nl, ncs) -> (mapTypePreds (mapTypePred overrideVars) preds, nl, ncs)) mNeg
    posLeafs' = joinUnionType $ map (rederiveFields . overrideVars) $ splitUnionType posLeafs
    overrideVars p@PartialType{ptVars} = p{ptVars=H.mapWithKey (\k old -> H.lookupDefault old k overrides) ptVars}
    rebound PartialType{ptVars} = S.fromList [k | k <- H.keys overrides, H.member k ptVars]
    rederiveFields p@PartialType{ptName, ptArgs}
      | S.null (rebound p) = p
      | otherwise = case [ dArgs | PartialType{ptArgs=dArgs} <- typeGraphData typeEnv ptName
                                 , H.keysSet dArgs == H.keysSet ptArgs ] of
          (dArgs:_) -> p{ptArgs = H.mapWithKey (\k v -> case H.lookup k dArgs of
                                                   Just d | not (S.null (S.intersection (rebound p) (typeFreeTVIntRefs d))) -> d
                                                   _ -> v) ptArgs}
          [] -> p
applyTypeVarOverrides _ overrides (TypeVar b loc i) = reapplyTypeVar overrides b loc i

-- | Re-applies parameter overrides to a variable that is still symbolic: the overrides are kept on
-- (or merged into) its 'TVApplied' reference. A spelling pass that only re-spells a reference, never
-- resolving it, uses this directly and needs no 'TypeEnv'.
reapplyTypeVar :: H.HashMap TypeVarName Type -> TypeVarAux -> TypeVarLoc -> Type -> Type
reapplyTypeVar overrides (TVApplied b ovr) loc i = TypeVar (TVApplied b (H.union overrides ovr)) loc i
reapplyTypeVar overrides b loc i                 = TypeVar (TVApplied b overrides) loc i
