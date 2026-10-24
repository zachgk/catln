-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.Construct
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Constructors and destructors for types and partials: the primitive leaves, singletons,
-- splitting and joining union types, and reading a partial back from a type.
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns    #-}

module Semantics.Types.Construct where

import           CtConstants
import           Data.Bifunctor       (Bifunctor (bimap, first))
import           Data.Either
import qualified Data.HashMap.Strict  as H
import qualified Data.HashSet         as S
import qualified Data.List            as L
import           Data.Maybe
import           Debug.Trace          (trace)
import           Semantics.Types.Data
import           Semantics.Types.Name
import           Text.Printf
import           Utils


-- | Helper function for conditional trace debugging
debugTrace :: (TypeGraph tg) => TypeEnv tg -> String -> a -> a
debugTrace TypeEnv{teDebug=True} msg x = trace msg x
debugTrace _ _ x                       = x

listClassNames :: (TypeGraph tg) => TypeEnv tg -> [ClassName]
listClassNames = scClassNames . teSatCache

-- | Defines some of the standard types used elsewhere in the compiler as 'Type'
intType, floatType, trueType, falseType, boolType, strType, cstrType, ioType :: Type
intType = singletonType intLeaf
floatType = singletonType floatLeaf
trueType = singletonType trueLeaf
falseType = singletonType falseLeaf
boolType = UnionType Nothing (joinUnionType [trueLeaf, falseLeaf]) []
-- | Every string, in whichever representation: the @\/Data\/String@ class.
strType = classPartial (partialVal (mkName strPrim))
cstrType = singletonType cstrLeaf
ioType = singletonType ioLeaf

-- | Whether a leaf's name is one of the primitive types whose values include constants
-- (@Integer@, @Float@, @Char@). Any other leaf denotes only constructed values.
leafHoldsConstants :: PartialType -> Bool
leafHoldsConstants PartialType{ptName=n} = n `elem` map ptName [intLeaf, floatLeaf, charLeaf]

-- | The singleton type containing exactly one constant value.
-- For example, @constantType (CInt 5)@ represents the type @{5}@, a subtype of @Integer@.
--
-- Except a string literal, which is typed as a 'cstrType' and not as the one string it is: the
-- string the interpreter holds packed, which is what a constant is. Nothing dispatches on which
-- string a value is — a string is matched against another by @==@ — so the type is the
-- representation and not the text.
constantType :: Constant -> Type
constantType CStr{} = cstrType
constantType c      = UnionType Nothing H.empty [c]

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
    predHas (PredExpr p)    = partialHas p
    predHas (PredClass p)   = partialHas p
    predHas (PredRel p)     = partialHas p
    predHas (PredConstEq _) = False
    partialHas PartialType{ptVars, ptArgs, ptPreds} =
      any typeContainsTypeVar ptVars || any typeContainsTypeVar ptArgs || predsHas ptPreds

ptVarArg :: PartialType -> TypeVarArgEnv
ptVarArg PartialType{ptArgs, ptVars} = (ambientEnv $ joinVarArgEnv ptVars ptArgs)

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

makeAbsolutePk :: PartialKey -> PartialKey
makeAbsolutePk (PartialKey name vars args)= PartialKey (makeAbsoluteName name) (S.map makeAbsolutePk vars) (S.map makeAbsolutePk args)

-- | Used to convert a 'UnionType' into its components while keeping types with the same name together
splitUnionTypeByName :: PartialLeafs -> H.HashMap TypeName [PartialType]
splitUnionTypeByName = H.mapWithKey (\k vs -> map (aux k) (S.toList vs))
  where aux name (PartialArgsOption vars args preds argMode) = PartialType name vars args preds argMode

-- | Used to combine the component 'PartialType' to form a 'UnionType' while keeping types with the same name together
joinUnionTypeByName :: H.HashMap TypeName [PartialType] -> PartialLeafs
joinUnionTypeByName leafs = H.map (S.fromList . map typeToArgOption) $ H.filter (not . null) leafs
  where typeToArgOption (PartialType _ pVars pArgs pPreds pArgMode) = PartialArgsOption pVars pArgs pPreds pArgMode

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
          Just (CGClass _ _ classTypes _) ->
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
    atomName (PredClass p)   = Just (ptName p)
    atomName (PredRel p)     = Just (ptName p)
    atomName (PredExpr _)    = Nothing
    atomName (PredConstEq _) = Nothing

partialToKey :: PartialType -> PartialKey
partialToKey PartialType{ptName, ptVars, ptArgs} = makeAbsolutePk $ PartialKey ptName (H.keysSet ptVars) (H.keysSet ptArgs)

partialToTypeSingleton :: PartialKey -> Type
partialToTypeSingleton = singletonType . partialToType

-- | Helper to create a 'Type' for a value (no args, no vars)
typeVal :: TypeName -> Type
typeVal = singletonType . partialVal

-- | Helper to create a relative 'Type' for a value (no args, no vars)
-- TODO Make this return a 'typeVal' when it is an absolute path
relTypeVal :: TypeName -> Type
relTypeVal n = TopType H.empty (PredsOne (PredRel $ partialVal n))

-- | Like 'relTypeVal' but admitting any arguments: the relation type of a name's APPLIED
-- spellings as well as its bare value ('partialVal' alone is exactly-zero-args, which would
-- exclude an applied form such as @#runtime(k=...)@ from the relation).
relTypeAppliedVal :: TypeName -> Type
relTypeAppliedVal n = TopType H.empty (PredsOne (PredRel (partialVal n){ptArgMode=PtArgUnbounded}))

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
      | H.null ptArgs && ptName `S.member` actArgedNames = p{ptArgMode = PtArgUnbounded}
      | otherwise                                        = p
relaxNameBoundArgs _ t = t

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

suffixLookup :: Name -> [Name] -> Maybe Name
suffixLookup s = L.find (relativeNameMatches s)

suffixLookupInDict :: Name -> H.HashMap Name b -> Maybe b
suffixLookupInDict s dict = case suffixLookup s (H.keys dict) of
  Just k  -> H.lookup k dict
  Nothing -> Nothing

-- | 'True' when a type is fully concrete: positive leafs only, with no type variables at any
-- nesting depth and no predicates. Such a type cannot act as a live link to any other position.
typeFullyConcrete :: Type -> Bool
typeFullyConcrete (UnionType Nothing leafs _) = all partialOk (splitUnionType leafs)
  where partialOk PartialType{ptVars, ptArgs, ptPreds} = ptPreds == PredsNone && all typeFullyConcrete ptVars && all typeFullyConcrete ptArgs
typeFullyConcrete _ = False

-- | The set of top-level partial names in a union type (empty for non-unions).
leafNameSet :: Type -> S.HashSet TypeName
leafNameSet (UnionType _ pos _) = S.fromList $ map ptName $ splitUnionType pos
leafNameSet _                   = S.empty

-- | The single name every top-level leaf of a plain union shares, when there is exactly one
-- (e.g. an application's resolved type after name-ambiguity pruning); 'Nothing' for an empty,
-- mixed-name, predicate-topped, or non-union type.
uniqueLeafName :: Type -> Maybe TypeName
uniqueLeafName t@(UnionType Nothing _ _) = case S.toList (leafNameSet t) of
  [n] -> Just n
  _   -> Nothing
uniqueLeafName _ = Nothing

unionPartialLeafs :: Foldable f => f PartialLeafs -> PartialLeafs
unionPartialLeafs = unionsWith S.union
