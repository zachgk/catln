--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Types
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Syntax.Types where

import           Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet          as S
import           Data.Maybe
import           Data.List
import           Data.Tuple.Sequence
import           GHC.Generics          (Generic)
import           Text.Printf

type Name = String

type ArgName = Name
type TypeVarName = Name
type TypeName = Name
type ClassName = Name
type TypePropName = Name


data PartialName
  = PTypeName TypeName
  | PClassName ClassName
  deriving (Eq, Ord, Show, Generic, Hashable)

type PartialType = (PartialName, H.HashMap TypeVarName Type, H.HashMap (TypeName, TypePropName) Type, H.HashMap ArgName Type)
type PartialLeafs = (H.HashMap PartialName (S.HashSet (H.HashMap TypeVarName Type, H.HashMap (TypeName, TypePropName) Type, H.HashMap ArgName Type)))
data Type
  = SumType PartialLeafs
  | TypeVar TypeVarAux
  | TopType
  deriving (Eq, Ord, Generic, Hashable)

data TypeVarAux
  = TVVar TypeVarName
  | TVArg ArgName
  deriving (Eq, Ord, Show, Generic, Hashable)

type Sealed = Bool -- whether the typeclass can be extended or not
-- TODO: ClassMap should be more granular. Can have class to only a certain object or based on type variables.
type ClassMap = (H.HashMap TypeName (S.HashSet ClassName), H.HashMap ClassName (Sealed, H.HashMap TypeVarName Type, [Type]))

instance Show Type where
  show TopType = "TopType"
  show (TypeVar v) = show v
  show (SumType partials) = join $ map showPartial $ splitPartialLeafs partials
    where
      showName (PTypeName t) = t
      showName (PClassName t) = t
      showArg (argName, argVal) = argName ++ "=" ++ show argVal
      showProp ((typeName, propName), propVal) = printf "%s_%s = %s" typeName propName (show propVal)
      showTypeVars vars | H.null vars = ""
      showTypeVars vars = printf "<%s>" (intercalate ", " $ map showArg $ H.toList vars)
      showProps props | H.null props = ""
      showProps props = printf "[%s]" (intercalate ", " $ map showProp $ H.toList props)
      showArgs args | H.null args = ""
      showArgs args = printf "(%s)" (intercalate ", " $ map showArg $ H.toList args)
      showPartial (partialName, partialTypeVars, partialProps, partialArgs) = showName partialName ++ showTypeVars partialTypeVars ++ showProps partialProps ++ showArgs partialArgs
      join [] = "∅"
      join [p] = p
      join ps = "(" ++ intercalate " | " ps ++ ")"


intLeaf, floatLeaf, strLeaf, ioLeaf :: PartialType
intLeaf = (PTypeName "Integer", H.empty, H.empty, H.empty)
floatLeaf = (PTypeName "Float", H.empty, H.empty, H.empty)
strLeaf = (PTypeName "String", H.empty, H.empty, H.empty)
ioLeaf = (PTypeName "IO", H.empty, H.empty, H.empty)

intType, floatType, boolType, strType, ioType :: Type
intType = singletonType intLeaf
floatType = singletonType floatLeaf
boolType = SumType $ joinPartialLeafs [(PTypeName "True", H.empty, H.empty, H.empty), (PTypeName "False", H.empty, H.empty, H.empty)]
strType = singletonType strLeaf
ioType = singletonType ioLeaf

bottomType :: Type
bottomType = SumType H.empty

isBottomType :: Type -> Bool
-- isBottomType t = compactType t == bottomType
isBottomType t = t == bottomType

splitPartialLeafs :: PartialLeafs -> [PartialType]
splitPartialLeafs partials = concatMap (\(k, vs) -> map (aux k) vs) $ H.toList $ fmap S.toList partials
  where aux name (vars, props, args) = (name, vars, props, args)

joinPartialLeafs :: [PartialType] -> PartialLeafs
joinPartialLeafs = foldr (\(pName, pVars, pProps, pArgs) partials -> H.insertWith S.union pName (S.singleton (pVars, pProps, pArgs)) partials) H.empty

singletonType :: PartialType -> Type
singletonType partial = SumType $ joinPartialLeafs [partial]

-- expands a class partial into a sum of the constituent type partials
-- TODO: Should preserve type properties when expanding
expandClassPartial :: ClassMap -> PartialType -> Type
expandClassPartial _ (PTypeName _, _, _, _) = error "bad type name found in expandClassPartial"
expandClassPartial _ (PClassName _, _, _, partialArgs) | not (H.null partialArgs) = error "expandClassPartial class with args"
expandClassPartial classMap@(_, classToType) (PClassName className, partialVars, _, _) = SumType $ joinPartialLeafs expanded
  where
    expanded = case H.lookup className classToType of
      Just (_, classVars, classTypes) -> splitPartialLeafs partials'
        where
          (SumType partials') = unionTypes classMap $ map mapClassType classTypes
          mapClassType TopType = TopType
          mapClassType (TypeVar (TVVar t)) = case H.lookup t classVars of
            Just v -> intersectTypes classMap v (H.lookupDefault TopType t partialVars)
            Nothing -> error $ printf "Unknown var %s in expandClassPartial" t
          mapClassType (TypeVar (TVArg t)) = error $ printf "Arg %s found in expandClassPartial" t
          mapClassType (SumType p) = SumType $ joinPartialLeafs $ map mapClassPartial $ splitPartialLeafs p
          mapClassPartial (n, v, p, a) = (n, fmap mapClassType v, fmap mapClassType p, fmap mapClassType a)
      Nothing -> error $ printf "Unknown class %s in expandClassPartial" className

-- assumes a compacted super type, does not check in superLeafs
hasPartial :: ClassMap -> PartialType -> Type -> Bool
hasPartial _ _ TopType = True
hasPartial _ _ (TypeVar v) = error $ "Can't hasPartial type vars: " ++ show v
hasPartial classMap@(typeToClass, _) sub@(subName, subVars, subProps, subArgs) super@(SumType superPartials) = case subName of
  (PTypeName typeName) -> checkDirect || any checkSuperClass (H.lookupDefault S.empty typeName typeToClass)
  PClassName{} -> checkDirect || hasType classMap (expandClassPartial classMap sub) super
  where
    checkDirect = case H.lookup subName superPartials of
      Just superArgsOptions -> any hasArgs superArgsOptions
      Nothing -> False
      where
        hasArgs (_, _, superArgs) | H.keysSet subArgs /= H.keysSet superArgs = False
        hasArgs (superVars, _, _) | not (H.keysSet subVars `isSubsetOf` H.keysSet superVars) = False
        hasArgs (_, superProps, _) | not (H.keysSet subProps `isSubsetOf` H.keysSet superProps) = False
        hasArgs (superVars, superProps, superArgs) = hasAll subArgs superArgs && hasAll subProps superProps && hasAll subVars superVars
        hasAll sb sp = and $ H.elems $ H.intersectionWith (hasType classMap) sb sp
    checkSuperClass superClassName = case H.lookup (PClassName superClassName) superPartials of
      Just superClassArgsOptions -> any (hasPartial classMap sub . expandClassPartial classMap) $ splitPartialLeafs $ H.singleton (PClassName superClassName) superClassArgsOptions
      Nothing -> False

-- Maybe rename to subtypeOf
hasType :: ClassMap -> Type -> Type -> Bool
hasType _ _ TopType = True
hasType _ TopType t = t == TopType
hasType _ t1 t2 | t1 == t2 = True
hasType _ (TypeVar v) t = error $ printf "Can't hasType for type var %s in %s" (show v) (show t)
hasType _ t (TypeVar v) = error $ printf "Can't hasType for %s in type var %s" (show t) (show v)
hasType classMap (SumType subPartials) superType = all (\p -> hasPartial classMap p superType) $ splitPartialLeafs subPartials

subPartialOf :: ClassMap -> PartialType -> PartialType -> Bool
subPartialOf classMap sub sup = hasPartial classMap sub (singletonType sup)

-- join partials where one is a subset of another
compactOverlapping :: ClassMap -> PartialLeafs -> PartialLeafs
compactOverlapping classMap = H.mapWithKey compactArgOptions
  where
    compactArgOptions partialName argOptions = S.filter (filterOption partialName argOptions) argOptions
    filterOption partialName argOptions option@(subVars, subProps, subArgs) = not $ any (\potentialSuperOption@(supVars, supProps, supArgs) -> option /= potentialSuperOption && hasType classMap (singletonType (partialName, subVars, subProps, subArgs)) (singletonType (partialName, supVars, supProps, supArgs))) argOptions

-- TODO: This should combine overlapping partials
-- TODO: This should merge type partials into class partials
compactType :: ClassMap -> Type -> Type
compactType _ TopType = TopType
compactType _ t@TypeVar{} = t
compactType classMap (SumType partials) = SumType $ compactOverlapping classMap nonEmpty
  where nonEmpty = H.filter (not . S.null) partials

unionType :: ClassMap -> Type -> Type -> Type
unionType _ TopType _ = TopType
unionType _ _ TopType = TopType
unionType _ t1 t2 | isBottomType t2 = t1
unionType _ t1 t2 | isBottomType t1 = t2
unionType _ (TypeVar v) t = error $ printf "Can't union type vars %s with %s " (show v) (show t)
unionType _ t (TypeVar v) = error $ printf "Can't union type vars %s with %s " (show t) (show v)
unionType classMap (SumType aPartials) (SumType bPartials) = compactType classMap $ SumType partials'
  where
    partials' = H.unionWith S.union aPartials bPartials

unionTypes :: Foldable f => ClassMap -> f Type -> Type
unionTypes classMap = foldr (unionType classMap) bottomType

intersectAllTypes :: Foldable f => ClassMap -> f Type -> Type
intersectAllTypes classMap = foldr (intersectTypes classMap) TopType

type TypePartialLeafs = PartialLeafs -- leaves only with PTypeName
type ClassPartialLeafs = PartialLeafs -- leaves only with PClassName
intersectTypePartialLeaves :: ClassMap -> TypePartialLeafs -> TypePartialLeafs -> PartialLeafs
intersectTypePartialLeaves classMap aPartials bPartials = partials'
  where
    partials' = H.filter (not . S.null) $ H.intersectionWith intersectArgsOptions (fmap S.toList aPartials) (fmap S.toList bPartials)
    intersectArgsOptions as bs = S.fromList $ catMaybes $ [intersectArgs a b | a <- as, b <- bs]
    intersectArgs (_, _, aArgs) (_, _, bArgs) | H.keysSet aArgs /= H.keysSet bArgs = Nothing
    intersectArgs (aVars, aProps, aArgs) (bVars, bProps, bArgs) = sequenceT (intersectMap aVars bVars, intersectMap aProps bProps, intersectMap aArgs bArgs)
    intersectMap a b = sequence $ H.intersectionWith subIntersect a b
    subIntersect aType bType = let joined = intersectTypes classMap aType bType
                                in if isBottomType joined
                                   then Nothing
                                   else Just joined

intersectTypeWithClassPartialLeaves :: ClassMap -> TypePartialLeafs -> ClassPartialLeafs -> PartialLeafs
intersectTypeWithClassPartialLeaves classMap typePartials classPartials = intersectTypePartialLeaves classMap typePartials typePartialsFromClassPartials
  where (SumType typePartialsFromClassPartials) = unionTypes classMap $ map (expandClassPartial classMap) $ splitPartialLeafs classPartials

intersectClassPartialLeaves :: ClassMap -> ClassPartialLeafs -> ClassPartialLeafs -> PartialLeafs
intersectClassPartialLeaves = intersectTypePartialLeaves

intersectTypes :: ClassMap -> Type -> Type -> Type
intersectTypes _ TopType t = t
intersectTypes _ t TopType = t
intersectTypes _ t1 t2 | t1 == t2 = t1
intersectTypes _ (TypeVar v) t = error $ printf "Can't intersect type vars %s with %s" (show v) (show t)
intersectTypes _ t (TypeVar v) = error $ printf "Can't intersect type vars %s with %s" (show t) (show v)
intersectTypes classMap (SumType aPartials) (SumType bPartials) = unionTypes classMap
  [ SumType $ intersectTypePartialLeaves classMap aTypePartials bTypePartials
  , SumType $ intersectClassPartialLeaves classMap aClassPartials bClassPartials
  , SumType $ intersectTypeWithClassPartialLeaves classMap aTypePartials bClassPartials
  , SumType $ intersectTypeWithClassPartialLeaves classMap bTypePartials aClassPartials
  ]
  where
    isTypeLeaf (PTypeName _) _ = True
    isTypeLeaf (PClassName _) _ = False
    isClassLeaf name v = not $ isTypeLeaf name v
    aTypePartials = H.filterWithKey isTypeLeaf aPartials
    aClassPartials = H.filterWithKey isClassLeaf aPartials
    bTypePartials = H.filterWithKey isTypeLeaf bPartials
    bClassPartials = H.filterWithKey isClassLeaf bPartials

isSubsetOf :: (Eq a, Hashable a) => S.HashSet a -> S.HashSet a -> Bool
x `isSubsetOf` y = all (`S.member` y) x

isSubmapOf :: (Eq k, Eq v, Hashable k) => H.HashMap k v -> H.HashMap k v -> Bool
as `isSubmapOf` bs = and $ H.mapWithKey aux as
  where aux ak av = case H.lookup ak bs of
          Just bv -> av == bv
          Nothing -> True

-- normal type, type to powerset
powerset :: [x] -> [[x]]
powerset [] = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs

powersetType :: Type -> Type
powersetType TopType = TopType
powersetType (TypeVar t) = TypeVar t
powersetType (SumType partials) = SumType partials'
  where
    partials' = joinPartialLeafs $ concatMap fromPartialType $ splitPartialLeafs partials
    fromArgs args = powerset $ H.toList args
    fromPartialType (name, vars, props, args) = [(name, H.fromList v, H.fromList p, H.fromList a) | v <- fromArgs vars, p <- fromArgs props, a <- fromArgs args]
