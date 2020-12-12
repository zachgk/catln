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

import           Prelude hiding (unzip)
import Data.Zip
import           Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet          as S
import           Data.Maybe
import           Data.List (intercalate)
import           GHC.Generics          (Generic)
import           Text.Printf
import Data.Aeson (ToJSON)
import Data.Aeson.Types (ToJSONKey)

type Name = String

type ArgName = Name
type TypeVarName = Name
type TypeName = Name
type ClassName = Name
type TypePropName = Name


data PartialName
  = PTypeName TypeName
  | PClassName ClassName
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, ToJSONKey)

type PartialType = (PartialName, H.HashMap TypeVarName Type, H.HashMap (TypeName, TypePropName) Type, H.HashMap ArgName Type)
type PartialArgsOption = (H.HashMap TypeVarName Type, H.HashMap (TypeName, TypePropName) Type, H.HashMap ArgName Type)
type PartialLeafs = (H.HashMap PartialName (S.HashSet PartialArgsOption))
data Type
  = SumType PartialLeafs
  | TypeVar TypeVarAux
  | TopType
  deriving (Eq, Ord, Generic, Hashable, ToJSON)

data TypeVarAux
  = TVVar TypeVarName
  | TVArg ArgName
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

type Sealed = Bool -- whether the typeclass can be extended or not
-- TODO: ClassMap should be more granular. Can have class to only a certain object or based on type variables.
type ClassMap = (H.HashMap TypeName (S.HashSet ClassName), H.HashMap ClassName (Sealed, H.HashMap TypeVarName Type, [Type]))

type TypeVarEnv = H.HashMap TypeVarName Type
type ArgEnv = H.HashMap ArgName Type

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


intLeaf, floatLeaf, trueLeaf, falseLeaf, strLeaf, ioLeaf :: PartialType
intLeaf = (PTypeName "Integer", H.empty, H.empty, H.empty)
floatLeaf = (PTypeName "Float", H.empty, H.empty, H.empty)
trueLeaf = (PTypeName "True", H.empty, H.empty, H.empty)
falseLeaf = (PTypeName "False", H.empty, H.empty, H.empty)
strLeaf = (PTypeName "String", H.empty, H.empty, H.empty)
ioLeaf = (PTypeName "IO", H.empty, H.empty, H.empty)

intType, floatType, boolType, strType, ioType :: Type
intType = singletonType intLeaf
floatType = singletonType floatLeaf
boolType = SumType $ joinPartialLeafs [trueLeaf, falseLeaf]
strType = singletonType strLeaf
ioType = singletonType ioLeaf

bottomType :: Type
bottomType = SumType H.empty

unionsWith :: (Ord k, Hashable k) => (a->a->a) -> [H.HashMap k a] -> H.HashMap k a
unionsWith f = foldl (H.unionWith f) H.empty

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
expandClassPartial _ (PTypeName n, _, _, _) = error $ printf "Bad type name %s found in expandClassPartial" n
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
hasPartialWithVarEnv :: ClassMap -> TypeVarEnv -> PartialType -> Type -> Bool
hasPartialWithVarEnv _ _ _ TopType = True
hasPartialWithVarEnv classMap venv sub (TypeVar (TVVar v)) = case H.lookup v venv of
  Just sup -> hasPartialWithVarEnv classMap venv sub sup
  Nothing -> error $ printf "hasPartialWithVarEnv with unknown type var %s" v
hasPartialWithVarEnv _ _ _ (TypeVar v) = error $ "Can't hasPartial type vars: " ++ show v
hasPartialWithVarEnv classMap@(typeToClass, _) venv sub@(subName, subVars, subProps, subArgs) super@(SumType superPartials) = case subName of
  (PTypeName typeName) -> checkDirect || any checkSuperClass (H.lookupDefault S.empty typeName typeToClass)
  PClassName{} -> checkDirect || hasTypeWithVarEnv classMap venv (expandClassPartial classMap sub) super
  where
    venv' = H.union subVars venv
    checkDirect = case H.lookup subName superPartials of
      Just superArgsOptions -> any hasArgs superArgsOptions
      Nothing -> False
      where
        hasArgs (_, _, superArgs) | H.keysSet subArgs /= H.keysSet superArgs = False
        hasArgs (superVars, _, _) | not (H.keysSet subVars `isSubsetOf` H.keysSet superVars) = False
        hasArgs (_, superProps, _) | not (H.keysSet subProps `isSubsetOf` H.keysSet superProps) = False
        hasArgs (superVars, superProps, superArgs) = hasAll subArgs superArgs && hasAll subProps superProps && hasAll subVars superVars
        hasAll sb sp = and $ H.elems $ H.intersectionWith (hasTypeWithVarEnv classMap venv') sb sp
    checkSuperClass superClassName = case H.lookup (PClassName superClassName) superPartials of
      Just superClassArgsOptions -> any (hasPartialWithVarEnv classMap venv' sub . expandClassPartial classMap) $ splitPartialLeafs $ H.singleton (PClassName superClassName) superClassArgsOptions
      Nothing -> False

hasPartial :: ClassMap -> PartialType -> Type -> Bool
hasPartial classMap = hasPartialWithVarEnv classMap H.empty

-- Maybe rename to subtypeOf
hasTypeWithVarEnv :: ClassMap -> TypeVarEnv -> Type -> Type -> Bool
hasTypeWithVarEnv _ _ _ TopType = True
hasTypeWithVarEnv _ _ TopType t = t == TopType
hasTypeWithVarEnv _ _ t1 t2 | t1 == t2 = True
hasTypeWithVarEnv classMap venv (TypeVar (TVVar v)) t2 = case H.lookup v venv of
  Just t1 -> hasTypeWithVarEnv classMap venv t1 t2
  Nothing -> error $ printf "hasTypeWithVarEnv with unkonwn type var %s" v
hasTypeWithVarEnv classMap venv t1 (TypeVar (TVVar v)) = case H.lookup v venv of
  Just t2 -> hasTypeWithVarEnv classMap venv t1 t2
  Nothing -> error $ printf "hasTypeWithVarEnv with unkonwn type var %s" v
hasTypeWithVarEnv _ _ (TypeVar v) t = error $ printf "Can't hasType for type var %s in %s" (show v) (show t)
hasTypeWithVarEnv _ _ t (TypeVar v) = error $ printf "Can't hasType for %s in type var %s" (show t) (show v)
hasTypeWithVarEnv classMap venv (SumType subPartials) superType = all (\p -> hasPartialWithVarEnv classMap venv p superType) $ splitPartialLeafs subPartials

hasType :: ClassMap -> Type -> Type -> Bool
hasType classMap = hasTypeWithVarEnv classMap H.empty

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
unionType _ t1 t2 | t1 == t2 = t1
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
intersectTypePartialLeaves :: ClassMap -> TypeVarEnv -> TypePartialLeafs -> TypePartialLeafs -> (TypeVarEnv, PartialLeafs)
intersectTypePartialLeaves classMap venv aPartials bPartials = partials'
  where
    partials' = (\(pVenvs, p) -> (mergeVenvs pVenvs, p)) $ unzip $ H.filter (not . S.null . snd) $ H.intersectionWith intersectArgsOptions (fmap S.toList aPartials) (fmap S.toList bPartials)

    intersectArgsOptions :: [PartialArgsOption] -> [PartialArgsOption] -> (TypeVarEnv, S.HashSet PartialArgsOption)
    intersectArgsOptions as bs = (\(oVenvs, o) -> (mergeVenvs oVenvs, S.fromList o)) $ unzip $ catMaybes $ [intersectArgs a b | a <- as, b <- bs]

    intersectArgs :: PartialArgsOption -> PartialArgsOption -> Maybe (TypeVarEnv, PartialArgsOption)
    intersectArgs (_, _, aArgs) (_, _, bArgs) | H.keysSet aArgs /= H.keysSet bArgs = Nothing
    intersectArgs (aVars, aProps, aArgs) (bVars, bProps, bArgs) = do
      (varsVenvs, vars') <- fmap unzip $ intersectMap H.empty aVars bVars
      (propsVenvs, props') <- fmap unzip $ intersectMap venv aProps bProps
      (argsVenvs, args') <- fmap unzip $ intersectMap venv aArgs bArgs
      let venvs' = mergeVenvs [mergeVenvs varsVenvs, mergeVenvs propsVenvs, mergeVenvs argsVenvs]
      return (venvs', (vars', props', args'))

    mergeVenvs p = foldr (H.unionWith (intersectTypes classMap)) H.empty p
    -- intersectMap unions so that all typeVars or props from either a or b are kept
    intersectMap vev a b = traverse subValidate $ H.unionWith subUnion (fmap (vev,) a) (fmap (vev,) b)
    subUnion (aVenv, a) (bVenv, b) = intersectTypesWithVarEnv classMap (mergeVenvs [aVenv, bVenv]) a b
    subValidate (vev, subTp) = if isBottomType subTp then Nothing else Just (vev, subTp)

intersectTypeWithClassPartialLeaves :: ClassMap -> TypeVarEnv -> TypePartialLeafs -> ClassPartialLeafs -> (TypeVarEnv, PartialLeafs)
intersectTypeWithClassPartialLeaves classMap venv typePartials classPartials = intersectTypePartialLeaves classMap venv typePartials typePartialsFromClassPartials
  where (SumType typePartialsFromClassPartials) = unionTypes classMap $ map (expandClassPartial classMap) $ splitPartialLeafs classPartials

intersectClassPartialLeaves :: ClassMap -> TypeVarEnv -> ClassPartialLeafs -> ClassPartialLeafs -> (TypeVarEnv, PartialLeafs)
intersectClassPartialLeaves = intersectTypePartialLeaves

intersectTypesWithVarEnv :: ClassMap -> TypeVarEnv -> Type -> Type -> (TypeVarEnv, Type)
intersectTypesWithVarEnv _ venv TopType t = (venv, t)
intersectTypesWithVarEnv _ venv t TopType = (venv, t)
intersectTypesWithVarEnv _ venv t1 t2 | t1 == t2 = (venv, t1)
intersectTypesWithVarEnv _ _ (TypeVar v1) (TypeVar v2) = error $ printf "Can't intersect type vars %s with %s" (show v1) (show v2)
intersectTypesWithVarEnv classMap venv tv@(TypeVar (TVVar v)) t = (H.insertWith (intersectTypes classMap) v t venv, tv)
intersectTypesWithVarEnv classMap venv t tv@(TypeVar (TVVar v)) = (H.insertWith (intersectTypes classMap) v t venv, tv)
intersectTypesWithVarEnv _ _ (TypeVar v) t = error $ printf "Can't intersect type vars %s with %s" (show v) (show t)
intersectTypesWithVarEnv _ _ t (TypeVar v) = error $ printf "Can't intersect type vars %s with %s" (show t) (show v)
intersectTypesWithVarEnv classMap venv1 (SumType aPartials) (SumType bPartials) = (venv5, unionTypes classMap $ map SumType [res1, res2, res3, res4])
  where
    isTypeLeaf (PTypeName _) _ = True
    isTypeLeaf (PClassName _) _ = False
    isClassLeaf name v = not $ isTypeLeaf name v
    aTypePartials = H.filterWithKey isTypeLeaf aPartials
    aClassPartials = H.filterWithKey isClassLeaf aPartials
    bTypePartials = H.filterWithKey isTypeLeaf bPartials
    bClassPartials = H.filterWithKey isClassLeaf bPartials

    (venv2, res1) = intersectTypePartialLeaves classMap venv1 aTypePartials bTypePartials
    (venv3, res2) = intersectClassPartialLeaves classMap venv2 aClassPartials bClassPartials
    (venv4, res3) = intersectTypeWithClassPartialLeaves classMap venv3 aTypePartials bClassPartials
    (venv5, res4) = intersectTypeWithClassPartialLeaves classMap venv4 bTypePartials aClassPartials

intersectTypes :: ClassMap -> Type -> Type -> Type
intersectTypes classMap a b = snd $ intersectTypesWithVarEnv classMap H.empty a b

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

substituteVarsWithVarEnv :: TypeVarEnv -> Type -> Type
substituteVarsWithVarEnv venv (SumType partials) = SumType $ joinPartialLeafs $ map substitutePartial $ splitPartialLeafs partials
  where substitutePartial (partialName, partialVars, partialProps, partialArgs) = (partialName, partialVars, fmap (substituteVarsWithVarEnv venv') partialProps, fmap (substituteVarsWithVarEnv venv') partialArgs)
          where venv' = H.union venv partialVars
substituteVarsWithVarEnv venv (TypeVar (TVVar v)) = case H.lookup v venv of
  Just v' -> v'
  Nothing -> error $ printf "Could not substitute unknown type var %s" v
substituteVarsWithVarEnv _ t = t

substituteVars :: Type -> Type
substituteVars = substituteVarsWithVarEnv H.empty

substituteArgsWithArgEnv :: ArgEnv -> Type -> Type
substituteArgsWithArgEnv aenv (SumType partials) = SumType $ joinPartialLeafs $ map substitutePartial $ splitPartialLeafs partials
  where substitutePartial (partialName, partialVars, partialProps, partialArgs) = (partialName, partialVars, partialProps, fmap (substituteArgsWithArgEnv aenv') partialArgs)
          where aenv' = H.union aenv partialArgs
substituteArgsWithArgEnv aenv (TypeVar (TVArg v)) = case H.lookup v aenv of
  Just v' -> v'
  Nothing -> error $ printf "Could not substitute unknown type arg %s" v
substituteArgsWithArgEnv _ t = t

substituteArgs :: Type -> Type
substituteArgs = substituteArgsWithArgEnv H.empty
