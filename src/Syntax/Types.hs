--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Types
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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import Data.Bifunctor

-- |The name is the basic type used for various kinds of names
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

data PtArgMode
  = PtArgExact -- matches only the exact args
  | PtArgAny -- matches with any additional args
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

data PartialType = PartialType {
  ptName :: PartialName,
  ptVars :: H.HashMap TypeVarName Type,
  ptProps :: H.HashMap (TypeName, TypePropName) Type,
  ptArgs :: H.HashMap ArgName Type,
  ptArgMode :: PtArgMode
  } deriving (Eq, Ord, Generic, Hashable, ToJSON)

type PartialArgsOption = (H.HashMap TypeVarName Type, H.HashMap (TypeName, TypePropName) Type, H.HashMap ArgName Type, PtArgMode)
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
type ClassMap = (
    H.HashMap TypeName (S.HashSet ClassName),
    H.HashMap ClassName (Sealed, H.HashMap TypeVarName Type, [Type])
  )

type TypeVarEnv = H.HashMap TypeVarName Type
type TypeArgEnv = H.HashMap ArgName Type
type ArgEnv = H.HashMap ArgName Type

instance Show PartialType where
  show (PartialType ptName ptVars ptProps ptArgs _) = concat [showName ptName, showTypeVars ptVars, showProps ptProps, showArgs ptArgs]
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

instance Show Type where
  show TopType = "TopType"
  show (TypeVar v) = show v
  show (SumType partials) = join $ map show $ splitPartialLeafs partials
    where
      join [] = "∅"
      join [p] = p
      join ps = "(" ++ intercalate " | " ps ++ ")"


intLeaf, floatLeaf, trueLeaf, falseLeaf, strLeaf, ioLeaf :: PartialType
intLeaf = PartialType (PTypeName "Integer") H.empty H.empty H.empty PtArgExact
floatLeaf = PartialType (PTypeName "Float") H.empty H.empty H.empty PtArgExact
trueLeaf = PartialType (PTypeName "True") H.empty H.empty H.empty PtArgExact
falseLeaf = PartialType (PTypeName "False") H.empty H.empty H.empty PtArgExact
strLeaf = PartialType (PTypeName "String") H.empty H.empty H.empty PtArgExact
ioLeaf = PartialType (PTypeName "IO") H.empty H.empty H.empty PtArgExact

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
  where aux name (vars, props, args, argMode) = PartialType name vars props args argMode

joinPartialLeafs :: [PartialType] -> PartialLeafs
joinPartialLeafs = foldr (\(PartialType pName pVars pProps pArgs pArgMode) partials -> H.insertWith S.union pName (S.singleton (pVars, pProps, pArgs, pArgMode)) partials) H.empty

singletonType :: PartialType -> Type
singletonType partial = SumType $ joinPartialLeafs [partial]

-- expands a class partial into a sum of the constituent type partials
-- TODO: Should preserve type properties when expanding
expandClassPartial :: ClassMap -> PartialType -> Type
expandClassPartial _ PartialType{ptName=PTypeName n} = error $ printf "Bad type name %s found in expandClassPartial" n
expandClassPartial _ p@PartialType{ptName=PClassName{}, ptArgs} | not (H.null ptArgs) = error $ printf "expandClassPartial class with args: %s" (show p)
expandClassPartial classMap@(_, classToType) PartialType{ptName=PClassName className, ptVars} = SumType $ joinPartialLeafs expanded
  where
    expanded = case H.lookup className classToType of
      Just (_, classVars, classTypes) -> splitPartialLeafs partials'
        where
          (SumType partials') = unionTypes classMap $ map mapClassType classTypes
          mapClassType TopType = TopType
          mapClassType (TypeVar (TVVar t)) = case H.lookup t classVars of
            Just v -> intersectTypes classMap v (H.lookupDefault TopType t ptVars)
            Nothing -> error $ printf "Unknown var %s in expandClassPartial" t
          mapClassType (TypeVar (TVArg t)) = error $ printf "Arg %s found in expandClassPartial" t
          mapClassType (SumType p) = SumType $ joinPartialLeafs $ map mapClassPartial $ splitPartialLeafs p
          mapClassPartial (PartialType n v p a am) = PartialType n (fmap mapClassType v) (fmap mapClassType p) (fmap mapClassType a) am
      Nothing -> error $ printf "Unknown class %s in expandClassPartial" className

-- assumes a compacted super type, does not check in superLeafs
hasPartialWithEnv :: ClassMap -> TypeVarEnv -> TypeArgEnv -> PartialType -> Type -> Bool
hasPartialWithEnv _ _ _ _ TopType = True
hasPartialWithEnv classMap venv aenv sub (TypeVar (TVVar v)) = case H.lookup v venv of
  Just sup -> hasPartialWithEnv classMap venv aenv sub sup
  Nothing -> error $ printf "hasPartialWithEnv with unknown type var %s" v
hasPartialWithEnv classMap venv aenv sub (TypeVar (TVArg v)) = case H.lookup v aenv of
  Just sup -> hasPartialWithEnv classMap venv aenv sub sup
  Nothing -> error $ printf "hasPartialWithEnv with unknown type arg %s" v
hasPartialWithEnv classMap@(typeToClass, _) venv aenv sub@(PartialType subName subVars subProps subArgs subArgMode) super@(SumType superPartials) = case subName of
  (PTypeName typeName) -> checkDirect || any checkSuperClass (H.lookupDefault S.empty typeName typeToClass)
  PClassName{} -> checkDirect || hasTypeWithEnv classMap venv aenv (expandClassPartial classMap sub) super
  where
    checkDirect = case H.lookup subName superPartials of
      Just superArgsOptions -> any hasArgs superArgsOptions
      Nothing -> False
      where
        hasArgs (_, _, superArgs, _) | subArgMode == PtArgExact && H.keysSet subArgs /= H.keysSet superArgs = False
        hasArgs (_, _, superArgs, superArgMode) | superArgMode == PtArgExact && not (H.keysSet subArgs `isSubsetOf` H.keysSet superArgs) = False
        hasArgs (superVars, superProps, superArgs, _) = hasAll subArgs superArgs && hasAll subProps superProps && hasAll subVars superVars
          where
            venv' = substituteVarsWithVarEnv venv <$> H.unionWith (intersectTypes classMap) superVars subVars
            aenv' = substituteArgsWithArgEnv aenv <$> H.unionWith (intersectTypes classMap) superArgs subArgs
            hasAll sb sp = and $ H.elems $ H.intersectionWith (hasTypeWithEnv classMap venv' aenv') sb sp
    checkSuperClass superClassName = case H.lookup (PClassName superClassName) superPartials of
      Just superClassArgsOptions -> any (hasPartialWithEnv classMap venv aenv sub . expandClassPartial classMap) $ splitPartialLeafs $ H.singleton (PClassName superClassName) superClassArgsOptions
      Nothing -> False

hasPartial :: ClassMap -> PartialType -> Type -> Bool
hasPartial classMap = hasPartialWithEnv classMap H.empty H.empty

-- Maybe rename to subtypeOf
hasTypeWithEnv :: ClassMap -> TypeVarEnv -> TypeArgEnv -> Type -> Type -> Bool
hasTypeWithEnv _ _ _ _ TopType = True
hasTypeWithEnv _ _ _ t1 t2 | t1 == t2 = True
hasTypeWithEnv classMap venv aenv (TypeVar (TVVar v)) t2 = case H.lookup v venv of
  Just t1 -> hasTypeWithEnv classMap venv aenv t1 t2
  Nothing -> error $ printf "hasTypeWithEnv with unknown type var %s" v
hasTypeWithEnv classMap venv aenv t1 (TypeVar (TVVar v)) = case H.lookup v venv of
  Just t2 -> hasTypeWithEnv classMap venv aenv t1 t2
  Nothing -> error $ printf "hasTypeWithEnv with unknown type var %s" v
hasTypeWithEnv classMap venv aenv (TypeVar (TVArg v)) t2 = case H.lookup v aenv of
  Just t1 -> hasTypeWithEnv classMap venv aenv t1 t2
  Nothing -> error $ printf "hasTypeWithEnv with unknown type arg %s" v
hasTypeWithEnv classMap venv aenv t1 (TypeVar (TVArg v)) = case H.lookup v aenv of
  Just t2 -> hasTypeWithEnv classMap venv aenv t1 t2
  Nothing -> error $ printf "hasTypeWithEnv with unknown type arg %s" v
hasTypeWithEnv _ _ _ TopType t = t == TopType
hasTypeWithEnv classMap venv aenv (SumType subPartials) superType = all (\p -> hasPartialWithEnv classMap venv aenv p superType) $ splitPartialLeafs subPartials

hasType :: ClassMap -> Type -> Type -> Bool
hasType classMap = hasTypeWithEnv classMap H.empty H.empty

subPartialOf :: ClassMap -> PartialType -> PartialType -> Bool
subPartialOf classMap sub sup = hasPartial classMap sub (singletonType sup)

-- join partials where one is a subset of another
compactOverlapping :: ClassMap -> PartialLeafs -> PartialLeafs
compactOverlapping classMap = H.mapWithKey compactArgOptions
  where
    compactArgOptions partialName argOptions = S.filter (filterOptions partialName argOptions) argOptions
    filterOptions partialName argOptions option = not $ any (\potentialSuperOption -> option /= potentialSuperOption && hasType classMap (optionToType partialName option) (optionToType partialName potentialSuperOption)) argOptions
    optionToType name (vars, props, args, argMode) = singletonType (PartialType name vars props args argMode)

-- joins partials with only one difference between their args or vars
-- TODO: Should check if props are suitable for joining
compactJoinPartials :: ClassMap -> PartialLeafs -> PartialLeafs
compactJoinPartials classMap partials = joinPartialLeafs $ concat $ H.elems $ fmap joinMatchArgPartials $ H.fromListWith (++) $ map prepGroupJoinable $ splitPartialLeafs partials
  where
    -- group partials by argSet and varSet to check for joins
    prepGroupJoinable partial@PartialType{ptName, ptArgs, ptVars, ptArgMode} = ((ptName, H.keysSet ptArgs, H.keysSet ptVars, ptArgMode), [partial])

    -- Checks pairs of tuples for joins
    joinMatchArgPartials [] = []
    joinMatchArgPartials (p:ps) = joinMatchArgPartialsAux p ps []
    joinMatchArgPartialsAux curPartial [] tried = curPartial:joinMatchArgPartials tried
    joinMatchArgPartialsAux curPartial (toTry:toTrys) tried = case tryJoin curPartial toTry of
      Just joined -> joinMatchArgPartialsAux joined toTrys tried
      Nothing -> joinMatchArgPartialsAux curPartial toTrys (toTry:tried)

    -- if two partials differ by only one arg or var, joins them else Nothing
    tryJoin (PartialType name1 vars1 props1 args1 mode1) (PartialType _ vars2 props2 args2 _) = if numDifferences args1 args2 + numDifferences vars1 vars2 == 1
      then Just $ PartialType name1 (joinMap vars1 vars2) (joinMap props1 props2) (joinMap args1 args2) mode1
      else Nothing

    numDifferences m1 m2 = sum $ fromEnum <$> H.intersectionWith (/=) m1 m2
    joinMap m1 m2 = H.unionWith (unionType classMap) m1 m2

-- compacts partials where a type variable is the bottomType to a bottomType
compactBottomTypeVars :: PartialLeafs -> PartialLeafs
compactBottomTypeVars partials = joinPartialLeafs $ mapMaybe aux $ splitPartialLeafs partials
  where
    aux partial@PartialType{ptVars} = if any isBottomType ptVars
      then Nothing
      else Just partial

-- TODO: This should combine overlapping partials
-- TODO: This should merge type partials into class partials
compactType :: ClassMap -> Type -> Type
compactType _ TopType = TopType
compactType _ t@TypeVar{} = t
compactType classMap (SumType partials) = SumType $ (compactOverlapping classMap . compactJoinPartials classMap . nonEmpty . compactBottomTypeVars) partials
  where nonEmpty = H.filter (not . S.null)

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
    partials' = first mergeVenvs $ unzip $ H.filter (not . S.null . snd) $ H.intersectionWith intersectArgsOptions (fmap S.toList aPartials) (fmap S.toList bPartials)

    intersectArgsOptions :: [PartialArgsOption] -> [PartialArgsOption] -> (TypeVarEnv, S.HashSet PartialArgsOption)
    intersectArgsOptions as bs = bimap mergeVenvs S.fromList $ unzip $ catMaybes $ [intersectArgs a b | a <- as, b <- bs]

    intersectArgs :: PartialArgsOption -> PartialArgsOption -> Maybe (TypeVarEnv, PartialArgsOption)
    intersectArgs (_, _, aArgs, aArgMode) (_, _, bArgs, bArgMode) | aArgMode == PtArgExact && bArgMode == PtArgExact && H.keysSet aArgs /= H.keysSet bArgs = Nothing
    intersectArgs (aVars, aProps, aArgs, aArgMode) (bVars, bProps, bArgs, bArgMode) = do
      (varsVenvs, vars') <- unzip <$> intersectMap H.empty aVars bVars
      (propsVenvs, props') <- unzip <$> intersectMap venv aProps bProps
      (argsVenvs, args') <- unzip <$> intersectMap venv aArgs bArgs
      let venvs' = mergeVenvs [mergeVenvs propsVenvs, mergeVenvs argsVenvs, vars']
      let argMode' = case (aArgMode, bArgMode) of
            (PtArgAny, _) -> PtArgAny
            (_, PtArgAny) -> PtArgAny
            (PtArgExact, PtArgExact) -> PtArgExact
      return (mergeVenvs varsVenvs, (venvs', props', args', argMode'))

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
intersectTypesWithVarEnv classMap venv1 (SumType aPartials) (SumType bPartials) = (venv5, compactType classMap $ unionTypes classMap $ map SumType [res1, res2, res3, res4])
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
    fromPartialType (PartialType name vars props args argMode) = [PartialType name vars (H.fromList p) (H.fromList a) argMode | p <- fromArgs props, a <- fromArgs args]

substituteVarsWithVarEnv :: TypeVarEnv -> Type -> Type
substituteVarsWithVarEnv venv (SumType partials) = SumType $ joinPartialLeafs $ map substitutePartial $ splitPartialLeafs partials
  where substitutePartial partial@PartialType{ptVars, ptProps, ptArgs} = partial{
          ptVars = fmap (substituteVarsWithVarEnv venv) ptVars,
          ptProps = fmap (substituteVarsWithVarEnv ptVars') ptProps,
          ptArgs = fmap (substituteVarsWithVarEnv ptVars') ptArgs
                                                                        }
          where ptVars' = fmap (substituteVarsWithVarEnv venv) ptVars
substituteVarsWithVarEnv venv (TypeVar (TVVar v)) = case H.lookup v venv of
  Just v' -> v'
  Nothing -> error $ printf "Could not substitute unknown type var %s" v
substituteVarsWithVarEnv _ t = t

substituteVars :: Type -> Type
substituteVars = substituteVarsWithVarEnv H.empty

substituteArgsWithArgEnv :: ArgEnv -> Type -> Type
substituteArgsWithArgEnv aenv (SumType partials) = SumType $ joinPartialLeafs $ map substitutePartial $ splitPartialLeafs partials
  where substitutePartial partial@PartialType{ptArgs} = partial{
          ptArgs = fmap (substituteArgsWithArgEnv aenv') ptArgs
                                                                 }
          where aenv' = H.union aenv ptArgs
substituteArgsWithArgEnv aenv (TypeVar (TVArg v)) = case H.lookup v aenv of
  Just v' -> v'
  Nothing -> error $ printf "Could not substitute unknown type arg %s" v
substituteArgsWithArgEnv _ t = t

substituteArgs :: Type -> Type
substituteArgs = substituteArgsWithArgEnv H.empty

-- gets arg while substituting the variables used in the surrounding context
typeGetArg :: ArgName -> PartialType -> Maybe Type
typeGetArg argName PartialType{ptArgs, ptVars} = do
  arg <- H.lookup argName ptArgs
  return $ case arg of
    TopType -> TopType
    TypeVar (TVVar v) -> H.lookupDefault TopType v ptVars
    TypeVar (TVArg _) -> error $ printf "Not yet implemented"
    SumType partialLeafs -> SumType $ joinPartialLeafs $ map substitutePartial $ splitPartialLeafs partialLeafs
      where
        substitutePartial partial@PartialType{ptVars=vs} = partial{ptVars = fmap (substituteVarsWithVarEnv ptVars) vs}

typesGetArg :: ClassMap -> ArgName -> Type -> Maybe Type
typesGetArg classMap argName (SumType partialLeafs) = fmap (unionTypes classMap) $ mapM (typeGetArg argName) $ splitPartialLeafs partialLeafs
typesGetArg _ _ _ = Nothing
