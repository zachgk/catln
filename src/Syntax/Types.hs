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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Syntax.Types where

import           Data.Aeson          (ToJSON)
import           Data.Aeson.Types    (ToJSONKey)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Hashable
import           Data.List           (intercalate)
import           Data.Maybe
import           Data.Zip
import           GHC.Generics        (Generic)
import           Prelude             hiding (unzip)
import           Text.Printf

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
  ptName    :: PartialName,
  ptVars    :: H.HashMap TypeVarName Type,
  ptProps   :: H.HashMap (TypeName, TypePropName) Type,
  ptArgs    :: H.HashMap ArgName Type,
  ptArgMode :: PtArgMode
  } deriving (Eq, Ord, Generic, Hashable, ToJSON)

type PartialArgsOption = (H.HashMap TypeVarName Type, H.HashMap (TypeName, TypePropName) Type, H.HashMap ArgName Type, PtArgMode)
type PartialLeafs = (H.HashMap PartialName (S.HashSet PartialArgsOption))
data Type
  = UnionType PartialLeafs
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
    H.HashMap ClassName (Sealed, H.HashMap TypeVarName Type, [Type], Maybe String, String)
  )

type TypeVarEnv = H.HashMap TypeVarName Type
type TypeArgEnv = H.HashMap ArgName Type
type ArgEnv = H.HashMap ArgName Type

instance Show PartialType where
  show (PartialType ptName ptVars ptProps ptArgs _) = concat [showName ptName, showTypeVars ptVars, showProps ptProps, showArgs ptArgs]
    where
      showName (PTypeName t)  = t
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
  show (UnionType partials) = join $ map show $ splitUnionType partials
    where
      join []  = "∅"
      join [p] = p
      join ps  = "(" ++ intercalate " | " ps ++ ")"


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
boolType = UnionType $ joinUnionType [trueLeaf, falseLeaf]
strType = singletonType strLeaf
ioType = singletonType ioLeaf

bottomType :: Type
bottomType = UnionType H.empty

unionsWith :: (Ord k, Hashable k) => (a->a->a) -> [H.HashMap k a] -> H.HashMap k a
unionsWith f = foldl (H.unionWith f) H.empty

isBottomType :: Type -> Bool
-- isBottomType t = compactType t == bottomType
isBottomType t = t == bottomType

splitUnionType :: PartialLeafs -> [PartialType]
splitUnionType partials = concatMap (\(k, vs) -> map (aux k) vs) $ H.toList $ fmap S.toList partials
  where aux name (vars, props, args, argMode) = PartialType name vars props args argMode

joinUnionType :: [PartialType] -> PartialLeafs
joinUnionType = foldr (\(PartialType pName pVars pProps pArgs pArgMode) partials -> H.insertWith S.union pName (S.singleton (pVars, pProps, pArgs, pArgMode)) partials) H.empty

splitUnionTypeByName :: PartialLeafs -> H.HashMap PartialName [PartialType]
splitUnionTypeByName = H.mapWithKey (\k vs -> map (aux k) (S.toList vs))
  where aux name (vars, props, args, argMode) = PartialType name vars props args argMode

joinUnionTypeByName :: H.HashMap PartialName [PartialType] -> PartialLeafs
joinUnionTypeByName = H.map (S.fromList . map typeToArgOption)
  where typeToArgOption (PartialType _ pVars pProps pArgs pArgMode) = (pVars, pProps, pArgs, pArgMode)

singletonType :: PartialType -> Type
singletonType partial = UnionType $ joinUnionType [partial]

-- expands a class partial into a sum of the constituent type partials
-- TODO: Should preserve type properties when expanding
expandClassPartial :: ClassMap -> PartialType -> Type
expandClassPartial _ PartialType{ptName=PTypeName n} = error $ printf "Bad type name %s found in expandClassPartial" n
expandClassPartial _ p@PartialType{ptName=PClassName{}, ptArgs} | not (H.null ptArgs) = error $ printf "expandClassPartial class with args: %s" (show p)
expandClassPartial classMap@(_, classToType) PartialType{ptName=PClassName className, ptVars=classVarsP} = expanded
  where
    expanded = case H.lookup className classToType of
      Just (_, classVarsDecl, classTypes, _, _) -> unionAllTypes classMap $ map mapClassType classTypes
        where
          classVars = H.unionWith (intersectTypes classMap) classVarsP classVarsDecl
          mapClassType TopType = TopType
          mapClassType (TypeVar (TVVar t)) = case H.lookup t classVars of
            Just v -> intersectTypes classMap v (H.lookupDefault TopType t classVars)
            Nothing -> error $ printf "Unknown var %s in expandClassPartial" t
          mapClassType (TypeVar (TVArg t)) = error $ printf "Arg %s found in expandClassPartial" t
          mapClassType (UnionType p) = UnionType $ joinUnionType $ map mapClassPartial $ splitUnionType p
          mapClassPartial tp@PartialType{ptVars} = tp{ptVars=fmap (substituteVarsWithVarEnv classVars) ptVars}
      Nothing -> error $ printf "Unknown class %s in expandClassPartial" className

isSubPartialOfWithEnvBase :: ClassMap -> TypeVarEnv -> TypeArgEnv -> PartialType -> PartialType -> Bool
isSubPartialOfWithEnvBase _ _ _ PartialType{ptName=subName} PartialType{ptName=superName} | subName /= superName = False
isSubPartialOfWithEnvBase _ _ _ PartialType{ptArgs=subArgs, ptArgMode=subArgMode} PartialType{ptArgs=superArgs} | subArgMode == PtArgExact && H.keysSet subArgs /= H.keysSet superArgs = False
isSubPartialOfWithEnvBase _ _ _ PartialType{ptArgs=subArgs} PartialType{ptArgs=superArgs, ptArgMode=superArgMode} | superArgMode == PtArgExact && not (H.keysSet subArgs `isSubsetOf` H.keysSet superArgs) = False
isSubPartialOfWithEnvBase classMap venv aenv PartialType{ptVars=subVars, ptProps=subProps, ptArgs=subArgs} PartialType{ptVars=superVars, ptProps=superProps, ptArgs=superArgs} = hasAll subArgs superArgs && hasAll subProps superProps && hasAll subVars superVars
  where
    venv' = substituteVarsWithVarEnv venv <$> H.unionWith (intersectTypes classMap) superVars subVars
    aenv' = substituteArgsWithArgEnv aenv <$> H.unionWith (intersectTypes classMap) superArgs subArgs
    hasAll sb sp = and $ H.elems $ H.intersectionWith (isSubtypeOfWithEnv classMap venv' aenv') sb sp

isSubPartialOfWithEnv :: ClassMap -> TypeVarEnv -> TypeArgEnv -> PartialType -> PartialType -> Bool
isSubPartialOfWithEnv classMap venv aenv sub super | isSubPartialOfWithEnvBase classMap venv aenv sub super = True
isSubPartialOfWithEnv classMap venv aenv sub super@PartialType{ptName=PClassName{}} = isSubtypeOfWithEnv classMap venv aenv (singletonType sub) (expandClassPartial classMap super)
isSubPartialOfWithEnv _ _ _ _ _ = False

isSubtypeOfWithEnv :: ClassMap -> TypeVarEnv -> TypeArgEnv -> Type -> Type -> Bool
isSubtypeOfWithEnv _ _ _ _ TopType = True
isSubtypeOfWithEnv _ _ _ t1 t2 | t1 == t2 = True
isSubtypeOfWithEnv classMap venv aenv (TypeVar (TVVar v)) t2 = case H.lookup v venv of
  Just t1 -> isSubtypeOfWithEnv classMap venv aenv t1 t2
  Nothing -> error $ printf "isSubtypeOfWithEnv with unknown type var %s" v
isSubtypeOfWithEnv classMap venv aenv t1 (TypeVar (TVVar v)) = case H.lookup v venv of
  Just t2 -> isSubtypeOfWithEnv classMap venv aenv t1 t2
  Nothing -> error $ printf "isSubtypeOfWithEnv with unknown type var %s" v
isSubtypeOfWithEnv classMap venv aenv (TypeVar (TVArg v)) t2 = case H.lookup v aenv of
  Just t1 -> isSubtypeOfWithEnv classMap venv aenv t1 t2
  Nothing -> error $ printf "isSubtypeOfWithEnv with unknown type arg %s" v
isSubtypeOfWithEnv classMap venv aenv t1 (TypeVar (TVArg v)) = case H.lookup v aenv of
  Just t2 -> isSubtypeOfWithEnv classMap venv aenv t1 t2
  Nothing -> error $ printf "isSubtypeOfWithEnv with unknown type arg %s" v
isSubtypeOfWithEnv _ _ _ TopType t = t == TopType
isSubtypeOfWithEnv classMap venv aenv (UnionType subPartials) super@(UnionType superPartials) = all isSubPartial $ splitUnionType subPartials
  where
    isSubPartial sub | any (isSubPartialOfWithEnv classMap venv aenv sub) $ splitUnionType superPartials = True
    isSubPartial sub@PartialType{ptName=PClassName{}} | isSubtypeOfWithEnv classMap venv aenv (expandClassPartial classMap sub) super = True
    isSubPartial _ = False

isSubtypeOf :: ClassMap -> Type -> Type -> Bool
isSubtypeOf classMap = isSubtypeOfWithEnv classMap H.empty H.empty

isSubtypePartialOf :: ClassMap -> PartialType -> Type -> Bool
isSubtypePartialOf classMap subPartial = isSubtypeOf classMap (singletonType subPartial)

-- join partials where one is a subset of another
-- TODO: This currently joins only with matching names.
-- More matches could improve the effectiveness of the compaction, but slows down the code significantly
compactOverlapping :: ClassMap -> PartialLeafs -> PartialLeafs
compactOverlapping classMap = joinUnionTypeByName . fmap aux . splitUnionTypeByName
  where
    aux [] = []
    aux (partial:rest) = if any (isSubtypePartialOf classMap partial . singletonType) rest
      then aux rest
      else partial : aux rest

-- joins partials with only one difference between their args or vars
-- TODO: Should check if props are suitable for joining
compactJoinPartials :: ClassMap -> PartialLeafs -> PartialLeafs
compactJoinPartials classMap partials = joinUnionType $ concat $ H.elems $ fmap joinMatchArgPartials $ H.fromListWith (++) $ map prepGroupJoinable $ splitUnionType partials
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
    tryJoin (PartialType name1 vars1 props1 args1 mode1) (PartialType _ vars2 props2 args2 _) = if numDifferences args1 args2 + numDifferences vars1 vars2 == 1
      then Just $ PartialType name1 (joinMap vars1 vars2) (joinMap props1 props2) (joinMap args1 args2) mode1
      else Nothing

    numDifferences m1 m2 = sum $ fromEnum <$> H.intersectionWith (/=) m1 m2
    joinMap m1 m2 = H.unionWith (unionTypes classMap) m1 m2

-- compacts partials where a type variable is the bottomType to a bottomType
compactBottomTypeVars :: PartialLeafs -> PartialLeafs
compactBottomTypeVars partials = joinUnionType $ mapMaybe aux $ splitUnionType partials
  where
    aux partial@PartialType{ptVars} = if any isBottomType ptVars
      then Nothing
      else Just partial

-- TODO: This should merge type partials into class partials
compactType :: ClassMap -> Type -> Type
compactType _ TopType = TopType
compactType _ t@TypeVar{} = t
compactType classMap (UnionType partials) = UnionType $ (compactOverlapping classMap . compactJoinPartials classMap . compactBottomTypeVars) partials

unionTypes :: ClassMap -> Type -> Type -> Type
unionTypes _ TopType _ = TopType
unionTypes _ _ TopType = TopType
unionTypes _ t1 t2 | isBottomType t2 = t1
unionTypes _ t1 t2 | isBottomType t1 = t2
unionTypes _ t1 t2 | t1 == t2 = t1
unionTypes _ (TypeVar v) t = error $ printf "Can't union type vars %s with %s " (show v) (show t)
unionTypes _ t (TypeVar v) = error $ printf "Can't union type vars %s with %s " (show t) (show v)
unionTypes classMap (UnionType aPartials) (UnionType bPartials) = compactType classMap $ UnionType partials'
  where
    partials' = H.unionWith S.union aPartials bPartials

unionAllTypes :: Foldable f => ClassMap -> f Type -> Type
unionAllTypes classMap = foldr (unionTypes classMap) bottomType

intersectAllTypes :: Foldable f => ClassMap -> f Type -> Type
intersectAllTypes classMap = foldr (intersectTypes classMap) TopType

intersectPartialsBase :: ClassMap -> TypeVarEnv -> PartialType -> PartialType -> Maybe (TypeVarEnv, [PartialType])
intersectPartialsBase _ _ PartialType{ptName=aName} PartialType{ptName=bName} | aName /= bName = Nothing
intersectPartialsBase _ _ PartialType{ptArgs=aArgs, ptArgMode=aArgMode} PartialType{ptArgs=bArgs, ptArgMode=bArgMode} | aArgMode == PtArgExact && bArgMode == PtArgExact && H.keysSet aArgs /= H.keysSet bArgs = Nothing
intersectPartialsBase classMap venv (PartialType name aVars aProps aArgs aArgMode) (PartialType _ bVars bProps bArgs bArgMode) = do
  (varsVenvs, vars') <- unzip <$> intersectMap H.empty aVars bVars
  (propsVenvs, props') <- unzip <$> intersectMap venv aProps bProps
  (argsVenvs, args') <- unzip <$> intersectMap venv aArgs bArgs
  let venvs' = mergeAllVarEnvs classMap [mergeAllVarEnvs classMap propsVenvs, mergeAllVarEnvs classMap argsVenvs, vars']
  let argMode' = case (aArgMode, bArgMode) of
        (PtArgAny, _)            -> PtArgAny
        (_, PtArgAny)            -> PtArgAny
        (PtArgExact, PtArgExact) -> PtArgExact
  return (mergeAllVarEnvs classMap varsVenvs, [PartialType name venvs' props' args' argMode'])
  where
    -- intersectMap unions so that all typeVars or props from either a or b are kept
    intersectMap vev a b = traverse subValidate $ H.unionWith subUnion (fmap (vev,) a) (fmap (vev,) b)
    subUnion (aVenv, a) (bVenv, b) = intersectTypesWithVarEnv classMap (mergeAllVarEnvs classMap [aVenv, bVenv]) a b
    subValidate (vev, subTp) = if isBottomType subTp then Nothing else Just (vev, subTp)

intersectPartials :: ClassMap -> TypeVarEnv -> PartialType -> PartialType -> Maybe (TypeVarEnv, [PartialType])
intersectPartials classMap venv a b = case catMaybes [base, aExpandClass, bExpandClass] of
  [] -> Nothing
  combined -> Just $ foldr1 (\(venv1, a') (venv2, b') -> (mergeVarEnvs classMap venv1 venv2, a' ++ b')) combined
  where
    base = intersectPartialsBase classMap venv a b
    typeAsUnion (v, UnionType pl) = (v, splitUnionType pl)
    typeAsUnion _                 = error "Expected a union"
    aExpandClass = case a of
      PartialType{ptName=PClassName{}} -> Just $ typeAsUnion $ intersectTypesWithVarEnv classMap venv (expandClassPartial classMap a) (singletonType b)
      _ -> Nothing
    bExpandClass = case b of
      PartialType{ptName=PClassName{}} -> Just $ typeAsUnion $ intersectTypesWithVarEnv classMap venv (singletonType a) (expandClassPartial classMap b)
      _ -> Nothing

intersectTypesWithVarEnv :: ClassMap -> TypeVarEnv -> Type -> Type -> (TypeVarEnv, Type)
intersectTypesWithVarEnv _ venv TopType t = (venv, t)
intersectTypesWithVarEnv _ venv t TopType = (venv, t)
intersectTypesWithVarEnv _ venv t1 t2 | t1 == t2 = (venv, t1)
intersectTypesWithVarEnv _ _ (TypeVar v1) (TypeVar v2) = error $ printf "Can't intersect type vars %s with %s" (show v1) (show v2)
intersectTypesWithVarEnv classMap venv tv@(TypeVar (TVVar v)) t = (H.insertWith (intersectTypes classMap) v t venv, tv)
intersectTypesWithVarEnv classMap venv t tv@(TypeVar (TVVar v)) = (H.insertWith (intersectTypes classMap) v t venv, tv)
intersectTypesWithVarEnv _ _ (TypeVar v) t = error $ printf "Can't intersect type vars %s with %s" (show v) (show t)
intersectTypesWithVarEnv _ _ t (TypeVar v) = error $ printf "Can't intersect type vars %s with %s" (show t) (show v)
intersectTypesWithVarEnv _ venv _ t | isBottomType t = (venv, t)
intersectTypesWithVarEnv _ venv t _ | isBottomType t = (venv, t)
intersectTypesWithVarEnv classMap venv (UnionType aPartials) (UnionType bPartials) = case catMaybes [intersectPartials classMap venv aPartial bPartial | aPartial <- splitUnionType aPartials, bPartial <- splitUnionType bPartials] of
  [] -> (venv, bottomType)
  combined -> do
    let (venvs', partials') = unzip combined
    (mergeAllVarEnvs classMap venvs', compactType classMap $ UnionType $ joinUnionType $ concat partials')

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
powerset []     = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs

powersetType :: ClassMap -> Type -> Type
powersetType _ TopType = TopType
powersetType _ (TypeVar t) = TypeVar t
powersetType classMap (UnionType partials) = compactType classMap $ UnionType partials'
  where
    partials' = joinUnionType $ concatMap fromPartialType $ splitUnionType partials
    fromArgs args = powerset $ H.toList args
    fromPartialType (PartialType name vars props args argMode) = [PartialType name vars (H.fromList p) (H.fromList a) argMode | p <- fromArgs props, a <- fromArgs args]

mergeVarEnvs :: ClassMap -> TypeVarEnv -> TypeVarEnv -> TypeVarEnv
mergeVarEnvs classMap = H.unionWith (intersectTypes classMap)

mergeAllVarEnvs :: Foldable f => ClassMap -> f TypeVarEnv -> TypeVarEnv
mergeAllVarEnvs classMap = foldr (mergeVarEnvs classMap) H.empty

substituteVarsWithVarEnv :: TypeVarEnv -> Type -> Type
substituteVarsWithVarEnv venv (UnionType partials) = UnionType $ joinUnionType $ map substitutePartial $ splitUnionType partials
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
substituteArgsWithArgEnv aenv (UnionType partials) = UnionType $ joinUnionType $ map substitutePartial $ splitUnionType partials
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
    UnionType partialLeafs -> UnionType $ joinUnionType $ map substitutePartial $ splitUnionType partialLeafs
      where
        substitutePartial partial@PartialType{ptVars=vs} = partial{ptVars = fmap (substituteVarsWithVarEnv ptVars) vs}

typesGetArg :: ClassMap -> ArgName -> Type -> Maybe Type
typesGetArg classMap argName (UnionType partialLeafs) = fmap (unionAllTypes classMap) $ mapM (typeGetArg argName) $ splitUnionType partialLeafs
typesGetArg _ _ _ = Nothing
