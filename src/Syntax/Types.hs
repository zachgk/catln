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


type PartialType = (TypeName, H.HashMap TypeVarName Type, H.HashMap ArgName Type)
type PartialLeafs = (H.HashMap TypeName (S.HashSet (H.HashMap TypeVarName Type, H.HashMap ArgName Type)))
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
type ClassMap = (H.HashMap TypeName (S.HashSet ClassName), H.HashMap ClassName (Sealed, H.HashMap TypeVarName Type, Type))

instance Show Type where
  show TopType = "TopType"
  show (TypeVar v) = show v
  show t | t == bottomType = "∅"
  show (SumType partials) = "(" ++ intercalate " | " partials' ++ ")"
    where
      showArg (argName, argVal) = argName ++ "=" ++ show argVal
      showTypeVars vars | H.null vars = ""
      showTypeVars vars = printf "<%s>" (intercalate ", " $ map showArg $ H.toList vars)
      showArgs args | H.null args = ""
      showArgs args = printf "(%s)" (intercalate ", " $ map showArg $ H.toList args)
      showPartial (partialName, partialTypeVars, partialArgs) = partialName ++ showTypeVars partialTypeVars ++ showArgs partialArgs
      partials' = map showPartial $ splitPartialLeafs partials


intLeaf, floatLeaf, strLeaf :: PartialType
intLeaf = ("Integer", H.empty, H.empty)
floatLeaf = ("Float", H.empty, H.empty)
strLeaf = ("String", H.empty, H.empty)

intType, floatType, boolType, strType :: Type
intType = SumType $ joinPartialLeafs [intLeaf]
floatType = SumType $ joinPartialLeafs [floatLeaf]
boolType = SumType $ joinPartialLeafs [("True", H.empty, H.empty), ("False", H.empty, H.empty)]
strType = SumType $ joinPartialLeafs [strLeaf]

bottomType :: Type
bottomType = SumType H.empty

splitPartialLeafs :: PartialLeafs -> [PartialType]
splitPartialLeafs partials = concatMap (\(k, vs) -> map (aux k) vs) $ H.toList $ fmap S.toList partials
  where aux name (vars, args) = (name, vars, args)

joinPartialLeafs :: [PartialType] -> PartialLeafs
joinPartialLeafs = foldr (\(pName, pVars, pArgs) partials -> H.insertWith S.union pName (S.singleton (pVars, pArgs)) partials) H.empty

-- assumes a compacted super type, does not check in superLeafs
hasPartial :: PartialType -> Type -> Bool
hasPartial _ TopType = True
hasPartial _ (TypeVar v) = error $ "Can't hasPartial type vars: " ++ show v
hasPartial (subName, subVars, subArgs) (SumType superPartials) = case H.lookup subName superPartials of
  Just superArgsOptions -> any hasArgs superArgsOptions
  Nothing -> False
  where
    hasArgs (_, superArgs) | H.keysSet subArgs /= H.keysSet superArgs = False
    hasArgs (superVars, superArgs) = hasAll subArgs superArgs && hasAll subVars superVars
    hasAll sub sup = and $ H.elems $ H.intersectionWith hasType sub sup

-- Maybe rename to subtypeOf
hasType :: Type -> Type -> Bool
hasType _ TopType = True
hasType TopType t = t == TopType
hasType (TypeVar v) _ = error $ "Can't hasType type vars: " ++ show v
hasType _ (TypeVar v) = error $ "Can't hasType type vars: " ++ show v
hasType (SumType subPartials) superType = all (`hasPartial` superType) $ splitPartialLeafs subPartials

subPartialOf :: PartialType -> PartialType -> Bool
subPartialOf sub sup = sub `hasPartial` SumType (joinPartialLeafs [sup])

-- TODO: This should combine overlapping partials
compactType :: Type -> Type
compactType TopType = TopType
compactType t@TypeVar{} = t
compactType (SumType partials) = SumType nonEmpty
  where nonEmpty = H.filter (not . S.null) partials

unionType :: Type -> Type -> Type
unionType TopType _ = TopType
unionType _ TopType = TopType
unionType t1 t2 | t2 == bottomType = t1
unionType t1 t2 | t1 == bottomType = t2
unionType (TypeVar v) t = error $ printf "Can't union type vars %s with %s " (show v) (show t)
unionType t (TypeVar v) = error $ printf "Can't union type vars %s with %s " (show t) (show v)
unionType (SumType aPartials) (SumType bPartials) = compactType $ SumType partials'
  where
    partials' = H.unionWith S.union aPartials bPartials

unionTypes :: Foldable f => f Type -> Type
unionTypes = foldr unionType bottomType

intersectAllTypes :: Foldable f => f Type -> Type
intersectAllTypes = foldr intersectTypes TopType

intersectTypes :: Type -> Type -> Type
intersectTypes TopType t = t
intersectTypes t TopType = t
intersectTypes (TypeVar v) t = error $ printf "Can't intersect type vars %s with %s" (show v) (show t)
intersectTypes t (TypeVar v) = error $ printf "Can't intersect type vars %s with %s" (show t) (show v)
intersectTypes (SumType aPartials) (SumType bPartials) = compactType $ SumType partials'
  where
    partials' = H.intersectionWith intersectArgsOptions (fmap S.toList aPartials) (fmap S.toList bPartials)
    intersectArgsOptions as bs = S.fromList $ catMaybes $ [intersectArgs a b | a <- as, b <- bs]
    intersectArgs (aVars, _) (bVars, _) | H.keysSet aVars /= H.keysSet bVars = Nothing
    intersectArgs (_, aArgs) (_, bArgs) | H.keysSet aArgs /= H.keysSet bArgs = Nothing
    intersectArgs (aVars, aArgs) (bVars, bArgs) = sequenceT (intersectMap aVars bVars, intersectMap aArgs bArgs)
    intersectMap a b = sequence $ H.intersectionWith subIntersect a b
    subIntersect aType bType = let joined = intersectTypes aType bType
                                in if joined == bottomType
                                   then Nothing
                                   else Just joined

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
    fromPartialType (name, vars, args) = [(name, H.fromList v, H.fromList a) | v <- fromArgs vars, a <- fromArgs args]
