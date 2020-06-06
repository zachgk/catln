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
import           GHC.Generics          (Generic)

type Name = String

type TypeName = Name
type ClassName = Name

type PartialType = (TypeName, H.HashMap TypeName Type)
type PartialLeafs = (H.HashMap TypeName (S.HashSet (H.HashMap TypeName Type)))
data Type
  = SumType PartialLeafs
  | TopType
  deriving (Eq, Ord, Generic, Hashable)

type Sealed = Bool -- whether the typeclass can be extended or not
type ClassMap = (H.HashMap TypeName (S.HashSet ClassName), H.HashMap ClassName (Sealed, S.HashSet TypeName))

instance Show Type where
  show t | t == bottomType = "∅"
  show (SumType partials) = "(" ++ intercalate " | " partials' ++ ")"
    where
      showPartialArgs (argName, argVal) = argName ++ "=" ++ show argVal
      showPartial (partialName, partialArgs) = partialName ++ "(" ++ intercalate ", " (map showPartialArgs $ H.toList partialArgs) ++ ")"
      partials' = map showPartial $ splitPartialLeafs partials
  show TopType = "TopType"


intLeaf, floatLeaf, strLeaf :: PartialType
intLeaf = ("Integer", H.empty)
floatLeaf = ("Float", H.empty)
strLeaf = ("String", H.empty)

intType, floatType, boolType, strType :: Type
intType = SumType $ joinPartialLeafs [intLeaf]
floatType = SumType $ joinPartialLeafs [floatLeaf]
boolType = SumType $ joinPartialLeafs [("True", H.empty), ("False", H.empty)]
strType = SumType $ joinPartialLeafs [strLeaf]

bottomType :: Type
bottomType = SumType H.empty

splitPartialLeafs :: PartialLeafs -> [PartialType]
splitPartialLeafs partials = concatMap (\(k, vs) -> zip (repeat k) vs) $ H.toList $ fmap S.toList partials

joinPartialLeafs :: [PartialType] -> PartialLeafs
joinPartialLeafs = foldr (\(pName, pArgs) partials -> H.insertWith S.union pName (S.singleton pArgs) partials) H.empty

-- assumes a compacted super type, does not check in superLeafs
hasPartial :: PartialType -> Type -> Bool
hasPartial _ TopType = True
hasPartial (subName, subArgs) (SumType superPartials) = case H.lookup subName superPartials of
  Just superArgsOptions -> any hasArg superArgsOptions
  Nothing -> False
  where
    hasArg superArgs | H.keysSet subArgs /= H.keysSet superArgs = False
    hasArg superArgs = and $ H.elems $ H.intersectionWith hasType subArgs superArgs

-- Maybe rename to subtypeOf
hasType :: Type -> Type -> Bool
hasType _ TopType = True
hasType TopType t = t == TopType
hasType (SumType subPartials) superType = all (`hasPartial` superType) $ splitPartialLeafs subPartials

-- TODO: This should combine overlapping partials
compactType :: Type -> Type
compactType TopType = TopType
compactType (SumType partials) = SumType partials

unionType :: Type -> Type -> Type
unionType TopType _ = TopType
unionType _ TopType = TopType
unionType (SumType aPartials) (SumType bPartials) = compactType $ SumType partials'
  where
    partials' = H.unionWith S.union aPartials bPartials

unionTypes :: Foldable f => f Type -> Type
unionTypes = foldr unionType bottomType

intersectTypes :: Type -> Type -> Type
intersectTypes TopType t = t
intersectTypes t TopType = t
intersectTypes (SumType aPartials) (SumType bPartials) = compactType $ SumType partials'
  where
    partials' = H.intersectionWith intersectArgsOptions (fmap S.toList aPartials) (fmap S.toList bPartials)
    intersectArgsOptions as bs = S.fromList $ catMaybes $ [intersectArgs a b | a <- as, b <- bs]
    intersectArgs :: H.HashMap TypeName Type -> H.HashMap TypeName Type -> Maybe (H.HashMap TypeName Type)
    intersectArgs aArgs bArgs = if H.keysSet aArgs == H.keysSet bArgs
      then  sequence $ H.intersectionWith subIntersect aArgs bArgs
      else Nothing
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
powersetType (SumType partials) = SumType partials'
  where
    partials' = joinPartialLeafs $ concatMap fromPartialType $ splitPartialLeafs partials
    fromPartialType (name, args) = map ((name,) . H.fromList) (powerset $ H.toList args)
