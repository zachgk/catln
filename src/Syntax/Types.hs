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

type RawPartialType = (TypeName, H.HashMap TypeName RawType)
type RawPartialLeafs = (H.HashMap TypeName (S.HashSet (H.HashMap TypeName RawType)))
data RawType
  = RawSumType RawPartialLeafs
  | RawTopType
  deriving (Eq, Ord, Generic, Hashable)

data LeafType = LeafType String (H.HashMap String LeafType)
  deriving (Eq, Ord, Generic, Hashable)

newtype Type = SumType (S.HashSet LeafType)
  deriving (Eq, Ord, Generic, Hashable)

type Sealed = Bool -- whether the typeclass can be extended or not
type ClassMap = (H.HashMap TypeName (S.HashSet ClassName), H.HashMap ClassName (Sealed, S.HashSet TypeName))

instance Show RawType where
  show t | t == rawBottomType = "∅"
  show (RawSumType partials) = "(" ++ intercalate " | " partials' ++ ")"
    where
      showPartialArgs (argName, argVal) = argName ++ "=" ++ show argVal
      showPartial (partialName, partialArgs) = partialName ++ "(" ++ intercalate ", " (map showPartialArgs $ H.toList partialArgs) ++ ")"
      partials' = map showPartial $ splitPartialLeafs partials
  show RawTopType = "RawTopType"

instance Show LeafType where
  show (LeafType name args) = if null args then name
    else name ++ "(" ++ args' ++ ")"
    where
      args' = intercalate ", " (map prettyArg $ H.toList args)
      prettyArg (argName, argType) = show argType ++ " " ++ argName

instance Show Type where
  show (SumType leafs) | S.null leafs = "∅"
  show (SumType leafs) = if S.size leafs == 1
    then sumString
    else "(" ++ sumString ++ ")"
    where sumString = intercalate " | " (map show $ S.toList leafs)


rintLeaf, rfloatLeaf, rstrLeaf :: RawPartialType
rintLeaf = ("Integer", H.empty)
rfloatLeaf = ("Float", H.empty)
rstrLeaf = ("String", H.empty)

rintType, rfloatType, rboolType, rstrType :: RawType
rintType = RawSumType $ joinPartialLeafs [rintLeaf]
rfloatType = RawSumType $ joinPartialLeafs [rfloatLeaf]
rboolType = RawSumType $ joinPartialLeafs [("True", H.empty), ("False", H.empty)]
rstrType = RawSumType $ joinPartialLeafs [rstrLeaf]

intLeaf, floatLeaf, strLeaf :: LeafType
intLeaf = LeafType "Integer" H.empty
floatLeaf = LeafType "Float" H.empty
strLeaf = LeafType "String" H.empty

intType, floatType, boolType, strType :: Type
intType = SumType $ S.singleton intLeaf
floatType = SumType $ S.singleton floatLeaf
boolType = SumType $ S.fromList [LeafType "True" H.empty, LeafType "False" H.empty]
strType = SumType $ S.singleton strLeaf

rawBottomType :: RawType
rawBottomType = RawSumType H.empty

splitPartialLeafs :: RawPartialLeafs -> [RawPartialType]
splitPartialLeafs partials = concatMap (\(k, vs) -> zip (repeat k) vs) $ H.toList $ fmap S.toList partials

joinPartialLeafs :: [RawPartialType] -> RawPartialLeafs
joinPartialLeafs = foldr (\(pName, pArgs) partials -> H.insertWith S.union pName (S.singleton pArgs) partials) H.empty

-- assumes a compacted super type, does not check in superLeafs
hasRawPartial :: RawPartialType -> RawType -> Bool
hasRawPartial _ RawTopType = True
hasRawPartial (subName, subArgs) (RawSumType superPartials) = case H.lookup subName superPartials of
  Just superArgsOptions -> any hasArg superArgsOptions
  Nothing -> False
  where
    hasArg superArgs | H.keysSet subArgs /= H.keysSet superArgs = False
    hasArg superArgs = and $ H.elems $ H.intersectionWith hasRawType subArgs superArgs

-- Maybe rename to subtypeOf
hasRawType :: RawType -> RawType -> Bool
hasRawType _ RawTopType = True
hasRawType RawTopType t = t == RawTopType
hasRawType (RawSumType subPartials) superType = all (`hasRawPartial` superType) $ splitPartialLeafs subPartials

-- Maybe rename to subtypeOf
hasType :: Type -> Type -> Bool
hasType (SumType subLeafs) (SumType superLeafs) = all (`elem` superLeafs) subLeafs

-- TODO: This should combine overlapping partials
compactRawType :: RawType -> RawType
compactRawType RawTopType = RawTopType
compactRawType (RawSumType partials) = RawSumType partials

unionRawType :: RawType -> RawType -> RawType
unionRawType RawTopType _ = RawTopType
unionRawType _ RawTopType = RawTopType
unionRawType (RawSumType aPartials) (RawSumType bPartials) = compactRawType $ RawSumType partials'
  where
    partials' = H.unionWith S.union aPartials bPartials

unionRawTypes :: Foldable f => f RawType -> RawType
unionRawTypes = foldr unionRawType rawBottomType

intersectRawTypes :: RawType -> RawType -> RawType
intersectRawTypes RawTopType t = t
intersectRawTypes t RawTopType = t
intersectRawTypes (RawSumType aPartials) (RawSumType bPartials) = compactRawType $ RawSumType partials'
  where
    partials' = H.intersectionWith intersectArgsOptions (fmap S.toList aPartials) (fmap S.toList bPartials)
    intersectArgsOptions as bs = S.fromList $ catMaybes $ [intersectArgs a b | a <- as, b <- bs]
    intersectArgs :: H.HashMap TypeName RawType -> H.HashMap TypeName RawType -> Maybe (H.HashMap TypeName RawType)
    intersectArgs aArgs bArgs = if H.keysSet aArgs == H.keysSet bArgs
      then  sequence $ H.intersectionWith subIntersect aArgs bArgs
      else Nothing
    subIntersect aType bType = let joined = intersectRawTypes aType bType
                                in if joined == rawBottomType
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

powersetRawType :: RawType -> RawType
powersetRawType RawTopType = RawTopType
powersetRawType (RawSumType partials) = RawSumType partials'
  where
    partials' = joinPartialLeafs $ concatMap fromPartialType $ splitPartialLeafs partials
    fromPartialType (name, args) = map ((name,) . H.fromList) (powerset $ H.toList args)
