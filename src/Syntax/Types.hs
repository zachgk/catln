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
import           Data.Either
import           Data.Maybe
import           Data.List
import           GHC.Generics          (Generic)

type Name = String

type TypeName = Name
type ClassName = Name

data RawLeafType = RawLeafType TypeName (H.HashMap TypeName RawLeafType)
  deriving (Eq, Ord, Generic, Hashable)

type RawPartialType = (TypeName, H.HashMap TypeName RawType)
type RawLeafSet = S.HashSet RawLeafType
type RawPartialLeafs = (H.HashMap TypeName (S.HashSet (H.HashMap TypeName RawType)))
data RawType
  = RawSumType RawLeafSet RawPartialLeafs
  | RawTopType
  deriving (Eq, Ord, Generic, Hashable)

data LeafType = LeafType String (H.HashMap String LeafType)
  deriving (Eq, Ord, Generic, Hashable)

newtype Type = SumType (S.HashSet LeafType)
  deriving (Eq, Ord, Generic, Hashable)

type Sealed = Bool -- whether the typeclass can be extended or not
data TypeClass = TypeClass Name Sealed RawLeafSet
  deriving (Eq, Ord, Show, Generic, Hashable)

type ClassMap = (H.HashMap TypeName (S.HashSet ClassName), H.HashMap ClassName (Sealed, S.HashSet TypeName))

instance Show RawLeafType where
  show (RawLeafType name args) = if null args then name
    else name ++ "(" ++ args' ++ ")"
    where
      args' = intercalate ", " (map prettyArg $ H.toList args)
      prettyArg (argName, argType) = show argType ++ " " ++ argName

instance Show RawType where
  show t | t == rawBottomType = "∅"
  show (RawSumType leafs partials) = "(" ++ intercalate " | " (leafs' ++ partials') ++ ")"
    where
      leafs' = map show $ S.toList leafs
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
rintType = RawSumType S.empty $ joinPartialLeafs [rintLeaf]
rfloatType = RawSumType S.empty $ joinPartialLeafs [rfloatLeaf]
rboolType = RawSumType S.empty $ joinPartialLeafs [("True", H.empty), ("False", H.empty)]
rstrType = RawSumType S.empty $ joinPartialLeafs [rstrLeaf]

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
rawBottomType = RawSumType S.empty H.empty

hasRawLeaf :: RawLeafType -> RawType -> Bool
hasRawLeaf _ RawTopType = True
hasRawLeaf leaf@(RawLeafType name args) (RawSumType superLeafs superPartials) = inSuperLeafs || inSuperPartials
  where
    inSuperLeafs = S.member leaf superLeafs
    inSuperPartials = case H.lookup name superPartials of
      Just superArgsList -> any inSuperPartial superArgsList
      Nothing -> False
    inSuperPartial superArgs = H.keysSet args == H.keysSet superArgs && and (H.elems $ H.intersectionWith hasRawLeaf args superArgs)

splitPartialLeafs :: RawPartialLeafs -> [RawPartialType]
splitPartialLeafs partials = concatMap (\(k, vs) -> zip (repeat k) vs) $ H.toList $ fmap S.toList partials

joinPartialLeafs :: [RawPartialType] -> RawPartialLeafs
joinPartialLeafs = foldr (\(pName, pArgs) partials -> H.insertWith S.union pName (S.singleton pArgs) partials) H.empty

-- assumes a compacted super type, does not check in superLeafs
hasRawPartial :: RawPartialType -> RawType -> Bool
hasRawPartial _ RawTopType = True
hasRawPartial (subName, subArgs) (RawSumType _ superPartials) = case H.lookup subName superPartials of
  Just superArgsOptions -> any hasArg superArgsOptions
  Nothing -> False
  where
    hasArg superArgs | H.keysSet subArgs /= H.keysSet superArgs = False
    hasArg superArgs = and $ H.elems $ H.intersectionWith hasRawType subArgs superArgs

-- Maybe rename to subtypeOf
hasRawType :: RawType -> RawType -> Bool
hasRawType _ RawTopType = True
hasRawType RawTopType t = t == RawTopType
hasRawType (RawSumType subLeafs subPartials) superType = subLeafsMatch && subPartialsMatch
  where
    subLeafsMatch = all (`hasRawLeaf` superType) subLeafs
    subPartialsMatch = all (`hasRawPartial` superType) $ splitPartialLeafs subPartials

-- Maybe rename to subtypeOf
hasType :: Type -> Type -> Bool
hasType (SumType subLeafs) (SumType superLeafs) = all (`elem` superLeafs) subLeafs

finishCompactRawTypes :: RawType -> Maybe RawLeafSet
finishCompactRawTypes RawTopType = Nothing
finishCompactRawTypes (RawSumType leafs partials) = fmap (S.union leafs) partials'
  where
    fromArgs prodName args = productRawLeafTypes prodName <$> traverse finishCompactRawTypes args
    fromPartial prodName argsOptions = S.unions <$> traverse (fromArgs prodName) argsOptions
    partials' = fmap S.unions $ H.elems <$> H.traverseWithKey fromPartial (fmap S.toList partials)

productRawLeafTypes :: TypeName -> H.HashMap TypeName RawLeafSet -> RawLeafSet
productRawLeafTypes prodName args = S.fromList $ RawLeafType prodName <$> traverse S.toList args

compactRawType :: RawType -> RawType
compactRawType RawTopType = RawTopType
compactRawType (RawSumType leafs partials) = RawSumType leafs' (fmap S.fromList partials')
  where
    (leafs', partials') = H.foldrWithKey accumLeafsPartials (leafs, H.empty) $ fmap S.toList partials
    accumLeafsPartials partialName partialArgsOptions (leafsAccum, partialsAccum) = let (newLeafs, partialArgsOptions') = compactPartial partialName partialArgsOptions
                                                                                        leafsAccum' = S.union leafsAccum newLeafs
                                                                                        partialsAccum' = if null partialArgsOptions'
                                                                                                            then partialsAccum
                                                                                                            else H.insertWith (++) partialName partialArgsOptions' partialsAccum
                                                                                     in (leafsAccum', partialsAccum')
    compactPartial name argsOptions = let items' = map (compactPartialItem name) argsOptions
                                       in (S.unions $ lefts items', rights items')
    compactPartialItem name args = case traverse finishCompactRawTypes args of
      Just leafArgs -> Left $ productRawLeafTypes name leafArgs
      Nothing -> Right $ fmap compactRawType args

unionRawType :: RawType -> RawType -> RawType
unionRawType RawTopType _ = RawTopType
unionRawType _ RawTopType = RawTopType
unionRawType (RawSumType aLeafs aPartials) (RawSumType bLeafs bPartials) = compactRawType $ RawSumType leafs' partials'
  where
    leafs' = S.union aLeafs bLeafs
    partials' = H.unionWith S.union aPartials bPartials

unionRawTypes :: Foldable f => f RawType -> RawType
unionRawTypes = foldr unionRawType rawBottomType

intersectRawTypes :: RawType -> RawType -> RawType
intersectRawTypes RawTopType t = t
intersectRawTypes t RawTopType = t
intersectRawTypes (RawSumType aLeafs aPartials) (RawSumType bLeafs bPartials) = compactRawType $ RawSumType leafs' partials'
  where
    leafs' = S.unions [S.intersection aLeafs bLeafs, intersectLeafsPartials aLeafs bPartials, intersectLeafsPartials bLeafs aPartials]
    partials' = S.fromList <$> H.intersectionWith intersectArgsOptions (fmap S.toList aPartials) (fmap S.toList bPartials)
    intersectArgsOptions as bs = catMaybes $ [intersectArgs a b | a <- as, b <- bs]
    intersectArgs :: H.HashMap TypeName RawType -> H.HashMap TypeName RawType -> Maybe (H.HashMap TypeName RawType)
    intersectArgs aArgs bArgs = if H.keysSet aArgs == H.keysSet bArgs
      then  sequence $ H.intersectionWith subIntersect aArgs bArgs
      else Nothing
    subIntersect aType bType = let joined = intersectRawTypes aType bType
                                in if joined == rawBottomType
                                   then Nothing
                                   else Just joined
    intersectLeafsPartials leafs partials = S.filter (leafInPartials partials) leafs
    leafInPartials partials (RawLeafType leafName leafArgs) = case H.lookup leafName partials of
      Just partialArgOptions -> any (leafArgsInPartialArgs leafArgs) partialArgOptions
      Nothing -> False
    leafArgsInPartialArgs leafArgs partialArgs = H.keysSet leafArgs == H.keysSet partialArgs && and (H.intersectionWith hasRawLeaf leafArgs partialArgs)

isSubsetOf :: (Eq a, Hashable a) => S.HashSet a -> S.HashSet a -> Bool
x `isSubsetOf` y = all (`S.member` y) x

isSubmapOf :: (Eq k, Eq v, Hashable k) => H.HashMap k v -> H.HashMap k v -> Bool
as `isSubmapOf` bs = and $ H.mapWithKey aux as
  where aux ak av = case H.lookup ak bs of
          Just bv -> av == bv
          Nothing -> True

intersectRawLeafsWithPowerset :: RawLeafType -> RawLeafSet -> [RawLeafType]
intersectRawLeafsWithPowerset (RawLeafType aName aArgs) bs = concatMap subIntersect $ S.toList bs
  where
    subIntersect (RawLeafType bName _) | aName /= bName = []
    subIntersect (RawLeafType _ bArgs) = [RawLeafType aName aArgs | aArgs `isSubmapOf` bArgs]


-- normal type, type to powerset
powerset :: [x] -> [[x]]
powerset [] = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs

powersetRawType :: RawType -> RawType
powersetRawType RawTopType = RawTopType
powersetRawType (RawSumType leafs partials) = RawSumType leafs' partials'
  where
    leafs' = S.fromList $ concatMap fromLeaf $ S.toList leafs
    fromLeaf (RawLeafType name args) = map (RawLeafType name . H.fromList) $ powerset $ H.toList args
    partials' = joinPartialLeafs $ concatMap fromPartialType $ splitPartialLeafs partials
    fromPartialType (name, args) = map ((name,) . H.fromList) (powerset $ H.toList args)
