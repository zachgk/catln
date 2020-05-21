--------------------------------------------------------------------
-- |
-- Module    :  Syntax
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

module Syntax where

import           Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet          as S
import           Data.Void             (Void)
import           Data.Either
import           Data.Maybe

import           GHC.Generics          (Generic)
import           Text.Megaparsec.Error (ParseErrorBundle)

type Name = String

type TypeName = Name
type ClassName = Name

data RawLeafType = RawLeafType TypeName (H.HashMap TypeName RawLeafType)
  deriving (Eq, Ord, Show, Generic)
instance Hashable RawLeafType

type RawPartialType = (TypeName, H.HashMap TypeName RawType)
type RawLeafSet = S.HashSet RawLeafType
type RawPartialLeafs = (H.HashMap TypeName [H.HashMap TypeName RawType])
data RawType
  = RawSumType RawLeafSet RawPartialLeafs
  | RawTopType
  deriving (Eq, Ord, Show, Generic)
instance Hashable RawType

data LeafType = LeafType String (H.HashMap String LeafType)
  deriving (Eq, Ord, Show, Generic)
instance Hashable LeafType

newtype Type = SumType (S.HashSet LeafType)
  deriving (Eq, Ord, Show, Generic)
instance Hashable Type

type Sealed = Bool -- whether the typeclass can be extended or not
data TypeClass = TypeClass Name Sealed RawLeafSet
  deriving (Eq, Ord, Show, Generic)
instance Hashable TypeClass

type ClassMap = (H.HashMap TypeName (S.HashSet ClassName), H.HashMap ClassName (Sealed, S.HashSet TypeName))

rintLeaf, rfloatLeaf, rboolLeaf, rstrLeaf :: RawLeafType
rintLeaf = RawLeafType "Integer" H.empty
rfloatLeaf = RawLeafType "Float" H.empty
rboolLeaf = RawLeafType "Boolean" H.empty
rstrLeaf = RawLeafType "String" H.empty

rintType, rfloatType, rboolType, rstrType :: RawType
rintType = RawSumType (S.singleton rintLeaf) H.empty
rfloatType = RawSumType (S.singleton rfloatLeaf) H.empty
rboolType = RawSumType (S.singleton rboolLeaf) H.empty
rstrType = RawSumType (S.singleton rstrLeaf) H.empty

intLeaf, floatLeaf, boolLeaf, strLeaf :: LeafType
intLeaf = LeafType "Integer" H.empty
floatLeaf = LeafType "Float" H.empty
boolLeaf = LeafType "Boolean" H.empty
strLeaf = LeafType "String" H.empty

intType, floatType, boolType, strType :: Type
intType = SumType $ S.singleton intLeaf
floatType = SumType $ S.singleton floatLeaf
boolType = SumType $ S.singleton boolLeaf
strType = SumType $ S.singleton strLeaf

newtype Import = Import String
  deriving (Eq, Ord, Show)

newtype Export = Export String
  deriving (Eq, Ord, Show)

data Constant
  = CInt Integer
  | CFloat Double
  | CStr String
  deriving (Eq, Ord, Show, Generic)
instance Hashable Constant

type RawExpr = Expr

data Expr m
  = CExpr m Constant
  | Value m Name
  | TupleApply m (m, Expr m) (H.HashMap Name (Expr m))
  deriving (Eq, Ord, Show, Generic)
instance Hashable m => Hashable (Expr m)

-- Compiler Annotation
data CompAnnot m = CompAnnot Name (H.HashMap Name (Expr m))
  deriving (Eq, Ord, Show, Generic)
instance Hashable m => Hashable (CompAnnot m)

data Guard m
  = IfGuard (Expr m)
  | ElseGuard
  | NoGuard
  deriving (Eq, Ord, Show, Generic)
instance Hashable m => Hashable (Guard m)

data RawDeclSubStatement m
  = RawDeclSubStatementDecl (RawDecl m)
  | RawDeclSubStatementAnnot (CompAnnot m)
  deriving (Eq, Ord, Show)

data DeclLHS m = DeclLHS m m Name (H.HashMap Name m) (Guard m) -- objM, arrM
  deriving (Eq, Ord, Show)

data RawDecl m = RawDecl (DeclLHS m) [RawDeclSubStatement m] (Maybe (Expr m))
  deriving (Eq, Ord, Show)

data RawTypeDef m = RawTypeDef Name RawLeafSet
  deriving (Eq, Ord, Show)

type RawClassDef = (TypeName, ClassName)

data RawStatement m
  = RawDeclStatement (RawDecl m)
  | RawTypeDefStatement (RawTypeDef m)
  | RawClassDefStatement RawClassDef
  deriving (Eq, Ord, Show)

type FileImport = String
type RawPrgm m = ([FileImport], [RawStatement m]) -- TODO: Include [Export]

data Object m = Object m Name (H.HashMap Name m)
  deriving (Eq, Ord, Show, Generic)
instance Hashable m => Hashable (Object m)

data Arrow m = Arrow m [CompAnnot m] (Guard m) (Maybe (Expr m)) -- m is result metadata
  deriving (Eq, Ord, Show, Generic)
instance Hashable m => Hashable (Arrow m)

type ObjectMap m = (H.HashMap (Object m) [Arrow m])
type Prgm m = (ObjectMap m, ClassMap) -- TODO: Include [Export]

type ParseErrorRes = ParseErrorBundle String Void

data ReplRes m
  = ReplStatement (RawStatement m)
  | ReplExpr (Expr m)
  | ReplErr ParseErrorRes
  deriving (Eq, Show)

--- ResArrowTree
type ResBuildEnv f = H.HashMap LeafType [(Guard Typed, ResArrow f)]
type ResExEnv f = H.HashMap (Arrow Typed) (ResArrowTree f, [ResArrowTree f]) -- (result, [compAnnot trees])
data ResArrow f
  = ResEArrow (Arrow Typed)
  | PrimArrow Type f
  | ConstantArrow Constant

data ResArrowTree f
  = ResArrowCompose (ResArrowTree f) (ResArrowTree f)
  | ResArrowMatch (H.HashMap LeafType (ResArrowTree f))
  | ResArrowTuple String (H.HashMap String (ResArrowTree f))
  | ResArrowTupleApply (ResArrowTree f) (H.HashMap String (ResArrowTree f))
  | ResArrowSingle (ResArrow f)
  | ResArrowID
  deriving (Show)

instance Show (ResArrow f) where
  show (ResEArrow arrow) = "(ResEArrow " ++ show arrow ++ ")"
  show (PrimArrow tp _) = "(PrimArrow " ++ show tp ++ ")"
  show (ConstantArrow c) = "(ConstantArrow " ++ show c ++ ")"


-- compile errors
data CNote
  = GenCNote String
  | GenCErr String
  | ParseCErr String
  | TypeCheckCErr
  | BuildTreeCErr String
  | AssertCErr String
  | EvalCErr String
  deriving (Eq, Show)

data CRes r
  = CRes [CNote] r
  | CErr [CNote]
  deriving (Eq, Show)

getCNotes :: CRes r -> [CNote]
getCNotes (CRes notes _) = notes
getCNotes (CErr notes) = notes

partitionCRes :: [CRes r] -> ([CNote], ([CNote], [r]))
partitionCRes = aux ([], ([], []))
  where
    aux x [] = x
    aux (errNotes, (resNotes, res)) ((CRes newResNotes newRes):xs) = aux (errNotes, (newResNotes ++ resNotes, newRes:res)) xs
    aux (errNotes, (resNotes, res)) ((CErr newErrNotes):xs) = aux (newErrNotes ++ errNotes, (resNotes, res)) xs

instance Functor CRes where
  fmap f (CRes notes r) = CRes notes (f r)
  fmap _ (CErr notes) = CErr notes

instance Applicative CRes where
  pure = CRes []
  (CRes notesA f) <*> (CRes notesB b) = CRes (notesA ++ notesB) (f b)
  resA <*> resB = CErr (getCNotes resA ++ getCNotes resB)

instance Monad CRes where
  return = pure
  (CRes notesA a) >>= f = case f a of
    (CRes notesB b) -> CRes (notesA ++ notesB) b
    (CErr notesB) -> CErr (notesA ++ notesB)
  (CErr notes) >>= _ = CErr notes


-- Metadata for the Programs
newtype PreTyped = PreTyped RawType
  deriving (Eq, Ord, Show, Generic)
instance Hashable PreTyped

newtype Typed = Typed Type
  deriving (Eq, Ord, Show, Generic)
instance Hashable Typed

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
splitPartialLeafs partials = concatMap (\(k, vs) -> zip (repeat k) vs) $ H.toList partials

joinPartialLeafs :: [RawPartialType] -> RawPartialLeafs
joinPartialLeafs = foldr (\(pName, pArgs) partials -> H.insertWith (++) pName [pArgs] partials) H.empty

-- assumes a compacted super type, does not check in superLeafs
hasRawPartial :: (TypeName, [H.HashMap TypeName RawType]) -> RawType -> Bool
hasRawPartial _ RawTopType = True
hasRawPartial (subName, subArgsOptions) (RawSumType _ superPartials) = case H.lookup subName superPartials of
  Just superArgsOptions -> all (`hasArgOption` superArgsOptions) subArgsOptions
  Nothing -> False
  where
    hasArgOption subArgs = any (hasArg subArgs)
    hasArg subArgs superArgs = and $ H.elems $ H.intersectionWith hasRawType subArgs superArgs

-- Maybe rename to subtypeOf
hasRawType :: RawType -> RawType -> Bool
hasRawType _ RawTopType = True
hasRawType RawTopType t = t == RawTopType
hasRawType (RawSumType subLeafs subPartials) superType = subLeafsMatch && subPartialsMatch
  where
    subLeafsMatch = all (`hasRawLeaf` superType) subLeafs
    subPartialsMatch = all (`hasRawPartial` superType) $ H.toList subPartials

-- Maybe rename to subtypeOf
hasType :: Type -> Type -> Bool
hasType (SumType subLeafs) (SumType superLeafs) = all (`elem` superLeafs) subLeafs

finishCompactRawTypes :: RawType -> Maybe RawLeafSet
finishCompactRawTypes RawTopType = Nothing
finishCompactRawTypes (RawSumType leafs partials) = fmap (S.union leafs) partials'
  where
    fromArgs prodName args = productRawLeafTypes prodName <$> traverse finishCompactRawTypes args
    fromPartial prodName argsOptions = S.unions <$> traverse (fromArgs prodName) argsOptions
    partials' = fmap S.unions $ H.elems <$> H.traverseWithKey fromPartial partials

productRawLeafTypes :: TypeName -> H.HashMap TypeName RawLeafSet -> RawLeafSet
productRawLeafTypes prodName args = S.fromList $ RawLeafType prodName <$> traverse S.toList args

compactRawType :: RawType -> RawType
compactRawType RawTopType = RawTopType
compactRawType (RawSumType leafs partials) = RawSumType leafs' partials'
  where
    (leafs', partials') = H.foldrWithKey accumLeafsPartials (leafs, H.empty) partials
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

unionRawTypes :: RawType -> RawType -> RawType
unionRawTypes RawTopType _ = RawTopType
unionRawTypes _ RawTopType = RawTopType
unionRawTypes (RawSumType aLeafs aPartials) (RawSumType bLeafs bPartials) = compactRawType $ RawSumType leafs' partials'
  where
    leafs' = S.union aLeafs bLeafs
    partials' = H.unionWith (++) aPartials bPartials

unionRawTypesList :: [RawType] -> RawType
unionRawTypesList = foldr unionRawTypes rawBottomType

intersectRawTypes :: RawType -> RawType -> RawType
intersectRawTypes RawTopType t = t
intersectRawTypes t RawTopType = t
intersectRawTypes (RawSumType aLeafs aPartials) (RawSumType bLeafs bPartials) = compactRawType $ RawSumType leafs' partials'
  where
    leafs' = S.unions [S.intersection aLeafs bLeafs, intersectLeafsPartials aLeafs bPartials, intersectLeafsPartials bLeafs aPartials]
    partials' = H.intersectionWith intersectArgsOptions aPartials bPartials
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

-- normal type, type to powerset
-- TODO: Fix leaf intersection, it currently does not use powerset of b but just b
intersectRawTypeWithPowerset :: RawType -> RawType -> RawType
intersectRawTypeWithPowerset RawTopType t = t
intersectRawTypeWithPowerset t RawTopType = t
intersectRawTypeWithPowerset (RawSumType aLeafs aPartials) (RawSumType bLeafs bPartials) = compactRawType $ RawSumType leafs' partials'
  where
    leafs' = S.unions [S.intersection aLeafs bLeafs, intersectLeafsPartials aLeafs bPartials, intersectLeafsPartials bLeafs aPartials]
    partials' = H.intersectionWith intersectArgsOptions aPartials bPartials
    intersectArgsOptions as bs = catMaybes $ [intersectArgs a b | a <- as, b <- bs]
    intersectArgs :: H.HashMap TypeName RawType -> H.HashMap TypeName RawType -> Maybe (H.HashMap TypeName RawType)
    intersectArgs aArgs bArgs = sequence $ H.intersectionWith subIntersect aArgs bArgs
    subIntersect aType bType = let joined = intersectRawTypes aType bType
                                in if joined == rawBottomType
                                   then Nothing
                                   else Just joined
    intersectLeafsPartials leafs partials = S.filter (leafInPartials partials) leafs
    leafInPartials partials (RawLeafType leafName leafArgs) = case H.lookup leafName partials of
      Just partialArgOptions -> any (leafArgsInPartialArgs leafArgs) partialArgOptions
      Nothing -> False
    leafArgsInPartialArgs leafArgs partialArgs = and (H.intersectionWith hasRawLeaf leafArgs partialArgs)

typedIs :: Typed -> Type -> Bool
typedIs (Typed t1) t2 = t1 == t2

getExprMeta :: Expr m -> m
getExprMeta expr = case expr of
  CExpr m _   -> m
  Value m _   -> m
  TupleApply m _ _ -> m
