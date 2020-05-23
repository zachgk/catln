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
import           Data.Void             (Void)

import           GHC.Generics          (Generic)
import           Text.Megaparsec.Error (ParseErrorBundle)

import Syntax.Types

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

getExprMeta :: Expr m -> m
getExprMeta expr = case expr of
  CExpr m _   -> m
  Value m _   -> m
  TupleApply m _ _ -> m

typedIs :: Typed -> Type -> Bool
typedIs (Typed t1) t2 = t1 == t2
