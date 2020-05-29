--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Common
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

module TypeCheck.Common where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet          as S
import           Data.UnionFind.ST
import           Data.Hashable
import           Data.List
import           GHC.Generics          (Generic)

import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           Text.Printf

data TypeCheckError
  = GenTypeCheckError String
  | AbandonCon SConstraint
  | FailInfer String Scheme [SConstraint]
  | TupleMismatch (TypeCheckResult TypedMeta) (TypeCheckResult TExpr) Typed (TypeCheckResult (H.HashMap String TExpr)) [SConstraint]
  deriving (Eq, Ord, Generic)
instance Hashable TypeCheckError

data SType = SType RawType RawType String -- SType upper lower description
  deriving (Eq, Ord, Generic)
instance Hashable SType
type Scheme = TypeCheckResult SType

type Pnt s = Point s Scheme

type EnvValMap s = (H.HashMap String (VarMeta s))
data FEnv s = FEnv [Constraint s] (EnvValMap s) [TypeCheckError]

data Constraint s
  = EqualsKnown (Pnt s) RawType
  | EqPoints (Pnt s) (Pnt s)
  | BoundedBy (Pnt s) (Pnt s)
  | BoundedByKnown (Pnt s) RawType
  | ArrowTo (Pnt s) (Pnt s) -- ArrowTo src dest
  | PropEq (Pnt s, Name) (Pnt s)
  | AddArgs (Pnt s, S.HashSet String) (Pnt s)
  | UnionOf (Pnt s) [Pnt s]
  deriving (Eq)

data SConstraint
  = SEqualsKnown Scheme RawType
  | SEqPoints Scheme Scheme
  | SBoundedBy Scheme Scheme
  | SBoundedByKnown Scheme RawType
  | SArrowTo Scheme Scheme
  | SPropEq (Scheme, Name) Scheme
  | SAddArgs (Scheme, S.HashSet String) Scheme
  | SUnionOf Scheme [Scheme]
  deriving (Eq, Ord, Generic)
instance Hashable SConstraint

data TypeCheckResult r
  = TypeCheckResult [TypeCheckError] r
  | TypeCheckResE [TypeCheckError]
  deriving (Eq, Ord, Generic)
instance Hashable r => Hashable (TypeCheckResult r)

getTCRE :: TypeCheckResult r -> [TypeCheckError]
getTCRE (TypeCheckResult notes _) = notes
getTCRE (TypeCheckResE notes) = notes

instance Functor TypeCheckResult where
  fmap f (TypeCheckResult notes r) = TypeCheckResult notes (f r)
  fmap _ (TypeCheckResE notes) = TypeCheckResE notes

instance Applicative TypeCheckResult where
  pure = TypeCheckResult []
  (TypeCheckResult notesA f) <*> (TypeCheckResult notesB b) = TypeCheckResult (notesA ++ notesB) (f b)
  resA <*> resB = TypeCheckResE (getTCRE resA ++ getTCRE resB)

instance Monad TypeCheckResult where
  return = pure
  (TypeCheckResult notesA a) >>= f = case f a of
    (TypeCheckResult notesB b) -> TypeCheckResult (notesA ++ notesB) b
    (TypeCheckResE notesB) -> TypeCheckResE (notesA ++ notesB)
  (TypeCheckResE notes) >>= _ = TypeCheckResE notes


type PreMeta = PreTyped
type PExpr = Expr PreMeta
type PCompAnnot = CompAnnot PExpr
type PGuard = Guard PExpr
type PArrow = Arrow PreMeta
type PObjArg = ObjArg PreMeta
type PObject = Object PreMeta
type PPrgm = Prgm PreMeta
type PReplRes = ReplRes PreMeta

type ShowMeta = Scheme
type SExpr = Expr ShowMeta
type SCompAnnot = CompAnnot SExpr
type SGuard = Guard SExpr
type SArrow = Arrow ShowMeta
type SObjArg = ObjArg ShowMeta
type SObject = Object ShowMeta
type SPrgm = Prgm ShowMeta
type SReplRes = ReplRes ShowMeta

type VarMeta s = Pnt s
type VExpr s = Expr (VarMeta s)
type VCompAnnot s = CompAnnot (VExpr s)
type VGuard s = Guard (VExpr s)
type VArgMetaMap s = ArgMetaMap (VarMeta s)
type VArrow s = Arrow (VarMeta s)
type VObjArg s = ObjArg (VarMeta s)
type VObject s = Object (VarMeta s)
type VObjectMap s = [(VObject s, [VArrow s])]
type VPrgm s = (VObjectMap s, ClassMap)
type VReplRes s = ReplRes (VarMeta s)

type TypedMeta = Typed
type TExpr = Expr TypedMeta
type TCompAnnot = CompAnnot TExpr
type TGuard = Guard TExpr
type TArrow = Arrow TypedMeta
type TObjArg = ObjArg TypedMeta
type TObject = Object TypedMeta
type TPrgm = Prgm TypedMeta
type TReplRes = ReplRes TypedMeta

-- implicit graph
-- type TypeGraphObjects = H.HashMap Name (S.HashSet (H.HashMap TypeName RawLeafType))
type TypeGraphObjects s = Pnt s
type TypeGraphLeafs s = H.HashMap RawLeafType [Pnt s]
type TypeGraph s = (TypeGraphObjects s, TypeGraphLeafs s)

instance Show TypeCheckError where
  show (GenTypeCheckError s) = s
  show (AbandonCon c) = printf "Abandon %s" (show c)
  show (FailInfer desc scheme constraints) = printf "Failed to infer %s\n\tScheme: %s\n\tConstraints: %s" desc (show scheme) (show constraints)
  show (TupleMismatch baseM baseExpr m maybeArgs constraints) = printf "Tuple Apply Mismatch:\n\t(%s %s)(%s) ≠ %s\n\tConstraints: %s" (show baseM) (show baseExpr) args' (show m) (show constraints)
    where
      showArg (argName, argVal) = printf "%s = %s" argName (show argVal)
      args' = case maybeArgs of
        TypeCheckResult _ args -> intercalate ", " $ map showArg $ H.toList args
        TypeCheckResE _ -> "TypeCheckResE"

instance Show SType where
  show (SType upper lower desc) = concat [show upper, " ⊇ ", desc, " ⊇ ", show lower]

instance Show SConstraint where
  show (SEqualsKnown s t) = printf "%s == %s" (show s) (show t)
  show (SEqPoints s1 s2) = printf "%s == %s" (show s1) (show s2)
  show (SBoundedBy s1 s2) = printf "%s ⊆ %s" (show s1) (show s2)
  show (SBoundedByKnown s t) = printf "%s ⊆ %s" (show s) (show t)
  show (SArrowTo f t) = printf "%s -> %s" (show t) (show f)
  show (SPropEq (s1, n) s2) = printf "(%s).%s == %s"  (show s1) n (show s2)
  show (SAddArgs (base, args) res) = printf "(%s)(%s) == %s" (show base) args' (show res)
    where args' = intercalate ", " $ S.toList args
  show (SUnionOf s _) = printf "SUnionOf for %s" (show s)

instance Show r => Show (TypeCheckResult r) where
  show (TypeCheckResult [] r) = show r
  show (TypeCheckResult notes r) = concat ["TCRes [", show notes, "] (", show r, ")"]
  show (TypeCheckResE notes) = concat ["TCErr [", show notes, "]"]

getPnt :: VarMeta s -> Pnt s
getPnt x = x

getPntExpr :: VExpr s -> Pnt s
getPntExpr = getPnt . getExprMeta

addErr :: FEnv s -> TypeCheckError -> FEnv s
addErr (FEnv cons pmap errs) newErr = FEnv cons pmap (newErr:errs)

fLookup :: FEnv s -> String -> (Maybe (VarMeta s), FEnv s)
fLookup env@(FEnv _ pmap _) k = case H.lookup k pmap of
  Just v  -> (Just v, env)
  Nothing -> (Nothing, addErr env (GenTypeCheckError $ "Failed to lookup " ++ k))

addConstraints :: FEnv s -> [Constraint s] -> FEnv s
addConstraints (FEnv oldCons defMap errs) newCons = FEnv (newCons ++ oldCons) defMap errs

fInsert :: FEnv s -> String -> VarMeta s -> FEnv s
fInsert (FEnv cons pmap errs) k v = FEnv cons (H.insert k v pmap) errs

tryIntersectRawTypes :: RawType -> RawType -> String -> TypeCheckResult RawType
tryIntersectRawTypes a b desc= let c = intersectRawTypes a b
                            in if c == rawBottomType
                                  then TypeCheckResE [GenTypeCheckError $ "Failed to intersect(" ++ desc ++ "): " ++ show a ++ " --- " ++ show b]
                                  else return c
