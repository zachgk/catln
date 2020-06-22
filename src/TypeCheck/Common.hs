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
{-# LANGUAGE DeriveAnyClass #-}

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
  deriving (Eq, Ord, Generic, Hashable)

data SType = SType Type Type String -- SType upper lower description
  deriving (Eq, Ord, Generic, Hashable)
type Scheme = TypeCheckResult SType

type Pnt s = Point s Scheme

type EnvValMap s = (H.HashMap String (VarMeta s))
data FEnv s = FEnv [Constraint s] (TypeGraph s) (EnvValMap s) [TypeCheckError]

data BoundObjs = BoundAllObjs | BoundTypeObjs
  deriving (Eq, Ord, Show, Generic, Hashable)

data Constraint s
  = EqualsKnown (Pnt s) Type
  | EqPoints (Pnt s) (Pnt s)
  | BoundedBy (Pnt s) (Pnt s)
  | BoundedByKnown (Pnt s) Type
  | BoundedByObjs BoundObjs (Pnt s)
  | ArrowTo (Pnt s) (Pnt s) -- ArrowTo src dest
  | PropEq (Pnt s, ArgName) (Pnt s)
  | AddArgs (Pnt s, S.HashSet String) (Pnt s)
  | PowersetTo (Pnt s) (Pnt s)
  | UnionOf (Pnt s) [Pnt s]
  deriving (Eq)

data SConstraint
  = SEqualsKnown Scheme Type
  | SEqPoints Scheme Scheme
  | SBoundedBy Scheme Scheme
  | SBoundedByKnown Scheme Type
  | SBoundedByObjs BoundObjs Scheme
  | SArrowTo Scheme Scheme
  | SPropEq (Scheme, ArgName) Scheme
  | SAddArgs (Scheme, S.HashSet String) Scheme
  | SPowersetTo Scheme Scheme
  | SUnionOf Scheme [Scheme]
  deriving (Eq, Ord, Generic, Hashable)

data TypeCheckResult r
  = TypeCheckResult [TypeCheckError] r
  | TypeCheckResE [TypeCheckError]
  deriving (Eq, Ord, Generic, Hashable)

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

type VarMeta s = (Pnt s, PreTyped)
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
type UnionObj s = (Pnt s, Pnt s) -- a union of all TypeObj for argument inference, union of all Object types for function limiting
type TypeGraphVal s = (VObject s, VArrow s) -- (match object type, if matching then can implicit to type in arrow)
type TypeGraph s = H.HashMap TypeName [TypeGraphVal s] -- H.HashMap (Root tuple name for filtering) [vals]
type TypeEnv s = (UnionObj s, TypeGraph s)

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
  show (SBoundedByObjs b s) = printf "%s %s" (show b) (show s)
  show (SArrowTo f t) = printf "%s -> %s" (show t) (show f)
  show (SPropEq (s1, n) s2) = printf "(%s).%s == %s"  (show s1) n (show s2)
  show (SAddArgs (base, args) res) = printf "(%s)(%s) == %s" (show base) args' (show res)
    where args' = intercalate ", " $ S.toList args
  show (SPowersetTo s t) = printf "𝒫(%s) ⊇ %s" (show s) (show t)
  show (SUnionOf s _) = printf "SUnionOf for %s" (show s)

instance Show r => Show (TypeCheckResult r) where
  show (TypeCheckResult [] r) = show r
  show (TypeCheckResult notes r) = concat ["TCRes [", show notes, "] (", show r, ")"]
  show (TypeCheckResE notes) = concat ["TCErr [", show notes, "]"]

getPnt :: VarMeta s -> Pnt s
getPnt (p, _) = p

getPntExpr :: VExpr s -> Pnt s
getPntExpr = getPnt . getExprMeta

addErr :: FEnv s -> TypeCheckError -> FEnv s
addErr (FEnv cons graph pmap errs) newErr = FEnv cons graph pmap (newErr:errs)

fLookup :: FEnv s -> String -> (Maybe (VarMeta s), FEnv s)
fLookup env@(FEnv _ _ pmap _) k = case H.lookup k pmap of
  Just v  -> (Just v, env)
  Nothing -> (Nothing, addErr env (GenTypeCheckError $ "Failed to lookup " ++ k))

addConstraints :: FEnv s -> [Constraint s] -> FEnv s
addConstraints (FEnv oldCons graph defMap errs) newCons = FEnv (newCons ++ oldCons) graph defMap errs

fInsert :: FEnv s -> String -> VarMeta s -> FEnv s
fInsert (FEnv cons graph pmap errs) k v = FEnv cons graph (H.insert k v pmap) errs

fAddTypeGraph :: FEnv s -> TypeName -> TypeGraphVal s -> FEnv s
fAddTypeGraph (FEnv cons graph pmap errs) k v = FEnv cons (H.insertWith (++) k [v] graph) pmap errs

tryIntersectTypes :: Type -> Type -> String -> TypeCheckResult Type
tryIntersectTypes a b desc= let c = intersectTypes a b
                            in if c == bottomType
                                  then TypeCheckResE [GenTypeCheckError $ "Failed to intersect(" ++ desc ++ "): " ++ show a ++ " --- " ++ show b]
                                  else return c
