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
import qualified Data.IntMap.Lazy as IM
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
  | TupleMismatch TypedMeta TExpr Typed (H.HashMap String TExpr)
  | TCWithMatchingConstraints [SConstraint] TypeCheckError
  deriving (Eq, Ord, Generic, Hashable)

type SplitSType = (Type, Type, String)
data SType
  = SType Type Type String -- SType upper lower (description in type)
  | SVar TypeVarAux Pnt
  deriving (Eq, Ord, Generic, Hashable)
type Scheme = TypeCheckResult SType

type Pnt = Int

type EnvValMap = (H.HashMap String VarMeta)
data FEnv = FEnv (IM.IntMap Scheme) [Constraint] TypeEnv EnvValMap
  deriving (Eq, Show)

data BoundObjs = BoundAllObjs | BoundTypeObjs
  deriving (Eq, Ord, Show, Generic, Hashable)

data Constraint
  = EqualsKnown Pnt Type
  | EqPoints Pnt Pnt
  | BoundedByKnown Pnt Type
  | BoundedByObjs BoundObjs Pnt
  | ArrowTo Pnt Pnt -- ArrowTo src dest
  | PropEq (Pnt, ArgName) Pnt
  | AddArgs (Pnt, S.HashSet String) Pnt
  | PowersetTo Pnt Pnt
  | UnionOf Pnt [Pnt]
  deriving (Eq, Show)

data SConstraint
  = SEqualsKnown Scheme Type
  | SEqPoints Scheme Scheme
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

data VarMeta = VarMeta Pnt PreTyped
  deriving (Eq, Show)
type VExpr = Expr VarMeta
type VCompAnnot = CompAnnot VExpr
type VGuard = Guard VExpr
type VArgMetaMap = ArgMetaMap VarMeta
type VArrow = Arrow VarMeta
type VObjArg = ObjArg VarMeta
type VObject = Object VarMeta
type VObjectMap = [(VObject, [VArrow])]
type VPrgm = (VObjectMap, ClassMap)
type VReplRes = ReplRes VarMeta

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
type UnionObj = (Pnt, Pnt) -- a union of all TypeObj for argument inference, union of all Object types for function limiting
type TypeGraphVal = (VObject, VArrow) -- (match object type, if matching then can implicit to type in arrow)
type TypeGraph = H.HashMap TypeName [TypeGraphVal] -- H.HashMap (Root tuple name for filtering) [vals]
type TypeEnv = (UnionObj, TypeGraph)

instance Meta VarMeta where
  getMetaType (VarMeta _ p) = getMetaType p

instance Show TypeCheckError where
  show (GenTypeCheckError s) = s
  show (AbandonCon c) = printf "Abandon %s" (show c)
  show (TupleMismatch baseM baseExpr m args) = printf "Tuple Apply Mismatch:\n\t(%s %s)(%s) ≠ %s\n\t" (show baseM) (show baseExpr) args' (show m)
    where
      showArg (argName, argVal) = printf "%s = %s" argName (show argVal)
      args' = intercalate ", " $ map showArg $ H.toList args
  show (TCWithMatchingConstraints constraints er) = printf "%s\n\tConstraints: %s" (show er) (show constraints)

instance Show SType where
  show (SType upper lower desc) = concat [show upper, " ⊇ ", desc, " ⊇ ", show lower]
  show (SVar varName p) = printf "SVar %d %s" p (show varName)

instance Show SConstraint where
  show (SEqualsKnown s t) = printf "%s == %s" (show s) (show t)
  show (SEqPoints s1 s2) = printf "%s == %s" (show s1) (show s2)
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

getPnt :: VarMeta -> Pnt
getPnt (VarMeta p _) = p

getPntExpr :: VExpr -> Pnt
getPntExpr = getPnt . getExprMeta

fLookup :: FEnv -> String -> TypeCheckResult VarMeta
fLookup (FEnv _ _ _ pmap) k = case H.lookup k pmap of
  Just v  -> return v
  Nothing -> TypeCheckResE [GenTypeCheckError $ "Failed to lookup " ++ k]

addConstraints :: FEnv -> [Constraint] -> FEnv
addConstraints (FEnv pnts oldCons graph defMap) newCons = FEnv pnts (newCons ++ oldCons) graph defMap

fInsert :: FEnv -> String -> VarMeta -> FEnv
fInsert (FEnv pnts cons graph pmap) k v = FEnv pnts cons graph (H.insert k v pmap)

fAddTypeGraph :: FEnv -> TypeName -> TypeGraphVal -> FEnv
fAddTypeGraph (FEnv pnts cons (unionObj, graph) pmap) k v = FEnv pnts cons (unionObj, H.insertWith (++) k [v] graph) pmap

tryIntersectTypes :: Type -> Type -> String -> TypeCheckResult Type
tryIntersectTypes a b desc= let c = intersectTypes a b
                            in if c == bottomType
                                  then TypeCheckResE [GenTypeCheckError $ "Failed to intersect(" ++ desc ++ "): " ++ show a ++ " --- " ++ show b]
                                  else return c

-- Point operations

descriptor :: FEnv -> Pnt -> Scheme
descriptor (FEnv pnts _ _ _) p = pnts IM.! p

equivalent :: FEnv -> Pnt -> Pnt -> Bool
equivalent (FEnv pnts _ _ _) p1 p2 = (pnts IM.! p1) == (pnts IM.! p2)

fresh :: FEnv -> Scheme -> (Pnt, FEnv)
fresh (FEnv pnts cons typeEnv pmap) scheme = (pnt', FEnv pnts' cons typeEnv pmap)
  where
    pnt' = IM.size pnts
    pnts' = IM.insert pnt' scheme pnts

setDescriptor :: FEnv -> Pnt -> Scheme -> FEnv
setDescriptor (FEnv pnts cons typeEnv pmap) p s = FEnv pnts' cons typeEnv pmap
  where pnts' = IM.insert p s pnts
