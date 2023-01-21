--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Common
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines the common types and structures used for
-- typechecking.
--------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module TypeCheck.Common where

import qualified Data.HashMap.Strict as H
import           Data.Hashable
import qualified Data.IntMap.Lazy    as IM
import           Data.List
import           GHC.Generics        (Generic)

import           CRes
import           Data.Aeson          (ToJSON, toJSON)
import           Data.Maybe
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           Utils

data TypeCheckError
  = GenTypeCheckError CodeRange String
  | TupleMismatch TypedMeta TExpr (Meta ()) (H.HashMap String TExpr)
  deriving (Eq, Ord, Generic, Hashable)

data SType = SType Type Type String -- SType upper lower (description in type)
  deriving (Eq, Ord, Generic, Hashable, ToJSON)
type Scheme = TypeCheckResult SType

type Pnt = Int

data EnvDef = DefVar VarMeta | DefKnown Type
  deriving (Eq, Show, Generic, Hashable, ToJSON)
type EnvValMap = (H.HashMap String EnvDef)
data FEnv = FEnv { fePnts               :: IM.IntMap Scheme
                 , feCons               :: [Constraint]
                 , feUnionAllObjs       :: VarMeta -- union of all TypeObj for argument inference
                 , feVTypeGraph         :: VTypeGraph
                 , feTTypeGraph         :: TTypeGraph
                 , feUpdatedDuringEpoch :: Bool -- ^ If a pnt is updated during the epoch
                 , feClassGraph         :: ClassGraph
                 , feDefMap             :: EnvValMap
                 , feTrace              :: TraceConstrain
                 } deriving (Show)

type UnionObj = (Pnt, Pnt) -- a union of all TypeObj for argument inference, union of all Object types for function limiting

data Constraint
  = EqualsKnown VarMeta Type
  | EqPoints VarMeta VarMeta
  | BoundedByKnown VarMeta Type
  | BoundedByObjs VarMeta
  | ArrowTo VarMeta VarMeta -- ArrowTo src dest
  | PropEq (VarMeta, ArgName) VarMeta
  | VarEq (VarMeta, TypeVarName) VarMeta
  | AddArg (VarMeta, String) VarMeta
  | AddInferArg VarMeta VarMeta -- AddInferArg base arg
  | PowersetTo VarMeta VarMeta
  | UnionOf VarMeta [VarMeta]
  deriving (Show, Generic, ToJSON)

data SConstraint
  = SEqualsKnown Scheme Type
  | SEqPoints Scheme Scheme
  | SBoundedByKnown Scheme Type
  | SBoundedByObjs Scheme
  | SArrowTo Scheme Scheme
  | SPropEq (Scheme, ArgName) Scheme
  | SVarEq (Scheme, TypeVarName) Scheme
  | SAddArg (Scheme, String) Scheme
  | SAddInferArg Scheme Scheme
  | SPowersetTo Scheme Scheme
  | SUnionOf Scheme [Scheme]
  deriving (Eq, Ord, Generic, Hashable)

data TypeCheckResult r
  = TypeCheckResult [TypeCheckError] r
  | TypeCheckResE [TypeCheckError]
  deriving (Eq, Ord, Generic, Hashable)

instance ToJSON r => ToJSON (TypeCheckResult r) where
  toJSON res = toJSON $ typeCheckToRes res

instance MonadFail TypeCheckResult where
  fail s = TypeCheckResE [GenTypeCheckError Nothing s]

getTCRE :: TypeCheckResult r -> [TypeCheckError]
getTCRE (TypeCheckResult notes _) = notes
getTCRE (TypeCheckResE notes)     = notes

instance Functor TypeCheckResult where
  fmap f (TypeCheckResult notes r) = TypeCheckResult notes (f r)
  fmap _ (TypeCheckResE notes)     = TypeCheckResE notes

instance Applicative TypeCheckResult where
  pure = TypeCheckResult []
  (TypeCheckResult notesA f) <*> (TypeCheckResult notesB b) = TypeCheckResult (notesA ++ notesB) (f b)
  resA <*> resB = TypeCheckResE (getTCRE resA ++ getTCRE resB)

instance Monad TypeCheckResult where
  return = pure
  (TypeCheckResult notesA a) >>= f = case f a of
    (TypeCheckResult notesB b) -> TypeCheckResult (notesA ++ notesB) b
    (TypeCheckResE notesB)     -> TypeCheckResE (notesA ++ notesB)
  (TypeCheckResE notes) >>= _ = TypeCheckResE notes


type PreMeta = Meta ()
type PExpr = Expr ()
type PCompAnnot = CompAnnot PExpr
type PGuard = Guard PExpr
type PArrow = Arrow Expr ()
type PObjArg = ObjArg Expr ()
type PObject = Object Expr ()
type PPrgm = Prgm Expr ()
type PPrgmGraphData = GraphData PPrgm String
type InitialPPrgmGraphData = GraphData (ExprPrgm Expr ()) String

data ShowMetaDat = ShowMeta SType VarMetaDat
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)
type ShowMeta = Meta ShowMetaDat
type SExpr = Expr ShowMetaDat
type SCompAnnot = CompAnnot SExpr
type SGuard = Guard SExpr
type SArrow = Arrow Expr ShowMetaDat
type SObjArg = ObjArg Expr ShowMetaDat
type SObject = Object Expr ShowMetaDat
type SObjectMap = ObjectMap Expr ShowMetaDat
type SObjectMapItem = ObjectMapItem Expr ShowMetaDat
type SPrgm = Prgm Expr ShowMetaDat

data VarMetaDat = VarMetaDat Pnt (Maybe VObject) (MetaVarEnv VarMetaDat) (MetaArgEnv VarMetaDat)
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)
type VarMeta = Meta VarMetaDat
type VExpr = Expr VarMetaDat
type VCompAnnot = CompAnnot VExpr
type VGuard = Guard VExpr
type VMetaVarEnv = MetaVarEnv VarMetaDat
type VMetaArgEnv = MetaArgEnv VarMetaDat
type VArrow = Arrow Expr VarMetaDat
type VObjArg = ObjArg Expr VarMetaDat
type VObject = Object Expr VarMetaDat
type VObjectMap = ObjectMap Expr VarMetaDat
type VObjectMapItem = ObjectMapItem Expr VarMetaDat
type VPrgm = (VObjectMap, ClassGraph, [VCompAnnot])

type TypedMeta = Meta ()
type TExpr = Expr ()
type TCompAnnot = CompAnnot TExpr
type TGuard = Guard TExpr
type TArrow = Arrow Expr ()
type TObjArg = ObjArg Expr ()
type TObject = Object Expr ()
type TObjectMap = ObjectMap Expr ()
type TObjectMapItem = ObjectMapItem Expr ()
type TPrgm = Prgm Expr ()
type FinalTPrgm = ExprPrgm Expr ()

-- implicit graph
type VTypeGraphVal = (VObject, VArrow) -- (match object type, if matching then can implicit to type in arrow)
type VTypeGraph = H.HashMap TypeName [VTypeGraphVal] -- H.HashMap (Root tuple name for filtering) [vals]
type TTypeGraphVal = (TObject, TArrow) -- (match object type, if matching then can implicit to type in arrow)
type TTypeGraph = H.HashMap TypeName [TTypeGraphVal] -- H.HashMap (Root tuple name for filtering) [vals]

instance MetaDat VarMetaDat where
  emptyMetaDat = error "VarMetaDat"

instance MetaDat ShowMetaDat where
  emptyMetaDat = error "VarMetaDat"

instance Show TypeCheckError where
  show (GenTypeCheckError _ s) = s
  show (TupleMismatch baseM baseExpr m args) = printf "Tuple Apply Mismatch:\n\t(%s %s)(%s) ≠ %s\n\t" (show baseM) (show baseExpr) args' (show m)
    where
      showArg (argName, argVal) = printf "%s = %s" argName (show argVal)
      args' = intercalate ", " $ map showArg $ H.toList args

instance CNoteTC TypeCheckError where
  posCNote (GenTypeCheckError pos _) = pos
  posCNote (TupleMismatch _ _ m _)   = getMetaPos m

  typeCNote _ = CNoteError

instance Show SType where
  show (SType upper lower desc) = concat [show upper, " ⊇ ", desc, " ⊇ ", show lower]

instance Show SConstraint where
  show (SEqualsKnown s t) = printf "%s == %s" (show s) (show t)
  show (SEqPoints s1 s2) = printf "%s == %s" (show s1) (show s2)
  show (SBoundedByKnown s t) = printf "%s ⊆ %s" (show s) (show t)
  show (SBoundedByObjs s) = printf "BoundedByObjs %s" (show s)
  show (SArrowTo f t) = printf "%s -> %s" (show t) (show f)
  show (SPropEq (s1, n) s2) = printf "(%s).%s == %s"  (show s1) n (show s2)
  show (SVarEq (s1, n) s2) = printf "(%s).%s == %s"  (show s1) n (show s2)
  show (SAddArg (base, arg) res) = printf "(%s)(%s) == %s" (show base) arg (show res)
  show (SAddInferArg base res) = printf "(%s)(?) == %s" (show base) (show res)
  show (SPowersetTo s t) = printf "𝒫(%s) ⊇ %s" (show s) (show t)
  show (SUnionOf s _) = printf "SUnionOf for %s" (show s)

instance Show r => Show (TypeCheckResult r) where
  show (TypeCheckResult [] r) = show r
  show (TypeCheckResult notes r) = concat ["TCRes [", show notes, "] (", show r, ")"]
  show (TypeCheckResE notes) = concat ["TCErr [", show notes, "]"]

typeCheckToRes :: TypeCheckResult r -> CRes r
typeCheckToRes tc = case tc of
  TypeCheckResult notes res -> CRes (map MkCNote notes) res
  TypeCheckResE notes       -> CErr (map MkCNote notes)

resToTypeCheck :: CRes r -> TypeCheckResult r
resToTypeCheck cres = case cres of
  CRes notes res -> TypeCheckResult (map fromCNote notes) res
  CErr notes     -> TypeCheckResE (map fromCNote notes)
  where
    fromCNote :: CNote -> TypeCheckError
    fromCNote note = GenTypeCheckError (posCNote note) (show note)

getPnt :: VarMeta -> Pnt
getPnt (Meta _ _ (VarMetaDat p _ _ _)) = p

fLookup :: FEnv -> Maybe VObject -> String -> TypeCheckResult EnvDef
fLookup FEnv{feDefMap} obj k = case suffixLookup k (H.keys feDefMap) of
  Just key -> case suffixLookupInDict key feDefMap of
    Just v  -> return v
    Nothing -> TypeCheckResE [GenTypeCheckError Nothing $ printf "Failed to lookup %s in %s with keys %s" k (show obj) (show $ H.keys feDefMap)]
  Nothing -> TypeCheckResE [GenTypeCheckError Nothing $ printf "Failed to lookup %s in %s with keys %s" k (show obj) (show $ H.keys feDefMap)]

addConstraints :: FEnv -> [Constraint] -> FEnv
addConstraints env@FEnv{feCons} newCons = env {feCons = newCons ++ feCons}

fInsert :: FEnv -> String -> EnvDef -> FEnv
fInsert env@FEnv{feDefMap} k v = env{feDefMap = H.insert k v feDefMap}

fAddVTypeGraph :: FEnv -> TypeName -> VTypeGraphVal -> FEnv
fAddVTypeGraph env@FEnv{feVTypeGraph} k v = env {feVTypeGraph = H.insertWith (++) k [v] feVTypeGraph}

fAddTTypeGraph :: FEnv -> TypeName -> TTypeGraphVal -> FEnv
fAddTTypeGraph env@FEnv{feTTypeGraph} k v = env {feTTypeGraph = H.insertWith (++) k [v] feTTypeGraph}

tryIntersectTypes :: FEnv -> Type -> Type -> String -> TypeCheckResult Type
tryIntersectTypes FEnv{feClassGraph} a b desc = let c = intersectTypes feClassGraph a b
                                                            in if isBottomType c
                                                                  then TypeCheckResE [GenTypeCheckError Nothing $ "Failed to intersect(" ++ desc ++ "): " ++ show a ++ " --- " ++ show b]
                                                                  else return c

-- This ensures schemes are correct
-- It differs from Constrain.checkScheme because it checks for bugs in the internal compiler, not bugs in the user code
verifyScheme :: ClassGraph -> VarMeta -> Scheme -> Scheme -> Maybe String
verifyScheme classGraph (Meta _ _ (VarMetaDat _ _ varEnv argEnv)) (TypeCheckResult _ (SType oldUb _ _)) (TypeCheckResult _ (SType ub _ _)) = listToMaybe $ catMaybes [
  if verifyTypeVars (H.keys varEnv) ub then Nothing else Just "verifyTypeVars",
  if verifySchemeUbLowers then Nothing else Just "verifySchemeUbLowers",
  if verifyCompacted then Nothing else Just "verifyCompacted"
  ]
  where
    verifyTypeVars venv (UnionType partialLeafs) = all (verifyTypeVarsPartial venv) $ splitUnionType partialLeafs
    verifyTypeVars venv (TypeVar (TVVar v)) = isJust $ suffixLookup v venv
    verifyTypeVars _ _ = True
    verifyTypeVarsPartial venv PartialType{ptVars, ptArgs} = all (verifyTypeVars venv) ptVars
                                                                        && all (verifyTypeVars (H.keys ptVars)) ptArgs

    verifySchemeUbLowers  = isSubtypeOfWithMetaEnv classGraph varEnv argEnv ub oldUb
    verifyCompacted = ub == compactType classGraph ub
verifyScheme _ _ _ _ = Nothing


-- Point operations

descriptor :: FEnv -> VarMeta -> Scheme
descriptor FEnv{fePnts} p = fePnts IM.! getPnt p

equivalent :: FEnv -> VarMeta -> VarMeta -> Bool
equivalent FEnv{fePnts} m1 m2 = (fePnts IM.! getPnt m1) == (fePnts IM.! getPnt m2)

fresh :: FEnv -> Scheme -> (Pnt, FEnv)
fresh env@FEnv{fePnts} scheme = (pnt', env{fePnts = pnts'})
  where
    pnt' = IM.size fePnts
    pnts' = IM.insert pnt' scheme fePnts

setDescriptor :: FEnv -> VarMeta -> Scheme -> String -> FEnv
setDescriptor env@FEnv{feClassGraph, fePnts, feTrace, feUpdatedDuringEpoch} m scheme' msg = env{fePnts = pnts', feTrace = feTrace', feUpdatedDuringEpoch = feUpdatedDuringEpoch || schemeChanged}
  where
    p = getPnt m
    scheme = fePnts IM.! p
    schemeChanged = scheme /= scheme'
    pnts' = IM.insert p scheme' fePnts
    feTrace' = if schemeChanged
      then case verifyScheme feClassGraph m scheme scheme' of
             Just failVerification -> error $ printf "Scheme failed verification %s during typechecking of %s: %s \n\t\t in obj: %s with old scheme: %s" failVerification msg (show scheme') (show m) (show scheme)
             Nothing -> case feTrace of
              ((curConstraint, curChanged):curEpoch):prevEpochs -> ((curConstraint, (p, scheme'):curChanged):curEpoch):prevEpochs
              _ -> error "no epochs in feTrace"
      else feTrace

pointUb :: FEnv -> VarMeta -> TypeCheckResult Type
pointUb env p = do
  (SType ub _ _) <- descriptor env p
  return ub

resolveTypeVar :: TypeVarAux -> VarMeta -> TypeCheckResult VarMeta
resolveTypeVar (TVVar v) m@(Meta _ _ (VarMetaDat _ _ objVars _)) = case H.lookup v objVars of
  Just m' -> return m'
  Nothing -> TypeCheckResE [GenTypeCheckError (getMetaPos m) "Unknown variable in resolveTypeVar var"]
resolveTypeVar (TVArg v) m@(Meta _ _ (VarMetaDat _ _ _ argEnv)) = case H.lookup v argEnv of
  Just m' -> return m'
  Nothing -> TypeCheckResE [GenTypeCheckError (getMetaPos m) "Unknown variable in resolveTypeVar arg"]

descriptorResolve :: FEnv -> VarMeta -> TypeCheckResult (VarMeta, SType)
descriptorResolve env m = do
  scheme@(SType ub _ _) <- descriptor env m
  case ub of
    (TypeVar v) -> do
      m' <- resolveTypeVar v m
      descriptorResolve env m'
    _ -> return (m, scheme)

-- trace constrain
type TraceConstrainEpoch = [(Constraint, [(Pnt, Scheme)])]
type TraceConstrain = [TraceConstrainEpoch]

nextConstrainEpoch :: FEnv -> FEnv
nextConstrainEpoch env@FEnv{feTrace} = case feTrace of
  []         -> env
  prevEpochs -> env{feTrace = []:prevEpochs, feUpdatedDuringEpoch = False}

startConstraint :: Constraint -> FEnv -> FEnv
startConstraint c env@FEnv{feTrace = curEpoch:prevEpochs} = env{feTrace = ((c, []):curEpoch):prevEpochs}
startConstraint _ _ = error "bad input to startConstraint"
