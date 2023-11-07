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

import           Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Data.IntMap.Lazy    as IM
import           Data.List
import           GHC.Generics        (Generic)

import           CRes
import           Data.Aeson          (ToJSON, toJSON)
import           Data.Maybe          (catMaybes, fromMaybe, listToMaybe,
                                      mapMaybe)
import           Data.String.Builder (literal)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           Utils

data TypeCheckError
  = GenTypeCheckError CodeRange String
  | TracedTypeCheckError VarMeta [SConstraint] CodeRange String
  | ConstraintTypeCheckError VConstraint SConstraint [TypeCheckError]
  | TupleMismatch TypedMeta TExpr (Meta ()) (H.HashMap String TExpr)
  deriving (Eq, Ord, Generic, Hashable)

-- | SType actual required (description in type)
-- Contains what the type actually is, and what the requirements are (both upper bounds)
-- Covered by the relationship that the actual should be a subset of the required
data SType = SType {
  stypeAct  :: !Type,
  stypeReq  :: !Type,
  stypeDesc :: !String
                   }
  deriving (Eq, Ord, Generic, Hashable, ToJSON)
type Scheme = TypeCheckResult SType

data SchemeActReq = SchemeAct | SchemeReq
  deriving (Eq, Show)

type Pnt = Int

data FEnv = FEnv { fePnts               :: IM.IntMap Scheme
                 , feCons               :: [VConstraint]
                 , feUnionAllObjs       :: VarMeta -- union of all TypeObj for argument inference
                 , feVTypeGraph         :: VTypeGraph
                 , feTTypeGraph         :: TTypeGraph
                 , feUpdatedDuringEpoch :: Bool -- ^ If a pnt is updated during the epoch
                 , feClassGraph         :: ClassGraph
                 , feTrace              :: TraceConstrain
                 } deriving (Show)

type UnionObj = (Pnt, Pnt) -- a union of all TypeObj for argument inference, union of all Object types for function limiting

-- |
-- Constraints represent known relationships between VarMeta formed during Encode
-- Each constraint can affect other the actual type, the required type, or both for the "Scheme".
type CVarArgEnv p = H.HashMap TypeVarAux p
data Constraint p
  = EqualsKnown Int (CVarArgEnv p) p Type -- ^ Both Actual and Req
  | EqPoints Int (CVarArgEnv p) p p -- ^ Both Actual and Req
  | BoundedByKnown Int (CVarArgEnv p) p Type -- ^ Both Actual and Req
  | BoundedByObjs Int (CVarArgEnv p) p
  | ArrowTo Int (CVarArgEnv p) p p -- ArrowTo src dest
  | PropEq Int (CVarArgEnv p) (p, TypeVarAux) p -- ^ Both Actual and Req
  | AddArg Int (CVarArgEnv p) (p, String) p -- ^ Both Actual and Req,
  | AddInferArg Int (CVarArgEnv p) p p -- ^ Both Actual and Req, AddInferArg base arg
  | PowersetTo Int (CVarArgEnv p) p p -- ^ Actual (maybe should make it req too)
  | UnionOf Int (CVarArgEnv p) p [p] -- ^ Both Actual and Req
  deriving (Eq, Ord, Hashable, Generic, ToJSON)

type VConstraint = Constraint VarMeta
type SConstraint = Constraint Scheme
type RConstraint = Constraint SType

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

tcreToMaybe :: TypeCheckResult r -> Maybe r
tcreToMaybe (TypeCheckResult _ r) = Just r
tcreToMaybe TypeCheckResE{}       = Nothing

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
type PGuardExpr = GuardExpr Expr ()
type PCompAnnot = CompAnnot PExpr
type PArrow = Arrow Expr ()
type PObjArg = ObjArg Expr ()
type PObject = Object Expr ()
type PPrgm = Prgm Expr ()
type PObjArr = ObjArr Expr ()
type PEPrgm = ExprPrgm Expr ()
type PPrgmGraphData = GraphData PPrgm String
type PEPrgmGraphData = GraphData PEPrgm String
type InitialPPrgmGraphData = GraphData (ExprPrgm Expr ()) String

data ShowMetaDat = ShowMeta SType VarMetaDat
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)
type ShowMeta = Meta ShowMetaDat
type SExpr = Expr ShowMetaDat
type SCompAnnot = CompAnnot SExpr
type SArrow = Arrow Expr ShowMetaDat
type SObjArg = ObjArg Expr ShowMetaDat
type SObjArr = ObjArr Expr ShowMetaDat
type SObjectMap = ExprObjectMap Expr ShowMetaDat
type SObjectMapItem = ExprObjectMapItem Expr ShowMetaDat
type SPrgm = ExprPrgm Expr ShowMetaDat

data VarMetaDat = VarMetaDat (Maybe Pnt) (Maybe VEObject)
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)
type VarMeta = Meta VarMetaDat
type VExpr = Expr VarMetaDat
type VCompAnnot = CompAnnot VExpr
type VMetaVarArgEnv = MetaVarArgEnv VarMetaDat
type VArrow = Arrow Expr VarMetaDat
type VObjArg = ObjArg Expr VarMetaDat
type VGuardExpr = GuardExpr Expr VarMetaDat
type VObjArr = ObjArr Expr VarMetaDat
type VObject = Object Expr VarMetaDat
type VObjectMap = ObjectMap Expr VarMetaDat
type VObjectMapItem = ObjectMapItem Expr VarMetaDat
type VPrgm = (VObjectMap, ClassGraph, [VCompAnnot])
type VEObject = VExpr
type VEObjectMap = ExprObjectMap Expr VarMetaDat
type VEObjectMapItem = ExprObjectMapItem Expr VarMetaDat
type VEPrgm = (VEObjectMap, ClassGraph, [VCompAnnot])

type TypedMeta = Meta ()
type TExpr = Expr ()
type TCompAnnot = CompAnnot TExpr
type TGuardExpr = GuardExpr Expr ()
type TArrow = Arrow Expr ()
type TObjArg = ObjArg Expr ()
type TObject = Object Expr ()
type TObjectMap = ObjectMap Expr ()
type TObjectMapItem = ObjectMapItem Expr ()
type TObjArr = ObjArr Expr ()
type TPrgm = Prgm Expr ()
type FinalTPrgm = ExprPrgm Expr ()
type TEObjectMap = ExprObjectMap Expr ()
type TEObjectMapItem = ExprObjectMapItem Expr ()
type TEPrgm = ExprPrgm Expr ()

-- implicit graph
type VTypeGraph = H.HashMap TypeName [VObjArr] -- H.HashMap (Root tuple name for filtering) [vals]
type TTypeGraph = H.HashMap TypeName [TObjArr] -- H.HashMap (Root tuple name for filtering) [vals]

instance MetaDat VarMetaDat where
  emptyMetaDat = VarMetaDat Nothing Nothing

instance MetaDat ShowMetaDat where
  emptyMetaDat = error "VarMetaDat"

instance Show TypeCheckError where
  show (GenTypeCheckError _ s) = s
  show (TracedTypeCheckError _ trc _ msg) = printf "%s\n%s" msg (showTraceConstrain trc)
  show (ConstraintTypeCheckError c sc subErrs) = printf "Failed to typecheck constraint %s\n\t\tUsing Points: %s\n\t%s" (show sc) (show $ map showCodeRange $ mapMaybe getMetaPos $ constraintMetas c) (intercalate "\n\t" $ map show subErrs)
  show (TupleMismatch baseM baseExpr m args) = printf "Tuple Apply Mismatch:\n\t(%s %s)(%s) ≠ %s\n\t" (show baseM) (show baseExpr) args' (show m)
    where
      showArg (argName, argVal) = printf "%s = %s" argName (show argVal)
      args' = intercalate ", " $ map showArg $ H.toList args

instance CNoteTC TypeCheckError where
  posCNote (GenTypeCheckError pos _)        = pos
  posCNote (TracedTypeCheckError _ _ pos _) = pos
  posCNote (TupleMismatch _ _ m _)          = getMetaPos m
  posCNote ConstraintTypeCheckError{}       = Nothing

  typeCNote _ = CNoteError
  showRecursiveCNote _ n = literal $ show n

instance Show SType where
  show (SType act req desc) = printf "{%s :: ACT %s; REQ  %s}" desc (show act) (show req)

instance (Show p) => Show (Constraint p) where
  show (EqualsKnown i _ s t) = printf "%s %d==_Known %s" (show s) i (show t)
  show (EqPoints i _ s1 s2) = printf "%s %d== %s" (show s1) i (show s2)
  show (BoundedByKnown i _ s t) = printf "%s %d⊆_Known %s" (show s) i (show t)
  show (BoundedByObjs i _ s) = printf "BoundedByObjs%d %s" i (show s)
  show (ArrowTo i _ s d) = printf "%s %d-> %s" (show s) i (show d)
  show (PropEq i _ (s1, n) s2) = printf "(%s).%s %d== %s"  (show s1) (show n) i (show s2)
  show (AddArg i _ (base, arg) res) = printf "(%s)(%s) %d== %s" (show base) arg i (show res)
  show (AddInferArg i _ base res) = printf "(%s)(?) %d== %s" (show base) i (show res)
  show (PowersetTo i _ s t) = printf "𝒫(%s) %d⊇ %s" (show s) i (show t)
  show (UnionOf i _ s _) = printf "SUnionOf %d for %s" i (show s)

instance Show r => Show (TypeCheckResult r) where
  show (TypeCheckResult [] r) = show r
  show (TypeCheckResult notes r) = concat ["TCRes [", show notes, "] (", show r, ")"]
  show (TypeCheckResE notes) = concat ["TCErr [", show notes, "]"]

typeCheckToRes :: TypeCheckResult r -> CRes r
typeCheckToRes tc = case tc of
  TypeCheckResult notes res -> CRes (map MkCNote notes) res
  TypeCheckResE notes       -> CErr (map MkCNote notes)

eqScheme :: FEnv -> TypeVarArgEnv -> Scheme -> Scheme -> Bool
eqScheme FEnv{feClassGraph} vaenv (TypeCheckResult _ (SType ub1 lb1 _)) (TypeCheckResult _ (SType ub2 lb2 _)) = isEqTypeWithEnv feClassGraph vaenv ub1 ub2 && isEqTypeWithEnv feClassGraph vaenv lb1 lb2
eqScheme _ _ a b = a == b

getStypeActReq :: SchemeActReq -> SType -> Type
getStypeActReq SchemeAct SType{stypeAct} = stypeAct
getStypeActReq SchemeReq SType{stypeReq} = stypeReq

resToTypeCheck :: CRes r -> TypeCheckResult r
resToTypeCheck cres = case cres of
  CRes notes res -> TypeCheckResult (map fromCNote notes) res
  CErr notes     -> TypeCheckResE (map fromCNote notes)
  where
    fromCNote :: CNote -> TypeCheckError
    fromCNote note = GenTypeCheckError (posCNote note) (show note)

constraintMetas :: Constraint p -> [p]
constraintMetas (EqualsKnown _ _ p2 _)    = [p2]
constraintMetas (EqPoints _ _ p2 p3)      = [p2, p3]
constraintMetas (BoundedByKnown _ _ p2 _) = [p2]
constraintMetas (BoundedByObjs _ _ p2)    = [p2]
constraintMetas (ArrowTo _ _ p2 p3)       = [p2, p3]
constraintMetas (PropEq _ _ (p2, _) p3)   = [p2, p3]
constraintMetas (AddArg _ _ (p2, _) p3)   = [p2, p3]
constraintMetas (AddInferArg _ _ p2 p3)   = [p2, p3]
constraintMetas (PowersetTo _ _ p2 p3)    = [p2, p3]
constraintMetas (UnionOf _ _ p2 p3s)      = p2:p3s

constraintVarArgEnv :: Constraint p -> CVarArgEnv p
constraintVarArgEnv (EqualsKnown _ vaenv _ _)    = vaenv
constraintVarArgEnv (EqPoints _ vaenv _ _)       = vaenv
constraintVarArgEnv (BoundedByKnown _ vaenv _ _) = vaenv
constraintVarArgEnv (BoundedByObjs _ vaenv _)    = vaenv
constraintVarArgEnv (ArrowTo _ vaenv _ _)        = vaenv
constraintVarArgEnv (PropEq _ vaenv _ _)         = vaenv
constraintVarArgEnv (AddArg _ vaenv _ _)         = vaenv
constraintVarArgEnv (AddInferArg _ vaenv _ _)    = vaenv
constraintVarArgEnv (PowersetTo _ vaenv _ _)     = vaenv
constraintVarArgEnv (UnionOf _ vaenv _ _)        = vaenv

getPnt :: VarMeta -> Maybe Pnt
getPnt (Meta _ _ (VarMetaDat p _)) = p

addConstraints :: FEnv -> [VConstraint] -> FEnv
addConstraints env@FEnv{feCons} newCons = env {feCons = newCons ++ feCons}

fAddVTypeGraph :: FEnv -> TypeName -> VObjArr -> FEnv
fAddVTypeGraph env@FEnv{feVTypeGraph} k v = env {feVTypeGraph = H.insertWith (++) k [v] feVTypeGraph}

fAddTTypeGraph :: FEnv -> TypeName -> TObjArr -> FEnv
fAddTTypeGraph env@FEnv{feTTypeGraph} k v = env {feTTypeGraph = H.insertWith (++) k [v] feTTypeGraph}

-- This ensures schemes are correct
-- It differs from Constrain.checkScheme because it checks for bugs in the internal compiler, not bugs in the user code
verifyScheme :: ClassGraph -> VMetaVarArgEnv -> VarMeta -> Scheme -> Scheme -> Maybe String
verifyScheme classGraph vaenv (Meta _ _ (VarMetaDat _ _)) (TypeCheckResult _ (SType oldAct oldReq _)) (TypeCheckResult _ (SType act req _)) = listToMaybe $ catMaybes [
  if verifySchemeActLowers then Nothing else Just "verifySchemeActLowers",
  if verifySchemeReqLowers then Nothing else Just "verifySchemeReqLowers",
  if verifyCompacted then Nothing else Just "verifyCompacted"
  ]
  where
    verifySchemeActLowers  = case act of
      TypeVar{} -> True
      _         -> isSubtypeOfWithMetaEnv classGraph vaenv act oldAct
    verifySchemeReqLowers  = isSubtypeOfWithMetaEnv classGraph vaenv req oldReq
    verifyCompacted = act == compactType classGraph act
verifyScheme _ _ _ _ _ = Nothing


-- Point operations

descriptor :: FEnv -> VarMeta -> Scheme
descriptor FEnv{fePnts} m = case getPnt m of
  Just p  -> fePnts IM.! p
  Nothing -> pure (SType (getMetaType m) (getMetaType m) "noPnt descriptor")

equivalent :: FEnv -> VarMeta -> VarMeta -> Bool
equivalent env m1 m2 = descriptor env m1 == descriptor env m2

fresh :: FEnv -> Scheme -> (Pnt, FEnv)
fresh env@FEnv{fePnts} scheme = (pnt', env{fePnts = pnts'})
  where
    pnt' = IM.size fePnts
    pnts' = IM.insert pnt' scheme fePnts

setDescriptor :: FEnv -> VConstraint -> VarMeta -> Scheme -> String -> FEnv
setDescriptor env _ (Meta _ _ (VarMetaDat Nothing _)) _ _ = env
setDescriptor env@FEnv{feClassGraph, fePnts, feTrace, feUpdatedDuringEpoch} con m@(Meta _ _ (VarMetaDat (Just p) _)) scheme' msg = env{fePnts = pnts', feTrace = feTrace', feUpdatedDuringEpoch = feUpdatedDuringEpoch || schemeChanged}
  where
    scheme = descriptor env m
    schemeChanged :: Bool
    schemeChanged = case (scheme, scheme') of
      (TypeCheckResult _ (SType TopType{} _ _), TypeCheckResult _ (SType TypeVar{} _ _)) -> True
      _ ->  fromMaybe False $ tcreToMaybe $ do
        showVaenv <- fmap stypeAct <$> mapM (descriptor env) (constraintVarArgEnv con)
        return $ not (eqScheme env showVaenv scheme scheme')
    pnts' = if schemeChanged then IM.insert p scheme' fePnts else fePnts -- Only update if changed to avoid meaningless updates
    feTrace' = if schemeChanged
      then case verifyScheme feClassGraph (constraintVarArgEnv con) m scheme scheme' of
             Just failVerification -> error $ printf "Scheme failed verification %s during typechecking of %s:\n\t\t New Scheme: %s \n\t\t Old Scheme: %s\n\t\t Obj: %s" failVerification msg (show scheme') (show scheme) (show m)
             Nothing -> case feTrace of
              ((curConstraint, curChanged):curEpoch):prevEpochs -> ((curConstraint, (p, scheme'):curChanged):curEpoch):prevEpochs
              _ -> error "no epochs in feTrace"
      else feTrace

pointUb :: FEnv -> VarMeta -> TypeCheckResult Type
pointUb env p = do
  (SType ub _ _) <- descriptor env p
  return ub

resolveTypeVar :: TypeVarAux -> VConstraint -> TypeCheckResult VarMeta
resolveTypeVar v con = case H.lookup v (constraintVarArgEnv con) of
  Just m' -> return m'
  Nothing -> TypeCheckResE [GenTypeCheckError Nothing $ printf "Unknown variable in resolveTypeVar var: %s" (show v)]

descriptorVaenv :: FEnv -> CVarArgEnv VarMeta -> CVarArgEnv Scheme
descriptorVaenv env = fmap (descriptor env)

descriptorSTypeVaenv :: FEnv -> CVarArgEnv VarMeta -> TypeCheckResult STypeVarArgEnv
descriptorSTypeVaenv env vaenv = sequence $ descriptorVaenv env vaenv

type STypeVarArgEnv = H.HashMap TypeVarAux SType
descriptorConVaenv :: FEnv -> VConstraint -> TypeCheckResult STypeVarArgEnv
descriptorConVaenv env con = descriptorSTypeVaenv env (constraintVarArgEnv con)

-- trace constrain
type TraceConstrainEpoch = [(VConstraint, [(Pnt, Scheme)])]
type TraceConstrain = [TraceConstrainEpoch]
type STraceConstrain = [(SConstraint, [(Pnt, Scheme)])]

nextConstrainEpoch :: FEnv -> FEnv
nextConstrainEpoch env@FEnv{feTrace} = case feTrace of
  []         -> env
  prevEpochs -> env{feTrace = []:prevEpochs, feUpdatedDuringEpoch = False}

startConstraint :: VConstraint -> FEnv -> FEnv
startConstraint c env@FEnv{feTrace = curEpoch:prevEpochs} = env{feTrace = ((c, []):curEpoch):prevEpochs}
startConstraint _ _ = error "bad input to startConstraint"

showTraceConstrain :: [SConstraint] -> String
showTraceConstrain epochs = intercalate "\n" $ map showConstraintPair $ reverse epochs
  where
    showConstraintPair pair = show pair
