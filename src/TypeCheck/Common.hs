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
import qualified Data.HashMap.Strict        as H
import qualified Data.IntMap.Lazy           as IM
import           Data.List
import           GHC.Generics               (Generic)

import           Control.Monad.State
import           Control.Monad.Trans.Writer (execWriter, tell)
import           CRes
import           Data.Aeson                 (ToJSON, toJSON)
import           Data.Maybe                 (catMaybes, fromJust, fromMaybe,
                                             listToMaybe, mapMaybe, maybeToList)
import           Data.String.Builder        (literal)
import           MapMeta                    (MetaType (ArrMeta), clearMetaDat)
import           Semantics
import           Semantics.Prgm
import           Semantics.TypeGraph        (ReachesTree)
import           Semantics.Types
import           Text.Printf
import           Utils

data TypeCheckError
  = GenTypeCheckError (Maybe (Meta ())) String
  | TracedTypeCheckError VarMeta [SConstraint] String
  | ConstraintTypeCheckError VConstraint SConstraint [TypeCheckError]
  | TupleMismatch TypedMeta TExpr (Meta ()) (H.HashMap String TExpr)
  deriving (Eq, Ord, Generic, Hashable)

-- | SType actual required (description in type)
-- Contains what the type actually is, and what the requirements are (both upper bounds)
-- Covered by the relationship that the actual should be a subset of the required
data SType = SType {
  stypeAct  :: !Type,
  stypeReq  :: !Type,
  stypeTree :: !(Maybe ReachesTree),
  stypeDesc :: !String
                   }
  deriving (Eq, Ord, Generic, Hashable, ToJSON)
type Scheme = TypeCheckResult SType

data SchemeActReq = SchemeAct | SchemeReq
  deriving (Eq, Show)

type Pnt = Int

type FEnvTypeEnv = TypeEnv (ObjArrTypeGraph Expr ())
data FEnv = FEnv { fePnts               :: IM.IntMap Scheme
                 , feConsDats           :: [[VConstraint]]
                 , feCons               :: [VConstraint]
                 , feUnionAllObjs       :: VarMeta -- union of all TypeObj for argument inference
                 , feVTypeGraph         :: VTypeGraph
                 , feTTypeGraph         :: TTypeGraph
                 , feUpdatedDuringEpoch :: Bool -- ^ If a pnt is updated during the epoch
                 , feTypeEnv            :: FEnvTypeEnv
                 , feTrace              :: TraceConstrain
                 } deriving (Show)

type UnionObj = (Pnt, Pnt) -- a union of all TypeObj for argument inference, union of all Object types for function limiting

-- |
-- Constraints represent known relationships between VarMeta formed during Encode
-- Each constraint can affect other the actual type, the required type, or both for the "Scheme".
type CVarArgEnv p = H.HashMap TypeVarAux (p, p)
type COVarArgEnv p = H.HashMap TypeVarAux p
data ConstraintDat p
  = EqualsKnown Int p Type -- ^ Both Actual and Req
  | EqPoints Int p p -- ^ Both Actual and Req
  | BoundedByKnown Int p Type -- ^ Both Actual and Req
  | BoundedByObjs Int p Type
  | NoReturnArg Int p
  | ArrowTo Int p p -- ArrowTo src dest
  | PropEq Int (p, TypeVarAux) p -- ^ Both Actual and Req
  | AddArg Int (p, TypeVarAux) p -- ^ Both Actual and Req,
  | AddInferArg Int p p -- ^ Both Actual and Req, AddInferArg base arg
  | SetArgMode Int Bool p p -- ^ Actual (maybe should make it req too). Bool is true for powerset and false for spread.
  | ConWhere Int p p p -- ^ Where has a predicates
  | UnionOf Int p [p] -- ^ Both Actual and Req
  deriving (Eq, Ord, Hashable, Generic, ToJSON)

data Constraint p = Constraint {
  conOs    :: [VObjArr],
  conVaenv :: CVarArgEnv p,
  conDat   :: ConstraintDat p
                               }
  deriving (Eq, Ord, Hashable, Generic, ToJSON)
type VConstraintDat = ConstraintDat VarMeta
type VConstraint = Constraint VarMeta
type SConstraint = Constraint Scheme
type SConstraintDat = ConstraintDat Scheme
type RConstraint = Constraint SType
type RConstraintDat = ConstraintDat SType

mapMCon :: Monad m => (p1 -> m p2) -> Constraint p1 -> m (Constraint p2)
mapMCon f (Constraint oas vaenv dat) = do
  vaenv' <- forM vaenv $ \(va, vb) -> do
    va' <- f va
    vb' <- f vb
    return (va', vb')
  dat' <- mapMConDat f dat
  return $ Constraint oas vaenv' dat'

mapMConDat :: Monad m => (p1 -> m p2) -> ConstraintDat p1 -> m (ConstraintDat p2)
mapMConDat f (EqualsKnown i p t) = do
  p' <- f p
  return $ EqualsKnown i p' t
mapMConDat f (EqPoints i p1 p2) = do
  p1' <- f p1
  EqPoints i p1' <$> f p2
mapMConDat f (BoundedByKnown i p t) = do
  p' <- f p
  return $ BoundedByKnown i p' t
mapMConDat f (BoundedByObjs i p t) = do
  p' <- f p
  return $ BoundedByObjs i p' t
mapMConDat f (NoReturnArg i p) = NoReturnArg i <$> f p
mapMConDat f (ArrowTo i p1 p2) = do
  p1' <- f p1
  ArrowTo i p1' <$> f p2
mapMConDat f (PropEq i (p1, name) p2) = do
  p1' <- f p1
  PropEq i (p1', name) <$> f p2
mapMConDat f (AddArg i (p1, argName) p2) = do
  p1' <- f p1
  AddArg i (p1', argName) <$> f p2
mapMConDat f (AddInferArg i p1 p2) = do
  p1' <- f p1
  AddInferArg i p1' <$> f p2
mapMConDat f (SetArgMode i m p1 p2) = do
  p1' <- f p1
  SetArgMode i m p1' <$> f p2
mapMConDat f (ConWhere i p1 p2 p3) = do
  p1' <- f p1
  p2' <- f p2
  ConWhere i p1' p2' <$> f p3
mapMConDat f (UnionOf i p1 p2s) = do
  p1' <- f p1
  p2s' <- mapM f p2s
  return $ UnionOf i p1' p2s'

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
type PCompAnnot = CompAnnot PExpr
type PObjArr = ObjArr Expr ()
type PPrgm = Prgm Expr ()
type PPrgmGraphData = GraphData PPrgm FileImport

data ShowMetaDat = ShowMeta SType VarMetaDat
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)
type ShowMeta = Meta ShowMetaDat
type SExpr = Expr ShowMetaDat
type SCompAnnot = CompAnnot SExpr
type SObjArr = ObjArr Expr ShowMetaDat
type SPrgm = Prgm Expr ShowMetaDat

data VarMetaDat = VarMetaDat (Maybe Pnt) (Maybe VObject)
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)
type VarMeta = Meta VarMetaDat
type VExpr = Expr VarMetaDat
type VCompAnnot = CompAnnot VExpr
type VObjArr = ObjArr Expr VarMetaDat
type VObject = VExpr
type VObjectMap = ObjectMap Expr VarMetaDat
type VObjectMapItem = ObjArr Expr VarMetaDat
type VPrgm = (VObjectMap, ClassGraph, [VCompAnnot])

type TypedMetaDat = Maybe ReachesTree
type TypedMeta = Meta TypedMetaDat
type TExpr = Expr TypedMetaDat
type TCompAnnot = CompAnnot TExpr
type TObjArr = ObjArr Expr TypedMetaDat
type TObjectMap = ObjectMap Expr TypedMetaDat
type TPrgm = Prgm Expr TypedMetaDat

-- implicit graph
type VTypeGraph = H.HashMap TypeName [VObjArr] -- H.HashMap (Root tuple name for filtering) [vals]
type TTypeGraph = H.HashMap TypeName [TObjArr] -- H.HashMap (Root tuple name for filtering) [vals]

instance MetaDat VarMetaDat where
  emptyMetaDat = VarMetaDat Nothing Nothing

instance MetaDat ShowMetaDat where
  emptyMetaDat = error "VarMetaDat"

instance Show TypeCheckError where
  show (GenTypeCheckError _ s) = s
  show (TracedTypeCheckError _ trc msg) = printf "%s\n%s" msg (showTraceConstrain trc)
  show (ConstraintTypeCheckError c sc subErrs) = printf "Failed to typecheck constraint %s\n\t\tUsing Points: %s\n\t%s" (show sc) (show $ map showCodeRange $ mapMaybe getMetaPos $ constraintMetas c) (intercalate "\n\t" $ map show subErrs)
  show (TupleMismatch baseM baseExpr m args) = printf "Tuple Apply Mismatch:\n\t(%s %s)(%s) ≠ %s\n\t" (show baseM) (show baseExpr) args' (show m)
    where
      showArg (argName, argVal) = printf "%s = %s" argName (show argVal)
      args' = intercalate ", " $ map showArg $ H.toList args

instance CNoteTC TypeCheckError where
  metaCNote (GenTypeCheckError m _)      = m
  metaCNote (TracedTypeCheckError m _ _) = clearMetaDat ArrMeta m
  metaCNote (TupleMismatch _ _ m _)      = Just m
  metaCNote ConstraintTypeCheckError{}   = Nothing

  typeCNote _ = CNoteError
  showRecursiveCNote _ n = literal $ show n

instance Show SType where
  show (SType act req _ desc) = printf "{%s :: ACT %s; REQ  %s}" desc (show act) (show req)

instance (Show p) => Show (ConstraintDat p) where
  show (EqualsKnown i s t) = printf "%s %d==_Known %s" (show s) i (show t)
  show (EqPoints i s1 s2) = printf "%s %d== %s" (show s1) i (show s2)
  show (BoundedByKnown i s t) = printf "%s %d⊆_Known %s" (show s) i (show t)
  show (BoundedByObjs i s _) = printf "BoundedByObjs%d %s" i (show s)
  show (NoReturnArg i s) = printf "NoReturnArg%d %s" i (show s)
  show (ArrowTo i s d) = printf "%s %d-> %s" (show s) i (show d)
  show (PropEq i (s1, n) s2) = printf "(%s).(%s) %d== %s"  (show s1) (show n) i (show s2)
  show (AddArg i (base, arg) res) = printf "(%s)(%s) %d== %s" (show base) (show arg) i (show res)
  show (AddInferArg i base res) = printf "(%s)(?) %d== %s" (show base) i (show res)
  show (SetArgMode i True s t) = printf "𝒫(%s) %d⊇ %s" (show s) i (show t)
  show (SetArgMode i False s t) = printf "%s.. %d⊇ %s" (show s) i (show t)
  show (ConWhere i b c r) = printf "%s %d| %s = %s" (show b) i (show c) (show r)
  show (UnionOf i s _) = printf "SUnionOf %d for %s" i (show s)

instance (Show p) => Show (Constraint p) where
  show (Constraint _ _ d) = show d

instance Show r => Show (TypeCheckResult r) where
  show (TypeCheckResult [] r) = show r
  show (TypeCheckResult notes r) = concat ["TCRes [", show notes, "] (", show r, ")"]
  show (TypeCheckResE notes) = concat ["TCErr [", show notes, "]"]

newtype SConstraintEpoch = SConstraintEpoch [(SConstraint, [(Pnt, Scheme)])]
instance Show SConstraintEpoch where
  show (SConstraintEpoch cons) = intercalate "\n" $ map showCon cons
    where
      showCon (con, pnts) = printf "Updating points %s with %s" (show $ map fst pnts) (show con)

newtype SConstrainPnt = SConstrainPnt [[(SConstraint, Scheme)]]
instance Show SConstrainPnt where
  show (SConstrainPnt epochs) = intercalate "\n" $ zipWith (curry showEpoch) [1..] (reverse $ tail epochs)
    where
      showEpoch :: (Integer, [(SConstraint, Scheme)]) -> String
      showEpoch (epoch, changes) = printf "Epoch %d: \n\t%s" epoch (intercalate "\n\t" $ map showChange changes)
      showChange (con, scheme) = printf "To %s from %s" (show scheme) (show con)


typeCheckToRes :: TypeCheckResult r -> CRes r
typeCheckToRes tc = case tc of
  TypeCheckResult notes res -> CRes (map MkCNote notes) res
  TypeCheckResE notes       -> CErr (map MkCNote notes)

eqScheme :: FEnv -> TypeVarArgEnv -> Scheme -> Scheme -> Bool
eqScheme FEnv{feTypeEnv} vaenv (TypeCheckResult _ (SType ub1 lb1 _ _)) (TypeCheckResult _ (SType ub2 lb2 _ _)) = isEqTypeWithEnv feTypeEnv vaenv ub1 ub2 && isEqTypeWithEnv feTypeEnv vaenv lb1 lb2
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
    fromCNote note = GenTypeCheckError (metaCNote note) (show note)

constraintDatMetas :: ConstraintDat p -> [p]
constraintDatMetas conDat = execWriter $ mapMConDat f conDat
  where
    f p = do
        tell [p]
        return p

constraintMetas :: Constraint p -> [p]
constraintMetas (Constraint _ _ d) = constraintDatMetas d

constraintVarArgEnvIO :: Constraint p -> CVarArgEnv p
constraintVarArgEnvIO (Constraint _ vaenv _) = vaenv

constraintVarArgEnv :: Constraint p -> COVarArgEnv p
constraintVarArgEnv = fmap snd . constraintVarArgEnvIO

getPnt :: VarMeta -> Maybe Pnt
getPnt Meta{getMetaDat=VarMetaDat p _} = p

addConstraints :: [VConstraintDat] -> FEnv -> FEnv
addConstraints newCons env@FEnv{feConsDats=curCons:parentCons} = env {feConsDats = (map (Constraint [] H.empty) newCons ++ curCons):parentCons}
addConstraints _ FEnv{feConsDats=[]} = error $ printf "No constraint stack found in addConstraints"

-- | Starts a block of constraints, adding it to the block stack, where each block is associated with an ObjArr and its varargs
-- | In higher order functions, it is associated with both the main ObjArr and the higher order definition
startConstrainBlock :: FEnv -> FEnv
startConstrainBlock env@FEnv{feConsDats} = env{feConsDats=[]:feConsDats}

-- | Ends  the constraint block, popping it off the stack and adding its contents to either the block underneath or to the list of finished constraints
endConstraintBlock :: Maybe VObjArr -> CVarArgEnv VarMeta -> FEnv -> FEnv
endConstraintBlock oa vaenv env@FEnv{feConsDats=[newCons], feCons} = env{feConsDats=[], feCons = map (\(Constraint oas vaenvs d) -> Constraint (maybeToList oa ++ oas) (H.union vaenv vaenvs) d) newCons ++ feCons}
endConstraintBlock oa vaenv env@FEnv{feConsDats=d1:d2:ds} = env{feConsDats=(map (\(Constraint oas vaenvs d) -> Constraint (maybeToList oa ++ oas) (H.union vaenv vaenvs) d) d1 ++ d2):ds}
endConstraintBlock _ _ FEnv{feConsDats=[]} = error $ printf "No constraint stack found in endConstraintBlock"

fAddVTypeGraph :: TypeName -> VObjArr -> FEnv -> FEnv
fAddVTypeGraph k v env@FEnv{feVTypeGraph} = env {feVTypeGraph = H.insertWith (++) (makeAbsoluteName k) [v] feVTypeGraph}

fAddTTypeGraph :: TypeName -> TObjArr -> FEnv -> FEnv
fAddTTypeGraph k v env@FEnv{feTTypeGraph} = env {feTTypeGraph = H.insertWith (++) (makeAbsoluteName k) [v] feTTypeGraph}

-- This ensures schemes are correct
-- It differs from Constrain.checkScheme because it checks for bugs in the internal compiler, not bugs in the user code
verifyScheme :: TypeEnv tg -> TypeVarArgEnv -> VarMeta -> Scheme -> Scheme -> Maybe String
verifyScheme typeEnv vaenv Meta{getMetaDat=VarMetaDat _ _} (TypeCheckResult _ (SType oldAct oldReq _ _)) (TypeCheckResult _ (SType act req _ _)) = listToMaybe $ catMaybes [
  if verifySchemeActLowers then Nothing else Just (printf "verifySchemeActLowers\n\t\tGrows by: %s" (show $ differenceTypeWithEnv typeEnv vaenv act oldAct)),
  if verifySchemeReqLowers then Nothing else Just "verifySchemeReqLowers",
  if verifyCompacted then Nothing else Just (printf "verifyCompacted\n\t\tCan be compacted to %s" (show $ compactType typeEnv vaenv act))
  ]
  where
    verifySchemeActLowers  = case act of
      TypeVar{} -> True
      _         -> isSubtypeOfWithEnv typeEnv vaenv act oldAct
    verifySchemeReqLowers  = isSubtypeOfWithEnv typeEnv vaenv req oldReq
    verifyCompacted = act == compactType typeEnv vaenv act
verifyScheme _ _ _ _ _ = Nothing


-- Point operations
descriptorPnt :: FEnv -> Pnt -> Scheme
descriptorPnt FEnv{fePnts} p = fePnts IM.! p

descriptor :: FEnv -> VarMeta -> Scheme
descriptor env m = case getPnt m of
  Just p  -> descriptorPnt env p
  Nothing -> pure (SType (getMetaType m) (getMetaType m) Nothing "noPnt descriptor")

equivalent :: FEnv -> VarMeta -> VarMeta -> Bool
equivalent env m1 m2 = descriptor env m1 == descriptor env m2

fresh :: Scheme -> StateT FEnv TypeCheckResult Pnt
fresh  scheme = do
  env@FEnv{fePnts, feTrace=tc@TraceConstrain{tcInitial}} <- get
  let pnt' = IM.size fePnts
  let pnts' = IM.insert pnt' scheme fePnts
  let feTrace' = tc{tcInitial=H.insert pnt' scheme tcInitial}
  put env{fePnts = pnts', feTrace=feTrace'}
  return pnt'

setDescriptor :: FEnv -> VConstraint -> VarMeta -> Scheme -> String -> FEnv
setDescriptor env _ Meta{getMetaDat=VarMetaDat Nothing _} _ _ = env
setDescriptor env@FEnv{feTypeEnv, fePnts, feTrace, feUpdatedDuringEpoch} con m@Meta{getMetaDat=VarMetaDat (Just p) _} scheme' msg = env{fePnts = pnts', feTrace = feTrace', feUpdatedDuringEpoch = feUpdatedDuringEpoch || schemeChanged}
  where
    scheme = descriptor env m
    vaenv = fmap stypeAct <$> mapM (descriptor env) (constraintVarArgEnv con)
    schemeChanged :: Bool
    schemeChanged = case (scheme, scheme') of
      (TypeCheckResult _ SType{stypeAct=TopType{}}, TypeCheckResult _ SType{stypeAct=TypeVar{}}) -> True
      _ ->  fromMaybe False $ tcreToMaybe $ do
        showVaenv <- vaenv
        return $ not (eqScheme env showVaenv scheme scheme')
    scheme'' = case verifyScheme feTypeEnv (fromJust $ tcreToMaybe vaenv) m scheme scheme' of
      _ | not schemeChanged -> scheme'
      -- Just failVerification -> error $ printf "Scheme failed verification %s\n\t\tDuring typechecking of %s:\n\t\t New Scheme: %s \n\t\t Old Scheme: %s\n\t\t Obj: %s\n\t\t Con: %s" failVerification msg (show scheme') (show scheme) (show m) (show con)
      Just failVerification -> TypeCheckResE [GenTypeCheckError (clearMetaDat ArrMeta m) $ printf "Scheme failed verification %s\n\t\tDuring typechecking of %s:\n\t\t New Scheme: %s \n\t\t Old Scheme: %s\n\t\t Obj: %s\n\t\t Con: %s" failVerification msg (show scheme') (show scheme) (show m) (show con)]
      Nothing -> scheme'
    pnts' = if schemeChanged then IM.insert p scheme'' fePnts else fePnts -- Only update if changed to avoid meaningless updates
    feTrace' = if schemeChanged
      then traceConstrainChange feTrace p scheme''
      else feTrace

pointUb :: FEnv -> VarMeta -> TypeCheckResult Type
pointUb env p = stypeAct <$> descriptor env p

resolveTypeVar :: TypeVarAux -> VConstraint -> TypeCheckResult VarMeta
resolveTypeVar v con = case H.lookup v (constraintVarArgEnv con) of
  Just m' -> return m'
  Nothing -> TypeCheckResE [GenTypeCheckError Nothing $ printf "Unknown variable in resolveTypeVar var: %s" (show v)]

descriptorVaenv :: FEnv -> COVarArgEnv VarMeta -> COVarArgEnv Scheme
descriptorVaenv env = fmap (descriptor env)

descriptorSTypeVaenv :: FEnv -> COVarArgEnv VarMeta -> TypeCheckResult STypeVarArgEnv
descriptorSTypeVaenv env vaenv = sequence $ descriptorVaenv env vaenv

type STypeVarArgEnv = H.HashMap TypeVarAux SType
descriptorConVaenv :: FEnv -> VConstraint -> TypeCheckResult STypeVarArgEnv
descriptorConVaenv env con = descriptorSTypeVaenv env (constraintVarArgEnv con)

-- trace constrain
type TraceConstrainEpoch = [(VConstraint, [(Pnt, Scheme)])]
type STraceConstrain = [(SConstraint, [(Pnt, Scheme)])]
data TraceConstrain = TraceConstrain {
  tcEpochs  :: [TraceConstrainEpoch],
  tcInitial :: H.HashMap Pnt Scheme,
  tcCons    :: [VConstraint],
  tcAllObjs :: VarMeta
                                      }
  deriving (Show, Generic, ToJSON)

mkTraceConstrain :: TraceConstrain
mkTraceConstrain = TraceConstrain [[]] H.empty [] emptyMetaN

nextConstrainEpoch :: FEnv -> FEnv
nextConstrainEpoch env@FEnv{feTrace=tc@TraceConstrain{tcCons=[]}, feCons, feUnionAllObjs} = nextConstrainEpoch env{feTrace=tc{tcCons=feCons, tcAllObjs=feUnionAllObjs}} -- On first epoch, set constraints and all objs
nextConstrainEpoch env@FEnv{feTrace=tc@TraceConstrain{tcEpochs}} = case tcEpochs of
  []         -> env
  prevEpochs -> env{feTrace = tc{tcEpochs=[]:prevEpochs}, feUpdatedDuringEpoch = False}

startConstraint :: VConstraint -> FEnv -> FEnv
startConstraint c env@FEnv{feTrace = tc@TraceConstrain{tcEpochs=curEpoch:prevEpochs}} = env{feTrace = tc{tcEpochs=((c, []):curEpoch):prevEpochs}}
startConstraint _ _ = error "bad input to startConstraint"

traceConstrainChange :: TraceConstrain -> Pnt -> Scheme -> TraceConstrain
traceConstrainChange tc@TraceConstrain{tcEpochs=((curConstraint, curChanged):curEpoch):prevEpochs} p scheme' = tc{tcEpochs=((curConstraint, (p, scheme'):curChanged):curEpoch):prevEpochs}
traceConstrainChange _ _ _ = error "no epochs in TraceConstrain"

filterTraceConstrain :: TraceConstrain -> Pnt -> TraceConstrain
filterTraceConstrain tc@TraceConstrain{tcEpochs, tcCons, tcInitial} p = tc{tcEpochs=tcEpochs', tcCons=tcCons', tcInitial=tcInitial'}
  where
    tcEpochs' = map (filter (elem p . map fst . snd)) tcEpochs
    tcCons' = filter (elem p . mapMaybe getPnt . constraintMetas) tcCons
    tcInitial' = H.filterWithKey (\k _ -> k == p) tcInitial

-- | Reverses the epochs order to be chronological
flipTraceConstrain :: TraceConstrain -> TraceConstrain
flipTraceConstrain tc@TraceConstrain{tcEpochs} = tc{tcEpochs=reverse $ map reverse tcEpochs}

showTraceConstrain :: [SConstraint] -> String
showTraceConstrain epochs = intercalate "\n" $ map showConstraintPair $ reverse epochs
  where
    showConstraintPair pair = show pair
