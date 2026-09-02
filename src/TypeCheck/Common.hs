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
import qualified Data.HashSet               as S
import qualified Data.IntMap.Lazy           as IM
import           Data.List
import           Data.UUID                  (nil)
import           GHC.Generics               (Generic)

import           Control.Monad.State
import           Control.Monad.Trans.Writer (execWriter, tell)
import           CRes
import           Data.Aeson                 (ToJSON, toJSON)
import           Data.Maybe                 (catMaybes, fromJust, fromMaybe,
                                             isJust, listToMaybe, mapMaybe,
                                             maybeToList)
import           Data.String.Builder        (literal)
import           MapMeta                    (MetaType (ArrMeta), clearMetaDat,
                                             mapMetaObjArr)
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
  | DefinitionOutsideDeclaration TypeName Type Type Type CodeRange
  | UnfulfilledDeclaration TypeName Type Type Type CodeRange
  | RefineOutsideDeclaration TypeName Type Type Type CodeRange
  deriving (Eq, Ord, Generic, Hashable)

-- | SType actual required (description in type)
-- Contains what the type actually is, and what the requirements are (both upper bounds)
-- Covered by the relationship that the actual should be a subset of the required
--
-- 'stypeReqDecl' is the ORIGIN-SEPARATED sub-requirement: only content whose provenance is a
-- spelled type (a declared return, an annotation, a declared field bound, a pattern spelling)
-- is ever met into it. The accumulated 'stypeReq' additionally receives requirement content
-- derived from the point's own actual — 'arrowConstrainUbs'\'s reverse narrowing writes the
-- source's own met forms back as its requirement (the contract-discovery mechanism), and
-- 'EqReqAct' meets the actual into the requirement. Those echoes are load-bearing for
-- contract discovery and arrow choice, but they must not answer the ADMISSION question
-- ("may this form stay unsettled here?"): a form's own echo would license the form itself.
-- The decl channel is the requirement admission consults; like 'stypeReq' it only narrows,
-- and it is usually WIDER than 'stypeReq' (⊤ wherever no declaration engages — no forcing).
data SType = SType {
  stypeAct     :: !Type,
  stypeReq     :: !Type,
  stypeReqDecl :: !Type,
  stypeTree    :: !(Maybe ReachesTree),
  stypeDesc    :: !String
                   }
  deriving (Eq, Ord, Generic, Hashable, ToJSON)
type Scheme = TypeCheckResult SType

data SchemeActReq = SchemeAct | SchemeReq
  deriving (Eq, Show)

type Pnt = Int

type FEnvTypeEnv = TypeEnv (ObjArrTypeGraph Expr ())
data FEnv = FEnv { fePnts                :: IM.IntMap Scheme
                 , feConsDats            :: [[VConstraint]]
                 , feCons                :: [VConstraint]
                 , feUnionAllObjs        :: VarMeta -- union of all TypeObj for argument inference
                 , feVTypeGraph          :: VTypeGraph
                 , feTTypeGraph          :: TTypeGraph
                 , feTTypeGraphCleared   :: ObjectMap Expr () -- ^ 'feTTypeGraph' with metadata cleared, maintained incrementally by 'fAddTTypeGraph' (cleared once per arrow) so 'buildTypeEnv' does not re-clear the whole typechecked graph every epoch.
                 , feReachesVPart        :: ObjectMap Expr () -- ^ The previous epoch's live (V-side) contribution to the reaches type graph. The typechecked side only changes between solve fragments (where the universe key change already bumps the env stamp), so comparing the small live part alone decides 'envUnchanged'.
                 , feUpdatedDuringEpoch  :: Bool -- ^ If a pnt is updated during the epoch
                 , feTypeEnv             :: FEnvTypeEnv
                 , feReachesTypeEnv      :: FEnvTypeEnv -- ^ Per-epoch cache of 'buildTypeEnv' (the constraint-independent reaches type graph), recomputed once at the top of each 'runConstraints' epoch and reused across all 'mkReachesEnv' calls. Initialized to the base 'feTypeEnv' but never read until the first epoch populates it.
                 , feCompletedAllObjsKey :: Type -- ^ The 'feUnionAllObjs' descriptor value from which 'feCompletedAllObjs' was built (recomputed each epoch). 'computeConstraint' uses the cache only when a 'BoundedByObjs' constraint's (showCon-substituted) bound equals this key, so the cache is an EXACT memo of the union+completion — no staleness. Initialized to 'BottomType'.
                 , feCompletedAllObjs    :: Type -- ^ Per-epoch cache of the completed @feUnionAllObjs@ bound (@unionTypes key (completeDataPartials key)@ for @key = feCompletedAllObjsKey@). Every 'BoundedByObjs' constraint is bounded by the same 'feUnionAllObjs' descriptor (substituted uniformly by 'showCon'), so this otherwise-identical union+completion is computed once per epoch and reused by every constraint whose bound matches the key. Initialized to 'PTopType' but never read until the first epoch populates it.
                 , feInputArrowNames     :: S.HashSet Name -- ^ Per-epoch cache of 'typeGraphInputEmbeddedNames' on the reaches env: every name embedded in some arrow object's input spelling — the forms some arrow can accept UNSETTLED. A rewritable form whose name is absent has no consuming arrow anywhere, so deferring it past its own rewrite frontier is meaningless ('arrowConstrainUbs'\'s consumption-dest settle forcing). Recomputed when the per-epoch env inputs change (same guard as 'feCompletedAllObjs').
                 , feLocalDefNames       :: S.HashSet Name -- ^ Absolute names defined by the programs currently being encoded (not yet in 'feTypeEnv'\'s object map). Used to tell a destructuring binding variable from a reference to a same-batch definition.
                 , feEnvStamp            :: Int -- ^ Version counter for the per-epoch env inputs ('feReachesTypeEnv' graph and 'feCompletedAllObjsKey' universe): bumped at each epoch start iff either differs from the previous epoch. Constraint kinds that read those env caches skip re-execution only within the same stamp (see 'feConMemo').
                 , feConMemo             :: IM.IntMap ConMemo -- ^ Per-constraint memo of the last execution's resolved inputs, keyed by the constraint's position in the 'runConstraints' list. 'executeConstraint' skips 'computeConstraint'+'saveConstraint' when the constraint's resolved view (and, for env-reading kinds, the env stamp) is unchanged: a pure function of unchanged inputs recomputes the same stores, which 'setScheme' would refuse as unchanged — a provable no-op.
                 , feTrace               :: TraceConstrain
                 } deriving (Show)

-- | A constraint's last-execution inputs and result: env stamp ('feEnvStamp'), the resolved
-- constraint view ('showCon'+'stypeConstraint'), the resolved object-side arg entries an
-- 'ArrowTo' additionally reads ('mkReachesEnv'), and the returned prune decision.
data ConMemo = ConMemo Int RConstraint [(Maybe SExpr, Maybe ShowMeta)] Bool
  deriving (Show)

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
  | EqReqAct Int p -- ^ An input argument's actual and requirement denote the same thing (an LHS input actually IS whatever it is required to be, and vice versa), so meet the two and set BOTH to @act ∩ req@.
  | ArrowTo Int p p -- ArrowTo src dest
  | PropEq Int (p, TypeVarAux) p -- ^ Both Actual and Req
  | FnInputBound Int (p, ArgName, ArgName) p -- ^ Actual only. @FnInputBound (call, fnArgKey, inputKey) input@ bounds a call-site lambda argument's input point by the matched declarations' input type for that position (see 'fnArgInputBound')
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
mapMConDat f (EqReqAct i p) = EqReqAct i <$> f p
mapMConDat f (ArrowTo i p1 p2) = do
  p1' <- f p1
  ArrowTo i p1' <$> f p2
mapMConDat f (PropEq i (p1, name) p2) = do
  p1' <- f p1
  PropEq i (p1', name) <$> f p2
mapMConDat f (FnInputBound i (p1, fnKey, inKey) p2) = do
  p1' <- f p1
  FnInputBound i (p1', fnKey, inKey) <$> f p2
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
type VTypeGraph = VObjectMap -- H.HashMap (Root tuple name for filtering) [vals]
type TTypeGraph = TObjectMap -- H.HashMap (Root tuple name for filtering) [vals]

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
  show (DefinitionOutsideDeclaration name defType declType diff loc) = printf "Definition Outside Declaration for %s:\n\tDefinition input type: %s\n\tDeclaration input type: %s\n\tUndeclared part: %s\n\tLocation: %s" (nameStr name) (show defType) (show declType) (show diff) (maybe "Unknown" showCodeRange loc)
  show (UnfulfilledDeclaration name declType defType diff loc) = printf "Unfulfilled Declaration for %s:\n\tDeclaration input type: %s\n\tDefinition input type: %s\n\tUndefined part: %s\n\tLocation: %s" (nameStr name) (show declType) (show defType) (show diff) (maybe "Unknown" showCodeRange loc)
  show (RefineOutsideDeclaration name refineType declType diff loc) = printf "Refine Outside Declaration for %s:\n\tRefine input type: %s\n\tDeclaration input type: %s\n\tUndeclared part: %s\n\tLocation: %s" (nameStr name) (show refineType) (show declType) (show diff) (maybe "Unknown" showCodeRange loc)

instance CNoteTC TypeCheckError where
  metaCNote (GenTypeCheckError m _)      = m
  metaCNote (TracedTypeCheckError m _ _) = clearMetaDat ArrMeta m
  metaCNote (TupleMismatch _ _ m _)      = Just m
  metaCNote ConstraintTypeCheckError{}   = Nothing
  metaCNote (DefinitionOutsideDeclaration _ _ _ _ loc) = case loc of
    Just pos -> Just $ Meta PTopType (Just pos) nil ()
    Nothing  -> Nothing
  metaCNote (UnfulfilledDeclaration _ _ _ _ loc) = case loc of
    Just pos -> Just $ Meta PTopType (Just pos) nil ()
    Nothing  -> Nothing
  metaCNote (RefineOutsideDeclaration _ _ _ _ loc) = case loc of
    Just pos -> Just $ Meta PTopType (Just pos) nil ()
    Nothing  -> Nothing

  typeCNote _ = CNoteError
  showRecursiveCNote _ n = literal $ show n

instance Show SType where
  show (SType act req reqDecl _ desc) = printf "{%s :: ACT %s; REQ  %s%s}" desc (show act) (show req) declSuffix
    where
      -- The decl channel is ⊤ at most points (no declaration engages); print it only when
      -- it carries information so traces and errors stay readable.
      declSuffix :: String
      declSuffix = case reqDecl of
        PTopType -> ""
        _        -> printf "; DECL %s" (show reqDecl)

instance (Show p) => Show (ConstraintDat p) where
  show (EqualsKnown i s t) = printf "%s %d==_Known %s" (show s) i (show t)
  show (EqPoints i s1 s2) = printf "%s %d== %s" (show s1) i (show s2)
  show (BoundedByKnown i s t) = printf "%s %d⊆_Known %s" (show s) i (show t)
  show (BoundedByObjs i s _) = printf "BoundedByObjs%d %s" i (show s)
  show (NoReturnArg i s) = printf "NoReturnArg%d %s" i (show s)
  show (EqReqAct i s) = printf "EqReqAct%d %s" i (show s)
  show (ArrowTo i s d) = printf "%s %d-> %s" (show s) i (show d)
  show (PropEq i (s1, n) s2) = printf "(%s).(%s) %d== %s"  (show s1) (show n) i (show s2)
  show (FnInputBound i (s, fnKey, inKey) inp) = printf "%s %d⊆_FnInput (%s).(%s).(%s)" (show inp) i (show s) (show fnKey) (show inKey)
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

-- | NOTE: deliberately compares only the actual and accumulated-requirement channels. The
-- decl channel ('stypeReqDecl') is excluded: including it makes decl-only narrowings force
-- whole-scheme commits, which drag along act/req RESPELLINGS this equality deliberately
-- refuses (the spelling-stability doctrine of 'setDescriptor'\'s refusal path) — measured as
-- template erasure (@tail=∀ConsList[/T=⊤]@) across core/list.ct. Decl-only progress commits
-- through its own dedicated path in 'setDescriptor' instead.
eqScheme :: FEnv -> TypeVarArgEnv -> Scheme -> Scheme -> Bool
eqScheme FEnv{feTypeEnv} vaenv (TypeCheckResult _ (SType ub1 lb1 _ _ _)) (TypeCheckResult _ (SType ub2 lb2 _ _ _)) = isEqTypeWithEnv feTypeEnv vaenv ub1 ub2 && isEqTypeWithEnv feTypeEnv vaenv lb1 lb2
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

-- | Adds constraints to the innermost open block.
--
-- Constraints are ACCUMULATED IN REVERSE of the order 'TypeCheck.Encode' emits them, so that
-- each add is O(1) rather than O(list). 'finalizeConstraints' flips the whole accumulator once
-- at the end of encoding; every function here maintains that reversal exactly, so the flip
-- recovers emission order and nothing else has to think about it. See 'finalizeConstraints' for
-- why emission order is the order the solver wants.
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

-- | Puts the encoded constraints into DATAFLOW ORDER, which is the order 'TypeCheck.Encode'
-- emitted them in.
--
-- 'runConstraintsIdx' executes the list front to back once per epoch, and a constraint sees the
-- points every constraint before it in the list already wrote. So a value crosses as many links
-- per epoch as the list order lets it: run in dataflow order it rides a whole expression in one
-- epoch, run against it it advances one link per epoch and the epoch count becomes proportional
-- to nesting depth.
--
-- 'fromExpr' encodes post-order — a subexpression's constraints are emitted before those of the
-- application containing it — which is exactly dataflow order for the actual types that flow
-- outward from leaves. The accumulator above holds the exact global reverse of that (both
-- 'addConstraints' and both 'endConstraintBlock' arms prepend, and a block ends before the
-- enclosing block emits anything further), so ONE reverse here recovers it.
--
-- This is an ordering of the same constraint set, not a change to any constraint: the solver
-- still runs to the same fixpoint, and order only decides how many epochs it takes to get
-- there. It is not a correctness crutch either — 'runConstraintsLimit' is a runaway guard, and
-- the point of ordering is that the guard stops being reachable by ordinary programs. Before
-- this, a left-nested chain cost 3 epochs per operand, so an eight-operand @++@ chain (25 epoch
-- budget, 26 needed) failed to typecheck at all.
--
-- This is the order the ACTUAL types want. Requirements flow the other way, so 'runConstraints'
-- ALTERNATES the direction each epoch rather than leaving the requirement channel permanently
-- in its worst order — see the note on the recursive call in 'runConstraintsIdx'. The two
-- belong together: this ordering on its own breaks calls through a class top.
finalizeConstraints :: FEnv -> FEnv
finalizeConstraints env@FEnv{feCons} = env{feCons = reverse feCons}

fAddVTypeGraph :: VObjArr -> FEnv -> FEnv
fAddVTypeGraph oa env@FEnv{feVTypeGraph} = env {feVTypeGraph = singletonObjectMap oa <> feVTypeGraph}

-- | The metadata-cleared form is cached HERE, at promotion time, so each typechecked arrow is
-- cleared exactly once: 'buildTypeEnv' runs at every solve epoch and re-clearing the whole
-- (growing) typechecked graph there is quadratic across a page's solve fragments.
fAddTTypeGraph :: TObjArr -> FEnv -> FEnv
fAddTTypeGraph oa env@FEnv{feTTypeGraph, feTTypeGraphCleared} = env
  { feTTypeGraph = singletonObjectMap oa <> feTTypeGraph
  , feTTypeGraphCleared = singletonObjectMap (mapMetaObjArr clearMetaDat Nothing oa) <> feTTypeGraphCleared
  }

-- | Structurally detects whether an EXTERNAL 'TypeVar' ('TVExt') present in @old@ has been widened
-- back to Top in @new@. Such a variable is a reference to a variable of an enclosing scope (e.g. a
-- higher-order argument's parameter, @/fn(/v=$_A)@, tied to a container element); replacing it with
-- Top drops that cross-scope link. Resolving a variable to a concrete (narrower) type is not a loss.
-- This complements 'verifySchemeActLowers', which cannot see either case: in
-- a subtype test a free 'TypeVar' behaves like Top, so the widening looks like a legal narrowing.
--
-- An INTERNAL 'TypeVar' ('TVInt') is currently ignored, on the reasoning that it resolves through
-- its own partial's @ptVars@ so a @/value=$V@ showing as Top is equivalent. That reasoning holds
-- ONLY when the partial binds @V@ to Top. When it binds @V@ to something below Top the replacement
-- is a REAL loss, of exactly the same kind as the 'TVExt' case: it discards both the value the
-- binding carries here and the link tying this occurrence to the variable's other occurrences.
--
-- TODO(#42): make the 'TVInt' case binding-aware instead of blanket-excluded — a 'TVInt' loss
-- counts when the enclosing partial binds the name below Top. Measured live: core/set.ct's
-- @allIn[/E=∀Ord](items -> ConsList[$T -> $E], ..)@ has a requirement projection replace
-- @ConsList[/T=$/E]@ by @ConsList[/T=⊤]@; this guard is the one designed to refuse that write
-- ('updateSchemeProp'), and the blanket exclusion is why it does not fire.
-- Only partials matched unambiguously by name (a single partial of that name on each side) are
-- compared, and only argument/variable keys present on both sides, so it never false-positives on a
-- union whose set of options legitimately changed.
typeVarLost :: Type -> Type -> Bool
-- Replacement by ⊤ drops BOTH the pointer and any occurrence-local inner — still a loss.
-- For a future producer: replacement of an occurrence BY ITS OWN INNER is deliberate
-- knowledge-preserving erasure, and already falls to the catch-all False clause below.
typeVarLost (TypeVar _ TVOuter _) PTopType = True
typeVarLost (TypeVar _ _ _)     _        = False
typeVarLost (UnionType _ oldLeafs _) (UnionType _ newLeafs _) =
  or [ partialVarLost op np
     | (name, [op]) <- H.toList (splitUnionTypeByName oldLeafs)
     , Just [np] <- [H.lookup name (splitUnionTypeByName newLeafs)] ]
  where
    partialVarLost op np =
      or [ typeVarLost ov nv | (k, ov) <- H.toList (ptArgs op), Just nv <- [H.lookup k (ptArgs np)] ]
      || or [ typeVarLost ov nv | (k, ov) <- H.toList (ptVars op), Just nv <- [H.lookup k (ptVars np)] ]
typeVarLost _ _ = False

-- | Structurally detects whether a SLOT gained a var pointer where @old@ held ⊤ — a
-- declaration's @/fn(..)=Top@ becoming the internal pointer @/fn(..)=$B@ (see 'updateTypeProp').
-- 'eqScheme' cannot see that: a FREE type variable denotes ⊤, so the two spellings are the same
-- set and the write would be dropped as a no-op, leaving the explicit link unlanded.
--
-- A gained 'ptVars' KEY needs no rule here. An unspelled parameter is ⊤ (see 'PartialType' in
-- "Semantics.Types"), so resolving one is a strict narrowing that 'eqScheme' sees by itself,
-- both at the top level and nested inside an arg. Detecting it structurally would also
-- misfire where the partial's own field determines the parameter — @/Pair(first=Integer)@ and
-- @/Pair[/A=Integer](first=Integer)@ are the same set, since @/Pair@ declares @first -> $A@ —
-- and force a commit for no change.
--
-- Gain-only, like its neighbors: a slot losing its pointer stays a no-op under the 'eqScheme'
-- path, so the variable is not widened back to ⊤ and there is no set/lose oscillation.
-- Mirrors the matching discipline of 'typeVarLost' (single same-name partial on each side;
-- nested positions compared only on shared keys).
varPointerGained :: Type -> Type -> Bool
varPointerGained (UnionType _ oldLeafs _) (UnionType _ newLeafs _) =
  or [ partialPointerGained op np
     | (name, [op]) <- H.toList (splitUnionTypeByName oldLeafs)
     , Just [np] <- [H.lookup name (splitUnionTypeByName newLeafs)] ]
  where
    partialPointerGained op np =
      or [ slotGained ov nv | (k, ov) <- H.toList (ptArgs op), Just nv <- [H.lookup k (ptArgs np)] ]
      || or [ slotGained ov nv | (k, ov) <- H.toList (ptVars op), Just nv <- [H.lookup k (ptVars np)] ]
    -- Slot-level only: a whole-type Top-to-TypeVar transition stays under 'eqScheme'.
    slotGained PTopType TypeVar{} = True
    slotGained ov nv              = varPointerGained ov nv
varPointerGained _ _ = False

-- | Structurally detects whether @new@ strictly NARROWS a nested 'ptVars' dimension that @old@
-- spells wider (e.g. under an outer @Cons[T=Integer]@, the declaration-clamped tail
-- @Cons[T=∀Ord](head=$T, …)@ narrowing to @Cons[T=Integer](head=Integer, …)@). The two spellings
-- denote the SAME set under declaration-boundedness — the outer binding already bounds the
-- declaration-linked slot's content, so 'eqScheme' rightly judges the schemes equal — but the
-- stored spelling is what every consumer reads, and the wide form freezes a deep element one
-- level short (TreeBuild finds no arrows from it). A strictly-finer dimension must therefore
-- commit. Gain-only like the neighboring rules: the reverse widening stays a no-op under the
-- 'eqScheme' path, so the finer spelling is kept once found and there is no oscillation.
-- Mirrors the matching discipline of 'typeVarLost' (single same-name partial on each side;
-- nested positions compared only on shared keys). Var LINKS are skipped — only two resolved
-- dimension values are compared.
nestedVarNarrowed :: TypeGraph tg => TypeEnv tg -> Type -> Type -> Bool
nestedVarNarrowed typeEnv (UnionType _ oldLeafs _) (UnionType _ newLeafs _) =
  or [ partialNarrowed op np
     | (name, [op]) <- H.toList (splitUnionTypeByName oldLeafs)
     , Just [np] <- [H.lookup name (splitUnionTypeByName newLeafs)] ]
  where
    partialNarrowed op np =
      or [ dimNarrowed ov nv | (k, ov) <- H.toList (ptVars op), Just nv <- [H.lookup k (ptVars np)] ]
      || or [ nestedVarNarrowed typeEnv ov nv | (k, ov) <- H.toList (ptArgs op), Just nv <- [H.lookup k (ptArgs np)] ]
    dimNarrowed ov@UnionType{} nv@UnionType{} =
      isSubtypeOfWithEnv typeEnv H.empty nv ov && not (isSubtypeOfWithEnv typeEnv H.empty ov nv)
    dimNarrowed _ _ = False
nestedVarNarrowed _ _ _ = False

-- | Structurally detects a nested arg slot going from ⊤ to spelled content
-- (@Node[..](left=⊤)@ → @Node[..](left=∀BinTree[..])@). Under declaration-boundedness the
-- two spellings denote the same set — an absent\/⊤ arg slot reads at its declared bound,
-- so 'eqScheme' rightly judges the schemes equal — but the stored ⊤ freezes the spelling
-- every consumer reads: binder projections deliver ⊤, the definition body constructs
-- ⊤-slotted values, and eval-time TreeBuild (whose concrete 'ObjSrc' frame resolves the
-- destination's vars) finds no arrows from the starved form. Without this rule the
-- enrichment committing was a convergence-order lottery: it lands only when it arrives in
-- the same write as a genuine change (the full-arity completion at the epoch the universe
-- happens to resolve), and is refused as a pure respelling ever after. ⊤→content ONLY —
-- pure syntax, no subtype calls (the general narrower-spelling rule is measured
-- intractable, see 'nestedSlotSettled'); a bare var link is not content (gaining one is
-- 'varPointerGained''s rule). Gain-only like the neighboring rules (a content→⊤ write
-- stays a no-op under the 'eqScheme' path), so the filled spelling is kept once found and
-- there is no oscillation.
-- | Whether @x@ IS @d@ with some positions still ⊤ — the lossless-adoption guard for the
-- decl-harmonize in 'setDescriptor': same union top\/constants, same single leaf per name,
-- same var\/arg keysets, and every non-⊤ position of @x@ literally equal to @d@'s (⊤
-- positions may take @d@'s content, recursively). Purely syntactic — no set-algebra
-- judges, whose conservative answers lose content (see the harmonize comment).
slotFillAdopt :: Type -> Type -> Bool
slotFillAdopt x d | x == d = True
slotFillAdopt (UnionType xt xLs xCs) (UnionType dt dLs dCs) =
  xt == dt && xCs == dCs
    && H.keysSet xByName == H.keysSet dByName
    && and [ ok | (n, xps) <- H.toList xByName
           , let ok = case (xps, H.lookup n dByName) of
                   ([xp], Just [dp]) -> partialAdopt xp dp
                   _                 -> False ]
  where
    xByName = splitUnionTypeByName xLs
    dByName = splitUnionTypeByName dLs
    partialAdopt xp dp =
      ptArgMode xp == ptArgMode dp && ptPreds xp == ptPreds dp
        && H.keysSet (ptVars xp) == H.keysSet (ptVars dp)
        && H.keysSet (ptArgs xp) == H.keysSet (ptArgs dp)
        && and [ fillOk v (H.lookupDefault PTopType k (ptVars dp)) | (k, v) <- H.toList (ptVars xp) ]
        && and [ fillOk v (H.lookupDefault PTopType k (ptArgs dp)) | (k, v) <- H.toList (ptArgs xp) ]
    fillOk PTopType _ = True
    fillOk v dv       = v == dv || slotFillAdopt v dv
slotFillAdopt _ _ = False

nestedSlotFilled :: Type -> Type -> Bool
nestedSlotFilled (UnionType _ oldLeafs _) (UnionType _ newLeafs _) =
  or [ partialFilled op np
     | (name, [op]) <- H.toList (splitUnionTypeByName oldLeafs)
     , Just [np] <- [H.lookup name (splitUnionTypeByName newLeafs)] ]
  where
    partialFilled op np =
      or [ slotFilled ov nv | (k, ov) <- H.toList (ptArgs op), Just nv <- [H.lookup k (ptArgs np)] ]
    slotFilled ov nv | ov == nv = False
    slotFilled PTopType TypeVar{} = False
    slotFilled PTopType _         = True
    slotFilled ov nv              = nestedSlotFilled ov nv
nestedSlotFilled _ _ = False

-- | Structurally detects the settle machinery's respelling of a nested arg slot
-- ('TypeCheck.TypeUtils.settlePropSlot' and the 'arrowConstrainUbs' settle-forcing) that
-- 'eqScheme' judges equal: the slot's positive-leaf NAME SET strictly shrank — a
-- settle-forced unsettled call form was dropped, invisible because the algebra answers its
-- class membership SAT-conservatively (@tail=(∀List ∪ rangeFrom(..))@ ≡ @tail=∀List@). The
-- settled spelling must commit: the stored spelling is what every consumer reads (the
-- requirement side's declared-bound tighten rides the same store — a scheme commits whole).
-- Deliberately ONLY the leaf-drop signature: a general strictly-narrower-spelling rule
-- fires on the natural respelling churn of every epoch and the per-commit verification
-- cost is measured intractable. Gain-only like the neighboring rules (re-adding a dropped
-- leaf stays a no-op under the 'eqScheme' path), so there is no oscillation. Mirrors the
-- matching discipline of 'typeVarLost' (single same-name partial on each side; shared keys
-- only). Consulted only AFTER 'eqScheme' judged the schemes equal; identical spellings
-- short-circuit before any walk.
nestedSlotSettled :: TypeGraph tg => TypeEnv tg -> Type -> Type -> Bool
nestedSlotSettled typeEnv (UnionType _ oldLeafs _) (UnionType _ newLeafs _) =
  or [ partialSettled op np
     | (name, [op]) <- H.toList (splitUnionTypeByName oldLeafs)
     , Just [np] <- [H.lookup name (splitUnionTypeByName newLeafs)] ]
  where
    partialSettled op np =
      or [ slotSettled ov nv | (k, ov) <- H.toList (ptArgs op), Just nv <- [H.lookup k (ptArgs np)] ]
    slotSettled ov nv | ov == nv = False -- identical spellings cannot contain a respelling
    slotSettled ov@(UnionType _ oLs _) nv@(UnionType _ nLs _) =
      (H.keysSet nLs /= H.keysSet oLs && S.null (S.difference (H.keysSet nLs) (H.keysSet oLs)))
      || nestedSlotSettled typeEnv ov nv
    slotSettled _ _ = False
nestedSlotSettled _ _ _ = False

-- This ensures schemes are correct
-- It differs from Constrain.checkScheme because it checks for bugs in the internal compiler, not bugs in the user code
verifyScheme :: TypeGraph tg => TypeEnv tg -> TypeVarArgEnv -> VarMeta -> Scheme -> Scheme -> Maybe String
verifyScheme typeEnv vaenv Meta{getMetaDat=VarMetaDat _ _} (TypeCheckResult _ (SType oldAct oldReq oldReqDecl _ _)) (TypeCheckResult _ (SType act req reqDecl _ _)) = listToMaybe $ catMaybes [
  if verifySchemeActLowers then Nothing else Just (printf "verifySchemeActLowers\n\t\tGrows by: %s" (show $ differenceTypeWithEnv typeEnv vaenv act oldAct)),
  if verifyNoTypeVarLost then Nothing else Just (printf "verifyNoTypeVarLost\n\t\tLost a TypeVar going from %s to %s" (show oldAct) (show act)),
  if verifySchemeReqLowers then Nothing else Just "verifySchemeReqLowers",
  if verifySchemeReqDeclLowers then Nothing else Just "verifySchemeReqDeclLowers",
  if verifyCompacted then Nothing else Just (printf "verifyCompacted\n\t\tCan be compacted to %s" (show $ compactType typeEnv vaenv act))
  ]
  where
    -- Monotonicity test for the verify channels. 'isSubtypeOfWithEnv' is a SPELLING-SENSITIVE
    -- walk and is incomplete on the pairs these channels actually compare: a genuine
    -- narrowing whose two sides are spelled in different vocabularies is refused. core/map.ct
    -- produced 52 identical refusals of
    --   old @insert[/MV=⊤, /MK=∀Ord](/m=∀/Data/Map[/K=$/MK, /V=$/MV], ..)@
    --   new @insert[/MV=Integer, /MK=Integer](/m=/Data/Map/Node[..](..), ..)@
    -- — every slot strictly narrower, but proving @Node[..] ⊆ ∀Map[/K=$/MK, /V=$/MV]@ needs a
    -- class expansion plus var reconciliation the walk does not perform. The DIFFERENCE
    -- oracle decides the same question exactly (@new - old ≡ ∅@ iff @new ⊆ old@), so it backs
    -- the walk as a second opinion — the prover-consistency doctrine 'predImplies' already
    -- applies via 'predsEmpty'. Refusing a real narrowing poisons the point: the update is
    -- dropped and re-derived identically every epoch, which is what those 52 errors were.
    lowers a b = isSubtypeOfWithEnv typeEnv vaenv a b
                   || isBottomType (snd (differenceTypeWithEnv typeEnv vaenv a b))
    verifySchemeActLowers  = case act of
      TypeVar{} -> True
      _         -> lowers act oldAct
    -- The old side may predate scope normalization (an Encode-time preset holding a foreign
    -- declaration's var spelling): read its out-of-scope vars as the ⊤ they denote before
    -- comparing, so replacing such a spelling by ⊤ is not reported as a lost variable.
    verifyNoTypeVarLost = not (typeVarLost (dropOutOfScopeTVExt vaenv oldAct) act)
    verifySchemeReqLowers  = lowers req oldReq
    verifySchemeReqDeclLowers = lowers reqDecl oldReqDecl
    verifyCompacted = act == compactType typeEnv vaenv act
verifyScheme _ _ _ _ _ = Nothing


-- Point operations
descriptorPnt :: FEnv -> Pnt -> Scheme
descriptorPnt FEnv{fePnts} p = fePnts IM.! p

descriptor :: FEnv -> VarMeta -> Scheme
descriptor env m = case getPnt m of
  Just p  -> descriptorPnt env p
  Nothing -> pure (SType (getMetaType m) (getMetaType m) (getMetaType m) Nothing "noPnt descriptor")

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
setDescriptor env@FEnv{feTypeEnv, fePnts, feTrace, feUpdatedDuringEpoch} con m@Meta{getMetaDat=VarMetaDat (Just p) _} scheme' msg = env{fePnts = pnts', feTrace = feTrace', feUpdatedDuringEpoch = feUpdatedDuringEpoch || schemeChanged || isJust declOnlyCommit}
  where
    scheme = descriptor env m
    vaenv = fmap stypeAct <$> mapM (descriptor env) (constraintVarArgEnv con)
    schemeChanged :: Bool
    schemeChanged = case (scheme, scheme') of
      (TypeCheckResult _ SType{stypeAct=UnionType (Just _) _ _}, TypeCheckResult _ SType{stypeAct=TypeVar{}}) -> True
      -- GAINING a nested external 'TypeVar' (@old@ has Top where @new@ has @$_A@) is a real change
      -- even though 'eqScheme' considers the two schemes equal (in a subtype/equality test a free
      -- 'TypeVar' behaves like Top, so @/fn(/v=Top)@ and @/fn(/v=$_A)@ look equal). Without this a
      -- constraint that first SETS the variable is treated as a no-op and dropped, so the explicit
      -- link never lands. Only a gain is flagged, not a loss: a loss stays a no-op (skipped by the
      -- 'eqScheme' path below), so once set the variable is not widened back to Top — which both
      -- keeps the information and avoids a set/lose oscillation that would never converge.
      (TypeCheckResult _ (SType a1 _ _ _ _), TypeCheckResult _ (SType a2 _ _ _ _)) | typeVarLost a2 a1 -> True
      -- GAINING a var POINTER in a slot that held ⊤ (see 'varPointerGained') is invisible to
      -- 'eqScheme' — a free variable denotes ⊤, so the two spellings are the same set — yet the
      -- link must land. A gained 'ptVars' key needs no rule here: an unspelled parameter is ⊤,
      -- so 'eqScheme' sees that narrowing itself.
      -- Gain-only for the same convergence reason as above: dropping the pointer later stays a
      -- no-op, so the resolved value is kept once found.
      (TypeCheckResult _ (SType a1 _ _ _ _), TypeCheckResult _ (SType a2 _ _ _ _)) | varPointerGained a1 a2 -> True
      -- STRICTLY NARROWING a nested 'ptVars' dimension (see 'nestedVarNarrowed') is a spelling
      -- improvement of the same set that 'eqScheme' judges equal, but the finer spelling is what
      -- consumers read — it must commit. Gain-only like the two rules above.
      (TypeCheckResult _ (SType a1 _ _ _ _), TypeCheckResult _ (SType a2 _ _ _ _)) | nestedVarNarrowed feTypeEnv a1 a2 -> True
      -- FILLING a ⊤ arg slot with its spelled content (see 'nestedSlotFilled') is likewise a
      -- spelling improvement of the same declaration-bounded set that must commit — the ⊤
      -- spelling starves every consumer downstream. Gain-only like its neighbors.
      (TypeCheckResult _ (SType a1 _ _ _ _), TypeCheckResult _ (SType a2 _ _ _ _)) | nestedSlotFilled a1 a2 -> True
      -- The settle machinery's leaf-drop respelling (see 'nestedSlotSettled') is checked
      -- only when 'eqScheme' judged the schemes equal — the refusal path. The actual side
      -- alone is inspected; the requirement side's tighten rides the same whole-scheme store.
      (TypeCheckResult _ (SType a1 _ _ _ _), TypeCheckResult _ (SType a2 _ _ _ _)) ->
        fromMaybe False $ tcreToMaybe $ do
          showVaenv <- vaenv
          return $ not (eqScheme env showVaenv scheme scheme')
                 || (a1 /= a2 && nestedSlotSettled feTypeEnv a1 a2)
      _ ->  fromMaybe False $ tcreToMaybe $ do
        showVaenv <- vaenv
        return $ not (eqScheme env showVaenv scheme scheme')
    scheme'' = case verifyScheme feTypeEnv (fromJust $ tcreToMaybe vaenv) m scheme scheme' of
      _ | not schemeChanged -> scheme'
      -- Just failVerification -> error $ printf "Scheme failed verification %s\n\t\tDuring typechecking of %s:\n\t\t New Scheme: %s \n\t\t Old Scheme: %s\n\t\t Obj: %s\n\t\t Con: %s" failVerification msg (show scheme') (show scheme) (show m) (show con)
      Just failVerification -> TypeCheckResE [GenTypeCheckError (clearMetaDat ArrMeta m) $ printf "Scheme failed verification %s\n\t\tDuring typechecking of %s:\n\t\t New Scheme: %s \n\t\t Old Scheme: %s\n\t\t Obj: %s\n\t\t Con: %s" failVerification msg (show scheme') (show scheme) (show m) (show con)]
      Nothing -> scheme'
    -- The decl channel ('stypeReqDecl') commits through its OWN path. When the act/req sides
    -- are judged unchanged, the store is refused above — deliberately: the refusal path is
    -- what keeps act/req spellings stable across epochs (see 'nestedSlotSettled'). A decl
    -- narrowing arriving on such a store must still land, but carrying the OLD act/req values
    -- — including the decl channel in 'eqScheme' instead made decl progress force
    -- whole-scheme commits whose act/req respellings the refusal path exists to block
    -- (measured: member-template erasure, @tail=∀ConsList[/T=⊤]@, across core/list.ct).
    -- Verification for this path is the decl monotonicity check alone: the act/req values
    -- are the already-stored ones (re-verifying them would also spuriously fire
    -- 'verifyCompacted' on never-verified seed presets).
    declOnlyCommit = case (scheme, scheme') of
      _ | schemeChanged -> Nothing
      (TypeCheckResult _ old@SType{stypeReqDecl=dOld}, TypeCheckResult notes SType{stypeReqDecl=dNew}) ->
        case tcreToMaybe vaenv of
          Just showVaenv
            | isEqTypeWithEnv feTypeEnv showVaenv dOld dNew -> Nothing
            | isSubtypeOfWithEnv feTypeEnv showVaenv dNew dOld ->
                -- HARMONIZE a channel that IS the arriving decl with some positions still
                -- ⊤ by adopting the decl's spelling verbatim ('slotFillAdopt'): a
                -- ⊤-slotted @Node[..](left=⊤)@ and the decl's @Node[..](left=∀BinTree[..])@
                -- denote the same declaration-bounded set, but the ⊤ spelling is what
                -- every consumer reads (binder projections deliver ⊤, the body constructs
                -- starved values, eval-time TreeBuild finds no arrows from them) — and no
                -- other write ever respells it: the enrichment previously committed only
                -- when it rode the same epoch as a genuine change (a convergence-order
                -- lottery). The guard must be SYNTACTIC and lossless — adoption fires only
                -- when every non-⊤ position already equals the decl's, so nothing can be
                -- dropped or widened. Both semantic guards failed here: an @x ∩ decl@ meet
                -- manufactures artifact spellings (a member template with its inner dim
                -- eroded to ⊤ escapes the definition⊆declaration judge — core/list.ct's
                -- restToString), and an 'isEqTypeWithEnv' gate answers class membership
                -- SAT-conservatively (@∀List ∪ raw@ ≡ @∀List@), silently dropping raw
                -- alternatives (measured: 9 failures incl. testFunctorIdentity).
                let harmonize x | slotFillAdopt x dNew = dNew
                                | otherwise = x
                in Just (TypeCheckResult notes old{stypeReqDecl=dNew, stypeAct=harmonize (stypeAct old), stypeReq=harmonize (stypeReq old)})
            | otherwise -> Just (TypeCheckResE [GenTypeCheckError (clearMetaDat ArrMeta m) $ printf "Scheme failed verification verifySchemeReqDeclLowers (decl-only commit)\n\t\tDuring typechecking of %s:\n\t\t New Scheme: %s \n\t\t Old Scheme: %s\n\t\t Obj: %s\n\t\t Con: %s" msg (show scheme') (show scheme) (show m) (show con)])
          Nothing -> Nothing
      _ -> Nothing
    pnts' | schemeChanged = IM.insert p scheme'' fePnts -- Only update if changed to avoid meaningless updates
          | Just declScheme <- declOnlyCommit = IM.insert p declScheme fePnts
          | otherwise = fePnts
    feTrace' | schemeChanged = traceConstrainChange feTrace p scheme''
             | Just declScheme <- declOnlyCommit = traceConstrainChange feTrace p declScheme
             | otherwise = feTrace

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
-- Refresh the traced constraint list and universe point EVERY epoch boundary (not just the
-- first): the constraint accumulator grows per solve fragment, and the per-point trace
-- listing must cover them all, not just the first fragment's.
nextConstrainEpoch env@FEnv{feTrace=tc@TraceConstrain{tcEpochs}, feCons, feUnionAllObjs} = case tcEpochs of
  []         -> env'
  prevEpochs -> env'{feTrace = tc'{tcEpochs=[]:prevEpochs}, feUpdatedDuringEpoch = False}
  where
    tc' = tc{tcCons=feCons, tcAllObjs=feUnionAllObjs}
    env' = env{feTrace=tc'}

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
showTraceConstrain epochs = intercalate "\n" $ map show $ reverse epochs
