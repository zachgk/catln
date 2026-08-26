--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Encode
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module encodes before type checking to extract all of the
-- types into isolated variables and all of the constraints that
-- relate the variables together. It prepares for typechecking.
--------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TypeCheck.Encode where

import           Control.Monad
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import qualified Data.IntMap.Lazy    as IM
import           Prelude             hiding (unzip)

import           Control.Monad.State
import           CtConstants         (contextValStr)
import           Data.Bifunctor      (Bifunctor (first))
import           Data.Maybe
import           Data.UUID           (nil)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           TypeCheck.Common
import           TypeCheck.TypeUtils (addUnionObjToEnv)
import           Utils

-- represents how a specified variables corresponds to the known types.
-- It could be a bound on actual, required, or both
data TypeBound = BAct | BReq | BActReq
  deriving (Eq)

data EncodeState
  = EncodeOut !(Maybe VObject) -- ^ Used for outputs including in definitions (with object) and global annots (without obj)
  | EncodeIn -- ^ Used for inputs
  deriving (Eq, Ord, Show)

isEncodeOut :: EncodeState -> Bool
isEncodeOut EncodeOut{} = True
isEncodeOut EncodeIn{}  = False

showIncodeInOut :: EncodeState -> String
showIncodeInOut EncodeOut{} = "Out"
showIncodeInOut EncodeIn{}  = "In"

asEncodeIn :: EncodeState -> EncodeState
asEncodeIn (EncodeOut _)  = EncodeIn
asEncodeIn est@EncodeIn{} = est

encodeObj :: EncodeState -> Maybe VObject
encodeObj (EncodeOut obj) = obj
encodeObj EncodeIn{}      = Nothing

makeBaseFEnv :: PPrgm -> FEnv
makeBaseFEnv prgm = FEnv{
  fePnts = IM.empty,
  feConsDats = [],
  feCons = [],
  feUnionAllObjs = Meta PTopType Nothing nil (VarMetaDat (Just 0) Nothing),
  feVTypeGraph = mempty,
  feTTypeGraph = mempty,
  feUpdatedDuringEpoch = False,
  feTypeEnv = mkTypeEnv prgm,
  feReachesTypeEnv = mkTypeEnv prgm,
  feCompletedAllObjsKey = BottomType,
  feCompletedAllObjs = PTopType,
  feInputArrowNames = S.empty,
  feLocalDefNames = S.empty,
  feEnvStamp = 0,
  feConMemo = IM.empty,
  feTrace = mkTraceConstrain
  }

mkVarMetaDat :: EncodeState -> Pnt -> VarMetaDat
mkVarMetaDat (EncodeOut obj) p = VarMetaDat (Just p) obj
mkVarMetaDat EncodeIn p        = VarMetaDat (Just p) Nothing

fromMeta :: TypeBound -> EncodeState -> PreMeta -> String -> StateT FEnv TypeCheckResult VarMeta
fromMeta bound est m description  = do
  let tp = getMetaType m
  -- A requirement-seeded preset is declaration-origin by construction (a declared return, an
  -- annotation spelling), so it also seeds the origin-pure decl channel ('stypeReqDecl').
  p <- case bound of
        BAct    -> fresh (TypeCheckResult [] $ SType tp PTopType PTopType Nothing description)
        BReq    -> fresh (TypeCheckResult [] $ SType PTopType tp tp Nothing description)
        BActReq -> fresh (TypeCheckResult [] $ SType tp tp tp Nothing description)
  return (mapMetaDat (\_ -> mkVarMetaDat est p) m)

fromExpr :: EncodeState -> PExpr -> StateT FEnv TypeCheckResult VExpr
fromExpr est (CExpr m c) = do
  m' <- fromMeta BAct est m ("Constant " ++ show c)
  modify $ addConstraints [EqualsKnown 3 m' (constantType c)]
  return $ CExpr m' c
fromExpr est@EncodeOut{} (Value m name) = do
  m' <- fromMeta BAct est m ("Out Val " ++ nameStr name)
  modify $ addConstraints [BoundedByKnown 4 m' (relTypeVal name), BoundedByObjs 4 m' PTopType]
  return $ Value m' name
fromExpr est@EncodeIn{} (Value m name) = do
  m' <- fromMeta BAct est m ("In Val " ++ nameStr name)
  modify $ addConstraints [BoundedByKnown 5 m' (relTypeVal name)]
  return $ Value m' name
fromExpr est (HoleExpr m hole) = do
  m' <- fromMeta BAct est m ("Hole " ++ show hole)
  return $ HoleExpr m' hole
fromExpr est (AliasExpr base alias) = do
  base' <- fromExpr est base
  alias' <- fromExpr est alias
  modify $ addConstraints [BoundedByKnown 6 (getExprMeta base') (TypeVar (TVArg $ inExprSingleton alias) TVInt)]
  return $ AliasExpr base' alias'
fromExpr est (EWhere m base cond) = do
  m' <- fromMeta BAct est m (printf "Where %s | %s" (show base) (show cond))
  base' <- fromExpr est base
  let condEst = case est of
        EncodeIn -> EncodeOut Nothing
        _        -> est
  cond' <- fromExpr condEst cond
  bool <- fresh $ TypeCheckResult [] $ SType PTopType boolType boolType Nothing "ifGuardBool"
  let bool' = Meta boolType (getMetaPos $ getExprMeta cond) nil (mkVarMetaDat est bool)
  modify $ addConstraints [ArrowTo 30 (getExprMeta cond') bool', ConWhere 30 (getExprMeta base') (getExprMeta cond') m']
  return $ EWhere m' base' cond'
fromExpr est (TupleApply m (baseM, baseExpr) arg) = do
  m' <- fromMeta BAct est m $ printf "Tuple Meta %s %s(%s)" (showIncodeInOut est) (show baseExpr) (show arg)
  baseM' <- fromMeta BAct est baseM $ printf "Tuple BaseMeta %s %s(%s)" (showIncodeInOut est) (show baseExpr) (show arg)
  baseExpr' <- fromExpr est baseExpr
  arg'' <- case arg of
    EAppArg a -> do
      arg' <- fromObjArr est a
      -- An input argument actually is only whatever it is required to be (it only ever receives
      -- declared values), so tie the argument's type slot ('arrM') actual down to its
      -- (use-narrowed) requirement. Emitting this here in the Tuple-arg handling covers every input
      -- argument form — matched-value args (@f(x= True)@, wildcards @f(x= _)@) and destructuring
      -- patterns as well as typed parameters — whereas the old 'exprVarArgs'-driven emission in
      -- 'fromObjectMap' only reached the parameter/binding forms it collected.
      let eqReqActs = [EqReqAct 34 arrM' | EncodeIn <- [est], Just (_, arrM') <- [oaArr arg']]
      let constraints = case (est, oaObj arg', oaArr arg') of
            (EncodeOut{}, Just obj, Just (Just _argExpr', arrM')) ->
              -- Output with (x=x)
              [
                            AddArg 8 (baseM', TVArg $ inExprSingleton obj) m',
                            PropEq 11 (m', TVArg $ inExprSingleton obj) arrM'
                            ]
              -- A lambda argument (@fn(v)= v@): each of its parameters is typed by the callee's
              -- declared input type for that position (@fn(v -> $_A)@), resolved per call site
              -- during solving (see 'fnArgInputBound'). Without this nothing constrains the
              -- lambda's parameter, so its body — and hence the function's output — stays Top.
              ++ [ FnInputBound 45 (m', inExprSingleton obj, inExprSingleton nObj) nArrM
                 | ObjArr{oaObj=Just nObj, oaArr=Just (_, nArrM)} <- exprAppliedArgs obj ]
            (EncodeOut{}, Nothing, Just (Just _argExpr', _arrM')) ->
              -- Output with (x) infer
              [
                            AddInferArg 13 baseM' m'
                            ]
            (EncodeIn{}, Just obj, Just (Just _argExpr', arrM')) ->
              -- Input with (x=x)
              -- Fix B (Bug 1): use the base expression's own meta (not the separate
              -- baseM' slot) as the AddArg source. baseM' is kept equal to the base expr
              -- via EqPoints 16, but reading the base expr directly keeps the added
              -- argument's base in sync with the pattern even before that link settles.
              [
                        AddArg 19 (getExprMeta baseExpr', TVArg $ inExprSingleton obj) m',
                        PropEq 21 (m', TVArg $ inExprSingleton obj) arrM'
                        ]
            (EncodeIn{}, Just obj@TupleApply{}, Just (Nothing, arrM')) ->
              -- Input with a destructuring pattern (e.g. @Box(v)@): the pattern's own
              -- structure does not name the declared argument it fills, so infer which
              -- argument it fills by type. The 'PropEq' is kept so that when the pattern's
              -- structural key /does/ name the argument (e.g. a higher-order @f(y)@ where
              -- @f@ is the declared arg) the argument's annotated type is still linked in;
              -- when it does not (the @Box(v)@ case) the 'PropEq' is a harmless no-op.
              [
                        AddInferArg 24 (getExprMeta baseExpr') m',
                        PropEq 25 (m', TVArg $ inExprSingleton obj) arrM'
                        ]
            (EncodeIn{}, Just obj, Just (Nothing, arrM')) ->
              -- Input with (x -> T)
              [
                        AddArg 24 (getExprMeta baseExpr', TVArg $ inExprSingleton obj) m',
                        PropEq 25 (m', TVArg $ inExprSingleton obj) arrM'
                        ]
            _ -> error $ printf "Invalid fromExpr in %s mode for %s" (show est) (show arg)
      modify $ addConstraints (eqReqActs ++ constraints)
      modify $ endConstraintBlock (if isJust (oaObj arg') then Just arg' else Nothing) (maybe H.empty (fmap (first getExprMeta . head) . exprVarArgs) (oaObj arg'))
      return $ EAppArg arg'
    EAppVar vn vm -> do
      let baseName = printf "VarApply %s[%s = %s]" (show baseExpr) (show vn) (show vm) :: String
      vm' <- fromMeta BAct est vm $ printf "%s val" baseName
      -- As with the 'EAppArg' inputs above, an input type-variable application's actual is only
      -- whatever it is required to be, so pull its value slot's actual down to its requirement.
      modify $ addConstraints ([AddArg 27 (baseM', TVVar vn) m', PropEq 27 (m', TVVar vn) vm'] ++ [EqReqAct 34 vm' | EncodeIn <- [est]])
      return $ EAppVar vn vm'
    EAppSpread a@HoleExpr{} -> do -- Shortcut for Hole since no need to track neglected args
      a' <- fromExpr est a
      modify $ addConstraints [SetArgMode 11 False baseM' m']
      return $ EAppSpread a'
    EAppSpread a -> error $ printf "Not yet implemented %s" (show a)
  let baseConstraints = case est of
        EncodeOut{} -> [
                ArrowTo 7 (getExprMeta baseExpr') baseM',
                BoundedByObjs 9 m' PTopType
                       ]
        EncodeIn{} -> [
                EqPoints 16 (getExprMeta baseExpr') baseM',
                BoundedByObjs 17 m' PTopType
                      ]
  modify $ addConstraints baseConstraints
  return $ TupleApply m' (baseM', baseExpr') arg''

-- | Non-linear matching: when a pattern binds one variable to several argument positions
-- (e.g. @f(a@x, b@x)@), those positions must share a type. Encode that by equating their type
-- points. The implicit requirement that the type be an 'Eq' (needed to compare the positions at
-- runtime) is intentionally left for 'TreeBuild'/'Eval' to enforce.
nonLinearEqConstraints :: VExpr -> [VConstraintDat]
nonLinearEqConstraints obj =
  [ EqPoints 40 m1 m2
  | grp <- exprNonLinearArgGroups obj
  , ((_, m1), (_, m2)) <- zip grp (tail grp)
  ]

-- | An annotation is a root statement: nothing consumes it, so no 'ArrowTo' source position
-- ever writes its requirement (application metas otherwise get their requirements from their
-- consumers). An annotation's requirement is its own spelled annotation — seed the
-- application meta with the annotation name's relation type, which 'BoundedByObjs' then
-- narrows to the declared annotation object (e.g. @#assert(test -> Boolean)@), giving the
-- argument slots their requirements through the usual arg projection ('PropEq').
annotSeedConstraints :: [VExpr] -> [VConstraintDat]
annotSeedConstraints = map (\a -> BoundedByKnown 46 (getExprMeta a) (relTypeAppliedVal (exprPath a)))

-- | Each destructured argument's binder ADDITIONALLY reads its slot from the COMPLETE
-- object form — the spine's outermost meta. The object universe only enriches the
-- full-arity form (a partial application's arg keyset matches no declared object, so
-- 'BoundedByObjs'\/'AddArg' completion deliver declared content only at the outermost
-- layer), while the per-layer 'PropEq' emitted where each arg is APPLIED reads the
-- still-⊤ inner layer for every arg but the last: the binders starve
-- (@Node(left, key, value, right)@ resolves only @right@), the definition body reads ⊤
-- args, and the constructed value fails TreeBuild against its declared slots — the
-- curried-layer starvation behind the order-independent @==@ examples.
topArgPropEqs :: VExpr -> [VConstraintDat]
topArgPropEqs obj' =
  [ PropEq 47 (getExprMeta obj', TVArg $ inExprSingleton nObj) nArrM
  | ObjArr{oaObj=Just nObj, oaArr=Just (_, nArrM)} <- exprAppliedArgs obj' ]

fromObjArr :: EncodeState -> PObjArr -> StateT FEnv TypeCheckResult VObjArr
fromObjArr est oa@ObjArr{oaObj, oaAnnots, oaArr} = do
  ObjArrTypeGraph (ObjectMap objTg) <- gets (teTypeGraph . feTypeEnv)
  localDefNames <- gets feLocalDefNames
  let definedNames = (S.map makeAbsoluteName $ H.keysSet objTg) <> localDefNames
  -- A value in a @field= value@ destructuring position is a /binding variable/ (it names a
  -- fresh local, e.g. @field= x@) rather than a /reference/ to an existing definition — a
  -- declared type\/value to pattern-match against (e.g. @field= True@) or another binding
  -- referenced by name. The two share a 'Value' AST but must be encoded differently: a
  -- binding is open and receives the field's declared type (see below), whereas a reference
  -- keeps its own resolved type. A name is a binding exactly when nothing defines it.
  let isBindingVar t = case maybeGetSingleton t of
        Just p  -> not $ S.member (makeAbsoluteName $ ptName p) definedNames
        Nothing -> False
  oaObj' <- case oaObj of
    Just inExpr -> do
      inExpr' <- fromExpr (asEncodeIn est) inExpr
      modify $ addConstraints $ topArgPropEqs inExpr'
      return $ Just inExpr'
    Nothing -> return Nothing
  modify $ addConstraints $ maybe [] nonLinearEqConstraints oaObj'
  modify startConstrainBlock
  oaAnnots' <- mapM (fromExpr est) oaAnnots
  modify $ addConstraints $ annotSeedConstraints oaAnnots'
  oaArr' <- case oaArr of
    Nothing -> return Nothing
    Just (arrExpr, arrM) -> do
      case arrExpr of
        -- A destructuring binding variable (the @x@ in @field= x@): both the binding
        -- value and the field's arrow slot are seeded open rather than as the nominal
        -- singleton of the binding's own name. The field's declared type flows into the
        -- slot (and thence the binding) via the links below; a competing name singleton
        -- would instead intersect with the field's declared type to bottom, since a fresh
        -- binding name is not a member of that type. A non-binding pattern value (e.g.
        -- @field= True@) keeps the normal encoding so it still matches against the field.
        -- The desugared context wrappers (@/ContextIn(value= /main)@) fill the reserved
        -- @value@ field with the wrapped definition, not a fresh binding, so they are
        -- excluded by field name.
        Just (Value m name)
          | EncodeIn <- est
          , maybe True ((/= contextValStr) . nameStr . makeAbsoluteName . exprPath) oaObj
          , getMetaType arrM == PTopType
          , isBindingVar (getMetaType m) -> do
          arrM' <- fromMeta BReq est (mWithType PTopType arrM) $ printf "oa arrM %s" (show oa)
          m' <- fromMeta BReq est (mWithType PTopType m) ("In Bind " ++ nameStr name)
          return $ Just (Just (Value m' name), arrM')
        Just argExpr -> do
          arrM' <- fromMeta BAct est arrM $ printf "oa arrM %s" (show oa)
          argExpr' <- fromExpr est argExpr
          -- An In-mode destructure in ARG-VALUE position (@this= Node(left, ..)@,
          -- @m= Node(left, key@nk, ..)@) is an object spine too — its binders need the
          -- complete-form links just like a definition object's.
          case est of
            EncodeIn -> modify $ addConstraints $ topArgPropEqs argExpr'
            _        -> return ()
          return $ Just (Just argExpr', arrM')
        Nothing -> do
          arrM' <- fromMeta BAct est arrM $ printf "oa arrM %s" (show oa)
          return $ Just (Nothing, arrM')
  let constraints = case (est, oaObj', oaArr') of
        (EncodeOut{}, Just _obj, Just (Just argExpr', arrM')) ->
          -- Output with (x=x)
          [
                        BoundedByObjs 10 arrM' PTopType,
                        ArrowTo 10 (getExprMeta argExpr') arrM'
                        ]
        (EncodeOut{}, Nothing, Just (Just argExpr', arrM')) ->
          -- Output with (x) infer
          [
                        BoundedByObjs 15 arrM' PTopType,
                        ArrowTo 15 (getExprMeta argExpr') arrM'
                        ]
        (EncodeIn{}, Just obj, Just (Just argExpr', arrM')) ->
          -- Input with (x=x)
          [
            BoundedByObjs 18 (getExprMeta argExpr') PTopType,
            BoundedByKnown 18 (getExprMeta argExpr') (TypeVar (TVArg $ inExprSingleton obj) TVInt),
                     EqPoints 20 (getExprMeta argExpr') arrM'
                    ]
        (EncodeIn{}, Just _obj, Just (Nothing, _arrM')) ->
          -- Input with (x -> T)
          []
  modify $ addConstraints constraints
  let oa' = oa{
        oaObj=oaObj',
        oaArr=oaArr',
        oaAnnots=oaAnnots'
        }
  return oa'

fromObjectMap :: PObjArr -> StateT FEnv TypeCheckResult VObjArr
fromObjectMap oa@ObjArr{oaAnnots, oaObj=Just obj, oaArr} = do
  let inEst = EncodeIn
  modify startConstrainBlock
  obj' <- fromExpr inEst obj
  modify $ addConstraints $ nonLinearEqConstraints obj'
  modify $ addConstraints $ topArgPropEqs obj'
  -- The input arguments' 'EqReqAct' constraints (tying each arg's actual down to its requirement)
  -- are emitted per-argument in 'fromExpr' (the Tuple-arg handling), covering every argument form.

  let est = EncodeOut (Just obj')
  oaAnnots' <- mapM (fromExpr est) oaAnnots
  modify $ addConstraints $ annotSeedConstraints oaAnnots'
  oaArr' <- forM oaArr $ \(maybeArrE, arrM) -> do
    mUserReturn' <- fromMeta BAct est arrM (printf "Specified result from %s" (show $ exprPath obj'))
    case maybeArrE of
      Just arrE -> do
        vExpr <- fromExpr est arrE
        -- The Arrow result carries the declared return as its REQUIREMENT (the actual is
        -- inferred from the body): the ArrowTo destination bound that lets the arrow choose
        -- among definitions by the required result ('arrowConstrainUbs').
        arrM' <- fromMeta BReq est arrM $ printf "Arrow result from %s" (show $ exprPath obj')
        modify $ addConstraints [ArrowTo 32 (getExprMeta vExpr) arrM', ArrowTo 33 (getExprMeta vExpr) mUserReturn', NoReturnArg 33 arrM']
        return (Just vExpr, arrM')
      Nothing -> return (Nothing, mUserReturn')
  let oa' = oa{oaObj=Just obj', oaArr=oaArr', oaAnnots=oaAnnots'}
  modify $ fAddVTypeGraph oa'
  -- The object's IMPLICIT type variables — external spellings introduced through usage in its
  -- argument or result annotations (@unwrap(b -> Box[$T -> $_T]) -> $_T@) rather than by an
  -- explicit application (@unwrap[$U]@, collected into the block vaenv by 'exprVarArgs') —
  -- belong to the object's OWN scope: they are its cross-occurrence links, and the declared
  -- result resolves through them at every call ('arrowDestType' via 'exprVarArgsWithSrc').
  -- Register each with a fresh point so the scope normalization applied at every store
  -- ('setScheme'/'dropOutOfScopeTVExt', which reads an UNREGISTERED external spelling as the ⊤
  -- it denotes) keeps the object's own links symbolic instead of erasing them.
  let explicitVaenv = first getExprMeta . head <$> exprVarArgs obj'
  let annotTys = [getMetaType m | ObjArr{oaArr=Just (_, m)} <- exprAppliedArgs obj']
              ++ [getMetaType m | (_, m) <- maybeToList oaArr']
  let scopeBase = H.fromList [(k, PTopType) | k <- H.keys explicitVaenv]
  let implicitVs = S.toList $ S.unions $ map (outOfScopeTVExtVars scopeBase) annotTys
  implicitVaenv <- fmap H.fromList $ forM implicitVs $ \v -> do
    p <- fresh $ TypeCheckResult [] $ SType PTopType PTopType PTopType Nothing $ printf "implicit object var %s in %s" (show v) (show $ exprPath obj')
    let vm = Meta PTopType (getMetaPos $ getExprMeta obj') nil (mkVarMetaDat inEst p)
    return (v, (vm, vm))
  modify $ endConstraintBlock (Just oa') (H.union explicitVaenv implicitVaenv)
  return oa'
fromObjectMap oa = error $ printf "Invalid oa in fromObjectMap %s" (show oa)

fromPrgm :: PPrgm -> StateT FEnv TypeCheckResult VPrgm
fromPrgm (Prgm objMap classGraph annots) = do
  objMap' <- mapMObjectMap fromObjectMap objMap

  modify startConstrainBlock
  annots' <- mapM (fromExpr (EncodeOut Nothing)) annots
  modify $ addConstraints $ annotSeedConstraints annots'
  modify $ endConstraintBlock Nothing H.empty

  return (objMap', classGraph, annots')

addTypeGraphArrow :: TObjArr -> StateT FEnv TypeCheckResult ()
addTypeGraphArrow oa = modify (fAddTTypeGraph oa)

addTypeGraphPrgm :: TPrgm -> StateT FEnv TypeCheckResult ()
addTypeGraphPrgm Prgm{prgmObjMap} = mapM_ addTypeGraphArrow $ flatObjectMap prgmObjMap

fromPrgms :: [PPrgm] -> [TPrgm] -> StateT FEnv TypeCheckResult [VPrgm]
fromPrgms pprgms tprgms = do
  mapM_ addTypeGraphPrgm tprgms
  let localNames = S.fromList $ map (makeAbsoluteName . exprPath . oaObjExpr) $ concatMap (flatObjectMap . prgmObjMap) pprgms
  modify $ \e -> e{feLocalDefNames = localNames}
  vprgms <- mapM fromPrgm pprgms
  let (Prgm{prgmObjMap=tjoinObjMap}) = mconcat tprgms
  addUnionObjToEnv (mconcat $ map fst3 vprgms) tjoinObjMap
  -- Last, once every constraint (including the universe's, added just above) is in: hand the
  -- solver the list in the order values flow through it rather than the reverse the O(1)
  -- accumulator built. See 'finalizeConstraints'.
  modify finalizeConstraints
  return vprgms
