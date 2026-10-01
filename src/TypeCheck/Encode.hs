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
import           TypeCheck.TypeUtils (addUnionObjToEnv,
                                      recursiveObjsWithPrecedence)
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

-- The instruments are given rather than chosen, because which of them a solve is worth watching
-- with is the question of whoever is going to read them and not of the solve.
makeBaseFEnv :: [AnyInstrument] -> PPrgm -> FEnv
-- The static env is immutable for the FEnv's lifetime, so it carries a query-cache token
-- from birth: the set-algebra meets that deliberately keep 'feTypeEnv' (rather than the
-- per-epoch reaches env) still reach the reach-query and sat-goal memos. Without it, the
-- SAT engine ran un-memoized under this env and a cyclic escape derivation re-decided the
-- same goal unboundedly.
makeBaseFEnv instruments prgm = FEnv{
  fePnts = IM.empty,
  feConsDats = [],
  feCons = [],
  feUnionAllObjs = Meta PTopType Nothing nil (VarMetaDat (Just 0) Nothing),
  feVTypeGraph = mempty,
  feTTypeGraph = mempty,
  feTTypeGraphCleared = mempty,
  feReachesVPart = mempty,
  feUpdatedDuringEpoch = False,
  feTypeEnv = tokenedTypeEnv,
  feReachesTypeEnv = tokenedTypeEnv,
  feUniverseKey = BottomType,
  feInputArrowNames = S.empty,
  feLocalDefNames = S.empty,
  feEnvStamp = 0,
  feCallArrs = IM.empty,
  feBatchDeclVars = H.empty,
  feConMemo = IM.empty,
  feInstruments = instruments
  }
  where
    baseTypeEnv = mkTypeEnv prgm
    tokenedTypeEnv = baseTypeEnv{teQueryCacheKey = Just (unsafeFreshQueryToken (teTypeGraph baseTypeEnv))}

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
  modify $ addConstraints [BoundedByKnown 6 (getExprMeta base') (TypeVar (TVArg $ inExprSingleton alias) TVInt PTopType)]
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
  -- The output base link ('ArrowTo') is emitted HERE — after the base subexpression, BEFORE the
  -- argument constraints — because the actual channel flows @baseExpr' -> baseM'@ (this ArrowTo)
  -- and then @baseM' -> m'@ (the arg's 'AddArg'), and 'runConstraintsIdx' executes in emission
  -- order. Emitted after the argument instead, every application layer's pair sat in anti-flow
  -- order: a forward epoch ran the AddArg before its ArrowTo feeder and a backward epoch the
  -- reverse, so each curried-call layer cost a full epoch per link in BOTH directions (a 7-arg
  -- call chain burned ~13 epochs reaching its head — m30/map.ct's insertStep hit the epoch limit
  -- on exactly this). In flow order the whole chain rides one forward epoch, and its requirement
  -- mirror rides the alternated backward epochs.
  -- The input base link ('EqPoints') sits here for the same reason: @baseM'@ is the source of the
  -- argument's 'AddArg', so it must be synced with the base subexpression before that runs.
  modify $ addConstraints $ case est of
    EncodeOut{} -> [ArrowTo 7 (getExprMeta baseExpr') baseM']
    EncodeIn{}  -> [EqPoints 16 (getExprMeta baseExpr') baseM']
  -- An applied argument written as a pattern (@fn(v)@, @Box(v)@) is one of two different things,
  -- told apart by whether its head names something the program defines. @Box@ does — it is a data
  -- type, and the argument it fills is not named anywhere in the pattern, so it must be inferred.
  -- @fn@ does not — nothing defines it, so it is the name of the argument the pattern fills (a
  -- higher-order parameter), and there is nothing to infer. This is the same defined-name test
  -- 'fromObjArr' uses to tell a destructuring binding variable from a reference, and the
  -- encode-time form of the head-vs-argument comparison 'exprVarArgsWithSrc' makes against a call
  -- source once one is known.
  definedNames <- getDefinedNames
  let patternHeadIsDefined e = S.member (makeAbsoluteName $ exprPath e) definedNames
  arg'' <- case arg of
    EAppArg a -> do
      arg' <- fromObjArr est a
      -- An input argument actually is only whatever it is required to be (it only ever receives
      -- declared values), so tie the argument's type slot ('arrM') actual down to its
      -- (use-narrowed) requirement. Emitting this here in the Tuple-arg handling covers every input
      -- argument form — matched-value args (@f(x= True)@, wildcards @f(x= _)@) and destructuring
      -- patterns as well as typed parameters — whereas the old 'exprVarArgs'-driven emission in
      -- 'fromObjectMap' only reached the parameter/binding forms it collected. Emitted AFTER the
      -- argument's own links below, since the requirement it pulls the actual down to is what those
      -- links deliver.
      let eqReqActs = [EqReqAct 34 arrM' | EncodeIn <- [est], Just (_, arrM') <- [oaArr arg']]
      let constraints = case (est, oaObj arg', oaArr arg') of
            (EncodeOut{}, Just obj, Just (Just _argExpr', arrM')) ->
              -- Output with (x=x). A higher-order argument (@fn(v)= body@) is an Obj/Arrow pair
              -- the call adds to its environment ('attachCallArrs'), so its slot holds the INPUT
              -- OBJECT — the lambda's pattern @fn(v)@ — and its result is the arrow's: the call's
              -- requirement on the slot (the callee's declared @fn(v -> $_A)@) reaches the
              -- pattern, and through it the parameters.
              [
                            AddArg 8 (baseM', TVArg $ inExprSingleton obj) m',
                            PropEq 11 (m', TVArg $ inExprSingleton obj) (hoSlotPoint obj arrM')
                            ]
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
            (EncodeIn{}, Just obj@TupleApply{}, Just (Nothing, arrM'))
              | patternHeadIsDefined obj ->
              -- Input with a destructuring pattern (e.g. @Box(v)@): the pattern's own structure
              -- does not name the declared argument it fills, so infer which argument it fills by
              -- type. The 'PropEq' still links the pattern's own type into that slot.
              [
                        AddInferArg 24 (getExprMeta baseExpr') m',
                        PropEq 25 (m', TVArg $ inExprSingleton obj) arrM'
                        ]
            (EncodeIn{}, Just obj, Just (Nothing, arrM')) ->
              -- Input with (x -> T), and the higher-order parameter pattern @fn(v)@ whose head
              -- names the argument it fills. Both name their argument through their own key
              -- ('inExprSingleton'), so it is added by that name like any other. A higher-order
              -- parameter's slot holds its INPUT OBJECT (@fn(/v=$_A)@) — its declared result is
              -- the arrow the parameter adds to the body's environment ('mkReachesEnv').
              [
                        AddArg 24 (getExprMeta baseExpr', TVArg $ inExprSingleton obj) m',
                        PropEq 25 (m', TVArg $ inExprSingleton obj) (hoSlotPoint obj arrM')
                        ]
            _ -> error $ printf "Invalid fromExpr in %s mode for %s" (show est) (show arg)
      modify $ addConstraints (constraints ++ eqReqActs)
      modify $ endConstraintBlock (if isJust (oaObj arg') then Just arg' else Nothing) (maybe H.empty (fmap (first getExprMeta . head) . exprVarArgs) (oaObj arg'))
      return $ EAppArg arg'
    EAppVar vn vm -> do
      let baseName = printf "VarApply %s[%s = %s]" (show baseExpr) (show vn) (show vm) :: String
      vm' <- fromMeta BAct est vm $ printf "%s val" baseName
      -- As with the 'EAppArg' inputs above, an input type-variable application's actual is only
      -- whatever it is required to be, so pull its value slot's actual down to its requirement.
      -- That 'EqReqAct' is a self-loop on the value slot with no source in this layer, and the
      -- 'PropEq' below reads the slot on BOTH channels, so it goes FIRST: run after, it leaves the
      -- projection nothing to carry on the requirement channel for a whole epoch, and by the time
      -- it has something the layer's own 'BoundedByObjs' has already filled that requirement from
      -- the objects universe — which cannot carry the object's own scope, and so spells the
      -- declaration's reference to its own variable (@[$M -> Monad[$T -> $_A]]@) as the ⊤ it
      -- denotes. The two spellings are the same SET, so the later projection is judged a no-op and
      -- the erased one is what every consumer then reads (measured: a lambda parameter's declared
      -- type, which reaches the lambda through that bound).
      modify $ addConstraints ([EqReqAct 34 vm' | EncodeIn <- [est]] ++ [AddArg 27 (baseM', TVVar vn) m', PropEq 27 (m', TVVar vn) vm'])
      return $ EAppVar vn vm'
    EAppSpread a@HoleExpr{} -> do -- Shortcut for Hole since no need to track neglected args
      a' <- fromExpr est a
      modify $ addConstraints [SetArgMode 11 False baseM' m']
      return $ EAppSpread a'
    EAppSpread a -> error $ printf "Not yet implemented %s" (show a)
  -- 'BoundedByObjs' narrows only @m'@'s requirement channels, and requirements travel backward, so
  -- emitting it last is running it first in the reversed epoch — ahead of the 'PropEq' carrying the
  -- narrowed argument slot down into the argument.
  modify $ addConstraints $ case est of
    EncodeOut{} -> [BoundedByObjs 9 m' PTopType]
    EncodeIn{}  -> [BoundedByObjs 17 m' PTopType]
  -- The higher-order arguments this call level is given — its own and every level of its base
  -- (a curried @f(a)(fn(v)= ..)(b)@ is the call at each level) — are Obj/Arrow pairs the call
  -- adds to its environment: each lambda's pattern is an object whose arrow is its body. Recorded
  -- against the level's point and attached to the constraints reading it at finalization.
  case (est, getPnt m') of
    (EncodeOut{}, Just p) -> do
      baseArrs <- gets (\e -> maybe [] (\bp -> IM.findWithDefault [] bp (feCallArrs e)) (getPnt (getExprMeta baseExpr')))
      let ownArrs = [ oa | EAppArg oa@ObjArr{oaObj=Just obj, oaArr=Just (Just _, _)} <- [arg''], not (null (exprAppliedArgs obj)) ]
      unless (null (baseArrs ++ ownArrs)) $ modify $ \e -> e{feCallArrs = IM.insert p (ownArrs ++ baseArrs) (feCallArrs e)}
    _ -> return ()
  return $ TupleApply m' (baseM', baseExpr') arg''

-- | The point an argument's slot is read from: a higher-order argument (its object applied to
-- parameters, @fn(v)@) contributes its INPUT OBJECT — the pattern's own point — while any other
-- argument contributes its value ('arrM').
hoSlotPoint :: VExpr -> VarMeta -> VarMeta
hoSlotPoint obj arrM
  | not (null (exprAppliedArgs obj)) = getExprMeta obj
  | otherwise                        = arrM

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
--
-- A higher-order argument's slot is its input object, read from its pattern ('hoSlotPoint'); its
-- result point is the arrow the argument adds to the body's environment, never the slot. A
-- destructuring pattern (@Box(v)@, whose head names a defined type) is not higher-order: its slot
-- is its value, as the per-layer handling in 'fromExpr' reads it ('patternHeadIsDefined').
topArgPropEqs :: S.HashSet Name -> VExpr -> [VConstraintDat]
topArgPropEqs definedNames obj' =
  [ PropEq 47 (getExprMeta obj', TVArg $ inExprSingleton nObj) slot
  | ObjArr{oaObj=Just nObj, oaArr=Just (_, nArrM)} <- exprAppliedArgs obj'
  , let slot | S.member (makeAbsoluteName $ exprPath nObj) definedNames = nArrM
             | otherwise                                             = hoSlotPoint nObj nArrM ]

-- | The absolute names that something in the program defines: the declared types and values of
-- the typechecked type graph, plus those the batch being encoded is adding ('feLocalDefNames').
-- A name this does not hold defines nothing, so it names a fresh local instead — a destructuring
-- binding variable, or the argument a pattern is written against.
getDefinedNames :: StateT FEnv TypeCheckResult (S.HashSet Name)
getDefinedNames = do
  ObjArrTypeGraph (ObjectMap objTg) <- gets (teTypeGraph . feTypeEnv)
  localDefNames <- gets feLocalDefNames
  return $ S.map makeAbsoluteName (H.keysSet objTg) <> localDefNames

fromObjArr :: EncodeState -> PObjArr -> StateT FEnv TypeCheckResult VObjArr
fromObjArr est oa@ObjArr{oaObj, oaAnnots, oaArr} = do
  definedNames <- getDefinedNames
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
      modify $ addConstraints $ topArgPropEqs definedNames inExpr'
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
            EncodeIn -> do
              argDefinedNames <- getDefinedNames
              modify $ addConstraints $ topArgPropEqs argDefinedNames argExpr'
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
            BoundedByKnown 18 (getExprMeta argExpr') (TypeVar (TVArg $ inExprSingleton obj) TVInt PTopType),
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

-- | A definition's references to its own arguments, spelled by the argument's absolute name.
--
-- A name written in a definition's body or guards that is one of the definition's arguments means
-- that argument, whatever else the program calls it: @count@ in a definition taking a parameter
-- @count@ is the parameter, and not @\/Data\/List\/count@. A relative name resolves to every
-- name it matches, and the parameter — which names no object — is the candidate the object bounds
-- then prune away, so the body would read the function where it wrote its argument. Spelled by
-- its absolute name, the reference matches only the argument.
--
-- Only a reference the argument can BE is taken as it: one whose applied arguments the argument's
-- own keys cover. A plain parameter takes none, and @fn(v)@ takes @v@, so a method call such as
-- @lst.count(fn(v)= ..)@ beside an Integer parameter @count@ still reaches the other names.
--
-- Rewritten before encoding rather than resolved during inference, because only here is it known
-- which values are references: a name in a pattern is a key or a binding, and the same relative
-- expansion reads both. The keys of an application's arguments are left as written for the same
-- reason — they name the slot filled, not the argument in scope.
scopeArgRefs :: [ArgName] -> PExpr -> PExpr
scopeArgRefs keys = go
  where
    go e@Value{}       = spine S.empty e
    go e@CExpr{}       = e
    go e@HoleExpr{}    = e
    go (AliasExpr b a) = AliasExpr (go b) a
    go (EWhere m b c)  = EWhere m (go b) (go c)
    go e@TupleApply{}  = spine (H.keysSet $ exprAppliedArgsMap e) e

    -- An application's spine, with the argument keys applied along the whole of it: the value at
    -- its root is what those arguments are applied to.
    spine applied (TupleApply m (bm, b) arg) = TupleApply m (bm, spine applied b) (goArg arg)
    spine applied e@(Value m n) = case [ k | k <- keys
                                           , relativeNameMatches n (pkName k)
                                           , applied `S.isSubsetOf` pkArgs k ] of
      [k] -> Value (mWithType (relTypeVal (pkName k)) m) (pkName k)
      _   -> e
    spine _ e = go e

    goArg (EAppArg a)    = EAppArg a{oaArr = fmap (first (fmap go)) (oaArr a)}
    goArg a@EAppVar{}    = a
    goArg (EAppSpread e) = EAppSpread (go e)

-- | 'scopeArgRefs' over the guard conditions of an object, which read its arguments as its body does.
scopeArgRefsInConds :: [ArgName] -> PExpr -> PExpr
scopeArgRefsInConds keys (EWhere m b c) = EWhere m (scopeArgRefsInConds keys b) (scopeArgRefs keys c)
scopeArgRefsInConds keys (AliasExpr b a) = AliasExpr (scopeArgRefsInConds keys b) a
scopeArgRefsInConds _ e = e

fromObjectMap :: PObjArr -> StateT FEnv TypeCheckResult VObjArr
fromObjectMap oa@ObjArr{oaAnnots=oaAnnots0, oaObj=Just obj0, oaArr=oaArr0} = do
  let argKeys = [ k | TVArg k <- H.keys (exprVarArgs obj0) ]
  let obj = scopeArgRefsInConds argKeys obj0
  let oaAnnots = map (scopeArgRefs argKeys) oaAnnots0
  let oaArr = fmap (first (fmap (scopeArgRefs argKeys))) oaArr0
  let inEst = EncodeIn
  modify startConstrainBlock
  obj' <- fromExpr inEst obj
  modify $ addConstraints $ nonLinearEqConstraints obj'
  objDefinedNames <- getDefinedNames
  modify $ addConstraints $ topArgPropEqs objDefinedNames obj'
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
  -- The object is read as its declarations augment it ('BoundedByObjs', the declaration taking
  -- precedence): a variable its declarations apply that it does not bind itself — the declared
  -- result of a higher-order argument (@applyInc[.., $B](.., fn(v -> $_A) -> $_B)@'s @$_B@), which
  -- an unannotated definition's @fn(v)@ arrow returns — is one of its own variables too, so a
  -- result spelled in it stays a link (bound per call at dispatch) rather than the ⊤ it would
  -- read as out of scope.
  let objName = makeAbsoluteName (exprPath obj')
  batchDeclVars <- gets (H.lookupDefault S.empty objName . feBatchDeclVars)
  ObjectMap tTg <- gets feTTypeGraph
  let depDeclVars = S.unions [ declArgResultVars decl | decl@ObjArr{oaObj=Just{}} <- maybe [] omiDecls (H.lookup objName tTg) ]
  let declVs = S.toList $ S.difference (S.union batchDeclVars depDeclVars) (S.fromList (H.keys explicitVaenv))
  let implicitVs = S.toList $ S.union (S.fromList declVs) $ S.unions $ map (outOfScopeTVExtVars scopeBase) annotTys
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

-- | Encode one solve fragment against the already-typechecked dependency objects
-- (pre-enumerated with their precedence map — see 'recursiveObjsWithPrecedence').
-- The fragment's constraints build in an EMPTY accumulator so its own finalized
-- (dataflow-ordered) list is returned for solving; the page accumulator keeps every
-- fragment's constraints appended, in solve order, for the trace.
fromPrgmsFragment :: [PPrgm] -> (TObjectMap, H.HashMap TypeName [Int]) -> StateT FEnv TypeCheckResult ([VPrgm], [VConstraint])
fromPrgmsFragment pprgms (tobjMapRec, tPrecedenceMap) = do
  accumCons <- gets feCons
  modify $ \e -> e{feCons=[], feBatchDeclVars = H.unionWith S.union (declVarsOf pprgms) (feBatchDeclVars e)}
  vprgms <- mapM fromPrgm pprgms
  addUnionObjToEnv (mconcat $ map fst3 vprgms) tobjMapRec tPrecedenceMap
  -- Last, once every constraint (including the universe's, added just above) is in: hand the
  -- solver the list in the order values flow through it rather than the reverse the O(1)
  -- accumulator built. See 'finalizeConstraints'.
  modify finalizeConstraints
  modify attachCallArrs
  fragCons <- gets feCons
  modify $ \e -> e{feCons = accumCons ++ fragCons}
  return (vprgms, fragCons)

-- | Give each constraint that reads a call the arrows that call adds to its environment: an
-- 'ArrowTo' whose source is a call point carries that call's higher-order arguments
-- ('feCallArrs'), so its reach resolves the call's @fn(v)@ input objects through them. Scoped per
-- call: two calls in one definition that each pass a @fn(v)@ keep their lambdas apart.
attachCallArrs :: FEnv -> FEnv
attachCallArrs env@FEnv{feCons, feCallArrs}
  | IM.null feCallArrs = env
  | otherwise = env{feCons = map attach feCons}
  where
    attach con@Constraint{conDat=ArrowTo _ src _}
      | Just p <- getPnt src, Just arrs <- IM.lookup p feCallArrs = con{conCallArrs = arrs}
    attach con = con

-- | The type variables each name's declarations give their higher-order arguments' RESULTS
-- (@applyInc[.., $B](.., fn(v -> $_A) -> $_B)@'s @/B@), over the programs being encoded. These are
-- what a definition reads its unannotated @fn(v)@ arrow as returning, so they join its scope; the
-- declaration's other variables (a class parameter an argument is typed by) are not the
-- definition's and are left alone.
declVarsOf :: [PPrgm] -> H.HashMap TypeName (S.HashSet TypeVarAux)
declVarsOf pprgms = H.fromListWith S.union
  [ (makeAbsoluteName name, declArgResultVars decl)
  | Prgm{prgmObjMap=ObjectMap om} <- pprgms
  , (name, omi) <- H.toList om
  , decl@ObjArr{oaObj=Just{}} <- omiDecls omi ]

-- | The variables a declaration's higher-order arguments' declared results reference.
declArgResultVars :: (MetaDat m, Show m) => ObjArr Expr m -> S.HashSet TypeVarAux
declArgResultVars decl = S.unions
  [ typeVarRefs (getMetaType aM)
  | ObjArr{oaObj=Just aObj, oaArr=Just (_, aM)} <- exprAppliedArgs (oaObjExpr decl)
  , not (null (exprAppliedArgs aObj)) ]
  where
    typeVarRefs t = case t of
      TypeVar v@TVVar{} _ _                -> S.singleton v
      TypeVar (TVApplied b@TVVar{} _) _ _  -> S.singleton b
      _                                    -> S.filter isVar (outOfScopeTVExtVars H.empty t)
    isVar TVVar{} = True
    isVar _       = False

fromPrgms :: [PPrgm] -> [TPrgm] -> StateT FEnv TypeCheckResult [VPrgm]
fromPrgms pprgms tprgms = do
  mapM_ addTypeGraphPrgm tprgms
  modify $ \e -> e{feLocalDefNames = prgmsLocalDefNames pprgms}
  (vprgms, _) <- fromPrgmsFragment pprgms (recursiveObjsWithPrecedence $ prgmObjMap $ mconcat tprgms)
  return vprgms

-- | The absolute names every object the page itself defines, set once per page.
prgmsLocalDefNames :: [PPrgm] -> S.HashSet TypeName
prgmsLocalDefNames pprgms = S.fromList $ map (makeAbsoluteName . exprPath . oaObjExpr) $ concatMap (flatObjectMap . prgmObjMap) pprgms
