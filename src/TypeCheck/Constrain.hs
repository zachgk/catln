{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Constrain
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines the 'runConstraints'. It executes the set
-- of type variable constraints against the type variables until
-- the variables converge. It tries to execute each constraint
-- and then determines if it is changing the variable.
--------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns  #-}

module TypeCheck.Constrain where

import qualified Data.HashMap.Strict        as H
import qualified Data.HashSet               as S
import qualified Data.IntMap.Strict         as IM
import           Data.List                  (intercalate, partition)
import           Data.Maybe
import qualified Data.Zip                   as Z

import           Control.Monad.State.Strict
import           CtConstants                (macroThisKey)
import           Semantics                  (ObjArrTypeGraph (..),
                                             classMemberLeaves,
                                             convertingMembers,
                                             memberAsDeclared,
                                             setDeclsVersionName,
                                             stampQueryToken)
import           Semantics.Prgm             (Expr, Meta (..), ObjArr (..),
                                             ObjectBasis (ArgObj),
                                             ObjectMap (..), ObjectMapItem (..),
                                             exprAppliedArgs, exprVarArgs,
                                             getExprMeta, inExprSingleton,
                                             oaObjExpr, pattern ObjectMap)
import           Semantics.TypeGraph        (ReachesEnv (..),
                                             typeGuaranteedWithin)
import           Semantics.Types
import           Text.Printf
import           TypeCheck.Common
import           TypeCheck.Show
import           TypeCheck.TypeUtils

-- | Checks if a scheme is solved because it's upper bound and lower bound are equal
isSolved :: Scheme -> Bool
isSolved _                                 = False

-- | Updates a 'VarMeta' point to a new 'Scheme' value
setScheme :: FEnv -> VConstraint -> (RConstraint, RConstraint) -> VarMeta -> Scheme -> String -> FEnv
setScheme env con (oldCon, newCon) p scheme baseMsg = setDescriptor env con p (checkScheme (fmap normalizeScope scheme)) (msg "" "")
  where
    -- A stored point type may reference only variables its own constraint scope binds: nested
    -- external spellings from foreign declarations denote ⊤ and are read as ⊤
    -- ('dropOutOfScopeTVExt'), so denotationally-equal forms cannot flip between spellings
    -- across epochs. In-scope variables (and bare top-level ones) stay symbolic.
    scopeKeys = fmap (const PTopType) (constraintVarArgEnv con)
    normalizeScope s@SType{stypeAct=act, stypeReq=req, stypeReqDecl=reqDecl} =
      s{stypeAct=dropOutOfScopeTVExt (feTypeEnv env) (ambientEnv $ scopeKeys) act, stypeReq=dropOutOfScopeTVExt (feTypeEnv env) (ambientEnv $ scopeKeys) req, stypeReqDecl=dropOutOfScopeTVExt (feTypeEnv env) (ambientEnv $ scopeKeys) reqDecl}
    -- checkScheme (TypeCheckResult _ (SType ub _ desc)) | containsBottomType ub = error $ msg desc $ printf "Actual type contains bottomType: %s" (show ub)
    checkScheme (TypeCheckResult notes (SType ub _ _ _ desc)) | containsBottomType ub = TypeCheckResE (mkTracedTypeCheckError env p (msg desc "Actual type contains bottomType") : notes)
    checkScheme (TypeCheckResult notes (SType _ req _ _ desc)) | containsBottomType req = TypeCheckResE (mkTracedTypeCheckError env p (msg desc "Required type contains bottomType") : notes)
    checkScheme (TypeCheckResult notes (SType _ _ reqDecl _ desc)) | containsBottomType reqDecl = TypeCheckResE (mkTracedTypeCheckError env p (msg desc "Required declaration type contains bottomType") : notes)
    -- NOTE: the @act ⊆ req@ invariant is NOT enforced here. During inference a requirement (req)
    -- may legitimately run ahead of the still-widening actual (act) — e.g. an 'ArrowTo' dest that
    -- requires @Integer@ while its actual is still the wider @Integer ∪ Float@ reach. The invariant
    -- is checked once, after convergence, in 'TypeCheck.Decode.toMeta'.
    checkScheme s = s

    msg :: String -> String -> String
    msg desc problem = printf "Scheme failed check at setScheme %s(point %s): \n\t %s - %s \n\t Constraining:%s\n\t\tto: %s\n" baseMsg (show p) problem desc (show oldCon) (show newCon)

setSchemeAct :: FEnv -> VConstraint -> (RConstraint, RConstraint) -> VarMeta -> Type -> String -> FEnv
setSchemeAct env con cons p t msg = setScheme env con cons p scheme' ("Act " ++ msg)
  where
    scheme = descriptor env p
    scheme' = fmap (\s -> s{stypeAct=t}) scheme

setSchemeReq :: FEnv -> VConstraint -> (RConstraint, RConstraint) -> VarMeta -> Type -> String -> FEnv
setSchemeReq env con cons p t msg = setScheme env con cons p scheme' ("Req " ++ msg)
  where
    scheme = descriptor env p
    scheme' = fmap (\s -> s{stypeReq=t}) scheme

setSchemeActReq :: FEnv -> VConstraint -> (RConstraint, RConstraint) -> SchemeActReq -> VarMeta -> Type -> String -> FEnv
setSchemeActReq env con cons SchemeAct p t msg = setSchemeAct env con cons p t msg
setSchemeActReq env con cons SchemeReq p t msg = setSchemeReq env con cons p t msg

setSchemeConVaenv :: FEnv -> VConstraint -> (RConstraint, RConstraint) -> SchemeActReq -> TypeVarArgEnv -> String -> FEnv
setSchemeConVaenv env con cons actOrReq types msg = foldr (\(m, t) env' -> setSchemeActReq env' con cons actOrReq m t msg) env (H.elems $ Z.zip (constraintVarArgEnv con) (envAmbient types))

-- | Tries to join two 'SType' as equal to each other and returns their updated values
equalizeSTypes :: FEnv -> TypeVarArgEnv -> (SType, SType) -> (TypeVarArgEnv, SType, SType)
equalizeSTypes FEnv{feTypeEnv} vaenv (stype1@SType{stypeAct=act1, stypeReq=req1, stypeReqDecl=reqDecl1}, stype2@SType{stypeAct=act2, stypeReq=req2, stypeReqDecl=reqDecl2}) = do
  let (vaenv'1, actBoth) = intersectTypesWithVarEnv feTypeEnv vaenv act1 act2
  let (vaenv'2, reqBoth) = intersectTypesWithVarEnv feTypeEnv vaenv req1 req2
  -- Equalized points denote the same term, so both decl-origin requirement spellings apply.
  -- The meet's var discoveries are NOT folded into the returned env: the decl channel is a
  -- passenger here and must not perturb the actual-side var resolution. Identical spellings
  -- skip the meet (a per-epoch respell of the same set — the decl channel's own comparisons
  -- are spelling-insensitive, so nothing downstream depends on the respelled form).
  let reqDeclBoth | reqDecl1 == reqDecl2 = reqDecl1
                  | otherwise            = intersectTypesEnv feTypeEnv vaenv reqDecl1 reqDecl2
  let vaenv' = H.unionWith (intersectTypes feTypeEnv) vaenv'1 vaenv'2
  (vaenv', stype1{stypeAct=actBoth, stypeReq=reqBoth, stypeReqDecl=reqDeclBoth}, stype2{stypeAct=actBoth, stypeReq=reqBoth, stypeReqDecl=reqDeclBoth})


-- | A helper for the 'PropEq' 'Constraint' that applies to both the actual and required types.
-- After the meets, the slot settle ('settlePropSlot') drops settle-forced unsettled forms from
-- the super's actual slot and tightens its requirement slot to the declared-bound meet.
updateSchemeProp :: FEnv -> (S.HashSet TypeVarAux, S.HashSet TypeVarAux) -> STypeVarArgEnv -> SType -> TypeVarAux -> SType -> (STypeVarArgEnv, SType, SType)
updateSchemeProp env@FEnv{feTypeEnv} (lambdaArgs, defArgs) vaenv super@SType{stypeAct=superAct, stypeReq=superReq, stypeReqDecl=superReqDecl} propName sub@SType{stypeAct=subAct, stypeReq=subReq, stypeReqDecl=subReqDecl} = (vaenv', super{stypeAct=superAct'', stypeReq=superReq'', stypeReqDecl=superReqDecl''}, sub{stypeAct=subAct', stypeReq=subReq', stypeReqDecl=subReqDecl'})
  where
    (actVaenv', superAct', subAct') = updateTypeProp feTypeEnv (ambientEnv $ fmap stypeAct vaenv) superAct propName subAct
    -- A requirement-side projection never commits a respell that LOSES a type-var spelling
    -- (@∀Map[/K=$/MK, /V=$/MV]@ meeting a sub whose frame reads the pointers as ⊤ would
    -- store @∀Map[/K=⊤, /V=⊤]@ — read WIDER, failing 'verifySchemeReqLowers'): the same
    -- 'typeVarLost' guard the reverse narrowing applies. The refused projection re-runs next
    -- epoch once the sub's spelling resolves; the actual side is untouched (its meets only
    -- gain spellings here).
    (reqVaenv', superReq'0, subReq'0) = updateTypeProp feTypeEnv (ambientEnv $ fmap stypeReq vaenv) superReq propName subReq
    superReq' | typeVarLost superReq superReq'0 = superReq
              | otherwise                       = superReq'0
    subReq' | typeVarLost subReq subReq'0 = subReq
            | otherwise                   = subReq'0
    -- The slot projection is a spelled-structure relation, so the decl channel flows through it
    -- too — this is how a pattern's declared field content reaches a binding variable's point.
    -- Fully-inert channel (both sides ⊤) skips the projection outright.
    (declVaenv', superReqDecl'0, subReqDecl'0)
      | superReqDecl == PTopType && subReqDecl == PTopType = (ambientEnv $ fmap stypeReqDecl vaenv, superReqDecl, subReqDecl)
      | otherwise = updateTypeProp feTypeEnv (ambientEnv $ fmap stypeReqDecl vaenv) superReqDecl propName subReqDecl
    superReqDecl' | typeVarLost superReqDecl superReqDecl'0 = superReqDecl
                  | otherwise                               = superReqDecl'0
    subReqDecl' | typeVarLost subReqDecl subReqDecl'0 = subReqDecl
                | otherwise                           = subReqDecl'0
    (superAct'', superReq'', superReqDecl'') = settlePropSlot env (ambientEnv $ fmap stypeAct vaenv) propName superAct' superReq' superReqDecl'
    vaenv' = argRefReq $ H.intersectionWith (\t s -> s{stypeReqDecl=t}) (envAmbient declVaenv') $ H.intersectionWith (\t s -> s{stypeReq=t}) (envAmbient reqVaenv') $ H.mapWithKey argRefAct $ H.intersectionWith (\t s -> (t, s)) (envAmbient actVaenv') vaenv
    -- The same for the actual: a slot holding a bare reference to an argument reads the
    -- argument's point as its own, so the slot's actual is met into the argument's. What the
    -- slot holds is what it takes once converted ('useNeeds'), so the argument keeps the values
    -- that convert to it: a @String\/Text@ given where a @\/Catln\/CStr@ is taken is still one the
    -- argument may be.
    argRefAct tv (t, s) = case subAct of
      TypeVar stv@TVArg{} _ _ | stv == tv, t /= stypeAct s -> s{stypeAct = keepConverting (stypeAct s) t}
      _ -> s{stypeAct=t}
    -- A slot whose value is a bare reference to one of a lambda's own arguments (@r= v@ in
    -- @fn(acc, v)= acc + v@) IS that argument, so what the slot is required to be the argument is
    -- required to be: the requirement is what its use needs of it. The slot's own requirement is
    -- narrowed by the projection above, but the argument's point is reached only through the
    -- constraint's environment, so the argument's requirement is met with it there. This is how a
    -- lambda's @v@ learns from @+@'s declaration that the result it must return makes it an
    -- @Integer@, and so that the list it folds is a list of them.
    --
    -- A lambda's and a definition's alike: what a definition accepts follows both from what its
    -- signature spells and from what its body needs, so a use's requirement narrows its argument
    -- whether or not a declaration covers it (the bind continuation @cont(arg@x)= x + 1@ accepts
    -- only what @+@ does). A DECLARATION states what it accepts and is not narrowed by any body;
    -- a definition narrowed below its declaration leaves the declaration unfulfilled. For a
    -- definition an open class is a requirement like any other (@x + 1@ needs @x ∈ ∀Add@): the
    -- meet with it is exact, and a type in scope that is not a member never becomes one. A
    -- lambda's argument takes only a closed requirement, as for every requirement read from a
    -- destination ('closedType'): the declaration it is given to passes it a value, and an open
    -- class read there as that value (@filter(lst= [], fn(v)= v > 2)@ giving @/A=∀Ord@) is no
    -- value a result can be built from. A lambda still narrows to a closed type through what it
    -- calls: @cont(arg= v)@ with @v ∈ Maybe ∪ Integer@ and @arg ∈ ∀Add@ requires @Integer@.
    argRefReq e = case subAct of
      TypeVar tv@TVArg{} _ _ | subReq' /= PTopType, S.member tv defArgs || (S.member tv lambdaArgs && closedType feTypeEnv subReq')
                             , Just s <- H.lookup tv e
                             ->
        H.insert tv s{stypeReq = narrowReq (stypeReq s)} e
      _ -> e
    -- An argument required as its declared variable (@a -> $T@) keeps that spelling and carries
    -- the use's requirement as its inner, which lies within it; met as a value, the variable would
    -- read as ⊤ and be dropped.
    narrowReq (TypeVar v loc i) = TypeVar v loc (useNeeds i)
    narrowReq r                 = useNeeds r
    -- What a use needs of an argument is a value the slot it is given in can take: one within the
    -- slot's requirement, or one that converts to it, as the slot is built through the arrows that
    -- reach its type ('TreeBuild.toTExprDest'). A @String\/Text@ given where a @\/Catln\/CStr@ is
    -- required converts to one, so it is not ruled out of the argument by being given there. The
    -- members are read through the class the argument is typed by, as a class itself converts to
    -- nothing ('reaches' of a class source is a leaf).
    useNeeds r = keepConverting r (intersectTypes feTypeEnv r subReq')
    -- A type narrowed to @narrowed@, with what it was that converts into it kept. What converts is
    -- read from the class arrows ('classArrowsOf'): the members of a class one is written from that
    -- reach what is narrowed to, whatever the type was read as so far — an argument still ⊤ when a
    -- use first narrows it keeps the members it may yet be.
    -- A member within what the argument was is kept as the class spells it, which is the spelling
    -- the coverage of a declaration over the class reads ('narrowByClassArrows').
    keepConverting was narrowed = unionAllTypes feTypeEnv (narrowed : [ keep (memberAsDeclared feTypeEnv l) | l <- converting narrowed ])
      where keep m | isSubtypeOf feTypeEnv m was = m
                   | otherwise                    = intersectTypes feTypeEnv was m
    converting = convertingMembers feTypeEnv

-- | The arguments of the bodies a constraint is inside, as (the lambdas', the definitions'): each
-- enclosing object with a body that takes parameters binds its own — a lambda a call is given
-- (@fn(acc, v)= ..@, an argument object), or a definition, including the continuation a bind
-- desugars to (an object with an arrow, whatever its basis).
bodyArgsOf :: Constraint p -> (S.HashSet TypeVarAux, S.HashSet TypeVarAux)
bodyArgsOf Constraint{conOs} = (argsWhere (== ArgObj), argsWhere (/= ArgObj))
  where
    argsWhere basisOk = S.fromList
      [ k
      | oa@ObjArr{oaObj=Just obj, oaArr=Just (Just _, _)} <- conOs
      , basisOk (oaBasis oa), not (null (exprAppliedArgs obj))
      , k <- H.keys (exprVarArgs obj) ]

-- | The argument an application inferred: the one key its actual holds that its base's does not,
-- once each is a single partial. 'Nothing' while inference still offers more than one, which is
-- the reading 'TypeCheck.Decode.toExpr' gives its @learnedArgName@ once inference has finished.
inferredArgName :: SType -> SType -> Maybe TypeVarAux
inferredArgName SType{stypeAct=UnionType Nothing baseLeafs []} SType{stypeAct=UnionType Nothing appLeafs []} =
  case (splitUnionType baseLeafs, splitUnionType appLeafs) of
    ([PartialType{ptArgs=baseArgs}], [PartialType{ptArgs=appArgs}]) -> case H.keys (H.difference appArgs baseArgs) of
      [argName] -> Just (TVArg argName)
      _         -> Nothing
    _ -> Nothing
inferredArgName _ _ = Nothing

-- | A helper for the 'AddArg' 'Constraint'
addArgToType :: FEnv -> TypeVarArgEnv -> Type -> TypeVarAux -> Maybe Type
addArgToType _ _ PTopType _ = Nothing
-- A base read through an arrow ('Encode' links a call's base by 'ArrowTo') holds both readings of
-- the call: the base left as written (@inner(input)@, a positive leaf) and the base followed to
-- what it evaluates to (@inner(input) -> String@), which may be a class and so a top. The argument
-- is added to each reading, and the one that cannot be called is refuted downstream. A top is
-- read through the members of its class that are known, whether the class is sealed or open (a
-- user ruling, 2026-10-21): an open class may gain members on a later page, but what a call is
-- applied to here is what is in scope here, and an open class read as nothing known left the whole
-- application ⊤ — @lcmPrompt(input= .., page= ..)@, whose base reaches @∀String@ through the
-- one-argument declaration beside it, once @String@ was opened to hold its representations.
--
-- A member read out of the class is a value of it, not a call being assembled, so it takes the
-- argument only where its declaration holds one: a data member whose declared arguments do not
-- (@String\/Text(chars)@ given a @page@) is no value at all, and is dropped. A positive leaf is the
-- call as written and keeps the reading 'addArgToType' gives any form.
addArgToType env@FEnv{feTypeEnv=typeEnv@TypeEnv{teNames}} vaenv (UnionType mTop@(Just _) pos []) newArg
  | (topLeafs, _) <- expandTypesWithNamesFull typeEnv vaenv (UnionType mTop H.empty []) teNames
  , not (H.null topLeafs)
  = let fromMembers = case addArgToType env vaenv (UnionType Nothing topLeafs []) newArg of
          Just (UnionType Nothing ls []) -> [ p | p <- splitUnionType ls, null (typeGraphData typeEnv (ptName p)) || relArgKeysFeasible typeEnv p (ptName p) ]
          _                              -> []
        fromCall = if H.null pos then Nothing else addArgToType env vaenv (UnionType Nothing pos []) newArg
    in case (fromMembers, fromCall) of
         ([], Nothing) -> Nothing
         (ms, Nothing) -> Just $ unionAllTypes typeEnv (map singletonType ms)
         (ms, Just c)  -> Just $ unionAllTypes typeEnv (c : map singletonType ms)
addArgToType _ _ (UnionType (Just _) _ _) _ = Nothing
-- A type-variable source is a point's own top-level type, where no partial encloses it: the
-- parent and scope layers are both the constraint's env there (the fragment rule of
-- 'substituteKeep'), so every loc resolves through the same lookup. A miss is the ⊤ an unbound
-- variable denotes ('vaenvLookup'), which the ⊤ clause above already reads as "not known yet"
-- (the constraint is retried each epoch), and a 'TVApplied' reference resolves through its
-- base — a keyed lookup could never hit it. Measured 2026-09-18 (build/scratch/addargtrace.py,
-- three core + five integration files): no type variable of any loc reaches this clause.
addArgToType env vaenv (TypeVar v _ _) newArg = addArgToType env vaenv (vaenvLookup (feTypeEnv env) vaenv v) newArg
addArgToType FEnv{feTypeEnv} _ (UnionType Nothing partials []) newArg = Just $ unionAllTypes feTypeEnv $ map singletonType kept
  where
    grafted = map fromPartial $ splitUnionType partials
    -- An applied arg can REFUTE a name-ambiguity candidate: a relative reference expands to
    -- every matching name (@insert@ ⇒ @/Data/Map/insert + /sorted./Data/List/insert@), and
    -- the application then grafts its args onto every candidate — but no value of a name can
    -- carry arg keys outside the name's declared arg-sets ('declaredArgKeys' — partial
    -- application never adds args), so a candidate whose REQUIRED keyset (lower-bounded mode)
    -- no declared arg-set covers admits no value. Keeping it rides every downstream ACT union:
    -- convergence slows past the epoch budget and TreeBuild's followup chains fail on the
    -- grafted form, silently dropping real dispatch arms ("Failed match in eval callTree").
    -- ARROW-ONLY names only: a DATA-named leaf with extra keys is a legitimate raw
    -- method-style form (@Integer(/r=Integer)@ from @5 - 2@), and an undeclared name is open
    -- world. Purely structural (parse-time arg-name sets), so the decision is epoch-stable;
    -- if every candidate would drop, keep them all (never publish an empty union).
    argsRefuted p@PartialType{ptArgMode=PtArgMode lb _ _} =
      lb && null (typeGraphData feTypeEnv (ptName p)) && not (relArgKeysFeasible feTypeEnv p (ptName p))
    kept = case filter (not . argsRefuted) grafted of
      [] -> grafted
      ps -> ps
    fromPartial partial@PartialType{ptVars, ptArgs} = case newArg of
      TVArg newArg' -> partial{ptArgs=H.insertWith (unionTypes feTypeEnv) newArg' PTopType ptArgs}
      TVVar newArg' -> partial{ptVars=H.insertWith (unionTypes feTypeEnv) newArg' PTopType ptVars}
      TVApplied k _ -> error $ printf "Unexpected TVApplied %s used as an arg name in addArgToType" (show k)
addArgToType _ _ _ _ = Nothing

-- | A helper for the 'AddArg' 'Constraint'
addArgToScheme :: FEnv -> STypeVarArgEnv -> SType -> TypeVarAux -> SType -> SType
addArgToScheme env@FEnv{feTypeEnv} vaenv SType{stypeAct=srcAct, stypeReq=srcReq, stypeReqDecl=srcReqDecl} newArgName dest@SType{stypeAct=destAct, stypeReq=destReq, stypeReqDecl=destReqDecl} = dest{stypeAct=destAct', stypeReq=destReq', stypeReqDecl=destReqDecl'}
  where
    envOf f = ambientEnv (fmap f vaenv)
    destAct' = refuteByDeclaredClass env newArgName $ case addArgToType env (envOf stypeAct) srcAct newArgName of
      Just addDestAct -> snd $ intersectTypesWithVarEnv feTypeEnv (envOf stypeAct) destAct addDestAct
      Nothing         -> destAct
    destReq' = case addArgToType env (envOf stypeReq) srcReq newArgName of
      Just addDestReq -> intersectTypesEnv feTypeEnv (envOf stypeReq) destReq addDestReq
      Nothing         -> destReq
    destReqDecl' = case addArgToType env (envOf stypeReqDecl) srcReqDecl newArgName of
      Just addDestReqDecl -> intersectTypesEnv feTypeEnv (envOf stypeReqDecl) destReqDecl addDestReqDecl
      Nothing             -> destReqDecl

-- | An application's readings, less those whose declaration takes the argument just applied as a
-- class none of whose known members the argument can be (a user ruling, 2026-10-21).
--
-- A call written with a relative name is every function so named (@entries.length@ is both
-- @\/Data\/List\/length@ and @\/Data\/String\/length@), and the argument applied to it is what
-- tells them apart. Where the reading declares the parameter as a class, and no member of the class
-- in scope meets what the argument is (@∀String@'s @String\/Text@ and @\/Catln\/CStr@ against an
-- @∀List@), the argument is not one the reading takes, and the reading is dropped. A class's open
-- world (a later page may declare a type that is both a list and a string) is left to that page:
-- the meet of two classes is not otherwise decided here. A reading whose declarations do not
-- declare the parameter, or declare it other than as a class, is kept; and the readings are left as
-- they were rather than emptied where every one would be dropped.
refuteByDeclaredClass :: FEnv -> TypeVarAux -> Type -> Type
refuteByDeclaredClass FEnv{feTypeEnv=typeEnv@TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)}} (TVArg argKey) t@(UnionType Nothing leafs consts)
  | length readings > 1, not (null kept), length kept < length readings = UnionType Nothing (joinUnionType kept) consts
  | otherwise = t
  where
    readings = splitUnionType leafs
    kept = filter (not . refuted) readings
    refuted p = case (declaredParam (ptName p), H.lookup argKey (ptArgs p)) of
      (Just cls@(UnionType (Just _) _ _), Just actual)
        | actual /= PTopType, not (typeContainsTypeVar actual)
        , members@(_:_) <- classMemberLeaves typeEnv cls
        -> all (\m -> isBottomType (intersectTypes typeEnv actual (singletonType m))) members
      _ -> False
    -- Read from the declaration's argument as it is written (@length(this -> ∀List)@), which a
    -- declaration has whether or not it is typechecked yet: a page solved together with the one
    -- declaring the reading has the declaration only as written, its object not yet typed.
    declaredParam name = case [ getMetaType argM
                              | item <- maybe [] pure (H.lookup name tg)
                              , oa <- omiDecls item ++ omiRuntimeDecls item
                              , ObjArr{oaObj=Just argObj, oaArr=Just (_, argM)} <- exprAppliedArgs (oaObjExpr oa)
                              , inExprSingleton argObj == argKey ] of
      [] -> Nothing
      ts -> Just (unionAllTypes typeEnv ts)
refuteByDeclaredClass _ _ t = t

-- | A helper for the 'SpreadArgs' 'Constraint': the base applied to every argument of the value
-- spread into it. Each base partial meets each partial the spread may be, taking the spread's
-- arguments over its own where both name one (it is written after them), and is left open to more
-- arguments where the spread is (@c@ matched by @Context(value, ..c)@ carries fields its pattern
-- never named). A base or spread not yet known as a union of partials gives nothing yet, as
-- 'addArgToType' does.
spreadArgsToType :: FEnv -> TypeVarArgEnv -> Type -> Type -> Maybe Type
spreadArgsToType env vaenv (TypeVar v _ _) spread = spreadArgsToType env vaenv (vaenvLookup (feTypeEnv env) vaenv v) spread
spreadArgsToType env vaenv base (TypeVar v _ _) = spreadArgsToType env vaenv base (vaenvLookup (feTypeEnv env) vaenv v)
spreadArgsToType FEnv{feTypeEnv} vaenv (UnionType Nothing basePartials []) (UnionType Nothing spreadPartials []) =
  case spreadValues of
    [] -> Nothing
    _  -> Just $ unionAllTypes feTypeEnv [ singletonType (spreadInto b s) | b <- splitUnionType basePartials, s <- spreadValues ]
  where
    -- What a spread gives is the arguments of the VALUE it evaluates to: a spread call is called
    -- before its arguments are read (it is built toward a value, 'TreeBuild'). So of what the spread
    -- may be, only the values give arguments. A call form still to be called (an arrow applies to
    -- it: @ContextIn(value= counter, io)@ before @counter@ runs) is not one, and read as one it
    -- gave the call's own arguments as the value's. Until some value is known, nothing is.
    spreadValues = [ sp | sp <- splitUnionType spreadPartials, null (typeGraphQuery feTypeEnv vaenv sp) ]
    -- Each argument is the spread's where the spread has it and the base's where it does not: one
    -- the spread requires is the spread's, and one it may lack (it lists it as optional, or admits
    -- unlisted ones) is either. An argument either side requires is required, and every other is
    -- optional ('PtArgMode').
    spreadInto b@PartialType{ptArgs=bArgs, ptArgMode=PtArgMode _ bUb _} s@PartialType{ptArgs=sArgs, ptArgMode=PtArgMode _ sUb _} =
      canonicalArgOpt feTypeEnv b{ptArgs=args, ptArgMode=PtArgMode True (bUb && sUb) (H.keysSet args `S.difference` req)}
      where
        sReq = ptRequiredArgs s
        req = ptRequiredArgs b `S.union` sReq
        args = H.fromList [ (k, argAt k) | k <- S.toList (H.keysSet bArgs `S.union` H.keysSet sArgs) ]
        argAt k
          | S.member k sReq = sVal
          | otherwise = unionTypes feTypeEnv (if sMay then sVal else BottomType) bVal
          where
            sMay = H.member k sArgs || not sUb
            sVal = H.lookupDefault PTopType k sArgs
            bVal = case H.lookup k bArgs of
              Just t            -> t
              Nothing | bUb     -> BottomType
                      | otherwise -> PTopType
-- A spread of a value nothing is known of yet may give any arguments, over any the base has: the
-- call is its base's name with any arg-set ('PtArgUnbounded'). Read as unknown instead, a spread of
-- an unannotated argument (@(..states, ..ctx)@ in core's 'within') left the call ⊤, which nothing
-- can build.
spreadArgsToType FEnv{feTypeEnv} _ (UnionType Nothing basePartials []) PTopType =
  Just $ unionAllTypes feTypeEnv [ singletonType b{ptArgs=H.empty, ptArgMode=PtArgUnbounded} | b <- splitUnionType basePartials ]
spreadArgsToType _ _ _ _ = Nothing

-- | A helper for the 'SpreadArgs' 'Constraint', on each channel as 'addArgToScheme'.
spreadArgsToScheme :: FEnv -> STypeVarArgEnv -> SType -> SType -> SType -> SType
spreadArgsToScheme env@FEnv{feTypeEnv} vaenv base spread dest@SType{stypeAct=destAct, stypeReq=destReq, stypeReqDecl=destReqDecl} = dest{stypeAct=destAct', stypeReq=destReq', stypeReqDecl=destReqDecl'}
  where
    envOf f = ambientEnv (fmap f vaenv)
    onChannel f destT = case spreadArgsToType env (envOf f) (f base) (f spread) of
      Just spreadDest -> intersectTypesEnv feTypeEnv (envOf f) destT spreadDest
      Nothing         -> destT
    destAct' = onChannel stypeAct destAct
    destReq' = onChannel stypeReq destReq
    destReqDecl' = onChannel stypeReqDecl destReqDecl

-- | A helper for the 'AddArg' 'Constraint'
-- | A helper for the 'AddInferArg' 'Constraint'. The grafted source is met INTO the destination
-- under the constraint's own var env, as 'addArgToScheme' does: the source carries the
-- declaration's in-scope external references (@bindap[/M=∀Monad[/T=$_A]]@), and a meet that
-- does not know the scope reads every such reference as the ⊤ it would denote unbound
-- (@/M=∀Monad[/T=⊤]@) — an erasure the destination then keeps for good, since it is a
-- same-set respelling no later write can distinguish. It went unnoticed only while the
-- destination was still ⊤ at the first graft, where the meet is the identity.
addInferArgToScheme :: FEnv -> STypeVarArgEnv -> SType -> SType -> SType
addInferArgToScheme env@FEnv{feTypeEnv} vaenv SType{stypeAct=srcAct, stypeReq=srcReq, stypeReqDecl=srcReqDecl} dest@SType{stypeAct=destAct, stypeReq=destReq, stypeReqDecl=destReqDecl} = dest{stypeAct=destAct', stypeReq=destReq', stypeReqDecl=destReqDecl'}
  where
    envOf f = ambientEnv (fmap f vaenv)
    destAct' = case addInferArgToType env (envOf stypeAct) srcAct of
      Just addDestAct -> snd $ intersectTypesWithVarEnv feTypeEnv (envOf stypeAct) destAct addDestAct
      Nothing         -> destAct
    destReq' = case addInferArgToType env (envOf stypeReq) srcReq of
      Just addDestReq -> intersectTypesEnv feTypeEnv (envOf stypeReq) destReq addDestReq
      Nothing         -> destReq
    destReqDecl' = case addInferArgToType env (envOf stypeReqDecl) srcReqDecl of
      Just addDestReqDecl -> intersectTypesEnv feTypeEnv (envOf stypeReqDecl) destReqDecl addDestReqDecl
      Nothing             -> destReqDecl

stypeConstraint :: SConstraint -> TypeCheckResult RConstraint
stypeConstraint = mapMCon id

updateCOVarArgEnv :: STypeVarArgEnv -> CVarArgEnv SType -> CVarArgEnv SType
updateCOVarArgEnv oVaenv' ioVaenv = H.intersectionWith (\(si, _) so -> (si, so)) ioVaenv oVaenv'

updateCOVarArgEnvAct :: TypeVarArgEnv -> CVarArgEnv SType -> CVarArgEnv SType
updateCOVarArgEnvAct oVaenv' ioVaenv = H.intersectionWith (\(si, so) oAct' -> (si, so{stypeAct=oAct'})) ioVaenv (envAmbient oVaenv')

computeConstraint :: FEnv -> RConstraint -> (Bool, RConstraint)
computeConstraint env con@(Constraint _ _ vaenv (EqualsKnown i p tp)) = (True, con{conVaenv = updateCOVarArgEnvAct vaenv' vaenv, conDat=EqualsKnown i stype' tp})
  where
    (vaenv', stype', _) = equalizeSTypes env (ambientEnv (fmap (stypeAct . snd) vaenv)) (p, SType tp tp tp Nothing "")
computeConstraint env con@(Constraint _ _ vaenv (EqPoints i p1 p2)) = (False, con{conVaenv = updateCOVarArgEnvAct vaenv' vaenv, conDat=EqPoints i stype' stype'})
  where
    (vaenv', stype', _) = equalizeSTypes env (ambientEnv (fmap (stypeAct . snd) vaenv)) (p1, p2)
computeConstraint FEnv{feTypeEnv} con@(Constraint _ _ vaenv (BoundedByKnown i p@SType{stypeAct=act, stypeReq=req, stypeReqDecl=reqDecl} boundTp)) = (True, con{conDat=BoundedByKnown i (p{stypeAct=act', stypeReq=req', stypeReqDecl=reqDecl'}) boundTp})
  where
    vaenv' = ambientEnv (fmap (stypeAct . snd) vaenv)
    boundTp' = relaxNameBoundArgs act $ case boundTp of
      TypeVar v _ _                                   -> H.lookupDefault PTopType (extKey v) vaenv'
      UnionType (Just (preds, negLeafs, _)) _ _ ->
        snd $ differenceTypeWithEnv feTypeEnv vaenv' (expandPredicates feTypeEnv vaenv' preds) (UnionType Nothing negLeafs [])
      t                                             -> t
    -- Per-channel bound: when the channel is a positive union, the predicate expansion is
    -- restricted to the channel's own leaf names — the dropped members are name-disjoint
    -- from every channel leaf, and 'differenceTypeWithEnv' never renames a leaf, so the
    -- meet result is unchanged ('expandPredicatesWithinNames''s operand contract). Any
    -- other channel shape (⊤, a type var, a predicate top) keeps the full shared bound —
    -- there the expansion IS the stored meet content and restriction would narrow it.
    boundFor chanT = case (boundTp, chanT) of
      (UnionType (Just (preds, negLeafs, _)) _ _, UnionType Nothing chanLeafs _) | not (H.null chanLeafs) ->
        relaxNameBoundArgs act $ snd $ differenceTypeWithEnv feTypeEnv vaenv' (expandPredicatesWithinNames feTypeEnv vaenv' (H.keysSet chanLeafs) preds) (UnionType Nothing negLeafs [])
      _ -> boundTp'
    act' = intersectTypesEnv feTypeEnv vaenv' act (boundFor act)
    req' = intersectTypesEnv feTypeEnv (ambientEnv (fmap (stypeReq . snd) vaenv)) req (boundFor req)
    -- A known bound is a spelled type (annotation seeds enter here), so it is decl-origin.
    reqDecl' = intersectTypesEnv feTypeEnv (ambientEnv (fmap (stypeReqDecl . snd) vaenv)) reqDecl (boundFor reqDecl)
computeConstraint _ con@(Constraint _ _ _ (BoundedByObjs _ SType{stypeReq=PTopType} _)) = (False, con)
-- The universe is met in an environment holding the constraint's LOCAL arrows
-- ('localArrowTypeEnv'): the meet joins every occurrence of a declaration variable, and a
-- higher-order argument's declared result is an occurrence held by the arrow that argument adds
-- to the environment (the definition's own argument arrows, or the call's lambdas), not by the
-- object ('intersectPartialsBase'). A constraint whose local arrows cannot be read is dropped,
-- as an 'ArrowTo' is.
computeConstraint env con@(Constraint _ _ _ BoundedByObjs{}) = case localArrowTypeEnv env con of
  TypeCheckResult _ argTypeEnv -> boundedByObjs (withLocalArrows argTypeEnv (feTypeEnv env)) con
  TypeCheckResE _              -> (True, con)
computeConstraint FEnv{feTypeEnv} con@(Constraint _ _ vaenv (NoReturnArg i p@SType{stypeAct=act@(UnionType Nothing _ _)})) = (True, con{conVaenv=updateCOVarArgEnvAct vaenv'' vaenv, conDat=NoReturnArg i p{stypeAct=act'}})
  where
    vaenv' = ambientEnv (fmap (stypeAct . snd) vaenv)
    argsBoundUb = setArgMode vaenv' PtArgExact $ powersetType feTypeEnv vaenv' $ UnionType Nothing (joinUnionType $ map partialToType $ H.keys $ snd $ splitVarArgEnv $ constraintVarArgEnv con) []
    (vaenv'', act') = differenceTypeWithEnv feTypeEnv vaenv' act argsBoundUb
computeConstraint FEnv{feTypeEnv} con@(Constraint _ _ vaenv (NoReturnArg i p@SType{stypeAct=UnionType (Just topData) posLeafs consts}))
  | not (H.null posLeafs) = (True, con{conVaenv=updateCOVarArgEnvAct vaenv'' vaenv, conDat=NoReturnArg i p{stypeAct=act'}})
  where
    vaenv' = ambientEnv (fmap (stypeAct . snd) vaenv)
    argsBoundUb = setArgMode vaenv' PtArgExact $ powersetType feTypeEnv vaenv' $ UnionType Nothing (joinUnionType $ map partialToType $ H.keys $ snd $ splitVarArgEnv $ constraintVarArgEnv con) []
    (vaenv'', posResult) = differenceTypeWithEnv feTypeEnv vaenv' (UnionType Nothing posLeafs []) argsBoundUb
    posLeafs' = case posResult of
      UnionType Nothing pos' _ -> pos'
      _                        -> posLeafs
    act' = UnionType (Just topData) posLeafs' consts
computeConstraint _ con@(Constraint _ _ _ NoReturnArg{}) = (False, con)
-- An input argument actually IS whatever it is required to be, AND VICE VERSA: a declaration LHS is
-- an input, so its actual and requirement denote the same thing and must agree. Meet the two and
-- assign it to BOTH bounds. Pulling the actual down to the requirement gives it the requirement's
-- object-universe bound ('BoundedByObjs' now narrows only the requirement); pulling the requirement
-- down to the actual carries an ANNOTATION written on the LHS (e.g. @Cons[$T -> Integer]@, whose
-- @/T=Integer@ arrives on the actual) into the requirement, so a destructured field @/head=$T@ then
-- resolves through @/T@ to @Integer@ in the requirement rather than staying Top - which was leaving
-- the field binding's own requirement Top, so its EqReqAct meet could not narrow anything. As uses
-- (and the ArrowTo reverse propagation) keep narrowing, this re-runs each epoch (returns False, not
-- "done"); the meet only shrinks either bound (@act ∩ req ⊆ act@, @⊆ req@), so both stay monotone
-- and a bare 'TypeVar' bound is preserved by the intersection.
computeConstraint FEnv{feTypeEnv} con@(Constraint _ _ vaenv (EqReqAct i p@SType{stypeAct=act, stypeReq=req})) = (False, con{conDat=EqReqAct i p{stypeAct=meet, stypeReq=meet}})
  where
    vaenv' = ambientEnv (fmap (stypeAct . snd) vaenv)
    -- A destructure pattern's field carries only the value inference gives it, which for a bare
    -- pattern variable (@#md(text)@) is Top: the data/annotation declaration says the field IS its
    -- declared type (@text -> String@), so an LHS input actually is that. 'BoundedByObjs' would
    -- narrow it via the object universe, but now only touches the requirement — and an open-class
    -- bound on the pattern's slot (@dshow(s -> DShow)@) re-widens the requirement's field, leaving
    -- the meet Top and building @Show/MD(text=Top)@. So complete each partial's concrete declared
    -- fields onto the actual directly (option 1): only fields whose declared type is fully concrete
    -- are carried (a parameter-typed field @v -> $A@ stays as inference left it, so no free
    -- parameter is reintroduced). This narrows the field on both bounds through the meet below.
    actC = completeConcreteFields feTypeEnv act
    -- An LHS input's actual and requirement denote the same thing, so meet them and set BOTH: pulling
    -- the actual down to the requirement gives it the requirement's object-universe bound
    -- ('BoundedByObjs' now narrows only the requirement); pulling the requirement down to the actual
    -- carries an LHS ANNOTATION (@Cons[$T -> Integer]@, whose @/T=Integer@ lands on the actual) into
    -- the requirement so a destructured field @/head=$T@ resolves through @/T@ there. Resolve the
    -- meet's freshly-discovered bindings back into the result: meeting a narrow actual
    -- (@∀ConsList[/T=∀Eq]@) with a requirement carrying a bare recursion var (@∀ConsList[/T=$_T]@)
    -- records @$_T ↦ ∀Eq@ but keeps @$_T@ in the type, which unresolved reads WIDER and re-widens it
    -- (a 'verifySchemeActLowers' failure), the trap 'intersectPartialsBase'/'arrowConstrainUbs'
    -- resolve back. Still-unresolved vars stay symbolic ('substituteKeep').
    (ivaenv, meet0) = intersectTypesWithVarEnv feTypeEnv vaenv' actC req
    -- Resolve the meet's freshly-discovered bindings back into the result. Meeting a concretized
    -- actual (@∀ConsList[/T=∀Eq]@) with a requirement carrying the recursion element var
    -- (@∀ConsList[/T=$_T]@) records @/T ↦ ∀Eq@ but keeps @$_T@ (which is literally @TVVar "/T"@,
    -- referring to the enclosing Cons's own element) in the type — unresolved it reads WIDER and the
    -- requirement re-widens (@verifySchemeReqLowers@) and the actual re-widens
    -- (@verifySchemeActLowers@). Resolving is exactly correct: the tail's element IS the enclosing
    -- element. 'substituteKeep' leaves any still-unresolved var symbolic.
    newBindings = H.difference ivaenv vaenv'
    meet1 | H.null newBindings = meet0
          | otherwise          = substituteKeep feTypeEnv newBindings meet0
    -- The meet once had to restore an outer var spelling it widened to ⊤ at a nested slot (a
    -- declaration's implicit-through-usage element link, @b -> Box[$T -> $_T]@, unbound in the
    -- constraint's vaenv). Measured 2026-09-18 on list, map, set, mapRecursion and boxUnwrap:
    -- that restoration fired zero times — 'substituteKeep' keeps such a spelling symbolic
    -- ("still-unresolved vars stay symbolic") — so the meet is stored as computed. The one
    -- place a loss still happens is the property install ('updateTypeProp' restores it there).
    meet = meet1
computeConstraint env con@(Constraint _ _ _ (ArrowTo i src dest)) = case arrowConstrainUbs env con src dest of
    TypeCheckResult _ (srcAct', srcReq', destAct', destRT') -> (False, con{conDat=ArrowTo i src{stypeAct=srcAct', stypeReq=srcReq'} dest{stypeAct=destAct', stypeTree=destRT'}})
    TypeCheckResE _ -> (True, con)
computeConstraint env con@(Constraint _ _ vaenv (PropEq i (super, name) sub)) = (False, con{conVaenv=updateCOVarArgEnv vaenv'' vaenv, conDat=PropEq i (super', name) sub'})
  where
    (vaenv', super', sub0) = updateSchemeProp env (bodyArgsOf con) (fmap snd vaenv) super name (if binder then mapSTypeTypes toSlot sub else sub)
    -- An input object's own variable ('PropEq' 51, 'TypeCheck.Encode'): the object binds this
    -- scope's variables, so the variable's value is spelled with them AMBIENT on its point and with
    -- them as the object's own parameters in the slot. Its references to its siblings cross that
    -- boundary each way: entering the slot they name the object one level out (parent references,
    -- 'siblingRefsToParent'), and returning to the point they are the ambient variables again.
    binder = i == 51
    siblings = S.fromList [ v | v@(TVVar _) <- H.keys vaenv, v /= name ]
    toSlot = siblingRefsToParent siblings
    toPoint = siblingRefsToAmbient siblings
    sub' = if binder then mapSTypeTypes toPoint sub0 else sub0
    vaenv'' = if binder then fmap (mapSTypeTypes toPoint) vaenv' else vaenv'
computeConstraint env con@(Constraint _ _ vaenv (PropEqInferred i (super, app, base) sub)) = case inferredArgName base app of
    Just name -> let (vaenv', super', sub') = updateSchemeProp env (bodyArgsOf con) (fmap snd vaenv) super name sub
                 in (False, con{conVaenv=updateCOVarArgEnv vaenv' vaenv, conDat=PropEqInferred i (super', app, base) sub'})
    Nothing -> (False, con)
computeConstraint FEnv{feTypeEnv, feReachesTypeEnv} con@(Constraint _ _ _ (HoResultReq i (call, argName, dest) result)) = case declaredResult of
    Just r | r /= PTopType -> (False, con{conDat=HoResultReq i (call, argName, dest) result{stypeReq=meet (stypeReq result) r}})
    _ -> (False, con)
  where
    -- The need is met into the lambda's requirement, which chooses among the arms the lambda's body
    -- reaches ('arrowConstrainUbs'), and not into its declaration-origin requirement: the lambda
    -- declares nothing, and a declaration engaged there settles and meets the forms its body gives
    -- (a @foldr@ step's @Cons(head= v, tail= acc)@ held to @∀ConsList[/T=⊤]@ came out as
    -- @Cons[/T=⊤](head=⊤, ..)@), and holds the body to it as a guarantee (@elem@'s @v == item@,
    -- whose @v@ is unresolved).
    meet a b = intersectTypes feTypeEnv a b
    -- The continuation is held to what the call's destination needs, as the call's declaration
    -- passes that need on to it. Its argument occurrences are no bound: under membership a
    -- declared variable is the set the values at all its occurrences belong to, so @m@'s value is
    -- evidence for @$M@, not a limit on what the continuation may add to it (@mmap(m= Some(5),
    -- fn(v)= Nothing)@ is an @mmap@ whose @$M@ holds both).
    -- What the receiver's own declaration needs ('stypeReqDecl'), not the requirement accumulated
    -- there: that one also carries what the receiver's other occurrences say (@l == r@ binds @$T@
    -- from @r@), which is evidence again, not a need.
    destReq = stypeReqDecl dest
    -- Each form the call may take reads the argument's declared result through the declarations
    -- applying to it; a form one of them says nothing of leaves the result unbounded.
    declaredResult
      | destReq == PTopType = Nothing
      | otherwise = case stypeReq call of
          UnionType Nothing leafs _ | not (H.null leafs) -> unionAllTypes feTypeEnv <$> mapM resultFor (splitUnionType leafs)
          _ -> Nothing
    -- A name with several declarations (@bindap@'s @mmap@, @fmap@ and @ap@ arms) says what the
    -- argument returns once per declaration, and the call is free to take any that applies: the
    -- union of what they say.
    resultFor leaf = case [ (dr, declResult decl) | decl <- applyingDecls leaf, (k, dr) <- declArgResults decl, pkName k == pkName argName, pkArgs k == pkArgs argName ] of
      []  -> Nothing
      drs -> unionAllTypes feTypeEnv <$> mapM readResult drs
    -- A variable result is what the destination needs of it through the declared result: the
    -- destination itself where the declaration returns that variable (@mmap@'s @-> $M@, @ap@'s
    -- @-> $B@), its parameter where the declaration returns another variable re-applied with it
    -- (@fmap@'s @-> $F[$T -> $_B]@ takes @B@ from the destination's @/T@). One the declared result
    -- does not show is unbounded by the destination.
    readResult (TypeVar (TVVar vn) _ _, res) = demandFor vn res >>= settled
    readResult (dr, _)                       = settled dr
    demandFor vn (TypeVar (TVVar vn') _ _) | vn == vn' = Just destReq
    demandFor vn (TypeVar (TVApplied _ overrides) _ _)
      | [p] <- [ p | (p, TypeVar (TVVar vn') _ _) <- H.toList overrides, vn' == vn ] = paramOf p destReq
    demandFor _ _ = Nothing
    -- The value a type gives a parameter. A type is its top's region and its leaves together, so
    -- the readings of the two join; each leaf gives its own slot, and a top the slot its class
    -- atoms all hold (they meet). A part that does not spell the slot gives no reading.
    paramOf p (UnionType mTop leafs _) = do
      fromLeafs <- mapM (H.lookup p . ptVars) (splitUnionType leafs)
      fromTop <- case mTop of
        Nothing -> Just Nothing
        Just (PredsOne (PredClass c), _, _) -> Just <$> H.lookup p (ptVars c)
        Just (PredsAnd as, _, _) -> case [ v | PredsOne (PredClass c) <- as, Just v <- [H.lookup p (ptVars c)] ] of
          [] -> Nothing
          vs -> Just (Just (foldr1 (intersectTypes feTypeEnv) vs))
        Just _ -> Nothing
      case maybe fromLeafs (: fromLeafs) fromTop of
        [] -> Nothing
        vs -> Just (unionAllTypes feTypeEnv vs)
    paramOf _ _ = Nothing
    TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap reachesTg)} = feReachesTypeEnv
    applyingDecls leaf = [ decl | decl@ObjArr{oaArr=Just{}} <- maybe [] omiDecls (H.lookup (makeAbsoluteName (ptName leaf)) reachesTg)
                                , not (isBottomType (intersectTypes feReachesTypeEnv (singletonType leaf) (getMetaType (getExprMeta (oaObjExpr decl))))) ]
    declResult decl = maybe PTopType (getMetaType . snd) (oaArr decl)
    declArgResults decl = [ (inExprSingleton aObj, getMetaType aM) | ObjArr{oaObj=Just aObj, oaArr=Just (_, aM)} <- exprAppliedArgs (oaObjExpr decl), not (null (exprAppliedArgs aObj)) ]
    -- Only a closed result is required of the lambda, as only a closed destination is chosen for
    -- a variable ('outVarBoundSrc'): an open class (@foldEntriesSteps[$B -> ToString]@'s bound)
    -- may gain members anywhere, so no result short of a member is within it.
    settled t | t == PTopType || typeContainsTypeVar t || not (closedType feTypeEnv t) = Nothing
              | otherwise = Just t
computeConstraint env con@(Constraint _ _ vaenv (AddArg i (src, newArgName) dest)) = (False, con{conDat=AddArg i (src, newArgName) dest'})
  where
    dest' = addArgToScheme env (fmap snd vaenv) src newArgName dest
computeConstraint env con@(Constraint _ _ vaenv (AddInferArg i src dest)) = (False, con{conDat=AddInferArg i src dest'})
  where
    dest' = addInferArgToScheme env (fmap snd vaenv) src dest
computeConstraint env con@(Constraint _ _ vaenv (SpreadArgs i (base, spread) dest)) = (False, con{conDat=SpreadArgs i (base, spread) dest'})
  where
    dest' = spreadArgsToScheme env (fmap snd vaenv) base spread dest
computeConstraint FEnv{feTypeEnv} con@(Constraint _ _ vaenv (SetArgMode i mode src@SType{stypeAct=srcAct, stypeReq=srcReq, stypeReqDecl=srcReqDecl} dest@SType{stypeAct=destAct, stypeReq=destReq, stypeReqDecl=destReqDecl})) = (False, con{conDat=SetArgMode i mode src dest{stypeAct=destAct', stypeReq=destReq', stypeReqDecl=destReqDecl'}})
  where
    vaenv' = ambientEnv (fmap (stypeAct . snd) vaenv)
    argModeType venv t = if mode
      then powersetType feTypeEnv venv t
      else setArgMode venv PtArgAny t
    destAct' = intersectTypes feTypeEnv (argModeType vaenv' srcAct) destAct
    -- The arg-mode transform is an upper bound on the destination, so it applies to the required
    -- type as well as the actual (both are upper bounds — see 'SType'); mirror it on each side.
    destReq' = intersectTypes feTypeEnv (argModeType (ambientEnv (fmap (stypeReq . snd) vaenv)) srcReq) destReq
    destReqDecl' = intersectTypes feTypeEnv (argModeType (ambientEnv (fmap (stypeReqDecl . snd) vaenv)) srcReqDecl) destReqDecl
-- A @where@ is its base's value, within the condition: the result is the base refined by the
-- condition's FORM as a predicate ('TypeCheck.Encode.condPredForm'), the condition as written with
-- the base's value named @$this@ and the scope's other arguments as their variables. The form is
-- what the type layer can decide and the runtime can evaluate: substituted at a call
-- (`arrowDestType` reads predicates under the call's bindings), @k(a= S(n= Z))@'s result
-- @S(n= Z) | $this.isZ == True@ is decided false and empty. Read from the condition's actual type
-- instead, the predicate carried the operands' REACHED values (@==(l= {True, False}, r= True)@),
-- which say nothing of the value, and a condition whose actual was not one partial carried none.
-- A condition the form cannot state (applying an argument by position, a bare argument) refines
-- nothing here; it is still evaluated where it is written ('Eval' @TWhere@, the arm's guard).
--
-- The scope's arguments are the constraint's own environment: a bare name among them is the
-- argument, shadowing any function it would otherwise name (as 'Semantics.TypeGraph.reachesPartials'
-- reads it); a bare name outside it is the relation it names, resolved where the predicate is
-- decided (@~True@).
computeConstraint FEnv{feTypeEnv} con@(Constraint _ _ vaenv (ConWhere i form baseVar base cond res)) = (False, con{conDat=ConWhere i form baseVar base cond res'})
  where
    isArg n = H.member (TVArg (partialKey n)) vaenv
    preds = maybe PredsNone (PredsOne . PredExpr) (form >>= resolve)
    resolve p
      | isArg (ptName p) = Nothing
      | otherwise = Just (resolveArgs p)
    resolveArgs p = p{ptArgs=fmap operand (ptArgs p)}
    operand (TopType negs (PredsOne (PredRel q)))
      | H.null (ptArgs q), isArg (ptName q) = if Just (ptName q) == baseVar
          then relTypeVal (mkName macroThisKey)
          else TypeVar (TVArg (partialKey (ptName q))) TVInt PTopType
      | otherwise = TopType negs (PredsOne (PredRel (resolveArgs q)))
    operand t = t
    -- The condition refines an upper bound on the result, so it applies to the required type as
    -- well as the actual (both are upper bounds — see 'SType'); refine each side against its own
    -- ambient var-env.
    refine side = intersectTypesEnv feTypeEnv (ambientEnv (fmap (side . snd) vaenv)) (side res) $ typeAddPreds (side base) preds
    res' = res{stypeAct=refine stypeAct, stypeReq=refine stypeReq, stypeReqDecl=refine stypeReqDecl}
computeConstraint env@FEnv{feTypeEnv} con@(Constraint _ _ vaenv (UnionOf i UnionBoth parent children)) = (False, con{conVaenv=updateCOVarArgEnvAct vaenv' vaenv, conDat=UnionOf i UnionBoth parentST' children})
  where
    chAct = unionAllTypesWithEnv feTypeEnv H.empty $ filter (not . isTypeVar) $ map stypeAct children
    chReq = unionAllTypesWithEnv feTypeEnv H.empty $ filter (not . isTypeVar) $ map stypeReq children
    chReqDecl = unionAllTypesWithEnv feTypeEnv H.empty $ filter (not . isTypeVar) $ map stypeReqDecl children
    actVaenv = ambientEnv (fmap (stypeAct . snd) vaenv)
    (vaenv', parentST', _) = equalizeSTypes env actVaenv (parent, SType (compactType feTypeEnv actVaenv chAct) chReq chReqDecl Nothing "")
-- One channel alone: the parent's bound on it is met with the union of the children's, and its
-- other bounds are left as they are. A child still a variable bounds nothing yet, so the union
-- waits for it (an unresolved child is ⊤, which the union already reads as no bound).
computeConstraint FEnv{feTypeEnv} con@(Constraint _ _ vaenv (UnionOf i ch parent children)) = (False, con{conDat=UnionOf i ch parent' children})
  where
    -- The channels met: the actual alone, or the two requirements. A requirement taken from the
    -- children's receivers is what their declarations need, so the declaration-origin channel
    -- carries it as well, where what a sub function returns is chosen.
    sides = case ch of
      UnionAct -> [(stypeAct, \t st -> st{stypeAct = t})]
      _        -> [(stypeReq, \t st -> st{stypeReq = t}), (stypeReqDecl, \t st -> st{stypeReqDecl = t})]
    meetSide st (side, set)
      | any (isTypeVar . side) children = st
      | childrenUnion == PTopType || typeVarLost (side st) met = st
      | otherwise = set met st
      where
        childrenUnion = unionAllTypesWithEnv feTypeEnv H.empty (map side children)
        met = intersectTypesEnv feTypeEnv (ambientEnv (fmap (side . snd) vaenv)) (side st) childrenUnion
    parent' = foldl meetSide parent sides

saveConstraint :: VConstraint -> RConstraint -> RConstraint -> FEnv -> FEnv
saveConstraint con@(Constraint _ _ _ (UnionOf _ UnionBoth parentPnt _)) oldCon newCon@(Constraint _ _ vaenv' (UnionOf _ _ parent' _)) env = env''
  -- TODO Delete special case of saveConstraint for UnionOf by fixing the bug
  where
    env' = setScheme env con (oldCon, newCon) parentPnt (return parent') "UnionOf"
    env'' = setSchemeConVaenv env' con (oldCon, newCon) SchemeAct (ambientEnv (fmap (stypeAct . snd) vaenv')) "EqualsKnown env"
saveConstraint vals old new env = saveDat $ saveVaenv env
  where
    saveDat en = foldr (\(p, v) e -> setScheme e vals (old, new) p (pure v) (show v)) en (zip (constraintDatMetas $ conDat vals) (constraintDatMetas $ conDat new))
    saveVaenv en = foldr saveVaenvPair en (H.elems $ H.intersectionWith (,) (conVaenv vals) (conVaenv new))
    saveVaenvPair ((pIn, pOut), (vIn, vOut)) en = en''
      where
        en' = setScheme en vals (old, new) pIn (pure vIn) (show vIn)
        en'' = setScheme en' vals (old, new) pOut (pure vOut) (show vOut)

-- |
-- This takes a constraint and tries to apply it in the environment.
-- It will return the updated environment and a boolean that is true if the constraint is done.
-- If it is done, it can be safely removed and no longer needs to be executed.
boundedByObjs :: FEnvTypeEnv -> RConstraint -> (Bool, RConstraint)
boundedByObjs feTypeEnv con@(Constraint _ _ vaenv (BoundedByObjs i p@SType{stypeReq=pReq, stypeReqDecl=pReqDecl} objMapBoundUb)) = (False, con{conDat=BoundedByObjs i (p{stypeReq=pReq', stypeReqDecl=pReqDecl'}) objMapBoundUb})
  where
    vaenv' = ambientEnv (fmap (stypeAct . snd) vaenv)
    argsBoundUb = setArgMode vaenv' PtArgExact $ powersetType feTypeEnv vaenv' $ UnionType Nothing (joinUnionType $ map partialToType $ H.keys $ snd $ splitVarArgEnv $ constraintVarArgEnv con) []
    -- A type is REQUIRED to be a declared/defined object, but that does not mean it ACTUALLY is one:
    -- being one of the known objects is only a requirement. So narrow only the requirement against
    -- the object-universe bound and leave the actual untouched.
    --
    -- Relax each bound object's self-referential field (one that recurses back to a type the
    -- object participates in) so a destructured value whose corresponding field is still an
    -- unconstrained pattern variable can reconcile with it (see 'relaxRecursiveSelfArgs').
    -- The objects universe is closed-world (only currently-declared data objects), but an OPEN
    -- (unsealed) class may gain instances in a future file, so a value typed by one is genuinely
    -- inhabitable and must survive the bound rather than empty. Admit the requirement's OWN
    -- open-class predicate components (with their real element bounds) as inhabitable bound members
    -- ('openClassPart pReq'): an open-class requirement (e.g. a @Functor[Eq]@ law argument with no
    -- in-scope instance) must survive the object-universe bound rather than empty. Sealed classes are
    -- excluded — they equal their declared members, already in the universe.
    --
    -- Only narrow a *concrete* requirement. A Top requirement meets the whole universe to the whole
    -- universe (every known object) — not a narrowing, and a performance blowup — so leave it be
    -- (the 'stypeReq=PTopType' guard above already short-circuits it). A named requirement instead
    -- meets only its name-matching objects, tightening the requirement's arg fields (e.g.
    -- @/f(/a=Top)@ narrows to @/f(/a=<declared>)@).
    -- The narrowing is applied per requirement channel — the accumulated 'stypeReq' and the
    -- decl-origin 'stypeReqDecl' — each against its own var-env and its own open-class
    -- components ('openClassPart'). Narrowing spelled requirement content against declared
    -- objects preserves the decl channel's origin purity: no actual-derived input enters.
    --
    -- The bound is the universe, the requirement's open-class part and the arguments together, and
    -- for a union requirement it is not built as one type: a meet distributes over a union, so the
    -- requirement is met with each part separately and the pieces joined. Built, the union re-canonicalized the whole
    -- universe — every object, its fields compacted again, its same-named leafs compared pairwise —
    -- once per execution of every such constraint, to be met with a requirement that reads only the
    -- objects of its own names. A requirement leaf is met with the SLICE of each part holding its
    -- name ('boundSliceFor'), as a value has one name and leafs of other names meet it in nothing;
    -- the relaxation of recursive fields is per leaf, so relaxing a slice is relaxing its leafs.
    narrowChannel chanVaenv chanReq = chanReq'
      where
        openPart = openClassPart feTypeEnv chanReq
        boundParts = [objMapBoundUb, openPart, argsBoundUb]
        -- The names the bound holds positive leafs under. Open-class parts are predicate tops and
        -- hold none.
        boundNames = S.fromList [ n | UnionType _ bLeafs _ <- boundParts, n <- H.keys bLeafs ]
        -- The universe bound is open-world on NAMES: a requirement leaf whose name matches no
        -- object in the universe (a compiler annotation whose compile.ct declaration is not in
        -- this import closure, or any name declared outside the current program) offers nothing
        -- to tighten against, so it passes through unchanged rather than emptying the requirement.
        -- A name the universe DOES know meets its name-matching objects as before.
        -- An absolute name is matched by no other name, so only a relative one is looked for by
        -- its suffix.
        nameKnown l
          | S.member (ptName l) boundNames = True
          | take 1 (nameStr (ptName l)) == "/" = False
          | otherwise = any (relativeNameMatches (ptName l)) (S.toList boundNames)
        meetParts t parts = unionAllTypesWithEnv feTypeEnv chanVaenv
          [ snd $ intersectTypesWithVarEnv feTypeEnv chanVaenv t (relaxRecursiveSelfArgs feTypeEnv part) | part <- parts ]
        chanReq' | chanReq == PTopType = chanReq
                 | otherwise           = case chanReq of
                     UnionType mTop leafs consts ->
                       let (knownLeafs, unknownLeafs) = partition nameKnown (splitUnionType leafs)
                           metKnown = [ meetParts (singletonType l) (map (boundSliceFor (ptName l)) boundParts)
                                      | l <- knownLeafs ]
                           -- A top or a constant can hold a value of any name, so it meets the
                           -- parts whole.
                           metTopConsts = unionTypesWithEnv feTypeEnv chanVaenv metTop metConsts
                           -- An open class's region is a part of the bound itself ('openClassPart'),
                           -- so the requirement's top lies within the bound and meets it in all of
                           -- itself — each member of the universe it holds is already inside it.
                           metTop = case mTop of
                             Nothing -> BottomType
                             Just _
                               | openPart /= BottomType && relaxRecursiveSelfArgs feTypeEnv openPart == openPart -> topPart
                               | otherwise -> meetParts topPart boundParts
                             where topPart = UnionType mTop H.empty []
                           metConsts
                             | null consts = BottomType
                             | otherwise = meetParts (UnionType Nothing H.empty consts) boundParts
                       -- Recombined under the channel's environment, the one each piece was met
                       -- under: a piece can keep an in-scope link (@inverse(/t=$_T)@ with
                       -- @T ↦ ∀Group@), which read in an empty environment denotes ⊤ and would
                       -- widen the requirement.
                       in unionAllTypesWithEnv feTypeEnv chanVaenv (UnionType Nothing (joinUnionType unknownLeafs) [] : metTopConsts : metKnown)
                     -- A requirement that is not a union (a variable occurrence) is met with
                     -- the bound whole, as joining its meets with the parts would read the
                     -- variable through its binding.
                     _ -> snd $ intersectTypesWithVarEnv feTypeEnv chanVaenv chanReq $ relaxRecursiveSelfArgs feTypeEnv $
                            unionTypes feTypeEnv (unionTypes feTypeEnv objMapBoundUb openPart) argsBoundUb
    reqChanVaenv = ambientEnv (fmap (stypeReq . snd) vaenv)
    declChanVaenv = ambientEnv (fmap (stypeReqDecl . snd) vaenv)
    pReq' = narrowChannel reqChanVaenv pReq
    -- Exact-input memo: the two channels are identical until an echo writer diverges them
    -- (most points, most epochs), and 'narrowChannel' is a pure function of (vaenv, req) —
    -- reuse the req result only when BOTH inputs match, so no cross-channel contamination.
    pReqDecl' | pReqDecl == pReq && declChanVaenv == reqChanVaenv = pReq'
              | otherwise = narrowChannel declChanVaenv pReqDecl
boundedByObjs _ con = (False, con)

-- | The part of a bound a value named @name@ can be within: its predicate top and constants whole,
-- as they can hold a value of any name, and of its positive leafs only those named @name@ — exactly,
-- or by a relative spelling of either name. A leaf of any other name meets such a value in nothing.
boundSliceFor :: TypeName -> Type -> Type
boundSliceFor name (UnionType mTop leafs consts) = UnionType mTop slice consts
  where
    -- An absolute name is matched by no other absolute name, so only the leafs kept under it and
    -- under relative names are candidates, and the absolute ones are not scanned.
    slice
      | isAbs name = maybe id (H.insert name) (H.lookup name leafs) $ H.filterWithKey (\k _ -> not (isAbs k) && relativeNameMatches k name) leafs
      | otherwise  = H.filterWithKey (\k _ -> k == name || relativeNameMatches name k || relativeNameMatches k name) leafs
    isAbs n = take 1 (nameStr n) == "/"
boundSliceFor _ t = t

executeConstraint :: Int -> VConstraint -> StateT FEnv TypeCheckResult Bool
executeConstraint cid con = do
  modify $ startConstraint con
  env@FEnv{feEnvStamp} <- get
  let conMemo = feConMemo env
  case stypeConstraint $ showCon env con of
    TypeCheckResE _ -> return True
    TypeCheckResult _ con' -> do
      -- Skip-if-unchanged: 'computeConstraint' is a pure function of the resolved view
      -- @con'@ plus, for the kinds that read the per-epoch env caches, those caches
      -- ('feReachesTypeEnv' for ArrowTo/PropEq/BoundedByObjs via 'mkReachesEnv' /
      -- 'settlePropSlot'; the universe is already substituted INTO a
      -- BoundedByObjs view by 'showCon', and 'feUniverseKey' tracks
      -- it). An ArrowTo or BoundedByObjs additionally resolves its 'conOs' object-side arg
      -- entries and call arrows ('conOaResolveKey'). When every component of that key equals the last execution's,
      -- the recomputed stores would all be refused as unchanged by 'setScheme' — a provable
      -- no-op, so skip the compute and save entirely and repeat the prune decision.
      let stampKey = case conDat con of
            ArrowTo{}        -> feEnvStamp
            PropEq{}         -> feEnvStamp
            PropEqInferred{} -> feEnvStamp
            _                -> 0
      let oaKey = case conDat con of
            ArrowTo{}       -> conOaResolveKey env con
            BoundedByObjs{} -> conOaResolveKey env con
            _               -> []
      -- 'conOs' is CONSTANT for a given constraint (the same cid always resolves the same
      -- object list), so it is zeroed out of the memoized view rather than structurally
      -- compared every execution — its object expressions carry every meta and source
      -- position, and comparing them was a third of a fragmented solve. The parts of the
      -- objects a compute actually reads are covered separately ('conOaResolveKey').
      let conMemoKey = con'{conOs=[], conCallArrs=[]}
      -- A 'BoundedByObjs' reads no more of the reaches graph than the declarations under its own
      -- objects' names, so it is keyed by those rather than by the env stamp: the bound of a
      -- point whose objects, universe and arguments are as they were is as it was, whatever
      -- arrows elsewhere in the fragment changed.
      let declsKey = case conDat con of
            BoundedByObjs{} -> conObjDeclsKey env con
            _               -> []
      case IM.lookup cid conMemo of
        Just (ConMemo memoStamp memoCon memoOa memoDecls memoPrune)
          | memoStamp == stampKey && memoCon == conMemoKey && memoOa == oaKey && memoDecls == declsKey -> return memoPrune
        _ -> do
          let (prune, con'') = computeConstraint env con'
          modify $ saveConstraint con con' con''
          modify $ \e -> e{feConMemo = IM.insert cid (ConMemo stampKey conMemoKey oaKey declsKey prune) (feConMemo e)}
          return prune

-- |
-- Applies the constraints continuously to update the environment.
-- It should eventually converge after which applying the constraints no longer has any effect.
-- This function also accepts a limit 'Integer' to indicate a maximum number of times a constraint can be used.
-- A 'TypeCheckError' will be thrown if it has not converged by the time the limit is reached.
-- Returns whether the constraints CONVERGED (a limit failure or an aborting env build returns
-- 'False' after noting its error), so a per-fragment solve loop can stop before dependents
-- solve against a half-settled fragment.
runConstraints :: Integer -> [VConstraint] -> StateT FEnv TypeCheckResult Bool
runConstraints limit cons = do
  -- Fresh skip-memo per constraint set: memo keys are positions in THIS list.
  modify $ \e -> e{feConMemo = IM.empty}
  modify startConstrainSolve
  runConstraintsIdx limit (zip [0..] cons)

runConstraintsIdx :: Integer -> [(Int, VConstraint)] -> StateT FEnv TypeCheckResult Bool
runConstraintsIdx _ [] = return True
runConstraintsIdx 0 _ = do
  env@FEnv{feUnionAllObjs} <- get
  if (stypeAct <$> descriptor env feUnionAllObjs) == pure PTopType
    then lift $ TypeCheckResult [GenTypeCheckError Nothing $ printf "Reached runConstraints limit without determining the TopTop (unionAllObjs)"] ()
    -- What was still changing is whatever the instruments watching this solve can say about it,
    -- which for the one every typecheck carries ('TypeCheck.Instruments.LastEpochs') is the last
    -- completed epoch. A solve watched by nothing says only that it ran out.
    else lift $ TypeCheckResult [GenTypeCheckError Nothing $ printf "Reached runConstraints limit with still changing constraints: \n\n%s" (intercalate "\n\n" $ instrumentDiagnostics env)] ()
  return False
runConstraintsIdx limit cons = do
  modify startConstrainEpoch
  env <- get
  -- Compute the constraint-independent reaches type graph once for this epoch and cache it, so
  -- the (many) 'arrowConstrainUbs'/'mkReachesEnv' calls in this epoch reuse it instead of each
  -- rebuilding the whole type graph. See the TODO on 'buildTypeEnv'.
  --
  -- A failed 'setScheme' check or verification stores a 'TypeCheckResE' in the failing point,
  -- and 'buildTypeEnv' reads every object's points, so lifting its result directly would abort
  -- the whole 'StateT' computation here on the following epoch - discarding 'feInstruments'
  -- (leaving 'catln typetrace' with nothing to write) along with all other typecheck artifacts. Instead,
  -- demote the failure to recoverable error notes and stop constraining: decoding still surfaces
  -- the poisoned points' errors, and consumers reject the program via 'failOnErrorNotes'.
  case buildTypeEnv env of
    TypeCheckResE errNotes -> do
      lift $ TypeCheckResult errNotes ()
      return False
    TypeCheckResult notes (reachesVPart, reachesTypeEnv) -> do
      lift $ TypeCheckResult notes ()
      -- Every 'BoundedByObjs' constraint is bounded by the same 'feUnionAllObjs' descriptor
      -- (substituted uniformly by 'showCon'); its value at the epoch's start is one of the
      -- per-epoch env inputs the stamp below tracks.
      let u = maybe BottomType stypeAct $ tcreToMaybe $ descriptor env $ feUnionAllObjs env
      let uUnchanged = u == feUniverseKey env
      -- Bump the env stamp iff a per-epoch env input changed: constraint kinds that read
      -- these caches only skip re-execution within an unchanged stamp (see 'feConMemo').
      -- Only the LIVE (V-side) part of the reaches graph is compared: the typechecked side
      -- changes only between solve fragments, where the fresh universe point already makes
      -- 'uUnchanged' False on the fragment's first epoch.
      let envUnchanged = uUnchanged && reachesVPart == feReachesVPart env
      -- The input-arrow name set is a pure function of the reaches graph: reuse it within an
      -- unchanged env, recompute otherwise (same policy as the universe key).
      let inputArrowNames = if envUnchanged then feInputArrowNames env else typeGraphInputEmbeddedNames reachesTypeEnv
      -- The versions of the live items ('ReuseScope'): an item as it was in the previous epoch
      -- keeps its version, and one new or changed is given the next. A reach query asked again
      -- under the new token is then handed the answer it gave before wherever the items it read
      -- are all as they were ('Semantics.objArrGraphQuery').
      let (vVersions, versionNext) = versionLiveItems (feReachesVPart env) (feVVersions env) (feVersionNext env) reachesVPart
      -- The reach-query memo's identity: an unchanged graph KEEPS the previous epoch's env
      -- value with its existing token (the memo stays warm across quiet epochs); a changed
      -- graph is stamped with a fresh process-unique token, which also retires the previous
      -- generation's entries ('objArrGraphQuery' resets on a new token).
      let reachesTypeEnv' = if envUnchanged
            then feReachesTypeEnv env
            else (stampQueryToken reachesTypeEnv){teReuse = Just (ReuseScope (feReuseScope env) vVersions)}
      modify $ \e -> e{feReachesTypeEnv = reachesTypeEnv', feReachesVPart = reachesVPart,
                       feVVersions = vVersions, feVersionNext = versionNext,
                       feUniverseKey = u,
                       feInputArrowNames = inputArrowNames,
                       feEnvStamp = if envUnchanged then feEnvStamp e else feEnvStamp e + 1}
      constraintsToPrune <- mapM (uncurry executeConstraint) cons
      updated <- gets feUpdatedDuringEpoch
      let cons' = mapMaybe (\(icon, shouldPrune) -> if shouldPrune then Nothing else Just icon) $ zip cons constraintsToPrune
      if not updated then return True else do
        modify nextConstrainEpoch
        -- ALTERNATE the traversal direction each epoch.
        --
        -- Two flows run through the same constraint set in OPPOSITE directions: actuals travel
        -- forward (a leaf's type outward to the application containing it) and requirements
        -- travel backward (a consumer's demand inward to what supplies it). A constraint sees
        -- whatever the constraints before it in this epoch already wrote, so ONE order is the
        -- good order for one flow and the pessimal order for the other. Alternating gives each
        -- flow its good order every other epoch instead of starving one of them.
        --
        -- Starving the requirement channel is not merely slow, it is WRONG: a requirement
        -- computed from partial knowledge is locked in by the narrowing invariant
        -- ('verifySchemeReqLowers'), so a demand derived before its siblings resolved cannot be
        -- widened back. Encoding in dataflow order WITHOUT this alternation over-narrowed
        -- requirements on calls through a class top (core's @insert(m= insert(m= ...))@, whose
        -- requirement must round-trip between @/Data/Map@ and its @BinTree@ members) — 70 of 574
        -- tests, against 2 on the unordered list.
        --
        -- The index paired with each constraint is its 'feConMemo' key and rides along, so
        -- reversing the running order leaves the skip-memo keys untouched.
        runConstraintsIdx (limit - 1) (reverse cons')

-- | The version each item of the live part of the reaches graph is at, from the previous epoch's
-- live part and versions: an item equal to the one the previous epoch held under its name keeps
-- that version, and any other takes the next one. The declarations kept under no name ('omDecls')
-- are versioned as one item, under 'setDeclsVersionName'. Gives the next version still unused.
versionLiveItems :: ObjectMap Expr () -> H.HashMap TypeName Int -> Int -> ObjectMap Expr () -> (H.HashMap TypeName Int, Int)
versionLiveItems (ObjectMapWith prevItems prevDecls) prevVersions next0 (ObjectMapWith items decls) =
  H.foldlWithKey' versionItem (declsVersion, next1) items
  where
    (declsVersion, next1) = case H.lookup setDeclsVersionName prevVersions of
      Just v | prevDecls == decls -> (H.singleton setDeclsVersionName v, next0)
      _                           -> (H.singleton setDeclsVersionName next0, next0 + 1)
    versionItem (versions, next) name item = case (H.lookup name prevItems, H.lookup name prevVersions) of
      (Just prevItem, Just v) | prevItem == item -> (H.insert name v versions, next)
      _                                          -> (H.insert name next versions, next + 1)

-- | The 'ArrowTo' constraints that hold a value to a declaration: an output argument's slot (10, 15)
-- and a definition's result (32).
guaranteedArrows :: [Int]
guaranteedArrows = [10, 15, 32]

-- | Reports each value whose every form is not guaranteed to end within what its point's
-- declaration requires ('typeGuaranteedWithin'), once inference has converged.
--
-- Inference itself only asks whether a value CAN satisfy a requirement: 'TypeCheck.Decode.toMeta'
-- accepts a form that meets the requirement or has some path to it, and a requirement narrows a
-- definition's own arguments from their use. So a definition whose body needs more of its input
-- than its declaration promises type-checks — @unwrap(b -> Box(v)) -> Integer@ answering its
-- @v@, or an argument @fn(v) -> $_B@ handed to @toString@ — and fails only where it is built or
-- called. This asks the stronger question at the two kinds of point a value is held to a
-- declaration: an argument it is passed as (an output argument's slot) and the result a
-- definition returns, and a value not guaranteed within it is an error. The requirement is the
-- declaration's alone ('stypeReqDecl'): the
-- accumulated one also holds what the value's own forms wrote back, which would answer for them.
-- A free variable is read through the constraint's own bindings.
verifyGuaranteedResults :: [VConstraint] -> StateT FEnv TypeCheckResult ()
verifyGuaranteedResults cons = do
  env <- get
  forM_ cons $ \con -> case conDat con of
    -- A hole produces no value, so it is within anything it is held to.
    ArrowTo _ srcM _ | maybe False (`S.member` feHolePoints env) (getPnt srcM) -> pure ()
    ArrowTo i srcM _ | i `elem` guaranteedArrows -> case stypeConstraint (showCon env con) of
      TypeCheckResult _ rcon@Constraint{conDat=ArrowTo _ srcS destS} -> do
        -- A variable the solve left unbound (⊤) ranges over what its declaration bounds it by.
        let varValue s = if stypeAct s == PTopType then stypeReqDecl s else stypeAct s
        let vaenv = ambientEnv $ fmap (varValue . snd) (conVaenv rcon)
        let req = stypeReqDecl destS
        let src = case stypeAct srcS of
              TypeVar v _ _ -> vaenvLookup (feTypeEnv env) vaenv v
              t             -> t
        let guaranteed = isSubtypeOfWithEnv (feTypeEnv env) vaenv src req || case mkReachesEnv env rcon of
              TypeCheckResult _ renv -> typeGuaranteedWithin (rTypeEnv renv) vaenv req src
              TypeCheckResE _        -> True
        unless guaranteed $
          lift $ TypeCheckResult [ResultNotGuaranteed (stypeDesc destS) src req (getMetaPos srcM)] ()
      _ -> pure ()
    _ -> pure ()
