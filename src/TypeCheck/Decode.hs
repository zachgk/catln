--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Decode
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module decodes after type checking is completed to produce
-- the final typechecked program.
--------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module TypeCheck.Decode where

import           Control.Monad.State
import           CtConstants         (proofAnnot, thisKey)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Maybe          (mapMaybe)
import           MapMeta             (MetaType (ArrMeta), clearMetaDat,
                                      mapMetaPrgm)
import           Semantics
import           Semantics.Prgm
import           Semantics.TypeGraph (reachesTo)
import           Semantics.Types
import           Text.Printf
import           TypeCheck.Common
import           Utils               (graphToNodes)

toMeta :: VarMeta -> StateT FEnv TypeCheckResult TypedMeta
toMeta  m@(Meta pt pos mid _) = do
  env@FEnv{feTypeEnv} <- get
  case descriptor env m of
    TypeCheckResult notes SType{stypeAct=ub, stypeReq=req, stypeTree=rt} -> do
      -- The @act ⊆ req@ invariant is deferred out of inference ('TypeCheck.Constrain.checkScheme')
      -- and enforced here, once inference has converged: the actual must satisfy the requirement.
      -- A bare type-variable actual is a placeholder that cannot be compared without its binding
      -- (which lived in the constraint var-arg env, unavailable here), so skip it — mirroring
      -- 'verifyScheme's 'verifySchemeActLowers'. No form is exempt from the check for being one
      -- the graph could still rewrite: what a term may become is decided by the arrows it has
      -- and by what its consumer permits, and by the time inference has converged the actual
      -- holds what survived that.
      -- Both schemes are UPPER BOUNDS on the point's values, and under req-only
      -- 'BoundedByObjs' the REQUIREMENT can legitimately be the tighter one (a
      -- module-generic definition-body actual against a slot-refined requirement), so plain
      -- containment is too strict. The invariant that must hold per enumerated FORM is
      -- compatibility: every actual leaf must be ABLE to satisfy the requirement (a non-⊥
      -- meet — the portion the requirement admits, which is also what the stored meta below
      -- carries onward). A leaf meeting to ⊥ (an @Integer ∪ String@ actual against an
      -- @Integer@ requirement flags the @String@ form) is the genuine error.
      --
      -- A form REACHING the requirement satisfies it as much as one already inside it. An arrow
      -- is a choice, not an obligation, so a term may still be spelled as the call it has not
      -- taken (@equal(this, other)@ beside @∀Boolean@ — the spelling eval's dispatch keying
      -- reads), and asking whether it is CONTAINED in the requirement asks the wrong question:
      -- what a requirement says is which destinations are permitted, and a term satisfies it by
      -- having a path to one. So a leaf whose arrows reach the requirement is accepted, and only
      -- a leaf that neither meets it nor reaches it is the genuine error. The reach is read from
      -- the base env, whose unresolved arrow metas make it WIDER — the safe direction for a
      -- check, since a wider reach can only withhold an error, never invent one.
      let leafCompatibleWithReq l = not (isBottomType (intersectTypes feTypeEnv (singletonType l) req))
                                  || reachesTo feTypeEnv H.empty (singletonType l) req
      let actSatisfiesReq = case ub of
            TypeVar{} -> True
            _ | isSubtypeOfWithEnv feTypeEnv H.empty ub req -> True
            UnionType _ leafs _ -> all leafCompatibleWithReq (splitUnionType leafs)
      let notes' = if actSatisfiesReq
            then notes
            else GenTypeCheckError (clearMetaDat ArrMeta m) (printf "Actual type %s is not a subtype of the required type %s" (show ub) (show req)) : notes
      -- The meta's preset type @pt@ is met into the final type. Under actual seeding
      -- ('TypeCheck.Encode.fromMeta' BAct) this is a no-op — the actual started at @pt@ and only
      -- narrowed. Under requirement seeding (BReq) @pt@ is an upper bound on the point's values
      -- like any other, and it bounds every form the point holds: a form the graph can still
      -- rewrite is a value of the point as much as a constructed one, and a bound that skipped it
      -- would not be a bound. Uniform over the union, as 'refineLeafByReq' below already is.
      let ubMet = intersectTypes feTypeEnv ub pt
      -- Materialize the REQUIREMENT into the stored meta, REFINING each actual leaf in place
      -- and never multiplying the enumeration: the meta's union is eval/TreeBuild's
      -- enumeration of the FORMS the term may take, while the requirement's same-name
      -- alternatives are a disjunction of candidate contracts (the reverse arrow direction's
      -- all-accepting-arrows union) — a general @act ∩ req@ crosses the two and manufactures
      -- instantiated alternatives the term never enumerates (a ⊤-fielded raw @operator+@ met
      -- with the Float instance's contract), which downstream per-leaf tree builds must then
      -- handle and cannot. A leaf's meet is adopted only when it is a genuine refinement — a
      -- single leaf of the same form, narrower content (the module-generic
      -- @Node(value=⊤, ..)@ against its slot-refined requirement) — and kept as-is otherwise;
      -- which forms exist is inference's decision, not Decode's. Uniform over the union: no
      -- settled/raw distinction.
      let refineLeafByReq l = case intersectTypes feTypeEnv (singletonType l) req of
            UnionType Nothing ls [] -> case splitUnionType ls of
              [l'] | ptName l' == ptName l -> l'
              _                            -> l
            _ -> l
      let ubMetReq = case ubMet of
            UnionType mTop leafs consts | req /= PTopType, not (isTypeVar req) ->
              UnionType mTop (joinUnionType (map refineLeafByReq (splitUnionType leafs))) consts
            _ -> ubMet
      lift $ TypeCheckResult notes' $ Meta ubMetReq pos mid rt
    TypeCheckResE notes -> lift $ TypeCheckResult notes (Meta BottomType pos mid Nothing)

member :: Name -> [Name] -> Bool
member x arr = case suffixLookup x arr of
  Just _  -> True
  Nothing -> False

toExpr :: VExpr -> StateT FEnv TypeCheckResult TExpr
toExpr (CExpr m c) = do
  m' <- toMeta m
  return $ CExpr m' c
toExpr (Value m name) = do
  let name' = case maybeGetSingleton $ getMetaType m of
        Just PartialType{ptName=n} -> n
        _                          -> makeAbsoluteName name -- TODO Maybe consider this an exception
  m' <- toMeta m
  return $ Value m' name'
toExpr (HoleExpr m hole) = do
  m' <- toMeta m
  return $ HoleExpr m' hole
toExpr (AliasExpr base alias) = do
  base' <- toExpr base
  alias' <- toExpr alias
  return $ AliasExpr base' alias'
toExpr (EWhere m base cond) = do
  m' <- toMeta m
  base' <- toExpr base
  cond' <- toExpr cond
  return $ EWhere m' base' cond'
toExpr expr@(TupleApply m (baseM, baseExpr) arg) = do
  let mclear = clearMetaDat ArrMeta m
  m' <- toMeta m
  baseM' <- toMeta baseM
  baseExpr' <- toExpr baseExpr
  arg' <- case arg of
    EAppArg a@ObjArr{oaObj, oaArr, oaAnnots} -> do
      -- The argument name learned during type inference: the single argument present in the
      -- tuple-application result meta but not in its base meta.
      let learnedArgName = case (getMetaType baseM', getMetaType m') of
            (UnionType Nothing basePartialLeafs [], UnionType Nothing partialLeafs []) -> case (splitUnionType basePartialLeafs, splitUnionType partialLeafs) of
              ([PartialType{ptArgs=basePartialArgs}], [PartialType{ptArgs}]) -> case S.toList $ S.difference (H.keysSet ptArgs) (H.keysSet basePartialArgs) of
                [argN] -> Right argN
                opts -> Left (printf "Failed argument inference due to multiple arg options %s in %s" (show opts) (show expr) :: String)
              (base, result) -> Left (printf "Failed argument inference due to multiple types with base %s and result %s in %s" (show base) (show result) (show expr) :: String)
            (baseM'', m'') -> Left (printf "Failed argument inference due to non UnionType in baseMeta %s or meta %s in %s" (show baseM'') (show m'') (show expr) :: String)
      let mkArgValue argName = Value (mWithType (singletonType $ partialToType argName) $ emptyMetaM m') (pkName argName)
      let decodeArr = mapM (\(oaArrExpr, oaArrM) -> do
            oaArrExpr' <- mapM toExpr oaArrExpr
            oaArrM' <- toMeta oaArrM
            return (oaArrExpr', oaArrM'))
      (oaObj', oaArr') <- case (oaObj, oaArr) of
        -- An anonymous applied argument (e.g. the call site @f(x)@): finalize the learned
        -- keyword as the object.
        (Nothing, _) -> do
          argName <- case learnedArgName of
            Right n  -> return n
            Left err -> lift $ TypeCheckResE [GenTypeCheckError mclear err]
          arr' <- decodeArr oaArr
          return (Just $ mkArgValue argName, arr')
        -- A destructuring pattern (e.g. @Box(v)@) whose head does not name the argument it
        -- fills: rewrite to the learned keyword form (@b= Box(v)@) so that downstream
        -- TreeBuild and Eval bind by the argument name rather than the pattern's type head.
        (Just joaObj@TupleApply{}, Just (Nothing, _arrM))
          | Right argName <- learnedArgName
          , makeAbsoluteName (pkName argName) /= makeAbsoluteName (exprPath joaObj) -> do
              joaObj' <- toExpr joaObj
              -- The argument slot takes the pattern's own (resolved) type, mirroring the named
              -- form @b= Box(v)@ where the arr slot is unified with the arr value's type.
              return (Just $ mkArgValue argName, Just (Just joaObj', getExprMeta joaObj'))
        -- An explicit object that is only a name (@code= 98@, a parameter @code -> Integer@) is the
        -- argument's KEY, not a reference: it names the slot it fills, and is written into the
        -- tuple under that name. So its name is the one written, made absolute, rather than
        -- whatever the name resolves to as a reference — which, for a key sharing its name with a
        -- declared function (@fromCode(code)@ beside @:Char.code@), is that function.
        (Just (Value km kn), _) -> do
          km' <- toMeta km
          arr' <- decodeArr oaArr
          return (Just $ Value km' (makeAbsoluteName kn), arr')
        -- An explicit object (named argument, or a pattern whose head already names the arg).
        (Just joaObj, _) -> do
          joaObj' <- toExpr joaObj
          arr' <- decodeArr oaArr
          return (Just joaObj', arr')
      oaAnnots' <- mapM toExpr oaAnnots
      return $ EAppArg a{oaObj=oaObj', oaArr=oaArr', oaAnnots=oaAnnots'}
    EAppVar vn vm -> EAppVar vn <$> toMeta vm
    EAppSpread a -> EAppSpread <$> toExpr a
  return $ TupleApply m' (baseM', baseExpr') arg'

toObjArr :: VObjArr -> StateT FEnv TypeCheckResult TObjArr
toObjArr oa@ObjArr{oaObj, oaArr, oaAnnots} = do
  oaObj' <- mapM toExpr oaObj
  oaArr' <- forM oaArr $ \(arrE, arrM) -> do
    arrE' <- mapM toExpr arrE
    arrM' <- toMeta arrM
    return (arrE', arrM')
  oaAnnots' <- mapM toExpr oaAnnots
  return oa{oaObj=oaObj', oaArr=oaArr', oaAnnots=oaAnnots'}

toPrgm :: VPrgm -> StateT FEnv TypeCheckResult TPrgm
toPrgm (objMap, classGraph, annots) = do
  objMap' <- mapMObjectMap toObjArr objMap
  annots' <- mapM toExpr annots
  return $ Prgm objMap' classGraph annots'

-- | Detects a /diagonal/ declaration: one whose every argument is typed by a single shared type
-- variable (e.g. @operator==($T l, $T r)@). Returns the variable's bound and the argument names
-- it binds. The arg→variable link is read from the declaration expression's applied args (their
-- arrow metas carry the variable, even though the decoded partial stores each arg as the bound).
diagonalVar :: ObjArr Expr TypedMetaDat -> PartialType -> Maybe (Type, [ArgName])
diagonalVar oa p = case sharedVars of
  [v] | not (H.null (ptArgs p)) && all (`elem` argsForVar v) (H.keys (ptArgs p)) ->
    Just (H.lookupDefault PTopType v (ptVars p), argsForVar v)
  _ -> Nothing
  where
    argVar = mapMaybe argToVar (exprAppliedArgs (oaObjExpr oa))
    -- A higher-order argument (@fn(v) -> $M@) is not typed by its arrow's result: the result
    -- is the arrow it adds to the body's environment, and its slot holds its input object.
    argToVar a = case (oaObj a, oaArr a) of
      (Just objE, Just (_, arrM)) | null (exprAppliedArgs objE) -> case getMetaType arrM of
        TypeVar (TVVar v) _ _ -> Just (inExprSingleton objE, v)
        _                     -> Nothing
      _ -> Nothing
    sharedVars = [v | (v, n) <- H.toList (H.fromListWith (+) [(v, 1 :: Int) | (_, v) <- argVar]), n >= 2]
    argsForVar v = [a | (a, v') <- argVar, v' == v]

-- | The /diagonal projection/ of a definition for the given (shared) argument positions: the
-- intersection of the definition's values at those positions. A diagonal declaration @($T,…,$T)@
-- is fulfilled at a concrete type @t@ iff some definition covers @t@ at /every/ shared position,
-- so the variable's bound must be covered by the union of these projections across definitions.
diagonalProjection :: (TypeGraph tg) => TypeEnv tg -> [ArgName] -> Type -> Type
diagonalProjection typeEnv argNames (UnionType Nothing leafs []) =
  unionAllTypes typeEnv [intersectAllTypes typeEnv (mapMaybe (`typeGetArg` p) argNames) | p <- splitUnionType leafs]
diagonalProjection _ _ _ = BottomType

-- | Detects a /class-parameter method/ declaration: one whose @this@ receiver is a parameterized
-- class predicate (e.g. @this -> TEqv[T=$T]@ for a method over @class(TEqv[$T])@) and at least
-- one other argument is typed by one of that class's parameter variables (e.g.
-- @:TEqv.rel(a -> $T, b -> $T)@). Such a declaration is fulfilled by covering its /receiver/
-- dimension — every class member must have a definition — because the parameter-typed arguments
-- are supplied per-member (each definition at its own member's parameter) and are validated by
-- the definition-within-declaration check, not by the full argument product (which would
-- spuriously demand the arguments at the parameter's whole bound, e.g. 'TopType').
isClassParamMethodDecl :: ObjArr Expr TypedMetaDat -> Bool
isClassParamMethodDecl oa = not (null receiverVars) && any isParamArg args
  where
    args = exprAppliedArgs (oaObjExpr oa)
    argType a = getMetaType . snd <$> oaArr a
    isThis a = maybe False ((`elem` [thisKey, "this"]) . nameStr . exprPath) (oaObj a)
    receiverVars =
      [ v
      | a <- args, isThis a
      , Just (TopType _ (PredsOne (PredClass PartialType{ptVars}))) <- [argType a]
      , v <- H.keys ptVars
      ]
    -- As in 'diagonalVar', a higher-order argument's arrow result does not type its slot.
    isParamArg a = not (isThis a) && maybe True (null . exprAppliedArgs) (oaObj a) && case argType a of
      Just (TypeVar (TVVar v) _ _) -> v `elem` receiverVars
      _                            -> False

-- | Universalize /destructuring binding variables/ that appear as argument values in a
-- definition's input type. A pattern like @MyJust(v= lv)@ matches /any/ @MyJust@ regardless
-- of the element, so for declaration-coverage its value at @v@ is the whole field domain,
-- not the singleton of the fresh binding name @lv@ (which would make the definition appear
-- to cover only that one instantiation). A binding variable is an argument whose value is a
-- bare nominal naming nothing defined in the program; concrete pattern values (e.g. @v= 5@,
-- whose value is @Integer@) are defined and so are left precise.
universalizeBindingArgs :: S.HashSet Name -> Type -> Type
universalizeBindingArgs defined = go
  where
    go (UnionType Nothing leafs []) = UnionType Nothing (joinUnionType $ map goPartial $ splitUnionType leafs) []
    go t = t
    goPartial p@PartialType{ptArgs} = p{ptArgs = H.map goArg ptArgs}
    goArg v
      | isBinding v = PTopType
      | otherwise   = go v
    isBinding v = case v of
      UnionType Nothing leafs [] -> case splitUnionType leafs of
        [PartialType{ptName, ptArgs, ptVars}] -> H.null ptArgs && H.null ptVars && not (S.member (makeAbsoluteName ptName) defined)
        _                                     -> False
      _ -> False

-- | The single partial of a type, when it has exactly one.
singleLeaf :: Type -> Maybe PartialType
singleLeaf (UnionType Nothing leafs []) = case splitUnionType leafs of
  [p] -> Just p
  _   -> Nothing
singleLeaf _ = Nothing

-- | What a declaration's definitions leave undefined, with the target they were compared as, or
-- 'Nothing' where they fulfill it. The definitions' input types are given universalized
-- ('universalizeBindingArgs'), with their union.
declUndefined :: FEnvTypeEnv -> [Type] -> Type -> ObjArr Expr TypedMetaDat -> Maybe (Type, Type)
declUndefined prgmTypeEnv defInputTypesU defUnionU oa
  | receiverFulfilled || isSubtypeOf prgmTypeEnv checkType target = Nothing
  | otherwise = Just (target, snd $ differenceTypeWithEnv prgmTypeEnv H.empty checkType target)
  where
    declType = getMetaType $ getExprMeta $ oaObjExpr oa
    -- A diagonal declaration (every argument typed by one shared variable, e.g.
    -- @operator==($T l, $T r)@) is fulfilled iff the variable's bound is covered by the
    -- diagonal projection of the definitions, not by the full product. Checking the product
    -- would spuriously demand off-diagonal pairs like @(Integer, Float)@.
    (checkType, target) = case diagonalVar oa =<< singleLeaf declType of
      Just (varBound, varArgs) -> (varBound, unionAllTypes prgmTypeEnv (map (diagonalProjection prgmTypeEnv varArgs) defInputTypesU))
      Nothing                  -> (declType, defUnionU)
    -- A class-parameter method is fulfilled by covering its receiver dimension alone (every
    -- class member must have a definition); its parameter-typed arguments are per-member and
    -- are validated by the definition-within-declaration check. This is an additional
    -- acceptance path: it never suppresses a failure the product check would otherwise pass.
    receiverFulfilled = isClassParamMethodDecl oa &&
      case typeGetArg (partialKey "this") =<< singleLeaf declType of
        Just recvDecl ->
          let recvDefs = mapMaybe (\t -> typeGetArg (partialKey "this") =<< singleLeaf t) defInputTypesU
          in not (null recvDefs) && isSubtypeOf prgmTypeEnv recvDecl (unionAllTypes prgmTypeEnv recvDefs)
        Nothing -> False

-- | Checks one name's definitions against its declarations, and its declarations against its
-- definitions.
--
-- The @importedDecls@ are the name's declarations from an earlier solve. They bound this solve's
-- definitions exactly as its own declarations do: an arm written on one page of a declaration made
-- on another (a class instance, such as @core\/maybe.ct@'s @compareTo@ arms for the declaration on
-- @core\/ord.ct@) is inside that declaration or it is an error, wherever the two were compiled.
-- Without them an arm whose declaration was solved first was checked against nothing. They are
-- not checked for fulfillment here, since what fulfills them was mostly defined in the solve that
-- declared them.
verifyObjMapItem :: TypeName -> [ObjArr Expr TypedMetaDat] -> ObjectMapItem Expr TypedMetaDat -> StateT FEnv TypeCheckResult ()
verifyObjMapItem name importedDecls (ObjectMapItem decls runtimeDecls refineDecls defs) = do
  FEnv{feTypeEnv, feLocalDefNames} <- get
  let prgmTypeEnv = feTypeEnv{tePrgmEnv = True}
  -- A runtime declaration has no body, but it behaves like a definition for coverage: it is
  -- bounded by the declarations (checked against declUnion) and contributes to the definition
  -- coverage that fulfills them (part of defUnion).
  let defLikeOAs = defs ++ runtimeDecls
  -- Only a refinement claims a result to prove; a @#proof@ anywhere else would be silently
  -- ignored, so it is an error. Checked before the coverage below, which returns early for a
  -- name without declarations or definitions.
  forM_ (decls ++ defLikeOAs) $ \oa -> when (oaHasAnnotRel proofAnnot oa) $
    lift $ TypeCheckResult [ProofWithoutRefine name (getMetaPos . getExprMeta =<< oaObj oa)] ()
  case (map (getMetaType . getExprMeta . oaObjExpr) (decls ++ importedDecls), map (getMetaType . getExprMeta . oaObjExpr) defLikeOAs) of
    ([], _) -> pure ()
    (_, []) -> pure ()
    (declInputTypes0, defInputTypes0) -> do
      -- A declaration may write a parameterized type in /param form/ (e.g. @Box[T]@) while a
      -- destructuring definition produces it in /field form/ (e.g. @Box(v=...)@, completed to
      -- @Box[T](v=$T)@). Normalize both to the completed form so coverage compares like with like.
      let declInputTypes = declInputTypes0
      let defInputTypes = defInputTypes0
      let declUnion = unionAllTypes feTypeEnv declInputTypes

      -- For /coverage/ (does the union of definitions fulfill a declaration?) a destructuring
      -- binding matches any value at its field, so universalize those fields; see
      -- 'universalizeBindingArgs'. This is used only for the fulfillment target below, never for
      -- the definition-within-declaration check, which needs the precise (non-widened) type.
      let ObjArrTypeGraph (ObjectMap objTg) = teTypeGraph feTypeEnv
      let definedNames = (S.map makeAbsoluteName $ H.keysSet objTg) <> feLocalDefNames
      let defInputTypesU = map (universalizeBindingArgs definedNames) defInputTypes
      let defUnionU = unionAllTypes feTypeEnv defInputTypesU

      -- Check each definition (including runtime declarations) against declUnion
      let defsWithLocs = zip defInputTypes (map (getMetaPos . getExprMeta) (mapMaybe oaObj defLikeOAs))
      forM_ defsWithLocs $ \(defType, defLoc) ->
        unless (isSubtypeOf prgmTypeEnv defType declUnion) $ do
          let undeclared = snd $ differenceTypeWithEnv prgmTypeEnv H.empty defType declUnion
          lift $ TypeCheckResult [DefinitionOutsideDeclaration name defType declUnion undeclared defLoc] ()

      -- Check each declaration against defUnion (which includes runtime declarations). Data-type
      -- (TypeObj) declarations are self-defining and need no separate definition, so they are
      -- exempt from the fulfillment check (they still bound definitions via declUnion above).
      let fulfillableDecls = filter ((/= TypeObj) . oaBasis) decls
      forM_ fulfillableDecls $ \oa ->
        forM_ (declUndefined prgmTypeEnv defInputTypesU defUnionU oa) $ \(target, notDefined) -> do
          let declType = getMetaType $ getExprMeta $ oaObjExpr oa
          let declLoc = getMetaPos $ getExprMeta $ oaObjExpr oa
          lift $ TypeCheckResult [UnfulfilledDeclaration name declType target notDefined declLoc] ()

      -- Check each refinement declaration against declUnion
      let refineWithLocs = zip
            (map (getMetaType . getExprMeta . oaObjExpr) refineDecls)
            (map (getMetaPos . getExprMeta) (mapMaybe oaObj refineDecls))
      forM_ refineWithLocs $ \(refineType, refineLoc) ->
        unless (isSubtypeOf prgmTypeEnv refineType declUnion) $ do
          let undeclared = snd $ differenceTypeWithEnv prgmTypeEnv H.empty refineType declUnion
          lift $ TypeCheckResult [RefineOutsideDeclaration name refineType declUnion undeclared refineLoc] ()

toPrgms :: [VPrgm] -> StateT FEnv TypeCheckResult [TPrgm]
toPrgms vprgms = do
  tprgms <- mapM toPrgm vprgms

  -- Verify declaration/definition input types against the merged program (all modules' object
  -- maps unioned per name). This lets a declaration in one module bound runtime definitions of
  -- the same name in another module (e.g. the generic Eq @operator==@ declaration in algebra.ct
  -- bounds the @#runtime@ Integer/Float implementations in primitives.ct).
  let ObjectMap mergedObjMap = mconcat (map prgmObjMap tprgms)
  -- The solved dependencies this solve was given hold the declarations made in earlier solves.
  FEnv{feTTypeGraph = ObjectMap depObjMap} <- get
  let importedDecls name = maybe [] omiDecls (H.lookup name depObjMap)
  _ <- H.traverseWithKey (\name -> verifyObjMapItem name (importedDecls name)) mergedObjMap

  return tprgms

-- | Checks the members a solve adds to classes against the declarations earlier solves made over
-- those classes.
--
-- A page that says @every(Maybe[$T -> Eq], isa= [Eq])@ makes every declaration over @Eq@ a
-- declaration over @Maybe@ too, so @:$T.eq[$T -> Eq]@ is only fulfilled if the page (or one
-- before it) defines @eq@ for a @Maybe@. 'verifyObjMapItem' cannot see that: the declaration
-- was solved, and checked, before the member existed. So each such declaration is checked again
-- here with every definition now known, and what the members added here leave undefined is
-- reported on this page. That part is the undefined part now, less what was already undefined
-- under the dependencies alone, so an older gap is reported once, where it was made.
verifyAddedMembers :: ClassGraph -> [TPrgm] -> [TPrgm] -> StateT FEnv TypeCheckResult ()
verifyAddedMembers (ClassGraph pageCg) deps solved = do
  FEnv{feTypeEnv, feLocalDefNames} <- get
  let addedTo = S.fromList [ pcnName k | (CGClass{cgcExtends}, k@PClassName{}, _) <- graphToNodes pageCg, not (null cgcExtends) ]
  let ObjectMap depItems = prgmObjMap (mconcat deps)
  let ObjectMap solvedItems = prgmObjMap (mconcat solved)
  let envNow = feTypeEnv{tePrgmEnv = True}
  let envBefore = (mkTypeEnv (mconcat (map (mapMetaPrgm clearMetaDat) deps))){tePrgmEnv = True}
  let inputType = getMetaType . getExprMeta . oaObjExpr
  let universalized env defined oas = let ts = map (universalizeBindingArgs defined . inputType) oas in (ts, unionAllTypes env ts)
  let definedIn env extra = let ObjArrTypeGraph (ObjectMap objTg) = teTypeGraph env in S.map makeAbsoluteName (H.keysSet objTg) <> extra
  unless (S.null addedTo) $
    forM_ (H.toList depItems) $ \(name, ObjectMapItem decls depRuntime _ depDefs) -> do
      let affected = [ oa | oa <- decls, oaBasis oa /= TypeObj, not (S.null (S.intersection addedTo (typePredClassNames (inputType oa)))) ]
      let pageDefs = maybe [] (\it -> omiDefs it ++ omiRuntimeDecls it) (H.lookup name solvedItems)
      let (nowTs, nowU) = universalized envNow (definedIn envNow feLocalDefNames) (depDefs ++ depRuntime ++ pageDefs)
      let (beforeTs, beforeU) = universalized envBefore (definedIn envBefore S.empty) (depDefs ++ depRuntime)
      forM_ affected $ \oa ->
        forM_ (declUndefined envNow nowTs nowU oa) $ \(target, notNow) -> do
          let notBefore = maybe BottomType snd (declUndefined envBefore beforeTs beforeU oa)
          let added = snd $ differenceTypeWithEnv envNow H.empty notNow notBefore
          unless (isBottomType added) $
            lift $ TypeCheckResult [UnfulfilledDeclaration name (inputType oa) target added (getMetaPos $ getExprMeta $ oaObjExpr oa)] ()

-- | The classes a type names in its predicates, at any depth.
typePredClassNames :: Type -> S.HashSet TypeName
typePredClassNames (TypeVar _ _ inner) = typePredClassNames inner
typePredClassNames (UnionType mTop leafs _) = maybe S.empty (\(preds, _, _) -> predsNames preds) mTop <> S.unions (map partialNames (splitUnionType leafs))
  where
    partialNames PartialType{ptVars, ptArgs, ptPreds} = S.unions (map typePredClassNames (H.elems ptVars ++ H.elems ptArgs)) <> predsNames ptPreds
    predsNames (PredsOne (PredClass p)) = S.insert (ptName p) (partialNames p)
    predsNames (PredsOne (PredRel p))   = partialNames p
    predsNames (PredsOne (PredExpr p))  = partialNames p
    predsNames (PredsOne PredConstEq{}) = S.empty
    predsNames (PredsAnd ps)            = S.unions (map predsNames ps)
    predsNames (PredsNot p)             = predsNames p
