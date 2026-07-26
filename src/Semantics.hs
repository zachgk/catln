--------------------------------------------------------------------
-- |
-- Module    :  Semantics
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module contains miscellaneous and general syntax elements.
-- It also contains a the instances of program metadata and some
-- general syntax-based utilities.
--------------------------------------------------------------------

module Semantics where

import qualified Data.HashMap.Strict as H

import           CtConstants         (builtinAnnotNames, operatorHasArrow)
import           Data.Graph          (graphFromEdges)
import qualified Data.HashSet        as S
import           Data.List           (partition)
import           Data.Maybe
import           Semantics.Prgm
import           Semantics.Types
import           System.Directory    (doesDirectoryExist)
import           System.IO.Unsafe    (unsafePerformIO)
import           Text.Printf

newtype ObjArrTypeGraph e m = ObjArrTypeGraph (ObjectMap e m)
  deriving (Show)
instance Semigroup (ObjArrTypeGraph e m) where
  (ObjArrTypeGraph a) <> (ObjArrTypeGraph b) = ObjArrTypeGraph (a <> b)
-- | Shared body of 'typeGraphQueryWithReason' (@prune=True@) and
-- 'typeGraphQueryAllWithReason' (@prune=False@ — every accepting arrow contributes).
objArrGraphQuery :: (ExprClass e, MetaDat m, Show m, Show (e m)) => Bool -> TypeEnv (ObjArrTypeGraph e m) -> TypeVarArgEnv -> PartialType -> [(String, Type, Type)]
objArrGraphQuery prune typeEnv@TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} vaenv partial@PartialType{ptName} = concatMap tryTArrow $ promises $ flatObjectMapItem $ H.lookupDefault mempty ptName tg
    where
      promises | prune     = mostSpecificPromises
               | otherwise = filter accepts
      -- Each declaration/definition is a PROMISE: given an input of one type, an output of a certain
      -- type can be produced. Among the promises the source triggers, keep only the MOST HELPFUL — an
      -- arrow whose object (input type) is a strict SUPERTYPE of another triggered arrow's object is
      -- dominated and dropped. The abstract accessor DECLARATION @:$T.eq -> Eqv[$T]@ (object
      -- @eq(this=$T->Eq)@, result the class-top @∀Eqv@) is thus dropped for a concrete source whenever
      -- the specific DEFINITION @:Integer.eq= IntOrdering@ (object @eq(this=Integer)@) also triggers,
      -- so the concrete rewrite is not absorbed by the declaration's class-top in the reaches union.
      -- Declarations and definitions compete on equal footing; specificity alone decides, and equally
      -- specific promises (e.g. a recursive function's own decl and def) are both kept.
      mostSpecificPromises arrows = filter (\oa -> not (any (`strictlyDominates` oa) triggered)) triggered
        where triggered = filter accepts arrows
      accepts oa@ObjArr{oaArr=Just{}} = not $ isBottomType $ intersectTypesEnv typeEnv vaenv (singletonType partial) (arrowObjType oa)
      accepts _ = False
      arrowObjType oa = getMetaType $ getExprMeta $ oaObjExpr oa
      -- @a@ strictly dominates @b@ when a's object is a strict subtype of b's (a is more specific).
      strictlyDominates a b = let oa' = arrowObjType a; ob = arrowObjType b
                              in oa' /= ob && isSubtypeOfWithEnv typeEnv vaenv oa' ob

      tryTArrow :: (ExprClass e, MetaDat m, Show m, Show (e m)) => ObjArr e m -> [(String, Type, Type)]
      tryTArrow oa@ObjArr{oaArr=Just{}} = do
        -- It is possible to send part of a partial through the arrow, so must compute the valid part
        -- If none of it is valid, then there is Nothing
        case intersectTypesEnv typeEnv vaenv (singletonType partial) (getMetaType $ getExprMeta $ oaObjExpr oa) of
          potentialSrc@(UnionType Nothing potSrcLeafs []) -> if not (isBottomType potentialSrc)
            then map (printf "using %s" (show oa), potentialSrc,) $ joinPartialDestTypes [arrowDestType typeEnv potentialSrcPartial oa | potentialSrcPartial <- splitUnionType potSrcLeafs]
            else []
          _ -> error "Unexpected non-union in TryTArrow"
      tryTArrow ObjArr{oaArr=Nothing} = []

      joinPartialDestTypes :: [[Type]] -> [Type]
      joinPartialDestTypes [] = [BottomType]
      joinPartialDestTypes (pdt1:restPdt) = [unionTypes typeEnv pdt1Poss restPdt' | pdt1Poss <- pdt1, restPdt' <- joinPartialDestTypes restPdt]

instance (ExprClass e, MetaDat m, Show m, Show (e m)) => TypeGraph (ObjArrTypeGraph e m) where
  typeGraphQueryWithReason = objArrGraphQuery True
  typeGraphQueryAllWithReason = objArrGraphQuery False
 -- TODO: Implement this to expand other PredExprs as well, and begin using the Nothing to represent having no meaningful way to break
  typeGraphExpandPredExpr typeEnv _vaenv partial | nameStr (ptName partial) == operatorHasArrow = Just $ map (\p -> p{ptPreds=PredsOne (PredExpr partial)}) $ topTypeAsPartials typeEnv
  typeGraphExpandPredExpr typeEnv _vaenv partial = Just $ map (\p -> p{ptPreds=PredsOne (PredExpr partial)}) $ topTypeAsPartials typeEnv

  typeGraphData TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} name = concatMap dataPartials $ flatObjectMapItem $ H.lookupDefault mempty name tg
    where
      -- Data definitions have no arrow (oaArr=Nothing); their object expression carries the
      -- declared type with linked ptVars/ptArgs.
      dataPartials oa@ObjArr{oaArr=Nothing} = case getMetaType $ getExprMeta $ oaObjExpr oa of
        UnionType Nothing leafs _ -> splitUnionType leafs
        _                         -> []
      dataPartials ObjArr{oaArr=Just{}} = []

  -- The arg keysets come from the object EXPRESSION structure ('exprAppliedArgs' /
  -- 'inExprSingleton' — the same keying every partial built from an expression uses), not
  -- the expression's meta type, which need not be resolved. A relatively-spelled name (a
  -- class-graph node's @#noCore@ vs the objMap's @/#noCore@) matches by
  -- 'relativeNameFilter'; its realizable arg-sets are bounded by the union over every
  -- matching declaration. A name matching no declaration is open world ('Nothing').
  typeGraphArgKeys TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} name = case H.lookup name tg of
    Just item -> Just $ itemArgKeys item
    Nothing   -> case relativeNameFilter name (H.keys tg) of
      []    -> Nothing
      names -> Just $ concatMap (itemArgKeys . (tg H.!)) names
    where
      itemArgKeys item = [ S.fromList [inExprSingleton (oaObjExpr a) | a <- exprAppliedArgs (oaObjExpr oa)]
                         | oa <- flatObjectMapItem item ]

  typeGraphDeclaredResult typeEnv@TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} vaenv partial@PartialType{ptName=pName} =
    case declDests of
      (_:_) -> unionAllTypes typeEnv declDests
      -- No declaration (e.g. @sorted@): the return type is INFERRED from the definition arms —
      -- 'PTopType' until they resolve, refining as they do. Each arm dest's own raw applied leaves
      -- are then replaced by THEIR declared result (one level, declared-only, so this terminates):
      -- @sorted@'s Cons arm dest @insert(raw)@ becomes @∀ConsList@, giving @sorted@ the @Cons@ its
      -- arms produce so a requirement built from it admits @Cons@ instead of collapsing to @Nil@.
      []    -> case [ armDest
                    | oa@ObjArr{oaArr=Just{}} <- omiDefs item
                    , not $ isBottomType $ intersectTypesEnv typeEnv vaenv (singletonType partial) (getMetaType $ getExprMeta $ oaObjExpr oa)
                    , armDest <- arrowDestType typeEnv partial oa ] of
                 []       -> PTopType
                 defDests -> unionAllTypes typeEnv (defDests ++ concatMap declResultOfRawLeaves defDests)
    where
      item = H.lookupDefault mempty pName tg
      declDests = [ dest
                  | oa@ObjArr{oaArr=Just{}} <- omiDecls item
                  , not $ isBottomType $ intersectTypesEnv typeEnv vaenv (singletonType partial) (getMetaType $ getExprMeta $ oaObjExpr oa)
                  , dest <- arrowDestType typeEnv partial oa ]
      -- The declared result of each raw applied leaf in a type (declared functions only — no
      -- recursion into undeclared ones, keeping this bounded).
      declResultOfRawLeaves (UnionType Nothing ls _) =
        [ d | l <- splitUnionType ls, null (typeGraphData typeEnv (ptName l))
            , oa@ObjArr{oaArr=Just{}} <- omiDecls (H.lookupDefault mempty (ptName l) tg)
            , not $ isBottomType $ intersectTypesEnv typeEnv vaenv (singletonType l) (getMetaType $ getExprMeta $ oaObjExpr oa)
            , d <- arrowDestType typeEnv l oa ]
      declResultOfRawLeaves _ = []

newtype CTSSConfig = CTSSConfig {
  -- TODO Replace stack path with (CTSSStackConfig = CTSSStackTest | CTSSStackBuild DirString BranchString) when adding support for auto-downloading core
  ctssStackPath :: String
                                }

testCtssConfig, buildCtssConfig :: CTSSConfig
testCtssConfig = CTSSConfig "stack"
-- | The standard-library location used by the CLI/LSP/WebDocs. During development
-- the compiler is run from a worktree (e.g. via @stack exec@) whose working directory
-- contains a local @stack/@ standard library; prefer it so multiple worktrees can be
-- developed and run in parallel without colliding on the single shared installed copy.
-- Only an installed compiler — run where no local @stack/core@ exists — falls back to
-- the @~/.catln/stack@ copy populated at install time. Resolved once (NOINLINE) from the
-- process's working directory, which is fixed for the run.
buildCtssConfig = unsafePerformIO $ do
  let localPath = ctssStackPath testCtssConfig
  localExists <- doesDirectoryExist (localPath ++ "/core")
  return $ if localExists then CTSSConfig localPath else CTSSConfig "~/.catln/stack"
{-# NOINLINE buildCtssConfig #-}

mkTypeEnv :: (ExprClass e, Show m, Show (e m), MetaDat m) => Prgm e m -> TypeEnv (ObjArrTypeGraph e m)
mkTypeEnv (Prgm objMap classGraph _) = TypeEnv classGraph (ObjArrTypeGraph objMap) typeNames False defaultTypeEnvDebug False False (mkSatCache classGraph typeNames)
  where
    -- The compiler-emitted annotation names ('builtinAnnotNames') are part of every program's
    -- vocabulary: desugar injects uses of them (doc comments become @#md@, @>@ statements become
    -- @#print@, ...) whether or not their compile.ct declarations are in the import closure.
    typeNames = S.fromList $ map makeAbsoluteName $ map mkName builtinAnnotNames ++ concatMap getAllObjArrNames (flatObjectMap objMap)

emptyMetaM :: (MetaDat m) => Meta m -> Meta m
emptyMetaM m = emptyMetaN{getMetaPos=getMetaPos m}

emptyMetaE :: (ExprClass e, MetaDat m) => e m -> Meta m
emptyMetaE e = emptyMetaM (getExprMeta e)

exprWithMeta :: (Show m) => Meta m -> Expr m -> Expr m
exprWithMeta m (CExpr _ c)        = CExpr m c
exprWithMeta m (Value _ n)        = Value m n
exprWithMeta m (HoleExpr _ h)     = HoleExpr m h
exprWithMeta m (EWhere _ base cond) = EWhere m base cond
exprWithMeta m (TupleApply _ b a) = TupleApply m b a
exprWithMeta _ e                  = error $ printf "exprWithMeta with unsupported expr %s" (show e)

-- | Returns the EWhere conditions in an expression
exprWhereConds :: Expr m -> [Expr m]
exprWhereConds CExpr{} = []
exprWhereConds Value{} = []
exprWhereConds HoleExpr{} = []
exprWhereConds (AliasExpr b _) = exprWhereConds b
exprWhereConds (EWhere _ _ c) = [c]
exprWhereConds (TupleApply _ (_, b) (EAppArg ObjArr{oaArr=Nothing})) = exprWhereConds b
exprWhereConds (TupleApply _ (_, b) (EAppArg ObjArr{oaArr=Just (me, _)})) = exprWhereConds b ++ maybe [] exprWhereConds me
exprWhereConds (TupleApply _ (_, b) EAppVar{}) = exprWhereConds b
exprWhereConds (TupleApply _ (_, b) (EAppSpread a)) = exprWhereConds b ++ exprWhereConds a

exprWithMetaType :: (Show m) => Type -> Expr m -> Expr m
exprWithMetaType t e = exprWithMeta (mWithType t (getExprMeta e)) e

classGraphFromObjs :: (ExprClass e, MetaDat m, Show m, Show (e m)) => ObjectMap e m -> ClassGraph
classGraphFromObjs objMap = ClassGraph $ graphFromEdges $ map (\oa -> (CGType, PTypeName (oaObjPath oa), [])) $ flatObjectMap objMap

mapExprPath :: (Show m) => ((Meta m, TypeName) -> Expr m) -> Expr m -> Expr m
mapExprPath f (Value m n) = f (m, n)
mapExprPath f (EWhere m b c) = EWhere m (mapExprPath f b) c
mapExprPath f (TupleApply m (bm, be) a) = TupleApply m (bm, mapExprPath f be) a
mapExprPath _ e = error $ printf "Unexpected expr to mapExprPath: %s" (show e)

mapOAObjExpr :: (MetaDat m, ExprClass e, Show (e m)) => (e m -> e m) -> ObjArr e m -> ObjArr e m
mapOAObjExpr f oa@ObjArr{oaObj=Just e}  = oa{oaObj = Just (f e)}
mapOAObjExpr _ oa@ObjArr{oaObj=Nothing} = oa

mapOAArrExpr :: (MetaDat m, ExprClass e, Show (e m)) => (e m -> e m) -> ObjArr e m -> ObjArr e m
mapOAArrExpr f oa@ObjArr{oaArr=Just (Just e, m)} = oa{oaArr = Just (Just (f e), m)}
mapOAArrExpr _ oa                           = oa

exprVarArgsWithSrcs :: (TypeGraph tg, MetaDat m, Show m) => TypeEnv tg -> [(Expr m, PartialType)] -> PossArgMetaMapWithSrc m
exprVarArgsWithSrcs typeEnv os = joinAllPossVarArgMetaWithSrc typeEnv (map (uncurry (exprVarArgsWithSrc typeEnv)) os)

exprVarArgsWithObjSrcs :: (TypeGraph tg, MetaDat m, Show m) => TypeEnv tg -> [(PartialType, ObjArr Expr m)] -> PossArgMetaMapWithSrc m
exprVarArgsWithObjSrcs typeEnv os = exprVarArgsWithSrcs typeEnv $ map (\(src, obj) -> (oaObjExpr obj, src)) os

arrowDestType :: (TypeGraph tg, ExprClass e, Show m, Show (e m), MetaDat m) => TypeEnv tg -> PartialType -> ObjArr e m -> [Type]
arrowDestType = arrowDestTypeBase False

-- | 'arrowDestType' for DISPATCH (TreeBuild/Eval arrow selection): additionally resolves
-- construction fields valued by an application of an object argument through the src
-- partial's arg slot (see @resolvedFields@ below). Dispatch works on a fixed, fully-resolved
-- src, so the extra resolution is stable there; during typecheck the same read would resolve
-- differently across epochs as the partial's vars narrow, growing the reached dest union and
-- violating monotonicity ('verifySchemeActLowers').
arrowDestTypeDispatch :: (TypeGraph tg, ExprClass e, Show m, Show (e m), MetaDat m) => TypeEnv tg -> PartialType -> ObjArr e m -> [Type]
arrowDestTypeDispatch = arrowDestTypeBase True

arrowDestTypeBase :: (TypeGraph tg, ExprClass e, Show m, Show (e m), MetaDat m) => Bool -> TypeEnv tg -> PartialType -> ObjArr e m -> [Type]
arrowDestTypeBase resolveAppFields typeEnv src oa@ObjArr{oaArr=Just (oaArrExpr, oaM)} = map (arrowDestTypeForVaenv . fmap snd) $ exprVarArgsWithSrc typeEnv (oaObjExpr oa) srcPartial
  where
    srcPartial = case maybeGetSingleton $ substituteTVExt H.empty $ singletonType src of
      Just s  -> s
      Nothing -> error $ printf "Non-singleton in arrowDestType"
    arrowDestTypeForVaenv vaenv = case oaArrExpr of
      -- _ | trace (printf "arrowDestType from %s with %s to %s.\n\t\tVanev: %s" (show src) (show oa) (show joined) (show vaenv)) False -> undefined
      Just n | isJust (maybeExprPath n) -> maybe joinedDispatch substitute (H.lookup (TVArg $ inExprSingleton n) vaenv)
      _                              -> joinedDispatch
      where
        substitute = substituteTVExt vaenv
        expr' = fmap (substitute . getExprType) oaArrExpr
        -- Re-link the declared result's Top class-parameter slots to the type variables the source
        -- bound before substituting: an accessor result parameterized by the receiver's own type
        -- (@:$T.eq -> Eqv[$T]@) desugars to @Eqv[/T=Top]@, dropping the @$/T@ link, so without this
        -- the element cannot follow the receiver (see 'relateArrowVars').
        arr' = substitute $ relateArrowVars vaenv $ getMetaType oaM
        joined = fromMaybe arr' expr'
        -- Only rewrite a body that directly CONSTRUCTS a data value (@Cons(head=x)@): its applied
        -- arguments are the constructor's fields. A function/operator body (@a && b@, @f(a=..)@) has
        -- applied arguments that are the callee's parameters, not the result's fields, so leave it.
        isDataConstruction = case maybeExprPath =<< oaArrExpr of
          Just n  -> not (null (typeGraphData typeEnv n))
          Nothing -> False
        joinedResolved | isDataConstruction = applyResolvedFields typeEnv resolvedFields joined
                       | otherwise          = joined
        -- DISPATCH ONLY ('arrowDestTypeDispatch'): the presented dest must be what the BUILT arm
        -- returns. The arm's body compiles toward the arrow's result meta ('TreeBuild.buildArrow'),
        -- so a body-meta leaf the graph can still rewrite (a raw applied term such as
        -- @mkComparison(..)@ in a @compare@ arm) settles INSIDE the arm; presenting it raw would
        -- make the caller's followup re-dispatch the already-settled result (applying
        -- @mkComparison@'s value-pattern arms to @Equal@). Replace the raw leaves by the result
        -- meta's presentation; settled leaves (data constructions, forwarded arguments) keep
        -- their tighter per-call form. Typecheck-side queries ('arrowDestType') keep the raw
        -- presentation — the reaches trees walk through it.
        joinedDispatch
          | resolveAppFields
          , arr' /= PTopType
          , UnionType mTop leafs consts <- joinedResolved
          , (_:_, settledLs) <- partition (null . typeGraphData typeEnv . ptName) (splitUnionType leafs)
          = unionTypes typeEnv (UnionType mTop (joinUnionType settledLs) consts) arr'
          | otherwise = joinedResolved
        -- A definition body that constructs a value — @Cons(head=x)@ — stores its fields with the
        -- body's (abstract) argument types: @head@ becomes Top because @x@ is abstract in the body.
        -- At dispatch the vaenv binds the argument (@x=Integer@), so resolve each field whose value
        -- is an argument reference through the vaenv, recovering @head=Integer@ (and hence the data
        -- parameter it defines — see 'applyResolvedFields'). This is the per-field analogue of the
        -- bare-path body case above (@= v@).
        resolvedFields = H.fromList $
          [ (inExprSingleton (oaObjExpr a), fv)
          | Just b <- [oaArrExpr]
          , a <- exprAppliedArgs b
          , Just (Just valExpr, _) <- [oaArr a]
          -- A bare argument reference (@head=x@): the field is the argument's bound value.
          , isJust (maybeExprPath valExpr)
          , null (exprAppliedArgs valExpr)
          , Just fv <- [H.lookup (TVArg (inExprSingleton valExpr)) vaenv] ]
          ++
          -- DISPATCH ONLY ('arrowDestTypeDispatch'): a field valued by an APPLICATION of one
          -- of the object's own arguments (@head= fn(v=head)@ where @fn@ is an object arg) is
          -- the argument's OUTPUT, which the dispatch-resolved src partial carries as the
          -- arg's slot — a declaration pointer slot (@/fn(..)=$B@, see 'updateTypeProp')
          -- resolves through the partial's own bindings (@/B=Integer@ from the call
          -- intersection). Unresolved (Top) slots and var reads contribute nothing.
          [ (inExprSingleton (oaObjExpr a), reduceApplied out)
          | resolveAppFields
          , Just b <- [oaArrExpr]
          , a <- exprAppliedArgs b
          , Just (Just valExpr, _) <- [oaArr a]
          , isJust (maybeExprPath valExpr)
          , not (null (exprAppliedArgs valExpr))
          , Just out <- [typeGetArgLoose (inExprSingleton valExpr) srcPartial]
          , out /= PTopType
          , not (isTypeVar out) ]
        -- The applied argument's slot holds what the argument RETURNS, which for a lambda whose
        -- body is itself a call is the raw call partial (@/operator+(l=Integer, r=Integer)@) —
        -- at the type level a term may always stay unrewritten, so 'arrowConstrainUbs' keeps the
        -- raw form alongside the arrows' dests, and a slot bounded only by a free declaration
        -- var (@fn(..) -> $_B@) has no consumer bound to drop it. The built arm body's tree,
        -- however, always applies the argument, so at dispatch the field settles to what the
        -- callee's arrows produce.
        reduceApplied = reduceAppliedType typeEnv vaenv
arrowDestTypeBase _ _ _ oa@ObjArr{oaArr=Nothing} = error $ printf "Unexpected call to arrowDestType with type obj (which has no arrow to find the dest type of): %s" (show oa)

-- | What a term settles to once applied: each leaf the type graph rewrites is replaced by what
-- its arrows produce; a leaf no arrow accepts (a data construction, a curried partial
-- application) is already a settled value and stays itself. Constant-carrying unions and
-- predicate tops are returned unchanged.
reduceAppliedType :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
-- A 'Just'-top component (a predicate region such as a dispatch meta's @∀Boolean@) already
-- denotes settled values and is carried through unchanged; only the POSITIVE leaves are raw
-- applied terms the graph may rewrite. Punting on the mixed shape would leave the raw self-leaf
-- in a consumer's destination and short-circuit its build to 'TCTId'.
reduceAppliedType typeEnv vaenv t@(UnionType mTop leafs consts) = case splitUnionType leafs of
  [] -> t
  ls -> unionAllTypesWithEnv typeEnv vaenv (UnionType mTop H.empty consts : map reduceLeaf ls)
  where
    reduceLeaf l = case typeGraphQueryWithReason typeEnv vaenv l of
      []    -> singletonType l
      dests -> unionAllTypesWithEnv typeEnv vaenv (map (\(_, _, dest) -> dest) dests)
reduceAppliedType _ _ t = t

-- | 'reduceAppliedType' iterated to a FIXPOINT: an arrow's destination may itself carry the
-- next call's raw form (@==@'s dest holds @equal(..)@ raw, whose dest holds the accessor's
-- raw form, …), and a consumer building toward a one-step reduction still short-circuits at
-- the second level. Each leaf NAME reduces at most once (visited set), so mutual recursion
-- terminates and a recursive function's own raw form stays put after its first step.
reduceAppliedTypeFix :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
reduceAppliedTypeFix typeEnv vaenv = go S.empty
  where
    go seen t@(UnionType mTop leafs consts) =
      let ls = splitUnionType leafs
          reducible l = not (S.member (ptName l) seen) && not (null (typeGraphQueryWithReason typeEnv vaenv l))
          todo = filter reducible ls
          keep = filter (not . reducible) ls
      in if null todo
           then t
           else
             let seen' = foldr (S.insert . ptName) seen todo
                 reduced = [ unionAllTypesWithEnv typeEnv vaenv [dest | (_, _, dest) <- typeGraphQueryWithReason typeEnv vaenv l] | l <- todo ]
             in go seen' $ unionAllTypesWithEnv typeEnv vaenv (UnionType mTop (joinUnionType keep) consts : reduced)
    go _ t = t

-- | 'reduceAppliedType', additionally settling the field and parameter positions of a leaf no
-- arrow accepts: a data construction storing a raw applied term (@Box[/v=/operator+(..)]@)
-- settles to what the term's arrows produce (@Box[/v=Integer]@). Every position settles one
-- step only — arrow results are not re-reduced — so the recursion follows the finite type
-- structure and always terminates. Note this cannot tell a reducible applied term from a
-- deferred value reference stored in a field (@/Context(/value=/main)@ would wrongly settle
-- to main's result), so callers must gate on context that distinguishes them (see the
-- sibling-coverage check in 'TreeBuild.buildCallTree').
reduceAppliedTypeDeep :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
reduceAppliedTypeDeep typeEnv vaenv t@(UnionType Nothing leafs []) = case splitUnionType leafs of
  [] -> t
  ls -> unionAllTypesWithEnv typeEnv vaenv (map reduceLeaf ls)
  where
    reduceLeaf l = case typeGraphQueryWithReason typeEnv vaenv l of
      []    -> singletonType (reduceLeafPositions l)
      dests -> unionAllTypesWithEnv typeEnv vaenv (map (\(_, _, dest) -> dest) dests)
    reduceLeafPositions p@PartialType{ptArgs, ptVars} = p
      { ptArgs = fmap (reduceAppliedTypeDeep typeEnv vaenv) ptArgs
      , ptVars = fmap (reduceAppliedTypeDeep typeEnv vaenv) ptVars }
reduceAppliedTypeDeep _ _ t = t

-- | Set resolved field values on a constructed type and bind the data parameters those fields
-- define. For @Cons(head=x)@ resolved to @head=Integer@, the Cons declaration links @head -> $T@,
-- so this both sets @/head=Integer@ and binds the element parameter @/T=Integer@. A field whose
-- declared type is not a bare parameter is set without binding.
applyResolvedFields :: (TypeGraph tg) => TypeEnv tg -> H.HashMap ArgName Type -> Type -> Type
applyResolvedFields _       fields t | H.null fields = t
applyResolvedFields typeEnv fields (UnionType Nothing leafs consts) =
  UnionType Nothing (joinUnionType (map applyLeaf (splitUnionType leafs))) consts
  where
    applyLeaf p = foldr applyField p (H.toList fields)
    applyField (fieldName, fieldType) p@PartialType{ptName, ptVars} =
      let declFieldType = case mapMaybe (H.lookup fieldName . ptArgs) (typeGraphData typeEnv ptName) of
            (t:_) -> Just t
            []    -> Nothing
      in case declFieldType of
           Just (TypeVar (TVVar v) TVInt) ->
             p{ ptVars = H.insertWith (intersectTypes typeEnv) v fieldType ptVars
              , ptArgs = H.insert fieldName fieldType (ptArgs p) }
           _ -> p{ptArgs = H.insert fieldName fieldType (ptArgs p)}
applyResolvedFields _ _ t = t

metaTypeVar :: Meta m -> Maybe TypeVarAux
metaTypeVar m = case getMetaType m of
  TypeVar v _ -> Just v
  _           -> Nothing

isSubtypeOfWithObjSrcs :: (TypeGraph tg, Show m, MetaDat m) => TypeEnv tg -> [(PartialType, ObjArr Expr m)] -> Type -> Type -> Bool
isSubtypeOfWithObjSrcs typeEnv objSrcs subType supType = any (\vaenv -> isSubtypeOfWithEnv typeEnv (fmap snd vaenv) subType supType) (exprVarArgsWithObjSrcs typeEnv objSrcs)
