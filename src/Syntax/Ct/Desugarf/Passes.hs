--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Desugarf.Passes
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module contains various passes run over the program after
-- it has finished desugaring. They apply various corrections to
-- the program left over from the parsing.
--------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module Syntax.Ct.Desugarf.Passes where

import           Control.Monad.Writer    (execWriter, tell)
import           CRes
import           CtConstants             (thisKey)
import           Data.Graph
import qualified Data.HashMap.Strict     as H
import qualified Data.HashSet            as S
import           Data.List               (nub)
import           Data.Maybe
import           MapMeta
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Parser.Syntax
import           Text.Printf
import           Utils                   (fmapGraph, graphLookup, graphToNodes)

-- Removes objects that match classes
-- When a class is used in the RHS of a multiTypeDef, it will create an object temporarily
-- This removes that temporary object and the CGType classGraph entry for it
removeClassInstanceObjects :: DesPrgm -> DesPrgm -> DesPrgm
removeClassInstanceObjects fullPrgm (Prgm objMap (ClassGraph cg) annots) = Prgm objMap' classGraph' annots
  where
    classNames = listClassNames $ mkTypeEnv fullPrgm
    notMatchesClassName n = null $ relativeNameFilter n classNames

    classGraph' = ClassGraph $ graphFromEdges $ filter classEntryMatches $ graphToNodes cg
    -- Only the items are objects matching a class: the declarations written from a set of objects
    -- ('omDecls') are kept as they are.
    objMap' = (objectMapFromList $ filter (notMatchesClassName . oaObjPath) $ flatObjectMap objMap){omDecls = omDecls objMap}

    classEntryMatches (CGType, PRelativeName n, _) = notMatchesClassName n
    classEntryMatches _                            = True


-- | A name the class graph needed resolved to exactly one type or class, and could not be: the
-- name, and the classes and the types it could have been.
data NameFailure = NameFailure Name [Name] [Name]

-- replaces uses of PRelativeName with PTypeName or PClassName when it can be determined
-- e.g. PRelativeName Boolean ==> PClassName /Data/Boolean
-- uses the mapMeta for objMap and annots, but must map the classGraph manually
-- the fullPrgmClassToTypes includes the imports and is used for when the def is inside an import
--
-- A name in the class graph — a member of a class, or a class something is declared a member of —
-- must resolve to exactly one type or class, and one that resolves to none or to several is
-- reported. One in an expression may be left relative for type inference to settle, so resolving
-- it cannot fail.
resolveRelativeNames :: DesPrgm -> DesPrgm -> CRes DesPrgm
resolveRelativeNames fullPrgm@(Prgm fullPrgmObjMap _ _) (Prgm objMap (ClassGraph cg) annots) = do
  entries <- mapM mapClassEntry $ graphToNodes cg
  return $ mapMetaPrgm resolveMeta (Prgm objMap (ClassGraph $ graphFromEdges entries) annots)
  where
    mapClassEntry (node, tp, subTypes) = do
      let within = entryContext node tp
      node' <- mapCGNode within node
      tp' <- required within $ resolveName True tp
      subTypes' <- mapM (required within . resolveName True) subTypes
      return (node', tp', subTypes')
    mapCGNode within (CGClass s clss ts doc) = do
      clss' <- required within $ resolveClassPartial True clss
      ts' <- mapM (required within . mapType True) ts
      return $ CGClass s clss' ts' doc
    mapCGNode _ CGType = return CGType
    classNames = nub $ listClassNames $ mkTypeEnv fullPrgm
    objNames = relAbsNamePrune $ nub $ map oaObjPath (concatMap (flatObjectMap . getRecursiveObjs) $ flatObjectMap fullPrgmObjMap)

    resolveMeta _ (Meta t p mid md) = return $ Meta (resolvedFreely $ mapType False t) p mid md

    -- | A type resolved without requiring it, which leaves a name it cannot resolve relative rather
    -- than failing.
    resolvedFreely :: Either NameFailure a -> a
    resolvedFreely (Right a) = a
    resolvedFreely (Left (NameFailure name _ _)) = error $ printf "The name %s failed to resolve where it was not required to" (nameStr name)

    -- | What a class graph entry declares, for saying where one of its names was written.
    entryContext :: CGNode -> PartialName -> String
    entryContext CGClass{} cls = printf "the members of the class %s" (partialNameStr cls)
    entryContext CGType tp     = printf "the classes %s is declared a member of" (partialNameStr tp)

    partialNameStr :: PartialName -> String
    partialNameStr (PRelativeName n) = nameStr n
    partialNameStr (PTypeName n)     = nameStr n
    partialNameStr (PClassName n)    = nameStr n

    -- | A name the class graph needed, reported where it failed to resolve. It is reported at the
    -- declaration on this page of one of the names it could have been, where there is one: a class
    -- declares its members beside it, so a member whose name is also another type's is reported at
    -- the class which wrote it.
    required :: String -> Either NameFailure a -> CRes a
    required _ (Right a) = return a
    required within (Left (NameFailure name foundClasses foundTypes)) = CErr [MkCNote $ GenCErr pos msg]
      where
        candidates = map makeAbsoluteName (foundClasses ++ foundTypes)
        pos = listToMaybe [p | oa <- flatObjectMap objMap, oaObjPath oa `elem` candidates, Just e <- [oaObj oa], let p = getMetaPos (getExprMeta e), isJust p] >>= id
        msg = case candidates of
          [] -> printf "The name %s in %s is not the name of any type or class in scope." (nameStr name) within
          (c:_) -> printf "The name %s in %s is ambiguous: it could be %s. Write the one meant in full, such as %s." (nameStr name) within (showOptions candidates) (nameStr c)
        showOptions [a]    = nameStr a
        showOptions [a, b] = nameStr a ++ " or " ++ nameStr b
        showOptions as     = concatMap ((++ ", ") . nameStr) (init as) ++ "or " ++ nameStr (last as)

    -- |
    -- requireResolveRelative -> type -> updated type
    -- It is required to resolve for the classGraph, but expressions can be left unresolved until type inference
    mapType :: Bool -> Type -> Either NameFailure Type
    mapType reqResolve (TopType np (PredsOne (PredRel p))) | H.null np = resolveRelPartial reqResolve p
    -- A written intersection (@Functor[$T -> Eq] && Eq@, 'Syntax.Ct.Desugarf.Expr.intersectWrittenTypes'):
    -- each named part resolves as it would alone, and the parts are joined again.
    mapType reqResolve (TopType np (PredsAnd ps)) | H.null np, not (null ps), Just rels <- mapM relAtom ps = foldr1 (intersectResolved reqResolve) <$> mapM (resolveRelPartial reqResolve) rels
      where
        relAtom (PredsOne (PredRel p)) = Just p
        relAtom _                      = Nothing
    mapType _ (TopType np ps) = return $ TopType np ps
    mapType _ tp@(TypeVar TVVar{} _ _) = return tp
    mapType _ (TypeVar TVArg{} _ _) = error "Invalid arg type"
    mapType reqResolve (TypeVar (TVApplied b ovr) loc i) = do
      ovr' <- mapM (mapType reqResolve) ovr
      i' <- mapType reqResolve i
      return $ TypeVar (TVApplied b ovr') loc i'
    mapType reqResolve (UnionType Nothing partials []) = do
      partials' <- mapM (mapPartial reqResolve) $ splitUnionType partials
      return $ UnionType Nothing (joinUnionType partials') []
    mapType _ t = return t

    mapPartial :: Bool -> PartialType -> Either NameFailure PartialType
    mapPartial reqResolve partial@PartialType{ptVars, ptArgs, ptPreds} = do
      ptVars' <- mapM (mapType reqResolve) ptVars
      ptArgs' <- mapM (mapType reqResolve) ptArgs
      ptPreds' <- mapTypePredsM (mapTypePredM (mapPartial reqResolve)) ptPreds
      return partial{ptVars = ptVars', ptArgs = ptArgs', ptPreds = ptPreds'}

    -- | Two resolved parts of an intersection as one type: classes (and still-relative names) are
    -- predicates on ⊤ and join as predicates; a data type keeps its leaf, filtered by the other
    -- part's predicates. Two data types are not an intersection anyone writes on purpose.
    intersectResolved :: Bool -> Type -> Type -> Type
    intersectResolved _ (TopType n1 p1) (TopType n2 p2) | H.null n1, H.null n2 = TopType H.empty (predsAnd p1 p2)
    intersectResolved _ (TopType n1 p1) t | H.null n1, Just l <- maybeGetSingleton t = singletonType l{ptPreds = predsAnd (ptPreds l) p1}
    intersectResolved r t top@TopType{} = intersectResolved r top t
    intersectResolved _ a b = error $ printf "An intersection (&&) can join at most one data type, not %s and %s" (show a) (show b)

    -- | Resolves possibilities for a relative partial for use in the class graph
    resolveClassPartial :: Bool -> PartialType -> Either NameFailure PartialType
    resolveClassPartial reqResolve p = do
      resolved <- resolveRelPartial reqResolve p
      return $ case resolved of
        (TopType np (PredsOne (PredClass p'))) | H.null np -> p'
        t | isJust (maybeGetSingleton t) -> getSingleton t
        p' -> error $ printf "Unexpected resolved class partial %s" (show p')

    -- | Resolves possibilities for a relative partial
    resolveRelPartial :: Bool -> PartialType -> Either NameFailure Type
    resolveRelPartial reqResolve p0 = do
      p <- mapPartial reqResolve p0
      resolved <- resolveName reqResolve (PRelativeName $ ptName p)
      return $ case resolved of
        PTypeName n'    -> singletonType p{ptName=n'}
        -- Carry the member's type parameters (e.g. the element binding @Functor[$T->$_T]@) onto the
        -- resolved class partial. Only 'ptVars' is restored — args/preds/argMode stay as a clean
        -- 'partialVal' so a var-less class member is byte-identical to before.
        PClassName n'   -> classPartial (partialVal n'){ptVars=ptVars p}
        PRelativeName{} -> TopType H.empty (PredsOne $ PredRel p)


    -- |
    -- Attempts to convert 'PRelativeName' to either a type or a class
    -- If reqResolve is true, the conversions must succeed otherwise it can be left optional
    resolveName :: Bool -> PartialName -> Either NameFailure PartialName
    resolveName reqResolve (PRelativeName name)= case (reqResolve, relativeNameFilter name classNames, relativeNameFilter name objNames) of
                  -- is a class, replace with class type
                  (_, [className], []) -> return $ PClassName $ makeAbsoluteName className

                  -- is data, use data after recursively cleaning classes
                  (_, [], [typeName]) -> return $ PTypeName $ makeAbsoluteName typeName

                  -- This case occurs when a class is used in a multiTypeDef
                  (_, [className], [typeName]) | className == typeName -> error $ printf "Found duplicate name %s" (show className)

                  (False, _, _) -> return $ PRelativeName name
                  (True, foundClassNames, foundTypeNames) -> Left $ NameFailure name foundClassNames foundTypeNames
    resolveName _ name = return name


-- |
-- Exposes a class's declared type parameters on the receivers of its methods.
--
-- A method declared with a bare class receiver, e.g. @:TEqv.rel(a -> $T, b -> $T)@ over
-- @class(TEqv[$T])@, desugars to a receiver typed as the bare class @∀TEqv@ (a class
-- predicate with no type parameters). The method's @$T@ is then disconnected from the class
-- parameter and floats to 'TopType'. This pass restores the link: for each method whose
-- @this@ receiver is a parameterized class, it fills the receiver's 'ptVars' with a PARENT
-- link for every declared class parameter (@∀TEqv@ becomes @TEqv[T=$_T]@ — the receiver
-- partial's parameter names the method declaration's own @$T@, the scope surrounding the
-- receiver spelling). A method argument that names @$T@ then shares the receiver's parameter
-- variable, so an instance that binds it concretely (@MyEqv isa TEqv[$T= Integer]@) flows
-- that binding into the method's arguments.
--
-- Parameters already present on the receiver (e.g. the explicit @:Result[$R, $E].default@ form)
-- are left untouched, and receivers that are not parameterized classes are unchanged.
--
-- Only a parameter the method itself names is linked: the link exists so the method's own @$T@
-- shares the receiver's parameter, and a method with no @$T@ (@:List.length -> Integer@) has no
-- variable for it to name. Linked anyway it was a reference to a binder that does not exist —
-- @length(this=∀List[/T=$_/T])@ — which a caller frame binding its own @/T@ captures by name, and
-- which the algebra cannot read (propDifferenceByIntersection, 6415: Seed 6239727124165917557
-- 9825796995401062037). Such a receiver keeps the bare class, whose parameter is unconstrained.
linkClassMethodReceivers :: DesPrgm -> DesPrgm -> DesPrgm
linkClassMethodReceivers (Prgm _ (ClassGraph fullCg) _) (Prgm om@(ObjectMap objMap) classGraph annots) = Prgm om{omItems = objMap'} classGraph annots
  where
    -- Map each ObjArr in place, preserving the decl/def categorization of the object map.
    objMap' = H.map enrichItem objMap
    enrichItem (ObjectMapItem ds rs rfs defs) =
      ObjectMapItem (map enrichObjArr ds) (map enrichObjArr rs) (map enrichObjArr rfs) (map enrichObjArr defs)

    enrichObjArr oa@ObjArr{oaObj=Just objExpr} = oa{oaObj=Just (enrichThis (namedByMethod oa objExpr) objExpr)}
    enrichObjArr oa                            = oa

    -- Whether the method names a variable: as one of the object's own variables, or in any of
    -- its metas (argument annotations, result, body).
    namedByMethod oa objExpr k = H.member (TVVar k) (exprVarArgs objExpr) || any (typeMentionsVar (TVVar k)) metaTypes
      where metaTypes = execWriter (mapMetaObjArrM (\_ m -> tell [getMetaType m] >> return m) Nothing oa)

    -- The @this@ receiver is applied last, so it is the outermost 'EAppArg'.
    enrichThis named (TupleApply m (bm, be) (EAppArg oa@ObjArr{oaObj=Just oe, oaArr=Just (ae, arrM)}))
      | isThisArg oe = TupleApply m (bm, enrichThis named be) (EAppArg oa{oaArr=Just (ae, mWithType (enrichReceiverType named (getMetaType arrM)) arrM)})
    enrichThis named (TupleApply m (bm, be) app) = TupleApply m (bm, enrichThis named be) app
    enrichThis _ e = e

    isThisArg oe = let p = nameStr (exprPath oe) in p == thisKey || p == "this"

    enrichReceiverType named (TopType negPartials (PredsOne (PredClass p@PartialType{ptName, ptVars})))
      | Just (CGClass _ PartialType{ptVars=classVarsDecl} _ _) <- graphLookup (PClassName ptName) fullCg
      , let linked = H.filterWithKey (\k _ -> named k) classVarsDecl
      , not (H.null linked)
      = TopType negPartials (PredsOne (PredClass p{ptVars=H.union ptVars (H.mapWithKey (\k _ -> TypeVar (TVVar k) TVExt PTopType) linked)}))
    enrichReceiverType _ t = t

-- |
-- Seeds an annotation-less definition's result meta with its declaration's return type.
--
-- A definition written under a separate declaration (@mkA(v -> Integer) -> BoxA@ /
-- @mkA(v)= ...@) carries a 'PTopType' result meta, so during constraint solving its body's
-- destination stays unresolved — unlike an inline-annotated definition, whose result meta is
-- the authored type. This pass copies the declared return type onto each of the item's
-- definitions (matching by absolute name across the import-merged program), making the two
-- authoring forms equivalent. In particular the body's calls then see a resolved destination,
-- which return-type-directed dispatch ('outVarBoundSrc') needs to bind a callee's output
-- variable (e.g. picking a @pure@-style instance).
--
-- A declaration variable cannot be copied as written: a return typed by one (@-> $M@) would
-- capture the definition's like-named variable rather than the declaration's. So its external
-- references are read as the ⊤ they denote outside the declaration ('freeTVExtToTopDeep'), and
-- what is copied is the return's variable-free upper bound — @-> Maybe[$T -> $_MV]@ seeds
-- @∀Maybe[/T=⊤]@. Every definition's result is within its declaration's, so this bound holds for
-- each of them. It is what keeps a generic definition's result a VALUE: left at ⊤, the result of
-- an arm whose body is a recursive call is inferred as that raw call, so the body's tree leaves
-- the call unevaluated and the rewrite that should finish it re-enters the arm and is dropped as
-- cyclical (a guarded generic @get@ over a map then found no arm for its own recursive call). A
-- return that is nothing but a variable (@-> $_B@) bounds nothing and is not copied.
-- A definition with its own authored return annotation is left untouched, and multiple
-- declarations contribute the union of their returns (the body's own arrow narrows it further).
seedDefResultTypes :: DesPrgm -> DesPrgm -> DesPrgm
seedDefResultTypes fullPrgm@(Prgm (ObjectMap fullObjMap) _ _) (Prgm om@(ObjectMap objMap) classGraph annots) =
    Prgm om{omItems = objMap'} classGraph annots
  where
    typeEnv = mkTypeEnv fullPrgm
    objMap' = H.mapWithKey seedItem objMap
    declReturn name = case [ t
                           | ObjectMapItem{omiDecls} <- maybeToList (H.lookup name fullObjMap)
                           , oa <- omiDecls
                           , Just (_, dm) <- [oaArr oa]
                           , let t = freeTVExtToTopDeep H.empty (getMetaType dm)
                           , t /= PTopType
                           , not (typeContainsTypeVar t) ] of
      [] -> Nothing
      ts -> Just $ unionAllTypes typeEnv ts
    seedItem name (ObjectMapItem ds rs rfs defs) = ObjectMapItem ds rs rfs (map (seedDef name) defs)
    seedDef name oa@ObjArr{oaArr=Just (e, m)}
      | getMetaType m == PTopType, Just t <- declReturn name = oa{oaArr=Just (e, mWithType t m)}
    seedDef _ oa = oa

-- | Types a sub function's captured parent arguments as the enclosing argument is typed.
--
-- A sub function closes over the parent arguments it reads, and each is curried into its signature
-- as a parameter carrying the parent's written meta ('Nested.curryApplyParentArgsSignature'). Where
-- the parent definition leaves the argument unannotated — its declaration states it, as for
-- @:Integer.toStringBase(base -> Integer)@ beside @:Integer.toStringBase(base) else = ..@ — that
-- meta is ⊤, so a sub declaration such as @digitsOnto@ stated it accepts any @base@. What a
-- definition accepts follows from its body too, and an arm needing @n < base@ then fails to fulfil
-- a declaration accepting a ⊤ @base@. The captured argument is the enclosing argument passed
-- through, so it takes the enclosing argument's type: the one the parent's declarations give it
-- (variable-free, as 'seedDefResultTypes' takes a result). A captured parameter is recognised by
-- carrying the parent argument's own meta, so its position; the parent definition itself is left
-- as written.
--
-- A sub function is solved with its parent and its definitions are bounded by what its callers
-- pass ('TypeCheck.Encode.nestedCallerConstraints'), but that narrows the DEFINITIONS only. The
-- captured parameter in the sub function's DECLARATION is the one desugar wrote, and the coverage
-- check holds the definitions to it: without this pass core fails with "Unfulfilled Declaration
-- for /Data/toStringBase.digitsOnto" (declared @base=⊤@, defined @base=Integer@).
seedCapturedArgTypes :: DesPrgm -> DesPrgm -> DesPrgm
seedCapturedArgTypes fullPrgm@(Prgm (ObjectMap fullObjMap) _ _) (Prgm om@(ObjectMap objMap) classGraph annots) =
    Prgm om{omItems = objMap'} classGraph annots
  where
    typeEnv = mkTypeEnv fullPrgm
    declArg name k = case [ t
                          | ObjectMapItem{omiDecls} <- maybeToList (H.lookup name fullObjMap)
                          , oa <- omiDecls
                          , Just obj <- [oaObj oa]
                          , Just ms <- [H.lookup k (exprArgs obj)]
                          , (_, am) <- ms
                          , let t = freeTVExtToTopDeep H.empty (getMetaType am)
                          , t /= PTopType
                          , not (typeContainsTypeVar t) ] of
      [] -> Nothing
      ts -> Just $ unionAllTypes typeEnv ts
    -- A field bound by destructuring (@day@ in @Date(year, month, day)@, or @d@ in @Date(day@d)@)
    -- is named by no declaration of the parent: it is the field of the data the pattern is written
    -- in, so it takes that field's declared type, by the same rule.
    dataField ctor field = case [ t
                                | (dataName, ObjectMapItem{omiDecls}) <- H.toList fullObjMap
                                , relativeNameMatches ctor dataName
                                , oa <- omiDecls
                                , isNothing (oaArr oa)
                                , Just obj <- [oaObj oa]
                                , (k, ms) <- H.toList (exprArgs obj)
                                , relativeNameMatches (pkName k) (makeAbsoluteName field) || pkName k == field
                                , (_, am) <- ms
                                , let t = freeTVExtToTopDeep H.empty (getMetaType am)
                                , t /= PTopType
                                , not (typeContainsTypeVar t) ] of
      [] -> Nothing
      ts -> Just $ unionAllTypes typeEnv ts
    -- Each binding a pattern destructures from a data value, with the data's name and the field.
    fieldBindings e@(TupleApply _ (_, b) arg) = fieldBindings b ++ case (arg, maybeExprPath e) of
      (EAppArg ObjArr{oaObj=Just f, oaArr=Just (Nothing, _)}, Just ctor) -> case f of
        Value _ field                   -> [(field, (ctor, field))]
        AliasExpr (Value _ field) alias | Just a <- maybeExprPath alias -> [(a, (ctor, field))]
        _                               -> []
      (EAppArg ObjArr{oaArr=Just (Just v, _)}, _) -> fieldBindings v
      _ -> []
    fieldBindings (AliasExpr b _) = fieldBindings b
    fieldBindings _ = []
    capturedArg name obj k = case declArg name k of
      Just t  -> Just t
      Nothing -> listToMaybe [ t | (bound, (ctor, field)) <- fieldBindings obj, relativeNameMatches (pkName k) (makeAbsoluteName bound) || pkName k == bound, Just t <- [dataField ctor field] ]
    seeds = H.fromList
      [ (pos, (name, t))
      | (name, ObjectMapItem{omiDefs}) <- H.toList objMap
      , d <- omiDefs
      , Just obj <- [oaObj d]
      , (k, ms) <- H.toList (exprArgs obj)
      , (_, am) <- ms
      , Just pos <- [getMetaPos am]
      , getMetaType am == PTopType
      , Just t <- [capturedArg name obj k] ]
    objMap'
      | H.null seeds = objMap
      | otherwise = H.mapWithKey seedItem objMap
    seedItem name (ObjectMapItem ds rs rfs defs) = ObjectMapItem (map (seedOa name) ds) rs rfs (map (seedOa name) defs)
    seedOa name oa@ObjArr{oaObj=Just obj} = oa{oaObj=Just (seedExpr name obj)}
    seedOa _ oa                           = oa
    seedExpr name (TupleApply m (bm, b) arg) = TupleApply m (bm, seedExpr name b) $ case arg of
      EAppArg a@ObjArr{oaBasis=ArgObj, oaArr=Just (Nothing, am)}
        | getMetaType am == PTopType
        , Just pos <- getMetaPos am
        , Just (owner, t) <- H.lookup pos seeds
        , owner /= name -> EAppArg a{oaArr=Just (Nothing, mWithType t am)}
      a -> a
    seedExpr name (AliasExpr b a) = AliasExpr (seedExpr name b) a
    seedExpr name (EWhere m b c) = EWhere m (seedExpr name b) c
    seedExpr _ e = e

-- |
-- Redirects a receiver-method definition written in a module other than the one that declares the
-- method, onto the declaration it implements.
--
-- An instance like @:Integer.eq= ...@ placed in @Data/Primitive@ (where @eq@ is declared in the
-- imported @Data/Algebra@) has its selector prefixed with the /defining/ module during desugar,
-- producing the orphan name @/Data/Primitive/eq@ — a definition with no declaration anywhere. It
-- therefore fails to fulfil the real @/Data/Algebra/eq@ accessor. For each such orphan whose simple
-- name matches a unique declared method elsewhere in the (import-merged) program, this rewrites the
-- selector to that declaration, so the instance lands on the accessor it implements.
--
-- Only receiver methods (objects carrying a @this@ argument) are redirected. Bare functions and
-- operators defined in a foreign module (e.g. @mempty -> Integer@, @_ ++ _@) keep their existing
-- names, since they do not require per-receiver coverage and are dispatched by other means.
reassignImportedMethodDefs :: DesPrgm -> DesPrgm -> DesPrgm
reassignImportedMethodDefs (Prgm (ObjectMap fullObjMap) _ _) (Prgm om@(ObjectMap objMap) classGraph annots) =
    Prgm om{omItems = objMap'} classGraph annots
  where
    -- Names that carry a declaration (or runtime declaration) somewhere in the full program.
    declaredNames = [k | (k, ObjectMapItem ds rs _ _) <- H.toList fullObjMap, not (null ds) || not (null rs)]
    declaredSet = S.fromList declaredNames
    simpleName = reverse . takeWhile (/= '/') . reverse
    -- The unique declared method of the same simple name, when there is exactly one.
    redirectTarget orphan = case filter ((== simpleName (nameStr orphan)) . simpleName . nameStr) declaredNames of
      [t] -> Just t
      _   -> Nothing
    hasThisArg oa = any (maybe False ((`elem` [thisKey, "this"]) . nameStr . exprPath) . oaObj) (exprAppliedArgs (oaObjExpr oa))

    objMap' = H.fromListWith (<>)
      [ (key', renameItem key key' item)
      | (key, item@(ObjectMapItem ds rs rfs defs)) <- H.toList objMap
      , let key' | S.member key declaredSet                 = key
                 | any hasThisArg (ds ++ rs ++ rfs ++ defs)
                 , Just target <- redirectTarget key        = target
                 | otherwise                                = key
      ]

    renameItem from to item@(ObjectMapItem ds rs rfs defs)
      | from == to = item
      | otherwise  = ObjectMapItem (rn ds) (rn rs) (rn rfs) (rn defs)
      where rn = map (renameOA from to)
    renameOA from to oa = oa{oaObj = fmap (mapExprPath (\(eM, eN) -> Value eM (if eN == from then to else eN))) (oaObj oa)}

-- |
-- Names each type-variable reference that names the partial it is written one level inside.
--
-- A reference may name a variable of the partial ENCLOSING the one it is written on — the
-- @$_/V@ in @peq[$V -> Eqq](x -> Box[$T -> $_V])@, once that argument sits in @peq@'s own slot —
-- or a variable of the ambient object scope. Both are spelled @$_T@ in source, so only the
-- lexical nesting can tell them apart, and that is why the decision is made HERE. Everything
-- downstream relies on it and must never re-derive it: doing so deep in the type algebra would
-- capture a like-named variable of whatever scope the type had been carried into.
--
-- A reference is a parent reference exactly when the partial IMMEDIATELY enclosing the one it is
-- written on binds its name ('tagParentVarRefs' states the one-level rule and why no deeper
-- reference has a spelling); anything else keeps the 'TVExt' the parser gave it and resolves
-- through the ambient environment.
--
-- An ANNOTATION meta ('ArrMeta') holds a FRAGMENT of the object's type — @∀List[/T=$_/T]@ on its
-- own, with the @sorted[$T]@ that encloses it left implicit — so the enclosing partial has to be
-- supplied rather than read off the fragment. That is what the parent set is seeded with: the
-- object's own variable application, which is the partial the fragment sits in once the object's
-- type is assembled. Reading the fragment's nesting alone instead spells the SAME reference
-- 'TVExt' in the annotation and 'TVParent' in the object's composed type, and one declaration
-- spelled two ways is two denotationally-equal forms that compare unequal: list.ct's @sorted@
-- stops being fulfilled by its own definition.
--
-- The object's COMPOSED input metas ('ExprMeta' 'InputMeta' — the tuple-apply types 'desObj'
-- pre-computes) DO materialize the object as their outermost partial, so they are seeded EMPTY
-- and the nesting decides everything. Left unrewritten, a pre-computed
-- @Cons[..](tail=∀ConsList[/T=$_/T])@ reaches the object universe spelled ambient and the scope
-- rules read the reference as foreign, widening the field bound to ⊤ for every consumer the
-- universe bounds ('BoundedByObjs').
--
-- A variable's BOUND (@[$M -> Monad[$T -> $_A]]@) is left alone, and structurally so: it is not
-- an 'ArrMeta'. It belongs to the scope CONTAINING the binder rather than inside it and travels
-- without it, so naming it a parent reference strands it — @∀Monad[/T=$^/A]@ reached TreeBuild as
-- a destination with no parent in view and nothing could reach it.
--
-- The CLASS GRAPH is rewritten with the object map. Its declaration and member templates
-- describe the same types the object map holds (@/Cons@'s @tail -> ∀ConsList[/T=$_/T]@), and a
-- declaration spelled one way in one and another way in the other is two denotationally-equal
-- forms that compare unequal — the hazard that makes accumulations flip between spellings across
-- epochs ('dropOutOfScopeTVExt' documents it), and the reason the ambient scope rules had to keep
-- rescuing an untagged reference by name.
classifyParentVarRefs :: DesPrgm -> DesPrgm
classifyParentVarRefs (Prgm om@(ObjectMap objMap) (ClassGraph classGraph) annots) =
  Prgm om{omItems = objMap'} (ClassGraph (fmapGraph classifyNode classGraph)) annots
  where
    objMap' = H.map classifyItem objMap
    classifyItem (ObjectMapItem ds rs rfs defs) =
      ObjectMapItem (map classifyOa ds) (map classifyOa rs) (map classifyOa rfs) (map classifyOa defs)
    classifyOa oa = retagDirectArgs $ mapMetaObjArr tagMeta Nothing oa
      where
        parentVars = S.fromList [v | v@TVVar{} <- H.keys $ maybe H.empty exprVarArgs (oaObj oa)]
        tagMeta ArrMeta m | not (S.null parentVars) = return $ mWithType (tagParentVarRefs parentVars (getMetaType m)) m
        tagMeta (ExprMeta InputMeta _) m = return $ mWithType (tagParentVarRefs S.empty (getMetaType m)) m
        tagMeta _ m = return m
        -- The object's own DIRECT argument annotations: a bare outer spelling naming one of the
        -- object's parameters (@Just[$T](val -> $_T)@) is the object's own parameter, spelled
        -- 'TVInt' exactly as 'tagParentVarRefs' spells the same slot in the composed input type
        -- and the class-graph template. A function-typed argument's annotation
        -- (@fmap[$F, $A, $B](fn(v -> $_A) -> $_B)@) is NOT a slot of the object: that argument's
        -- slot holds its input object @fn(/v=$_A)@, and its result is the arrow it adds to the
        -- body's environment. Seen from that arrow the declared @$_B@ is the enclosing object's
        -- variable — the outer reference it is written as — so it is left untagged. The nested
        -- argument's own annotation (@v -> $_A@, one level inside @fn@) and the object's result
        -- annotation are not direct slots of the object and are left to the nesting rule.
        retagDirectArgs oa'@ObjArr{oaObj=Just obj} | not (S.null parentVars) = oa'{oaObj=Just (onSpine obj)}
        retagDirectArgs oa' = oa'
        onSpine (TupleApply m (bm, be) (EAppArg a@ObjArr{oaObj=Just aObj}))
          | not (null (exprAppliedArgs aObj)) = TupleApply m (bm, onSpine be) (EAppArg a)
        onSpine (TupleApply m (bm, be) (EAppArg a@ObjArr{oaArr=Just (ae, am)})) =
          TupleApply m (bm, onSpine be) (EAppArg a{oaArr=Just (ae, retagDirect am)})
        onSpine (TupleApply m (bm, be) app) = TupleApply m (bm, onSpine be) app
        onSpine e = e
        retagDirect m = case getMetaType m of
          TypeVar v@TVVar{} TVExt i | S.member v parentVars -> mWithType (TypeVar v TVInt i) m
          _                                                -> m
    classifyNode (CGClass sealed p extends doc) =
      CGClass sealed (tagParentVarRefsPartial S.empty p) (map (tagParentVarRefs S.empty) extends) doc
    classifyNode CGType = CGType
