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
    objMap' = objectMapFromList $ filter (notMatchesClassName . oaObjPath) $ flatObjectMap objMap

    classEntryMatches (CGType, PRelativeName n, _) = notMatchesClassName n
    classEntryMatches _                            = True


-- replaces uses of PRelativeName with PTypeName or PClassName when it can be determined
-- e.g. PRelativeName Boolean ==> PClassName /Data/Boolean
-- uses the mapMeta for objMap and annots, but must map the classGraph manually
-- the fullPrgmClassToTypes includes the imports and is used for when the def is inside an import
resolveRelativeNames :: DesPrgm -> DesPrgm -> DesPrgm
resolveRelativeNames fullPrgm@(Prgm fullPrgmObjMap _ _) (Prgm objMap (ClassGraph cg) annots) = mapMetaPrgm resolveMeta (Prgm objMap classGraph' annots)
  where
    classGraph' = ClassGraph $ graphFromEdges $ map mapClassEntry $ graphToNodes cg
    mapClassEntry (node, tp, subTypes) = (mapCGNode node, resolveName True tp, map (resolveName True) subTypes)
    mapCGNode (CGClass s clss ts doc) = CGClass s (resolveClassPartial True clss) (fmap (mapType True) ts) doc
    mapCGNode CGType = CGType
    classNames = nub $ listClassNames $ mkTypeEnv fullPrgm
    objNames = relAbsNamePrune $ nub $ map oaObjPath (concatMap (flatObjectMap . getRecursiveObjs) $ flatObjectMap fullPrgmObjMap)

    resolveMeta _ (Meta t p mid md) = return $ Meta (mapType False t) p mid md

    -- |
    -- requireResolveRelative -> type -> updated type
    -- It is required to resolve for the classGraph, but expressions can be left unresolved until type inference
    mapType :: Bool -> Type -> Type
    mapType reqResolve (TopType np (PredsOne (PredRel p))) | H.null np = resolveRelPartial reqResolve p
    mapType _ (TopType np ps) = TopType np ps
    mapType _ tp@(TypeVar TVVar{} _ _) = tp
    mapType _ (TypeVar TVArg{} _ _) = error "Invalid arg type"
    mapType reqResolve (TypeVar (TVApplied b ovr) loc i) = TypeVar (TVApplied b (fmap (mapType reqResolve) ovr)) loc (mapType reqResolve i)
    mapType reqResolve (UnionType Nothing partials []) = UnionType Nothing (joinUnionType $ map (mapPartial reqResolve) $ splitUnionType partials) []
    mapType _ t = t

    mapPartial :: Bool -> PartialType -> PartialType
    mapPartial reqResolve partial@PartialType{ptVars, ptArgs, ptPreds} = partial{
          ptVars = fmap (mapType reqResolve) ptVars,
          ptArgs = fmap (mapType reqResolve) ptArgs,
          ptPreds = mapTypePreds (mapTypePred (mapPartial reqResolve)) ptPreds
                                                                                                                      }

    -- | Resolves possibilities for a relative partial for use in the class graph
    resolveClassPartial :: Bool -> PartialType -> PartialType
    resolveClassPartial reqResolve p = case resolveRelPartial reqResolve p of
      (TopType np (PredsOne (PredClass p'))) | H.null np -> p'
      t | isJust (maybeGetSingleton t) -> getSingleton t
      p' -> error $ printf "Unexpected resolved class partial %s" (show p')

    -- | Resolves possibilities for a relative partial
    resolveRelPartial :: Bool -> PartialType -> Type
    resolveRelPartial reqResolve p0 = case resolveName reqResolve (PRelativeName $ ptName p) of
      PTypeName n'    -> singletonType p{ptName=n'}
      -- Carry the member's type parameters (e.g. the element binding @Functor[$T->$_T]@) onto the
      -- resolved class partial. Only 'ptVars' is restored — args/preds/argMode stay as a clean
      -- 'partialVal' so a var-less class member is byte-identical to before.
      PClassName n'   -> classPartial (partialVal n'){ptVars=ptVars p}
      PRelativeName{} -> TopType H.empty (PredsOne $ PredRel p)
      where p = mapPartial reqResolve p0


    -- |
    -- Attempts to convert 'PRelativeName' to either a type or a class
    -- If reqResolve is true, the conversions must succeed otherwise it can be left optional
    resolveName :: Bool -> PartialName -> PartialName
    resolveName reqResolve (PRelativeName name)= case (reqResolve, relativeNameFilter name classNames, relativeNameFilter name objNames) of
                  -- is a class, replace with class type
                  (_, [className], []) -> PClassName $ makeAbsoluteName className

                  -- is data, use data after recursively cleaning classes
                  (_, [], [typeName]) -> PTypeName $ makeAbsoluteName typeName

                  -- This case occurs when a class is used in a multiTypeDef
                  (_, [className], [typeName]) | className == typeName -> error $ printf "Found duplicate name %s" (show className)

                  -- Used to check (line below) to ensure types are always known, but may not be true for Arg Values
                  -- (_, [], []) -> error $ printf "There is no possible types or classes that correspond to name %s in type %s.\n\n\tType Options: %s\n\n\tClass Options: %s" name  (show partial) (show objNames) (show classNames)
                  -- (_, [], []) -> error $ printf "There is no possible types or classes that correspond to name %s\n\tAvailable types: %s" name (show objNames)

                  (False, _, _) -> PRelativeName name
                  (True, foundTypeNames, foundClassNames) -> error $ printf "Could not resolve required name: %s \n\t Found possible typeNames: %s \n\t Found possible classNames: %s" (nameStr name) (show foundTypeNames) (show foundClassNames)
    resolveName _ name = name


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
linkClassMethodReceivers :: DesPrgm -> DesPrgm -> DesPrgm
linkClassMethodReceivers (Prgm _ (ClassGraph fullCg) _) (Prgm (ObjectMap objMap) classGraph annots) = Prgm (ObjectMap objMap') classGraph annots
  where
    -- Map each ObjArr in place, preserving the decl/def categorization of the object map.
    objMap' = H.map enrichItem objMap
    enrichItem (ObjectMapItem ds rs rfs defs) =
      ObjectMapItem (map enrichObjArr ds) (map enrichObjArr rs) (map enrichObjArr rfs) (map enrichObjArr defs)

    enrichObjArr oa@ObjArr{oaObj=Just objExpr} = oa{oaObj=Just (enrichThis objExpr)}
    enrichObjArr oa                            = oa

    -- The @this@ receiver is applied last, so it is the outermost 'EAppArg'.
    enrichThis (TupleApply m (bm, be) (EAppArg oa@ObjArr{oaObj=Just oe, oaArr=Just (ae, arrM)}))
      | isThisArg oe = TupleApply m (bm, enrichThis be) (EAppArg oa{oaArr=Just (ae, mWithType (enrichReceiverType (getMetaType arrM)) arrM)})
    enrichThis (TupleApply m (bm, be) app) = TupleApply m (bm, enrichThis be) app
    enrichThis e = e

    isThisArg oe = let p = nameStr (exprPath oe) in p == thisKey || p == "this"

    -- The class's parameters are named on the receiver as PARENT references: they name the
    -- parameter the enclosing method partial carries, and this is where that is known lexically.
    -- Spelled 'TVExt' they name nothing the ambient environment binds — a method's constraint
    -- scope holds argument links, not type variables — and survived only because the class
    -- partial they sit on happens to declare a parameter of the same name, which the ambient
    -- scope rules read as in-scope ('dropOutOfScopeTVExt'). That coincidence is not the reason
    -- they are meaningful.
    enrichReceiverType (TopType negPartials (PredsOne (PredClass p@PartialType{ptName, ptVars})))
      | Just (CGClass _ PartialType{ptVars=classVarsDecl} _ _) <- graphLookup (PClassName ptName) fullCg
      , not (H.null classVarsDecl)
      = TopType negPartials (PredsOne (PredClass p{ptVars=H.union ptVars (H.mapWithKey (\k _ -> TypeVar (TVVar k) TVParent PTopType) classVarsDecl)}))
    enrichReceiverType t = t

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
-- Only a variable-free declared return is copied: a return typed by a declaration variable
-- (@-> $M@) would capture the definition's like-named variable rather than the declaration's.
-- A definition with its own authored return annotation is left untouched, and multiple
-- declarations contribute the union of their returns (the body's own arrow narrows it further).
seedDefResultTypes :: DesPrgm -> DesPrgm -> DesPrgm
seedDefResultTypes fullPrgm@(Prgm (ObjectMap fullObjMap) _ _) (Prgm (ObjectMap objMap) classGraph annots) =
    Prgm (ObjectMap objMap') classGraph annots
  where
    typeEnv = mkTypeEnv fullPrgm
    objMap' = H.mapWithKey seedItem objMap
    declReturn name = case [ t
                           | ObjectMapItem{omiDecls} <- maybeToList (H.lookup name fullObjMap)
                           , oa <- omiDecls
                           , Just (_, dm) <- [oaArr oa]
                           , let t = getMetaType dm
                           , t /= PTopType
                           , not (typeContainsTypeVar t) ] of
      [] -> Nothing
      ts -> Just $ unionAllTypes typeEnv ts
    seedItem name (ObjectMapItem ds rs rfs defs) = ObjectMapItem ds rs rfs (map (seedDef name) defs)
    seedDef name oa@ObjArr{oaArr=Just (e, m)}
      | getMetaType m == PTopType, Just t <- declReturn name = oa{oaArr=Just (e, mWithType t m)}
    seedDef _ oa = oa

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
reassignImportedMethodDefs (Prgm (ObjectMap fullObjMap) _ _) (Prgm (ObjectMap objMap) classGraph annots) =
    Prgm (ObjectMap objMap') classGraph annots
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
classifyParentVarRefs (Prgm (ObjectMap objMap) (ClassGraph classGraph) annots) =
  Prgm (ObjectMap objMap') (ClassGraph (fmapGraph classifyNode classGraph)) annots
  where
    objMap' = H.map classifyItem objMap
    classifyItem (ObjectMapItem ds rs rfs defs) =
      ObjectMapItem (map classifyOa ds) (map classifyOa rs) (map classifyOa rfs) (map classifyOa defs)
    classifyOa oa = mapMetaObjArr tagMeta Nothing oa
      where
        parentVars = S.fromList [v | v@TVVar{} <- H.keys $ maybe H.empty exprVarArgs (oaObj oa)]
        tagMeta ArrMeta m | not (S.null parentVars) = return $ mWithType (tagParentVarRefs parentVars (getMetaType m)) m
        tagMeta (ExprMeta InputMeta _) m = return $ mWithType (tagParentVarRefs S.empty (getMetaType m)) m
        tagMeta _ m = return m
    classifyNode (CGClass sealed p extends doc) =
      CGClass sealed (tagParentVarRefsPartial S.empty p) (map (tagParentVarRefs S.empty) extends) doc
    classifyNode CGType = CGType
