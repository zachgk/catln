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
import           Utils                   (graphLookup, graphToNodes)

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
    mapCGNode (CGClass (s, clss, ts, doc)) = CGClass (s, resolveClassPartial True clss, fmap (mapType True) ts, doc)
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
    mapType _ tp@(TypeVar TVVar{} _) = tp
    mapType _ (TypeVar TVArg{} _) = error "Invalid arg type"
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
-- @this@ receiver is a parameterized class, it fills the receiver's 'ptVars' with an identity
-- binding for every declared class parameter (@∀TEqv@ becomes @TEqv[T=$T]@). A method argument
-- that names @$T@ then shares the receiver's parameter variable, so an instance that binds it
-- concretely (@MyEqv isa TEqv[$T= Integer]@) flows that binding into the method's arguments.
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

    enrichReceiverType (TopType negPartials (PredsOne (PredClass p@PartialType{ptName, ptVars})))
      | Just (CGClass (_, PartialType{ptVars=classVarsDecl}, _, _)) <- graphLookup (PClassName ptName) fullCg
      , not (H.null classVarsDecl)
      = TopType negPartials (PredsOne (PredClass p{ptVars=H.union ptVars (H.mapWithKey (\k _ -> TypeVar (TVVar k) TVInt) classVarsDecl)}))
    enrichReceiverType t = t

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
