--------------------------------------------------------------------
-- |
-- Module    :  Desugarf
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Desugarf where


import           Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import Control.Applicative ((<$>))
import Data.Graph

import           Syntax
import           Parser.Syntax
import CallGraph

data SemiDecl m = SemiDecl (DeclLHS m) (Maybe (Expr m))
  deriving (Eq, Ord, Show)

setMeta :: m -> Expr m -> Expr m
setMeta m (CExpr _ c)   = CExpr m c
setMeta m (Tuple _ n es) = Tuple m n es

scopeSubDeclFunNamesInExpr :: Name -> S.HashSet Name -> Expr m -> Expr m
scopeSubDeclFunNamesInExpr _ _ e@CExpr{} = e
scopeSubDeclFunNamesInExpr prefix replaceNames (Tuple m name args) = Tuple m name' args'
  where
    addPrefix n = prefix ++ "." ++ n
    name' = if S.member name replaceNames then addPrefix name else name
    args' = fmap (scopeSubDeclFunNamesInExpr prefix replaceNames) args

-- Renames sub functions by applying the parent names as a prefix to avoid name collisions
scopeSubDeclFunNames :: Name -> [SemiDecl m] -> Maybe (Expr m) -> ([SemiDecl m], Maybe (Expr m))
scopeSubDeclFunNames prefix decls maybeExpr = (decls', expr')
  where
    declNames = S.fromList $ map (\(SemiDecl (DeclLHS _ name _) _) -> name) decls
    addPrefix n = prefix ++ "." ++ n
    decls' = map (\(SemiDecl (DeclLHS m name args) subExpr) -> SemiDecl (DeclLHS m (addPrefix name) args) (fmap (scopeSubDeclFunNamesInExpr prefix declNames) subExpr)) decls
    expr' = fmap (scopeSubDeclFunNamesInExpr prefix declNames) maybeExpr

currySubFunctionSignature :: H.HashMap Name m -> CallGraph -> SemiDecl m -> (SemiDecl m, (Name, H.HashMap Name m))
currySubFunctionSignature parentArgMap (graph, nodeFromVertex, vertexFromKey) (SemiDecl (DeclLHS m name args) expr) = (SemiDecl (DeclLHS m name args') expr, (name, curryArgs))
  where
    getContained n = S.fromList $ map ((\(_, calledName, _) -> calledName) . nodeFromVertex) $ reachable graph $ (\(Just n') -> n') $ vertexFromKey n
    contained = getContained name
    curryArgs = H.filterWithKey (\k _ -> S.member k contained) parentArgMap
    args' = H.union args curryArgs


buildCallGraph :: [SemiDecl m] -> CallGraph
buildCallGraph decls = graphFromEdges $ map fromDecl decls
  where
    fromDecl (SemiDecl (DeclLHS _ name _) Nothing) = ((), name, [])
    fromDecl (SemiDecl (DeclLHS _ name _) (Just expr)) = ((), name, S.toList $ tupleNamesInExpr expr)

currySubFunctions :: H.HashMap Name m -> [SemiDecl m] -> Maybe (Expr m) -> ([SemiDecl m], Maybe (Expr m))
currySubFunctions parentArgMap decls expr = (decls', expr')
  where
    callGraph = buildCallGraph decls
    (decls2, exprUpdateSource) = unzip $ map (currySubFunctionSignature parentArgMap callGraph) decls
    exprUpdateMap = H.fromList exprUpdateSource
    updateExpr c@CExpr{} = c
    updateExpr (Tuple tm tn te) = Tuple tm tn (H.union (fmap updateExpr te) (H.mapWithKey (\argName argM -> Tuple argM argName H.empty) $ H.lookupDefault H.empty tn exprUpdateMap))
    expr' = fmap updateExpr expr
    decls' = map (\(SemiDecl lhs e) -> SemiDecl lhs (fmap updateExpr e)) decls2

removeSubDeclarations :: RawDecl m -> [SemiDecl m]
removeSubDeclarations (RawDecl (DeclLHS m declName args) subDecls expr) = decl':subDecls4
  where
    subDecls2 = concatMap removeSubDeclarations subDecls
    (subDecls3, expr2) = scopeSubDeclFunNames declName subDecls2 expr
    (subDecls4, expr3) = currySubFunctions args subDecls3 expr2
    decl' = SemiDecl (DeclLHS m declName args) expr3

declToObjArrow :: SemiDecl m -> (Object m, [Arrow m])
declToObjArrow (SemiDecl (DeclLHS m name args) expr) = (object, [arrow])
  where
    object = Object m name args
    arrow = Arrow m expr

desDecl :: (Eq m, Hashable m) => RawDecl m -> ObjectMap m
desDecl decl = H.fromList $ map declToObjArrow $ removeSubDeclarations decl

unionsWith :: (Ord k, Hashable k) => (a->a->a) -> [H.HashMap k a] -> H.HashMap k a
unionsWith f = foldl (H.unionWith f) H.empty

desDecls :: (Eq m, Ord m, Hashable m) => [RawDecl m] -> ObjectMap m
desDecls decls = unionsWith (++) $ map desDecl decls

addTypeDef :: PRawTypeDef -> (PObjectMap, ClassMap) -> (PObjectMap, ClassMap)
addTypeDef (RawTypeDef name leafs) (objMap, classMap) = (objMap', classMap')
  where
    leafArgConvert leafType = PreTyped $ RawSumType $ S.singleton leafType
    leafToObj (RawLeafType leafName leafArgs) = Object (PreTyped $ RawSumType $ S.singleton $ RawLeafType leafName leafArgs) leafName (fmap leafArgConvert leafArgs)
    newObjs = map leafToObj $ S.toList leafs
    additionalObjMap = H.fromList $ map (,[]) newObjs
    objMap' = mergeObjMaps objMap additionalObjMap
    leafNames = (\(RawLeafType leafName _) -> leafName) <$> S.toList leafs
    additionalClassMap = desClassDefs True $ map (,name) leafNames
    classMap' = mergeClassMaps additionalClassMap classMap

desTypeDefs :: [PRawTypeDef] -> (PObjectMap, ClassMap)
desTypeDefs = foldr addTypeDef empty
  where empty = (H.empty, (H.empty, H.empty))

desClassDefs :: Sealed -> [RawClassDef] -> ClassMap
desClassDefs sealed = foldr addDef empty
  where
    empty = (H.empty, H.empty)
    addDef (typeName, className) (typeToClass, classToType) = (H.insertWith S.union typeName (S.singleton className) typeToClass, H.insertWith addClass className (sealed, S.singleton typeName) classToType)
    addClass (sealed, set1) (_, set2) = (sealed, S.union set1 set2)

mergeObjMaps :: PObjectMap -> PObjectMap -> PObjectMap
mergeObjMaps = H.unionWith (++)

mergeClassMaps :: ClassMap -> ClassMap -> ClassMap
mergeClassMaps (toClassA, toTypeA) (toClassB, toTypeB) = (H.unionWith S.union toClassA toClassB, H.unionWith mergeClasses toTypeA toTypeB)
  where mergeClasses (sealedA, setA) (sealedB, setB) = if sealedA == sealedB
          then (sealedA, S.union setA setB)
          else error "Added to sealed class definition"

desPrgm :: PPrgm -> DesPrgm
desPrgm prgm = (objMap, classMap)
  where
    splitStatements statement = case statement of
          RawDeclStatement decl -> ([decl], [], [])
          RawTypeDefStatement typedef -> ([], [typedef], [])
          RawClassDefStatement classdef -> ([], [], [classdef])
    (decls, types, classes) = (\(a, b, c) -> (concat a, concat b, concat c)) $ unzip3 $ map splitStatements prgm
    declObjMap = desDecls decls
    (typeObjMap, sealedClasses) = desTypeDefs types
    unsealedClasses = desClassDefs False classes
    objMap = mergeObjMaps declObjMap typeObjMap
    classMap = mergeClassMaps sealedClasses unsealedClasses
