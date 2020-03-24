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

data PSemiDecl = PSemiDecl PDeclLHS [PCompAnnot] (Maybe PExpr)
  deriving (Eq, Ord, Show)

splitDeclSubStatements :: [PDeclSubStatement] -> ([PDecl], [PCompAnnot])
splitDeclSubStatements = aux ([], [])
  where
    aux (decls, annots) [] = (decls, annots)
    aux (decls, annots) (RawDeclSubStatementDecl decl : subSt) = aux (decl:decls, annots) subSt
    aux (decls, annots) (RawDeclSubStatementAnnot annot : subSt) = aux (decls, annot:annots) subSt

scopeSubDeclFunNamesInExpr :: Name -> S.HashSet Name -> PExpr -> PExpr
scopeSubDeclFunNamesInExpr _ _ e@CExpr{} = e
scopeSubDeclFunNamesInExpr prefix replaceNames (Value m name) = Value m name'
  where
    addPrefix n = prefix ++ "." ++ n
    name' = if S.member name replaceNames then addPrefix name else name
scopeSubDeclFunNamesInExpr prefix replaceNames (TupleApply m (bm, bExpr) args) = TupleApply m (bm, bExpr') args'
  where
    bExpr' = scopeSubDeclFunNamesInExpr prefix replaceNames bExpr
    args' = fmap (scopeSubDeclFunNamesInExpr prefix replaceNames) args

-- Renames sub functions by applying the parent names as a prefix to avoid name collisions
scopeSubDeclFunNames :: Name -> [PSemiDecl] -> Maybe PExpr -> ([PSemiDecl], Maybe PExpr)
scopeSubDeclFunNames prefix decls maybeExpr = (decls', expr')
  where
    declNames = S.fromList $ map (\(PSemiDecl (DeclLHS _ name _) _ _) -> name) decls
    addPrefix n = prefix ++ "." ++ n
    decls' = map (\(PSemiDecl (DeclLHS m name args) annot subExpr) -> PSemiDecl (DeclLHS m (addPrefix name) args) annot (fmap (scopeSubDeclFunNamesInExpr prefix declNames) subExpr)) decls
    expr' = fmap (scopeSubDeclFunNamesInExpr prefix declNames) maybeExpr

currySubFunctionSignature :: H.HashMap Name ParseMeta -> CallGraph -> PSemiDecl -> (PSemiDecl, (Name, H.HashMap Name ParseMeta))
currySubFunctionSignature parentArgMap (graph, nodeFromVertex, vertexFromKey) (PSemiDecl (DeclLHS m name args) annot expr) = (PSemiDecl (DeclLHS m name args') annot expr, (name, curryArgs))
  where
    getContained n = S.fromList $ map ((\(_, calledName, _) -> calledName) . nodeFromVertex) $ reachable graph $ (\(Just n') -> n') $ vertexFromKey n
    contained = getContained name
    curryArgs = H.filterWithKey (\k _ -> S.member k contained) parentArgMap
    args' = H.union args curryArgs


buildCallGraph :: [PSemiDecl] -> CallGraph
buildCallGraph decls = graphFromEdges $ map fromDecl decls
  where
    fromDecl (PSemiDecl (DeclLHS _ name _) _ Nothing) = ((), name, [])
    fromDecl (PSemiDecl (DeclLHS _ name _) _ (Just expr)) = ((), name, S.toList $ tupleNamesInExpr expr)

currySubFunctions :: H.HashMap Name ParseMeta -> [PSemiDecl] -> Maybe PExpr -> ([PSemiDecl], Maybe PExpr)
currySubFunctions parentArgMap decls expr = (decls', expr')
  where
    callGraph = buildCallGraph decls
    (decls2, exprUpdateSource) = unzip $ map (currySubFunctionSignature parentArgMap callGraph) decls
    exprUpdateMap = H.fromList exprUpdateSource
    updateExpr c@CExpr{} = c
    updateExpr v@(Value _ vn) = case H.lookup vn exprUpdateMap of
      Just newArgs -> TupleApply emptyMeta (emptyMeta, v) (H.mapWithKey (flip Value) newArgs)
      Nothing -> v
    updateExpr (TupleApply tm (tbm, tbe) tArgs) = TupleApply tm (tbm, tbe') tArgs'
      where
        tbe' = updateExpr tbe
        tArgs' = fmap updateExpr tArgs
    expr' = fmap updateExpr expr
    decls' = map (\(PSemiDecl lhs an e) -> PSemiDecl lhs an (fmap updateExpr e)) decls2

removeSubDeclarations :: PDecl -> [PSemiDecl]
removeSubDeclarations (RawDecl (DeclLHS m declName args) subStatements expr) = decl':subDecls4
  where
    (subDecls, annots) = splitDeclSubStatements subStatements
    subDecls2 = concatMap removeSubDeclarations subDecls
    (subDecls3, expr2) = scopeSubDeclFunNames declName subDecls2 expr
    (subDecls4, expr3) = currySubFunctions args subDecls3 expr2
    decl' = PSemiDecl (DeclLHS m declName args) annots expr3

declToObjArrow :: PSemiDecl -> (PObject, [PArrow])
declToObjArrow (PSemiDecl (DeclLHS m name args) annots expr) = (object, [arrow])
  where
    object = Object m name args
    arrow = Arrow (PreTyped RawTopType) annots expr

desDecl :: PDecl -> PObjectMap
desDecl decl = H.fromList $ map declToObjArrow $ removeSubDeclarations decl

unionsWith :: (Ord k, Hashable k) => (a->a->a) -> [H.HashMap k a] -> H.HashMap k a
unionsWith f = foldl (H.unionWith f) H.empty

desDecls :: [PDecl] -> PObjectMap
desDecls decls = unionsWith (++) $ map desDecl decls

addTypeDef :: PRawTypeDef -> (PObjectMap, ClassMap) -> (PObjectMap, ClassMap)
addTypeDef (RawTypeDef name leafs) (objMap, classMap) = (objMap', classMap')
  where
    leafArgConvert leafType = PreTyped $ RawSumType (S.singleton leafType) H.empty
    leafToObj (RawLeafType leafName leafArgs) = Object (PreTyped $ RawSumType (S.singleton $ RawLeafType leafName leafArgs) H.empty ) leafName (fmap leafArgConvert leafArgs)
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
    addClass (cSealed, set1) (_, set2) = (cSealed, S.union set1 set2)

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
