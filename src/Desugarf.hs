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
import           Data.Bifunctor                 ( first )

import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           Parser.Syntax
import           Parser                   (parseFile)
import CallGraph

data PSemiDecl = PSemiDecl PDeclLHS [PCompAnnot] (Maybe PExpr)
  deriving (Eq, Ord, Show)

splitDeclSubStatements :: [PDeclSubStatement] -> ([PDecl], [PCompAnnot])
splitDeclSubStatements = aux ([], [])
  where
    aux (decls, annots) [] = (decls, annots)
    aux (decls, annots) (RawDeclSubStatementDecl decl : subSt) = aux (decl:decls, annots) subSt
    aux (decls, annots) (RawDeclSubStatementAnnot annot : subSt) = aux (decls, annot:annots) subSt

scopeSubDeclFunNamesInS :: Name -> S.HashSet Name -> Name -> Name
scopeSubDeclFunNamesInS prefix replaceNames name = name'
  where
    addPrefix n = prefix ++ "." ++ n
    name' = if S.member name replaceNames then addPrefix name else name

scopeSubDeclFunNamesInExpr :: Name -> S.HashSet Name -> PExpr -> PExpr
scopeSubDeclFunNamesInExpr _ _ e@RawCExpr{} = e
scopeSubDeclFunNamesInExpr prefix replaceNames (RawValue m name) = RawValue m $ scopeSubDeclFunNamesInS prefix replaceNames name
scopeSubDeclFunNamesInExpr prefix replaceNames (RawTupleApply m (bm, bExpr) args) = RawTupleApply m (bm, bExpr') args'
  where
    bExpr' = scopeSubDeclFunNamesInExpr prefix replaceNames bExpr
    args' = fmap (scopeSubDeclFunNamesInExpr prefix replaceNames) args

scopeSubDeclFunNamesInMeta :: Name -> S.HashSet Name -> ParseMeta -> ParseMeta
scopeSubDeclFunNamesInMeta prefix replaceNames (PreTyped (RawSumType leafs partials)) = PreTyped $ RawSumType leafs' partials'
  where
    scopeS = scopeSubDeclFunNamesInS prefix replaceNames
    leafs' = S.fromList $ map (\(RawLeafType n args) -> RawLeafType (scopeS n) args) $ S.toList leafs
    partials' = H.fromList $ map (first scopeS) $ H.toList partials
scopeSubDeclFunNamesInMeta _ _ m@(PreTyped RawTopType) = m

-- Renames sub functions by applying the parent names as a prefix to avoid name collisions
scopeSubDeclFunNames :: Name -> [PSemiDecl] -> Maybe PExpr -> [PCompAnnot] -> ParseMeta -> ParseMeta -> ([PSemiDecl], Maybe PExpr, [PCompAnnot], ParseMeta, ParseMeta)
scopeSubDeclFunNames prefix decls maybeExpr annots objM arrM = (decls', expr', annots', objM', arrM')
  where
    declNames = S.fromList $ map (\(PSemiDecl (DeclLHS _ _ name _ _) _ _) -> name) decls
    addPrefix n = prefix ++ "." ++ n
    scopeM = scopeSubDeclFunNamesInMeta prefix declNames
    objM' = scopeM objM
    arrM' = scopeM arrM
    decls' = map (\(PSemiDecl (DeclLHS oM aM name args guard) annot subExpr) -> PSemiDecl (DeclLHS (scopeM oM) (scopeM aM) (addPrefix name) args guard) annot (fmap (scopeSubDeclFunNamesInExpr prefix declNames) subExpr)) decls
    expr' = fmap (scopeSubDeclFunNamesInExpr prefix declNames) maybeExpr
    annots' = map (\(CompAnnot n ca) -> CompAnnot n (fmap (scopeSubDeclFunNamesInExpr prefix declNames) ca)) annots

currySubFunctionSignature :: H.HashMap Name ParseMeta -> CallGraph -> PSemiDecl -> (PSemiDecl, (Name, H.HashMap Name ParseMeta))
currySubFunctionSignature parentArgMap (graph, nodeFromVertex, vertexFromKey) (PSemiDecl (DeclLHS objM arrM name args guard) annot expr) = (PSemiDecl (DeclLHS objM arrM name args' guard) annot expr, (name, curryArgs))
  where
    getContained n = S.fromList $ map ((\(_, calledName, _) -> calledName) . nodeFromVertex) $ reachable graph $ (\(Just n') -> n') $ vertexFromKey n
    contained = getContained name
    curryArgs = H.filterWithKey (\k _ -> S.member k contained) parentArgMap
    args' = H.union args curryArgs


buildCallGraph :: [PSemiDecl] -> CallGraph
buildCallGraph decls = graphFromEdges $ map fromDecl decls
  where
    fromDecl (PSemiDecl (DeclLHS _ _ name _ _) _ Nothing) = ((), name, [])
    fromDecl (PSemiDecl (DeclLHS _ _ name _ _) _ (Just expr)) = ((), name, S.toList $ tupleNamesInExpr expr)

currySubFunctionsUpdateExpr :: H.HashMap Name (H.HashMap Name ParseMeta) -> PExpr -> PExpr
currySubFunctionsUpdateExpr _ c@RawCExpr{} = c
currySubFunctionsUpdateExpr exprUpdateMap v@(RawValue _ vn) = case H.lookup vn exprUpdateMap of
  Just newArgs | H.null newArgs -> v
  Just newArgs -> RawTupleApply emptyMeta (emptyMeta, v) (H.mapWithKey (flip RawValue) newArgs)
  Nothing -> v
currySubFunctionsUpdateExpr exprUpdateMap (RawTupleApply tm (tbm, tbe) tArgs) = RawTupleApply tm (tbm, tbe') tArgs'
  where
    tbe' = currySubFunctionsUpdateExpr exprUpdateMap tbe
    tArgs' = fmap (currySubFunctionsUpdateExpr exprUpdateMap) tArgs

currySubFunctions :: H.HashMap Name ParseMeta -> [PSemiDecl] -> Maybe PExpr -> [PCompAnnot] -> ([PSemiDecl], Maybe PExpr, [PCompAnnot])
currySubFunctions parentArgMap decls expr annots = (decls', expr', annots')
  where
    callGraph = buildCallGraph decls
    (decls2, exprUpdateSource) = unzip $ map (currySubFunctionSignature parentArgMap callGraph) decls
    exprUpdateMap = H.fromList exprUpdateSource
    expr' = fmap (currySubFunctionsUpdateExpr exprUpdateMap) expr
    decls' = map (\(PSemiDecl lhs an e) -> PSemiDecl lhs an (fmap (currySubFunctionsUpdateExpr exprUpdateMap) e)) decls2
    annots' = map (\(CompAnnot n ca) -> CompAnnot n (fmap (currySubFunctionsUpdateExpr exprUpdateMap) ca)) annots

removeSubDeclarations :: PDecl -> [PSemiDecl]
removeSubDeclarations (RawDecl (DeclLHS objM arrM declName args guard) subStatements expr) = decl':subDecls4
  where
    (subDecls, annots1) = splitDeclSubStatements subStatements
    subDecls2 = concatMap removeSubDeclarations subDecls
    (subDecls3, expr2, annots2, objM', arrM') = scopeSubDeclFunNames declName subDecls2 expr annots1 objM arrM
    (subDecls4, expr3, annots3) = currySubFunctions args subDecls3 expr2 annots2
    decl' = PSemiDecl (DeclLHS objM' arrM' declName args guard) annots3 expr3

desExpr :: PArgMetaMap -> PExpr -> DesExpr
desExpr _ (RawCExpr m c) = CExpr m c
desExpr arrArgs (RawValue m n) = if H.member n arrArgs
  then Arg m n
  else Value m n
desExpr arrArgs (RawTupleApply m (bm, be) args) = TupleApply m (bm, desExpr arrArgs be) (fmap (desExpr arrArgs) args)

desGuard :: PArgMetaMap -> PGuard -> DesGuard
desGuard arrArgs (IfGuard e) = IfGuard (desExpr arrArgs e)
desGuard _ ElseGuard = ElseGuard
desGuard _ NoGuard = NoGuard

desAnnot :: PArgMetaMap -> PCompAnnot -> DesCompAnnot
desAnnot arrArgs (CompAnnot name args) = CompAnnot name (fmap (desExpr arrArgs) args)

declToObjArrow :: PSemiDecl -> (PObject, [PArrow])
declToObjArrow (PSemiDecl (DeclLHS objM arrM name args guard) annots expr) = (object, [arrow])
  where
    object = Object objM name args
    arrow = Arrow arrM (map (desAnnot args) annots) (desGuard args guard) (fmap (desExpr args) expr)

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

desStatements :: [PStatement] -> DesPrgm
desStatements statements = (objMap, classMap)
  where
    splitStatements statement = case statement of
          RawDeclStatement decl -> ([decl], [], [])
          RawTypeDefStatement typedef -> ([], [typedef], [])
          RawClassDefStatement classdef -> ([], [], [classdef])
    (decls, types, classes) = (\(a, b, c) -> (concat a, concat b, concat c)) $ unzip3 $ map splitStatements statements
    declObjMap = desDecls decls
    (typeObjMap, sealedClasses) = desTypeDefs types
    unsealedClasses = desClassDefs False classes
    objMap = mergeObjMaps declObjMap typeObjMap
    classMap = mergeClassMaps sealedClasses unsealedClasses


desPrgm :: PPrgm -> IO (CRes DesPrgm)
desPrgm ([], statements) = return $ return $ desStatements statements
desPrgm (curImport:restImports, statements) = do
  f <- readFile curImport
  case parseFile f of
    CErr notes -> return $ CErr notes
    CRes notes (parsedImports, parsedStatements) -> do
      maybePrgm' <- desPrgm (parsedImports ++ restImports, parsedStatements ++ statements)
      case maybePrgm' of
        CErr notes2 -> return $ CErr (notes ++ notes2)
        CRes notes2 res -> return $ CRes (notes ++ notes2) res

desFiles :: [FileImport] -> IO (CRes DesPrgm)
desFiles imports = desPrgm (imports, [])
