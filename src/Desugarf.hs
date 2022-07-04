--------------------------------------------------------------------
-- |
-- Module    :  Desugarf
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module desugars a 'RawPrgm' into a 'Prgm' by removing all
-- language features outside of a minimalist core set.
--------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}
module Desugarf where


import           Data.Bifunctor      (first)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Hashable
import           Data.Maybe
import           Text.Printf

import           CRes
import           Data.Graph          hiding (path)
import           Data.List
import           Desugarf.Passes
import           Parser.Syntax
import           Syntax
import           Syntax.Prgm
import           Syntax.Types
import           Utils

type StatementEnv = (String, [DesCompAnnot])

splitDeclSubStatements :: [PDeclSubStatement] -> ([PDecl], [PCompAnnot])
splitDeclSubStatements = aux ([], [])
  where
    aux (decls, annots) [] = (decls, annots)
    aux (decls, annots) (RawDeclSubStatementDecl decl : subSt) = aux (decl:decls, annots) subSt
    aux (decls, annots) (RawDeclSubStatementAnnot annot []: subSt) = aux (decls, annot:annots) subSt
    aux _ (RawDeclSubStatementAnnot _ (_:_):_) = error "Children in RawDeclSubStatementAnnot not currently supported"

scopeSubDeclFunNamesInS :: TypeName -> S.HashSet TypeName -> TypeName -> TypeName
scopeSubDeclFunNamesInS prefix replaceNames name = name'
  where
    addPrefix n = prefix ++ "." ++ n
    name' = if S.member name replaceNames then addPrefix name else name

scopeSubDeclFunNamesInPartialName :: TypeName -> S.HashSet TypeName -> PartialName -> PartialName
scopeSubDeclFunNamesInPartialName prefix replaceNames (PTypeName name) = PTypeName $ scopeSubDeclFunNamesInS prefix replaceNames name
scopeSubDeclFunNamesInPartialName prefix replaceNames (PClassName name) = PClassName $ scopeSubDeclFunNamesInS prefix replaceNames name
scopeSubDeclFunNamesInPartialName prefix replaceNames (PRelativeName name) = PRelativeName $ scopeSubDeclFunNamesInS prefix replaceNames name

scopeSubDeclFunNamesInExpr :: TypeName -> S.HashSet TypeName -> PSExpr -> PSExpr
scopeSubDeclFunNamesInExpr _ _ e@PSCExpr{} = e
scopeSubDeclFunNamesInExpr prefix replaceNames (PSValue m name) = PSValue m $ scopeSubDeclFunNamesInS prefix replaceNames name
scopeSubDeclFunNamesInExpr prefix replaceNames (PSTupleApply m (bm, bExpr) argName argVal) = PSTupleApply m (bm, bExpr') argName argVal'
  where
    bExpr' = scopeSubDeclFunNamesInExpr prefix replaceNames bExpr
    argVal' = scopeSubDeclFunNamesInExpr prefix replaceNames argVal

scopeSubDeclFunNamesInMeta :: TypeName -> S.HashSet TypeName -> ParseMeta -> ParseMeta
scopeSubDeclFunNamesInMeta prefix replaceNames (PreTyped (UnionType partials) pos) = PreTyped (UnionType partials') pos
  where
    scopeS = scopeSubDeclFunNamesInPartialName prefix replaceNames
    partials' = H.fromList $ map (first scopeS) $ H.toList partials
scopeSubDeclFunNamesInMeta _ _ m@(PreTyped TopType _) = m
scopeSubDeclFunNamesInMeta _ _ m@(PreTyped TypeVar{} _) = m

-- Renames sub functions by applying the parent names as a prefix to avoid name collisions
scopeSubDeclFunNames :: TypeName -> [PSemiDecl] -> Maybe PSExpr -> [PSCompAnnot] -> ParseMeta -> ParseMeta -> ([PSemiDecl], Maybe PSExpr, [PSCompAnnot], ParseMeta, ParseMeta)
scopeSubDeclFunNames prefix decls maybeExpr annots objM arrM = (decls', expr', annots', objM', arrM')
  where
    declNames = S.fromList $ map (\(PSemiDecl (DeclLHS _ (Pattern Object{objPath} _)) _ _) -> objPath) decls
    addPrefix n = prefix ++ "." ++ n
    scopeM = scopeSubDeclFunNamesInMeta prefix declNames
    objM' = scopeM objM
    arrM' = scopeM arrM
    decls' = map (\(PSemiDecl (DeclLHS aM (Pattern obj@Object{objM=om, objPath} guard)) annot subExpr) -> PSemiDecl (DeclLHS (scopeM aM) (Pattern (obj{objM=scopeM om, objPath = addPrefix objPath}) guard)) annot (fmap (scopeSubDeclFunNamesInExpr prefix declNames) subExpr)) decls
    expr' = fmap (scopeSubDeclFunNamesInExpr prefix declNames) maybeExpr
    annots' = map (scopeSubDeclFunNamesInExpr prefix declNames) annots

currySubFunctionSignature :: H.HashMap ArgName PObjArg -> PSemiDecl -> PSemiDecl
currySubFunctionSignature parentArgs (PSemiDecl (DeclLHS arrM (Pattern obj@Object{objArgs} guard)) annot expr) = PSemiDecl (DeclLHS arrM (Pattern obj{objArgs=objArgs'} guard)) annot expr
  where objArgs' = H.union objArgs parentArgs


currySubFunctionsUpdateExpr :: S.HashSet TypeName -> H.HashMap ArgName PObjArg -> PSExpr -> PSExpr
currySubFunctionsUpdateExpr _ _ c@PSCExpr{} = c
currySubFunctionsUpdateExpr _ parentArgs v@PSValue{} | H.null parentArgs = v
currySubFunctionsUpdateExpr toUpdate parentArgs v@(PSValue _ vn) = if S.member vn toUpdate
  then foldr (\(parentArgName, (parentArgM, _)) e -> PSTupleApply emptyMetaN (emptyMetaN, e) (Just parentArgName) (PSValue parentArgM parentArgName)) v $ H.toList parentArgs
  else v
currySubFunctionsUpdateExpr toUpdate parentArgs (PSTupleApply tm (tbm, tbe) tArgName tArgVal) = PSTupleApply tm (tbm, tbe') tArgName tArgVal'
  where
    tbe' = currySubFunctionsUpdateExpr toUpdate parentArgs tbe
    tArgVal' = currySubFunctionsUpdateExpr toUpdate parentArgs tArgVal

currySubFunctions :: H.HashMap ArgName PObjArg -> [PSemiDecl] -> Maybe PSExpr -> [PSCompAnnot] -> ([PSemiDecl], Maybe PSExpr, [PSCompAnnot])
currySubFunctions parentArgs decls expr annots = (decls', expr', annots')
  where
    toUpdate = S.fromList $ map (\(PSemiDecl (DeclLHS _ (Pattern Object{objPath} _)) _ _) -> objPath) decls
    decls2 = map (currySubFunctionSignature parentArgs) decls
    expr' = fmap (currySubFunctionsUpdateExpr toUpdate parentArgs) expr
    decls' = map (\(PSemiDecl lhs an e) -> PSemiDecl lhs an (fmap (currySubFunctionsUpdateExpr toUpdate parentArgs) e)) decls2
    annots' = map (currySubFunctionsUpdateExpr toUpdate parentArgs) annots

desObjDocComment :: [PDeclSubStatement] -> Maybe String
desObjDocComment (RawDeclSubStatementAnnot (RawTupleApply _ (_, RawValue _ "/Catln/#md") [RawTupleArgNamed "text" (RawCExpr _ (CStr doc))]) _: rest) = Just (++) <*> Just doc <*> desObjDocComment rest
desObjDocComment _ = Just ""

removeSubDeclarations :: PDecl -> [PSemiDecl]
removeSubDeclarations (RawDecl (DeclLHS arrM (Pattern obj@Object{objM, objArgs, objPath} guard1)) subStatements expr1) = decl':subDecls5
  where
    objDoc = desObjDocComment subStatements
    (subDecls, annots1) = splitDeclSubStatements subStatements
    subDecls2 = concatMap removeSubDeclarations subDecls
    (subDecls21, expr2) = traverse (semiDesExpr obj) expr1
    (subDecls22, annots2) = traverse (semiDesExpr obj) annots1
    (subDecls23, guard2) = semiDesGuard obj guard1
    subDecls3 = concat [subDecls2, subDecls21, subDecls22, subDecls23]
    (subDecls4, expr3, annots3, objM', arrM') = scopeSubDeclFunNames objPath subDecls3 expr2 annots2 objM arrM
    (subDecls5, expr4, annots4) = currySubFunctions objArgs subDecls4 expr3 annots3
    decl' = PSemiDecl (DeclLHS arrM' (Pattern obj{objDoc = objDoc, objM=objM'} guard2)) annots4 expr4

desExpr :: PArgMetaMap -> PSExpr -> DesExpr
desExpr _ (PSCExpr m c) = ICExpr m c
desExpr arrArgs (PSValue m n) = if H.member n arrArgs
  then IArg m n
  else IValue m n
desExpr arrArgs (PSTupleApply m (bm, be) argName argVal) = ITupleApply m (bm, desExpr arrArgs be) argName (desExpr arrArgs argVal)

desGuard :: PArgMetaMap -> PSGuard -> DesGuard
desGuard arrArgs = fmap (desExpr arrArgs)

semiDesExpr :: PObject -> PExpr -> ([PSemiDecl], PSExpr)
semiDesExpr _ (RawCExpr m c) = ([], PSCExpr m c)
semiDesExpr _ (RawValue m n) = ([], PSValue m n)
semiDesExpr obj (RawTupleApply m'' (bm, be) args) = (\(a, _, PSTupleApply _ (bm'', be'') argName'' argVal'') -> (a, PSTupleApply m'' (bm'', be'') argName'' argVal'')) $ foldl aux (subBe, bm, be') args
  where
    (subBe, be') = semiDesExpr obj be
    aux (sub, m, e) (RawTupleArgNamed argName argVal) = (subArgVal ++ sub, emptyMetaM "res" m'', PSTupleApply (emptyMetaM "app" m'') (m, e) (Just argName) argVal')
      where (subArgVal, argVal') = semiDesExpr obj argVal
    aux (sub, m, e) (RawTupleArgInfer argVal) = (subArgVal ++ sub, emptyMetaM "res" m'', PSTupleApply (emptyMetaM "app" m'') (m, e) Nothing argVal')
      where (subArgVal, argVal') = semiDesExpr obj argVal
semiDesExpr obj (RawParen e) = semiDesExpr obj e
semiDesExpr obj (RawMethods base methods) = semiDesExpr obj $ foldl addMethod base methods
  where
    addMethod b method@RawValue{} = RawTupleApply (emptyMetaE "app" b) (emptyMetaE "appBase" method, method) [RawTupleArgNamed "this" b]
    addMethod b (RawTupleApply m methodVal methodArgs) = RawTupleApply m methodVal (RawTupleArgNamed "this" b : methodArgs)
    addMethod _ _ = error "Unknown semiDesExpr obj method"
semiDesExpr obj@Object{objVars} r@(RawIfThenElse m i t e) = (concat [subI, subT, subE, [elseDecl, ifDecl]], expr')
  where
    condName = "$" ++ take 6 (printf "%08x" (hash r))
    (subI, i') = semiDesExpr obj i
    (subT, t') = semiDesExpr obj t
    (subE, e') = semiDesExpr obj e
    ifDecl = PSemiDecl (DeclLHS (emptyMetaE "obj" i) (Pattern (Object (emptyMetaE "patt" i) FunctionObj objVars H.empty Nothing condName) (IfGuard i'))) [] (Just t')
    elseDecl = PSemiDecl (DeclLHS (emptyMetaE "obj" e) (Pattern (Object (emptyMetaE "patt" e) FunctionObj objVars H.empty Nothing condName) ElseGuard)) [] (Just e')
    expr' = PSValue m condName
semiDesExpr obj@Object{objVars} r@(RawMatch m e matchItems) = (subE ++ subMatchItems, expr')
  where
    condName = "$" ++ take 6 (printf "%08x" (hash r))
    argName = condName ++ "-arg"
    (subE, e') = semiDesExpr obj e
    expr' = PSTupleApply m (emptyMetaM "app" m, PSValue (emptyMetaM "val" m) condName) (Just argName) e'
    subMatchItems = concatMap semiDesMatchItem matchItems
    semiDesMatchItem (Pattern patt pattGuard, matchExpr) = concat [[matchItemExpr'], subPattGuard, subMatchExpr]
      where
        (subPattGuard, pattGuard') = semiDesGuard obj pattGuard
        (subMatchExpr, matchExpr') = semiDesExpr obj matchExpr
        matchArg = H.singleton argName (emptyMetaM "arg" m, Just patt)
        matchItemExpr' = PSemiDecl (DeclLHS (emptyMetaM "obj" m) (Pattern (Object (emptyMetaM "patt" m) FunctionObj objVars matchArg Nothing condName) pattGuard')) [] (Just matchExpr')
semiDesExpr _ (RawCase _ _ ((Pattern _ ElseGuard, _):_)) = error "Can't use elseguard in match expr"
semiDesExpr _ (RawCase _ _ []) = error "Empty case"
semiDesExpr obj (RawCase _ _ [(_, matchExpr)]) = semiDesExpr obj matchExpr
semiDesExpr obj@Object{objVars} r@(RawCase m e ((Pattern firstObj@Object{objM=fm} firstGuard, firstExpr):restCases)) = (concat [[firstDecl, restDecl], subFG, subFE, subRE, subE], expr')
  where
    condName = "$" ++ take 6 (printf "%08x" (hash r))
    argName = condName ++ "-arg"
    declObj = Object (emptyMetaM "obj" m) FunctionObj objVars (H.singleton argName (fm, Just firstObj)) Nothing condName
    firstDecl = PSemiDecl (DeclLHS m (Pattern declObj firstGuard')) [] (Just firstExpr')
    (subFG, firstGuard') = semiDesGuard obj firstGuard
    (subFE, firstExpr') = semiDesExpr obj firstExpr
    restDecl = PSemiDecl (DeclLHS m (Pattern declObj ElseGuard)) [] (Just restExpr')
    (subRE, restExpr') = semiDesExpr obj (RawCase m e restCases)
    (subE, e') = semiDesExpr obj e
    expr' = PSTupleApply m (emptyMetaM "app" m, PSValue (emptyMetaE "val" e) condName) (Just argName) e'
semiDesExpr obj (RawList m []) = semiDesExpr obj (RawValue m "/Data/Nil")
semiDesExpr obj (RawList m (l:ls)) = semiDesExpr obj (RawTupleApply (emptyMetaM "listApp" (getExprMeta l)) (emptyMetaM "listBase" (getExprMeta l), RawValue m "/Data/Cons") [RawTupleArgNamed "head" l, RawTupleArgNamed "tail" (RawList m ls)])


semiDesGuard :: PObject -> PGuard -> ([PSemiDecl], PSGuard)
semiDesGuard obj (IfGuard e) = (subE, IfGuard e')
  where (subE, e') = semiDesExpr obj e
semiDesGuard _ ElseGuard = ([], ElseGuard)
semiDesGuard _ NoGuard = ([], NoGuard)

declToObjArrow :: StatementEnv -> PSemiDecl -> DesObjectMapItem
declToObjArrow (inheritPath, inheritAnnots) (PSemiDecl (DeclLHS arrM (Pattern object@Object{objPath} guard)) annots expr) = (object'', annots' ++ inheritAnnots, Just arrow)
  where
    -- Inherit the path in main object name. If main is a context, also inherit in the context function as well
    updateObjPath o@Object{objPath=originalPath} = o{objPath = getPath inheritPath originalPath}
    object' = updateObjPath object
    object'' = case objPath of
      "/Context" -> object{objArgs=H.adjust (\(m, Just o) -> (m, Just $ updateObjPath o)) "value" $ objArgs object}
      _ -> object'

    argMetaMap = formArgMetaMap object'
    annots' = map (desExpr argMetaMap) annots
    arrow = Arrow arrM (desGuard argMetaMap guard) (fmap (desExpr argMetaMap) expr)

desDecl :: StatementEnv -> PDecl -> DesPrgm
desDecl statementEnv decl = (objMap, emptyClassGraph, [])
  where
    objMap = map (declToObjArrow statementEnv) $ removeSubDeclarations decl

getPath :: String -> String -> String
getPath inheritPath name = if "/" `isPrefixOf` name then
  name
  else inheritPath ++ "/" ++ name

-- todo check
typeDefMetaToObj :: String -> H.HashMap TypeVarName Type -> ParseMeta -> Maybe PObject
typeDefMetaToObj _ _ (PreTyped TypeVar{} _) = Nothing
typeDefMetaToObj inheritPath varReplaceMap (PreTyped (UnionType partials) pos) = case splitUnionType partials of
  [partial@(PartialType (PRelativeName partialName) partialVars _ partialArgs _)] -> Just $ Object m' TypeObj (fmap toMeta partialVars') (fmap (\arg -> (PreTyped arg Nothing, Nothing)) partialArgs) Nothing (getPath inheritPath partialName)
    where
      ptName' = PTypeName $ getPath inheritPath partialName
      partialVars' = fmap (substituteVarsWithVarEnv varReplaceMap) partialVars
      m' = PreTyped (singletonType partial{ptVars=partialVars', ptName=ptName'}) $ labelPos "obj" pos
      toMeta t = PreTyped t Nothing
  _ -> error "Invalid call to typeDefMetaToObj with UnionType"
typeDefMetaToObj _ _ _ = error "Invalid call to typeDefMetaToObj"

desMultiTypeDef :: StatementEnv -> PMultiTypeDef -> [RawDeclSubStatement ParseMeta] -> Path -> DesPrgm
desMultiTypeDef (inheritPath, _) (MultiTypeDef className classVars dataMetas) subStatements path = (objMap', classGraph', [])
    where
      path' =  case path of
        Absolute p -> p
        Relative p -> inheritPath ++ "/" ++ p
      objMap' = map (, [], Nothing) objs
      objPaths = map objPath objs
      dataTypes = map getMetaType dataMetas
      objs = mapMaybe (typeDefMetaToObj inheritPath classVars) dataMetas
      typeCGNodes = map (CGType, , []) objPaths
      classCGNode = (CGClass (True, classVars, dataTypes, desObjDocComment subStatements, path'), getPath inheritPath className, objPaths)
      classGraph' = ClassGraph $ graphFromEdges (classCGNode:typeCGNodes)


desClassDecl :: StatementEnv -> RawClassDecl -> [RawDeclSubStatement ParseMeta] -> Path -> DesPrgm
desClassDecl (inheritPath, _) (className, classVars) subStatements path = ([], classGraph', [])
  where
    classGraph' = ClassGraph $ graphFromEdges [(CGClass (False, classVars, [], desObjDocComment subStatements, path'), getPath inheritPath className, [])]
    path' = case path of
      Relative p -> inheritPath ++ "/" ++ p
      Absolute p -> p

desTypeDef :: StatementEnv -> PTypeDef -> [RawDeclSubStatement ParseMeta] -> DesPrgm
desTypeDef (inheritPath, _) (TypeDef tp) subStatements = (objMap, emptyClassGraph, [])
  where
    objMap = case typeDefMetaToObj inheritPath H.empty tp of
          Just obj -> [(obj{objDoc = desObjDocComment subStatements}, [], Nothing)]
          Nothing  -> error "Type def could not be converted into meta"

desClassDef :: StatementEnv -> Sealed -> RawClassDef -> [RawDeclSubStatement ParseMeta] -> Path -> DesPrgm
desClassDef (inheritPath, _) sealed ((typeName, typeVars), className) subStatements path = ([], classGraph, [])
  where
    path' =  case path of
      Absolute p -> p
      Relative p -> inheritPath ++ "/" ++ p
    classGraph = ClassGraph $ graphFromEdges [(CGClass (sealed, H.empty, [singletonType (PartialType (PRelativeName typeName) typeVars H.empty H.empty PtArgExact)], desObjDocComment subStatements, path'), className, [typeName])]

emptyClassGraph :: ClassGraph
emptyClassGraph = ClassGraph $ graphFromEdges []

mergeObjMaps :: DesObjectMap -> DesObjectMap -> DesObjectMap
mergeObjMaps = (++)

desGlobalAnnot :: PCompAnnot -> DesCompAnnot
desGlobalAnnot p = case semiDesExpr undefined p of
  ([], d) -> desExpr H.empty d
  _ -> error "Global annotations do not support sub-expressions and lambdas"

desStatement :: StatementEnv -> PStatement -> DesPrgm
desStatement statementEnv (RawDeclStatement decl) = desDecl statementEnv decl
desStatement statementEnv (MultiTypeDefStatement multiTypeDef subStatements path) = desMultiTypeDef statementEnv multiTypeDef subStatements path
desStatement statementEnv (TypeDefStatement typeDef subStatements) = desTypeDef  statementEnv typeDef subStatements
desStatement statementEnv (RawClassDefStatement classDef subStatements path) = desClassDef statementEnv False classDef subStatements path
desStatement statementEnv (RawClassDeclStatement classDecls subStatements path) = desClassDecl statementEnv classDecls subStatements path
desStatement _ (RawGlobalAnnot a []) = ([], emptyClassGraph, [desGlobalAnnot a])
desStatement (inheritModule, inheritAnnots) (RawGlobalAnnot a subStatements) = mergePrgms $ map (desStatement (inheritModule, desGlobalAnnot a:inheritAnnots)) subStatements
desStatement (_, inheritAnnots) (RawModule _ subStatements (Absolute path)) = mergePrgms $ map (desStatement (path, inheritAnnots)) subStatements
desStatement (inheritModule, inheritAnnots) (RawModule _ subStatements (Relative path)) = mergePrgms $ map (desStatement (inheritModule ++ "/" ++ path, inheritAnnots)) subStatements

finalPasses :: DesPrgmGraphData -> GraphNodes DesPrgm String -> GraphNodes DesPrgm String
finalPasses (desPrgmGraph, nodeFromVertex, vertexFromKey) (prgm, prgmName, imports) = (prgm'', prgmName, imports)
  where
    -- Build fullPrgm with recursive imports
    vertex = fromJust $ vertexFromKey prgmName
    importTree = reachable desPrgmGraph vertex
    fullPrgm = mergePrgms $ map (fst3 . nodeFromVertex) importTree

    -- Run resolveRelativeNames pass
    prgm' = resolveRelativeNames fullPrgm prgm
    fullPrgm' = resolveRelativeNames fullPrgm fullPrgm

    -- Run expandDataReferences pass
    prgm'' = expandDataReferences fullPrgm' prgm'

desPrgm :: PPrgm -> CRes DesPrgm
desPrgm (_, statements) = return $ mergePrgms $ map (desStatement ("", [])) statements

desFiles :: PPrgmGraphData -> CRes DesPrgmGraphData
desFiles graphData = do
  -- initial desugar
  prgms' <- mapMFst3 desPrgm (graphToNodes graphData)
  let graphData' = graphFromEdges prgms'

  -- final passes
  let prgms'' = map (finalPasses graphData') prgms'
  return $ graphFromEdges prgms''
