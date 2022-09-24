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

splitDeclSubStatements :: [PStatementTree] -> ([(PDecl, [PStatementTree])], [PCompAnnot])
splitDeclSubStatements = aux ([], [])
  where
    aux (decls, annots) [] = (decls, annots)
    aux (decls, annots) (RawStatementTree (RawDeclStatement decl) declSubSt : subSt) = aux ((decl, declSubSt):decls, annots) subSt
    aux (decls, annots) (RawStatementTree (RawAnnot annot) []: subSt) = aux (decls, annot:annots) subSt
    aux _ (RawStatementTree (RawAnnot _) (_:_):_) = error "Children in RawAnnot not currently supported"
    aux _ s = error $ printf "Not yet supported subDeclStatemnt type: %s" (show s)

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
scopeSubDeclFunNamesInExpr _ _ e@CExpr{} = e
scopeSubDeclFunNamesInExpr prefix replaceNames (Value m name) = Value m $ scopeSubDeclFunNamesInS prefix replaceNames name
scopeSubDeclFunNamesInExpr prefix replaceNames (Arg m name) = Arg m $ scopeSubDeclFunNamesInS prefix replaceNames name
scopeSubDeclFunNamesInExpr _ _ e@HoleExpr{} = e
scopeSubDeclFunNamesInExpr prefix replaceNames (TupleApply m (bm, bExpr) arg) = TupleApply m (bm, bExpr') arg'
  where
    bExpr' = scopeSubDeclFunNamesInExpr prefix replaceNames bExpr
    arg' = mapTupleArgValue (scopeSubDeclFunNamesInExpr prefix replaceNames) arg
scopeSubDeclFunNamesInExpr prefix replaceNames (VarApply m bExpr varName varVal) = VarApply m bExpr' varName varVal
  where
    bExpr' = scopeSubDeclFunNamesInExpr prefix replaceNames bExpr

scopeSubDeclFunNamesInMeta :: TypeName -> S.HashSet TypeName -> ParseMeta -> ParseMeta
scopeSubDeclFunNamesInMeta prefix replaceNames (PreTyped (UnionType partials) pos) = PreTyped (UnionType partials') pos
  where
    scopeS = scopeSubDeclFunNamesInPartialName prefix replaceNames
    partials' = H.fromList $ map (first scopeS) $ H.toList partials
scopeSubDeclFunNamesInMeta _ _ m@(PreTyped TopType _) = m
scopeSubDeclFunNamesInMeta _ _ m@(PreTyped TypeVar{} _) = m

-- Renames sub functions by applying the parent names as a prefix to avoid name collisions
scopeSubDeclFunNames :: TypeName -> [PSemiDecl] -> Maybe PSExpr -> [PSCompAnnot] -> ParseMeta -> ParseMeta -> ([PSemiDecl], Maybe PSExpr, [PSCompAnnot], ParseMeta, ParseMeta)
scopeSubDeclFunNames prefix decls maybeExpr annots oM arrM = (decls', expr', annots', oM', arrM')
  where
    declNames = S.fromList $ map (\(PSemiDecl (DeclLHS _ (Pattern o _)) _ _) -> objPath o) decls
    addPrefix n = prefix ++ "." ++ n
    scopeM = scopeSubDeclFunNamesInMeta prefix declNames
    oM' = scopeM oM
    arrM' = scopeM arrM
    decls' = map (\(PSemiDecl (DeclLHS aM (Pattern obj guard)) annot subExpr) -> PSemiDecl (DeclLHS (scopeM aM) (Pattern (obj{deprecatedObjM=scopeM (objM obj), deprecatedObjPath = addPrefix (objPath obj)}) guard)) annot (fmap (scopeSubDeclFunNamesInExpr prefix declNames) subExpr)) decls
    expr' = fmap (scopeSubDeclFunNamesInExpr prefix declNames) maybeExpr
    annots' = map (scopeSubDeclFunNamesInExpr prefix declNames) annots

currySubFunctionSignature :: H.HashMap ArgName PObjArg -> PSemiDecl -> PSemiDecl
currySubFunctionSignature parentArgs (PSemiDecl (DeclLHS arrM (Pattern obj@Object{deprecatedObjArgs} guard)) annot expr) = PSemiDecl (DeclLHS arrM (Pattern obj{deprecatedObjArgs=objArgs'} guard)) annot expr
  where objArgs' = H.union deprecatedObjArgs parentArgs


currySubFunctionsUpdateExpr :: S.HashSet TypeName -> H.HashMap ArgName PObjArg -> PSExpr -> PSExpr
currySubFunctionsUpdateExpr _ _ c@CExpr{} = c
currySubFunctionsUpdateExpr _ parentArgs v@Value{} | H.null parentArgs = v
currySubFunctionsUpdateExpr toUpdate parentArgs v@(Value _ vn) = if S.member vn toUpdate
  then foldr (\(parentArgName, (parentArgM, _)) e -> TupleApply emptyMetaN (emptyMetaN, e) (TupleArgIO emptyMetaN parentArgName (Value parentArgM parentArgName))) v $ H.toList parentArgs
  else v
currySubFunctionsUpdateExpr _ _ Arg{} = error "Only values should be used at this point, yet to disambiguate Value vs Arg"
currySubFunctionsUpdateExpr _ _ e@HoleExpr{} = e
currySubFunctionsUpdateExpr toUpdate parentArgs (TupleApply tm (tbm, tbe) targ) = TupleApply tm (tbm, tbe') targ'
  where
    tbe' = currySubFunctionsUpdateExpr toUpdate parentArgs tbe
    targ' = mapTupleArgValue (currySubFunctionsUpdateExpr toUpdate parentArgs) targ
currySubFunctionsUpdateExpr toUpdate parentArgs (VarApply tm tbe tVarName tVarVal) = VarApply tm tbe' tVarName tVarVal
  where
    tbe' = currySubFunctionsUpdateExpr toUpdate parentArgs tbe

currySubFunctions :: H.HashMap ArgName PObjArg -> [PSemiDecl] -> Maybe PSExpr -> [PSCompAnnot] -> ([PSemiDecl], Maybe PSExpr, [PSCompAnnot])
currySubFunctions parentArgs decls expr annots = (decls', expr', annots')
  where
    toUpdate = S.fromList $ map (\(PSemiDecl (DeclLHS _ (Pattern o _)) _ _) -> objPath o) decls
    decls2 = map (currySubFunctionSignature parentArgs) decls
    expr' = fmap (currySubFunctionsUpdateExpr toUpdate parentArgs) expr
    decls' = map (\(PSemiDecl lhs an e) -> PSemiDecl lhs an (fmap (currySubFunctionsUpdateExpr toUpdate parentArgs) e)) decls2
    annots' = map (currySubFunctionsUpdateExpr toUpdate parentArgs) annots

desObjDocComment :: [PStatementTree] -> Maybe String
desObjDocComment ((RawStatementTree (RawAnnot (RawTupleApply _ (_, RawValue _ "/Catln/#md") [TupleArgIO _ "text" (RawCExpr _ (CStr doc))])) _):rest) = Just (++) <*> Just doc <*> desObjDocComment rest
desObjDocComment _ = Just ""

removeSubDeclarations :: (PDecl, [PStatementTree]) -> [PSemiDecl]
removeSubDeclarations (RawDecl (DeclLHS arrM (Pattern obj guard1)) expr1, subStatements) = decl':subDecls5
  where
    objDoc = desObjDocComment subStatements
    (subDecls, annots1) = splitDeclSubStatements subStatements
    subDecls2 = concatMap removeSubDeclarations subDecls
    (subDecls21, expr2) = traverse (semiDesExpr obj) expr1
    (subDecls22, annots2) = traverse (semiDesExpr obj) annots1
    (subDecls23, guard2) = semiDesGuard obj guard1
    subDecls3 = concat [subDecls2, subDecls21, subDecls22, subDecls23]
    (subDecls4, expr3, annots3, objM', arrM') = scopeSubDeclFunNames (objPath obj) subDecls3 expr2 annots2 (objM obj) arrM
    (subDecls5, expr4, annots4) = currySubFunctions ((,Nothing) <$> objArgs obj) subDecls4 expr3 annots3
    decl' = PSemiDecl (DeclLHS arrM' (Pattern obj{objDoc = objDoc, deprecatedObjM=objM'} guard2)) annots4 expr4

desExpr :: PArgMetaMap -> PSExpr -> DesExpr
desExpr _ (CExpr m c) = CExpr m c
desExpr arrArgs (Value m n) = if H.member n arrArgs
  then Arg m n
  else Value m n
desExpr _ Arg{} = error "Only values should be used at this point as it has not yet been disambiguated between Value and Arg"
desExpr _ (HoleExpr m h) = HoleExpr m h
desExpr arrArgs (TupleApply m (bm, be) arg) = TupleApply m (bm, desExpr arrArgs be) (mapTupleArgValue (desExpr arrArgs) arg)
desExpr arrArgs (VarApply m be varName varVal) = VarApply m (desExpr arrArgs be) varName varVal

desGuard :: PArgMetaMap -> PSGuard -> DesGuard
desGuard arrArgs = fmap (desExpr arrArgs)

semiDesExpr :: PObject -> PExpr -> ([PSemiDecl], PSExpr)
semiDesExpr _ (RawCExpr m c) = ([], CExpr m c)
semiDesExpr _ (RawValue m n) = ([], Value m n)
semiDesExpr _ (RawHoleExpr m h) = ([], HoleExpr m h)
semiDesExpr obj (RawTupleApply m'' (bm, be) args) = (\(a, _, TupleApply _ (bm'', be'') arg'') -> (a, TupleApply m'' (bm'', be'') arg'')) $ foldl aux (subBe, bm, be') args
  where
    (subBe, be') = semiDesExpr obj be
    aux (sub, m, e) (TupleArgIO argM argName argVal) = (subArgVal ++ sub, emptyMetaM "res" m'', TupleApply (emptyMetaM "app" m'') (m, e) (TupleArgIO argM argName argVal'))
      where (subArgVal, argVal') = semiDesExpr obj argVal
    aux (sub, m, e) (TupleArgO argM argVal) = (subArgVal ++ sub, emptyMetaM "res" m'', TupleApply (emptyMetaM "app" m'') (m, e) (TupleArgO argM argVal'))
      where (subArgVal, argVal') = semiDesExpr obj argVal
    aux _ TupleArgI{} = error "Unexpected TupleArgI in semiDesExpr"
semiDesExpr obj (RawVarsApply m be vs) = (subBe, foldr aux be' vs)
  where
    aux (varName, varVal) base = VarApply (emptyMetaM varName m) base varName varVal
    (subBe, be') = semiDesExpr obj be
semiDesExpr obj (RawContextApply _ (_, be) ctxs) = semiDesExpr obj $ applyRawArgs (rawVal "/Context") ((Just "value", be) : map (\(ctxName, ctxM) -> (Nothing, RawValue ctxM ctxName)) ctxs)
semiDesExpr obj (RawParen e) = semiDesExpr obj e
-- semiDesExpr obj (RawMethod (RawValue _ n) method) = semiDesExpr obj $ applyRawArgs method [(Nothing, RawValue (PreTyped (typeVal $ PRelativeName n) Nothing) "this")] -- Parse type methods like Integer.toString, TODO: Only for output expressions
semiDesExpr obj (RawMethod base method) = semiDesExpr obj $ applyRawArgs method [(Just "this", base)]
semiDesExpr obj r@(RawIfThenElse m i t e) = (concat [subI, subT, subE, [elseDecl, ifDecl]], expr')
  where
    condName = "$" ++ take 6 (printf "%08x" (hash r))
    (subI, i') = semiDesExpr obj i
    (subT, t') = semiDesExpr obj t
    (subE, e') = semiDesExpr obj e
    ifDecl = PSemiDecl (DeclLHS (emptyMetaE "obj" i) (Pattern (Object (emptyMetaE "patt" i) FunctionObj (objAppliedVars obj) H.empty Nothing condName) (IfGuard i'))) [] (Just t')
    elseDecl = PSemiDecl (DeclLHS (emptyMetaE "obj" e) (Pattern (Object (emptyMetaE "patt" e) FunctionObj (objAppliedVars obj) H.empty Nothing condName) ElseGuard)) [] (Just e')
    expr' = Value m condName
semiDesExpr obj r@(RawMatch m e matchItems) = (subE ++ subMatchItems, expr')
  where
    condName = "$" ++ take 6 (printf "%08x" (hash r))
    argName = condName ++ "-arg"
    (subE, e') = semiDesExpr obj e
    expr' = TupleApply m (emptyMetaM "app" m, Value (emptyMetaM "val" m) condName) (TupleArgIO (emptyMetaE "appArg" e') argName e')
    subMatchItems = concatMap semiDesMatchItem matchItems
    semiDesMatchItem (Pattern patt pattGuard, matchExpr) = concat [[matchItemExpr'], subPattGuard, subMatchExpr]
      where
        (subPattGuard, pattGuard') = semiDesGuard obj pattGuard
        (subMatchExpr, matchExpr') = semiDesExpr obj matchExpr
        matchArg = H.singleton argName (emptyMetaM "arg" m, Just patt)
        matchItemExpr' = PSemiDecl (DeclLHS (emptyMetaM "obj" m) (Pattern (Object (emptyMetaM "patt" m) FunctionObj (objAppliedVars obj) matchArg Nothing condName) pattGuard')) [] (Just matchExpr')
semiDesExpr _ (RawCase _ _ ((Pattern _ ElseGuard, _):_)) = error "Can't use elseguard in match expr"
semiDesExpr _ (RawCase _ _ []) = error "Empty case"
semiDesExpr obj (RawCase _ _ [(_, matchExpr)]) = semiDesExpr obj matchExpr
semiDesExpr obj r@(RawCase m e ((Pattern firstObj firstGuard, firstExpr):restCases)) = (concat [[firstDecl, restDecl], subFG, subFE, subRE, subE], expr')
  where
    condName = "$" ++ take 6 (printf "%08x" (hash r))
    argName = condName ++ "-arg"
    declObj = Object (emptyMetaM "obj" m) FunctionObj (objAppliedVars obj) (H.singleton argName (objM firstObj, Just firstObj)) Nothing condName
    firstDecl = PSemiDecl (DeclLHS m (Pattern declObj firstGuard')) [] (Just firstExpr')
    (subFG, firstGuard') = semiDesGuard obj firstGuard
    (subFE, firstExpr') = semiDesExpr obj firstExpr
    restDecl = PSemiDecl (DeclLHS m (Pattern declObj ElseGuard)) [] (Just restExpr')
    (subRE, restExpr') = semiDesExpr obj (RawCase m e restCases)
    (subE, e') = semiDesExpr obj e
    expr' = TupleApply m (emptyMetaM "app" m, Value (emptyMetaE "val" e) condName) (TupleArgIO (emptyMetaE "appArg" e') argName e')
semiDesExpr obj (RawList m []) = semiDesExpr obj (RawValue m "/Data/Nil")
semiDesExpr obj (RawList m (l:ls)) = semiDesExpr obj (RawTupleApply (emptyMetaM "listApp" (getExprMeta l)) (emptyMetaM "listBase" (getExprMeta l), RawValue m "/Data/Cons") [TupleArgIO (emptyMetaE "arg" l) "head" l, TupleArgIO (emptyMetaE "argRemaining" l) "tail" (RawList m ls)])


semiDesGuard :: PObject -> PGuard -> ([PSemiDecl], PSGuard)
semiDesGuard obj (IfGuard e) = (subE, IfGuard e')
  where (subE, e') = semiDesExpr obj e
semiDesGuard _ ElseGuard = ([], ElseGuard)
semiDesGuard _ NoGuard = ([], NoGuard)

declToObjArrow :: StatementEnv -> PSemiDecl -> DesObjectMapItem
declToObjArrow (inheritPath, inheritAnnots) (PSemiDecl (DeclLHS arrM (Pattern object guard)) annots expr) = (object'', annots' ++ inheritAnnots, Just arrow)
  where
    -- Inherit the path in main object name. If main is a context, also inherit in the context function as well
    updateObjPath o = o{deprecatedObjPath = addPath inheritPath (objPath o)}
    object' = updateObjPath object
    object'' = case objPath object of
      "/Context" -> object{deprecatedObjArgs=H.adjust (\(m, Just o) -> (m, Just $ updateObjPath o)) "value" $ deprecatedObjArgs object}
      _ -> object'

    argMetaMap = formArgMetaMap object'
    annots' = map (desExpr argMetaMap) annots
    arrow = Arrow arrM (desGuard argMetaMap guard) (fmap (desExpr argMetaMap) expr)

desDecl :: StatementEnv -> PDecl -> [PStatementTree] -> DesPrgm
desDecl statementEnv decl subStatements= (objMap, emptyClassGraph, [])
  where
    objMap = map (declToObjArrow statementEnv) $ removeSubDeclarations (decl, subStatements)

addPath :: String -> String -> String
addPath inheritPath name = if "/" `isPrefixOf` name then
  name
  else inheritPath ++ "/" ++ name

-- todo check
typeDefMetaToObj :: String -> H.HashMap TypeVarName Type -> ParseMeta -> Maybe PObject
typeDefMetaToObj _ _ (PreTyped TypeVar{} _) = Nothing
typeDefMetaToObj inheritPath varReplaceMap (PreTyped (UnionType partials) pos) = case splitUnionType partials of
  [partial@(PartialType (PRelativeName partialName) partialVars _ partialArgs _)] -> Just $ Object m' TypeObj (fmap toMeta partialVars') (fmap (\arg -> (PreTyped arg Nothing, Nothing)) partialArgs) Nothing (addPath inheritPath partialName)
    where
      ptName' = PTypeName $ addPath inheritPath partialName
      partialVars' = fmap (substituteVarsWithVarEnv varReplaceMap) partialVars
      m' = PreTyped (singletonType partial{ptVars=partialVars', ptName=ptName'}) $ labelPos "obj" pos
      toMeta t = PreTyped t Nothing
  _ -> error "Invalid call to typeDefMetaToObj with UnionType"
typeDefMetaToObj _ _ _ = error "Invalid call to typeDefMetaToObj"

-- | Desugars statements that inherit a path from a main statement
desInheritingSubstatements :: StatementEnv -> Path -> [PStatementTree] -> (DesPrgm, [DesCompAnnot])
desInheritingSubstatements (inheritModule, inheritAnnots) path subStatements = (prgm', annots)
  where
    path' = case path of
      (Absolute p) -> p
      (Relative p) -> inheritModule ++ "/" ++ p
    statementEnv' = (path', inheritAnnots)
    (objectMap, classGraph, annots) = mergePrgms $ map (desStatement statementEnv') subStatements
    prgm' = (objectMap, classGraph, []) -- Annots in subStatements are not global, but local to the main statement

desMultiTypeDef :: StatementEnv -> PMultiTypeDef -> [RawStatementTree ParseMeta] -> Path -> DesPrgm
desMultiTypeDef statementEnv@(inheritPath, _) (MultiTypeDef className classVars dataMetas) subStatements path = mergePrgm (objMap', classGraph', []) subPrgm
    where
      path' =  case path of
        Absolute p -> p
        Relative p -> inheritPath ++ "/" ++ p
      objMap' = map (, [], Nothing) objs
      objPaths = map objPath objs
      dataTypes = map getMetaType dataMetas
      objs = mapMaybe (typeDefMetaToObj inheritPath classVars) dataMetas
      typeCGNodes = map (CGType, , []) objPaths
      classCGNode = (CGClass (True, classVars, dataTypes, desObjDocComment subStatements, path'), addPath inheritPath className, objPaths)
      classGraph' = ClassGraph $ graphFromEdges (classCGNode:typeCGNodes)
      (subPrgm, _) = desInheritingSubstatements statementEnv path subStatements


desClassDecl :: StatementEnv -> RawClassDecl -> [RawStatementTree ParseMeta] -> Path -> DesPrgm
desClassDecl statementEnv@(inheritPath, _) (className, classVars) subStatements path = mergePrgm ([], classGraph', []) subPrgm
  where
    classGraph' = ClassGraph $ graphFromEdges [(CGClass (False, classVars, [], desObjDocComment subStatements, path'), addPath inheritPath className, [])]
    path' = case path of
      Relative p -> inheritPath ++ "/" ++ p
      Absolute p -> p
    (subPrgm, _) = desInheritingSubstatements statementEnv path subStatements

desTypeDef :: StatementEnv -> PTypeDef -> [RawStatementTree ParseMeta] -> DesPrgm
desTypeDef statementEnv@(inheritPath, _) (TypeDef tp) subStatements = mergePrgm (objMap, emptyClassGraph, []) subPrgm
  where
    (subPrgm, annots) = desInheritingSubstatements statementEnv (getPath $ objPath obj) subStatements
    obj = case typeDefMetaToObj inheritPath H.empty tp of
          Just o  -> o{objDoc = desObjDocComment subStatements}
          Nothing -> error "Type def could not be converted into meta"
    objMap = [(obj, annots, Nothing)]

desClassDef :: StatementEnv -> Sealed -> RawClassDef -> [RawStatementTree ParseMeta] -> Path -> DesPrgm
desClassDef statementEnv@(inheritPath, _) sealed ((typeName, typeVars), className) subStatements path = mergePrgm ([], classGraph, []) subPrgm
  where
    path' =  case path of
      Absolute p -> p
      Relative p -> inheritPath ++ "/" ++ p
    classGraph = ClassGraph $ graphFromEdges [(CGClass (sealed, H.empty, [singletonType (PartialType (PRelativeName typeName) typeVars H.empty H.empty PtArgExact)], desObjDocComment subStatements, path'), className, [typeName])]
    (subPrgm, _) = desInheritingSubstatements statementEnv path subStatements

emptyClassGraph :: ClassGraph
emptyClassGraph = ClassGraph $ graphFromEdges []

mergeObjMaps :: DesObjectMap -> DesObjectMap -> DesObjectMap
mergeObjMaps = (++)

desGlobalAnnot :: PCompAnnot -> DesCompAnnot
desGlobalAnnot p = case semiDesExpr undefined p of
  ([], d) -> desExpr H.empty d
  _ -> error "Global annotations do not support sub-expressions and lambdas"

desStatement :: StatementEnv -> PStatementTree -> DesPrgm
desStatement statementEnv@(inheritModule, inheritAnnots) (RawStatementTree statement subStatements) = case statement of
  RawDeclStatement decl -> desDecl statementEnv decl subStatements
  MultiTypeDefStatement multiTypeDef path -> desMultiTypeDef statementEnv multiTypeDef subStatements path
  TypeDefStatement typeDef -> desTypeDef  statementEnv typeDef subStatements
  RawClassDefStatement classDef path -> desClassDef statementEnv False classDef subStatements path
  RawClassDeclStatement classDecls path -> desClassDecl statementEnv classDecls subStatements path
  RawAnnot a | null subStatements -> ([], emptyClassGraph, [desGlobalAnnot a])
  RawAnnot a -> mergePrgms $ map (desStatement (inheritModule, desGlobalAnnot a:inheritAnnots)) subStatements
  RawModule _ path -> fst $ desInheritingSubstatements statementEnv path subStatements

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
