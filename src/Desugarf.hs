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

module Desugarf where


import           Data.Bifunctor      (first)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Hashable
import           Data.Maybe
import           Text.Printf

import           CRes
import           Data.Graph
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
    aux (decls, annots) (RawDeclSubStatementAnnot annot : subSt) = aux (decls, annot:annots) subSt
    aux (decls, annots) (RawDeclSubStatementComment _ : subSt) = aux (decls, annots) subSt

scopeSubDeclFunNamesInS :: TypeName -> S.HashSet TypeName -> TypeName -> TypeName
scopeSubDeclFunNamesInS prefix replaceNames name = name'
  where
    addPrefix n = prefix ++ "." ++ n
    name' = if S.member name replaceNames then addPrefix name else name

scopeSubDeclFunNamesInPartialName :: TypeName -> S.HashSet TypeName -> PartialName -> PartialName
scopeSubDeclFunNamesInPartialName prefix replaceNames (PTypeName name) = PTypeName $ scopeSubDeclFunNamesInS prefix replaceNames name
scopeSubDeclFunNamesInPartialName prefix replaceNames (PClassName name) = PClassName $ scopeSubDeclFunNamesInS prefix replaceNames name

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
    declNames = S.fromList $ map (\(PSemiDecl (DeclLHS _ (Pattern Object{objName} _)) _ _) -> objName) decls
    addPrefix n = prefix ++ "." ++ n
    scopeM = scopeSubDeclFunNamesInMeta prefix declNames
    objM' = scopeM objM
    arrM' = scopeM arrM
    decls' = map (\(PSemiDecl (DeclLHS aM (Pattern obj@Object{objM=om, objName} guard)) annot subExpr) -> PSemiDecl (DeclLHS (scopeM aM) (Pattern (obj{objM=scopeM om, objName = addPrefix objName}) guard)) annot (fmap (scopeSubDeclFunNamesInExpr prefix declNames) subExpr)) decls
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
    toUpdate = S.fromList $ map (\(PSemiDecl (DeclLHS _ (Pattern Object{objName} _)) _ _) -> objName) decls
    decls2 = map (currySubFunctionSignature parentArgs) decls
    expr' = fmap (currySubFunctionsUpdateExpr toUpdate parentArgs) expr
    decls' = map (\(PSemiDecl lhs an e) -> PSemiDecl lhs an (fmap (currySubFunctionsUpdateExpr toUpdate parentArgs) e)) decls2
    annots' = map (currySubFunctionsUpdateExpr toUpdate parentArgs) annots

desObjDocComment :: [PDeclSubStatement] -> Maybe String
desObjDocComment (RawDeclSubStatementComment doc: rest) = Just (++) <*> Just doc <*> desObjDocComment rest
desObjDocComment _ = Just ""

removeSubDeclarations :: PDecl -> [PSemiDecl]
removeSubDeclarations (RawDecl (DeclLHS arrM (Pattern obj@Object{objM, objName, objArgs} guard1)) subStatements expr1) = decl':subDecls5
  where
    objDoc = desObjDocComment subStatements
    (subDecls, annots1) = splitDeclSubStatements subStatements
    subDecls2 = concatMap removeSubDeclarations subDecls
    (subDecls21, expr2) = traverse semiDesExpr expr1
    (subDecls22, annots2) = traverse semiDesExpr annots1
    (subDecls23, guard2) = semiDesGuard guard1
    subDecls3 = concat [subDecls2, subDecls21, subDecls22, subDecls23]
    (subDecls4, expr3, annots3, objM', arrM') = scopeSubDeclFunNames objName subDecls3 expr2 annots2 objM arrM
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

semiDesExpr :: PExpr -> ([PSemiDecl], PSExpr)
semiDesExpr (RawCExpr m c) = ([], PSCExpr m c)
semiDesExpr (RawValue m n) = ([], PSValue m n)
semiDesExpr (RawTupleApply m'' (bm, be) args) = (\(a, _, PSTupleApply _ (bm'', be'') argName'' argVal'') -> (a, PSTupleApply m'' (bm'', be'') argName'' argVal'')) $ foldl aux (subBe, bm, be') args
  where
    (subBe, be') = semiDesExpr be
    aux (sub, m, e) (RawTupleArgNamed argName argVal) = (subArgVal ++ sub, emptyMetaM "res" m'', PSTupleApply (emptyMetaM "app" m'') (m, e) (Just argName) argVal')
      where (subArgVal, argVal') = semiDesExpr argVal
    aux (sub, m, e) (RawTupleArgInfer argVal) = (subArgVal ++ sub, emptyMetaM "res" m'', PSTupleApply (emptyMetaM "app" m'') (m, e) Nothing argVal')
      where (subArgVal, argVal') = semiDesExpr argVal
semiDesExpr (RawParen e) = semiDesExpr e
semiDesExpr (RawMethods base methods) = semiDesExpr $ foldl addMethod base methods
  where
    addMethod b method@RawValue{} = RawTupleApply (emptyMetaE "app" b) (emptyMetaE "appBase" method, method) [RawTupleArgNamed "this" b]
    addMethod b (RawTupleApply m methodVal methodArgs) = RawTupleApply m methodVal (RawTupleArgNamed "this" b : methodArgs)
    addMethod _ _ = error "Unknown semiDesExpr method"
semiDesExpr r@(RawIfThenElse m i t e) = (concat [subI, subT, subE, [elseDecl, ifDecl]], expr')
  where
    condName = "$" ++ take 6 (printf "%08x" (hash r))
    (subI, i') = semiDesExpr i
    (subT, t') = semiDesExpr t
    (subE, e') = semiDesExpr e
    ifDecl = PSemiDecl (DeclLHS (emptyMetaE "obj" i) (Pattern (Object (emptyMetaE "patt" i) FunctionObj condName H.empty H.empty Nothing) (IfGuard i'))) [] (Just t')
    elseDecl = PSemiDecl (DeclLHS (emptyMetaE "obj" e) (Pattern (Object (emptyMetaE "patt" e) FunctionObj condName H.empty H.empty Nothing) ElseGuard)) [] (Just e')
    expr' = PSValue m condName
semiDesExpr r@(RawMatch m e matchItems) = (subE ++ subMatchItems, expr')
  where
    condName = "$" ++ take 6 (printf "%08x" (hash r))
    argName = condName ++ "-arg"
    (subE, e') = semiDesExpr e
    expr' = PSTupleApply m (emptyMetaM "app" m, PSValue (emptyMetaM "val" m) condName) (Just argName) e'
    subMatchItems = concatMap semiDesMatchItem matchItems
    semiDesMatchItem (Pattern patt pattGuard, matchExpr) = concat [[matchItemExpr'], subPattGuard, subMatchExpr]
      where
        (subPattGuard, pattGuard') = semiDesGuard pattGuard
        (subMatchExpr, matchExpr') = semiDesExpr matchExpr
        matchArg = H.singleton argName (emptyMetaM "arg" m, Just patt)
        matchItemExpr' = PSemiDecl (DeclLHS (emptyMetaM "obj" m) (Pattern (Object (emptyMetaM "patt" m) FunctionObj condName H.empty matchArg Nothing) pattGuard')) [] (Just matchExpr')
semiDesExpr (RawCase _ _ ((Pattern _ ElseGuard, _):_)) = error "Can't use elseguard in match expr"
semiDesExpr (RawCase _ _ []) = error "Empty case"
semiDesExpr (RawCase _ _ [(_, matchExpr)]) = semiDesExpr matchExpr
semiDesExpr r@(RawCase m e ((Pattern firstObj@Object{objM=fm} firstGuard, firstExpr):restCases)) = (concat [[firstDecl, restDecl], subFG, subFE, subRE, subE], expr')
  where
    condName = "$" ++ take 6 (printf "%08x" (hash r))
    argName = condName ++ "-arg"
    declObj = Object (emptyMetaM "obj" m) FunctionObj condName H.empty (H.singleton argName (fm, Just firstObj)) Nothing
    firstDecl = PSemiDecl (DeclLHS m (Pattern declObj firstGuard')) [] (Just firstExpr')
    (subFG, firstGuard') = semiDesGuard firstGuard
    (subFE, firstExpr') = semiDesExpr firstExpr
    restDecl = PSemiDecl (DeclLHS m (Pattern declObj ElseGuard)) [] (Just restExpr')
    (subRE, restExpr') = semiDesExpr (RawCase m e restCases)
    (subE, e') = semiDesExpr e
    expr' = PSTupleApply m (emptyMetaM "app" m, PSValue (emptyMetaE "val" e) condName) (Just argName) e'
semiDesExpr (RawList m []) = semiDesExpr (RawValue m "Nil")
semiDesExpr (RawList m (l:ls)) = semiDesExpr (RawTupleApply (emptyMetaM "listApp" (getExprMeta l)) (emptyMetaM "listBase" (getExprMeta l), RawValue m "Cons") [RawTupleArgNamed "head" l, RawTupleArgNamed "tail" (RawList m ls)])


semiDesGuard :: PGuard -> ([PSemiDecl], PSGuard)
semiDesGuard (IfGuard e) = (subE, IfGuard e')
  where (subE, e') = semiDesExpr e
semiDesGuard ElseGuard = ([], ElseGuard)
semiDesGuard NoGuard = ([], NoGuard)

declToObjArrow :: StatementEnv -> PSemiDecl -> (DesObject, [DesArrow])
declToObjArrow (_, inheritAnnots) (PSemiDecl (DeclLHS arrM (Pattern object guard)) annots expr) = (object, [arrow])
  where
    argMetaMap = formArgMetaMap object
    annots' = map (desExpr argMetaMap) annots
    arrow = Arrow arrM (annots' ++ inheritAnnots) (desGuard argMetaMap guard) (fmap (desExpr argMetaMap) expr)

desDecl :: StatementEnv -> PDecl -> DesObjectMap
desDecl statementEnv decl = map (declToObjArrow statementEnv) $ removeSubDeclarations decl

typeDefMetaToObj :: H.HashMap TypeVarName Type -> ParseMeta -> Maybe PObject
typeDefMetaToObj _ (PreTyped TypeVar{} _) = Nothing
typeDefMetaToObj varReplaceMap (PreTyped (UnionType partials) pos) = case splitPartialLeafs partials of
  [partial@(PartialType (PTypeName partialName) partialVars _ partialArgs _)] -> Just $ Object m' TypeObj partialName (fmap toMeta partialVars') (fmap (\arg -> (PreTyped arg Nothing, Nothing)) partialArgs) Nothing
    where
      partialVars' = fmap (substituteVarsWithVarEnv varReplaceMap) partialVars
      m' = PreTyped (singletonType partial{ptVars=partialVars'}) $ labelPos "obj" pos
      toMeta t = PreTyped t Nothing
  _ -> error "Invalid call to typeDefMetaToObj with UnionType"
typeDefMetaToObj _ _ = error "Invalid call to typeDefMetaToObj"

desMultiTypeDef :: PMultiTypeDef -> [RawDeclSubStatement ParseMeta] -> DesPrgm
desMultiTypeDef (MultiTypeDef className classVars dataMetas) subStatements = (objMap', (typeToClass', classToType'), [])
    where
      objMap' = map (,[]) objs
      objNames = map objName objs
      dataTypes = map getMetaType dataMetas
      objs = mapMaybe (typeDefMetaToObj classVars) dataMetas
      classToType' = H.singleton className (True, classVars, dataTypes, desObjDocComment subStatements)
      typeToClass' = H.fromList $ map (,S.singleton className) objNames

desClassDecl :: RawClassDecl -> [RawDeclSubStatement ParseMeta] -> DesPrgm
desClassDecl (className, classVars) subStatements = ([], (H.empty, H.singleton className (False, classVars, [], desObjDocComment subStatements)), [])

desTypeDef :: PTypeDef -> [RawDeclSubStatement ParseMeta] -> DesObjectMap
desTypeDef (TypeDef tp) subStatements = case typeDefMetaToObj H.empty tp of
          Just Object{objM, objBasis, objName, objVars, objArgs, objDoc} -> [(Object objM objBasis objName objVars objArgs (desObjDocComment subStatements), [])]
          Nothing  -> error "Type def could not be converted into meta"

desClassDef :: Sealed -> RawClassDef -> [RawDeclSubStatement ParseMeta] -> DesPrgm
desClassDef sealed ((typeName, typeVars), className) subStatements = ([], classMap, [])
  where
    classMap = (
        H.singleton typeName (S.singleton className),
        H.singleton className 
        (sealed, H.empty, [singletonType (PartialType (PTypeName typeName) typeVars H.empty H.empty PtArgExact)], desObjDocComment subStatements)
      )

emptyClassMap :: ClassMap
emptyClassMap = (H.empty, H.empty)

mergeObjMaps :: DesObjectMap -> DesObjectMap -> DesObjectMap
mergeObjMaps = (++)

desGlobalAnnot :: PCompAnnot -> DesCompAnnot
desGlobalAnnot p = case semiDesExpr p of
  ([], d) -> desExpr H.empty d
  _ -> error "Global annotations do not support sub-expressions and lambdas"

desStatement :: StatementEnv -> PStatement -> DesPrgm
desStatement statementEnv (RawDeclStatement decl) = (desDecl statementEnv decl, emptyClassMap, [])
desStatement _ (MultiTypeDefStatement multiTypeDef subStatements) = desMultiTypeDef multiTypeDef subStatements
desStatement _ (TypeDefStatement typeDef subStatements) = (desTypeDef typeDef subStatements, emptyClassMap, [])
desStatement _ (RawClassDefStatement classDef subStatements) = desClassDef False classDef subStatements
desStatement _ (RawClassDeclStatement classDecls subStatements) = desClassDecl classDecls subStatements
desStatement _ RawComment{} = ([], emptyClassMap, [])
desStatement _ (RawGlobalAnnot a []) = ([], emptyClassMap, [desGlobalAnnot a])
desStatement (inheritModule, inheritAnnots) (RawGlobalAnnot a subStatements) = mergePrgms $ map (desStatement (inheritModule, desGlobalAnnot a:inheritAnnots)) subStatements
desStatement (inheritModule, inheritAnnots) (RawModule name subStatements) = mergePrgms $ map (desStatement (inheritModule ++ "/" ++ name, inheritAnnots)) subStatements

finalPasses :: DesPrgmGraphData -> GraphNodes DesPrgm String -> GraphNodes DesPrgm String
finalPasses (desPrgmGraph, nodeFromVertex, vertexFromKey) (prgm, prgmName, imports) = (prgm'', prgmName, imports)
  where
    -- Build fullPrgm with recursive imports
    vertex = fromJust $ vertexFromKey prgmName
    importTree = reachable desPrgmGraph vertex
    fullPrgm = mergePrgms $ map (fst3 . nodeFromVertex) importTree

    -- Run typeNameToClass pass
    prgm' = typeNameToClass fullPrgm prgm
    fullPrgm' = typeNameToClass fullPrgm fullPrgm

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
