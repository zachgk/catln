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
import           Data.Bifunctor                 ( first )
import           Data.List
import           Data.Maybe
import           Text.Printf

import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           Parser.Syntax
import           Parser                   (parseFile)
import           Desugarf.Passes

splitDeclSubStatements :: [PDeclSubStatement] -> ([PDecl], [PCompAnnot])
splitDeclSubStatements = aux ([], [])
  where
    aux (decls, annots) [] = (decls, annots)
    aux (decls, annots) (RawDeclSubStatementDecl decl : subSt) = aux (decl:decls, annots) subSt
    aux (decls, annots) (RawDeclSubStatementAnnot annot : subSt) = aux (decls, annot:annots) subSt

scopeSubDeclFunNamesInS :: TypeName -> S.HashSet TypeName -> TypeName -> TypeName
scopeSubDeclFunNamesInS prefix replaceNames name = name'
  where
    addPrefix n = prefix ++ "." ++ n
    name' = if S.member name replaceNames then addPrefix name else name

scopeSubDeclFunNamesInExpr :: TypeName -> S.HashSet TypeName -> PSExpr -> PSExpr
scopeSubDeclFunNamesInExpr _ _ e@PSCExpr{} = e
scopeSubDeclFunNamesInExpr prefix replaceNames (PSValue m name) = PSValue m $ scopeSubDeclFunNamesInS prefix replaceNames name
scopeSubDeclFunNamesInExpr prefix replaceNames (PSTupleApply m (bm, bExpr) args) = PSTupleApply m (bm, bExpr') args'
  where
    bExpr' = scopeSubDeclFunNamesInExpr prefix replaceNames bExpr
    args' = fmap (scopeSubDeclFunNamesInExpr prefix replaceNames) args

scopeSubDeclFunNamesInMeta :: TypeName -> S.HashSet TypeName -> ParseMeta -> ParseMeta
scopeSubDeclFunNamesInMeta prefix replaceNames (PreTyped (SumType partials)) = PreTyped $ SumType partials'
  where
    scopeS = scopeSubDeclFunNamesInS prefix replaceNames
    partials' = H.fromList $ map (first scopeS) $ H.toList partials
scopeSubDeclFunNamesInMeta _ _ m@(PreTyped TopType) = m
scopeSubDeclFunNamesInMeta _ _ m@(PreTyped TypeVar{}) = m

-- Renames sub functions by applying the parent names as a prefix to avoid name collisions
scopeSubDeclFunNames :: TypeName -> [PSemiDecl] -> Maybe PSExpr -> [PSCompAnnot] -> ParseMeta -> ParseMeta -> ([PSemiDecl], Maybe PSExpr, [PSCompAnnot], ParseMeta, ParseMeta)
scopeSubDeclFunNames prefix decls maybeExpr annots objM arrM = (decls', expr', annots', objM', arrM')
  where
    declNames = S.fromList $ map (\(PSemiDecl (DeclLHS _ (Pattern (Object _ _ name _ _) _)) _ _) -> name) decls
    addPrefix n = prefix ++ "." ++ n
    scopeM = scopeSubDeclFunNamesInMeta prefix declNames
    objM' = scopeM objM
    arrM' = scopeM arrM
    decls' = map (\(PSemiDecl (DeclLHS aM (Pattern (Object oM basis name vars args) guard)) annot subExpr) -> PSemiDecl (DeclLHS (scopeM aM) (Pattern (Object (scopeM oM) basis (addPrefix name) vars args) guard)) annot (fmap (scopeSubDeclFunNamesInExpr prefix declNames) subExpr)) decls
    expr' = fmap (scopeSubDeclFunNamesInExpr prefix declNames) maybeExpr
    annots' = map (\(CompAnnot n ca) -> CompAnnot n (fmap (scopeSubDeclFunNamesInExpr prefix declNames) ca)) annots

currySubFunctionSignature :: H.HashMap ArgName PObjArg -> PSemiDecl -> PSemiDecl
currySubFunctionSignature parentArgs (PSemiDecl (DeclLHS arrM (Pattern (Object objM basis name vars args) guard)) annot expr) = PSemiDecl (DeclLHS arrM (Pattern (Object objM basis name vars args') guard)) annot expr
  where args' = H.union args parentArgs


currySubFunctionsUpdateExpr :: S.HashSet TypeName -> H.HashMap ArgName PObjArg -> PSExpr -> PSExpr
currySubFunctionsUpdateExpr _ _ c@PSCExpr{} = c
currySubFunctionsUpdateExpr _ parentArgs v@PSValue{} | H.null parentArgs = v
currySubFunctionsUpdateExpr toUpdate parentArgs v@(PSValue _ vn) = if S.member vn toUpdate
  then PSTupleApply emptyMeta (emptyMeta, v) (H.mapWithKey (\k (parentArgM, _) -> PSValue parentArgM k) parentArgs)
  else v
currySubFunctionsUpdateExpr toUpdate parentArgs (PSTupleApply tm (tbm, tbe) tArgs) = PSTupleApply tm (tbm, tbe') tArgs'
  where
    tbe' = currySubFunctionsUpdateExpr toUpdate parentArgs tbe
    tArgs' = fmap (currySubFunctionsUpdateExpr toUpdate parentArgs) tArgs

currySubFunctions :: H.HashMap ArgName PObjArg -> [PSemiDecl] -> Maybe PSExpr -> [PSCompAnnot] -> ([PSemiDecl], Maybe PSExpr, [PSCompAnnot])
currySubFunctions parentArgs decls expr annots = (decls', expr', annots')
  where
    toUpdate = S.fromList $ map (\(PSemiDecl (DeclLHS _ (Pattern (Object _ _ declName _ _) _)) _ _) -> declName) decls
    decls2 = map (currySubFunctionSignature parentArgs) decls
    expr' = fmap (currySubFunctionsUpdateExpr toUpdate parentArgs) expr
    decls' = map (\(PSemiDecl lhs an e) -> PSemiDecl lhs an (fmap (currySubFunctionsUpdateExpr toUpdate parentArgs) e)) decls2
    annots' = map (\(CompAnnot n ca) -> CompAnnot n (fmap (currySubFunctionsUpdateExpr toUpdate parentArgs) ca)) annots

removeSubDeclarations :: PDecl -> [PSemiDecl]
removeSubDeclarations (RawDecl (DeclLHS arrM (Pattern (Object objM basis declName vars args) guard1)) subStatements expr1) = decl':subDecls5
  where
    (subDecls, annots1) = splitDeclSubStatements subStatements
    subDecls2 = concatMap removeSubDeclarations subDecls
    (subDecls21, expr2) = traverse semiDesExpr expr1
    (subDecls22, annots2) = traverse semiDesAnnot annots1
    (subDecls23, guard2) = semiDesGuard guard1
    subDecls3 = concat [subDecls2, subDecls21, subDecls22, subDecls23]
    (subDecls4, expr3, annots3, objM', arrM') = scopeSubDeclFunNames declName subDecls3 expr2 annots2 objM arrM
    (subDecls5, expr4, annots4) = currySubFunctions args subDecls4 expr3 annots3
    decl' = PSemiDecl (DeclLHS arrM' (Pattern (Object objM' basis declName vars args) guard2)) annots4 expr4

desExpr :: PArgMetaMap -> PSExpr -> DesExpr
desExpr _ (PSCExpr m c) = CExpr m c
desExpr arrArgs (PSValue m n) = if H.member n arrArgs
  then Arg m n
  else Value m n
desExpr arrArgs (PSTupleApply m (bm, be) args) = TupleApply m (bm, desExpr arrArgs be) (fmap (desExpr arrArgs) args)

desGuard :: PArgMetaMap -> PSGuard -> DesGuard
desGuard arrArgs = fmap (desExpr arrArgs)

desAnnot :: PArgMetaMap -> PSCompAnnot -> DesCompAnnot
desAnnot arrArgs (CompAnnot name args) = CompAnnot name (fmap (desExpr arrArgs) args)

semiDesExpr :: PExpr -> ([PSemiDecl], PSExpr)
semiDesExpr (RawCExpr m c) = ([], PSCExpr m c)
semiDesExpr (RawValue m n) = ([], PSValue m n)
semiDesExpr (RawTupleApply m (bm, be) args) = (subBe ++ subArgs, PSTupleApply m (bm, be') args')
  where
    (subBe, be') = semiDesExpr be
    (subArgs, args') = traverse semiDesExpr args
semiDesExpr r@(RawIfThenElse m i t e) = (concat [subI, subT, subE, [elseDecl, ifDecl]], expr')
  where
    condName = "\\" ++ take 6 (printf "%08x" (hash r))
    (subI, i') = semiDesExpr i
    (subT, t') = semiDesExpr t
    (subE, e') = semiDesExpr e
    ifDecl = PSemiDecl (DeclLHS emptyMeta (Pattern (Object emptyMeta FunctionObj condName H.empty H.empty) (IfGuard i'))) [] (Just t')
    elseDecl = PSemiDecl (DeclLHS emptyMeta (Pattern (Object emptyMeta FunctionObj condName H.empty H.empty) ElseGuard)) [] (Just e')
    expr' = PSValue m condName
semiDesExpr r@(RawMatch m e matchItems) = (subE ++ subMatchItems, expr')
  where
    condName = "\\" ++ take 6 (printf "%08x" (hash r))
    argName = condName ++ "-arg"
    (subE, e') = semiDesExpr e
    expr' = PSTupleApply m (emptyMeta, PSValue emptyMeta condName) (H.singleton argName e')
    subMatchItems = concatMap semiDesMatchItem $ H.toList matchItems
    semiDesMatchItem (Pattern patt pattGuard, matchExpr) = concat [[matchItemExpr'], subPattGuard, subMatchExpr]
      where
        (subPattGuard, pattGuard') = semiDesGuard pattGuard
        (subMatchExpr, matchExpr') = semiDesExpr matchExpr
        matchArg = H.singleton argName (emptyMeta, Just patt)
        matchItemExpr' = PSemiDecl (DeclLHS emptyMeta (Pattern (Object emptyMeta FunctionObj condName H.empty matchArg) pattGuard')) [] (Just matchExpr')
semiDesExpr (RawCase _ _ ((Pattern _ ElseGuard, _):_)) = error "Can't use elseguard in match expr"
semiDesExpr (RawCase _ _ []) = error "Empty case"
semiDesExpr (RawCase _ _ [(_, matchExpr)]) = semiDesExpr matchExpr
semiDesExpr r@(RawCase m e ((Pattern firstObj@(Object fm _ _ _ _) firstGuard, firstExpr):restCases)) = (concat [[firstDecl, restDecl], subFG, subFE, subRE, subE], expr')
  where
    condName = "\\" ++ take 6 (printf "%08x" (hash r))
    argName = condName ++ "-arg"
    declObj = Object emptyMeta FunctionObj condName H.empty (H.singleton argName (fm, Just firstObj))
    firstDecl = PSemiDecl (DeclLHS m (Pattern declObj firstGuard')) [] (Just firstExpr')
    (subFG, firstGuard') = semiDesGuard firstGuard
    (subFE, firstExpr') = semiDesExpr firstExpr
    restDecl = PSemiDecl (DeclLHS m (Pattern declObj ElseGuard)) [] (Just restExpr')
    (subRE, restExpr') = semiDesExpr (RawCase m e restCases)
    (subE, e') = semiDesExpr e
    expr' = PSTupleApply m (emptyMeta, PSValue emptyMeta condName) (H.singleton argName e')


semiDesGuard :: PGuard -> ([PSemiDecl], PSGuard)
semiDesGuard (IfGuard e) = (subE, IfGuard e')
  where (subE, e') = semiDesExpr e
semiDesGuard ElseGuard = ([], ElseGuard)
semiDesGuard NoGuard = ([], NoGuard)

semiDesAnnot :: PCompAnnot -> ([PSemiDecl], PSCompAnnot)
semiDesAnnot (CompAnnot name args) = (subArgs, CompAnnot name args')
  where (subArgs, args') = traverse semiDesExpr args

declToObjArrow :: PSemiDecl -> (PObject, [PArrow])
declToObjArrow (PSemiDecl (DeclLHS arrM (Pattern object guard)) annots expr) = (object, [arrow])
  where
    argMetaMap = formArgMetaMap object
    arrow = Arrow arrM (map (desAnnot argMetaMap) annots) (desGuard argMetaMap guard) (fmap (desExpr argMetaMap) expr)

desDecl :: PDecl -> PObjectMap
desDecl decl = H.fromListWith (++) $ map declToObjArrow $ removeSubDeclarations decl

unionsWith :: (Ord k, Hashable k) => (a->a->a) -> [H.HashMap k a] -> H.HashMap k a
unionsWith f = foldl (H.unionWith f) H.empty

desDecls :: [PDecl] -> PObjectMap
desDecls decls = unionsWith (++) $ map desDecl decls

typeDefMetaToObj :: ParseMeta -> Maybe PObject
typeDefMetaToObj (PreTyped TypeVar{}) = Nothing
typeDefMetaToObj m@(PreTyped (SumType partials)) = case splitPartialLeafs partials of
  [(partialName, partialVars, partialArgs)] -> Just $ Object m TypeObj partialName (fmap PreTyped partialVars) (fmap ((,Nothing) . PreTyped) partialArgs)
  _ -> error "Invalid call to typeDefMetaToObj with SumType"
typeDefMetaToObj _ = error "Invalid call to typeDefMetaToObj"

desMultiTypeDefs :: [PMultiTypeDef] -> (PObjectMap, ClassMap)
desMultiTypeDefs = foldr addTypeDef (H.empty, (H.empty, H.empty))
  where
    addTypeDef (MultiTypeDef className classVars dataMetas) (objMap, (typeToClass, classToType)) = (objMap', (typeToClass', classToType'))
      where
        objMap' = mergeObjMaps objMap $ H.fromList $ map (,[]) objs
        objNames = map (\(Object _ _ name _ _) -> name) objs
        dataTypes = map (\(PreTyped tp) -> tp) dataMetas
        objs = mapMaybe typeDefMetaToObj dataMetas
        unionSealType (sealed, cv, a) (_, _, b) = (sealed, cv, a ++ b)
        classToType' = H.insertWith unionSealType className (True, classVars, dataTypes) classToType
        newTypeToClass = H.fromList $ map (,S.singleton className) objNames
        typeToClass' = H.unionWith S.union typeToClass newTypeToClass

desTypeDefs :: [PTypeDef] -> PObjectMap
desTypeDefs = foldr addTypeDef H.empty
  where addTypeDef (TypeDef tp) = case typeDefMetaToObj tp of
          Just obj -> H.insert obj []
          Nothing -> error "Type def could not be converted into meta"

desClassDefs :: Sealed -> [RawClassDef] -> ClassMap
desClassDefs sealed = foldr addDef empty
  where
    empty = (H.empty, H.empty)
    addDef (typeName, className) (typeToClass, classToType) = (H.insertWith S.union typeName (S.singleton className) typeToClass, H.insertWith addClass className (sealed, H.empty, [SumType $ joinPartialLeafs [(typeName, H.empty, H.empty)]]) classToType)
    addClass (cSealed, cVars, set1) (_, _, set2) = (cSealed, cVars, set1 ++ set2)

mergeObjMaps :: PObjectMap -> PObjectMap -> PObjectMap
mergeObjMaps = H.unionWith (++)

mergeClassMaps :: ClassMap -> ClassMap -> ClassMap
mergeClassMaps (toClassA, toTypeA) (toClassB, toTypeB) = (H.unionWith S.union toClassA toClassB, H.unionWith mergeClasses toTypeA toTypeB)
  where mergeClasses (sealedA, classVarsA, setA) (sealedB, classVarsB, setB) = if sealedA == sealedB
          then (sealedA, H.unionWith unionType classVarsA classVarsB, setA ++ setB)
          else error "Added to sealed class definition"

desStatements :: [PStatement] -> DesPrgm
desStatements statements = (objMap, classMap)
  where
    splitStatements statement = case statement of
          RawDeclStatement decl -> ([decl], [], [], [])
          MultiTypeDefStatement multiTypedef -> ([], [multiTypedef], [], [])
          TypeDefStatement typedef -> ([], [], [typedef], [])
          RawClassDefStatement classdef -> ([], [], [], [classdef])
    (decls, multiTypes, types, classes) = (\(a, b, c, d) -> (concat a, concat b, concat c, concat d)) $ unzip4 $ map splitStatements statements
    declObjMap = desDecls decls
    typeObjMap = desTypeDefs types
    (multiTypeObjMap, sealedClasses) = desMultiTypeDefs multiTypes
    unsealedClasses = desClassDefs False classes
    objMap = foldr mergeObjMaps H.empty [declObjMap, multiTypeObjMap, typeObjMap]
    classMap = mergeClassMaps sealedClasses unsealedClasses

finalPasses :: DesPrgm -> DesPrgm
finalPasses = classToObjSum

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
        CRes notes2 res -> return $ CRes (notes ++ notes2) $ finalPasses res

desFiles :: [FileImport] -> IO (CRes DesPrgm)
desFiles imports = desPrgm (imports, [])
