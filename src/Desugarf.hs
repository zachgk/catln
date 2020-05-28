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
import           Data.Bifunctor                 ( first )

import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           Parser.Syntax
import           Parser                   (parseFile)
import           Text.Printf

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

scopeSubDeclFunNamesInExpr :: Name -> S.HashSet Name -> PSExpr -> PSExpr
scopeSubDeclFunNamesInExpr _ _ e@PSCExpr{} = e
scopeSubDeclFunNamesInExpr prefix replaceNames (PSValue m name) = PSValue m $ scopeSubDeclFunNamesInS prefix replaceNames name
scopeSubDeclFunNamesInExpr prefix replaceNames (PSTupleApply m (bm, bExpr) args) = PSTupleApply m (bm, bExpr') args'
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
scopeSubDeclFunNames :: Name -> [PSemiDecl] -> Maybe PSExpr -> [PSCompAnnot] -> ParseMeta -> ParseMeta -> ([PSemiDecl], Maybe PSExpr, [PSCompAnnot], ParseMeta, ParseMeta)
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

currySubFunctionSignature :: H.HashMap Name ParseMeta -> PSemiDecl -> PSemiDecl
currySubFunctionSignature parentArgs (PSemiDecl (DeclLHS objM arrM name args guard) annot expr) = PSemiDecl (DeclLHS objM arrM name args' guard) annot expr
  where args' = H.union args parentArgs


currySubFunctionsUpdateExpr :: S.HashSet Name -> H.HashMap Name ParseMeta -> PSExpr -> PSExpr
currySubFunctionsUpdateExpr _ _ c@PSCExpr{} = c
currySubFunctionsUpdateExpr _ parentArgs v@PSValue{} | H.null parentArgs = v
currySubFunctionsUpdateExpr toUpdate parentArgs v@(PSValue _ vn) = if S.member vn toUpdate
  then PSTupleApply emptyMeta (emptyMeta, v) (H.mapWithKey (flip PSValue) parentArgs)
  else v
currySubFunctionsUpdateExpr toUpdate parentArgs (PSTupleApply tm (tbm, tbe) tArgs) = PSTupleApply tm (tbm, tbe') tArgs'
  where
    tbe' = currySubFunctionsUpdateExpr toUpdate parentArgs tbe
    tArgs' = fmap (currySubFunctionsUpdateExpr toUpdate parentArgs) tArgs

currySubFunctions :: H.HashMap Name ParseMeta -> [PSemiDecl] -> Maybe PSExpr -> [PSCompAnnot] -> ([PSemiDecl], Maybe PSExpr, [PSCompAnnot])
currySubFunctions parentArgs decls expr annots = (decls', expr', annots')
  where
    toUpdate = S.fromList $ map (\(PSemiDecl (DeclLHS _ _ declName _ _) _ _) -> declName) decls
    decls2 = map (currySubFunctionSignature parentArgs) decls
    expr' = fmap (currySubFunctionsUpdateExpr toUpdate parentArgs) expr
    decls' = map (\(PSemiDecl lhs an e) -> PSemiDecl lhs an (fmap (currySubFunctionsUpdateExpr toUpdate parentArgs) e)) decls2
    annots' = map (\(CompAnnot n ca) -> CompAnnot n (fmap (currySubFunctionsUpdateExpr toUpdate parentArgs) ca)) annots

removeSubDeclarations :: PDecl -> [PSemiDecl]
removeSubDeclarations (RawDecl (DeclLHS objM arrM declName args guard1) subStatements expr1) = decl':subDecls5
  where
    (subDecls, annots1) = splitDeclSubStatements subStatements
    subDecls2 = concatMap removeSubDeclarations subDecls
    (subDecls21, expr2) = traverse semiDesExpr expr1
    (subDecls22, annots2) = traverse semiDesAnnot annots1
    (subDecls23, guard2) = semiDesGuard guard1
    subDecls3 = concat [subDecls2, subDecls21, subDecls22, subDecls23]
    (subDecls4, expr3, annots3, objM', arrM') = scopeSubDeclFunNames declName subDecls3 expr2 annots2 objM arrM
    (subDecls5, expr4, annots4) = currySubFunctions args subDecls4 expr3 annots3
    decl' = PSemiDecl (DeclLHS objM' arrM' declName args guard2) annots4 expr4

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
    ifDecl = PSemiDecl (DeclLHS emptyMeta emptyMeta condName H.empty (IfGuard i')) [] (Just t')
    elseDecl = PSemiDecl (DeclLHS emptyMeta emptyMeta condName H.empty ElseGuard) [] (Just e')
    expr' = PSValue m condName
semiDesExpr r@(RawMatch m e matchItems) = (subE ++ subMatchItems, expr')
  where
    condName = "\\" ++ take 6 (printf "%08x" (hash r))
    argName = condName ++ "-arg"
    (subE, e') = semiDesExpr e
    expr' = PSTupleApply m (emptyMeta, PSValue emptyMeta condName) (H.singleton argName e')
    subMatchItems = concatMap semiDesMatchItem $ H.toList matchItems
    semiDesMatchItem ((pattName, pattArgs, pattGuard), matchExpr) = concat [[matchItemExpr'], subPattGuard, subMatchExpr]
      where
        (subPattGuard, pattGuard') = semiDesGuard pattGuard
        (subMatchExpr, matchExpr') = semiDesExpr matchExpr
        metaToType (PreTyped tp) = tp
        matchArg = H.singleton argName (PreTyped $ RawSumType S.empty (H.singleton pattName [fmap metaToType pattArgs]) )
        matchItemExpr' = PSemiDecl (DeclLHS emptyMeta emptyMeta condName matchArg pattGuard') [] (Just matchExpr')

semiDesGuard :: PGuard -> ([PSemiDecl], PSGuard)
semiDesGuard (IfGuard e) = (subE, IfGuard e')
  where (subE, e') = semiDesExpr e
semiDesGuard ElseGuard = ([], ElseGuard)
semiDesGuard NoGuard = ([], NoGuard)

semiDesAnnot :: PCompAnnot -> ([PSemiDecl], PSCompAnnot)
semiDesAnnot (CompAnnot name args) = (subArgs, CompAnnot name args')
  where (subArgs, args') = traverse semiDesExpr args

declToObjArrow :: PSemiDecl -> (PObject, [PArrow])
declToObjArrow (PSemiDecl (DeclLHS objM arrM name args guard) annots expr) = (object, [arrow])
  where
    object = Object objM name args
    arrow = Arrow arrM (map (desAnnot args) annots) (desGuard args guard) (fmap (desExpr args) expr)

desDecl :: PDecl -> PObjectMap
desDecl decl = H.fromListWith (++) $ map declToObjArrow $ removeSubDeclarations decl

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
