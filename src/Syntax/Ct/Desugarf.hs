--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Desugarf
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
module Syntax.Ct.Desugarf where


import           Data.Bifunctor            (first, second)
import           Data.Either
import qualified Data.HashMap.Strict       as H
import qualified Data.HashSet              as S
import           Data.Hashable
import           Data.Maybe
import           Text.Printf

import           CRes
import           Data.Graph                hiding (path)
import           Data.List
import           MapMeta
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Desugarf.Passes
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Utils

type StatementEnv = (String, [DesCompAnnot])
type ParentArgs = H.HashMap ArgName ParseMeta

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
scopeSubDeclFunNamesInExpr prefix replaceNames (AliasExpr b a) = AliasExpr (scopeSubDeclFunNamesInExpr prefix replaceNames b) (scopeSubDeclFunNamesInExpr prefix replaceNames a)
scopeSubDeclFunNamesInExpr prefix replaceNames (TupleApply m (bm, bExpr) arg) = TupleApply m (bm, bExpr') arg'
  where
    bExpr' = scopeSubDeclFunNamesInExpr prefix replaceNames bExpr
    arg' = mapTupleArgValue (scopeSubDeclFunNamesInExpr prefix replaceNames) arg
scopeSubDeclFunNamesInExpr prefix replaceNames (VarApply m bExpr varName varVal) = VarApply m bExpr' varName varVal
  where
    bExpr' = scopeSubDeclFunNamesInExpr prefix replaceNames bExpr

scopeSubDeclFunNamesInMeta :: TypeName -> S.HashSet TypeName -> ParseMeta -> ParseMeta
scopeSubDeclFunNamesInMeta prefix replaceNames (Meta (UnionType partials) pos md) = Meta (UnionType partials') pos md
  where
    scopeS = scopeSubDeclFunNamesInPartialName prefix replaceNames
    partials' = H.fromList $ map (first scopeS) $ H.toList partials
scopeSubDeclFunNamesInMeta _ _ m@(Meta TopType _ _) = m
scopeSubDeclFunNamesInMeta _ _ m@(Meta TypeVar{} _ _) = m

-- Renames sub functions by applying the parent names as a prefix to avoid name collisions
scopeSubDeclFunNames :: TypeName -> [PSemiDecl] -> Maybe PSExpr -> [PSCompAnnot] -> ParseMeta -> ([PSemiDecl], Maybe PSExpr, [PSCompAnnot], ParseMeta)
scopeSubDeclFunNames prefix decls maybeExpr annots arrM = (decls', expr', annots', arrM')
  where
    declNames = S.fromList $ map (\(PSemiDecl (DeclLHS _ (Pattern o _)) _ _) -> eobjPath o) decls
    addPrefix n = prefix ++ "." ++ n
    scopeM = scopeSubDeclFunNamesInMeta prefix declNames
    arrM' = scopeM arrM
    decls' = map (\(PSemiDecl (DeclLHS aM (Pattern obj guard)) annot subExpr) -> PSemiDecl (DeclLHS (scopeM aM) (Pattern (mapExprObjPath (\(pM, pN) -> Value pM (addPrefix pN)) obj) guard)) annot (fmap (scopeSubDeclFunNamesInExpr prefix declNames) subExpr)) decls
    expr' = fmap (scopeSubDeclFunNamesInExpr prefix declNames) maybeExpr
    annots' = map (scopeSubDeclFunNamesInExpr prefix declNames) annots

-- | Apply args to a signature or input expression
curryApplyParentArgsSignature :: PSExpr -> ParentArgs -> PSExpr
curryApplyParentArgsSignature e parentArgs = applyExprIArgs e (map (second IArgM) $ H.toList parentArgs)

-- | Apply args to an output expression
curryApplyParentArgs :: PSExpr -> ParentArgs -> PSExpr
curryApplyParentArgs e parentArgs = applyExprIArgs e (map (\(parentArgName, parentArgM) -> (parentArgName, IArgE (Value parentArgM parentArgName))) $ H.toList parentArgs)

currySubFunctionSignature :: ParentArgs -> PSemiDecl -> PSemiDecl
currySubFunctionSignature parentArgs (PSemiDecl (DeclLHS arrM (Pattern obj guard)) annot expr) = PSemiDecl (DeclLHS arrM (Pattern obj' guard)) annot expr
  where
    obj' = mapExprObjPath applyParentArgsToObjPath obj
    applyParentArgsToObjPath (objPathM, objPathN) = curryApplyParentArgsSignature (Value objPathM objPathN) parentArgs

currySubFunctionsUpdateExpr :: S.HashSet TypeName -> ParentArgs -> PSExpr -> PSExpr
currySubFunctionsUpdateExpr _ _ c@CExpr{} = c
currySubFunctionsUpdateExpr _ parentArgs v@Value{} | H.null parentArgs = v
currySubFunctionsUpdateExpr toUpdate parentArgs v@(Value _ vn) = if S.member vn toUpdate
  then curryApplyParentArgs v parentArgs
  else v
currySubFunctionsUpdateExpr _ _ Arg{} = error "Only values should be used at this point, yet to disambiguate Value vs Arg"
currySubFunctionsUpdateExpr _ _ e@HoleExpr{} = e
currySubFunctionsUpdateExpr toUpdate parentArgs (AliasExpr b a) = AliasExpr (currySubFunctionsUpdateExpr toUpdate parentArgs b) (currySubFunctionsUpdateExpr toUpdate parentArgs a)
currySubFunctionsUpdateExpr toUpdate parentArgs (TupleApply tm (tbm, tbe) targ) = TupleApply tm (tbm, tbe') targ'
  where
    tbe' = currySubFunctionsUpdateExpr toUpdate parentArgs tbe
    targ' = mapTupleArgValue (currySubFunctionsUpdateExpr toUpdate parentArgs) targ
currySubFunctionsUpdateExpr toUpdate parentArgs (VarApply tm tbe tVarName tVarVal) = VarApply tm tbe' tVarName tVarVal
  where
    tbe' = currySubFunctionsUpdateExpr toUpdate parentArgs tbe

currySubFunctions :: ParentArgs -> [PSemiDecl] -> Maybe PSExpr -> [PSCompAnnot] -> ([PSemiDecl], Maybe PSExpr, [PSCompAnnot])
currySubFunctions parentArgs decls expr annots = (decls', expr', annots')
  where
    toUpdate = S.fromList $ map (\(PSemiDecl (DeclLHS _ (Pattern o _)) _ _) -> eobjPath o) decls
    decls2 = map (currySubFunctionSignature parentArgs) decls
    expr' = fmap (currySubFunctionsUpdateExpr toUpdate parentArgs) expr
    decls' = map (\(PSemiDecl lhs an e) -> PSemiDecl lhs an (fmap (currySubFunctionsUpdateExpr toUpdate parentArgs) e)) decls2
    annots' = map (currySubFunctionsUpdateExpr toUpdate parentArgs) annots

desObjDocComment :: [PStatementTree] -> Maybe String
desObjDocComment ((RawStatementTree (RawAnnot (RawTupleApply _ (_, RawValue _ "/Catln/#md") [TupleArgIO _ "text" (RawCExpr _ (CStr doc))])) _):rest) = Just (++) <*> Just doc <*> desObjDocComment rest
desObjDocComment _ = Just ""

removeSubDeclarations :: (PDecl, [PStatementTree]) -> [PSemiDecl]
removeSubDeclarations (RawDecl (DeclLHS arrM (Pattern obj@(ExprObject objBasis _ objExpression) guard1)) expr1, subStatements) = decl':subDecls5
  where
    objDoc = desObjDocComment subStatements
    (subDecls, annots1) = splitDeclSubStatements subStatements
    subDecls2 = concatMap removeSubDeclarations subDecls
    (subDecls21, expr2) = traverse (semiDesExpr (Just obj)) expr1
    (subDecls22, annots2) = traverse (semiDesExpr (Just obj)) annots1
    (subDecls23, guard2) = semiDesGuard obj guard1
    subDecls3 = concat [subDecls2, subDecls21, subDecls22, subDecls23]
    (subDecls4, expr3, annots3, arrM') = scopeSubDeclFunNames (eobjPath obj) subDecls3 expr2 annots2 arrM
    ([], objExpression') = semiDesExpr Nothing objExpression
    (subDecls5, expr4, annots4) = currySubFunctions (exprArgs objExpression) subDecls4 expr3 annots3
    decl' = PSemiDecl (DeclLHS arrM' (Pattern (ExprObject objBasis objDoc objExpression') guard2)) annots4 expr4

desExpr :: PArgMetaMap -> PSExpr -> DesExpr
desExpr _ (CExpr m c) = CExpr m c
desExpr arrArgs (Value m n) = if H.member n arrArgs
  then Arg m n
  else Value m n
desExpr _ Arg{} = error "Only values should be used at this point as it has not yet been disambiguated between Value and Arg"
desExpr _ (HoleExpr m h) = HoleExpr m h
desExpr arrArgs (AliasExpr b a) = AliasExpr (desExpr arrArgs b) (desExpr arrArgs a)
desExpr arrArgs (TupleApply m (bm, be) arg) = TupleApply m (bm, desExpr arrArgs be) (mapTupleArgValue (desExpr arrArgs) arg)
desExpr arrArgs (VarApply m be varName varVal) = VarApply m (desExpr arrArgs be) varName varVal

data DOEMode = DOEArgMode | DOEValMode deriving (Eq, Show)
data DOEValName = UseRelativeName | UseTypeName deriving (Eq, Show)

-- |
-- When finding objectExpressions, it requires argMode to know whether something is an arg or a val.
-- Calls with args as foo(f=bar) or foo(bar) would parse bar with argMode
-- Calls not in an arg size or at the root would need to parse the foo with varMode
-- When it creates a val, it requires the second boolean arg "useRelativeName" to know whether to create a relative or absolute val name
-- TODO: The useRelativeName arg (and all DOEValName) seem to not be used, so should remove
desObjValToArg :: DOEMode -> DOEValName -> DesExpr -> DesExpr
desObjValToArg _ _ (CExpr m c) = CExpr m c
desObjValToArg doeMode _ (Value m n) = case doeMode of
  DOEArgMode -> Arg m n
  DOEValMode -> Value m n
desObjValToArg _ _ (Arg _ n) = error $ printf "Found unexpected arg '%s' in desOjValToArg, expected all args to still be represented with Value" n
desObjValToArg _ _ (HoleExpr m h) = HoleExpr m h
desObjValToArg _ useRelativeName (AliasExpr b a) = AliasExpr (desObjValToArg DOEValMode useRelativeName b) (desObjValToArg DOEArgMode useRelativeName a)
desObjValToArg _ useRelativeName mainExpr@(TupleApply m (bm, be) tupleArg) = TupleApply m (bm, be') tupleArg'
  where
    be' = desObjValToArg DOEValMode useRelativeName be
    tupleArg' = case tupleArg of
      TupleArgI{} -> tupleArg
      TupleArgIO argM argName argVal -> TupleArgIO argM argName argVal'
        where argVal' = desObjValToArg DOEValMode useRelativeName argVal
      TupleArgO{} -> error $ printf "Unexpected TupleArgO in desObjValToArg: %s" (show mainExpr)
desObjValToArg _ useRelativeName (VarApply m be varName varVal) = VarApply m be' varName varVal
  where
    be' = desObjValToArg DOEValMode useRelativeName be


-- | Updates the types based on the format as they are fixed for inputs (due to arrows this does not work for output expressions)
desObjPropagateTypes :: DesExpr -> (Maybe PartialType, DesExpr)
desObjPropagateTypes (CExpr m c) = (Just $ constantPartialType c, CExpr (mWithType (constantType c) m) c)
desObjPropagateTypes (Value m n) = (Just t, Value (mWithType (singletonType t) m) n)
  where t = PartialType (PRelativeName n) H.empty H.empty [] PtArgExact
desObjPropagateTypes (Arg m n) = (Just t, Arg (mWithType (singletonType t) m) n)
  where t = PartialType (PRelativeName n) H.empty H.empty [] PtArgExact
desObjPropagateTypes (HoleExpr m h) = (Nothing, HoleExpr m h)
desObjPropagateTypes (AliasExpr base alias) = (basePartial, AliasExpr base' alias')
  where
    (basePartial, base') = desObjPropagateTypes base
    (_, alias') = desObjPropagateTypes alias
desObjPropagateTypes mainExpr@(TupleApply m (bm, be) tupleApplyArgs) = do
  let (Just basePartial@PartialType{ptArgs=baseArgs}, be') = desObjPropagateTypes be
  let bm' = mWithType (getMetaType $ getExprMeta be') bm
  case tupleApplyArgs of
      TupleArgI argM argName -> do
        let basePartial' = basePartial{ptArgs=H.insert argName (getMetaType argM) baseArgs}
        let m' = mWithType (singletonType basePartial') m
        (Just basePartial', TupleApply m' (bm', be') (TupleArgI argM argName))
      TupleArgIO argM argName argVal -> do
        let (_, argVal') = desObjPropagateTypes argVal
        let argM' = mWithType (getMetaType $ getExprMeta argVal) argM
        let basePartial' = basePartial{ptArgs=H.insert argName (getExprType argVal') baseArgs}
        let m' = mWithType (singletonType basePartial') m
        (Just basePartial', TupleApply m' (bm', be') (TupleArgIO argM' argName argVal'))
      TupleArgO{} -> error $ printf "Unexpected TupleArgO in desObjPropagateTypes: %s" (show mainExpr)
desObjPropagateTypes (VarApply m be varName varVal) = (Just basePartial', VarApply m' be' varName varVal)
  where
    (Just basePartial@PartialType{ptVars=baseVars}, be') = desObjPropagateTypes be

    basePartial' = basePartial{ptVars=H.insert varName (getMetaType varVal) baseVars}
    m' = mWithType (singletonType basePartial') m


desObj :: Bool -> String -> DOEValName -> DesObject -> DesObject
desObj isDef inheritPath useRelativeName object@ExprObject{eobjExpr} = object'
  where
    objExpr2 = desObjValToArg DOEValMode useRelativeName eobjExpr

    objExpr3 = if isDef
      then objExpr2
      else snd $ desObjPropagateTypes objExpr2

    -- Inherit the path in main object name. If main is a context, instead inherit in the context function as well
    updateExprPath = mapExprPath (\(eM, eN) -> Value eM (addPath inheritPath eN))
    objExpr4 = case eobjPath object of
      "/Context" -> mapExprAppliedArg updateExprPath "value" objExpr3
      _          -> updateExprPath objExpr3
    object' = object{eobjExpr=objExpr4}

desGuard :: PArgMetaMap -> PSGuard -> DesGuard
desGuard arrArgs = fmap (desExpr arrArgs)

semiDesExpr :: Maybe PObject -> PExpr -> ([PSemiDecl], PSExpr)
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
    aux (sub, m, e) (TupleArgI argM argName) = (sub, emptyMetaM "res" m'', TupleApply (emptyMetaM "app" m'') (m, e) (TupleArgI argM argName))
semiDesExpr obj (RawVarsApply m be vs) = (subBe, foldr aux be' vs)
  where
    aux (varName, varVal) base = VarApply (emptyMetaM varName m) base varName varVal
    (subBe, be') = semiDesExpr obj be
semiDesExpr obj@Just{} (RawContextApply _ (_, be) ctxs) = semiDesExpr obj $ applyRawArgs (RawValue emptyMetaN "/Context") ((Just "value", be) : map (\(ctxName, ctxM) -> (Nothing, RawValue ctxM ctxName)) ctxs)
semiDesExpr obj@Nothing (RawContextApply _ (_, be) ctxs) = semiDesExpr obj $ applyRawIArgs (RawValue emptyMetaN "/Context") (("value", IArgE be) : map (second IArgM) ctxs)
semiDesExpr obj (RawParen e) = semiDesExpr obj e
semiDesExpr obj@Nothing (RawMethod (RawValue m n) method) = semiDesExpr obj $ RawTupleApply (emptyMetaM "method" m) (emptyMetaE "base" method, method) [TupleArgI (Meta (singletonType $ partialVal $ PRelativeName n) (getMetaPos m) emptyMetaDat) "this"] -- Parse type methods like Integer.toString, Only for input expressions
semiDesExpr obj (RawMethod base method) = semiDesExpr obj $ applyRawArgs method [(Just "this", base)]
semiDesExpr obj@(Just jobj) r@(RawIfThenElse m i t e) = (concat [subI, subT, subE, [elseDecl, ifDecl]], expr')
  where
    condName = "$" ++ take 6 (printf "%08x" (hash r))
    (subI, i') = semiDesExpr obj i
    (subT, t') = semiDesExpr obj t
    (subE, e') = semiDesExpr obj e
    ifDecl = PSemiDecl (DeclLHS (emptyMetaE "obj" i) (Pattern (mkExprObj FunctionObj (H.toList $ exprAppliedVars $ eobjExpr jobj) [] Nothing condName) (IfGuard i'))) [] (Just t')
    elseDecl = PSemiDecl (DeclLHS (emptyMetaE "obj" e) (Pattern (mkExprObj FunctionObj (H.toList $ exprAppliedVars $ eobjExpr jobj) [] Nothing condName) ElseGuard)) [] (Just e')
    expr' = Value m condName
semiDesExpr obj@(Just jobj) r@(RawMatch m e matchItems) = (subE ++ subMatchItems, expr')
  where
    condName = "$" ++ take 6 (printf "%08x" (hash r))
    argName = condName ++ "-arg"
    (subE, e') = semiDesExpr obj e
    expr' = TupleApply m (emptyMetaM "app" m, Value (emptyMetaM "val" m) condName) (TupleArgIO (emptyMetaE "appArg" e') argName e')
    subMatchItems = concatMap semiDesMatchItem matchItems
    semiDesMatchItem (Pattern (ExprObject _ _ pattObjExpr) pattGuard, matchExpr) = concat [[matchItemExpr'], subPattGuard, subMatchExpr]
      where
        (subPattGuard, pattGuard') = semiDesGuard jobj pattGuard
        (subMatchExpr, matchExpr') = semiDesExpr obj matchExpr
        ([], pattObjExpr') = semiDesExpr obj pattObjExpr
        matchArg = [(Just argName, AliasExpr (HoleExpr emptyMetaN (HoleActive Nothing)) pattObjExpr')]
        matchItemExpr' = PSemiDecl (DeclLHS (emptyMetaM "obj" m) (Pattern (mkExprObj FunctionObj (H.toList $ exprAppliedVars $ eobjExpr jobj) matchArg Nothing condName) pattGuard')) [] (Just matchExpr')
semiDesExpr _ (RawCase _ _ ((Pattern _ ElseGuard, _):_)) = error "Can't use elseguard in match expr"
semiDesExpr _ (RawCase _ _ []) = error "Empty case"
semiDesExpr obj (RawCase _ _ [(_, matchExpr)]) = semiDesExpr obj matchExpr
semiDesExpr obj@(Just jobj) r@(RawCase m e ((Pattern firstObj firstGuard, firstExpr):restCases)) = (concat [[firstDecl, restDecl], subFG, subFE, subRE, subE], expr')
  where
    condName = "$" ++ take 6 (printf "%08x" (hash r))
    argName = condName ++ "-arg"
    ([], firstObjExpr') = semiDesExpr obj (eobjExpr firstObj)
    declObj = mkExprObj FunctionObj (H.toList $ exprAppliedVars $ eobjExpr jobj) [(Just argName, AliasExpr (HoleExpr emptyMetaN (HoleActive Nothing))firstObjExpr')] Nothing condName
    firstDecl = PSemiDecl (DeclLHS m (Pattern declObj firstGuard')) [] (Just firstExpr')
    (subFG, firstGuard') = semiDesGuard jobj firstGuard
    (subFE, firstExpr') = semiDesExpr obj firstExpr
    restDecl = PSemiDecl (DeclLHS m (Pattern declObj ElseGuard)) [] (Just restExpr')
    (subRE, restExpr') = semiDesExpr obj (RawCase m e restCases)
    (subE, e') = semiDesExpr obj e
    expr' = TupleApply m (emptyMetaM "app" m, Value (emptyMetaE "val" e) condName) (TupleArgIO (emptyMetaE "appArg" e') argName e')
semiDesExpr obj (RawList m []) = semiDesExpr obj (RawValue m "/Data/Nil")
semiDesExpr obj (RawList m (l:ls)) = semiDesExpr obj (RawTupleApply (emptyMetaM "listApp" (getExprMeta l)) (emptyMetaM "listBase" (getExprMeta l), RawValue m "/Data/Cons") [TupleArgIO (emptyMetaE "arg" l) "head" l, TupleArgIO (emptyMetaE "argRemaining" l) "tail" (RawList m ls)])
semiDesExpr _ _ = error "Unexpected semiDesExpr"

semiDesGuard :: PObject -> PGuard -> ([PSemiDecl], PSGuard)
semiDesGuard obj (IfGuard e) = (subE, IfGuard e')
  where (subE, e') = semiDesExpr (Just obj) e
semiDesGuard _ ElseGuard = ([], ElseGuard)
semiDesGuard _ NoGuard = ([], NoGuard)

declToObjArrow :: StatementEnv -> PSemiDecl -> DesObjectMapItem
declToObjArrow (inheritPath, inheritAnnots) (PSemiDecl (DeclLHS arrM (Pattern object guard)) annots expr) = (object', annots' ++ inheritAnnots, Just arrow)
  where
    object' = desObj True inheritPath UseRelativeName object

    argMetaMap = exprArgs $ eobjExpr object'
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

-- | Desugars statements that inherit a path from a main statement
desInheritingSubstatements :: StatementEnv -> Path -> [PStatementTree] -> (DesPrgm, [DesCompAnnot])
desInheritingSubstatements (inheritModule, inheritAnnots) path subStatements = (prgm', annots)
  where
    path' = case path of
      (Absolute p) -> p
      (Relative p) -> inheritModule ++ "/" ++ p
    statementEnv' = (path', inheritAnnots)
    (objectMap, classGraph, annots) = mergeExprPrgms $ map (desStatement statementEnv') subStatements
    prgm' = (objectMap, classGraph, []) -- Annots in subStatements are not global, but local to the main statement

-- | Parses an object from a 'MultiTypeDefData'
desMultiTypeDefObj :: String -> H.HashMap TypeVarName Type -> PExpr -> DesObject
desMultiTypeDefObj inheritPath varReplaceMap expr = desObj False inheritPath UseRelativeName $ ExprObject TypeObj Nothing expr''
  where
    ([], expr') = semiDesExpr Nothing expr

    -- This replaces references from obj vars of class vars
    -- Consider JOptional<$T> = Just<$T=$T> | Nothing
    -- For the object Just, it needs to be Just<TopType $T> as the classes $T is out of scope
    -- TODO: Consider adding back the following lines of code produced in experimental exprObject branch
    replaceMetaVar (ExprMeta ExprMetaApplyVarVal) (Meta t pos md) = Meta (substituteVarsWithVarEnv varReplaceMap t) pos md
    replaceMetaVar _ m = m
    expr'' = mapMetaAppliedExpr replaceMetaVar expr'


desMultiTypeDef :: StatementEnv -> PMultiTypeDef -> [RawStatementTree RawExpr ParseMetaDat] -> Path -> DesPrgm
desMultiTypeDef statementEnv@(inheritPath, _) (MultiTypeDef className classVars dataExprs) subStatements path = mergeExprPrgm (objMap', classGraph', []) subPrgm
    where
      path' =  case path of
        Absolute p -> p
        Relative p -> inheritPath ++ "/" ++ p
      objMap' = map (, [], Nothing) objs
      objPaths = map eobjPath objs
      dataTypes = map (either id (getExprType . snd . desObjPropagateTypes . snd . semiDesExpr Nothing) . eitherTypeVarExpr) dataExprs
      objs = map (desMultiTypeDefObj inheritPath classVars) $ rights $ map eitherTypeVarExpr dataExprs
      typeCGNodes = map (CGType, , []) objPaths
      classCGNode = (CGClass (True, classVars, dataTypes, desObjDocComment subStatements, path'), addPath inheritPath className, objPaths)
      classGraph' = ClassGraph $ graphFromEdges (classCGNode:typeCGNodes)
      (subPrgm, _) = desInheritingSubstatements statementEnv path subStatements

      eitherTypeVarExpr (RawValue _ n@('$':_)) = Left $ TypeVar $ TVVar n
      eitherTypeVarExpr e                      = Right e


desClassDecl :: StatementEnv -> RawClassDecl -> [RawStatementTree RawExpr ParseMetaDat] -> Path -> DesPrgm
desClassDecl statementEnv@(inheritPath, _) (className, classVars) subStatements path = mergeExprPrgm ([], classGraph', []) subPrgm
  where
    classGraph' = ClassGraph $ graphFromEdges [(CGClass (False, classVars, [], desObjDocComment subStatements, path'), addPath inheritPath className, [])]
    path' = case path of
      Relative p -> inheritPath ++ "/" ++ p
      Absolute p -> p
    (subPrgm, _) = desInheritingSubstatements statementEnv path subStatements

desTypeDef :: StatementEnv -> PTypeDef -> [RawStatementTree RawExpr ParseMetaDat] -> DesPrgm
desTypeDef statementEnv@(inheritPath, _) (TypeDef typeExpr) subStatements = mergeExprPrgm (objMap, emptyClassGraph, []) subPrgm
  where
    (subPrgm, annots) = desInheritingSubstatements statementEnv (getPath $ exprPath typeExpr) subStatements
    typeExpr' = snd $ semiDesExpr Nothing typeExpr
    obj = desObj False inheritPath UseRelativeName $ ExprObject TypeObj (desObjDocComment subStatements) typeExpr'
    objMap = [(obj, annots, Nothing)]

desClassDef :: StatementEnv -> Sealed -> RawClassDef ParseMetaDat -> [RawStatementTree RawExpr ParseMetaDat] -> Path -> DesPrgm
desClassDef statementEnv@(inheritPath, _) sealed (typeExpr, className) subStatements path = mergeExprPrgm ([], classGraph, []) subPrgm
  where
    path' =  case path of
      Absolute p -> p
      Relative p -> inheritPath ++ "/" ++ p
    (_, typeExpr') = desObjPropagateTypes $ snd $ semiDesExpr Nothing typeExpr
    classGraph = ClassGraph $ graphFromEdges [(CGClass (sealed, H.empty, [getExprType typeExpr'], desObjDocComment subStatements, path'), className, [exprPath typeExpr'])]
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
  RawAnnot a -> mergeExprPrgms $ map (desStatement (inheritModule, desGlobalAnnot a:inheritAnnots)) subStatements
  RawModule _ path -> fst $ desInheritingSubstatements statementEnv path subStatements

finalPasses :: DesPrgmGraphData -> GraphNodes DesPrgm String -> GraphNodes FinalDesPrgm String
finalPasses (desPrgmGraph, nodeFromVertex, vertexFromKey) (prgm1, prgmName, imports) = (prgm4, prgmName, imports)
  where
    -- Build fullPrgm with recursive imports
    vertex = fromJust $ vertexFromKey prgmName
    importTree = reachable desPrgmGraph vertex
    fullPrgm1 = mergeExprPrgms $ map (fst3 . nodeFromVertex) importTree

    prgm2 = fromExprPrgm prgm1
    fullPrgm2 = fromExprPrgm fullPrgm1

    -- Run resolveRelativeNames pass
    prgm3 = resolveRelativeNames fullPrgm2 prgm2
    fullPrgm3 = resolveRelativeNames fullPrgm2 fullPrgm2

    -- Run expandDataReferences pass
    prgm4 = expandDataReferences fullPrgm3 prgm3

desPrgm :: PPrgm -> CRes DesPrgm
desPrgm (_, statements) = return $ mergeExprPrgms $ map (desStatement ("", [])) statements

desFiles :: PPrgmGraphData -> CRes FinalDesPrgmGraphData
desFiles graphData = do
  -- initial desugar
  prgms' <- mapMFst3 desPrgm (graphToNodes graphData)
  let graphData' = graphFromEdges prgms'

  -- final passes
  let prgms'' = map (finalPasses graphData') prgms'
  return $ graphFromEdges prgms''
