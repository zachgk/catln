--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Desugarf.Nested
-- Copyright :  (c) Zach Kimberg 2022
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module flattens nested declarations
--------------------------------------------------------------------

module Syntax.Ct.Desugarf.Nested where

import           Data.Bifunctor          (first, second)
import qualified Data.HashMap.Strict     as H
import qualified Data.HashSet            as S
import           Text.Printf

import           Constants
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm

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
desObjDocComment ((RawStatementTree (RawAnnot annotExpr) _):rest) | maybeExprPath annotExpr == Just mdAnnot = Just (++) <*> Just annotText <*> desObjDocComment rest
  where
    (Just (_, Just (RawCExpr _ (CStr annotText)))) = H.lookup mdAnnotText $ exprAppliedArgsMap annotExpr
desObjDocComment _ = Just ""
