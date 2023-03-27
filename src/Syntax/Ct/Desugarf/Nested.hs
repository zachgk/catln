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

type ParentArgs = H.HashMap ArgName [ParseMeta]

splitDeclSubStatements :: [PStatementTree] -> ([(PObjArr, [PStatementTree])], [PCompAnnot])
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
scopeSubDeclFunNames :: PSObjArr -> [PSemiDecl] -> (PSObjArr, [PSemiDecl])
scopeSubDeclFunNames oa@ObjArr{oaObj=Just (GuardExpr objExpression _), oaM, oaAnnots, oaArr} decls = (oa{oaM=arrM', oaAnnots=annots', oaArr=expr'}, decls')
  where
    prefix = exprPath objExpression
    declNames = S.fromList $ map (\(PSemiDecl ObjArr{oaObj=Just (GuardExpr o _)}) -> exprPath o) decls
    addPrefix n = prefix ++ "." ++ n
    scopeM = scopeSubDeclFunNamesInMeta prefix declNames
    arrM' = scopeM oaM
    decls' = map (\(PSemiDecl doa@ObjArr{oaM=m, oaObj=Just (GuardExpr obj guard), oaArr=doaArr}) -> PSemiDecl doa{
                     oaM=scopeM m,
                     oaObj=Just (GuardExpr (mapExprPath (\(pM, pN) -> Value pM (addPrefix pN)) obj) guard),
                     oaArr = fmap (\(GuardExpr e g) -> GuardExpr (scopeSubDeclFunNamesInExpr prefix declNames e) g) doaArr
                     }) decls
    expr' = case oaArr of
      Just (GuardExpr e g) -> Just (GuardExpr (scopeSubDeclFunNamesInExpr prefix declNames e) g)
      Nothing -> Nothing
    annots' = map (scopeSubDeclFunNamesInExpr prefix declNames) oaAnnots
scopeSubDeclFunNames oa _ = error $ printf "scopeSubDeclFunNames without input expression: %s" (show oa)

-- | Apply args to a signature or input expression
curryApplyParentArgsSignature :: PSExpr -> ParentArgs -> PSExpr
curryApplyParentArgsSignature e parentArgs = applyExprIArgs e (map (second IArgM) $ H.toList $ fmap head parentArgs)

-- | Apply args to an output expression
curryApplyParentArgs :: PSExpr -> ParentArgs -> PSExpr
curryApplyParentArgs e parentArgs = applyExprIArgs e (map (\(parentArgName, parentArgM) -> (parentArgName, IArgE (Value parentArgM parentArgName))) $ H.toList $ fmap head parentArgs)

currySubFunctionSignature :: ParentArgs -> PSemiDecl -> PSemiDecl
currySubFunctionSignature parentArgs (PSemiDecl oa@ObjArr{oaObj=Just (GuardExpr obj guard)}) = PSemiDecl oa{oaObj=Just (GuardExpr obj' guard)}
  where
    obj' = mapExprPath applyParentArgsToObjPath obj
    applyParentArgsToObjPath (objPathM, objPathN) = curryApplyParentArgsSignature (Value objPathM objPathN) parentArgs
currySubFunctionSignature _ d = error $ printf "Invalid currySubFunctionSignature without input expression: %s" (show d)

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

currySubFunctions :: PSObjArr -> [PSemiDecl] -> (PSObjArr, [PSemiDecl])
currySubFunctions oa@ObjArr{oaObj=Just (GuardExpr objExpression _), oaAnnots, oaArr} decls = (oa{oaAnnots=annots', oaArr=expr'}, decls')
  where
    parentArgs = exprArgs objExpression
    toUpdate = S.fromList $ map (\(PSemiDecl ObjArr{oaObj=Just (GuardExpr o _)}) -> exprPath o) decls
    decls2 = map (currySubFunctionSignature parentArgs) decls
    expr' = case oaArr of
      Just (GuardExpr e g) -> Just $ GuardExpr (currySubFunctionsUpdateExpr toUpdate parentArgs e) g
      Nothing -> Nothing
    decls' = map (\(PSemiDecl doa@ObjArr{oaArr=doaArr}) -> PSemiDecl doa{oaArr=fmap (currySubFunctionsUpdateGuardExpr toUpdate parentArgs) doaArr}) decls2
    currySubFunctionsUpdateGuardExpr u p (GuardExpr e g) = GuardExpr (currySubFunctionsUpdateExpr u p e) g
    annots' = map (currySubFunctionsUpdateExpr toUpdate parentArgs) oaAnnots
currySubFunctions oa _ = error $ printf "currySubFunctions without input expression: %s" (show oa)

desObjDocComment :: [PStatementTree] -> Maybe String
desObjDocComment ((RawStatementTree (RawAnnot annotExpr) _):rest) | maybeExprPath annotExpr == Just mdAnnot = Just (++) <*> Just annotText <*> desObjDocComment rest
  where
    (Just (_, Just (RawCExpr _ (CStr annotText)))) = H.lookup mdAnnotText $ exprAppliedArgsMap annotExpr
desObjDocComment _ = Just ""
