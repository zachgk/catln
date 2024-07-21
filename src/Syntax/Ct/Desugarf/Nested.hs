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

import           Data.Bifunctor          (bimap, first, second)
import qualified Data.HashMap.Strict     as H
import qualified Data.HashSet            as S
import           Text.Printf

import           CtConstants
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Builder
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm

type ParentArgs = H.HashMap ArgName [ParseMeta]

scopeSubDeclFunNamesInS :: TypeName -> S.HashSet TypeName -> TypeName -> TypeName
scopeSubDeclFunNamesInS prefix replaceNames name = name'
  where
    addPrefix n = prefix ++ "." ++ n
    name' = if S.member name replaceNames then addPrefix name else name

scopeSubDeclFunNamesInExpr :: TypeName -> S.HashSet TypeName -> PSExpr -> PSExpr
scopeSubDeclFunNamesInExpr _ _ e@CExpr{} = e
scopeSubDeclFunNamesInExpr prefix replaceNames (Value m name) = Value (scopeSubDeclFunNamesInMeta prefix replaceNames m) $ scopeSubDeclFunNamesInS prefix replaceNames name
scopeSubDeclFunNamesInExpr _ _ e@HoleExpr{} = e
scopeSubDeclFunNamesInExpr prefix replaceNames (AliasExpr b a) = AliasExpr (scopeSubDeclFunNamesInExpr prefix replaceNames b) (scopeSubDeclFunNamesInExpr prefix replaceNames a)
scopeSubDeclFunNamesInExpr prefix replaceNames (EWhere m b a) = EWhere (scopeSubDeclFunNamesInMeta prefix replaceNames m) (scopeSubDeclFunNamesInExpr prefix replaceNames b) (scopeSubDeclFunNamesInExpr prefix replaceNames a)
scopeSubDeclFunNamesInExpr prefix replaceNames (TupleApply m (bm, bExpr) arg) = TupleApply m (bm, bExpr') arg'
  where
    bExpr' = scopeSubDeclFunNamesInExpr prefix replaceNames bExpr
    arg' = case arg of
      EAppArg a -> EAppArg $ mapTupleArgValue (scopeSubDeclFunNamesInExpr prefix replaceNames) a
      EAppSpread a -> error $ printf "Not yet implemented %s" (show a)
scopeSubDeclFunNamesInExpr prefix replaceNames (VarApply m bExpr varName varVal) = VarApply m bExpr' varName varVal
  where
    bExpr' = scopeSubDeclFunNamesInExpr prefix replaceNames bExpr

scopeSubDeclFunNamesInMeta :: TypeName -> S.HashSet TypeName -> ParseMeta -> ParseMeta
scopeSubDeclFunNamesInMeta prefix replaceNames (Meta (UnionType partials) pos md) = Meta (UnionType partials') pos md
  where
    scopeS = scopeSubDeclFunNamesInS prefix replaceNames
    partials' = H.mapKeys scopeS partials
scopeSubDeclFunNamesInMeta _ _ m@(Meta PTopType _ _) = m
scopeSubDeclFunNamesInMeta prefix replaceNames (Meta (TopType ps preds) pos md) = Meta (TopType ps preds') pos md
  where
    preds' = mapTypePreds scopePred preds
    scopePred (PredRel p@PartialType{ptName}) = PredRel p{ptName=scopeSubDeclFunNamesInS prefix replaceNames ptName}
    scopePred p = p
scopeSubDeclFunNamesInMeta _ _ m@(Meta TypeVar{} _ _) = m

-- Renames sub functions by applying the parent names as a prefix to avoid name collisions
scopeSubDeclFunNames :: PSObjArr -> DesObjectMap -> (PSObjArr, DesObjectMap)
scopeSubDeclFunNames oa@ObjArr{oaObj=Just objExpression, oaAnnots, oaArr} decls = (oa{oaAnnots=annots', oaArr=oaArr'}, decls')
  where
    prefix = exprPath objExpression
    declNames = S.fromList $ map oaObjPath decls
    addPrefix n = prefix ++ "." ++ n
    scopeM = scopeSubDeclFunNamesInMeta prefix declNames
    decls' = map (\doa@ObjArr{oaObj=Just obj, oaArr=Just (doaArr, m)} -> doa{
                     oaObj=Just (mapExprPath (\(pM, pN) -> Value (scopeSubDeclFunNamesInMeta prefix (S.singleton pN) pM) (addPrefix pN)) obj),
                     oaArr = Just (fmap (scopeSubDeclFunNamesInExpr prefix declNames) doaArr, scopeM m)
                     }) decls
    oaArr' = fmap (bimap (fmap (scopeSubDeclFunNamesInExpr prefix declNames)) scopeM) oaArr
    annots' = map (scopeSubDeclFunNamesInExpr prefix declNames) oaAnnots
scopeSubDeclFunNames oa _ = error $ printf "scopeSubDeclFunNames without input expression: %s" (show oa)

-- | Apply args to a signature or input expression
curryApplyParentArgsSignature :: PSExpr -> ParentArgs -> PSExpr
curryApplyParentArgsSignature e parentArgs = applyExprIArgs e (map (second IArgM) $ H.toList $ fmap (const Nothing) parentArgs)

-- | Apply args to an output expression
curryApplyParentArgs :: PSExpr -> ParentArgs -> PSExpr
curryApplyParentArgs e parentArgs = applyExprIArgs e (map (\(parentArgName, parentArgM) -> (parentArgName, IArgE (Value (emptyMetaM "nest" parentArgM) (pkName parentArgName)))) $ H.toList $ fmap head parentArgs)

currySubFunctionSignature :: ParentArgs -> DesObjectMapItem -> DesObjectMapItem
currySubFunctionSignature parentArgs oa@ObjArr{oaObj=Just obj} = oa{oaObj=Just obj'}
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
currySubFunctionsUpdateExpr _ _ e@HoleExpr{} = e
currySubFunctionsUpdateExpr toUpdate parentArgs (AliasExpr b a) = AliasExpr (currySubFunctionsUpdateExpr toUpdate parentArgs b) (currySubFunctionsUpdateExpr toUpdate parentArgs a)
currySubFunctionsUpdateExpr toUpdate parentArgs (EWhere m b a) = EWhere m (currySubFunctionsUpdateExpr toUpdate parentArgs b) (currySubFunctionsUpdateExpr toUpdate parentArgs a)
currySubFunctionsUpdateExpr toUpdate parentArgs (TupleApply tm (tbm, tbe) targ) = TupleApply tm (tbm, tbe') targ'
  where
    tbe' = currySubFunctionsUpdateExpr toUpdate parentArgs tbe
    targ' = case targ of
      EAppArg a -> EAppArg $ mapTupleArgValue (currySubFunctionsUpdateExpr toUpdate parentArgs) a
      EAppSpread a -> error $ printf "Not yet implemented: %s" (show a)
currySubFunctionsUpdateExpr toUpdate parentArgs (VarApply tm tbe tVarName tVarVal) = VarApply tm tbe' tVarName tVarVal
  where
    tbe' = currySubFunctionsUpdateExpr toUpdate parentArgs tbe

currySubFunctions :: PSObjArr -> DesObjectMap -> (PSObjArr, DesObjectMap)
currySubFunctions oa@ObjArr{oaObj=Just objExpression, oaAnnots, oaArr} decls = (oa{oaAnnots=annots', oaArr=oaArr'}, decls')
  where
    parentArgs = map snd <$> exprArgs objExpression
    toUpdate = S.fromList $ map oaObjPath decls
    decls2 = map (currySubFunctionSignature parentArgs) decls
    oaArr' = fmap (first (fmap (currySubFunctionsUpdateExpr toUpdate parentArgs))) oaArr
    decls' = map (\doa@ObjArr{oaArr=doaArr} -> doa{oaArr=fmap (first (fmap (currySubFunctionsUpdateExpr toUpdate parentArgs))) doaArr}) decls2
    annots' = map (currySubFunctionsUpdateExpr toUpdate parentArgs) oaAnnots
currySubFunctions oa _ = error $ printf "currySubFunctions without input expression: %s" (show oa)

desObjDocComment :: [PStatementTree] -> Maybe String
desObjDocComment ((RawStatementTree (RawAnnot annotExpr) _):rest) | maybeExprPath annotExpr == Just mdAnnot = Just (++) <*> Just annotText <*> desObjDocComment rest
  where
    (Just (Just (_, Just (RawCExpr _ (CStr annotText))))) = H.lookup (partialKey mdAnnotText) $ rawExprAppliedArgsMap annotExpr
desObjDocComment _ = Just ""
