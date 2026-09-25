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

import           Data.Bifunctor          (bimap, first)
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
    addPrefix n = mkName (nameStr prefix ++ "." ++ nameStr n)
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
      a@EAppVar{} -> a
      EAppSpread a -> error $ printf "Not yet implemented %s" (show a)

scopeSubDeclFunNamesInMeta :: TypeName -> S.HashSet TypeName -> ParseMeta -> ParseMeta
scopeSubDeclFunNamesInMeta prefix replaceNames (Meta (UnionType Nothing partials []) pos mid md) = Meta (UnionType Nothing partials' []) pos mid md
  where
    scopeS = scopeSubDeclFunNamesInS prefix replaceNames
    partials' = H.mapKeys scopeS partials
scopeSubDeclFunNamesInMeta _ _ m@Meta{getMetaType=PTopType} = m
scopeSubDeclFunNamesInMeta prefix replaceNames (Meta (TopType ps preds) pos mid md) = Meta (TopType ps preds') pos mid md
  where
    preds' = mapTypePreds scopePred preds
    scopePred (PredRel p@PartialType{ptName}) = PredRel p{ptName=scopeSubDeclFunNamesInS prefix replaceNames ptName}
    scopePred p = p
scopeSubDeclFunNamesInMeta _ _ m@Meta{getMetaType=TypeVar{}} = m
scopeSubDeclFunNamesInMeta _ _ m = m

-- Renames sub functions by applying the parent names as a prefix to avoid name collisions
scopeSubDeclFunNames :: PSObjArr -> DesObjectMap -> (PSObjArr, DesObjectMap)
scopeSubDeclFunNames oa@ObjArr{oaObj=Just objExpression, oaAnnots, oaArr} decls = (oa{oaAnnots=annots', oaArr=oaArr'}, objectMapFromList decls')
  where
    prefix = exprPath objExpression
    declNames = S.fromList $ map oaObjPath $ flatObjectMap decls
    addPrefix n = mkName (nameStr prefix ++ "." ++ nameStr n)
    scopeM = scopeSubDeclFunNamesInMeta prefix declNames
    scopeDecl doa@ObjArr{oaObj=Just obj, oaArr=Just (doaArr, m)} = doa{
      oaObj=Just (mapExprPath (\(pM, pN) -> Value (scopeSubDeclFunNamesInMeta prefix (S.singleton pN) pM) (addPrefix pN)) obj),
      oaArr = Just (fmap (scopeSubDeclFunNamesInExpr prefix declNames) doaArr, scopeM m)
    }
    scopeDecl _ = error "Unexpected scopeDecl"
    decls' = map scopeDecl $ flatObjectMap decls
    oaArr' = fmap (bimap (fmap (scopeSubDeclFunNamesInExpr prefix declNames)) scopeM) oaArr
    annots' = map (scopeSubDeclFunNamesInExpr prefix declNames) oaAnnots
scopeSubDeclFunNames oa _ = error $ printf "scopeSubDeclFunNames without input expression: %s" (show oa)

-- | Apply args to a signature or input expression. Each parent argument is curried into the sub
-- function's signature carrying the parent's declared type meta. Discarding it (leaving the parameter
-- 'TopType') lets a parent argument a sub function captures but does not use leak 'TopType' into the
-- file's object union, which fails type checking non-deterministically.
curryApplyParentArgsSignature :: PSExpr -> ParentArgs -> PSExpr
curryApplyParentArgsSignature e parentArgs = foldl addParentArg e (H.toList parentArgs)
  where
    addParentArg b (parentArgName, parentArgMetas) = TupleApply (emptyMetaE b) (emptyMetaE b, b) (EAppArg paramObjArr)
      where
        paramObjArr = ObjArr (Just (Value emptyMetaN (pkName parentArgName))) ArgObj Nothing [] (Just (Nothing, head parentArgMetas))

-- | Apply args to an output expression
curryApplyParentArgs :: PSExpr -> ParentArgs -> PSExpr
curryApplyParentArgs e parentArgs = applyExprIArgs e (map (\(parentArgName, parentArgM) -> (parentArgName, IArgE (Value (emptyMetaM parentArgM) (pkName parentArgName)))) $ H.toList $ fmap head parentArgs)

currySubFunctionSignature :: ParentArgs -> DesObjectMapItem -> DesObjectMapItem
currySubFunctionSignature parentArgs oa@ObjArr{oaObj=Just obj} = oa{oaObj=Just obj'}
  where
    obj' = mapExprPath applyParentArgsToObjPath obj
    applyParentArgsToObjPath (objPathM, objPathN) = curryApplyParentArgsSignature (Value objPathM objPathN) parentArgs
currySubFunctionSignature _ d = error $ printf "Invalid currySubFunctionSignature without input expression: %s" (show d)

-- | Curries parent args into a reference to a sub function. 'parentArgsFor' returns the parent args
-- to apply for a given referenced name, so a parent arg shadowed by the sub function's own parameter
-- is not applied (which would double-apply that argument).
currySubFunctionsUpdateExpr :: S.HashSet TypeName -> (TypeName -> ParentArgs) -> PSExpr -> PSExpr
currySubFunctionsUpdateExpr _ _ c@CExpr{} = c
currySubFunctionsUpdateExpr toUpdate parentArgsFor v@(Value _ vn) = if any (relativeNameMatches vn) (S.toList toUpdate)
  then curryApplyParentArgs v (parentArgsFor vn)
  else v
currySubFunctionsUpdateExpr _ _ e@HoleExpr{} = e
currySubFunctionsUpdateExpr toUpdate parentArgsFor (AliasExpr b a) = AliasExpr (currySubFunctionsUpdateExpr toUpdate parentArgsFor b) (currySubFunctionsUpdateExpr toUpdate parentArgsFor a)
currySubFunctionsUpdateExpr toUpdate parentArgsFor (EWhere m b a) = EWhere m (currySubFunctionsUpdateExpr toUpdate parentArgsFor b) (currySubFunctionsUpdateExpr toUpdate parentArgsFor a)
currySubFunctionsUpdateExpr toUpdate parentArgsFor (TupleApply tm (tbm, tbe) targ) = TupleApply tm (tbm, tbe') targ'
  where
    tbe' = currySubFunctionsUpdateExpr toUpdate parentArgsFor tbe
    targ' = case targ of
      EAppArg a -> EAppArg $ mapTupleArgValue (currySubFunctionsUpdateExpr toUpdate parentArgsFor) a
      a@EAppVar{} -> a
      EAppSpread a -> error $ printf "Not yet implemented: %s" (show a)

currySubFunctions :: PSObjArr -> DesObjectMap -> (PSObjArr, DesObjectMap)
currySubFunctions oa@ObjArr{oaObj=Just objExpression, oaAnnots, oaArr} decls = (oa{oaAnnots=annots', oaArr=oaArr'}, objectMapFromList decls')
  where
    parentArgs = map snd <$> exprArgs objExpression
    toUpdate = S.fromList $ map oaObjPath $ flatObjectMap decls
    -- Each sub function's own parameter names (unioned across its signature and arms), so a parent
    -- arg the sub function itself declares (e.g. a parent @range(stop)@ and a helper
    -- @rangeFrom(i, stop)@ that share @stop@) is not curried into it and double-applied.
    subOwnArgs = H.fromListWith S.union [(oaObjPath d, H.keysSet $ exprArgs (oaObjExpr d)) | d <- flatObjectMap decls]
    -- Only the parent args a sub function reads are curried into it: a sub function closes over
    -- what it refers to, not over everything in scope where it is written. The rest would be
    -- arguments of every call of it that nothing reads, and an argument is part of the type the
    -- call is entered at -- so @toString@'s @restToString@, which reads only the list it is
    -- handed, carried the whole outer list as well, and no two calls of it on the same tail of
    -- different lists were the same call.
    --
    -- Closed over the sibling sub functions it calls, since a call of a sibling is curried with
    -- the sibling's own parent args ('currySubFunctionsUpdateExpr'), which then have to be in
    -- scope where the call is written.
    parentArgsFor n = case [needs | (k, needs) <- H.toList subNeeds, relativeNameMatches n k] of
      (needs:_) -> H.filterWithKey (\a _ -> S.member a needs) parentArgs
      []        -> parentArgs
    subNeeds = subParentArgNeeds parentArgs subOwnArgs (flatObjectMap decls)
    decls2 = map (\d -> currySubFunctionSignature (parentArgsFor (oaObjPath d)) d) $ flatObjectMap decls
    oaArr' = fmap (first (fmap (currySubFunctionsUpdateExpr toUpdate parentArgsFor))) oaArr
    decls' = map (\doa@ObjArr{oaArr=doaArr} -> doa{oaArr=fmap (first (fmap (currySubFunctionsUpdateExpr toUpdate parentArgsFor))) doaArr}) decls2
    annots' = map (currySubFunctionsUpdateExpr toUpdate parentArgsFor) oaAnnots
currySubFunctions oa _ = error $ printf "currySubFunctions without input expression: %s" (show oa)

-- | The parent args each sub function reads, keyed by the sub function's name: the ones its arms,
-- guards and annotations refer to, and the ones of every sibling it calls, less its own parameters.
-- A fixed point, since siblings may call each other.
subParentArgNeeds :: ParentArgs -> H.HashMap TypeName (S.HashSet ArgName) -> [DesObjectMapItem] -> H.HashMap TypeName (S.HashSet ArgName)
subParentArgNeeds parentArgs subOwnArgs decls = go direct
  where
    names = H.keys subOwnArgs
    refsOf = H.fromListWith (++) [(oaObjPath d, objArrRefs d) | d <- decls]
    readsArg ref a = relativeNameMatches ref (pkName a)
    own n = H.lookupDefault S.empty n subOwnArgs
    direct = H.mapWithKey (\n refs -> S.fromList [a | a <- H.keys parentArgs, not (S.member a (own n)), opaque n || any (`readsArg` a) refs]) refsOf
    -- A sub function whose code is not here to read -- one the runtime implements
    -- (@#runtime@), or one with no arm to evaluate at all -- is handed every parent arg: the
    -- runtime's implementation reads them by name (@debugExpr@ reads its parent's @/expr@), and
    -- what it reads cannot be seen from the page.
    opaque n = S.member n opaqueNames
    opaqueNames = S.fromList [n | n <- H.keys refsOf, let ds = [d | d <- decls, oaObjPath d == n], any isRuntime ds || not (any hasBody ds)]
    isRuntime ObjArr{oaAnnots} = any (\a -> maybe False (`relativeNameMatches` mkName runtimeAnnot) (maybeExprPath a)) oaAnnots
    hasBody ObjArr{oaArr=Just (Just _, _)} = True
    hasBody _                              = False
    callees n = [k | ref <- H.lookupDefault [] n refsOf, k <- names, relativeNameMatches ref k]
    go needs
      | needs' == needs = needs
      | otherwise = go needs'
      where
        needs' = H.mapWithKey (\n ns -> S.filter (\a -> not (S.member a (own n))) $ S.unions (ns : [H.lookupDefault S.empty c needs | c <- callees n])) needs

-- | The names a definition refers to: what its arms evaluate, the guards it is matched under, and
-- its annotations. The names it binds -- its own parameters and pattern fields, and the keys of
-- the arguments it applies -- are not references.
objArrRefs :: DesObjectMapItem -> [TypeName]
objArrRefs ObjArr{oaObj, oaArr, oaAnnots} = maybe [] exprGuardRefs oaObj ++ maybe [] (maybe [] exprRefs . fst) oaArr ++ concatMap exprRefs oaAnnots

-- | The names an expression refers to. An applied argument contributes its value, not its key.
exprRefs :: PSExpr -> [TypeName]
exprRefs CExpr{}                 = []
exprRefs (Value _ n)             = [n]
exprRefs HoleExpr{}              = []
exprRefs (AliasExpr b a)         = exprRefs b ++ exprRefs a
exprRefs (EWhere _ b c)          = exprRefs b ++ exprRefs c
exprRefs (TupleApply _ (_, b) a) = exprRefs b ++ case a of
  EAppArg ObjArr{oaArr} -> maybe [] (maybe [] exprRefs . fst) oaArr
  EAppVar{}             -> []
  EAppSpread e          -> exprRefs e

-- | The names an object pattern refers to, which are those in its guards: the pattern itself only
-- binds.
exprGuardRefs :: PSExpr -> [TypeName]
exprGuardRefs (EWhere _ b c)          = exprGuardRefs b ++ exprRefs c
exprGuardRefs (AliasExpr b a)         = exprGuardRefs b ++ exprGuardRefs a
exprGuardRefs (TupleApply _ (_, b) a) = exprGuardRefs b ++ case a of
  EAppArg ObjArr{oaArr} -> maybe [] (maybe [] exprGuardRefs . fst) oaArr
  EAppSpread e          -> exprGuardRefs e
  EAppVar{}             -> []
exprGuardRefs _                       = []

desObjDocComment :: [PStatementTree] -> Maybe String
desObjDocComment ((RawStatementTree (RawAnnot annotExpr) _):rest) | maybeExprPath annotExpr == Just (mkName mdAnnot) = case H.lookup (partialKey (mkName mdAnnotText)) $ rawExprAppliedArgsMap annotExpr of
  (Just (Just (_, Just (RawCExpr _ (CStr annotText))))) -> Just (++) <*> Just annotText <*> desObjDocComment rest
  _ -> error "Unexpected in desObjDocComment"
desObjDocComment _ = Just ""
