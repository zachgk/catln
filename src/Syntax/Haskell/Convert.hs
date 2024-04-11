--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Haskell.Convert
-- Copyright :  (c) Zach Kimberg 2023
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines converting from HsModule to a RawPrgm
--------------------------------------------------------------------

module Syntax.Haskell.Convert where
import           Bag                     (bagToList)
import           BasicTypes
import           CtConstants
import           Data.Bifunctor          (first)
import           Data.Maybe              (fromJust, fromMaybe, mapMaybe,
                                          maybeToList)
import           DynFlags
import           FastString              (unpackFS)
import           GHC.Hs
import           Module
import           Name                    (nameStableString)
import           OccName
import           Outputable
import           RdrName
import           Semantics.Prgm
import           Semantics.Types
import           SrcLoc
import           Syntax.Ct.Desugarf.Expr (exprToType)
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Text.Printf

type MLHSArgs = Maybe (RawExpr ()) -- Maybe base with lhs arguments, is present when a lambda modifies the base args

-- https://wiki.haskell.org/Import
-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-ImpExp.html#t:ImportDecl
convertImport :: DynFlags -> ImportDecl GhcPs -> RawFileImport
convertImport _ (ImportDecl _ _ name Nothing _ _ qualifiedStyle _ maybeQualifiedAlias _maybeHiding) = rawVal "haskell" `applyRawArgs` concat [moduleArg, qualifiedArg, aliasArg]
  where
    moduleArg = [(Nothing, RawCExpr emptyMetaN $ CStr $ moduleNameSlashes $ unLoc name)]
    qualifiedArg = [(Just $ partialKey "qualified", rawVal "True") | qualifiedStyle /= NotQualified]
    aliasArg = [(Just $ partialKey "alias", RawCExpr emptyMetaN $ CStr $ moduleNameSlashes $ unLoc qualifiedAlias) | qualifiedAlias <- maybeToList maybeQualifiedAlias]
    -- hidingArg = case maybeHiding of
    --   Just (hiding, names) -> _
convertImport flags p = error $ printf "Convert unsupported import:\n%s\n%s" (showSDoc flags $ ppr p) (showSDocDebug flags $ ppr p)

convertIdP :: DynFlags -> IdP GhcPs -> String
convertIdP _ (Unqual n) = occNameString n
convertIdP _ (Qual m n) = moduleNameSlashes m ++ occNameString n
convertIdP flags p@Orig{} = error $ printf "Convert unsupported IdP:\n%s" (showSDoc flags $ ppr p)
convertIdP _ (Exact n) = nameStableString n

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Types.html#t:HsType
convertTypeToExpr :: DynFlags -> Maybe (RawExpr ()) -> HsType GhcPs -> RawExpr ()
convertTypeToExpr flags _ p@HsForAllTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags i (HsQualTy _ ctx body) = foldl aux (convertTypeToExpr flags i $ unLoc body) (unLoc ctx)
  where
    aux :: RawExpr () -> Located (HsType GhcPs) -> RawExpr ()
    aux b cond = RawWhere b (convertTypeToExpr flags Nothing $ unLoc cond)
convertTypeToExpr flags (Just i) (HsTyVar _ _ v) = i `applyRawArgs` [(Just $ partialKey "A", RawValue emptyMetaN $ convertIdP flags $ unLoc v)]
convertTypeToExpr flags Nothing (HsTyVar _ _ v) = RawValue emptyMetaN (convertIdP flags $ unLoc v)
convertTypeToExpr flags i (HsAppTy _ base v) = RawVarsApply emptyMetaN (convertTypeToExpr flags i $ unLoc base) [(convertTypeToExpr flags i $ unLoc v, emptyMetaN)]
convertTypeToExpr flags _ p@HsAppKindTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags i (HsFunTy _ base app) = convertTypeToExpr flags i (unLoc base) `applyRawArgs` [(Just $ partialKey "B", convertTypeToExpr flags Nothing $ unLoc app)]
convertTypeToExpr flags mb (HsListTy _ t) = case mb of
  Just b  -> b `applyRawArgs` [(Nothing, t')]
  Nothing -> t'
  where
    t' = rawVal ctListType `applyRawExprVars` [(partialKey "T", emptyMetaT $ exprToType $ convertTypeToExpr flags Nothing $ unLoc t)]
convertTypeToExpr flags _ (HsTupleTy _ _ tp) = rawVal "" `applyRawArgs` map ((Just $ partialKey "C",) . convertTypeToExpr flags Nothing . unLoc) tp
convertTypeToExpr flags _ p@HsSumTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsOpTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags i (HsParTy _ t) = convertTypeToExpr flags i $ unLoc t
convertTypeToExpr flags _ p@HsIParamTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsStarTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsKindSig{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsSpliceTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsDocTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags i (HsBangTy _ _ t) = convertTypeToExpr flags i (unLoc t)
convertTypeToExpr flags _ p@HsRecTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsExplicitListTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsExplicitTupleTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsTyLit{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsWildCardTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@XHsType{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Types.html#t:HsType
convertTypeToObj :: DynFlags -> String -> HsType GhcPs -> RawObjArr RawExpr ()
convertTypeToObj flags _ p@HsForAllTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@HsAppKindTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags i (HsFunTy _ base app) = RawObjArr (Just (convertTypeToExpr flags (Just $ RawValue emptyMetaN i) $ unLoc base)) FunctionObj Nothing [] (Just (Nothing, emptyMetaT $ exprToType $ convertTypeToExpr flags Nothing $ unLoc app)) Nothing
convertTypeToObj flags _ p@HsSumTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@HsOpTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@HsParTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@HsIParamTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@HsStarTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@HsKindSig{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@HsSpliceTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@HsDocTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@HsBangTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@HsRecTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@HsExplicitListTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@HsExplicitTupleTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@HsTyLit{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@HsWildCardTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@XHsType{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags i p = RawObjArr (Just (convertTypeToExpr flags (Just $ RawValue emptyMetaN i) p)) FunctionObj Nothing [] Nothing Nothing -- Fallback to using expr obj

convertExprRecordFields :: DynFlags -> RawExpr () -> HsRecFields GhcPs (LHsExpr GhcPs) -> RawExpr ()
convertExprRecordFields flags base (HsRecFields fields Nothing) = foldl convertRecordField base (map unLoc fields)
  where
    convertRecordField :: RawExpr () -> HsRecField' (FieldOcc GhcPs) (LHsExpr GhcPs) -> RawExpr ()
    convertRecordField b (HsRecField lbl arg _) = b `applyRawArgs` [(Just $ partialKey $ convertIdP flags $ unLoc $ rdrNameFieldOcc $ unLoc lbl, convertSimpleExpr "convertExprRecordFields" flags $ unLoc arg)]
convertExprRecordFields flags _ p = error $ printf "Convert unsupported record fields:\n%s" (showSDoc flags $ ppr p)

convertPatRecordFields :: DynFlags -> RawExpr () -> HsRecFields GhcPs (LPat GhcPs) -> RawExpr ()
convertPatRecordFields flags base (HsRecFields fields Nothing) = foldl convertRecordField base (map unLoc fields)
  where
    convertRecordField :: RawExpr () -> HsRecField' (FieldOcc GhcPs) (LPat GhcPs) -> RawExpr ()
    convertRecordField b (HsRecField lbl arg _) = b `applyRawArgs` [(Just $ partialKey $ convertIdP flags $ unLoc $ rdrNameFieldOcc $ unLoc lbl, convertPattern flags Nothing $ unLoc arg)]
convertPatRecordFields flags _ p = error $ printf "Convert unsupported record fields:\n%s" (showSDoc flags $ ppr p)

convertSignature :: DynFlags -> Sig GhcPs -> [RawStatementTree RawExpr ()]
convertSignature flags (TypeSig _ ids (HsWC _ (HsIB _ ldef))) = map (aux (unLoc ldef) . unLoc) ids
  where
    aux def i = RawStatementTree (RawDeclStatement $ convertTypeToObj flags (convertIdP flags i) def) []
convertSignature flags p@PatSynSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags (ClassOpSig _ _ names (HsIB _ t)) = map (aux . convertIdP flags . unLoc) names
  where
    aux n = RawStatementTree (RawDeclStatement $ convertTypeToObj flags n (unLoc t)) []
convertSignature flags p@IdSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags p@FixSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags p@InlineSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags p@SpecSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags p@SpecInstSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags p@MinimalSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags p@SCCFunSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags p@CompleteMatchSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags p@XSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags p = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)


-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Lit.html#t:HsOverLit
convertOverLit :: DynFlags -> HsOverLit GhcPs -> RawExpr ()
convertOverLit _ (OverLit _ (HsIntegral (IL _ _ c)) _) = RawCExpr emptyMetaN $ CInt c
convertOverLit _ (OverLit _ (HsFractional (FL _ _ c)) _) = RawCExpr emptyMetaN $ CFloat $ fromRational c
convertOverLit _ (OverLit _ (HsIsString _ s) _) = RawCExpr emptyMetaN $ CStr $ unpackFS s
convertOverLit flags l = error $ printf "Convert unsupported overlit:\n%s" (showSDoc flags $ ppr l)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Lit.html#t:HsLit
convertLiteral :: DynFlags -> HsLit GhcPs -> RawExpr ()
convertLiteral _ (HsString _ s) = RawCExpr emptyMetaN $ CStr $ unpackFS s
convertLiteral _ (HsChar _ c) = RawCExpr emptyMetaN $ CChar c
convertLiteral _ (HsCharPrim _ c) = RawCExpr emptyMetaN $ CChar c
convertLiteral flags l = error $ printf "Convert unsupported lit:\n%s" (showSDoc flags $ ppr l)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Types.html#t:HsConDetails
convertPatDetails :: DynFlags -> Maybe (RawExpr ()) -> HsConDetails (LPat GhcPs) (HsRecFields GhcPs (LPat GhcPs)) -> RawExpr ()
convertPatDetails _ Nothing (PrefixCon []) = RawHoleExpr emptyMetaN (HoleActive Nothing)
convertPatDetails flags base (PrefixCon args) = fromJust $ foldl (\b p -> Just $ convertPattern flags b p) base $ map unLoc args
convertPatDetails flags (Just base) (RecCon r) = convertPatRecordFields flags base r
convertPatDetails flags _ (InfixCon hd tl) = rawVal ctListCons`applyRawArgs` [(Just $ partialKey ctListConsHead, convertPattern flags Nothing $ unLoc hd), (Just $ partialKey ctListConsTail, convertPattern flags Nothing $ unLoc tl)]
convertPatDetails flags _ p = error $ printf "Convert unsupported conPatDetails:\n%s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Pat.html#t:Pat
convertPattern :: DynFlags -> Maybe (RawExpr ()) -> Pat GhcPs -> RawExpr ()
convertPattern _ _ WildPat{} = RawHoleExpr emptyMetaN $ HoleActive Nothing
convertPattern flags Nothing (VarPat _ v) = rawVal $ convertIdP flags $ unLoc v
convertPattern flags (Just b) (VarPat _ v) = b `applyRawArgs` [(Nothing, rawVal $ convertIdP flags $ unLoc v)]
convertPattern flags _ p@LazyPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags i (AsPat _ alias base) = RawAliasExpr (convertPattern flags i $ unLoc base) (rawVal $ convertIdP flags $ unLoc alias)
convertPattern flags base (ParPat _ p) = convertPattern flags base $ unLoc p
convertPattern flags _ p@BangPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags mb (ListPat _ lst) = case mb of
  Just b  -> b `applyRawArgs` [(Nothing, lst')]
  Nothing -> lst'
  where
    lst' = RawList emptyMetaN $ map (convertPattern flags Nothing . unLoc) lst
convertPattern flags mb (TuplePat _ pats _) = case mb of
  Just b  -> b `applyRawArgs` [(Nothing, tuple')]
  Nothing -> tuple'
  where
    tuple' = rawVal "" `applyRawArgs` map ((Nothing,) . convertPattern flags Nothing . unLoc) pats
convertPattern flags _ p@SumPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags (Just base) (ConPatIn i det) = base `applyRawArgs` [(Just $ partialKey $ convertIdP flags $ unLoc i, convertPatDetails flags (Just base) det)]
convertPattern flags Nothing (ConPatIn i det) = convertPatDetails flags (Just $ rawVal $ convertIdP flags $ unLoc i) det
convertPattern flags _ p@ConPatOut{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags _ p@ViewPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags _ p@SplicePat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags _ (LitPat _ lit) = convertLiteral flags lit
convertPattern flags _ (NPat _ n _ _) = convertOverLit flags $ unLoc n
convertPattern flags _ p@NPlusKPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags _ p@SigPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags _ p@CoPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags _ p@XPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)

convertSimpleExpr :: String -> DynFlags -> HsExpr GhcPs -> RawExpr ()
convertSimpleExpr msg flags e = case convertExpr flags Nothing e of
  (Nothing, e', []) -> e'
  (m', e', subs) -> error $ printf "convertSimpleExpr %s failed from %s to %s with match %s and subs %s" msg (showSDoc flags $ ppr e) (show e') (show m') (show subs)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Expr.html#t:HsExpr
convertExpr :: DynFlags -> Maybe (RawExpr ()) -> HsExpr GhcPs -> (MLHSArgs, RawExpr (), [RawStatementTree RawExpr ()])
convertExpr flags _ (HsVar _ v) = (Nothing, rawVal $ convertIdP flags $ unLoc v, [])
convertExpr flags _ p@HsUnboundVar{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsConLikeOut{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsRecFld{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsOverLabel{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsIPVar{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ (HsOverLit _ literal) = (Nothing, convertOverLit flags literal, [])
convertExpr flags _ (HsLit _ literal) = (Nothing, convertLiteral flags literal, [])
convertExpr flags b (HsLam _ (MG _ matches _)) = (base' >>= const (roaObj oa'), fromJust $ fst $ fromJust $ roaArr oa', subs)
  where
    [(base', oa', subs)] = concatMap (convertMatch flags b . unLoc) $ unLoc matches
convertExpr flags _ p@HsLamCase{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags eb (HsApp _ base app) = (baseMatch, baseE `applyRawEIArgs` [(fromMaybe (rawVal "") appBase, IArgE appE)], baseSubs ++ appSubs)
  where
    (baseMatch, baseE, baseSubs) = convertExpr flags eb (unLoc base)
    (appBase, appE, appSubs) = convertExpr flags (Just $ rawVal "") (unLoc app)
convertExpr flags _ p@HsAppType{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags eb (OpApp _ l op r) = (eb, convertSimpleExpr "opAppL" flags (unLoc op) `applyRawEIArgs` [(fromMaybe lKey lKey', IArgE le), (fromMaybe rKey rKey', IArgE re)], lSubs ++ rSubs)
  where
    lKey = rawVal operatorArgL
    rKey = rawVal operatorArgR
    (lKey', le, lSubs) = convertExpr flags (Just lKey) (unLoc l)
    (rKey', re, rSubs) = convertExpr flags (Just rKey) (unLoc r)
convertExpr flags base (NegApp _ e _) = (base', rawVal (operatorName "-") `applyRawArgs` [(Just $ partialKey operatorArgUnary, e')], subs')
  where
    (base', e', subs') = convertExpr flags base (unLoc e)
convertExpr flags base (HsPar _ p) = convertExpr flags base $ unLoc p
convertExpr flags (Just base) (SectionL _ fun arg) = (Just base', fun' `applyRawArgs` [(Nothing, arg'), sectionArg], [])
  where
    fun' = convertSimpleExpr "SectionRFun" flags $ unLoc fun
    arg' = convertSimpleExpr "SectionRArg" flags $ unLoc arg
    sectionArg = (Nothing, rawVal "sectionArg")
    base' = base `applyRawArgs` [sectionArg]
convertExpr flags (Just base) (SectionR _ fun arg) = (Just base', fun' `applyRawArgs` [sectionArg, (Nothing, arg')], [])
  where
    fun' = convertSimpleExpr "SectionRFun" flags $ unLoc fun
    arg' = convertSimpleExpr "SectionRArg" flags $ unLoc arg
    sectionArg = (Nothing, rawVal "sectionArg")
    base' = base `applyRawArgs` [sectionArg]
convertExpr flags base (ExplicitTuple _ t _) = (fmap mapBase base, e', concat subs)
  where
    e' = rawVal "" `applyRawArgs` map (Nothing,) outT
    enumT = zip [0..] (map unLoc t)
    (_, outT, subs) = unzip3 $ map (uncurry convertTupleArg) enumT

    mapBase b = applyRawIArgs b (mapMaybe (fmap (,IArgNothing) . uncurry convertTupleArgPat) enumT)

    convertTupleArgPat :: Int -> HsTupArg GhcPs -> Maybe PartialKey
    convertTupleArgPat _ Present{} = Nothing
    convertTupleArgPat i Missing{} = Just $ partialKey ("tupleArg" ++ show i)
    convertTupleArgPat _ _         = error "Unexpected TupleArg"

    convertTupleArg :: Int -> HsTupArg GhcPs -> (MLHSArgs, RawExpr (), [RawStatementTree RawExpr ()])
    convertTupleArg _ (Present _ a) = convertExpr flags Nothing $ unLoc a
    convertTupleArg i Missing{}             = (Nothing, rawVal ("tupleArg" ++ show i), [])
    convertTupleArg _ _             = error "Unexpected TupleArg"
convertExpr flags _ p@ExplicitSum{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ (HsCase _ cas (MG _ lmatches _)) = (Nothing, rawVal ctCase `applyRawArgs` [(Nothing, convertSimpleExpr "Case" flags $ unLoc cas)], map (uncurry RawStatementTree . first RawDeclStatement . mapBC . convertMatch flags Nothing . unLoc) $ unLoc lmatches)
  where
    mapBC [(_a, b, c)] = (b, c)
    mapBC _            = error "Not yet implemented HsCase mapBC"
convertExpr flags _ (HsIf _ _ i t e) = (Nothing, RawMethod i' (rawVal ctIf `applyRawArgs` [(Just $ partialKey ctThen, t'), (Just $ partialKey ctElse, e')]), concat [tSubs', iSubs', eSubs'])
  where
    (Nothing, t', tSubs') = convertExpr flags Nothing $ unLoc t
    (Nothing, i', iSubs') = convertExpr flags Nothing $ unLoc i
    (Nothing, e', eSubs') = convertExpr flags Nothing $ unLoc e
convertExpr flags _ p@HsMultiIf{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags base (HsLet _ binds e) = (base', e', convertLocalBindsLR flags (unLoc binds) ++ subs')
  where
    (base', e', subs') = convertExpr flags base (unLoc e)
convertExpr flags base (HsDo _ _ statements) = (last base', rawVal nestedDeclaration, concat stmts')
  where
    (base', stmts') = unzip $ map (convertStmtLR flags base . unLoc) $ unLoc statements
convertExpr flags _ (ExplicitList _ Nothing l) = (Nothing, RawList emptyMetaN $ map (convertSimpleExpr "List" flags . unLoc) l, [])
convertExpr flags _ (RecordCon _ name fields) = (Nothing, convertExprRecordFields flags name' fields, [])
  where
    name' = rawVal $ convertIdP flags $ unLoc name
convertExpr flags base (RecordUpd _ e fields) = (base', e' `applyRawArgs` fields', subs' ++ concat fieldsSubs')
  where
    (base', e', subs') = convertExpr flags base (unLoc e)
    (fields', fieldsSubs') = unzip $ map (mapField . unLoc) fields
    mapField :: HsRecField' (AmbiguousFieldOcc GhcPs) (Located (HsExpr GhcPs)) -> ((Maybe ArgName, RawExpr ()), [RawStatementTree RawExpr ()])
    mapField (HsRecField lbl val _) = let (Nothing, fieldE', fieldSubs') = convertExpr flags Nothing $ unLoc val
                                       in ((Just $ partialKey $ convertIdP flags $ rdrNameAmbiguousFieldOcc $ unLoc lbl, fieldE'), fieldSubs')
convertExpr flags base (ExprWithTySig _ e (HsWC _ tySigs)) = (base', RawWhere e' (convertTypeToExpr flags Nothing $ unLoc $ convertImplicitBndrs flags tySigs), subs')
  where
    (base', e', subs') = convertExpr flags base (unLoc e)
convertExpr flags _ (ArithSeq _ _ info) = (Nothing, aux info, [])
  where
    aux :: ArithSeqInfo GhcPs -> RawExpr ()
    aux (From f) = convertSimpleExpr "arithSeqFrom" flags (unLoc f) `RawMethod` rawVal "from"
    aux (FromThen f t) = convertSimpleExpr "arithSeqFromThen-f" flags (unLoc f) `RawMethod` (rawVal "from" `applyRawArgs` [(Just $ partialKey "then", convertSimpleExpr "arithSeqFromThen-t" flags (unLoc t))])
    aux (FromTo f t) = convertSimpleExpr "arithSeqFromTo-f" flags (unLoc f) `RawMethod` (rawVal "until" `applyRawArgs` [(Nothing, convertSimpleExpr "arithSeqFromTo-t" flags (unLoc t))])
    aux (FromThenTo f th to) = convertSimpleExpr "arithSeqFromThenTo-f" flags (unLoc f) `RawMethod` (rawVal "until" `applyRawArgs` [(Nothing, convertSimpleExpr "arithSeqFromThenTo-th" flags (unLoc th)), (Just $ partialKey "then", convertSimpleExpr "arithSeqFromThenTo-to" flags (unLoc to))])
convertExpr flags _ p@HsSCC{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsCoreAnn{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsBracket{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsRnBracketOut{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsTcBracketOut{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsSpliceE{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsProc{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsStatic{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsTick{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsBinTick{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsTickPragma{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsWrap{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@XExpr{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Expr.html#t:StmtLR
convertStmtLR :: DynFlags -> Maybe (RawExpr ()) -> StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> (MLHSArgs, [RawStatementTree RawExpr ()])
convertStmtLR flags base (LastStmt _ body _ _) = (base', [RawStatementTree (RawExprStatement expr') subStatements])
  where
    (base', expr', subStatements) = convertExpr flags base $ unLoc body
convertStmtLR flags _ (BindStmt _ pat body _ _) = (Nothing, [RawStatementTree (RawDeclStatement oa) subStatements])
  where
    -- TODO: Should use bind rather than basic objArr
    pat' = convertPattern flags Nothing $ unLoc pat
    (maybePat'', expr', subStatements) = convertExpr flags (Just pat') $ unLoc body
    oa = RawObjArr (Just $ fromMaybe pat' maybePat'') FunctionObj Nothing [] (Just (Just expr', emptyMetaN)) Nothing
convertStmtLR flags _ p@ApplicativeStmt{} = error $ printf "Convert unsupported stmtLR:\n%s" (showSDoc flags $ ppr p)
convertStmtLR flags base (BodyStmt _ body _ _) = (base', [RawStatementTree (RawExprStatement expr') subStatements])
  where
    (base', expr', subStatements) = convertExpr flags base $ unLoc body
convertStmtLR flags _ (LetStmt _ l) = (Nothing, convertLocalBindsLR flags $ unLoc l)
convertStmtLR flags _ p@ParStmt{} = error $ printf "Convert unsupported stmtLR:\n%s" (showSDoc flags $ ppr p)
convertStmtLR flags _ p@TransStmt{} = error $ printf "Convert unsupported stmtLR:\n%s" (showSDoc flags $ ppr p)
convertStmtLR flags _ p@RecStmt{} = error $ printf "Convert unsupported stmtLR:\n%s" (showSDoc flags $ ppr p)
convertStmtLR flags _ p@XStmtLR{} = error $ printf "Convert unsupported stmtLR:\n%s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Expr.html#t:GRHS
convertGRHS :: DynFlags -> GRHS GhcPs (LHsExpr GhcPs) -> (MLHSArgs, RawExpr (), [RawStatementTree RawExpr ()], Maybe (RawExpr ()))
convertGRHS flags (GRHS _ g e) = (base', e', subStatements, g')
  where
    (base', e', subStatements) = convertExpr flags Nothing $ unLoc e
    g' = case g of
      [] -> Nothing
      _  -> Just $ foldl1 joinAnd $ map (convertGuardStmt . unLoc) g

    convertGuardStmt :: StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> RawExpr ()
    convertGuardStmt (BodyStmt _ body _ _) = convertSimpleExpr "GRHSBody" flags $ unLoc body
    convertGuardStmt p = error $ printf "Convert unsupported guardStmt:\n%s" (showSDoc flags $ ppr p)

    joinAnd :: RawExpr () -> RawExpr () -> RawExpr ()
    joinAnd l r = rawVal "/operator&&" `applyRawArgs` [(Just $ partialKey operatorArgL, l), (Just $ partialKey operatorArgR, r)]
convertGRHS _ _ = error $ printf "Convert unsupported grhs:\n"

convertLocalBindsLR :: DynFlags -> HsLocalBindsLR GhcPs GhcPs -> [RawStatementTree RawExpr ()]
convertLocalBindsLR flags (HsValBinds _ (ValBinds _ binds sigs)) = concatMap (convertSignature flags . unLoc) sigs ++ concatMap (convertBindLR flags . unLoc) (bagToList binds)
convertLocalBindsLR flags p@HsIPBinds{} = error $ printf "Convert unsupported localBindsLR:\n%s" (showSDoc flags $ ppr p)
convertLocalBindsLR _ EmptyLocalBinds{} = []
convertLocalBindsLR flags p@XHsLocalBindsLR{} = error $ printf "Convert unsupported localBindsLR:\n%s" (showSDoc flags $ ppr p)
convertLocalBindsLR flags p = error $ printf "Convert unsupported localBindsLR:\n%s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Expr.html#t:GRHSs
convertGRHSs :: DynFlags -> GRHSs GhcPs (LHsExpr GhcPs) -> [(MLHSArgs, RawExpr (), [RawStatementTree RawExpr ()], Maybe (RawExpr ()))]
convertGRHSs flags (GRHSs _ grhss binds) = map (aux . unLoc) grhss
  where
    binds' = convertLocalBindsLR flags $ unLoc binds
    aux :: GRHS GhcPs (LHsExpr GhcPs) -> (MLHSArgs, RawExpr (), [RawStatementTree RawExpr ()], Maybe (RawExpr ()))
    aux grhs = (base', rhs', binds' ++ subStatements, guard')
      where
        (base', rhs', subStatements, guard') = convertGRHS flags grhs
convertGRHSs _ _ = error $ printf "Convert unsupported grhs:\n"

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Expr.html#t:MatchGroup
convertMatch :: DynFlags -> Maybe (RawExpr ()) -> Match GhcPs (LHsExpr GhcPs) -> [(MLHSArgs, RawObjArr RawExpr (), [RawStatementTree RawExpr ()])]
convertMatch flags i (Match _ _ pats grhs) = map aux $ convertGRHSs flags grhs
  where
    aux (base', arr, subStatements, guard) = (base', oa, subStatements)
      where
        objExpr = fromJust (foldl (\p b -> Just $ convertPattern flags p b) i (map unLoc pats))
        guardObjExpr = case guard of
          Just g  -> RawWhere objExpr g
          Nothing -> objExpr
        oa = RawObjArr (Just guardObjExpr) FunctionObj Nothing [] (Just (Just arr, emptyMetaN)) Nothing
convertMatch flags _ p = error $ printf "Convert unsupported match:\n%s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Binds.html#t:HsBindLR
convertBindLR :: DynFlags -> HsBind GhcPs -> [RawStatementTree RawExpr ()]
convertBindLR flags p@(FunBind _ i (MG _ lmatches _) _ _) = map wrap $ concatMap (conv . unLoc) $ unLoc lmatches
  where
    conv match = convertMatch flags (Just $ rawVal $ convertIdP flags $ unLoc i) match
    wrap (Nothing, oa, subStatements) = RawStatementTree (RawDeclStatement oa) subStatements
    wrap _ = error $ printf "Convert unsupported convertBindLR wrap:\n%s" (showSDoc flags $ ppr p)
convertBindLR flags (PatBind _ lhs rhs _) = [RawStatementTree (RawDeclStatement $ RawObjArr (Just guardObj) PatternObj Nothing [] (Just (Just arrExpr, emptyMetaN)) Nothing) subs]
  where
    obj = convertPattern flags Nothing $ unLoc lhs
    [(Nothing, arrExpr, subs, maybeGuard)] = convertGRHSs flags rhs
    guardObj = maybe obj (RawWhere obj) maybeGuard
convertBindLR flags p@VarBind{} = error $ printf "Convert unsupported bindLR:\n%s" (showSDoc flags $ ppr p)
convertBindLR flags p@AbsBinds{} = error $ printf "Convert unsupported bindLR:\n%s" (showSDoc flags $ ppr p)
convertBindLR flags p@PatSynBind{} = error $ printf "Convert unsupported bindLR:\n%s" (showSDoc flags $ ppr p)
convertBindLR flags p@XHsBindsLR{} = error $ printf "Convert unsupported bindLR:\n%s" (showSDoc flags $ ppr p)
convertBindLR flags p = error $ printf "Convert unsupported bindLR:\n%s" (showSDoc flags $ ppr p)

convertTyVarBndr :: DynFlags -> HsTyVarBndr GhcPs -> TypeVarName
convertTyVarBndr flags (UserTyVar _ v) = partialKey $ convertIdP flags $ unLoc v
convertTyVarBndr flags p = error $ printf "Convert unsupported TyVarBndr:\n%s" (showSDoc flags $ ppr p)

convertConDeclField :: DynFlags -> RawExpr () -> ConDeclField GhcPs -> RawExpr ()
convertConDeclField flags base (ConDeclField _ names tp _doc) = base `applyRawEIArgs` map ((,IArgNothing) . withType . rawVal . convertIdP flags . unLoc . rdrNameFieldOcc . unLoc) names
  where
    withType :: RawExpr () -> RawExpr ()
    withType e = rawVal operatorType `applyRawArgs` [(Just $ partialKey operatorArgL, e), (Just $ partialKey operatorArgR, convertTypeToExpr flags Nothing $ unLoc tp)]
convertConDeclField flags _ p = error $ printf "Convert unsupported ConDeclField: \n %s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Types.html#t:HsConDetails
convertConDeclDetails :: DynFlags -> RawExpr () -> HsConDeclDetails GhcPs -> RawExpr ()
convertConDeclDetails flags base (PrefixCon args) = foldl (convertTypeToExpr flags . Just) base $ map unLoc args
convertConDeclDetails flags base (RecCon r) = foldl (convertConDeclField flags) base $ map unLoc $ unLoc r
convertConDeclDetails flags _ p@InfixCon{} = error $ printf "Convert unsupported ConDeclDetails:\n%s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Decls.html#t:ConDecl
convertConDecl :: DynFlags -> ConDecl GhcPs -> RawExpr ()
convertConDecl flags (ConDeclGADT _ [name] _ quants _ctx args _res _) = convertQTyVars flags beforeQuant quants
  where
    beforeQuant = convertConDeclDetails flags (rawVal (convertIdP flags $ unLoc name)) args
convertConDecl flags p@ConDeclGADT{} = error $ printf "Convert unsupported ConDeclGADT:\n%s" (showSDoc flags $ ppr p)
convertConDecl flags (ConDeclH98 _ name _ [] Nothing args Nothing) = convertConDeclDetails flags (rawVal (convertIdP flags $ unLoc name)) args
convertConDecl flags p@ConDeclH98{} = error $ printf "Convert unsupported ConDeclH98:\n%s" (showSDoc flags $ ppr p)
convertConDecl flags p@XConDecl{} = error $ printf "Convert unsupported XConDecl:\n%s" (showSDoc flags $ ppr p)

convertImplicitBndrs :: (Outputable t) => DynFlags -> HsImplicitBndrs GhcPs t -> t
convertImplicitBndrs _ (HsIB _ t) = t
convertImplicitBndrs flags p = error $ printf "Convert unsupported DerivingClause:\n%s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Decls.html#t:HsDerivingClause
convertDerivingClause :: DynFlags -> HsDerivingClause GhcPs -> ExtendedClasses RawExpr ()
convertDerivingClause flags (HsDerivingClause _ _derivingStrategy c) = map (convertTypeToExpr flags Nothing . unLoc . convertImplicitBndrs flags) $ unLoc c
convertDerivingClause flags p = error $ printf "Convert unsupported DerivingClause:\n%s" (showSDoc flags $ ppr p)

convertQTyVars :: DynFlags -> RawExpr () -> LHsQTyVars GhcPs -> RawExpr ()
convertQTyVars flags base (HsQTvs _ vars) = base `applyRawExprVars` map ((, emptyMetaN) . convertTyVarBndr flags . unLoc) vars
convertQTyVars flags _ p = error $ printf "Convert unsupported DerivingClause:\n%s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Decls.html#t:TyClDecl
convertTyClDecl :: DynFlags -> TyClDecl GhcPs -> [RawStatementTree RawExpr ()]
convertTyClDecl flags p@FamDecl{} = error $ printf "Convert unsupported TyClDecl:\n%s" (showSDoc flags $ ppr p)
convertTyClDecl flags (SynDecl _ name vars _ rhs) = [RawStatementTree (MultiTypeDefStatement $ MultiTypeDef (convertQTyVars flags (rawVal $ convertIdP flags $ unLoc name) vars) [convertTypeToExpr flags Nothing $ unLoc rhs] []) []]
convertTyClDecl flags (DataDecl _ name vars _fixity (HsDataDefn _ _ _ Nothing Nothing cons derivs)) = [RawStatementTree (MultiTypeDefStatement $ MultiTypeDef (convertQTyVars flags (rawVal $ convertIdP flags $ unLoc name) vars) (map (convertConDecl flags . unLoc) cons) (concatMap (convertDerivingClause flags . unLoc) $ unLoc derivs)) []]
-- convertTyClDecl flags (ClassDecl _ _ name vars _ fds sigs defs ats atDefs docs) | trace (printf "fds: %d, sigs: %d, ats: %d, atDefs: %d, docs: %d" (length fds) (length sigs) (length ats) (length atDefs) (length docs)) False = undefined
convertTyClDecl flags (ClassDecl _ _ name vars _ [] sigs defs [] [] []) = [RawStatementTree (RawClassDeclStatement $ convertQTyVars flags (rawVal $ convertIdP flags $ unLoc name) vars) (sigs' ++ defs')]
  where
    sigs' = concatMap (convertSignature flags . unLoc) sigs
    defs' = concatMap (convertBindLR flags . unLoc) $ bagToList defs
convertTyClDecl flags p@XTyClDecl{} = error $ printf "Convert unsupported TyClDecl:\n%s" (showSDoc flags $ ppr p)
convertTyClDecl flags p = error $ printf "Convert unsupported TyClDecl:\n%s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Decls.html#t:InstDecl
convertInstDecl :: DynFlags -> InstDecl GhcPs -> [RawStatementTree RawExpr ()]
convertInstDecl flags (ClsInstD _ (ClsInstDecl _ (HsIB _ polyTy) binds sigs [] [] _)) = [RawStatementTree (RawClassDefStatement $ mkClassDef $ convertTypeToExpr flags Nothing $ unLoc polyTy) (sigs' ++ binds')]
  where
    mkClassDef (RawVarsApply _ (RawValue _ b) [v]) = (fst v, [rawVal b])
    mkClassDef (RawWhere b c) = let (b', clsB) = mkClassDef b
                                    (_, clsC) = mkClassDef c
                                 in (b', clsB ++ clsC)
    mkClassDef e = error $ printf "Unsupported mkClassDef from %s" (show e)
    sigs' = concatMap (convertSignature flags . unLoc) sigs
    binds' = concatMap (convertBindLR flags . unLoc) $ bagToList binds
convertInstDecl flags p@DataFamInstD{} = error $ printf "Convert unsupported inst decl:\n%s" (showSDoc flags $ ppr p)
convertInstDecl flags p@TyFamInstD{} = error $ printf "Convert unsupported inst decl:\n%s" (showSDoc flags $ ppr p)
convertInstDecl flags p@XInstDecl{} = error $ printf "Convert unsupported inst decl:\n%s" (showSDoc flags $ ppr p)
convertInstDecl flags p = error $ printf "Convert unsupported inst decl:\n%s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Decls.html#t:HsDecl
convertDecl :: DynFlags -> HsDecl GhcPs -> [RawStatementTree RawExpr ()]
convertDecl flags (TyClD _ d) = convertTyClDecl flags d
convertDecl flags (InstD _ d) = convertInstDecl flags d
convertDecl flags p@DerivD{} = error $ printf "Convert unsupported decl:\n%s" (showSDoc flags $ ppr p)
convertDecl flags (ValD _ lr) = convertBindLR flags lr
convertDecl flags (SigD _ sig) = convertSignature flags sig
convertDecl flags p@KindSigD{} = error $ printf "Convert unsupported decl:\n%s" (showSDoc flags $ ppr p)
convertDecl flags p@DefD{} = error $ printf "Convert unsupported decl:\n%s" (showSDoc flags $ ppr p)
convertDecl flags p@ForD{} = error $ printf "Convert unsupported decl:\n%s" (showSDoc flags $ ppr p)
convertDecl flags p@WarningD{} = error $ printf "Convert unsupported decl:\n%s" (showSDoc flags $ ppr p)
convertDecl flags p@AnnD{} = error $ printf "Convert unsupported decl:\n%s" (showSDoc flags $ ppr p)
convertDecl flags p@RuleD{} = error $ printf "Convert unsupported decl:\n%s" (showSDoc flags $ ppr p)
convertDecl flags p@SpliceD{} = error $ printf "Convert unsupported decl:\n%s" (showSDoc flags $ ppr p)
convertDecl flags p@DocD{} = error $ printf "Convert unsupported decl:\n%s" (showSDoc flags $ ppr p)
convertDecl flags p@RoleAnnotD{} = error $ printf "Convert unsupported decl:\n%s" (showSDoc flags $ ppr p)
convertDecl flags p@XHsDecl{} = error $ printf "Convert unsupported decl:\n%s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs.html#t:HsModule
convertModule :: DynFlags -> HsModule GhcPs -> RawPrgm ()
convertModule flags (HsModule (Just name) Nothing imports decls Nothing Nothing) = (map (convertImport flags . unLoc) imports, [RawStatementTree (RawModule $ moduleNameSlashes $ unLoc name) (concatMap (convertDecl flags . unLoc) decls)])
convertModule flags p = error $ printf "Convert unsupported Module:\n%s" (showSDoc flags $ ppr p)
