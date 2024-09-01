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
import           CtConstants
import           Data.Bifunctor        (first)
import           Data.Maybe            (fromJust, fromMaybe, mapMaybe,
                                        maybeToList)
import           GHC.Data.Bag          (bagToList)
import           GHC.Data.FastString
import           GHC.Driver.Ppr
import           GHC.Driver.Session
import           GHC.Hs
import           GHC.Types.Name
import           GHC.Types.Name.Reader
import           GHC.Types.SourceText
import           GHC.Types.SrcLoc
import           GHC.Unit
import           GHC.Utils.Outputable
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Builder
import           Syntax.Ct.Prgm
import           Text.Printf

type MLHSArgs = Maybe (RawExpr ()) -- Maybe base with lhs arguments, is present when a lambda modifies the base args

-- https://wiki.haskell.org/Import
-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/GHC-Hs-ImpExp.html#t:ImportDecl
convertImport :: DynFlags -> ImportDecl GhcPs -> RawFileImport
convertImport _ (ImportDecl _ _ name _ _ _ qualifiedStyle _ maybeQualifiedAlias _maybeHiding) = mkRawFileImport $ rawVal "haskell" `applyRawArgs` concat [moduleArg, qualifiedArg, aliasArg]
  where
    moduleArg = [(Nothing, RawCExpr emptyMetaN $ CStr $ moduleNameSlashes $ unLoc name)]
    qualifiedArg = [(Just $ partialKey "qualified", rawVal "True") | qualifiedStyle /= NotQualified]
    aliasArg = [(Just $ partialKey "alias", RawCExpr emptyMetaN $ CStr $ moduleNameSlashes $ unLoc qualifiedAlias) | qualifiedAlias <- maybeToList maybeQualifiedAlias]
    -- hidingArg = case maybeHiding of
    --   Just (hiding, names) -> _

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/GHC-Types-Name-Reader.html#t:RdrName
convertIdP :: DynFlags -> IdP GhcPs -> String
convertIdP _ (Unqual n) = occNameString n
convertIdP _ (Qual m n) = moduleNameSlashes m ++ occNameString n
convertIdP flags p@Orig{} = error $ printf "Convert unsupported IdP:\n%s" (showSDoc flags $ ppr p)
convertIdP _ (Exact n) = nameStableString n

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/GHC-Hs-Type.html#t:HsType
convertTypeToExpr :: DynFlags -> Maybe (RawExpr ()) -> HsType GhcPs -> RawExpr ()
convertTypeToExpr flags _ p@HsForAllTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags i (HsQualTy _ ctx body) = foldl aux (convertTypeToExpr flags i $ unLoc body) (unLoc <$> unLoc ctx)
  where
    aux :: RawExpr () -> HsType GhcPs -> RawExpr ()
    aux b cond = RawWhere emptyMetaN b (convertTypeToExpr flags Nothing cond)
convertTypeToExpr flags (Just i) (HsTyVar _ _ v) = i `applyRawArgs` [(Just $ partialKey "A", rawVal $ convertIdP flags $ unLoc v)]
convertTypeToExpr flags Nothing (HsTyVar _ _ v) = rawVal $ convertIdP flags $ unLoc v
convertTypeToExpr flags i (HsAppTy _ base v) = convertTypeToExpr flags i (unLoc base) `applyRawExprEVars` [(convertTypeToExpr flags i $ unLoc v, Nothing)]
convertTypeToExpr flags _ p@HsAppKindTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags i (HsFunTy _ _ base app) = convertTypeToExpr flags i (unLoc base) `applyRawArgs` [(Just $ partialKey "B", convertTypeToExpr flags Nothing $ unLoc app)]
convertTypeToExpr flags mb (HsListTy _ t) = case mb of
  Just b  -> b `applyRawArgs` [(Nothing, t')]
  Nothing -> t'
  where
    t' = rawVal ctListType `applyRawExprVars` [(partialKey "T", Just $ convertTypeToExpr flags Nothing $ unLoc t)]
convertTypeToExpr flags _ (HsTupleTy _ _ tp) = rawAnon `applyRawArgs` map ((Just $ partialKey "C",) . convertTypeToExpr flags Nothing . unLoc) tp
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

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/GHC-Hs-Type.html#t:HsType
convertTypeToObj :: DynFlags -> String -> HsType GhcPs -> RawObjArr RawExpr ()
convertTypeToObj flags _ p@HsForAllTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@HsAppKindTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags i (HsFunTy _ _ base app) = RawObjArr (Just (convertTypeToExpr flags (Just $ rawVal i) $ unLoc base)) FunctionObj Nothing [] (Just (Nothing, Just appExpr)) Nothing
  where
    appExpr = convertTypeToExpr flags Nothing $ unLoc app
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
convertTypeToObj flags i p = RawObjArr (Just (convertTypeToExpr flags (Just $ rawVal i) p)) FunctionObj Nothing [] Nothing Nothing -- Fallback to using expr obj

convertExprRecordFields :: DynFlags -> RawExpr () -> HsRecFields GhcPs (LHsExpr GhcPs) -> RawExpr ()
convertExprRecordFields flags base (HsRecFields fields Nothing) = foldl convertRecordField base (map unLoc fields)
  where
    convertRecordField :: RawExpr () -> HsRecField GhcPs (LHsExpr GhcPs) -> RawExpr ()
    convertRecordField b (HsFieldBind _ lbl arg _) = b `applyRawArgs` [(Just $ partialKey $ convertIdP flags $ unLoc $ foLabel $ unLoc lbl, convertSimpleExpr "convertExprRecordFields" flags $ unLoc arg)]
convertExprRecordFields flags _ p = error $ printf "Convert unsupported record fields:\n%s" (showSDoc flags $ ppr p)

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/GHC-Hs-Pat.html#t:HsRecFields
convertPatRecordFields :: DynFlags -> RawExpr () -> HsRecFields GhcPs (LPat GhcPs) -> RawExpr ()
convertPatRecordFields flags base (HsRecFields fields Nothing) = foldl convertRecordField base (map unLoc fields)
  where
    convertRecordField :: RawExpr () -> HsRecField GhcPs (LPat GhcPs) -> RawExpr ()
    convertRecordField b (HsFieldBind _ lbl arg _) = b `applyRawArgs` [(Just $ partialKey $ convertIdP flags $ unLoc $ foLabel $ unLoc lbl, convertPattern flags Nothing $ unLoc arg)]
convertPatRecordFields flags _ p = error $ printf "Convert unsupported record fields:\n%s" (showSDoc flags $ ppr p)

convertSignature :: DynFlags -> Sig GhcPs -> [RawStatementTree RawExpr ()]
convertSignature flags (TypeSig _ ids (HsWC _ ldef)) = map (aux (unSig $ unLoc ldef) . unLoc) ids
  where
    aux def i = RawStatementTree (RawDeclStatement $ convertTypeToObj flags (convertIdP flags i) def) []
convertSignature flags p@PatSynSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags (ClassOpSig _ _ names t) = map (aux . convertIdP flags . unLoc) names
  where
    aux n = RawStatementTree (RawDeclStatement $ convertTypeToObj flags n (unSig $ unLoc t)) []
convertSignature flags p@IdSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags p@FixSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags p@InlineSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags p@SpecSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags p@SpecInstSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags p@MinimalSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags p@SCCFunSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)
convertSignature flags p@CompleteMatchSig{} = error $ printf "Convert unsupported signature:\n%s" (showSDoc flags $ ppr p)


-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/Language-Haskell-Syntax-Lit.html#t:OverLitVal
convertOverLit :: DynFlags -> HsOverLit GhcPs -> RawExpr ()
convertOverLit _ (OverLit _ (HsIntegral (IL _ _ c))) = RawCExpr emptyMetaN $ CInt c
convertOverLit _ (OverLit _ (HsFractional (FL _ _ c _ _))) = RawCExpr emptyMetaN $ CFloat $ fromRational c
convertOverLit _ (OverLit _ (HsIsString _ s)) = RawCExpr emptyMetaN $ CStr $ unpackFS s

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/Language-Haskell-Syntax-Lit.html#t:HsLit
convertLiteral :: DynFlags -> HsLit GhcPs -> RawExpr ()
convertLiteral _ (HsString _ s) = RawCExpr emptyMetaN $ CStr $ unpackFS s
convertLiteral _ (HsChar _ c) = RawCExpr emptyMetaN $ CChar c
convertLiteral _ (HsCharPrim _ c) = RawCExpr emptyMetaN $ CChar c
convertLiteral flags l = error $ printf "Convert unsupported lit:\n%s" (showSDoc flags $ ppr l)

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/GHC-Hs-Pat.html#t:HsConPatDetails
-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/Language-Haskell-Syntax-Type.html#t:HsConDetails
convertPatDetails :: DynFlags -> Maybe (RawExpr ()) -> HsConPatDetails GhcPs -> RawExpr ()
convertPatDetails _ Nothing (PrefixCon [] []) = RawHoleExpr emptyMetaN (HoleActive Nothing)
convertPatDetails flags base (PrefixCon [] args) = fromJust $ foldl (\b p -> Just $ convertPattern flags b p) base $ map unLoc args
convertPatDetails flags (Just base) (RecCon r) = convertPatRecordFields flags base r
convertPatDetails flags _ (InfixCon hd tl) = rawVal ctListCons`applyRawArgs` [(Just $ partialKey ctListConsHead, convertPattern flags Nothing $ unLoc hd), (Just $ partialKey ctListConsTail, convertPattern flags Nothing $ unLoc tl)]
convertPatDetails flags _ p = error $ printf "Convert unsupported conPatDetails:\n%s" (showSDoc flags $ ppr p)

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/GHC-Hs-Pat.html#t:Pat
convertPattern :: DynFlags -> Maybe (RawExpr ()) -> Pat GhcPs -> RawExpr ()
convertPattern _ _ WildPat{} = RawHoleExpr emptyMetaN $ HoleActive Nothing
convertPattern flags Nothing (VarPat _ v) = rawVal $ convertIdP flags $ unLoc v
convertPattern flags (Just b) (VarPat _ v) = b `applyRawArgs` [(Nothing, rawVal $ convertIdP flags $ unLoc v)]
convertPattern flags _ p@LazyPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags i (AsPat _ alias base) = RawAliasExpr (convertPattern flags i $ unLoc base) (rawVal $ convertIdP flags $ unLoc alias)
convertPattern flags base (ParPat _ _ p _) = convertPattern flags base $ unLoc p
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
    tuple' = rawAnon `applyRawArgs` map ((Nothing,) . convertPattern flags Nothing . unLoc) pats
convertPattern flags _ p@SumPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags (Just base) (ConPat _ i det) = base `applyRawArgs` [(Just $ partialKey $ convertIdP flags $ unLoc i, convertPatDetails flags (Just base) det)]
convertPattern flags Nothing (ConPat _ i det) = convertPatDetails flags (Just $ rawVal $ convertIdP flags $ unLoc i) det
convertPattern flags _ p@ViewPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags _ p@SplicePat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags _ (LitPat _ lit) = convertLiteral flags lit
convertPattern flags _ (NPat _ n _ _) = convertOverLit flags $ unLoc n
convertPattern flags _ p@NPlusKPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags _ p@SigPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)

convertSimpleExpr :: String -> DynFlags -> HsExpr GhcPs -> RawExpr ()
convertSimpleExpr msg flags e = case convertExpr flags Nothing e of
  (Nothing, e', []) -> e'
  (m', e', subs) -> error $ printf "convertSimpleExpr %s failed from %s to %s with match %s and subs %s" msg (showSDoc flags $ ppr e) (show e') (show m') (show subs)

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/Language-Haskell-Syntax-Expr.html#t:HsExpr
convertExpr :: DynFlags -> Maybe (RawExpr ()) -> HsExpr GhcPs -> (MLHSArgs, RawExpr (), [RawStatementTree RawExpr ()])
convertExpr flags _ (HsVar _ v) = (Nothing, rawVal $ convertIdP flags $ unLoc v, [])
convertExpr flags _ p@HsUnboundVar{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsRecSel{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsOverLabel{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsIPVar{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ (HsOverLit _ literal) = (Nothing, convertOverLit flags literal, [])
convertExpr flags _ (HsLit _ literal) = (Nothing, convertLiteral flags literal, [])
convertExpr flags b (HsLam _ (MG _ matches _)) = (base' >>= const (roaObj oa'), fromJust $ fst $ fromJust $ roaArr oa', subs)
  where
    (base', oa', subs) = head $ concatMap (convertMatch flags b . unLoc) $ unLoc matches
convertExpr flags _ p@HsLamCase{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags eb (HsApp _ base app) = (baseMatch, baseE `applyRawEArgs` [(appBase, appE)], baseSubs ++ appSubs)
  where
    (baseMatch, baseE, baseSubs) = convertExpr flags eb (unLoc base)
    (appBase, appE, appSubs) = convertExpr flags Nothing (unLoc app)
convertExpr flags _ p@HsAppType{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags eb (OpApp _ l op r) = case op' of
  RawValue _ "$" -> (eb, le `applyRawEArgs` [(Nothing, re)], lSubs ++ rSubs)
  _ -> (eb, op' `applyRawEArgs` [(Just $ fromMaybe lKey lKey', le), (Just $ fromMaybe rKey rKey', re)], lSubs ++ rSubs)
  where
    op' = convertSimpleExpr "opAppL" flags (unLoc op)
    lKey = rawVal operatorArgL
    rKey = rawVal operatorArgR
    (lKey', le, lSubs) = convertExpr flags (Just lKey) (unLoc l)
    (rKey', re, rSubs) = convertExpr flags (Just rKey) (unLoc r)
convertExpr flags base (NegApp _ e _) = (base', rawVal (operatorName "-") `applyRawArgs` [(Just $ partialKey operatorArgUnary, e')], subs')
  where
    (base', e', subs') = convertExpr flags base (unLoc e)
convertExpr flags base (HsPar _ _ p _) = convertExpr flags base $ unLoc p
convertExpr flags base (SectionL _ fun arg) = (Just base', fun' `applyRawArgs` [(Nothing, arg'), sectionArg], [])
  where
    fun' = convertSimpleExpr "SectionRFun" flags $ unLoc fun
    arg' = convertSimpleExpr "SectionRArg" flags $ unLoc arg
    sectionArg = (Nothing, rawVal "sectionArg")
    base' = case base of
      Just b  -> b `applyRawArgs` [sectionArg]
      Nothing -> rawAnon `applyRawArgs` [sectionArg]
convertExpr flags base (SectionR _ fun arg) = (Just base', fun' `applyRawArgs` [sectionArg, (Nothing, arg')], [])
  where
    fun' = convertSimpleExpr "SectionRFun" flags $ unLoc fun
    arg' = convertSimpleExpr "SectionRArg" flags $ unLoc arg
    sectionArg = (Nothing, rawVal "sectionArg")
    base' = case base of
      Just b  -> b `applyRawArgs` [sectionArg]
      Nothing -> rawAnon `applyRawArgs` [sectionArg]
convertExpr flags base (ExplicitTuple _ t _) = (fmap mapBase base, e', concat subs)
  where
    e' = rawAnon `applyRawArgs` map (Nothing,) outT
    enumT = zip [0..] t
    (_, outT, subs) = unzip3 $ map (uncurry convertTupleArg) enumT

    mapBase b = applyRawIArgs b (mapMaybe (fmap (,IArgNothing) . uncurry convertTupleArgPat) enumT)

    convertTupleArgPat :: Int -> HsTupArg GhcPs -> Maybe PartialKey
    convertTupleArgPat _ Present{} = Nothing
    convertTupleArgPat i Missing{} = Just $ partialKey ("tupleArg" ++ show i)

    convertTupleArg :: Int -> HsTupArg GhcPs -> (MLHSArgs, RawExpr (), [RawStatementTree RawExpr ()])
    convertTupleArg _ (Present _ a) = convertExpr flags Nothing $ unLoc a
    convertTupleArg i Missing{}             = (Nothing, rawVal ("tupleArg" ++ show i), [])
convertExpr flags _ p@ExplicitSum{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ (HsCase _ cas (MG _ lmatches _)) = (Nothing, rawVal ctCase `applyRawArgs` [(Nothing, convertSimpleExpr "Case" flags $ unLoc cas)], map (uncurry RawStatementTree . first RawDeclStatement . mapBC . convertMatch flags Nothing . unLoc) $ unLoc lmatches)
  where
    mapBC [(_a, b, c)] = (b, c)
    mapBC _            = error "Not yet implemented HsCase mapBC"
convertExpr flags _ (HsIf _ i t e) = (Nothing, RawMethod emptyMetaN i' (rawVal ctIf `applyRawArgs` [(Just $ partialKey ctThen, t'), (Just $ partialKey ctElse, e')]), concat [tSubs', iSubs', eSubs'])
  where
    (_, t', tSubs') = convertExpr flags Nothing $ unLoc t
    (_, i', iSubs') = convertExpr flags Nothing $ unLoc i
    (_, e', eSubs') = convertExpr flags Nothing $ unLoc e
convertExpr flags _ p@HsMultiIf{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags base (HsLet _ _ binds _ e) = (base', e', convertLocalBindsLR flags binds ++ subs')
  where
    (base', e', subs') = convertExpr flags base (unLoc e)
convertExpr flags base (HsDo _ _ statements) = (last base', rawVal nestedDeclaration, concat stmts')
  where
    (base', stmts') = unzip $ map (convertStmtLR flags base . unLoc) $ unLoc statements
convertExpr flags _ (ExplicitList _ l) = (Nothing, RawList emptyMetaN $ map (convertSimpleExpr "List" flags . unLoc) l, [])
convertExpr flags _ (RecordCon _ name fields) = (Nothing, convertExprRecordFields flags name' fields, [])
  where
    name' = rawVal $ convertIdP flags $ unLoc name
convertExpr flags base (RecordUpd _ e fields) = (base', e' `applyRawEArgs` fields', subs' ++ concat fieldsSubs')
  where
    (base', e', subs') = convertExpr flags base (unLoc e)
    (fields', fieldsSubs') = unzip $ either (map (mapUpd . unLoc)) (map (mapProj . unLoc)) fields

    -- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/Language-Haskell-Syntax-Pat.html#t:HsFieldBind
    mapUpd :: HsRecUpdField GhcPs -> ((Maybe (RawExpr ()), RawExpr ()), [RawStatementTree RawExpr ()])
    mapUpd (HsFieldBind _ lbl val _) = case convertExpr flags Nothing $ unLoc val of
      (Nothing, fieldE', fieldSubs') -> ((Just $ RawValue emptyMetaN $ convertIdP flags $ rdrNameAmbiguousFieldOcc $ unLoc lbl, fieldE'), fieldSubs')
      _ -> undefined

    mapProj :: RecUpdProj GhcPs -> ((Maybe (RawExpr ()), RawExpr ()), [RawStatementTree RawExpr ()])
    mapProj (HsFieldBind _ lbl val _) = case convertExpr flags Nothing $ unLoc val of
      (Nothing, fieldE', fieldSubs') -> ((Just $ foldl1 (RawMethod emptyMetaN) $ map (RawValue emptyMetaN . unpackFS . unLoc . dfoLabel . unLoc) $ fromFieldLabelStrings $ unLoc lbl, fieldE'), fieldSubs')
      _ -> undefined
    fromFieldLabelStrings (FieldLabelStrings s) = s

convertExpr flags _ p@HsGetField{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsProjection{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags base (ExprWithTySig _ e (HsWC _ tySigs)) = (base', RawWhere emptyMetaN e' (convertSigToExpr flags $ unLoc tySigs), subs')
  where
    (base', e', subs') = convertExpr flags base (unLoc e)
convertExpr flags _ (ArithSeq _ _ info) = (Nothing, aux info, [])
  where
    aux :: ArithSeqInfo GhcPs -> RawExpr ()
    aux (From f) = RawMethod emptyMetaN (convertSimpleExpr "arithSeqFrom" flags (unLoc f)) (rawVal "from")
    aux (FromThen f t) = RawMethod emptyMetaN (convertSimpleExpr "arithSeqFromThen-f" flags (unLoc f)) (rawVal "from" `applyRawArgs` [(Just $ partialKey "then", convertSimpleExpr "arithSeqFromThen-t" flags (unLoc t))])
    aux (FromTo f t) = RawMethod emptyMetaN (convertSimpleExpr "arithSeqFromTo-f" flags (unLoc f)) (rawVal "until" `applyRawArgs` [(Nothing, convertSimpleExpr "arithSeqFromTo-t" flags (unLoc t))])
    aux (FromThenTo f th to) = RawMethod emptyMetaN (convertSimpleExpr "arithSeqFromThenTo-f" flags (unLoc f)) (rawVal "until" `applyRawArgs` [(Nothing, convertSimpleExpr "arithSeqFromThenTo-th" flags (unLoc th)), (Just $ partialKey "then", convertSimpleExpr "arithSeqFromThenTo-to" flags (unLoc to))])
convertExpr flags _ p@HsTypedBracket{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsUntypedBracket{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsSpliceE{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsProc{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsStatic{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags _ p@HsPragE{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)

rawExprStatement :: RawExpr () -> RawStatement RawExpr ()
rawExprStatement e = RawDeclStatement $ RawObjArr (Just e) FunctionObj Nothing [] Nothing Nothing

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/Language-Haskell-Syntax-Expr.html#t:StmtLR
convertStmtLR :: DynFlags -> Maybe (RawExpr ()) -> StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> (MLHSArgs, [RawStatementTree RawExpr ()])
convertStmtLR flags base (LastStmt _ body _ _) = (base', [RawStatementTree (rawExprStatement expr') subStatements])
  where
    (base', expr', subStatements) = convertExpr flags base $ unLoc body
convertStmtLR flags _ (BindStmt _ pat body) = (Nothing, [RawStatementTree (RawBindStatement oa) subStatements])
  where
    -- TODO: Should use bind rather than basic objArr
    pat' = convertPattern flags Nothing $ unLoc pat
    (maybePat'', expr', subStatements) = convertExpr flags (Just pat') $ unLoc body
    oa = RawObjArr (Just $ fromMaybe pat' maybePat'') FunctionObj Nothing [] (Just (Just expr', Nothing)) Nothing
convertStmtLR flags _ p@ApplicativeStmt{} = error $ printf "Convert unsupported stmtLR:\n%s" (showSDoc flags $ ppr p)
convertStmtLR flags base (BodyStmt _ body _ _) = (base', [RawStatementTree (rawExprStatement expr') subStatements])
  where
    (base', expr', subStatements) = convertExpr flags base $ unLoc body
convertStmtLR flags _ (LetStmt _ l) = (Nothing, convertLocalBindsLR flags l)
convertStmtLR flags _ p@ParStmt{} = error $ printf "Convert unsupported stmtLR:\n%s" (showSDoc flags $ ppr p)
convertStmtLR flags _ p@TransStmt{} = error $ printf "Convert unsupported stmtLR:\n%s" (showSDoc flags $ ppr p)
convertStmtLR flags _ p@RecStmt{} = error $ printf "Convert unsupported stmtLR:\n%s" (showSDoc flags $ ppr p)

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/Language-Haskell-Syntax-Expr.html#t:GRHS
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

convertLocalBindsLR :: DynFlags -> HsLocalBindsLR GhcPs GhcPs -> [RawStatementTree RawExpr ()]
convertLocalBindsLR flags (HsValBinds _ (ValBinds _ binds sigs)) = concatMap (convertSignature flags . unLoc) sigs ++ concatMap (convertBindLR flags . unLoc) (bagToList binds)
convertLocalBindsLR flags p@HsIPBinds{} = error $ printf "Convert unsupported localBindsLR:\n%s" (showSDoc flags $ ppr p)
convertLocalBindsLR _ EmptyLocalBinds{} = []
convertLocalBindsLR flags p = error $ printf "Convert unsupported localBindsLR:\n%s" (showSDoc flags $ ppr p)

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/Language-Haskell-Syntax-Expr.html#t:GRHSs
convertGRHSs :: DynFlags -> GRHSs GhcPs (LHsExpr GhcPs) -> [(MLHSArgs, RawExpr (), [RawStatementTree RawExpr ()], Maybe (RawExpr ()))]
convertGRHSs flags (GRHSs _ grhss binds) = map (aux . unLoc) grhss
  where
    binds' = convertLocalBindsLR flags binds
    aux :: GRHS GhcPs (LHsExpr GhcPs) -> (MLHSArgs, RawExpr (), [RawStatementTree RawExpr ()], Maybe (RawExpr ()))
    aux grhs = (base', rhs', binds' ++ subStatements, guard')
      where
        (base', rhs', subStatements, guard') = convertGRHS flags grhs

-- https://www.stackage.org/haddock/lts-21.25/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Expr.html#t:MatchGroup
convertMatch :: DynFlags -> Maybe (RawExpr ()) -> Match GhcPs (LHsExpr GhcPs) -> [(MLHSArgs, RawObjArr RawExpr (), [RawStatementTree RawExpr ()])]
convertMatch flags i (Match _ _ pats grhs) = map aux $ convertGRHSs flags grhs
  where
    aux (base', arr, subStatements, guard) = (base', oa, subStatements)
      where
        objExpr = fromJust (foldl (\p b -> Just $ convertPattern flags p b) i (map unLoc pats))
        guardObjExpr = case guard of
          Just g  -> RawWhere emptyMetaN objExpr g
          Nothing -> objExpr
        oa = RawObjArr (Just guardObjExpr) FunctionObj Nothing [] (Just (Just arr, Nothing)) Nothing

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/Language-Haskell-Syntax-Binds.html#t:LHsBindsLR
convertBindLR :: DynFlags -> HsBind GhcPs -> [RawStatementTree RawExpr ()]
convertBindLR flags p@(FunBind _ i (MG _ lmatches _) _) = map wrap $ concatMap (conv . unLoc) $ unLoc lmatches
  where
    conv = convertMatch flags (Just $ rawVal $ convertIdP flags $ unLoc i)
    wrap (Nothing, oa, subStatements) = RawStatementTree (RawDeclStatement oa) subStatements
    wrap _ = error $ printf "Convert unsupported convertBindLR wrap:\n%s" (showSDoc flags $ ppr p)
convertBindLR flags (PatBind _ lhs rhs _) = case convertGRHSs flags rhs of
  [(Nothing, arrExpr, subs, maybeGuard)] -> [RawStatementTree (RawDeclStatement $ RawObjArr (Just guardObj) PatternObj Nothing [] (Just (Just arrExpr, Nothing)) Nothing) subs]
    where
      obj = convertPattern flags Nothing $ unLoc lhs
      guardObj = maybe obj (RawWhere emptyMetaN obj) maybeGuard
  _ -> undefined
convertBindLR flags p@VarBind{} = error $ printf "Convert unsupported bindLR:\n%s" (showSDoc flags $ ppr p)
convertBindLR flags p@PatSynBind{} = error $ printf "Convert unsupported bindLR:\n%s" (showSDoc flags $ ppr p)

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/Language-Haskell-Syntax-Type.html#t:HsTyVarBndr
convertTyVarBndr :: DynFlags -> HsTyVarBndr () GhcPs -> TypeVarName
convertTyVarBndr flags (UserTyVar _ _ v) = partialKey $ convertIdP flags $ unLoc v
convertTyVarBndr flags p = error $ printf "Convert unsupported TyVarBndr:\n%s" (showSDoc flags $ ppr p)

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/Language-Haskell-Syntax-Type.html#t:ConDeclField
convertConDeclField :: DynFlags -> RawExpr () -> ConDeclField GhcPs -> RawExpr ()
convertConDeclField flags base (ConDeclField _ names tp _doc) = base `applyRawEIArgs` map ((,IArgNothing) . withType . rawVal . convertIdP flags . unLoc . foLabel . unLoc) names
  where
    withType :: RawExpr () -> RawExpr ()
    withType e = rawVal operatorType `applyRawArgs` [(Just $ partialKey operatorArgL, e), (Just $ partialKey operatorArgR, convertTypeToExpr flags Nothing $ unLoc tp)]


-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/GHC-Hs-Decls.html#t:ConDecl
convertConDecl :: DynFlags -> ConDecl GhcPs -> RawExpr ()
convertConDecl flags (ConDeclGADT _ [name] _ _ctx args _res _) = convertConDeclDetails (rawVal (convertIdP flags $ unLoc name)) args
  where
    -- https://www.stackage.org/haddock/lts-21.25/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Types.html#t:HsConDetails
    convertConDeclDetails :: RawExpr () -> HsConDeclGADTDetails GhcPs -> RawExpr ()
    convertConDeclDetails base (PrefixConGADT as) = foldl (convertTypeToExpr flags . Just) base $ map (unLoc . hsScaledThing) as
    convertConDeclDetails base (RecConGADT r _) = foldl (convertConDeclField flags) base $ map unLoc $ unLoc r
convertConDecl flags p@ConDeclGADT{} = error $ printf "Convert unsupported ConDeclGADT:\n%s" (showSDoc flags $ ppr p)
convertConDecl flags (ConDeclH98 _ name _ [] Nothing args Nothing) = convertConDeclDetails (rawVal (convertIdP flags $ unLoc name)) args
  where
    -- https://www.stackage.org/haddock/lts-21.25/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Types.html#t:HsConDetails
    convertConDeclDetails :: RawExpr () -> HsConDeclH98Details GhcPs -> RawExpr ()
    convertConDeclDetails base (PrefixCon _ as) = foldl (convertTypeToExpr flags . Just) base $ map (unLoc . hsScaledThing) as
    convertConDeclDetails base (RecCon r) = foldl (convertConDeclField flags) base $ map unLoc $ unLoc r
    convertConDeclDetails _ InfixCon{} = error "Convert unsupported convertConDeclDetails:"
convertConDecl flags p@ConDeclH98{} = error $ printf "Convert unsupported ConDeclH98:\n%s" (showSDoc flags $ ppr p)

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/Language-Haskell-Syntax-Type.html#t:HsSigType
unSig :: HsSigType GhcPs -> HsType GhcPs
unSig (HsSig _ _ t) = unLoc t

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/Language-Haskell-Syntax-Type.html#t:HsSigType
convertSigToExpr :: DynFlags -> HsSigType GhcPs -> RawExpr ()
convertSigToExpr flags t = convertTypeToExpr flags Nothing $ unSig t

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/GHC-Hs-Decls.html#t:DerivClauseTys
convertDerivingClauseTys :: DynFlags -> DerivClauseTys GhcPs -> [RawExpr ()]
convertDerivingClauseTys flags (DctSingle _ t) = pure $ convertSigToExpr flags $ unLoc t
convertDerivingClauseTys flags (DctMulti _ t) = map (convertSigToExpr flags . unLoc) t

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/GHC-Hs-Decls.html#t:HsDerivingClause
convertDerivingClause :: DynFlags -> HsDerivingClause GhcPs -> [RawExpr ()]
convertDerivingClause flags (HsDerivingClause _ _derivingStrategy c) = convertDerivingClauseTys flags $ unLoc c

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/Language-Haskell-Syntax-Type.html#t:LHsQTyVars
convertQTyVars :: DynFlags -> RawExpr () -> LHsQTyVars GhcPs -> RawExpr ()
convertQTyVars flags base (HsQTvs _ vars) = base `applyRawExprVars` map ((, Nothing) . convertTyVarBndr flags . unLoc) vars

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/GHC-Hs-Decls.html#t:TyClDecl
convertTyClDecl :: DynFlags -> TyClDecl GhcPs -> [RawStatementTree RawExpr ()]
convertTyClDecl flags p@FamDecl{} = error $ printf "Convert unsupported TyClDecl:\n%s" (showSDoc flags $ ppr p)
convertTyClDecl flags (SynDecl _ name vars _ rhs) = [RawStatementTree (classWithObjs (convertQTyVars flags (rawVal $ convertIdP flags $ unLoc name) vars) [convertTypeToExpr flags Nothing $ unLoc rhs] []) []]
convertTyClDecl flags (DataDecl _ name vars _fixity (HsDataDefn _ _ _ Nothing Nothing cons derivs)) = [RawStatementTree (classWithObjs (convertQTyVars flags (rawVal $ convertIdP flags $ unLoc name) vars) (map (convertConDecl flags . unLoc) cons) (concatMap (convertDerivingClause flags . unLoc) derivs)) []]
convertTyClDecl flags (ClassDecl _ _ name vars _ [] sigs defs [] [] []) = [RawStatementTree (classDeclSt (convertQTyVars flags (rawVal $ convertIdP flags $ unLoc name) vars) []) (sigs' ++ defs')]
  where
    sigs' = concatMap (convertSignature flags . unLoc) sigs
    defs' = concatMap (convertBindLR flags . unLoc) $ bagToList defs
convertTyClDecl flags p = error $ printf "Convert unsupported TyClDecl:\n%s" (showSDoc flags $ ppr p)

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/GHC-Hs-Decls.html#t:InstDecl
convertInstDecl :: DynFlags -> InstDecl GhcPs -> [RawStatementTree RawExpr ()]
convertInstDecl flags (ClsInstD _ (ClsInstDecl _ polyTy binds sigs [] [] _)) = [RawStatementTree (classInstSt $ mkClassDef $ convertSigToExpr flags $ unLoc polyTy) (sigs' ++ binds')]
  where
    mkClassDef (RawVarsApply _ (RawValue _ b) [v]) = (fromJust $ roaObj v, [rawVal b])
    mkClassDef (RawWhere _ b c) = let (b', clsB) = mkClassDef b
                                      (_, clsC) = mkClassDef c
                                   in (b', clsB ++ clsC)
    mkClassDef e = error $ printf "Unsupported mkClassDef from %s" (show e)
    sigs' = concatMap (convertSignature flags . unLoc) sigs
    binds' = concatMap (convertBindLR flags . unLoc) $ bagToList binds
convertInstDecl flags p@DataFamInstD{} = error $ printf "Convert unsupported inst decl:\n%s" (showSDoc flags $ ppr p)
convertInstDecl flags p@TyFamInstD{} = error $ printf "Convert unsupported inst decl:\n%s" (showSDoc flags $ ppr p)
convertInstDecl flags p = error $ printf "Convert unsupported inst decl:\n%s" (showSDoc flags $ ppr p)

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/GHC-Hs-Decls.html#t:HsDecl
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

-- https://hackage.haskell.org/package/ghc-lib-parser-9.4.8.20231111/docs/GHC-Hs.html
convertModule :: DynFlags -> HsModule -> RawPrgm ()
convertModule flags (HsModule _ _ (Just name) Nothing imports decls Nothing Nothing) = (map (convertImport flags . unLoc) imports, [RawStatementTree (rawModule ("/Haskell/M/" ++ moduleNameSlashes (unLoc name))) (concatMap (convertDecl flags . unLoc) decls)])
convertModule flags p = error $ printf "Convert unsupported Module:\n%s" (showSDoc flags $ ppr p)
