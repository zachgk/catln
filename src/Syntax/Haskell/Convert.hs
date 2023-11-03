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
import           Constants
import           Data.Bifunctor          (first)
import           Data.Maybe              (fromJust, maybeToList)
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

-- https://wiki.haskell.org/Import
convertImport :: DynFlags -> ImportDecl GhcPs -> RawFileImport
convertImport _ (ImportDecl _ _ name Nothing _ _ NotQualified _ Nothing Nothing) = RawCExpr emptyMetaN $ CStr $ moduleNameSlashes $ unLoc name
convertImport _ (ImportDecl _ _ name Nothing _ _ _ _ maybeQualifiedAlias Nothing) = rawVal "haskell" `applyRawArgs` [(Nothing, RawCExpr emptyMetaN $ CStr $ moduleNameSlashes $ unLoc name), (Just $ partialKey "qualified", rawVal "True")] `applyRawArgs` [(Just $ partialKey "alias", RawCExpr emptyMetaN $ CStr $ moduleNameSlashes $ unLoc qualifiedAlias) | qualifiedAlias <- maybeToList maybeQualifiedAlias]
convertImport _ (ImportDecl _ _ name Nothing _ _ NotQualified _ Nothing (Just (_hiding, _names))) = RawCExpr emptyMetaN $ CStr $ moduleNameSlashes $ unLoc name -- TODO: Handle hiding and partial import
convertImport flags p = error $ printf "Convert unsupported import:\n%s\n%s" (showSDoc flags $ ppr p) (showSDocDebug flags $ ppr p)

convertIdP :: DynFlags -> IdP GhcPs -> String
convertIdP _ (Unqual n) = occNameString n
convertIdP _ (Qual m n) = moduleNameSlashes m ++ occNameString n
convertIdP flags p@Orig{} = error $ printf "Convert unsupported IdP:\n%s" (showSDoc flags $ ppr p)
convertIdP _ (Exact n) = nameStableString n

convertTypeToExpr :: DynFlags -> Maybe (RawExpr ()) -> HsType GhcPs -> RawExpr ()
convertTypeToExpr flags _ p@HsForAllTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsQualTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
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
convertTypeToExpr flags Nothing (HsTupleTy _ _ tp) = rawVal "" `applyRawArgs` map ((Just $ partialKey "C",) . convertTypeToExpr flags Nothing . unLoc) tp
convertTypeToExpr flags _ p@HsSumTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsOpTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags i (HsParTy _ t) = convertTypeToExpr flags i $ unLoc t
convertTypeToExpr flags _ p@HsIParamTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsStarTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsKindSig{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsSpliceTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsDocTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsBangTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsRecTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsExplicitListTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsExplicitTupleTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsTyLit{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@HsWildCardTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p@XHsType{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToExpr flags _ p = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Types.html#t:HsType
convertTypeToObj :: DynFlags -> String -> HsType GhcPs -> RawObjArr RawExpr ()
convertTypeToObj flags _ p@HsForAllTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@HsAppKindTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags i (HsFunTy _ base app) = RawObjArr (Just (convertTypeToExpr flags (Just $ RawValue emptyMetaN i) $ unLoc base)) FunctionObj Nothing [] (Just (Nothing, emptyMetaT $ exprToType $ convertTypeToExpr flags Nothing $ unLoc app)) Nothing
convertTypeToObj flags _ p@HsListTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
convertTypeToObj flags _ p@HsTupleTy{} = error $ printf "Convert unsupported type:\n%s" (showSDoc flags $ ppr p)
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

convertRecordFields :: DynFlags -> RawExpr () -> HsRecFields GhcPs (LPat GhcPs) -> RawExpr ()
convertRecordFields flags base (HsRecFields fields Nothing) = foldl convertRecordField base (map unLoc fields)
  where
    convertRecordField :: RawExpr () -> HsRecField' (FieldOcc GhcPs) (LPat GhcPs) -> RawExpr ()
    convertRecordField b (HsRecField lbl arg _) = b `applyRawArgs` [(Just $ partialKey $ convertIdP flags $ unLoc $ rdrNameFieldOcc $ unLoc lbl, convertPattern flags Nothing $ unLoc arg)]
convertRecordFields flags _ p = error $ printf "Convert unsupported record fields:\n%s" (showSDoc flags $ ppr p)


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

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Types.html#t:HsConDetails
convertPatDetails :: DynFlags -> Maybe (RawExpr ()) -> HsConDetails (LPat GhcPs) (HsRecFields GhcPs (LPat GhcPs)) -> RawExpr ()
convertPatDetails _ Nothing (PrefixCon []) = RawHoleExpr emptyMetaN (HoleActive Nothing)
convertPatDetails flags base (PrefixCon args) = fromJust $ foldl (\b p -> Just $ convertPattern flags b p) base $ map unLoc args
convertPatDetails flags (Just base) (RecCon r) = convertRecordFields flags base r
convertPatDetails flags Nothing (InfixCon hd tl) = rawVal ctListCons`applyRawArgs` [(Just $ partialKey ctListConsHead, convertPattern flags Nothing $ unLoc hd), (Just $ partialKey ctListConsTail, convertPattern flags Nothing $ unLoc tl)]
convertPatDetails flags _ p = error $ printf "Convert unsupported conPatDetails:\n%s" (showSDoc flags $ ppr p)

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
convertPattern flags (Just base) (ConPatIn i det) = base `applyRawArgs` [(Just $ partialKey $ convertIdP flags $ unLoc i, convertPatDetails flags Nothing det)]
convertPattern flags Nothing (ConPatIn i det) = convertPatDetails flags (Just $ rawVal $ convertIdP flags $ unLoc i) det
convertPattern flags _ p@ConPatOut{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags _ p@ViewPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags _ p@SplicePat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags _ p@LitPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags _ p@NPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags _ p@NPlusKPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags _ p@SigPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags _ p@CoPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)
convertPattern flags _ p@XPat{} = error $ printf "Convert unsupported pattern:\n%s" (showSDoc flags $ ppr p)

convertSimpleExpr :: DynFlags -> HsExpr GhcPs -> RawExpr ()
convertSimpleExpr flags e = case convertExpr flags e of
  (e', []) -> e'
  (e', subs) -> error $ printf "convertSimpleExpr failed from %s to %s with subs %s" (showSDoc flags $ ppr e) (show e') (show subs)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Expr.html#t:HsExpr
convertExpr :: DynFlags -> HsExpr GhcPs -> (RawExpr (), [RawStatementTree RawExpr ()])
convertExpr flags (HsVar _ v) = (rawVal $ convertIdP flags $ unLoc v, [])
convertExpr flags p@HsUnboundVar{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsConLikeOut{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsRecFld{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsOverLabel{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsIPVar{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsOverLit{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags (HsLit _ literal) = (convertLiteral literal, [])
  where
    convertLiteral :: HsLit GhcPs -> RawExpr ()
    convertLiteral (HsString _ s) = RawCExpr emptyMetaN $ CStr $ unpackFS s
    convertLiteral l = error $ printf "Convert unsupported lit:\n%s" (showSDoc flags $ ppr l)
convertExpr flags p@HsLam{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsLamCase{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags (HsApp _ base app) = (convertSimpleExpr flags (unLoc base) `applyRawArgs` [(Nothing, convertSimpleExpr flags $ unLoc app)], [])
convertExpr flags p@HsAppType{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags (OpApp _ l op r) = (convertSimpleExpr flags (unLoc op) `applyRawArgs` [(Just $ partialKey operatorArgL, convertSimpleExpr flags (unLoc l)), (Just $ partialKey operatorArgR, convertSimpleExpr flags (unLoc r))], [])
convertExpr flags p@NegApp{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags (HsPar _ p) = convertExpr flags $ unLoc p
convertExpr flags p@SectionL{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@SectionR{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags (ExplicitTuple _ t _) = (rawVal "" `applyRawArgs` map ((Nothing,) . convertTupleArg . unLoc) t, [])
  where
    convertTupleArg :: HsTupArg GhcPs -> RawExpr ()
    convertTupleArg (Present _ a) = convertSimpleExpr flags $ unLoc a
    convertTupleArg _             = error "Unknown TupleArg"
convertExpr flags p@ExplicitSum{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags (HsCase _ cas (MG _ lmatches _)) = (rawVal ctCase `applyRawArgs` [(Nothing, convertSimpleExpr flags $ unLoc cas)], map (uncurry RawStatementTree . first RawDeclStatement . convertMatch flags Nothing . unLoc) $ unLoc lmatches)
convertExpr flags (HsIf _ _ i t e) = (RawMethod (convertSimpleExpr flags (unLoc i)) (rawVal ctIf `applyRawArgs` [(Just $ partialKey ctThen, convertSimpleExpr flags $ unLoc t), (Just $ partialKey ctElse, convertSimpleExpr flags $ unLoc e)]), [])
convertExpr flags p@HsMultiIf{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsLet{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags (HsDo _ _ statements) = (rawVal nestedDeclaration, concatMap (convertStmtLR flags . unLoc) $ unLoc statements)
convertExpr flags (ExplicitList _ Nothing l) = (RawList emptyMetaN $ map (convertSimpleExpr flags . unLoc) l, [])
convertExpr flags p@RecordCon{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@RecordUpd{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@ExprWithTySig{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@ArithSeq{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsSCC{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsCoreAnn{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsBracket{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsRnBracketOut{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsTcBracketOut{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsSpliceE{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsProc{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsStatic{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsTick{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsBinTick{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsTickPragma{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@HsWrap{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p@XExpr{} = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)
convertExpr flags p = error $ printf "Convert unsupported expr:\n%s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Expr.html#t:StmtLR
convertStmtLR :: DynFlags -> StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> [RawStatementTree RawExpr ()]
convertStmtLR flags p@LastStmt{} = error $ printf "Convert unsupported stmtLR:\n%s" (showSDoc flags $ ppr p)
convertStmtLR flags (BindStmt _ pat body _ _) = [RawStatementTree (RawDeclStatement oa) subStatements]
  where
    -- TODO: Should use bind rather than basic objArr
    pat' = convertPattern flags Nothing $ unLoc pat
    (expr', subStatements) = convertExpr flags $ unLoc body
    oa = RawObjArr (Just pat') FunctionObj Nothing [] (Just (Just expr', emptyMetaN)) Nothing
convertStmtLR flags p@ApplicativeStmt{} = error $ printf "Convert unsupported stmtLR:\n%s" (showSDoc flags $ ppr p)
convertStmtLR flags (BodyStmt _ body _ _) = [RawStatementTree (RawExprStatement expr') subStatements]
  where
    (expr', subStatements) = convertExpr flags $ unLoc body
convertStmtLR flags (LetStmt _ l) = convertLocalBindsLR flags $ unLoc l
convertStmtLR flags p@ParStmt{} = error $ printf "Convert unsupported stmtLR:\n%s" (showSDoc flags $ ppr p)
convertStmtLR flags p@TransStmt{} = error $ printf "Convert unsupported stmtLR:\n%s" (showSDoc flags $ ppr p)
convertStmtLR flags p@RecStmt{} = error $ printf "Convert unsupported stmtLR:\n%s" (showSDoc flags $ ppr p)
convertStmtLR flags p@XStmtLR{} = error $ printf "Convert unsupported stmtLR:\n%s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Expr.html#t:GRHS
convertGRHS :: DynFlags -> GRHS GhcPs (LHsExpr GhcPs) -> (RawExpr (), [RawStatementTree RawExpr ()], Maybe (RawExpr ()))
convertGRHS flags (GRHS _ g e) = (e', subStatements, g')
  where
    (e', subStatements) = convertExpr flags $ unLoc e
    g' = case g of
      [] -> Nothing
      _  -> Just $ foldl1 joinAnd $ map (convertGuardStmt . unLoc) g

    convertGuardStmt :: StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> RawExpr ()
    convertGuardStmt (BodyStmt _ body _ _) = convertSimpleExpr flags $ unLoc body
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
convertGRHSs :: DynFlags -> GRHSs GhcPs (LHsExpr GhcPs) -> (RawExpr (), [RawStatementTree RawExpr ()], Maybe (RawExpr ()))
convertGRHSs flags (GRHSs _ [grhs] binds) = (rhs', binds' ++ subStatements, guard')
  where
    (rhs', subStatements, guard') = convertGRHS flags $ unLoc grhs
    binds' = convertLocalBindsLR flags $ unLoc binds
convertGRHSs _ _ = error $ printf "Convert unsupported grhs:\n"

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Expr.html#t:MatchGroup
convertMatch :: DynFlags -> Maybe String -> Match GhcPs (LHsExpr GhcPs) -> (RawObjArr RawExpr (), [RawStatementTree RawExpr ()])
convertMatch flags i (Match _ _ pats grhs) = (oa, subStatements)
  where
    (arr, subStatements, guard) = convertGRHSs flags grhs
    objExpr = fromJust (foldl (\p b -> Just $ convertPattern flags p b) (fmap rawVal i) (map unLoc pats))
    guardObjExpr = case guard of
      Just g  -> RawWhere objExpr g
      Nothing -> objExpr
    oa = RawObjArr (Just guardObjExpr) FunctionObj Nothing [] (Just (Just arr, emptyMetaN)) Nothing
convertMatch flags _ p = error $ printf "Convert unsupported match:\n%s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Binds.html#t:HsBindLR
convertBindLR :: DynFlags -> HsBind GhcPs -> [RawStatementTree RawExpr ()]
convertBindLR flags (FunBind _ i (MG _ lmatches _) _ _) = map (aux . unLoc) $ unLoc lmatches
  where
    aux match = let (oa, subStatements) = convertMatch flags (Just $ convertIdP flags $ unLoc i) match
                 in RawStatementTree (RawDeclStatement oa) subStatements
convertBindLR flags p@PatBind{} = error $ printf "Convert unsupported bindLR:\n%s" (showSDoc flags $ ppr p)
convertBindLR flags p@VarBind{} = error $ printf "Convert unsupported bindLR:\n%s" (showSDoc flags $ ppr p)
convertBindLR flags p@AbsBinds{} = error $ printf "Convert unsupported bindLR:\n%s" (showSDoc flags $ ppr p)
convertBindLR flags p@PatSynBind{} = error $ printf "Convert unsupported bindLR:\n%s" (showSDoc flags $ ppr p)
convertBindLR flags p@XHsBindsLR{} = error $ printf "Convert unsupported bindLR:\n%s" (showSDoc flags $ ppr p)
convertBindLR flags p = error $ printf "Convert unsupported bindLR:\n%s" (showSDoc flags $ ppr p)

convertTyVarBndr :: DynFlags -> HsTyVarBndr GhcPs -> TypeVarName
convertTyVarBndr flags (UserTyVar _ v) = partialKey $ convertIdP flags $ unLoc v
convertTyVarBndr flags p = error $ printf "Convert unsupported TyVarBndr:\n%s" (showSDoc flags $ ppr p)

convertConDeclDetails :: DynFlags -> RawExpr () -> HsConDeclDetails GhcPs -> RawExpr ()
convertConDeclDetails flags base (PrefixCon args) = foldl (convertTypeToExpr flags . Just) base $ map unLoc args
convertConDeclDetails flags _ p = error $ printf "Convert unsupported ConDeclDetails:\n%s" (showSDoc flags $ ppr p)

convertConDecl :: DynFlags -> ConDecl GhcPs -> RawExpr ()
convertConDecl flags p@ConDeclGADT{} = error $ printf "Convert unsupported ConDecl:\n%s" (showSDoc flags $ ppr p)
convertConDecl flags (ConDeclH98 _ name _ [] Nothing args Nothing) = convertConDeclDetails flags (rawVal (convertIdP flags $ unLoc name)) args
convertConDecl flags p@XConDecl{} = error $ printf "Convert unsupported ConDecl:\n%s" (showSDoc flags $ ppr p)
convertConDecl flags p = error $ printf "Convert unsupported ConDecl:\n%s" (showSDoc flags $ ppr p)

convertDerivingClause :: DynFlags -> HsDerivingClause GhcPs -> ExtendedClasses
convertDerivingClause flags (HsDerivingClause _ Nothing c) = map (fromRawExpr . convertTypeToExpr flags Nothing . unLoc . convertIB) $ unLoc c
  where
    convertIB :: (Outputable t) => HsImplicitBndrs GhcPs t -> t
    convertIB (HsIB _ t) = t
    convertIB p = error $ printf "Convert unsupported DerivingClause:\n%s" (showSDoc flags $ ppr p)

    fromRawExpr (RawValue _ n) = n
    fromRawExpr e = error $ printf "Unsupported Deriving %s" (show e)
convertDerivingClause flags p = error $ printf "Convert unsupported DerivingClause:\n%s" (showSDoc flags $ ppr p)

convertQTyVars :: DynFlags -> String -> LHsQTyVars GhcPs -> RawExpr ()
convertQTyVars flags name (HsQTvs _ vars) = rawVal name `applyRawExprVars` map ((, emptyMetaN) . convertTyVarBndr flags . unLoc) vars
convertQTyVars flags _ p = error $ printf "Convert unsupported DerivingClause:\n%s" (showSDoc flags $ ppr p)

-- https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/GHC-Hs-Decls.html#t:TyClDecl
convertTyClDecl :: DynFlags -> TyClDecl GhcPs -> [RawStatementTree RawExpr ()]
convertTyClDecl flags p@FamDecl{} = error $ printf "Convert unsupported TyClDecl:\n%s" (showSDoc flags $ ppr p)
convertTyClDecl flags (SynDecl _ name vars _ rhs) = [RawStatementTree (MultiTypeDefStatement $ MultiTypeDef (convertQTyVars flags (convertIdP flags $ unLoc name) vars) [convertTypeToExpr flags Nothing $ unLoc rhs] []) []]
convertTyClDecl flags (DataDecl _ name vars _fixity (HsDataDefn _ _ _ Nothing Nothing cons derivs)) = [RawStatementTree (MultiTypeDefStatement $ MultiTypeDef (convertQTyVars flags (convertIdP flags $ unLoc name) vars) (map (convertConDecl flags . unLoc) cons) (concatMap (convertDerivingClause flags . unLoc) $ unLoc derivs)) []]
-- convertTyClDecl flags (ClassDecl _ _ name vars _ fds sigs defs ats atDefs docs) | trace (printf "fds: %d, sigs: %d, ats: %d, atDefs: %d, docs: %d" (length fds) (length sigs) (length ats) (length atDefs) (length docs)) False = undefined
convertTyClDecl flags (ClassDecl _ _ name vars _ [] sigs defs [] [] []) = [RawStatementTree (RawClassDeclStatement $ convertQTyVars flags (convertIdP flags $ unLoc name) vars) (sigs' ++ defs')]
  where
    sigs' = concatMap (convertSignature flags . unLoc) sigs
    defs' = concatMap (convertBindLR flags . unLoc) $ bagToList defs
convertTyClDecl flags p@XTyClDecl{} = error $ printf "Convert unsupported TyClDecl:\n%s" (showSDoc flags $ ppr p)
convertTyClDecl flags p = error $ printf "Convert unsupported TyClDecl:\n%s" (showSDoc flags $ ppr p)

convertInstDecl :: DynFlags -> InstDecl GhcPs -> [RawStatementTree RawExpr ()]
convertInstDecl flags (ClsInstD _ (ClsInstDecl _ (HsIB _ polyTy) binds sigs [] [] _)) = [RawStatementTree (RawClassDefStatement $ mkClassDef $ convertTypeToExpr flags Nothing $ unLoc polyTy) (sigs' ++ binds')]
  where
    mkClassDef (RawVarsApply _ (RawValue _ b) [v]) = (fst v, [b])
    mkClassDef e = error $ printf "Unsupported mkClassDef from %s" (show e)
    sigs' = concatMap (convertSignature flags . unLoc) sigs
    binds' = concatMap (convertBindLR flags . unLoc) $ bagToList binds
convertInstDecl flags p@DataFamInstD{} = error $ printf "Convert unsupported inst decl:\n%s" (showSDoc flags $ ppr p)
convertInstDecl flags p@TyFamInstD{} = error $ printf "Convert unsupported inst decl:\n%s" (showSDoc flags $ ppr p)
convertInstDecl flags p@XInstDecl{} = error $ printf "Convert unsupported inst decl:\n%s" (showSDoc flags $ ppr p)
convertInstDecl flags p = error $ printf "Convert unsupported inst decl:\n%s" (showSDoc flags $ ppr p)

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
