--------------------------------------------------------------------
-- |
-- Module    :  Desugarf.Preprocessors
-- Copyright :  (c) Zach Kimberg 2022
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module runs declaration preprocessors.
-- such as where, case, and if.
-- language features outside of a minimalist core set.
--------------------------------------------------------------------

module Syntax.Ct.Desugarf.Preprocessors where

import           Constants
import           CRes
import           Data.Either
import           Data.Hashable
import qualified Data.HashMap.Strict     as H
import           Data.List               (intercalate)
import           Data.String.Builder     (build)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Formatter     (formatStatementTree)
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Text.Printf

showDeclTrees :: [PDeclTree] -> String
showDeclTrees trees = intercalate "" $ map (\(d, ss) -> build $ formatStatementTree True 0 $ RawStatementTree (RawDeclStatement d) ss) trees

ifDeclPreprocessor :: PDeclTree -> CRes [PDeclTree]
ifDeclPreprocessor (roa@RawObjArr{roaObj=Just (GuardExpr declObj _), roaArr=Just (Just (GuardExpr expr Nothing), _)}, subStatements) = return [(decl', matchDecls ++ subStatements')]
  where
    condName = "$" ++ take 6 (printf "%08x" (hash roa))
    argName = condName ++ "-arg"

    -- Main declaration
    expr' = case exprAppliedArgs expr of
      [ObjArr{oaObj=Just (GuardExpr matching _)}] -> rawVal condName `applyRawArgs` [(Just $ partialKey argName, rawVal "/Catln/ThenElse/fromBool" `applyRawArgs` [(Just $ partialKey "v", matching)])]
      _ -> error "Invalid matching expression"
    decl' = roa{roaArr=Just (Just (GuardExpr expr' Nothing), emptyMetaE "arrM" expr')}

    -- Pattern declarations
    (subStatements', matchDecls) = partitionEithers $ map mapEitherMatchStatements subStatements
    mapEitherMatchStatements (RawStatementTree (RawDeclStatement RawObjArr{roaObj=(Just (GuardExpr matchObj matchGuard)), roaAnnots, roaArr=(Just (Just (GuardExpr matchExpr _), matchDeclM))}) matchSubStatements) = Right (RawStatementTree (RawDeclStatement matchDeclLhs') matchSubStatements)
      where
        matchArg = [(Just $ partialKey argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) matchObj)]
        matchDeclLhs' = RawObjArr (Just $ GuardExpr (rawVal condName `applyRawExprVars` H.toList (exprAppliedVars declObj)`applyRawArgs` matchArg) matchGuard) FunctionObj Nothing roaAnnots (Just (Just (GuardExpr matchExpr Nothing), emptyMetaM "obj" matchDeclM)) Nothing

    mapEitherMatchStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherMatchStatements _ = error "Invalid subStatement in matchDeclPreprocessor"
ifDeclPreprocessor _ = error "Invalid ifDeclPreprocessor"

matchDeclPreprocessor :: PDeclTree -> CRes [PDeclTree]
matchDeclPreprocessor (roa@RawObjArr{roaObj=Just (GuardExpr declObj _), roaArr=Just (Just (GuardExpr expr Nothing), _)}, subStatements) = return [(decl', matchDecls ++ subStatements')]
  where
    condName = "$" ++ take 6 (printf "%08x" (hash roa))
    argName = condName ++ "-arg"

    -- Main declaration
    expr' = case exprAppliedArgs expr of
      [ObjArr{oaObj=Just (GuardExpr matching _)}] -> applyRawArgs (rawVal condName) [(Just $ partialKey argName, matching)]
      _ -> error "Invalid matching expression"
    decl' = roa{roaArr=Just (Just (GuardExpr expr' Nothing), emptyMetaE "arrM" expr')}

    -- Pattern declarations
    (subStatements', matchDecls) = partitionEithers $ map mapEitherMatchStatements subStatements
    mapEitherMatchStatements (RawStatementTree (RawDeclStatement RawObjArr{roaObj=(Just (GuardExpr matchObj matchGuard)), roaAnnots, roaArr=(Just (Just (GuardExpr matchExpr _), matchDeclM))}) matchSubStatements) = Right (RawStatementTree (RawDeclStatement matchDeclLhs') matchSubStatements)
      where
        matchArg = [(Just $ partialKey argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) matchObj)]
        matchDeclLhs' = RawObjArr (Just $ GuardExpr (rawVal condName `applyRawExprVars` H.toList (exprAppliedVars declObj)`applyRawArgs` matchArg) matchGuard) FunctionObj Nothing roaAnnots (Just (Just (GuardExpr matchExpr Nothing), emptyMetaM "obj" matchDeclM)) Nothing

    mapEitherMatchStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherMatchStatements _ = error "Invalid subStatement in matchDeclPreprocessor"
matchDeclPreprocessor _ = error "Invalid matchDeclPreprocessor"

caseDeclPreprocessor :: PDeclTree -> CRes [PDeclTree]
caseDeclPreprocessor (roa@RawObjArr{roaArr=Just (Just (GuardExpr expr _), _)}, subStatements) = return [(decl', cases' ++ subStatements')]
  where
    baseCondName = "$" ++ take 6 (printf "%08x" (hash roa))
    argName = baseCondName ++ "-arg"
    condName :: Int -> String
    condName i = printf "%s-%d" baseCondName i

    -- Main Declaration
    [ObjArr{oaObj=Just (GuardExpr matchingExpr _)}] = exprAppliedArgs expr
    expr' = applyRawArgs (rawVal (condName 0)) [(Just $ partialKey argName, matchingExpr)]
    decl' = roa{roaArr=Just (Just (GuardExpr expr' Nothing), emptyMetaE "arrM" expr')}

    -- Find cases
    (subStatements', cases) = partitionEithers $ map mapEitherCaseStatements subStatements
    mapEitherCaseStatements (RawStatementTree (RawDeclStatement decl) subs) = Right (decl, subs)
    mapEitherCaseStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherCaseStatements _ = error "Invalid subStatement in case preprocessor"

    initCase' = zipWith (curry buildInitCase) [0..] (init cases)
    buildInitCase (i, (RawObjArr{roaObj=(Just (GuardExpr caseObj caseGuard)), roaAnnots, roaArr=(Just (Just (GuardExpr caseExpr _), caseDeclM))}, caseSubStatements)) = [matchingCase, fallthroughCase]
      where
        declObj = rawVal (condName i) `applyRawArgs` [(Just $ partialKey argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) caseObj)]
        matchingCase = RawStatementTree (RawDeclStatement (RawObjArr (Just (GuardExpr declObj caseGuard)) FunctionObj Nothing roaAnnots (Just (Just (GuardExpr caseExpr Nothing), emptyMetaM "matching" caseDeclM)) Nothing)) caseSubStatements
        fallthroughExpr = applyRawArgs (rawVal (condName $ i + 1)) [(Just $ partialKey argName, matchingExpr)]
        fallthroughCase = RawStatementTree (RawDeclStatement (RawObjArr (Just (GuardExpr declObj Nothing)) FunctionObj Nothing [rawVal elseAnnot] (Just (Just (GuardExpr fallthroughExpr Nothing), emptyMetaM "fallthrough" caseDeclM)) Nothing)) []
    buildInitCase (_, decl) = error $ printf "Missing expression in buildInitCase %s" (show decl)

    (RawObjArr{roaObj=(Just (GuardExpr lastCaseObj lastCaseGuard)), roaArr=(Just (Just (GuardExpr lastCaseExpr _), lastCaseDeclM))}, lastCaseSubStatements) = last cases
    lastCaseDeclObj = rawVal (condName $ length cases - 1) `applyRawArgs` [(Just $ partialKey argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) lastCaseObj)]
    lastCase' = RawStatementTree (RawDeclStatement (RawObjArr (Just (GuardExpr lastCaseDeclObj lastCaseGuard)) FunctionObj Nothing [] (Just (Just (GuardExpr lastCaseExpr Nothing), emptyMetaM "fallthrough" lastCaseDeclM)) Nothing)) lastCaseSubStatements
    cases' = concat initCase' ++ [lastCase']
caseDeclPreprocessor _ = error "Invalid caseDeclPreprocessor"

-- | A declPreprocessor for multi-line expressions. Will search for the final result expression and move it to the top
nestedDeclPreprocessor :: PDeclTree -> CRes [PDeclTree]
nestedDeclPreprocessor (oa, subStatements) = case exprs' of
  [e] -> return [(oa{roaArr=Just (Just (GuardExpr e Nothing), emptyMetaE "arrM" e)}, subStatements')]
  [] -> fail $ printf "Found no output expressions in nested declaration: %s" (show oa)
  (_:_:_) -> fail $ printf "Found multiple output expressions in nested declaration: %s" (show oa)

  where
    (subStatements', exprs') = partitionEithers $ map findExpression subStatements
    findExpression (RawStatementTree (RawExprStatement expr) _) = Right expr
    findExpression e                                            = Left e

-- | Used for no declPreprocessor
defaultDeclPreprocessor :: PDeclTree -> CRes [PDeclTree]
defaultDeclPreprocessor declTree = return [declTree]

declPreprocessors :: PDeclTree -> CRes [PDeclTree]
declPreprocessors declTree@(RawObjArr{roaArr=Just (Just (GuardExpr expr _), _)}, _) = case maybeExprPath expr of
  Just "if"                             -> ifDeclPreprocessor declTree
  Just "match"                          -> matchDeclPreprocessor declTree
  Just "case"                           -> caseDeclPreprocessor declTree
  Just path | path == nestedDeclaration -> nestedDeclPreprocessor declTree
  _                                     -> defaultDeclPreprocessor declTree
declPreprocessors declTree = defaultDeclPreprocessor declTree
