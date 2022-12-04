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

import           CRes
import           Constants
import           Data.Either
import qualified Data.HashMap.Strict     as H
import           Data.Hashable
import           Data.List               (intercalate)
import           Data.String.Builder     (build)
import           Semantics
import           Semantics.Prgm
import           Syntax.Ct.Formatter     (formatStatementTree)
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Text.Printf

showDeclTrees :: [PDeclTree] -> String
showDeclTrees trees = intercalate "" $ map (\(d, ss) -> build $ formatStatementTree True 0 $ RawStatementTree (RawDeclStatement d) ss) trees

ifDeclPreprocessor :: PDeclTree -> CRes [PDeclTree]
ifDeclPreprocessor r@(RawDecl declLhs (Just expr), subStatements) = return [(decl', matchDecls ++ subStatements')]
  where
    condName = "$" ++ take 6 (printf "%08x" (hash r))
    argName = condName ++ "-arg"

    -- Main declaration
    expr' = case exprAppliedArgs expr of
      [TupleArgO _ matching] -> rawVal condName `applyRawArgs` [(Just argName, rawVal "/Catln/ThenElse/fromBool" `applyRawArgs` [(Just "v", matching)])]
      _ -> error "Invalid matching expression"
    decl' = RawDecl declLhs (Just expr')

    -- Pattern declarations
    DeclLHS _ (Pattern declObj _) = declLhs
    (subStatements', matchDecls) = partitionEithers $ map mapEitherMatchStatements subStatements
    mapEitherMatchStatements (RawStatementTree (RawDeclStatement (RawDecl (DeclLHS matchDeclM (Pattern matchObj matchGuard)) (Just matchExpr))) matchSubStatements) = Right (RawStatementTree (RawDeclStatement (RawDecl matchDeclLhs' (Just matchExpr))) matchSubStatements)
      where
        matchArg = [(Just argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) (eobjExpr matchObj))]
        matchDeclLhs' = DeclLHS (emptyMetaM "obj" matchDeclM) (Pattern (mkRawExprObj FunctionObj (H.toList $ exprAppliedVars $ eobjExpr declObj) matchArg Nothing condName) matchGuard)

    mapEitherMatchStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherMatchStatements _ = error "Invalid subStatement in matchDeclPreprocessor"
ifDeclPreprocessor _ = error "Invalid ifDeclPreprocessor"

matchDeclPreprocessor :: PDeclTree -> CRes [PDeclTree]
matchDeclPreprocessor r@(RawDecl declLhs (Just expr), subStatements) = return [(decl', matchDecls ++ subStatements')]
  where
    condName = "$" ++ take 6 (printf "%08x" (hash r))
    argName = condName ++ "-arg"

    -- Main declaration
    expr' = case exprAppliedArgs expr of
      [TupleArgO _ matching] -> applyRawArgs (rawVal condName) [(Just argName, matching)]
      _ -> error "Invalid matching expression"
    decl' = RawDecl declLhs (Just expr')

    -- Pattern declarations
    DeclLHS _ (Pattern declObj _) = declLhs
    (subStatements', matchDecls) = partitionEithers $ map mapEitherMatchStatements subStatements
    mapEitherMatchStatements (RawStatementTree (RawDeclStatement (RawDecl (DeclLHS matchDeclM (Pattern matchObj matchGuard)) (Just matchExpr))) matchSubStatements) = Right (RawStatementTree (RawDeclStatement (RawDecl matchDeclLhs' (Just matchExpr))) matchSubStatements)
      where
        matchArg = [(Just argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) (eobjExpr matchObj))]
        matchDeclLhs' = DeclLHS (emptyMetaM "obj" matchDeclM) (Pattern (mkRawExprObj FunctionObj (H.toList $ exprAppliedVars $ eobjExpr declObj) matchArg Nothing condName) matchGuard)

    mapEitherMatchStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherMatchStatements _ = error "Invalid subStatement in matchDeclPreprocessor"
matchDeclPreprocessor _ = error "Invalid matchDeclPreprocessor"

caseDeclPreprocessor :: PDeclTree -> CRes [PDeclTree]
caseDeclPreprocessor r@(RawDecl declLhs (Just expr), subStatements) = return [(decl', cases' ++ subStatements')]
  where
    baseCondName = "$" ++ take 6 (printf "%08x" (hash r))
    argName = baseCondName ++ "-arg"
    condName :: Int -> String
    condName i = printf "%s-%d" baseCondName i

    -- Main Declaration
    [TupleArgO _ matchingExpr] = exprAppliedArgs expr
    expr' = applyRawArgs (rawVal (condName 0)) [(Just argName, matchingExpr)]
    decl' = RawDecl declLhs (Just expr')

    -- Find cases
    (subStatements', cases) = partitionEithers $ map mapEitherCaseStatements subStatements
    mapEitherCaseStatements (RawStatementTree (RawDeclStatement decl) subs) = Right (decl, subs)
    mapEitherCaseStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherCaseStatements _ = error "Invalid subStatement in case preprocessor"

    initCase' = zipWith (curry buildInitCase) [0..] (init cases)
    buildInitCase (i, (RawDecl (DeclLHS caseDeclM (Pattern caseObj caseGuard)) (Just caseExpr), caseSubStatements)) = [matchingCase, fallthroughCase]
      where
        declObj = mkRawExprObj FunctionObj [] [(Just argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) (eobjExpr caseObj))] Nothing (condName i)
        matchingCase = RawStatementTree (RawDeclStatement (RawDecl (DeclLHS (emptyMetaM "matching" caseDeclM) (Pattern declObj caseGuard)) (Just caseExpr))) caseSubStatements
        fallthroughExpr = applyRawArgs (rawVal (condName $ i + 1)) [(Just argName, matchingExpr)]
        fallthroughCase = RawStatementTree (RawDeclStatement (RawDecl (DeclLHS (emptyMetaM "fallthrough" caseDeclM) (Pattern declObj ElseGuard)) (Just fallthroughExpr))) []
    buildInitCase (_, decl) = error $ printf "Missing expression in buildInitCase %s" (show decl)

    (RawDecl (DeclLHS lastCaseDeclM (Pattern lastCaseObj lastCaseGuard)) (Just lastCaseExpr), lastCaseSubStatements) = last cases
    lastCaseDeclObj = mkRawExprObj FunctionObj [] [(Just argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) (eobjExpr lastCaseObj))] Nothing (condName $ length cases - 1)
    lastCase' = RawStatementTree (RawDeclStatement (RawDecl (DeclLHS (emptyMetaM "fallthrough" lastCaseDeclM) (Pattern lastCaseDeclObj lastCaseGuard)) (Just lastCaseExpr))) lastCaseSubStatements
    cases' = concat initCase' ++ [lastCase']
caseDeclPreprocessor _ = error "Invalid caseDeclPreprocessor"

-- | A declPreprocessor for multi-line expressions. Will search for the final result expression and move it to the top
nestedDeclPreprocessor :: PDeclTree -> CRes [PDeclTree]
nestedDeclPreprocessor decl@(RawDecl lhs _, subStatements) = case exprs' of
  [e] -> return [(RawDecl lhs (Just e), subStatements')]
  [] -> fail $ printf "Found no output expressions in nested declaration: %s" (show decl)
  (_:_:_) -> fail $ printf "Found multiple output expressions in nested declaration: %s" (show decl)

  where
    (subStatements', exprs') = partitionEithers $ map findExpression subStatements
    findExpression (RawStatementTree (RawExprStatement expr) _) = Right expr
    findExpression e                                            = Left e

-- | Used for no declPreprocessor
defaultDeclPreprocessor :: PDeclTree -> CRes [PDeclTree]
defaultDeclPreprocessor declTree = return [declTree]

declPreprocessors :: PDeclTree -> CRes [PDeclTree]
declPreprocessors declTree@(RawDecl _ (Just expr), _) = case maybeExprPath expr of
  Just "if"                             -> ifDeclPreprocessor declTree
  Just "match"                          -> matchDeclPreprocessor declTree
  Just "case"                           -> caseDeclPreprocessor declTree
  Just path | path == nestedDeclaration -> nestedDeclPreprocessor declTree
  _                                     -> defaultDeclPreprocessor declTree
declPreprocessors declTree = defaultDeclPreprocessor declTree
