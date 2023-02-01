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
ifDeclPreprocessor r@(RawDecl oa@RawObjArr{roaObj=Just (RawGuardExpr declObj _), roaArr=Just (RawGuardExpr expr NoGuard)}, subStatements) = return [(decl', matchDecls ++ subStatements')]
  where
    condName = "$" ++ take 6 (printf "%08x" (hash r))
    argName = condName ++ "-arg"

    -- Main declaration
    expr' = case exprAppliedArgs expr of
      [TupleArgO _ matching] -> rawVal condName `applyRawArgs` [(Just argName, rawVal "/Catln/ThenElse/fromBool" `applyRawArgs` [(Just "v", matching)])]
      _ -> error "Invalid matching expression"
    decl' = RawDecl oa{roaArr=Just (RawGuardExpr expr' NoGuard)}

    -- Pattern declarations
    (subStatements', matchDecls) = partitionEithers $ map mapEitherMatchStatements subStatements
    mapEitherMatchStatements (RawStatementTree (RawDeclStatement (RawDecl RawObjArr{roaM=matchDeclM, roaObj=(Just (RawGuardExpr matchObj matchGuard)), roaArr=(Just (RawGuardExpr matchExpr _))})) matchSubStatements) = Right (RawStatementTree (RawDeclStatement (RawDecl matchDeclLhs')) matchSubStatements)
      where
        matchArg = [(Just argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) matchObj)]
        matchDeclLhs' = RawObjArr (Just $ RawGuardExpr (rawVal condName `applyRawExprVars` H.toList (exprAppliedVars declObj)`applyRawArgs` matchArg) matchGuard) FunctionObj Nothing [] (emptyMetaM "obj" matchDeclM) (Just (RawGuardExpr matchExpr NoGuard))

    mapEitherMatchStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherMatchStatements _ = error "Invalid subStatement in matchDeclPreprocessor"
ifDeclPreprocessor _ = error "Invalid ifDeclPreprocessor"

matchDeclPreprocessor :: PDeclTree -> CRes [PDeclTree]
matchDeclPreprocessor r@(RawDecl oa@RawObjArr{roaObj=Just (RawGuardExpr declObj _), roaArr=Just (RawGuardExpr expr NoGuard)}, subStatements) = return [(decl', matchDecls ++ subStatements')]
  where
    condName = "$" ++ take 6 (printf "%08x" (hash r))
    argName = condName ++ "-arg"

    -- Main declaration
    expr' = case exprAppliedArgs expr of
      [TupleArgO _ matching] -> applyRawArgs (rawVal condName) [(Just argName, matching)]
      _ -> error "Invalid matching expression"
    decl' = RawDecl oa{roaArr=Just (RawGuardExpr expr' NoGuard)}

    -- Pattern declarations
    (subStatements', matchDecls) = partitionEithers $ map mapEitherMatchStatements subStatements
    mapEitherMatchStatements (RawStatementTree (RawDeclStatement (RawDecl RawObjArr{roaM=matchDeclM, roaObj=(Just (RawGuardExpr matchObj matchGuard)), roaArr=(Just (RawGuardExpr matchExpr _))})) matchSubStatements) = Right (RawStatementTree (RawDeclStatement (RawDecl matchDeclLhs')) matchSubStatements)
      where
        matchArg = [(Just argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) matchObj)]
        matchDeclLhs' = RawObjArr (Just $ RawGuardExpr (rawVal condName `applyRawExprVars` H.toList (exprAppliedVars declObj)`applyRawArgs` matchArg) matchGuard) FunctionObj Nothing [] (emptyMetaM "obj" matchDeclM) (Just (RawGuardExpr matchExpr NoGuard))

    mapEitherMatchStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherMatchStatements _ = error "Invalid subStatement in matchDeclPreprocessor"
matchDeclPreprocessor _ = error "Invalid matchDeclPreprocessor"

caseDeclPreprocessor :: PDeclTree -> CRes [PDeclTree]
caseDeclPreprocessor r@(RawDecl oa@RawObjArr{roaArr=Just (RawGuardExpr expr _)}, subStatements) = return [(decl', cases' ++ subStatements')]
  where
    baseCondName = "$" ++ take 6 (printf "%08x" (hash r))
    argName = baseCondName ++ "-arg"
    condName :: Int -> String
    condName i = printf "%s-%d" baseCondName i

    -- Main Declaration
    [TupleArgO _ matchingExpr] = exprAppliedArgs expr
    expr' = applyRawArgs (rawVal (condName 0)) [(Just argName, matchingExpr)]
    decl' = RawDecl oa{roaArr=Just (RawGuardExpr expr' NoGuard)}

    -- Find cases
    (subStatements', cases) = partitionEithers $ map mapEitherCaseStatements subStatements
    mapEitherCaseStatements (RawStatementTree (RawDeclStatement decl) subs) = Right (decl, subs)
    mapEitherCaseStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherCaseStatements _ = error "Invalid subStatement in case preprocessor"

    initCase' = zipWith (curry buildInitCase) [0..] (init cases)
    buildInitCase (i, (RawDecl RawObjArr{roaM=caseDeclM, roaObj=(Just (RawGuardExpr caseObj caseGuard)), roaArr=(Just (RawGuardExpr caseExpr _))}, caseSubStatements)) = [matchingCase, fallthroughCase]
      where
        declObj = rawVal (condName i) `applyRawArgs` [(Just argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) caseObj)]
        matchingCase = RawStatementTree (RawDeclStatement (RawDecl (RawObjArr (Just (RawGuardExpr declObj caseGuard)) FunctionObj Nothing [] (emptyMetaM "matching" caseDeclM) (Just (RawGuardExpr caseExpr NoGuard))))) caseSubStatements
        fallthroughExpr = applyRawArgs (rawVal (condName $ i + 1)) [(Just argName, matchingExpr)]
        fallthroughCase = RawStatementTree (RawDeclStatement (RawDecl (RawObjArr (Just (RawGuardExpr declObj ElseGuard)) FunctionObj Nothing [] (emptyMetaM "fallthrough" caseDeclM) (Just (RawGuardExpr fallthroughExpr NoGuard))))) []
    buildInitCase (_, decl) = error $ printf "Missing expression in buildInitCase %s" (show decl)

    (RawDecl RawObjArr{roaM=lastCaseDeclM, roaObj=(Just (RawGuardExpr lastCaseObj lastCaseGuard)), roaArr=(Just (RawGuardExpr lastCaseExpr _))}, lastCaseSubStatements) = last cases
    lastCaseDeclObj = rawVal (condName $ length cases - 1) `applyRawArgs` [(Just argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) lastCaseObj)]
    lastCase' = RawStatementTree (RawDeclStatement (RawDecl (RawObjArr (Just (RawGuardExpr lastCaseDeclObj lastCaseGuard)) FunctionObj Nothing [] (emptyMetaM "fallthrough" lastCaseDeclM) (Just (RawGuardExpr lastCaseExpr NoGuard))))) lastCaseSubStatements
    cases' = concat initCase' ++ [lastCase']
caseDeclPreprocessor _ = error "Invalid caseDeclPreprocessor"

-- | A declPreprocessor for multi-line expressions. Will search for the final result expression and move it to the top
nestedDeclPreprocessor :: PDeclTree -> CRes [PDeclTree]
nestedDeclPreprocessor decl@(RawDecl oa, subStatements) = case exprs' of
  [e] -> return [(RawDecl oa{roaArr=Just (RawGuardExpr e NoGuard)}, subStatements')]
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
declPreprocessors declTree@(RawDecl RawObjArr{roaArr=Just (RawGuardExpr expr _)}, _) = case maybeExprPath expr of
  Just "if"                             -> ifDeclPreprocessor declTree
  Just "match"                          -> matchDeclPreprocessor declTree
  Just "case"                           -> caseDeclPreprocessor declTree
  Just path | path == nestedDeclaration -> nestedDeclPreprocessor declTree
  _                                     -> defaultDeclPreprocessor declTree
declPreprocessors declTree = defaultDeclPreprocessor declTree
