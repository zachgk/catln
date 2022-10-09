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
import           Semantics
import           Semantics.Prgm
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Text.Printf

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

nestedDeclPreprocessor :: PDeclTree -> CRes [PDeclTree]
nestedDeclPreprocessor decl@(RawDecl lhs _, subStatements) = case exprs' of
  [e] -> return [(RawDecl lhs (Just e), subStatements')]
  [] -> fail $ printf "Found no output expressions in nested declaration: %s" (show decl)
  (_:_:_) -> fail $ printf "Found multiple output expressions in nested declaration: %s" (show decl)

  where
    (subStatements', exprs') = partitionEithers $ map findExpression subStatements
    findExpression (RawStatementTree (RawExprStatement expr) _) = Right expr
    findExpression e                                            = Left e

defaultDeclPreprocessor :: PDeclTree -> CRes [PDeclTree]
defaultDeclPreprocessor declTree = return [declTree]

declPreprocessors :: PDeclTree -> CRes [PDeclTree]
declPreprocessors declTree@(RawDecl _ (Just expr), _) = case maybeExprPath expr of
  Just "match"                          -> matchDeclPreprocessor declTree
  Just path | path == nestedDeclaration -> nestedDeclPreprocessor declTree
  -- "/case" -> _
  -- "/if" -> _
  _                                     -> defaultDeclPreprocessor declTree
declPreprocessors declTree = defaultDeclPreprocessor declTree
