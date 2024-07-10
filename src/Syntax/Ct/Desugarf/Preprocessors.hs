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
import           CtConstants
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

ifDeclPreprocessor :: PDeclTree -> CRes [PStatementTree]
ifDeclPreprocessor (roa@RawObjArr{roaObj=Just declObj, roaArr=Just (Just expr, _, _)}, subStatements) = return [RawStatementTree (RawDeclStatement decl') (matchDecls ++ subStatements')]
  where
    condName = "$" ++ take 6 (printf "%08x" (hash roa))
    argName = condName ++ "-arg"

    -- Main declaration
    expr' = case exprAppliedArgs expr of
      [ObjArr{oaObj=Just matching}] -> rawVal condName `applyRawArgs` [(Just $ partialKey argName, rawVal "/Catln/ThenElse/fromBool" `applyRawArgs` [(Just $ partialKey "v", matching)])]
      _ -> error "Invalid matching expression"
    decl' = roa{roaArr=Just (Just expr', Nothing, emptyMetaE "arrM" expr')}

    -- Pattern declarations
    (subStatements', matchDecls) = partitionEithers $ map mapEitherMatchStatements subStatements
    mapEitherMatchStatements (RawStatementTree (RawDeclStatement RawObjArr{roaObj=(Just matchObj), roaAnnots, roaArr=(Just (Just matchExpr, _, matchDeclM))}) matchSubStatements) = Right (RawStatementTree (RawDeclStatement matchDeclLhs') matchSubStatements)
      where
        matchArg = [(Just $ partialKey argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) matchObj)]
        matchDeclLhs' = RawObjArr (Just $ rawVal condName `applyRawExprVars` H.toList (exprAppliedVars declObj)`applyRawArgs` matchArg) FunctionObj Nothing roaAnnots (Just (Just matchExpr, Nothing, emptyMetaM "obj" matchDeclM)) Nothing

    mapEitherMatchStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherMatchStatements _ = error "Invalid subStatement in matchDeclPreprocessor"
ifDeclPreprocessor _ = fail "Invalid ifDeclPreprocessor"

matchDeclPreprocessor :: PDeclTree -> CRes [PStatementTree]
matchDeclPreprocessor (roa@RawObjArr{roaObj=Just declObj, roaArr=Just (Just expr, _, _)}, subStatements) = return [RawStatementTree (RawDeclStatement decl') (matchDecls ++ subStatements')]
  where
    condName = "$" ++ take 6 (printf "%08x" (hash roa))
    argName = condName ++ "-arg"

    -- Main declaration
    expr' = case exprAppliedArgs expr of
      [ObjArr{oaObj=Just matching}] -> applyRawArgs (rawVal condName) [(Just $ partialKey argName, matching)]
      _ -> error "Invalid matching expression"
    decl' = roa{roaArr=Just (Just expr', Nothing, emptyMetaE "arrM" expr')}

    -- Pattern declarations
    (subStatements', matchDecls) = partitionEithers $ map mapEitherMatchStatements subStatements
    mapEitherMatchStatements (RawStatementTree (RawDeclStatement RawObjArr{roaObj=(Just matchObj), roaAnnots, roaArr=(Just (Just matchExpr, _, matchDeclM))}) matchSubStatements) = Right (RawStatementTree (RawDeclStatement matchDeclLhs') matchSubStatements)
      where
        matchArg = [(Just $ partialKey argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) matchObj)]
        matchDeclLhs' = RawObjArr (Just $ rawVal condName `applyRawExprVars` H.toList (exprAppliedVars declObj)`applyRawArgs` matchArg) FunctionObj Nothing roaAnnots (Just (Just matchExpr, Nothing, emptyMetaM "obj" matchDeclM)) Nothing

    mapEitherMatchStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherMatchStatements _ = error "Invalid subStatement in matchDeclPreprocessor"
matchDeclPreprocessor _ = fail "Invalid matchDeclPreprocessor"

caseDeclPreprocessor :: PDeclTree -> CRes [PStatementTree]
caseDeclPreprocessor (roa@RawObjArr{roaArr=Just (Just expr, _, _)}, subStatements) = return [RawStatementTree (RawDeclStatement decl') (cases' ++ subStatements')]
  where
    baseCondName = "$" ++ take 6 (printf "%08x" (hash roa))
    argName = baseCondName ++ "-arg"
    condName :: Int -> String
    condName i = printf "%s-%d" baseCondName i

    -- Main Declaration
    [ObjArr{oaObj=Just matchingExpr}] = exprAppliedArgs expr
    expr' = applyRawArgs (rawVal (condName 0)) [(Just $ partialKey argName, matchingExpr)]
    decl' = roa{roaArr=Just (Just expr', Nothing, emptyMetaE "arrM" expr')}

    -- Find cases
    (subStatements', cases) = partitionEithers $ map mapEitherCaseStatements subStatements
    mapEitherCaseStatements (RawStatementTree (RawDeclStatement decl) subs) = Right (decl, subs)
    mapEitherCaseStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherCaseStatements _ = error "Invalid subStatement in case preprocessor"

    initCase' = zipWith (curry buildInitCase) [0..] (init cases)
    buildInitCase (i, (RawObjArr{roaObj=(Just caseObj), roaAnnots, roaArr=(Just (Just caseExpr, _, caseDeclM))}, caseSubStatements)) = [matchingCase, fallthroughCase]
      where
        declObj = rawVal (condName i) `applyRawArgs` [(Just $ partialKey argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) caseObj)]
        matchingCase = RawStatementTree (RawDeclStatement (RawObjArr (Just declObj) FunctionObj Nothing roaAnnots (Just (Just caseExpr, Nothing, emptyMetaM "matching" caseDeclM)) Nothing)) caseSubStatements
        fallthroughExpr = applyRawArgs (rawVal (condName $ i + 1)) [(Just $ partialKey argName, matchingExpr)]
        fallthroughCase = RawStatementTree (RawDeclStatement (RawObjArr (Just declObj) FunctionObj Nothing [rawVal elseAnnot] (Just (Just fallthroughExpr, Nothing, emptyMetaM "fallthrough" caseDeclM)) Nothing)) []
    buildInitCase (_, decl) = error $ printf "Missing expression in buildInitCase %s" (show decl)

    (RawObjArr{roaObj=(Just lastCaseObj), roaArr=(Just (Just lastCaseExpr, _, lastCaseDeclM))}, lastCaseSubStatements) = last cases
    lastCaseDeclObj = rawVal (condName $ length cases - 1) `applyRawArgs` [(Just $ partialKey argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) lastCaseObj)]
    lastCase' = RawStatementTree (RawDeclStatement (RawObjArr (Just lastCaseDeclObj) FunctionObj Nothing [] (Just (Just lastCaseExpr, Nothing, emptyMetaM "fallthrough" lastCaseDeclM)) Nothing)) lastCaseSubStatements
    cases' = concat initCase' ++ [lastCase']
caseDeclPreprocessor _ = fail "Invalid caseDeclPreprocessor"

modDeclPreprocessor :: PDeclTree -> CRes [PStatementTree]
modDeclPreprocessor (roa@RawObjArr{roaArr=Just (Just modNameExpr, _, _)}, subStatements) = do
  modName <- case exprAppliedArgs modNameExpr of
    [ObjArr{oaObj=Just (RawValue _ modName)}] -> return modName
    args -> fail $ printf "Invalid arguments to module from %s.\n\tParsed args to %s" (show roa) (show args)
  return [RawStatementTree (RawModule modName) subStatements]
modDeclPreprocessor (roa, _) = fail $ printf "Invalid modDeclPreprocessor: %s" (show roa)

dataDeclPreprocessor :: PDeclTree -> CRes [PStatementTree]
dataDeclPreprocessor (roa@RawObjArr{roaArr=Just (Just dataExpr, _, _)}, subStatements) = do
  res <- case exprAppliedArgs dataExpr of
    (ObjArr{oaObj=Just tp}:restArgs) -> case restArgs of
      [ObjArr{oaObj=Just (RawValue _ "isa"), oaArr=Just (Just (RawList _ extends), _)}] -> return $ TypeDefStatement tp extends
      [] -> return $ TypeDefStatement tp []
      _ -> fail $ printf "Unknown remaining arguments to data in %s" (show roa)
    _ -> fail $ printf "Missing first argument to data in %s" (show roa)
  return [RawStatementTree res subStatements]
dataDeclPreprocessor (roa, _) = fail $ printf "Invalid dataDeclPreprocessor: %s" (show roa)

-- | A declPreprocessor for multi-line expressions. Will search for the final result expression and move it to the top
nestedDeclPreprocessor :: PDeclTree -> CRes [PStatementTree]
nestedDeclPreprocessor (oa, subStatements) = aux [] subStatements
  where
    aux :: [PStatementTree] -> [PStatementTree] -> CRes [PStatementTree]
    aux accStmts [RawStatementTree (RawDeclStatement RawObjArr{roaObj=Just e, roaArr=Nothing}) []] = return [RawStatementTree (RawDeclStatement oa{roaArr=Just (Just e, Nothing, emptyMetaE "arrM" e)}) accStmts] -- expr statement
    aux accStmts (s@(RawStatementTree RawDeclStatement{} _) : restStmt) = aux (s:accStmts) restStmt
    aux accStmts (s@(RawStatementTree RawAnnot{} _) : restStmt) = aux (s:accStmts) restStmt
    aux accStmts [] = return [RawStatementTree (RawDeclStatement oa{roaArr=Just (Just $ rawVal "", Nothing, emptyMetaN)}) accStmts]
    aux accStmts (RawStatementTree (RawBindStatement roa@RawObjArr{roaObj=Just objExpr, roaArr=Just (Just arrExpr, _, arrM)}) []:restStmt) = return [RawStatementTree (RawDeclStatement oa{roaArr=Just (Just subExpr, Nothing, arrM)}) (accStmts ++ [subDef])]
      where
        subName = "$" ++ take 6 (printf "%08x" (hash roa))
        subArgName = subName ++ "-arg"
        subExpr = rawVal subName `applyRawArgs` [(Just $ partialKey subArgName, arrExpr)]
        subObjExpr = rawVal subName `applyRawIArgs` [(partialKey subArgName, IArgE objExpr)]
        subDef = RawStatementTree (RawDeclStatement (RawObjArr (Just subObjExpr) TypeObj Nothing [] (Just (Just $ rawVal nestedDeclaration, Nothing, emptyMetaN)) Nothing)) restStmt
    aux _ s = fail $ printf "Unsupported subDeclStatemnt type: %s" (show s)

declPreprocessorList :: H.HashMap String (PDeclTree -> CRes [PStatementTree])
declPreprocessorList = H.fromList [
  ("if", ifDeclPreprocessor),
  ("match", matchDeclPreprocessor),
  ("case", caseDeclPreprocessor),
  (modStr, modDeclPreprocessor),
  (dataStr, dataDeclPreprocessor),
  (annotStr, dataDeclPreprocessor),
  (nestedDeclaration, nestedDeclPreprocessor)
                               ]

declPreprocessors :: PDeclTree -> CRes (Maybe [PStatementTree])
declPreprocessors (roa@RawObjArr{roaObj=Just expr, roaArr=Nothing}, declSubStatements) = case maybeExprPath expr of
  Just p -> case H.lookup p declPreprocessorList of
    Just preprocessor -> Just <$> preprocessor (roa{roaObj=Nothing, roaArr=Just (Just expr, Nothing, emptyMetaE "dpp" expr)}, declSubStatements)
    Nothing           -> return Nothing
  Nothing -> return Nothing
declPreprocessors declTree@(RawObjArr{roaArr=Just (Just expr, _, _)}, _) = case maybeExprPath expr of
  Just p -> case H.lookup p declPreprocessorList of
    Just preprocessor -> Just <$> preprocessor declTree
    Nothing           -> return Nothing
  Nothing -> return Nothing
declPreprocessors _ = return Nothing
