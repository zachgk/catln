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
import           Data.Maybe              (fromJust)
import           Data.String.Builder     (build)
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Builder
import           Syntax.Ct.Formatter     (formatStatementTree)
import           Syntax.Ct.Parser.Expr   (pApply)
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Text.Megaparsec         (errorBundlePretty, runParser)
import           Text.Printf

-- | Helper for the X isa Y where this is the Y
type ExtendedClasses e m = [e m]

data MultiTypeDef m = MultiTypeDef (RawExpr m) [RawExpr m] (ExtendedClasses RawExpr m)
  deriving (Eq)

type RawClassDef m = (RawExpr m, ExtendedClasses RawExpr m)

data DPPStatement
  = RawModule String
  | RawApplyStatement (RawApply RawExpr ())
  | MultiTypeDefStatement (MultiTypeDef ())
  | TypeDefStatement (RawExpr ()) (ExtendedClasses RawExpr ())
  | RawClassDefStatement (RawClassDef ()) -- Every _ isa _
  | RawClassDeclStatement (RawExpr ()) (ExtendedClasses RawExpr ())
  deriving (Eq)

data DPPRes
  = DPPStatements [PStatementTree]
  | DPPNothing
  | DPPStTree DPPStatement [PStatementTree]
  deriving (Eq)

type DeclPreprocessor = PDeclTree -> CRes DPPRes

showDeclTrees :: [PDeclTree] -> String
showDeclTrees trees = intercalate "" $ map (\(d, ss) -> build $ formatStatementTree True 0 $ RawStatementTree (RawDeclStatement d) ss) trees

ifDeclPreprocessor :: DeclPreprocessor
ifDeclPreprocessor (roa@RawObjArr{roaObj=Just declObj, roaArr=Just (Just expr, _)}, subStatements) = return $ DPPStatements [RawStatementTree (RawDeclStatement decl') (matchDecls ++ subStatements')]
  where
    condName = "$" ++ take 6 (printf "%08x" (hash roa))
    argName = condName ++ "-arg"

    -- Main declaration
    expr' = case rawExprAppliedArgs expr of
      [RawObjArr{roaObj=Just matching}] -> rawVal condName `applyRawArgs` [(Just $ partialKey argName, rawVal "/Catln/ThenElse/fromBool" `applyRawArgs` [(Just $ partialKey "v", matching)])]
      _ -> error "Invalid matching expression"
    decl' = roa{roaArr=Just (Just expr', Nothing)}

    -- Pattern declarations
    (subStatements', matchDecls) = partitionEithers $ map mapEitherMatchStatements subStatements
    mapEitherMatchStatements (RawStatementTree (RawDeclStatement RawObjArr{roaObj=(Just matchObj), roaAnnots, roaArr=(Just (Just matchExpr, matchDeclM))}) matchSubStatements) = Right (RawStatementTree (RawDeclStatement matchDeclLhs') matchSubStatements)
      where
        matchArg = [(Just $ partialKey argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) matchObj)]
        matchDeclLhs' = RawObjArr (Just $ rawVal condName `applyRawExprVars` H.toList (Nothing <$ exprAppliedVars declObj)`applyRawArgs` matchArg) FunctionObj Nothing roaAnnots (Just (Just matchExpr, matchDeclM)) Nothing

    mapEitherMatchStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherMatchStatements _ = error "Invalid subStatement in matchDeclPreprocessor"
ifDeclPreprocessor _ = fail "Invalid ifDeclPreprocessor"

matchDeclPreprocessor :: DeclPreprocessor
matchDeclPreprocessor (roa@RawObjArr{roaObj=Just declObj, roaArr=Just (Just expr, _)}, subStatements) = return $ DPPStatements [RawStatementTree (RawDeclStatement decl') (matchDecls ++ subStatements')]
  where
    condName = "$" ++ take 6 (printf "%08x" (hash roa))
    argName = condName ++ "-arg"

    -- Main declaration
    expr' = case rawExprAppliedArgs expr of
      [RawObjArr{roaObj=Just matching}] -> applyRawArgs (rawVal condName) [(Just $ partialKey argName, matching)]
      _ -> error "Invalid matching expression"
    decl' = roa{roaArr=Just (Just expr', Nothing)}

    -- Pattern declarations
    (subStatements', matchDecls) = partitionEithers $ map mapEitherMatchStatements subStatements
    mapEitherMatchStatements (RawStatementTree (RawDeclStatement RawObjArr{roaObj=(Just matchObj), roaAnnots, roaArr=(Just (Just matchExpr, _matchDeclM))}) matchSubStatements) = Right (RawStatementTree (RawDeclStatement matchDeclLhs') matchSubStatements)
      where
        matchArg = [(Just $ partialKey argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) matchObj)]
        matchDeclLhs' = RawObjArr (Just $ rawVal condName `applyRawExprVars` H.toList (Nothing <$ exprAppliedVars declObj) `applyRawArgs` matchArg) FunctionObj Nothing roaAnnots (Just (Just matchExpr, Nothing)) Nothing

    mapEitherMatchStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherMatchStatements _ = error "Invalid subStatement in matchDeclPreprocessor"
matchDeclPreprocessor _ = fail "Invalid matchDeclPreprocessor"

caseDeclPreprocessor :: DeclPreprocessor
caseDeclPreprocessor (roa@RawObjArr{roaArr=Just (Just expr, _)}, subStatements) = return $ DPPStatements [RawStatementTree (RawDeclStatement decl') (cases' ++ subStatements')]
  where
    baseCondName = "$" ++ take 6 (printf "%08x" (hash roa))
    argName = baseCondName ++ "-arg"
    condName :: Int -> String
    condName = printf "%s-%d" baseCondName

    -- Main Declaration
    matchingExpr = fromJust $ roaObj $ head $ rawExprAppliedArgs expr
    expr' = applyRawArgs (rawVal (condName 0)) [(Just $ partialKey argName, matchingExpr)]
    decl' = roa{roaArr=Just (Just expr', Nothing)}

    -- Find cases
    (subStatements', cases) = partitionEithers $ map mapEitherCaseStatements subStatements
    mapEitherCaseStatements (RawStatementTree (RawDeclStatement decl) subs) = Right (decl, subs)
    mapEitherCaseStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherCaseStatements _ = error "Invalid subStatement in case preprocessor"

    initCase' = zipWith (curry buildInitCase) [0..] (init cases)
    buildInitCase (i, (RawObjArr{roaObj=(Just caseObj), roaAnnots, roaArr=(Just (Just caseExpr, _caseDeclM))}, caseSubStatements)) = [matchingCase, fallthroughCase]
      where
        declObj = rawVal (condName i) `applyRawArgs` [(Just $ partialKey argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) caseObj)]
        matchingCase = RawStatementTree (RawDeclStatement (RawObjArr (Just declObj) FunctionObj Nothing roaAnnots (Just (Just caseExpr, Nothing)) Nothing)) caseSubStatements
        fallthroughExpr = applyRawArgs (rawVal (condName $ i + 1)) [(Just $ partialKey argName, matchingExpr)]
        fallthroughCase = RawStatementTree (RawDeclStatement (RawObjArr (Just declObj) FunctionObj Nothing [rawVal elseAnnot] (Just (Just fallthroughExpr, Nothing)) Nothing)) []
    buildInitCase (_, decl) = error $ printf "Missing expression in buildInitCase %s" (show decl)

    (lastCaseOa, lastCaseSubStatements) = last cases
    lastCaseObj = fromJust $ roaObj lastCaseOa
    lastCaseExpr = fromJust $ fst $ fromJust $ roaArr lastCaseOa

    lastCaseDeclObj = rawVal (condName $ length cases - 1) `applyRawArgs` [(Just $ partialKey argName, RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) lastCaseObj)]
    lastCase' = RawStatementTree (RawDeclStatement (RawObjArr (Just lastCaseDeclObj) FunctionObj Nothing [] (Just (Just lastCaseExpr, Nothing)) Nothing)) lastCaseSubStatements
    cases' = concat initCase' ++ [lastCase']
caseDeclPreprocessor _ = fail "Invalid caseDeclPreprocessor"

modDeclPreprocessor :: DeclPreprocessor
modDeclPreprocessor (roa@RawObjArr{roaArr=Just (Just modNameExpr, _)}, subStatements) = do
  modName <- case rawExprAppliedArgs modNameExpr of
    [RawObjArr{roaObj=Just (RawValue _ modName)}] -> return modName
    args -> fail $ printf "Invalid arguments to module from %s.\n\tParsed args to %s" (show roa) (show args)
  return $ DPPStTree (RawModule modName) subStatements
modDeclPreprocessor (roa, _) = fail $ printf "Invalid modDeclPreprocessor: %s" (show roa)

dataDeclPreprocessor :: DeclPreprocessor
dataDeclPreprocessor (roa@RawObjArr{roaArr=Just (Just dataExpr, _)}, subStatements) = do
  res <- case rawExprAppliedArgs dataExpr of
    (RawObjArr{roaObj=Just tp}:restArgs) -> case restArgs of
      [RawObjArr{roaObj=Just (RawValue _ "isa"), roaArr=Just (Just (RawList _ extends), _)}] -> return $ TypeDefStatement tp extends
      [] -> return $ TypeDefStatement tp []
      _ -> fail $ printf "Unknown remaining arguments to data in %s" (show roa)
    _ -> fail $ printf "Missing first argument to data in %s" (show roa)
  return $ DPPStTree res subStatements
dataDeclPreprocessor (roa, _) = fail $ printf "Invalid dataDeclPreprocessor: %s" (show roa)

classDeclPreprocessor :: DeclPreprocessor
classDeclPreprocessor (roa@RawObjArr{roaArr=Just (Just classExpr, _)}, subStatements) = do
  res <- case rawExprAppliedArgs classExpr of
    (RawObjArr{roaObj=Just tp}:restArgs) -> case restArgs of
      [RawObjArr{roaObj=Just (RawList _ objs), roaArr=_}] -> return $ MultiTypeDefStatement $ MultiTypeDef tp objs []
      [RawObjArr{roaObj=Just (RawList _ objs), roaArr=_}, RawObjArr{roaObj=Just (RawValue _ "isa"), roaArr=Just (Just (RawList _ extends), _)}] -> return $ MultiTypeDefStatement $ MultiTypeDef tp objs extends
      [RawObjArr{roaObj=Just (RawValue _ "isa"), roaArr=Just (Just (RawList _ extends), _)}] -> return $ RawClassDeclStatement tp extends
      [] -> return $ RawClassDeclStatement tp []
      remain -> fail $ printf "Unknown remaining arguments to class in %s as %s" (show roa) (show remain)
    _ -> fail $ printf "Missing first argument to class in %s" (show roa)
  return $ DPPStTree res subStatements
classDeclPreprocessor (roa, _) = fail $ printf "Invalid classDeclPreprocessor: %s" (show roa)

classInstDeclPreprocessor :: DeclPreprocessor
classInstDeclPreprocessor (roa@RawObjArr{roaArr=Just (Just classExpr, _)}, subStatements) = do
  res <- case rawExprAppliedArgs classExpr of
    (RawObjArr{roaObj=Just tp}:restArgs) -> case restArgs of
      [RawObjArr{roaObj=Just (RawValue _ "isa"), roaArr=Just (Just (RawList _ extends), _)}] -> return $ RawClassDefStatement (tp, extends)
      _ -> fail $ printf "Unknown remaining arguments to classInst in %s" (show roa)
    _ -> fail $ printf "Missing first argument to classInst in %s" (show roa)
  return $ DPPStTree res subStatements
classInstDeclPreprocessor (roa, _) = fail $ printf "Invalid classInstDeclPreprocessor: %s" (show roa)

applyDeclPreprocessor :: DeclPreprocessor
applyDeclPreprocessor (roa@RawObjArr{roaArr=Just (Just applyExpr, _)}, subStatements) = do
  res <- case rawExprAppliedArgs applyExpr of
    [RawObjArr{roaObj=Just (RawApplyExpr _ a)}] -> return $ RawApplyStatement a
    [RawObjArr{roaObj=Just (RawFmtStrExpr _ "a" a)}] -> case runParser pApply "<applyFormatString>" a of
      Right a' -> return $ RawApplyStatement a'
      Left err -> fail $ show $ errorBundlePretty err
    _ -> fail $ printf "Missing first argument to classInst in %s" (show roa)
  return $ DPPStTree res subStatements
applyDeclPreprocessor (roa, _) = fail $ printf "Invalid applyDeclPreprocessor: %s" (show roa)


-- | A declPreprocessor for multi-line expressions. Will search for the final result expression and move it to the top
nestedDeclPreprocessor :: DeclPreprocessor
nestedDeclPreprocessor (oa, subStatements) = DPPStatements <$> aux [] subStatements
  where
    aux :: [PStatementTree] -> [PStatementTree] -> CRes [PStatementTree]
    aux accStmts [RawStatementTree (RawDeclStatement RawObjArr{roaObj=Just e, roaArr=Nothing}) []] = return [RawStatementTree (RawDeclStatement oa{roaArr=Just (Just e, Nothing)}) accStmts] -- expr statement
    aux accStmts (s@(RawStatementTree RawDeclStatement{} _) : restStmt) = aux (s:accStmts) restStmt
    aux accStmts (s@(RawStatementTree RawAnnot{} _) : restStmt) = aux (s:accStmts) restStmt
    aux accStmts [] = return [RawStatementTree (RawDeclStatement oa{roaArr=Just (Just rawAnon, Nothing)}) accStmts]
    aux accStmts (RawStatementTree (RawBindStatement roa@RawObjArr{roaObj=Just objExpr, roaArr=Just (Just arrExpr, arrM)}) []:restStmt) = return [RawStatementTree (RawDeclStatement oa{roaArr=Just (Just subExpr, arrM)}) (accStmts ++ [subDef])]
      where
        subName = "$" ++ take 6 (printf "%08x" (hash roa))
        subArgName = subName ++ "-arg"
        subExpr = rawVal subName `applyRawArgs` [(Just $ partialKey subArgName, arrExpr)]
        subObjExpr = rawVal subName `applyRawIArgs` [(partialKey subArgName, IArgE objExpr)]
        subDef = RawStatementTree (RawDeclStatement (RawObjArr (Just subObjExpr) TypeObj Nothing [] (Just (Just $ rawVal nestedDeclaration, Nothing)) Nothing)) restStmt
    aux _ s = fail $ printf "Unsupported subDeclStatemnt type: %s" (show s)

declPreprocessorList :: H.HashMap String DeclPreprocessor
declPreprocessorList = H.fromList [
  ("if", ifDeclPreprocessor),
  ("match", matchDeclPreprocessor),
  ("case", caseDeclPreprocessor),
  (modStr, modDeclPreprocessor),
  (dataStr, dataDeclPreprocessor),
  (annotStr, dataDeclPreprocessor),
  (classStr, classDeclPreprocessor),
  (everyStr, classInstDeclPreprocessor),
  (applyStr, applyDeclPreprocessor),
  (nestedDeclaration, nestedDeclPreprocessor)
                               ]

declPreprocessors :: DeclPreprocessor
declPreprocessors (roa@RawObjArr{roaObj=Just expr, roaArr=Nothing}, declSubStatements) = case maybeExprPath expr of
  Just p -> case H.lookup p declPreprocessorList of
    Just preprocessor -> preprocessor (roa{roaObj=Nothing, roaArr=Just (Just expr, Nothing)}, declSubStatements)
    Nothing           -> return DPPNothing
  Nothing -> return DPPNothing
declPreprocessors declTree@(RawObjArr{roaArr=Just (Just expr, _)}, _) = case maybeExprPath expr of
  Just p -> case H.lookup p declPreprocessorList of
    Just preprocessor -> preprocessor declTree
    Nothing           -> return DPPNothing
  Nothing -> return DPPNothing
declPreprocessors _ = return DPPNothing
