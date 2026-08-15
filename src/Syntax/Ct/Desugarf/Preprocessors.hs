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
  | TypeDefStatement (RawExpr ()) (ExtendedClasses RawExpr ())
  | MultiTypeDefStatement (MultiTypeDef ())
  | RawClassDefStatement (RawClassDef ()) -- Every _ isa _
  | RawClassDeclStatement (RawExpr ()) (ExtendedClasses RawExpr ())
  deriving (Eq)

data DPPRes
  = DPPStatements [PStatementTree]
  | DPPStTree DPPStatement [PStatementTree]
  deriving (Eq)

type DeclPreprocessor = PDeclTree -> CRes [DPPRes]

showDeclTrees :: [PDeclTree] -> String
showDeclTrees trees = intercalate "" $ map (\(d, ss) -> build $ formatStatementTree True 0 $ RawStatementTree (RawDeclStatement d) ss) trees

ifDeclPreprocessor :: DeclPreprocessor
ifDeclPreprocessor (roa@RawObjArr{roaObj=Just declObj, roaArr=Just (Just expr, _)}, subStatements) = return [DPPStatements [RawStatementTree (RawDeclStatement decl') (matchDecls ++ subStatements')]]
  where
    condName = "$" ++ take 6 (printf "%08x" (hash roa))
    argName = condName ++ "-arg"

    -- Main declaration
    expr' = case rawExprAppliedArgs expr of
      [RawObjArr{roaObj=Just matching}] -> rawVal condName `applyRawArgs` [(Just $ partialKey (mkName argName), rawVal "/Catln/ThenElse/fromBool" `applyRawArgs` [(Just $ partialKey "v", matching)])]
      _ -> error "Invalid matching expression"
    decl' = roa{roaArr=Just (Just expr', Nothing)}

    -- Pattern declarations
    (subStatements', matchDecls) = partitionEithers $ map mapEitherMatchStatements subStatements
    mapEitherMatchStatements (RawStatementTree (RawDeclStatement RawObjArr{roaObj=(Just matchObj), roaAnnots, roaArr=(Just (Just matchExpr, matchDeclM))}) matchSubStatements) = Right (RawStatementTree (RawDeclStatement matchDeclLhs') matchSubStatements)
      where
        matchArg = [(Just $ partialKey (mkName argName), RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) matchObj)]
        matchDeclLhs' = RawObjArr (Just $ rawVal condName `applyRawExprVars` H.toList (Nothing <$ exprAppliedVars declObj)`applyRawArgs` matchArg) FunctionObj Nothing roaAnnots (Just (Just matchExpr, matchDeclM)) Nothing

    mapEitherMatchStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherMatchStatements _ = error "Invalid subStatement in matchDeclPreprocessor"
ifDeclPreprocessor _ = fail "Invalid ifDeclPreprocessor"

matchDeclPreprocessor :: DeclPreprocessor
matchDeclPreprocessor (roa@RawObjArr{roaObj=Just declObj, roaArr=Just (Just expr, _)}, subStatements) = return [DPPStatements [RawStatementTree (RawDeclStatement decl') (matchDecls ++ subStatements')]]
  where
    condName = "$" ++ take 6 (printf "%08x" (hash roa))
    argName = condName ++ "-arg"

    -- Main declaration
    expr' = case rawExprAppliedArgs expr of
      [RawObjArr{roaObj=Just matching}] -> applyRawArgs (rawVal condName) [(Just $ partialKey (mkName argName), matching)]
      _ -> error "Invalid matching expression"
    decl' = roa{roaArr=Just (Just expr', Nothing)}

    -- Pattern declarations
    (subStatements', matchDecls) = partitionEithers $ map mapEitherMatchStatements subStatements
    mapEitherMatchStatements (RawStatementTree (RawDeclStatement RawObjArr{roaObj=(Just matchObj), roaAnnots, roaArr=(Just (Just matchExpr, _matchDeclM))}) matchSubStatements) = Right (RawStatementTree (RawDeclStatement matchDeclLhs') matchSubStatements)
      where
        matchArg = [(Just $ partialKey (mkName argName), RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) matchObj)]
        matchDeclLhs' = RawObjArr (Just $ rawVal condName `applyRawExprVars` H.toList (Nothing <$ exprAppliedVars declObj) `applyRawArgs` matchArg) FunctionObj Nothing roaAnnots (Just (Just matchExpr, Nothing)) Nothing

    mapEitherMatchStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherMatchStatements _ = error "Invalid subStatement in matchDeclPreprocessor"
matchDeclPreprocessor _ = fail "Invalid matchDeclPreprocessor"

caseDeclPreprocessor :: DeclPreprocessor
caseDeclPreprocessor (roa@RawObjArr{roaArr=Just (Just expr, _)}, subStatements) = return [DPPStatements [RawStatementTree (RawDeclStatement decl') (cases' ++ subStatements')]]
  where
    baseCondName = "$" ++ take 6 (printf "%08x" (hash roa))
    argName = baseCondName ++ "-arg"
    condName :: Int -> String
    condName = printf "%s-%d" baseCondName

    -- Main Declaration
    matchingExpr = fromJust $ roaObj $ head $ rawExprAppliedArgs expr
    expr' = applyRawArgs (rawVal (condName 0)) [(Just $ partialKey (mkName argName), matchingExpr)]
    decl' = roa{roaArr=Just (Just expr', Nothing)}

    -- Find cases
    (subStatements', cases) = partitionEithers $ map mapEitherCaseStatements subStatements
    mapEitherCaseStatements (RawStatementTree (RawDeclStatement decl) subs) = Right (decl, subs)
    mapEitherCaseStatements statementTree@(RawStatementTree RawAnnot{} _) = Left statementTree
    mapEitherCaseStatements _ = error "Invalid subStatement in case preprocessor"

    initCase' = zipWith (curry buildInitCase) [0..] (init cases)
    buildInitCase (i, (RawObjArr{roaObj=(Just caseObj), roaAnnots, roaArr=(Just (Just caseExpr, _caseDeclM))}, caseSubStatements)) = [matchingCase, fallthroughCase]
      where
        declObj = rawVal (condName i) `applyRawArgs` [(Just $ partialKey (mkName argName), RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) caseObj)]
        matchingCase = RawStatementTree (RawDeclStatement (RawObjArr (Just declObj) FunctionObj Nothing roaAnnots (Just (Just caseExpr, Nothing)) Nothing)) caseSubStatements
        fallthroughExpr = applyRawArgs (rawVal (condName $ i + 1)) [(Just $ partialKey (mkName argName), matchingExpr)]
        fallthroughCase = RawStatementTree (RawDeclStatement (RawObjArr (Just declObj) FunctionObj Nothing [rawVal elseAnnot] (Just (Just fallthroughExpr, Nothing)) Nothing)) []
    buildInitCase (_, decl) = error $ printf "Missing expression in buildInitCase %s" (show decl)

    (lastCaseOa, lastCaseSubStatements) = last cases
    lastCaseObj = fromJust $ roaObj lastCaseOa
    lastCaseExpr = fromJust $ fst $ fromJust $ roaArr lastCaseOa

    lastCaseDeclObj = rawVal (condName $ length cases - 1) `applyRawArgs` [(Just $ partialKey (mkName argName), RawAliasExpr (RawHoleExpr emptyMetaN (HoleActive Nothing)) lastCaseObj)]
    lastCase' = RawStatementTree (RawDeclStatement (RawObjArr (Just lastCaseDeclObj) FunctionObj Nothing [] (Just (Just lastCaseExpr, Nothing)) Nothing)) lastCaseSubStatements
    cases' = concat initCase' ++ [lastCase']
caseDeclPreprocessor _ = fail "Invalid caseDeclPreprocessor"

modDeclPreprocessor :: DeclPreprocessor
modDeclPreprocessor (roa@RawObjArr{roaArr=Just (Just modNameExpr, _)}, subStatements) = do
  modName <- case rawExprAppliedArgs modNameExpr of
    [RawObjArr{roaObj=Just (RawValue _ modName)}] -> return modName
    args -> fail $ printf "Invalid arguments to module from %s.\n\tParsed args to %s" (show roa) (show args)
  return [DPPStTree (RawModule (nameStr modName)) subStatements]
modDeclPreprocessor (roa, _) = fail $ printf "Invalid modDeclPreprocessor: %s" (show roa)

dataDeclPreprocessor :: DeclPreprocessor
dataDeclPreprocessor (roa@RawObjArr{roaArr=Just (Just dataExpr, _)}, subStatements) = do
  res <- case rawExprAppliedArgs dataExpr of
    (RawObjArr{roaObj=Just tp}:restArgs) -> case restArgs of
      [RawObjArr{roaObj=Just (RawValue _ "isa"), roaArr=Just (Just (RawList _ extends), _)}] -> return $ TypeDefStatement tp extends
      [] -> return $ TypeDefStatement tp []
      _ -> fail $ printf "Unknown remaining arguments to data in %s" (show roa)
    _ -> fail $ printf "Missing first argument to data in %s" (show roa)
  return [DPPStTree res subStatements]
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
  return [DPPStTree res subStatements]
classDeclPreprocessor (roa, _) = fail $ printf "Invalid classDeclPreprocessor: %s" (show roa)

classInstDeclPreprocessor :: DeclPreprocessor
classInstDeclPreprocessor (roa@RawObjArr{roaArr=Just (Just classExpr, _)}, subStatements) = do
  res <- case rawExprAppliedArgs classExpr of
    (RawObjArr{roaObj=Just tp}:restArgs) -> case restArgs of
      [RawObjArr{roaObj=Just (RawValue _ "isa"), roaArr=Just (Just (RawList _ extends), _)}] -> return $ RawClassDefStatement (tp, extends)
      _ -> fail $ printf "Unknown remaining arguments to classInst in %s" (show roa)
    _ -> fail $ printf "Missing first argument to classInst in %s" (show roa)
  return [DPPStTree res subStatements]
classInstDeclPreprocessor (roa, _) = fail $ printf "Invalid classInstDeclPreprocessor: %s" (show roa)

applyDeclPreprocessor :: DeclPreprocessor
applyDeclPreprocessor (roa@RawObjArr{roaArr=Just (Just applyExpr, _)}, subStatements) = do
  res <- case rawExprAppliedArgs applyExpr of
    [RawObjArr{roaObj=Just (RawApplyExpr _ a)}] -> return $ RawApplyStatement a
    [RawObjArr{roaObj=Just (RawFmtStrExpr _ "a" a)}] -> case runParser pApply "<applyFormatString>" a of
      Right a' -> return $ RawApplyStatement a'
      Left err -> fail $ show $ errorBundlePretty err
    _ -> fail $ printf "Missing first argument to classInst in %s" (show roa)
  return [DPPStTree res subStatements]
applyDeclPreprocessor (roa, _) = fail $ printf "Invalid applyDeclPreprocessor: %s" (show roa)


-- | A declPreprocessor for multi-line expressions. Will search for the final result expression and move it to the top
nestedDeclPreprocessor :: DeclPreprocessor
nestedDeclPreprocessor (oa, subStatements) = pure . DPPStatements <$> aux [] subStatements
  where
    -- The enclosing declaration's return-type annotation (e.g. the @-> Integer@ in
    -- @inc(a -> Integer) -> Integer =@). It must be carried onto the rewritten body: a bind desugars
    -- to a 'bindap' call whose result type is a bare variable, so without the declared return there
    -- is nothing to pin it and inference diverges.
    oaReturnMeta = case roaArr oa of
      Just (_, retM) -> retM
      Nothing        -> Nothing
    -- The statements kept are accumulated by consing, so what has been kept is in the reverse of
    -- the order it was written and is reversed back at each point it is used. A declaration
    -- carries its statements in the order they were written for the same reason it carries them at
    -- all: they are read as a body, and something reporting on them one at a time reports on them
    -- in that order. A @#debug@ of each step of a computation is the clearest case — read back to
    -- front it is not a trace of the computation but a list of the same values.
    aux :: [PStatementTree] -> [PStatementTree] -> CRes [PStatementTree]
    aux accStmts [RawStatementTree (RawDeclStatement RawObjArr{roaObj=Just e, roaArr=Nothing}) []] = return [RawStatementTree (RawDeclStatement oa{roaArr=Just (Just e, Nothing)}) (reverse accStmts)] -- expr statement
    aux accStmts (s@(RawStatementTree RawDeclStatement{} _) : restStmt) = aux (s:accStmts) restStmt
    aux accStmts (s@(RawStatementTree RawAnnot{} _) : restStmt) = aux (s:accStmts) restStmt
    aux accStmts [] = return [RawStatementTree (RawDeclStatement oa{roaArr=Just (Just rawAnon, Nothing)}) (reverse accStmts)]
    aux accStmts (RawStatementTree (RawBindStatement roa@RawObjArr{roaObj=Just objExpr, roaArr=Just (Just arrExpr, _)}) []:restStmt) =
      -- Desugar @x <- m@ followed by the rest of the body to @bindap(m= m, fn(v)= cont(x= v))@, where
      -- @cont@ is a fresh sub function whose body is the remaining statements. Routing the
      -- continuation through a named sub function (rather than inlining it as one expression) lets it
      -- hold anything a declaration can — local decls, @match@, @while@, further binds — since those
      -- are preprocessed recursively as @cont@'s own sub statements. The bind's own pattern (@objExpr@,
      -- e.g. @x@) becomes @cont@'s parameter, and @bindap@'s @fn@ lambda simply passes the bound
      -- element to it.
      return [RawStatementTree (RawDeclStatement oa{roaArr=Just (Just bindapCall, oaReturnMeta)}) (reverse accStmts ++ [contDef])]
      where
        contName = "$" ++ take 6 (printf "%08x" (hash roa))
        contArgName = contName ++ "-arg"
        -- @cont(contArg\@objExpr)= <restStmt>@ — a nested declaration so its body is preprocessed. The
        -- bind's pattern is aliased onto the parameter (as the working inline form does) rather than
        -- destructured in the parameter position, which would match it as a type and leave it 'TopType'.
        contObjExpr = applyRawBareParams (rawVal contName) [RawAliasExpr (rawVal contArgName) objExpr]
        contDef = RawStatementTree (RawDeclStatement (RawObjArr (Just contObjExpr) TypeObj Nothing [] (Just (Just $ rawVal nestedDeclaration, Nothing)) Nothing)) restStmt
        -- @bindap(m= arrExpr, fn(v)= cont(contArg= v))@.
        contCall = rawVal contName `applyRawArgs` [(Just $ partialKey (mkName contArgName), rawVal bindApV)]
        fnHead = applyRawBareParams (rawVal bindApFn) [rawVal bindApV]
        bindapCall = rawVal bindApName `applyRawEArgs` [(Just (rawVal bindApM), arrExpr), (Just fnHead, contCall)]
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
  Just p -> case H.lookup (nameStr p) declPreprocessorList of
    Just preprocessor -> preprocessor (roa{roaObj=Nothing, roaArr=Just (Just expr, Nothing)}, declSubStatements)
    Nothing           -> return []
  Nothing -> return []
declPreprocessors declTree@(RawObjArr{roaArr=Just (Just expr, _)}, _) = case maybeExprPath expr of
  Just p -> case H.lookup (nameStr p) declPreprocessorList of
    Just preprocessor -> preprocessor declTree
    Nothing           -> return []
  Nothing -> return []
declPreprocessors _ = return []
