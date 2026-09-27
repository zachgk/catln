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
import           Data.Bifunctor          (first)
import           Data.Either
import           Data.Hashable
import qualified Data.HashMap.Strict     as H
import           Data.List               (intercalate)
import           Data.Maybe              (fromJust)
import           Data.String.Builder     (build)
import           MapMeta                 (DerivedMeta (..))
import           Semantics               (emptyMetaE)
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

-- | Split a guarded arm into its pattern and its conditions: @x2 | x2 >= 0@ becomes
-- @(x2, [x2 >= 0])@.
--
-- A branch arm's guard belongs to the generated arm as a whole, not to the pattern nested inside
-- its argument. 'Semantics.exprWhereConds' reads only an 'AliasExpr' base, so a guard left inside
-- the alias is invisible to it — and that is the field both 'TreeBuild.completeTreeSet' and
-- 'TreeBuild.buildGuards' read to know an arm is guarded. Left there, the arm looked
-- unconditional: it covered the source on its own, its siblings were dropped as adding nothing,
-- and it was then run without its condition ever being evaluated.
splitWhere :: RawExpr m -> (RawExpr m, [RawExpr m])
splitWhere (RawWhere _ base cond) = let (base', cs) = splitWhere base in (base', cs ++ [cond])
splitWhere e                      = (e, [])

-- | Re-apply an arm's guards around the generated arm's whole object, which is where
-- 'Semantics.exprWhereConds' looks for them.
applyWheres :: (MetaDat m) => RawExpr m -> [RawExpr m] -> RawExpr m
applyWheres = foldl (RawWhere emptyMetaN)

-- | The argument a @match@ or @case@ arm's pattern becomes on its generated arm, which receives the
-- scrutinee as @argName@. The arm's role is decided by position, as a function argument's is:
--
-- * A bare name (@x2 | x2 >= 0= ..@, @val -> $_R= ..@) is a BINDER, the way @f(x)@ binds @x@. It
--   becomes an INPUT of the application, @$h($h-arg\@x2)@ — the field matched and the value bound
--   under the alias, which is how 'Semantics.Prgm.exprVarArgs' keys an argument — and the arm's
--   @-> T@, if written, is the binder's type, as @f(x -> T)@'s is.
-- * Anything else (@Cons(head, tail)@, a hole, a constant) is a PATTERN the scrutinee is matched
--   against. It becomes the argument's value, @$h($h-arg= Cons(head, tail))@, where it destructures
--   as a function argument's pattern does and binds what it names.
--
-- Both used to be written as the alias of a hole (@$h-arg= _\@pattern@). That is right for a binder,
-- but for anything else it bound the pattern's NAME instead of matching it: a constructor arm
-- (@_\@LessThan@) accepted every value, so the arms shared one ⊤ domain and the last answered.
-- A written nullary constructor arm is therefore spelled as a typed binder, @c -> LessThan= ..@.
branchArmArg :: (MetaDat m) => String -> RawExpr m -> Maybe (RawExpr m) -> RawObjArr RawExpr m
branchArmArg argName pat@RawValue{} armType = RawObjArr (Just $ RawAliasExpr (RawValue (emptyMetaE pat) (pkName $ partialKey $ mkName argName)) pat) ArgObj Nothing [] (Just (Nothing, armType)) Nothing
branchArmArg argName pat _ = RawObjArr (Just $ RawValue (emptyMetaE pat) (pkName $ partialKey $ mkName argName)) ArgObj Nothing [] (Just (Just pat, Nothing)) Nothing

-- | Apply a generated arm's argument ('branchArmArg') to its base.
applyBranchArmArg :: (MetaDat m) => RawExpr m -> RawObjArr RawExpr m -> RawExpr m
applyBranchArmArg base arg = case base of
  RawTupleApply m base' baseArgs -> RawTupleApply m base' (baseArgs ++ [(False, arg)])
  _                              -> RawTupleApply (emptyMetaE base) (emptyMetaE base, base) [(False, arg)]

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
        -- The guard is hoisted off the pattern onto the whole generated arm (see 'splitWhere').
        (matchPat, matchConds) = splitWhere matchObj
        -- The arm's pattern goes in the argument position, where it destructures exactly as a
        -- function argument's pattern does: an unbound name binds, a constructor matches. Wrapping
        -- it as @_\@pattern@ instead made it the ALIAS of a hole, and 'TypeCheck.Encode' bounds an
        -- alias's base by the ARG VARIABLE THE ALIAS NAMES rather than by the alias's type — so a
        -- constructor arm ([Then]/[Else]) constrained nothing and every arm shared one ⊤ domain.
        matchArg = [(Just $ partialKey (mkName argName), matchPat)]
        matchDeclLhs' = RawObjArr (Just $ applyWheres (rawValDerived DerivedBranchArm (getExprMeta matchObj) condName `applyRawExprVars` H.toList (Nothing <$ exprAppliedVars declObj)`applyRawArgs` matchArg) matchConds) FunctionObj Nothing roaAnnots (Just (Just matchExpr, matchDeclM)) Nothing

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
    mapEitherMatchStatements (RawStatementTree (RawDeclStatement RawObjArr{roaObj=(Just matchObj), roaAnnots, roaArr=(Just (Just matchExpr, matchDeclM))}) matchSubStatements) = Right (RawStatementTree (RawDeclStatement matchDeclLhs') matchSubStatements)
      where
        -- The guard is hoisted off the pattern onto the whole generated arm (see 'splitWhere').
        (matchPat, matchConds) = splitWhere matchObj
        -- The arm's @-> T@ types its binder ('branchArmArg'), so it is not the arm's result.
        matchArg = branchArmArg argName matchPat matchDeclM
        matchDeclLhs' = RawObjArr (Just $ applyWheres (rawValDerived DerivedBranchArm (getExprMeta matchObj) condName `applyRawExprVars` H.toList (Nothing <$ exprAppliedVars declObj) `applyBranchArmArg` matchArg) matchConds) FunctionObj Nothing roaAnnots (Just (Just matchExpr, Nothing)) Nothing

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
    buildInitCase (i, (RawObjArr{roaObj=(Just caseObj), roaAnnots, roaArr=(Just (Just caseExpr, caseDeclM))}, caseSubStatements)) = [matchingCase, fallthroughCase]
      where
        -- The guard is hoisted off the pattern and applied to the whole generated arm, so
        -- 'exprWhereConds' can see it (see 'splitWhere').
        (casePat, caseConds) = splitWhere caseObj
        -- The arm and its fall through are both arrows of the one written case, told apart by what
        -- each stands for (see 'derivedMetaID').
        patObj kind arg = rawValDerived kind (getExprMeta caseObj) (condName i) `applyBranchArmArg` arg
        declObj = applyWheres (patObj DerivedBranchArm (branchArmArg argName casePat caseDeclM)) caseConds
        matchingCase = RawStatementTree (RawDeclStatement (RawObjArr (Just declObj) FunctionObj Nothing roaAnnots (Just (Just caseExpr, Nothing)) Nothing)) caseSubStatements
        -- The fallthrough hands on the value it received rather than evaluating the scrutinee
        -- again: the scrutinee is an expression of the enclosing arm, which an arm binding a
        -- like-named variable (@c -> LessThan= ..@ under @f(c)@) keeps out of this one's scope.
        fallthroughExpr = applyRawArgs (rawVal (condName $ i + 1)) [(Just $ partialKey (mkName argName), rawVal argName)]
        -- The fallthrough runs when this case does NOT apply, so it takes any value — neither the
        -- guard nor the pattern — as its plain input @$h-arg@. Reusing the guarded object here made the else arm conditional on
        -- the very condition whose failure it exists to catch, and a constructor arm's pattern would
        -- leave every other constructor with nowhere to go.
        fallthroughCase = RawStatementTree (RawDeclStatement (RawObjArr (Just (patObj DerivedCaseFallthrough (RawObjArr (Just $ rawVal argName) ArgObj Nothing [] (Just (Nothing, Nothing)) Nothing))) FunctionObj Nothing [rawVal elseAnnot] (Just (Just fallthroughExpr, Nothing)) Nothing)) []
    buildInitCase (_, decl) = error $ printf "Missing expression in buildInitCase %s" (show decl)

    (lastCaseOa, lastCaseSubStatements) = last cases
    lastCaseObj = fromJust $ roaObj lastCaseOa
    (lastCaseExpr, lastCaseDeclM) = first fromJust $ fromJust $ roaArr lastCaseOa

    (lastCasePat, lastCaseConds) = splitWhere lastCaseObj
    lastCaseDeclObj = applyWheres (rawValDerived DerivedBranchArm (getExprMeta lastCaseObj) (condName $ length cases - 1) `applyBranchArmArg` branchArmArg argName lastCasePat lastCaseDeclM) lastCaseConds
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
        contArgName = bindContArgName contName
        -- @cont(contArg\@objExpr)= <restStmt>@ — a nested declaration so its body is preprocessed. The
        -- bind's pattern is aliased onto the parameter (as the working inline form does) rather than
        -- destructured in the parameter position, which would match it as a type and leave it 'TopType'.
        contObjExpr = applyRawBareParams (rawValDerived DerivedBindCont (getExprMeta objExpr) contName) [RawAliasExpr (rawVal contArgName) objExpr]
        contDef = RawStatementTree (RawDeclStatement (RawObjArr (Just contObjExpr) TypeObj Nothing [] (Just (Just $ rawVal nestedDeclaration, Nothing)) Nothing)) restStmt
        -- @bindap(m= arrExpr, fn(v)= cont(contArg= v))@.
        contCall = rawVal contName `applyRawArgs` [(Just $ partialKey (mkName contArgName), rawVal bindApV)]
        fnHead = applyRawBareParams (rawVal bindApFn) [rawVal bindApV]
        bindapCall = rawVal bindApName `applyRawEArgs` [(Just (rawVal bindApM), arrExpr), (Just fnHead, contCall)]
    aux _ s = fail $ printf "Unsupported subDeclStatemnt type: %s" (show s)

-- | The bodies a declaration is written as a choice between the statements under it: each of those
-- is an arm, which desugar makes an arrow of its own ('MapMeta.DerivedBranchArm'). They are the
-- names 'declPreprocessorList' gives the if, match and case preprocessors.
branchDeclNames :: [String]
branchDeclNames = ["if", "match", "case"]

-- | The parameter of the continuation desugar writes for a bind, named from the continuation's own
-- name: the bound value is passed to the continuation as this argument.
bindContArgName :: String -> String
bindContArgName contName = contName ++ "-arg"

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
