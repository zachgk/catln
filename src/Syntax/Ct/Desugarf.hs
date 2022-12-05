--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Desugarf
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module desugars a 'RawPrgm' into a 'Prgm' by removing all
-- language features outside of a minimalist core set.
--------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}
module Syntax.Ct.Desugarf where


import           Data.Either
import qualified Data.HashMap.Strict              as H
import           Data.Maybe
import           Text.Printf

import           CRes
import           Data.Graph                       hiding (path)
import           Data.List
import           MapMeta
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Desugarf.Expr
import           Syntax.Ct.Desugarf.Nested
import           Syntax.Ct.Desugarf.Passes
import           Syntax.Ct.Desugarf.Preprocessors
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Utils

type StatementEnv = (String, [DesCompAnnot])

-- | Flattens a declaration tree (with nested declarations)
flattenNestedDeclarations :: PDeclTree -> [PSemiDecl]
flattenNestedDeclarations (RawDecl (DeclLHS arrM (Pattern obj@(ExprObject objBasis _ objExpression) guard1)) expr1, subStatements) = decl':subDecls4
  where
    objDoc = desObjDocComment subStatements
    (subDecls, annots1) = splitDeclSubStatements subStatements
    subDecls2 = concatMap flattenNestedDeclarations subDecls
    expr2 = fmap (semiDesExpr (Just obj)) expr1
    annots2 = fmap (semiDesExpr (Just obj)) annots1
    guard2 = semiDesGuard obj guard1
    (subDecls3, expr3, annots3, arrM') = scopeSubDeclFunNames (eobjPath obj) subDecls2 expr2 annots2 arrM
    objExpression' = semiDesExpr Nothing objExpression
    (subDecls4, expr4, annots4) = currySubFunctions (exprArgsLinear objExpression) subDecls3 expr3 annots3
    decl' = PSemiDecl (DeclLHS arrM' (Pattern (ExprObject objBasis objDoc objExpression') guard2)) annots4 expr4

data DOEMode = DOEArgMode | DOEValMode deriving (Eq, Show)
data DOEValName = UseRelativeName | UseTypeName deriving (Eq, Show)

-- |
-- When finding objectExpressions, it requires argMode to know whether something is an arg or a val.
-- Calls with args as foo(f=bar) or foo(bar) would parse bar with argMode
-- Calls not in an arg size or at the root would need to parse the foo with varMode
-- When it creates a val, it requires the second boolean arg "useRelativeName" to know whether to create a relative or absolute val name
-- TODO: The useRelativeName arg (and all DOEValName) seem to not be used, so should remove
desObjValToArg :: DOEMode -> DOEValName -> DesExpr -> DesExpr
desObjValToArg _ _ (CExpr m c) = CExpr m c
desObjValToArg doeMode _ (Value m n) = case doeMode of
  DOEArgMode -> Arg m n
  DOEValMode -> Value m n
desObjValToArg _ _ (Arg _ n) = error $ printf "Found unexpected arg '%s' in desOjValToArg, expected all args to still be represented with Value" n
desObjValToArg _ _ (HoleExpr m h) = HoleExpr m h
desObjValToArg _ useRelativeName (AliasExpr b a) = AliasExpr (desObjValToArg DOEValMode useRelativeName b) (desObjValToArg DOEArgMode useRelativeName a)
desObjValToArg _ useRelativeName mainExpr@(TupleApply m (bm, be) tupleArg) = TupleApply m (bm, be') tupleArg'
  where
    be' = desObjValToArg DOEValMode useRelativeName be
    tupleArg' = case tupleArg of
      TupleArgI{} -> tupleArg
      TupleArgIO argM argName argVal -> TupleArgIO argM argName argVal'
        where argVal' = desObjValToArg DOEValMode useRelativeName argVal
      TupleArgO{} -> error $ printf "Unexpected TupleArgO in desObjValToArg: %s" (show mainExpr)
desObjValToArg _ useRelativeName (VarApply m be varName varVal) = VarApply m be' varName varVal
  where
    be' = desObjValToArg DOEValMode useRelativeName be



desObj :: Bool -> String -> DOEValName -> DesObject -> DesObject
desObj isDef inheritPath useRelativeName object@ExprObject{eobjExpr} = object'
  where
    objExpr2 = desObjValToArg DOEValMode useRelativeName eobjExpr

    objExpr3 = if isDef
      then objExpr2
      else snd $ desObjPropagateTypes objExpr2

    -- Inherit the path in main object name. If main is a context, instead inherit in the context function as well
    updateExprPath = mapExprPath (\(eM, eN) -> Value eM (addPath inheritPath eN))
    objExpr4 = case eobjPath object of
      "/Context" -> mapExprAppliedArg updateExprPath "value" objExpr3
      _          -> updateExprPath objExpr3
    object' = object{eobjExpr=objExpr4}

desGuard :: PArgMetaMap -> PSGuard -> DesGuard
desGuard arrArgs = fmap (desExpr arrArgs)

semiDesGuard :: PObject -> PGuard -> PSGuard
semiDesGuard obj (IfGuard e) = IfGuard (semiDesExpr (Just obj) e)
semiDesGuard _ ElseGuard     = ElseGuard
semiDesGuard _ NoGuard       = NoGuard

declToObjArrow :: StatementEnv -> PSemiDecl -> DesObjectMapItem
declToObjArrow (inheritPath, inheritAnnots) (PSemiDecl (DeclLHS arrM (Pattern object guard)) annots expr) = (object', annots' ++ inheritAnnots, Just arrow)
  where
    object' = desObj True inheritPath UseRelativeName object

    argMetaMap = exprArgs $ eobjExpr object'
    annots' = map (desExpr argMetaMap) annots
    arrow = Arrow arrM (desGuard argMetaMap guard) (fmap (desExpr argMetaMap) expr)

desDecl :: StatementEnv -> PDecl -> [PStatementTree] -> CRes DesPrgm
desDecl statementEnv decl subStatements = do
  preprocessed <- declPreprocessors (decl, subStatements)
  let objMap = map (declToObjArrow statementEnv) $ concatMap flattenNestedDeclarations preprocessed
  return (objMap, emptyClassGraph, [])

addPath :: String -> String -> String
addPath inheritPath name = if "/" `isPrefixOf` name then
  name
  else inheritPath ++ "/" ++ name

-- | Desugars statements that inherit a path from a main statement
desInheritingSubstatements :: StatementEnv -> Path -> [PStatementTree] -> CRes (DesPrgm, [DesCompAnnot])
desInheritingSubstatements (inheritModule, inheritAnnots) path subStatements = do
  let statementEnv' = (path', inheritAnnots)
  subStatements' <- mapM (desStatement statementEnv') subStatements
  let (objectMap, classGraph, annots) = mergeExprPrgms subStatements'
  let prgm' = (objectMap, classGraph, []) -- Annots in subStatements are not global, but local to the main statement
  return (prgm', annots)
  where
    path' = case path of
      (Absolute p) -> p
      (Relative p) -> inheritModule ++ "/" ++ p

-- | Parses an object from a 'MultiTypeDefData'
desMultiTypeDefObj :: String -> H.HashMap TypeVarName Type -> PExpr -> DesObject
desMultiTypeDefObj inheritPath varReplaceMap expr = desObj False inheritPath UseRelativeName $ ExprObject TypeObj Nothing expr''
  where
    expr' = semiDesExpr Nothing expr

    -- This replaces references from obj vars of class vars
    -- Consider JOptional<$T> = Just<$T=$T> | Nothing
    -- For the object Just, it needs to be Just<TopType $T> as the classes $T is out of scope
    -- TODO: Consider adding back the following lines of code produced in experimental exprObject branch
    replaceMetaVar (ExprMeta ExprMetaApplyVarVal) (Meta t pos md) = Meta (substituteVarsWithVarEnv varReplaceMap t) pos md
    replaceMetaVar _ m = m
    expr'' = mapMetaAppliedExpr replaceMetaVar expr'


desMultiTypeDef :: StatementEnv -> PMultiTypeDef -> [RawStatementTree RawExpr ParseMetaDat] -> Path -> CRes DesPrgm
desMultiTypeDef statementEnv@(inheritPath, _) (MultiTypeDef className classVars dataExprs) subStatements path = do
  let dataTypes = map (either id (getExprType . snd . desObjPropagateTypes . semiDesExpr Nothing) . eitherTypeVarExpr) dataExprs

  let objs = map (desMultiTypeDefObj inheritPath classVars) $ rights $ map eitherTypeVarExpr dataExprs
  let objPaths = map (PRelativeName . eobjPath) objs
  let objMap' = map (, [], Nothing) objs

  let typeCGNodes = map (CGType, , []) objPaths
  let classCGNode = (CGClass (True, classVars, dataTypes, desObjDocComment subStatements, path'), PClassName (addPath inheritPath className), objPaths)
  let classGraph' = ClassGraph $ graphFromEdges (classCGNode:typeCGNodes)

  (subPrgm, _) <- desInheritingSubstatements statementEnv path subStatements
  return $ mergeExprPrgm (objMap', classGraph', []) subPrgm
    where
      path' =  case path of
        Absolute p -> p
        Relative p -> inheritPath ++ "/" ++ p

      eitherTypeVarExpr (RawValue _ n@('$':_)) = Left $ TypeVar $ TVVar n
      eitherTypeVarExpr e                      = Right e


desClassDecl :: StatementEnv -> RawClassDecl -> [RawStatementTree RawExpr ParseMetaDat] -> Path -> CRes DesPrgm
desClassDecl statementEnv@(inheritPath, _) (className, classVars) subStatements path = do
  let classGraph' = ClassGraph $ graphFromEdges [(CGClass (False, classVars, [], desObjDocComment subStatements, path'), PClassName (addPath inheritPath className), [])]
  (subPrgm, _) <- desInheritingSubstatements statementEnv path subStatements
  return $ mergeExprPrgm ([], classGraph', []) subPrgm
  where
    path' = case path of
      Relative p -> inheritPath ++ "/" ++ p
      Absolute p -> p

desTypeDef :: StatementEnv -> PTypeDef -> [RawStatementTree RawExpr ParseMetaDat] -> CRes DesPrgm
desTypeDef statementEnv@(inheritPath, _) (TypeDef typeExpr) subStatements = do
  (subPrgm, annots) <- desInheritingSubstatements statementEnv (getPath $ exprPath typeExpr) subStatements
  let typeExpr' = semiDesExpr Nothing typeExpr
  let obj = desObj False inheritPath UseRelativeName $ ExprObject TypeObj (desObjDocComment subStatements) typeExpr'
  let objMap = [(obj, annots, Nothing)]
  return $ mergeExprPrgm (objMap, emptyClassGraph, []) subPrgm

desClassDef :: StatementEnv -> Sealed -> RawClassDef ParseMetaDat -> [RawStatementTree RawExpr ParseMetaDat] -> Path -> CRes DesPrgm
desClassDef statementEnv@(inheritPath, _) sealed (typeExpr, className) subStatements path = do
  let (_, typeExpr') = desObjPropagateTypes $ semiDesExpr Nothing typeExpr
  let classGraph = ClassGraph $ graphFromEdges [(CGClass (sealed, H.empty, [getExprType typeExpr'], desObjDocComment subStatements, path'), PClassName className, [PRelativeName $ exprPath typeExpr'])]
  (subPrgm, _) <- desInheritingSubstatements statementEnv path subStatements
  return $ mergeExprPrgm ([], classGraph, []) subPrgm
  where
    path' =  case path of
      Absolute p -> p
      Relative p -> inheritPath ++ "/" ++ p

emptyClassGraph :: ClassGraph
emptyClassGraph = ClassGraph $ graphFromEdges []

mergeObjMaps :: DesObjectMap -> DesObjectMap -> DesObjectMap
mergeObjMaps = (++)

desGlobalAnnot :: PCompAnnot -> CRes DesCompAnnot
desGlobalAnnot = return . desExpr H.empty . semiDesExpr undefined

desStatement :: StatementEnv -> PStatementTree -> CRes DesPrgm
desStatement statementEnv@(inheritModule, inheritAnnots) (RawStatementTree statement subStatements) = case statement of
  RawDeclStatement decl -> desDecl statementEnv decl subStatements
  MultiTypeDefStatement multiTypeDef path -> desMultiTypeDef statementEnv multiTypeDef subStatements path
  TypeDefStatement typeDef -> desTypeDef  statementEnv typeDef subStatements
  RawClassDefStatement classDef path -> desClassDef statementEnv False classDef subStatements path
  RawClassDeclStatement classDecls path -> desClassDecl statementEnv classDecls subStatements path
  RawExprStatement e -> CErr [MkCNote $ GenCErr (getMetaPos $ getExprMeta e) $ printf "All expression statements should be in a nested declaration but instead found: %s" (show e)]
  RawAnnot a | null subStatements -> do
                 a' <- desGlobalAnnot a
                 return ([], emptyClassGraph, [a'])
  RawAnnot a -> do
    a' <- desGlobalAnnot a
    statements' <- mapM (desStatement (inheritModule, a':inheritAnnots)) subStatements
    return $ mergeExprPrgms statements'
  RawModule _ path -> fst <$> desInheritingSubstatements statementEnv path subStatements

finalPasses :: DesPrgmGraphData -> GraphNodes DesPrgm String -> GraphNodes FinalDesPrgm String
finalPasses (desPrgmGraph, nodeFromVertex, vertexFromKey) (prgm1, prgmName, imports) = (prgm4, prgmName, imports)
  where
    -- Build fullPrgm with recursive imports
    vertex = fromJust $ vertexFromKey prgmName
    importTree = reachable desPrgmGraph vertex
    fullPrgm1 = mergeExprPrgms $ map (fst3 . nodeFromVertex) importTree

    prgm2 = fromExprPrgm prgm1
    fullPrgm2 = fromExprPrgm fullPrgm1

    -- Run resolveRelativeNames pass
    prgm3 = resolveRelativeNames fullPrgm2 prgm2
    fullPrgm3 = resolveRelativeNames fullPrgm2 fullPrgm2

    -- Run expandDataReferences pass
    prgm4 = expandDataReferences fullPrgm3 prgm3

desPrgm :: PPrgm -> CRes DesPrgm
desPrgm (_, statements) = do
  statements' <- mapM (desStatement ("", [])) statements
  return $ mergeExprPrgms statements'

desFiles :: PPrgmGraphData -> CRes FinalDesPrgmGraphData
desFiles graphData = do
  -- initial desugar
  prgms' <- mapMFst3 desPrgm (graphToNodes graphData)
  let graphData' = graphFromEdges prgms'

  -- final passes
  let prgms'' = map (finalPasses graphData') prgms'
  return $ graphFromEdges prgms''
