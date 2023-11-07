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
import           Data.Bifunctor                   (first)
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
flattenNestedDeclarations (roa@RawObjArr{roaObj=Just (GuardExpr objExpression _), roaAnnots}, subStatements) = decl':subDecls4
  where
    objDoc = desObjDocComment subStatements
    (subDecls, annots1) = splitDeclSubStatements subStatements
    subDecls2 = concatMap flattenNestedDeclarations subDecls
    annots2 = fmap (semiDesExpr SDOutput (Just objExpression)) (annots1 ++ roaAnnots)

    oa2 = (semiDesObjArr roa){oaAnnots=annots2, oaDoc=objDoc}

    (oa3, subDecls3) = scopeSubDeclFunNames oa2 subDecls2
    (oa4, subDecls4) = currySubFunctions oa3 subDecls3
    decl' = PSemiDecl oa4
flattenNestedDeclarations d = error $ printf "flattenNestedDeclarations without input expression: %s" (show d)

data DOEValName = UseRelativeName | UseTypeName deriving (Eq, Show)

-- |
-- When finding objectExpressions, it requires argMode to know whether something is an arg or a val.
-- Calls with args as foo(f=bar) or foo(bar) would parse bar with argMode
-- Calls not in an arg size or at the root would need to parse the foo with varMode
-- When it creates a val, it requires the second boolean arg "useRelativeName" to know whether to create a relative or absolute val name
-- TODO: The useRelativeName arg (and all DOEValName) seem to not be used, so should remove
desObjValToArg :: DOEValName -> DesExpr -> DesExpr
desObjValToArg _ (CExpr m c) = CExpr m c
desObjValToArg _ (Value m n) = Value m n
desObjValToArg _ (HoleExpr m h) = HoleExpr m h
desObjValToArg useRelativeName (AliasExpr b a) = AliasExpr (desObjValToArg useRelativeName b) (desObjValToArg useRelativeName a)
desObjValToArg useRelativeName (TupleApply m (bm, be) arg) = TupleApply m (bm, be') arg'
  where
    be' = desObjValToArg useRelativeName be
    arg' = mapTupleArgValue (desObjValToArg useRelativeName) arg
desObjValToArg useRelativeName (VarApply m be varName varVal) = VarApply m be' varName varVal
  where
    be' = desObjValToArg useRelativeName be



desObj :: Bool -> String -> DOEValName -> DesObjArr -> DesObjArr
desObj isDef inheritPath useRelativeName oa@ObjArr{oaObj=Just (GuardExpr objE objG)} = oa{oaObj=Just (GuardExpr objExpr4 objG2)}
  where
    objExpr2 = desObjValToArg useRelativeName objE

    objExpr3 = if isDef
      then objExpr2
      else snd $ desObjPropagateTypes objExpr2

    -- Inherit the path in main object name. If main is a context, instead inherit in the context function as well
    updateExprPath = mapExprPath (\(eM, eN) -> Value eM (addPath inheritPath eN))
    objExpr4 = case oaObjPath oa of
      "/Context" -> mapExprAppliedArg updateExprPath "value" objExpr3
      _          -> updateExprPath objExpr3


    objG2 = fmap desExpr objG
desObj _ _ _ oa = error $ printf "Unexpected desObj with no input exrpression: %s" (show oa)

semiDesObjArr :: PObjArr -> PSObjArr
semiDesObjArr roa@RawObjArr{roaObj=Just{}} = oa{
  oaObj=Just (GuardExpr oE' oG'),
  oaAnnots = fmap (semiDesExpr SDOutput (Just oE)) oaAnnots,
  oaArr=oaArr'
  }
  where
    [oa@ObjArr{oaObj=Just (GuardExpr oE oG), oaAnnots, oaArr}] = desObjArr roa
    oE' = semiDesExpr SDInput Nothing oE
    oG' = fmap (semiDesExpr SDOutput (Just oE)) oG
    oaArr' = first (fmap (\(GuardExpr aE aG) -> GuardExpr (semiDesExpr SDOutput (Just oE) aE) (fmap (semiDesExpr SDOutput (Just oE)) aG))) oaArr
semiDesObjArr oa = error $ printf "Unexpected semiDesObjArr with no input expression: %s" (show oa)

declToObjArrow :: StatementEnv -> PSemiDecl -> DesObjectMapItem
declToObjArrow (inheritPath, inheritAnnots) (PSemiDecl oa@ObjArr{oaAnnots, oaArr}) = oa3
  where
    oa2 = desObj True inheritPath UseRelativeName oa

    oa3 = oa2{
      oaAnnots=map desExpr oaAnnots ++ inheritAnnots,
      oaArr = first (fmap desGuardExpr) oaArr
      }

desDecl :: StatementEnv -> PObjArr -> [PStatementTree] -> CRes DesPrgm
desDecl statementEnv decl subStatements = do
  preprocessed <- declPreprocessors (decl, subStatements)
  let objMap = map (declToObjArrow statementEnv) $ concatMap flattenNestedDeclarations preprocessed
  return (objMap, classGraphFromObjs objMap, [])

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
desMultiTypeDefObj :: String -> H.HashMap TypeVarName Type -> PExpr -> DesObjArr
desMultiTypeDefObj inheritPath varReplaceMap expr = desObj False inheritPath UseRelativeName $ ObjArr (Just $ GuardExpr expr'' Nothing) TypeObj Nothing [] (Nothing, emptyMetaE "" expr'')
  where
    expr' = semiDesExpr SDInput Nothing expr

    -- This replaces references from obj vars of class vars
    -- Consider JOptional<$T> = Just<$T=$T> | Nothing
    -- For the object Just, it needs to be Just<TopType $T> as the classes $T is out of scope
    -- TODO: Consider adding back the following lines of code produced in experimental exprObject branch
    replaceMetaVar (ExprMeta InputMeta ExprMetaApplyVarVal) (Meta t pos md) = Meta (substituteVarsWithVarEnv varReplaceMap t) pos md
    replaceMetaVar _ m = m
    expr'' = mapMetaAppliedExpr replaceMetaVar InputMeta expr'


desMultiTypeDef :: StatementEnv -> PMultiTypeDef -> [RawStatementTree RawExpr ParseMetaDat] -> Path -> CRes DesPrgm
desMultiTypeDef statementEnv@(inheritPath, _) (MultiTypeDef clssExpr dataGuardExprs extends) subStatements path = do

  let clss@PartialType{ptVars=classVars} = exprToPartialType clssExpr
  let clss' = clss{ptName=PClassName path'}

  let dataExprs = map rgeExpr dataGuardExprs -- TODO: Handle GuardExpr guards
  let className = fromPartialName $ ptName clss
  let dataTypes = map (either id (getExprType . snd . desObjPropagateTypes . semiDesExpr SDInput Nothing) . eitherTypeVarExpr) dataExprs

  let objs = map (desMultiTypeDefObj inheritPath classVars) $ rights $ map eitherTypeVarExpr dataExprs
  let objPaths = map (PRelativeName . oaObjPath) objs

  let typeCGNodes = map (CGType, , []) objPaths
  let classCGNode = (CGClass (True, clss', dataTypes, desObjDocComment subStatements), PClassName (addPath inheritPath className), objPaths)
  let extendNodes = [(CGClass (False, partialVal (PClassName extendClass), [singletonType clss'], Nothing), PClassName extendClass, [ptName clss']) | extendClass <- extends]
  let classGraph' = ClassGraph $ graphFromEdges (classCGNode:extendNodes ++ typeCGNodes)

  (subPrgm, _) <- desInheritingSubstatements statementEnv path subStatements
  return $ mergeExprPrgm (objs, classGraph', []) subPrgm
    where
      path' =  case path of
        Absolute p -> p
        Relative p -> inheritPath ++ "/" ++ p

      eitherTypeVarExpr e@(RawValue _ n) = case parseTVVar n of
        Just t  -> Left t
        Nothing -> Right e
      eitherTypeVarExpr e                      = Right e

desClassDecl :: StatementEnv -> PExpr -> [RawStatementTree RawExpr ParseMetaDat] -> Path -> CRes DesPrgm
desClassDecl statementEnv@(inheritPath, _) clssExpr subStatements path = do
  let clss = exprToPartialType clssExpr
  let className = fromPartialName $ ptName clss
  let classGraph' = ClassGraph $ graphFromEdges [(CGClass (False, clss, [], desObjDocComment subStatements), PClassName (addPath inheritPath className), [])]
  (subPrgm, _) <- desInheritingSubstatements statementEnv path subStatements
  return $ mergeExprPrgm ([], classGraph', []) subPrgm

desTypeDef :: StatementEnv -> PExpr -> [RawStatementTree RawExpr ParseMetaDat] -> CRes DesPrgm
desTypeDef statementEnv@(inheritPath, _) typeExpr subStatements = do
  (subPrgm, annots) <- desInheritingSubstatements statementEnv (getPath $ exprPath typeExpr) subStatements
  let typeExpr' = semiDesExpr SDInput Nothing typeExpr
  let obj = desObj False inheritPath UseRelativeName $ ObjArr (Just (GuardExpr typeExpr' Nothing)) TypeObj (desObjDocComment subStatements) annots (Nothing, emptyMetaE "arrM" typeExpr)
  return $ mergeExprPrgm ([obj], emptyClassGraph, []) subPrgm

desClassDef :: StatementEnv -> Sealed -> RawClassDef ParseMetaDat -> [RawStatementTree RawExpr ParseMetaDat] -> Path -> CRes DesPrgm
desClassDef statementEnv@(inheritPath, _) sealed (typeExpr, extends) subStatements path = do
  let (_, typeExpr') = desObjPropagateTypes $ semiDesExpr SDInput Nothing typeExpr
  let classGraph = ClassGraph $ graphFromEdges [(CGClass (sealed, partialVal (PClassName path'), [getExprType typeExpr'], desObjDocComment subStatements), PClassName className, [PRelativeName $ exprPath typeExpr']) | className <- extends ]
  (subPrgm, _) <- desInheritingSubstatements statementEnv path subStatements
  return $ mergeExprPrgm ([], classGraph, []) subPrgm
  where
    path' =  case path of
      Absolute p -> p
      Relative p -> inheritPath ++ "/" ++ p

mergeObjMaps :: DesObjectMap -> DesObjectMap -> DesObjectMap
mergeObjMaps = (++)

desGlobalAnnot :: PCompAnnot -> CRes DesCompAnnot
desGlobalAnnot = return . desExpr . semiDesExpr SDOutput Nothing

desStatement :: StatementEnv -> PStatementTree -> CRes DesPrgm
desStatement statementEnv@(inheritModule, inheritAnnots) (RawStatementTree statement subStatements) = case statement of
  RawDeclStatement decl -> desDecl statementEnv decl subStatements
  MultiTypeDefStatement multiTypeDef path -> desMultiTypeDef statementEnv multiTypeDef subStatements path
  TypeDefStatement typeDef -> desTypeDef statementEnv typeDef subStatements
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
  RawApplyStatement{} -> error "Not yet implemented"
  RawModule _ path -> fst <$> desInheritingSubstatements statementEnv path subStatements

finalPasses :: DesPrgmGraphData -> GraphNodes DesPrgm String -> GraphNodes DesPrgm String
finalPasses (desPrgmGraph, nodeFromVertex, vertexFromKey) (prgm1, prgmName, imports) = (prgm4, prgmName, imports)
  where
    -- Build fullPrgm with recursive imports
    vertex = fromJust $ vertexFromKey prgmName
    importTree = reachable desPrgmGraph vertex
    fullPrgm1 = mergeExprPrgms $ map (fst3 . nodeFromVertex) importTree

    -- Run removeClassInstanceObjects
    prgm2 = removeClassInstanceObjects fullPrgm1 prgm1
    fullPrgm2 = removeClassInstanceObjects fullPrgm1 fullPrgm1

    -- Run resolveRelativeNames pass
    prgm3 = resolveRelativeNames fullPrgm2 prgm2
    fullPrgm3 = resolveRelativeNames fullPrgm2 fullPrgm2

    -- Run expandDataReferences pass
    prgm4 = expandDataReferences fullPrgm3 prgm3


desPrgm :: PPrgm -> CRes DesPrgm
desPrgm (_, statements) = do
  statements' <- mapM (desStatement ("", [])) statements
  return $ mergeExprPrgms statements'

desFiles :: PPrgmGraphData -> CRes DesPrgmGraphData
desFiles graphData = do
  -- initial desugar
  prgms' <- mapMFst3 desPrgm (graphToNodes graphData)
  let graphData' = graphFromEdges prgms'

  -- final passes
  let prgms'' = map (finalPasses graphData') prgms'
  return $ graphFromEdges prgms''
