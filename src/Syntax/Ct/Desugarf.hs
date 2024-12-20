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

import           Control.Monad                    (forM)
import           Control.Monad.Trans
import           CRes
import           CtConstants
import           Data.Bifunctor                   (first)
import           Data.Graph                       hiding (path)
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
flattenNestedDeclarations :: StatementEnv -> PDeclTree -> CRes (DesObjArr, DesObjectMap)
flattenNestedDeclarations statementEnv (roa@RawObjArr{roaObj=Just objExpression, roaAnnots}, subStatements) = do
  let objDoc = desObjDocComment subStatements
  subStatements' <- mapM (desStatement statementEnv) subStatements
  let (Prgm subDecls _ annots1) = mconcat subStatements'
  let annots2 = annots1 ++ fmap (semiDesExpr SDOutput (Just objExpression)) roaAnnots

  let oa2 = (semiDesObjArr roa){oaAnnots=annots2, oaDoc=objDoc}

  let (oa3, subDecls2) = scopeSubDeclFunNames oa2 subDecls
  let (oa4, subDecls3) = currySubFunctions oa3 subDecls2
  return (oa4, subDecls3)
flattenNestedDeclarations _ d = error $ printf "flattenNestedDeclarations without input expression: %s" (show d)

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
desObjValToArg useRelativeName (EWhere m b a) = EWhere m (desObjValToArg useRelativeName b) (desObjValToArg useRelativeName a)
desObjValToArg useRelativeName (TupleApply m (bm, be) arg) = TupleApply m (bm, be') arg'
  where
    be' = desObjValToArg useRelativeName be
    arg' = case arg of
      EAppArg a -> EAppArg $ mapTupleArgValue (desObjValToArg useRelativeName) a
      a@EAppVar{} -> a
      EAppSpread a -> EAppSpread a


desObj :: Bool -> String -> DOEValName -> DesObjArr -> DesObjArr
desObj isDef inheritPath useRelativeName oa@ObjArr{oaObj=Just objE} = oa{oaObj=Just objExpr4}
  where
    objExpr2 = desObjValToArg useRelativeName objE

    objExpr3 = if isDef
      then objExpr2
      else exprPropagateTypes objExpr2

    -- Inherit the path in main object name. If main is a context, instead inherit in the context function as well
    updateExprPath = mapExprPath (\(eM, eN) -> Value eM (show $ relPathAddPrefix inheritPath (getPath eN)))
    objExpr4 = case oaObjPath oa of
      ContextInStr -> mapExprAppliedArg updateExprPath (partialKey contextValStr) objExpr3
      _          -> updateExprPath objExpr3
desObj _ _ _ oa = error $ printf "Unexpected desObj with no input exrpression: %s" (show oa)

semiDesObjArr :: PObjArr -> PSObjArr
semiDesObjArr roa@RawObjArr{roaObj=Just oE} = oa{
  oaObj=Just oE'',
  oaAnnots = fmap (semiDesExpr SDOutput (Just oE')) oaAnnots,
  oaArr=oaArr'
  }
  where
    oa@ObjArr{oaObj=moE', oaAnnots, oaArr} = head $ desObjArr (Just oE) roa
    oE' = fromJust moE'
    oE'' = semiDesExpr (SDInput False) (Just oE) oE'
    oaArr' = fmap (first (fmap (semiDesExpr SDOutput (Just oE')))) oaArr
semiDesObjArr oa = error $ printf "Unexpected semiDesObjArr with no input expression: %s" (show oa)

splitPrgmAnnots :: DesPrgm -> (DesPrgm, [DesCompAnnot])
splitPrgmAnnots prgm@Prgm{prgmAnnots} = (prgm{prgmAnnots=[]}, prgmAnnots)

declToObjArrow :: StatementEnv -> DesObjArr -> DesObjectMapItem
declToObjArrow (inheritPath, inheritAnnots) oa@ObjArr{oaAnnots, oaArr} = oa3
  where
    oa2 = desObj True inheritPath UseRelativeName oa

    oa3 = oa2{
      oaAnnots=map desExpr oaAnnots ++ inheritAnnots,
      oaArr = fmap (first (fmap desExpr)) oaArr
      }

desDecl :: StatementEnv -> PObjArr -> [PStatementTree] -> CRes DesPrgm
desDecl statementEnv decl subStatements = do
  preprocessed <- declPreprocessors (decl, subStatements)
  case preprocessed of
    DPPStatements newStatements -> mconcat <$> mapM (desStatement statementEnv) newStatements
    DPPNothing -> do
      (decl', subObjMap) <- flattenNestedDeclarations statementEnv (decl, subStatements)
      let decl'' = declToObjArrow statementEnv decl'
      let objMap = decl'' : subObjMap
      return $ Prgm objMap (classGraphFromObjs objMap) []
    DPPStTree st subStatements' -> case st of
      MultiTypeDefStatement multiTypeDef -> desMultiTypeDef statementEnv multiTypeDef subStatements'
      TypeDefStatement typeDef extends -> desTypeDef statementEnv typeDef extends subStatements'
      RawClassDefStatement classDef -> desClassDef statementEnv False classDef subStatements'
      RawClassDeclStatement classDecls extends -> desClassDecl statementEnv classDecls extends subStatements'
      RawApplyStatement{} -> error "Not yet implemented"
      RawModule name -> desInheritingSubstatements statementEnv (getPath $ makeAbsoluteName name) subStatements'

-- | Desugars statements that inherit a path from a main statement
desInheritingSubstatements :: StatementEnv -> NPath -> [PStatementTree] -> CRes DesPrgm
desInheritingSubstatements (inheritModule, inheritAnnots) path subStatements = do
  let statementEnv' = (show path', inheritAnnots)
  subStatements' <- mapM (desStatement statementEnv') subStatements
  let (Prgm objectMap classGraph annots) = mconcat subStatements'
  return $ Prgm objectMap classGraph annots
  where
    path' = relPathAddPrefix inheritModule path

-- | Parses an object from a 'MultiTypeDefData'
desMultiTypeDefObj :: String -> H.HashMap TypeVarName Type -> PExpr -> DesObjArr
desMultiTypeDefObj inheritPath varReplaceMap expr = desObj False inheritPath UseRelativeName $ ObjArr (Just expr'') TypeObj Nothing [] Nothing
  where
    expr' = semiDesExpr SDType (Just expr) expr

    -- This replaces references from obj vars of class vars
    -- Consider JOptional<$T> = Just<$T=$T> | Nothing
    -- For the object Just, it needs to be Just<TopType $T> as the classes $T is out of scope
    replaceMetaVar (ExprMeta InputMeta ExprMetaApplyVarVal) (Meta t pos mid md) = return $ Meta (substituteVarsWithVarEnv varReplaceMap t) pos mid md
    replaceMetaVar _ m = return m
    expr'' = mapMetaAppliedExpr replaceMetaVar InputMeta expr'


desMultiTypeDef :: StatementEnv -> MultiTypeDef ParseMetaDat -> [RawStatementTree RawExpr ParseMetaDat] -> CRes DesPrgm
desMultiTypeDef statementEnv@(inheritPath, _) (MultiTypeDef clssExpr dataExprs extends) subStatements = do

  let clss@PartialType{ptVars=classVars} = exprToPartialType clssExpr
  let clss' = clss{ptName=show path'}

  let dataTypes = map (either id (getExprType . exprPropagateTypes . semiDesExpr SDType Nothing) . eitherTypeVarExpr) dataExprs

  let objs = map (desMultiTypeDefObj inheritPath classVars) $ rights $ map eitherTypeVarExpr dataExprs
  let objPaths = map (PRelativeName . oaObjPath) objs

  let typeCGNodes = map (CGType, , []) objPaths
  let classCGNode = (CGClass (True, clss', dataTypes, desObjDocComment subStatements), PClassName (makeAbsoluteName $ show path'), objPaths)
  let extendNodes = [(CGClass (False, exprToPartialType extendClass, [singletonType clss'], Nothing), PClassName (exprPath extendClass), [PClassName $ makeAbsoluteName $ show path']) | extendClass <- extends]
  let classGraph' = ClassGraph $ graphFromEdges (classCGNode:extendNodes ++ typeCGNodes)

  (subPrgm, annots) <- splitPrgmAnnots <$> desInheritingSubstatements statementEnv path subStatements
  return (Prgm objs classGraph' annots <> subPrgm)
    where
      path = getPath $ exprPath clssExpr
      path' = relPathAddPrefix inheritPath path

      eitherTypeVarExpr e@(RawValue _ n) = case parseTVVar n of
        Just t  -> Left t
        Nothing -> Right e
      eitherTypeVarExpr e                      = Right e

desClassDecl :: StatementEnv -> PExpr -> ExtendedClasses RawExpr ParseMetaDat -> [RawStatementTree RawExpr ParseMetaDat] -> CRes DesPrgm
desClassDecl statementEnv@(inheritPath, _) clssExpr extends subStatements = do
  let path = getPath $ exprPath clssExpr
  let path' = show $ relPathAddPrefix inheritPath path
  let clss = exprToPartialType clssExpr
  let extendNodes = [(CGClass (False, exprToPartialType extendClass, [singletonType clss], Nothing), PClassName (exprPath extendClass), [PClassName $ makeAbsoluteName path']) | extendClass <- extends]
  let classGraph' = ClassGraph $ graphFromEdges ((CGClass (False, clss, [], desObjDocComment subStatements), PClassName (makeAbsoluteName path'), []):extendNodes)
  subPrgm <- desInheritingSubstatements statementEnv path subStatements
  return (Prgm [] classGraph' [] <> subPrgm)

desTypeDef :: StatementEnv -> PExpr -> ExtendedClasses RawExpr ParseMetaDat -> [RawStatementTree RawExpr ParseMetaDat] -> CRes DesPrgm
desTypeDef statementEnv@(inheritPath, _) typeExpr extends subStatements = do
  (subPrgm, annots) <- splitPrgmAnnots <$> desInheritingSubstatements statementEnv (getPath $ exprPath typeExpr) subStatements
  let typeExpr' = semiDesExpr SDType (Just typeExpr) typeExpr
  let type' = exprToPartialType typeExpr
  let obj = desObj False inheritPath UseRelativeName $ ObjArr (Just typeExpr') TypeObj (desObjDocComment subStatements) annots Nothing
  let objMap = [obj]
  let extendNodes = [(CGClass (False, exprToPartialType extendClass, [singletonType type'], Nothing), PClassName (exprPath extendClass), [PTypeName $ ptName type']) | extendClass <- extends]
  let classGraph = classGraphFromObjs objMap <> ClassGraph (graphFromEdges extendNodes)
  return (Prgm objMap classGraph [] <> subPrgm)

desClassDef :: StatementEnv -> Sealed -> RawClassDef ParseMetaDat -> [RawStatementTree RawExpr ParseMetaDat] -> CRes DesPrgm
desClassDef statementEnv@(inheritPath, _) sealed (typeExpr, extends) subStatements = do
  let typeExpr' = exprPropagateTypes $ semiDesExpr SDType (Just typeExpr) typeExpr
  let classGraph = ClassGraph $ graphFromEdges [(CGClass (sealed, partialVal $ show path', [getExprType typeExpr'], desObjDocComment subStatements), PClassName (exprPath className), [PRelativeName $ exprPath typeExpr']) | className <- extends ]
  subPrgm <- desInheritingSubstatements statementEnv path subStatements
  return (Prgm [] classGraph [] <> subPrgm)
  where
    path = getPath $ fromJust $ maybeExprPath typeExpr
    path' = relPathAddPrefix inheritPath path

mergeObjMaps :: DesObjectMap -> DesObjectMap -> DesObjectMap
mergeObjMaps = (++)

desGlobalAnnot :: PCompAnnot -> CRes DesCompAnnot
desGlobalAnnot = return . desExpr . semiDesExpr SDOutput Nothing

desStatement :: StatementEnv -> PStatementTree -> CRes DesPrgm
desStatement statementEnv@(inheritModule, inheritAnnots) (RawStatementTree statement subStatements) = case statement of
  RawDeclStatement decl -> desDecl statementEnv decl subStatements
  RawBindStatement{} -> error $ printf "Not yet implemented"
  RawAnnot a | null subStatements -> do
                 a' <- desGlobalAnnot a
                 return (Prgm [] mempty [a'])
  RawAnnot a -> do
    a' <- desGlobalAnnot a
    statements' <- mapM (desStatement (inheritModule, a':inheritAnnots)) subStatements
    return $ mconcat statements'

desFinalPasses :: [DesPrgm] -> [DesPrgm] -> CResT IO [DesPrgm]
desFinalPasses prgms fullPrgms = do
  let fullPrgm1 = mconcat (prgms ++ fullPrgms)
  let fullPrgm2 = removeClassInstanceObjects fullPrgm1 fullPrgm1

  forM prgms $ \prgm1 -> do
    -- Run removeClassInstanceObjects
    let prgm2 = removeClassInstanceObjects fullPrgm1 prgm1

    -- Run resolveRelativeNames pass
    let prgm3 = resolveRelativeNames fullPrgm2 prgm2
    lift $ mapMetaPrgmM addMetaID prgm3

desPrgm :: (PPrgm, FileImport, [FileImport]) -> CRes (DesPrgm, FileImport, [FileImport])
desPrgm (RawPrgm _ statements, name, imports) = do
  -- desImports
  statements' <- mapM (desStatement ("", [])) statements
  let prgm' = mconcat statements'

  -- desPrgm
  return (prgm', name, imports)

-- | Checks whether a program is valid for desugaring
-- | This mostly means without disqualifying annotations such as 'ctxAnnot'
validPrgm :: PPrgm -> Bool
validPrgm prgm = not $ rawPrgmHasAnnot prgm ctxAnnot

desFiles :: PPrgmGraphData -> CResT IO DesPrgmGraphData
desFiles prgms1 = do
  -- initial desugar
  prgms2 <- asCResT $ mapM desPrgm (filter (validPrgm . fst3) $ graphToNodes prgms1)
  let prgms3 = graphFromEdges prgms2
  mapGraphWithDeps desFinalPasses prgms3
