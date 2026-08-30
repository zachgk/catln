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
import           CtConstants
import           Data.Bifunctor                   (first)
import           Data.Graph                       hiding (path)
import           Data.List                        (partition)
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

  -- A type declared within a declaration is a type of the module it is written in rather than a
  -- helper of the declaration, so it is neither renamed under the declaration's name nor given the
  -- declaration's arguments. Only the sub functions are scoped and curried. What is left out is the
  -- shape 'singletonObjectMap' reads as a type object: a 'TypeObj' with no arrow. Both halves of
  -- that matter, as the continuation a bind desugars to is a 'TypeObj' /with/ an arrow and is a sub
  -- function like any other.
  let (subTypes, subFuns) = partition (\d -> oaBasis d == TypeObj && isNothing (oaArr d)) $ flatObjectMap subDecls
  let (oa3, subDecls2) = scopeSubDeclFunNames oa2 (objectMapFromList subFuns)
  let (oa4, subDecls3) = currySubFunctions oa3 subDecls2
  return (oa4, subDecls3 <> objectMapFromList subTypes)
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
    updateExprPath = mapExprPath (\(eM, eN) -> Value eM (mkName (show $ relPathAddPrefix inheritPath (getPath (nameStr eN)))))
    objExpr4 = case nameStr (oaObjPath oa) of
      ContextInStr -> mapExprAppliedArg updateExprPath (partialKey (mkName contextValStr)) objExpr3
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
    [] -> do
      (decl', subObjMap) <- flattenNestedDeclarations statementEnv (decl, subStatements)
      let decl'' = declToObjArrow statementEnv decl'
      let objMap = singletonObjectMap decl'' <> subObjMap
      return $ Prgm objMap (classGraphFromObjs objMap) []
    _ -> do
      preprocessed' <- mapM fromSt preprocessed
      return $ mconcat preprocessed'
    where
      fromSt (DPPStatements newStatements) = mconcat <$> mapM (desStatement statementEnv) newStatements
      fromSt (DPPStTree st subStatements') = case st of
        MultiTypeDefStatement multiTypeDef -> desMultiTypeDef statementEnv multiTypeDef subStatements'
        TypeDefStatement typeDef extends -> desTypeDef statementEnv typeDef extends subStatements'
        RawClassDefStatement classDef -> desClassDef statementEnv False classDef subStatements'
        RawClassDeclStatement classDecls extends -> desClassDecl statementEnv classDecls extends subStatements'
        RawApplyStatement{} -> error "Not yet implemented"
        RawModule name -> desInheritingSubstatements statementEnv (getPath $ nameStr $ makeAbsoluteName (mkName name)) subStatements'

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
    -- 'emptyTypeEnv'' is exact here: desugar-time occurrences always carry a ⊤ inner (only
    -- the parser constructs them), so the substitution's inner-meet is never consulted.
    replaceMetaVar (ExprMeta InputMeta ExprMetaApplyVarVal) (Meta t pos mid md) = return $ Meta (substituteVarsWithVarEnv emptyTypeEnv' varReplaceMap t) pos mid md
    replaceMetaVar _ m = return m
    expr'' = mapMetaAppliedExpr replaceMetaVar InputMeta expr'


desMultiTypeDef :: StatementEnv -> MultiTypeDef ParseMetaDat -> [RawStatementTree RawExpr ParseMetaDat] -> CRes DesPrgm
desMultiTypeDef statementEnv@(inheritPath, _) (MultiTypeDef clssExpr dataExprs extends) subStatements = do

  let clss@PartialType{ptVars=classVars} = exprToPartialType clssExpr
  let clss' = clss{ptName=makeAbsoluteName (mkName (show path'))}

  let dataTypes = map (either id (substituteVarsWithVarEnv emptyTypeEnv' classVars . getExprType . exprPropagateTypes . semiDesExpr SDType Nothing) . eitherTypeVarExpr) dataExprs

  let objs = objectMapFromList $ map (desMultiTypeDefObj inheritPath classVars) $ rights $ map eitherTypeVarExpr dataExprs
  let objPaths = map (PRelativeName . oaObjPath) $ flatObjectMap objs

  let typeCGNodes = map (CGType, , []) objPaths
  let classCGNode = (CGClass (True, clss', dataTypes, desObjDocComment subStatements), PClassName (makeAbsoluteName (mkName (show path'))), objPaths)
  let extendNodes = [(CGClass (False, exprToPartialType extendClass, [classPartial clss'], Nothing), PClassName (exprPath extendClass), [PClassName $ makeAbsoluteName $ mkName $ show path']) | extendClass <- extends]
  let classGraph' = ClassGraph $ graphFromEdges (classCGNode:extendNodes ++ typeCGNodes)

  (subPrgm, annots) <- splitPrgmAnnots <$> desInheritingSubstatements statementEnv path subStatements
  return (Prgm objs classGraph' annots <> subPrgm)
    where
      path = getPath $ nameStr $ exprPath clssExpr
      path' = relPathAddPrefix inheritPath path

      eitherTypeVarExpr e@(RawValue _ n) = case parseTVVar (nameStr n) of
        Just t  -> Left t
        Nothing -> Right e
      eitherTypeVarExpr e                      = Right e

desClassDecl :: StatementEnv -> PExpr -> ExtendedClasses RawExpr ParseMetaDat -> [RawStatementTree RawExpr ParseMetaDat] -> CRes DesPrgm
desClassDecl statementEnv@(inheritPath, _) clssExpr extends subStatements = do
  let path = getPath $ nameStr $ exprPath clssExpr
  let path' = show $ relPathAddPrefix inheritPath path
  let clss = exprToPartialType clssExpr
  let extendNodes = [(CGClass (False, exprToPartialType extendClass, [classPartial clss], Nothing), PClassName (exprPath extendClass), [PClassName $ makeAbsoluteName (mkName path')]) | extendClass <- extends]
  let classGraph' = ClassGraph $ graphFromEdges ((CGClass (False, clss, [], desObjDocComment subStatements), PClassName (makeAbsoluteName (mkName path')), []):extendNodes)
  subPrgm <- desInheritingSubstatements statementEnv path subStatements
  return (Prgm mempty classGraph' [] <> subPrgm)

desTypeDef :: StatementEnv -> PExpr -> ExtendedClasses RawExpr ParseMetaDat -> [RawStatementTree RawExpr ParseMetaDat] -> CRes DesPrgm
desTypeDef statementEnv@(inheritPath, _) typeExpr extends subStatements = do
  (subPrgm, annots) <- splitPrgmAnnots <$> desInheritingSubstatements statementEnv (getPath $ nameStr $ exprPath typeExpr) subStatements
  let typeExpr' = semiDesExpr SDType (Just typeExpr) typeExpr
  let type' = exprToPartialType typeExpr
  let obj = desObj False inheritPath UseRelativeName $ ObjArr (Just typeExpr') TypeObj (desObjDocComment subStatements) annots Nothing
  let objMap = objectMapFromList [obj]
  -- Register the class membership under the member's /resolved absolute/ name (e.g.
  -- @/Scratch/MyEqv@). 'exprToPartialType' yields the relative source name (@MyEqv@), which the
  -- class-graph resolution pass does not absolutize for concrete member partials, so a membership
  -- lookup against the absolute type name would miss (unlike the @every@ form, which already
  -- stores the resolved name). 'oaObjPath' is the data object's absolute path.
  let memberName = oaObjPath obj
  -- Record the class's type-parameter binding for /this member/ on the member entry itself (e.g.
  -- @data IntOrdering isa Ordering[$T -> Integer]@ stores the member as @IntOrdering[$T=Integer]@).
  -- The shared class node keeps only one parent partial, so per-member element bindings must live on
  -- each member or they are lost when memberships merge.
  let extendNodes = [(CGClass (False, ec, [singletonType type'{ptName=memberName, ptVars=ptVars ec}], Nothing), PClassName (exprPath extendClass), [PTypeName memberName]) | extendClass <- extends, let ec = exprToPartialType extendClass]
  let classGraph = classGraphFromObjs objMap <> ClassGraph (graphFromEdges extendNodes)
  return (Prgm objMap classGraph [] <> subPrgm)

desClassDef :: StatementEnv -> Sealed -> RawClassDef ParseMetaDat -> [RawStatementTree RawExpr ParseMetaDat] -> CRes DesPrgm
desClassDef statementEnv@(inheritPath, _) sealed (typeExpr, extends) subStatements = do
  let typeExpr' = exprPropagateTypes $ semiDesExpr SDType (Just typeExpr) typeExpr
  let classGraph = ClassGraph $ graphFromEdges [(CGClass (sealed, partialVal (mkName (show path')), [getExprType typeExpr'], desObjDocComment subStatements), PClassName (exprPath className), [PRelativeName $ exprPath typeExpr']) | className <- extends ]
  subPrgm <- desInheritingSubstatements statementEnv path subStatements
  return (Prgm mempty classGraph [] <> subPrgm)
  where
    path = getPath $ nameStr $ fromJust $ maybeExprPath typeExpr
    path' = relPathAddPrefix inheritPath path

desGlobalAnnot :: PCompAnnot -> CRes DesCompAnnot
desGlobalAnnot = return . desExpr . semiDesExpr SDOutput Nothing

desStatement :: StatementEnv -> PStatementTree -> CRes DesPrgm
desStatement statementEnv@(inheritModule, inheritAnnots) (RawStatementTree statement subStatements) = case statement of
  RawDeclStatement decl -> desDecl statementEnv decl subStatements
  RawBindStatement{} -> error $ printf "Not yet implemented"
  RawAnnot a | null subStatements -> do
                 a' <- desGlobalAnnot a
                 return (Prgm mempty mempty [a'])
  RawAnnot a -> do
    a' <- desGlobalAnnot a
    statements' <- mapM (desStatement (inheritModule, a':inheritAnnots)) subStatements
    return $ mconcat statements'

-- | Each node carries the name of the page it came from, which names the scope that page's
-- synthesized metas are numbered in (see 'MetaIDScope').
desFinalPasses :: [(DesPrgm, FileImport, [DesPrgm], [DesPrgm])] -> [DesPrgm]
desFinalPasses = map desFinalPass

-- | One node's final passes. Rewriting a program is all this does — it reads no file and asks
-- nothing of the world — so it is a function rather than an action, which is what lets a caller
-- holding only the programs (such as the debugger's expression compile, @CtService.debugExprRun@)
-- run it without an 'IO' to run it in.
desFinalPass :: (DesPrgm, FileImport, [DesPrgm], [DesPrgm]) -> DesPrgm
desFinalPass (prgm1, name, sccPeerDeps, myExtDeps) = addPrgmMetaIDs (desugarMetaIDScope name) prgm7
  where
    fullPrgm1 = mconcat ([prgm1] ++ sccPeerDeps ++ myExtDeps)
    fullPrgm2 = removeClassInstanceObjects fullPrgm1 fullPrgm1

    -- Run removeClassInstanceObjects
    prgm2 = removeClassInstanceObjects fullPrgm1 prgm1

    -- Run resolveRelativeNames pass
    prgm3 = resolveRelativeNames fullPrgm2 prgm2

    -- Expose each class's type parameters on the receivers of its methods, using the fully
    -- resolved class graph (so a method's @$T@ ties to the class parameter it is declared over).
    fullPrgm3 = resolveRelativeNames fullPrgm2 fullPrgm2

    -- Redirect a receiver-method instance defined in a module other than the one declaring the
    -- method (e.g. @:Integer.eq@ in @Data/Primitive@) onto the imported declaration it implements.
    prgm3b = reassignImportedMethodDefs fullPrgm3 prgm3

    prgm4 = linkClassMethodReceivers fullPrgm3 prgm3b

    -- Complete authored type annotations with their declared data fields, so a bare data-type
    -- reference (e.g. @[$M -> BoxA]@) denotes the full type rather than the 0-arg partial.
    fullPrgm4 = completeAnnotationTypes fullPrgm3 fullPrgm3
    prgm5 = completeAnnotationTypes fullPrgm3 prgm4

    -- Copy each declaration's (variable-free) return type onto its annotation-less definitions,
    -- so a def under a separate decl solves like an inline-annotated one.
    prgm6 = seedDefResultTypes fullPrgm4 prgm5

    -- Name each reference to one of an object's OWN declared variables as the parent reference it
    -- is, while the binder that gives it meaning is still lexically in view. Last, so it sees the
    -- annotations the passes above complete and seed.
    prgm7 = classifyParentVarRefs prgm6

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
  -- Each node travels with its own key, which 'mapGraphWithDeps' does not pass on and
  -- 'desFinalPasses' names its meta id scope by.
  let prgms3 = fmapGraphWithKey (\name prgm -> (prgm, name)) $ graphFromEdges prgms2
  mapGraphWithDeps (return . desFinalPasses . map withKey) prgms3
  where
    withKey ((prgm, name), sccPeerDeps, myExtDeps) = (prgm, name, map fst sccPeerDeps, myExtDeps)
