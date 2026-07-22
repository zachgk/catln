--------------------------------------------------------------------
-- |
-- Module    :  Testing.Generation
-- Copyright :  (c) Zach Kimberg 2023
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines arbitrary code generation for property testing.
-- It helps test both the compiler itself and code written in it.
--------------------------------------------------------------------

module Testing.Generation where

import           Control.Monad
import           CtConstants
import           Data.Bifunctor          (bimap)
import           Data.Either             (partitionEithers)
import qualified Data.HashMap.Strict     as H
import           Data.List               (partition)
import           Data.Maybe
import qualified Data.Set                as S
import           Eval.Common             (Val (..))
import           Hedgehog
import qualified Hedgehog.Gen            as HG
import           Hedgehog.Range          (linear, linearFrac, singleton)
import           Semantics               (classGraphFromObjs, mkTypeEnv)
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Desugarf.Expr (desFileImport)
import           Syntax.Ct.Prgm          (RawExpr (RawCExpr), mkRawFileImport)
import           Text.Printf
import           Utils

-- | A fixed pool of variable names shared by the type-var generators (and, later, a
-- 'TypeVarArgEnv' generator), so a bound fraction of generated variables actually collide.
varNamePool :: [Name]
varNamePool = map mkName ["a", "b", "c"]

-- | Bare type variables as direct inputs ('TVVar'\/'TVArg' × 'TVInt'\/'TVExt'). With no
-- external env a variable's standalone reading is unconstrained, i.e. ⊤ ('vaenvLookup').
genTypeVar :: Gen Type
genTypeVar = do
  n   <- HG.element varNamePool
  aux <- HG.element [TVVar (partialKey n), TVArg (partialKey n)]
  loc <- HG.element [TVInt, TVExt]
  return $ TypeVar aux loc

genTypeFromExpr :: Prgm Expr () -> Expr () -> Gen PartialType
genTypeFromExpr _ (CExpr _ c) = return $ constantPartialType c
genTypeFromExpr _ (Value _ n) = return $ partialVal n
genTypeFromExpr prgm (TupleApply _ (_, baseExpr) (EAppArg oa)) = do
  base@PartialType{ptArgs=baseArgs} <- genTypeFromExpr prgm baseExpr
  shouldAddArg <- HG.bool
  if shouldAddArg
    then case oaArr oa of
           (Just (Just arrExpr, _)) -> do
             arrExpr' <- genTypeFromExpr prgm arrExpr
             return base{ptArgs = H.insert (inExprSingleton $ oaObjExpr oa) (singletonType arrExpr') baseArgs}
           (Just (Nothing, oaM)) -> return base{ptArgs = H.insert (inExprSingleton $ oaObjExpr oa) (getMetaType oaM) baseArgs}
           Nothing -> return base
    else return base
genTypeFromExpr prgm (TupleApply _ (_, baseExpr) (EAppVar varName m)) = do
  base@PartialType{ptVars=baseVars} <- genTypeFromExpr prgm baseExpr
  shouldAddVar <- HG.bool
  return $ if shouldAddVar
    then base{ptVars = H.insert varName (getMetaType m) baseVars}
    else base
genTypeFromExpr prgm (EWhere _ base cond) = do
  baseType <- genTypeFromExpr prgm base
  condType <- genTypeFromExpr prgm cond
  return $ partialAddPreds baseType (PredsOne $ PredExpr condType)
genTypeFromExpr _ e = error $ printf "Unimplemented genTypeFromExpr for %s" (show e)

-- | Generates a 'Type' from the program. A bare argument-less reference like @/Cons@ (exact
-- mode, no fields) is a legitimate standalone value — the partial-application form a @/Cons@
-- has before its args are applied (arg-set @{}@, disjoint from the field forms) — and is
-- generated as-is.
genType :: Prgm Expr () -> Gen Type
genType prgm@(Prgm objMap _ _) = HG.choice gens >>= withRandomArgModes
  where
    TypeEnv{teClassGraph=ClassGraph cg} = mkTypeEnv prgm

    -- Randomly assign a 'PtArgMode' to the generated partials, exercising the new
    -- lower\/upper-bound modes through the (precise) set operations. Only plain
    -- positive-partial unions are re-moded.
    --
    -- The fully-unbounded mode (lb=False, ub=False) is excluded for partials with args:
    -- its intersection with a lower-bounded partial of the same name is "required x,
    -- optional typed y, extras allowed", which provably cannot be expressed as positive
    -- partials (it needs a negative partial). The other three modes are exact under all
    -- the set operations.
    withRandomArgModes :: Type -> Gen Type
    withRandomArgModes (UnionType Nothing leafs []) | not (H.null leafs) = do
      leafs' <- mapM remode $ splitUnionType leafs
      return $ UnionType Nothing (joinUnionType leafs') []
      where
        remode p = do
          mode <- HG.element [PtArgMode True True, PtArgMode True False, PtArgMode False True]
          -- the lower bound is vacuous with no args, so keep it set then (avoids
          -- producing a representation set-equivalent to a different one)
          let mode' = if H.null (ptArgs p) then mode{ptArgLb=True} else mode
          return p{ptArgMode = mode'}
    withRandomArgModes t = return t

    gens = if graphEmpty cg
      then [genBasic, genConst, genMultiConst, genTypeVar]
      else [genBasic, genCGOld, genCG, genCGRel, genObjM, genObj, genConst, genMultiConst, genPredShape, genMixed, genTypeVar]

    -- Compound predicate formulae: conjunctions (and negated conjunctions) of PredClass/PredRel
    -- atoms drawn from the class graph. Exercises P∨¬P=⊤ / P∧¬P=∅ and the De Morgan handling of
    -- the union/intersection/complement paths for non-atomic predicates.
    --
    -- Atom kinds mirror the compiler's name resolution ('resolveRelPartial'): a CLASS name
    -- resolves to a 'PredClass' (or stays a 'PredRel' while relative), while a data-TYPE name in
    -- type position resolves to a positive leaf - never a 'PredClass'. So 'PredClass' atoms are
    -- generated only for class nodes; any node may appear as a 'PredRel'.
    genPreds :: Gen TypePredicates
    genPreds = do
      atoms <- HG.list (linear 1 2) genAtom
      HG.choice [ return $ foldr1 predsAnd (map PredsOne atoms)
                , return $ predsNot $ foldr1 predsAnd (map PredsOne atoms) ]
      where
        genAtom = do
          classNode <- HG.element $ graphToNodes cg
          case snd3 classNode of
            cls@PClassName{} -> do
              let nm = fromPartialName cls
              HG.element [PredClass (partialVal nm), PredRel (partialVal nm)]
            t -> return $ PredRel (partialVal (fromPartialName t))

    genPredShape :: Gen Type
    genPredShape = TopType H.empty <$> genPreds

    -- Mixed types as direct inputs: a subtractive 'Just'-top component (predicates, negative
    -- leafs, negative constants) unioned with positive leafs and constants. These arise as
    -- intermediate results of the set operations; generating them directly exercises the
    -- mixed-distribute intersection, the mixed-left subtype branch, and the mixed
    -- compactType\/complement clauses as top-level inputs.
    genMixed :: Gen Type
    genMixed = do
      preds     <- HG.choice [genPreds, genPreds, return PredsNone]
      negLeafs  <- HG.choice [return H.empty, leafsOf genObj]
      negConsts <- HG.choice [return [], nubConsts <$> HG.list (linear 1 2) genConstant]
      posLeafs  <- HG.choice [return H.empty, leafsOf genObj]
      posConsts <- HG.choice [return [], nubConsts <$> HG.list (linear 1 2) genConstant]
      if H.null posLeafs && H.null negLeafs && preds == PredsNone && null negConsts && null posConsts
        then genCG
        else return $ UnionType (Just (preds, negLeafs, negConsts)) posLeafs posConsts
      where
        nubConsts = foldr (\c acc -> if c `elem` acc then acc else c : acc) []
        leafsOf g = do
          t <- g
          return $ case t of
            UnionType Nothing ls [] -> ls
            _                       -> H.empty

    -- Unions of several constants, and constants joined with a positive partial. Single-constant
    -- 'genConst' never exercises the multi-constant union paths or the constant-not-covered-by-top
    -- expansion in 'unionTypesWithEnv'\/'intersectTypesWithVarEnv'.
    genMultiConst :: Gen Type
    genMultiConst = do
      cs <- HG.list (linear 1 3) genConstant
      leafs <- if nullObjectMap objMap then return H.empty else HG.choice [return H.empty, someLeafs]
      return $ UnionType Nothing leafs (nubConsts cs)
      where
        nubConsts = foldr (\c acc -> if c `elem` acc then acc else c : acc) []
        someLeafs = do
          t <- genObj
          return $ case t of
            UnionType Nothing ls [] -> ls
            _                       -> H.empty

    genConstant :: Gen Constant
    genConstant = HG.choice [ CInt <$> HG.integral (linear (-100) 100)
                            , CFloat <$> HG.double (linearFrac (-100) 100)
                            , CStr <$> HG.string (linear 0 5) HG.alpha
                            , CChar <$> HG.unicode ]

    -- Find all functions with single argument having thisKey
    typePropFunctions = mapMaybe checkFunction $ flatObjectMap objMap
      where
        checkFunction ObjArr{oaBasis=FunctionObj, oaObj=Just objExpr} =
          case exprAppliedArgs objExpr of
            [arg] -> case oaObj arg of
              Just (Value _ n) | nameStr n == thisKey -> Just objExpr
              _                                       -> Nothing
            _ -> Nothing
        checkFunction _ = Nothing

    genBasic :: Gen Type
    genBasic = HG.element [PTopType, BottomType]

    genConst :: Gen Type
    genConst = do
      c <- HG.choice [ CInt <$> HG.integral (linear (-100) 100)
                     , CFloat <$> HG.double (linearFrac (-100) 100)
                     , CStr <$> HG.string (linear 0 5) HG.alpha
                     , CChar <$> HG.unicode ]
      return $ UnionType Nothing H.empty [c]

    genCGOld :: Gen Type
    genCGOld = do
      classNode <- HG.element $ graphToNodes cg
      return $ typeVal $ fromPartialName $ snd3 classNode

    genCG :: Gen Type
    genCG = do
      classNode <- HG.element $ graphToNodes cg
      return $ case snd3 classNode of
        cls@PClassName{} -> TopType H.empty (PredsOne $ PredClass $ partialVal $ fromPartialName cls)
        t                -> typeVal $ fromPartialName t

    genCGRel :: Gen Type
    genCGRel = do
      classNode <- HG.element $ graphToNodes cg
      return $ relTypeVal $ fromPartialName $ snd3 classNode

    genObjM :: Gen Type
    genObjM = do
      oa <- HG.element $ flatObjectMap objMap
      let objExpr = fromJust $ oaObj oa
      return $ getExprType objExpr

    genObj :: Gen Type
    genObj = do
      oa <- HG.element $ flatObjectMap objMap
      let objExpr = fromJust $ oaObj oa
      basePartial <- genTypeFromExpr prgm objExpr

      -- Optionally add type properties as refinement predicates
      if null typePropFunctions
        then return $ singletonType basePartial
        else do
          -- TODO: Fix support for shouldAddProps
          -- The current implementation of it doesn't actually make sense or produce valid PredExpr
          let shouldAddProps = False -- HG.bool
          if shouldAddProps
            then do
              -- Select a random subset of type properties
              numProps <- HG.int (linear 0 (min 3 $ length typePropFunctions))
              selectedProps <- take numProps <$> HG.shuffle typePropFunctions
              -- Add each as a predicate
              finalPartial <- foldM addPred basePartial selectedProps
              return $ singletonType finalPartial
            else return $ singletonType basePartial
      where
        addPred partial prop = do
          propType <- genTypeFromExpr prgm prop
          return $ partialAddPreds partial (PredsOne $ PredExpr propType)

-- | Generates a type structurally RELATED to a given one. When the input is a positive-partial
-- union, one leaf is mutated along a single dimension: its argument mode flipped among the three
-- set-exact modes, or one declared field widened to ⊤ \/ narrowed to another type. Otherwise it
-- falls back to an independent 'genType'.
--
-- Two independently generated types almost always carry different names and so are trivially
-- disjoint; the difference and intersection then short-circuit before touching the per-field
-- machinery. Drawing the second operand as a same-name single-field mutation instead forces the
-- pair through 'subtractArg'\/'subtractVar', 'looseArgModeRejects' and 'argModeCovers' — the
-- precise arg-set-interval reasoning that is otherwise never exercised.
genRelatedType :: Prgm Expr () -> Type -> Gen Type
genRelatedType prgm a = case a of
  UnionType Nothing leafs [] | not (H.null leafs) -> do
    p  <- HG.element (splitUnionType leafs)
    p' <- mutate p
    return $ completeDataPartialsTop (mkTypeEnv prgm) $ singletonType p'
  _ -> genType prgm
  where
    -- the three modes that are exact under all set operations (see 'genType.withRandomArgModes')
    safeModes = [PtArgExact, PtArgAny, PtArgMode False True]
    mutate p = HG.choice $
      [ do m <- HG.element safeModes
           return p{ptArgMode = if H.null (ptArgs p) then m{ptArgLb=True} else m}
      ] ++ [ mutateArg p | not (H.null (ptArgs p)) ]
    mutateArg p = do
      k       <- HG.element (H.keys (ptArgs p))
      newVal  <- HG.choice [return PTopType, singletonType <$> genPartialType prgm]
      return p{ptArgs = H.insert k newVal (ptArgs p)}

genPartialType :: Prgm Expr () -> Gen PartialType
genPartialType prgm@(Prgm objMap _ _) = do
  gen <- if graphEmpty cg
    then if nullObjectMap objMap
      then HG.discard
      else return [genObj, genWithTVExt genObj]
    else return [genCG, genObj, genWithTVExt genCG, genWithTVExt genObj]
  HG.choice gen
  where
    TypeEnv{teClassGraph=ClassGraph cg} = mkTypeEnv prgm
    genCG :: Gen PartialType
    genCG = do
      classNode <- HG.element $ graphToNodes cg
      return $ partialVal $ fromPartialName $ snd3 classNode

    genObj :: Gen PartialType
    genObj = do
      oa <- HG.element $ flatObjectMap objMap
      let objExpr = fromJust $ oaObj oa
      genTypeFromExpr prgm objExpr

    -- | Wraps a base partial generator and randomly adds a 'TVExt'-tagged
    -- type variable to one of its 'ptVars'.  This exercises the external
    -- variable resolution path in substitution/intersection.
    genWithTVExt :: Gen PartialType -> Gen PartialType
    genWithTVExt baseGen = do
      base <- baseGen
      varName <- HG.string (linear 1 5) HG.lower
      extName <- HG.string (linear 1 5) HG.lower
      let extVar = TypeVar (TVVar (partialKey (mkName extName))) TVExt
      return base{ptVars = H.insert (partialKey (mkName varName)) extVar (ptVars base)}

genInputExpr :: Gen (Expr ())
genInputExpr = do
  n <- makeAbsoluteName . mkName <$> genName
  varArgNames <- HG.set (linear 0 5) genName
  varOrArg <- HG.list (singleton $ S.size varArgNames) HG.bool
  let (varNames, argNames) = bimap (map fst) (map fst) $ partition snd $ zip (S.toList varArgNames) varOrArg
  let val = Value (emptyMetaT $ typeVal n) n
  withVars <- foldM genVar val (map (partialKey . mkName) varNames)
  foldM genArg withVars (map mkName argNames)
  where
    genName = HG.string (linear 5 10) HG.lower
    genVar base varName = do
      return $ TupleApply emptyMetaN (emptyMetaN, base) (EAppVar varName emptyMetaN)
    genArg base argName = do
      return $ TupleApply emptyMetaN (emptyMetaN, base) (EAppArg (ObjArr (Just (Value emptyMetaN argName)) ArgObj Nothing [] (Just (Nothing, emptyMetaN))))

-- | Generates an output expression equivalent to an input expression
genOutputExpr :: Expr () -> Expr () -> Gen (Expr ())
genOutputExpr (Value _ n) _ = return $ Value emptyMetaN n
genOutputExpr (TupleApply _ (_, b) _) input = genOutputExpr b input
-- genOutputExpr (TupleApply _ (_, b) arg@ObjArr{oaArr}) input = do
--   b' <- genOutputExpr b input
--   oaArr' <- case oaArr of
--     (Just e, _) -> do
--       e' <- genOutputExpr e input
--       return (Just e', emptyMetaN)
--     (Nothing, _) -> return (Just (HoleExpr emptyMetaN (HoleActive Nothing)), emptyMetaN)
--   return $ TupleApply emptyMetaN (emptyMetaN, b') arg{oaArr=oaArr'}
-- genOutputExpr (VarApply _ b _ _) input = genOutputExpr b input
-- genOutputExpr (VarApply _ b varName _) input = do
--   b' <- genOutputExpr b input
--   return $ VarApply emptyMetaN b' varName emptyMetaN
genOutputExpr e _ = error $ printf "Not yet implemented getOutputExpr for %s" (show e)


genPrgm :: Gen (Prgm Expr ())
genPrgm = do
  inputExprs <- HG.list (linear 1 5) (HG.either_ genInputExpr genInputExpr)
  let (dataTypes, funs) = partitionEithers inputExprs
  let dataObjs = map (\obj -> ObjArr (Just obj) TypeObj Nothing [] Nothing) dataTypes

  let allInputs = dataTypes ++ funs
  funObjs <- forM funs $ \obj -> do
                                let otherInputs = filter (/= obj) allInputs
                                if null otherInputs
                                  then return $ ObjArr (Just obj) FunctionObj Nothing [] (Just (Nothing, emptyMetaN))
                                  else do
                                    arrGoal <- HG.element otherInputs
                                    arr <- genOutputExpr arrGoal obj
                                    return $ ObjArr (Just obj) FunctionObj Nothing [] (Just (Just arr, emptyMetaN))

  let objMap = objectMapFromList (dataObjs ++ funObjs)

  return $ Prgm objMap (classGraphFromObjs objMap) []

genPrgms :: Gen [GraphNodes (Prgm Expr ()) FileImport]
genPrgms = do
  prgm <- genPrgm
  prgmName <- HG.string (linear 5 10) HG.lower
  return [(prgm, desFileImport $ mkRawFileImport $ RawCExpr emptyMetaN $ CStr prgmName, [])]

-- | Generate a random 'Val' for a concrete 'PartialType'.
-- Returns 'HG.discard' for unknown types.
--
-- A partial with fields (e.g. @Cons(head, tail)@) is generated recursively: each field's type is
-- expanded to its concrete leaves and a value generated for it via 'genValForType'. A field whose
-- type is a bare type variable (e.g. @Cons@'s polymorphic @head -> $T@) is defaulted to 'Integer',
-- since the structural derivations that consume these values (such as @length@) are
-- element-agnostic. Recursion is bounded by 'genValForType' (see there).
genVal :: (TypeGraph tg) => TypeEnv tg -> PartialType -> Gen Val
genVal typeEnv pt
  | pt == intLeaf   = IntVal <$> HG.integral (linear (-1000) 1000)
  | pt == floatLeaf = FloatVal <$> HG.double (linearFrac (-1000) 1000)
  | pt == strLeaf   = StrVal <$> HG.string (linear 0 20) HG.unicode
  | pt == charLeaf  = CharVal <$> HG.unicode
  | nameStr (ptName pt) == truePrim  = pure $ TupleVal truePrim H.empty
  | nameStr (ptName pt) == falsePrim = pure $ TupleVal falsePrim H.empty
  -- A nullary data type (no fields) has a single inhabitant: itself. This covers data-valued
  -- args such as a chosen typeclass instance (e.g. an 'Eqv' instance for a property test).
  | H.null (ptArgs pt)     = pure $ TupleVal (nameStr (ptName pt)) H.empty
  | otherwise = do
      argVals <- mapM genArg (H.toList (ptArgs pt))
      pure $ TupleVal (nameStr (ptName pt)) (H.fromList argVals)
  where
    TypeEnv{teNames} = typeEnv
    genArg (k, at) = (nameStr (pkName k),) <$> genValForType typeEnv (fieldType at)
    -- A field carrying a class/relation (e.g. @tail -> ConsList@) expands to its member data leaves;
    -- a bare type variable has no members to expand to, so default it to 'Integer'.
    fieldType TypeVar{} = intType
    fieldType at        = UnionType Nothing (expandTypeWithNames typeEnv H.empty at teNames) []

-- | Generate a random 'Val' for a 'Type'.
-- For union types, picks a random variant and delegates to 'genVal'.
--
-- Recursion through self-referential types (e.g. a @ConsList@ whose @tail@ is again a @ConsList@) is
-- bounded with 'HG.recursive': the nullary variants (e.g. @Nil@) are the non-recursive base cases
-- and the field-bearing variants (e.g. @Cons@) the recursive ones, so Hedgehog shrinks the size on
-- each recursive step and always terminates at a base case. When a union has no nullary variant
-- (e.g. an overlap domain narrowed to @Cons@ alone) every choice recurses, but each recursive field
-- still bottoms out through its own bounded 'genValForType'.
genValForType :: (TypeGraph tg) => TypeEnv tg -> Type -> Gen Val
genValForType typeEnv (UnionType Nothing leafs []) = case splitUnionType leafs of
  [] -> HG.discard
  ps -> case partition (H.null . ptArgs) ps of
    ([], recs)        -> HG.choice (map (genVal typeEnv) recs)
    (terminals, [])   -> HG.choice (map (genVal typeEnv) terminals)
    (terminals, recs) -> HG.recursive HG.choice (map (genVal typeEnv) terminals) (map (genVal typeEnv) recs)
genValForType _ _ = HG.discard
