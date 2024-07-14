--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Desugarf.Expr
-- Copyright :  (c) Zach Kimberg 2022
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module desugars a 'RawPrgm' into a 'Prgm' by removing all
-- language features outside of a minimalist core set.
--------------------------------------------------------------------

module Syntax.Ct.Desugarf.Expr where


import           Data.Bifunctor          (Bifunctor (first))
import qualified Data.HashMap.Strict     as H
import           Data.Maybe
import           Text.Printf

import           Data.Char               (toLower)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Builder
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Utils

desExpr :: PSExpr -> DesExpr
desExpr (CExpr m c) = CExpr m c
desExpr (Value m n) = Value m n
desExpr (HoleExpr m h) = HoleExpr m h
desExpr (AliasExpr b a) = AliasExpr (desExpr b) (desExpr a)
desExpr (EWhere b a) = EWhere (desExpr b) (desExpr a)
desExpr (TupleApply m (bm, be) arg) = TupleApply m (bm, desExpr be) (mapTupleArgValue desExpr arg)
desExpr (VarApply m be varName varVal) = VarApply m (desExpr be) varName varVal

-- | Updates the types based on the format as they are fixed for inputs (due to arrows this does not work for output expressions)
desObjPropagateTypes :: DesExpr -> DesExpr
desObjPropagateTypes (CExpr m c) = CExpr (mWithType (constantType c) m) c
desObjPropagateTypes (Value m n) | getMetaType m == PTopType = Value (mWithType t m) n
  where
    t = relTypeVal n
desObjPropagateTypes (Value m n) = Value m n
desObjPropagateTypes (HoleExpr m h) = HoleExpr m h
desObjPropagateTypes (AliasExpr base alias) = AliasExpr base' alias'
  where
    base' = desObjPropagateTypes base
    alias' = desObjPropagateTypes alias
desObjPropagateTypes (EWhere base cond) = EWhere base cond
desObjPropagateTypes mainExpr@(TupleApply m (bm, be) tupleApplyArgs) = do
  let be' = desObjPropagateTypes be
  let bm' = mWithType (getMetaType $ getExprMeta be') bm
  case tupleApplyArgs of
      arg@ObjArr{oaObj=Just argObj, oaArr=Just (Just argVal, argM)} -> do
        let argName = inExprSingleton argObj
        let argVal' = desObjPropagateTypes argVal
        let tp' = typeSetArg argName (getExprType argVal') (getExprType be')
        let m' = mWithType tp' m
        TupleApply m' (bm', be') arg{oaArr=Just (Just argVal', argM)}
      ObjArr{oaObj=Just argObj, oaArr=Just (Nothing, argM)} -> do
        let argName = inExprSingleton argObj
        let tp' = typeSetArg argName (getMetaType argM) (getExprType be')
        let m' = mWithType tp' m
        TupleApply m' (bm', be') (mkIObjArr argM argName)
      _ -> error $ printf "Unexpected ObjArr in desObjPropagateTypes (probably because arrow only ObjArr): %s" (show mainExpr)
desObjPropagateTypes (VarApply m be varName varVal) = VarApply m' be' varName varVal
  where
    be' = desObjPropagateTypes be

    tp' = typeSetVar varName (getMetaType varVal) (getExprType be')
    m' = mWithType tp' m

data SemiDesMode
  = SDInput Bool -- Used for parsing input expressions and indicates it is in a method
  | SDOutput -- Used for parsing input expressions
  | SDType -- Used for parsing type expressions

semiDesExpr :: SemiDesMode -> Maybe PObjExpr -> PExpr -> PSExpr
semiDesExpr _ _ (RawCExpr m c) = CExpr m c
semiDesExpr _ _ (RawValue m n) = Value m n
semiDesExpr _ _ (RawHoleExpr m h) = HoleExpr m h
semiDesExpr _ _ RawMacroValue{} = error "Not yet implemented"
semiDesExpr sdm@(SDInput True) obj (RawTheExpr t) = semiDesExpr sdm obj t
semiDesExpr SDInput{} _ (RawTheExpr t) = Value (emptyMetaM "the" tM) tN
  where
    (tM, tN) = desugarTheExpr t
semiDesExpr sdm obj (RawSpread e) = Value m' n
  where
    Value m n = semiDesExpr sdm obj e
    m' = mWithType (spreadType H.empty $ getMetaType m) m
semiDesExpr sdm obj (RawAliasExpr base alias) = AliasExpr (semiDesExpr sdm obj base) (semiDesExpr sdm obj alias)
semiDesExpr sdm obj (RawWhere base cond) = EWhere (semiDesExpr sdm obj base) (semiDesExpr sdm obj cond)
semiDesExpr sdm obj (RawTupleApply _ (_, RawValue _ "/operator::") [RawObjArr{roaArr=(Just (Just e, _, _))}, RawObjArr{roaArr=(Just (Just tp, _, _))}]) = semiDesExpr sdm obj (rawExprWithType (exprToType tp) e)
semiDesExpr sdm obj (RawTupleApply _ (_, be) []) = semiDesExpr sdm obj be
semiDesExpr sdm obj (RawTupleApply m'' (bm, be) args) = (\(_, TupleApply _ (bm'', be'') arg'') -> TupleApply m'' (bm'', be'') arg'') $ foldl aux (bm, be') (zip [0..] args)
  where
    be' = semiDesExpr sdm obj be
    aux :: (ParseMeta, PSExpr) -> (Int, PObjArr) -> (ParseMeta, PSExpr)
    aux (m, e) (argIndex, rarg@RawObjArr{roaArr=Just (Just (RawTupleApply _ (_, RawValue _ "/operator::") [RawObjArr{roaArr=(Just newArr)}, RawObjArr{roaArr=(Just (Just tp, _, _))}]), _, _)}) = case aux (m, e) (argIndex, rarg{roaArr=Just newArr}) of
      (tam, TupleApply tamm tab taoa@ObjArr{oaArr=Just (tboaArrE, tboaArrM)}) -> (tam, TupleApply tamm tab taoa{oaArr=Just (tboaArrE, mWithType (exprToType tp) tboaArrM)})
      _ -> error $ printf "Unexpected result in semiDesExpr TupleApply aux"
    aux (m, e) (argIndex, rarg@RawObjArr{roaObj=Just (RawTheExpr v), roaArr}) = aux (m, e) (argIndex, rarg{roaObj=Just (RawValue (emptyMetaM "the" vM) vN), roaArr=roaArr'})
      where
        (vM, vN) = desugarTheExpr v
        roaArr' = case roaArr of
          Just (arrE, _, _) -> Just (arrE, Nothing, vM)
          Nothing           -> Just (Nothing, Nothing, vM)
    aux (m, e) (_argIndex, rarg) = (emptyMetaM "res" m'', TupleApply (emptyMetaM "app" m'') (m, e) arg'')
      where
        [arg] = desObjArr rarg

        -- indexAnnots = [exprVal argStartAnnot | argIndex == 0] ++ [exprVal argEndAnnot | argIndex == length args - 1]
        indexAnnots = [] -- TODO: Consider whether to re-add start and end annots

        -- Currently uses oaObj as "first and only expr"
        -- This disambiguates it between whether the only expression is an obj or an arr
        arg' = case (sdm, arg) of
          (SDOutput, ObjArr{oaObj, oaArr=Just (Nothing, _)}) -> arg{oaObj=Nothing, oaArr=Just ((,emptyMetaE "arrM" $ fromJust oaObj) oaObj)}
          (_, _) -> arg

        -- SemiDes all sub-expressions
        objSdm = case sdm of
          SDInput{} -> sdm
          _         -> SDInput False
        arg'' = arg'{
          oaObj=fmap (semiDesExpr objSdm Nothing) (oaObj arg'),
          oaAnnots=indexAnnots ++ fmap (semiDesExpr sdm obj) (oaAnnots arg'),
          oaArr=fmap (first (fmap (semiDesExpr sdm (oaObj arg')))) (oaArr arg')
          }
semiDesExpr sdm obj (RawVarsApply m be vs) = foldr aux be' vs
  where
    be' = semiDesExpr sdm obj be
    aux RawObjArr{roaObj=Just varExpr, roaArr} base = VarApply (emptyMetaM (show varName) m) base varName varVal
      where
        varName = case maybeExprPath varExpr of
              Just ('$':n) -> partialKey n
              Just n     -> partialKey n
              Nothing -> error $ printf "No type name found in varExpr %s (type %s)" (show varExpr) (show $ exprToType varExpr)
        varVal = maybe emptyMetaN thr3 roaArr
    aux roa _ = error $ printf "Unexpected semiDesExpr var: %s" (show roa)
semiDesExpr sdm obj@Just{} (RawContextApply _ (_, be) ctxs) = semiDesExpr sdm obj $ applyRawEArgs (RawValue emptyMetaN "/Context") ((Just $ rawVal "value", be) : map mapCtx ctxs)
  where
    mapCtx ctx = (Just $ rawVal $ snd $ desugarTheExpr $ fromJust $ roaObj ctx, fromJust $ roaObj ctx)
semiDesExpr sdm obj@Nothing (RawContextApply _ (_, be) ctxs) = semiDesExpr sdm obj $ applyRawIArgs (RawValue emptyMetaN "/Context") ((partialKey "value", IArgE be) : map mapCtx ctxs)
  where
    mapCtx ctx = (partialToKey $ exprToPartialType $ fromJust $ roaObj ctx, IArgM $ thr3 $ fromJust $ roaArr ctx)
semiDesExpr sdm obj (RawParen e) = semiDesExpr sdm obj e
semiDesExpr sdm obj@Nothing (RawMethod (RawTheExpr n) method) = semiDesExpr sdm' obj (method `applyRawIArgs` [(partialKey "this", IArgM (Meta (exprToType n) (getMetaPos $ getExprMeta n) emptyMetaDat))]) -- Parse type methods like :Integer.toString, Only for input expressions
  where
    sdm' = case sdm of
      SDInput _ -> SDInput True
      _         -> sdm
semiDesExpr sdm obj (RawMethod base method) = semiDesExpr sdm' obj $ applyRawArgs method [(Just $ partialKey "this", base)]
  where
    sdm' = case sdm of
      SDInput _ -> SDInput True
      _         -> sdm
semiDesExpr sdm obj (RawList m []) = semiDesExpr sdm obj (RawValue m "/Data/Nil")
semiDesExpr sdm obj (RawList m (l:ls)) = semiDesExpr sdm obj (RawValue m "/Data/Cons" `applyRawArgs` [(Just $ partialKey "head", l), (Just $ partialKey "tail", RawList m ls)])
semiDesExpr _ _ e = error $ printf "Not yet implemented semiDesExpr for %s" (show e)

-- | Desugars a "TheExpr type" by applying the type to a default name
desugarTheExpr :: PExpr -> (ParseMeta, Name)
desugarTheExpr t = (mWithType t' (getExprMeta t), defaultName)
  where
    t' = exprToType t
    n' = fromMaybe (error $ printf "desugarTheExpr can't get name from %s" (show t')) $ maybeGetTypeName t'
    defaultName = lowerCaseFirstLetter n'
    lowerCaseFirstLetter (n:ns) = toLower n : ns
    lowerCaseFirstLetter ns     = ns

exprToPartialType :: PExpr -> PartialType
exprToPartialType e = case maybeGetSingleton $ getExprType des of
    Just t' -> t'
    Nothing -> case getExprType des of
      TopType _ (PredsOne (PredRel n)) -> n
      t' -> error $ printf "Failed exprToPartialType with type %s in expr %s" (show t') (show des)
  where
    des = desObjPropagateTypes $ desExpr $ semiDesExpr SDType Nothing e

exprToType :: PExpr -> Type
exprToType (RawValue _ n) | isJust (parseTVVar n) = fromJust $ parseTVVar n
exprToType e                        = getExprType $ desObjPropagateTypes $ desExpr $ semiDesExpr SDType Nothing e

exprToTypeMeta :: PExpr -> ParseMeta
exprToTypeMeta e = mWithType (exprToType e) (getExprMeta e)
