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


import           Data.Bifunctor          (Bifunctor (first), second)
import qualified Data.HashMap.Strict     as H
import           Data.Maybe
import           Text.Printf

import           CtConstants
import           Data.Char               (toLower)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Builder
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm

desExpr :: PSExpr -> DesExpr
desExpr (CExpr m c) = CExpr m c
desExpr (Value m n) = Value m n
desExpr (HoleExpr m h) = HoleExpr m h
desExpr (AliasExpr b a) = AliasExpr (desExpr b) (desExpr a)
desExpr (EWhere m b a) = EWhere m (desExpr b) (desExpr a)
desExpr (TupleApply m (bm, be) arg) = TupleApply m (bm, desExpr be) arg'
  where
    arg' = case arg of
      EAppArg a    -> EAppArg $ mapTupleArgValue desExpr a
      a@EAppVar{}  -> a
      EAppSpread a -> EAppSpread $ desExpr a

data SemiDesMode
  = SDInput Bool -- Used for parsing input expressions and indicates it is in a method
  | SDOutput -- Used for parsing input expressions
  | SDType -- Used for parsing type expressions

mobjExprVaenv :: Maybe PObjExpr -> PVarArgMap
mobjExprVaenv = maybe H.empty exprVarArgs

semiDesExpr :: SemiDesMode -> Maybe PObjExpr -> PExpr -> PSExpr
semiDesExpr _ _ (RawCExpr m c) = CExpr m c
semiDesExpr _ _ (RawValue m n) = Value m n
semiDesExpr _ _ (RawHoleExpr m h) = HoleExpr m h
semiDesExpr _ _ RawMacroValue{} = error "Not yet implemented"
semiDesExpr sdm@(SDInput True) obj (RawTheExpr t) = semiDesExpr sdm obj t
semiDesExpr SDInput{} _ e@(RawTheExpr t) = Value (emptyMetaE e) tN
  where
    (_tM, tN) = desugarTheExpr t
semiDesExpr sdm obj (RawTupleApply _ (_, e@RawValue{}) [(True, _)]) = e' `setExprMeta` m'
  where
    e' = semiDesExpr sdm obj e
    m = getExprMeta e'
    m' = mWithType (spreadType H.empty $ getMetaType m) m
semiDesExpr sdm obj (RawAliasExpr base alias) = AliasExpr (semiDesExpr sdm obj base) (semiDesExpr sdm obj alias)
semiDesExpr sdm obj (RawWhere m base cond) = EWhere m (semiDesExpr sdm obj base) (semiDesExpr SDOutput obj cond)
semiDesExpr sdm obj (RawTupleApply _ (_, RawValue _ "/operator::") [(False, RawObjArr{roaArr=(Just (Just e, _))}), (False, RawObjArr{roaArr=(Just (Just tp, _))})]) = semiDesExpr sdm obj (rawExprWithType (exprToType (mobjExprVaenv obj) tp) e)
semiDesExpr sdm obj (RawTupleApply _ (_, be) []) = semiDesExpr sdm obj be
semiDesExpr sdm obj (RawTupleApply m'' (bm, be) args) = (`setExprMeta` m'') $ snd $ foldl aux (bm, be') (zip [0..] args)
  where
    be' = semiDesExpr sdm obj be
    aux :: (ParseMeta, PSExpr) -> (Int, (Bool, PObjArr)) -> (ParseMeta, PSExpr)
    aux (m, e) (argIndex, (False, rarg@RawObjArr{roaArr=Just (Just (RawTupleApply _ (_, RawValue _ "/operator::") [(False, RawObjArr{roaArr=(Just newArr)}), (False, RawObjArr{roaArr=(Just (Just tp, _))})]), _)})) = case aux (m, e) (argIndex, (False, rarg{roaArr=Just newArr})) of
      (tam, TupleApply tamm tab (EAppArg taoa@ObjArr{oaArr=Just (tboaArrE, tboaArrM)})) -> (tam, TupleApply tamm tab (EAppArg taoa{oaArr=Just (tboaArrE, mWithType (exprToType (mobjExprVaenv obj) tp) tboaArrM)}))
      _ -> error $ printf "Unexpected result in semiDesExpr TupleApply aux"
    aux (m, e) (argIndex, (False, rarg@RawObjArr{roaObj=Just (RawTheExpr v), roaArr})) = aux (m, e) (argIndex, (False, rarg{roaObj=Just (RawValue (emptyMetaE vM) vN), roaArr=roaArr'}))
      where
        (vM, vN) = desugarTheExpr v
        roaArr' = case roaArr of
          Just (arrE, _) -> Just (arrE, Just vM)
          Nothing        -> Just (Nothing, Just vM)
    aux (m, e) (_argIndex, (False, rarg)) = (emptyMetaM m'', TupleApply (emptyMetaM m'') (m, e) (EAppArg arg''))
      where
        arg = head $ desObjArr obj rarg

        -- indexAnnots = [exprVal argStartAnnot | argIndex == 0] ++ [exprVal argEndAnnot | argIndex == length args - 1]
        indexAnnots = [] -- TODO: Consider whether to re-add start and end annots

        -- Currently uses oaObj as "first and only expr"
        -- This disambiguates it between whether the only expression is an obj or an arr
        arg' = case (sdm, arg) of
          (SDOutput, ObjArr{oaObj, oaArr=Just (Nothing, _)}) -> arg{oaObj=Nothing, oaArr=Just (oaObj, emptyMetaE $ fromJust oaObj)}
          (_, _) -> arg

        -- SemiDes all sub-expressions
        objSdm = case sdm of
          SDInput{} -> sdm
          _         -> SDInput False
        arg'' = arg'{
          oaObj=fmap (semiDesExpr objSdm obj) (oaObj arg'),
          oaAnnots=indexAnnots ++ fmap (semiDesExpr sdm obj) (oaAnnots arg'),
          oaArr=fmap (first (fmap (semiDesExpr sdm (oaObj arg')))) (oaArr arg')
          }
    aux (m, e) (_, (True, oa)) = (emptyMetaM m'', TupleApply (emptyMetaM m'') (m, e) (EAppSpread $ semiDesExpr sdm obj $ oaObjExpr arg))
      where
        arg = head $ desObjArr obj oa
semiDesExpr sdm obj (RawVarsApply m be vs) = (`setExprMeta` m) $ foldr aux be' vs
  where
    be' = semiDesExpr sdm obj be
    aux RawObjArr{roaObj=Just varExpr, roaArr} base = TupleApply (emptyMetaM m) (emptyMetaE base, base) (EAppVar varName varVal)
      where
        varName = case maybeExprPath varExpr of
              Just ('$':n) -> partialKey n
              Just n     -> partialKey n
              Nothing -> error $ printf "No type name found in varExpr %s (type %s)" (show varExpr) (show $ exprToType (mobjExprVaenv obj) varExpr)
        varVal = maybe emptyMetaN (exprToTypeMeta (mobjExprVaenv obj)) (roaArr >>= snd)
    aux roa _ = error $ printf "Unexpected semiDesExpr var: %s" (show roa)
semiDesExpr sdm@SDOutput{} obj (RawContextApply m (_, be) ctxs) = (`setExprMeta` m) $ semiDesExpr sdm obj $ applyRawEArgs (RawValue emptyMetaN contextOutStr) ((Just $ rawVal contextValStr, be) : map mapCtx ctxs)
  where
    mapCtx ctx = (Just $ rawVal $ snd $ desugarTheExpr $ fromJust $ roaObj ctx, fromJust $ roaObj ctx)
semiDesExpr sdm@SDInput{} obj (RawContextApply m (_, be) ctxs) = (`setExprMeta` m) $ semiDesExpr sdm obj $ applyRawIArgs (RawValue emptyMetaN ContextInStr) ((partialKey contextValStr, IArgE be) : map mapCtx ctxs)
  where
    mapCtx RawObjArr{roaObj=Just ctxObj, roaArr=Just (Just ctxArr, _)} = (partialToKey $ exprToPartialType ctxObj, IArgE ctxArr)
    mapCtx RawObjArr{roaObj=Just ctxObj, roaArr=Just (_, ctxM)} = (partialToKey $ exprToPartialType ctxObj, IArgM ctxM)
    mapCtx ctx = error $ printf "Invalid input context: %s" (show ctx)
semiDesExpr sdm obj (RawParen e) = semiDesExpr sdm obj e
semiDesExpr sdm obj (RawMethod m (RawTheExpr n) method) = (`setExprMeta` m) $ semiDesExpr sdm' obj (method `applyRawIArgs` [(partialKey "this", IArgM (Just n))]) -- Parse type methods like :Integer.toString, Only for input expressions
  where
    sdm' = case sdm of
      SDInput _ -> SDInput True
      _         -> sdm
semiDesExpr sdm obj (RawMethod m base method) = (`setExprMeta` m) $ semiDesExpr sdm' obj $ applyRawArgs method [(Just $ partialKey "this", base)]
  where
    sdm' = case sdm of
      SDInput _ -> SDInput True
      _         -> sdm
semiDesExpr sdm obj (RawList m []) = semiDesExpr sdm obj (RawValue m "/Data/Nil")
semiDesExpr sdm obj (RawList m (l:ls)) = semiDesExpr sdm obj (RawValue m "/Data/Cons" `applyRawArgs` [(Just $ partialKey "head", l), (Just $ partialKey "tail", RawList (emptyMetaM m) ls)])
semiDesExpr _ _ e = error $ printf "Not yet implemented semiDesExpr for %s" (show e)

-- | Desugars a "TheExpr type" by applying the type to a default name
desugarTheExpr :: PExpr -> (PExpr, Name)
desugarTheExpr t = (t, defaultName)
  where
    t' = exprToType H.empty t
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
    des = exprPropagateTypes $ desExpr $ semiDesExpr SDType Nothing e

exprToType :: PVarArgMap -> PExpr -> Type
exprToType _ (RawValue _ n) | isJust (parseTVVar n) = fromJust $ parseTVVar n
-- TODO Migrate the TypeVar check to the one below and remove the one above. This is for having type variables without the $ prefix.
-- exprToType vaenv (RawValue _ n) | H.member (TVVar $ partialKey $ makeAbsoluteName n) vaenv  = TypeVar (TVVar $ partialKey n) TVInt
exprToType _ e                        = getExprType $ exprPropagateTypes $ desExpr $ semiDesExpr SDType Nothing e

exprToTypeMeta :: PVarArgMap -> PExpr -> ParseMeta
exprToTypeMeta vaenv e = mWithType (exprToType vaenv e) (getExprMeta e)

desFileImport :: RawFileImport -> FileImport
desFileImport rawImp = rawImp{
    impRaw=semiDesExpr SDOutput Nothing $ impRaw rawImp,
    impAbs=semiDesExpr SDOutput Nothing $ impAbs rawImp
  }

desObjArr :: Maybe PObjExpr -> PObjArr -> [ObjArr RawExpr ParseMetaDat]
desObjArr mainObj (RawObjArr obj basis@TypeObj doc annots arr Nothing) = [ObjArr obj basis doc annots arr']
  where
    arr' = fmap (second (maybe emptyMetaN (exprToTypeMeta (mobjExprVaenv mainObj)))) arr
desObjArr mainObj (RawObjArr obj@(Just objExpr) basis doc annots arr Nothing) = [ObjArr obj basis doc annots (Just arr')]
  where
    arr' = maybe (Nothing, emptyMetaE objExpr) (second (maybe emptyMetaN (exprToTypeMeta (mobjExprVaenv mainObj)))) arr
desObjArr mainObj (RawObjArr obj basis doc annots (Just (arrE, arrM)) Nothing) = [ObjArr obj basis doc annots (Just (arrE, maybe emptyMetaN (exprToTypeMeta (mobjExprVaenv mainObj)) arrM))]
desObjArr _ roa = error $ printf "Not yet implemented: %s" (show roa)
