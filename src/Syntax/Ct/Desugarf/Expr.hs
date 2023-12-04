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

import           Constants
import           Data.Char               (toLower)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm

desExpr :: PSExpr -> DesExpr
desExpr (CExpr m c) = CExpr m c
desExpr (Value m n) = Value m n
desExpr (HoleExpr m h) = HoleExpr m h
desExpr (AliasExpr b a) = AliasExpr (desExpr b) (desExpr a)
desExpr (TupleApply m (bm, be) arg) = TupleApply m (bm, desExpr be) (mapTupleArgValue desExpr arg)
desExpr (VarApply m be varName varVal) = VarApply m (desExpr be) varName varVal

desGuardExpr :: PSGuardExpr -> DesGuardExpr
desGuardExpr (GuardExpr e g) = GuardExpr (desExpr e) (fmap desExpr g)

-- | Updates the types based on the format as they are fixed for inputs (due to arrows this does not work for output expressions)
desObjPropagateTypes :: DesExpr -> (Maybe PartialType, DesExpr)
desObjPropagateTypes e | isJust (maybeGetSingleton $ getExprType e) = (maybeGetSingleton $ getExprType e, e)
desObjPropagateTypes (CExpr m c) = (Just $ constantPartialType c, CExpr (mWithType (constantType c) m) c)
desObjPropagateTypes (Value m n) = (Just t, Value (mWithType (singletonType t) m) n)
  where
    t = partialVal (PRelativeName n)
desObjPropagateTypes (HoleExpr m h) = (Nothing, HoleExpr m h)
desObjPropagateTypes (AliasExpr base alias) = (basePartial, AliasExpr base' alias')
  where
    (basePartial, base') = desObjPropagateTypes base
    (_, alias') = desObjPropagateTypes alias
desObjPropagateTypes mainExpr@(TupleApply m (bm, be) tupleApplyArgs) = do
  let (Just basePartial@PartialType{ptArgs=baseArgs}, be') = desObjPropagateTypes be
  let bm' = mWithType (getMetaType $ getExprMeta be') bm
  case tupleApplyArgs of
      ObjArr{oaObj=Just (GuardExpr argObj _), oaArr=(Just (GuardExpr argVal _), argM)} -> do
        let argName = inExprSingleton argObj
        let (_, argVal') = desObjPropagateTypes argVal
        let basePartial' = basePartial{ptArgs=H.insert argName (getExprType argVal') baseArgs}
        let m' = mWithType (singletonType basePartial') m
        (Just basePartial', TupleApply m' (bm', be') (mkIOObjArr argM argName argVal'))
      ObjArr{oaObj=Just (GuardExpr argObj _), oaArr=(Nothing, argM)} -> do
        let argName = inExprSingleton argObj
        let basePartial' = basePartial{ptArgs=H.insert argName (getMetaType argM) baseArgs}
        let m' = mWithType (singletonType basePartial') m
        (Just basePartial', TupleApply m' (bm', be') (mkIObjArr argM argName))
      _ -> error $ printf "Unexpected ObjArr in desObjPropagateTypes (probably because arrow only ObjArr): %s" (show mainExpr)
desObjPropagateTypes (VarApply m be varName varVal) = (Just basePartial', VarApply m' be' varName varVal)
  where
    (Just basePartial@PartialType{ptVars=baseVars}, be') = desObjPropagateTypes be

    basePartial' = basePartial{ptVars=H.insert varName (getMetaType varVal) baseVars}
    m' = mWithType (singletonType basePartial') m

data SemiDesMode
  = SDInput -- Used for parsing input expressions
  | SDOutput -- Used for parsing input expressions
  | SDType -- Used for parsing type expressions

semiDesExpr :: SemiDesMode -> Maybe PObjExpr -> PExpr -> PSExpr
semiDesExpr _ _ (RawCExpr m c) = CExpr m c
semiDesExpr _ _ (RawValue m n) = Value m n
semiDesExpr _ _ (RawHoleExpr m h) = HoleExpr m h
semiDesExpr _ _ RawMacroValue{} = error "Not yet implemented"
semiDesExpr _ _ (RawTheExpr t) = Value (emptyMetaM "the" tM) tN
  where
    (tM, tN) = desugarTheExpr t
semiDesExpr sdm obj (RawSpread e) = Value m' n
  where
    Value m n = semiDesExpr sdm obj e
    m' = mWithType (singletonType (fromJust $ maybeGetSingleton $ getMetaType m){ptArgMode=PtArgAny}) m
semiDesExpr sdm obj (RawAliasExpr base alias) = AliasExpr (semiDesExpr sdm obj base) (semiDesExpr sdm obj alias)
semiDesExpr sdm obj (RawTupleApply _ (_, RawValue _ "/operator::") [RawObjArr{roaArr=(Just (Just (GuardExpr e _), _))}, RawObjArr{roaArr=(Just (Just (GuardExpr tp _), _))}]) = semiDesExpr sdm obj (rawExprWithType (exprToType tp) e)
semiDesExpr sdm obj (RawTupleApply m'' (bm, be) args) = (\(_, TupleApply _ (bm'', be'') arg'') -> TupleApply m'' (bm'', be'') arg'') $ foldl aux (bm, be') (zip [0..] args)
  where
    be' = semiDesExpr sdm obj be
    aux :: (ParseMeta, PSExpr) -> (Int, PObjArr) -> (ParseMeta, PSExpr)
    aux (m, e) (argIndex, rarg@RawObjArr{roaArr=Just (Just (GuardExpr (RawTupleApply _ (_, RawValue _ "/operator::") [RawObjArr{roaArr=(Just newArr)}, RawObjArr{roaArr=(Just (Just (GuardExpr tp _), _))}]) _), _)}) = case aux (m, e) (argIndex, rarg{roaArr=Just newArr}) of
      (tam, TupleApply tamm tab taoa@ObjArr{oaArr=(tboaArrE, tboaArrM)}) -> (tam, TupleApply tamm tab taoa{oaArr=(tboaArrE, mWithType (exprToType tp) tboaArrM)})
      _ -> error $ printf "Unexpected result in semiDesExpr TupleApply aux"
    aux (m, e) (argIndex, rarg@RawObjArr{roaObj=Just (GuardExpr (RawTheExpr v) g), roaArr}) = aux (m, e) (argIndex, rarg{roaObj=Just (GuardExpr (RawValue (emptyMetaM "the" vM) vN) g), roaArr=roaArr'})
      where
        (vM, vN) = desugarTheExpr v
        roaArr' = case roaArr of
          Just (arrE, _) -> Just (arrE, vM)
          Nothing        -> Just (Nothing, vM)
    aux (m, e) (argIndex, rarg) = (emptyMetaM "res" m'', TupleApply (emptyMetaM "app" m'') (m, e) arg'')
      where
        [arg@ObjArr{oaObj=argObj, oaAnnots=argAnnots, oaArr=argArr}] = desObjArr rarg

        indexAnnots = [exprVal argStartAnnot | argIndex == 0] ++ [exprVal argEndAnnot | argIndex == length args - 1]

        -- SemiDes all sub-expressions
        arg' = arg{
          oaObj=fmap semiDesGuardExpr argObj,
          oaAnnots=indexAnnots ++ fmap (semiDesExpr sdm obj) argAnnots,
          oaArr=first (fmap semiDesGuardExpr) argArr
          }

        -- Currently uses oaObj as "first and only expr"
        -- This disambiguates it between whether the only expression is an obj or an arr
        arg'' = case (sdm, arg') of
          (SDOutput, ObjArr{oaObj, oaArr=(Nothing, _)}) -> arg'{oaObj=Nothing, oaArr=(,emptyMetaE "arrM" $ rgeExpr $ fromJust oaObj) oaObj}
          (_, _) -> arg'

        semiDesGuardExpr (GuardExpr ge gg) = GuardExpr (semiDesExpr sdm obj ge) (fmap (semiDesExpr sdm obj) gg)
semiDesExpr sdm obj (RawVarsApply m be vs) = foldr aux be' vs
  where
    be' = semiDesExpr sdm obj be
    aux (varExpr, varVal) base = VarApply (emptyMetaM (show varName) m) base varName varVal
      where varName = case fromPartialName $ ptName $ exprToPartialType varExpr of
              '$':n -> partialKey n
              n     -> partialKey n
semiDesExpr sdm obj@Just{} (RawContextApply _ (_, be) ctxs) = semiDesExpr sdm obj $ applyRawArgs (RawValue emptyMetaN "/Context") ((Just $ partialKey "value", be) : map (\(ctxName, ctxM) -> (Nothing, RawValue ctxM (pkName ctxName))) ctxs)
semiDesExpr sdm obj@Nothing (RawContextApply _ (_, be) ctxs) = semiDesExpr sdm obj $ applyRawIArgs (RawValue emptyMetaN "/Context") ((partialKey "value", IArgE be) : map (second IArgM) ctxs)
semiDesExpr sdm obj (RawParen e) = semiDesExpr sdm obj e
semiDesExpr sdm obj@Nothing (RawMethod (RawTheExpr (RawValue m n)) method) = semiDesExpr sdm obj (method `applyRawIArgs` [(partialKey "this", IArgM (Meta (typeVal $ PRelativeName n) (getMetaPos m) emptyMetaDat))]) -- Parse type methods like :Integer.toString, Only for input expressions
semiDesExpr sdm obj (RawMethod base method) = semiDesExpr sdm obj $ applyRawArgs method [(Just $ partialKey "this", base)]
semiDesExpr sdm obj (RawList m []) = semiDesExpr sdm obj (RawValue m "/Data/Nil")
semiDesExpr sdm obj (RawList m (l:ls)) = semiDesExpr sdm obj (RawValue m "/Data/Cons" `applyRawArgs` [(Just $ partialKey "head", l), (Just $ partialKey "tail", RawList m ls)])
semiDesExpr _ _ e = error $ printf "Not yet implemented semiDesExpr for %s" (show e)

-- | Desugars a "TheExpr type" by applying the type to a default name
desugarTheExpr :: PExpr -> (ParseMeta, Name)
desugarTheExpr t = (mWithType (singletonType t') (getExprMeta t), defaultName)
  where
    t' = exprToPartialType t
    defaultName = lowerCaseFirstLetter $ fromPartialName $ ptName t'
    lowerCaseFirstLetter (n:ns) = toLower n : ns
    lowerCaseFirstLetter ns     = ns

exprToPartialType :: PExpr -> PartialType
exprToPartialType = fromJust . fst . desObjPropagateTypes . desExpr . semiDesExpr SDType Nothing

exprToType :: PExpr -> Type
exprToType t@(RawValue _ n) = case parseTVVar n of
      Just t' -> t'
      Nothing -> (singletonType . exprToPartialType) t
exprToType t                        = (singletonType . exprToPartialType) t

exprToTypeMeta :: PExpr -> ParseMeta
exprToTypeMeta e = mWithType (exprToType e) (getExprMeta e)
