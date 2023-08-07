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


import           Data.Bifunctor          (second)
import qualified Data.HashMap.Strict     as H
import           Data.Maybe
import           Text.Printf

import           Data.Char               (toLower)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm

desExpr :: PArgMetaMap -> PSExpr -> DesExpr
desExpr _ (CExpr m c) = CExpr m c
desExpr arrArgs (Value m n) = if H.member n arrArgs
  then Arg m n
  else Value m n
desExpr _ Arg{} = error "Only values should be used at this point as it has not yet been disambiguated between Value and Arg"
desExpr _ (HoleExpr m h) = HoleExpr m h
desExpr arrArgs (AliasExpr b a) = AliasExpr (desExpr arrArgs b) (desExpr arrArgs a)
desExpr arrArgs (TupleApply m (bm, be) arg) = TupleApply m (bm, desExpr arrArgs be) (mapTupleArgValue (desExpr arrArgs) arg)
desExpr arrArgs (VarApply m be varName varVal) = VarApply m (desExpr arrArgs be) varName varVal

desGuardExpr :: PArgMetaMap -> PSGuardExpr -> DesGuardExpr
desGuardExpr arrArgs (GuardExpr e g) = GuardExpr (desExpr arrArgs e) (fmap (desExpr arrArgs) g)

-- | Updates the types based on the format as they are fixed for inputs (due to arrows this does not work for output expressions)
desObjPropagateTypes :: DesExpr -> (Maybe PartialType, DesExpr)
desObjPropagateTypes (CExpr m c) = (Just $ constantPartialType c, CExpr (mWithType (constantType c) m) c)
desObjPropagateTypes (Value m n) = (Just t, Value (mWithType (singletonType t) m) n)
  where
    t = partialVal (PRelativeName n)
desObjPropagateTypes (Arg m n) = (Just t, Arg (mWithType (singletonType t) m) n)
  where t = partialVal (PRelativeName n)
desObjPropagateTypes (HoleExpr m h) = (Nothing, HoleExpr m h)
desObjPropagateTypes (AliasExpr base alias) = (basePartial, AliasExpr base' alias')
  where
    (basePartial, base') = desObjPropagateTypes base
    (_, alias') = desObjPropagateTypes alias
desObjPropagateTypes mainExpr@(TupleApply m (bm, be) tupleApplyArgs) = do
  let (Just basePartial@PartialType{ptArgs=baseArgs}, be') = desObjPropagateTypes be
  let bm' = mWithType (getMetaType $ getExprMeta be') bm
  case tupleApplyArgs of
      ObjArr{oaObj=Just (GuardExpr argObj _), oaArr=Just (GuardExpr argVal _)} -> do
        let (argName, argM) = exprPathM argObj
        let (_, argVal') = desObjPropagateTypes argVal
        let argM' = mWithType (getMetaType $ getExprMeta argVal) argM
        let basePartial' = basePartial{ptArgs=H.insert argName (getExprType argVal') baseArgs}
        let m' = mWithType (singletonType basePartial') m
        (Just basePartial', TupleApply m' (bm', be') (mkIOObjArr argM' argName argVal'))
      ObjArr{oaObj=Just (GuardExpr argObj _), oaArr=Nothing} -> do
        let (argName, argM) = exprPathM argObj
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
semiDesExpr sdm obj (RawTheExpr t) = semiDesExpr sdm obj $ desugarTheExpr t
semiDesExpr sdm obj (RawAliasExpr base alias) = AliasExpr (semiDesExpr sdm obj base) (semiDesExpr sdm obj alias)
semiDesExpr sdm obj (RawTupleApply _ (_, RawValue _ "/operator:") [ObjArr{oaArr=(Just (GuardExpr e _))}, ObjArr{oaArr=(Just (GuardExpr tp _))}]) = semiDesExpr sdm obj (rawExprWithType (exprToType tp) e)
semiDesExpr sdm obj (RawTupleApply m'' (bm, be) args) = (\(_, TupleApply _ (bm'', be'') arg'') -> TupleApply m'' (bm'', be'') arg'') $ foldl aux (bm, be') args
  where
    be' = semiDesExpr sdm obj be
    aux (m, e) arg@ObjArr{oaObj=argObj, oaAnnots=argAnnots, oaArr=argArr} = (emptyMetaM "res" m'', TupleApply (emptyMetaM "app" m'') (m, e) arg'')
      where
        -- SemiDes all sub-expressions
        arg' = arg{
          oaObj=fmap semiDesGuardExpr argObj,
          oaAnnots=fmap (semiDesExpr sdm obj) argAnnots,
          oaArr=fmap semiDesGuardExpr argArr
          }

        -- Currently uses oaObj as "first and only expr"
        -- This disambiguates it between whether the only expression is an obj or an arr
        arg'' = case (sdm, arg') of
          (SDOutput, ObjArr{oaObj, oaArr=Nothing}) -> arg'{oaObj=Nothing, oaArr=oaObj}
          (_, _) -> arg'

        semiDesGuardExpr (GuardExpr ge gg) = GuardExpr (semiDesExpr sdm obj ge) (fmap (semiDesExpr sdm obj) gg)
semiDesExpr sdm obj (RawVarsApply m be vs) = foldr aux be' vs
  where
    be' = semiDesExpr sdm obj be
    aux (varExpr, varVal) base = VarApply (emptyMetaM varName m) base varName varVal
      where varName = case fromPartialName $ ptName $ exprToPartialType varExpr of
              '$':n -> n
              n     -> n
semiDesExpr sdm obj@Just{} (RawContextApply _ (_, be) ctxs) = semiDesExpr sdm obj $ applyRawArgs (RawValue emptyMetaN "/Context") ((Just "value", be) : map (\(ctxName, ctxM) -> (Nothing, RawValue ctxM ctxName)) ctxs)
semiDesExpr sdm obj@Nothing (RawContextApply _ (_, be) ctxs) = semiDesExpr sdm obj $ applyRawIArgs (RawValue emptyMetaN "/Context") (("value", IArgE be) : map (second IArgM) ctxs)
semiDesExpr sdm obj (RawParen e) = semiDesExpr sdm obj e
semiDesExpr sdm obj@Nothing (RawMethod (RawTheExpr (RawValue m n)) method) = semiDesExpr sdm obj (method `applyRawIArgs` [("this", IArgM (Meta (singletonType $ partialVal $ PRelativeName n) (getMetaPos m) emptyMetaDat))]) -- Parse type methods like :Integer.toString, Only for input expressions
semiDesExpr sdm obj (RawMethod base method) = semiDesExpr sdm obj $ applyRawArgs method [(Just "this", base)]
semiDesExpr sdm obj (RawList m []) = semiDesExpr sdm obj (RawValue m "/Data/Nil")
semiDesExpr sdm obj (RawList m (l:ls)) = semiDesExpr sdm obj (RawValue m "/Data/Cons" `applyRawArgs` [(Just "head", l), (Just "tail", RawList m ls)])

-- | Desugars a "TheExpr type" by applying the type to a default name
desugarTheExpr :: PExpr -> PExpr
desugarTheExpr t = RawValue (mWithType (singletonType t') (getExprMeta t)) defaultName
  where
    t' = exprToPartialType t
    defaultName = lowerCaseFirstLetter $ fromPartialName $ ptName t'
    lowerCaseFirstLetter (n:ns) = toLower n : ns
    lowerCaseFirstLetter ns     = ns

exprToPartialType :: PExpr -> PartialType
exprToPartialType = fromJust . fst . desObjPropagateTypes . desExpr H.empty . semiDesExpr SDType Nothing

exprToType :: PExpr -> Type
exprToType t@(RawValue _ n) = case parseTVVar n of
      Just t' -> t'
      Nothing -> (singletonType . exprToPartialType) t
exprToType t                        = (singletonType . exprToPartialType) t

exprToTypeMeta :: PExpr -> ParseMeta
exprToTypeMeta e = mWithType (exprToType e) (getExprMeta e)
