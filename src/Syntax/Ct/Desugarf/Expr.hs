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
      TupleArgI argM argName -> do
        let basePartial' = basePartial{ptArgs=H.insert argName (getMetaType argM) baseArgs}
        let m' = mWithType (singletonType basePartial') m
        (Just basePartial', TupleApply m' (bm', be') (TupleArgI argM argName))
      TupleArgIO argM argName argVal -> do
        let (_, argVal') = desObjPropagateTypes argVal
        let argM' = mWithType (getMetaType $ getExprMeta argVal) argM
        let basePartial' = basePartial{ptArgs=H.insert argName (getExprType argVal') baseArgs}
        let m' = mWithType (singletonType basePartial') m
        (Just basePartial', TupleApply m' (bm', be') (TupleArgIO argM' argName argVal'))
      TupleArgO{} -> error $ printf "Unexpected TupleArgO in desObjPropagateTypes: %s" (show mainExpr)
desObjPropagateTypes (VarApply m be varName varVal) = (Just basePartial', VarApply m' be' varName varVal)
  where
    (Just basePartial@PartialType{ptVars=baseVars}, be') = desObjPropagateTypes be

    basePartial' = basePartial{ptVars=H.insert varName (getMetaType varVal) baseVars}
    m' = mWithType (singletonType basePartial') m


semiDesExpr :: Maybe PObjExpr -> PExpr -> PSExpr
semiDesExpr _ (RawCExpr m c) = CExpr m c
semiDesExpr _ (RawValue m n) = Value m n
semiDesExpr _ (RawHoleExpr m h) = HoleExpr m h
semiDesExpr obj (RawTheExpr t) = semiDesExpr obj $ desugarTheExpr t
semiDesExpr obj (RawAliasExpr base alias) = AliasExpr (semiDesExpr obj base) (semiDesExpr obj alias)
semiDesExpr obj (RawTupleApply _ (_, RawValue _ "/operator:") [RawObjArr{roaArr=(Just (RawGuardExpr e _))}, RawObjArr{roaArr=(Just (RawGuardExpr tp _))}]) = semiDesExpr obj (rawExprWithType (exprToType tp) e)
semiDesExpr obj (RawTupleApply m'' (bm, be) args) = (\(_, TupleApply _ (bm'', be'') arg'') -> TupleApply m'' (bm'', be'') arg'') $ foldl aux (bm, be') args
  where
    be' = semiDesExpr obj be
    aux (m, e) RawObjArr{roaObj=(Just (RawGuardExpr argInExpr Nothing)), roaM=argM, roaArr=(Just (RawGuardExpr argVal Nothing))} = (emptyMetaM "res" m'', TupleApply (emptyMetaM "app" m'') (m, e) (TupleArgIO argM argName' argVal'))
      where
        argVal' = semiDesExpr obj argVal
        (Value _ argName') = semiDesExpr obj argInExpr
    aux (m, e) RawObjArr{roaObj=Nothing, roaM=argM, roaArr=(Just (RawGuardExpr argVal Nothing))} = (emptyMetaM "res" m'', TupleApply (emptyMetaM "app" m'') (m, e) (TupleArgO argM argVal'))
      where argVal' = semiDesExpr obj argVal
    aux (m, e) RawObjArr{roaObj=(Just (RawGuardExpr argInExpr Nothing)), roaArr=Nothing} = (emptyMetaM "res" m'', TupleApply (emptyMetaM "app" m'') (m, e) (TupleArgI argM' argName'))
      where
        (Value argM' argName') = semiDesExpr obj argInExpr
    aux _ oa = error $ printf "Could not semiDesExpr with unsupported term %s" (show oa)
semiDesExpr obj (RawVarsApply m be vs) = foldr aux be' vs
  where
    be' = semiDesExpr obj be
    aux (varName, varVal) base = VarApply (emptyMetaM varName m) base varName varVal
semiDesExpr obj@Just{} (RawContextApply _ (_, be) ctxs) = semiDesExpr obj $ applyRawArgs (RawValue emptyMetaN "/Context") ((Just "value", be) : map (\(ctxName, ctxM) -> (Nothing, RawValue ctxM ctxName)) ctxs)
semiDesExpr obj@Nothing (RawContextApply _ (_, be) ctxs) = semiDesExpr obj $ applyRawIArgs (RawValue emptyMetaN "/Context") (("value", IArgE be) : map (second IArgM) ctxs)
semiDesExpr obj (RawParen e) = semiDesExpr obj e
semiDesExpr obj@Nothing (RawMethod (RawTheExpr (RawValue m n)) method) = semiDesExpr obj (method `applyRawIArgs` [("this", IArgM (Meta (singletonType $ partialVal $ PRelativeName n) (getMetaPos m) emptyMetaDat))]) -- Parse type methods like :Integer.toString, Only for input expressions
semiDesExpr obj (RawMethod base method) = semiDesExpr obj $ applyRawArgs method [(Just "this", base)]
semiDesExpr obj (RawList m []) = semiDesExpr obj (RawValue m "/Data/Nil")
semiDesExpr obj (RawList m (l:ls)) = semiDesExpr obj (RawValue m "/Data/Cons" `applyRawArgs` [(Just "head", l), (Just "tail", RawList m ls)])

-- | Desugars a "TheExpr type" by applying the type to a default name
desugarTheExpr :: PExpr -> PExpr
desugarTheExpr t = RawValue (mWithType (singletonType t') (getExprMeta t)) defaultName
  where
    t' = exprToPartialType t
    defaultName = lowerCaseFirstLetter $ fromPartialName $ ptName t'
    lowerCaseFirstLetter (n:ns) = toLower n : ns
    lowerCaseFirstLetter ns     = ns

exprToPartialType :: PExpr -> PartialType
exprToPartialType = fromJust . fst . desObjPropagateTypes . desExpr H.empty . semiDesExpr Nothing

exprToType :: PExpr -> Type
exprToType (RawValue _ n@('$':_)) = TypeVar $ TVVar n
exprToType t                      = (singletonType . exprToPartialType) t

exprToTypeMeta :: PExpr -> ParseMeta
exprToTypeMeta e = mWithType (exprToType e) (getExprMeta e)
