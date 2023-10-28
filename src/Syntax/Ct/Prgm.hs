--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Prgm
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines most of the types that make up the Catln Syntax.
--------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}

module Syntax.Ct.Prgm where

import           Data.Hashable
import qualified Data.HashMap.Strict as H
import           Data.Void           (Void)
import           GHC.Generics        (Generic)

import           Data.Aeson          hiding (Object)
import           Semantics.Prgm
import           Semantics.Types
import           Text.Megaparsec
import           Text.Printf
import           Utils

data RawObjArr e m = RawObjArr {
  roaObj    :: !(Maybe (GuardExpr e m)),
  roaBasis  :: !ObjectBasis,
  roaDoc    :: !(Maybe DocComment),
  roaAnnots :: ![CompAnnot (e m)],
  roaM      :: !(Meta m),
  roaArr    :: !(Maybe (GuardExpr e m)),
  roaDef    :: !(Maybe (e m))
                               }
  deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)

data TypeProperty e m
  = TypePropProj TypeName (e m)
  | TypePropRel TypeName (e m)
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- Expr before desugar
data RawExpr m
  = RawCExpr (Meta m) Constant
  | RawValue (Meta m) TypeName
  | RawHoleExpr (Meta m) Hole
  | RawMacroValue (Meta m) TypeName
  | RawTheExpr (RawExpr m) -- ^ Written :TypeName and read as The TypeName
  | RawAliasExpr (RawExpr m) (RawExpr m) -- ^ base aliasExpr
  | RawTupleApply (Meta m) (Meta m, RawExpr m) [RawObjArr RawExpr m]
  | RawVarsApply (Meta m) (RawExpr m) [(RawExpr m, Meta m)]
  | RawContextApply (Meta m) (Meta m, RawExpr m) [(ArgName, Meta m)]
  | RawParen (RawExpr m)
  | RawMethod (RawExpr m) (RawExpr m) -- ^ base methodValue
  | RawList (Meta m) [RawExpr m]
  | RawTypeProp (Meta m) (RawExpr m) (TypeProperty RawExpr m)
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- | Helper for the X isa Y where this is the Y
type ExtendedClass = ClassName
type ExtendedClasses = [ExtendedClass]

data MultiTypeDef m = MultiTypeDef (RawExpr m) [GuardExpr RawExpr m] ExtendedClasses
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

type RawClassDef m = (RawExpr m, ExtendedClasses)

data RawApplyTerm e m = RATermDeep (e m) | RATermChild (e m)
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)
newtype RawApply e m = RawApply [RawApplyTerm e m]
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

data Path = Relative String | Absolute String
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

data RawStatement e m
  = RawDeclStatement (RawObjArr e m)
  | MultiTypeDefStatement (MultiTypeDef m) Path
  | TypeDefStatement (e m)
  | RawClassDefStatement (RawClassDef m) Path
  | RawClassDeclStatement (e m) Path
  | RawExprStatement (e m)
  | RawAnnot (CompAnnot (e m))
  | RawApplyStatement (RawApply e m)
  | RawModule String Path
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

data RawStatementTree e m = RawStatementTree (RawStatement e m) [RawStatementTree e m]
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

type FileImport = String
type RawPrgm m = ([FileImport], [RawStatementTree RawExpr m]) -- TODO: Include [Export]

type ParseErrorRes = ParseErrorBundle String Void
data ReplRes m
  = ReplStatement (RawStatementTree RawExpr m)
  | ReplExpr (RawExpr m)
  | ReplErr ParseErrorRes
  deriving (Eq, Show)

instance ExprClass RawExpr where
  getExprMeta expr = case expr of
    RawCExpr m _          -> m
    RawValue m _          -> m
    RawHoleExpr m _       -> m
    RawMacroValue m _     -> m
    RawTheExpr e          -> getExprMeta e
    RawAliasExpr b _      -> getExprMeta b
    RawTupleApply m _ _   -> m
    RawVarsApply m _ _    -> m
    RawContextApply m _ _ -> m
    RawParen e            -> getExprMeta e
    RawMethod e _         -> getExprMeta e
    RawList m _           -> m
    RawTypeProp m _ _     -> m

  getExprArg _ = Nothing

  maybeExprPathM (RawValue m n)               = Just (n, m)
  maybeExprPathM (RawTupleApply _ (_, e) _)   = maybeExprPathM e
  maybeExprPathM (RawVarsApply _ e _)         = maybeExprPathM e
  maybeExprPathM (RawContextApply _ (_, e) _) = maybeExprPathM e
  maybeExprPathM (RawParen e)                 = maybeExprPathM e
  maybeExprPathM (RawMethod _ e)              = maybeExprPathM e
  maybeExprPathM _                            = Nothing

  exprAppliedArgs (RawValue _ _) = []
  exprAppliedArgs (RawTupleApply _ (_, be) args) = exprAppliedArgs be ++ concatMap desObjArr args
  exprAppliedArgs (RawVarsApply _ e _) = exprAppliedArgs e
  exprAppliedArgs (RawContextApply _ (_, e) _) = exprAppliedArgs e
  exprAppliedArgs (RawParen e) = exprAppliedArgs e
  exprAppliedArgs (RawMethod _ e) = exprAppliedArgs e
  exprAppliedArgs _ = error "Unsupported RawExpr exprAppliedArgs"


  exprAppliedOrdVars (RawValue _ _) = []
  exprAppliedOrdVars (RawTupleApply _ (_, be) _) = exprAppliedOrdVars be
  -- exprAppliedOrdVars (RawVarsApply _ e vars) = H.union (exprAppliedOrdVars e) (H.fromList vars)
  exprAppliedOrdVars RawVarsApply{} = error "Not implemented"
  exprAppliedOrdVars (RawContextApply _ (_, e) _) = exprAppliedOrdVars e
  exprAppliedOrdVars (RawParen e) = exprAppliedOrdVars e
  exprAppliedOrdVars (RawMethod _ e) = exprAppliedOrdVars e
  exprAppliedOrdVars _ = error "Unsupported RawExpr exprAppliedOrdVars"

  exprVarArgs RawCExpr{} = H.empty
  exprVarArgs RawHoleExpr{} = H.empty
  exprVarArgs RawValue{} = H.empty
  exprVarArgs RawTheExpr{} = H.empty
  exprVarArgs (RawAliasExpr base alias) = H.unionWith (++) (exprVarArgs base) (exprVarArgs alias)
  exprVarArgs (RawTupleApply _ (_, be) args) = H.unionWith (++) (exprVarArgs be) (unionsWith (++) $ map oaVarArgs args)
  exprVarArgs (RawVarsApply _ e vars) = H.unionWith (++) (exprVarArgs e) (unionsWith (++) $ map aux vars)
    where
      aux (n, m) = H.singleton (TVVar TVInt $ exprPath n) [m]
  exprVarArgs (RawContextApply _ (_, e) _) = exprVarArgs e
  exprVarArgs (RawParen e) = exprVarArgs e
  exprVarArgs (RawMethod be me) = H.unionWith (++) (exprVarArgs be) (exprVarArgs me)
  exprVarArgs e = error $ printf "Unsupported RawExpr exprVarArgs for %s" (show e)

instance ObjArrClass RawObjArr where
  oaVarArgs roa = exprArg roa
    where
      exprArg RawObjArr{roaArr=(Just (GuardExpr argVal Nothing))} = exprVarArgs argVal
      exprArg RawObjArr{roaObj=(Just (GuardExpr obj Nothing)), roaArr= Nothing} = case exprPathM obj of
        (n, m) -> H.singleton (TVArg TVInt n) [m]
      exprArg oa = error $ printf "exprVarArgs not defined for arg %s" (show oa)
  getOaAnnots = roaAnnots

instance (Show m, Show (e m)) => Show (RawObjArr e m) where
  show RawObjArr{roaObj, roaM, roaArr, roaDef} = printf "%s%s%s%s%s" (showNoMaybe roaObj) showM showEquals (showNoMaybe roaArr) showDef
    where
      showNoMaybe :: (Show a) => Maybe a -> String
      showNoMaybe (Just a) = show a
      showNoMaybe Nothing  = ""

      showM :: String
      showM = if getMetaType roaM /= topType
        then printf " -> %s " (show roaM)
        else ""

      showEquals :: String
      showEquals = case (roaObj, roaArr) of
        (Just _, Just _) -> "= "
        _                -> ""

      showDef :: String
      -- showDef = maybe "" show roaDef
      showDef = case roaDef of
        Just def -> printf " ? %s" (show def)
        Nothing  -> ""

desObjArr :: (Show m, Show (e m)) => RawObjArr e m -> [ObjArr e m]
desObjArr (RawObjArr obj basis doc annots m arr Nothing) = [ObjArr obj basis doc annots m arr]
desObjArr roa = error $ printf "Not yet implemented: %s" (show roa)
