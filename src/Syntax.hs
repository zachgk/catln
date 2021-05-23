--------------------------------------------------------------------
-- |
-- Module    :  Syntax
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module contains miscellaneous and general syntax elements.
-- It also contains a the instances of program metadata and some
-- general syntax-based utilities.
--------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Syntax where

import qualified Data.HashMap.Strict   as H
import           Data.Hashable
import           Data.Void             (Void)

import           GHC.Generics          (Generic)
import           Text.Megaparsec.Error (ParseErrorBundle)

import           Data.Aeson            hiding (Object)
import           Data.Maybe
import           Syntax.Prgm
import           Syntax.Types
import           Text.Megaparsec
import           Text.Printf

type ParseErrorRes = ParseErrorBundle String Void

data ReplRes m
  = ReplStatement (RawStatement m)
  | ReplExpr (RawExpr m)
  | ReplErr ParseErrorRes
  deriving (Eq, Show)

type CodeRange = Maybe (SourcePos, SourcePos, String)

-- Metadata for the Programs
data PreTyped = PreTyped Type CodeRange
  deriving (Generic, Hashable, ToJSON)

data Typed = Typed Type CodeRange
  deriving (Eq, Ord, Generic, Hashable, ToJSON)

instance Show PreTyped where
  show (PreTyped t _) = show t

instance Show Typed where
  show (Typed t _) = show t

instance Hashable SourcePos where
  hashWithSalt s (SourcePos name line col) = s `hashWithSalt` show name `hashWithSalt` unPos line `hashWithSalt` unPos col

instance ToJSON SourcePos where
  toJSON (SourcePos name line col) = object ["name".=name, "line".=unPos line, "col".=unPos col]

class Meta m where
  getMetaType :: m -> Type
  getMetaPos :: m -> CodeRange
  labelPosM :: String -> m -> m

instance Meta PreTyped where
  getMetaType (PreTyped t _) = t
  getMetaPos (PreTyped _ pos) = pos
  labelPosM s (PreTyped t pos) = PreTyped t (labelPos s pos)

instance Meta Typed where
  getMetaType (Typed t _) = t
  getMetaPos (Typed _ pos) = pos
  labelPosM s (Typed t pos) = Typed t (labelPos s pos)

labelPos :: String -> CodeRange -> CodeRange
labelPos s (Just (p1, p2, sPrefix)) = Just (p1, p2, label')
  where label' = case sPrefix of
          [] -> s
          _  -> printf "%s-%s" sPrefix s
labelPos _ Nothing = Nothing

type ArgMetaMapWithSrc m = H.HashMap ArgName (m, Type)
-- |
-- The 'formArgMetaMapWithSrc' is similar to the 'formArgMetaMap' function.
-- It differs in that it accepts an additional partial type that is matched by the object and matches against that partial type.
formArgMetaMapWithSrc :: ClassMap -> Object m -> PartialType -> ArgMetaMapWithSrc m
formArgMetaMapWithSrc classMap obj@Object{objArgs=baseArgs} src@PartialType{ptArgs=srcArgs} = H.union (aux obj) (H.intersectionWith (,) (fmap fst baseArgs) srcArgs)
  where
    aux (Object m _ name _ args _ _) | H.null args = H.singleton name (m, singletonType src)
    aux Object{objArgs} = H.foldr (H.unionWith unionCombine) H.empty $ H.mapWithKey fromArg objArgs
      where
        unionCombine _ _ = error "Duplicate var matched"
        fromArg k (m, Nothing) = case H.lookup k srcArgs of
          Just srcArg -> H.singleton k (m, srcArg)
          Nothing     -> H.empty
        fromArg k (_, Just arg) = case H.lookup k srcArgs of
          Just (UnionType srcArg) -> mergeMaps $ map (formArgMetaMapWithSrc classMap arg) $ splitUnionType srcArg
          Just TopType -> (,TopType) <$> formArgMetaMap arg
          Just _ -> H.empty
          Nothing -> H.empty
        mergeMaps [] = H.empty
        mergeMaps (x:xs) = foldr (H.intersectionWith (\(m1, t1) (_, t2) -> (m1, unionTypes classMap t1 t2))) x xs

formVarMap :: ClassMap -> Type -> TypeVarEnv
formVarMap classMap (UnionType partialLeafs) = unionsWith (unionTypes classMap) $ map ptVars $ splitUnionType partialLeafs
formVarMap _ _ = error $ printf "Unknown formVarMap"

-- fullDest means to use the greatest possible type (after implicit).
-- Otherwise, it uses the minimal type that *must* be reached
arrowDestType :: (Meta m, Show m, ExprClass e, Show (e m)) => Bool -> ClassMap -> PartialType -> Object m -> Arrow (e m) m -> Type
arrowDestType fullDest classMap src obj@Object{objM} (Arrow arrM _ _ maybeExpr) = case mapM getExprArg maybeExpr of
  Just (Just _) -> fromMaybe (error "Unfound expr") expr'
  _             -> joined
  where
    varEnv = formVarMap classMap $ intersectTypes classMap (getMetaType objM) (singletonType src)
    argEnv = snd <$> formArgMetaMapWithSrc classMap obj ((\(UnionType pl) -> head $ splitUnionType pl) $ substituteVars $ singletonType src)
    substitute = substituteVarsWithVarEnv varEnv . substituteArgsWithArgEnv argEnv
    expr' = fmap (substitute . getMetaType . getExprMeta) maybeExpr
    arr' = substitute $ getMetaType arrM
    joined = if fullDest
      then unionTypes classMap (fromMaybe bottomType expr') arr'
      else intersectTypes classMap (fromMaybe TopType expr') arr'

metaTypeVar :: (Meta m) => m -> Maybe TypeVarAux
metaTypeVar m = case getMetaType m of
  TypeVar v -> Just v
  _         -> Nothing


isSubtypePartialOfWithObj :: (Meta m) => ClassMap -> Object m -> PartialType -> Type -> Bool
isSubtypePartialOfWithObj classMap Object{objVars, objArgs} = isSubtypePartialOfWithEnv classMap (fmap getMetaType objVars) (fmap (getMetaType . fst) objArgs)

isSubtypeOfWithObj :: (Meta m) => ClassMap -> Object m -> Type -> Type -> Bool
isSubtypeOfWithObj classMap obj@Object{objVars} = isSubtypeOfWithEnv classMap (fmap getMetaType objVars) (getMetaType <$> formArgMetaMap obj)

isSubtypeOfWithObjSrc :: (Meta m) => ClassMap -> PartialType -> Object m -> Type -> Type -> Bool
isSubtypeOfWithObjSrc classMap srcType obj@Object{objVars} = isSubtypeOfWithEnv classMap (fmap getMetaType objVars) (snd <$> formArgMetaMapWithSrc classMap obj srcType )
