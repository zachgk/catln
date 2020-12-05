--------------------------------------------------------------------
-- |
-- Module    :  Syntax
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Syntax where

import           Data.Hashable
import qualified Data.HashMap.Strict as H
import           Data.Void             (Void)
import           Data.List                      ( intercalate )

import           GHC.Generics          (Generic)
import           Text.Megaparsec.Error (ParseErrorBundle)
import           Text.Printf

import Syntax.Types
import Syntax.Prgm
import Data.Aeson (ToJSON)
import Data.Maybe

type ParseErrorRes = ParseErrorBundle String Void

data ReplRes m
  = ReplStatement (RawStatement m)
  | ReplExpr (RawExpr m)
  | ReplErr ParseErrorRes
  deriving (Eq, Show)

--- ResArrowTree
type ResBuildEnvItem f = (PartialType, Guard (Expr Typed), ResArrow f)
type ResBuildEnv f = H.HashMap TypeName [ResBuildEnvItem f]
type ResExEnv f = H.HashMap (Arrow (Expr Typed) Typed) (ResArrowTree f, [ResArrowTree f]) -- (result, [compAnnot trees])
data ResArrow f
  = ResEArrow (Object Typed) (Arrow (Expr Typed) Typed)
  | PrimArrow Type f
  | ConstantArrow Constant
  | ArgArrow Type String
  deriving (Eq, Generic, Hashable)
type TBEnv f = (ResBuildEnv f, H.HashMap PartialType (ResArrow f), ClassMap)

data ResArrowTree f
  = ResArrowCompose (ResArrowTree f) (ResArrowTree f)
  | ResArrowMatch (H.HashMap PartialType (ResArrowTree f))
  | ResArrowCond [(ResArrowTree f, ResArrowTree f)] (ResArrowTree f) -- [(if, then)] else
  | ResArrowTuple String (H.HashMap String (ResArrowTree f))
  | ResArrowTupleApply (ResArrowTree f) String (ResArrowTree f)
  | ResArrowSingle (ResArrow f)
  | ResArrowID
  deriving (Eq, Generic, Hashable)

instance Show (ResArrow f) where
  show (ResEArrow obj arrow) = printf "(ResEArrow: %s -> %s)" (show obj) (show arrow)
  show (PrimArrow tp _) = "(PrimArrow " ++ show tp ++ ")"
  show (ConstantArrow c) = "(ConstantArrow " ++ show c ++ ")"
  show (ArgArrow tp n) = "(ArgArrow " ++ show tp ++ " " ++ n ++ ")"

instance Show (ResArrowTree f) where
  show (ResArrowCompose a b) = show a ++ " -> " ++ show b
  show (ResArrowMatch args) = "match (" ++ args' ++ ")"
    where
      showArg (leaf, tree) = show leaf ++ " -> " ++ show tree
      args' = intercalate ", " $ map showArg $ H.toList args
  show (ResArrowCond ifTrees elseTree) = "( [" ++ ifTrees' ++ "] ( else " ++ show elseTree ++ ") )"
    where
      showIfTree (condTree, thenTree) = "if " ++ show condTree ++ " then " ++ show thenTree
      ifTrees' = intercalate ", " $ map showIfTree ifTrees
  show (ResArrowTuple name args) = if H.null args
    then name
    else name ++ "(" ++ args' ++ ")"
    where
      showArg (argName, val) = argName ++ " = " ++ show val
      args' = intercalate ", " $ map showArg $ H.toList args
  show (ResArrowTupleApply base argName argVal) = printf "(%s)(%s = %s)" (show base) argName (show argVal)
  show (ResArrowSingle a) = show a
  show ResArrowID = "ResArrowID"


-- Metadata for the Programs
newtype PreTyped = PreTyped Type
  deriving (Eq, Ord, Generic, Hashable, ToJSON)

newtype Typed = Typed Type
  deriving (Eq, Ord, Generic, Hashable, ToJSON)

instance Show PreTyped where
  show (PreTyped t) = show t

instance Show Typed where
  show (Typed t) = show t

class Meta m where
  getMetaType :: m -> Type

instance Meta PreTyped where
  getMetaType (PreTyped t) = t

instance Meta Typed where
  getMetaType (Typed t) = t

type ArgMetaMapWithSrc m = H.HashMap ArgName (m, Type)
formArgMetaMapWithSrc :: ClassMap -> Object m -> PartialType -> ArgMetaMapWithSrc m
formArgMetaMapWithSrc _ (Object m _ name _ args) src | H.null args = H.singleton name (m, singletonType src)
formArgMetaMapWithSrc classMap (Object _ _ _ _ args) (_, _, _, srcArgs) = H.foldr (H.unionWith unionCombine) H.empty $ H.mapWithKey fromArg args
  where
    unionCombine _ _ = error "Duplicate var matched"
    fromArg k (m, Nothing) = case H.lookup k srcArgs of
      Just srcArg -> H.singleton k (m, srcArg)
      Nothing -> H.empty
    fromArg k (m, Just arg@(Object _ _ argName _ _)) = case H.lookup k srcArgs of
      Just (SumType srcArg) -> mergeMaps $ map (formArgMetaMapWithSrc classMap arg) $ splitPartialLeafs srcArg
      Just TopType -> H.singleton argName (m, TopType)
      Just _ -> H.empty
      Nothing -> H.empty
    mergeMaps [] = H.empty
    mergeMaps (x:xs) = foldr (H.intersectionWith (\(m1, t1) (_, t2) -> (m1, unionType classMap t1 t2))) x xs

formVarMap :: ClassMap -> Type -> TypeVarEnv
formVarMap classMap (SumType partialLeafs) = unionsWith (unionType classMap) $ map (\(_, vars, _, _) -> vars) $ splitPartialLeafs partialLeafs
formVarMap _ _ = error "Unknown formVarMap"

-- fullDest means to use the greatest possible type (after implicit).
-- Otherwise, it uses the minimal type that *must* be reached
arrowDestType :: (Meta m, Show m, ExprClass e) => Bool -> ClassMap -> PartialType -> Object m -> Arrow (e m) m -> Type
arrowDestType fullDest classMap src obj@(Object objM _ _ _ _) (Arrow arrM _ _ maybeExpr) = case mapM getExprArg maybeExpr of
  Just (Just _) -> fromMaybe (error "Unfound expr") expr'
  _ -> joined
  where
    varEnv = formVarMap classMap $ intersectTypes classMap (getMetaType objM) (singletonType src)
    argEnv = snd <$> formArgMetaMapWithSrc classMap obj src
    substitute = substituteVarsWithVarEnv varEnv . substituteArgsWithArgEnv argEnv
    expr' = fmap (substitute . getMetaType . getExprMeta) maybeExpr
    arr' = substitute $ getMetaType arrM
    joined = if fullDest
      then unionType classMap (fromMaybe bottomType expr') arr'
      else intersectTypes classMap (fromMaybe TopType expr') arr'

metaTypeVar :: (Meta m) => m -> Maybe TypeVarAux
metaTypeVar m = case getMetaType m of
  TypeVar v -> Just v
  _ -> Nothing
