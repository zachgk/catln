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
import           Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import qualified Data.Text.Lazy as T
import Text.Pretty.Simple
import           Text.Printf

import Syntax.Types
import Syntax.Prgm

type ParseErrorRes = ParseErrorBundle String Void

data ReplRes m
  = ReplStatement (RawStatement m)
  | ReplExpr (RawExpr m)
  | ReplErr ParseErrorRes
  deriving (Eq, Show)

--- ResArrowTree
type ResBuildEnv f = H.HashMap TypeName [(PartialType, Guard (Expr Typed), ResArrow f)]
type ResExEnv f = H.HashMap (Arrow (Expr Typed) Typed) (ResArrowTree f, [ResArrowTree f]) -- (result, [compAnnot trees])
data ResArrow f
  = ResEArrow (Object Typed) (Arrow (Expr Typed) Typed)
  | PrimArrow Type f
  | ConstantArrow Constant
  | ArgArrow Type String
  deriving (Eq, Generic, Hashable)

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


-- compile errors
type EStacktrace = [String]
data CNote
  = GenCNote String
  | GenCErr String
  | ParseCErr ParseErrorRes
  | TypeCheckCErr
  | BuildTreeCErr String
  | AssertCErr String
  | EvalCErr EStacktrace String
  | WrapCN [CNote] String
  deriving (Eq, Show)

prettyCNote :: CNote -> String
prettyCNote (ParseCErr p) = errorBundlePretty p
prettyCNote (WrapCN n s) = s ++ "\n\t\t" ++ intercalate "\n\t\t" (map prettyCNote n)
prettyCNote (EvalCErr st err) = printf "%s\n\tStack trace:\n\t\t%s" err (intercalate "\n\t\t" st)
prettyCNote n = T.unpack $ pShow n

wrapCErr :: [CNote] -> String -> CRes r
wrapCErr notes s = CErr [WrapCN notes s]

data CRes r
  = CRes [CNote] r
  | CErr [CNote]
  deriving (Eq, Show)

getCNotes :: CRes r -> [CNote]
getCNotes (CRes notes _) = notes
getCNotes (CErr notes) = notes

partitionCRes :: [CRes r] -> ([CNote], [CRes r])
partitionCRes = aux ([], [])
  where
    aux x [] = x
    aux (errRes, resRes) (r@CRes{}:xs) = aux (errRes, r:resRes) xs
    aux (errRes, resRes) ((CErr newErrNotes):xs) = aux (newErrNotes ++ errRes, resRes) xs

instance Functor CRes where
  fmap f (CRes notes r) = CRes notes (f r)
  fmap _ (CErr notes) = CErr notes

instance Applicative CRes where
  pure = CRes []
  (CRes notesA f) <*> (CRes notesB b) = CRes (notesA ++ notesB) (f b)
  resA <*> resB = CErr (getCNotes resA ++ getCNotes resB)

instance Monad CRes where
  return = pure
  (CRes notesA a) >>= f = case f a of
    (CRes notesB b) -> CRes (notesA ++ notesB) b
    (CErr notesB) -> CErr (notesA ++ notesB)
  (CErr notes) >>= _ = CErr notes


-- Metadata for the Programs
newtype PreTyped = PreTyped Type
  deriving (Eq, Ord, Generic, Hashable)

newtype Typed = Typed Type
  deriving (Eq, Ord, Generic, Hashable)

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
formArgMetaMapWithSrc classMap (Object _ _ _ _ args) (_, _, srcArgs) = H.foldr (H.unionWith unionCombine) H.empty $ H.mapWithKey fromArg args
  where
    unionCombine _ _ = error "Duplicate var matched"
    fromArg k (m, Nothing) = case H.lookup k srcArgs of
      Just srcArg -> H.singleton k (m, srcArg)
      Nothing -> H.empty
    fromArg k (_, Just arg) = case H.lookup k srcArgs of
      Just (SumType srcArg) -> mergeMaps $ map (formArgMetaMapWithSrc classMap arg) $ splitPartialLeafs srcArg
      Just _ -> H.empty
      Nothing -> H.empty
    mergeMaps [] = H.empty
    mergeMaps (x:xs) = foldr (H.intersectionWith (\(m1, t1) (_, t2) -> (m1, unionType classMap t1 t2))) x xs

-- fullDest means to use the greatest possible type (after implicit).
-- Otherwise, it uses the minimal type that *must* be reached
arrowDestType :: (Meta m, Show m, ExprClass e) => Bool -> ClassMap -> PartialType -> Object m -> Arrow (e m) m -> Type
arrowDestType fullDest classMap src@(_, _, srcArgs) obj@(Object _ _ _ _ objArgs) (Arrow arrM _ _ maybeExpr) = case getMetaType arrM of
  arrType@(TypeVar TVVar{}) -> do
    let argsMatchingTypeVar = H.filter (\(m, _) -> getMetaType m == arrType) objArgs
    case H.elems $ H.intersectionWith const srcArgs argsMatchingTypeVar of
      [] -> basicDest arrType
      -- if the result is a type variable then it should be the intersection of all type variable args in the src
      srcArgsAtTypeVar -> intersectAllTypes classMap srcArgsAtTypeVar
  (TypeVar (TVArg t)) -> case H.lookup t objArgs of
    Just (objArgM, _) -> getMetaType objArgM
    _ -> error $ printf "arrowDestType with unknown arg %s" (show t)
  arrType -> basicDest arrType
  where
    basicDest arrType = case (maybeExpr, mapM getExprArg maybeExpr) of
      (_, Just (Just n)) -> maybe arrType snd (H.lookup n $ formArgMetaMapWithSrc classMap obj src)
      (Just e, _) | not fullDest -> getMetaType $ getExprMeta e
      _ -> arrType

metaTypeVar :: (Meta m) => m -> Maybe TypeVarAux
metaTypeVar m = case getMetaType m of
  TypeVar v -> Just v
  _ -> Nothing
