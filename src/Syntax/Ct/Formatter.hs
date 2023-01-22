--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Formatter
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides a formatter that produces a nicely formatted string
-- from the syntax.
--------------------------------------------------------------------

module Syntax.Ct.Formatter where

import           Control.Monad
import qualified Data.HashMap.Strict as H
import           Data.List
import           Data.String.Builder
import           Text.Printf

import           Constants
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Prgm

formatIndent :: Int -> String
formatIndent indent = concat $ replicate indent "  "

formatImport :: FileImport -> Builder
formatImport imp = do
  literal $ "import " ++ imp ++ "\n"

formatPattern :: Pattern RawExpr m -> String
formatPattern (Pattern (ExprObject _ _ pattExprObj) pattGuard) = formatExpr pattExprObj ++ formatGuard pattGuard

formatGuard :: Guard (RawExpr m) -> String
formatGuard (IfGuard e) = printf " if %s" (formatExpr e)
formatGuard ElseGuard   = " else"
formatGuard NoGuard     = ""

formatType :: Type -> String
formatType TopType = ""
formatType (TypeVar (TVVar t)) = t
formatType (TypeVar TVArg{}) = error "Unexpected TVArg in formatter"
formatType (UnionType partials) = join $ map formatPartialType $ splitUnionType partials
  where
    formatPartialType (PartialType ptName ptVars ptArgs ptPreds _) = concat [showName ptName, showTypeVars ptVars, showArgs ptArgs, showPreds ptPreds]
      where
        showName p  = fromPartialName p
        showArg (argName, argVal) = if argVal == TopType
          then argName
          else argName ++ ": " ++ formatType argVal
        showTypeVars vars | H.null vars = ""
        showTypeVars vars = printf "[%s]" (intercalate ", " $ map showArg $ H.toList vars)
        showArgs args | H.null args = ""
        showArgs args = printf "(%s)" (intercalate ", " $ map showArg $ H.toList args)
        showPreds preds | null preds = ""
        showPreds preds = printf "| %s" (intercalate ", " $ map formatPartialType preds)

formatMeta :: Meta m -> String
formatMeta m = case getMetaType m of
  TopType -> ""
  t       -> ": " ++ formatType t

formatExpr ::  RawExpr m -> String
formatExpr (RawCExpr _ (CInt c)) = show c
formatExpr (RawCExpr _ (CFloat c)) = show c
formatExpr (RawCExpr _ (CStr c)) = show c
formatExpr (RawValue m n) = n ++ formatMeta m
formatExpr (RawHoleExpr m (HoleActive Nothing)) = "_" ++ formatMeta m
formatExpr (RawHoleExpr m (HoleActive (Just a))) = "_" ++ a ++ formatMeta m
formatExpr (RawHoleExpr _ HoleUndefined) = "undefined"
formatExpr (RawHoleExpr _ HoleTodefine) = "todefine"
formatExpr (RawTheExpr t) = printf ":%s" (formatExpr t)
formatExpr (RawAliasExpr base alias) = printf "%s@%s" (formatExpr base) (formatExpr alias)
formatExpr (RawTupleApply _ (_, RawValue _ n) args) | operatorPrefix `isPrefixOf` n = case args of
  [TupleArgIO _ _ a] -> operatorName ++ formatExpr a
  [TupleArgIO _ _ l, TupleArgIO _ _ r] -> printf "%s %s %s" (formatExpr l) operatorName (formatExpr r)
  _ -> error "Non unary or binary operator found in formatExpr"
  where
    operatorName = drop (length operatorPrefix) n
formatExpr (RawTupleApply _ (_, be) args) = printf "%s(%s)" (formatExpr be) (intercalate ", " $ map formatTupleArg args)
  where
    formatTupleArg :: TupleArg RawExpr m -> String
    formatTupleArg (TupleArgI m n)    = n ++ formatMeta m
    formatTupleArg (TupleArgO _ v)    = formatExpr v
    formatTupleArg (TupleArgIO _ n v) = printf "%s= %s" n (formatExpr v)
formatExpr (RawVarsApply _ be vars) = printf "%s[%s]" (formatExpr be) (intercalate ", " $ map (\(varN, varM) -> printf "%s%s" varN (formatMeta varM)) vars)
formatExpr (RawContextApply _ (_, be) ctxs) = printf "%s{%s}" (formatExpr be) (intercalate ", " $ map (\(ctxN, ctxM) -> ctxN ++ formatMeta ctxM) ctxs)
formatExpr (RawParen e) = printf "(%s)" (formatExpr e)
formatExpr (RawMethod base method) = printf "%s.%s" (formatExpr base) (formatExpr method)
formatExpr (RawList _ l) = printf "[%s]" $ intercalate ", " $ map formatExpr l

formatStatement :: Int -> RawStatement RawExpr m -> String
formatStatement indent statement = formatIndent indent ++ statement' ++ "\n"
  where
    statement' = case statement of
      RawDeclStatement (RawDecl (DeclLHS m patt) maybeArr) -> printf "%s%s%s" (formatPattern patt) showM showArr
        where
          showM :: String
          showM  = case getMetaType m of
            TopType -> ""
            t       -> printf " -> %s" (formatType t)

          showArr :: String
          showArr = case maybeArr of
            Just (RawValue _ n) | n == nestedDeclaration -> " ="
            Just arr -> printf " = %s" (formatExpr arr)
            Nothing  -> ""
      MultiTypeDefStatement (MultiTypeDef className classVars objs) _ -> printf "class %s%s = %s" className showClassVars showObjs
        where
          showClassVars :: String
          showClassVars = if null classVars
                then ""
                else printf "[%s]" $ intercalate ", " $ map showClassVar $ H.toList classVars
          showClassVar (n, t) = if t == TopType
            then n
            else printf "%s = %s" n (formatType t)

          showObjs = intercalate " | " $ map formatExpr objs
      TypeDefStatement (TypeDef typeExpr) -> if "#" `isPrefixOf` exprPath typeExpr
        then printf "annot %s" (formatExpr typeExpr)
        else printf "data %s" (formatExpr typeExpr)
      RawClassDefStatement (obj, className) _ -> printf "every %s isa %s" (formatExpr obj) className
      RawClassDeclStatement (className, classVars) _ -> printf "class %s%s" className showClassVars
        where
          showClassVars :: String
          showClassVars = if null classVars
                then ""
                else printf "[%s]" $ intercalate ", " $ map showClassVar $ H.toList classVars
          showClassVar (n, TopType) = n
          showClassVar (n, t)       = printf "%s = %s" n (formatType t)
      RawExprStatement e -> formatExpr e
      RawAnnot annot | exprPath annot == mdAnnot -> printf "# %s" annotText'
        where
          (Just (_, Just (RawCExpr _ (CStr annotText)))) = H.lookup mdAnnotText $ exprAppliedArgsMap annot
          annotText' = concatMap (\c -> if c == '\n' then "\n" ++ formatIndent (indent + 1) else pure c) annotText
      RawAnnot annot -> formatExpr annot
      RawModule modul _ -> printf "module %s" modul

-- |
-- A root statement has an additional ending newline
-- Some statement types like modules will have subTrees that are also roots.
-- Others like declarations will not
keepRootStatement :: RawStatement RawExpr m -> Bool
keepRootStatement RawDeclStatement{} = False
keepRootStatement _                  = True

formatStatementTree :: Bool -> Int -> RawStatementTree RawExpr m -> Builder
formatStatementTree rootStatement indent (RawStatementTree statement subTree) = do
  literal $ formatStatement indent statement

  -- Check if the subTree should also be a rootStatement with an additional ending newline
  let subTreeRootStatement = rootStatement && keepRootStatement statement

  forM_ subTree $ \s -> do
    formatStatementTree subTreeRootStatement (indent + 1) s
  when rootStatement ""

formatPrgm :: Int -> RawPrgm m -> Builder
formatPrgm indent (imports, statements) = do
  forM_ imports $ \imp -> do
    formatImport imp
  unless (null imports) "\n"
  forM_ statements $ \s -> do
    formatStatementTree True indent s
