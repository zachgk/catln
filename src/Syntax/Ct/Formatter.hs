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

buildJust :: Maybe a -> (a -> Builder) -> Builder
buildJust (Just a) f = f a
buildJust Nothing _  = ""

formatIndent :: Int -> String
formatIndent indent = concat $ replicate indent "  "

formatImport :: FileImport -> Builder
formatImport imp = do
  literal $ "import " ++ imp ++ "\n"

formatPattern :: Int -> Pattern RawExpr m -> String
formatPattern indent (Pattern (ExprObject _ _ pattExprObj) pattGuard) = formatExpr indent pattExprObj ++ formatGuard indent pattGuard

formatGuard :: Int -> Guard (RawExpr m) -> String
formatGuard indent (IfGuard e) = printf "| %s" (formatExpr indent e)
formatGuard _ ElseGuard        = " else"
formatGuard _ NoGuard          = ""

formatMeta :: Meta m -> String
formatMeta m = case getMetaType m of
  TopType -> ""
  t       -> show t ++ " "

formatExpr :: Int -> RawExpr m -> String
formatExpr _ (RawCExpr _ (CInt c)) = show c
formatExpr _ (RawCExpr _ (CFloat c)) = show c
formatExpr _ (RawCExpr _ (CStr c)) = show c
formatExpr _ (RawValue m n) = formatMeta m ++ n
formatExpr _ (RawHoleExpr _ (HoleActive Nothing)) = "_"
formatExpr _ (RawHoleExpr _ (HoleActive (Just a))) = "_" ++ a
formatExpr _ (RawHoleExpr _ HoleUndefined) = "undefined"
formatExpr _ (RawHoleExpr _ HoleTodefine) = "todefine"
formatExpr indent (RawAliasExpr base alias) = printf "%s@%s" (formatExpr indent base) (formatExpr indent alias)
formatExpr indent (RawTupleApply _ (_, RawValue _ n) args) | operatorPrefix `isPrefixOf` n = case args of
  [TupleArgIO _ _ a] -> operatorName ++ formatExpr indent a
  [TupleArgIO _ _ l, TupleArgIO _ _ r] -> printf "%s %s %s" (formatExpr indent l) operatorName (formatExpr indent r)
  _ -> error "Non unary or binary operator found in formatExpr"
  where
    operatorName = drop (length operatorPrefix) n
formatExpr indent (RawTupleApply _ (_, be) args) = printf "%s(%s)" (formatExpr indent be) (intercalate ", " $ map formatTupleArg args)
  where
    formatTupleArg :: TupleArg RawExpr m -> String
    formatTupleArg (TupleArgI _ n)    = n
    formatTupleArg (TupleArgO _ v)    = formatExpr indent v
    formatTupleArg (TupleArgIO _ n v) = printf "%s = %s" n (formatExpr indent v)
formatExpr indent (RawVarsApply _ be vars) = printf "%s<%s>" (formatExpr indent be) (intercalate ", " $ map (\(varN, varM) -> printf "%s%s" (formatMeta varM) varN) vars)
formatExpr indent (RawContextApply _ (_, be) ctxs) = printf "%s{%s}" (formatExpr indent be) (intercalate ", " $ map (\(ctxN, ctxM) -> formatMeta ctxM ++ ctxN) ctxs)
formatExpr indent (RawParen e) = printf "(%s)" (formatExpr indent e)
formatExpr indent (RawMethod base method) = printf "%s.%s" (formatExpr indent base) (formatExpr indent method)
formatExpr indent (RawList _ l) = printf "[%s]" $ intercalate ", " $ map (formatExpr indent) l

formatStatement :: Int -> RawStatement RawExpr m -> Builder
formatStatement indent statement = do
  literal $ formatIndent indent
  case statement of
    RawDeclStatement (RawDecl (DeclLHS _ patt) maybeArr) -> literal $ printf "%s%s" (formatPattern indent patt) showArr
      where
        showArr :: String
        showArr = case maybeArr of
          Just arr -> printf " = %s" (formatExpr indent arr)
          Nothing  -> ""
    MultiTypeDefStatement (MultiTypeDef className classVars objs) _ -> literal $ printf "class %s%s = %s" className showClassVars showObjs
      where
        showClassVars :: String
        showClassVars = if null classVars
              then ""
              else printf "<%s>" $ intercalate ", " $ map (\(n, t) -> printf "%s = %s" n (show t)) $ H.toList classVars
        showObjs = intercalate " | " $ map (formatExpr indent) objs
    TypeDefStatement (TypeDef typeExpr) -> literal $ printf "data %s" (formatExpr indent typeExpr)
    RawClassDefStatement (obj, className) _ -> literal $ printf "every %s isa %s" (formatExpr indent obj) className
    RawClassDeclStatement (className, classVars) _ -> literal $ printf "class %s%s" className showClassVars
      where
        showClassVars :: String
        showClassVars = if null classVars
              then ""
              else printf "<%s>" $ intercalate ", " $ map (\(n, t) -> printf "%s = %s" n (show t)) $ H.toList classVars
    RawExprStatement e -> literal $ formatExpr indent e
    RawAnnot annot | exprPath annot == mdAnnot -> literal $ printf "# %s" annotText'
      where
        (Just (_, Just (RawCExpr _ (CStr annotText)))) = H.lookup mdAnnotText $ exprAppliedArgsMap annot
        annotText' = concatMap (\c -> if c == '\n' then "\n" ++ formatIndent (indent + 1) else pure c) annotText
    RawAnnot annot -> literal $ formatExpr indent annot
    RawModule modul _ -> literal $ printf "module %s" modul
  "\n"

formatStatementTree :: Int -> RawStatementTree RawExpr m -> Builder
formatStatementTree indent (RawStatementTree statement subTree) = do
  formatStatement indent statement
  forM_ subTree $ \s -> do
    formatStatementTree (indent + 1) s
    when (indent == 0) "\n"

formatPrgm :: Int -> RawPrgm m -> Builder
formatPrgm indent (imports, statements) = do
  forM_ imports $ \imp -> do
    formatImport imp
  unless (null imports) "\n"
  forM_ statements $ \s -> do
    formatStatementTree indent s
