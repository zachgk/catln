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
import           Semantics.Annots
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Prgm

formatIndent :: Int -> String
formatIndent indent = concat $ replicate indent "  "

formatImport :: FileImport -> Builder
formatImport imp = do
  literal $ "import " ++ imp ++ "\n"

formatPartialType :: PartialType -> String
formatPartialType (PartialType ptName ptVars ptArgs ptPreds _) = concat [showName ptName, showTypeVars ptVars, showArgs ptArgs, showPreds ptPreds]
  where
    showName p  = fromPartialName p
    showArg (argName, argVal) = if argVal == TopType
      then '$':argName
      else argName ++ ": " ++ formatType argVal
    showTypeVars vars | H.null vars = ""
    showTypeVars vars = printf "[%s]" (intercalate ", " $ map showArg $ H.toList vars)
    showArgs args | H.null args = ""
    showArgs args = printf "(%s)" (intercalate ", " $ map showArg $ H.toList args)
    showPreds preds | null preds = ""
    showPreds preds = printf "| %s" (intercalate ", " $ map formatPartialType preds)

formatType :: Type -> String
formatType TopType = ""
formatType (TypeVar (TVVar TVInt t)) = "$" ++ t
formatType (TypeVar (TVVar TVExt t)) = "$_" ++ t
formatType (TypeVar TVArg{}) = error "Unexpected TVArg in formatter"
formatType (UnionType partials) = join $ map formatPartialType $ splitUnionType partials

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
  [RawObjArr{ roaArr=(Just (GuardExpr a _))}] -> operatorName ++ formatExpr a
  [RawObjArr{ roaArr=(Just (GuardExpr l _))}, RawObjArr{roaArr=(Just (GuardExpr r _))}] -> if n == operatorType
    then printf "%s%s %s" (formatExpr l) operatorName (formatExpr r) -- Show types as "x: 5" instead of "x : 5"
    else printf "%s %s %s" (formatExpr l) operatorName (formatExpr r)
  _ -> error "Non unary or binary operator found in formatExpr"
  where
    operatorName = drop (length operatorPrefix) n
formatExpr (RawTupleApply _ (_, be) args) = printf "%s(%s)" (formatExpr be) (intercalate ", " $ map formatObjArr args)
formatExpr (RawVarsApply _ be vars) = printf "%s[%s]" (formatExpr be) (intercalate ", " $ map (\(varN, varM) -> printf "%s%s" (formatExpr varN) (formatMeta varM)) vars)
formatExpr (RawContextApply _ (_, be) ctxs) = printf "%s{%s}" (formatExpr be) (intercalate ", " $ map (\(ctxN, ctxM) -> ctxN ++ formatMeta ctxM) ctxs)
formatExpr (RawParen e) = printf "(%s)" (formatExpr e)
formatExpr (RawMethod base method) = printf "%s.%s" (formatExpr base) (formatExpr method)
formatExpr (RawList _ l) = printf "[%s]" $ intercalate ", " $ map formatExpr l

formatObjArr :: RawObjArr RawExpr m -> String
formatObjArr roa@RawObjArr{roaObj, roaM, roaArr} = printf "%s%s%s%s%s" (showGuardExpr True roaObj) showElse showM showEquals (showGuardExpr False roaArr)
  where
    isNestedDeclaration = case roaArr of
      (Just (GuardExpr (RawValue _ n) _)) | n == nestedDeclaration -> True
      _                                                            -> False

    showGuardExpr False _ | isNestedDeclaration = ""
    showGuardExpr _ (Just (GuardExpr e g)) = formatExpr e ++ formatGuard g
    showGuardExpr _ Nothing = ""

    showM :: String
    showM  = case getMetaType roaM of
      TopType -> ""
      t       -> printf " -> %s" (formatType t)

    showEquals :: String
    showEquals = case (roaObj, roaArr) of
      _ | isNestedDeclaration -> " ="
      (Just _, Just _)        -> "= "
      _                       -> ""

    formatGuard :: ExprCond RawExpr m -> String
    formatGuard (Just e) = printf " if %s" (formatExpr e)
    formatGuard Nothing  = ""

    showElse :: String
    showElse = if hasElseAnnot roa then " else " else ""

formatStatement :: (Show m) => Int -> RawStatement RawExpr m -> String
formatStatement indent statement = formatIndent indent ++ statement' ++ "\n"
  where
    statement' = case statement of
      RawDeclStatement objArr -> formatObjArr objArr
      MultiTypeDefStatement (MultiTypeDef clss objs) _ -> printf "class %s = %s" (formatPartialType clss) showObjs
        where
          formatGuardExpr :: GuardExpr RawExpr m -> String
          formatGuardExpr (GuardExpr e Nothing) = formatExpr e
          formatGuardExpr (GuardExpr e (Just g)) = printf "%s where %s" (formatExpr e) (formatExpr g)

          showObjs = intercalate " | " $ map formatGuardExpr objs
      TypeDefStatement typeExpr -> if "#" `isPrefixOf` exprPath typeExpr
        then printf "annot %s" (formatExpr typeExpr)
        else printf "data %s" (formatExpr typeExpr)
      RawClassDefStatement (obj, className) _ -> printf "every %s isa %s" (formatExpr obj) className
      RawClassDeclStatement clss _ -> printf "class %s" (formatPartialType clss)
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

isHiddenStatement :: RawStatement RawExpr m -> Bool
isHiddenStatement (RawAnnot annot) | isElseAnnot annot = True
isHiddenStatement (RawAnnot annot) | isCtxAnnot annot = True
isHiddenStatement _ = False

formatStatementTree :: (Show m) => Bool -> Int -> RawStatementTree RawExpr m -> Builder
formatStatementTree rootStatement indent (RawStatementTree statement subTree) = do
  unless (isHiddenStatement statement) (literal $ formatStatement indent statement)

  -- Check if the subTree should also be a rootStatement with an additional ending newline
  let subTreeRootStatement = rootStatement && keepRootStatement statement

  forM_ subTree $ \s -> do
    formatStatementTree subTreeRootStatement (indent + 1) s
  when rootStatement ""

formatPrgm :: (Show m) => Int -> RawPrgm m -> Builder
formatPrgm indent (imports, statements) = do
  forM_ imports $ \imp -> do
    formatImport imp
  unless (null imports) "\n"
  forM_ statements $ \s -> do
    formatStatementTree True indent s
