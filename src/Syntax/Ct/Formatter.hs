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

import           CtConstants
import           Semantics.Annots
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Prgm
import           Utils               (fst3, snd3)

formatIndent :: Int -> String
formatIndent indent = concat $ replicate indent "  "

formatImport :: RawFileImport -> Builder
formatImport RawFileImport{rawImpRaw} = do
  literal $ "import " ++ formatExpr rawImpRaw ++ "\n"

formatExpr ::  RawExpr m -> String
formatExpr (RawCExpr _ (CInt c)) = show c
formatExpr (RawCExpr _ (CFloat c)) = show c
formatExpr (RawCExpr _ (CStr c)) = show c
formatExpr (RawCExpr _ (CChar c)) = show c
formatExpr (RawValue _ n) = n
formatExpr (RawMacroValue _ n) = "${" ++ n ++ "}"
formatExpr (RawHoleExpr _ (HoleActive Nothing)) = "_"
formatExpr (RawHoleExpr _ (HoleActive (Just a))) = "_" ++ a
formatExpr (RawHoleExpr _ HoleUndefined) = "undefined"
formatExpr (RawHoleExpr _ HoleTodefine) = "todefine"
formatExpr (RawTheExpr t) = printf ":%s" (formatExpr t)
formatExpr (RawSpread t) = printf "%s.." (formatExpr t)
formatExpr (RawAliasExpr base alias) = printf "%s@%s" (formatExpr base) (formatExpr alias)
formatExpr (RawWhere base cond) = printf "%s | %s" (formatExpr base) (formatExpr cond)
formatExpr (RawTupleApply _ (_, RawValue _ n) args) | operatorPrefix `isPrefixOf` n = case args of
  [RawObjArr{ roaArr=(Just (Just a, _, _))}] -> op ++ formatExpr a
  [RawObjArr{ roaArr=(Just (Just l, _, _))}, RawObjArr{roaArr=(Just (Just r, _, _))}] -> if n == operatorType
    then printf "%s%s %s" (formatExpr l) op (formatExpr r) -- Show types as "x: 5" instead of "x : 5"
    else printf "%s %s %s" (formatExpr l) op (formatExpr r)
  _ -> error "Non unary or binary operator found in formatExpr"
  where
    op = drop (length operatorPrefix) n
formatExpr (RawTupleApply _ (_, be) args) = printf "%s(%s)" (formatExpr be) (intercalate ", " $ map formatObjArr args)
formatExpr (RawVarsApply _ be vars) = printf "%s[%s]" (formatExpr be) (intercalate ", " $ map formatObjArr vars)
formatExpr (RawContextApply _ (_, be) ctxs) = printf "%s{%s}" (formatExpr be) (intercalate ", " $ map formatObjArr ctxs)
formatExpr (RawParen e) = printf "(%s)" (formatExpr e)
formatExpr (RawMethod base method) = printf "%s.%s" (formatExpr base) (formatExpr method)
formatExpr (RawList _ l) = printf "[%s]" $ intercalate ", " $ map formatExpr l
formatExpr (RawTypeProp _ base (TypePropProj p (RawValue _ vn))) | vn == truePrim = printf "%s_%s" (formatExpr base) p
formatExpr (RawTypeProp _ base (TypePropProj p v)) = printf "%s_%s(%s)" (formatExpr base) p (formatExpr v)
formatExpr (RawTypeProp _ base (TypePropRel p v)) = printf "%s__%s(%s)" (formatExpr base) p (formatExpr v)

formatIsa :: ExtendedClasses RawExpr m -> String
formatIsa []      = ""
formatIsa classes = " isa " ++ intercalate ", " (map formatExpr classes)

-- | Formats either ObjArr or Bind Statement
formatObjArrLike :: String -> RawObjArr RawExpr m -> String
formatObjArrLike eq roa@RawObjArr{roaObj, roaArr, roaDef} = printf "%s%s%s%s%s%s" (showE True roaObj) showElse showM showEquals (showE False roaArrExpr) showDef
  where
    roaArrExpr = fst3 =<< roaArr

    isNestedDeclaration = case roaArr of
      (Just (Just (RawValue _ n), _, _)) | n == nestedDeclaration -> True
      _                                                           -> False

    showE False _ | isNestedDeclaration = ""
    showE _ (Just e) = formatExpr e
    showE _ Nothing = ""

    showM :: String
    showM  = case fmap snd3 roaArr of
      Nothing         -> ""
      Just Nothing    -> ""
      (Just (Just t)) -> printf " -> %s" (formatExpr t)

    showEquals :: String
    showEquals = case (roaObj, roaArr) of
      _ | isNestedDeclaration       -> " " ++ eq
      (Just _, Just (Just{}, _, _)) -> eq ++ " "
      _                             -> ""

    showElse :: String
    showElse = if hasElseAnnot roa then " else " else ""

    showDef :: String
    showDef = case roaDef of
      Just d  -> printf " ? %s" (formatExpr d)
      Nothing -> ""

formatObjArr :: RawObjArr RawExpr m -> String
formatObjArr = formatObjArrLike "="

formatStatement :: (MetaDat m, Show m) => Int -> RawStatement RawExpr m -> String
formatStatement indent statement = formatIndent indent ++ statement' ++ "\n"
  where
    statement' = case statement of
      RawDeclStatement objArr -> formatObjArr objArr
      MultiTypeDefStatement (MultiTypeDef clss objs extends) -> printf "class %s = %s%s" (formatExpr clss) showObjs (formatIsa extends)
        where
          showObjs = intercalate " || " $ map formatExpr objs
      TypeDefStatement typeExpr extends -> printf "%s %s%s" kw (formatExpr typeExpr) (formatIsa extends)
        where
          kw :: String
          kw = if "#" `isPrefixOf` exprPath typeExpr then "annot" else "data"
      RawClassDefStatement (obj, className) -> printf "every %s%s" (formatExpr obj) (formatIsa className)
      RawClassDeclStatement clss extends -> printf "class %s%s" (formatExpr clss) (formatIsa extends)
      RawBindStatement oa -> formatObjArrLike "<-" oa
      RawExprStatement e -> formatExpr e
      RawAnnot annot | exprPath annot == mdAnnot -> printf "# %s" annotText'
        where
          (Just (_, Just (RawCExpr _ (CStr annotText)))) = H.lookup (partialKey mdAnnotText) $ exprAppliedArgsMap annot
          annotText' = concatMap (\c -> if c == '\n' then "\n" ++ formatIndent (indent + 1) else pure c) annotText
      RawAnnot annot | exprPath annot == printAnnot -> case H.lookup (partialKey printAnnotText) $ exprAppliedArgsMap annot of
                         (Just (_, Just e)) -> printf "> %s" (formatExpr e)
                         e -> error $ printf "Can't format unexpected print annot %s" (show e)
      RawAnnot annot -> formatExpr annot
      RawApplyStatement (RawApply terms) -> "apply " ++ unwords (formatExpr term1 : map formatTerm termsRest)
        where
          (RATermDeep term1:termsRest) = terms

          formatTerm :: RawApplyTerm RawExpr m -> String
          formatTerm (RATermDeep e)  = formatExpr e
          formatTerm (RATermChild e) = "> " ++ formatExpr e
      RawModule modul -> printf "module %s" modul

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

formatStatementTree :: (MetaDat m, Show m) => Bool -> Int -> RawStatementTree RawExpr m -> Builder
formatStatementTree rootStatement indent (RawStatementTree statement subTree) = do
  unless (isHiddenStatement statement) (literal $ formatStatement indent statement)

  -- Check if the subTree should also be a rootStatement with an additional ending newline
  let subTreeRootStatement = rootStatement && keepRootStatement statement

  forM_ subTree $ \s -> do
    formatStatementTree subTreeRootStatement (indent + 1) s
  when rootStatement ""

formatPrgm :: (MetaDat m, Show m) => Int -> RawPrgm m -> Builder
formatPrgm indent (imports, statements) = do
  forM_ imports $ \imp -> do
    formatImport imp
  unless (null imports) "\n"
  forM_ statements $ \s -> do
    formatStatementTree True indent s

formatRootPrgm :: (MetaDat m, Show m) => RawPrgm m -> String
formatRootPrgm = build . formatPrgm 0
