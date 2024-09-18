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

formatIndent :: Int -> String
formatIndent indent = concat $ replicate indent "  "

formatImport :: RawFileImport -> Builder
formatImport AFileImport{impRaw} = do
  literal $ "import " ++ formatExpr impRaw ++ "\n"


formatRawApply :: (Show m) => RawApply RawExpr m -> String
formatRawApply (RawApply (RATermDeep term1:termsRest)) = unwords (formatExpr term1 : map formatTerm termsRest)
  where
    formatTerm :: (Show m) => RawApplyTerm RawExpr m -> String
    formatTerm (RATermDeep e)  = formatExpr e
    formatTerm (RATermChild e) = "> " ++ formatExpr e
formatRawApply _ = undefined

formatExpr :: (Show m) => RawExpr m -> String
formatExpr (RawCExpr _ (CInt c)) = show c
formatExpr (RawCExpr _ (CFloat c)) = show c
formatExpr (RawCExpr _ (CStr c)) = show c
formatExpr (RawCExpr _ (CChar c)) = show c
formatExpr (RawValue _ n) | n == anonStr = ""
formatExpr (RawValue _ n) = n
formatExpr (RawMacroValue _ n) = "${" ++ n ++ "}"
formatExpr (RawFmtStrExpr _ n s) = n ++ ("\"" ++ s ++ "\"")
formatExpr (RawApplyExpr m n) = formatExpr $ RawFmtStrExpr m "a" (formatRawApply n)
formatExpr (RawHoleExpr _ (HoleActive Nothing)) = "_"
formatExpr (RawHoleExpr _ (HoleActive (Just a))) = "_" ++ a
formatExpr (RawHoleExpr _ HoleUndefined) = "undefined"
formatExpr (RawHoleExpr _ HoleTodefine) = "todefine"
formatExpr (RawTheExpr t) = printf ":%s" (formatExpr t)
formatExpr (RawTupleApply _ (_, v@(RawValue _ vn)) [(True, _)]) | vn /= anonStr = printf "%s.." (formatExpr v)
formatExpr (RawAliasExpr base alias) = printf "%s@%s" (formatExpr base) (formatExpr alias)
formatExpr (RawWhere _ base cond) = printf "%s | %s" (formatExpr base) (formatExpr cond)
formatExpr (RawTupleApply _ (_, RawValue _ n) args) | operatorPrefix `isPrefixOf` n = case args of
  [(False, RawObjArr{ roaArr=(Just (Just a, _))})] -> op ++ formatExpr a
  [(False, RawObjArr{ roaArr=(Just (Just l, _))}), (False, RawObjArr{roaArr=(Just (Just r, _))})] -> if n == operatorType
    then printf "%s%s %s" (formatExpr l) op (formatExpr r) -- Show types as "x: 5" instead of "x : 5"
    else printf "%s %s %s" (formatExpr l) op (formatExpr r)
  _ -> error "Non unary or binary operator found in formatExpr"
  where
    op = drop (length operatorPrefix) n
formatExpr (RawTupleApply _ (_, be) args) = printf "%s(%s)" (formatExpr be) (intercalate ", " $ map aux args)
  where
    aux (spread, a) = (if spread then ".." else "") ++ formatObjArr a
formatExpr (RawVarsApply _ be vars) = printf "%s[%s]" (formatExpr be) (intercalate ", " $ map formatObjArr vars)
formatExpr (RawContextApply _ (_, be) ctxs) = printf "%s{%s}" (formatExpr be) (intercalate ", " $ map formatObjArr ctxs)
formatExpr (RawParen e) = printf "(%s)" (formatExpr e)
formatExpr (RawMethod _ base method) = printf "%s.%s" (formatExpr base) (formatExpr method)
formatExpr (RawList _ l) = printf "[%s]" $ intercalate ", " $ map formatExpr l
formatExpr (RawTypeProp _ base (TypePropProj p (RawValue _ vn))) | vn == truePrim = printf "%s_%s" (formatExpr base) p
formatExpr (RawTypeProp _ base (TypePropProj p v)) = printf "%s_%s(%s)" (formatExpr base) p (formatExpr v)
formatExpr (RawTypeProp _ base (TypePropRel p v)) = printf "%s__%s(%s)" (formatExpr base) p (formatExpr v)

-- | Formats either ObjArr or Bind Statement
formatObjArrLike :: (Show m) => String -> RawObjArr RawExpr m -> String
formatObjArrLike eq roa@RawObjArr{roaObj, roaArr, roaDef} = printf "%s%s%s%s%s%s" (showE True roaObj) showElse showM showEquals (showE False roaArrExpr) showDef
  where
    roaArrExpr = fst =<< roaArr

    isNestedDeclaration = case roaArr of
      (Just (Just (RawValue _ n), _)) | n == nestedDeclaration -> True
      _                                                        -> False

    showE False _ | isNestedDeclaration = ""
    showE _ (Just e) = formatExpr e
    showE _ Nothing = ""

    showM :: String
    showM  = case fmap snd roaArr of
      Nothing         -> ""
      Just Nothing    -> ""
      (Just (Just t)) -> printf " -> %s" (formatExpr t)

    showEquals :: String
    showEquals = case (roaObj, roaArr) of
      _ | isNestedDeclaration    -> " " ++ eq
      (Just _, Just (Just{}, _)) -> eq ++ " "
      _                          -> ""

    showElse :: String
    showElse = if hasElseAnnot roa then " else " else ""

    showDef :: String
    showDef = case roaDef of
      Just d  -> printf " ? %s" (formatExpr d)
      Nothing -> ""

formatObjArr :: (Show m) => RawObjArr RawExpr m -> String
formatObjArr = formatObjArrLike "="

formatStatement :: (MetaDat m, Show m) => Int -> RawStatement RawExpr m -> String
formatStatement indent statement = formatIndent indent ++ statement' ++ "\n"
  where
    statement' = case statement of
      RawDeclStatement objArr -> formatObjArr objArr
      RawBindStatement oa -> formatObjArrLike "<-" oa
      RawAnnot annot | exprPath annot == mdAnnot -> case H.lookup (partialKey mdAnnotText) $ rawExprAppliedArgsMap annot of
        (Just (Just (_, Just (RawCExpr _ (CStr annotText))))) -> printf "# %s" annotText'
          where
            annotText' = concatMap (\c -> if c == '\n' then "\n" ++ formatIndent (indent + 1) else pure c) annotText
        _ -> undefined
      RawAnnot annot | exprPath annot == printAnnot -> case H.lookup (partialKey printAnnotText) $ rawExprAppliedArgsMap annot of
                         (Just (Just (_, Just e))) -> printf "> %s" (formatExpr e)
                         e -> error $ printf "Can't format unexpected print annot %s" (show e)
      RawAnnot annot -> formatExpr annot

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
formatPrgm indent (RawPrgm imports statements) = do
  forM_ imports $ \imp -> do
    formatImport imp
  unless (null imports) "\n"
  forM_ statements $ \s -> do
    formatStatementTree True indent s

formatRootPrgm :: (MetaDat m, Show m) => RawPrgm m -> String
formatRootPrgm = build . formatPrgm 0
