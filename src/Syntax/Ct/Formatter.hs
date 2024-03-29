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
import           Semantics.Annots
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Prgm

formatIndent :: Int -> String
formatIndent indent = concat $ replicate indent "  "

formatImport :: RawFileImport -> Builder
formatImport imp = do
  literal $ "import " ++ formatExpr imp ++ "\n"

formatPartialKey :: PartialKey -> String
formatPartialKey = formatPartialType . partialToType

formatPartialType :: PartialType -> String
formatPartialType (PartialType ptName ptVars ptArgs ptPreds _) = concat [ptName, showTypeVars ptVars, showArgs ptArgs, showPreds ptPreds]
  where
    showArg (argName, argVal) = if argVal == topType
      then '$':formatPartialKey argName
      else formatPartialKey argName ++ ": " ++ formatType argVal
    showTypeVars vars | H.null vars = ""
    showTypeVars vars = printf "[%s]" (intercalate ", " $ map showArg $ H.toList vars)
    showArgs args | H.null args = ""
    showArgs args = printf "(%s)" (intercalate ", " $ map showArg $ H.toList args)
    showPreds preds | null preds = ""
    -- showPreds preds = printf "| %s" (intercalate ", " $ map formatPartialType preds)
    showPreds _ = undefined

formatType :: Type -> String
formatType (TopType []) = ""
formatType (TopType _) = undefined
formatType (TypeVar (TVVar t) TVInt) = "$" ++ formatPartialKey t
formatType (TypeVar (TVVar t) TVExt) = "$_" ++ formatPartialKey t
formatType (TypeVar TVArg{} _) = error "Unexpected TVArg in formatter"
formatType (UnionType partials) = join $ map formatPartialType $ splitUnionType partials

formatMeta :: Meta m -> String
formatMeta m = case getMetaType m of
  (TopType []) -> ""
  t            -> ": " ++ formatType t

formatExpr ::  RawExpr m -> String
formatExpr (RawCExpr _ (CInt c)) = show c
formatExpr (RawCExpr _ (CFloat c)) = show c
formatExpr (RawCExpr _ (CStr c)) = show c
formatExpr (RawValue m n) = n ++ formatMeta m
formatExpr (RawMacroValue _ n) = "${" ++ n ++ "}"
formatExpr (RawHoleExpr m (HoleActive Nothing)) = "_" ++ formatMeta m
formatExpr (RawHoleExpr m (HoleActive (Just a))) = "_" ++ a ++ formatMeta m
formatExpr (RawHoleExpr _ HoleUndefined) = "undefined"
formatExpr (RawHoleExpr _ HoleTodefine) = "todefine"
formatExpr (RawTheExpr t) = printf ":%s" (formatExpr t)
formatExpr (RawSpread t) = printf "%s.." (formatExpr t)
formatExpr (RawAliasExpr base alias) = printf "%s@%s" (formatExpr base) (formatExpr alias)
formatExpr (RawTupleApply _ (_, RawValue _ n) args) | operatorPrefix `isPrefixOf` n = case args of
  [RawObjArr{ roaArr=(Just (Just (GuardExpr a _), _))}] -> operatorName ++ formatExpr a
  [RawObjArr{ roaArr=(Just (Just (GuardExpr l _), _))}, RawObjArr{roaArr=(Just (Just (GuardExpr r _), _))}] -> if n == operatorType
    then printf "%s%s %s" (formatExpr l) operatorName (formatExpr r) -- Show types as "x: 5" instead of "x : 5"
    else printf "%s %s %s" (formatExpr l) operatorName (formatExpr r)
  _ -> error "Non unary or binary operator found in formatExpr"
  where
    operatorName = drop (length operatorPrefix) n
formatExpr (RawTupleApply _ (_, be) args) = printf "%s(%s)" (formatExpr be) (intercalate ", " $ map formatObjArr args)
formatExpr (RawVarsApply _ be vars) = printf "%s[%s]" (formatExpr be) (intercalate ", " $ map (\(varN, varM) -> printf "%s%s" (formatExpr varN) (formatMeta varM)) vars)
formatExpr (RawContextApply _ (_, be) ctxs) = printf "%s{%s}" (formatExpr be) (intercalate ", " $ map (\(ctxN, ctxM) -> formatPartialKey ctxN ++ formatMeta ctxM) ctxs)
formatExpr (RawParen e) = printf "(%s)" (formatExpr e)
formatExpr (RawMethod base method) = printf "%s.%s" (formatExpr base) (formatExpr method)
formatExpr (RawList _ l) = printf "[%s]" $ intercalate ", " $ map formatExpr l
formatExpr (RawTypeProp _ base (TypePropProj p (RawValue _ vn))) | vn == truePrim = printf "%s_%s" (formatExpr base) p
formatExpr (RawTypeProp _ base (TypePropProj p v)) = printf "%s_%s(%s)" (formatExpr base) p (formatExpr v)
formatExpr (RawTypeProp _ base (TypePropRel p v)) = printf "%s__%s(%s)" (formatExpr base) p (formatExpr v)

formatIsa :: ExtendedClasses -> String
formatIsa []      = ""
formatIsa classes = " isa " ++ intercalate ", " classes

formatObjArr :: RawObjArr RawExpr m -> String
formatObjArr roa@RawObjArr{roaObj, roaArr, roaDef} = printf "%s%s%s%s%s%s" (showGuardExpr True roaObj) showElse showM showEquals (showGuardExpr False roaArrExpr) showDef
  where
    roaArrExpr = fst =<< roaArr

    isNestedDeclaration = case roaArr of
      (Just (Just (GuardExpr (RawValue _ n) _), _)) | n == nestedDeclaration -> True
      _                                                            -> False

    showGuardExpr False _ | isNestedDeclaration = ""
    showGuardExpr _ (Just (GuardExpr e g)) = formatExpr e ++ formatGuard g
    showGuardExpr _ Nothing = ""

    showM :: String
    showM  = case fmap (getMetaType . snd) roaArr of
      Nothing             -> ""
      (Just (TopType [])) -> ""
      (Just t)            -> printf " -> %s" (formatType t)

    showEquals :: String
    showEquals = case (roaObj, roaArr) of
      _ | isNestedDeclaration    -> " ="
      (Just _, Just (Just{}, _)) -> "= "
      _                          -> ""

    formatGuard :: ExprCond RawExpr m -> String
    formatGuard (Just e) = printf " | %s" (formatExpr e)
    formatGuard Nothing  = ""

    showElse :: String
    showElse = if hasElseAnnot roa then " else " else ""

    showDef :: String
    showDef = case roaDef of
      Just d  -> printf " ? %s" (formatExpr d)
      Nothing -> ""

formatStatement :: (MetaDat m, Show m) => Int -> RawStatement RawExpr m -> String
formatStatement indent statement = formatIndent indent ++ statement' ++ "\n"
  where
    statement' = case statement of
      RawDeclStatement objArr -> formatObjArr objArr
      MultiTypeDefStatement (MultiTypeDef clss objs extends) -> printf "class %s = %s%s" (formatExpr clss) showObjs (formatIsa extends)
        where
          formatGuardExpr :: GuardExpr RawExpr m -> String
          formatGuardExpr (GuardExpr e Nothing) = formatExpr e
          formatGuardExpr (GuardExpr e (Just g)) = printf "%s | %s" (formatExpr e) (formatExpr g)

          showObjs = intercalate " | " $ map formatGuardExpr objs
      TypeDefStatement typeExpr -> if "#" `isPrefixOf` exprPath typeExpr
        then printf "annot %s" (formatExpr typeExpr)
        else printf "data %s" (formatExpr typeExpr)
      RawClassDefStatement (obj, className) -> printf "every %s%s" (formatExpr obj) (formatIsa className)
      RawClassDeclStatement clss -> printf "class %s" (formatExpr clss)
      RawExprStatement e -> formatExpr e
      RawAnnot annot | exprPath annot == mdAnnot -> printf "# %s" annotText'
        where
          (Just (_, Just (RawCExpr _ (CStr annotText)))) = H.lookup (partialKey mdAnnotText) $ exprAppliedArgsMap annot
          annotText' = concatMap (\c -> if c == '\n' then "\n" ++ formatIndent (indent + 1) else pure c) annotText
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
