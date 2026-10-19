--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Builder
-- Copyright :  (c) Zach Kimberg 2024
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines utilities for building syntax elements
--------------------------------------------------------------------

module Syntax.Ct.Builder where
import           CtConstants
import           Data.Bifunctor          (first)
import           Data.Char               (toLower)
import           Data.Maybe              (fromMaybe)
import           Data.UUID               (nil)
import           MapMeta                 (DerivedMeta, derivedMetaID)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Text.Megaparsec         (SourcePos)

emptyMeta :: SourcePos -> SourcePos -> ParseMeta
emptyMeta p1 p2 = Meta PTopType (Just (p1, p2)) nil emptyMetaDat

rawVal :: (MetaDat m) => String -> RawExpr m
rawVal s = RawValue m (mkName s)
  where
    m = emptyMetaN
    -- m = Meta (typeVal (PTypeName name)) Nothing emptyMetaDat

-- | The name of an arrow desugar synthesizes for a written statement, carrying the statement's
-- position and an id derived from the statement's ('derivedMetaID'). The statement is named by the
-- meta of its object, as 'Syntax.Ct.MapRawMeta.rawStatementExprIds' names statements.
rawValDerived :: (MetaDat m) => DerivedMeta -> Meta m -> String -> RawExpr m
rawValDerived kind origin s = RawValue (emptyMetaM origin){getMetaId=derivedMetaID kind (getMetaId origin)} (mkName s)

applyRawArgs :: (MetaDat m, Show m) => RawExpr m -> [(Maybe ArgName, RawExpr m)] -> RawExpr m
applyRawArgs base args = applyRawEArgs base (map mapArg args)
  where
    mapArg :: (MetaDat m) => (Maybe ArgName, RawExpr m) -> (Maybe (RawExpr m), RawExpr m)
    mapArg (Just argName, argVal) = (Just $ RawValue (emptyMetaE argVal) (pkName argName), argVal)
    mapArg (Nothing, argVal) = (Nothing, argVal)

applyRawEArgs :: (MetaDat m, Show m) => RawExpr m -> [(Maybe (RawExpr m), RawExpr m)] -> RawExpr m
applyRawEArgs base [] = base
applyRawEArgs base args = case base of
  (RawTupleApply m base' baseArgs) -> RawTupleApply m base' (baseArgs ++ args')
  _ -> RawTupleApply (emptyMetaE base) (emptyMetaE base, base) args'
  where
    args' = map ((False,) . mapArg) args
    mapArg :: (MetaDat m, Show m) => (Maybe (RawExpr m), RawExpr m) -> RawObjArr RawExpr m
    mapArg (Just argName, argVal) = RawObjArr (Just argName) ArgObj Nothing [] (Just (Just argVal, Nothing)) Nothing
    mapArg (Nothing, argVal) = RawObjArr Nothing ArgObj Nothing [] (Just (Just argVal, Nothing)) Nothing

data IArg e = IArgNothing | IArgE (e ()) | IArgM (Maybe (e ()))
applyRawIArgs :: PExpr -> [(ArgName, IArg RawExpr)] -> PExpr
applyRawIArgs base args = applyRawEIArgs base (map mapArg args)
  where
    mapArg :: (ArgName, IArg RawExpr) -> (PExpr, IArg RawExpr)
    mapArg (argName, IArgE argVal) = (RawValue (emptyMetaE argVal) (pkName argName), IArgE argVal)
    mapArg (argName, IArgM argM) = (RawValue emptyMetaN (pkName argName), IArgM argM)
    mapArg (argName, IArgNothing) = (RawValue (emptyMetaE base) (pkName argName), IArgNothing)

applyRawEIArgs :: PExpr -> [(PExpr, IArg RawExpr)] -> PExpr
applyRawEIArgs base [] = base
applyRawEIArgs base args = case base of
  (RawTupleApply m base' baseArgs) -> RawTupleApply m base' (baseArgs ++ args')
  _ -> RawTupleApply (emptyMetaE base) (emptyMetaE base, base) args'
  where
    args' = map ((False,) . mapArg) args
    mapArg :: (PExpr, IArg RawExpr) -> RawObjArr RawExpr ()
    mapArg (argName, IArgE argVal) = RawObjArr (Just argName) ArgObj Nothing [] (Just (Just argVal, Nothing)) Nothing
    mapArg (argName, IArgM argM) = RawObjArr (Just argName) ArgObj Nothing [] (Just (Nothing, argM)) Nothing
    mapArg (argName, IArgNothing) = RawObjArr (Just argName) ArgObj Nothing [] (Just (Nothing, Nothing)) Nothing

-- | A bare (patternless) call-site parameter expression, e.g. the @v@ in a higher-order argument
-- @fn(v)= …@ (or the alias @v\@y@). Its 'roaArr' is 'Nothing' (unlike an 'IArgNothing' arg, which
-- yields @Just (Nothing, Nothing)@), matching how the parser represents a nested parameter.
rawBareParam :: RawExpr m -> RawObjArr RawExpr m
rawBareParam e = RawObjArr (Just e) ArgObj Nothing [] Nothing Nothing

-- | Apply bare (patternless) parameter expressions to a base, producing @base(p1)(p2)…@ exactly as a
-- call-site higher-order head like the @fn(v)@ in @fn(v)= body@ parses.
applyRawBareParams :: (MetaDat m, Show m) => RawExpr m -> [RawExpr m] -> RawExpr m
applyRawBareParams base []     = base
applyRawBareParams base params = RawTupleApply (emptyMetaE base) (emptyMetaE base, base) (map ((False,) . rawBareParam) params)

applyRawExprVars :: (MetaDat m) => RawExpr m -> [(TypeVarName, Maybe (RawExpr m))] -> RawExpr m
applyRawExprVars base vars = applyRawExprEVars base vars'
  where
    vars' = map (first (RawValue (emptyMetaE base) . pkName)) vars

applyRawExprEVars :: (MetaDat m) => RawExpr m -> [(RawExpr m, Maybe (RawExpr m))] -> RawExpr m
applyRawExprEVars base []   = base
applyRawExprEVars base vars = case base of
  (RawVarsApply m base' baseArgs) -> RawVarsApply m base' (baseArgs ++ vars')
  _                               -> RawVarsApply (emptyMetaE base) base vars'
  where
    vars' = map aux vars
    aux (n, m) = RawObjArr (Just n) ArgObj Nothing [] (Just (Nothing, m)) Nothing

rawStr :: (MetaDat m) => String -> RawExpr m
rawStr = RawCExpr emptyMetaN . CStr

rawInt :: (MetaDat m) => Integer -> RawExpr m
rawInt = RawCExpr emptyMetaN . CInt

rawComment :: (MetaDat m, Show m) => String -> RawExpr m
rawComment s = rawVal mdAnnot `applyRawArgs` [(Just $ partialKey (mkName mdAnnotText), rawStr s)]

exprVal :: (MetaDat m) => String -> Expr m
exprVal s = Value m (mkName s)
  where
    m = emptyMetaN
    -- m = Meta (typeVal (PTypeName name)) Nothing emptyMetaDat

applyExprOArgs :: (MetaDat m, Show m) => Expr m -> [(Maybe ArgName, Expr m)] -> Expr m
applyExprOArgs = foldl addArg
  where
    addArg b a = TupleApply (emptyMetaE b) (emptyMetaE b, b) (EAppArg $ mapArg a)
      where
        mapArg (Just argName, argVal) = ObjArr (Just (Value (emptyMetaE  b) (pkName argName))) ArgObj Nothing [] (Just (Just argVal, emptyMetaE argVal))
        mapArg (Nothing, argVal) = ObjArr Nothing ArgObj Nothing [] (Just (Just argVal, emptyMetaE argVal))

applyExprIArgs :: Expr () -> [(ArgName, IArg Expr)] -> Expr ()
applyExprIArgs = foldl addArg
  where
    addArg b a = TupleApply (emptyMetaE b) (emptyMetaE b, b) (EAppArg $ mapArg a)
      where
        mapArg :: (ArgName, IArg Expr) -> ObjArr Expr ()
        mapArg (argName, IArgE argVal) = ObjArr (Just (Value (emptyMetaE b) (pkName argName))) ArgObj Nothing [] (Just(Just argVal, emptyMetaE argVal))
        mapArg (argName, IArgM _argM) = ObjArr (Just (Value emptyMetaN (pkName argName))) ArgObj Nothing [] (Just (Nothing, emptyMetaE b))
        mapArg (argName, IArgNothing) = ObjArr (Just (Value (emptyMetaE  b) (pkName argName))) ArgObj Nothing [] (Just (Nothing, emptyMetaE b))

applyExprVars :: (MetaDat m) => Expr m -> [(TypeVarName, Meta m)] -> Expr m
applyExprVars = foldl addVar
  where
    addVar b (varName, varVal) = TupleApply (emptyMetaE b) (emptyMetaE b, b) (EAppVar varName varVal)

rawAnon :: PExpr
rawAnon = rawVal anonStr

-- | A type reference written as the spread @Box..@: every value of the type, whatever arguments it
-- carries, rather than the argument-less partial application the bare name is.
--
-- The distinction is the language's, not a spelling convenience. A bare name is a term in its own
-- right — the partial application a constructor is before anything has been given to it — so an
-- annotation that writes one admits only that, and a field holding a constructed value
-- (@data(Cv(inner -> Bv))@ where @Bv@ carries fields of its own) meets the two to bottom. What that
-- annotation means is @Bv..@, and a generated declaration has to say it as a written one does.
--
-- The same shape 'Syntax.Ct.Parser.Expr.pValue' builds for a written @..@ — a spread application of
-- an active hole — so a generated declaration and a hand-written one desugar through one path. It
-- goes on the NAME, before any type parameters: @Box..[$T -> Integer]@ is what parses, where
-- @Box[$T -> Integer]..@ is a parse error.
--
-- The base value carries the relative-name type its own name denotes, as the parser gives it: the
-- spread is desugared by widening THAT type ('Syntax.Ct.Desugarf.Expr.semiDesExpr' over
-- 'Semantics.Types.spreadType'), and a base built by 'rawVal' — whose meta is ⊤ until the types are
-- propagated, which happens after — has nothing there to widen, so the spread is silently dropped
-- and the bare name is what reaches the program.
rawSpreadVal :: String -> PExpr
rawSpreadVal name = RawTupleApply m (m, base) [(True, rawInObjArr True (RawHoleExpr m (HoleActive Nothing)))]
  where
    m = emptyMetaN
    base = RawValue (mWithType (relTypeVal (mkName name)) m) (mkName name)

rawInObjArr :: Bool -> PExpr -> PObjArr
rawInObjArr withArr e = RawObjArr (Just e) FunctionObj Nothing [] (if withArr then Just (Nothing, Nothing) else Nothing) Nothing

rawOutObjArr :: PExpr -> PObjArr
rawOutObjArr e = RawObjArr Nothing FunctionObj Nothing [] (Just (Just e, Nothing)) Nothing

rawModule :: String -> PStatement
rawModule n = RawDeclStatement $ rawOutObjArr (rawVal modStr `applyRawExprVars` [(partialKey (mkName n), Nothing)])

classInstSt :: (PExpr, [PExpr]) -> PStatement
classInstSt (cls, extends) = RawDeclStatement $ rawOutObjArr (rawVal everyStr `applyRawEIArgs` [(cls, IArgNothing), (rawVal isaStr, IArgE $ RawList emptyMetaN extends)])

classDeclSt :: PExpr -> [PExpr] -> PStatement
classDeclSt cls extends = RawDeclStatement $ rawOutObjArr (rawVal classStr `applyRawEIArgs` [(cls, IArgNothing)] `applyRawArgs` [(Just $ partialKey (mkName isaStr), RawList emptyMetaN extends) | not (null extends)])

classWithObjs :: PExpr -> [PExpr] -> [PExpr] -> PStatement
classWithObjs cls objs extends = RawDeclStatement $ rawOutObjArr (rawVal classStr `applyRawEIArgs` [(cls, IArgNothing), (RawList emptyMetaN objs, IArgNothing)] `applyRawArgs` [(Just $ partialKey (mkName isaStr), RawList emptyMetaN extends) | not (null extends)])

-- | @data(tp)@, as 'classWithObjs' is @class(cls, [objs])@ — the one declaration statement that had
-- no builder, since nothing in the compiler built a type until the builtins page did.
dataSt :: PExpr -> PStatement
dataSt tp = RawDeclStatement $ rawOutObjArr (rawVal dataStr `applyRawEIArgs` [(tp, IArgNothing)])

-- | The key a state is kept under in a context: the name of its type, lowercased (@IO@ and
-- @/Catln/IO@ are @io@, @Counter(i)@ is @counter@). A state is found by its type, so a function
-- reading a @Counter@ finds the one another function added, whatever either called it. 'Nothing'
-- where no type is written (@{io}@, @{c= x}@).
contextTypeKey :: PExpr -> Maybe String
contextTypeKey (RawValue _ n)
  | isTypeName = Just $ map toLower base
  | otherwise = Nothing
  where
    base = last $ npathPath $ getPath $ nameStr n
    isTypeName = take 1 base /= map toLower (take 1 base)
contextTypeKey (RawTupleApply _ (_, b) _) = contextTypeKey b
contextTypeKey (RawVarsApply _ b _) = contextTypeKey b
contextTypeKey (RawAliasExpr b _) = contextTypeKey b
contextTypeKey (RawParen e) = contextTypeKey e
contextTypeKey _ = Nothing

-- | A state written in a context's braces: the key it is kept under ('contextTypeKey') and the name
-- the body binds it to. @{c= Counter(i)}@ is the @counter@ state bound as @c@, @{io -> IO}@ the
-- @io@ state bound as @io@, and @{IO}@ the @io@ state under its own key. A state given no type
-- (@{io}@) is keyed by the name it is written with.
contextState :: PObjArr -> Maybe (String, String)
contextState RawObjArr{roaObj=Just v@(RawValue _ n), roaArr} = case roaArr of
  Just (Just pat, _) -> Just (keyOr pat, name)
  Just (_, Just t)   -> Just (keyOr t, name)
  _                  -> Just (keyOr v, keyOr v)
  where
    name = nameStr n
    keyOr e = fromMaybe name (contextTypeKey e)
contextState _ = Nothing
