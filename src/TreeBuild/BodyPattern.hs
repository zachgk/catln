--------------------------------------------------------------------
-- |
-- Module    :  TreeBuild.BodyPattern
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module chooses and instantiates the arms written with a body
-- pattern ('bodyPatternParts'): an arm whose higher-order parameter is
-- written with the body the function given to it must have
-- (@fwdBwd(x, params, f(x, params)= a(x, params) + b(x, params))@).
-- The tree build matches the pattern against the body of the function
-- a call gives, which it holds as the call's lambda, and builds the arm
-- with what the pattern bound in place of its variables.
--------------------------------------------------------------------

module TreeBuild.BodyPattern (
  instantiateBodyPatterns
  ) where

import           Control.Monad
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Maybe
import           Text.Printf

import           CRes
import           CtConstants
import           Eval.Common
import           Semantics.Prgm
import           Semantics.Types

type BPExpr = Expr EvalMetaDat
type BPObjArr = ObjArr Expr EvalMetaDat

-- | What a body pattern bound: each literal variable the constant it matched, and each sub-function
-- variable (keyed by the parameter it binds) the part of the body it matched. A part of the body is
-- written in the names the matched function gives its parameters, kept beside it.
data Bindings = Bindings {
  bLiterals :: H.HashMap Name BPExpr,
  bSubFns   :: H.HashMap ArgName ([ArgName], BPExpr)
  }

instance Semigroup Bindings where
  Bindings l1 s1 <> Bindings l2 s2 = Bindings (l1 <> l2) (s1 <> s2)

instance Monoid Bindings where
  mempty = Bindings H.empty H.empty

-- | The arm for a call: the arm itself when it has no body pattern and no guard matching an arrow's
-- definition, and otherwise the arm instantiated for the functions the call gives it ('Just'), or
-- 'Nothing' when one of them does not have the body its pattern asks for or a guard's definition
-- does not match its pattern. The function is the call's lambda for the slot ('tbLambdas'); a call
-- that gives none is reported, as the body is then not known where the arm is chosen.
instantiateBodyPatterns :: TBEnv -> PartialType -> BPObjArr -> CRes (Maybe BPObjArr)
instantiateBodyPatterns env@TBEnv{tbLambdas} srcType oa
  | null patterns && null defGuards = return (Just oa)
  | otherwise = do
    matched <- forM patterns $ \(fObj, pat, _) -> do
      let key = inExprSingleton fObj
      case filter (lambdaFor key) tbLambdas of
        (ObjArr{oaObj=Just lamObj, oaArr=Just (Just body, _)}:_) -> return $ matchBody env (H.keys (exprAppliedArgsMap fObj)) (lambdaParams lamObj) pat body
        _ -> CErr [MkCNote $ BuildTreeCErr (getMetaPos $ getExprMeta fObj) $ printf "The definition %s is chosen by the body of the function given as %s, but the call %s gives no function whose body is known where it is built" (show $ oaObjPath oa) (show key) (show srcType)]
    case sequence matched of
      Nothing -> return Nothing
      Just patBindings -> do
        let bindings = mconcat patBindings
        guarded <- forM defGuards (resolveDefGuard env srcType oa bindings)
        return $ case sequence guarded of
          Just guardBindings -> Just (instantiate (bindings <> mconcat guardBindings) oa{oaObj = fmap stripDefGuards (oaObj oa)})
          Nothing -> Nothing
  where
    patterns = mapMaybe bodyPatternParts (exprAppliedArgs (oaObjExpr oa))
    defGuards = objDefGuards (oaObjExpr oa)
    lambdaFor key ObjArr{oaObj=Just lamObj, oaArr=Just (Just _, _)} = let k = inExprSingleton lamObj in makeAbsoluteName (pkName k) == makeAbsoluteName (pkName key) && S.map (makeAbsoluteName . pkName) (pkArgs k) == S.map (makeAbsoluteName . pkName) (pkArgs key)
    lambdaFor _ _ = False
    lambdaParams lamObj = H.keys (exprAppliedArgsMap lamObj)

-- | The guards of an object that match an arrow's definition ('arrowDefGuardParts').
objDefGuards :: BPExpr -> [(BPExpr, BPExpr, BPExpr)]
objDefGuards (EWhere _ b c) = maybeToList (arrowDefGuardParts c) ++ objDefGuards b
objDefGuards (AliasExpr b _) = objDefGuards b
objDefGuards _ = []

-- | The object without its guards matching an arrow's definition: they are answered where the arm
-- is chosen ('resolveDefGuard'), so the arm built for the call has nothing of them left to decide.
stripDefGuards :: BPExpr -> BPExpr
stripDefGuards (EWhere m b c)
  | isJust (arrowDefGuardParts c) = stripDefGuards b
  | otherwise = EWhere m (stripDefGuards b) c
stripDefGuards (AliasExpr b a) = AliasExpr (stripDefGuards b) a
stripDefGuards e = e

-- | The innermost value an expression is applied to: the name of a call.
exprHead :: BPExpr -> BPExpr
exprHead (TupleApply _ (_, be) _) = exprHead be
exprHead e                        = e

-- | Answers a guard matching an arrow's definition, @(obj \@-> T)\@pattern@, for a call: what the
-- pattern binds when the answer matches it, or 'Nothing' when it does not. The answer is @Some@ of
-- the definition reached from @obj@ by an arrow to @T@, or @Nothing@ when no definition is. The
-- input is the function named (by name, or by a value variable a body pattern bound) applied to the
-- types of what it is given: the call's own arguments, which the guard names, and constants. The
-- definition is the function it is, of its parameters with its body, and it is bound as a
-- sub-function variable is, so the arm calling it is given its body in place.
resolveDefGuard :: TBEnv -> PartialType -> BPObjArr -> Bindings -> (BPExpr, BPExpr, BPExpr) -> CRes (Maybe Bindings)
resolveDefGuard TBEnv{tbResEnv, tbTypeEnv} srcType oa Bindings{bLiterals} (defInput, resultE, pat) = do
  calleeName <- case exprHead defInput of
    AliasExpr HoleExpr{} (Value _ n) | Just (Value _ bn) <- H.lookup (makeAbsoluteName n) bLiterals -> return bn
    Value _ n -> return n
    h -> guardErr $ printf "its input is applied to %s rather than to the name of a function" (show h)
  args <- forM (H.toList (exprAppliedArgsMap defInput)) $ \(k, v) -> case v of
    Just (_, Just argE) -> (k,) <$> argType argE
    _                   -> guardErr $ printf "its input gives %s no value" (show k)
  let input = (partialVal calleeName){ptArgs = H.fromList args}
  let arrowResult = getMetaType (getExprMeta resultE)
  let reached = [ d
                | items <- maybeToList (suffixLookupInDict calleeName tbResEnv)
                , (itemType, Just d@ObjArr{oaArr=Just (Just _, arrM)}, _) <- items
                , not $ isBottomType $ intersectTypes tbTypeEnv (singletonType input) (singletonType itemType)
                , isSubtypeOf tbTypeEnv (getMetaType arrM) arrowResult ]
  answer <- case reached of
    []  -> return Nothing
    [d] -> Just <$> defFunction d
    ds  -> guardErr $ printf "%s reaches several definitions, at %s, and it gives one" (show input) (show [ getMetaPos (getExprMeta (oaObjExpr d)) | d <- ds ])
  matchAnswer answer
  where
    guardErr :: String -> CRes a
    guardErr msg = CErr [MkCNote $ BuildTreeCErr (getMetaPos $ getExprMeta defInput) $ printf "The guard of %s matching the definition an arrow reaches cannot be answered for %s: %s" (show $ oaObjPath oa) (show srcType) msg]

    argType (Value _ n) | Just t <- listToMaybe [ t | (k, t) <- H.toList (ptArgs srcType), makeAbsoluteName (pkName k) == makeAbsoluteName n ] = return t
    argType (CExpr _ c) = return (constantType c)
    argType e = guardErr $ printf "the value %s it gives is neither an argument of the call nor a constant" (show e)

    -- A definition is the function of its parameters it is when each of them is only named. One
    -- that destructures or guards what it is given is one case of a function, not all of it.
    defFunction d@ObjArr{oaObj=Just dObj, oaArr=Just (Just dBody, _)}
      | all keyOnly (exprAppliedArgs dObj) && isNothing (exprWhereCond dObj) = return (H.keys (exprAppliedArgsMap dObj), dBody)
      | otherwise = guardErr $ printf "the definition at %s is not a function of its parameters alone" (show $ getMetaPos $ getExprMeta $ oaObjExpr d)
    defFunction d = guardErr $ printf "the definition %s has no body" (show $ oaObjPath d)
    keyOnly ObjArr{oaObj=Just k, oaArr=Just (Nothing, _)} = exprIsName k
    keyOnly _                                             = False

    -- The name a value of the pattern is resolved to, which is the name of its type.
    patName e = case maybeGetSingleton (getMetaType (getExprMeta (exprHead e))) of
      Just PartialType{ptName} -> Just ptName
      Nothing                  -> Nothing
    matchAnswer answer = case (pat, answer) of
      (Value{}, Nothing) | patName pat == Just (mkName ctMaybeNothing) -> return (Just mempty)
      (Value{}, Just _) | patName pat == Just (mkName ctMaybeNothing) -> return Nothing
      (TupleApply{}, _) | patName pat == Just (mkName ctMaybeSome) -> case (H.toList (exprAppliedArgsMap pat), answer) of
        ([(_, Just (_, Just (AliasExpr HoleExpr{} defFn)))], Just fn) | not (null (exprAppliedArgs defFn)) -> return $ Just mempty{bSubFns = H.singleton (inExprSingleton defFn) fn}
        ([(_, Just (_, Just (AliasExpr HoleExpr{} _)))], Nothing) -> return Nothing
        _ -> guardErr $ printf "its pattern %s is not Some(value@name)" (show pat)
      _ -> guardErr $ printf "its pattern %s is neither Some(value@name) nor Nothing" (show pat)

-- | Whether two names are the same name: a reference is written relative to where it is, and the
-- pattern and the body are written in different places.
sameName :: Name -> Name -> Bool
sameName a b = makeAbsoluteName a == makeAbsoluteName b || relativeNameMatches a b || relativeNameMatches b a

-- | The parameter a name refers to, of the given ones, by its name.
paramNamed :: [ArgName] -> Name -> Maybe ArgName
paramNamed params n = listToMaybe [ p | p <- params, makeAbsoluteName (pkName p) == makeAbsoluteName n ]

-- | Matches a body pattern against a function's body. The pattern names the parameters of the
-- function it is over by the keys of its slot (@patParams@), and the body names them as the lambda
-- does (@bodyParams@): one parameter is the one of the same key.
matchBody :: TBEnv -> [ArgName] -> [ArgName] -> BPExpr -> BPExpr -> Maybe Bindings
matchBody TBEnv{tbTypeEnv} patParams bodyParams = go
  where
    -- A value variable (@${fun}@): the alias of an untyped hole, matching a name the body uses
    -- that is not one of its parameters.
    go (AliasExpr (HoleExpr hm _) (Value _ n)) body@(Value _ bn)
      | getMetaType hm == PTopType = if isNothing (paramNamed bodyParams bn) then Just mempty{bLiterals=H.singleton (makeAbsoluteName n) body} else Nothing
    -- A literal variable: the alias of a typed hole, matching a constant of its type.
    go (AliasExpr (HoleExpr hm _) (Value _ n)) body@(CExpr _ c)
      | getMetaType hm /= PTopType && isSubtypeOf tbTypeEnv (constantType c) (getMetaType hm) = Just mempty{bLiterals=H.singleton (makeAbsoluteName n) body}
    -- A sub-function variable: the alias of a hole with the parameter it binds, matching any part.
    go (AliasExpr HoleExpr{} subFn) body
      | not (null (exprAppliedArgs subFn)) = Just mempty{bSubFns=H.singleton (inExprSingleton subFn) (bodyParams, body)}
    go (AliasExpr _ _) _ = Nothing
    go (CExpr _ c) (CExpr _ c') = if c == c' then Just mempty else Nothing
    go (Value _ n) (Value _ bn) = case (paramNamed patParams n, paramNamed bodyParams bn) of
      -- A parameter of the function is the body's parameter under the same key.
      (Just p, Just bp) -> if makeAbsoluteName (pkName p) == makeAbsoluteName (pkName bp) then Just mempty else Nothing
      (Nothing, Nothing) -> if sameName n bn then Just mempty else Nothing
      _ -> Nothing
    go pat@TupleApply{} body@TupleApply{} = do
      headBindings <- go (exprHead pat) (exprHead body)
      guard (all (isJust . oaObj) (exprAppliedArgs body))
      let patArgs = keyed (exprAppliedArgsMap pat)
          bodyArgs = keyed (exprAppliedArgsMap body)
      guard (H.keysSet patArgs == H.keysSet bodyArgs)
      fmap ((headBindings <>) . mconcat) $ forM (H.toList patArgs) $ \(k, patArg) -> case (patArg, H.lookup k bodyArgs) of
        (Just (_, Just patV), Just (Just (_, Just bodyV))) -> go patV bodyV
        _                                                  -> Nothing
    go _ _ = Nothing
    keyed = H.fromList . map (\(k, v) -> (makeAbsoluteName (pkName k), v)) . H.toList

-- | The arm with what its body patterns bound in place of their variables: a literal variable is the
-- constant it matched, and an application of a sub-function variable is the part of the body it
-- matched, given the application's arguments for its parameters. What it binds is then part of the
-- arm as written, so the arm is an ordinary one wherever it is built.
instantiate :: Bindings -> BPObjArr -> BPObjArr
instantiate Bindings{bLiterals, bSubFns} oa = oa{
  oaArr = fmap (\(e, m) -> (fmap subst e, m)) (oaArr oa),
  oaAnnots = map subst (oaAnnots oa)
  }
  where
    subst :: BPExpr -> BPExpr
    subst e@(Value _ n) = fromMaybe e (H.lookup (makeAbsoluteName n) bLiterals)
    subst e@(AliasExpr HoleExpr{} (Value _ n)) = fromMaybe e (H.lookup (makeAbsoluteName n) bLiterals)
    subst e@CExpr{} = e
    subst e@HoleExpr{} = e
    subst (AliasExpr b a) = AliasExpr (subst b) a
    subst (EWhere m b c) = EWhere m (subst b) (subst c)
    subst e@(TupleApply m (bm, be) arg)
      | Just (params, part) <- subFnApplied e = apply params part (H.mapMaybe (\v -> subst <$> (v >>= snd)) $ H.fromList [ (makeAbsoluteName (pkName k), v) | (k, v) <- H.toList (exprAppliedArgsMap e) ])
      | otherwise = TupleApply m (bm, subst be) (substArg arg)
    substArg (EAppArg a) = EAppArg a{oaArr = fmap (\(e, m) -> (fmap subst e, m)) (oaArr a)}
    substArg (EAppSpread a) = EAppSpread (subst a)
    substArg a@EAppVar{} = a

    -- An application of a sub-function variable, by the name and keys it is applied with.
    subFnApplied e = do
      n <- maybeExprPath e
      let keys = S.map (makeAbsoluteName . pkName) (H.keysSet (exprAppliedArgsMap e))
      listToMaybe [ v | (k, v) <- H.toList bSubFns, makeAbsoluteName (pkName k) == makeAbsoluteName n, S.map (makeAbsoluteName . pkName) (pkArgs k) == keys ]

    -- The part with each of its parameters given the argument for it.
    apply params part args = go part
      where
        go e@(Value _ n) = case [ v | p <- params, makeAbsoluteName (pkName p) == makeAbsoluteName n, Just v <- [H.lookup (makeAbsoluteName (pkName p)) args] ] of
          (v:_) -> v
          []    -> e
        go e@CExpr{} = e
        go e@HoleExpr{} = e
        go (AliasExpr b a) = AliasExpr (go b) a
        go (EWhere m b c) = EWhere m (go b) (go c)
        go (TupleApply m (bm, be) arg) = TupleApply m (bm, go be) $ case arg of
          EAppArg a    -> EAppArg a{oaArr = fmap (\(e, am) -> (fmap go e, am)) (oaArr a)}
          EAppSpread a -> EAppSpread (go a)
          a@EAppVar{}  -> a
