--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Desugarf.Expr
-- Copyright :  (c) Zach Kimberg 2022
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module desugars a 'RawPrgm' into a 'Prgm' by removing all
-- language features outside of a minimalist core set.
--------------------------------------------------------------------

module Syntax.Ct.Desugarf.Expr where


import           Data.Bifunctor          (Bifunctor (first), second)
import qualified Data.HashMap.Strict     as H
import           Data.Maybe
import           Text.Printf

import           CtConstants
import           Data.Char               (toLower)
import           Data.List               (isPrefixOf)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Builder
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm


desExpr :: PSExpr -> DesExpr
desExpr (CExpr m c) = CExpr m c
desExpr (Value m n) = Value m n
desExpr (HoleExpr m h) = HoleExpr m h
desExpr (AliasExpr b a) = AliasExpr (desExpr b) (desExpr a)
desExpr (EWhere m b a) = EWhere m (desExpr b) (desExpr a)
desExpr (TupleApply m (bm, be) arg) = TupleApply m (bm, desExpr be) arg'
  where
    arg' = case arg of
      EAppArg a    -> EAppArg $ mapTupleArgValue desExpr a
      a@EAppVar{}  -> a
      EAppSpread a -> EAppSpread $ desExpr a

data SemiDesMode
  = SDInput Bool -- Used for parsing input expressions and indicates it is in a method
  | SDOutput -- Used for parsing input expressions
  | SDType -- Used for parsing type expressions

mobjExprVaenv :: Maybe PObjExpr -> PVarArgMap
mobjExprVaenv = maybe H.empty exprVarArgs

semiDesExpr :: SemiDesMode -> Maybe PObjExpr -> PExpr -> PSExpr
semiDesExpr _ _ (RawCExpr m c) = CExpr m c
semiDesExpr _ _ (RawValue m n) = Value m n
semiDesExpr _ _ (RawHoleExpr m h) = HoleExpr m h
semiDesExpr _ _ RawMacroValue{} = error "Not yet implemented"
semiDesExpr sdm@(SDInput True) obj (RawTheExpr t) = semiDesExpr sdm obj t
semiDesExpr SDInput{} _ e@(RawTheExpr t) = Value (emptyMetaE e) tN
  where
    (_tM, tN) = desugarTheExpr t
-- The type spread @X..@ (a value given only the hole spread) is every value of @X@. A value spread
-- into it instead (@X(..c)@) is a call given @c@'s arguments.
semiDesExpr sdm obj (RawTupleApply _ (_, e@RawValue{}) [(True, RawObjArr{roaObj=Just RawHoleExpr{}})]) = e' `setExprMeta` m'
  where
    e' = semiDesExpr sdm obj e
    m = getExprMeta e'
    -- A name not typed yet is spread as itself: the spread of ⊤ is ⊤, which says nothing of the
    -- args, and @data((.._))@ then declared only the argument-less @()@.
    m' = mWithType (spreadType H.empty $ case getMetaType m of
                      PTopType -> maybe PTopType (singletonType . partialVal) (maybeExprPath e')
                      t        -> t) m
semiDesExpr sdm obj (RawAliasExpr base alias) = AliasExpr (semiDesExpr sdm obj base) (semiDesExpr sdm obj alias)
-- A condition refining a TYPE is written over the value of that type, which it names @$this@ or,
-- as what an arrow produces, @$out@ ('thisMacro', 'outMacro'). Nothing else binds either name:
-- the value is bound under the one spelling 'macroThisKey' wherever a value predicate is decided
-- ('Semantics.typeGraphPredExprHolds', 'Eval.evalPredExprOn'), the spelling the type-property
-- forms below desugar to directly. Left as written, the name was a relative value name nothing
-- resolved and the condition was never evaluated. A condition on a body or a pattern is over
-- the scope's own arguments and keeps its names.
semiDesExpr sdm obj (RawWhere m base cond) = EWhere m (semiDesExpr sdm obj base) cond'
  where
    cond0 = semiDesExpr SDOutput obj cond
    cond' = case sdm of
      SDType -> mapExprValues refinedValue cond0
      _      -> cond0
    -- The parser types a value by the name it is written with ('relTypeVal'), so the type is
    -- renamed with it.
    refinedValue (vm, n)
      | nameStr n `elem` [thisMacro, outMacro] = Value (mWithType (relTypeVal n') vm) n'
      | otherwise = Value vm n
      where n' = mkName macroThisKey
semiDesExpr sdm obj (RawTypeProp m base (TypePropProj t v)) = semiDesExpr sdm obj (RawWhere m base (rawVal operatorEq `applyRawArgs` [(Just $ partialKey (mkName operatorArgL), RawMethod (emptyMetaE v) (rawVal macroThisKey) (rawVal (nameStr t))), (Just $ partialKey (mkName operatorArgR), v)])) -- $this.even == v
-- A relation type property @base__rel(v)@ refines @base@ to the values for which the boolean
-- relation @rel@ holds against @v@ — e.g. @Int__gte(0)@ is @Int where $this.gte(r= 0)@. Mirrors the
-- projection case above, but the property is a relation applied to @v@ rather than a value compared
-- to it. The relation's operand is named with the right-operand key (@r@), the convention shared by
-- the binary comparison relations (@gte@/@lte@/@gt@/@lt@).
semiDesExpr sdm obj (RawTypeProp m base (TypePropRel t v)) = semiDesExpr sdm obj (RawWhere m base (RawMethod (emptyMetaE v) (rawVal macroThisKey) (rawVal (nameStr t) `applyRawArgs` [(Just $ partialKey (mkName operatorArgR), v)]))) -- $this.gte(r= v)
semiDesExpr sdm obj (RawTupleApply _ (_, RawValue _ "/operator::") [(False, RawObjArr{roaArr=(Just (Just e, _))}), (False, RawObjArr{roaArr=(Just (Just tp, _))})]) = semiDesExpr sdm obj (rawExprWithType (exprToType (mobjExprVaenv obj) tp) e)
semiDesExpr sdm obj (RawTupleApply _ (_, be) []) = semiDesExpr sdm obj be
-- A function declared in a context is written with the context before its arguments
-- (@println{io -> IO}(msg -> String)@), but runs as the call in the context
-- (@println(msg -> String){io -> IO}@): it is the call that the states are given to.
semiDesExpr sdm@SDInput{} obj (RawTupleApply m (bm, RawContextApply cm (cbm, f) ctxs) args) = semiDesExpr sdm obj (RawContextApply cm (cbm, RawTupleApply m (bm, f) args) ctxs)
-- A named spread in a pattern (@Context(value, ..c)@) names the whole matched value: the spread
-- stands for the fields the pattern does not list, and @c@ for the value carrying them. It is the
-- open pattern with the value aliased (@Context(value, .._)\@c@), so building a value from @c@
-- (@Context(..c, value= x)@) carries every field it has.
semiDesExpr sdm@SDInput{} obj (RawTupleApply m (bm, be) args)
  | (pre, (True, spreadArg) : post) <- break namedSpread args
  , Just alias <- spreadArgExpr spreadArg
  = semiDesExpr sdm obj (RawAliasExpr (RawTupleApply m (bm, be) (pre ++ (True, rawInObjArr True (RawHoleExpr (emptyMetaE alias) (HoleActive Nothing))) : post)) alias)
  where
    namedSpread (True, oa) = isJust (spreadArgExpr oa)
    namedSpread _          = False
    spreadArgExpr RawObjArr{roaObj=Just v@RawValue{}, roaArr=Nothing} = Just v
    spreadArgExpr _                                                   = Nothing
semiDesExpr sdm obj (RawTupleApply m'' (bm, be) args) = (`setExprMeta` m'') $ snd $ foldl aux (bm, be') (zip [0..] args)
  where
    be' = semiDesExpr sdm obj be
    prefixOperatorApply = case (be, args) of
      (RawValue _ n, [_]) -> operatorPrefix `isPrefixOf` nameStr n
      _                   -> False
    aux :: (ParseMeta, PSExpr) -> (Int, (Bool, PObjArr)) -> (ParseMeta, PSExpr)
    -- The bracket of a PREFIX operator's operand is the operator's own application syntax, not a
    -- grouping, so it is looked through before the operand is read. A prefix operator binds
    -- tighter than @::@, so its typed operand can only be written bracketed — @(-(_ :: $T))@ —
    -- and read with the bracket it missed the typed-operand case below: the operand became a
    -- value hole carrying the type (@a= Hole $T@) instead of an argument of that type
    -- (@a -> $T@), so nothing bound @$T@ from the argument and every call read the result as
    -- the variable's bound. Only there: elsewhere a written bracket is how an argument keeps a
    -- typed VALUE pattern (@ContextIn(value= (_ :: $V), .._)@ in core's @Context@), which the
    -- typed-operand case would turn into an argument declaration.
    aux (m, e) (argIndex, (False, rarg@RawObjArr{roaArr=Just (Just (RawParen inner), am)})) | prefixOperatorApply = aux (m, e) (argIndex, (False, rarg{roaArr=Just (Just inner, am)}))
    aux (m, e) (argIndex, (False, rarg@RawObjArr{roaArr=Just (Just (RawTupleApply _ (_, RawValue _ "/operator::") [(False, RawObjArr{roaArr=(Just newArr)}), (False, RawObjArr{roaArr=(Just (Just tp, _))})]), _)})) = case aux (m, e) (argIndex, (False, rarg{roaArr=Just newArr})) of
      -- A /named/ typed operand @l :: $T@ desugars like a function argument @l -> $T@: move the
      -- operand value into the object position with a type-only arrow slot, rather than leaving it
      -- as a binding expression in the arrow. This registers the operand as a variable argument
      -- ('exprVarArgs') so a definition body can reference it (e.g. @(l :: $T == r :: $T)= …l…@)
      -- without the operand resolving to its own name singleton. Anonymous operands (@_ :: $T@,
      -- used by operators that do not name their operands) keep the existing form.
      (tam, TupleApply tamm tab (EAppArg taoa@ObjArr{oaArr=Just (Just bindExpr@(Value _ _), tboaArrM)})) ->
        (tam, TupleApply tamm tab (EAppArg taoa{oaObj=Just bindExpr, oaArr=Just (Nothing, mWithType (exprToType (mobjExprVaenv obj) tp) tboaArrM)}))
      (tam, TupleApply tamm tab (EAppArg taoa@ObjArr{oaArr=Just (tboaArrE, tboaArrM)})) -> (tam, TupleApply tamm tab (EAppArg taoa{oaArr=Just (tboaArrE, mWithType (exprToType (mobjExprVaenv obj) tp) tboaArrM)}))
      _ -> error $ printf "Unexpected result in semiDesExpr TupleApply aux"
    aux (m, e) (argIndex, (False, rarg@RawObjArr{roaObj=Just (RawTheExpr v), roaArr})) = aux (m, e) (argIndex, (False, rarg{roaObj=Just (RawValue (emptyMetaE vM) vN), roaArr=roaArr'}))
      where
        (vM, vN) = desugarTheExpr v
        roaArr' = case roaArr of
          Just (arrE, _) -> Just (arrE, Just vM)
          Nothing        -> Just (Nothing, Just vM)
    aux (m, e) (_argIndex, (False, rarg)) = (emptyMetaM m'', TupleApply (emptyMetaM m'') (m, e) (EAppArg arg''))
      where
        arg = head $ desObjArr obj rarg

        -- indexAnnots = [exprVal argStartAnnot | argIndex == 0] ++ [exprVal argEndAnnot | argIndex == length args - 1]
        indexAnnots = [] -- TODO: Consider whether to re-add start and end annots

        -- Currently uses oaObj as "first and only expr"
        -- This disambiguates it between whether the only expression is an obj or an arr
        arg' = case (sdm, arg) of
          (SDOutput, ObjArr{oaObj, oaArr=Just (Nothing, _)}) -> arg{oaObj=Nothing, oaArr=Just (oaObj, emptyMetaE $ fromJust oaObj)}
          (_, _) -> arg

        -- SemiDes all sub-expressions
        objSdm = case sdm of
          SDInput{} -> sdm
          _         -> SDInput False
        arg'' = arg'{
          oaObj=fmap (semiDesExpr objSdm obj) (oaObj arg'),
          oaAnnots=indexAnnots ++ fmap (semiDesExpr sdm obj) (oaAnnots arg'),
          oaArr=fmap (first (fmap (semiDesExpr sdm (oaObj arg')))) (oaArr arg')
          }
    aux (m, e) (_, (True, oa)) = (emptyMetaM m'', TupleApply (emptyMetaM m'') (m, e) (EAppSpread $ semiDesExpr sdm obj $ oaObjExpr arg))
      where
        arg = head $ desObjArr obj oa
semiDesExpr sdm obj (RawVarsApply m be vs) = (`setExprMeta` m) $ foldr aux be' vs
  where
    be' = semiDesExpr sdm obj be
    aux RawObjArr{roaObj=Just varExpr, roaArr} base = TupleApply (emptyMetaM m) (emptyMetaE base, base) (EAppVar varName varVal)
      where
        varName = case fmap nameStr (maybeExprPath varExpr) of
              Just ('$':n) -> partialKey (mkName n)
              Just n     -> partialKey (mkName n)
              Nothing -> error $ printf "No type name found in varExpr %s (type %s)" (show varExpr) (show $ exprToType (mobjExprVaenv obj) varExpr)
        varVal = maybe emptyMetaN (exprToTypeMeta (mobjExprVaenv obj)) (roaArr >>= snd)
    aux roa _ = error $ printf "Unexpected semiDesExpr var: %s" (show roa)
-- A value given a context (@(){Counter(0)}@) is the 'ContextStr' holding it, its states the
-- anonymous tuple under 'contextStatesStr', each kept under the key of its type ('contextTypeKey').
-- A state given as a variable (@(){io}@), whose type is not written, is kept under the variable's
-- name, and one written with its key (@(){counter= c}@) under that key.
semiDesExpr sdm@SDOutput{} obj (RawContextApply m (_, be) ctxs) = (`setExprMeta` m) $ semiDesExpr sdm obj $ applyRawEArgs (RawValue emptyMetaN (mkName ContextStr)) [(Just $ rawVal contextValStr, be), (Just $ rawVal contextStatesStr, rawAnon `applyRawEArgs` map mapCtx ctxs)]
  where
    mapCtx RawObjArr{roaObj=Just k, roaArr=Just (Just v, _)} = (Just k, v)
    mapCtx RawObjArr{roaObj=Just v} = (Just $ rawVal $ fromMaybe (varKey v) (contextTypeKey v), v)
    mapCtx ctx = error $ printf "Invalid context value: %s" (show ctx)
    varKey (RawValue _ n) = nameStr n
    varKey v = error $ printf "A state given to a context has no type or name to keep it under: %s" (show v)
-- A context written as a type (@(){IO}@) is every 'ContextStr' whose value is of the type it is
-- given, holding a state of each type in the braces and any others besides. A state is kept under
-- the key of its type ('contextState'): @{IO}@ and @{out -> IO}@ are both the @io@ state.
semiDesExpr SDType obj (RawContextApply m (_, be) ctxs) = (`setExprMeta` m) $ semiDesExpr SDType obj $ RawTupleApply m (emptyMetaN, RawValue emptyMetaN (mkName ContextStr)) [stateArg (rawVal contextValStr) be, stateArg (rawVal contextStatesStr) states]
  where
    states = RawTupleApply m (emptyMetaN, rawAnon) (map mapCtx ctxs ++ [(True, rawInObjArr True (RawHoleExpr emptyMetaN (HoleActive Nothing)))])
    stateArg k t = (False, RawObjArr (Just k) ArgObj Nothing [] (Just (Nothing, Just t)) Nothing)
    mapCtx ctx@RawObjArr{roaObj=Just o, roaArr} | Just (k, _) <- contextState ctx = stateArg (rawVal k) (fromMaybe o (roaArr >>= snd))
    mapCtx ctx = error $ printf "Invalid context type: %s" (show ctx)

-- A function taking states (@inc{c= Counter(i)}@) is the call in a context holding them
-- (@ContextIn(value= inc, ctx= (counter= Counter(i)\@c, .._))@): each state is found by the key of
-- its type and bound under the name it is written with ('contextState'). The context may hold states
-- the function does not name. It answers with only its own, and the bind that called it keeps the rest.
semiDesExpr sdm@SDInput{} obj (RawContextApply m (_, be) ctxs) = (`setExprMeta` m) $ semiDesExpr sdm obj $ RawTupleApply m (emptyMetaN, RawValue emptyMetaN (mkName ContextInStr)) [stateArg (rawVal contextValStr) (Just (Just be, Nothing)), stateArg (rawVal contextStatesStr) (Just (Just states, Nothing))]
  where
    states = RawTupleApply m (emptyMetaN, rawAnon) (map mapCtx ctxs ++ [(True, rawInObjArr True (RawHoleExpr emptyMetaN (HoleActive Nothing)))])
    stateArg k arr = (False, RawObjArr (Just k) ArgObj Nothing [] arr Nothing)
    mapCtx ctx@RawObjArr{roaObj=Just o, roaArr} | Just (k, l) <- contextState ctx = case roaArr of
      -- @{c= Counter(i)}@: the pattern, naming the whole state where its key is not the name.
      Just (Just pat, _) -> stateArg (rawVal k) (Just (Just $ if k == l then pat else RawAliasExpr pat (rawVal l), Nothing))
      -- @{io -> IO}@, @{out -> IO}@: the argument, renamed where its key is not the name.
      Just (Nothing, mt) -> stateArg (if k == l then rawVal k else RawAliasExpr (rawVal k) (rawVal l)) (Just (Nothing, mt))
      -- @{IO}@ is a state of that type; @{io}@ one of any.
      Nothing -> stateArg (rawVal k) (Just (Nothing, o <$ contextTypeKey o))
    mapCtx ctx = error $ printf "Invalid input context: %s" (show ctx)
semiDesExpr sdm obj (RawParen e) = semiDesExpr sdm obj e
semiDesExpr sdm obj (RawMethod m (RawTheExpr n) method) = (`setExprMeta` m) $ semiDesExpr sdm' obj (method `applyRawIArgs` [(partialKey "this", IArgM (Just n'))]) -- Parse type methods like :Integer.toString, Only for input expressions
  where
    sdm' = case sdm of
      SDInput _ -> SDInput True
      _         -> sdm
    -- A BARE type receiver @:X@ is THE type: every value of it, which is the spread @X..@ (a
    -- bare name alone is the argument-less value, so @:Plain.nameOf@ over
    -- @data(Plain(name, other))@ would otherwise declare over a value no definition
    -- destructuring the fields could fulfil). Only the bare form: a receiver carrying a
    -- pattern (@:Cons[$T](head, tail)@) does not reach this clause, and a type VARIABLE
    -- receiver (@:$T.toString@) is already every value of its binding and has no name to
    -- spread. Built as the parser builds a written @X..@, with the base typed by its
    -- relation so the spread has something to widen.
    n' = case n of
      RawValue vm vn | isNothing (parseTVVar (nameStr vn)) ->
        RawTupleApply (emptyMetaM vm) (emptyMetaM vm, RawValue (mWithType (relTypeVal vn) vm) vn) [(True, rawInObjArr True (RawHoleExpr (emptyMetaM vm) (HoleActive Nothing)))]
      _ -> n
semiDesExpr sdm obj (RawMethod m base method) = (`setExprMeta` m) $ semiDesExpr sdm' obj $ applyRawArgs method [(Just $ partialKey "this", base)]
  where
    sdm' = case sdm of
      SDInput _ -> SDInput True
      _         -> sdm
semiDesExpr sdm obj (RawList m []) = semiDesExpr sdm obj (RawValue m (mkName ctListNil))
semiDesExpr sdm obj (RawList m (l:ls)) = semiDesExpr sdm obj (RawValue m (mkName ctListCons) `applyRawArgs` [(Just $ partialKey (mkName ctListConsHead), l), (Just $ partialKey (mkName ctListConsTail), RawList (emptyMetaM m) ls)])
semiDesExpr _ _ e = error $ printf "Not yet implemented semiDesExpr for %s" (show e)

-- | The expression with every value rewritten: the values applied, aliased, conditioned on and
-- passed as argument values, not the names arguments are passed under.
mapExprValues :: ((Meta m, TypeName) -> Expr m) -> Expr m -> Expr m
mapExprValues f = go
  where
    go (Value m n)                 = f (m, n)
    go e@CExpr{}                   = e
    go e@HoleExpr{}                = e
    go (AliasExpr b a)             = AliasExpr (go b) (go a)
    go (EWhere m b c)              = EWhere m (go b) (go c)
    go (TupleApply m (bm, be) app) = TupleApply m (bm, go be) (goApp app)
    goApp (EAppArg oa)   = EAppArg oa{oaArr=fmap (first (fmap go)) (oaArr oa)}
    goApp (EAppSpread e) = EAppSpread (go e)
    goApp a@EAppVar{}    = a

-- | A condition in a type position of the arrow that applies an argument by position
-- (@Nat | isZero($out)@), if there is one: in the result type or an argument's type, read
-- through brackets, applications and the types' own refinements. A condition on a type is kept
-- as the single partial of a 'PredExpr', which names its arguments. The argument a positional
-- value fills is inferred by the type checker ('TypeCheck.Encode', @AddInferArg@) for an
-- expression it encodes, which a type's condition is not, so the position cannot be named here;
-- the condition is refused ('Syntax.Ct.Desugarf.desDecl') rather than built without the value.
rawTypeCondPositional :: PObjArr -> Maybe PExpr
rawTypeCondPositional roa = listToMaybe $ filter positional $ concatMap typeConds (maybeToList (roaArr roa >>= snd)) ++ maybe [] objConds (roaObj roa)
  where
    -- The conditions in the types of an object's arguments. The object's own condition is the
    -- arrow's guard, over its arguments, and is encoded as an expression.
    objConds (RawTupleApply _ (_, b) args)   = objConds b ++ concatMap (argConds . snd) args
    objConds (RawVarsApply _ b vs)           = objConds b ++ concatMap argConds vs
    objConds (RawContextApply _ (_, b) args) = objConds b ++ concatMap argConds args
    objConds (RawParen e)                    = objConds e
    objConds (RawWhere _ b _)                = objConds b
    objConds (RawMethod _ b e)               = objConds b ++ objConds e
    objConds _                               = []
    argConds arg = concatMap typeConds (maybeToList (roaArr arg >>= snd)) ++ maybe [] objConds (roaObj arg)
    -- The conditions a type expression is refined by, at its root and in its arguments' types.
    typeConds (RawWhere _ b c)             = typeConds b ++ [c]
    typeConds (RawParen e)                 = typeConds e
    typeConds (RawTupleApply _ (_, b) args) = typeConds b ++ concatMap (argConds . snd) args
    typeConds (RawVarsApply _ b vs)        = typeConds b ++ concatMap argConds vs
    typeConds (RawTypeProp _ b _)          = typeConds b
    typeConds _                            = []
    -- Whether a condition applies an argument by position anywhere within it.
    positional (RawTupleApply _ (_, b) args) = positional b || any positionalApplied args
    positional (RawParen e)                  = positional e
    positional (RawMethod _ b e)             = positional b || positional e
    positional (RawWhere _ b c)              = positional b || positional c
    positional (RawAliasExpr b a)            = positional b || positional a
    positional (RawVarsApply _ b _)          = positional b
    positional (RawList _ ls)                = any positional ls
    positional _                             = False
    positionalApplied (True, _)                                           = False -- a spread passes its values under their names
    positionalApplied (False, RawObjArr{roaObj=Just _, roaArr=Nothing})   = True
    positionalApplied (False, RawObjArr{roaObj=Nothing})                  = True
    positionalApplied (False, RawObjArr{roaArr=Just (Just v, _)})         = positional v
    positionalApplied _                                                   = False

-- | Desugars a "TheExpr type" by applying the type to a default name
desugarTheExpr :: PExpr -> (PExpr, Name)
desugarTheExpr t = (t, mkName defaultName)
  where
    t' = exprToType H.empty t
    n' = fromMaybe (error $ printf "desugarTheExpr can't get name from %s" (show t')) $ maybeGetTypeName t'
    defaultName = lowerCaseFirstLetter (nameStr n')
    lowerCaseFirstLetter (n:ns) = toLower n : ns
    lowerCaseFirstLetter ns     = ns

exprToPartialType :: PExpr -> PartialType
exprToPartialType e = case maybeGetSingleton $ getExprType des of
    Just t' -> t'
    Nothing -> case getExprType des of
      TopType _ (PredsOne (PredRel n)) -> n
      t' -> error $ printf "Failed exprToPartialType with type %s in expr %s" (show t') (show des)
  where
    des = exprPropagateTypes $ desExpr $ semiDesExpr SDType Nothing e

exprToType :: PVarArgMap -> PExpr -> Type
exprToType _ (RawValue _ n) | isJust (parseTVVar (nameStr n)) = fromJust $ parseTVVar (nameStr n)
-- TODO Migrate the TypeVar check to the one below and remove the one above. This is for having type variables without the $ prefix.
-- exprToType vaenv (RawValue _ n) | H.member (TVVar $ partialKey $ makeAbsoluteName n) vaenv  = TypeVar (TVVar $ partialKey n) TVInt
-- An applied type variable @$F[$T -> $_B]@: whatever @$F@ resolves to, with the named
-- parameter slots rebound ('TVApplied'). The var-name extraction mirrors 'semiDesExpr's
-- 'RawVarsApply' handling for ordinary class applications.
exprToType vaenv (RawVarsApply _ (RawValue _ n) vs) | isJust (parseTVVar (nameStr n)) =
  case fromJust $ parseTVVar (nameStr n) of
    TypeVar bAux loc i -> TypeVar (TVApplied bAux (H.fromList $ mapMaybe toOverride vs)) loc i
    t -> error $ printf "parseTVVar returned a non-var %s for applied type var %s" (show t) (nameStr n)
  where
    toOverride RawObjArr{roaObj=Just varExpr, roaArr} = (varName,) . exprToType vaenv <$> (roaArr >>= snd)
      where
        varName = case fmap nameStr (maybeExprPath varExpr) of
          Just ('$':vn) -> partialKey (mkName vn)
          Just vn       -> partialKey (mkName vn)
          Nothing       -> error $ printf "No var name found in applied type var %s" (show varExpr)
    toOverride roa = error $ printf "Unexpected var application in applied type var: %s" (show roa)
-- A written intersection @A && B@ ('Syntax.Ct.Parser.Expr.pTypeTerm'): the values in both.
exprToType vaenv (RawTupleApply _ (_, RawValue _ op) [(False, RawObjArr{roaObj=Just (RawValue _ ln), roaArr=Just (Just l, _)}), (False, RawObjArr{roaObj=Just (RawValue _ rn), roaArr=Just (Just r, _)})])
  | nameStr op == operatorName "&&", nameStr ln == operatorArgL, nameStr rn == operatorArgR = intersectWrittenTypes (exprToType vaenv l) (exprToType vaenv r)
exprToType _ e                        = getExprType $ exprPropagateTypes $ desExpr $ semiDesExpr SDType Nothing e

-- | The type a written @A && B@ denotes. Names are not resolved yet, so each side is still the
-- relation its name denotes ('relTypeVal'), or an intersection already: the result is ⊤ filtered
-- by the predicates of both, which name resolution reads part by part
-- ('Syntax.Ct.Desugarf.Passes').
intersectWrittenTypes :: Type -> Type -> Type
intersectWrittenTypes (TopType n1 p1) (TopType n2 p2) = TopType (joinUnionType (splitUnionType n1 ++ splitUnionType n2)) (predsAnd p1 p2)
intersectWrittenTypes a b = error $ printf "An intersection (&&) of types can only join named types, not %s and %s" (show a) (show b)

exprToTypeMeta :: PVarArgMap -> PExpr -> ParseMeta
exprToTypeMeta vaenv e = mWithType (exprToType vaenv e) (getExprMeta e)

desFileImport :: RawFileImport -> FileImport
desFileImport rawImp = rawImp{
    impRaw=semiDesExpr SDOutput Nothing $ impRaw rawImp,
    impAbs=semiDesExpr SDOutput Nothing $ impAbs rawImp
  }

desObjArr :: Maybe PObjExpr -> PObjArr -> [ObjArr RawExpr ParseMetaDat]
desObjArr mainObj (RawObjArr obj basis@TypeObj doc annots arr Nothing) = [ObjArr obj basis doc annots arr']
  where
    arr' = fmap (second (maybe emptyMetaN (exprToTypeMeta (mobjExprVaenv mainObj)))) arr
desObjArr mainObj (RawObjArr obj@(Just objExpr) basis doc annots arr Nothing) = [ObjArr obj basis doc annots (Just arr')]
  where
    arr' = maybe (Nothing, emptyMetaE objExpr) (second (maybe emptyMetaN (exprToTypeMeta (mobjExprVaenv mainObj)))) arr
desObjArr mainObj (RawObjArr obj basis doc annots (Just (arrE, arrM)) Nothing) = [ObjArr obj basis doc annots (Just (arrE, maybe emptyMetaN (exprToTypeMeta (mobjExprVaenv mainObj)) arrM))]
desObjArr _ roa = error $ printf "Not yet implemented: %s" (show roa)
