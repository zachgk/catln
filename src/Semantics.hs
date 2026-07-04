--------------------------------------------------------------------
-- |
-- Module    :  Semantics
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module contains miscellaneous and general syntax elements.
-- It also contains a the instances of program metadata and some
-- general syntax-based utilities.
--------------------------------------------------------------------

module Semantics where

import qualified Data.HashMap.Strict as H

import           CtConstants         (operatorHasArrow)
import           Data.Graph          (graphFromEdges)
import qualified Data.HashSet        as S
import           Data.Maybe
import           Semantics.Prgm
import           Semantics.Types
import           System.Directory    (doesDirectoryExist)
import           System.IO.Unsafe    (unsafePerformIO)
import           Text.Printf

newtype ObjArrTypeGraph e m = ObjArrTypeGraph (ObjectMap e m)
  deriving (Show)
instance Semigroup (ObjArrTypeGraph e m) where
  (ObjArrTypeGraph a) <> (ObjArrTypeGraph b) = ObjArrTypeGraph (a <> b)
instance (ExprClass e, MetaDat m, Show m, Show (e m)) => TypeGraph (ObjArrTypeGraph e m) where
  typeGraphQueryWithReason typeEnv@TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} vaenv partial@PartialType{ptName} = concatMap tryTArrow $ flatObjectMapItem $ H.lookupDefault mempty ptName tg
    where
      tryTArrow :: (ExprClass e, MetaDat m, Show m, Show (e m)) => ObjArr e m -> [(String, Type)]
      tryTArrow oa@ObjArr{oaArr=Just{}} = do
        -- It is possible to send part of a partial through the arrow, so must compute the valid part
        -- If none of it is valid, then there is Nothing
        case intersectTypesEnv typeEnv vaenv (singletonType partial) (getMetaType $ getExprMeta $ oaObjExpr oa) of
          potentialSrc@(UnionType Nothing potSrcLeafs []) -> if not (isBottomType potentialSrc)
            then map (printf "using %s" (show oa),) $ joinPartialDestTypes [arrowDestType typeEnv potentialSrcPartial oa | potentialSrcPartial <- splitUnionType potSrcLeafs]
            else []
          _ -> error "Unexpected non-union in TryTArrow"
      tryTArrow ObjArr{oaArr=Nothing} = []

      joinPartialDestTypes :: [[Type]] -> [Type]
      joinPartialDestTypes [] = [BottomType]
      joinPartialDestTypes (pdt1:restPdt) = [unionTypes typeEnv pdt1Poss restPdt' | pdt1Poss <- pdt1, restPdt' <- joinPartialDestTypes restPdt]

 -- TODO: Implement this to expand other PredExprs as well, and begin using the Nothing to represent having no meaningful way to break
  typeGraphExpandPredExpr typeEnv _vaenv partial | nameStr (ptName partial) == operatorHasArrow = Just $ map (\p -> p{ptPreds=PredsOne (PredExpr partial)}) $ topTypeAsPartials typeEnv
  typeGraphExpandPredExpr typeEnv _vaenv partial = Just $ map (\p -> p{ptPreds=PredsOne (PredExpr partial)}) $ topTypeAsPartials typeEnv

  typeGraphData TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} name = concatMap dataPartials $ flatObjectMapItem $ H.lookupDefault mempty name tg
    where
      -- Data definitions have no arrow (oaArr=Nothing); their object expression carries the
      -- declared type with linked ptVars/ptArgs.
      dataPartials oa@ObjArr{oaArr=Nothing} = case getMetaType $ getExprMeta $ oaObjExpr oa of
        UnionType Nothing leafs _ -> splitUnionType leafs
        _                         -> []
      dataPartials ObjArr{oaArr=Just{}} = []

newtype CTSSConfig = CTSSConfig {
  -- TODO Replace stack path with (CTSSStackConfig = CTSSStackTest | CTSSStackBuild DirString BranchString) when adding support for auto-downloading core
  ctssStackPath :: String
                                }

testCtssConfig, buildCtssConfig :: CTSSConfig
testCtssConfig = CTSSConfig "stack"
-- | The standard-library location used by the CLI/LSP/WebDocs. During development
-- the compiler is run from a worktree (e.g. via @stack exec@) whose working directory
-- contains a local @stack/@ standard library; prefer it so multiple worktrees can be
-- developed and run in parallel without colliding on the single shared installed copy.
-- Only an installed compiler — run where no local @stack/core@ exists — falls back to
-- the @~/.catln/stack@ copy populated at install time. Resolved once (NOINLINE) from the
-- process's working directory, which is fixed for the run.
buildCtssConfig = unsafePerformIO $ do
  let localPath = ctssStackPath testCtssConfig
  localExists <- doesDirectoryExist (localPath ++ "/core")
  return $ if localExists then CTSSConfig localPath else CTSSConfig "~/.catln/stack"
{-# NOINLINE buildCtssConfig #-}

mkTypeEnv :: (ExprClass e, Show m, Show (e m), MetaDat m) => Prgm e m -> TypeEnv (ObjArrTypeGraph e m)
mkTypeEnv (Prgm objMap classGraph _) = TypeEnv classGraph (ObjArrTypeGraph objMap) typeNames False defaultTypeEnvDebug False False (mkSatCache classGraph typeNames)
  where
    typeNames = S.fromList $ map makeAbsoluteName $ concatMap getAllObjArrNames $ flatObjectMap objMap

emptyMetaM :: (MetaDat m) => Meta m -> Meta m
emptyMetaM m = emptyMetaN{getMetaPos=getMetaPos m}

emptyMetaE :: (ExprClass e, MetaDat m) => e m -> Meta m
emptyMetaE e = emptyMetaM (getExprMeta e)

exprWithMeta :: (Show m) => Meta m -> Expr m -> Expr m
exprWithMeta m (CExpr _ c)        = CExpr m c
exprWithMeta m (Value _ n)        = Value m n
exprWithMeta m (HoleExpr _ h)     = HoleExpr m h
exprWithMeta m (EWhere _ base cond) = EWhere m base cond
exprWithMeta m (TupleApply _ b a) = TupleApply m b a
exprWithMeta _ e                  = error $ printf "exprWithMeta with unsupported expr %s" (show e)

-- | Returns the EWhere conditions in an expression
exprWhereConds :: Expr m -> [Expr m]
exprWhereConds CExpr{} = []
exprWhereConds Value{} = []
exprWhereConds HoleExpr{} = []
exprWhereConds (AliasExpr b _) = exprWhereConds b
exprWhereConds (EWhere _ _ c) = [c]
exprWhereConds (TupleApply _ (_, b) (EAppArg ObjArr{oaArr=Nothing})) = exprWhereConds b
exprWhereConds (TupleApply _ (_, b) (EAppArg ObjArr{oaArr=Just (me, _)})) = exprWhereConds b ++ maybe [] exprWhereConds me
exprWhereConds (TupleApply _ (_, b) EAppVar{}) = exprWhereConds b
exprWhereConds (TupleApply _ (_, b) (EAppSpread a)) = exprWhereConds b ++ exprWhereConds a

exprWithMetaType :: (Show m) => Type -> Expr m -> Expr m
exprWithMetaType t e = exprWithMeta (mWithType t (getExprMeta e)) e

classGraphFromObjs :: (ExprClass e, MetaDat m, Show m, Show (e m)) => ObjectMap e m -> ClassGraph
classGraphFromObjs objMap = ClassGraph $ graphFromEdges $ map (\oa -> (CGType, PTypeName (oaObjPath oa), [])) $ flatObjectMap objMap

mapExprPath :: (Show m) => ((Meta m, TypeName) -> Expr m) -> Expr m -> Expr m
mapExprPath f (Value m n) = f (m, n)
mapExprPath f (EWhere m b c) = EWhere m (mapExprPath f b) c
mapExprPath f (TupleApply m (bm, be) a) = TupleApply m (bm, mapExprPath f be) a
mapExprPath _ e = error $ printf "Unexpected expr to mapExprPath: %s" (show e)

mapOAObjExpr :: (MetaDat m, ExprClass e, Show (e m)) => (e m -> e m) -> ObjArr e m -> ObjArr e m
mapOAObjExpr f oa@ObjArr{oaObj=Just e}  = oa{oaObj = Just (f e)}
mapOAObjExpr _ oa@ObjArr{oaObj=Nothing} = oa

mapOAArrExpr :: (MetaDat m, ExprClass e, Show (e m)) => (e m -> e m) -> ObjArr e m -> ObjArr e m
mapOAArrExpr f oa@ObjArr{oaArr=Just (Just e, m)} = oa{oaArr = Just (Just (f e), m)}
mapOAArrExpr _ oa                           = oa

exprVarArgsWithSrcs :: (TypeGraph tg, MetaDat m, Show m) => TypeEnv tg -> [(Expr m, PartialType)] -> PossArgMetaMapWithSrc m
exprVarArgsWithSrcs typeEnv os = joinAllPossVarArgMetaWithSrc typeEnv (map (uncurry (exprVarArgsWithSrc typeEnv)) os)

exprVarArgsWithObjSrcs :: (TypeGraph tg, MetaDat m, Show m) => TypeEnv tg -> [(PartialType, ObjArr Expr m)] -> PossArgMetaMapWithSrc m
exprVarArgsWithObjSrcs typeEnv os = exprVarArgsWithSrcs typeEnv $ map (\(src, obj) -> (oaObjExpr obj, src)) os

arrowDestType :: (TypeGraph tg, ExprClass e, Show m, Show (e m), MetaDat m) => TypeEnv tg -> PartialType -> ObjArr e m -> [Type]
arrowDestType typeEnv src oa@ObjArr{oaArr=Just (oaArrExpr, oaM)} = map (arrowDestTypeForVaenv . fmap snd) $ exprVarArgsWithSrc typeEnv (oaObjExpr oa) srcPartial
  where
    srcPartial = case maybeGetSingleton $ substituteVars $ singletonType src of
      Just s  -> s
      Nothing -> error $ printf "Non-singleton in arrowDestType"
    arrowDestTypeForVaenv vaenv = case oaArrExpr of
      -- _ | trace (printf "arrowDestType from %s with %s to %s.\n\t\tVanev: %s" (show src) (show oa) (show joined) (show vaenv)) False -> undefined
      Just n | isJust (maybeExprPath n) -> maybe joinedResolved substitute (H.lookup (TVArg $ inExprSingleton n) vaenv)
      _                              -> joinedResolved
      where
        substitute = substituteWithVarArgEnv vaenv
        expr' = fmap (substitute . getExprType) oaArrExpr
        arr' = substitute $ getMetaType oaM
        joined = fromMaybe arr' expr'
        -- Only rewrite a body that directly CONSTRUCTS a data value (@Cons(head=x)@): its applied
        -- arguments are the constructor's fields. A function/operator body (@a && b@, @f(a=..)@) has
        -- applied arguments that are the callee's parameters, not the result's fields, so leave it.
        isDataConstruction = case maybeExprPath =<< oaArrExpr of
          Just n  -> not (null (typeGraphData typeEnv n))
          Nothing -> False
        joinedResolved | isDataConstruction = applyResolvedFields typeEnv resolvedFields joined
                       | otherwise          = joined
        -- A definition body that constructs a value — @Cons(head=x)@ — stores its fields with the
        -- body's (abstract) argument types: @head@ becomes Top because @x@ is abstract in the body.
        -- At dispatch the vaenv binds the argument (@x=Integer@), so resolve each field whose value
        -- is an argument reference through the vaenv, recovering @head=Integer@ (and hence the data
        -- parameter it defines — see 'applyResolvedFields'). This is the per-field analogue of the
        -- bare-path body case above (@= v@).
        resolvedFields = H.fromList
          [ (inExprSingleton (oaObjExpr a), fv)
          | Just b <- [oaArrExpr]
          , a <- exprAppliedArgs b
          , Just (Just valExpr, _) <- [oaArr a]
          -- Only a bare argument reference (@head=x@); an application (@head=fn(v=x)@) names a
          -- function whose result — not the function itself — is the field, so leave it.
          , isJust (maybeExprPath valExpr)
          , null (exprAppliedArgs valExpr)
          , Just fv <- [H.lookup (TVArg (inExprSingleton valExpr)) vaenv] ]
arrowDestType _ _ oa@ObjArr{oaArr=Nothing} = error $ printf "Unexpected call to arrowDestType with type obj (which has no arrow to find the dest type of): %s" (show oa)

-- | Set resolved field values on a constructed type and bind the data parameters those fields
-- define. For @Cons(head=x)@ resolved to @head=Integer@, the Cons declaration links @head -> $T@,
-- so this both sets @/head=Integer@ and binds the element parameter @/T=Integer@. A field whose
-- declared type is not a bare parameter is set without binding.
applyResolvedFields :: (TypeGraph tg) => TypeEnv tg -> H.HashMap ArgName Type -> Type -> Type
applyResolvedFields _       fields t | H.null fields = t
applyResolvedFields typeEnv fields (UnionType Nothing leafs consts) =
  UnionType Nothing (joinUnionType (map applyLeaf (splitUnionType leafs))) consts
  where
    applyLeaf p = foldr applyField p (H.toList fields)
    applyField (fieldName, fieldType) p@PartialType{ptName, ptVars} =
      let declFieldType = case mapMaybe (H.lookup fieldName . ptArgs) (typeGraphData typeEnv ptName) of
            (t:_) -> Just t
            []    -> Nothing
      in case declFieldType of
           Just (TypeVar (TVVar v) TVInt) ->
             p{ ptVars = H.insertWith (intersectTypes typeEnv) v fieldType ptVars
              , ptArgs = H.insert fieldName fieldType (ptArgs p) }
           _ -> p{ptArgs = H.insert fieldName fieldType (ptArgs p)}
applyResolvedFields _ _ t = t

metaTypeVar :: Meta m -> Maybe TypeVarAux
metaTypeVar m = case getMetaType m of
  TypeVar v _ -> Just v
  _           -> Nothing

isSubtypeOfWithObjSrcs :: (TypeGraph tg, Show m, MetaDat m) => TypeEnv tg -> [(PartialType, ObjArr Expr m)] -> Type -> Type -> Bool
isSubtypeOfWithObjSrcs typeEnv objSrcs subType supType = any (\vaenv -> isSubtypeOfWithEnv typeEnv (fmap snd vaenv) subType supType) (exprVarArgsWithObjSrcs typeEnv objSrcs)
