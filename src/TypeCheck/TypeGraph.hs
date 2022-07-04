--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.TypeGraph
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module handles the type graph during type checking.
-- It mainly supports the 'ArrowTo' constraint to compute
-- what resulting types can be produced by using all of the rewrite
-- rules that are in scope. This module also computes what the Any
-- types are which result from joining the types of all objects.
--------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TupleSections    #-}

module TypeCheck.TypeGraph where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Maybe

import           Data.List           (partition)
import           Parser.Syntax       (emptyMetaN)
import           Syntax
import           Syntax.Prgm
import           Syntax.Types
import           Text.Printf
import           TypeCheck.Common
import           TypeCheck.Show
import           Utils

-- |
-- The object precedence is used to avoid increasing the scope of objects accidentally.
-- For example, if a data type is defined, functions using that data type shouldn't change the valid arguments to it
-- Especially, not specifying bounds should not turn them into TopType.
-- Similarly, matches or patterns are less effective then functions.
-- TODO May need to differentiate top level of functions from inner levels
objectPrecedence :: ObjectMapItem e m -> [Int]
objectPrecedence (Object{objBasis=TypeObj}, _, _)=    [1]
objectPrecedence (Object{objBasis=FunctionObj}, _, arrs) = [2, declDef]
  where
    declDef = if any hasDefinition arrs
      then 2 -- Definition objects have priority [2,2]
      else 1 -- Declaration objects have priority [2,1], better than definitions
    hasDefinition (Arrow _ _ (Just _)) = True
    hasDefinition (Arrow _ _ Nothing)  = False
objectPrecedence (Object{objBasis=PatternObj}, _, _) = [3]
objectPrecedence (Object{objBasis=MatchObj}, _, _) =   [4]

-- | Gets an object and all sub-ojects (recursively) from it's arguments
getRecursiveObjs :: ObjectMapItem e m -> ObjectMap e m
getRecursiveObjs (obj@Object{objArgs}, annots, arr) = (obj, annots, arr) : subObjMap
  where
    subObjMap = concatMap (filter notMatchObj . concatMap (getRecursiveObjs . (,[],Nothing)) . maybeToList . snd) (H.elems objArgs)
    notMatchObj (Object{objBasis}, _, _) = objBasis /= MatchObj

-- | This creates 'feUnionAllObjs' and adds it to the 'FEnv'
addUnionObjToEnv :: FEnv -> VObjectMap -> TObjectMap -> FEnv
addUnionObjToEnv env1@FEnv{feClassGraph} vobjMap tobjMap = do
  let vobjMapRec = concatMap getRecursiveObjs vobjMap
  let tobjMapRec = concatMap getRecursiveObjs tobjMap

  -- Finds the best precedence for all each object name
  let buildPrecedenceMap = fmap (minimum . map objectPrecedence) . H.fromListWith (++) . map (\(obj@Object{objPath}, annots, arrs) -> (objPath, [(obj, annots, arrs)]))
  let vPrecedenceMap = buildPrecedenceMap vobjMapRec
  let tPrecedenceMap = buildPrecedenceMap tobjMapRec
  let precedenceMap = H.unionWith min vPrecedenceMap tPrecedenceMap

  -- Filter the objects to only those with the best precedence
  let filterBestPrecedence = filter (\omi@(Object{objPath}, _, _) -> objectPrecedence omi == H.lookupDefault (error "Could not find obj in union") objPath precedenceMap)
  let vobjs' = map fst3 $ filterBestPrecedence vobjMapRec
  let tobjs' = map fst3 $ filterBestPrecedence tobjMapRec

  -- Builds vars to use for union and union powerset
  let (unionAllObjs, env2) = fresh env1 $ TypeCheckResult [] $ SType TopType bottomType "unionAllObjs"
  let (unionAllObjsPs, env3) = fresh env2 $ TypeCheckResult [] $ SType TopType bottomType "unionAllObjsPs"

  -- Build a variable to store union of tobjs
  let typecheckedAllType = unionAllTypes feClassGraph $ map (getMetaType . objM) tobjs'
  let (typecheckedAllObjs, env4) = fresh env3 $ TypeCheckResult [] $ SType typecheckedAllType bottomType "typecheckedAll"
  let typecheckedAllObjs' = VarMeta typecheckedAllObjs emptyMetaN Nothing

  -- Builds metas to use for union and union powerset
  let unionAllObjs' = VarMeta unionAllObjs emptyMetaN Nothing
  let unionAllObjsPs' = VarMeta unionAllObjsPs emptyMetaN Nothing

  let constraints = [
        UnionOf unionAllObjs' (typecheckedAllObjs' : map objM vobjs'),
        PowersetTo unionAllObjs' unionAllObjsPs'
        ]
  let env5 = (\env -> env{feUnionAllObjs=unionAllObjsPs'}) env4
  addConstraints env5 constraints


inferArgFromPartial :: FEnv -> PartialType -> Type
inferArgFromPartial FEnv{feVTypeGraph, feTTypeGraph, feClassGraph} partial@PartialType{ptName=PTypeName name, ptArgs} = do
  let vtypeArrows = H.lookupDefault [] name feVTypeGraph
  let vTypes = unionAllTypes feClassGraph $ map tryArrow vtypeArrows

  let ttypeArrows = H.lookupDefault [] name feTTypeGraph
  let tTypes = unionAllTypes feClassGraph $ map tryArrow ttypeArrows

  unionTypes feClassGraph vTypes tTypes
  where
    tryArrow (Object{objArgs}, _) = if H.keysSet ptArgs `isSubsetOf` H.keysSet objArgs
      then UnionType $ joinUnionType $ map addArg $ S.toList $ S.difference (H.keysSet objArgs) (H.keysSet ptArgs)
      else bottomType
    addArg arg = partial{ptArgs=H.insertWith (unionTypes feClassGraph) arg TopType ptArgs}
inferArgFromPartial _ _ = bottomType

isTypeVar :: Type -> Bool
isTypeVar TypeVar{} = True
isTypeVar _         = False

data ReachesTree
  = ReachesTree (H.HashMap PartialType ReachesTree)
  | ReachesLeaf [Type]
  deriving (Show)

unionReachesTree :: ClassGraph -> ReachesTree -> Type
unionReachesTree classGraph (ReachesTree children) = do
  let (keys, vals) = unzip $ H.toList children
  let keys' = UnionType $ joinUnionType keys
  let vals' = map (unionReachesTree classGraph) vals
  let both = keys':vals'
  case partition isTypeVar both of
    ([onlyVar], []) -> onlyVar
    (_, sums)       -> unionAllTypes classGraph sums
unionReachesTree classGraph (ReachesLeaf leafs) = unionAllTypes classGraph leafs

joinReachesTrees :: ReachesTree -> ReachesTree -> ReachesTree
joinReachesTrees (ReachesTree a) (ReachesTree b) = ReachesTree $ H.unionWith joinReachesTrees a b
joinReachesTrees (ReachesLeaf a) (ReachesLeaf b) = ReachesLeaf (a ++ b)
joinReachesTrees _ _ = error "joinReachesTrees for mixed tree and leaf not yet defined"

joinAllReachesTrees :: Foldable f => f ReachesTree -> ReachesTree
joinAllReachesTrees = foldr1 joinReachesTrees

isSubtypePartialOfWithMaybeObj :: (Meta m) => ClassGraph -> Maybe (Object m) -> PartialType -> Type -> Bool
isSubtypePartialOfWithMaybeObj classGraph (Just obj) = isSubtypePartialOfWithObj classGraph obj
isSubtypePartialOfWithMaybeObj classGraph Nothing    = isSubtypePartialOf classGraph

reachesHasCutSubtypeOf :: (Meta m) => ClassGraph -> Maybe (Object m) -> ReachesTree -> Type -> Bool
reachesHasCutSubtypeOf classGraph mObj (ReachesTree children) superType = all childIsSubtype $ H.toList children
  where childIsSubtype (key, val) = isSubtypePartialOfWithMaybeObj classGraph mObj key superType || reachesHasCutSubtypeOf classGraph mObj val superType
reachesHasCutSubtypeOf classGraph mObj (ReachesLeaf leafs) superType = any (\t -> isSubtypeOfWithMaybeObj classGraph mObj t superType) leafs

reachesPartial :: FEnv -> PartialType -> TypeCheckResult ReachesTree
reachesPartial env@FEnv{feVTypeGraph, feTTypeGraph, feClassGraph} partial@PartialType{ptName=PTypeName name} = do

  let vtypeArrows = H.lookupDefault [] name feVTypeGraph
  vtypes <- mapM tryVArrow vtypeArrows

  let ttypeArrows = H.lookupDefault [] name feTTypeGraph
  ttypes <- mapM tryTArrow ttypeArrows

  return $ ReachesLeaf (catMaybes vtypes ++ catMaybes ttypes)
  where
    tryVArrow (obj@Object{objM}, arr) =
      pointUb env objM >>= \objUb -> do
      -- It is possible to send part of a partial through the arrow, so must compute the valid part
      -- If none of it is valid, then there is Nothing
      let potentialSrc@(UnionType potSrcLeafs) = intersectTypes feClassGraph (singletonType partial) objUb
      if not (isBottomType potentialSrc)
        -- TODO: Should this line below call `reaches` to make this recursive?
        -- otherwise, no reaches path requiring multiple steps can be found
        then do
          sobj <- showObj env obj
          sarr <- showArrow env arr
          return $ Just $ unionAllTypes feClassGraph [arrowDestType True feClassGraph potentialSrcPartial sobj sarr | potentialSrcPartial <- splitUnionType potSrcLeafs]
        else return Nothing
    tryTArrow (obj@Object{objM}, arr) = do
      -- It is possible to send part of a partial through the arrow, so must compute the valid part
      -- If none of it is valid, then there is Nothing
      let potentialSrc@(UnionType potSrcLeafs) = intersectTypes feClassGraph (singletonType partial) (getMetaType objM)
      if not (isBottomType potentialSrc)
        -- TODO: Should this line below call `reaches` to make this recursive?
        -- otherwise, no reaches path requiring multiple steps can be found
        then return $ Just $ unionAllTypes feClassGraph [arrowDestType True feClassGraph potentialSrcPartial obj arr | potentialSrcPartial <- splitUnionType potSrcLeafs]
        else return Nothing
reachesPartial env@FEnv{feClassGraph} partial@PartialType{ptName=PClassName{}} = reaches env (expandClassPartial feClassGraph partial)
reachesPartial env@FEnv{feClassGraph, feUnionAllObjs} partial@PartialType{ptName=PRelativeName{}} = do
  reachesAsClass <- reaches env (expandClassPartial feClassGraph partial)
  SType (UnionType allObjsUb) _ _ <- descriptor env feUnionAllObjs
  reachesAsType <- mapM (reachesPartial env . (\name -> partial{ptName=name})) (H.keys allObjsUb)
  return $ joinReachesTrees reachesAsClass (joinAllReachesTrees reachesAsType)

reaches :: FEnv -> Type -> TypeCheckResult ReachesTree
reaches _     TopType            = return $ ReachesLeaf [TopType]
reaches _     (TypeVar v)            = error $ printf "reaches with typevar %s" (show v)
reaches env (UnionType src) = do
  let partials = splitUnionType src
  resultsByPartials <- mapM (reachesPartial env) partials
  return $ ReachesTree $ H.fromList $ zip partials resultsByPartials

rootReachesPartial :: FEnv -> PartialType -> TypeCheckResult (PartialType, ReachesTree)
rootReachesPartial env src = do
  reached <- reachesPartial env src
  let reachedWithId = ReachesTree $ H.singleton src reached
  return (src, reachedWithId)

arrowConstrainUbs :: FEnv -> Type -> VarMeta -> Type -> VarMeta -> TypeCheckResult (Type, Type)
arrowConstrainUbs env@FEnv{feUnionAllObjs} TopType srcM dest@UnionType{} destM = do
  unionPnt <- descriptor env feUnionAllObjs
  case unionPnt of
    (SType unionUb@UnionType{} _ _) -> do
      (src', dest') <- arrowConstrainUbs env unionUb srcM dest destM
      return (src', dest')
    _ -> return (TopType, dest)
arrowConstrainUbs _ TopType _ dest _ = return (TopType, dest)
arrowConstrainUbs env src@(TypeVar v) srcM dest destM = do
  src' <- resolveTypeVar v srcM
  (_, cdest) <- arrowConstrainUbs env (getMetaType src') srcM dest destM
  return (src, cdest)
arrowConstrainUbs env (UnionType srcPartials) (VarMeta _ _ srcObj) dest _ = do
  let classGraph = feClassGraph env
  let srcPartialList = splitUnionType srcPartials
  srcPartialList' <- mapM (rootReachesPartial env) srcPartialList
  let partialMap = H.fromList srcPartialList'
  let partialMap' = H.filter (\t -> reachesHasCutSubtypeOf classGraph srcObj t dest) partialMap
  let (srcPartialList'', destByPartial) = unzip $ H.toList partialMap'
  let srcPartials' = joinUnionType srcPartialList''
  let destByGraph = unionAllTypes classGraph $ fmap (unionReachesTree classGraph) destByPartial
  dest' <- tryIntersectTypes env dest destByGraph "executeConstraint ArrowTo"
  return (compactType classGraph $ UnionType srcPartials', dest')
