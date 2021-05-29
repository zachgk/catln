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
-- TODO Should prioritize function declarations above definitions
-- TODO May need to differentiate top level of functions from inner levels
objectPrecedence :: Object m -> Int
objectPrecedence Object{objBasis=TypeObj}=    1
objectPrecedence Object{objBasis=FunctionObj} = 2
objectPrecedence Object{objBasis=PatternObj}= 3
objectPrecedence Object{objBasis=MatchObj}=   4

-- | This creates 'feUnionAllObjs' and adds it to the 'FEnv'
addUnionObjToEnv :: FEnv -> VObjectMap -> TObjectMap -> FEnv
addUnionObjToEnv env1@FEnv{feClassMap} vobjMap tobjMap = do
  let notMatchObj Object{objBasis} = objBasis /= MatchObj
  let getRecursiveObjs obj@Object{objArgs} = obj : concatMap (filter notMatchObj . concatMap getRecursiveObjs . maybeToList . snd) (H.elems objArgs)
  let vobjs = concatMap (getRecursiveObjs . fst) vobjMap
  let tobjs = concatMap (getRecursiveObjs . fst) tobjMap

  -- Finds the best precedence for all each object name
  let buildPrecedenceMap = fmap (minimum . map objectPrecedence) . H.fromListWith (++) . map (\obj@Object{objName} -> (objName, [obj]))
  let vPrecedenceMap = buildPrecedenceMap vobjs
  let tPrecedenceMap = buildPrecedenceMap tobjs
  let precedenceMap = H.unionWith min vPrecedenceMap tPrecedenceMap

  -- Filter the objects to only those with the best precedence
  let filterBestPrecedence = filter (\obj@Object{objName} -> objectPrecedence obj == H.lookupDefault (error "Could not find obj in union") objName precedenceMap)
  let vobjs' = filterBestPrecedence vobjs
  let tobjs' = filterBestPrecedence tobjs

  -- Builds vars to use for union and union powerset
  let (unionAllObjs, env2) = fresh env1 $ TypeCheckResult [] $ SType TopType bottomType "unionAllObjs"
  let (unionAllObjsPs, env3) = fresh env2 $ TypeCheckResult [] $ SType TopType bottomType "unionAllObjsPs"

  -- Build a variable to store union of tobjs
  let typecheckedAllType = unionAllTypes feClassMap $ map (getMetaType . objM) tobjs'
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
inferArgFromPartial FEnv{feVTypeGraph, feTTypeGraph, feClassMap} partial@PartialType{ptName=PTypeName name, ptArgs} = do
  let vtypeArrows = H.lookupDefault [] name feVTypeGraph
  let vTypes = unionAllTypes feClassMap $ map tryArrow vtypeArrows

  let ttypeArrows = H.lookupDefault [] name feTTypeGraph
  let tTypes = unionAllTypes feClassMap $ map tryArrow ttypeArrows

  unionTypes feClassMap vTypes tTypes
  where
    tryArrow (Object{objArgs}, _) = if H.keysSet ptArgs `isSubsetOf` H.keysSet objArgs
      then UnionType $ joinUnionType $ map addArg $ S.toList $ S.difference (H.keysSet objArgs) (H.keysSet ptArgs)
      else bottomType
    addArg arg = partial{ptArgs=H.insertWith (unionTypes feClassMap) arg TopType ptArgs}
inferArgFromPartial _ PartialType{ptName=PClassName{}} = bottomType

isTypeVar :: Type -> Bool
isTypeVar TypeVar{} = True
isTypeVar _         = False

data ReachesTree
  = ReachesTree (H.HashMap PartialType ReachesTree)
  | ReachesLeaf [Type]
  deriving (Show)

unionReachesTree :: ClassMap -> ReachesTree -> Type
unionReachesTree classMap (ReachesTree children) = do
  let (keys, vals) = unzip $ H.toList children
  let keys' = UnionType $ joinUnionType keys
  let vals' = map (unionReachesTree classMap) vals
  let both = keys':vals'
  case partition isTypeVar both of
    ([onlyVar], []) -> onlyVar
    (_, sums)       -> unionAllTypes classMap sums
unionReachesTree classMap (ReachesLeaf leafs) = unionAllTypes classMap leafs

isSubtypePartialOfWithMaybeObj :: (Meta m) => ClassMap -> Maybe (Object m) -> PartialType -> Type -> Bool
isSubtypePartialOfWithMaybeObj classMap (Just obj) = isSubtypePartialOfWithObj classMap obj
isSubtypePartialOfWithMaybeObj classMap Nothing    = isSubtypePartialOf classMap

reachesHasCutSubtypeOf :: (Meta m) => ClassMap -> Maybe (Object m) -> ReachesTree -> Type -> Bool
reachesHasCutSubtypeOf classMap mObj (ReachesTree children) superType = all childIsSubtype $ H.toList children
  where childIsSubtype (key, val) = isSubtypePartialOfWithMaybeObj classMap mObj key superType || reachesHasCutSubtypeOf classMap mObj val superType
reachesHasCutSubtypeOf classMap mObj (ReachesLeaf leafs) superType = any (\t -> isSubtypeOfWithMaybeObj classMap mObj t superType) leafs

reachesPartial :: FEnv -> PartialType -> TypeCheckResult ReachesTree
reachesPartial env@FEnv{feVTypeGraph, feTTypeGraph, feClassMap} partial@PartialType{ptName=PTypeName name} = do

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
      let potentialSrc@(UnionType potSrcLeafs) = intersectTypes feClassMap (singletonType partial) objUb
      if not (isBottomType potentialSrc)
        -- TODO: Should this line below call `reaches` to make this recursive?
        -- otherwise, no reaches path requiring multiple steps can be found
        then do
          sobj <- showObj env obj
          sarr <- showArrow env arr
          return $ Just $ unionAllTypes feClassMap [arrowDestType True feClassMap potentialSrcPartial sobj sarr | potentialSrcPartial <- splitUnionType potSrcLeafs]
        else return Nothing
    tryTArrow (obj@Object{objM}, arr) = do
      -- It is possible to send part of a partial through the arrow, so must compute the valid part
      -- If none of it is valid, then there is Nothing
      let potentialSrc@(UnionType potSrcLeafs) = intersectTypes feClassMap (singletonType partial) (getMetaType objM)
      if not (isBottomType potentialSrc)
        -- TODO: Should this line below call `reaches` to make this recursive?
        -- otherwise, no reaches path requiring multiple steps can be found
        then return $ Just $ unionAllTypes feClassMap [arrowDestType True feClassMap potentialSrcPartial obj arr | potentialSrcPartial <- splitUnionType potSrcLeafs]
        else return Nothing
reachesPartial env@FEnv{feClassMap} partial@PartialType{ptName=PClassName{}} = reaches env (expandClassPartial feClassMap partial)

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
  let classMap = feClassMap env
  let srcPartialList = splitUnionType srcPartials
  srcPartialList' <- mapM (rootReachesPartial env) srcPartialList
  let partialMap = H.fromList srcPartialList'
  let partialMap' = H.filter (\t -> reachesHasCutSubtypeOf classMap srcObj t dest) partialMap
  let (srcPartialList'', destByPartial) = unzip $ H.toList partialMap'
  let srcPartials' = joinUnionType srcPartialList''
  let destByGraph = unionAllTypes classMap $ fmap (unionReachesTree classMap) destByPartial
  dest' <- tryIntersectTypes env dest destByGraph "executeConstraint ArrowTo"
  return (compactType classMap $ UnionType srcPartials', dest')
