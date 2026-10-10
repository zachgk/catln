--------------------------------------------------------------------
-- |
-- Module    :  Semantics.TypeGraph
-- Copyright :  (c) Zach Kimberg 2023
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module handles the type graph during type checking.
-- It uses base type(s) and arrows that can convert them
-- and checks if a conversion path is possible
--------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Semantics.TypeGraph where
import           Control.Monad
import           Data.Aeson          (ToJSON)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.String.Builder (build)
import           GHC.Generics        (Generic)
import           Semantics.Types
import           Text.Printf
import           Utils               (withIndent)

data ReachesTree
  = ReachesPartialTree !(H.HashMap PartialType ReachesTree) -- ReachesPartialTree are required (all must be handled to handle a partial)
  | ReachesTypeTree !(H.HashMap Type (String, ReachesTree)) -- ReachesTypeTree are optional (can choose to follow only one)
  | ReachesLeaf
  deriving (Eq, Ord, Generic, Hashable, ToJSON)

data ReachesEnv tg = ReachesEnv {
  rTypeEnv :: !(TypeEnv tg),
  rVaenv   :: !TypeVarArgEnv,
  rVisited :: !(S.HashSet PartialType)
                             }

instance Show ReachesTree where
  show = build . aux 0
    where
      aux indent (ReachesPartialTree children) = do
        forM_ (H.toList children) $ \(key, subTree) -> do
          withIndent indent $ show key
          aux (indent + 1) subTree
      aux indent (ReachesTypeTree children) = do
        forM_ (H.toList children) $ \(key, (reason, subTree)) -> do
          withIndent indent (printf "%s by %s" (show key) reason)
          aux (indent + 1) subTree
      aux _ ReachesLeaf = return ()

unionReachesTree :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> ReachesTree -> Type
-- A raw applied form whose settled data this whole union does not already contain (a recursive
-- result like @sorted(raw)@ next to only its @Nil@ base case) is presented BY its declared result
-- too ('presentRawResult': @sorted(raw) ∪ ∀ConsList@), so a requirement built from the union admits
-- the @Cons@ the term reaches. A raw form whose settled result IS already present anywhere in the
-- union (a wrapper @pureConsInt(raw)@ beside its @Cons@ result, or @restToString(raw)@ beside its
-- @String@) is left exact, preserving raw-vs-settled dispatch keying. The coverage is the WHOLE
-- union, so a settled sibling provided by another reach child is seen.
unionReachesTree typeEnv vaenv tree@(ReachesPartialTree children) =
  presentRawResult typeEnv vaenv (completePartials typeEnv vaenv tree) u
  where
    -- Flattened single-compaction union: each child contributes its source key and its
    -- subtree's union as separate operands ('unionAllTypesWithEnvOnce' merges all positive
    -- operands raw and compacts once), instead of one compaction per child plus one per
    -- fold step. The TypeVar cases keep replacing the key with the pointer, as before.
    u = unionAllTypesWithEnvOnce typeEnv vaenv $ concatMap unionChild $ H.toList children
    unionChild (key, val) = case unionReachesTree typeEnv vaenv val of
      (TypeVar (TVArg argName) tl i) | makeAbsoluteName (ptName key) == makeAbsoluteName (pkName argName) -> [TypeVar (TVArg $ makeAbsolutePk argName) tl i]
      (TypeVar (TVVar varName) tl i) -> [TypeVar (TVVar $ makeAbsolutePk varName) tl i]
      val' -> [singletonType key, val']
unionReachesTree typeEnv vaenv (ReachesTypeTree children) = unionAllTypesWithEnvOnce typeEnv vaenv (H.keys children ++ map (unionReachesTree typeEnv vaenv . snd) (H.elems children))
unionReachesTree _ _ ReachesLeaf = BottomType

joinReachesTrees :: ReachesTree -> ReachesTree -> ReachesTree
joinReachesTrees (ReachesPartialTree a) (ReachesPartialTree b) = ReachesPartialTree $ H.unionWith joinReachesTrees a b
joinReachesTrees ReachesLeaf ReachesLeaf = ReachesLeaf
joinReachesTrees (ReachesPartialTree t) v | H.null t = v
joinReachesTrees v (ReachesPartialTree t) | H.null t = v
joinReachesTrees a b = error $ printf "joinReachesTrees for mixed tree and leaf not yet defined: \n\t%s\n\t%s" (show a) (show b)

joinAllReachesTrees :: Foldable f => f ReachesTree -> ReachesTree
joinAllReachesTrees = foldr1 joinReachesTrees

reachesHasCutSubtypeOf :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> ReachesTree -> Type -> Bool
reachesHasCutSubtypeOf typeEnv vaenv (ReachesPartialTree children) superType = all childIsSubtype $ H.toList children
  where childIsSubtype (key, val) = isSubtypeOfWithEnv typeEnv vaenv (singletonType key) superType || reachesHasCutSubtypeOf typeEnv vaenv val superType
reachesHasCutSubtypeOf typeEnv vaenv (ReachesTypeTree children) superType = any childIsSubtype $ H.toList children
  where childIsSubtype (key, (_, val)) = isSubtypeOfWithEnv typeEnv vaenv key superType || reachesHasCutSubtypeOf typeEnv vaenv val superType
reachesHasCutSubtypeOf _ _ ReachesLeaf _ = False

-- | Cuts a reaches tree to a required destination. At a source partial's choice node
-- ('ReachesTypeTree'), a child arrow is GUARANTEED when its claimed type satisfies the
-- requirement, or its subtree can be cut to within it ('reachesHasCutSubtypeOf' — the arrows
-- BEYOND it reach the requirement, which is how a claim that must rewrite further is judged:
-- by the paths leading out of it, not by exempting it from the check). When the guaranteed
-- children jointly COVER the source partial — every value the source admits is accepted by
-- some guaranteed arrow — only they are kept: the requirement forces the arrow choice, and
-- no input is stranded by ignoring the others. Without joint coverage the node is left whole:
-- the remaining arrows are value dispatch (each accepting inputs the guaranteed ones do not),
-- which the compiler is not free to choose among. Required nodes ('ReachesPartialTree') keep
-- every child — their keys are the source terms themselves, which eval needs intact.
cutReachesTree :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> ReachesTree -> ReachesTree
cutReachesTree typeEnv vaenv req (ReachesPartialTree children) = ReachesPartialTree $ H.mapWithKey cutChild children
  where
    cutChild srcPartial (ReachesTypeTree arms) = ReachesTypeTree $ fmap recurseArm arms'
      where
        guaranteed key (_, subTree) = isSubtypeOfWithEnv typeEnv vaenv key req || reachesHasCutSubtypeOf typeEnv vaenv subTree req
        kept = H.filterWithKey guaranteed arms
        -- The portion of the source each kept arm accepts, re-queried from the graph (the tree
        -- keeps its lean committed shape; see 'typeGraphQueryWithReason').
        keptCoverage = unionAllTypesWithEnvOnce typeEnv vaenv
          [ coveredSrc
          | (reason, coveredSrc, dest) <- typeGraphQueryWithReason typeEnv vaenv srcPartial
          , Just (keptReason, _) <- [H.lookup dest kept]
          , reason == keptReason ]
        covers = isSubtypeOfWithEnv typeEnv vaenv (singletonType srcPartial) keptCoverage
        arms' = if not (H.null kept) && covers then kept else arms
        recurseArm (reason, subTree) = (reason, cutReachesTree typeEnv vaenv req subTree)
    cutChild _ subTree = cutReachesTree typeEnv vaenv req subTree
cutReachesTree typeEnv vaenv req (ReachesTypeTree children) = ReachesTypeTree $ fmap (fmap (cutReachesTree typeEnv vaenv req)) children
cutReachesTree _ _ _ ReachesLeaf = ReachesLeaf


-- | Whether every form a value of type @t@ can take is guaranteed to end within @req@: the
-- check that a value flowing into a point satisfies the point's requirement for ALL of the
-- values it may be, where 'reachesHasCutSubtypeOf' asks whether SOME path does.
--
-- A form passes when it is itself within @req@. Otherwise it still rewrites, and it passes when
-- the promises it triggers that are guaranteed to end within @req@ jointly cover it — every value
-- it admits is accepted by one of them — which is the rule 'cutReachesTree' applies when a
-- requirement chooses among arrows. A promise is guaranteed when its result is within @req@ or
-- every form of its result passes in turn. Every promise is asked, declarations included
-- ('typeGraphQueryAllWithReason'): a call is accepted on the strength of its callee's DECLARED
-- result, and the callee's own definitions are each held to that declaration where they are
-- checked. The reach query's pruning would drop a declaration whenever its definitions are the
-- more specific promises, and a recursive call judged through its own definitions only comes
-- back to itself. A form with no guaranteed promise that is not within @req@ fails, and so does
-- one re-entered along its own path, which proves nothing about where it ends.
--
-- A value predicate the type layer leaves undecided is not a failure of the guarantee but the
-- residue the runtime evaluates: a value held to a refinement, or accepted by a promise refined by
-- one, is within it when its shape is, and the predicate is evaluated on the value where it is
-- built ('TreeBuild.refinesTo', dispatch on the refined arm). One decided against the value — a
-- meet the type layer emptied — is the genuine negative it was.
typeGuaranteedWithin :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Bool
typeGuaranteedWithin typeEnv vaenv req = typeOk S.empty
  where
    -- A free variable is read as what it is bound to, or the ⊤ it denotes unbound, before the
    -- form is judged. Kept symbolic, a meet with a promise's object leaves it as written, and the
    -- covered source then reads exactly as the source: covered by its own spelling, whatever the
    -- variable stands for (@toString(this=$_B)@ with @$_B@ unbound).
    typeOk visited t0
      | within t req = True
      | otherwise = case t of
          UnionType Nothing leafs consts -> all (partialOk visited) (splitUnionType leafs ++ map constantPartialType consts)
          _                              -> False
      where t = substituteTVExt typeEnv vaenv t0
    partialOk visited p
      | within (singletonType p) req = True
      -- A bare reference to one of the definition's own arguments is that argument, as
      -- 'reachesPartials' reads it: an argument shadows every other name it matches, so
      -- @step(count, ..)= count@ is the parameter even where a function @/Data/List/count@ is
      -- imported, whose arrows would otherwise be the only promises asked.
      | H.null (ptArgs p), H.null (ptVars p), Just argValue <- H.lookup (extKey (TVArg (partialKey (ptName p)))) vaenv = typeOk visited (intersectTypesEnv typeEnv vaenv argValue (TopType H.empty (ptPreds p)))
      | S.member p visited = False
      | otherwise = not (null kept) && within (singletonType p) (unionAllTypesWithEnvOnce typeEnv vaenv (map (srcVarsOnly p) kept))
      where
        visited' = S.insert p visited
        kept = [ coveredSrc | (_, coveredSrc, dest) <- typeGraphQueryAllWithReason typeEnv vaenv p, typeOk visited' dest ]
    -- Within, or within by shape and not decided outside: the value predicates are the runtime's.
    within sub sup = isSubtypeOfWithEnv typeEnv vaenv sub sup
      || (typeHasPredExpr sup
          && isSubtypeOfWithEnv typeEnv vaenv sub (typeWithoutPredExprs sup)
          && not (isBottomType (intersectTypesEnv typeEnv vaenv sub sup)))
    -- What a promise covers is the source met with its object, and the meet brings the promise's
    -- own parameters along (@toString(this=∀ToString)@ meeting @toString[$T](this -> $T)@ covers
    -- @toString[/T=∀ToString](this=$/T)@). Those name the promise's scope, set by its arguments;
    -- they do not narrow which values of the source it accepts, so the covered source is read
    -- without the ones the source does not have. An argument may point at one (@this=$/T@), so
    -- its value is read into the arguments first ('substituteTVExt' descends with the partial's
    -- own bindings): dropped unread, the pointer would dangle and read as the ⊤ that covers
    -- anything.
    --
    -- A higher-order argument's input object carries the same scope one level in
    -- (@fn[/B=$^B](acc=$/B)@ — 'hopInputObject'): its parameters are hops to the promise's, read
    -- into its own arguments by the same substitution, so they are dropped where the source's slot
    -- does not spell them, as the promise's own are.
    srcVarsOnly PartialType{ptVars=srcVars, ptArgs=srcArgs} t = case substituteTVExt typeEnv vaenv t of
      UnionType Nothing leafs consts ->
        UnionType Nothing (joinUnionType [ l{ptVars = H.intersection (ptVars l) srcVars, ptArgs = H.mapWithKey inputObjVars (ptArgs l)} | l <- splitUnionType leafs ]) consts
      t' -> t'
      where
        inputObjVars k v
          | S.null (pkArgs k) = v
          | otherwise = case (v, H.lookup k srcArgs >>= maybeGetSingleton) of
              (UnionType Nothing ls cs, Just srcObj) ->
                UnionType Nothing (joinUnionType [ o{ptVars = H.intersection (ptVars o) (ptVars srcObj)} | o <- splitUnionType ls ]) cs
              _ -> v

reachesPartials :: (TypeGraph tg) => ReachesEnv tg -> [PartialType] -> ReachesTree
reachesPartials typeEnv partials = ReachesPartialTree $ H.fromList $ zip partials (map (reachesPartial typeEnv) partials)
  where
    reachesPartial :: (TypeGraph tg) => ReachesEnv tg -> PartialType -> ReachesTree
    reachesPartial ReachesEnv{rVisited} p | S.member p rVisited = ReachesLeaf
    -- A bare reference to an argument is the argument. Its predicates (a body's @a | a.isZ ==
    -- True@ types @a | PredExpr …@) are the occurrence's inner, which is what an occurrence
    -- denotes: the variable within that bound. Only a closed inner is installed (the 'TypeVar'
    -- discipline); a predicate over other arguments is left to where the condition is written.
    reachesPartial ReachesEnv{rVaenv} PartialType{ptName=argName, ptPreds} | extKey (TVArg (partialKey argName)) `H.member` rVaenv = ReachesTypeTree $ H.singleton (TypeVar (TVArg $ partialKey argName) TVInt inner) ("arg", ReachesLeaf)
      where
        inner = case ptPreds of
          PredsNone -> PTopType
          _ | typeHasTypeVars (TopType H.empty ptPreds) -> PTopType
            | otherwise -> TopType H.empty ptPreds
    reachesPartial env@ReachesEnv{rTypeEnv, rVisited, rVaenv} partial = do

      let ttypes = typeGraphQueryWithReason rTypeEnv rVaenv partial

      let env' = env{rVisited=S.insert partial rVisited}
      if null ttypes
        then ReachesLeaf
        else ReachesTypeTree $ H.fromList [(dest, (reason, reaches env' dest)) | (reason, _, dest) <- ttypes]

-- | Union a type's leaves with their declared result (@leaf ∪ result@), so a form is recognised
-- by what its arrows produce as well as by what it is. A recursive call in an argument position
-- (@insert(lst= sorted(raw))@) matches no arrow object structurally (@sorted(raw) ∩ Nil = ∅@);
-- unioning @sorted@'s declared result — 'PTopType' until inferred, else e.g. @ConsList@ — lets
-- both @insert@ arms apply, so the reach stays a genuine upper bound. Intersecting a class-top
-- result against a concrete member is transitive (@∀Functor ∩ Cons = Cons@ via
-- 'expandClassPartialWithNamesFull'), so the arms the produced value would reach are followed.
--
-- Every leaf is asked, not only the function-named ones. What a name is declared as says nothing
-- about whether arrows leave it — a data type may be the source of one — and a leaf with no
-- arrows answers 'PTopType' and is skipped by the guard below, so asking costs nothing where
-- there is nothing to add. Coverage is judged against the ACCUMULATING union itself (which always
-- contains the whole original union): a form whose declared result is ALREADY present adds
-- nothing, so a leaf standing beside what it produces is kept exact. One level only: the query
-- inspects direct arguments.
--
-- Only a leaf whose reach is not COMPLETE is asked ('completePartials'). A complete leaf already
-- has in the union everything it evaluates to: the arrows accepting it cover it ('typeGraphQuery'
-- keeps a set of promises covering the source), and everything they produce was followed to the
-- end. Its declared result is then no more information, only less precise: @isZero(n= Z)@ reaches
-- the @True@ its arm returns, and adding the declared @Boolean@ read the call as either. A leaf is
-- complete when it is within the complete partials, which the union may have joined.
presentRawResult :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> [PartialType] -> Type -> Type
presentRawResult typeEnv vaenv complete t@(UnionType Nothing leafs _) =
  foldr addResult t (filter (not . isComplete) (splitUnionType leafs))
  where
    completeType = UnionType Nothing (joinUnionType complete) []
    isComplete l = not (null complete) && isSubtypeOfWithEnv typeEnv vaenv (singletonType l) completeType
    -- A 'PTopType' result (a genuinely undeclared function) carries no settled data to admit and
    -- would only widen the union to Top, so it is not added; only a concrete or class-top result is.
    addResult l acc =
      let r = typeGraphDeclaredResult typeEnv vaenv l
      in if isBottomType r || isTop r || isSubtypeOfWithEnv typeEnv vaenv r acc
           then acc
           else unionTypesWithEnv typeEnv vaenv r acc
    isTop PTopType = True
    isTop _        = False
presentRawResult _ _ _ t = t

-- | The partials whose reach the tree followed to the end, so that what they evaluate to is all in
-- the union ('presentRawResult').
--
-- A partial is complete when the arrows accepting it were followed and every result they give is
-- complete in turn. A result is complete when each of its forms is: settled data; a call form
-- complete itself; a call form cut where it was already being reached, which is the recursion of
-- one of its own ancestors and gives nothing they do not; or a call form no arrow accepts whose
-- declared result is presented in its place. A result the reach stopped at (a predicate top) is
-- complete only with no call form beside it. Anything else is a form nothing says the value of —
-- a generated match helper no arrow accepted, with no declaration — and the partials above it keep
-- their declared result: being followed one level says nothing about what is below.
completePartials :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> ReachesTree -> [PartialType]
completePartials typeEnv vaenv = go S.empty
  where
    go anc (ReachesPartialTree children) = concat [ [p | partialComplete anc p sub] ++ go (S.insert p anc) sub | (p, sub) <- H.toList children ]
    go anc (ReachesTypeTree children) = concatMap (go anc . snd) (H.elems children)
    go _ ReachesLeaf = []

    partialComplete anc p ReachesLeaf = settled p || S.member p anc || presented p
    partialComplete anc p (ReachesTypeTree children) = all (resultComplete (S.insert p anc)) (H.toList children)
    partialComplete _ _ ReachesPartialTree{} = False

    resultComplete anc (_, (_, ReachesPartialTree forms)) = all (uncurry (partialComplete anc)) (H.toList forms)
    resultComplete _ (dest, (_, ReachesLeaf)) = case dest of
      UnionType _ leafs _ -> all settled (splitUnionType leafs)
      _                   -> True
    resultComplete anc (_, (_, ReachesTypeTree results)) = all (resultComplete anc) (H.toList results)

    settled p = not (null (typeGraphData typeEnv (ptName p)))
    presented p = case typeGraphDeclaredResult typeEnv vaenv p of
      PTopType -> False
      r        -> not (isBottomType r)

reaches :: (TypeGraph tg) => ReachesEnv tg -> Type -> ReachesTree
reaches _     (UnionType (Just _) _ _) = ReachesLeaf
-- A variable destination denotes its binding in the querying scope: an arrow defined in that
-- scope returns one of its variables (a definition's augmented @fn(v) -> $_B@). A concrete binding
-- is reached through; a variable the scope has not bound yet is a link with nothing further to
-- rewrite.
reaches env (TypeVar v _ _) = case vaenvLookup (rVaenv env) v of
  PTopType  -> ReachesLeaf
  TypeVar{} -> ReachesLeaf
  bound     -> reaches env bound
reaches env (UnionType Nothing src consts) =
  reachesPartials env $ splitUnionType src ++ map constantPartialType consts

reachesTo :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Bool
reachesTo typeEnv vaenv srcType = reachesHasCutSubtypeOf typeEnv vaenv reached
  where
    reachesEnv = ReachesEnv typeEnv vaenv S.empty
    reached = reaches reachesEnv srcType
