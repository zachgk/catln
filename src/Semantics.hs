{-# LANGUAGE PatternSynonyms #-}
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

import           Control.Exception   (bracket_, evaluate)
import           CtConstants         (builtinAnnotNames, contextValStr,
                                      errPageSuffix, macroThisKey,
                                      pattern ContextInStr)
import           Data.Graph          (graphFromEdges)
import           Data.Hashable       (Hashable (..), Hashed, hash, hashed)
import qualified Data.HashSet        as S
import           Data.IORef          (IORef, atomicModifyIORef', newIORef,
                                      readIORef)
import           Data.List           (foldl', isPrefixOf, isSuffixOf, partition)
import           Data.Maybe
import           Data.Time.Calendar  (Day, fromGregorian)
import           Semantics.Prgm
import           Semantics.TypeGraph (ReachesEnv (..), completePartials,
                                      reachesPartials, reachesTo,
                                      unionReachesTree)
import           Semantics.Types
import           System.Directory    (doesDirectoryExist, getHomeDirectory)
import           System.FilePath     (joinPath)
import           System.IO.Unsafe    (unsafePerformIO)
import           Text.Printf

newtype ObjArrTypeGraph e m = ObjArrTypeGraph (ObjectMap e m)
  deriving (Eq, Show)

-- | The item an env's graph holds under a name, read as the env's reads are noted
-- ('noteGraphRead'). Every reading of one item of an 'ObjArrTypeGraph' goes through here.
graphItemOf :: TypeEnv (ObjArrTypeGraph e m) -> TypeName -> Maybe (ObjectMapItem e m)
graphItemOf typeEnv@TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} name = noteGraphRead typeEnv name (H.lookup name tg)

-- | 'graphItemOf', reading a name the graph does not hold as an item with nothing in it.
graphItemOrEmpty :: TypeEnv (ObjArrTypeGraph e m) -> TypeName -> ObjectMapItem e m
graphItemOrEmpty typeEnv name = fromMaybe mempty (graphItemOf typeEnv name)

-- | The name the declarations written from a set of objects ('omDecls') are versioned under in a
-- 'ReuseScope', as they are kept under no name of their own. No program name is spelled this way.
setDeclsVersionName :: TypeName
setDeclsVersionName = mkName "\0omDecls"

-- | The name the input-embedded names of a reaches graph ('typeGraphInputEmbeddedNames') are
-- versioned under in a 'ReuseScope': a fact about every item at once, read through the solve's
-- per-epoch copy of it ('TypeCheck.Common.feInputArrowNames').
inputArrowNamesVersionName :: TypeName
inputArrowNamesVersionName = mkName "\0inputArrowNames"

instance Semigroup (ObjArrTypeGraph e m) where
  (ObjArrTypeGraph a) <> (ObjArrTypeGraph b) = ObjArrTypeGraph (a <> b)

-- | The arrows written from classes (@(_ -> String) -> String\/Text..@) a type graph holds, among its
-- declarations written from a set of objects ('omDecls'): each as the class, which is the type of
-- every member of it, and what every member converts to.
--
-- The result is read as the members of the class within it, as the class is expanded into them:
-- written @String\/Text..@, it is the member @String\/Text(chars= ConsList[Char])..@ the class
-- completes from its declared fields, which is the spelling everything reading the class's members
-- reads. The two denote the same values, but as written the first leaves the field open, and the set
-- algebra reads it as wider.
--
-- A declaration applying arguments to the members (@(_ -> Univariate)(x -> Float) -> Float@) is not
-- one: it converts nothing, and is read by 'classSignaturesOf'.
classArrowsOf :: (MetaDat m, Show m) => TypeEnv (ObjArrTypeGraph Expr m) -> [(Type, Type)]
classArrowsOf typeEnv@TypeEnv{teTypeGraph=ObjArrTypeGraph om} =
  [ (cls, asMembers cls (getMetaType m))
  | ObjArr{oaObj=Just obj, oaArr=Just (_, m)} <- noteGraphRead typeEnv setDeclsVersionName (omDecls om)
  , Just (cls, []) <- [setDeclParts obj] ]
  where
    asMembers cls res = case [ memberAsDeclared typeEnv l | l <- classMemberLeaves typeEnv cls, isSubtypeOf typeEnv (singletonType l) res ] of
      []      -> res
      members -> unionAllTypes typeEnv members

-- | A class's member as its data declaration has it, fields and all. A class lists a member by name
-- (@String\/Text..@), which is every value the declaration makes, and the declaration's own spelling
-- (@String\/Text(chars= ∀ConsList[T= Char])@) is the one an arrow written from the member reads its
-- input as. The two denote the same values, but the set algebra does not complete the first from
-- the declaration, and reads it as wider.
memberAsDeclared :: (TypeGraph tg) => TypeEnv tg -> PartialType -> Type
memberAsDeclared typeEnv l = case typeGraphData typeEnv (ptName l) of
  []    -> singletonType l
  decls -> intersectTypes typeEnv (singletonType l) (unionAllTypes typeEnv (map singletonType decls))

-- | The data members a class type stands for: its positive leaves, and what its predicates expand
-- to, expanded in turn where a member is a class itself.
classMemberLeaves :: (TypeGraph tg) => TypeEnv tg -> Type -> [PartialType]
classMemberLeaves typeEnv = go (10 :: Int)
  where
    go fuel t@(UnionType mTop leafs _) = splitUnionType leafs ++ case mTop of
      Just (preds, _, _) | fuel > 0, let e = expandPredicates typeEnv H.empty preds, e /= t -> go (fuel - 1) e
      _ -> []
    go _ _ = []

-- | The members of a class a class arrow is written from that are not within @t@ and reach it: what
-- converts into a slot of type @t@ on the way in, as the tree build converts an argument toward the
-- slot it is given in. A @\/Catln\/CStr@ is one for a @String\/Text@ slot, and a @String\/Text@ for a
-- @\/Catln\/CStr@ slot.
convertingMembers :: (MetaDat m, Show m) => TypeEnv (ObjArrTypeGraph Expr m) -> Type -> [PartialType]
convertingMembers typeEnv t =
  [ l
  | (cls, _) <- classArrowsOf typeEnv
  , not (isBottomType (intersectTypes typeEnv cls t))
  , l <- classMemberLeaves typeEnv cls
  , not (isSubtypeOf typeEnv (singletonType l) t)
  , reachesTo typeEnv H.empty (singletonType l) t ]

-- | A declaration's input as what its definitions must take once the class arrows have converted
-- what they convert.
--
-- An argument slot declared to take a whole class an arrow is written from takes every member, and
-- every member converts to the arrow's result on the way in ('TypeCheck.Decode.verifyClassArrows'
-- holds each member to that). So the definitions only need to take the result there:
-- @(_ -> String) -> String\/Text..@ makes @isEmpty(this -> String)@ fulfilled by arms over
-- @String\/Text(chars= Nil)@ and @String\/Text(chars= Cons(..))@ together. It is the declaration that
-- is narrowed rather than each definition widened, as several definitions may take the result
-- between them where none takes it alone. Only the declaration's own arguments narrow, as only an
-- argument is converted on the way into a call; a value held in one of their fields is taken as it
-- is.
narrowByClassArrows :: (TypeGraph tg) => TypeEnv tg -> [(Type, Type)] -> Type -> Type
narrowByClassArrows _ [] t = t
narrowByClassArrows typeEnv arrows (UnionType mTop leafs consts) = UnionType mTop (joinUnionType (map narrowLeaf (splitUnionType leafs))) consts
  where
    narrowLeaf p@PartialType{ptArgs} = p{ptArgs = fmap (narrowSlotByClassArrows typeEnv arrows) ptArgs}
narrowByClassArrows _ _ t = t

-- | One argument slot as 'narrowByClassArrows' narrows it: the members of a class an arrow is
-- written from, where the slot takes the whole class, replaced by what they convert to.
--
-- Only a slot that states what it takes. A ⊤ slot takes the class too, but it takes every value as
-- it is, and its definitions are written for every value; a slot typed by a declaration variable is
-- what a call binds it to, which is not decided here.
narrowSlotByClassArrows :: (TypeGraph tg) => TypeEnv tg -> [(Type, Type)] -> Type -> Type
narrowSlotByClassArrows typeEnv arrows t
  | t == PTopType || typeContainsTypeVar t = t
  | otherwise = foldl' narrowBy t arrows
  where
    narrowBy acc (cls, res)
      | isSubtypeOf typeEnv cls acc = unionTypes typeEnv (snd $ differenceTypeWithEnv typeEnv H.empty acc cls) res
      | otherwise = acc
-- | THE REACH-QUERY MEMO. Profiling a near-empty program against core put 48.5% of all
-- time in 'objArrGraphQuery' — 121,525 calls at ~9ms each — while the reaches graph they
-- query changes at most once per epoch. The query is called from PURE set-algebra
-- contexts ('compactType' chains), so the memo is an 'unsafePerformIO' global keyed
-- STRUCTURALLY by the env's identity token ('teQueryCacheKey'), the behavior flags a
-- same-token env can vary in, the queried partial, and the full vaenv — a hash collision
-- falls back to 'Eq' in the 'H.HashMap', never a wrong answer. Soundness rests on
-- token-per-identical-env-value: every hit under a token is served from the same
-- immutable env the entry was computed against. The table keeps one token generation and
-- resets when a new token appears, bounding memory to one epoch's distinct queries.
-- Queries under an in-flight sat decision bypass the memo entirely — kept from when
-- 'teInSatDecision' demoted answers; it no longer does, so the bypass costs only sharing.
--
-- HISTORY (the "time-dependence mystery", resolved 2026-08-21): the first landing of
-- this memo produced stored-vs-recomputed divergences under identical keys, traced
-- through StableName/fingerprint instrumentation to 'unsafeFreshQueryToken' executing
-- its IO exactly ONCE per process — GHC's full-laziness floated the constant
-- 'unsafePerformIO' body (which ignored its argument) out of the lambda, so every env
-- shared token 1 and the memo mixed answers across epochs and programs. The mint's IO
-- now forces its argument INSIDE the action, so the expression depends on the argument
-- and cannot float.
type QueryMemoKey = (Int, (Bool, Bool, Bool, Bool), PartialType, TypeVarArgEnv)

-- | Keeps the THREE most-recently-used token generations: the per-page static env's
-- token, the current epoch's reaches token, and the current constraint's merged reaches
-- token (see 'TypeCheck.TypeUtils.mkReachesEnv') rotate constantly, so a keep-one policy
-- thrashed them away, while keep-all accumulated large-vaenv keys without bound. A new
-- (fourth) token evicts the least-recently-used generation.
{-# NOINLINE queryMemoRef #-}
queryMemoRef :: IORef [(Int, H.HashMap (Hashed QueryMemoKey) ([(String, Type, Type)], ReadDeps))]
queryMemoRef = unsafePerformIO (newIORef [])

-- | A query asked again in another env of a 'ReuseScope': the same question in every part of the
-- env the scope does not version — the behavior flags, the fuel a body may still be read again
-- with, the local arrows merged in ('teLocalKey') — about the same partial in the same var env.
type QueryReuseKey = ((Bool, Bool, Bool, Bool), Int, Maybe Int, PartialType, TypeVarArgEnv)

-- | THE REACH QUERIES OF A SCOPE. Each epoch of a solve reads the program through a new env, and so
-- under a new token, and each constraint with local arrows through yet another; under a new token
-- every query was worked out again. Measured over core, 82% of the queries worked out again gave
-- the answer an earlier env of the epoch's scope had already given to the same question, since an
-- epoch changes few of the items there are, and a query reads few of them. So each answer is kept
-- with the versions of the items it read ('ReadDeps'), and an env of the same scope holding each of
-- them at the same version is handed it.
--
-- An env with local arrows merged in hands on and is handed answers only where the merge named its
-- arrows ('teLocalKey'), as they are no item of the scope. Its reads are noted all the same: an
-- answer worked out in it is part of the answers that are kept.
--
-- One scope at a time: a new scope (the next fragment of a solve) starts the table again, as none
-- of the answers of another is an answer in it.
{-# NOINLINE queryReuseRef #-}
queryReuseRef :: IORef (Int, H.HashMap QueryReuseKey ([(String, Type, Type)], ReadDeps))
queryReuseRef = unsafePerformIO (newIORef (0, H.empty))

{-# NOINLINE queryTokenRef #-}
queryTokenRef :: IORef Int
queryTokenRef = unsafePerformIO (newIORef 0)

-- | A fresh, process-globally-unique token stamping one rebuilt reaches graph. The
-- argument is forced INSIDE the IO action: the action mentions the argument, so
-- full-laziness cannot float it to a shared constant (see the memo history above —
-- 'seq'-on-the-outside or a NOINLINE dummy argument are NOT sufficient).
{-# NOINLINE unsafeFreshQueryToken #-}
unsafeFreshQueryToken :: tg -> Int
unsafeFreshQueryToken g = unsafePerformIO $ do
  _ <- evaluate g
  atomicModifyIORef' queryTokenRef (\n -> (n + 1, n + 1))

-- | Stamps an env that reads its own graph with a fresh token: the reach-query memo's identity
-- ('teQueryCacheKey') and the identity value-predicate decisions are remembered under
-- ('teDecisionKey'), which the merges below carry.
stampQueryToken :: TypeEnv tg -> TypeEnv tg
stampQueryToken env = env{teQueryCacheKey = Just tok, teDecisionKey = Just tok}
  where tok = unsafeFreshQueryToken (teTypeGraph env)

-- | A merged env with a scope's local arrows added to @base@ (@merged = local <> base@): a
-- different value, so it is minted its own query token, but it reads the base's graph with the
-- local arrows beside it, so a decision keys on the base's identity unless it names one of them
-- ('teLocalNames').
--
-- The arrows are named by @localKey@, an id standing for exactly them in the base's 'ReuseScope'
-- ('internLocalArrows'), or 'Nothing' where the caller has none to give.
withLocalArrowsOf :: Maybe Int -> ObjArrTypeGraph e m -> TypeEnv tg -> TypeEnv tg -> TypeEnv tg
withLocalArrowsOf localKey (ObjArrTypeGraph (ObjectMap local)) base merged =
  merged{ teQueryCacheKey = Just (unsafeFreshQueryToken (teTypeGraph merged))
        , teDecisionKey = teDecisionKey base
        , teLocalNames = S.union (H.keysSet local) (teLocalNames base)
        , teLocalKey = localKey }

-- | The local arrows an env may be merged with, as what they are made from: a constraint's own
-- arrows ('TypeCheck.TypeUtils.localArrowTypeEnv'), or the lambdas a body read again is given
-- ('typeGraphWithArgArrows'), each beside the arrows the env merged into already holds.
data LocalArrows
  = LocalArrowObjs [ObjArr Expr ()] (Maybe Int)
  | LocalLambdas [(Type, Type)] (Maybe Int)
  deriving (Eq)

instance Hashable LocalArrows where
  hashWithSalt s (LocalArrowObjs objs k) = s `hashWithSalt` (0 :: Int) `hashWithSalt` objs `hashWithSalt` k
  hashWithSalt s (LocalLambdas lams k)   = s `hashWithSalt` (1 :: Int) `hashWithSalt` lams `hashWithSalt` k

-- | The ids 'internLocalArrows' has handed out in one 'ReuseScope', and the next.
{-# NOINLINE localArrowsRef #-}
localArrowsRef :: IORef (Int, Int, H.HashMap LocalArrows Int)
localArrowsRef = unsafePerformIO (newIORef (0, 0, H.empty))

-- | An id standing for exactly these local arrows in the scope of the env they are merged into: two
-- merges are handed one id when they merge the same arrows, so a query asked in either is the same
-- question ('teLocalKey'). 'Nothing' outside a 'ReuseScope'. One scope at a time, as the answers
-- are kept ('queryReuseRef').
{-# NOINLINE internLocalArrows #-}
internLocalArrows :: TypeEnv tg -> LocalArrows -> Maybe Int
internLocalArrows TypeEnv{teReuse=Nothing} _ = Nothing
-- The arrows are hashed through before the table is touched: reading them can ask the graph, and so
-- intern other arrows, and a table read while its own update is still being worked out is a loop.
internLocalArrows TypeEnv{teReuse=Just ReuseScope{rsScope}} arrows = unsafePerformIO $ do
  _ <- evaluate (hash arrows)
  atomicModifyIORef' localArrowsRef $ \(scope, next, table) ->
    let (next', table') = if scope == rsScope then (next, table) else (0, H.empty)
    in case H.lookup arrows table' of
         Just i  -> ((rsScope, next', table'), Just i)
         Nothing -> ((rsScope, next' + 1, H.insert arrows next' table'), Just next')

-- | The query keys whose answers are being computed right now, by hash. See the re-entrancy note
-- in 'objArrGraphQuery'.
{-# NOINLINE queryInProgressRef #-}
queryInProgressRef :: IORef (S.HashSet Int)
queryInProgressRef = unsafePerformIO (newIORef S.empty)

-- | Force a query's answer far enough that nothing left in it can ask the graph anything.
forceQueryResult :: [(String, Type, Type)] -> ()
forceQueryResult = foldl' (\acc (_, src, dest) -> acc `seq` forceType src `seq` forceType dest) ()

objArrGraphQuery :: (ExprClass e, MetaDat m, Show m, Show (e m)) => Bool -> TypeEnv (ObjArrTypeGraph e m) -> TypeVarArgEnv -> PartialType -> [(String, Type, Type)]
objArrGraphQuery prune typeEnv vaenv partial = case teQueryCacheKey typeEnv of
  Nothing -> objArrGraphQueryRaw prune typeEnv vaenv partial
  Just _ | teInSatDecision typeEnv -> objArrGraphQueryRaw prune typeEnv vaenv partial
  Just tok -> objArrGraphQueryMemo prune typeEnv vaenv partial tok

objArrGraphQueryMemo :: (ExprClass e, MetaDat m, Show m, Show (e m)) => Bool -> TypeEnv (ObjArrTypeGraph e m) -> TypeVarArgEnv -> PartialType -> Int -> [(String, Type, Type)]
objArrGraphQueryMemo prune typeEnv vaenv partial tok = unsafePerformIO $ do
    -- RE-ENTRANCY. A query's own work can ask the graph about the SAME key again — resolving a
    -- raw applied leaf reaches for what its arrows produce, which is another query. Handing such
    -- a query the memo's entry would hand it the very thunk it is already inside, which GHC ends
    -- as @<<loop>>@. So a key currently being computed is not served from the memo at all: the
    -- re-entrant query recomputes, unshared. That is the answer 'reachesPartial' already gives a
    -- cycle in the reach ('rVisited'), and the recomputation terminates — the loop was the
    -- SHARING, never the recursion.
    --
    -- Tracked by hash rather than by the key itself, to keep the set monomorphic. A collision
    -- costs a memo miss and nothing else, since the fallthrough recomputes the same answer.
    inProgress <- readIORef queryInProgressRef
    -- Hashed once, for the in-progress mark, the lookup and the store.
    hkey <- evaluate (hashed key)
    if S.member (hash hkey) inProgress
      then return $ objArrGraphQueryRaw prune typeEnv vaenv partial
      else do
        gens <- readIORef queryMemoRef
        case lookup tok gens >>= H.lookup hkey of
          Just (r, deps) -> noteReadDeps deps >> return r
          Nothing -> do
            reused <- reusedAnswer
            answer@(r, deps) <- maybe (compute hkey) return reused
            noteReadDeps deps
            remember hkey answer
            return r
    where
      -- Computed HERE, inside the mark, rather than published as a thunk to be forced later.
      -- The window to guard is while the answer is being worked out, and a thunk's forcing
      -- happens wherever it is demanded — outside any bracket this function could hold, and
      -- in a nested 'unsafePerformIO' that GHC is free to float or duplicate (this module
      -- already fights full-laziness for 'unsafeFreshQueryToken'). Forcing it here makes the
      -- mark's extent exactly the computation's, and publishes a value rather than a thunk —
      -- a half-evaluated one would leave the same hazard one level down. 'forceType' goes
      -- through the structure only, which is all of it that can ask the graph anything.
      compute hkey = bracket_
        (atomicModifyIORef' queryInProgressRef (\s -> (S.insert (hash hkey) s, ())))
        (atomicModifyIORef' queryInProgressRef (\s -> (S.delete (hash hkey) s, ())))
        (memoReading typeEnv $ do
            let res = objArrGraphQueryRaw prune typeEnv vaenv partial
            _ <- evaluate (forceQueryResult res)
            return res)
      flags = (prune, teDisableCompact typeEnv, tePrgmEnv typeEnv, teArgModeProjection typeEnv)
      key = (tok, flags, partial, vaenv)
      reuseKey = (flags, teRederiveFuel typeEnv, teLocalKey typeEnv, partial, vaenv)
      reuseScope = case teReuse typeEnv of
        Just scope | S.null (teLocalNames typeEnv) || isJust (teLocalKey typeEnv) -> Just scope
        _ -> Nothing
      -- An answer an earlier env of this env's scope gave, if every item it read is still at the
      -- version it read.
      reusedAnswer = case reuseScope of
        Nothing -> return Nothing
        Just ReuseScope{rsScope, rsVersions} -> do
          -- Hashed through before the table is read or updated: the local arrows it names are
          -- interned when first read ('internLocalArrows'), which can ask the graph.
          _ <- evaluate (hash reuseKey)
          (scope, table) <- readIORef queryReuseRef
          return $ case H.lookup reuseKey table of
            Just answer@(_, deps)
              | scope == rsScope
              , all (\(n, v) -> H.lookup n rsVersions == Just v) (H.toList deps) -> Just answer
            _ -> Nothing
      remember hkey answer = do
        atomicModifyIORef' queryMemoRef $ \gens2 ->
          let mTok = fromMaybe H.empty (lookup tok gens2)
              rest = filter ((/= tok) . fst) gens2
          in ((tok, H.insert hkey answer mTok) : take 2 rest, ())
        case reuseScope of
          Nothing -> return ()
          Just ReuseScope{rsScope} -> atomicModifyIORef' queryReuseRef $ \(scope, table) ->
            if scope == rsScope
              then ((scope, H.insert reuseKey answer table), ())
              else ((rsScope, H.singleton reuseKey answer), ())

-- | Shared body of 'typeGraphQueryWithReason' (@prune=True@) and
-- 'typeGraphQueryAllWithReason' (@prune=False@ — every accepting arrow contributes).
objArrGraphQueryRaw :: (ExprClass e, MetaDat m, Show m, Show (e m)) => Bool -> TypeEnv (ObjArrTypeGraph e m) -> TypeVarArgEnv -> PartialType -> [(String, Type, Type)]
objArrGraphQueryRaw prune typeEnv vaenv partial@PartialType{ptName} = concatMap tryTArrow $ promises $ mapMaybe withSrc $ universeObjectMapItem item
    where
      -- Each arrow with what it accepts OF THIS SOURCE, met once. Whether the arrow triggers, the
      -- cover the pruning checks, and what the arrow is sent all read the same meet.
      withSrc oa@ObjArr{oaArr=Just{}} = case intersectTypesEnv typeEnv vaenv (singletonType partial) (arrowObjType oa) of
        src | isBottomType src -> Nothing
            | otherwise -> Just (oa, src)
      withSrc _ = Nothing
      item = graphItemOrEmpty typeEnv ptName
      -- A refinement declaration is not an arrow a term is rewritten through: it is a further
      -- promise about the result of the arrows that are. Offered as one, it became an alternative
      -- beside the arms it refines, and the reach union then read the call as any of their results
      -- (@tri(n -> Z) -> AB@ and @tri(n -> Z) -> BC@ beside the arm's @B@ gave all of @Tri@). So the
      -- promises are the item's declarations and definitions, and the result of each one with no
      -- body is met with every refinement covering the part of the source it accepts ('refinedDest').
      --
      -- A definition's result is what its body computes, which the reach follows further; a
      -- refinement is a claim about it, checked on its own (its proof or derived tests). Met with
      -- the claim, a computed result outside it — a refinement that is wrong — read as no value at
      -- all, and the definition failed to type-check rather than the refinement failing to hold. A
      -- declaration or primitive has nothing but its promises to say what it returns, so there the
      -- refinements are what is known.
      promises | prune     = mostSpecificPromises
               | otherwise = id
      -- Each declaration/definition is a PROMISE: given an input of one type, an output of a certain
      -- type can be produced. Among the promises the source triggers, keep only the MOST HELPFUL — an
      -- arrow whose object (input type) is a strict SUPERTYPE of another triggered arrow's object is
      -- dominated and dropped. The abstract accessor DECLARATION @:$T.eq -> Eqv[$T]@ (object
      -- @eq(this=$T->Eq)@, result the class-top @∀Eqv@) is thus dropped for a concrete source whenever
      -- the specific DEFINITION @:Integer.eq= IntOrdering@ (object @eq(this=Integer)@) also triggers,
      -- so the concrete rewrite is not absorbed by the declaration's class-top in the reaches union.
      -- Declarations and definitions compete on equal footing; specificity alone decides, and equally
      -- specific promises (e.g. a recursive function's own decl and def) are both kept.
      --
      -- Specificity may only choose among VALID promise sets, and a valid set must COVER the full
      -- source: the reach union enumerates every form the term may take (an upper bound), so pruning
      -- to promises that accept only part of the source writes an incomplete enumeration. For
      -- @fmap(lst=⊤)@ the valid sets are the declaration alone or the Nil and Cons definition arms
      -- together; the Nil arm alone is more specific but accepts only @lst=Nil@. Mid-inference a
      -- definition arm whose object pattern has not yet resolved presents as ⊤ and is dominated
      -- exactly like a declaration, so without the cover check its claim would be omitted from the
      -- union — and monotonic narrowing makes the omission permanent. The check parallels
      -- 'completeTreeSet' in "TreeBuild" with opposite polarity: dispatch must ERROR when coverage
      -- fails, while a reach query instead DECLINES TO PRUNE (keeping every triggered promise is the
      -- sound fallback for an upper bound).
      mostSpecificPromises triggered
        | length kept == length triggered || covered = kept
        | otherwise = triggered
        where
          kept = filter (\(oa, _) -> not (any ((`strictlyDominates` oa) . fst) triggered)) triggered
          -- What each kept promise accepts OF THIS SOURCE.
          keptSrcs = map snd kept
          covered = materializedCovers || symbolicCovers
          -- The kept sources are joined with one compaction ('unionAllTypesWithEnvOnce'), as the
          -- tree builder's coverage check joins its kept arms ('Semantics.TypeGraph'): folded
          -- pairwise, the union re-canonicalized the growing join once per promise, comparing
          -- its same-named leafs pairwise each time, for one subtype question.
          materializedCovers = isSubtypeOfWithEnv typeEnv vaenv (singletonType partial) (unionAllTypesWithEnvOnce typeEnv vaenv keptSrcs)
          -- The engine sees joint covers a leafwise subtype check cannot (e.g. Nil and Cons arms
          -- jointly covering a class-typed argument); out of fragment the materialized check decides.
          symbolicCovers = case traverse unionLeafs keptSrcs of
            Just optss
              | dispatchInFragment partial && all dispatchInFragment (concat optss)
              -> not (satLeafEscapesAll typeEnv vaenv S.empty partial (concat optss))
            _ -> False
          unionLeafs (UnionType Nothing leafs []) = Just (splitUnionType leafs)
          unionLeafs _                            = Nothing
      arrowObjType oa = getMetaType $ getExprMeta $ oaObjExpr oa
      -- @a@ strictly dominates @b@ when a's object is a strict subtype of b's (a is more specific).
      strictlyDominates a b = let oa' = arrowObjType a; ob = arrowObjType b
                              in oa' /= ob && isSubtypeOfWithEnv typeEnv vaenv oa' ob

      -- Part of a partial can be sent through the arrow: what it accepts of the source ('withSrc').
      tryTArrow :: (ExprClass e, MetaDat m, Show m, Show (e m)) => (ObjArr e m, Type) -> [(String, Type, Type)]
      tryTArrow (oa, potentialSrc@(UnionType Nothing potSrcLeafs [])) =
        map (printf "using %s" (show oa), potentialSrc,) $ joinPartialDestTypes [map (refinedDest oa potentialSrcPartial) (arrowDestType typeEnv vaenv potentialSrcPartial oa) | potentialSrcPartial <- splitUnionType potSrcLeafs]
      tryTArrow _ = error "Unexpected non-union in TryTArrow"

      refinedDest oa src dest = case oaArr oa of
        Just (Nothing, _) -> foldl' (intersectTypesEnv typeEnv vaenv) dest (coveringRefinementResults typeEnv vaenv item src)
        _                 -> dest

      joinPartialDestTypes :: [[Type]] -> [Type]
      joinPartialDestTypes [] = [BottomType]
      joinPartialDestTypes (pdt1:restPdt) = [unionTypes typeEnv pdt1Poss restPdt' | pdt1Poss <- pdt1, restPdt' <- joinPartialDestTypes restPdt]

-- | The input templates an arrow object presents (see 'typeGraphArrowTemplates').
-- A hole-spelled declaration (@(_ :: $T ++ _ :: $T)[$T -> Semigroup]@) presents its
-- slots as ⊤ in the top meta while the arg ObjArrs still carry the spelled types
-- (@Value /l -> $/T@). Enrich exactly those lost slots from the args' own spelled
-- metas — the declared linkage the template exists to expose. A slot the top meta
-- already presents (or a genuinely ⊤-spelled arg) is untouched.
oaInputTemplates :: (ExprClass e, MetaDat m, Show m, Show (e m)) => ObjArr e m -> [PartialType]
oaInputTemplates oa@ObjArr{oaArr=Just{}} = case getMetaType $ getExprMeta $ oaObjExpr oa of
  UnionType Nothing leafs _ -> map enrich $ splitUnionType leafs
  _                         -> []
  where
    argSpellings = H.fromList [ (inExprSingleton (oaObjExpr argOa), getMetaType argM)
                              | argOa@ObjArr{oaArr=Just (_, argM)} <- exprAppliedArgs (oaObjExpr oa) ]
    enrich p = p{ptArgs=H.mapWithKey enrichSlot (ptArgs p)}
    enrichSlot slotK PTopType = H.lookupDefault PTopType slotK argSpellings
    enrichSlot _ v            = v
oaInputTemplates ObjArr{oaArr=Nothing} = []

instance (ExprClass e, MetaDat m, Show m, Show (e m)) => TypeGraph (ObjArrTypeGraph e m) where
  typeGraphQueryWithReason = objArrGraphQuery True
  typeGraphQueryAllWithReason = objArrGraphQuery False
  typeGraphDeclaredVarBindings typeEnv vaenv partial@PartialType{ptName=pName} =
    case [ oa | oa@ObjArr{oaArr=Just{}} <- maybe [] omiDecls (graphItemOf typeEnv pName)
              , not $ isBottomType $ intersectTypesEnv typeEnv vaenv (singletonType partial) (getMetaType $ getExprMeta $ oaObjExpr oa) ] of
      [decl] -> case exprVarArgsWithSrc typeEnv (oaObjExpr decl) partial of
        [binds] -> H.fromList [ (v, abType b) | (v@TVVar{}, b) <- H.toList binds ]
        _       -> H.empty
      _ -> H.empty
  -- A lambda added here SHADOWS an arrow of the same name the env already holds (an enclosing
  -- definition's argument, or another call's lambda), as 'TypeCheck.TypeUtils.localArrowTypeEnv'
  -- shadows them: inside the call, the name is this lambda. The env reads one level less fuel
  -- ('teRederiveFuel'): it is the scope of a body being read again.
  typeGraphWithArgArrows typeEnv [] = typeEnv
  typeGraphWithArgArrows typeEnv@TypeEnv{teTypeGraph=ObjArrTypeGraph baseOm@(ObjectMap baseTg)} arrows = merged{teRederiveFuel = teRederiveFuel typeEnv - 1}
    where
      lamArrows = [ ObjArr (Just (setExprMeta (mkValue (ptName p)) (emptyMetaT inT))) ArgObj Nothing [] (Just (Nothing, emptyMetaT outT))
                  | (inT, outT) <- arrows, Just p <- [maybeGetSingleton inT] ]
      objMap@(ObjectMap lamTg) = objectMapFromList lamArrows
      unshadowed = baseOm{omItems = H.mapWithKey (\n item -> if H.member n lamTg then item{omiDecls = notArg (omiDecls item), omiDefs = notArg (omiDefs item)} else item) baseTg}
      notArg = filter ((/= ArgObj) . oaBasis)
      -- The lambdas declare no class (see 'TypeCheck.TypeUtils.localArrowTypeEnv'), so they are
      -- merged with no class graph and the program's is kept as it is.
      base = typeEnv{teTypeGraph = ObjArrTypeGraph unshadowed}
      merged = withLocalArrowsOf (internLocalArrows typeEnv (LocalLambdas arrows (teLocalKey typeEnv))) (ObjArrTypeGraph objMap) base (mkTypeEnv (Prgm objMap emptyClassGraph []) <> base)
  typeGraphDeclaredArgResults typeEnv name =
    [ (inExprSingleton aObj, getMetaType aM)
    | decl <- maybe [] omiDecls (graphItemOf typeEnv (makeAbsoluteName name))
    , ObjArr{oaObj=Just aObj, oaArr=Just (_, aM)} <- exprAppliedArgs (oaObjExpr decl)
    , not (null (exprAppliedArgs aObj)) ]
  -- The predicate is evaluated on the shape the way the derived refinement tests evaluate it on a
  -- value ('Eval.Testing' binds @$this@ and reduces the application tree bottom-up through the
  -- evaluator): @$this@ is the shape, an argument is reduced first, and an application is read at
  -- what the reach gives it — the settled forms every arrow accepting it produces, followed to the
  -- end. A reach cut short decides nothing: a predicate bottoming out at a primitive
  -- (@Integer_nonNeg@'s @>=@) reaches its declared @Boolean@, which is neither only @True@ nor only
  -- not, so the predicate stays on the leaf for the derived tests and the runtime.
  --
  -- The predicate keeps the spelling it was written in (@~isZ@, @nonNeg@), and the graph is keyed
  -- by absolute name, so an application is resolved as 'Eval.Testing' resolves it; one that
  -- matches several names, or none, is not followed.
  typeGraphPredExprHolds typeEnv@TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} vaenv val e = do
    r <- predResult (singletonType val) e
    -- A predicate that reduces to no value at all: in the open world an arrow may yet accept the
    -- application, so nothing is decided; in the closed world ('tePrgmEnv', the program as
    -- written) every arrow is in hand and none does, so the predicate is never @True@ of the
    -- value. The open world decides the same application FALSE through the declaration it
    -- might come to satisfy (@length(this= Float)@ is an @Integer@ were @Float@ a @List@), and
    -- a proof the open world makes must hold closed.
    if isBottomType r then (if tePrgmEnv typeEnv then Just False else Nothing)
    else if isSubtypeOfWithEnv typeEnv vaenv r trueType then Just True
    else if isBottomType (intersectTypesEnv typeEnv vaenv r trueType) then Just False
    else Nothing
    where
      predResult value p
        | nameStr (ptName p) == macroThisKey = Just value
        | otherwise = do
            args <- traverse (argResult value) (ptArgs p)
            name <- resolveName (ptName p)
            let p' = p{ptName=name, ptArgs=args, ptPreds=PredsNone}
            if settled p'
              then Just (singletonType p')
              -- An application no arrow and no declaration accepts, in the closed world: a dead
              -- end, which reduces to no value (see above). In the open world it stays an
              -- incomplete reach, undecided.
              else if tePrgmEnv typeEnv && null (typeGraphQuery typeEnv vaenv p')
              then Just BottomType
              else do
                let tree = reachesPartials (ReachesEnv typeEnv vaenv S.empty) [p']
                if p' `elem` completePartials typeEnv vaenv tree
                  then Just (settledPart (unionReachesTree typeEnv vaenv tree))
                  else Nothing
      -- An operand is settled data or a constant, an application spelled as a leaf, or the
      -- relation or expression top the condition's spelling gives a method call and @$this@
      -- (@~isZ(/this=~/$this)@).
      argResult value t = case t of
        UnionType Nothing leafs _ | all settled (splitUnionType leafs) -> Just t
        UnionType Nothing leafs [] | [l] <- splitUnionType leafs -> predResult value l
        TopType _ (PredsOne (PredRel p)) -> predResult value p
        TopType _ (PredsOne (PredExpr p)) -> predResult value p
        _ -> Nothing
      settled p = not (null (typeGraphData typeEnv (ptName p)))
      settledPart (UnionType mTop leafs consts) = UnionType mTop (joinUnionType (filter settled (splitUnionType leafs))) consts
      settledPart t = t
      resolveName n
        | H.member n tg = Just n
        | otherwise = case relativeNameFilter n (H.keys tg) of
            [k] -> Just k
            _   -> Nothing

  typeGraphData typeEnv name = concatMap dataPartials $ flatObjectMapItem $ graphItemOrEmpty typeEnv name
    where
      -- Data definitions have no arrow (oaArr=Nothing); their object expression carries the
      -- declared type with linked ptVars/ptArgs. A declared VALUE of the type — a note's
      -- @Task/Simple(contents= "..", state= False)@, an arrow-less object of the same name whose
      -- every argument is given a value ('isValueObjArr') — is not the definition: its
      -- constants are one value's, not the type's fields, and reading them as the declaration
      -- made the spread floor of @Task/Simple..@ that one value (only the first task of a note
      -- was then a @DShow@).
      dataPartials oa@ObjArr{oaArr=Nothing} | not (isValueObjArr oa) = case getMetaType $ getExprMeta $ oaObjExpr oa of
        UnionType Nothing leafs _ -> splitUnionType leafs
        _                         -> []
      dataPartials _ = []

  -- The arrow-object complement of 'typeGraphData': input templates of declarations and
  -- definition arms. A mid-solve object whose input presentation is not yet a plain leaf
  -- union (e.g. still ⊤) contributes nothing — callers treat missing templates as
  -- no-information, the monotone-safe direction. A relatively-spelled name (a call's
  -- @/operator++@ against a module-keyed @/Data/…/operator++@ objMap entry) matches by
  -- 'relativeNameFilter', the same fallback 'typeGraphArgKeys' uses.
  --
  -- An absolute name matches only itself ('relativeNameMatches'), so one the graph does not hold
  -- names nothing in it and the keys are not scanned for it (once per meet of such a partial,
  -- 'intersectPartialsBaseN').
  typeGraphArrowTemplates typeEnv@TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} name = case graphItemOf typeEnv name of
    Just item -> itemTemplates item
    Nothing | nameIsAbsolute name -> []
    Nothing -> case relativeNameFilter name (H.keys tg) of
      []    -> []
      names -> concatMap (itemTemplates . graphItemOrEmpty typeEnv) names
    where
      itemTemplates = concatMap oaInputTemplates . flatObjectMapItem

  -- A function declared in a context (@println{io -> IO}(msg -> \/Catln\/CStr)@) is an arrow from the
  -- context call (@ContextIn(value= println(msg -> \/Catln\/CStr), ctx= ..)@), so the arguments it
  -- takes are spelled one level further in, on the call in its @value@. They are its inputs as much
  -- as a plain function's own arguments are.
  typeGraphInputEmbeddedNames typeEnv@TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} =
    noteWholeGraphRead typeEnv $ S.unions [ S.unions (map leafNameSet (inputSlots tmpl))
             | item <- H.elems tg
             , oa <- flatObjectMapItem item
             , tmpl <- oaInputTemplates oa ]
    where
      inputSlots tmpl = H.elems (ptArgs tmpl) ++ contextCallSlots tmpl
      contextCallSlots tmpl
        | nameStr (makeAbsoluteName (ptName tmpl)) == ContextInStr
        , Just (UnionType _ calls _) <- H.lookup (partialKey (mkName contextValStr)) (ptArgs tmpl)
        = concatMap (H.elems . ptArgs) (splitUnionType calls)
        | otherwise = []

  -- The arg keysets come from the object EXPRESSION structure ('exprAppliedArgs' /
  -- 'inExprSingleton' — the same keying every partial built from an expression uses), not
  -- the expression's meta type, which need not be resolved. A relatively-spelled name (a
  -- class-graph node's @#noCore@ vs the objMap's @/#noCore@) matches by
  -- 'relativeNameFilter'; its realizable arg-sets are bounded by the union over every
  -- matching declaration. A name matching no declaration is open world ('Nothing').
  --
  -- A declaration whose own arguments end in a spread (@data(/Context[$V](value -> $V, .._))@)
  -- admits arguments beyond the ones it lists, so it bounds none of them: the name is open
  -- ('Nothing'). Read as its listed keys alone, every @Context..@ was closed to @Context(value)@
  -- exactly, and a context holding any state was outside its own type.
  typeGraphArgKeys typeEnv@TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} name = case graphItemOf typeEnv name of
    Just item | itemOpen item -> Nothing
              | otherwise     -> Just $ itemArgKeys item
    -- Only a relative spelling is looked for by suffix ('typeGraphArrowTemplates').
    Nothing   | nameIsAbsolute name -> Nothing
    Nothing   -> case relativeNameFilter name (H.keys tg) of
      []    -> Nothing
      names | any (itemOpen . graphItemOrEmpty typeEnv) names -> Nothing
            | otherwise -> Just $ concatMap (itemArgKeys . graphItemOrEmpty typeEnv) names
    where
      itemArgKeys item = [ S.fromList [inExprSingleton (oaObjExpr a) | a <- exprAppliedArgs (oaObjExpr oa)]
                         | oa <- flatObjectMapItem item ]
      itemOpen item = any (exprSpreadsArgs . oaObjExpr) (flatObjectMapItem item)

  typeGraphNameOpen typeEnv@TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} name = case graphItemOf typeEnv name of
    Just item -> itemOpen item
    Nothing   | nameIsAbsolute name -> False
    Nothing   -> any (itemOpen . graphItemOrEmpty typeEnv) (relativeNameFilter name (H.keys tg))
    where
      itemOpen item = any (exprSpreadsArgs . oaObjExpr) (flatObjectMapItem item)

  typeGraphDeclaredResult typeEnv vaenv partial@PartialType{ptName=pName} =
    foldl' (intersectTypesEnv typeEnv vaenv) unrefined (coveringRefinementResults typeEnv vaenv item partial)
    where
      -- What the declarations alone promise. Every refinement covering the call then narrows it
      -- ('coveringRefinementResults'): each is a further promise about the same result.
      unrefined = case declDests of
        (_:_) -> unionAllTypes typeEnv declDests
        -- No declaration (e.g. @sorted@): the return type is INFERRED from the definition arms —
        -- 'PTopType' until they resolve, refining as they do. Each arm dest's own raw applied leaves
        -- are then replaced by THEIR declared result (one level, declared-only, so this terminates):
        -- @sorted@'s Cons arm dest @insert(raw)@ becomes @∀ConsList@, giving @sorted@ the @Cons@ its
        -- arms produce so a requirement built from it admits @Cons@ instead of collapsing to @Nil@.
        []    -> case [ armDest
                      | oa@ObjArr{oaArr=Just{}} <- omiDefs item
                      , not $ isBottomType $ intersectTypesEnv typeEnv vaenv (singletonType partial) (getMetaType $ getExprMeta $ oaObjExpr oa)
                      , armDest <- arrowDestType typeEnv vaenv partial oa ] of
                   []       -> PTopType
                   defDests -> unionAllTypes typeEnv (defDests ++ concatMap declResultOfRawLeaves defDests)
      item = graphItemOrEmpty typeEnv pName
      declDests = [ dest
                  | oa@ObjArr{oaArr=Just{}} <- omiDecls item
                  , not $ isBottomType $ intersectTypesEnv typeEnv vaenv (singletonType partial) (getMetaType $ getExprMeta $ oaObjExpr oa)
                  , dest <- arrowDestType typeEnv vaenv partial oa ]
      -- The declared result of each raw applied leaf in a type (declared functions only — no
      -- recursion into undeclared ones, keeping this bounded).
      declResultOfRawLeaves (UnionType Nothing ls _) =
        [ d | l <- splitUnionType ls, null (typeGraphData typeEnv (ptName l))
            , oa@ObjArr{oaArr=Just{}} <- omiDecls (graphItemOrEmpty typeEnv (ptName l))
            , not $ isBottomType $ intersectTypesEnv typeEnv vaenv (singletonType l) (getMetaType $ getExprMeta $ oaObjExpr oa)
            , d <- arrowDestType typeEnv vaenv l oa ]
      declResultOfRawLeaves _ = []

data CTSSConfig = CTSSConfig {
  -- TODO Replace stack path with (CTSSStackConfig = CTSSStackTest | CTSSStackBuild DirString BranchString) when adding support for auto-downloading core
  ctssStackPath :: String,
  -- | Where a syntax may keep what reading it found, for the next run to be answered out of, or
  -- 'Nothing' for a compiler which reads every file it reaches.
  --
  -- Absent by default, and so for every test: a cache is about a program read over and over by a
  -- reader who comes back to it, and a test which wrote into a shared one would be a test whose
  -- answer depended on what had been run before it. A doc server is what has one.
  --
  -- Held here rather than found by whoever wants it, so that what a run caches is the run's own
  -- setting and a test can be given a directory of its own.
  ctssCachePath :: Maybe FilePath,
  -- | The day a run reads @today@ as, or 'Nothing' for a run which reads it off the clock.
  --
  -- A note may write a query saying @due before tomorrow@, and what that asks for is decided by the
  -- day it is asked on. Off the clock, the same query is a different question every day, and a test
  -- written over one could only assert what today happens to make true.
  --
  -- Held here for the reason 'ctssCachePath' is: a run's own setting rather than something found
  -- wherever it is wanted. Every test pins it, so a test's queries ask one thing forever; a doc
  -- server leaves it absent and reads the day each evaluation is made on.
  ctssToday     :: Maybe Day,
  -- | The pages whose notes a build does not print, by the path they are displayed as.
  --
  -- A test may compile a page that is written to fail, and assert the errors it fails with. Those
  -- errors are what the test expects, so printing them only buries the ones that are not among the
  -- run's output. Tests quiet every page named @*.err.ct@, which is how such a page is named; the
  -- compiler quiets none, since a page someone asked to build is one whose errors they asked for.
  ctssQuietPage :: String -> Bool
                                }

testCtssConfig, buildCtssConfig :: CTSSConfig
-- | Pinned to a Saturday, which has a @last friday@ behind it and a @next monday@ ahead of it, and
-- which sits among the dates the Obsidian fixtures write.
testCtssConfig = CTSSConfig "stack" Nothing (Just (fromGregorian 2026 1 3)) (errPageSuffix `isSuffixOf`)
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
  -- Resolved here rather than kept as a @~@ path: nothing in "System.Directory" expands one, so a
  -- @~@ handed to it makes a directory of that name wherever the compiler happens to be running.
  -- The stack path beside it is left as written because its own resolution expands it
  -- ('Syntax.InferImport.importPathCandidates').
  home <- getHomeDirectory
  let cachePath = joinPath [home, ".catln", "cache", "import"]
  return $ CTSSConfig (if localExists then localPath else "~/.catln/stack") (Just cachePath) Nothing (const False)
{-# NOINLINE buildCtssConfig #-}

mkTypeEnv :: (ExprClass e, Show m, Show (e m), MetaDat m) => Prgm e m -> TypeEnv (ObjArrTypeGraph e m)
-- The env is stamped with a query token here, as it is made: it is a value built from one program,
-- and every change a consumer makes to it afterwards is a flag the memos key on (the evaluator's
-- projection, the tree builder's closed world), or a merge, which clears the token. Without one,
-- an env that lives for a whole phase — the evaluator's, consulted at every dispatch — remembered
-- nothing, and a value predicate on a leaf it compared was evaluated through the graph at every
-- comparison.
mkTypeEnv (Prgm objMap classGraph _) = stampQueryToken $ TypeEnv classGraph (ObjArrTypeGraph objMap) typeNames boundArgKeys False defaultTypeEnvDebug False False False Nothing Nothing S.empty defaultRederiveFuel Nothing Nothing (mkSatCache classGraph typeNames)
  where
    -- Every name an argument pattern binds, with the arg keys that pattern gives it, recursing
    -- through the patterns written inside its arguments. Read by 'declaredArgKeys' for a name
    -- with no object of its own.
    boundArgKeys = H.fromListWith (++) [ (makeAbsoluteName $ oaObjPath a, [argKeysOf a])
                                       | oa <- flatObjectMap objMap
                                       , Just objE <- [oaObj oa]
                                       , a <- boundArgsIn objE ]
    boundArgsIn e = concat [a : maybe [] boundArgsIn (oaObj a) ++ maybe [] boundArgsIn (oaArr a >>= fst) | a <- exprAppliedArgs e]
    argKeysOf a = S.fromList $ maybe [] (map (inExprSingleton . oaObjExpr) . exprAppliedArgs) (oaObj a)
    -- The compiler-emitted annotation names ('builtinAnnotNames') are part of every program's
    -- vocabulary: desugar injects uses of them (doc comments become @#md@, @>@ statements become
    -- @#print@, ...) whether or not their compile.ct declarations are in the import closure.
    --
    -- An argument's value head joins them only when it names something no other name already
    -- does ('getAllObjArrNameSplit'). A relative head that resolves to a declared name is a
    -- reference to it, not a second name.
    (declaredNames, valueHeads) = mconcat $ map getAllObjArrNameSplit (flatObjectMap objMap)
    declaredAbs = map makeAbsoluteName $ map mkName builtinAnnotNames ++ declaredNames
    headDeclares h = "/" `isPrefixOf` nameStr h || not (any (relativeNameMatches h) declaredAbs)
    typeNames = S.fromList $ declaredAbs ++ map makeAbsoluteName (filter headDeclares valueHeads) ++ openKeys
    -- A key given to a declaration that admits keys beyond the ones it lists
    -- (@data(/Context[$V](value -> $V, .._))@, see 'typeGraphArgKeys') is declared where it is
    -- given: @/Context(value= (), counter= Counter(0))@ is the only place @counter@ need be written,
    -- as a pattern's argument is the only place its name is. Read only from its patterns, a key no
    -- function took as an argument was no name at all, and its value met ∅.
    openKeys = [ makeAbsoluteName k
               | oa <- flatObjectMap objMap
               , e <- maybeToList (oaObj oa) ++ maybe [] (maybeToList . fst) (oaArr oa) ++ oaAnnots oa
               , c <- subExprs e
               , Just h <- [maybeExprPath c]
               , headOpen h
               , ObjArr{oaObj=Just keyE} <- exprAppliedArgs c
               , Just k <- [maybeExprPath keyE] ]
    subExprs e = e : concatMap subExprs (exprChildren e)
    ObjectMap objItems = objMap
    openNames = [n | (n, item) <- H.toList objItems, any (exprSpreadsArgs . oaObjExpr) (flatObjectMapItem item)]
    headOpen h
      | "/" `isPrefixOf` nameStr h = h `elem` openNames
      | otherwise = any (relativeNameMatches h) openNames

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

-- | The type node of every name an object map holds an object under: one per name, however many
-- objects it holds there, as a graph has one node per key.
classGraphFromObjs :: (ExprClass e, MetaDat m, Show m, Show (e m)) => ObjectMap e m -> ClassGraph
classGraphFromObjs objMap = ClassGraph $ graphFromEdges $ map (\n -> (CGType, PTypeName n, [])) $ S.toList $ S.fromList $ map oaObjPath $ flatObjectMap objMap

mapExprPath :: (Show m) => ((Meta m, TypeName) -> Expr m) -> Expr m -> Expr m
mapExprPath f (Value m n) = f (m, n)
mapExprPath f (EWhere m b c) = EWhere m (mapExprPath f b) c
-- An aliased pattern (@/Context(.._)\@c@) has its base's path; the alias names the whole value.
mapExprPath f (AliasExpr b a) = AliasExpr (mapExprPath f b) a
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

-- | The results that the refinement declarations of an item promise for a call, one per refinement
-- whose input covers the WHOLE call.
--
-- A refinement is a further promise about the declared result on part of its domain
-- (@isZero(n -> Z) -> True@ beside @isZero(n -> Nat) -> Boolean@), so what the call is declared to
-- return is the declarations' result met with every one of these. A refinement that covers only
-- part of the call says nothing about it: whether the call's value falls in the part it covers is
-- not known, so it is left out rather than read as applying.
coveringRefinementResults :: (TypeGraph tg, ExprClass e, Show m, Show (e m), MetaDat m) => TypeEnv tg -> TypeVarArgEnv -> ObjectMapItem e m -> PartialType -> [Type]
coveringRefinementResults typeEnv vaenv item partial =
  [ unionAllTypes typeEnv dests
  | oa@ObjArr{oaArr=Just{}} <- omiRefineDecls item
  , isSubtypeOfWithEnv typeEnv vaenv (singletonType partial) (getMetaType $ getExprMeta $ oaObjExpr oa)
  , let dests = arrowDestType typeEnv vaenv partial oa
  , not (null dests) ]

arrowDestType :: (TypeGraph tg, ExprClass e, Show m, Show (e m), MetaDat m) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> ObjArr e m -> [Type]
arrowDestType = arrowDestTypeBase False

-- | 'arrowDestType' for DISPATCH (TreeBuild/Eval arrow selection): additionally resolves
-- construction fields valued by an application of an object argument through the src
-- partial's arg slot (see @resolvedFields@ below). Dispatch works on a fixed, fully-resolved
-- src, so the extra resolution is stable there; during typecheck the same read would resolve
-- differently across epochs as the partial's vars narrow, growing the reached dest union and
-- violating monotonicity ('verifySchemeActLowers').
arrowDestTypeDispatch :: (TypeGraph tg, ExprClass e, Show m, Show (e m), MetaDat m) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> ObjArr e m -> [Type]
arrowDestTypeDispatch = arrowDestTypeBase True

arrowDestTypeBase :: (TypeGraph tg, ExprClass e, Show m, Show (e m), MetaDat m) => Bool -> TypeEnv tg -> TypeVarArgEnv -> PartialType -> ObjArr e m -> [Type]
arrowDestTypeBase resolveAppFields typeEnv ambient src oa@ObjArr{oaArr=Just (oaArrExpr, oaM)} = map (fromCallerScope . arrowDestTypeForVaenv . withScope . withDeclaredArgResults . ambientEnv . fmap abType) $ exprVarArgsWithSrc typeEnv (oaObjExpr oa) srcPartial
  where
    -- The source's external references are the QUERYING scope's variables: a call written in a
    -- definition body passes its arguments as links to the definition's own variables
    -- (@idf(/x=$_/Q)@ in @ident[$Q](y -> $Q)= idf(x= y)@). To the arrow they are values it knows
    -- nothing more of, and its result is spelled in them — @idf@ returns its @x@, which is the
    -- caller's @$_/Q@. They are renamed apart for the arrow's reading ('callerScopeName'), so a
    -- variable of the arrow's own with the same name (@idf[$Q]@) cannot capture them, and named
    -- back in the result. Read as the ⊤ they would denote in the arrow's scope instead, the
    -- caller's link is gone from the result: the definition's own result reads ⊤ where it is
    -- @$Q@. An arrow the querying scope itself adds ('ArgObj') is defined in that scope, so its
    -- references are already the scope's own ('withScope').
    linksCallerScope = oaBasis oa /= ArgObj
    toCallerScope = renameExtVarAuxes (mapTypeVarAuxName callerScopeName)
    fromCallerScope | linksCallerScope = renameExtVarAuxes (mapTypeVarAuxName fromCallerScopeName)
                    | otherwise        = id
    -- An arrow the querying scope itself adds to its environment — an argument of the enclosing
    -- definition, or a lambda the call is given ('ArgObj') — is defined IN that scope, so its
    -- result is read with that scope's variables as well as its own match's: the augmented
    -- @fn(v) -> $_B@ of a definition returns the definition's @$_B@. A variable the scope has not
    -- bound concretely yet stays the link it is (bound as itself), rather than the ⊤ an unbound
    -- reference would read as. A global arrow is defined in its own scope and never sees the
    -- querying one: a callee's own variable would otherwise be captured by a caller's of the same
    -- name. The arrow's own bindings take precedence.
    withScope local
      | oaBasis oa == ArgObj = H.union local (H.mapWithKey scopeLink ambient)
      | otherwise            = H.union local callerLinks
    -- The caller's renamed links stay what they are while the arrow reads them: each is bound to
    -- itself, so substituting the arrow's result keeps the link rather than reading it as ⊤.
    callerLinks = H.fromList [ (extKey v, TypeVar v TVExt PTopType) | v <- S.toList (typeExtVarAuxes srcSpelled) ]
    scopeLink (VarKey _ v) PTopType = TypeVar v TVExt PTopType
    scopeLink _ t                   = t
    -- The arm's higher-order arguments read as its declaration gives them: an unannotated
    -- @fn(v)@ is the declared @fn(v -> $_A) -> $_B@, so a result variable only the declaration
    -- spells (@$_B@, which the arm's body result is written in) is bound to what the call's
    -- argument returns — its input object resolved through the environment's arrows.
    withDeclaredArgResults vaenv0 = foldr bindResult (withDeclaredVars vaenv0) (typeGraphDeclaredArgResults typeEnv (ptName srcPartial))
      where
        -- A variable only the declaration spells (@$M@, tied to @m@) is read as the declaration
        -- binds it from this call ('typeGraphDeclaredVarBindings'); the arm's own bindings stay.
        withDeclaredVars env = H.union (H.fromList
          [ (extKey v, t) | (v, t) <- H.toList (typeGraphDeclaredVarBindings typeEnv H.empty srcPartial)
                          , t /= PTopType, maybe True (== PTopType) (H.lookup (extKey v) env) ]) env
        bindResult (k, TypeVar v _ _) env
          | maybe True (== PTopType) (H.lookup (extKey v) env)
          , Just slot <- typeGetArgLoose k srcPartial
          , res <- hoSlotResult typeEnv env k slot
          , res /= PTopType
          = H.insert (extKey v) res env
        bindResult _ env = env
    srcPartial = case maybeGetSingleton srcSpelled of
      Just s  -> s
      Nothing -> error $ printf "Non-singleton in arrowDestType"
    srcSpelled | linksCallerScope = toCallerScope (singletonType src)
               | otherwise        = substituteTVExt typeEnv H.empty (singletonType src)
    arrowDestTypeForVaenv vaenv = case oaArrExpr of
      -- _ | trace (printf "arrowDestType from %s with %s to %s.\n\t\tVanev: %s" (show src) (show oa) (show joined) (show vaenv)) False -> undefined
      -- A body that is an argument reference (@= x@) or a call of a higher-order argument
      -- (@= fn(v= x)@): the argument's result — for a higher-order argument, its input object
      -- resolved through the environment's arrows ('hoSlotResult').
      --
      -- An argument whose arrow this frame does not hold (a definition built for no particular
      -- call, where no lambda is given) returns what its declaration says it does
      -- (@fn(v -> $_A) -> $M@, the declared @$M@ read as the call binds it), the declaration
      -- taking precedence over the slot as 'typeGraphDeclaredArgResults' has it.
      --
      -- A body that is the argument
      -- within a condition (@= a | a.isZ == True@, whose path is the argument's) is the
      -- argument's result within the condition's predicates, which the body's where carries
      -- ('TypeCheck.Constrain', the @ConWhere@ case); read as the bare argument, the condition
      -- was lost at every call, and @k(a= S(n= Z))@ read as @S(n= Z)@ where it is empty.
      Just n | isJust (maybeExprPath n) -> withinDeclared $ maybe joinedDispatch (withinWhere n . orDeclaredArgResult (inExprSingleton n) . substitute . hoSlotResult typeEnv vaenv (inExprSingleton n)) (H.lookup (extKey (TVArg $ inExprSingleton n)) vaenv)
      _                              -> withinDeclared joinedDispatch
      where
        substitute = substituteTVExt typeEnv vaenv
        orDeclaredArgResult k PTopType = case lookup k (typeGraphDeclaredArgResults typeEnv (ptName srcPartial)) of
          Just declared -> substitute declared
          Nothing       -> PTopType
        orDeclaredArgResult _ t = t
        withinWhere n t = case exprWhereCond n of
          Just _  -> within (substitute (TopType H.empty (carriedPreds (getExprType n)))) t
          Nothing -> t
        -- The arrow's DECLARED result refines what the body gives at every call: a definition
        -- @h(a -> Nat) -> Nat_isZ= a@ promises @a@ within @Nat_isZ@, so @h(a= S(n= Z))@ is
        -- @S(n= Z) | $this.isZ == True@ — decided false, empty — where the body alone read
        -- @S(n= Z)@ and the refinement was left to the runtime (the static-first rule). Only the
        -- declaration's VALUE predicates are met here: its classes and shape are the result's
        -- requirement, which acts by arrow choice ('TypeCheck.TypeUtils.arrowConstrainUbs'), and
        -- a shape meet can respell a representation eval's value matching reads. The meta reads
        -- as the declaration met with the inferred actual ('TypeCheck.Show.showM', and
        -- 'TypeCheck.Decode.toMeta' once typed), so the predicates are on its top or its leaves.
        withinDeclared = case predsExprAtoms (carriedPreds arr'0) of
          []    -> id
          atoms -> within (TopType H.empty (mkAnd (map (PredsOne . PredExpr) atoms)))
        -- A value read within predicates. An occurrence keeps its link and takes the predicates
        -- as its inner (@⟦v⟧ ∩ ⟦inner⟧@, closed inners only — 'TypeVar'): met as a type it would
        -- read as the predicates alone, the binding lost.
        within predTop t@(TypeVar v loc inner)
          | typeHasTypeVars predTop = t
          | otherwise               = TypeVar v loc (intersectTypes typeEnv inner predTop)
        within predTop t = intersectTypes typeEnv t predTop
        -- The predicates a type carries: on its top or on its one leaf (@/a | PredExpr …@).
        carriedPreds (UnionType (Just (preds, _, _)) _ _) = preds
        carriedPreds (UnionType Nothing leafs []) | [l] <- splitUnionType leafs = ptPreds l
        carriedPreds _ = PredsNone
        expr' = fmap (substitute . getExprType) oaArrExpr
        -- Re-link the declared result's Top class-parameter slots to the type variables the source
        -- bound before substituting: an accessor result parameterized by the receiver's own type
        -- (@:$T.eq -> Eqv[$T]@) desugars to @Eqv[/T=Top]@, dropping the @$/T@ link, so without this
        -- the element cannot follow the receiver (see 'relateArrowVars').
        arr'0 = substitute $ relateArrowVars vaenv $ getMetaType oaM
        -- DISPATCH ONLY: the arm's own result meta can be Top, and then there is nothing to
        -- substitute. A result spelled by a declaration variable is erased to the Top it denotes
        -- before tree build — measured @oaM=TopType@ for @f[$B](..) -> $_B@ — while a concrete one
        -- survives, which is the whole difference between a tail-recursive fold that builds and one
        -- that does not. The DECLARATION still holds the result, and instantiating it at this call
        -- is exactly 'typeGraphDeclaredResult', the same value 'TreeBuild.buildPartialCallTree'
        -- already carries beside it as the call's value bound. Kept out of the typecheck-side
        -- 'arrowDestType' for the reason 'arrowDestTypeDispatch' gives: a read that resolves
        -- differently as the partial's vars narrow violates monotonicity there.
        arr' | arr'0 /= PTopType = arr'0
             | resolveAppFields  = typeGraphDeclaredResult typeEnv vaenv srcPartial
             | otherwise         = arr'0
        -- The body's type is preferred, but a Top body type is not information: an arm returning a
        -- bare declaration variable (@foldl(lst= Nil, ..)= init@) presents one, and an arrow whose
        -- presented dest is Top is refused outright ("Arrow has indeterminate return type").
        joined = rereadForCall $ case fmap dropLocalArrowCalls expr' of
          Just t | t /= PTopType || not resolveAppFields -> t
          _                                              -> arr'
        -- A body that is a call given lambdas, read again for THIS call. The body's type was
        -- inferred once, for the definition's own parameters, and the call it makes holds an
        -- input object whose arrow is local to it — so its stored result is all the definition
        -- says, whatever the caller passes. A bind's continuation is one: @cont(x)@ (@x@ inferred
        -- @∀Add@) holds @bindap(m= b, fn(v)= cont2(x= x, y= v))@, and a caller passing
        -- @x= Integer@ read @∀Add@ back where the body gives an @Integer@. Read again with the
        -- caller's bindings, each lambda's body gives its result as this call has it, and the
        -- call given them is answered by the arrows. Both readings bound the value, so the
        -- result is their meet. Only a call that binds something the body was inferred wider
        -- for is read again; any other reads as the definition already says.
        rereadForCall t = case oaArrExpr of
          Just body | teRederiveFuel typeEnv > 0
                    , not (null (bodyLambdas body))
                    , any narrowedRef (exprArgRefs body)
                    -> intersectTypes typeEnv t (rereadExpr typeEnv vaenv body)
          _ -> t
        -- A reference to one of the definition's parameters that this call binds within what the
        -- parameter was inferred as, and not to all of it.
        narrowedRef e = case (H.lookup (extKey (TVArg k)) vaenv, H.lookup k ownParams) of
          (Just fv, Just declared) | not (typeHasVars fv), fv /= PTopType
            -> isSubtypeOf typeEnv fv declared && not (isSubtypeOf typeEnv declared fv)
          _ -> False
          where k = inExprSingleton e
        ownParams = fmap (getMetaType . snd . head) $ H.filter (not . null) $ snd $ splitVarArgEnv $ exprVarArgs (oaObjExpr oa)
        -- A body that is a call given a lambda presents what the call reaches, never the raw call:
        -- the lambda is an arrow local to that call, so outside it the unrewritten form cannot be
        -- rewritten or read (the rule 'TypeCheck.TypeUtils.arrowConstrainUbs' applies to the
        -- call's own result). The arm's result carries what the call reaches; the raw leaves are
        -- replaced by it.
        dropLocalArrowCalls t@(UnionType mTop leafs consts) = case partition hasLocalArrowSlot (splitUnionType leafs) of
          ([], _)     -> t
          (_, keptLs) -> unionTypes typeEnv (UnionType mTop (joinUnionType keptLs) consts) arr'
        dropLocalArrowCalls t = t
        hasLocalArrowSlot l = any (\(k, v) -> isJust (hoSlotInputObject k v)) (H.toList (ptArgs l))
        -- Only rewrite a body that directly CONSTRUCTS a data value (@Cons(head=x)@): its applied
        -- arguments are the constructor's fields. A function/operator body (@a && b@, @f(a=..)@) has
        -- applied arguments that are the callee's parameters, not the result's fields, so leave it.
        -- A body that constructs a value or calls a function is read with the values its argument
        -- references are bound to ('applyArgRefBindings'); a construction also takes the fields
        -- valued by an application of one of the object's own arguments ('resolvedFields').
        joinedResolved = case oaArrExpr of
          Just b | isDataConstruction typeEnv b -> applyArgRefBindings typeEnv b resolvedFields joined
                 | otherwise                    -> applyArgRefBindings typeEnv b argRefFields joined
          Nothing -> joined
        -- DISPATCH ONLY ('arrowDestTypeDispatch'): the presented dest must be what the BUILT arm
        -- returns. The arm's body compiles toward the arrow's result meta ('TreeBuild.buildArrow'),
        -- so a body-meta leaf the graph can still rewrite (a raw applied term such as
        -- @mkComparison(..)@ in a @compare@ arm) settles INSIDE the arm; presenting it raw would
        -- make the caller's followup re-dispatch the already-settled result (applying
        -- @mkComparison@'s value-pattern arms to @Equal@). Replace the raw leaves by the result
        -- meta's presentation; settled leaves (data constructions, forwarded arguments) keep
        -- their tighter per-call form. Typecheck-side queries ('arrowDestType') keep the raw
        -- presentation — the reaches trees walk through it.
        joinedDispatch
          | resolveAppFields
          , arr' /= PTopType
          , UnionType mTop leafs consts <- joinedResolved
          , (_:_, settledLs) <- partition (null . typeGraphData typeEnv . ptName) (splitUnionType leafs)
          = unionTypes typeEnv (UnionType mTop (joinUnionType settledLs) consts) arr'
          | otherwise = joinedResolved
        -- A definition body that constructs a value — @Cons(head=x)@ — stores its fields with the
        -- body's (abstract) argument types: @head@ becomes Top because @x@ is abstract in the body.
        -- At dispatch the vaenv binds the argument (@x=Integer@), so resolve each field whose value
        -- is an argument reference through the vaenv, recovering @head=Integer@ (and hence the data
        -- parameter it defines — see 'applyResolvedFields'). This is the per-field analogue of the
        -- bare-path body case above (@= v@).
        resolvedFields = H.union argRefFields $ H.fromList
          -- DISPATCH ONLY ('arrowDestTypeDispatch'): a field valued by an APPLICATION of one
          -- of the object's own arguments (@head= fn(v=head)@ where @fn@ is an object arg) is
          -- the argument's OUTPUT, which the dispatch-resolved src partial carries as the
          -- arg's slot — a declaration pointer slot (@/fn(..)=$B@, see 'updateTypeProp')
          -- resolves through the partial's own bindings (@/B=Integer@ from the call
          -- intersection). Unresolved (Top) slots and var reads contribute nothing.
          [ (inExprSingleton (oaObjExpr a), reduceApplied out)
          | resolveAppFields
          , Just b <- [oaArrExpr]
          , a <- exprAppliedArgs b
          , Just (Just valExpr, _) <- [oaArr a]
          , isJust (maybeExprPath valExpr)
          , not (null (exprAppliedArgs valExpr))
          , Just out <- [hoSlotResult typeEnv vaenv (inExprSingleton valExpr) <$> typeGetArgLoose (inExprSingleton valExpr) srcPartial]
          , out /= PTopType
          , not (isTypeVar out) ]
        argRefFields = maybe H.empty (exprArgRefBindings vaenv) oaArrExpr
        -- The applied argument's slot holds what the argument RETURNS, which for a lambda whose
        -- body is itself a call is the raw call partial (@/operator+(l=Integer, r=Integer)@) —
        -- at the type level a term may always stay unrewritten, so 'arrowConstrainUbs' keeps the
        -- raw form alongside the arrows' dests, and a slot bounded only by a free declaration
        -- var (@fn(..) -> $_B@) has no consumer bound to drop it. The built arm body's tree,
        -- however, always applies the argument, so at dispatch the field settles to what the
        -- callee's arrows produce.
        reduceApplied = reduceAppliedType typeEnv vaenv
arrowDestTypeBase _ _ _ _ oa@ObjArr{oaArr=Nothing} = error $ printf "Unexpected call to arrowDestType with type obj (which has no arrow to find the dest type of): %s" (show oa)

-- | The lambdas an application is given (@f(fn(v)= body)@): each applied argument whose object
-- takes parameters and which has a body, as its object and body.
bodyLambdas :: (ExprClass e, MetaDat m, Show m) => e m -> [(e m, e m, Meta m)]
bodyLambdas e = [ (lamObj, lamBody, lamM) | a <- exprAppliedArgs e, Just lamObj <- [oaObj a]
                                          , not (null (exprAppliedArgs lamObj)), Just (Just lamBody, lamM) <- [oaArr a] ]

-- | The argument references an expression makes (@x@ in @f(a= x)@), through its applications and
-- the bodies of the lambdas they are given.
exprArgRefs :: (ExprClass e, MetaDat m, Show m) => e m -> [e m]
exprArgRefs e
  | isJust (maybeExprPath e), null (exprAppliedArgs e) = [e]
  | otherwise = concat [ exprArgRefs v | a <- exprAppliedArgs e, Just (Just v, _) <- [oaArr a] ]

-- | An expression's type read again under the bindings of one call ('arrowDestTypeBase'): an
-- argument reference is what the call binds it to, and an application is answered by the
-- arrows for the arguments it is given here — each lambda it is given an arrow from its input
-- object to its body read the same way, with its parameters bound as its pattern types them.
-- An application's own type is the form it is applied as, not what it gives, so a reading is
-- met with what it gives only where that is known — a lambda's result, the definition's — and
-- a form this cannot read (an application whose arguments are not all named, one no arrow
-- answers or whose answer is still to be rewritten) reads as unknown.
rereadExpr :: (TypeGraph tg, ExprClass e, MetaDat m, Show m, Show (e m)) => TypeEnv tg -> TypeVarArgEnv -> e m -> Type
rereadExpr typeEnv vaenv e
  -- A reference is what the call binds it to. Its own type is only its name, which says nothing
  -- of the value.
  | isJust (maybeExprPath e), null (exprAppliedArgs e) = case H.lookup (extKey (TVArg (inExprSingleton e))) vaenv of
      Just fv | not (isTypeVar fv) -> fv
      _                            -> PTopType
  | Just callee <- maybeExprPath e
  , args@(_:_) <- exprAppliedArgs e
  , Just slots <- mapM slotOf args
  = let lams = [ (getExprType lamObj, intersectTypes typeEnv (getMetaType lamM) (rereadExpr typeEnvL' (withParams lamObj) lamBody))
               | (lamObj, lamBody, lamM) <- bodyLambdas e ]
        -- The lambdas' bodies are read in the scope they are written in, one level down.
        typeEnvL' = typeEnv{teRederiveFuel = teRederiveFuel typeEnv - 1}
        typeEnvL = typeGraphWithArgArrows typeEnv lams
        partial = (partialVal callee){ptArgs = H.fromList slots}
    in if teRederiveFuel typeEnv <= 0 then PTopType else case typeGraphQuery typeEnvL vaenv partial of
         []    -> PTopType
         dests -> settledOnly (reduceAppliedTypeDeep typeEnvL vaenv (unionAllTypesWithEnv typeEnvL vaenv dests))
  | otherwise = getExprType e
  where
    -- What the arrows answer is read only once it has settled: a form still to be rewritten (a
    -- call no arrow here answers yet) reads nothing of what it will give, so the reading is
    -- unknown rather than that form. So does an answer spelled in a variable: a callee's own
    -- variable is not bound in the scope the reading is for, and read there it is the ⊤ it
    -- denotes, which would hand the definition's result a link it has no binding for.
    settledOnly t@(UnionType _ leafs _)
      | not (typeHasVars t)
      , all (not . null . typeGraphData typeEnv . ptName) (splitUnionType leafs) = t
    settledOnly _ = PTopType
    -- A lambda's slot holds its input object; any other argument is its value read again.
    slotOf a = case (oaObj a, oaArr a) of
      (Just o, _) | not (null (exprAppliedArgs o)) -> Just (inExprSingleton o, getExprType o)
      (Just o, Just (Just v, _))                    -> Just (inExprSingleton o, rereadExpr typeEnv vaenv v)
      _                                             -> Nothing
    withParams lamObj = H.union (H.fromList [ (extKey (TVArg (inExprSingleton p)), getMetaType pM)
                                            | ObjArr{oaObj=Just p, oaArr=Just (_, pM)} <- exprAppliedArgs lamObj ]) vaenv

-- | What a term settles to once applied: each leaf the type graph rewrites is replaced by what
-- its arrows produce; a leaf no arrow accepts (a data construction, a curried partial
-- application) is already a settled value and stays itself. Constant-carrying unions and
-- predicate tops are returned unchanged.
reduceAppliedType :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
-- A 'Just'-top component (a predicate region such as a dispatch meta's @∀Boolean@) already
-- denotes settled values and is carried through unchanged; only the POSITIVE leaves are raw
-- applied terms the graph may rewrite. Punting on the mixed shape would leave the raw self-leaf
-- in a consumer's destination and short-circuit its build to 'TCTId'.
reduceAppliedType typeEnv vaenv t@(UnionType mTop leafs consts) = case splitUnionType leafs of
  [] -> t
  ls -> unionAllTypesWithEnv typeEnv vaenv (UnionType mTop H.empty consts : map reduceLeaf ls)
  where
    reduceLeaf l = case typeGraphQueryWithReason typeEnv vaenv l of
      []    -> singletonType l
      dests -> unionAllTypesWithEnv typeEnv vaenv (map (\(_, _, dest) -> dest) dests)
reduceAppliedType _ _ t = t

-- | 'reduceAppliedType' iterated to a FIXPOINT: an arrow's destination may itself carry the
-- next call's raw form (@==@'s dest holds @equal(..)@ raw, whose dest holds the accessor's
-- raw form, …), and a consumer building toward a one-step reduction still short-circuits at
-- the second level. Each leaf NAME reduces at most once (visited set), so mutual recursion
-- terminates and a recursive function's own raw form stays put after its first step.
reduceAppliedTypeFix :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
reduceAppliedTypeFix typeEnv vaenv = go S.empty
  where
    go seen t@(UnionType mTop leafs consts) =
      let ls = splitUnionType leafs
          reducible l = not (S.member (ptName l) seen) && not (null (typeGraphQueryWithReason typeEnv vaenv l))
          todo = filter reducible ls
          keep = filter (not . reducible) ls
      in if null todo
           then t
           else
             let seen' = foldr (S.insert . ptName) seen todo
                 reduced = [ unionAllTypesWithEnv typeEnv vaenv [dest | (_, _, dest) <- typeGraphQueryWithReason typeEnv vaenv l] | l <- todo ]
             in go seen' $ unionAllTypesWithEnv typeEnv vaenv (UnionType mTop (joinUnionType keep) consts : reduced)
    go _ t = t

-- | 'reduceAppliedType', additionally settling the field and parameter positions of a leaf no
-- arrow accepts: a data construction storing a raw applied term (@Box[/v=/operator+(..)]@)
-- settles to what the term's arrows produce (@Box[/v=Integer]@). Every position settles one
-- step only — arrow results are not re-reduced — so the recursion follows the finite type
-- structure and always terminates. Note this cannot tell a reducible applied term from a
-- deferred value reference stored in a field (@/Context(/value=/main)@ would wrongly settle
-- to main's result), so callers must gate on context that distinguishes them (see the
-- sibling-coverage check in 'TreeBuild.buildCallTree').
reduceAppliedTypeDeep :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
reduceAppliedTypeDeep typeEnv vaenv t@(UnionType Nothing leafs []) = case splitUnionType leafs of
  [] -> t
  ls -> unionAllTypesWithEnv typeEnv vaenv (map reduceLeaf ls)
  where
    reduceLeaf l = case typeGraphQueryWithReason typeEnv vaenv l of
      []    -> singletonType (reduceLeafPositions l)
      dests -> unionAllTypesWithEnv typeEnv vaenv (map (\(_, _, dest) -> dest) dests)
    reduceLeafPositions p@PartialType{ptArgs, ptVars} = p
      { ptArgs = fmap (reduceAppliedTypeDeep typeEnv vaenv) ptArgs
      , ptVars = fmap (reduceAppliedTypeDeep typeEnv vaenv) ptVars }
reduceAppliedTypeDeep _ _ t = t

-- | Whether an expression directly CONSTRUCTS a data value (@Cons(head=x)@): its applied arguments
-- are the constructor's fields. A function or operator application (@a && b@, @f(a=..)@) has applied
-- arguments that are the callee's parameters, not the result's fields.
isDataConstruction :: (TypeGraph tg, ExprClass e) => TypeEnv tg -> e m -> Bool
isDataConstruction typeEnv e = case maybeExprPath e of
  Just n  -> not (null (typeGraphData typeEnv n))
  Nothing -> False

-- | The bindings of an application's ARGUMENT REFERENCES: each applied argument whose value is a
-- bare reference to one of the enclosing object's arguments (@head=x@, @m= m@) is that argument's
-- bound value in the env.
--
-- A lambda (@fn(v)= y@) is not one, whatever its body: its slot holds its input object, and its body
-- is what it returns. Read as a reference, the slot was the referenced argument's value, which is
-- not an input object, and met with the slot it was nothing.
exprArgRefBindings :: (ExprClass e, Show m, Show (e m), MetaDat m) => TypeVarArgEnv -> e m -> H.HashMap ArgName Type
exprArgRefBindings vaenv e = H.fromList
  [ (inExprSingleton (oaObjExpr a), fv)
  | a <- exprAppliedArgs e
  , null (exprAppliedArgs (oaObjExpr a))
  , Just (Just valExpr, _) <- [oaArr a]
  , isJust (maybeExprPath valExpr)
  , null (exprAppliedArgs valExpr)
  , Just fv <- [H.lookup (extKey (TVArg (inExprSingleton valExpr))) vaenv] ]

-- | An application's type read with the values its argument references are bound to
-- ('exprArgRefBindings'). The type is the abstract one the expression was inferred with, where an
-- argument reference is the argument's declared type (@getL(m=Top, key=Top)@ for a generated
-- match arm whose inputs only its call site types); where a call is being built, or an arm's
-- result read, for particular bindings, each slot is what that argument is here. A construction
-- sets each field and binds the data parameter it defines ('applyResolvedFields':
-- @Cons(head=x)@ under @x=Integer@ is @Cons[/T=Integer](head=Integer)@); a call narrows each slot to
-- the binding (@mmap(m= m)@ under @m=Some(Integer)@ is @mmap(m=Some(Integer))@). Both the
-- result an arm presents ('arrowDestTypeBase') and the source of a call built inside an arm
-- ('TreeBuild.toTExprDest') are read this way, so the two sides of a call agree on what its
-- arguments are. A narrower binding only narrows the slot.
--
-- A binding holding values outside its slot which reach it is an argument the call converts on
-- the way in: a definition
-- accepts what converts to what its body needs ('TypeCheck.Constrain.updateSchemeProp'), and the
-- argument is built toward the slot ('TreeBuild.towardCallSlot'), so what the slot holds is what
-- the conversion gives, a value of the slot's type. Read narrowed to the binding instead, a
-- @String\/Text@ given to a @\/Catln\/CStr@ slot is the empty slot, and a call from nothing is no call.
applyArgRefBindings :: (TypeGraph tg, ExprClass e) => TypeEnv tg -> e m -> H.HashMap ArgName Type -> Type -> Type
applyArgRefBindings typeEnv e bindings t
  | H.null bindings = t
  | isDataConstruction typeEnv e = applyResolvedFields typeEnv bindings t
  | otherwise = case (maybeExprPath e, t) of
      (Just callee, UnionType Nothing leafs consts) ->
        UnionType Nothing (joinUnionType [ if relativeNameMatches callee (ptName l) then l{ptArgs = H.mapWithKey readSlot (ptArgs l)} else l | l <- splitUnionType leafs ]) consts
      _ -> t
  where
    readSlot k slot = case H.lookup k bindings of
      Just fv | not (isTypeVar fv), not (bindingConvertsInto typeEnv fv slot) -> intersectTypes typeEnv slot fv
      _ -> slot

-- | Whether an argument bound to @fv@ is converted on its way into a call slot of type @slot@: some
-- of what it holds is outside the slot and reaches it. What is outside and reaches nothing in the
-- slot is not converted — it is the slot's imprecision, as an open class read against another is —
-- and the binding is read within the slot ('applyArgRefBindings'). A slot of ⊤ or of a declaration
-- variable takes the argument as it is.
bindingConvertsInto :: (TypeGraph tg) => TypeEnv tg -> Type -> Type -> Bool
bindingConvertsInto typeEnv fv slot
  | slot == PTopType || typeContainsTypeVar slot || isSubtypeOf typeEnv fv slot = False
  | otherwise = let outside = snd $ differenceTypeWithEnv typeEnv H.empty fv slot
                in not (isBottomType outside) && reachesTo typeEnv H.empty outside slot

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
           Just (TypeVar (TVVar v) TVInt _) ->
             p{ ptVars = H.insertWith (intersectTypes typeEnv) v fieldType ptVars
              , ptArgs = H.insert fieldName fieldType (ptArgs p) }
           _ -> p{ptArgs = H.insert fieldName fieldType (ptArgs p)}
applyResolvedFields _ _ t = t

metaTypeVar :: Meta m -> Maybe TypeVarAux
metaTypeVar m = case getMetaType m of
  TypeVar v _ _ -> Just v
  _             -> Nothing

isSubtypeOfWithObjSrcs :: (TypeGraph tg, Show m, MetaDat m) => TypeEnv tg -> [(PartialType, ObjArr Expr m)] -> Type -> Type -> Bool
isSubtypeOfWithObjSrcs typeEnv objSrcs subType supType = any (\vaenv -> isSubtypeOfWithEnv typeEnv (ambientEnv $ (fmap abType vaenv)) subType supType) (exprVarArgsWithObjSrcs typeEnv objSrcs)
