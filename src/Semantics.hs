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
import           CtConstants         (builtinAnnotNames, operatorHasArrow)
import           Data.Graph          (graphFromEdges)
import           Data.Hashable       (hash)
import qualified Data.HashSet        as S
import           Data.IORef          (IORef, atomicModifyIORef', newIORef,
                                      readIORef)
import           Data.List           (foldl', isPrefixOf, partition, sortOn)
import           Data.Maybe
import           Data.Time.Calendar  (Day, fromGregorian)
import           Semantics.Prgm
import           Semantics.Types
import           System.Directory    (doesDirectoryExist, getHomeDirectory)
import           System.FilePath     (joinPath)
import           System.IO.Unsafe    (unsafePerformIO)
import           Text.Printf

newtype ObjArrTypeGraph e m = ObjArrTypeGraph (ObjectMap e m)
  deriving (Eq, Show)
instance Semigroup (ObjArrTypeGraph e m) where
  (ObjArrTypeGraph a) <> (ObjArrTypeGraph b) = ObjArrTypeGraph (a <> b)
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
type QueryMemoKey = (Int, (Bool, Bool, Bool, Bool), PartialType, [(VarKey, Type)])

-- | Keeps the THREE most-recently-used token generations: the per-page static env's
-- token, the current epoch's reaches token, and the current constraint's merged reaches
-- token (see 'TypeCheck.TypeUtils.mkReachesEnv') rotate constantly, so a keep-one policy
-- thrashed them away, while keep-all accumulated large-vaenv keys without bound. A new
-- (fourth) token evicts the least-recently-used generation.
{-# NOINLINE queryMemoRef #-}
queryMemoRef :: IORef [(Int, H.HashMap QueryMemoKey [(String, Type, Type)])]
queryMemoRef = unsafePerformIO (newIORef [])

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
  Just tok -> unsafePerformIO $ do
    let key = (tok, (prune, teDisableCompact typeEnv, tePrgmEnv typeEnv, teArgModeProjection typeEnv), partial, sortOn fst (H.toList vaenv))
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
    if S.member (hash key) inProgress
      then return $ objArrGraphQueryRaw prune typeEnv vaenv partial
      else do
        gens <- readIORef queryMemoRef
        case lookup tok gens >>= H.lookup key of
          Just r -> return r
          Nothing -> do
            -- Computed HERE, inside the mark, rather than published as a thunk to be forced later.
            -- The window to guard is while the answer is being worked out, and a thunk's forcing
            -- happens wherever it is demanded — outside any bracket this function could hold, and
            -- in a nested 'unsafePerformIO' that GHC is free to float or duplicate (this module
            -- already fights full-laziness for 'unsafeFreshQueryToken'). Forcing it here makes the
            -- mark's extent exactly the computation's, and publishes a value rather than a thunk —
            -- a half-evaluated one would leave the same hazard one level down. 'forceType' goes
            -- through the structure only, which is all of it that can ask the graph anything.
            r <- bracket_
                   (atomicModifyIORef' queryInProgressRef (\s -> (S.insert (hash key) s, ())))
                   (atomicModifyIORef' queryInProgressRef (\s -> (S.delete (hash key) s, ())))
                   (do let res = objArrGraphQueryRaw prune typeEnv vaenv partial
                       _ <- evaluate (forceQueryResult res)
                       return res)
            atomicModifyIORef' queryMemoRef $ \gens2 ->
              let mTok = fromMaybe H.empty (lookup tok gens2)
                  rest = filter ((/= tok) . fst) gens2
              in ((tok, H.insert key r mTok) : take 2 rest, ())
            return r

-- | Shared body of 'typeGraphQueryWithReason' (@prune=True@) and
-- 'typeGraphQueryAllWithReason' (@prune=False@ — every accepting arrow contributes).
objArrGraphQueryRaw :: (ExprClass e, MetaDat m, Show m, Show (e m)) => Bool -> TypeEnv (ObjArrTypeGraph e m) -> TypeVarArgEnv -> PartialType -> [(String, Type, Type)]
objArrGraphQueryRaw prune typeEnv@TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} vaenv partial@PartialType{ptName} = concatMap tryTArrow $ promises $ flatObjectMapItem $ H.lookupDefault mempty ptName tg
    where
      promises | prune     = mostSpecificPromises
               | otherwise = filter accepts
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
      mostSpecificPromises arrows
        | length kept == length triggered || covered = kept
        | otherwise = triggered
        where
          triggered = filter accepts arrows
          kept = filter (\oa -> not (any (`strictlyDominates` oa) triggered)) triggered
          -- What each kept promise accepts OF THIS SOURCE (the same intersect 'tryTArrow' performs).
          keptSrcs = [intersectTypesEnv typeEnv vaenv (singletonType partial) (arrowObjType oa) | oa <- kept]
          covered = materializedCovers || symbolicCovers
          materializedCovers = isSubtypeOfWithEnv typeEnv vaenv (singletonType partial) (unionAllTypesWithEnv typeEnv vaenv keptSrcs)
          -- The engine sees joint covers a leafwise subtype check cannot (e.g. Nil and Cons arms
          -- jointly covering a class-typed argument); out of fragment the materialized check decides.
          symbolicCovers = case traverse unionLeafs keptSrcs of
            Just optss
              | dispatchInFragment partial && all dispatchInFragment (concat optss)
              -> not (satLeafEscapesAll typeEnv vaenv S.empty partial (concat optss))
            _ -> False
          unionLeafs (UnionType Nothing leafs []) = Just (splitUnionType leafs)
          unionLeafs _                            = Nothing
      accepts oa@ObjArr{oaArr=Just{}} = not $ isBottomType $ intersectTypesEnv typeEnv vaenv (singletonType partial) (arrowObjType oa)
      accepts _ = False
      arrowObjType oa = getMetaType $ getExprMeta $ oaObjExpr oa
      -- @a@ strictly dominates @b@ when a's object is a strict subtype of b's (a is more specific).
      strictlyDominates a b = let oa' = arrowObjType a; ob = arrowObjType b
                              in oa' /= ob && isSubtypeOfWithEnv typeEnv vaenv oa' ob

      tryTArrow :: (ExprClass e, MetaDat m, Show m, Show (e m)) => ObjArr e m -> [(String, Type, Type)]
      tryTArrow oa@ObjArr{oaArr=Just{}} = do
        -- It is possible to send part of a partial through the arrow, so must compute the valid part
        -- If none of it is valid, then there is Nothing
        case intersectTypesEnv typeEnv vaenv (singletonType partial) (getMetaType $ getExprMeta $ oaObjExpr oa) of
          potentialSrc@(UnionType Nothing potSrcLeafs []) -> if not (isBottomType potentialSrc)
            then map (printf "using %s" (show oa), potentialSrc,) $ joinPartialDestTypes [arrowDestType typeEnv vaenv potentialSrcPartial oa | potentialSrcPartial <- splitUnionType potSrcLeafs]
            else []
          _ -> error "Unexpected non-union in TryTArrow"
      tryTArrow ObjArr{oaArr=Nothing} = []

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
  typeGraphDeclaredArgResults TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} name =
    [ (inExprSingleton aObj, getMetaType aM)
    | decl <- maybe [] omiDecls (H.lookup (makeAbsoluteName name) tg)
    , ObjArr{oaObj=Just aObj, oaArr=Just (_, aM)} <- exprAppliedArgs (oaObjExpr decl)
    , not (null (exprAppliedArgs aObj)) ]
 -- TODO: Implement this to expand other PredExprs as well, and begin using the Nothing to represent having no meaningful way to break
  typeGraphExpandPredExpr typeEnv _vaenv partial | nameStr (ptName partial) == operatorHasArrow = Just $ map (\p -> p{ptPreds=PredsOne (PredExpr partial)}) $ topTypeAsPartials typeEnv
  typeGraphExpandPredExpr typeEnv _vaenv partial = Just $ map (\p -> p{ptPreds=PredsOne (PredExpr partial)}) $ topTypeAsPartials typeEnv

  typeGraphData TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} name = concatMap dataPartials $ flatObjectMapItem $ H.lookupDefault mempty name tg
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
  typeGraphArrowTemplates TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} name = case H.lookup name tg of
    Just item -> itemTemplates item
    Nothing -> case relativeNameFilter name (H.keys tg) of
      []    -> []
      names -> concatMap (itemTemplates . (tg H.!)) names
    where
      itemTemplates = concatMap oaInputTemplates . flatObjectMapItem

  typeGraphInputEmbeddedNames TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} =
    S.unions [ S.unions (map leafNameSet (H.elems (ptArgs tmpl)))
             | item <- H.elems tg
             , oa <- flatObjectMapItem item
             , tmpl <- oaInputTemplates oa ]

  -- The arg keysets come from the object EXPRESSION structure ('exprAppliedArgs' /
  -- 'inExprSingleton' — the same keying every partial built from an expression uses), not
  -- the expression's meta type, which need not be resolved. A relatively-spelled name (a
  -- class-graph node's @#noCore@ vs the objMap's @/#noCore@) matches by
  -- 'relativeNameFilter'; its realizable arg-sets are bounded by the union over every
  -- matching declaration. A name matching no declaration is open world ('Nothing').
  typeGraphArgKeys TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} name = case H.lookup name tg of
    Just item -> Just $ itemArgKeys item
    Nothing   -> case relativeNameFilter name (H.keys tg) of
      []    -> Nothing
      names -> Just $ concatMap (itemArgKeys . (tg H.!)) names
    where
      itemArgKeys item = [ S.fromList [inExprSingleton (oaObjExpr a) | a <- exprAppliedArgs (oaObjExpr oa)]
                         | oa <- flatObjectMapItem item ]

  typeGraphDeclaredResult typeEnv@TypeEnv{teTypeGraph=ObjArrTypeGraph (ObjectMap tg)} vaenv partial@PartialType{ptName=pName} =
    case declDests of
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
    where
      item = H.lookupDefault mempty pName tg
      declDests = [ dest
                  | oa@ObjArr{oaArr=Just{}} <- omiDecls item
                  , not $ isBottomType $ intersectTypesEnv typeEnv vaenv (singletonType partial) (getMetaType $ getExprMeta $ oaObjExpr oa)
                  , dest <- arrowDestType typeEnv vaenv partial oa ]
      -- The declared result of each raw applied leaf in a type (declared functions only — no
      -- recursion into undeclared ones, keeping this bounded).
      declResultOfRawLeaves (UnionType Nothing ls _) =
        [ d | l <- splitUnionType ls, null (typeGraphData typeEnv (ptName l))
            , oa@ObjArr{oaArr=Just{}} <- omiDecls (H.lookupDefault mempty (ptName l) tg)
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
  ctssToday     :: Maybe Day
                                }

testCtssConfig, buildCtssConfig :: CTSSConfig
-- | Pinned to a Saturday, which has a @last friday@ behind it and a @next monday@ ahead of it, and
-- which sits among the dates the Obsidian fixtures write.
testCtssConfig = CTSSConfig "stack" Nothing (Just (fromGregorian 2026 1 3))
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
  return $ CTSSConfig (if localExists then localPath else "~/.catln/stack") (Just cachePath) Nothing
{-# NOINLINE buildCtssConfig #-}

mkTypeEnv :: (ExprClass e, Show m, Show (e m), MetaDat m) => Prgm e m -> TypeEnv (ObjArrTypeGraph e m)
mkTypeEnv (Prgm objMap classGraph _) = TypeEnv classGraph (ObjArrTypeGraph objMap) typeNames boundArgKeys False defaultTypeEnvDebug False False False Nothing (mkSatCache classGraph typeNames)
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
    typeNames = S.fromList $ declaredAbs ++ map makeAbsoluteName (filter headDeclares valueHeads)

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
arrowDestTypeBase resolveAppFields typeEnv ambient src oa@ObjArr{oaArr=Just (oaArrExpr, oaM)} = map (arrowDestTypeForVaenv . withScope . withDeclaredArgResults . ambientEnv . fmap snd) $ exprVarArgsWithSrc typeEnv (oaObjExpr oa) srcPartial
  where
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
      | otherwise            = local
    scopeLink (VarKey _ v) PTopType = TypeVar v TVExt PTopType
    scopeLink _ t                   = t
    -- The arm's higher-order arguments read as its declaration gives them: an unannotated
    -- @fn(v)@ is the declared @fn(v -> $_A) -> $_B@, so a result variable only the declaration
    -- spells (@$_B@, which the arm's body result is written in) is bound to what the call's
    -- argument returns — its input object resolved through the environment's arrows.
    withDeclaredArgResults vaenv0 = foldr bindResult vaenv0 (typeGraphDeclaredArgResults typeEnv (ptName srcPartial))
      where
        bindResult (k, TypeVar v _ _) env
          | maybe True (== PTopType) (H.lookup (extKey v) env)
          , Just slot <- typeGetArgLoose k srcPartial
          , res <- hoSlotResult typeEnv env k slot
          , res /= PTopType
          = H.insert (extKey v) res env
        bindResult _ env = env
    srcPartial = case maybeGetSingleton $ substituteTVExt typeEnv H.empty $ singletonType src of
      Just s  -> s
      Nothing -> error $ printf "Non-singleton in arrowDestType"
    arrowDestTypeForVaenv vaenv = case oaArrExpr of
      -- _ | trace (printf "arrowDestType from %s with %s to %s.\n\t\tVanev: %s" (show src) (show oa) (show joined) (show vaenv)) False -> undefined
      -- A body that is an argument reference (@= x@) or a call of a higher-order argument
      -- (@= fn(v= x)@): the argument's result — for a higher-order argument, its input object
      -- resolved through the environment's arrows ('hoSlotResult').
      Just n | isJust (maybeExprPath n) -> maybe joinedDispatch (substitute . hoSlotResult typeEnv vaenv (inExprSingleton n)) (H.lookup (extKey (TVArg $ inExprSingleton n)) vaenv)
      _                              -> joinedDispatch
      where
        substitute = substituteTVExt typeEnv vaenv
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
        joined = case fmap dropLocalArrowCalls expr' of
          Just t | t /= PTopType || not resolveAppFields -> t
          _                                              -> arr'
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
        isDataConstruction = case maybeExprPath =<< oaArrExpr of
          Just n  -> not (null (typeGraphData typeEnv n))
          Nothing -> False
        joinedResolved | isDataConstruction = applyResolvedFields typeEnv resolvedFields joined
                       | otherwise          = applyResolvedCallArgs joined
        -- The same holds for a body that CALLS a function (@getL(m= left, key= key)@): its raw
        -- call form stores each argument with the body's abstract type, @getL(m=Top, key=Top)@
        -- for a generated match arm whose inputs only its call site types. The call is applied to
        -- the very values the arm's own arguments are bound to, so each slot valued by a bare
        -- argument reference is read through the vaenv, narrowing it to the binding
        -- (@getL(m=BinTree[K=Ord], key=Ord)@). A narrower source only narrows the slot, so the
        -- read is monotone.
        applyResolvedCallArgs (UnionType Nothing leafs consts)
          | Just callee <- maybeExprPath =<< oaArrExpr
          , not (H.null argRefFields) =
            UnionType Nothing (joinUnionType [ if relativeNameMatches callee (ptName l) then l{ptArgs = H.mapWithKey resolveSlot (ptArgs l)} else l | l <- splitUnionType leafs ]) consts
        applyResolvedCallArgs t = t
        resolveSlot k slot = case H.lookup k argRefFields of
          Just fv | not (isTypeVar fv) -> intersectTypes typeEnv slot fv
          _                            -> slot
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
        -- A bare argument reference (@head=x@): the field is the argument's bound value.
        argRefFields = H.fromList
          [ (inExprSingleton (oaObjExpr a), fv)
          | Just b <- [oaArrExpr]
          , a <- exprAppliedArgs b
          , Just (Just valExpr, _) <- [oaArr a]
          , isJust (maybeExprPath valExpr)
          , null (exprAppliedArgs valExpr)
          , Just fv <- [H.lookup (extKey (TVArg (inExprSingleton valExpr))) vaenv] ]
        -- The applied argument's slot holds what the argument RETURNS, which for a lambda whose
        -- body is itself a call is the raw call partial (@/operator+(l=Integer, r=Integer)@) —
        -- at the type level a term may always stay unrewritten, so 'arrowConstrainUbs' keeps the
        -- raw form alongside the arrows' dests, and a slot bounded only by a free declaration
        -- var (@fn(..) -> $_B@) has no consumer bound to drop it. The built arm body's tree,
        -- however, always applies the argument, so at dispatch the field settles to what the
        -- callee's arrows produce.
        reduceApplied = reduceAppliedType typeEnv vaenv
arrowDestTypeBase _ _ _ _ oa@ObjArr{oaArr=Nothing} = error $ printf "Unexpected call to arrowDestType with type obj (which has no arrow to find the dest type of): %s" (show oa)

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
isSubtypeOfWithObjSrcs typeEnv objSrcs subType supType = any (\vaenv -> isSubtypeOfWithEnv typeEnv (ambientEnv $ (fmap snd vaenv)) subType supType) (exprVarArgsWithObjSrcs typeEnv objSrcs)
